{-# LANGUAGE OverloadedStrings #-}

import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows
import Text.XML.HXT.HTTP
import Data.Tree.NTree.TypeDefs

import Control.Applicative
import Control.Monad

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE

site = "http://lan.developer.lifx.com/docs/lifx-products"

download :: String -> IOSArrow b XmlTree
download src
    = readDocument [withParseHTML yes, withHTTP [], withWarnings no] src
      >>>
      getXPathTrees "//content[@class='content']//text()"

extractJson :: String -> Either String String
extractJson s = unlines <$> snipLines (lines s)
  where snipLines (x:xs) = do
          let y = init xs
              z = last xs
              expectx = "[block:parameters]"
              expectz = "[/block]"
          unless (x == expectx) $ Left $
            "got '" ++ x ++ "' but expected '" ++ expectx ++ "'"
          unless (z == expectz) $ Left $
            "got '" ++ z ++ "' but expected '" ++ expectz ++ "'"
          return y

data OrigTable =
  OrigTable
  { otCols :: !Int
  , otRows :: !Int
  , otData :: M.Map T.Text T.Text
  } deriving (Show, Read, Eq, Ord)

instance FromJSON OrigTable where
  parseJSON (Object v) = do
    cols <- v .: "cols"
    rows <- v .: "rows"
    dataMap <- v .: "data"
    return $ OrigTable cols rows dataMap

transformTable :: OrigTable -> Either String [[(T.Text, T.Text)]]
transformTable ot = mapM trans1 [0 .. otRows ot - 1]
  where trans1 n = mapM (trans2 n) [0 .. otCols ot - 1]
        trans2 r c = (,) <$> lkup "h" c <*> lkup (show r) c
        lkup r c = lkup2 $ r ++ "-" ++ show c
        lkup2 k = maybe (Left $ "couldn't find " ++ k) Right
                  (M.lookup (T.pack k) (otData ot))

encConfig = defConfig { confCompare = compare }

extractTable :: String -> Either String L.ByteString
extractTable s = do
  j <- extractJson s
  ot <- eitherDecode $ LTE.encodeUtf8 $ LT.pack j
  nt <- transformTable ot
  return $ encodePretty' encConfig $ map M.fromList nt

main :: IO ()
main = do
  [NTree (XText stuff) _ ] <- runX $ download site
  tbl <- case extractTable stuff of
    Left msg -> fail msg
    Right x -> return x
  L.putStr tbl
  putStrLn ""
