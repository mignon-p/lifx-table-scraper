This Haskell program scrapes the table from the web page
<http://lan.developer.lifx.com/docs/lifx-products> and dumps it to
standard output as JSON, in a format which should be easy to digest by
other programs.

If you're not familiar with Haskell, you can build and run this
program like this:

1. Install the [Haskell Platform][1]

2. In this directory, type the following:
    ```
    cabal sandbox init
    cabal install
    .cabal-sandbox/bin/lifx-table-scraper > ~/some/where/to/save/table.json
    ```

To re-run the program (which will re-scrape the table from the
website), just repeat `.cabal-sandbox/bin/lifx-table-scraper`.  If the
program has changed, you can rebuild it by repeating `cabal install`.
It shouldn't be necessary to repeat `cabal sandbox init`.

[1]: https://www.haskell.org/downloads
