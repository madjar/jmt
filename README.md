# jmt, a bot that quotes movie

Just set the `SLACK_API_TOKEN` environment variable, and call it with some srt files.

First, initiate the cabal sandbox with

    cabal sandbox init
    cabal install --only-dependencies

Then run it

    cabal run "Les tontons flingueurs.srt"
