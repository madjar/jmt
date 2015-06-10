# jmt, a bot that quotes movie

Just set the `SLACK_API_TOKEN` environment variable, and call it with some srt files.

## Install

You can use [stack](https://github.com/commercialhaskell/stack) to
build jmt (you don't even need haskell installed)

    stack build
    SLACK_API_TOKEN="your token" stack exec jmt "Les tontons flingueurs.srt"
