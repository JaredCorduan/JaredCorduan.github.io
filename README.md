# Jared's Hakyll Blog

## Overview

This is a static website built using [hakyll](https://jaspervdj.be/hakyll).

## Build the main binary

```bash
cabal build jblog
```

Build the binary any time `site.hs` changes.


## Generate blog pages

```bash
cabal run jblog:site -- build
```

## Serve the blog locally

```bash
cabal run jblog:site -- watch
```

## Make a new entry

To make a regular post, create a markdown page named
`posts/20YY-MM-DD-my-title.md`.
Commit the new file to `trunk` and push to Github.

To make a slush post, create a markdown page named
`slush/20YY-MM-DD-my-title.md`.
Commit the new file to `trunk` and push to Github.

### Helper script

Alternatively, use `blog` to create a new stub.

To make a note (a work-in-progress post):
```bash
./blog stub foo bar baz
```

To list the notes (and get their indices):
```bash
./blog notes
```

To move a note to slush or posts:
```bash
./blog publish 42
./blog publish -s 42
```

For more help:
```bash
./blog --help
```

## Deploying

Deployment to Github is done using a
[github-action](https://github.com/JaredCorduan/JaredCorduan.github.io/actions).

## Branches

Everything is in `trunk`.
