# Jared's Hakyll Blog

## Overview

This is a static website built using (hakyll)[https://jaspervdj.be/hakyll].
Deployment to Github is done using
(this)[https://jaspervdj.be/hakyll/tutorials/github-pages-tutorial.html] guide.

## Branches

The website contents are contained in the `master` branch.
The source files are contained in the `develop` branch.

## Build the main binary

```bash
stack build
```

Build the binary any time `site.hs` changes.


## Generate blog pages

```bash
stack exec site rebuild
```

## Serve the blog locally

```bash
stack exec site watch
```

## Make a new entry

Create a markdown page named
`posts/20YY-MM-DD-my-title.md`.
and generate the blog pages as above.

## Deploying

```bash
./deploy
```
