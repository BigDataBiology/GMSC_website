#!/usr/bin/env bash

set -ev
if test -d dist/ ; then
    rm -rf dist.backup
    mv dist dist.backup
fi
rm -rf dist/
mkdir dist

elm make --optimize src/Main.elm --output=dist/index.html
cp src/style.css dist/
cp -r src/assets dist/ && cp _redirects dist/
