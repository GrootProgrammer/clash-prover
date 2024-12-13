#!/bin/bash

echo "clean"
cabal clean
rm debug.log
echo "build"
cabal build > output.txt 2>&1
echo "render"
python showGraphviz.py
echo "done"
