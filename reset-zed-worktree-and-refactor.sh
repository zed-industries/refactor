#!/bin/bash

set -eux

refactor_dir="$PWD"
cd "$1"

if [ ! -f "index.scip" ]; then
    echo "No index.scip file"
    exit 1
fi
git checkout .

cd "$refactor_dir"

cargo run --bin splitter -- "$1"
