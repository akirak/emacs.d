#!/usr/bin/env bash

if ! command -v sk >/dev/null 2>&1; then
    echo "sk (skim) is unavailable" >&2
    exit 1
fi

cd straight/repos || exit 1
parent="$PWD"
for dir in $(sk -m -p 'Select Emacs packages to update: ' -c ls); do
    [ ! -d $dir ] && continue
    echo "Entering $dir"
    cd "$parent/$dir" || continue
    if [ -d .git ]; then
        git pull --ff-only --recurse-submodules origin
    fi
done
