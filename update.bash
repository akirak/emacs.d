#!/bin/bash

# A script for getting configuration updates from origin

# This script can be used to update the configuration before Emacs
# starts.

# Clean the working tree

echo "Checking if $PWD has changes..."
if ! git diff-index --name-status --exit-code HEAD; then
    echo "The working tree has changes. Please stash or commit them."
    echo "Entering a subshell."
    $SHELL -i
    if ! git diff-index --name-status --exit-code HEAD; then
        echo "There are still changes. Aborting." >&2
        echo "Press enter to exit"
        read
        exit 1
    fi
else
    echo "No changes in the working tree."
fi

# Retrieve the current SHA of nix submodule

cd nix
previous_nix_sha=$(git show-ref -s HEAD)
cd ..

# Rebase HEAD onto its corresponding remote branch

remote=origin
branch=$(git symbolic-ref --short HEAD)
git fetch $remote
if git diff-tree --quiet HEAD..$remote/$branch; then
    echo "The branch is up-to-date."
else
    git --no-pager log --oneline --summary HEAD..$remote/$branch
    echo -n "Rebase onto $remote/$branch? (y/n): "
    read need_rebase
    if [[ ${need_rebase} = [Yy]* ]]; then
        git rebase $remote/$branch
        if [ $? -gt 0 ]; then
            echo "There was an error during the rebase."
            echo "Press enter."
            read
            exit 1
        fi

        # Update the Nix module if necessary
        cd nix
        current_nix_sha=$(git show-ref -s HEAD)
        if [ ${current_nix_sha} != ${previous_nix_sha} ]; then
            echo "The nix module has been updated. Needs updating..."
            set -e
            ${MAKE:-make}
            cd ..
        else
            echo "The nix module has no changes."
        fi
    fi
fi

if command -v sk >/dev/null 2>&1; then
    cd straight/repos
    for dir in $(sk -m -p 'Select Emacs packages to update: ' -c ls); do
        [ ! -d $dir ] && continue
        echo "Entering $dir"
        cd $dir
        if [ -d .git ]; then
            git pull --recurse-submodules origin
        fi
        cd ..
    done
    cd ../..
else
    echo "sk (skim) was unavailable, so Emacs packages weren't updated."
fi
