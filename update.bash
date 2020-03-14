#!/usr/bin/env bash

# A script for getting configuration updates from origin

# This script can be used to update the configuration before Emacs
# starts.

# Clean the working tree

section() {
    # TODO: Colourize the output
    echo "$*"
}

section "Updating the Emacs configuration..."

echo "Checking if $PWD has changes..."
if ! git diff-index --name-status --exit-code HEAD; then
    echo "The working tree has changes. Please stash or commit them."
    echo "Entering a subshell."
    $SHELL -i
    if ! git diff-index --name-status --exit-code HEAD; then
        echo "There are still changes. Aborting." >&2
        echo "Press enter to exit"
        read -r
        exit 1
    fi
else
    echo "No changes in the working tree."
fi

# Rebase HEAD onto its corresponding remote branch

remote=origin
branch=$(git symbolic-ref --short HEAD)
git fetch $remote
if git --no-pager log --exit-code --oneline --summary HEAD..$remote/$branch; then
    echo "The branch is up-to-date."
else
    echo -n "Rebase onto $remote/$branch? (y/n): "
    read -r need_rebase
    if [[ ${need_rebase} = [Yy]* ]]; then
        if ! git rebase "$remote/$branch"; then
            echo "There was an error during the rebase."
            echo "Press enter."
            read -r
            exit 1
        fi
    fi
fi

section "Updating the submodules in the Emacs configuration..."

# Update the submodule
git submodule update --recursive

pull_all_packages_ff() {
    local initial="$PWD"
    local parent="$PWD/straight/repos"
    cd "$parent" || return 1
    for d in *; do
        cd "$parent/$d" || continue
        git pull --ff-only --recurse-submodules origin
    done
    cd "$initial" || exit 1
}

pull_package() {
    local initial="$PWD"
    local package="$1"
    cd "straight/repos/$package" || return 1
    git pull --ff-only --recurse-submodules origin
    cd "$initial" || exit 1
}

echo -n "Rebuild the updated packages? (y/n): "
read -r update
if [[ ${update} = [Yy]* ]]; then
    echo -n "Pull all packages? (y/n): "
    read -r fetch
    if [[ ${fetch} = [Yy]* ]]; then
        pull_all_packages_ff
    else
        pull_package melpa
    fi

    section "Rebuilding org autoloads..."
    cd straight/repos/org
    make autoloads
    cd ../../..

    section "Rebuilding outdated packages..."
    emacs -nw -q --load init.el \
          --eval '(progn (akirak/straight-rebuild-outdated-packages) (kill-emacs))'
fi

echo "Done."
exit 0
