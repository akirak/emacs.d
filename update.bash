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
        read
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
    read need_rebase
    if [[ ${need_rebase} = [Yy]* ]]; then
        git rebase $remote/$branch
        if [ $? -gt 0 ]; then
            echo "There was an error during the rebase."
            echo "Press enter."
            read
            exit 1
        fi
    fi
fi

section "Updating the submodules in the Emacs configuration..."

# Update the submodule
git submodule update --recursive

section "Updating my favorite packages..."

# Auto-update packages I want to keep up-to-date
# It is important to update the MELPA package cache to prevent missing package errors
cd straight/repos
for pkg in melpa org ivy counsel swiper org-starter org-reverse-datetree \
           transient magit org-ql company-mode lsp-mode treemacs; do
    [[ ! -d $pkg ]] && continue
    echo "Trying to update $pkg package..."
    cd $pkg
    # Run git-safe-update if the program exists
    if command -v git-safe-update >/dev/null; then
        git-safe-update
    else
        git pull
    fi
    if [[ $? -gt 0 ]]; then
        echo "Failed to update the repository of $pkg package"
    fi
    cd ..
done
