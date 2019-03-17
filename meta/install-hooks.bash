#!/bin/bash

# This script must be run at the root of the repository

HOOKS=(prepare-commit-msg pre-push)

for hook in ${HOOKS[*]}; do
    cp -fv -t .git/hooks meta/git-hooks/$hook \
        && chmod a+x .git/hooks/$hook \
            || exit 1
done
