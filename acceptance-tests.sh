#!/bin/sh

# As a user, I can run git-vogue check and get pretty output.
pushd fixtures/bad_repo/
git init .
git vogue init
OUT=$(git commit)
if [[ $? == 0 ]]; then
    echo "Succeeded when failure expected"
    exit 1;
fi
