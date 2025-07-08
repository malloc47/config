#!/usr/bin/env bash
# Fetch go-task binary to run other workflows
mkdir -p ~/bin
curl -s -L https://github.com/go-task/task/releases/download/v3.43.3/task_darwin_arm64.tar.gz | tar xzvf - -C ~/bin/ task
~/bin/task darwin:install
