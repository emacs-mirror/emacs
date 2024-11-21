#!/bin/bash

languages=(
    'bash'
    'c'
    'cmake'
    'cpp'
    'css'
    'c-sharp'
    'dockerfile'
    'elixir'
    'go'
    'go-mod'
    'go-work'
    'heex'
    'html'
    'java'
    'javascript'
    'json'
    'lua'
    'python'
    'ruby'
    'rust'
    'toml'
    'tsx'
    'typescript'
    'yaml'
)

for language in "${languages[@]}"
do
    ./build.sh $language
done
