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
    'heex'
    'html'
    'java'
    'javascript'
    'json'
    'lua'
    'python'
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
