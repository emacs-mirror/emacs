#!/bin/bash

languages=(
    'bash'
    'c'
    'cmake'
    'cpp'
    'css'
    'c-sharp'
    'dockerfile'
    'go'
    'go-mod'
    'html'
    'javascript'
    'json'
    'python'
    'rust'
    'toml'
    'tsx'
    'typescript'
)

for language in "${languages[@]}"
do
    ./build.sh $language
done
