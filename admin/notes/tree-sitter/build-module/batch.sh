#!/bin/bash

languages=(
    'c'
    'cpp'
    'css'
    'c-sharp'
    'go'
    'html'
    'javascript'
    'json'
    'python'
    'rust'
    'typescript'
)

for language in "${languages[@]}"
do
    ./build.sh $language
done
