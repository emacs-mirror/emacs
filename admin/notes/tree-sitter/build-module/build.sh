#!/bin/bash

lang=$1
topdir="$PWD"

if [ $(uname) == "Darwin" ]
then
    soext="dylib"
else
    soext="so"
fi

echo "Building ${lang}"

### Retrieve sources

repo="tree-sitter-${lang}"
sourcedir="tree-sitter-${lang}/src"
grammardir="tree-sitter-${lang}"

case "${lang}" in
    "typescript")
        sourcedir="tree-sitter-typescript/typescript/src"
        grammardir="tree-sitter-typescript/typescript"
        ;;
    "tsx")
        repo="tree-sitter-typescript"
        sourcedir="tree-sitter-typescript/tsx/src"
        grammardir="tree-sitter-typescript/tsx"
        ;;
esac

git clone "https://github.com/tree-sitter/${repo}.git" \
    --depth 1 --quiet
cp "${grammardir}"/grammar.js "${sourcedir}"
# We have to go into the source directory to compile, because some
# C files refer to files like "../../common/scanner.h".
cd "${sourcedir}"

### Build

cc -c -I. parser.c
# Compile scanner.c.
if test -f scanner.c
then
    cc -fPIC -c -I. scanner.c
fi
# Compile scanner.cc.
if test -f scanner.cc
then
    c++ -fPIC -I. -c scanner.cc
fi
# Link.
if test -f scanner.cc
then
    c++ -fPIC -shared *.o -o "libtree-sitter-${lang}.${soext}"
else
    cc -fPIC -shared *.o -o "libtree-sitter-${lang}.${soext}"
fi

### Copy out

mkdir -p "${topdir}/dist"
cp "libtree-sitter-${lang}.${soext}" "${topdir}/dist"
cd "${topdir}"
rm -rf "${repo}"
