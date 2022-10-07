#!/bin/bash

MANUAL_DIR="../../../../doc/lispref"
THIS_DIR=$(pwd)

echo "Build manual"
cd "${MANUAL_DIR}"
make elisp.html HTML_OPTS="--html --css-ref=./manual.css"

cd "${THIS_DIR}"

echo "Copy manual"
cp -f "${MANUAL_DIR}/elisp.html/Parsing-Program-Source.html" .
cp -f "${MANUAL_DIR}/elisp.html/Language-Definitions.html" .
cp -f "${MANUAL_DIR}/elisp.html/Using-Parser.html" .
cp -f "${MANUAL_DIR}/elisp.html/Retrieving-Node.html" .
cp -f "${MANUAL_DIR}/elisp.html/Accessing-Node.html" .
cp -f "${MANUAL_DIR}/elisp.html/Pattern-Matching.html" .
cp -f "${MANUAL_DIR}/elisp.html/Multiple-Languages.html" .
cp -f "${MANUAL_DIR}/elisp.html/Tree_002dsitter-C-API.html" .

cp -f "${MANUAL_DIR}/elisp.html/Parser_002dbased-Font-Lock.html" .
cp -f "${MANUAL_DIR}/elisp.html/Parser_002dbased-Indentation.html" .
