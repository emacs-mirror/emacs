#!/bin/bash

## Copyright (C) 2017-2024 Free Software Foundation, Inc.

## This file is part of GNU Emacs.

## GNU Emacs is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## GNU Emacs is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


function git_up {
    echo [build] Making git worktree for Emacs $VERSION
    cd $REPO_DIR/emacs-$MAJOR_VERSION
    git pull
    git worktree add ../$BRANCH $BRANCH

    cd ../$BRANCH
    ./autogen.sh
}

function build_zip {
    echo [build] Building Emacs-$VERSION

    ## Clean the install location because we use it twice
    rm -rf $HOME/emacs-build/install/emacs-$VERSION
    mkdir --parents $HOME/emacs-build/build/emacs-$VERSION
    cd $HOME/emacs-build/build/emacs-$VERSION

    ## Do we need this or is it the default?
    export PKG_CONFIG_PATH=/mingw64/lib/pkgconfig


    ## Running configure forces a rebuild of the C core which takes
    ## time that is not always needed, so do not do it unless we have
    ## to.
    if [ ! -f Makefile ] || (($CONFIG))
    then
        echo [build] Configuring Emacs
        $REPO_DIR/$BRANCH/configure \
            --without-dbus \
            --without-compress-install \
            $CACHE \
            CFLAGS="$CFLAGS"
    fi

    make -j 4 $INSTALL_TARGET \
         prefix=$HOME/emacs-build/install/emacs-$VERSION
    cd $HOME/emacs-build/install/emacs-$VERSION
    zip -r -9 emacs-$OF_VERSION-no-deps.zip *
    mv emacs-$OF_VERSION-no-deps.zip $HOME/emacs-upload

    if [ -z $SNAPSHOT ];
    then
        DEPS_FILE=$HOME/emacs-build/deps/emacs-$MAJOR_VERSION-deps.zip
    else
        ## Pick the most recent snapshot whatever that is
        DEPS_FILE=`ls $HOME/emacs-build/deps/emacs-$MAJOR_VERSION-*-deps.zip | tail -n 1`
    fi

    echo [build] Using $DEPS_FILE
    unzip -d bin $DEPS_FILE

    zip -r -9 emacs-$OF_VERSION.zip *
    mv emacs-$OF_VERSION.zip ~/emacs-upload
}

function build_installer {
    cd $HOME/emacs-build/install/
    echo [build] Calling makensis in `pwd`
    cp $REPO_DIR/$BRANCH/admin/nt/dist-build/emacs.nsi .

    makensis -v4 \
             -DEMACS_VERSION=$ACTUAL_VERSION \
             -DVERSION_BRANCH=$VERSION \
             -DOUT_VERSION=$OF_VERSION emacs.nsi
    rm emacs.nsi
    mv emacs-$OF_VERSION-installer.exe ~/emacs-upload
}

set -o errexit

SNAPSHOT=
CACHE=

BUILD=1
BUILD_64=1
GIT_UP=0
CONFIG=1
CFLAGS="-O2 -static"
INSTALL_TARGET="install-strip"

## The location of the git repo
REPO_DIR=$HOME/emacs-build/git/


while getopts "gb:hnsiV:" opt; do
  case $opt in
    g)
        BUILD_32=0
        BUILD_64=0
        GIT_UP=1
        ;;
    n)
        CONFIG=0
        ;;
    i)
        BUILD=0
        ;;
    b)
        REQUIRED_BRANCH=$OPTARG
        echo "Setting Required branch $REQUIRED_BRANCH"
        ;;
    V)
        VERSION=$OPTARG
        ;;
    s)
        SNAPSHOT="-snapshot"
        CFLAGS="-O2 -static -g3"
        INSTALL_TARGET="install"
        ;;
    h)
        echo "build-zips.sh"
        echo "  -b args -- build args branch"
        echo "  -g git update and worktree only"
        echo "  -i build installer only"
        echo "  -n do not configure"
        echo "  -s snapshot build"
        exit 0
        ;;
    \?)
        echo "Invalid option: -$OPTARG" >&2
        ;;
  esac
done


## ACTUAL_VERSION is the version declared by emacs
if [ -z $ACTUAL_VERSION ];
then
    ACTUAL_VERSION=`
  sed -n 's/^AC_INIT(\[*GNU Emacs]*,[	 ]*\[*\([^]	 ,)]*\).*/\1/p' < ../../../configure.ac
`
fi

if [ -z $ACTUAL_VERSION ];
then
    echo [build] Cannot determine Emacs version
    exit 1
fi

## VERSION is the version that we want to call Emacs
VERSION=$ACTUAL_VERSION


MAJOR_VERSION="$(echo $VERSION | cut -d'.' -f1)"


## VERSION includes the word snapshot if necessary
VERSION=$VERSION$SNAPSHOT

## OF version includes the date if we have a snapshot
OF_VERSION=$VERSION

if [ -z $SNAPSHOT ];
then
    BRANCH=emacs-$VERSION
else
    BRANCH=master
    CACHE=-C
    OF_VERSION="$VERSION-`date +%Y-%m-%d`"
fi

echo Checking for required branch
if [ -z $REQUIRED_BRANCH ];
then
    :
else
    BRANCH=$REQUIRED_BRANCH
    echo [build] Building from Branch $BRANCH
    VERSION=$VERSION-${BRANCH/\//_}
    OF_VERSION="$VERSION-`date +%Y-%m-%d`"
    ## Use snapshot dependencies
    SNAPSHOT=1
    CFLAGS="-O2 -static -g3"
    INSTALL_TARGET="install"
fi

if (($GIT_UP))
then
    git_up
fi

if (($BUILD_64))
then
    if (($BUILD))
    then
        build_zip
    fi
    build_installer
fi
