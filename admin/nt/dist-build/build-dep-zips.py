#!/usr/bin/python3

## Copyright (C) 2017-2018 Free Software Foundation, Inc.

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
import argparse
import multiprocessing as mp
import glob
import os
import shutil
import re

from subprocess import check_output

## Constants
EMACS_MAJOR_VERSION="27"


## Options
DRY_RUN=False

## Packages to fiddle with
SKIP_PKGS=["mingw-w64-gcc-libs"]
MUNGE_PKGS ={"mingw-w64-libwinpthread-git":"mingw-w64-winpthreads-git"}
ARCH_PKGS=["mingw-w64-mpc",
           "mingw-w64-termcap",
           "mingw-w64-xpm-nox"]
SRC_REPO="https://sourceforge.net/projects/msys2/files/REPOS/MINGW/Sources"


def check_output_maybe(*args,**kwargs):
    if(DRY_RUN):
        print("Calling: {}{}".format(args,kwargs))
    else:
        return check_output(*args,**kwargs)

def extract_deps():

    # This list derives from the features we want Emacs to compile with.
    PKG_REQ='''mingw-w64-x86_64-giflib
mingw-w64-x86_64-gnutls
mingw-w64-x86_64-libjpeg-turbo
mingw-w64-x86_64-libpng
mingw-w64-x86_64-librsvg
mingw-w64-x86_64-libtiff
mingw-w64-x86_64-libxml2
mingw-w64-x86_64-xpm-nox
mingw-w64-x86_64-lcms2'''.split()

    # Get a list of all dependencies needed for packages mentioned above.
    # Run `pactree -lu' for each element of $PKG_REQ.
    pkgs = set()
    for x in PKG_REQ:
        pkgs.update(
            check_output(["pactree", "-lu", x]).decode("utf-8").split()
        )

    return sorted(pkgs)

def gather_deps(deps, arch, directory):

    os.mkdir(arch)
    os.chdir(arch)

    ## Replace the architecture with the correct one
    deps = [re.sub(r"x86_64",arch,x) for x in deps]

    ## find all files the transitive dependencies
    deps_files = check_output(
        ["pacman", "-Ql"] + deps
    ).decode("utf-8").split("\n")

    ## Produces output like
    ## mingw-w64-x86_64-zlib /mingw64/lib/libminizip.a

    ## drop the package name
    tmp = deps_files.copy()
    deps_files=[]
    for d in tmp:
        slt = d.split()
        if(not slt==[]):
            deps_files.append(slt[1])

    ## sort uniq
    deps_files = sorted(list(set(deps_files)))
    ## copy all files into local
    print("Copying dependencies: {}".format(arch))
    check_output_maybe(["rsync", "-R"] + deps_files + ["."])

    ## And package them up
    os.chdir(directory)
    print("Zipping: {}".format(arch))
    check_output_maybe("zip -9r ../../emacs-{}-{}{}-deps.zip *"
                       .format(EMACS_MAJOR_VERSION, DATE, arch),
                       shell=True)
    os.chdir("../../")


def download_source(tarball):
    print("Downloading {}...".format(tarball))
    check_output_maybe(
        "wget -a ../download.log -O {} {}/{}/download"
        .format(tarball, SRC_REPO, tarball),
        shell=True
    )
    print("Downloading {}... done".format(tarball))

def gather_source(deps):


    ## Source for gcc-libs is part of gcc
    ## Source for libwinpthread is in libwinpthreads
    ## mpc, termcap, xpm -- has x86_64, and i686 versions

    ## This needs to have been run first at the same time as the
    ## system was updated.
    os.mkdir("emacs-src")
    os.chdir("emacs-src")

    to_download = []
    for pkg in deps:
        pkg_name_and_version= \
            check_output(["pacman","-Q", pkg]).decode("utf-8").strip()

        ## Produces output like:
        ## mingw-w64-x86_64-zlib 2.43.2
        pkg_name_components = pkg_name_and_version.split()
        pkg_name=pkg_name_components[0]
        pkg_version=pkg_name_components[1]

        ## make a simple name to make lookup easier
        simple_pkg_name = re.sub(r"x86_64-","",pkg_name)

        if(simple_pkg_name in SKIP_PKGS):
            continue

        ## Some packages have different source files for different
        ## architectures. For these we need two downloads.
        if(simple_pkg_name in ARCH_PKGS):
            downloads = [pkg_name,
                         re.sub(r"x86_64","i686",pkg_name)]
        else:
            downloads = [simple_pkg_name]

        for d in downloads:
            ## Switch names if necessary
            d = MUNGE_PKGS.get(d,d)

            tarball = "{}-{}.src.tar.gz".format(d,pkg_version)

            to_download.append(tarball)

    ## Download in parallel or it is just too slow
    p = mp.Pool(16)
    p.map(download_source,to_download)

    print("Zipping")
    check_output_maybe("zip -9 ../emacs-{}-{}deps-mingw-w64-src.zip *"
                       .format(EMACS_MAJOR_VERSION,DATE),
                       shell=True)

    os.chdir("..")


def clean():
    print("Cleaning")
    os.path.isdir("emacs-src") and shutil.rmtree("emacs-src")
    os.path.isdir("i686") and shutil.rmtree("i686")
    os.path.isdir("x86_64") and shutil.rmtree("x86_64")
    os.path.isfile("download.log") and os.remove("download.log")


if(os.environ["MSYSTEM"] != "MSYS"):
    print("Run this script in an MSYS-shell!")
    exit(1)


parser = argparse.ArgumentParser()
parser.add_argument("-s", help="snapshot build",
                    action="store_true")

parser.add_argument("-t", help="32 bit deps only",
                    action="store_true")

parser.add_argument("-f", help="64 bit deps only",
                    action="store_true")

parser.add_argument("-r", help="source code only",
                    action="store_true")

parser.add_argument("-c", help="clean only",
                    action="store_true")

parser.add_argument("-d", help="dry run",
                    action="store_true")

args = parser.parse_args()
do_all=not (args.c or args.r or args.f or args.t)

deps=extract_deps()

DRY_RUN=args.d

if args.s:
    DATE="{}-".format(check_output(["date", "+%Y-%m-%d"]).decode("utf-8").strip())
else:
    DATE=""

if( do_all or args.t ):
    gather_deps(deps,"i686","mingw32")

if( do_all or args.f ):
    gather_deps(deps,"x86_64","mingw64")

if( do_all or args.r ):
    gather_source(deps)

if( args.c ):
    clean()
