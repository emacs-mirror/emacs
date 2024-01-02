#!/usr/bin/python3

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
import argparse
import os
import shutil
import re
import functools
import operator

from subprocess import check_output

## Constants
EMACS_MAJOR_VERSION="28"

# This list derives from the features we want Emacs to compile with.
PKG_REQ='''mingw-w64-x86_64-giflib
mingw-w64-x86_64-gnutls
mingw-w64-x86_64-harfbuzz
mingw-w64-x86_64-jansson
mingw-w64-x86_64-lcms2
mingw-w64-x86_64-libjpeg-turbo
mingw-w64-x86_64-libpng
mingw-w64-x86_64-librsvg
mingw-w64-x86_64-libtiff
mingw-w64-x86_64-libxml2
mingw-w64-x86_64-xpm-nox'''.split()

DLL_REQ='''libgif
libgnutls
libharfbuzz
libjansson
liblcms2
libturbojpeg
libpng
librsvg
libtiff
libxml
libXpm'''.split()


## Options
DRY_RUN=False


def check_output_maybe(*args,**kwargs):
    if(DRY_RUN):
        print("Calling: {}{}".format(args,kwargs))
    else:
        return check_output(*args,**kwargs)

## DLL Capture
def gather_deps():

    os.mkdir("x86_64")
    os.chdir("x86_64")

    for dep in full_dll_dependency():
        check_output_maybe(["cp /mingw64/bin/{}*.dll .".format(dep)],
                           shell=True)

    print("Zipping")
    check_output_maybe("zip -9r ../emacs-{}-{}deps.zip *"
                       .format(EMACS_MAJOR_VERSION, DATE),
                       shell=True)
    os.chdir("../")

## Return all Emacs dependencies
def full_dll_dependency():
    deps = [dll_dependency(dep) for dep in DLL_REQ]
    return set(sum(deps, []) + DLL_REQ)

## Dependencies for a given DLL
def dll_dependency(dll):
    output = check_output(["/mingw64/bin/ntldd", "--recursive",
                           "/mingw64/bin/{}*.dll".format(dll)]).decode("utf-8")
    ## munge output
    return ntldd_munge(output)

def ntldd_munge(out):
    deps = out.splitlines()
    rtn = []
    for dep in deps:
        ## Output looks something like this

        ## KERNEL32.dll => C:\Windows\SYSTEM32\KERNEL32.dll (0x0000000002a30000)
        ## libwinpthread-1.dll => C:\msys64\mingw64\bin\libwinpthread-1.dll (0x0000000000090000)

        ## if it's the former, we want it, if its the later we don't
        splt = dep.split()
        if len(splt) > 2 and "msys64" in splt[2]:
            print("Adding dep", splt[0])
            rtn.append(splt[0].split(".")[0])

    return rtn

#### Source Capture

## Packages to fiddle with
## Source for gcc-libs is part of gcc
SKIP_SRC_PKGS=["mingw-w64-gcc-libs"]
SKIP_DEP_PKGS=frozenset(["mingw-w64-x86_64-glib2"])
MUNGE_SRC_PKGS={"mingw-w64-libwinpthread-git":"mingw-w64-winpthreads-git"}
MUNGE_DEP_PKGS={
    "mingw-w64-x86_64-libwinpthread":"mingw-w64-x86_64-libwinpthread-git",
    "mingw-w64-x86_64-libtre": "mingw-w64-x86_64-libtre-git",
}

## Currently no packages seem to require this!
ARCH_PKGS=[]
SRC_REPO="https://repo.msys2.org/mingw/sources"


def immediate_deps(pkgs):
    package_info = check_output(["pacman", "-Si"] + pkgs).decode("utf-8").splitlines()

    ## Extract the packages listed for "Depends On:" lines.
    dependencies = [line.split(":")[1].split() for line in package_info
                    if line.startswith("Depends On")]
    ## Flatten dependency lists from multiple packages into one list.
    dependencies = functools.reduce(operator.iconcat, dependencies, [])

    ## Remove > signs TODO can we get any other punctuation here?
    dependencies = [d.split(">")[0] for d in dependencies if d]
    dependencies = [d for d in dependencies if not d == "None"]

    dependencies = [MUNGE_DEP_PKGS.get(d, d) for d in dependencies]
    return dependencies


## Extract all the msys2 packages that are dependencies of our direct dependencies
def extract_deps():

    print( "Extracting deps" )

    # Get a list of all dependencies needed for packages mentioned above.
    pkgs = set(PKG_REQ)
    newdeps = pkgs
    print("adding...")
    while True:
        subdeps = frozenset(immediate_deps(list(newdeps)))
        newdeps = subdeps - SKIP_DEP_PKGS - pkgs
        if not newdeps:
            break
        print('\n'.join(newdeps))
        pkgs |= newdeps

    return list(pkgs)


def download_source(tarball):
    print("Acquiring {}...".format(tarball))

    if not os.path.exists("../emacs-src-cache/{}".format(tarball)):
        print("Downloading {}...".format(tarball))
        check_output_maybe(
            "wget -a ../download.log -O ../emacs-src-cache/{} {}/{}"
            .format(tarball, SRC_REPO, tarball),
            shell=True
        )
        print("Downloading {}... done".format(tarball))

    print("Copying {} from local".format(tarball))
    shutil.copyfile("../emacs-src-cache/{}".format(tarball),
                    "{}".format(tarball))


## Fetch all the source code
def gather_source(deps):

    if not os.path.exists("emacs-src-cache"):
        os.mkdir("emacs-src-cache")

    os.mkdir("emacs-src")
    os.chdir("emacs-src")

    for pkg in deps:
        pkg_name_and_version= \
            check_output(["pacman","-Q", pkg]).decode("utf-8").strip()

        ## Produces output like:
        ## mingw-w64-x86_64-zlib 2.43.2
        pkg_name_components = pkg_name_and_version.split()
        pkg_name=pkg_name_components[0]
        pkg_version=pkg_name_components[1]

        ## source pkgs don't have an architecture in them
        pkg_name = re.sub(r"x86_64-","",pkg_name)

        if(pkg_name in SKIP_SRC_PKGS):
            continue

        ## Switch names if necessary
        pkg_name = MUNGE_SRC_PKGS.get(pkg_name,pkg_name)

        tarball = "{}-{}.src.tar.gz".format(pkg_name,pkg_version)

        download_source(tarball)

    print("Zipping")
    check_output_maybe("zip -9 ../emacs-{}-{}deps-mingw-w64-src.zip *"
                       .format(EMACS_MAJOR_VERSION,DATE),
                       shell=True)

    os.chdir("..")


def clean():
    print("Cleaning")
    os.path.isdir("emacs-src") and shutil.rmtree("emacs-src")
    os.path.isdir("x86_64") and shutil.rmtree("x86_64")
    os.path.isfile("download.log") and os.remove("download.log")


if(os.environ["MSYSTEM"] != "MSYS"):
    print("Run this script in an MSYS-shell!")
    exit(1)


parser = argparse.ArgumentParser()
parser.add_argument("-s", help="snapshot build",
                    action="store_true")

parser.add_argument("-r", help="source code only",
                    action="store_true")

parser.add_argument("-c", help="clean only",
                    action="store_true")

parser.add_argument("-d", help="dry run",
                    action="store_true")

parser.add_argument("-l", help="list dependencies only",
                    action="store_true")

args = parser.parse_args()
do_all=not (args.c or args.r)



DRY_RUN=args.d

if( args.l ):
    print("List of dependencies")
    print( deps )
    exit(0)

if args.s:
    DATE="{}-".format(check_output(["date", "+%Y-%m-%d"]).decode("utf-8").strip())
else:
    DATE=""

if( do_all):
    gather_deps()

if( do_all or args.r ):
    deps=extract_deps()
    gather_source(deps)

if( args.c ):
    clean()
