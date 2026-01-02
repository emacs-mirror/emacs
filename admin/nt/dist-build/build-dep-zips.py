#!/usr/bin/python3

## Copyright (C) 2017-2026 Free Software Foundation, Inc.

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
import subprocess

from subprocess import check_output

## Constants
EMACS_MAJOR_VERSION= os.getenv('EMACS_MAJOR_VERSION') or "30"

# Base URI for the package sources mapped in PKG_REQ
SRC_REPO="https://repo.msys2.org/mingw/sources"

# Map items in `dynamic-library-alist' to source packages
PKG_REQ='''mingw-w64-x86_64-giflib
mingw-w64-x86_64-gnutls
mingw-w64-x86_64-harfbuzz
mingw-w64-x86_64-jansson
mingw-w64-x86_64-lcms2
mingw-w64-x86_64-libjpeg-turbo
mingw-w64-x86_64-libpng
mingw-w64-x86_64-librsvg
mingw-w64-x86_64-libwebp
mingw-w64-x86_64-libtiff
mingw-w64-x86_64-libxml2
mingw-w64-x86_64-gmp
mingw-w64-x86_64-xpm-nox
mingw-w64-x86_64-tree-sitter
mingw-w64-x86_64-sqlite3'''.split()

# Emacs style path to dependency DLLs on build system
DLL_SRC="c:/msys64/mingw64/bin"

# libraries we never include
DLL_SKIP=["libgccjit-0.dll"]

# Report first existing file for entries in dynamic-library-alist
# ELISP_PROG="""
# (message "%s" (mapconcat 'identity (remove nil
# 	(mapcar (lambda(lib)
# 		  (seq-find
# 		   (lambda(file)
# 		     (file-exists-p
# 		      (file-name-concat "{}"
# 					file)))
# 		   (cdr lib)))
# 		dynamic-library-alist)
# 	) "\\n"))
# """.format(DLL_SRC)

## Options
DRY_RUN=False
# NEW_EMACS="bin/emacs.exe"

def check_output_maybe(*args,**kwargs):
    if(DRY_RUN):
        print("Calling: {}{}".format(args,kwargs))
    else:
        return check_output(*args,**kwargs)

####################
## DLL Capture

# entry point
def gather_deps():

    os.mkdir("x86_64")
    os.chdir("x86_64")

    #full=full_dll_dependency(init_deps())
    #filtered=filter(lambda x: x not in DLL_SKIP, full)
    #print("full:",full.len(), " filtered:",filtered.len())
    #exit

    for dep in full_dll_dependency(init_deps()):
        if dep not in DLL_SKIP:
            if args.l != True:
                print("Adding dep", dep)
            check_output_maybe(["cp /mingw64/bin/{} .".format(dep)], shell=True)
        else:
            if args.l != True:
                print("Skipping dep", dep)

    zipfile="../emacs-{}-{}deps.zip".format(EMACS_MAJOR_VERSION, DATE)
    tmpfile="{}.tmp".format(zipfile)
    print("Zipping deps in", os.getcwd(), "as", tmpfile)
    check_output_maybe("zip -9vr {} *.dll".format(tmpfile), shell=True)
    if os.path.isfile(zipfile):
        os.remove(zipfile)
    os.rename(tmpfile, zipfile)
    print("Deps updated in", os.getcwd(), "as", zipfile)
    os.chdir("../")

# Return dependencies listed in Emacs
def init_deps():
    return '''libXpm-nox4.dll
libpng16-16.dll
libjpeg-8.dll
libgif-7.dll
librsvg-2-2.dll
libwebp-7.dll
libwebpdemux-2.dll
libsqlite3-0.dll
libgdk_pixbuf-2.0-0.dll
libglib-2.0-0.dll
libgio-2.0-0.dll
libgobject-2.0-0.dll
libgnutls-30.dll
libxml2-2.dll
zlib1.dll
liblcms2-2.dll
libgccjit-0.dll
libtree-sitter.dll'''.split()
    # job_args=[NEW_EMACS, "--batch", "--eval", ELISP_PROG]
    # #print("args: ", job_args)
    # return subprocess.check_output(job_args, stderr=subprocess.STDOUT
    #                                ).decode('utf-8').splitlines()

# Return all second order dependencies
def full_dll_dependency(dlls):
    deps = [dll_dependency(dep) for dep in dlls]
    return set(sum(deps, []) + dlls)

#xs = filter(lambda x: x.attribute == value, xs)

# Dependencies for a given DLL
def dll_dependency(dll):
    output = check_output(["/mingw64/bin/ntldd", "--recursive",
                           "/mingw64/bin/{}".format(dll)]
                          ).decode("utf-8")
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
        if len(splt) > 2 and "mingw64" in splt[2]:
            rtn.append(splt[0])

    return rtn

#### Source Capture

## Packages to fiddle with
## Source for gcc-libs is part of gcc
SKIP_SRC_PKGS=["mingw-w64-gcc-libs"]
SKIP_DEP_PKGS=["mingw-w64-glib2", "mingw-w64-ca-certificates-20211016-3"]
MUNGE_SRC_PKGS={
    "mingw-w64-libwinpthread-git":"mingw-w64-winpthreads-git",
    "mingw-w64-gettext-runtime":"mingw-w64-gettext"
}
MUNGE_DEP_PKGS={
    "mingw-w64-x86_64-libwinpthread":"mingw-w64-x86_64-libwinpthread-git",
    "mingw-w64-x86_64-libtre": "mingw-w64-x86_64-libtre-git",
}
SRC_EXT={
    "mingw-w64-freetype": ".src.tar.zst",
    "mingw-w64-fribidi": ".src.tar.zst",
    "mingw-w64-glib2": ".src.tar.zst",
    "mingw-w64-harfbuzz": ".src.tar.zst",
    "mingw-w64-libunistring": ".src.tar.zst",
    "mingw-w64-winpthreads-git": ".src.tar.zst",
    "mingw-w64-ca-certificates": ".src.tar.zst",
    "mingw-w64-libxml2": ".src.tar.zst",
    "mingw-w64-ncurses": ".src.tar.zst",
    "mingw-w64-openssl": ".src.tar.zst",
    "mingw-w64-pango": ".src.tar.zst",
    "mingw-w64-python": ".src.tar.zst",
    "mingw-w64-sqlite3": ".src.tar.zst",
    "mingw-w64-xpm-nox": ".src.tar.zst",
    "mingw-w64-xz": ".src.tar.zst",
    "mingw-w64-bzip2": ".src.tar.zst",
    "mingw-w64-cairo": ".src.tar.zst",
    "mingw-w64-expat": ".src.tar.zst",
    "mingw-w64-fontconfig":  ".src.tar.zst",
    "mingw-w64-gdk-pixbuf2":  ".src.tar.zst",
    "mingw-w64-giflib":  ".src.tar.zst",
    "mingw-w64-gmp":  ".src.tar.zst",
    "mingw-w64-gnutls":  ".src.tar.zst",
    "mingw-w64-graphite2":  ".src.tar.zst",
    "mingw-w64-jbigkit":  ".src.tar.zst",
    "mingw-w64-lcms2":  ".src.tar.zst",
    "mingw-w64-lerc":  ".src.tar.zst",
    "mingw-w64-libdatrie":  ".src.tar.zst",
    "mingw-w64-libffi":  ".src.tar.zst",
    "mingw-w64-libiconv":  ".src.tar.zst",
    "mingw-w64-libiconv":  ".src.tar.zst",
    "mingw-w64-libpng":  ".src.tar.zst",
    "mingw-w64-librsvg": ".src.tar.zst",
    "mingw-w64-libsystre": ".src.tar.zst",
    "mingw-w64-libtasn": ".src.tar.zst",
    "mingw-w64-libthai": ".src.tar.zst",
    "mingw-w64-libtiff": ".src.tar.zst",
    "mingw-w64-libtre-git": ".src.tar.zst",
    "mingw-w64-libwebp": ".src.tar.zst",
    "mingw-w64-mpdecimal": ".src.tar.zst",
    "mingw-w64-nettle": ".src.tar.zst",
    "mingw-w64-p11-kit": ".src.tar.zst",
    "mingw-w64-pcre": ".src.tar.zst",
    "mingw-w64-pixman": ".src.tar.zst",
    "mingw-w64-python-packaging": ".src.tar.zst",
    "mingw-w64-readline": ".src.tar.zst",
    "mingw-w64-tcl": ".src.tar.zst",
    "mingw-w64-termcap": ".src.tar.zst",
    "mingw-w64-tk": ".src.tar.zst",
    "mingw-w64-tree-sitter": ".src.tar.zst",
    "mingw-w64-tzdata": ".src.tar.zst",
    "mingw-w64-wineditline": ".src.tar.zst",
    "mingw-w64-zlib": ".src.tar.zst",
    "mingw-w64-zstd": ".src.tar.zst",
    "mingw-w64-brotli": ".src.tar.zst",
    "mingw-w64-gettext": ".src.tar.zst",
    "mingw-w64-libdeflate": ".src.tar.zst",
    "mingw-w64-libidn2": ".src.tar.zst",
    "mingw-w64-libjpeg-turbo": ".src.tar.zst",
    "mingw-w64-libtasn1": ".src.tar.zst",
    "mingw-w64-pcre2": ".src.tar.zst",
}

## Currently no packages seem to require this!
ARCH_PKGS=[]

def immediate_deps(pkg):
    package_info = check_output(["pacman", "-Si", pkg]).decode("utf-8").split("\n")

    ## Extract the "Depends On" line
    depends_on = [x for x in package_info if x.startswith("Depends On")][0]
    ## Remove "Depends On" prefix
    dependencies = depends_on.split(":")[1]

    ## Split into dependencies
    dependencies = dependencies.strip().split(" ")

    ## Remove > signs TODO can we get any other punctuation here?
    dependencies = [d.split(">")[0] for d in dependencies if d]
    dependencies = [d for d in dependencies if not d == "None"]

    dependencies = [MUNGE_DEP_PKGS.get(d, d) for d in dependencies]
    return dependencies


## Extract all the msys2 packages that are dependencies of our direct dependencies
def extract_deps():

    print( "Extracting deps" )

    # Get a list of all dependencies needed for packages mentioned above.
    pkgs = PKG_REQ[:]
    n = 0
    while n < len(pkgs):
        subdeps = immediate_deps(pkgs[n])
        for p in subdeps:
            if not (p in pkgs or p in SKIP_DEP_PKGS):
                pkgs.append(p)
        n = n + 1

    return sorted(pkgs)


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

        ## src archive is usually a .tar.gz
        if pkg_name in SRC_EXT.keys():
            src_ext = SRC_EXT[pkg_name]
        else:
            src_ext = ".src.tar.gz"

        tarball = "{}-{}{}".format(pkg_name,pkg_version,src_ext)

        download_source(tarball)

    srczip="../emacs-{}-{}deps-mingw-w64-src.zip".format(EMACS_MAJOR_VERSION,DATE)
    tmpzip="{}.tmp".format(srczip)
    print("Zipping Dsrc in", os.getcwd(), "as", tmpzip)
    check_output_maybe("zip -9 {} *".format(tmpzip), shell=True)
    if os.path.isfile(srczip):
        os.remove(srczip)
    os.rename(tmpzip, srczip)
    print("Dsrc updated in", os.getcwd(), "as", srczip)

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

#parser.add_argument("emacs", help="emacs executable")

parser.add_argument("-s", help="snapshot build",
                    action="store_true")

parser.add_argument("-r", help="source code only",
                    action="store_true")

parser.add_argument("-c", help="clean only",
                    action="store_true")

parser.add_argument("-d", help="dry run",
                    action="store_true")

parser.add_argument("-l", help="list dependencies",
                    action="store_true")

parser.add_argument("-e", help="extract direct dependencies",
                    action="store_true")

args = parser.parse_args()
do_all=not (args.c or args.r)

#NEW_EMACS=args.emacs

DRY_RUN=args.d

if( args.e ):
    print("\n".join(init_deps()))

if( args.l ):
    print("List of dependencies:")
    print(full_dll_dependency(init_deps()))
    print("List of source packages:")
    print( extract_deps() )

if( args.e or args.l ):
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
