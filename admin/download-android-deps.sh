#!/bin/sh

# This script downloads and extracts dependencies from
# https://sourceforge.net/projects/android-ports-for-gnu-emacs/ and
# Android source code repositories for the convenience of packagers.
#
# See https://forum.f-droid.org/t/emacs-packaging/30424/12 for
# context.

set -e

bits_64=no

if [ "$1" == "64" ]; then
    bits_64=yes
fi

ndk_path=

mirror=${2-https://master.dl.sourceforge.net/project/android-ports-for-gnu-emacs}

download_tarball ()
{
    echo "Downloading $mirror/$1"
    curl -OL $mirror/$1
    hash=`shasum $1 | cut -d " " -f 1`
    if test "$hash" != "$3"; then
	echo "Hash mismatch detected with archive $1:\
 expected $3, but received $hash."
	exit 1
    fi
    tar xfz $1 $2

    if test ! -d "$2"; then
	echo "\`$1' was extracted but without producing the directory \`$2'." >&2
	exit 1
    fi

    ndk_path="$ndk_path $PWD/$2"
}

# 31e74492a49cde9e420e2c71f6d6de0f2b9d6fd3  cairo-1.16.0-emacs.tar.gz
# 98de96764c64f31a6df23adec65425e1813f571b  gdk-pixbuf-2.22.1-emacs.tar.gz
# a407c568961d729bb2d0175a34e0d4ed4a269978  giflib-5.2.1-emacs.tar.gz
# e63bc0a628cec770a3a5124c00873d4a44c8c1ac  glib-2.33.14-emacs.tar.gz
# 66518ea7905cdb42a22b6f003551ca3f73d249f0  gmp-6.3.0-emacs.tar.gz
# 4e92fb479c96f1c0e9eb3577262f1ebe609a752e  gnutls-3.8.5-emacs-armv7a.tar.gz
# 0491c778a81e42490789db9b30a9b7c69b650618  gnutls-3.8.5-emacs.tar.gz
# 22dc71d503ab2eb263dc8411de9da1db144520f5  harfbuzz-7.1.0-emacs.tar.gz
# 590f6c6c06dfe19a8190f526b5b76de34b999a07  libcroco-0.6.13-emacs.tar.gz
# 23508b52a8d9fc3f3750c0187e4141b0d06f79c9  libffi-3.4.5-emacs.tar.gz
# b9398f30e882b140ec790a761663e829ba9bce31  libiconv-1.17-emacs.tar.gz
# 5091fe6f8b368ea2dcc92e2fd003add7bbc63a0a  libjpeg-turbo-3.0.2-emacs.tar.gz
# 85e10d1d289d7fd4621cb9648533a0cc69f352a8  libpng-1.6.41-emacs.tar.gz
# 8361966e19fe25ae987b08799f1442393ae6366b  libselinux-3.6-emacs.tar.gz
# fdc827211075d9b70a8ba6ceffa02eb48d6741e9  libtasn1-4.19.0-emacs.tar.gz
# 73c3174f7b22d3cfedad9eb58db916199575eea4  libxml2-2.12.4-emacs.tar.gz
# 94882f5494e5435f885d64b57e3e7b3ee5345a3b  nettle-3.8-emacs.tar.gz
# b4680fcfec66220a09618489584c8f3270cc16fd  p11-kit-0.24.1-emacs.tar.gz
# 89bb17b09d4381835b32291a65107dc33281e88b  pango-1.38.1-emacs.tar.gz
# 1c8f3b0cbad474da0ab09018c4ecf2119ac4a52d  pixman-0.38.4-emacs.tar.gz
# b687c8439d51634d921674dd009645e24873ca36  rsvg-2.40.21-emacs.tar.gz
# eda251614598aacb06f5984a0a280833de456b29  tiff-4.5.1-emacs.tar.gz
# 9d032de89c874354c22d304f7e968f4ca6de8c0a  tree-sitter-0.26.3-emacs.tar.gz

download_tarball "giflib-5.2.1-emacs.tar.gz" "giflib-5.2.1" \
		 "a407c568961d729bb2d0175a34e0d4ed4a269978"
download_tarball "libjpeg-turbo-3.0.2-emacs.tar.gz" "libjpeg-turbo-3.0.2" \
		 "5091fe6f8b368ea2dcc92e2fd003add7bbc63a0a"
download_tarball "libpng-1.6.41-emacs.tar.gz" "libpng-1.6.41" \
		 "85e10d1d289d7fd4621cb9648533a0cc69f352a8"
download_tarball "libxml2-2.12.4-emacs.tar.gz" "libxml2-2.12.4" \
		 "73c3174f7b22d3cfedad9eb58db916199575eea4"
download_tarball "gmp-6.3.0-emacs.tar.gz" "gmp-6.3.0" \
		 "66518ea7905cdb42a22b6f003551ca3f73d249f0"
download_tarball "nettle-3.8-emacs.tar.gz" "nettle-3.8" \
		 "94882f5494e5435f885d64b57e3e7b3ee5345a3b"

if test "$bits_64" = "yes"; then
    download_tarball "gnutls-3.8.5-emacs.tar.gz" "gnutls-3.8.5" \
		     "0491c778a81e42490789db9b30a9b7c69b650618"
else
    download_tarball "gnutls-3.8.5-emacs-armv7a.tar.gz" "gnutls-3.8.5-armv7a" \
		     "4e92fb479c96f1c0e9eb3577262f1ebe609a752e"
fi

download_tarball "p11-kit-0.24.1-emacs.tar.gz" "p11-kit-0.24.1" \
		 "b4680fcfec66220a09618489584c8f3270cc16fd"
download_tarball "libtasn1-4.19.0-emacs.tar.gz" "libtasn1-4.19.0" \
		 "fdc827211075d9b70a8ba6ceffa02eb48d6741e9"
download_tarball "libselinux-3.6-emacs.tar.gz" "libselinux-3.6" \
		 "8361966e19fe25ae987b08799f1442393ae6366b"
download_tarball "tree-sitter-0.26.3-emacs.tar.gz" "tree-sitter-0.26.3" \
		 "9d032de89c874354c22d304f7e968f4ca6de8c0a"
download_tarball "harfbuzz-7.1.0-emacs.tar.gz" "harfbuzz-7.1.0" \
		 "22dc71d503ab2eb263dc8411de9da1db144520f5"
download_tarball "tiff-4.5.1-emacs.tar.gz" "tiff-4.5.1" \
		 "eda251614598aacb06f5984a0a280833de456b29"
download_tarball "gdk-pixbuf-2.22.1-emacs.tar.gz" "gdk-pixbuf-2.22.1" \
		 "98de96764c64f31a6df23adec65425e1813f571b"
download_tarball "glib-2.33.14-emacs.tar.gz" "glib-2.33.14" \
		 "e63bc0a628cec770a3a5124c00873d4a44c8c1ac"
download_tarball "libcroco-0.6.13-emacs.tar.gz" "libcroco-0.6.13" \
		 "590f6c6c06dfe19a8190f526b5b76de34b999a07"
download_tarball "rsvg-2.40.21-emacs.tar.gz" "librsvg-2.40.21" \
		 "b687c8439d51634d921674dd009645e24873ca36"
download_tarball "cairo-1.16.0-emacs.tar.gz" "cairo-1.16.0" \
		 "31e74492a49cde9e420e2c71f6d6de0f2b9d6fd3"
download_tarball "libiconv-1.17-emacs.tar.gz" "libiconv-1.17" \
		 "b9398f30e882b140ec790a761663e829ba9bce31"
download_tarball "pango-1.38.1-emacs.tar.gz" "pango-1.38.1" \
		 "89bb17b09d4381835b32291a65107dc33281e88b"
download_tarball "pixman-0.38.4-emacs.tar.gz" "pixman-0.38.4" \
		 "1c8f3b0cbad474da0ab09018c4ecf2119ac4a52d"
download_tarball "libffi-3.4.5-emacs.tar.gz" "libffi-3.4.5" \
		 "23508b52a8d9fc3f3750c0187e4141b0d06f79c9"

rm -rf sqlite
git clone https://android.googlesource.com/platform/external/sqlite -b android-7.1.2_r39
ndk_path="$ndk_path $PWD/sqlite/dist"
rm -rf pcre
git clone https://android.googlesource.com/platform/external/pcre -b android-7.1.2_r39
ndk_path="$ndk_path $PWD/pcre"
rm -rf libwebp
git clone https://chromium.googlesource.com/webm/libwebp -b v1.5.0
ndk_path="$ndk_path $PWD/libwebp"

sed -e 's/NEON := c.neon/NEON := c/g'				\
    -e '/WEBP_CFLAGS *+=/s/-DHAVE_CPU_FEATURES_H//g'		\
    -e 's/USE_CPUFEATURES *.*=.*$/USE_CPUFEATURES := no/g'	\
    -i libwebp/Android.mk

echo $ndk_path > search-path.txt
