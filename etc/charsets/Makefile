# Makefile for charset maps (for maintainers only)
# Copyright (C) 2001, 2002
#   National Institute of Advanced Industrial Science and Technology (AIST)
#   Registration Number H13PRO009

# This file is part of GNU Emacs.

# GNU Emacs is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to
# the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

SED = sed

MAPS =	8859-2.map \
	8859-3.map \
	8859-4.map \
	8859-5.map \
	8859-6.map \
	8859-7.map \
	8859-8.map \
	8859-9.map \
	8859-10.map \
	8859-13.map \
	8859-14.map \
	8859-15.map \
	8859-16.map \
	gb2312-1980.map \
	ksc5601-1987.map \
	jisx0201.map \
	jisx0208-1990.map \
	jisx0212-1990.map \
	cns11643-1.map \
	cns11643-2.map \
	big5.map \
	viscii.map \
	koi8-r.map \
	ibm866.map \
	mac-roman.map \

all: ${MAPS}

GLIBCDIR=../../../src/glibc-2.2.4/localedata/charmaps

8859-%.map: ${GLIBCDIR}/ISO-8859-%
	${SED} -n \
	  -e '/^<U[^x]*x[89A-Fa-f]/s/<U\(....\)[^x]*\(...\).*/0\2 0x\1/p' \
	  < $< > $@

gb2312-1980.map: ${GLIBCDIR}/GB2312
	${SED} -n -e \
	  '/x[a-f]..x[a-f]/s/<U\(....\)[^x]*\(...\).\(...\).*/0x\2\3 0x\1/p' \
	  < $< \
	  | ${SED} -e 's/xa/2/g' -e 's/xb/3/g' -e 's/xc/4/g' \
		   -e 's/xd/5/g' -e 's/xe/6/g' -e 's/xf/7/g' \
	  > $@

ksc5601-1987.map: ${GLIBCDIR}/EUC-KR
	${SED} -n -e \
	  '/x[a-f]..x[a-f]/s/<U\(....\)[^x]*\(...\).\(...\).*/0x\2\3 0x\1/p' \
	  < $< \
	  | ${SED} -e 's/xa/2/g' -e 's/xb/3/g' -e 's/xc/4/g' \
		   -e 's/xd/5/g' -e 's/xe/6/g' -e 's/xf/7/g' \
	  > $@

jisx0201.map: ${GLIBCDIR}/JIS_C6220-1969-RO ${GLIBCDIR}/EUC-JP
	${SED} -n -e '/^<U/s/<U\(....\)[^x]*\(...\).*/0\2 0x\1/p' \
	  < ${GLIBCDIR}/JIS_C6220-1969-RO > $@
	${SED} -n -e '/x8e/s/<U\(....\)[^x]*....\(...\).*/0\2 0x\1/p' \
	  < ${GLIBCDIR}/EUC-JP >> $@

# We map 0x2140 to U+FF3C (FULL WIDTH REVERSE SOLIDUS), not to U+005C
# (REVERSE SOLIDUS).
jisx0208-1990.map: ${GLIBCDIR}/EUC-JP
	${SED} -n -e \
	  '/^[^x]*x[a-f]..x[a-f]/s,<U\(....\)[^/]*\(........\).*,0x\2 0x\1,p' \
	  < $< \
	  | ${SED} -e 's,/xa,2,g' -e 's,/xb,3,g' -e 's,/xc,4,g' \
		   -e 's,/xd,5,g' -e 's,/xe,6,g' -e 's,/xf,7,g' \
	  > $@

jisx0212-1990.map: ${GLIBCDIR}/EUC-JP
	${SED} -n -e \
	  '/x8f/s,<U\(....\)[^/]*/x8f\(........\).*,0x\2 0x\1,p' \
	  < $< \
	  | ${SED} -e 's,/xa,2,g' -e 's,/xb,3,g' -e 's,/xc,4,g' \
		   -e 's,/xd,5,g' -e 's,/xe,6,g' -e 's,/xf,7,g' \
	  > $@

cns11643-1.map: ${GLIBCDIR}/EUC-TW
	${SED} -n -e \
	  '/^[^x]*x[a-f]..x[a-f]/s,<U\(....\)[^/]*\(........\).*,0x\2 0x\1,p' \
	  < $< \
	  | ${SED} -e 's,/xa,2,g' -e 's,/xb,3,g' -e 's,/xc,4,g' \
		   -e 's,/xd,5,g' -e 's,/xe,6,g' -e 's,/xf,7,g' \
	  > $@

cns11643-2.map: ${GLIBCDIR}/EUC-TW
	${SED} -n -e \
	  '/x8e.xa2/s,<U\(....\)[^/]*/x8e/xa2\(........\).*,0x\2 0x\1,p' \
	  < $< \
	  | ${SED} -e 's,/xa,2,g' -e 's,/xb,3,g' -e 's,/xc,4,g' \
		   -e 's,/xd,5,g' -e 's,/xe,6,g' -e 's,/xf,7,g' \
	  > $@

big5.map: ${GLIBCDIR}/BIG5
	${SED} -n -e \
	  '/^%I[^x]*x[a-f]/s,.*<U\(....\)[^x]*\(...\)..\(..\).*,0\2\3 0x\1,p' \
	  < $< > $@
	${SED} -n -e \
	  '/^<[^x]*x[a-f]/s,<U\(....\)[^x]*\(...\)..\(..\).*,0\2\3 0x\1,p' \
	  < $< >> $@

big5-extra.map: ${GLIBCDIR}/BIG5

viscii.map: ${GLIBCDIR}/VISCII
	${SED} -n -e \
	  '/^<U/s/<U\(....\)[^x]*x\(..\).*/0x\2 0x\1/p' < $< > $@

koi8-r.map: ${GLIBCDIR}/KOI8-R
	${SED} -n -e \
	  '/^<U/s/<U\(....\)[^x]*x\(..\).*/0x\2 0x\1/p' < $< > $@

ibm866.map: ${GLIBCDIR}/IBM866
	${SED} -n -e \
	  '/^<U/s/<U\(....\)[^x]*x\(..\).*/0x\2 0x\1/p' < $< > $@

mac-roman.map: ${GLIBCDIR}/MACINTOSH
	${SED} -n -e \
	  '/^<U[^x]*x[89A-Fa-f]/s/<U\(....\)[^x]*x\(..\).*/0x\2 0x\1/p' \
	  < $< > $@

maintainer-clean:
	rm -rf ${MAPS}
