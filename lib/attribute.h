/* ATTRIBUTE_* macros for using attributes in GCC and similar compilers

   Copyright 2020 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* Written by Paul Eggert.  */

/* Provide public ATTRIBUTE_* names for the private _GL_ATTRIBUTE_*
   macros used within Gnulib.  */

#ifndef _GL_ATTRIBUTE_H
#define _GL_ATTRIBUTE_H

/* C2X standard attributes have macro names that do not begin with
   'ATTRIBUTE_'.  */
#define DEPRECATED _GL_ATTRIBUTE_DEPRECATED
#define FALLTHROUGH _GL_ATTRIBUTE_FALLTHROUGH
#define MAYBE_UNUSED _GL_ATTRIBUTE_MAYBE_UNUSED
#define NODISCARD _GL_ATTRIBUTE_NODISCARD

/* Selected GCC attributes; see:
   https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html
   These names begin with 'ATTRIBUTE_' to avoid name clashes.  */
#define ATTRIBUTE_ALLOC_SIZE(args) _GL_ATTRIBUTE_ALLOC_SIZE (args)
#define ATTRIBUTE_ALWAYS_INLINE _GL_ATTRIBUTE_ALWAYS_INLINE
#define ATTRIBUTE_ARTIFICIAL _GL_ATTRIBUTE_ARTIFICIAL
#define ATTRIBUTE_COLD _GL_ATTRIBUTE_COLD
#define ATTRIBUTE_CONST _GL_ATTRIBUTE_CONST
#define ATTRIBUTE_DEPRECATED _GL_ATTRIBUTE_DEPRECATED
#define ATTRIBUTE_ERROR(msg) _GL_ATTRIBUTE_ERROR (msg)
#define ATTRIBUTE_EXTERNALLY_VISIBLE _GL_ATTRIBUTE_EXTERNALLY_VISIBLE
#define ATTRIBUTE_FORMAT(spec) _GL_ATTRIBUTE_FORMAT (spec)
#define ATTRIBUTE_LEAF _GL_ATTRIBUTE_LEAF
#define ATTRIBUTE_MAY_ALIAS _GL_ATTRIBUTE_MAY_ALIAS
#define ATTRIBUTE_MALLOC _GL_ATTRIBUTE_MALLOC
#define ATTRIBUTE_NOINLINE _GL_ATTRIBUTE_NOINLINE
#define ATTRIBUTE_NONNULL(args) _GL_ATTRIBUTE_NONNULL (args)
#define ATTRIBUTE_NONSTRING _GL_ATTRIBUTE_NONSTRING
#define ATTRIBUTE_NOTHROW _GL_ATTRIBUTE_NOTHROW
#define ATTRIBUTE_PACKED _GL_ATTRIBUTE_PACKED
#define ATTRIBUTE_PURE _GL_ATTRIBUTE_PURE
#define ATTRIBUTE_RETURNS_NONNULL _GL_ATTRIBUTE_RETURNS_NONNULL
#define ATTRIBUTE_SENTINEL(pos) _GL_ATTRIBUTE_SENTINEL (pos)
#define ATTRIBUTE_WARNING(msg) _GL_ATTRIBUTE_WARNING (msg)

#endif /* _GL_ATTRIBUTE_H */
