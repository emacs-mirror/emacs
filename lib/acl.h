/* acl.c - access control lists

   Copyright (C) 2002, 2008-2025 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.

   Written by Paul Eggert.  */

#ifndef _GL_ACL_H
#define _GL_ACL_H 1

/* This file uses _GL_ATTRIBUTE_CONST, _GL_ATTRIBUTE_DEPRECATED.  */
#if !_GL_CONFIG_H_INCLUDED
 #error "Please include config.h first."
#endif

#include <sys/types.h>
#include <sys/stat.h>

#ifdef __cplusplus
extern "C" {
#endif

/* file_has_acl flags guaranteed to not collide with any <dirent.h>
   DT_* or _GL_DT_* value.  */
enum
  {
    /* Get scontext information as well.  */
    ACL_GET_SCONTEXT = 0x10000,

    /* Follow symlinks.  */
    ACL_SYMLINK_FOLLOW = 0x20000,
  };

/* Information about an ACL.  */
struct aclinfo
{
  /* If 'size' is nonnegative, a buffer holding the concatenation
     of extended attribute names, each terminated by NUL
     (either u.__gl_acl_ch, or heap-allocated).  */
  char *buf;

  /* The number of useful bytes at the start of buf, counting trailing NULs.
     If negative, there was an error in getting the ACL info,
     and u.err is the corresponding errno.  */
  ssize_t size;

  /* Security context string.  Do not modify its contents.  */
  char *scontext;
  /* Security context errno value.  It is zero if there was no
     error getting the security context.  When nonzero, scontext is "?".  */
  int scontext_err;

  union
  {
    /* An errno value, when there was an error getting the ACL info.  */
    int err;

    /* A small array of char, big enough for most listxattr results.
       The size is somewhat arbitrary; it equals the max length of a
       trivial NFSv4 ACL (a size used by file-has-acl.c in 2023-2024
       but no longer relevant now), and a different value might be
       better once experience is gained.  For internal use only.  */
    char __gl_acl_ch[152];
  } u;
};

bool acl_errno_valid (int) _GL_ATTRIBUTE_CONST;
int file_has_acl (char const *, struct stat const *);
int file_has_aclinfo (char const *restrict, struct aclinfo *restrict, int);
int fdfile_has_aclinfo (int, char const *restrict,
                        struct aclinfo *restrict, int);

#if HAVE_LINUX_XATTR_H && HAVE_LISTXATTR
bool aclinfo_has_xattr (struct aclinfo const *, char const *)
  _GL_ATTRIBUTE_PURE;
void aclinfo_free (struct aclinfo *);
#else
# define aclinfo_has_xattr(ai, xattr) false
# define aclinfo_free(ai) ((void) 0)
#endif
#if (HAVE_LINUX_XATTR_H && HAVE_LISTXATTR \
     && (HAVE_SMACK || USE_SELINUX_SELINUX_H))
void aclinfo_scontext_free (char *);
#else
# define aclinfo_scontext_free(s) ((void) 0)
#endif

int qset_acl (char const *, int, mode_t);
int xset_acl (char const *, int, mode_t);
/* Old name of xset_acl.  */
_GL_ATTRIBUTE_DEPRECATED int set_acl (char const *, int, mode_t);

int qcopy_acl (char const *, int, char const *, int, mode_t);
int xcopy_acl (char const *, int, char const *, int, mode_t);
/* Old name of xcopy_acl.  */
_GL_ATTRIBUTE_DEPRECATED int copy_acl (char const *, int, char const *, int,
                                       mode_t);

int chmod_or_fchmod (char const *, int, mode_t);


#ifdef __cplusplus
}
#endif

#endif
