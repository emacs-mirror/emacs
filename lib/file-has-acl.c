/* Test whether a file has a nontrivial ACL.  -*- coding: utf-8 -*-

   Copyright (C) 2002-2003, 2005-2025 Free Software Foundation, Inc.

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

   Written by Paul Eggert, Andreas Grünbacher, and Bruno Haible.  */

#include <config.h>

/* Without this pragma, gcc 4.7.0 20120126 may suggest that the
   file_has_acl function might be candidate for attribute 'const'  */
#if _GL_GNUC_PREREQ (4, 6)
# pragma GCC diagnostic ignored "-Wsuggest-attribute=const"
#endif

#include "acl.h"

#include <dirent.h>
#include <limits.h>

#include "acl-internal.h"
#include "attribute.h"
#include "minmax.h"

/* Check the assumption that UCHAR_MAX < INT_MAX.  */
static_assert (ACL_SYMLINK_FOLLOW & ~ (unsigned char) -1);

static char const UNKNOWN_SECURITY_CONTEXT[] = "?";

#if HAVE_LINUX_XATTR_H && HAVE_LISTXATTR
# define USE_LINUX_XATTR true
#else
# define USE_LINUX_XATTR false
#endif

#if USE_LINUX_XATTR
# if USE_SELINUX_SELINUX_H
#  include <selinux/selinux.h>
# endif
# include <stdckdint.h>
# include <string.h>
# include <arpa/inet.h>
# include <sys/xattr.h>
# include <linux/xattr.h>
# ifndef XATTR_NAME_SMACK
#  define XATTR_NAME_SMACK "security.SMACK64"
# endif
# ifndef XATTR_NAME_SELINUX
#  define XATTR_NAME_SELINUX "security.selinux"
# endif
# ifndef XATTR_NAME_NFSV4_ACL
#  define XATTR_NAME_NFSV4_ACL "system.nfs4_acl"
# endif
# ifndef XATTR_NAME_POSIX_ACL_ACCESS
#  define XATTR_NAME_POSIX_ACL_ACCESS "system.posix_acl_access"
# endif
# ifndef XATTR_NAME_POSIX_ACL_DEFAULT
#  define XATTR_NAME_POSIX_ACL_DEFAULT "system.posix_acl_default"
# endif

# ifdef HAVE_SMACK
#  include <sys/smack.h>
# else
static char const *
smack_smackfs_path (void)
{
  return NULL;
}
static ssize_t
smack_new_label_from_path (MAYBE_UNUSED const char *path,
                           MAYBE_UNUSED const char *xattr,
                           MAYBE_UNUSED int follow, MAYBE_UNUSED char **label)
{
  return -1;
}
static ssize_t
smack_new_label_from_file (MAYBE_UNUSED int fd,
                           MAYBE_UNUSED const char *xattr,
                           MAYBE_UNUSED char **label)
{
  return -1;
}
# endif
static bool
is_smack_enabled (void)
{
 return !!smack_smackfs_path ();
}

enum {
  /* ACE4_ACCESS_ALLOWED_ACE_TYPE = 0x00000000, */
  ACE4_ACCESS_DENIED_ACE_TYPE  = 0x00000001,
  ACE4_IDENTIFIER_GROUP        = 0x00000040
};

/* AI indicates XATTR may be present but wasn't accessible.
   This is the case when [l]listxattr failed with E2BIG,
   or is not supported (!acl_errno_valid()), or failed with EACCES
   which in Linux kernel 6.12 NFS can mean merely that we lack read access.
*/

static bool
aclinfo_may_indicate_xattr (struct aclinfo const *ai)
{
  return ai->size < 0 && (!acl_errno_valid (ai->u.err)
                          || ai->u.err == EACCES || ai->u.err == E2BIG);
}

/* Does NAME have XATTR?  */

static bool
has_xattr (char const *xattr, struct aclinfo const *ai,
           int fd, char const *restrict name, int flags)
{
  if (ai && aclinfo_has_xattr (ai, xattr))
    return true;
  else if (!ai || aclinfo_may_indicate_xattr (ai))
    {
      int ret = (fd < 0
                 ? ((flags & ACL_SYMLINK_FOLLOW ? getxattr : lgetxattr)
                    (name, xattr, NULL, 0))
                 : fgetxattr (fd, xattr, NULL, 0));
      if (0 <= ret || (errno == ERANGE || errno == E2BIG))
        return true;
    }
  return false;
}

/* Does AI's xattr set contain XATTR?  */

bool
aclinfo_has_xattr (struct aclinfo const *ai, char const *xattr)
{
  if (0 < ai->size)
    {
      char const *blim = ai->buf + ai->size;
      for (char const *b = ai->buf; b < blim; b += strlen (b) + 1)
        for (char const *a = xattr; *a == *b; a++, b++)
          if (!*a)
            return true;
    }
  return false;
}

/* Get attributes of the file FD aka NAME into AI, if USE_ACL.
   Ignore FD if it is negative.
   If FLAGS & ACL_GET_SCONTEXT, also get security context.
   If FLAGS & ACL_SYMLINK_FOLLOW, follow symbolic links.  */
static void
get_aclinfo (int fd, char const *name, struct aclinfo *ai, int flags)
{
  int scontext_err = ENOTSUP;
  ai->buf = ai->u.__gl_acl_ch;
  ssize_t acl_alloc = sizeof ai->u.__gl_acl_ch;

  if (! (USE_ACL || flags & ACL_GET_SCONTEXT))
    ai->size = 0;
  else
    {
      ssize_t (*lsxattr) (char const *, char *, size_t)
        = (flags & ACL_SYMLINK_FOLLOW ? listxattr : llistxattr);
      while (true)
        {
          ai->size = (fd < 0
                      ? lsxattr (name, ai->buf, acl_alloc)
                      : flistxattr (fd, ai->buf, acl_alloc));
          if (0 < ai->size)
            break;
          ai->u.err = ai->size < 0 ? errno : 0;
          if (! (ai->size < 0 && ai->u.err == ERANGE && acl_alloc < SSIZE_MAX))
            break;

          /* The buffer was too small.  Find how large it should have been.  */
          ssize_t size = (fd < 0
                          ? lsxattr (name, NULL, 0)
                          : flistxattr (fd, NULL, 0));
          if (size <= 0)
            {
              ai->size = size;
              ai->u.err = size < 0 ? errno : 0;
              break;
            }

          /* Grow allocation to at least 'size'.  Grow it by a nontrivial
             amount, to defend against denial of service by an adversary
             that fiddles with ACLs.  */
          if (ai->buf != ai->u.__gl_acl_ch)
            {
              free (ai->buf);
              ai->buf = ai->u.__gl_acl_ch;
            }
          if (ckd_add (&acl_alloc, acl_alloc, acl_alloc >> 1))
            acl_alloc = SSIZE_MAX;
          if (acl_alloc < size)
            acl_alloc = size;
          if (SIZE_MAX < acl_alloc)
            {
              ai->u.err = ENOMEM;
              break;
            }
          char *newbuf = malloc (acl_alloc);
          if (!newbuf)
            {
              ai->u.err = errno;
              break;
            }
          ai->buf = newbuf;
        }
    }

  /* A security context can exist only if extended attributes do.  */
  if (flags & ACL_GET_SCONTEXT
      && (0 < ai->size || aclinfo_may_indicate_xattr (ai)))
    {
      if (is_smack_enabled ())
        {
          if (ai->size < 0 || aclinfo_has_xattr (ai, XATTR_NAME_SMACK))
            {
              static char const SMACK64[] = "security.SMACK64";
              ssize_t r =
                (fd < 0
                 ? smack_new_label_from_path (name, SMACK64,
                                              flags & ACL_SYMLINK_FOLLOW,
                                              &ai->scontext)
                 : smack_new_label_from_file (fd, SMACK64, &ai->scontext));
              scontext_err = r < 0 ? errno : 0;
            }
        }
      else
        {
# if USE_SELINUX_SELINUX_H
          if (ai->size < 0 || aclinfo_has_xattr (ai, XATTR_NAME_SELINUX))
            {
              ssize_t r =
                (fd < 0
                 ? ((flags & ACL_SYMLINK_FOLLOW ? getfilecon : lgetfilecon)
                    (name, &ai->scontext))
                 : fgetfilecon (fd, &ai->scontext));
              scontext_err = r < 0 ? errno : 0;
#  ifndef SE_SELINUX_INLINE
              /* Gnulib's selinux-h module is not in use, so getfilecon and
                 lgetfilecon can misbehave, be it via an old version of
                 libselinux where these would return 0 and set the result
                 context to NULL, or via a modern kernel+lib operating on a
                 file from a disk whose attributes were set by a kernel from
                 around 2006.  In that latter case, the functions return a
                 length of 10 for the "unlabeled" context.  Map both failures
                 to a return value of -1, and set errno to ENOTSUP in the
                 first case, and ENODATA in the latter.  */
              if (r == 0)
                scontext_err = ENOTSUP;
              if (r == 10 && memeq (ai->scontext, "unlabeled", 10))
                {
                  freecon (ai->scontext);
                  scontext_err = ENODATA;
                }
#  endif
            }
# endif
        }
    }
  ai->scontext_err = scontext_err;
  if (scontext_err)
    ai->scontext = (char *) UNKNOWN_SECURITY_CONTEXT;
}

# ifndef aclinfo_scontext_free
/* Free the pointer that file_has_aclinfo put into scontext.
   However, do nothing if the argument is a null pointer;
   This lets the caller replace the scontext member with a null pointer if it
   is willing to own the member and call this function later.  */
void
aclinfo_scontext_free (char *scontext)
{
  if (scontext != UNKNOWN_SECURITY_CONTEXT)
    {
      if (is_smack_enabled ())
        free (scontext);
      else if (scontext)
        freecon (scontext);
    }
}
# endif

/* Free AI's heap storage.  */
void
aclinfo_free (struct aclinfo *ai)
{
  if (ai->buf != ai->u.__gl_acl_ch)
    free (ai->buf);
  aclinfo_scontext_free (ai->scontext);
}

/* Return 1 if given ACL in XDR format is non-trivial, 0 if it is trivial.
   -1 upon failure to determine it.  Possibly change errno.  Assume that
   the ACL is valid, except avoid undefined behavior even if invalid.

   See <https://linux.die.net/man/5/nfs4_acl>.  The NFSv4 acls are
   defined in Internet RFC 7530 and as such, every NFSv4 server
   supporting ACLs should support NFSv4 ACLs (they differ from from
   POSIX draft ACLs).  The ACLs can be obtained via the
   nfsv4-acl-tools, e.g., the nfs4_getfacl command.  Gnulib provides
   only basic support of NFSv4 ACLs, i.e., recognize trivial vs
   nontrivial ACLs.  */

static int
acl_nfs4_nontrivial (uint32_t *xattr, ssize_t nbytes)
{
  enum { BYTES_PER_NETWORK_UINT = 4};

  /* Grab the number of aces in the acl.  */
  nbytes -= BYTES_PER_NETWORK_UINT;
  if (nbytes < 0)
    return -1;
  uint32_t num_aces = ntohl (*xattr++);
  if (6 < num_aces)
    return 1;
  int ace_found = 0;

  for (int ace_n = 0; ace_n < num_aces; ace_n++)
    {
      /* Get the acl type and flag.  Skip the mask; it's too risky to
         test it and it does not seem to be needed.  Get the wholen.  */
      nbytes -= 4 * BYTES_PER_NETWORK_UINT;
      if (nbytes < 0)
        return -1;
      uint32_t type = ntohl (xattr[0]);
      uint32_t flag = ntohl (xattr[1]);
      uint32_t wholen = ntohl (xattr[3]);
      xattr += 4;
      int whowords = (wholen / BYTES_PER_NETWORK_UINT
                      + (wholen % BYTES_PER_NETWORK_UINT != 0));
      int64_t wholen4 = whowords;
      wholen4 *= BYTES_PER_NETWORK_UINT;

      /* Trivial ACLs have only ACE4_ACCESS_ALLOWED_ACE_TYPE or
         ACE4_ACCESS_DENIED_ACE_TYPE.  */
      if (ACE4_ACCESS_DENIED_ACE_TYPE < type)
        return 1;

      /* RFC 7530 says FLAG should be 0, but be generous to NetApp and
         also accept the group flag.  */
      if (flag & ~ACE4_IDENTIFIER_GROUP)
        return 1;

      /* Get the who string.  Check NBYTES - WHOLEN4 before storing
         into NBYTES, to avoid truncation on conversion.  */
      if (nbytes - wholen4 < 0)
        return -1;
      nbytes -= wholen4;

      /* For a trivial ACL, max 6 (typically 3) ACEs, 3 allow, 3 deny.
         Check that there is at most one ACE of each TYPE and WHO.  */
      int who2
        = (wholen == 6 && memeq (xattr, "OWNER@", 6) ? 0
           : wholen == 6 && memeq (xattr, "GROUP@", 6) ? 2
           : wholen == 9 && memeq (xattr, "EVERYONE@", 9) ? 4
           : -1);
      if (who2 < 0)
        return 1;
      int ace_found_bit = 1 << (who2 | type);
      if (ace_found & ace_found_bit)
        return 1;
      ace_found |= ace_found_bit;

      xattr += whowords;
    }

  return 0;
}
#endif

#if (!USE_LINUX_XATTR && USE_ACL && HAVE_ACL_GET_FILE \
     && !HAVE_ACL_EXTENDED_FILE && !HAVE_ACL_TYPE_EXTENDED)
/* FreeBSD, NetBSD >= 10, Cygwin >= 2.5 */

# if HAVE_ACL_GET_FD && !HAVE_ACL_GET_LINK_NP /* Cygwin >= 2.5 */
#  include <fcntl.h>
#  ifdef O_PATH
#   define acl_get_fd_np(fd, type) acl_get_fd (fd)

/* Like acl_get_file, but do not follow symbolic links.  */
static acl_t
acl_get_link_np (char const *name, acl_type_t type)
{
  int fd = open (name, O_PATH | O_NOFOLLOW);
  if (fd < 0)
    return NULL;
  acl_t r = acl_get_fd (fd);
  int err = errno;
  close (fd);
  errno = err;
  return r;
}
#   define HAVE_ACL_GET_LINK_NP 1
#  endif
# endif

static acl_t
acl_get_fdfile (int fd, char const *name, acl_type_t type, int flags)
{
  acl_t (*get) (char const *, acl_type_t) = acl_get_file;
# if HAVE_ACL_GET_LINK_NP /* FreeBSD, NetBSD >= 10, Cygwin >= 2.5 */
  if (0 <= fd)
    return acl_get_fd_np (fd, type);
  if (! (flags & ACL_SYMLINK_FOLLOW))
    get = acl_get_link_np;
# else
  /* Ignore FD and FLAGS, unfortunately.  */
# endif
  return get (name, type);
}
#endif

/* Return 1 if NAME has a nontrivial access control list,
   0 if ACLs are not supported, or if NAME has no or only a base ACL,
   and -1 (setting errno) on error.  Note callers can determine
   if ACLs are not supported as errno is set in that case also.
   Set *AI to ACL info regardless of return value.
   FLAGS should be a <dirent.h> d_type value, optionally ORed with
     - _GL_DT_NOTDIR if it is known that NAME is not a directory,
     - ACL_GET_SCONTEXT to retrieve security context and return 1 if present,
     - ACL_SYMLINK_FOLLOW to follow the link if NAME is a symbolic link;
       otherwise do not follow them if possible.
   If the d_type value is not known, use DT_UNKNOWN though this may be less
   efficient.  */
int
file_has_aclinfo (char const *restrict name,
                  struct aclinfo *restrict ai, int flags)
{
  return fdfile_has_aclinfo (-1, name, ai, flags);
}

/* Return 1 if FD aka NAME has a nontrivial access control list,
   0 if ACLs are not supported, or if NAME has no or only a base ACL,
   and -1 (setting errno) on error.  Note callers can determine
   if ACLs are not supported as errno is set in that case also.
   Ignore FD if it is negative.
   Set *AI to ACL info regardless of return value.
   FLAGS should be a <dirent.h> d_type value, optionally ORed with
     - _GL_DT_NOTDIR if it is known that NAME is not a directory,
     - ACL_GET_SCONTEXT to retrieve security context and return 1 if present,
     - ACL_SYMLINK_FOLLOW to follow the link if NAME is a symbolic link;
       otherwise do not follow them if possible.
   If the d_type value is not known, use DT_UNKNOWN though this may be less
   efficient.  */
int
fdfile_has_aclinfo (MAYBE_UNUSED int fd,
                    MAYBE_UNUSED char const *restrict name,
                    struct aclinfo *restrict ai, int flags)
{
  MAYBE_UNUSED unsigned char d_type = flags & UCHAR_MAX;

#if USE_LINUX_XATTR
  int initial_errno = errno;
  get_aclinfo (fd, name, ai, flags);

  if (!aclinfo_may_indicate_xattr (ai) && ai->size <= 0)
    {
      errno = ai->size < 0 ? ai->u.err : initial_errno;
      return ai->size;
    }

  /* In Fedora 39, a file can have both NFSv4 and POSIX ACLs,
     but if it has an NFSv4 ACL that's the one that matters.
     In earlier Fedora the two types of ACLs were mutually exclusive.
     Attempt to work correctly on both kinds of systems.  */

  if (!has_xattr (XATTR_NAME_NFSV4_ACL, ai, fd, name, flags))
    return
      (has_xattr (XATTR_NAME_POSIX_ACL_ACCESS, ai, fd, name, flags)
       || ((d_type == DT_DIR || d_type == DT_UNKNOWN)
           && has_xattr (XATTR_NAME_POSIX_ACL_DEFAULT, ai, fd, name, flags)));

  /* A buffer large enough to hold any trivial NFSv4 ACL.
     The max length of a trivial NFSv4 ACL is 6 words for owner,
     6 for group, 7 for everyone, all times 2 because there are both
     allow and deny ACEs.  There are 6 words for owner because of
     type, flag, mask, wholen, "OWNER@"+pad and similarly for group;
     everyone is another word to hold "EVERYONE@".  */
  uint32_t buf[2 * (6 + 6 + 7)];

  int ret = (fd < 0
             ? ((flags & ACL_SYMLINK_FOLLOW ? getxattr : lgetxattr)
                (name, XATTR_NAME_NFSV4_ACL, buf, sizeof buf))
             : fgetxattr (fd, XATTR_NAME_NFSV4_ACL, buf, sizeof buf));
  if (ret < 0)
    switch (errno)
      {
      case ENODATA: return 0;
      case ERANGE : return 1; /* ACL must be nontrivial.  */
      default: return - acl_errno_valid (errno);
      }

  /* It looks like a trivial ACL, but investigate further.  */
  ret = acl_nfs4_nontrivial (buf, ret);
  errno = ret < 0 ? EINVAL : initial_errno;
  return ret;

#else /* !USE_LINUX_XATTR */

  ai->buf = ai->u.__gl_acl_ch;
  ai->size = -1;
  ai->u.err = ENOTSUP;
  ai->scontext = (char *) UNKNOWN_SECURITY_CONTEXT;
  ai->scontext_err = ENOTSUP;

# if USE_ACL
#  if HAVE_ACL_GET_FILE

  {
    /* POSIX 1003.1e (draft 17 -- abandoned) specific version.  */
    /* Linux, FreeBSD, NetBSD >= 10, Mac OS X, Cygwin >= 2.5 */
    int ret;

#   if HAVE_ACL_EXTENDED_FILE /* Linux */
      /* On Linux, acl_extended_file is an optimized function: It only
         makes two calls to getxattr(), one for ACL_TYPE_ACCESS, one for
         ACL_TYPE_DEFAULT.  */
    ret = (fd < 0
           ? ((flags & ACL_SYMLINK_FOLLOW
               ? acl_extended_file
               : acl_extended_file_nofollow)
              (name))
           : acl_extended_fd (fd));
#   elif HAVE_ACL_TYPE_EXTENDED /* Mac OS X */
    /* On Mac OS X, acl_get_file (name, ACL_TYPE_ACCESS)
       and acl_get_file (name, ACL_TYPE_DEFAULT)
       always return NULL / EINVAL.  There is no point in making
       these two useless calls.  The real ACL is retrieved through
       ACL_TYPE_EXTENDED.  */
    acl_t acl =
      (fd < 0
       ? ((flags & ACL_SYMLINK_FOLLOW ? acl_get_file : acl_get_link_np)
          (name, ACL_TYPE_EXTENDED))
       : acl_get_fd_np (fd, ACL_TYPE_EXTENDED));
    if (acl)
      {
        ret = acl_extended_nontrivial (acl);
        acl_free (acl);
      }
    else
      ret = -1;
#   else /* FreeBSD, NetBSD >= 10, Cygwin >= 2.5 */

    acl_t acl = acl_get_fdfile (fd, name, ACL_TYPE_ACCESS, flags);
    if (acl)
      {
        ret = acl_access_nontrivial (acl);
        int saved_errno = errno;
        acl_free (acl);
        errno = saved_errno;
        /* On Linux, FreeBSD, NetBSD,
               acl_get_file (name, ACL_TYPE_ACCESS)
           and acl_get_file (name, ACL_TYPE_DEFAULT) on a directory
           either both succeed or both fail; it depends on the
           file system.  Therefore there is no point in making the second
           call if the first one already failed.  */
        if (ret == 0
            && (d_type == DT_DIR
                || (d_type == DT_UNKNOWN && !(flags & _GL_DT_NOTDIR))))
          {
            acl = acl_get_fdfile (fd, name, ACL_TYPE_DEFAULT, flags);
            if (acl)
              {
#    ifdef __CYGWIN__ /* Cygwin >= 2.5 */
                ret = acl_access_nontrivial (acl);
                saved_errno = errno;
                acl_free (acl);
                errno = saved_errno;
#    else
                ret = (0 < acl_entries (acl));
                acl_free (acl);
#    endif
              }
            else
              {
                ret = -1;
#    ifdef __CYGWIN__ /* Cygwin >= 2.5 */
                if (d_type == DT_UNKNOWN)
                  ret = 0;
#    endif
              }
          }
      }
    else
      ret = -1;
#   endif

    return ret < 0 ? - acl_errno_valid (errno) : ret;
  }

#  else /* !HAVE_ACL_GET_FILE */

  /* The remaining APIs always follow symlinks and operate on
     platforms where symlinks do not have ACLs, so skip the APIs if
     NAME is known to be a symlink.  */
  if (d_type != DT_LNK)
    {

#   if HAVE_FACL && defined GETACL /* Solaris, Cygwin < 2.5, not HP-UX */

#    ifdef ACL_NO_TRIVIAL

      /* Solaris 10 (newer version), which has additional API declared in
         <sys/acl.h> (acl_t) and implemented in libsec (acl_set, acl_trivial,
         acl_fromtext, ...).

         Ignore FD, unfortunately.  That is better than mishandling
         ZFS-style ACLs, as the general case code does.  */
      return acl_trivial (name);

#    else /* Solaris, Cygwin, general case */

      /* Solaris 2.5 through Solaris 10, Cygwin, and contemporaneous versions
         of Unixware.  The acl() call returns the access and default ACL both
         at once.  */
      {
        /* Initially, try to read the entries into a stack-allocated buffer.
           Use malloc if it does not fit.  */
        enum
          {
            alloc_init = 4000 / sizeof (aclent_t), /* >= 3 */
            alloc_max = MIN (INT_MAX, SIZE_MAX / sizeof (aclent_t))
          };
        aclent_t buf[alloc_init];
        size_t alloc = alloc_init;
        aclent_t *entries = buf;
        aclent_t *malloced = NULL;
        int count;

        for (;;)
          {
            count = (fd < 0
                     ? acl (name, GETACL, alloc, entries)
                     : facl (fd, GETACL, alloc, entries));
            if (count < 0 && errno == ENOSPC)
              {
                /* Increase the size of the buffer.  */
                free (malloced);
                if (alloc > alloc_max / 2)
                  {
                    errno = ENOMEM;
                    return -1;
                  }
                alloc = 2 * alloc; /* <= alloc_max */
                entries = malloced =
                  (aclent_t *) malloc (alloc * sizeof (aclent_t));
                if (entries == NULL)
                  return -1;
                continue;
              }
            break;
          }
        if (count < 0)
          {
            if (errno == ENOSYS || errno == ENOTSUP)
              ;
            else
              {
                free (malloced);
                return -1;
              }
          }
        else if (count == 0)
          ;
        else
          {
            /* Don't use MIN_ACL_ENTRIES:  It's set to 4 on Cygwin, but Cygwin
               returns only 3 entries for files with no ACL.  But this is safe:
               If there are more than 4 entries, there cannot be only the
               "user::", "group::", "other:", and "mask:" entries.  */
            if (count > 4)
              {
                free (malloced);
                return 1;
              }

            if (acl_nontrivial (count, entries))
              {
                free (malloced);
                return 1;
              }
          }
        free (malloced);
      }

#     ifdef ACE_GETACL
      /* Solaris also has a different variant of ACLs, used in ZFS and NFSv4
         file systems (whereas the other ones are used in UFS file systems).  */
      {
        /* Initially, try to read the entries into a stack-allocated buffer.
           Use malloc if it does not fit.  */
        enum
          {
            alloc_init = 4000 / sizeof (ace_t), /* >= 3 */
            alloc_max = MIN (INT_MAX, SIZE_MAX / sizeof (ace_t))
          };
        ace_t buf[alloc_init];
        size_t alloc = alloc_init;
        ace_t *entries = buf;
        ace_t *malloced = NULL;
        int count;

        for (;;)
          {
            count = (fd < 0
                     ? acl (name, ACE_GETACL, alloc, entries)
                     : facl (fd, ACE_GETACL, alloc, entries));
            if (count < 0 && errno == ENOSPC)
              {
                /* Increase the size of the buffer.  */
                free (malloced);
                if (alloc > alloc_max / 2)
                  {
                    errno = ENOMEM;
                    return -1;
                  }
                alloc = 2 * alloc; /* <= alloc_max */
                entries = malloced = (ace_t *) malloc (alloc * sizeof (ace_t));
                if (entries == NULL)
                  return -1;
                continue;
              }
            break;
          }
        if (count < 0)
          {
            if (errno == ENOSYS || errno == EINVAL)
              ;
            else
              {
                free (malloced);
                return -1;
              }
          }
        else if (count == 0)
          ;
        else
          {
            /* In the old (original Solaris 10) convention:
               If there are more than 3 entries, there cannot be only the
               ACE_OWNER, ACE_GROUP, ACE_OTHER entries.
               In the newer Solaris 10 and Solaris 11 convention:
               If there are more than 6 entries, there cannot be only the
               ACE_OWNER, ACE_GROUP, ACE_EVERYONE entries, each once with
               NEW_ACE_ACCESS_ALLOWED_ACE_TYPE and once with
               NEW_ACE_ACCESS_DENIED_ACE_TYPE.  */
            if (count > 6)
              {
                free (malloced);
                return 1;
              }

            if (acl_ace_nontrivial (count, entries))
              {
                free (malloced);
                return 1;
              }
          }
        free (malloced);
      }
#     endif

      return 0;
#    endif

#   elif HAVE_GETACL /* HP-UX */

      {
        struct acl_entry entries[NACLENTRIES];
        int count;

        count = (fd < 0
                 ? getacl (name, NACLENTRIES, entries)
                 : fgetacl (fd, NACLENTRIES, entries));

        if (count < 0)
          {
            /* ENOSYS is seen on newer HP-UX versions.
               EOPNOTSUPP is typically seen on NFS mounts.
               ENOTSUP was seen on Quantum StorNext file systems (cvfs).  */
            if (errno == ENOSYS || errno == EOPNOTSUPP || errno == ENOTSUP)
              ;
            else
              return -1;
          }
        else if (count == 0)
          return 0;
        else /* count > 0 */
          {
            if (count > NACLENTRIES)
              /* If NACLENTRIES cannot be trusted, use dynamic memory
                 allocation.  */
              abort ();

            /* If there are more than 3 entries, there cannot be only the
               (uid,%), (%,gid), (%,%) entries.  */
            if (count > 3)
              return 1;

            {
              struct stat statbuf;

              if ((fd < 0 ? stat (name, &statbuf) : fstat (fd, &statbuf)) < 0
                  && errno != EOVERFLOW)
                return -1;

              return acl_nontrivial (count, entries);
            }
          }
      }

#    if HAVE_ACLV_H /* HP-UX >= 11.11 */

      {
        struct acl entries[NACLVENTRIES];
        int count;

        /* Ignore FD, unfortunately.  */
        count = acl ((char *) name, ACL_GET, NACLVENTRIES, entries);

        if (count < 0)
          {
            /* EOPNOTSUPP is seen on NFS in HP-UX 11.11, 11.23.
               EINVAL is seen on NFS in HP-UX 11.31.  */
            if (errno == ENOSYS || errno == EOPNOTSUPP || errno == EINVAL)
              ;
            else
              return -1;
          }
        else if (count == 0)
          return 0;
        else /* count > 0 */
          {
            if (count > NACLVENTRIES)
              /* If NACLVENTRIES cannot be trusted, use dynamic memory
                 allocation.  */
              abort ();

            /* If there are more than 4 entries, there cannot be only the
               four base ACL entries.  */
            if (count > 4)
              return 1;

            return aclv_nontrivial (count, entries);
          }
      }

#    endif

#   elif HAVE_ACLX_GET && defined ACL_AIX_WIP /* AIX */

      acl_type_t type;
      char aclbuf[1024];
      void *acl = aclbuf;
      size_t aclsize = sizeof (aclbuf);
      mode_t mode;

      for (;;)
        {
          /* The docs say that type being 0 is equivalent to ACL_ANY, but it
             is not true, in AIX 5.3.  */
          type.u64 = ACL_ANY;
          if (0 <= (fd < 0
                    ? aclx_get (name, 0, &type, aclbuf, &aclsize, &mode)
                    : aclx_fget (fd, 0, &type, aclbuf, &aclsize, &mode)))
            break;
          if (errno == ENOSYS)
            return 0;
          if (errno != ENOSPC)
            {
              if (acl != aclbuf)
                free (acl);
              return -1;
            }
          aclsize = 2 * aclsize;
          if (acl != aclbuf)
            free (acl);
          acl = malloc (aclsize);
          if (acl == NULL)
            return -1;
        }

      if (type.u64 == ACL_AIXC)
        {
          int result = acl_nontrivial ((struct acl *) acl);
          if (acl != aclbuf)
            free (acl);
          return result;
        }
      else if (type.u64 == ACL_NFS4)
        {
          int result = acl_nfs4_nontrivial ((nfs4_acl_int_t *) acl);
          if (acl != aclbuf)
            free (acl);
          return result;
        }
      else
        {
          /* A newer type of ACL has been introduced in the system.
             We should better support it.  */
          if (acl != aclbuf)
            free (acl);
          errno = EINVAL;
          return -1;
        }

#   elif HAVE_STATACL /* older AIX */

      union { struct acl a; char room[4096]; } u;

      if ((fd < 0
           ? statacl ((char *) name, STX_NORMAL, &u.a, sizeof u)
           : fstatacl (fd, STX_NORMAL, &u.a, sizeof u))
          < 0)
        return -1;

      return acl_nontrivial (&u.a);

#   elif HAVE_ACLSORT /* NonStop Kernel */

      {
        struct acl entries[NACLENTRIES];
        int count;

        /* Ignore FD, unfortunately.  */
        count = acl ((char *) name, ACL_GET, NACLENTRIES, entries);

        if (count < 0)
          {
            if (errno == ENOSYS || errno == ENOTSUP)
              ;
            else
              return -1;
          }
        else if (count == 0)
          return 0;
        else /* count > 0 */
          {
            if (count > NACLENTRIES)
              /* If NACLENTRIES cannot be trusted, use dynamic memory
                 allocation.  */
              abort ();

            /* If there are more than 4 entries, there cannot be only the
               four base ACL entries.  */
            if (count > 4)
              return 1;

            return acl_nontrivial (count, entries);
          }
      }
#   endif
    }
#  endif
# endif
#endif

  return 0;
}

/* Return 1 if NAME has a nontrivial access control list,
   0 if ACLs are not supported, or if NAME has no or only a base ACL,
   and -1 (setting errno) on error.  Note callers can determine
   if ACLs are not supported as errno is set in that case also.
   SB must be set to the stat buffer of NAME,
   obtained through stat() or lstat().  */
int
file_has_acl (char const *name, struct stat const *sb)
{
  int flags = IFTODT (sb->st_mode);
  if (!S_ISDIR (sb->st_mode))
    flags |= _GL_DT_NOTDIR;
  struct aclinfo ai;
  int r = file_has_aclinfo (name, &ai, flags);
  aclinfo_free (&ai);
  return r;
}
