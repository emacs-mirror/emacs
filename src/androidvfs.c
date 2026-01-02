/* Android virtual file-system support for GNU Emacs.

Copyright (C) 2023-2026 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#include <config.h>
#include <fcntl.h>
#include <unistd.h>
#include <assert.h>
#include <dlfcn.h>
#include <dirent.h>
#include <errno.h>
#include <minmax.h>
#include <string.h>
#include <systime.h>
#include <semaphore.h>

#include <sys/stat.h>
#include <sys/mman.h>

#include <stat-time.h>
#include <md5.h>

#include <linux/ashmem.h>

#include "android.h"
#include "androidterm.h"
#include "systime.h"
#include "blockinput.h"
#include "coding.h"

#if __ANDROID_API__ >= 9
#include <android/asset_manager.h>
#include <android/asset_manager_jni.h>
#define OLD_ANDROID_ASSETS 0
#else /* __ANDROID_API__ < 9 */
#include "android-asset.h"
#define OLD_ANDROID_ASSETS 1
#endif /* __ANDROID_API__ >= 9 */

#include <android/log.h>

/* This file implements support for the various special-purpose
   directories found on Android systems through a series of functions
   that substitute for Unix system call wrappers.  Such directories
   are not mounted in the Unix virtual file-system, but instead
   require the use of special system APIs to access; Emacs pretends
   they are mounted at specific folders within the root directory.

   There are presently two directories: /assets, granting access to
   asset files stored within the APK, and /content, providing direct
   access to content URIs (in Android 4.4 and later) and content
   directory trees (in Android 5.0 and later.)

   Substitutes for the C library `open', `fstat', `close', `fclose',
   `unlink', `symlink', `rmdir', `rename', `stat' system call wrappers
   are implemented, which delegate their actions to function tables
   contained inside ``VFS nodes''.

   The functions of a VFS node are to provide the implementations of
   the Unix file system operations that can be carried out on the file
   designated by its name and to connect useful information (such as
   internal file handles or identifiers) with those file names.  To
   those ends, there exist several different types of vnodes, each
   with a different set of functions and supplementary attributes.

   The key to locating the correct vnode for any given file name is an
   additional file system operation, defined by each node, which
   ``names'' children.  This operation takes a relative file name and
   returns a second node designating a constituent sub-file.

   When a file system function is called, it invokes the `name'
   operation of a special root vnode conceptually located at the top
   of the Unix file system hierarchy, handing it the complete file
   name given to it.  This vnode's name operation examines the first
   component of the relative file name it receives and creates either
   an asset, content, or Unix vnode, and calls the new vnode's `name'
   operation with the remainder of the file name.

   The vnode(s) created by each `name' operation may in turn create
   different vnodes based on the components of the names they have
   been provided that are used to repeat this process until no
   components remain.  The vnode created for the last component of the
   file name will provide its file system operations or be passed as
   an argument to other file system operations to which the file has
   been passed as an argument.

   The substitute functions defined have two caveats, which however
   don't prove problematic in an Emacs context: the first is that the
   treatment of `..' is inconsistent with Unix, and has not really
   been tested, while the second is that errno values do not always
   conform to what the corresponding Unix system calls may return.
   These caveats are described in more detail inside the last few
   pages of this file.  */

/* Structure describing an array of VFS operations.  */

struct android_vnode;

struct android_vdir
{
  /* Return a `struct dirent' describing the next file in this
     directory stream, or NULL if the stream has reached its end.  */
  struct dirent *(*readdir) (struct android_vdir *);

  /* Close and release all resources allocated for this directory
     stream.  */
  void (*closedir) (struct android_vdir *);

  /* Return a ``file descriptor'' tied to this directory stream.  */
  int (*dirfd) (struct android_vdir *);
};

struct android_vops
{
  /* Name a child of the given VFS node, which should be a
     directory.

     LENGTH should be the length of NAME, excluding that of any
     trailing NULL byte.

     NAME should be a normalized and NULL-terminated relative file
     name; it may contain a leading separator characters, but no
     consecutive ones.

     If NAME is empty, create another VFS node designating the same
     file instead.

     NAME should also be located within writable storage; it may be
     overwritten as the vnode sees fit.

     Value is a VFS node corresponding to the child, or NULL upon
     failure.

     A VFS node may be returned even if NAME does not exist, the
     expectation being that either a later filesystem operation will
     fail, or will create the file.  */
  struct android_vnode *(*name) (struct android_vnode *, char *, size_t);

  /* Open the specified VNODE, returning either a file descriptor or
     an asset file descriptor.

     FLAGS and MODE mean the same as they do to the Unix `open' system
     call.

     ASSET_P stipulates if an asset file descriptor may be returned;
     if true, *ASSET may be set to an asset file descriptor.

     If an asset file descriptor is unavailable or ASSET_P is false,
     *FD will be set to a file descriptor.

     If the vnode cannot be opened, value is -1 with errno set
     accordingly.  Otherwise, value is 0 if a file descriptor was
     returned, and 1 if an asset file descriptor was returned.  */
  int (*open) (struct android_vnode *, int, mode_t, bool,
	       int *, AAsset **);

  /* Close the specified VNODE, releasing all of its resources.
     Save errno before making system calls that may set it, and
     restore it to its original value before returning.

     This is unrelated to `android_close', which primarily releases on
     stat buffers linked to file or asset file descriptors.  */
  void (*close) (struct android_vnode *);

  /* Unlink the file and the specified VNODE.  Value and errno are the
     same as Unix `unlink'.  */
  int (*unlink) (struct android_vnode *);

  /* Create a symlink from the specified VNODE to the target TARGET.
     Value and errno are the same as `symlink' on Linux (which notably
     means that errno is set to EPERM if VNODE doesn't support
     symlinks.)  */
  int (*symlink) (const char *, struct android_vnode *);

  /* Remove VNODE from its parent directory.  VNODE must be an empty
     directory.  Value and errno are the same as Unix `rmdir'.  */
  int (*rmdir) (struct android_vnode *);

  /* Move the file designated by SRC to DST, overwriting DST if
     KEEP_EXISTING is false.

     If KEEP_EXISTING is true and DST already exists, value is -1 with
     errno set to EEXIST.

     If VNODE does not natively support checking for a preexisting DST
     and KEEP_EXISTING is true, value is -1 with errno set to ENOSYS.

     Value is otherwise the same as `rename'.  */
  int (*rename) (struct android_vnode *, struct android_vnode *, bool);

  /* Return statistics for the specified VNODE, and FLAGS, as in a call
     to `fstatat'.  Value and errno are the same as with Unix
     `stat'.  */
  int (*stat) (struct android_vnode *, struct stat *, int);

  /* Return whether or not VNODE is accessible.
     Value, errno and MODE are the same as with Unix `access'.  */
  int (*access) (struct android_vnode *, int);

  /* Make a directory designated by VNODE, like Unix `mkdir'.  */
  int (*mkdir) (struct android_vnode *, mode_t);

  /* Change the access mode of the provided VNODE to MODE.  Value is
     the same as with `chmod'.  FLAGS is passed verbatim from the call
     to the delegating at-func, and is probably
     AT_SYMLINK_NOFOLLOW.  */
  int (*chmod) (struct android_vnode *, mode_t, int);

  /* Return the target of VNODE if it is a symbolic link, or -1.
     Value and errno are the same as with `readlink'.  */
  ssize_t (*readlink) (struct android_vnode *, char *, size_t);

  /* Open the specified VNODE as a directory.
     Value is a ``directory handle'', or NULL upon failure.  */
  struct android_vdir *(*opendir) (struct android_vnode *);
};

struct android_vnode
{
  /* Operations associated with this vnode.  */
  struct android_vops *ops;

  /* Type of this vnode and its flags.  */
  short type, flags;
};

/* Structure describing a special named vnode relative to the root
   vnode, or another directory vnode.  */

struct android_special_vnode
{
  /* The name of the special file.  */
  const char *name;

  /* The length of that name.  */
  size_t length;

  /* Function called to create the initial vnode from the rest of the
     component.  */
  struct android_vnode *(*initial) (char *, size_t);

  /* If non-nil, an encoding system into which file name buffers are to
     be re-encoded before being handed to VFS functions.  */
  Lisp_Object special_coding_system;
};

static_assert (NIL_IS_ZERO); /* special_coding_system above.  */

enum android_vnode_type
  {
    ANDROID_VNODE_UNIX,
    ANDROID_VNODE_AFS,
    ANDROID_VNODE_CONTENT,
    ANDROID_VNODE_CONTENT_AUTHORITY,
    ANDROID_VNODE_CONTENT_AUTHORITY_NAMED,
    ANDROID_VNODE_SAF_ROOT,
    ANDROID_VNODE_SAF_TREE,
    ANDROID_VNODE_SAF_FILE,
    ANDROID_VNODE_SAF_NEW,
  };



/* Structure describing the android.database.Cursor class.  */

struct android_cursor_class
{
  jclass class;
  jmethodID close;
};

/* Structure describing the EmacsDirectoryEntry class.  */

struct emacs_directory_entry_class
{
  jclass class;
  jfieldID d_type;
  jfieldID d_name;
};

/* The java.lang.String class.  */
jclass java_string_class;

/* Fields and methods associated with the Cursor class.  */
static struct android_cursor_class cursor_class;

/* Fields and methods associated with the EmacsDirectoryEntry
   class.  */
static struct emacs_directory_entry_class entry_class;

/* Fields and methods associated with the ParcelFileDescriptor
   class.  */
struct android_parcel_file_descriptor_class fd_class;

/* Global references to several exception classes.  */
static jclass file_not_found_exception, security_exception;
static jclass operation_canceled_exception;
static jclass unsupported_operation_exception, out_of_memory_error;

/* Initialize `cursor_class' using the given JNI environment ENV.
   Calling this function is not necessary on Android 4.4 and
   earlier.  */

static void
android_init_cursor_class (JNIEnv *env)
{
  jclass old;

  cursor_class.class
    = (*env)->FindClass (env, "android/database/Cursor");
  eassert (cursor_class.class);

  old = cursor_class.class;
  cursor_class.class
    = (jclass) (*env)->NewGlobalRef (env, (jobject) old);
  (*env)->DeleteLocalRef (env, old);

  if (!cursor_class.class)
    emacs_abort ();

#define FIND_METHOD(c_name, name, signature)		\
  cursor_class.c_name					\
    = (*env)->GetMethodID (env, cursor_class.class,	\
			   name, signature);		\
  assert (cursor_class.c_name);
  FIND_METHOD (close, "close", "()V");
#undef FIND_METHOD
}

/* Initialize `entry_class' using the given JNI environment ENV.
   Calling this function is not necessary on Android 4.4 and
   earlier.  */

static void
android_init_entry_class (JNIEnv *env)
{
  jclass old;

  entry_class.class
    = (*env)->FindClass (env, "org/gnu/emacs/EmacsDirectoryEntry");
  eassert (entry_class.class);

  old = entry_class.class;
  entry_class.class
    = (jclass) (*env)->NewGlobalRef (env, (jobject) old);
  (*env)->DeleteLocalRef (env, old);

  if (!entry_class.class)
    emacs_abort ();

  entry_class.d_type = (*env)->GetFieldID (env, entry_class.class,
					   "d_type", "I");
  entry_class.d_name = (*env)->GetFieldID (env, entry_class.class,
					   "d_name",
					   "Ljava/lang/String;");
  assert (entry_class.d_type && entry_class.d_name);
}


/* Initialize `fd_class' using the given JNI environment ENV.  Called on
   API 12 (Android 3.1) and later by androidselect.c and on 5.0 and
   later in this file.  */

void
android_init_fd_class (JNIEnv *env)
{
  jclass old;
  static bool fd_class_initialized;

  if (fd_class_initialized)
    return;

  fd_class.class
    = (*env)->FindClass (env, "android/os/ParcelFileDescriptor");
  eassert (fd_class.class);

  old = fd_class.class;
  fd_class.class
    = (jclass) (*env)->NewGlobalRef (env, (jobject) old);
  (*env)->DeleteLocalRef (env, old);

  if (!fd_class.class)
    emacs_abort ();

#define FIND_METHOD(c_name, name, signature)		\
  fd_class.c_name					\
    = (*env)->GetMethodID (env, fd_class.class,		\
			   name, signature);		\
  assert (fd_class.c_name);
  FIND_METHOD (close, "close", "()V");
  FIND_METHOD (get_fd, "getFd", "()I");
  FIND_METHOD (detach_fd, "detachFd", "()I");
#undef FIND_METHOD

  fd_class_initialized = true;
}



/* Account for SAF file names two times as large as PATH_MAX; larger
   values are prohibitively slow, but smaller values can't face up to
   some long file names within several nested layers of directories.

   Buffers holding components or other similar file name constituents
   which don't represent SAF files must continue to use PATH_MAX, for
   that is the restriction imposed by the Unix file system.  */

#define EMACS_PATH_MAX (PATH_MAX * 2)

/* Delete redundant instances of `.' and `..' from NAME in-place.
   NAME must be *LENGTH long, excluding a mandatory trailing NULL
   byte.

   Transform each directory component in NAME to avoid instances
   of the `.' and `..' directories.  For example, turn:

     a/../b/c/.

   into

     b/c/

   and return NULL, writing the new length of NAME into *LENGTH.

   If there are more `..' components in NAME than there are normal
   file name components, return NAME incremented to the position after
   the first `..' component that cannot be transformed.  For example,
   if NAME is

     a/../../a

   value will be

     a

   If NAME is a directory separator and LENGTH is 1, return without
   modifying NAME.  In any other case, omit any leading directory
   separator when writing to NAME.  This is useful when a vnode that
   can only be opened as a directory is desired, as this status is
   made clear by suffixing the file name with a trailing
   directory separator.  */

static char *
android_vfs_canonicalize_name (char *name, size_t *length)
{
  size_t nellipsis, i;
  char *last_component, *prec_component, *fill, *orig_name;
  size_t size;

  /* Special case described in the last paragraph of the comment
     above.  */

  size = *length;
  orig_name = name;

  if (*name == '/' && size == 1)
    return NULL;
  else if (*name == '/')
    size -= 1;

  nellipsis = 0; /* Number of ellipsis encountered within the current
		    file name component, or -1.  */
  prec_component = NULL; /* Pointer to the separator character of the
			    component immediately preceding the
			    component currently being written.  */
  last_component = name; /* Pointer to the separator character of
			    the component currently being read.  */
  fill = name; /* Pointer to the next character that will be written
		  within NAME.  */

  /* Adjust name to skip the leading directory separator.  But only
     after fill is set.  */
  if (*name == '/')
    name++;

  for (i = 0; i < size; ++i)
    {
      switch (name[i])
	{
	case '/':
	  /* See if the previous component was `..' or `.'.

	     If it is .., and if no previous directory separator was
	     encountered, return or look up a vnode representing the
	     parent.  */

	  if (nellipsis == 2)
	    {
	      /* .. */

	      if (!prec_component)
		{
		  /* Return the content of the component, i.e. the text
		     _after_ this separator.  */
		  i++;
		  goto parent_vnode;
		}

	      /* Return to the last component.  */
	      fill = prec_component;

	      /* Restore last_component to prec_component, and
		 prec_component back to the component before that.  */
	      last_component = prec_component;

	      if (last_component != orig_name)
		prec_component = memrchr (orig_name, '/',
					  last_component - orig_name - 1);
	      else
		prec_component = NULL;

	      /* prec_component may now be NULL.  If last_component is
		 identical to the initial value of NAME, then fill has
		 really been returned to the beginning of the string, so
		 leave it be.  But if it's something else, then it must
		 be the first separator character in the string, so set
		 prec_component to this initial value itself.  */

	      if (!prec_component && last_component != orig_name)
		prec_component = orig_name;
	    }
	  else if (nellipsis == 1)
	    /* If it's ., return to this component.  */
	    fill = last_component;
	  else
	    {
	      /* Record the position of the last directory separator,
		 so NAME can be overwritten from there onwards if `..'
		 or `.' are encountered.  */
	      prec_component = last_component;
	      last_component = fill;
	    }

	  /* Allow tracking ellipses again.  */
	  nellipsis = 0;
	  break;

	case '.':
	  if (nellipsis != -1)
	    nellipsis++;
	  break;

	default:
	  nellipsis = -1;
	  break;
	}

      /* Now copy this character over from NAME.  */
      *fill++ = name[i];
    }

  /* See if the previous component was `..' or `.'.

     If it is .., and if no previous directory separator was
     encountered, return or look up a vnode representing the
     parent.  */

  if (nellipsis == 2)
    {
      /* .. */

      if (!prec_component)
	/* Look up the rest of the vnode in its parent.  */
	goto parent_vnode;

      /* Return to the last component.  */
      fill = prec_component;
      nellipsis = -2;
    }
  else if (nellipsis == 1)
    {
      /* If it's ., return to this component.  */
      fill = last_component;
      nellipsis = -2;
    }

  /* Now, if there's enough room and an ellipsis file name was the
     last component of END, append a trailing `/' before NULL
     terminating it, indicating that the file name must be a
     directory.  */

  if (fill + 1 < name + size && nellipsis == -2)
    *fill++ = '/';

  /* NULL terminate fill.  */
  *fill = '\0';
  *length = fill - orig_name;
  return NULL;

 parent_vnode:
  /* .. was encountered and the parent couldn't be found through
     stripping off preceding components.

     Find the parent vnode and name the rest of NAME starting from
     there.  */
  return name + i;
}



/* Unix vnode implementation.  These VFS nodes directly wrap around
   the Unix filesystem, with the exception of the root vnode.  */

struct android_unix_vnode
{
  /* The vnode data itself.  */
  struct android_vnode vnode;

  /* Length of the name without a trailing null byte.  */
  size_t name_length;

  /* Name of the vnode.  */
  char *name;
};

struct android_unix_vdir
{
  /* The directory function table.  */
  struct android_vdir vdir;

  /* The directory stream.  */
  DIR *directory;
};

/* The vnode representing the root filesystem.  */
static struct android_unix_vnode root_vnode;

static struct android_vnode *android_unix_name (struct android_vnode *,
						char *, size_t);
static int android_unix_open (struct android_vnode *, int,
			      mode_t, bool, int *, AAsset **);
static void android_unix_close (struct android_vnode *);
static int android_unix_unlink (struct android_vnode *);
static int android_unix_symlink (const char *, struct android_vnode *);
static int android_unix_rmdir (struct android_vnode *);
static int android_unix_rename (struct android_vnode *,
				struct android_vnode *, bool);
static int android_unix_stat (struct android_vnode *, struct stat *, int);
static int android_unix_access (struct android_vnode *, int);
static int android_unix_mkdir (struct android_vnode *, mode_t);
static int android_unix_chmod (struct android_vnode *, mode_t, int);
static ssize_t android_unix_readlink (struct android_vnode *, char *,
				      size_t);
static struct android_vdir *android_unix_opendir (struct android_vnode *);

/* Vector of VFS operations associated with Unix filesystem VFS
   nodes.  */

static struct android_vops unix_vfs_ops =
  {
    android_unix_name,
    android_unix_open,
    android_unix_close,
    android_unix_unlink,
    android_unix_symlink,
    android_unix_rmdir,
    android_unix_rename,
    android_unix_stat,
    android_unix_access,
    android_unix_mkdir,
    android_unix_chmod,
    android_unix_readlink,
    android_unix_opendir,
  };

static struct android_vnode *
android_unix_name (struct android_vnode *vnode, char *name,
		   size_t length)
{
  struct android_unix_vnode *vp, *input, temp;
  char *fill, *remainder;
  size_t j;

  /* Canonicalize NAME.  */
  input = (struct android_unix_vnode *) vnode;
  remainder = android_vfs_canonicalize_name (name, &length);

  /* If remainder is set, it's a name relative to the parent vnode.  */
  if (remainder)
    goto parent_vnode;

  /* Create a new unix vnode.  */
  vp = xmalloc (sizeof *vp);

  /* If name is empty, duplicate the current vnode, but reset its file
     operation vector to that for Unix vnodes.  */

  if (length < 1)
    {
      memcpy (vp, vnode, sizeof *vp);
      vp->vnode.ops = &unix_vfs_ops;
      vp->name = xstrdup (vp->name);
      return &vp->vnode;
    }

  /* Otherwise, fill in the vnode.  */

  vp->vnode.ops = &unix_vfs_ops;
  vp->vnode.type = ANDROID_VNODE_UNIX;
  vp->vnode.flags = 0;

  /* Generate the new name of the vnode.  Remove any trailing slash
     from vp->name.  */

  vp->name_length = input->name_length + length;
  vp->name = xmalloc (vp->name_length + 2);

  /* Copy the parent name over.  */
  fill = mempcpy (vp->name, input->name, input->name_length);

  /* Check if it contains a trailing slash.  input->name cannot be
     empty, as the root vnode's name is `/'.  */

  if (fill[-1] != '/' && *name != '/')
    /* If not, append a trailing slash and adjust vp->name_length
       correspondingly.  */
    *fill++ = '/', vp->name_length++;
  else if (fill[-1] == '/' && *name == '/')
    /* If name has a leading slash and fill does too, move fill
       backwards so that name's slash will override that of fill.  */
    fill--, vp->name_length--;

  /* Now copy NAME.  */
  fill = mempcpy (fill, name, length);

  /* And NULL terminate fill.  */
  *fill = '\0';
  return &vp->vnode;

 parent_vnode:
  /* .. was encountered and the parent couldn't be found through
     stripping off preceding components.

     Find the parent vnode and name the rest of NAME starting from
     there.  */

  if (input->name_length == 1)
    /* This is the vnode representing the root directory; just look
       within itself... */
    vnode = &root_vnode.vnode;
  else
    {
      /* Create a temporary unix vnode within the parent and use it
         instead.  First, establish the length of vp->name before its
         last component.  */

      for (j = input->name_length - 1; j; --j)
	{
	  if (input->name[j - 1] == '/')
	    break;
	}

      /* There must be at least one leading directory separator in an
	 asset vnode's `name' field.  */

      if (!j)
	abort ();

      /* j is now the length of the string minus the size of its last
	 component.  Create a temporary vnode with that as its
	 name.  */

      temp.vnode.ops = &unix_vfs_ops;
      temp.vnode.type = ANDROID_VNODE_UNIX;
      temp.vnode.flags = 0;
      temp.name_length = j;
      temp.name = xmalloc (j + 1);
      fill = mempcpy (temp.name, input->name, j);
      *fill = '\0';

      /* Search for the remainder of NAME relative to its parent.  */
      vnode = android_unix_name (&temp.vnode, remainder,
				 strlen (remainder));
      xfree (temp.name);
      return vnode;
    }

  /* Virtual directories must be ignored in accessing the root directory
     through a Unix subdirectory of the root, as, `/../' */
  return android_unix_name (vnode, remainder, strlen (remainder));
}

/* Create a Unix vnode representing the given file NAME.  Use this
   function to create vnodes that aren't rooted in the root VFS
   node.  */

static struct android_vnode *
android_unix_vnode (const char *name)
{
  struct android_unix_vnode *vp;

  vp = xmalloc (sizeof *vp);
  vp->vnode.ops = &unix_vfs_ops;
  vp->vnode.type = ANDROID_VNODE_UNIX;
  vp->vnode.flags = 0;
  vp->name_length = strlen (name);
  vp->name = xstrdup (name);
  return &vp->vnode;
}

static int
android_unix_open (struct android_vnode *vnode, int flags,
		   mode_t mode, bool asset_p, int *fd,
		   AAsset **asset)
{
  struct android_unix_vnode *vp;
  int fds;

  vp = (struct android_unix_vnode *) vnode;
  fds = open (vp->name, flags, mode);

  if (fds < 0)
    return -1;

  *fd = fds;
  return 0;
}

static void
android_unix_close (struct android_vnode *vnode)
{
  struct android_unix_vnode *vp;
  int save_errno;

  save_errno = errno;
  vp = (struct android_unix_vnode *) vnode;
  xfree (vp->name);
  xfree (vp);
  errno = save_errno;
}

static int
android_unix_unlink (struct android_vnode *vnode)
{
  struct android_unix_vnode *vp;

  vp = (struct android_unix_vnode *) vnode;
  return unlink (vp->name);
}

static int
android_unix_symlink (const char *target, struct android_vnode *vnode)
{
  struct android_unix_vnode *vp;

  vp = (struct android_unix_vnode *) vnode;
  return symlink (target, vp->name);
}

static int
android_unix_rmdir (struct android_vnode *vnode)
{
  struct android_unix_vnode *vp;

  vp = (struct android_unix_vnode *) vnode;
  return rmdir (vp->name);
}

static int
android_unix_rename (struct android_vnode *src,
		     struct android_vnode *dst,
		     bool keep_existing)
{
  struct android_unix_vnode *vp, *dest;

  if (src->type != dst->type)
    {
      /* If the types of both vnodes differ, complain that they're on
	 two different filesystems (which is correct from a abstract
	 viewpoint.)  */
      errno = EXDEV;
      return -1;
    }

  vp = (struct android_unix_vnode *) src;
  dest = (struct android_unix_vnode *) dst;

  return (keep_existing
	  ? renameat_noreplace (AT_FDCWD, vp->name,
				AT_FDCWD, dest->name)
	  : rename (vp->name, dest->name));
}

static int
android_unix_stat (struct android_vnode *vnode, struct stat *statb,
		   int flags)
{
  struct android_unix_vnode *vp;

  vp = (struct android_unix_vnode *) vnode;
  return fstatat (AT_FDCWD, vp->name, statb, flags);
}

static int
android_unix_access (struct android_vnode *vnode, int mode)
{
  struct android_unix_vnode *vp;

  vp = (struct android_unix_vnode *) vnode;
  return access (vp->name, mode);
}

static int
android_unix_mkdir (struct android_vnode *vnode, mode_t mode)
{
  struct android_unix_vnode *vp;

  vp = (struct android_unix_vnode *) vnode;
  return mkdir (vp->name, mode);
}

static int
android_unix_chmod (struct android_vnode *vnode, mode_t mode,
		    int flags)
{
  struct android_unix_vnode *vp;

  vp = (struct android_unix_vnode *) vnode;
  return fchmodat (AT_FDCWD, vp->name, mode, flags);
}

static ssize_t
android_unix_readlink (struct android_vnode *vnode, char *buffer,
		       size_t size)
{
  struct android_unix_vnode *vp;

  vp = (struct android_unix_vnode *) vnode;
  return readlink (vp->name, buffer, size);
}

static struct dirent *
android_unix_readdir (struct android_vdir *vdir)
{
  struct android_unix_vdir *dir;

  dir = (struct android_unix_vdir *) vdir;
  return readdir (dir->directory);
}

static void
android_unix_closedir (struct android_vdir *vdir)
{
  struct android_unix_vdir *dir;

  dir = (struct android_unix_vdir *) vdir;
  closedir (dir->directory);
  xfree (vdir);
}

static int
android_unix_dirfd (struct android_vdir *vdir)
{
  struct android_unix_vdir *dir;

  dir = (struct android_unix_vdir *) vdir;
  return dirfd (dir->directory);
}

static struct android_vdir *
android_unix_opendir (struct android_vnode *vnode)
{
  struct android_unix_vnode *vp;
  struct android_unix_vdir *dir;
  DIR *directory;

  /* Try to opendir the vnode.  */
  vp = (struct android_unix_vnode *) vnode;
  directory = opendir (vp->name);

  if (!directory)
    return NULL;

  dir = xmalloc (sizeof *dir);
  dir->vdir.readdir = android_unix_readdir;
  dir->vdir.closedir = android_unix_closedir;
  dir->vdir.dirfd = android_unix_dirfd;
  dir->directory = directory;
  return &dir->vdir;
}



/* Asset directory handling functions.  ``directory-tree'' is a file in
   the root of the assets directory describing its contents.

   See lib-src/asset-directory-tool for more details.  */

/* The Android directory tree.  */
static const char *directory_tree;

/* The size of the directory tree.  */
static size_t directory_tree_size;

/* The asset manager being used.  */
static AAssetManager *asset_manager;

/* Read an unaligned (32-bit) long from the address POINTER.  */

static unsigned int
android_extract_long (const char *pointer)
{
  unsigned int number;

  memcpy (&number, pointer, sizeof number);
  return number;
}

/* Scan to the file FILE in the asset directory tree.  Return a
   pointer to the end of that file (immediately before any children)
   in the directory tree, or NULL if that file does not exist.

   If returning non-NULL, also return the offset to the end of the
   last subdirectory or file in *LIMIT_RETURN.  LIMIT_RETURN may be
   NULL.

   FILE must have less than 11 levels of nesting.  If it ends with a
   trailing slash, then NULL will be returned if it is not actually a
   directory.  */

static const char *
android_scan_directory_tree (const char *file, size_t *limit_return)
{
  char *token, *saveptr, *copy, *start, *max, *limit;
  size_t token_length, ntokens, i, len;
  char *tokens[20];

  USE_SAFE_ALLOCA;

  /* Skip past the 5 or 9 byte header.  */
#if !OLD_ANDROID_ASSETS
  start = (char *) directory_tree + 5;
#else /* OLD_ANDROID_ASSETS */
  start = (char *) directory_tree + 9;
#endif /* OLD_ANDROID_ASSETS */

  /* Figure out the current limit.  */
  limit = (char *) directory_tree + directory_tree_size;

  /* Now, split `file' into tokens, with the delimiter being the file
     name separator.  Look for the file and seek past it.  Create a copy
     of FILE for the enjoyment of `strtok_r'.  */

  ntokens = 0;
  saveptr = NULL;
  len = strlen (file) + 1;
  copy = SAFE_ALLOCA (len);
  memcpy (copy, file, len);
  memset (tokens, 0, sizeof tokens);

  while ((token = strtok_r (copy, "/", &saveptr)))
    {
      copy = NULL;

      /* Make sure ntokens is within bounds.  */
      if (ntokens == ARRAYELTS (tokens))
	goto fail;

      len = strlen (token) + 1;
      tokens[ntokens] = SAFE_ALLOCA (len);
      memcpy (tokens[ntokens], token, len);
      ntokens++;
    }

  /* If there are no tokens, just return the start of the directory
     tree.  */

  if (!ntokens)
    {
      SAFE_FREE ();

      /* Return the size of the directory tree as the limit.
         Do not subtract the initial header bytes, as the limit
         is an offset from the start of the file.  */

      if (limit_return)
	*limit_return = directory_tree_size;

      return start;
    }

  /* Loop through tokens, indexing the directory tree each time.  */

  for (i = 0; i < ntokens; ++i)
    {
      token = tokens[i];

      /* Figure out how many bytes to compare.  */
      token_length = strlen (token);

    again:

      /* If this would be past the directory, return NULL.  */
      if (start + token_length > limit)
	goto fail;

      /* Now compare the file name.  */
      if (!memcmp (start, token, token_length))
	{
	  /* They probably match.  Find the NULL byte.  It must be
	     either one byte past start + token_length, with the last
	     byte a trailing slash (indicating that it is a directory),
	     or just start + token_length.  Return 4 or 8 bytes past the
	     next NULL byte.  */

	  max = memchr (start, 0, limit - start);

	  if (max != start + token_length
	      && !(max == start + token_length + 1
		   && *(max - 1) == '/'))
	    goto false_positive;

	  /* Return it if it exists and is in range, and this is the
	     last token.  Otherwise, set it as start and the limit as
	     start + the offset and continue the loop.  */

	  if (max && max + (OLD_ANDROID_ASSETS ? 9 : 5) <= limit)
	    {
	      if (i < ntokens - 1)
		{
		  start = max + (OLD_ANDROID_ASSETS ? 9 : 5);
		  limit = ((char *) directory_tree
			   + android_extract_long (max + (OLD_ANDROID_ASSETS
							  ? 5 : 1)));

		  /* Make sure limit is still in range.  */
		  if (limit > directory_tree + directory_tree_size
		      || start > directory_tree + directory_tree_size)
		    goto fail;

		  continue;
		}

	      /* Now see if max is not a directory and file is.  If
	         file is a directory, then return NULL.  */
	      if (*(max - 1) != '/' && file[strlen (file) - 1] == '/')
		max = NULL;
	      else
		{
		  /* Figure out the limit.  */
		  if (limit_return)
		    *limit_return
		      = android_extract_long (max + (OLD_ANDROID_ASSETS
						     ? 5 : 1));

		  /* Go to the end of this file.  */
		  max += (OLD_ANDROID_ASSETS ? 9 : 5);
		}

	      SAFE_FREE ();
	      return max;
	    }

	  /* Return NULL otherwise.  */
	  __android_log_print (ANDROID_LOG_WARN, __func__,
			       "could not scan to end of directory tree"
			       ": %s", file);
	  goto fail;
	}

    false_positive:

      /* No match was found.  Set start to the next sibling and try
	 again.  */

      start = memchr (start, 0, limit - start);

      if (!start || start + (OLD_ANDROID_ASSETS ? 9 : 5) > limit)
	goto fail;

      start = ((char *) directory_tree
	       + android_extract_long (start
				       + (OLD_ANDROID_ASSETS ? 5 : 1)));

      /* Make sure start is still in bounds.  */

      if (start > limit)
	goto fail;

      /* Continue the loop.  */
      goto again;
    }

 fail:
  SAFE_FREE ();
  return NULL;
}

/* Return whether or not the directory tree entry DIR is a
   directory.

   DIR should be a value returned by
   `android_scan_directory_tree'.  */

static bool
android_is_directory (const char *dir)
{
  /* If the directory is the directory tree, then it is a
     directory.  */
  if (dir == directory_tree + (OLD_ANDROID_ASSETS ? 9 : 5))
    return true;

#if !OLD_ANDROID_ASSETS
  /* Otherwise, look 5 bytes behind.  If it is `/', then it is a
     directory.  */
  return (dir - 6 >= directory_tree
	  && *(dir - 6) == '/');
#else /* OLD_ANDROID_ASSETS */
  /* Otherwise, look 9 bytes behind.  If it is `/', then it is a
     directory.  */
  return (dir - 10 >= directory_tree
	  && *(dir - 10) == '/');
#endif /* OLD_ANDROID_ASSETS */
}

/* Initialize asset retrieval.  ENV should be a JNI environment for
   the Emacs thread, and MANAGER should be a local reference to a Java
   asset manager object created for the Emacs service context.  */

static void
android_init_assets (JNIEnv *env, jobject manager)
{
  AAsset *asset;

  /* Set the asset manager.  */
  asset_manager = AAssetManager_fromJava (env, manager);

  /* Initialize the directory tree.  */
  asset = AAssetManager_open (asset_manager, "directory-tree",
			      AASSET_MODE_BUFFER);

  if (!asset)
    {
      __android_log_print (ANDROID_LOG_FATAL, __func__,
			   "Failed to open directory tree");
      emacs_abort ();
    }

  directory_tree = AAsset_getBuffer (asset);

  if (!directory_tree)
    emacs_abort ();

  /* Now figure out how big the directory tree is, and compare the
     first few bytes.  */
  directory_tree_size = AAsset_getLength (asset);
#if !OLD_ANDROID_ASSETS
  if (directory_tree_size < 5
      || memcmp (directory_tree, "EMACS", 5))
    {
      __android_log_print (ANDROID_LOG_FATAL, __func__,
			   "Directory tree has bad magic");
      emacs_abort ();
    }
#else /* OLD_ANDROID_ASSETS */
  if (directory_tree_size < 9
      || memcmp (directory_tree, "EMACS____", 9))
    {
      __android_log_print (ANDROID_LOG_FATAL, __func__,
			   "Directory tree has bad magic");
      emacs_abort ();
    }
#endif /* OLD_ANDROID_ASSETS */

  /* Hold a VM reference to the asset manager to prevent the native
     object from being deleted.  */
  (*env)->NewGlobalRef (env, manager);

  /* Abort if there's no more memory for the global reference.  */
  if ((*env)->ExceptionCheck (env))
    abort ();
}



/* Asset-to-file descriptor conversion.  */

/* Pointer to the `ASharedMemory_create' function which is loaded
   dynamically.  */
static int (*asharedmemory_create) (const char *, size_t);

/* Do the same as android_hack_asset_fd, but use an unlinked temporary
   file to cater to old Android kernels where ashmem files are not
   readable.  */

static int
android_hack_asset_fd_fallback (AAsset *asset)
{
  int fd;
  char filename[PATH_MAX];
  size_t size;
  void *mem;

  /* Assets must be small enough to fit in size_t, if off_t is
     larger.  */
  size = AAsset_getLength (asset);

  /* Get an unlinked file descriptor from a file in the cache
     directory, which is guaranteed to only be written to by Emacs.
     Creating an ashmem file descriptor and reading from it doesn't
     work on these old Android versions.  */

  snprintf (filename, PATH_MAX, "%s/temp~unlinked.%d",
	    android_cache_dir, getpid ());
  fd = open (filename, O_CREAT | O_RDWR | O_TRUNC,
	     S_IRUSR | S_IWUSR);

  if (fd < 0)
    return -1;

  if (unlink (filename) && errno != ENOENT)
    goto fail;

  if (ftruncate (fd, size))
    goto fail;

  mem = mmap (NULL, size, PROT_WRITE, MAP_SHARED, fd, 0);
  if (mem == MAP_FAILED)
    {
      __android_log_print (ANDROID_LOG_ERROR, __func__,
			   "mmap: %s", strerror (errno));
      goto fail;
    }

  if (AAsset_read (asset, mem, size) != size)
    {
      /* Too little was read.  Close the file descriptor and
	 report an error.  */
      __android_log_print (ANDROID_LOG_ERROR, __func__,
			   "AAsset_read: %s", strerror (errno));
      goto fail;
    }

  munmap (mem, size);
  return fd;

 fail:
  close (fd);
  return -1;
}

/* Return whether or not shared memory file descriptors can also be
   read from, and are thus suitable for creating asset files.

   This does not work on some ancient Android systems running old
   versions of the kernel.  */

static bool
android_detect_ashmem (void)
{
  int fd, rc;
  void *mem;
  char test_buffer[10];

  memcpy (test_buffer, "abcdefghi", 10);

  /* Create the file descriptor to be used for the test.  */

  /* Android 28 and earlier let Emacs access /dev/ashmem directly, so
     prefer that over using ASharedMemory.  */

  if (android_get_current_api_level () <= 28)
    {
      fd = open ("/dev/ashmem", O_RDWR);

      if (fd < 0)
	return false;

      /* An empty name means the memory area will exist until the file
	 descriptor is closed, because no other process can
	 attach.  */
      rc = ioctl (fd, ASHMEM_SET_NAME, "");

      if (rc < 0)
	{
	  close (fd);
	  return false;
	}

      rc = ioctl (fd, ASHMEM_SET_SIZE, sizeof test_buffer);

      if (rc < 0)
	{
	  close (fd);
	  return false;
	}
    }
  else
    {
      /* On the other hand, SELinux restrictions on Android 29 and
	 later require that Emacs use a system service to obtain
	 shared memory.  Load this dynamically, as this service is not
	 available on all versions of the NDK.  */

      if (!asharedmemory_create)
	{
	  *(void **) (&asharedmemory_create)
	    = dlsym (RTLD_DEFAULT, "ASharedMemory_create");

	  if (!asharedmemory_create)
	    {
	      __android_log_print (ANDROID_LOG_FATAL, __func__,
				   "dlsym: %s\n",
				   strerror (errno));
	      emacs_abort ();
	    }
	}

      fd = (*asharedmemory_create) ("", sizeof test_buffer);

      if (fd < 0)
	return false;
    }

  /* Now map the resource and write the test contents.  */

  mem = mmap (NULL, sizeof test_buffer, PROT_WRITE,
	      MAP_SHARED, fd, 0);
  if (mem == MAP_FAILED)
    {
      close (fd);
      return false;
    }

  /* Copy over the test contents.  */
  memcpy (mem, test_buffer, sizeof test_buffer);

  /* Return anyway even if munmap fails.  */
  munmap (mem, sizeof test_buffer);

  /* Try to read the content back into test_buffer.  If this does not
     compare equal to the original string, or the read fails, then
     ashmem descriptors are not readable on this system.  */

  if ((read (fd, test_buffer, sizeof test_buffer)
       != sizeof test_buffer)
      || memcmp (test_buffer, "abcdefghi", sizeof test_buffer))
    {
      __android_log_print (ANDROID_LOG_WARN, __func__,
			   "/dev/ashmem does not produce real"
			   " temporary files on this system, so"
			   " Emacs will fall back to creating"
			   " unlinked temporary files.");
      close (fd);
      return false;
    }

  close (fd);
  return true;
}

/* Get a file descriptor backed by a temporary in-memory file for the
   given asset.  */

static int
android_hack_asset_fd (AAsset *asset)
{
  static bool ashmem_readable_p;
  static bool ashmem_initialized;
  int fd, rc;
  unsigned char *mem;
  size_t size;

  /* The first time this function is called, try to determine whether
     or not ashmem file descriptors can be read from.  */

  if (!ashmem_initialized)
    ashmem_readable_p
      = android_detect_ashmem ();
  ashmem_initialized = true;

  /* If it isn't, fall back.  */

  if (!ashmem_readable_p)
    return android_hack_asset_fd_fallback (asset);

  /* Assets must be small enough to fit in size_t, if off_t is
     larger.  */
  size = AAsset_getLength (asset);

  /* Android 28 and earlier let Emacs access /dev/ashmem directly, so
     prefer that over using ASharedMemory.  */

  if (android_get_current_api_level () <= 28)
    {
      fd = open ("/dev/ashmem", O_RDWR);

      if (fd < 0)
	return -1;

      /* An empty name means the memory area will exist until the file
	 descriptor is closed, because no other process can
	 attach.  */
      rc = ioctl (fd, ASHMEM_SET_NAME, "");

      if (rc < 0)
	{
	  __android_log_print (ANDROID_LOG_ERROR, __func__,
			       "ioctl ASHMEM_SET_NAME: %s",
			       strerror (errno));
	  close (fd);
	  return -1;
	}

      rc = ioctl (fd, ASHMEM_SET_SIZE, size);

      if (rc < 0)
	{
	  __android_log_print (ANDROID_LOG_ERROR, __func__,
			       "ioctl ASHMEM_SET_SIZE: %s",
			       strerror (errno));
	  close (fd);
	  return -1;
	}

      if (!size)
	return fd;

      /* Now map the resource.  */
      mem = mmap (NULL, size, PROT_WRITE, MAP_SHARED, fd, 0);
      if (mem == MAP_FAILED)
	{
	  __android_log_print (ANDROID_LOG_ERROR, __func__,
			       "mmap: %s", strerror (errno));
	  close (fd);
	  return -1;
	}

      if (AAsset_read (asset, mem, size) != size)
	{
	  /* Too little was read.  Close the file descriptor and
	     report an error.  */
	  __android_log_print (ANDROID_LOG_ERROR, __func__,
			       "AAsset_read: %s", strerror (errno));
	  close (fd);
	  return -1;
	}

      /* Return anyway even if munmap fails.  */
      munmap (mem, size);
      return fd;
    }

  /* On the other hand, SELinux restrictions on Android 29 and later
     require that Emacs use a system service to obtain shared memory.
     Load this dynamically, as this service is not available on all
     versions of the NDK.  */

  if (!asharedmemory_create)
    {
      *(void **) (&asharedmemory_create)
	= dlsym (RTLD_DEFAULT, "ASharedMemory_create");

      if (!asharedmemory_create)
	{
	  __android_log_print (ANDROID_LOG_FATAL, __func__,
			       "dlsym: %s\n",
			       strerror (errno));
	  emacs_abort ();
	}
    }

  fd = (*asharedmemory_create) ("", size);

  if (fd < 0)
    {
      __android_log_print (ANDROID_LOG_ERROR, __func__,
			   "ASharedMemory_create: %s",
			   strerror (errno));
      return -1;
    }

  /* Now map the resource.  */
  mem = mmap (NULL, size, PROT_WRITE, MAP_SHARED, fd, 0);
  if (mem == MAP_FAILED)
    {
      __android_log_print (ANDROID_LOG_ERROR, __func__,
			   "mmap: %s", strerror (errno));
      close (fd);
      return -1;
    }

  if (AAsset_read (asset, mem, size) != size)
    {
      /* Too little was read.  Close the file descriptor and
	 report an error.  */
      __android_log_print (ANDROID_LOG_ERROR, __func__,
			   "AAsset_read: %s", strerror (errno));
      close (fd);
      return -1;
    }

  /* Return anyway even if munmap fails.  */
  munmap (mem, size);
  return fd;
}



/* ``Asset file system'' vnode implementation.  These vnodes map to
   asset files within the application package, provided by the Android
   ``asset manager''.  */

struct android_afs_vnode
{
  /* The vnode data itself.  */
  struct android_vnode vnode;

  /* Length of the name without a trailing null byte.  */
  size_t name_length;

  /* Name of the vnode.  */
  char *name;
};

struct android_afs_vdir
{
  /* The directory function table.  */
  struct android_vdir vdir;

  /* The next directory stream in `all_afs_vdirs'.  */
  struct android_afs_vdir *next;

  /* Pointer to the directory in directory_tree.  */
  char *asset_dir;

  /* And the end of the files in asset_dir.  */
  char *asset_limit;

  /* Path to the directory relative to /.  */
  char *asset_file;

  /* File descriptor representing this directory stream, or NULL.  */
  int fd;
};

struct android_afs_open_fd
{
  /* The next table entry.  */
  struct android_afs_open_fd *next;

  /* The open file descriptor.  */
  int fd;

  /* The stat buffer associated with this entry.  */
  struct stat statb;
};

static struct android_vnode *android_afs_name (struct android_vnode *,
					       char *, size_t);
static int android_afs_open (struct android_vnode *, int,
			     mode_t, bool, int *, AAsset **);
static void android_afs_close (struct android_vnode *);
static int android_afs_unlink (struct android_vnode *);
static int android_afs_symlink (const char *, struct android_vnode *);
static int android_afs_rmdir (struct android_vnode *);
static int android_afs_rename (struct android_vnode *,
			       struct android_vnode *, bool);
static int android_afs_stat (struct android_vnode *, struct stat *, int);
static int android_afs_access (struct android_vnode *, int);
static int android_afs_mkdir (struct android_vnode *, mode_t);
static int android_afs_chmod (struct android_vnode *, mode_t, int);
static ssize_t android_afs_readlink (struct android_vnode *, char *,
				     size_t);
static struct android_vdir *android_afs_opendir (struct android_vnode *);

/* Vector of VFS operations associated with asset VFS nodes.  */

static struct android_vops afs_vfs_ops =
  {
    android_afs_name,
    android_afs_open,
    android_afs_close,
    android_afs_unlink,
    android_afs_symlink,
    android_afs_rmdir,
    android_afs_rename,
    android_afs_stat,
    android_afs_access,
    android_afs_mkdir,
    android_afs_chmod,
    android_afs_readlink,
    android_afs_opendir,
  };

/* Chain consisting of all open asset directory streams.  */
static struct android_afs_vdir *all_afs_vdirs;

/* List linking open file descriptors to asset information.  This
   assumes Emacs does not use dup on regular files.  */
static struct android_afs_open_fd *afs_file_descriptors;

static struct android_vnode *
android_afs_name (struct android_vnode *vnode, char *name,
		  size_t length)
{
  size_t j;
  char *remainder, *fill;
  struct android_afs_vnode *vp, *input;
  struct android_afs_vnode temp;

  input = (struct android_afs_vnode *) vnode;

  /* Canonicalize NAME.  */
  remainder = android_vfs_canonicalize_name (name, &length);

  /* If remainder is set, it's a name relative to the parent
     vnode.  */
  if (remainder)
    goto parent_vnode;

  /* Allocate a new vnode.  */
  vp = xmalloc (sizeof *vp);

  /* See the specified name is empty.  */

  if (length < 1)
    {
      memcpy (vp, vnode, sizeof *vp);
      vp->name = xstrdup (vp->name);
      return &vp->vnode;
    }

  /* Recompute length.  */
  vp->vnode.ops = &afs_vfs_ops;
  vp->vnode.type = ANDROID_VNODE_AFS;
  vp->vnode.flags = 0;

  /* Generate the new name of the vnode.  Remove any trailing slash
     from vp->name.  */

  vp->name_length = input->name_length + length;
  vp->name = xmalloc (vp->name_length + 2);

  /* Copy the parent name over.  */
  fill = mempcpy (vp->name, input->name, input->name_length);

  /* Check if it contains a trailing slash.  input->name cannot be
     empty, as the root vnode's name is `/'.  */

  if (fill[-1] != '/' && *name != '/')
    /* If not, append a trailing slash and adjust vp->name_length
       correspondingly.  */
    *fill++ = '/', vp->name_length++;
  else if (fill[-1] == '/' && *name == '/')
    /* If name has a leading slash and fill does too, move fill
       backwards so that name's slash will override that of fill.  */
    fill--, vp->name_length--;

  /* Now copy NAME.  */
  fill = mempcpy (fill, name, length);

  /* And NULL terminate fill.  */
  *fill = '\0';
  return &vp->vnode;

 parent_vnode:
  /* .. was encountered and the parent couldn't be found through
     stripping off preceding components.

     Find the parent vnode and name the rest of NAME starting from
     there.  */

  if (input->name_length == 1)
    /* This is the vnode representing the /assets directory... */
    vnode = &root_vnode.vnode;
  else
    {
      /* Create a temporary asset vnode within the parent and use it
         instead.  First, establish the length of vp->name before its
         last component.  */

      for (j = input->name_length - 1; j; --j)
	{
	  if (input->name[j - 1] == '/')
	    break;
	}

      /* There must be at least one leading directory separator in an
	 asset vnode's `name' field.  */

      if (!j)
	abort ();

      /* j is now the length of the string minus the size of its last
	 component.  Create a temporary vnode with that as its
	 name.  */

      temp.vnode.ops = &afs_vfs_ops;
      temp.vnode.type = ANDROID_VNODE_AFS;
      temp.vnode.flags = 0;
      temp.name_length = j;
      temp.name = xmalloc (j + 1);
      fill = mempcpy (temp.name, input->name, j);
      *fill = '\0';

      /* Search for the remainder of NAME relative to its parent.  */
      vnode = android_afs_name (&temp.vnode, remainder,
				strlen (remainder));
      xfree (temp.name);
      return vnode;
    }

  return (*vnode->ops->name) (vnode, remainder, strlen (remainder));
}

/* Find the vnode designated by the normalized NAME relative to the
   root of the asset file system.  NAME may be modified, and must be
   LENGTH bytes long, excluding its terminating NULL byte.  */

static struct android_vnode *
android_afs_initial (char *name, size_t length)
{
  struct android_afs_vnode temp;

  /* Create a temporary vnode at the root of the asset file
     system.  */

  temp.vnode.ops = &afs_vfs_ops;
  temp.vnode.type = ANDROID_VNODE_AFS;
  temp.vnode.flags = 0;
  temp.name_length = 1;
  temp.name = (char *) "/";

  /* Try to name this vnode.  If NAME is empty, it will be duplicated
     instead.  */
  return android_afs_name (&temp.vnode, name, length);
}

/* Make FD close-on-exec.  If any system call fails, do not abort, but
   log a warning to the system log.  */

static void
android_close_on_exec (int fd)
{
  int flags, rc;

  flags = fcntl (fd, F_GETFD);

  if (flags < 0)
    {
      __android_log_print (ANDROID_LOG_WARN, __func__,
			   "fcntl: %s", strerror (errno));
      return;
    }

  rc = fcntl (fd, F_SETFD, flags | O_CLOEXEC);

  if (rc < 0)
    {
      __android_log_print (ANDROID_LOG_WARN, __func__,
			   "fcntl: %s", strerror (errno));
      return;
    }
}

static int
android_afs_open (struct android_vnode *vnode, int flags,
		  mode_t mode, bool asset_p, int *fd_return,
		  AAsset **asset_return)
{
  AAsset *asset;
  struct android_afs_vnode *vp;
  const char *asset_dir;
  int fd;
  struct android_afs_open_fd *info;

  vp = (struct android_afs_vnode *) vnode;

  /* Return suitable error indications for unsupported file
     operations.  */

  if ((flags & O_WRONLY) || (flags & O_RDWR))
    {
      errno = EROFS;
      return -1;
    }

  if (flags & O_DIRECTORY)
    {
      errno = ENOSYS;
      return -1;
    }

  /* Now try to open this asset.  Asset manager APIs expect there to
     be no trailing directory separator.  */
  asset = AAssetManager_open (asset_manager, vp->name + 1,
			      AASSET_MODE_STREAMING);

  /* If it can't be opened, return an error indication.  */

  if (!asset)
    {
      /* Scan the directory tree for this file.  */
      asset_dir = android_scan_directory_tree (vp->name, NULL);

      /* Default errno to ENOTENT.  */
      errno = ENOENT;

      /* Maybe the caller wants to open a directory vnode as a
	 file?  */

      if (asset_dir && android_is_directory (asset_dir))
	/* In that case, set errno to ENOSYS.  */
	errno = ENOSYS;

      return -1;
    }

  /* An asset has been opened.  If the caller wants a file descriptor,
     a temporary one must be created and the file contents read
     inside.  */

  if (!asset_p)
    {
      /* Create a shared memory file descriptor containing the asset
	 contents.

         The documentation misleads people into thinking that
         AAsset_openFileDescriptor does precisely this.  However, it
         instead returns an offset into any uncompressed assets in the
         ZIP archive.  This cannot be found in its documentation.  */

      fd = android_hack_asset_fd (asset);

      if (fd == -1)
	{
	  AAsset_close (asset);
	  errno = EIO;
	  return -1;
	}

      /* If O_CLOEXEC is specified, make the file descriptor close on
	 exec too.  */

      if (flags & O_CLOEXEC)
	android_close_on_exec (fd);

      /* Keep a record linking ``hacked'' file descriptors with
	 their file status.  */
      info = xzalloc (sizeof *info);
      info->fd   = fd;
      info->next = afs_file_descriptors;

      /* Fill in some information that will be reported to
	 callers of android_fstat, among others.  */
      info->statb.st_mode = S_IFREG | S_IRUSR | S_IRGRP | S_IROTH;

      /* Owned by root.  */
      info->statb.st_uid = 0;
      info->statb.st_gid = 0;

      /* Concoct a nonexistent device and an inode number.  */
      info->statb.st_dev = -1;
      info->statb.st_ino = 0;

      /* Size of the file.  */
      info->statb.st_size = AAsset_getLength (asset);

      /* If the installation date can be ascertained, return that as
	 the file's modification time.  */

      if (timespec_valid_p (emacs_installation_time))
	{
#ifdef STAT_TIMESPEC
	  STAT_TIMESPEC (&info->statb, st_mtim) = emacs_installation_time;
#else /* !STAT_TIMESPEC */
          /* Headers supplied by the NDK r10b contain a `struct stat'
	     without POSIX fields for nano-second timestamps.  */
	  info->statb.st_mtime = emacs_installation_time.tv_sec;
	  info->statb.st_mtime_nsec = emacs_installation_time.tv_nsec;
#endif /* STAT_TIMESPEC */
	}

      /* Chain info onto afs_file_descriptors.  */
      afs_file_descriptors = info;

      AAsset_close (asset);

      /* Return the file descriptor.  */
      *fd_return = fd;
      return 0;
    }

  /* Return the asset itself.  */
  *asset_return = asset;
  return 1;
}

static void
android_afs_close (struct android_vnode *vnode)
{
  struct android_afs_vnode *vp;
  int save_errno;

  save_errno = errno;
  vp = (struct android_afs_vnode *) vnode;
  xfree (vp->name);
  xfree (vp);
  errno = save_errno;
}

static int
android_afs_unlink (struct android_vnode *vnode)
{
  const char *dir;
  struct android_afs_vnode *vp;

  /* If the vnode already exists, return EROFS.  Else, return
     ENOENT.  */

  vp = (struct android_afs_vnode *) vnode;
  dir = android_scan_directory_tree (vp->name, NULL);

  if (dir)
    errno = EROFS;
  else
    errno = ENOENT;

  return -1;
}

static int
android_afs_symlink (const char *linkname, struct android_vnode *vnode)
{
  struct android_afs_vnode *vp;

  /* If this vnode already exists, return EEXIST.  */
  vp = (struct android_afs_vnode *) vnode;

  if (android_scan_directory_tree (vp->name, NULL))
    {
      errno = EEXIST;
      return -1;
    }

  /* Symlinks aren't supported on this (read-only) ``file system'',
     so return -1 with EROFS.  */
  errno = EROFS;
  return -1;
}

static int
android_afs_rmdir (struct android_vnode *vnode)
{
  const char *dir;
  struct android_afs_vnode *vp;

  /* If the vnode already exists and is a directory, return EROFS.
     Else, return ENOTDIR or ENOENT.  */

  vp = (struct android_afs_vnode *) vnode;
  dir = android_scan_directory_tree (vp->name, NULL);

  if (dir && android_is_directory (dir))
    errno = EROFS;
  else if (dir)
    errno = ENOTDIR;
  else
    errno = ENOENT;

  return -1;
}

static int
android_afs_rename (struct android_vnode *src, struct android_vnode *dst,
		    bool keep_existing)
{
  /* If src and dst are different kinds of vnodes, return EXDEV.
     Else, return EROFS.  */

  errno = EROFS;
  if (src->type != dst->type)
    errno = EXDEV;

  return -1;
}

static int
android_afs_stat (struct android_vnode *vnode, struct stat *statb,
		  int flags)
{
  const char *dir;
  struct android_afs_vnode *vp;
  AAsset *asset_desc;

  /* Scan for the vnode to see whether or not it exists.  */

  vp = (struct android_afs_vnode *) vnode;
  dir = android_scan_directory_tree (vp->name, NULL);

  if (!dir)
    {
      /* Return ENOENT; whether the lookup failed because directory
	 components within vp->path weren't really directories is not
	 important to Emacs's error reporting.  */
      errno = ENOENT;
      return -1;
    }

  if (android_is_directory (dir))
    {
      memset (statb, 0, sizeof *statb);

      /* Fill in the stat buffer.  */
      statb->st_mode = S_IFDIR | S_IRUSR | S_IRGRP | S_IROTH;

      /* Grant search permissions as well.  */
      statb->st_mode |= S_IXUSR | S_IXGRP | S_IXOTH;

      /* Concoct a nonexistent device and an inode number.  */
      statb->st_dev = -1;
      statb->st_ino = 0;
      goto set_file_times;
    }

  /* AASSET_MODE_STREAMING is fastest here.  */
  asset_desc = AAssetManager_open (asset_manager, vp->name + 1,
				   AASSET_MODE_STREAMING);

  if (!asset_desc)
    {
      /* If the asset exists in the directory tree but can't be
	 located by the asset manager, report OOM.  */
      errno = ENOMEM;
      return 1;
    }

  memset (statb, 0, sizeof *statb);

  /* Fill in the stat buffer.  */
  statb->st_mode = S_IFREG | S_IRUSR | S_IRGRP | S_IROTH;
  statb->st_dev = -1;
  statb->st_ino = 0;
  statb->st_size = AAsset_getLength (asset_desc);

  /* Close the asset.  */
  AAsset_close (asset_desc);

 set_file_times:

  /* If the installation date can be ascertained, return that as the
     file's modification time.  */

  if (timespec_valid_p (emacs_installation_time))
    {
#ifdef STAT_TIMESPEC
      STAT_TIMESPEC (statb, st_mtim) = emacs_installation_time;
#else /* !STAT_TIMESPEC */
      /* Headers supplied by the NDK r10b contain a `struct stat'
	 without POSIX fields for nano-second timestamps.  */
      statb->st_mtime = emacs_installation_time.tv_sec;
      statb->st_mtime_nsec = emacs_installation_time.tv_nsec;
#endif /* STAT_TIMESPEC */
    }

  return 0;
}

static int
android_afs_access (struct android_vnode *vnode, int mode)
{
  const char *dir;
  struct android_afs_vnode *vp;

  /* Validate MODE.  */

  if (mode != F_OK && !(mode & (W_OK | X_OK | R_OK)))
    {
      errno = EINVAL;
      return -1;
    }

  /* Scan for the vnode to see whether or not it exists.  */

  vp = (struct android_afs_vnode *) vnode;
  dir = android_scan_directory_tree (vp->name, NULL);

  if (dir)
    {
      /* It exists.  If MODE contains W_OK or X_OK, return
	 EACCESS.  */

      if (mode & (W_OK | X_OK))
	{
	  errno = EACCES;
	  return -1;
	}

      /* If vp->name is a directory and DIR isn't, return ENOTDIR.  */

      if (vp->name[vp->name_length] == '/'
	  && !android_is_directory (dir))
	{
	  errno = ENOTDIR;
	  return -1;
	}

      return 0;
    }

  errno = ENOENT;
  return -1;
}

static int
android_afs_mkdir (struct android_vnode *vnode, mode_t mode)
{
  struct android_afs_vnode *vp;
  const char *dir;

  /* If the vnode already exists, return EEXIST in lieu of EROFS.  */

  vp = (struct android_afs_vnode *) vnode;
  dir = android_scan_directory_tree (vp->name, NULL);

  if (dir)
    errno = EEXIST;
  else
    errno = EROFS;

  return -1;
}

static int
android_afs_chmod (struct android_vnode *vnode, mode_t mode,
		   int flags)
{
  errno = EROFS;
  return -1;
}

static ssize_t
android_afs_readlink (struct android_vnode *vnode, char *buffer,
		      size_t size)
{
  struct android_afs_vnode *vp;
  const char *dir;

  vp = (struct android_afs_vnode *) vnode;
  dir = android_scan_directory_tree (vp->name, NULL);

  /* As there are no symlinks in /assets, just return -1 with errno
     set to a reasonable value contingent upon whether VP->name
     actually exists.  */

  if (dir)
    errno = EINVAL;
  else
    errno = ENOENT;

  return -1;
}

static struct dirent *
android_afs_readdir (struct android_vdir *vdir)
{
  static struct dirent dirent;
  const char *last;
  struct android_afs_vdir *dir;

  dir = (struct android_afs_vdir *) vdir;

  /* There are no more files to read.  */
  if (dir->asset_dir >= dir->asset_limit)
    return NULL;

  /* Otherwise, scan forward looking for the next NULL byte.  */
  last = memchr (dir->asset_dir, 0,
		 dir->asset_limit - dir->asset_dir);

  /* No more NULL bytes remain.  */
  if (!last)
    return NULL;

  /* Forward last past the NULL byte.  */
  last++;

  /* Make sure it is still within the directory tree.  */
  if (last >= directory_tree + directory_tree_size)
    return NULL;

  /* Now, fill in the dirent with the name.  */
  memset (&dirent, 0, sizeof dirent);
  dirent.d_ino = 0;
  dirent.d_off = 0;
  dirent.d_reclen = sizeof dirent;

  /* Note that dir->asset_dir is actually a NULL terminated
     string.  */
  memcpy (dirent.d_name, dir->asset_dir,
	  MIN (sizeof dirent.d_name,
	       last - dir->asset_dir));
  dirent.d_name[sizeof dirent.d_name - 1] = '\0';

  /* Strip off the trailing slash, if any.  */
  if (dirent.d_name[MIN (sizeof dirent.d_name,
			 last - dir->asset_dir)
		    - 2] == '/')
    dirent.d_name[MIN (sizeof dirent.d_name,
		       last - dir->asset_dir)
		  - 2] = '\0';

  /* If this is not a directory, return DT_REG.  Otherwise, return
     DT_DIR.  */

  if (last - 2 >= directory_tree && last[-2] == '/')
    dirent.d_type = DT_DIR;
  else
    dirent.d_type = DT_REG;

  /* Forward dir->asset_dir to the file past last.  */
#if !OLD_ANDROID_ASSETS
  dir->asset_dir = ((char *) directory_tree
		    + android_extract_long ((char *) last));
#else /* OLD_ANDROID_ASSETS */
  dir->asset_dir = ((char *) directory_tree
		    + android_extract_long ((char *) last + 4));
#endif /* OLD_ANDROID_ASSETS */

  return &dirent;
}

static void
android_afs_closedir (struct android_vdir *vdir)
{
  struct android_afs_vdir *dir, **next, *tem;

  dir = (struct android_afs_vdir *) vdir;

  /* If the ``directory file descriptor'' has been opened, close
     it.  */

  if (dir->fd != -1)
    close (dir->fd);

  xfree (dir->asset_file);

  /* Now unlink this directory.  */

  for (next = &all_afs_vdirs; (tem = *next);)
    {
      if (tem == dir)
	*next = dir->next;
      else
	next = &(*next)->next;
    }

  /* Free the directory itself.  */

  xfree (dir);
}

static int
android_afs_dirfd (struct android_vdir *vdir)
{
  struct android_afs_vdir *dir;

  dir = (struct android_afs_vdir *) vdir;

  /* Since `android_afs_opendir' tries to avoid opening a file
     descriptor if readdir isn't called, dirfd can fail if open fails.

     open sets errno to a set of errors different from what POSIX
     stipulates for dirfd, but for ease of implementation the open
     errors are used instead.  */

  if (dir->fd >= 0)
    return dir->fd;

  dir->fd = open ("/dev/null", O_RDONLY | O_CLOEXEC);
  return dir->fd;
}

static struct android_vdir *
android_afs_opendir (struct android_vnode *vnode)
{
  char *asset_dir;
  struct android_afs_vdir *dir;
  struct android_afs_vnode *vp;
  size_t limit;

  vp = (struct android_afs_vnode *) vnode;

  /* Scan for the asset directory by vp->name.  */

  asset_dir
    = (char *) android_scan_directory_tree (vp->name, &limit);

  if (!asset_dir)
    {
      errno = ENOENT;
      return NULL;
    }

  /* Verify that asset_dir is indeed a directory.  */

  if (!android_is_directory (asset_dir))
    {
      errno = ENOTDIR;
      return NULL;
    }

  /* Fill in the directory stream.  */
  dir = xmalloc (sizeof *dir);
  dir->vdir.readdir = android_afs_readdir;
  dir->vdir.closedir = android_afs_closedir;
  dir->vdir.dirfd = android_afs_dirfd;
  dir->asset_dir = asset_dir;
  dir->asset_limit = (char *) directory_tree + limit;
  dir->fd = -1;
  dir->asset_file = xzalloc (vp->name_length + 2);
  strcpy (dir->asset_file, vp->name);

  /* Make sure dir->asset_file is terminated with /.  */
  if (dir->asset_file[vp->name_length - 1] != '/')
    dir->asset_file[vp->name_length] = '/';

  /* Make sure dir->asset_limit is within bounds.  It is a limit,
     and as such can be exactly one byte past directory_tree.  */
  if (dir->asset_limit > directory_tree + directory_tree_size)
    {
      xfree (dir->asset_file);
      xfree (dir);
      errno = EACCES;
      return NULL;
    }

  dir->next = all_afs_vdirs;
  all_afs_vdirs = dir;
  return &dir->vdir;
}

/* Return the file name corresponding to DIRFD if it is a
   ``directory'' file descriptor returned by `android_afs_dirfd' or
   NULL otherwise.  These file names are relative to the `/assets'
   directory, but with a leading separator character.  */

static char *
android_afs_get_directory_name (int dirfd)
{
  struct android_afs_vdir *dir;

  for (dir = all_afs_vdirs; dir; dir = dir->next)
    {
      if (dir->fd == dirfd && dirfd != -1)
	return dir->asset_file;
    }

  return NULL;
}



struct android_content_vdir
{
  /* The directory function table.  */
  struct android_vdir vdir;

  /* The next directory stream in `all_content_vdirs'.  */
  struct android_content_vdir *next;

  /* Pointer to the next file to return.  */
  const char **next_name;

  /* Temporary file descriptor used to identify this directory to
     at-funcs, or -1.  */
  int fd;
};

static struct android_vnode *android_authority_initial (char *, size_t);
static struct android_vnode *android_authority_initial_name (char *, size_t);
static struct android_vnode *android_saf_root_initial (char *, size_t);

/* Content provider meta-interface.  This implements a vnode at
   /content, which is a directory itself containing two additional
   directories.

   /content/storage only exists on Android 5.0 and later, and contains
   a list of each directory tree Emacs has been granted permanent
   access to through the Storage Access Framework.

   /content/by-authority and /content/by-authority-named exists on
   Android 4.4 and later; it contains no directories, but provides a
   `name' function that converts children into content URIs.  */

static struct android_vnode *android_content_name (struct android_vnode *,
						   char *, size_t);
static int android_content_open (struct android_vnode *, int,
				 mode_t, bool, int *, AAsset **);
static void android_content_close (struct android_vnode *);
static int android_content_unlink (struct android_vnode *);
static int android_content_symlink (const char *, struct android_vnode *);
static int android_content_rmdir (struct android_vnode *);
static int android_content_rename (struct android_vnode *,
				   struct android_vnode *, bool);
static int android_content_stat (struct android_vnode *, struct stat *, int);
static int android_content_access (struct android_vnode *, int);
static int android_content_mkdir (struct android_vnode *, mode_t);
static int android_content_chmod (struct android_vnode *, mode_t, int);
static ssize_t android_content_readlink (struct android_vnode *, char *,
					 size_t);
static struct android_vdir *android_content_opendir (struct android_vnode *);

/* Vector of VFS operations associated with the content VFS node.  */

static struct android_vops content_vfs_ops =
  {
    android_content_name,
    android_content_open,
    android_content_close,
    android_content_unlink,
    android_content_symlink,
    android_content_rmdir,
    android_content_rename,
    android_content_stat,
    android_content_access,
    android_content_mkdir,
    android_content_chmod,
    android_content_readlink,
    android_content_opendir,
  };

/* Table of directories contained within a top-level vnode.  */

static const char *content_directory_contents[] =
  {
    "storage", "by-authority", "by-authority-named",
  };

/* Chain consisting of all open content directory streams.  */
static struct android_content_vdir *all_content_vdirs;

static struct android_vnode *
android_content_name (struct android_vnode *vnode, char *name,
		      size_t length)
{
  char *remainder;
  struct android_vnode *vp;
  char *component_end;
  struct android_special_vnode *special;
  size_t i;
  int api;

  static struct android_special_vnode content_vnodes[] = {
    { "storage", 7, android_saf_root_initial,			},
    { "by-authority", 12, android_authority_initial,		},
    { "by-authority-named", 18, android_authority_initial_name,	},
  };

  /* Canonicalize NAME.  */
  remainder = android_vfs_canonicalize_name (name, &length);

  /* If remainder is set, it's a name relative to the root vnode.  */
  if (remainder)
    goto parent_vnode;

  /* If LENGTH is empty or NAME is a single directory separator,
     return a copy of this vnode.  */

  if (length < 1 || (*name == '/' && length == 1))
    {
      vp = xmalloc (sizeof *vp);
      memcpy (vp, vnode, sizeof *vp);
      return vp;
    }

  api = android_get_current_api_level ();

  /* If NAME starts with a directory separator, move it past that.  */

  if (*name == '/')
    name++, length -= 1;

  /* Look for the first directory separator.  */
  component_end = strchr (name, '/');

  /* If not there, use name + length.  */

  if (!component_end)
    component_end = name + length;
  else
    /* Move past the separator character.  */
    component_end++;

  /* Now, find out if the first component is a special vnode; if so,
     call its root lookup function with the rest of NAME there.  What is
     more, content files are inaccessible in the absence of a GUI.  */

  if (api < 19 || !android_init_gui)
    i = 3;
  else if (api < 21)
    i = 1;
  else
    i = 0;

  for (; i < ARRAYELTS (content_vnodes); ++i)
    {
      special = &content_vnodes[i];

      if (component_end - name == special->length
	  && !memcmp (special->name, name, special->length))
	return (*special->initial) (component_end,
				    length - special->length);

      /* Detect the case where a special is named with a trailing
	 directory separator.  */

      if (component_end - name == special->length + 1
	  && !memcmp (special->name, name, special->length)
	  && name[special->length] == '/')
	/* Make sure to include the directory separator.  */
	return (*special->initial) (component_end - 1,
				    length - special->length);
    }

  errno = ENOENT;
  return NULL;

 parent_vnode:
  /* The parent of this vnode is always the root filesystem.  */
  vp = &root_vnode.vnode;
  return (*vnode->ops->name) (vnode, remainder, strlen (remainder));
}

static int
android_content_open (struct android_vnode *vnode, int flags,
		      mode_t mode, bool asset_p, int *fd,
		      AAsset **asset)
{
  /* Don't allow opening this special directory.  */
  errno = ENOSYS;
  return -1;
}

static void
android_content_close (struct android_vnode *vnode)
{
  int save_errno;

  save_errno = errno;
  xfree (vnode);
  errno = save_errno;
}

static int
android_content_unlink (struct android_vnode *vnode)
{
  errno = ENOSYS;
  return -1;
}

static int
android_content_symlink (const char *target, struct android_vnode *vnode)
{
  errno = ENOSYS;
  return -1;
}

static int
android_content_rmdir (struct android_vnode *vnode)
{
  errno = ENOSYS;
  return -1;
}

static int
android_content_rename (struct android_vnode *src,
			struct android_vnode *dst,
			bool keep_existing)
{
  if (src->type != dst->type)
    {
      /* If the types of both vnodes differ, complain that they're on
	 two different filesystems (which is correct from a abstract
	 viewpoint.)  */
      errno = EXDEV;
      return -1;
    }

  /* Otherwise, return ENOSYS.  */
  errno = ENOSYS;
  return -1;
}

static int
android_content_stat (struct android_vnode *vnode,
		      struct stat *statb, int flags)
{
  memset (statb, 0, sizeof *statb);

  statb->st_uid = getuid ();
  statb->st_gid = getgid ();
  statb->st_ino = 0;
  statb->st_dev = -2;
  statb->st_mode = S_IFDIR | S_IRUSR | S_IXUSR;
  return 0;
}

static int
android_content_access (struct android_vnode *vnode, int mode)
{
  /* Validate MODE.  */

  if (mode != F_OK && !(mode & (W_OK | X_OK | R_OK)))
    {
      errno = EINVAL;
      return -1;
    }

  /* Return EROFS if the caller is trying to check for write access to
     this vnode.  */

  if (mode != F_OK && (mode & (W_OK | X_OK)))
    {
      errno = EROFS;
      return -1;
    }

  return 0;
}

static int
android_content_mkdir (struct android_vnode *vnode, mode_t mode)
{
  errno = EEXIST;
  return -1;
}

static int
android_content_chmod (struct android_vnode *vnode, mode_t mode,
		       int flags)
{
  errno = EACCES;
  return -1;
}

static ssize_t
android_content_readlink (struct android_vnode *vnode, char *buffer,
			  size_t size)
{
  errno = EINVAL;
  return -1;
}

static struct dirent *
android_content_readdir (struct android_vdir *vdir)
{
  static struct dirent dirent;
  struct android_content_vdir *dir;
  const char *name;

  dir = (struct android_content_vdir *) vdir;

  /* There are no more files to be read.  */
  if (dir->next_name == (content_directory_contents
			 + ARRAYELTS (content_directory_contents)))
    return NULL;

  /* Get the next child.  */
  name = *dir->next_name++;

  /* Now, fill in the dirent with the name.  */
  memset (&dirent, 0, sizeof dirent);
  dirent.d_ino = 0;
  dirent.d_off = 0;
  dirent.d_reclen = sizeof dirent;
  dirent.d_type = DT_DIR;
  strcpy (dirent.d_name, name);
  return &dirent;
}

static void
android_content_closedir (struct android_vdir *vdir)
{
  struct android_content_vdir *dir, **next, *tem;

  dir = (struct android_content_vdir *) vdir;

  /* If the ``directory file descriptor'' has been opened, close
     it.  */

  if (dir->fd != -1)
    close (dir->fd);

  /* Now unlink this directory.  */

  for (next = &all_content_vdirs; (tem = *next);)
    {
      if (tem == dir)
	*next = dir->next;
      else
	next = &(*next)->next;
    }

  xfree (dir);
}

static int
android_content_dirfd (struct android_vdir *vdir)
{
  struct android_content_vdir *dir;

  dir = (struct android_content_vdir *) vdir;

  /* Since `android_content_opendir' tries to avoid opening a file
     descriptor if readdir isn't called, dirfd can fail if open fails.

     open sets errno to a set of errors different from what POSIX
     stipulates for dirfd, but for ease of implementation the open
     errors are used instead.  */

  if (dir->fd >= 0)
    return dir->fd;

  dir->fd = open ("/dev/null", O_RDONLY | O_CLOEXEC);
  return dir->fd;
}

static struct android_vdir *
android_content_opendir (struct android_vnode *vnode)
{
  struct android_content_vdir *dir;
  int api;

  /* Allocate the virtual directory.  */
  dir = xmalloc (sizeof *dir);
  dir->vdir.readdir = android_content_readdir;
  dir->vdir.closedir = android_content_closedir;
  dir->vdir.dirfd = android_content_dirfd;
  dir->fd = -1;

  /* Fill in the directory contents.  */
  dir->next_name = content_directory_contents;
  api = android_get_current_api_level ();

  /* Android 4.4 and earlier don't support /content/storage.  */

  if (api < 21)
    dir->next_name++;

  /* Android 4.3 and earlier don't support /content/by-authority.  */

  if (api < 19)
    dir->next_name += 2;

  /* Link this stream onto the list of all content directory
     streams.  */
  dir->next = all_content_vdirs;
  all_content_vdirs = dir;
  return &dir->vdir;
}

/* Return the file name corresponding to DIRFD if it is a
   ``directory'' file descriptor returned by `android_content_dirfd'
   or NULL otherwise.  */

static char *
android_content_get_directory_name (int dirfd)
{
  struct android_content_vdir *dir;

  for (dir = all_content_vdirs; dir; dir = dir->next)
    {
      if (dir->fd == dirfd && dirfd != -1)
	return (char *) "/content";
    }

  return NULL;
}

/* Find the vnode designated by the normalized NAME relative to the
   root of the content file system.  NAME may be modified, and must be
   LENGTH bytes long, excluding its terminating NULL byte.  */

static struct android_vnode *
android_content_initial (char *name, size_t length)
{
  struct android_vnode temp;

  /* Create a temporary vnode at the root of the asset file
     system.  */

  temp.ops = &content_vfs_ops;
  temp.type = ANDROID_VNODE_CONTENT;
  temp.flags = 0;

  /* Try to name this vnode.  If NAME is empty, it will be duplicated
     instead.  */
  return android_content_name (&temp, name, length);
}



#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wmissing-prototypes"
#else /* GNUC */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-prototypes"
#endif /* __clang__ */

/* Content URI management functions.  */

JNIEXPORT jstring JNICALL
NATIVE_NAME (displayNameHash) (JNIEnv *env, jobject object,
			       jbyteArray display_name)
{
  char checksum[9], block[MD5_DIGEST_SIZE];
  jbyte *data;

  data = (*env)->GetByteArrayElements (env, display_name, NULL);
  if (!data)
    return NULL;

  /* Hash the buffer.  */
  md5_buffer ((char *) data, (*env)->GetArrayLength (env, display_name),
	      block);
  (*env)->ReleaseByteArrayElements (env, display_name, data, JNI_ABORT);

  /* Generate the digest string.  */
  hexbuf_digest (checksum, (char *) block, 4);
  checksum[8] = '\0';
  return (*env)->NewStringUTF (env, checksum);
}

#ifdef __clang__
#pragma clang diagnostic pop
#else /* GNUC */
#pragma GCC diagnostic pop
#endif /* __clang__ */

/* Return the content URI corresponding to a `/content/by-authority'
   file name, or NULL if it is invalid for some reason.  FILENAME
   should be relative to /content/by-authority, with no leading
   directory separator character.

   WITH_CHECKSUM should be true if FILENAME contains a display name and
   a checksum for that display name.  */

static char *
android_get_content_name (const char *filename, bool with_checksum)
{
  char *fill, *buffer;
  size_t length;
  char checksum[9], new_checksum[9], block[MD5_DIGEST_SIZE];
  const char *p2, *p1;

  /* Make sure FILENAME isn't obviously invalid: it must contain an
     authority name and a file name component.  */

  fill = strchr (filename, '/');
  if (!fill || *(fill + 1) == '\0')
    {
      errno = ENOENT;
      return NULL;
    }

  /* FILENAME must also not be a directory.  Accessing content
     provider directories is not supported by this interface.  */

  length = strlen (filename);
  if (filename[length] == '/')
    {
      errno = ENOTDIR;
      return NULL;
    }

  if (!with_checksum)
    goto no_checksum;

  /* Content file names hold two components providing a display name and
     a short checksum that protects against files being opened under
     display names besides those provided in the content file name at
     the time of generation.  */

  p1 = strrchr (filename, '/'); /* Display name.  */
  p2 = memrchr (filename, '/', p1 - filename); /* Start of checksum.  */

  /* If the name be excessively short or the checksum of an invalid
     length, return.  */
  if (!p2 || (p1 - p2) != 9)
    {
      errno = ENOENT;
      return NULL;
    }

  /* Copy the checksum into CHECKSUM.  */
  memcpy (checksum, p2 + 1, 8);
  new_checksum[8] = checksum[8] = '\0';

  /* Hash this string and store 8 bytes of the resulting digest into
     new_checksum.  */
  md5_buffer (p1 + 1, strlen (p1 + 1), block);
  hexbuf_digest (new_checksum, (char *) block, 4);

  /* Compare both checksums.  */
  if (strcmp (new_checksum, checksum))
    {
      errno = ENOENT;
      return NULL;
    }

  /* Remove the checksum and file display name from the URI.  */
  length = p2 - filename;

 no_checksum:
  if (length > INT_MAX)
    {
      errno = ENOMEM;
      return NULL;
    }

  /* Prefix FILENAME with content:// and return the buffer containing
     that URI.  */
  buffer = xmalloc (sizeof "content://" + length + 1);
  sprintf (buffer, "content://%.*s", (int) length, filename);
  return buffer;
}

/* Return whether or not the specified URI is an accessible content
   URI.  MODE specifies what to check.

   URI must be a string in the JVM's extended UTF-8 format.  */

static bool
android_check_content_access (const char *uri, int mode)
{
  jobject string;
  jboolean rc, read, write;
  jmethodID method;

  string = (*android_java_env)->NewStringUTF (android_java_env, uri);
  android_exception_check ();

  /* Establish what is being checked.  Checking for read access is
     identical to checking if the file exists.  */

  read = (bool) (mode & R_OK || (mode == F_OK));
  write = (bool) (mode & W_OK);
  method = service_class.check_content_uri;

  rc = (*android_java_env)->CallNonvirtualBooleanMethod (android_java_env,
							 emacs_service,
							 service_class.class,
							 method, string, read,
							 write);
  android_exception_check_1 (string);
  ANDROID_DELETE_LOCAL_REF (string);
  return rc;
}



/* Functions shared by authority and SAF nodes.  */

/* Check for JNI exceptions, clear them, and set errno accordingly.
   Also, free each of the N local references given as arguments if an
   exception takes place.

   Value is 1 if an exception has taken place, 0 otherwise.

   If the exception thrown derives from FileNotFoundException, set
   errno to ENOENT.

   If the exception thrown derives from SecurityException, set errno
   to EACCES.

   If the exception thrown derives from OperationCanceledException,
   set errno to EINTR.

   If the exception thrown derives from UnsupportedOperationException,
   set errno to ENOSYS.

   If the exception thrown derives from OutOfMemoryException, call
   `memory_full'.

   If the exception thrown is anything else, set errno to EIO.  */

static int
android_saf_exception_check (int n, ...)
{
  jthrowable exception;
  JNIEnv *env;
  va_list ap;
  int new_errno;

  env = android_java_env;
  va_start (ap, n);

  /* First, check for an exception.  */

  if (!(*env)->ExceptionCheck (env))
    {
      /* No exception has taken place.  Return 0.  */
      va_end (ap);
      return 0;
    }

  /* Print the exception.  */
  (*env)->ExceptionDescribe (env);

  exception = (*env)->ExceptionOccurred (env);

  if (!exception)
    /* JNI couldn't return a local reference to the exception.  */
    memory_full (0);

  /* Clear the exception, making it safe to subsequently call other
     JNI functions.  */
  (*env)->ExceptionClear (env);

  /* Delete each of the N arguments.  */

  while (n > 0)
    {
      ANDROID_DELETE_LOCAL_REF (va_arg (ap, jobject));
      n--;
    }

  /* Now set errno or signal memory_full as required.  */

  if ((*env)->IsInstanceOf (env, (jobject) exception,
			    file_not_found_exception))
    new_errno = ENOENT;
  else if ((*env)->IsInstanceOf (env, (jobject) exception,
				 security_exception))
    new_errno = EACCES;
  else if ((*env)->IsInstanceOf (env, (jobject) exception,
				 operation_canceled_exception))
    new_errno = EINTR;
  else if ((*env)->IsInstanceOf (env, (jobject) exception,
				 unsupported_operation_exception))
    new_errno = ENOSYS;
  else if ((*env)->IsInstanceOf (env, (jobject) exception,
				 out_of_memory_error))
    {
      ANDROID_DELETE_LOCAL_REF ((jobject) exception);
      memory_full (0);
    }
  else
    new_errno = EIO;

  /* expression is still a local reference! */
  ANDROID_DELETE_LOCAL_REF ((jobject) exception);
  errno = new_errno;
  va_end (ap);
  return 1;
}

/* Verify that OBJECT is non-NULL.  If NULL, free each of the N local
   references given as arguments, and clear exceptions.

   Value is 1 if it be NULL, 0 otherwise.  */

static int
android_saf_check_nonnull (const void *object, int n, ...)
{
  va_list ap;

  if (object)
    return 0;

  va_start (ap, n);

  /* Clear the active exception, making it safe to subsequently call
     other JNI functions.  */
  (*android_java_env)->ExceptionClear (android_java_env);

  /* Delete each of the N arguments.  */

  while (n > 0)
    {
      ANDROID_DELETE_LOCAL_REF (va_arg (ap, jobject));
      n--;
    }

  va_end (ap);
  return 1;
}



/* Content authority-based vnode implementation.

   /content/by-authority is a simple vnode implementation that converts
   components to content:// URIs.

   It does not canonicalize file names by removing parent directory
   separators, as these characters can appear in legitimate content
   file names.  */

struct android_authority_vnode
{
  /* The vnode data itself.  */
  struct android_vnode vnode;

  /* URI associated with this vnode, or NULL if this is the root of
     the content authority tree.  */
  char *uri;
};

static struct android_vnode *android_authority_name (struct android_vnode *,
						     char *, size_t);
static int android_authority_open (struct android_vnode *, int,
				   mode_t, bool, int *, AAsset **);
static void android_authority_close (struct android_vnode *);
static int android_authority_unlink (struct android_vnode *);
static int android_authority_symlink (const char *, struct android_vnode *);
static int android_authority_rmdir (struct android_vnode *);
static int android_authority_rename (struct android_vnode *,
				     struct android_vnode *, bool);
static int android_authority_stat (struct android_vnode *, struct stat *, int);
static int android_authority_access (struct android_vnode *, int);
static int android_authority_mkdir (struct android_vnode *, mode_t);
static int android_authority_chmod (struct android_vnode *, mode_t, int);
static ssize_t android_authority_readlink (struct android_vnode *, char *,
					   size_t);
static struct android_vdir *android_authority_opendir (struct android_vnode *);

/* Vector of VFS operations associated with the content VFS node.  */

static struct android_vops authority_vfs_ops =
  {
    android_authority_name,
    android_authority_open,
    android_authority_close,
    android_authority_unlink,
    android_authority_symlink,
    android_authority_rmdir,
    android_authority_rename,
    android_authority_stat,
    android_authority_access,
    android_authority_mkdir,
    android_authority_chmod,
    android_authority_readlink,
    android_authority_opendir,
  };

static struct android_vnode *
android_authority_name (struct android_vnode *vnode, char *name,
			size_t length)
{
  struct android_authority_vnode *vp;
  char *uri_name;

  if (!android_init_gui)
    {
      errno = EIO;
      return NULL;
    }

  /* If NAME is empty or consists of a single directory separator
     _and_ VP->uri is NULL, return a copy of VNODE.  */

  vp = (struct android_authority_vnode *) vnode;

  if (length < 1 || (*name == '/' && length == 1 && !vp->uri))
    {
      vp = xmalloc (sizeof *vp);
      memcpy (vp, vnode, sizeof *vp);

      if (vp->uri)
	vp->uri = xstrdup (vp->uri);

      return &vp->vnode;
    }

  /* Else, if VP->uri is NULL, then it is the root of the by-authority
     tree.  If NAME starts with a directory separator character,
     remove it.  */

  if (!vp->uri)
    {
      if (*name == '/')
	name++, length -= 1;

      /* If the provided URI is a directory, return NULL and set errno
	 to ENOTDIR.  Content files are never directories.  */

      if (name[length - 1] == '/')
	{
	  errno = ENOTDIR;
	  return NULL;
	}

      /* If the URI is not a valid JNI string, return immediately.  This
	 should not be possible, since /content file names are encoded
	 into JNI strings at the naming stage; the check is performed
	 only out of an abundance of caution.  */

      if (android_verify_jni_string (name))
	goto no_entry;

      if (vp->vnode.type == ANDROID_VNODE_CONTENT_AUTHORITY_NAMED)
	/* This indicates that the two trailing components of NAME
	   provide a checksum and a file display name, to be verified,
	   then excluded from the content URI.  */
	uri_name = android_get_content_name (name, true);
      else
	uri_name = android_get_content_name (name, false);

      if (!uri_name)
	goto error;

      /* Now fill in the vnode.  */
      vp = xmalloc (sizeof *vp);
      vp->vnode.ops = &authority_vfs_ops;
      vp->vnode.type = ANDROID_VNODE_CONTENT_AUTHORITY;
      vp->vnode.flags = 0;
      vp->uri = uri_name;
      return &vp->vnode;
    }

  /* Content files can't have children.  */
 no_entry:
  errno = ENOENT;
 error:
  return NULL;
}

static int
android_authority_open (struct android_vnode *vnode, int flags,
			mode_t mode, bool asset_p, int *fd_return,
			AAsset **asset)
{
  struct android_authority_vnode *vp;
  jobject string;
  int fd;
  JNIEnv *env;

  vp = (struct android_authority_vnode *) vnode;

  if (vp->uri == NULL)
    {
      /* This is the `by-authority' directory itself, which can't be
	 opened.  */
      errno = ENOSYS;
      return -1;
    }

  /* Save the JNI environment within `env', to make wrapping
     subsequent lines referencing CallNonvirtualIntMethod
     feasible.  */
  env = android_java_env;

  /* Allocate a JNI string to hold VP->uri.  */
  string = (*env)->NewStringUTF (env, vp->uri);
  android_exception_check ();

  /* Try to open the file descriptor.  */
  fd = (*env)->CallNonvirtualIntMethod (env, emacs_service,
					service_class.class,
					service_class.open_content_uri,
					string,
					(jboolean) ((mode & O_WRONLY
						     || mode & O_RDWR)
						    != 0),
					(jboolean) !(mode & O_WRONLY),
					(jboolean) ((mode & O_TRUNC)
						    != 0));
  if (android_saf_exception_check (1, string))
    return -1;
  ANDROID_DELETE_LOCAL_REF (string);

  /* If fd is -1, just assume that the file does not exist,
     and return -1 with errno set to ENOENT.  */

  if (fd == -1)
    {
      errno = ENOENT;
      return -1;
    }

  if (mode & O_CLOEXEC)
    android_close_on_exec (fd);

  *fd_return = fd;
  return 0;
}

static void
android_authority_close (struct android_vnode *vnode)
{
  struct android_authority_vnode *vp;
  int save_errno;

  vp = (struct android_authority_vnode *) vnode;
  save_errno = errno;
  xfree (vp->uri);
  xfree (vp);
  errno = save_errno;
}

static int
android_authority_unlink (struct android_vnode *vnode)
{
  errno = EROFS;
  return -1;
}

static int
android_authority_symlink (const char *target,
			   struct android_vnode *vnode)
{
  errno = EROFS;
  return -1;
}

static int
android_authority_rmdir (struct android_vnode *vnode)
{
  errno = EROFS;
  return -1;
}

static int
android_authority_rename (struct android_vnode *src,
			  struct android_vnode *dst,
			  bool keep_existing)
{
  if (src->type != dst->type)
    {
      /* If the types of both vnodes differ, complain that they're on
	 two different filesystems (which is correct from a abstract
	 viewpoint.)  */
      errno = EXDEV;
      return -1;
    }

  /* Otherwise, return ENOSYS.  */
  errno = ENOSYS;
  return -1;
}

static int
android_authority_stat (struct android_vnode *vnode,
			struct stat *statb, int flags)
{
  int rc, fd, save_errno;
  struct android_authority_vnode *vp;

  /* If this is a vnode representing `by-authority', return some
     information about this directory.  */

  vp = (struct android_authority_vnode *) vnode;

  if (!vp->uri)
    {
      memset (statb, 0, sizeof *statb);
      statb->st_uid = getuid ();
      statb->st_gid = getgid ();
      statb->st_ino = 0;
      statb->st_dev = -3;
      statb->st_mode = S_IFDIR | S_IRUSR;
      return 0;
    }

  /* Try to open the file and call fstat.  */
  rc = (*vnode->ops->open) (vnode, O_RDONLY, 0, false, &fd, NULL);

  if (rc < 0)
    return -1;

  /* If rc is 1, then an asset file descriptor has been returned.
     This is impossible, so assert that it doesn't transpire.  */
  assert (rc != 1);

  /* Now, try to stat the file.  */
  rc = fstat (fd, statb);
  save_errno = errno;

  /* Close the file descriptor.  */
  close (fd);

  /* Restore errno.  */
  errno = save_errno;
  return rc;
}

static int
android_authority_access (struct android_vnode *vnode, int mode)
{
  struct android_authority_vnode *vp;

  vp = (struct android_authority_vnode *) vnode;

  /* Validate MODE.  */

  if (mode != F_OK && !(mode & (W_OK | X_OK | R_OK)))
    {
      errno = EINVAL;
      return -1;
    }

  if (!vp->uri)
    {
      /* Return EACCES if the caller is trying to check for write
	 access to `by-authority'.  */

      if (mode != F_OK && (mode & (W_OK | X_OK)))
	{
	  errno = EACCES;
	  return -1;
	}

      return 0;
    }

  return (android_check_content_access (vp->uri, mode)
	  ? 0 : -1);
}

static int
android_authority_mkdir (struct android_vnode *vnode, mode_t mode)
{
  errno = EACCES;
  return -1;
}

static int
android_authority_chmod (struct android_vnode *vnode, mode_t mode,
			 int flags)
{
  errno = EACCES;
  return -1;
}

static ssize_t
android_authority_readlink (struct android_vnode *vnode, char *buffer,
			    size_t size)
{
  errno = EINVAL;
  return -1;
}

static struct android_vdir *
android_authority_opendir (struct android_vnode *vnode)
{
  struct android_authority_vnode *vp;

  /* Forbid listing the `by-authority' directory.  */
  vp = (struct android_authority_vnode *) vnode;
  errno = vp->uri ? ENOTDIR : EACCES;
  return NULL;
}

/* Find the vnode designated by NAME relative to the root of the
   by-authority directory.

   If NAME is empty or a single leading separator character, return
   a vnode representing the by-authority directory itself.

   Otherwise, represent the remainder of NAME as a URI (without
   normalizing it) and return a vnode corresponding to that.

   Value may also be NULL with errno set if the designated vnode is
   not available, such as when Android windowing has not been
   initialized.  */

static struct android_vnode *
android_authority_initial (char *name, size_t length)
{
  struct android_authority_vnode temp;

  temp.vnode.ops = &authority_vfs_ops;
  temp.vnode.type = ANDROID_VNODE_CONTENT_AUTHORITY;
  temp.vnode.flags = 0;
  temp.uri = NULL;

  return android_authority_name (&temp.vnode, name, length);
}

/* Find the vnode designated by NAME relative to the root of the
   by-authority-named directory.

   If NAME is empty or a single leading separator character, return
   a vnode representing the by-authority directory itself.

   Otherwise, represent the remainder of NAME as a URI (without
   normalizing it) and return a vnode corresponding to that.

   Value may also be NULL with errno set if the designated vnode is
   not available, such as when Android windowing has not been
   initialized.  */

static struct android_vnode *
android_authority_initial_name (char *name, size_t length)
{
  struct android_authority_vnode temp;

  temp.vnode.ops = &authority_vfs_ops;
  temp.vnode.type = ANDROID_VNODE_CONTENT_AUTHORITY_NAMED;
  temp.vnode.flags = 0;
  temp.uri = NULL;

  return android_authority_name (&temp.vnode, name, length);
}



/* SAF ``root'' vnode implementation.

   The Storage Access Framework is a system service that manages a
   registry of document providers, which are a type of file system
   server.

   Normally, document providers can only provide individual files
   through preestablished ``content URIs''.  Android 5.0 and later add
   to that ``tree URIs'', which designate entire file system trees.

   Authorization to access document trees and content URIs is granted
   transiently by default when an Intent is received, but Emacs can
   also receive persistent authorization for a given document tree.

   /content/storage returns a list of directories, each representing a
   single authority providing at least one tree URI Emacs holds
   persistent authorization for.

   Each one of those directories then contains one document tree that
   Emacs is authorized to access.  */

struct android_saf_root_vnode
{
  /* The vnode data.  */
  struct android_vnode vnode;

  /* The name of the document authority this directory represents, or
     NULL.  */
  char *authority;
};

struct android_saf_root_vdir
{
  /* The directory stream function table.  */
  struct android_vdir vdir;

  /* The next directory stream in `all_saf_root_vdirs'.  */
  struct android_saf_root_vdir *next;

  /* Array of strings, one for each directory to return.  */
  jobjectArray array;

  /* Name of the authority this directory lists, or NULL.  */
  char *authority;

  /* Length of that array, and the current within it.  */
  jsize length, i;

  /* ``Directory'' file descriptor associated with this stream, or
     -1.  */
  int fd;
};

static struct android_vnode *android_saf_root_name (struct android_vnode *,
						    char *, size_t);
static int android_saf_root_open (struct android_vnode *, int,
				  mode_t, bool, int *, AAsset **);
static void android_saf_root_close (struct android_vnode *);
static int android_saf_root_unlink (struct android_vnode *);
static int android_saf_root_symlink (const char *, struct android_vnode *);
static int android_saf_root_rmdir (struct android_vnode *);
static int android_saf_root_rename (struct android_vnode *,
				    struct android_vnode *, bool);
static int android_saf_root_stat (struct android_vnode *, struct stat *, int);
static int android_saf_root_access (struct android_vnode *, int);
static int android_saf_root_mkdir (struct android_vnode *, mode_t);
static int android_saf_root_chmod (struct android_vnode *, mode_t, int);
static ssize_t android_saf_root_readlink (struct android_vnode *, char *,
					  size_t);
static struct android_vdir *android_saf_root_opendir (struct android_vnode *);

/* Vector of VFS operations associated with the SAF root VFS node.  */

static struct android_vops saf_root_vfs_ops =
  {
    android_saf_root_name,
    android_saf_root_open,
    android_saf_root_close,
    android_saf_root_unlink,
    android_saf_root_symlink,
    android_saf_root_rmdir,
    android_saf_root_rename,
    android_saf_root_stat,
    android_saf_root_access,
    android_saf_root_mkdir,
    android_saf_root_chmod,
    android_saf_root_readlink,
    android_saf_root_opendir,
  };

/* Chain containing all SAF root directories.  */
static struct android_saf_root_vdir *all_saf_root_vdirs;

/* Defined in the next page.  */
static struct android_vnode *android_saf_tree_from_name (char *, const char *,
							 const char *);

/* Ascertain and return whether or not AUTHORITY designates a content
   provider offering at least one directory tree accessible to
   Emacs.  */

static bool
android_saf_valid_authority_p (const char *authority)
{
  jobject string;
  jboolean valid;
  jmethodID method;

  /* Make certain AUTHORITY can actually be represented as a Java
     string.  */

  if (android_verify_jni_string (authority))
    return false;

  /* Build a string containing AUTHORITY.  */

  string = (*android_java_env)->NewStringUTF (android_java_env,
					      authority);
  android_exception_check ();

  method = service_class.valid_authority;
  valid
    = (*android_java_env)->CallNonvirtualBooleanMethod (android_java_env,
							emacs_service,
							service_class.class,
							method, string);
  android_exception_check_1 (string);
  ANDROID_DELETE_LOCAL_REF (string);
  return valid;
}

static struct android_vnode *
android_saf_root_name (struct android_vnode *vnode, char *name,
		       size_t length)
{
  char *remainder, *component_end;
  struct android_saf_root_vnode *vp;
  struct android_vnode *new;
  char component[PATH_MAX];

  /* Canonicalize NAME.  */
  remainder = android_vfs_canonicalize_name (name, &length);

  /* If remainder is set, it's a name relative to the root vnode.  */
  if (remainder)
    goto parent_vnode;

  /* If LENGTH is empty or NAME is a single directory separator,
     return a copy of this vnode.  */

  if (length < 1 || (*name == '/' && length == 1))
    {
      vp = xmalloc (sizeof *vp);
      memcpy (vp, vnode, sizeof *vp);

      if (vp->authority)
	vp->authority = xstrdup (vp->authority);

      return &vp->vnode;
    }

  vp = (struct android_saf_root_vnode *) vnode;

  /* If NAME starts with a directory separator, move it past that.  */

  if (*name == '/')
    name++, length -= 1;

  /* Look for the first directory separator.  */
  component_end = strchr (name, '/');

  /* If not there, use name + length.  */

  if (!component_end)
    component_end = name + length;

  if (component_end - name >= PATH_MAX)
    {
      errno = ENAMETOOLONG;
      return NULL;
    }

  /* Copy the component over.  */
  memcpy (component, name, component_end - name);
  component[component_end - name] = '\0';

  /* Create a SAF document vnode for this tree if it represents an
     authority.  */

  if (vp->authority)
    return android_saf_tree_from_name (component_end, component,
				       vp->authority);

  /* Create the vnode.  */
  vp = xmalloc (sizeof *vp);
  vp->vnode.ops = &saf_root_vfs_ops;
  vp->vnode.type = ANDROID_VNODE_SAF_ROOT;
  vp->vnode.flags = 0;
  vp->authority = xstrdup (component);

  /* If there is more of this component to be named, name it through
     the new vnode.  */

  if (component_end != name + length)
    {
      new = (*vp->vnode.ops->name) (&vp->vnode, component_end,
				    length - (component_end - name));
      (*vp->vnode.ops->close) (&vp->vnode);

      return new;
    }

  return &vp->vnode;

 parent_vnode:
  vp = (struct android_saf_root_vnode *) vnode;

  /* .. was encountered and the parent couldn't be found through
     stripping off preceding components.

     Find the parent vnode and name the rest of NAME starting from
     there.  */

  if (!vp->authority)
    /* Look this file name up relative to the root of the contents
       directory.  */
    return android_content_initial (remainder, strlen (remainder));
  else
    /* Look this file name up relative to the root of the storage
       directory.  */
    return android_saf_root_initial (remainder, strlen (remainder));
}

static int
android_saf_root_open (struct android_vnode *vnode, int flags,
		       mode_t mode, bool asset_p, int *fd_return,
		       AAsset **asset)
{
  /* /content/storage or one of its authority children cannot be
     opened, as they are virtual directories.  */

  errno = ENOSYS;
  return -1;
}

static void
android_saf_root_close (struct android_vnode *vnode)
{
  struct android_saf_root_vnode *vp;
  int save_errno;

  vp = (struct android_saf_root_vnode *) vnode;
  save_errno = errno;
  xfree (vp->authority);
  xfree (vp);
  errno = save_errno;
}

static int
android_saf_root_unlink (struct android_vnode *vnode)
{
  errno = EROFS;
  return -1;
}

static int
android_saf_root_symlink (const char *target,
			  struct android_vnode *vnode)
{
  errno = EROFS;
  return -1;
}

static int
android_saf_root_rmdir (struct android_vnode *vnode)
{
  errno = EROFS;
  return -1;
}

static int
android_saf_root_rename (struct android_vnode *src,
			 struct android_vnode *dst,
			 bool keep_existing)
{
  errno = EROFS;
  return -1;
}

static int
android_saf_root_stat (struct android_vnode *vnode,
		       struct stat *statb, int flags)
{
  struct android_saf_root_vnode *vp;

  /* Verify that the authority actually exists and return ENOENT
     otherwise, lest `locate-dominating-file' & co call an operation
     that doesn't require listing URIs under this authority, such as
     access.  */

  vp = (struct android_saf_root_vnode *) vnode;

  if (vp->authority
      && !android_saf_valid_authority_p (vp->authority))
    {
      errno = ENOENT;
      return -1;
    }

  /* Make up some imaginary statistics for this vnode.  */

  memset (statb, 0, sizeof *statb);
  statb->st_uid = getuid ();
  statb->st_gid = getgid ();
  statb->st_ino = 0;
  statb->st_dev = -4;
  statb->st_mode = S_IFDIR | S_IRUSR | S_IXUSR;
  return 0;
}

static int
android_saf_root_access (struct android_vnode *vnode, int mode)
{
  struct android_saf_root_vnode *vp;

  /* Validate MODE.  */

  if (mode != F_OK && !(mode & (W_OK | X_OK | R_OK)))
    {
      errno = EINVAL;
      return -1;
    }

  /* Now, don't allow writing or executing this directory.  */

  if (mode != F_OK && (mode & (W_OK | X_OK)))
    {
      errno = EROFS;
      return -1;
    }

  /* Verify that the authority actually exists and return ENOENT
     otherwise, lest `locate-dominating-file' & co call an operation
     that doesn't require listing URIs under this authority, such as
     access.  */

  vp = (struct android_saf_root_vnode *) vnode;

  if (vp->authority
      && !android_saf_valid_authority_p (vp->authority))
    {
      errno = ENOENT;
      return -1;
    }

  return 0;
}

static int
android_saf_root_mkdir (struct android_vnode *vnode, mode_t mode)
{
  errno = EROFS;
  return -1;
}

static int
android_saf_root_chmod (struct android_vnode *vnode, mode_t mode,
			int flags)
{
  errno = EACCES;
  return -1;
}

static ssize_t
android_saf_root_readlink (struct android_vnode *vnode, char *buffer,
			   size_t size)
{
  errno = EINVAL;
  return -1;
}

static struct dirent *
android_saf_root_readdir (struct android_vdir *vdir)
{
  static struct dirent *dirent;
  jobject string;
  const char *chars;
  size_t length, size;
  struct android_saf_root_vdir *dir;

  dir = (struct android_saf_root_vdir *) vdir;

  if (dir->i == dir->length)
    {
      /* At the end of the stream.  Free dirent and return NULL.  */

      xfree (dirent);
      dirent = NULL;
      return NULL;
    }

  /* Get this string.  */
  string = (*android_java_env)->GetObjectArrayElement (android_java_env,
						       dir->array, dir->i++);
  android_exception_check ();
  chars = (*android_java_env)->GetStringUTFChars (android_java_env,
						  (jstring) string,
						  NULL);
  android_exception_check_nonnull ((void *) chars, string);

  /* Figure out how large it is, and then resize dirent to fit--this
     string is always ASCII.  */
  length = strlen (chars) + 1;
  size   = offsetof (struct dirent, d_name) + length;
  dirent = xrealloc (dirent, size);

  /* Clear dirent.  */
  memset (dirent, 0, size);

  /* Fill in the generic directory information and copy the string
     over.  */
  dirent->d_ino = 0;
  dirent->d_off = 0;
  dirent->d_reclen = size;
  dirent->d_type = DT_DIR;
  strcpy (dirent->d_name, chars);

  /* Release the string data and the local reference to STRING.  */
  (*android_java_env)->ReleaseStringUTFChars (android_java_env,
					      (jstring) string, chars);
  ANDROID_DELETE_LOCAL_REF (string);
  return dirent;
}

static void
android_saf_root_closedir (struct android_vdir *vdir)
{
  struct android_saf_root_vdir *dir, **next, *tem;

  dir = (struct android_saf_root_vdir *) vdir;

  /* If the ``directory file descriptor'' has been opened, close
     it.  */

  if (dir->fd != -1)
    close (dir->fd);

  /* Delete the local reference to the file name array.  */
  ANDROID_DELETE_LOCAL_REF (dir->array);

  /* Free the authority name if set.  */
  xfree (dir->authority);

  /* Now unlink this directory.  */

  for (next = &all_saf_root_vdirs; (tem = *next);)
    {
      if (tem == dir)
	*next = dir->next;
      else
	next = &(*next)->next;
    }

  /* Free the directory itself.  */
  xfree (dir);
}

static int
android_saf_root_dirfd (struct android_vdir *vdir)
{
  struct android_saf_root_vdir *dir;

  dir = (struct android_saf_root_vdir *) vdir;

  /* Since `android_saf_root_opendir' tries to avoid opening a file
     descriptor if readdir isn't called, dirfd can fail if open fails.

     open sets errno to a set of errors different from what POSIX
     stipulates for dirfd, but for ease of implementation the open
     errors are used instead.  */

  if (dir->fd >= 0)
    return dir->fd;

  dir->fd = open ("/dev/null", O_RDONLY | O_CLOEXEC);
  return dir->fd;
}

static struct android_vdir *
android_saf_root_opendir (struct android_vnode *vnode)
{
  struct android_saf_root_vnode *vp;
  jobjectArray array;
  jmethodID method;
  jstring authority;
  struct android_saf_root_vdir *dir;
  size_t length;

  vp = (struct android_saf_root_vnode *) vnode;

  if (vp->authority)
    {
      /* Build a string containing the authority.  */
      length = strlen (vp->authority);
      authority = (*android_java_env)->NewStringUTF (android_java_env,
						     vp->authority);
      android_exception_check ();

      /* Acquire a list of every tree provided by this authority.  */

      method = service_class.get_document_trees;
      array
	= (*android_java_env)->CallNonvirtualObjectMethod (android_java_env,
							   emacs_service,
							   service_class.class,
							   method, authority);
      android_exception_check_1 (authority);
      ANDROID_DELETE_LOCAL_REF (authority);

      /* Ascertain the length of the array.  If it is empty or NULL,
         return ENOENT.  */

      if (!array)
	{
	  errno = ENOENT;
	  return NULL;
	}

      length = (*android_java_env)->GetArrayLength (android_java_env, array);

      if (!length)
	{
	  ANDROID_DELETE_LOCAL_REF (array);
	  errno = ENOENT;
	  return NULL;
	}

      /* Now allocate the directory stream.  It will retain a local
	 reference to the array, and should thus only be used within the
	 same JNI local reference frame.  */

      dir = xmalloc (sizeof *dir);
      dir->vdir.readdir = android_saf_root_readdir;
      dir->vdir.closedir = android_saf_root_closedir;
      dir->vdir.dirfd = android_saf_root_dirfd;
      dir->fd = -1;
      dir->array = array;
      dir->length = length;
      dir->i = 0;
      dir->authority = xstrdup (vp->authority);

      /* Link this stream onto the list of all SAF root directory
	 streams.  */
      dir->next = all_saf_root_vdirs;
      all_saf_root_vdirs = dir;
      return &dir->vdir;
    }

  /* Acquire a list of every document authority.  */

  method = service_class.get_document_authorities;
  array = (*android_java_env)->CallNonvirtualObjectMethod (android_java_env,
							   emacs_service,
							   service_class.class,
							   method);
  android_exception_check ();

  if (!array)
    emacs_abort ();

  /* Now allocate the directory stream.  It will retain a local
     reference to the array, and should thus only be used within the
     same JNI local reference frame.  */

  dir = xmalloc (sizeof *dir);
  dir->vdir.readdir = android_saf_root_readdir;
  dir->vdir.closedir = android_saf_root_closedir;
  dir->vdir.dirfd = android_saf_root_dirfd;
  dir->fd = -1;
  dir->array = array;
  dir->length = (*android_java_env)->GetArrayLength (android_java_env,
						     array);
  dir->i = 0;
  dir->authority = NULL;

  /* Link this stream onto the list of all SAF root directory
     streams.  */
  dir->next = all_saf_root_vdirs;
  all_saf_root_vdirs = dir;
  return &dir->vdir;
}

/* Find the vnode designated by NAME relative to the root of the
   storage directory.

   If NAME is empty or a single leading separator character, return a
   vnode representing the storage directory itself.

   If NAME actually resides in a parent directory, look for it within
   the vnode representing the content directory.  */

static struct android_vnode *
android_saf_root_initial (char *name, size_t length)
{
  struct android_saf_root_vnode temp;

  temp.vnode.ops = &saf_root_vfs_ops;
  temp.vnode.type = ANDROID_VNODE_SAF_ROOT;
  temp.vnode.flags = 0;
  temp.authority = NULL;

  return android_saf_root_name (&temp.vnode, name, length);
}

/* Return any open SAF root directory stream for which dirfd has
   returned the file descriptor DIRFD.  Return NULL otherwise.  */

static struct android_saf_root_vdir *
android_saf_root_get_directory (int dirfd)
{
  struct android_saf_root_vdir *dir;

  for (dir = all_saf_root_vdirs; dir; dir = dir->next)
    {
      if (dir->fd == dirfd && dirfd != -1)
	return dir;
    }

  return NULL;
}



/* Functions common to both SAF directory and file nodes.  */

/* Whether or not Emacs is within an operation running from the SAF
   thread.  */
static bool inside_saf_critical_section;

/* Return file status for the document designated by ID_NAME within
   the document tree identified by URI_NAME.

   If NO_CACHE, don't cache the resulting file status.  Enable this
   option if the file status is subject to imminent change.

   If the file status is available, place it within *STATB and return
   0.  If not, return -1 and set errno to EPERM.  */

static int
android_saf_stat (const char *uri_name, const char *id_name,
		  struct stat *statb, bool no_cache)
{
  jmethodID method;
  jstring uri, id;
  jobject status;
  jlong mode, size, mtim, *longs;

  /* Now guarantee that it is safe to call functions which
     synchronize with the SAF thread.  */

  if (inside_saf_critical_section)
    {
      errno = EIO;
      return -1;
    }

  /* Build strings for both URI and ID.  */
  uri = (*android_java_env)->NewStringUTF (android_java_env, uri_name);
  android_exception_check ();

  if (id_name)
    {
      id = (*android_java_env)->NewStringUTF (android_java_env,
					      id_name);
      android_exception_check_1 (uri);
    }
  else
    id = NULL;

  /* Try to retrieve the file status.  */
  method = service_class.stat_document;
  inside_saf_critical_section = true;
  status
    = (*android_java_env)->CallNonvirtualObjectMethod (android_java_env,
						       emacs_service,
						       service_class.class,
						       method, uri, id,
						       (jboolean) no_cache);
  inside_saf_critical_section = false;

  /* Check for exceptions and release unneeded local references.  */

  if (id)
    {
      if (android_saf_exception_check (2, uri, id))
	return -1;

      ANDROID_DELETE_LOCAL_REF (id);
    }
  else if (android_saf_exception_check (1, uri))
    return -1;

  ANDROID_DELETE_LOCAL_REF (uri);

  /* Check for failure.  */

  if (!status)
    {
      errno = EPERM;
      return -1;
    }

  /* Read the file status from the array returned.  */

  longs = (*android_java_env)->GetLongArrayElements (android_java_env,
						     status, NULL);
  android_exception_check_nonnull (longs, status);
  mode = longs[0];
  size = longs[1];
  mtim = longs[2];
  (*android_java_env)->ReleaseLongArrayElements (android_java_env, status,
						 longs, JNI_ABORT);
  ANDROID_DELETE_LOCAL_REF (status);

  /* Fill in STATB with this information.  */
  memset (statb, 0, sizeof *statb);
  statb->st_size = MAX (0, MIN (TYPE_MAXIMUM (off_t), size));
  statb->st_mode = mode;
  statb->st_dev = -4;
#ifdef STAT_TIMESPEC
  STAT_TIMESPEC (statb, st_mtim).tv_sec = mtim / 1000;
  STAT_TIMESPEC (statb, st_mtim).tv_nsec = (mtim % 1000) * 1000000;
#else /* !STAT_TIMESPEC */
  /* Headers supplied by the NDK r10b contain a `struct stat' without
     POSIX fields for nano-second timestamps.  */
  statb->st_mtime = mtim / 1000;
  statb->st_mtime_nsec = (mtim % 1000) * 1000000;
#endif /* STAT_TIMESPEC */
  statb->st_uid = getuid ();
  statb->st_gid = getgid ();
  return 0;
}

/* Detect if Emacs has access to the document designated by the
   document ID ID_NAME within the tree URI_NAME.  If ID_NAME is NULL,
   use the document ID in URI_NAME itself.

   If WRITABLE, also check that the file is writable, which is true
   if it is either a directory or its flags contains
   FLAG_SUPPORTS_WRITE.

   Value is 0 if the file is accessible, and -1 with errno set
   appropriately if not.  */

static int
android_saf_access (const char *uri_name, const char *id_name,
		    bool writable)
{
  jmethodID method;
  jstring uri, id;
  jint rc;

  /* Now guarantee that it is safe to call functions which
     synchronize with the SAF thread.  */

  if (inside_saf_critical_section)
    {
      errno = EIO;
      return -1;
    }

  /* Build strings for both URI and ID.  */
  uri = (*android_java_env)->NewStringUTF (android_java_env, uri_name);
  android_exception_check ();

  if (id_name)
    {
      id = (*android_java_env)->NewStringUTF (android_java_env,
					      id_name);
      android_exception_check_1 (uri);
    }
  else
    id = NULL;

  /* Try to retrieve the file status.  */
  method = service_class.access_document;
  inside_saf_critical_section = true;
  rc = (*android_java_env)->CallNonvirtualIntMethod (android_java_env,
						     emacs_service,
						     service_class.class,
						     method, uri, id,
						     (jboolean) writable);
  inside_saf_critical_section = false;

  /* Check for exceptions and release unneeded local references.  */

  if (id)
    {
      if (android_saf_exception_check (2, uri, id))
	return -1;

      ANDROID_DELETE_LOCAL_REF (id);
    }
  else if (android_saf_exception_check (1, uri))
    return -1;

  ANDROID_DELETE_LOCAL_REF (uri);

  switch (rc)
    {
    case -1:
      /* -1 means it doesn't exist.  */
      errno = ENOENT;
      return -1;

    case -2:
      /* -2 means access has been denied.  */
      errno = EACCES;
      return -1;

    case -3:
      /* -3 refers to an internal error.  */
      errno = EIO;
      return -1;
    }

  /* Return success.  */
  return 0;
}

/* Delete the document designated by DOC_ID within the tree identified
   through the URI TREE.  Return 0 if the document has been deleted,
   set errno and return -1 upon failure.

   DOC_NAME should be the name of the file itself, as a file name
   whose constituent components lead to a document named DOC_ID.  It
   isn't used to search for a document ID, but is used to invalidate
   the file cache.  */

static int
android_saf_delete_document (const char *tree, const char *doc_id,
			     const char *doc_name)
{
  jobject id, uri, name;
  jmethodID method;
  jint rc;

  /* Build the strings holding the ID, URI and NAME.  */
  id = (*android_java_env)->NewStringUTF (android_java_env,
					  doc_id);
  android_exception_check ();
  uri = (*android_java_env)->NewStringUTF (android_java_env,
					   tree);
  android_exception_check_1 (id);
  name = (*android_java_env)->NewStringUTF (android_java_env,
					    doc_name);
  android_exception_check_2 (id, name);

  /* Now, try to delete the document.  */
  method = service_class.delete_document;
  rc = (*android_java_env)->CallNonvirtualIntMethod (android_java_env,
						     emacs_service,
						     service_class.class,
						     method, uri, id,
						     name);

  if (android_saf_exception_check (3, id, uri, name))
    return -1;

  ANDROID_DELETE_LOCAL_REF (id);
  ANDROID_DELETE_LOCAL_REF (uri);
  ANDROID_DELETE_LOCAL_REF (name);

  if (rc)
    {
      errno = EACCES;
      return -1;
    }

  return 0;
}

/* Declared further below.  */
static int android_document_id_from_name (const char *, const char *,
					  char **);

/* Rename the document designated by DOC_ID inside the directory tree
   identified by URI, which should be within the directory by the name
   of DIR, to NAME.  If the document can't be renamed, return -1 and
   set errno to a value describing the error.  Return 0 if the rename
   is successful.

   Android permits the same document to appear in multiple
   directories, but stores the display name inside the document
   ``inode'' itself instead of the directory entries that refer to it.
   Because of this, this operation may cause other directory entries
   outside DIR to be renamed.  */

static int
android_saf_rename_document (const char *uri, const char *doc_id,
			     const char *dir, const char *name)
{
  int rc;
  jstring uri1, doc_id1, dir1, name1;
  jmethodID method;

  /* Now build the strings for the URI, document ID, directory name
     and directory ID.  */

  uri1 = (*android_java_env)->NewStringUTF (android_java_env, uri);
  android_exception_check ();
  doc_id1 = (*android_java_env)->NewStringUTF (android_java_env, doc_id);
  android_exception_check_1 (uri1);
  dir1 = (*android_java_env)->NewStringUTF (android_java_env, dir);
  android_exception_check_2 (doc_id1, uri1);
  name1 = (*android_java_env)->NewStringUTF (android_java_env, name);
  android_exception_check_3 (dir1, doc_id1, uri1);

  method = service_class.rename_document;
  rc = (*android_java_env)->CallNonvirtualIntMethod (android_java_env,
						     emacs_service,
						     service_class.class,
						     method, uri1, doc_id1,
						     dir1, name1);

  /* Check for exceptions.  */

  if (android_saf_exception_check (4, uri1, doc_id1, dir1, name1))
    {
      /* Substitute EXDEV for ENOSYS, so callers fall back on
	 delete-then-copy.  */

      if (errno == ENOSYS)
	errno = EXDEV;

      return -1;
    }

  /* Delete unused local references.  */
  ANDROID_DELETE_LOCAL_REF (uri1);
  ANDROID_DELETE_LOCAL_REF (doc_id1);
  ANDROID_DELETE_LOCAL_REF (dir1);
  ANDROID_DELETE_LOCAL_REF (name1);

  /* Then check for errors handled within the Java code.  */

  if (rc == -1)
    {
      /* UnsupportedOperationException.  Trick the caller into falling
	 back on delete-then-copy code.  */
      errno = EXDEV;
      return -1;
    }

  return 0;
}

/* Move the document designated by *DOC_ID from the directory under
   DIR_NAME to the directory designated by DST_ID.  All three
   directories are located within the tree identified by the given
   URI.

   If the document's ID changes as a result of the movement, free
   *DOC_ID and store the new document ID within.

   Value is 0 upon success, -1 otherwise with errno set.  */

static int
android_saf_move_document (const char *uri, char **doc_id,
			   const char *dir_name, const char *dst_id)
{
  char *src_id, *id;
  jobject uri1, doc_id1, dir_name1, dst_id1, src_id1;
  jstring result;
  jmethodID method;
  int rc;
  const char *new_id;

  /* Obtain the name of the source directory.  */
  src_id = NULL;
  rc = android_document_id_from_name (uri, dir_name, &src_id);

  if (rc != 1)
    {
      /* This file is either not a directory or nonexistent.  */
      xfree (src_id);

      switch (rc)
	{
	case 0:
	  errno = ENOTDIR;
	  return -1;

	case -1:
	case -2:
	  errno = ENOENT;
	  return -1;

	default:
	  emacs_abort ();
	}
    }

  /* Build Java strings for all five arguments.  */
  id = *doc_id;
  uri1 = (*android_java_env)->NewStringUTF (android_java_env, uri);
  android_exception_check ();
  doc_id1 = (*android_java_env)->NewStringUTF (android_java_env, id);
  android_exception_check_1 (uri1);
  dir_name1 = (*android_java_env)->NewStringUTF (android_java_env, dir_name);
  android_exception_check_2 (doc_id1, uri1);
  dst_id1 = (*android_java_env)->NewStringUTF (android_java_env, dst_id);
  android_exception_check_3 (dir_name1, doc_id1, uri1);
  src_id1 = (*android_java_env)->NewStringUTF (android_java_env, src_id);
  xfree (src_id);
  android_exception_check_4 (dst_id1, dir_name1, doc_id1, uri1);

  /* Do the rename.  */
  method = service_class.move_document;
  result
    = (*android_java_env)->CallNonvirtualObjectMethod (android_java_env,
						       emacs_service,
						       service_class.class,
						       method, uri1,
						       doc_id1, dir_name1,
						       dst_id1, src_id1);
  if (android_saf_exception_check (5, src_id1, dst_id1, dir_name1,
				   doc_id1, uri1))
    {
      /* Substitute EXDEV for ENOSYS, so callers fall back on
	 delete-then-copy.  */

      if (errno == ENOSYS)
	errno = EXDEV;

      return -1;
    }

  /* Delete unused local references.  */
  ANDROID_DELETE_LOCAL_REF (src_id1);
  ANDROID_DELETE_LOCAL_REF (dst_id1);
  ANDROID_DELETE_LOCAL_REF (dir_name1);
  ANDROID_DELETE_LOCAL_REF (doc_id1);
  ANDROID_DELETE_LOCAL_REF (uri1);

  if (result)
    {
      /* The document ID changed.  Free id and replace *DOC_ID with
	 the new ID.  */
      xfree (id);
      new_id = (*android_java_env)->GetStringUTFChars (android_java_env,
						       result, NULL);
      android_exception_check_nonnull ((void *) new_id, result);
      *doc_id = xstrdup (new_id);
      (*android_java_env)->ReleaseStringUTFChars (android_java_env, result,
						  new_id);
      ANDROID_DELETE_LOCAL_REF (result);
    }

  return 0;
}



/* SAF directory vnode.  A file within a SAF directory tree is
   identified by the URI of the directory tree itself, an opaque
   ``file identifier'' value, and a display name.  This information is
   recorded in each vnode representing either a directory or a file
   itself.  */

struct android_saf_tree_vnode
{
  /* The vnode data itself.  */
  struct android_vnode vnode;

  /* The URI of the directory tree represented.  This is Java string
     data in ``modified UTF format'', which is essentially a modified
     UTF-8 format capable of storing NULL bytes while also utilizing
     NULL termination.  */
  const char *tree_uri;

  /* The ID of the document tree designated by TREE_URI.  */
  char *tree_id;

  /* The document ID of the directory represented, or NULL if this is
     the root directory of the tree.  Since file and new vnodes don't
     represent the root directory, this field is always set in
     them.  */
  char *document_id;

  /* The file name of this tree vnode.  This is a ``path'' to the
     file, where each directory component consists of the display name
     of a directory leading up to a file within, terminated with a
     directory separator character.  */
  char *name;
};

struct android_saf_tree_vdir
{
  /* The virtual directory stream function table.  */
  struct android_vdir vdir;

  /* The next directory in `all_saf_tree_vdirs'.  */
  struct android_saf_tree_vdir *next;

  /* Name of this directory relative to the root file system.  */
  char *name;

  /* Local reference to the cursor representing the directory
     stream.  */
  jobject cursor;

  /* The ``directory'' file descriptor used to identify this directory
     stream, or -1.  */
  int fd;
};

static struct android_vnode *android_saf_tree_name (struct android_vnode *,
						    char *, size_t);
static int android_saf_tree_open (struct android_vnode *, int,
				  mode_t, bool, int *, AAsset **);
static void android_saf_tree_close (struct android_vnode *);
static int android_saf_tree_unlink (struct android_vnode *);
static int android_saf_tree_symlink (const char *, struct android_vnode *);
static int android_saf_tree_rmdir (struct android_vnode *);
static int android_saf_tree_rename (struct android_vnode *,
				    struct android_vnode *, bool);
static int android_saf_tree_stat (struct android_vnode *, struct stat *, int);
static int android_saf_tree_access (struct android_vnode *, int);
static int android_saf_tree_mkdir (struct android_vnode *, mode_t);
static int android_saf_tree_chmod (struct android_vnode *, mode_t, int);
static ssize_t android_saf_tree_readlink (struct android_vnode *, char *,
					  size_t);
static struct android_vdir *android_saf_tree_opendir (struct android_vnode *);

/* Vector of VFS operations associated with SAF tree VFS nodes.  */

static struct android_vops saf_tree_vfs_ops =
  {
    android_saf_tree_name,
    android_saf_tree_open,
    android_saf_tree_close,
    android_saf_tree_unlink,
    android_saf_tree_symlink,
    android_saf_tree_rmdir,
    android_saf_tree_rename,
    android_saf_tree_stat,
    android_saf_tree_access,
    android_saf_tree_mkdir,
    android_saf_tree_chmod,
    android_saf_tree_readlink,
    android_saf_tree_opendir,
  };

/* Vector of VFS operations associated with SAF file VFS nodes.
   Defined later in the next page.  */
static struct android_vops saf_file_vfs_ops;

/* Vector of VFS operations associated with SAF ``new'' VFS nodes.
   Defined two pages below.  */
static struct android_vops saf_new_vfs_ops;

/* Chain of all open SAF directory streams.  */
static struct android_saf_tree_vdir *all_saf_tree_vdirs;

/* Find the document ID of the file within TREE_URI designated by
   NAME.

   NAME is a ``file name'' comprised of the display names of
   individual files.  Each constituent component prior to the last
   must name a directory file within TREE_URI.

   If NAME is not correct for the Java ``modified UTF-8'' coding
   system, return -1 and set errno to ENOENT.

   Upon success, return 0 or 1 (contingent upon whether or not the
   last component within NAME is a directory) and place the document
   ID of the named file in ID.

   If the designated file doesn't exist, but the penultimate component
   within NAME does and is also a directory, return -2 and place the
   document ID of that directory within *ID.

   If the designated file can't be located, return -1 and set errno
   accordingly.  The reasons for which a file can't be located are not
   all immediately obvious: quitting, for example, can cause document
   ID lookup to be canceled.  */

static int
android_document_id_from_name (const char *tree_uri, const char *name,
			       char **id)
{
  jobjectArray result;
  jstring uri;
  jbyteArray java_name;
  jint rc;
  jmethodID method;
  const char *doc_id;

  /* Verify the format of NAME.  Don't allow creating files that
     contain characters that can't be encoded in Java.  */

  if (android_verify_jni_string (name))
    {
      errno = ENOENT;
      return -1;
    }

  /* Now guarantee that it is safe to call
     `document_id_from_name'.  */

  if (inside_saf_critical_section)
    {
      errno = EIO;
      return -1;
    }

  /* First, create the array that will hold the result.  */
  result = (*android_java_env)->NewObjectArray (android_java_env, 1,
						java_string_class,
						NULL);
  android_exception_check ();

  /* Next, create the string for the tree URI and name.  */
  java_name = (*android_java_env)->NewStringUTF (android_java_env,
						 name);
  android_exception_check_1 (result);
  uri = (*android_java_env)->NewStringUTF (android_java_env, tree_uri);
  android_exception_check_2 (result, java_name);

  /* Now, call documentIdFromName.  This will synchronize with the SAF
     thread, so make sure reentrant calls don't happen.  */
  method = service_class.document_id_from_name;
  inside_saf_critical_section = true;
  rc = (*android_java_env)->CallNonvirtualIntMethod (android_java_env,
						     emacs_service,
						     service_class.class,
						     method,
						     uri, java_name,
						     result);
  inside_saf_critical_section = false;

  if (android_saf_exception_check (3, result, uri, java_name))
    return -1;

  ANDROID_DELETE_LOCAL_REF (uri);
  ANDROID_DELETE_LOCAL_REF (java_name);

  /* If rc indicates failure, don't try to copy from result.  */

  if (rc == -1)
    {
      ANDROID_DELETE_LOCAL_REF (result);
      errno = ENOENT;
      return -1;
    }

  eassert (rc == -2 || rc >= 0);

  /* Otherwise, obtain the contents of the string returned in Java
     ``UTF-8'' encoding.  */
  uri = (*android_java_env)->GetObjectArrayElement (android_java_env,
						    result, 0);
  android_exception_check_nonnull (uri, result);
  ANDROID_DELETE_LOCAL_REF (result);

  doc_id = (*android_java_env)->GetStringUTFChars (android_java_env,
						   uri, NULL);
  android_exception_check_nonnull ((void *) doc_id, uri);

  /* Make *ID its copy.  */
  *id = xstrdup (doc_id);

  /* And release it.  */
  (*android_java_env)->ReleaseStringUTFChars (android_java_env,
					      (jstring) uri, doc_id);
  ANDROID_DELETE_LOCAL_REF (uri);
  return rc;
}

static struct android_vnode *
android_saf_tree_name (struct android_vnode *vnode, char *name,
		       size_t length)
{
  char *remainder;
  int rc;
  struct android_saf_tree_vnode *vp, *new;
  size_t vp_length;
  char *filename, *fill, *doc_id, *end;
  struct android_saf_root_vnode root;
  struct android_saf_tree_vnode tree;

  /* Canonicalize NAME.  */
  remainder = android_vfs_canonicalize_name (name, &length);

  /* If remainder is set, it's a name relative to the root vnode.  */
  if (remainder)
    goto parent_vnode;

  /* If LENGTH is empty or NAME is a single directory separator,
     return a copy of this vnode.  */

  if (length < 1 || (*name == '/' && length == 1))
    {
      vp = xmalloc (sizeof *vp);
      memcpy (vp, vnode, sizeof *vp);

      /* Duplicate the information contained within VNODE.  */

      vp->tree_uri = xstrdup (vp->tree_uri);
      vp->tree_id = xstrdup (vp->tree_id);
      vp->name = xstrdup (vp->name);

      if (vp->document_id)
	vp->document_id = xstrdup (vp->name);

      return &vp->vnode;
    }

  /* Now, search for the document ID of the file designated by NAME
     relative to this vnode.  */

  vp = (struct android_saf_tree_vnode *) vnode;
  vp_length = strlen (vp->name);

  /* If NAME starts with a directory separator, move it past that.  */

  if (*name == '/')
    name++, length -= 1;

  /* Concatenate VP->name with NAME.  Leave one byte at the end for an
     extra trailing directory separator.  */

  filename = xmalloc (vp_length + length + 2);
  fill = stpcpy (filename, vp->name);
  fill = stpcpy (fill, name);

  /* And search for a document ID in the result.  */
  rc = android_document_id_from_name (vp->tree_uri, name,
				      &doc_id);

  if (rc < 0)
    {
      if (rc == -2)
	{
	  /* This is a vnode representing a nonexistent file in a real
	     directory, so create a vnode whose sole use is to create
	     the file.  */

	  new = xmalloc (sizeof *new);
	  new->vnode.ops = &saf_new_vfs_ops;
	  new->vnode.type = ANDROID_VNODE_SAF_NEW;
	  new->vnode.flags = 0;

	  /* Here, doc_id is actually the ID of the penultimate
	     component in NAME.  */

	  new->document_id = doc_id;
	  new->tree_uri = xstrdup (vp->tree_uri);
	  new->tree_id = xstrdup (vp->tree_id);
	  new->name = filename;
	  return &new->vnode;
	}

      /* The document ID can't be found.  */
      xfree (filename);
      return NULL;
    }

  if (!rc)
    {
      /* rc set to 0 means that NAME is a regular file.  Detect if
         NAME is supposed to be a directory; if it is, set errno to
         ENODIR.  */

      if (name[length - 1] == '/')
	{
	  xfree (filename);
	  xfree (doc_id);
	  errno = ENOTDIR;
	  return NULL;
	}
    }

  /* So this is either a directory or really a file.  Fortunately,
     directory and file vnodes share everything in common except for a
     few file operations, so create a new directory vnode with the new
     file name and return it.  */

  new = xmalloc (sizeof *new);
  new->vnode.ops = (rc ? &saf_tree_vfs_ops
		    : &saf_file_vfs_ops);
  new->vnode.type = (rc ? ANDROID_VNODE_SAF_TREE
		     : ANDROID_VNODE_SAF_FILE);
  new->vnode.flags = 0;

  if (rc)
    {
      /* If fill[-1] is not a directory separator character, append
	 one to the end of filename.  */

      if (fill[-1] != '/')
	{
	  *fill++ = '/';
	  *fill   = '\0';
	}
    }

  new->document_id = doc_id;
  new->tree_uri = xstrdup (vp->tree_uri);
  new->tree_id = xstrdup (vp->tree_id);
  new->name = filename;
  return &new->vnode;

 parent_vnode:
  vp = (struct android_saf_tree_vnode *) vnode;

  /* .. was encountered and the parent couldn't be found through
     stripping off preceding components.

     Find the parent vnode and name the rest of NAME starting from
     there.  */

  if (!vp->document_id)
    {
      /* VP->document_id is NULL, meaning this is the root of this
	 directory tree.  The parent vnode is an SAF root vnode with
	 VP->tree_uri's authority.  */

      root.vnode.ops = &saf_root_vfs_ops;
      root.vnode.type = ANDROID_VNODE_SAF_ROOT;
      root.vnode.flags = 0;

      /* Derive the authority from the URI.  */

      fill = (char *) vp->tree_uri;

      if (strncmp (fill, "content://", 10))
	emacs_abort ();

      /* Skip the content header.  */
      fill += sizeof "content://" - 1;

      /* The authority segment of the URI is between here and the
	 next slash.  */

      end = strchr (fill, '/');

      if (!end)
	emacs_abort ();

      root.authority = xmalloc (end - fill + 1);
      memcpy (root.authority, fill, end - fill);
      root.authority[end - fill] = '\0';

      /* Now search using this vnode.  */
      vnode = (*root.vnode.ops->name) (&root.vnode, remainder,
				       strlen (remainder));
      xfree (root.authority);
      return vnode;
    }

  /* Otherwise, strip off the last directory component.  */

  fill = strrchr (vp->name, '/');
  if (!fill)
    emacs_abort ();

  /* Create a new vnode at the top of the directory tree, and search
     for remainder from there.  */

  tree.vnode.ops = &saf_tree_vfs_ops;
  tree.vnode.type = ANDROID_VNODE_SAF_TREE;
  tree.vnode.flags = 0;
  tree.document_id = NULL;
  tree.name = (char *) "/";
  tree.tree_uri = vp->tree_uri;
  tree.tree_id = vp->tree_id;

  length   = strlen (remainder + (*remainder == '/'));
  filename = xmalloc (fill - vp->name + length + 2);
  fill = mempcpy (filename, vp->name,
		  /* Include the separator character (*FILL) within
		     this copy.  */
		  fill - vp->name + 1);
  /* Skip a leading separator in REMAINDER.  */
  strcpy (fill, remainder + (*remainder == '/'));

  /* Use this filename to find a vnode relative to the start of this
     tree.  */

  vnode = android_saf_tree_name (&tree.vnode, filename,
				 strlen (filename));
  xfree (filename);
  return vnode;
}

static int
android_saf_tree_open (struct android_vnode *vnode, int flags,
		       mode_t mode, bool asset_p, int *fd,
		       AAsset **asset)
{
  /* Don't allow opening this special directory.  */
  errno = ENOSYS;
  return -1;
}

static void
android_saf_tree_close (struct android_vnode *vnode)
{
  struct android_saf_tree_vnode *vp;
  int save_errno;

  vp = (struct android_saf_tree_vnode *) vnode;

  save_errno = errno;
  xfree ((void *) vp->tree_uri);
  xfree (vp->tree_id);
  xfree (vp->name);
  xfree (vp->document_id);
  xfree (vp);
  errno = save_errno;
}

static int
android_saf_tree_unlink (struct android_vnode *vnode)
{
  errno = EISDIR;
  return -1;
}

static int
android_saf_tree_symlink (const char *target, struct android_vnode *vnode)
{
  errno = EPERM;
  return -1;
}

static int
android_saf_tree_rmdir (struct android_vnode *vnode)
{
  struct android_saf_tree_vnode *vp;

  vp = (struct android_saf_tree_vnode *) vnode;

  /* Don't allow deleting the root directory.  */

  if (!vp->document_id)
    {
      errno = EROFS;
      return -1;
    }

  return android_saf_delete_document (vp->tree_uri,
				      vp->document_id,
				      vp->name);
}

static int
android_saf_tree_rename (struct android_vnode *src,
			 struct android_vnode *dst,
			 bool keep_existing)
{
  char *last, *dst_last;
  struct android_saf_tree_vnode *vp, *vdst;
  char path[EMACS_PATH_MAX], path1[EMACS_PATH_MAX];
  char *fill, *dst_id;
  int rc;

  /* If dst isn't a tree, file or new vnode, return EXDEV.  */

  if (dst->type != ANDROID_VNODE_SAF_TREE
      && dst->type != ANDROID_VNODE_SAF_FILE
      && dst->type != ANDROID_VNODE_SAF_NEW)
    {
      errno = EXDEV;
      return -1;
    }

  vp = (struct android_saf_tree_vnode *) src;
  vdst = (struct android_saf_tree_vnode *) dst;

  /* if vp and vdst refer to different tree URIs, return EXDEV.  */

  if (strcmp (vp->tree_uri, vdst->tree_uri))
    {
      errno = EXDEV;
      return -1;
    }

  /* If `keep_existing' and the destination vnode designates an
     existing file, return EEXIST.  */

  if (keep_existing && dst->type != ANDROID_VNODE_SAF_NEW)
    {
      errno = EEXIST;
      return -1;
    }

  /* Unix `rename' maps to two Android content provider operations.
     The first case is a simple rename, where src and dst are both
     located within the same directory.  Compare the file names of
     both up to the component before the last.  */

  last = strrchr (vp->name, '/');
  eassert (last != NULL);

  if (last[1] == '\0')
    {
      if (last == vp->name)
	{
	  /* This means the caller is trying to rename the root
	     directory of the tree.  */
	  errno = EROFS;
	  return -1;
	}

      /* The name is terminated by a trailing directory separator.
         Search backwards for the preceding directory separator.  */
      last = memrchr (vp->name, '/', last - vp->name);
      eassert (last != NULL);
    }

  /* Find the end of the second-to-last component in vdst's name.  */

  dst_last = strrchr (vdst->name, '/');
  eassert (dst_last != NULL);

  if (dst_last[1] == '\0')
    {
      if (dst_last == vdst->name)
	{
	  /* Forbid overwriting the root of the tree either.  */
	  errno = EROFS;
	  return -1;
	}

      dst_last = memrchr (vdst->name, '/', dst_last - vdst->name);
      eassert (dst_last != NULL);
    }

  if (dst_last - vdst->name != last - vp->name
      || memcmp (vp->name, vdst->name, last - vp->name))
    {
      /* The second case is where the file must be moved from one
         directory to the other, and possibly then recreated under a
         new name.  */

      /* The names of the source and destination directories will have
	 to be copied to path.  */

      if (last - vp->name >= EMACS_PATH_MAX
	  || dst_last - vdst->name >= EMACS_PATH_MAX)
	{
	  errno = ENAMETOOLONG;
	  return -1;
	}

      fill = mempcpy (path, vp->name, last - vp->name);
      *fill = '\0';

      /* If vdst doesn't already exist, its document_id field is
	 already the name of its parent directory.  */

      if (dst->type == ANDROID_VNODE_SAF_NEW)
	{
	  /* First, move the document.  This will update
	     VP->document_id if it changes.  */

	  if (android_saf_move_document (vp->tree_uri,
					 &vp->document_id,
					 path,
					 vdst->document_id))
	    return -1;

	  fill = mempcpy (path, vdst->name, dst_last - vdst->name);
	  *fill = '\0';

	  /* Next, rename the document, if its display name differs
	     from that of the source.  */

	  if (strcmp (dst_last + 1, last + 1)
	      /* By now vp->document_id is already in the destination
		 directory.  */
	      && android_saf_rename_document (vp->tree_uri,
					      vp->document_id,
					      path,
					      dst_last + 1))
	    return -1;

	  return 0;
	}

      /* Retrieve the ID designating the destination document's parent
	 directory.  */

      fill = mempcpy (path1, vdst->name, dst_last - vdst->name);
      *fill = '\0';

      rc = android_document_id_from_name (vp->tree_uri,
					  path1, &dst_id);

      if (rc != 1)
	{
	  /* This file is either not a directory or nonexistent.  */

	  switch (rc)
	    {
	    case 0:
	      errno = ENOTDIR;
	      goto error;

	    case -1:
	      /* dst_id is not set here, as the penultimate component
		 also couldn't be located.  */
	      errno = ENOENT;
	      return -1;

	    case -2:
	      errno = ENOENT;
	      goto error;

	    default:
	      emacs_abort ();
	    }
	}

      /* vdst already exists, so it needs to be deleted first.  */

      if (android_saf_delete_document (vdst->tree_uri,
				       vdst->document_id,
				       vdst->name))
        goto error;

      /* First, move the document.  This will update
	 VP->document_id if it changes.  */

      if (android_saf_move_document (vp->tree_uri,
				     &vp->document_id,
				     path, dst_id))
        goto error;

      /* Next, rename the document, if its display name differs from
	 that of the source.  */

      if (strcmp (dst_last + 1, last + 1)
	  /* By now vp->document_id is already in the destination
	     directory.  */
	  && android_saf_rename_document (vp->tree_uri,
					  vp->document_id,
					  path1,
					  dst_last + 1))
	goto error;

      xfree (dst_id);
      return 0;

    error:
      xfree (dst_id);
      return 1;
    }

  /* Otherwise, do this simple rename.  The name of the parent
     directory is required, as it provides the directory whose entries
     will be modified.  */

  if (last - vp->name >= EMACS_PATH_MAX)
    {
      errno = ENAMETOOLONG;
      return -1;
    }

  /* If the destination document exists, delete it.  */

  if (dst->type != ANDROID_VNODE_SAF_NEW
      && android_saf_delete_document (vdst->tree_uri,
				      vdst->document_id,
				      vdst->name))
    return -1;

  fill = mempcpy (path, vp->name, last - vp->name);
  *fill = '\0';
  return android_saf_rename_document (vp->tree_uri,
				      vp->document_id,
				      path,
				      dst_last + 1);
}

static int
android_saf_tree_stat (struct android_vnode *vnode,
		       struct stat *statb, int flags)
{
  struct android_saf_tree_vnode *vp;

  vp = (struct android_saf_tree_vnode *) vnode;

  return android_saf_stat (vp->tree_uri, vp->document_id,
			   statb, false);
}

static int
android_saf_tree_access (struct android_vnode *vnode, int mode)
{
  struct android_saf_tree_vnode *vp;

  vp = (struct android_saf_tree_vnode *) vnode;

  /* Validate MODE.  */

  if (mode != F_OK && !(mode & (W_OK | X_OK | R_OK)))
    {
      errno = EINVAL;
      return -1;
    }

  return android_saf_access (vp->tree_uri, vp->document_id,
			     mode & W_OK);
}

static int
android_saf_tree_mkdir (struct android_vnode *vnode, mode_t mode)
{
  /* Since tree vnodes represent files that already exist, return
     EEXIST.  */
  errno = EEXIST;
  return -1;
}

static int
android_saf_tree_chmod (struct android_vnode *vnode, mode_t mode,
			int flags)
{
  /* Return EACCESS should MODE contain unusual bits besides the
     standard file access permissions.  */

  if (mode & ~0777)
    {
      errno = EACCES;
      return -1;
    }

  /* Otherwise, no further action is necessary, as SAF nodes already
     pretend to be S_IRUSR | S_IWUSR.  */
  return 0;
}

static ssize_t
android_saf_tree_readlink (struct android_vnode *vnode, char *buffer,
			   size_t size)
{
  /* Return EINVAL.  Symlinks aren't exposed to clients by the
     SAF.  */
  errno = EINVAL;
  return -1;
}

/* Open a database Cursor containing each directory entry within the
   supplied SAF tree vnode VP.

   Value is NULL upon failure with errno set to a suitable value, a
   local reference to the Cursor object otherwise.  */

static jobject
android_saf_tree_opendir_1 (struct android_saf_tree_vnode *vp)
{
  jobject uri, id, cursor;
  jmethodID method;

  if (inside_saf_critical_section)
    {
      errno = EIO;
      return NULL;
    }

  /* Build strings for both URI and ID.  */
  uri = (*android_java_env)->NewStringUTF (android_java_env,
					   vp->tree_uri);
  android_exception_check ();

  if (vp->document_id)
    {
      id = (*android_java_env)->NewStringUTF (android_java_env,
					      vp->document_id);
      android_exception_check_1 (uri);
    }
  else
    id = NULL;

  /* Try to open the cursor.  */
  method = service_class.open_document_directory;
  inside_saf_critical_section = true;
  cursor
    = (*android_java_env)->CallNonvirtualObjectMethod (android_java_env,
						       emacs_service,
						       service_class.class,
						       method, uri, id);
  inside_saf_critical_section = false;

  if (id)
    {
      if (android_saf_exception_check (2, id, uri))
	return NULL;

      ANDROID_DELETE_LOCAL_REF (id);
    }
  else if (android_saf_exception_check (1, uri))
    return NULL;

  ANDROID_DELETE_LOCAL_REF (uri);

  /* Return the resulting cursor.  */
  return cursor;
}

static struct dirent *
android_saf_tree_readdir (struct android_vdir *vdir)
{
  struct android_saf_tree_vdir *dir;
  static struct dirent *dirent;
  jobject entry, d_name;
  jint d_type;
  jmethodID method;
  size_t length, size;
  const char *chars;
  struct coding_system coding;

  dir = (struct android_saf_tree_vdir *) vdir;

  /* Try to read one entry from the cursor.  */
  method = service_class.read_directory_entry;
  entry
    = (*android_java_env)->CallNonvirtualObjectMethod (android_java_env,
						       emacs_service,
						       service_class.class,
						       method, dir->cursor);
  android_exception_check ();

  /* If ENTRY is NULL, we're at the end of the directory.  */

  if (!entry)
    {
      xfree (entry);
      entry = NULL;
      return NULL;
    }

  /* Load both fields from ENTRY.  */
  d_name = (*android_java_env)->GetObjectField (android_java_env, entry,
						entry_class.d_name);
  if (!d_name)
    {
      /* If an error transpires, d_name is set to NULL.  */
      (*android_java_env)->ExceptionClear (android_java_env);
      ANDROID_DELETE_LOCAL_REF (entry);

      /* XXX: what would be a better error indication? */
      errno = EIO;
      return NULL;
    }

  /* d_type is 1 if this is a directory, and 0 if it's a regular
     file.  */
  d_type = (*android_java_env)->GetIntField (android_java_env, entry,
					     entry_class.d_type);
  ANDROID_DELETE_LOCAL_REF (entry);

  /* Copy the name of the directory over.  */
  chars = (*android_java_env)->GetStringUTFChars (android_java_env,
						  (jstring) d_name,
						  NULL);
  android_exception_check_nonnull ((void *) chars, d_name);

  /* Decode this JNI string into utf-8-emacs; see
     android_vfs_convert_name for considerations regarding coding
     systems.  */
  length = strlen (chars);
  setup_coding_system (Qandroid_jni, &coding);
  coding.mode |= CODING_MODE_LAST_BLOCK;
  coding.source = (const unsigned char *) chars;
  coding.dst_bytes = 0;
  coding.destination = NULL;
  decode_coding_object (&coding, Qnil, 0, 0, length, length, Qnil);

  /* Release the string data and the local reference to STRING.  */
  (*android_java_env)->ReleaseStringUTFChars (android_java_env,
					      (jstring) d_name,
					      chars);

  /* Resize dirent to accommodate the decoded text.  */
  size   = offsetof (struct dirent, d_name) + 1 + coding.produced;
  dirent = xrealloc (dirent, size);

  /* Clear dirent.  */
  memset (dirent, 0, size);

  /* Fill in the generic directory information and copy the string
     over.  */
  dirent->d_ino = 0;
  dirent->d_off = 0;
  dirent->d_reclen = size;
  dirent->d_type = d_type ? DT_DIR : DT_UNKNOWN;
  memcpy (dirent->d_name, coding.destination, coding.produced);
  dirent->d_name[coding.produced] = '\0';

  /* Free the coding system destination buffer.  */
  xfree (coding.destination);

  ANDROID_DELETE_LOCAL_REF (d_name);
  return dirent;
}

static void
android_saf_tree_closedir (struct android_vdir *vdir)
{
  struct android_saf_tree_vdir *dir, **next, *tem;

  dir = (struct android_saf_tree_vdir *) vdir;

  /* dir->name is allocated by asprintf, which uses regular
     malloc.  */
  free (dir->name);

  /* Yes, DIR->cursor is a local reference.  */
  (*android_java_env)->CallVoidMethod (android_java_env,
				       dir->cursor,
				       cursor_class.close);
  (*android_java_env)->ExceptionClear (android_java_env);
  ANDROID_DELETE_LOCAL_REF (dir->cursor);

  /* If the ``directory file descriptor'' has been opened, close
     it.  */
  if (dir->fd != -1)
    close (dir->fd);

  /* Now unlink this directory.  */

  for (next = &all_saf_tree_vdirs; (tem = *next);)
    {
      if (tem == dir)
	*next = dir->next;
      else
	next = &(*next)->next;
    }

  xfree (dir);
}

static int
android_saf_tree_dirfd (struct android_vdir *vdir)
{
  struct android_saf_tree_vdir *dir;

  dir = (struct android_saf_tree_vdir *) vdir;

  /* Since `android_saf_tree_opendir' tries to avoid opening a file
     descriptor if readdir isn't called, dirfd can fail if open fails.

     open sets errno to a set of errors different from what POSIX
     stipulates for dirfd, but for ease of implementation the open
     errors are used instead.  */

  if (dir->fd >= 0)
    return dir->fd;

  dir->fd = open ("/dev/null", O_RDONLY | O_CLOEXEC);
  return dir->fd;
}

static struct android_vdir *
android_saf_tree_opendir (struct android_vnode *vnode)
{
  struct android_saf_tree_vnode *vp;
  struct android_saf_tree_vdir *dir;
  char *fill, *end;
  jobject cursor;
  char component[EMACS_PATH_MAX];

  vp = (struct android_saf_tree_vnode *) vnode;

  /* First, fill the directory stream with the right functions and
     file name.  */

  dir = xmalloc (sizeof *dir);
  dir->vdir.readdir = android_saf_tree_readdir;
  dir->vdir.closedir = android_saf_tree_closedir;
  dir->vdir.dirfd = android_saf_tree_dirfd;

  /* Derive the authority from the URI.  */

  fill = (char *) vp->tree_uri;

  if (strncmp (fill, "content://", 10))
    emacs_abort ();

  /* Skip the content header.  */
  fill += sizeof "content://" - 1;

  /* The authority segment of the URI is between here and the
     next slash.  */

  end = strchr (fill, '/');

  if (!end)
    emacs_abort ();

  if (end - fill >= EMACS_PATH_MAX)
    {
      errno = ENAMETOOLONG;
      xfree (dir);
      return NULL;
    }

  /* Copy the authority over.  */

  memcpy (component, fill, end - fill);
  component[end - fill] = '\0';

  if (asprintf (&dir->name, "/content/storage/%s/%s%s",
		component, vp->tree_id, vp->name) < 0)
    {
      /* Out of memory.  */
      xfree (dir);
      memory_full (0);
    }

  /* Now open a cursor that iterates through each file in this
     directory.  */

  cursor = android_saf_tree_opendir_1 (vp);

  if (!cursor)
    {
      xfree (dir->name);
      xfree (dir);
      return NULL;
    }

  dir->cursor = cursor;
  dir->fd = -1;
  dir->next = all_saf_tree_vdirs;
  all_saf_tree_vdirs = dir;
  return &dir->vdir;
}

/* Create a vnode designating the file NAME within a directory tree
   whose identifier is TREE.  As with all other `name' functions, NAME
   may be modified.

   AUTHORITY is the name of the content provider authority that is
   offering TREE.

   Value is NULL and errno is set if no document tree or provider by
   those names exists, or some other error takes place (for example,
   if TREE and AUTHORITY aren't encoded correctly.)  */

static struct android_vnode *
android_saf_tree_from_name (char *name, const char *tree,
			    const char *authority)
{
  struct android_saf_tree_vnode root;
  jobject tree_string, authority_string, result;
  jmethodID method;
  const char *uri;
  struct android_vnode *vp;

  /* It's not a given that NAME and TREE are actually in the modified
     UTF-8 format used by the JVM to encode strings, and the JVM
     aborts when encountering a string that is not.  Make sure they
     are valid before continuing.  */

  if (android_verify_jni_string (name)
      || android_verify_jni_string (authority))
    {
      errno = ENOENT;
      return NULL;
    }

  tree_string = (*android_java_env)->NewStringUTF (android_java_env,
						   tree);
  android_exception_check ();

  authority_string
    = (*android_java_env)->NewStringUTF (android_java_env,
					 authority);
  android_exception_check_1 (tree_string);

  /* Now create the URI and detect if Emacs has the rights to access
     it.  */

  method = service_class.get_tree_uri;
  result
    = (*android_java_env)->CallNonvirtualObjectMethod (android_java_env,
						       emacs_service,
						       service_class.class,
						       method, tree_string,
						       authority_string);
  android_exception_check_2 (tree_string, authority_string);
  ANDROID_DELETE_LOCAL_REF (tree_string);
  ANDROID_DELETE_LOCAL_REF (authority_string);

  /* If it doesn't, return NULL and set errno to ENOENT.  */

  if (!result)
    {
      errno = ENOENT;
      return NULL;
    }

  /* Otherwise, decode this string.  */
  uri = (*android_java_env)->GetStringUTFChars (android_java_env, result,
						NULL);
  android_exception_check_nonnull ((void *) uri, result);

  /* Fill in root.tree_uri with values that represent the root of this
     document tree.  */

  root.vnode.ops = &saf_tree_vfs_ops;
  root.vnode.type = ANDROID_VNODE_SAF_TREE;
  root.vnode.flags = 0;
  root.tree_uri = uri;
  root.tree_id = (char *) tree;
  root.document_id = NULL;
  root.name = (char *) "/";

  vp = (*root.vnode.ops->name) (&root.vnode, name, strlen (name));
  (*android_java_env)->ReleaseStringUTFChars (android_java_env,
					      (jstring) result, uri);
  ANDROID_DELETE_LOCAL_REF (result);
  return vp;
}

/* Return any open SAF tree directory stream for which dirfd has
   returned the file descriptor DIRFD.  Return NULL otherwise.  */

static struct android_saf_tree_vdir *
android_saf_tree_get_directory (int dirfd)
{
  struct android_saf_tree_vdir *dir;

  for (dir = all_saf_tree_vdirs; dir; dir = dir->next)
    {
      if (dir->fd == dirfd && dirfd != -1)
	return dir;
    }

  return NULL;
}



/* SAF file vnode.  The information used to uniquely identify a file
   is identical to that used to identify an SAF directory, but the
   vnode operations are different.  */

/* Define `struct android_saf_file_vnode' to be identical to a file
   vnode.  */

#define android_saf_file_vnode android_saf_tree_vnode

/* Structure describing an open ParcelFileDescriptor.  */

struct android_parcel_fd
{
  /* The next open parcel file descriptor.  */
  struct android_parcel_fd *next;

  /* Global reference to this parcel file descriptor.  */
  jobject descriptor;

  /* The modification time of this parcel file descriptor, or
     `invalid_timespec'.  */
  struct timespec mtime;

  /* The file descriptor itself.  */
  int fd;
};

static struct android_vnode *android_saf_file_name (struct android_vnode *,
						    char *, size_t);
static int android_saf_file_open (struct android_vnode *, int,
				  mode_t, bool, int *, AAsset **);
static int android_saf_file_unlink (struct android_vnode *);
static int android_saf_file_rmdir (struct android_vnode *);
static struct android_vdir *android_saf_file_opendir (struct android_vnode *);

/* Vector of VFS operations associated with SAF tree VFS nodes.  */

static struct android_vops saf_file_vfs_ops =
  {
    android_saf_file_name,
    android_saf_file_open,
    android_saf_tree_close,
    android_saf_file_unlink,
    android_saf_tree_symlink,
    android_saf_file_rmdir,
    android_saf_tree_rename,
    android_saf_tree_stat,
    android_saf_tree_access,
    android_saf_tree_mkdir,
    android_saf_tree_chmod,
    android_saf_tree_readlink,
    android_saf_file_opendir,
  };

/* Chain of all parcel file descriptors currently open.  */
static struct android_parcel_fd *open_parcel_fds;

static struct android_vnode *
android_saf_file_name (struct android_vnode *vnode, char *name,
		       size_t length)
{
  struct android_saf_file_vnode *vp;

  /* If LENGTH is empty, make a copy of this vnode and return it.  */

  if (length < 1)
    {
      vp = xmalloc (sizeof *vp);
      memcpy (vp, vnode, sizeof *vp);

      /* Duplicate the information contained within VNODE.  */

      vp->tree_uri = xstrdup (vp->tree_uri);
      vp->tree_id = xstrdup (vp->tree_id);
      vp->name = xstrdup (vp->name);
      vp->document_id = xstrdup (vp->name);

      return &vp->vnode;
    }

  /* A file vnode has no children of its own.  */
  errno = ENOTDIR;
  return NULL;
}

static int
android_saf_file_open (struct android_vnode *vnode, int flags,
		       mode_t mode, bool asset_p, int *fd_return,
		       AAsset **asset)
{
  struct android_saf_file_vnode *vp;
  jobject uri, id, descriptor;
  jmethodID method;
  jboolean read, trunc, write;
  jint fd;
  struct android_parcel_fd *info;
  struct stat statb;

  if (inside_saf_critical_section)
    {
      errno = EIO;
      return -1;
    }

  /* O_APPEND isn't supported as a consequence of Android content
     providers defaulting to truncating the file.  */

  if (flags & O_APPEND)
    {
      errno = EOPNOTSUPP;
      return -1;
    }

  /* Build strings for both the URI and ID.  */

  vp = (struct android_saf_file_vnode *) vnode;
  uri = (*android_java_env)->NewStringUTF (android_java_env,
					   vp->tree_uri);
  android_exception_check ();
  id = (*android_java_env)->NewStringUTF (android_java_env,
					  vp->document_id);
  android_exception_check_1 (uri);

  /* Open a parcel file descriptor according to flags.  Documentation
     for the SAF openDocument operation is scant and seldom helpful.
     From observations made, it is clear that their file access modes
     are inconsistently implemented, and that at least:

       r   = either an FIFO or a real file, without truncation.
       w   = either an FIFO or a real file, with OR without truncation.
       wt  = either an FIFO or a real file, with truncation.
       rw  = a real file, without truncation.
       rwt = a real file, with truncation.

     This diverges from the self-contradicting documentation, where
     openDocument says nothing about truncation, and openFile mentions
     that w can elect not to truncate and programs which rely on
     truncation should use wt.

     Since Emacs is prepared to handle FIFOs within fileio.c, simply
     specify the straightforward relationship between FLAGS and the
     file access modes listed above.  */

  method = service_class.open_document;
  read = trunc = write = false;

  if ((flags & O_RDWR) == O_RDWR || (flags & O_WRONLY))
    write = true;

  if (flags & O_TRUNC)
    trunc = true;

  if ((flags & O_RDWR) == O_RDWR || !write)
    read = true;

  inside_saf_critical_section = true;
  descriptor
    = (*android_java_env)->CallNonvirtualObjectMethod (android_java_env,
						       emacs_service,
						       service_class.class,
						       method, uri, id,
						       read, write, trunc);
  inside_saf_critical_section = false;

  if (android_saf_exception_check (2, uri, id))
    return -1;

  ANDROID_DELETE_LOCAL_REF (uri);
  ANDROID_DELETE_LOCAL_REF (id);

  if (!descriptor)
    {
      /* Assume that permission has been denied if DESCRIPTOR cannot
	 be opened.  */
      errno = EPERM;
      return -1;
    }

  /* Allocate a record for this file descriptor.  Parcel file
     descriptors should be closed using their own `close' function,
     which takes care of notifying the source that it has been
     closed.  */
  info = xmalloc (sizeof *info);

  /* Now obtain the file descriptor.  */
  fd = (*android_java_env)->CallIntMethod (android_java_env,
					   descriptor,
					   fd_class.get_fd);
  android_exception_check_1 (descriptor);

  /* Create a global reference to descriptor.  */
  info->descriptor
    = (*android_java_env)->NewGlobalRef (android_java_env,
					 descriptor);

  if (!info->descriptor)
    {
      /* If the global reference can't be created, delete
	 descriptor.  */
      (*android_java_env)->ExceptionClear (android_java_env);
      (*android_java_env)->CallVoidMethod (android_java_env,
					   descriptor,
					   fd_class.close);
      (*android_java_env)->ExceptionClear (android_java_env);
      ANDROID_DELETE_LOCAL_REF (descriptor);

      /* Free INFO.  */
      xfree (info);

      /* Set errno to EMFILE and return.  */
      errno = EMFILE;
      return -1;
    }

  /* Delete the local ref to DESCRIPTOR.  */
  ANDROID_DELETE_LOCAL_REF (descriptor);

  /* Try to retrieve the modification time of this file from the
     content provider.

     Refrain from introducing the file status into the file status
     cache if FLAGS & O_RDWR or FLAGS & O_WRONLY: the cached file
     status will contain a size and modification time inconsistent
     with the result of any modifications that later transpire.  */

  if (!android_saf_stat (vp->tree_uri, vp->document_id,
			 &statb, write))
    info->mtime = get_stat_mtime (&statb);
  else
    info->mtime = invalid_timespec ();

  /* Set info->fd and chain it onto the list.  */
  info->fd = fd;
  info->next = open_parcel_fds;
  open_parcel_fds = info;

  /* Return the file descriptor.  */
  *fd_return = fd;
  return 0;
}

static int
android_saf_file_unlink (struct android_vnode *vnode)
{
  struct android_saf_file_vnode *vp;

  vp = (struct android_saf_file_vnode *) vnode;
  return android_saf_delete_document (vp->tree_uri,
				      vp->document_id,
				      vp->name);
}

static int
android_saf_file_rmdir (struct android_vnode *vnode)
{
  errno = ENOTDIR;
  return -1;
}

static struct android_vdir *
android_saf_file_opendir (struct android_vnode *vnode)
{
  errno = ENOTDIR;
  return NULL;
}

/* Close FD if it's a parcel file descriptor and return true.
   If FD isn't, return false.

   Such file descriptors need to be closed using a function
   written in Java, to tell the sender that it has been
   closed.  */

static bool
android_close_parcel_fd (int fd)
{
  struct android_parcel_fd *tem, **next, *temp;

  for (next = &open_parcel_fds; (tem = *next);)
    {
      if (tem->fd == fd)
	{
	  (*android_java_env)->CallVoidMethod (android_java_env,
					       tem->descriptor,
					       fd_class.close);

	  /* Ignore exceptions for the same reason EINTR errors from
	     `close' should be ignored.  */
	  (*android_java_env)->ExceptionClear (android_java_env);
	  (*android_java_env)->DeleteGlobalRef (android_java_env,
						tem->descriptor);

	  temp = tem->next;
	  xfree (tem);
	  *next = temp;

	  return true;
	}
      else
	next = &(*next)->next;
    }

  return false;
}



/* SAF ``new'' vnodes.  These nodes share their data structures
   with tree and file vnodes, but represent files that don't actually
   exist within a directory.  In them, the document ID represents not
   the file designated by the vnode itself, but rather its parent
   directory.

   The only vops defined serve to create directories or files, at
   which point the vnode becomes invalid.  */

#define android_saf_new_vnode android_saf_tree_vnode

static struct android_vnode *android_saf_new_name (struct android_vnode *,
						   char *, size_t);
static int android_saf_new_open (struct android_vnode *, int,
				 mode_t, bool, int *, AAsset **);
static int android_saf_new_unlink (struct android_vnode *);
static int android_saf_new_symlink (const char *, struct android_vnode *);
static int android_saf_new_rmdir (struct android_vnode *);
static int android_saf_new_rename (struct android_vnode *,
				   struct android_vnode *, bool);
static int android_saf_new_stat (struct android_vnode *, struct stat *, int);
static int android_saf_new_access (struct android_vnode *, int);
static int android_saf_new_mkdir (struct android_vnode *, mode_t);
static int android_saf_new_chmod (struct android_vnode *, mode_t, int);
static ssize_t android_saf_new_readlink (struct android_vnode *, char *,
					 size_t);
static struct android_vdir *android_saf_new_opendir (struct android_vnode *);

/* Vector of VFS operations associated with SAF new VFS nodes.  */

static struct android_vops saf_new_vfs_ops =
  {
    android_saf_new_name,
    android_saf_new_open,
    android_saf_tree_close,
    android_saf_new_unlink,
    android_saf_new_symlink,
    android_saf_new_rmdir,
    android_saf_new_rename,
    android_saf_new_stat,
    android_saf_new_access,
    android_saf_new_mkdir,
    android_saf_new_chmod,
    android_saf_new_readlink,
    android_saf_new_opendir,
  };

static struct android_vnode *
android_saf_new_name (struct android_vnode *vnode, char *name,
		      size_t length)
{
  struct android_saf_new_vnode *vp;

  /* If LENGTH is empty, make a copy of this vnode and return it.  */

  if (length < 1)
    {
      vp = xmalloc (sizeof *vp);
      memcpy (vp, vnode, sizeof *vp);

      /* Duplicate the information contained within VNODE.  */

      vp->tree_uri = xstrdup (vp->tree_uri);
      vp->tree_id = xstrdup (vp->tree_id);
      vp->name = xstrdup (vp->name);
      vp->document_id = xstrdup (vp->name);

      return &vp->vnode;
    }

  /* A nonexistent vnode has no children of its own.  */
  errno = ENOTDIR;
  return NULL;
}

static int
android_saf_new_open (struct android_vnode *vnode, int flags,
		      mode_t mode, bool asset_p, int *fd_return,
		      AAsset **asset)
{
  struct android_saf_new_vnode *vp;
  char *end;
  jstring name, id, uri, new_id;
  const char *new_doc_id;
  jmethodID method;

  /* If creating a file wasn't intended, return ENOENT.  */

  if (!(flags & O_CREAT))
    {
      errno = ENOENT;
      return -1;
    }

  /* If vp->name indicates that it's a directory, return ENOENT.  */

  vp = (struct android_saf_new_vnode *) vnode;
  end = strrchr (vp->name, '/');

  /* VP->name must contain at least one directory separator.  */
  eassert (end);

  if (end[1] == '\0')
    {
      errno = ENOENT;
      return -1;
    }

  /* Otherwise, try to create a new document.  First, build strings
     for the name, ID and document URI.  */

  name = (*android_java_env)->NewStringUTF (android_java_env,
					    end + 1);
  android_exception_check ();
  id = (*android_java_env)->NewStringUTF (android_java_env,
					  vp->document_id);
  android_exception_check_1 (name);
  uri = (*android_java_env)->NewStringUTF (android_java_env,
					   vp->tree_uri);
  android_exception_check_2 (name, id);

  /* Next, try to create a new document and retrieve its ID.  */

  method = service_class.create_document;
  new_id = (*android_java_env)->CallNonvirtualObjectMethod (android_java_env,
							    emacs_service,
							    service_class.class,
							    method, uri, id,
							    name);

  if (android_saf_exception_check (3, name, id, uri))
    return -1;

  /* Delete unused local references.  */
  ANDROID_DELETE_LOCAL_REF (name);
  ANDROID_DELETE_LOCAL_REF (id);
  ANDROID_DELETE_LOCAL_REF (uri);

  if (!new_id)
    {
      /* The file couldn't be created for some reason.  */
      errno = EIO;
      return -1;
    }

  /* Now, free VP->document_id and replace it with the service
     document ID.  */

  new_doc_id = (*android_java_env)->GetStringUTFChars (android_java_env,
						       new_id, NULL);
  android_exception_check_nonnull ((void *) new_doc_id, new_id);

  xfree (vp->document_id);
  vp->document_id = xstrdup (new_doc_id);

  (*android_java_env)->ReleaseStringUTFChars (android_java_env,
					      new_id, new_doc_id);
  ANDROID_DELETE_LOCAL_REF (new_id);

  /* Finally, transform this vnode into a file vnode and call its
     `open' function.  */
  vp->vnode.type = ANDROID_VNODE_SAF_FILE;
  vp->vnode.ops = &saf_file_vfs_ops;
  return (*vp->vnode.ops->open) (vnode, flags, mode, asset_p,
				 fd_return, asset);
}

static int
android_saf_new_unlink (struct android_vnode *vnode)
{
  errno = ENOENT;
  return -1;
}

static int
android_saf_new_symlink (const char *target, struct android_vnode *vnode)
{
  errno = EPERM;
  return -1;
}

static int
android_saf_new_rmdir (struct android_vnode *vnode)
{
  errno = ENOENT;
  return -1;
}

static int
android_saf_new_rename (struct android_vnode *src,
			struct android_vnode *dst,
			bool keep_existing)
{
  errno = ENOENT;
  return -1;
}

static int
android_saf_new_stat (struct android_vnode *vnode,
		      struct stat *statb, int flags)
{
  errno = ENOENT;
  return -1;
}

static int
android_saf_new_access (struct android_vnode *vnode, int mode)
{
  if (mode != F_OK && !(mode & (W_OK | X_OK | R_OK)))
    errno = EINVAL;
  else
    errno = ENOENT;

  return -1;
}

static int
android_saf_new_mkdir (struct android_vnode *vnode, mode_t mode)
{
  struct android_saf_new_vnode *vp;
  jstring name, id, uri, new_id;
  jmethodID method;
  const char *new_doc_id;
  char *end;

  vp = (struct android_saf_tree_vnode *) vnode;

  /* Find the last component of vp->name.  */
  end = strrchr (vp->name, '/');

  /* VP->name must contain at least one directory separator.  */
  eassert (end);

  if (end[1] == '\0')
    {
      /* There's a trailing directory separator.  Search
	 backwards.  */

      end--;
      while (end != vp->name && *end != '/')
	end--;

      /* vp->name[0] is always a directory separator.  */
      eassert (*end == '/');
    }

  /* Otherwise, try to create a new document.  First, build strings
     for the name, ID and document URI.  */

  name = (*android_java_env)->NewStringUTF (android_java_env,
					    end + 1);
  android_exception_check ();
  id = (*android_java_env)->NewStringUTF (android_java_env,
					  vp->document_id);
  android_exception_check_1 (name);
  uri = (*android_java_env)->NewStringUTF (android_java_env,
					   vp->tree_uri);
  android_exception_check_2 (name, id);

  /* Next, try to create a new document and retrieve its ID.  */

  method = service_class.create_directory;
  new_id = (*android_java_env)->CallNonvirtualObjectMethod (android_java_env,
							    emacs_service,
							    service_class.class,
							    method, uri, id,
							    name);

  if (android_saf_exception_check (3, name, id, uri))
    return -1;

  /* Delete unused local references.  */
  ANDROID_DELETE_LOCAL_REF (name);
  ANDROID_DELETE_LOCAL_REF (id);
  ANDROID_DELETE_LOCAL_REF (uri);

  if (!new_id)
    {
      /* The file couldn't be created for some reason.  */
      errno = EIO;
      return -1;
    }

  /* Now, free VP->document_id and replace it with the service
     document ID.  */

  new_doc_id = (*android_java_env)->GetStringUTFChars (android_java_env,
						       new_id, NULL);

  if (android_saf_check_nonnull (new_doc_id, 3, name, id, uri))
    return -1;

  xfree (vp->document_id);
  vp->document_id = xstrdup (new_doc_id);

  (*android_java_env)->ReleaseStringUTFChars (android_java_env,
					      new_id, new_doc_id);
  ANDROID_DELETE_LOCAL_REF (new_id);

  /* Finally, transform this vnode into a directory vnode.  */
  vp->vnode.type = ANDROID_VNODE_SAF_TREE;
  vp->vnode.ops = &saf_tree_vfs_ops;
  return 0;
}

static int
android_saf_new_chmod (struct android_vnode *vnode, mode_t mode,
		       int flags)
{
  errno = ENOENT;
  return -1;
}

static ssize_t
android_saf_new_readlink (struct android_vnode *vnode, char *buffer,
			  size_t size)
{
  errno = ENOENT;
  return -1;
}

static struct android_vdir *
android_saf_new_opendir (struct android_vnode *vnode)
{
  errno = ENOENT;
  return NULL;
}



/* Synchronization between SAF and Emacs.  Consult EmacsSafThread.java
   for more details.  */

/* Semaphore posted upon the completion of an SAF operation.  */
static sem_t saf_completion_sem;

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wmissing-prototypes"
#else /* GNUC */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-prototypes"
#endif /* __clang__ */

JNIEXPORT jint JNICALL
NATIVE_NAME (safSyncAndReadInput) (JNIEnv *env, jobject object)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  while (sem_wait (&saf_completion_sem) < 0)
    {
      if (input_blocked_p ())
	continue;

      process_pending_signals ();

      if (!NILP (Vquit_flag))
	{
	  __android_log_print (ANDROID_LOG_VERBOSE, __func__,
			       "quitting from IO operation");
	  return 1;
	}
    }

  return 0;
}

JNIEXPORT void JNICALL
NATIVE_NAME (safSync) (JNIEnv *env, jobject object)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  while (sem_wait (&saf_completion_sem) < 0)
    process_pending_signals ();
}

JNIEXPORT void JNICALL
NATIVE_NAME (safPostRequest) (JNIEnv *env, jobject object)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  sem_post (&saf_completion_sem);
}

JNIEXPORT jboolean JNICALL
NATIVE_NAME (ftruncate) (JNIEnv *env, jobject object, jint fd)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  if (ftruncate (fd, 0) < 0)
    return false;

  /* Reset the file pointer.  */
  if (lseek (fd, 0, SEEK_SET) < 0)
    return false;

  return true;
}

#ifdef __clang__
#pragma clang diagnostic pop
#else /* GNUC */
#pragma GCC diagnostic pop
#endif /* __clang__ */



/* Root vnode.  This vnode represents the root inode, and is a regular
   Unix vnode with modifications to `name' so that it returns asset and
   content vnodes, and to `opendir', so that asset and content vnodes
   are read from the root directory, whether or not Emacs holds rights
   to access the underlying filesystem.  */

struct android_root_vdir
{
  /* The directory function table.  */
  struct android_vdir vdir;

  /* The directory stream, or NULL if it could not be opened.  */
  DIR *directory;

  /* Index of the next directory to return in `special_vnodes'.  */
  int index;
};

/* File descriptor for instances of the foregoing structure when the
   true root is unavailable.  */
static int root_fd = -1;

/* Number of open instances referencing this file descriptor.  */
static ptrdiff_t root_fd_references;

static struct android_vnode *android_root_name (struct android_vnode *,
						char *, size_t);
static struct android_vdir *android_root_opendir (struct android_vnode *);

/* Vector of VFS operations associated with Unix root filesystem VFS
   nodes.  */

static struct android_vops root_vfs_ops =
  {
    android_root_name,
    android_unix_open,
    android_unix_close,
    android_unix_unlink,
    android_unix_symlink,
    android_unix_rmdir,
    android_unix_rename,
    android_unix_stat,
    android_unix_access,
    android_unix_mkdir,
    android_unix_chmod,
    android_unix_readlink,
    android_root_opendir,
  };

/* Array of special named vnodes.  */

static struct android_special_vnode special_vnodes[] =
  {
    { "assets",  6, android_afs_initial,	},
    { "content", 7, android_content_initial,
      LISPSYM_INITIALLY (Qandroid_jni),		},
  };

/* Convert the file name NAME from Emacs's internal character encoding
   to CODING, and return a Lisp string with the data so produced.

   Calling this function creates an implicit assumption that
   `file-name-coding-system' is compatible with `utf-8-emacs', which is
   not unacceptable as users with cause to modify
   file-name-coding-system should be aware and prepared for adverse
   consequences affecting files stored on different filesystems,
   including virtual ones.  */

static Lisp_Object
android_vfs_convert_name (const char *name, Lisp_Object coding)
{
  Lisp_Object name1;

  /* Convert the contents of the buffer after BUFFER_END from the file
     name coding system to special->special_coding_system.  */
  name1 = build_string (name);
  name1 = code_convert_string (name1, coding, Qt, true, true, true);
  return name1;
}

static struct android_vnode *
android_root_name (struct android_vnode *vnode, char *name,
		   size_t length)
{
  char *component_end;
  struct android_special_vnode *special;
  size_t i;
  Lisp_Object file_name;
  struct android_vnode *vp;
  struct android_unix_vnode *unix_vp;

  /* Skip any leading separator in NAME.  */

  if (*name == '/')
    name++, length--;

  /* Look for the first directory separator.  */
  component_end = strchr (name, '/');

  /* If not there, use name + length.  */

  if (!component_end)
    component_end = name + length;
  else
    /* Move past the separator character.  */
    component_end++;

  /* Now, find out if the first component is a special vnode; if so,
     call its root lookup function with the rest of NAME there.  */

  for (i = 0; i < ARRAYELTS (special_vnodes); ++i)
    {
      special = &special_vnodes[i];

      if (component_end - name == special->length
	  && !memcmp (special->name, name, special->length))
	{
	  if (!NILP (special->special_coding_system))
	    {
	      USE_SAFE_ALLOCA;

	      file_name
		= android_vfs_convert_name (component_end,
					    special->special_coding_system);

	      /* Allocate a buffer and copy file_name into the same.  */
	      length = SBYTES (file_name) + 1;
	      name = SAFE_ALLOCA (length);

	      /* Copy the trailing NULL byte also.  */
	      memcpy (name, SDATA (file_name), length);
	      vp = (*special->initial) (name, length - 1);
	      SAFE_FREE ();
	      return vp;
	    }

	  return (*special->initial) (component_end,
				      length - special->length);
	}

      /* Detect the case where a special is named with a trailing
	 directory separator.  */

      if (component_end - name == special->length + 1
	  && !memcmp (special->name, name, special->length)
	  && name[special->length] == '/')
	{
	  if (!NILP (special->special_coding_system))
	    {
	      USE_SAFE_ALLOCA;

	      file_name
		= android_vfs_convert_name (component_end - 1,
					    special->special_coding_system);

	      /* Allocate a buffer and copy file_name into the same.  */
	      length = SBYTES (file_name) + 1;
	      name = SAFE_ALLOCA (length);

	      /* Copy the trailing NULL byte also.  */
	      memcpy (name, SDATA (file_name), length);
	      vp = (*special->initial) (name, length - 1);
	      SAFE_FREE ();
	      return vp;
	    }

	  /* Make sure to include the directory separator.  */
	  return (*special->initial) (component_end - 1,
				      length - special->length);
	}
    }

  /* Otherwise, continue searching for a vnode normally, but duplicate
     the vnode manually if length is 0, as `android_unix_name' resets
     the vnode operation vector in copies.  */

  if (!length)
    {
      unix_vp = xmalloc (sizeof *unix_vp);
      memcpy (unix_vp, vnode, sizeof *unix_vp);
      unix_vp->name = xstrdup (unix_vp->name);
      return &unix_vp->vnode;
    }

  return android_unix_name (vnode, name, length);
}

static struct dirent *
android_root_readdir (struct android_vdir *vdir)
{
  struct android_root_vdir *dir;
  static struct dirent dirent, *p;

  dir = (struct android_root_vdir *) vdir;
  p   = dir->directory ? readdir (dir->directory) : NULL;

  if (p || dir->index >= ARRAYELTS (special_vnodes))
    return p;

  dirent.d_ino = 0;
  dirent.d_off = 0;
  dirent.d_reclen = sizeof dirent;
  dirent.d_type = DT_DIR;

  /* No element in special_vnode must overflow dirent.d_name.  */
  strcpy ((char *) &dirent.d_name,
	  special_vnodes[dir->index++].name);
  return &dirent;
}

static void
android_root_closedir (struct android_vdir *vdir)
{
  struct android_root_vdir *dir;

  dir = (struct android_root_vdir *) vdir;

  if (dir->directory)
    closedir (dir->directory);

  if (root_fd_references--)
    ;
  else
    {
      /* Close root_fd, for which no references remain.  */
      close (root_fd);
      root_fd = -1;
    }

  xfree (vdir);
}

static int
android_root_dirfd (struct android_vdir *vdir)
{
  eassert (root_fd != -1);
  return root_fd;
}

static struct android_vdir *
android_root_opendir (struct android_vnode *vnode)
{
  struct android_unix_vnode *vp;
  struct android_root_vdir *dir;
  DIR *directory;

  /* Try to opendir the vnode.  */
  vp = (struct android_unix_vnode *) vnode;

  directory = opendir (vp->name);

  /* Proceed with the remaining code if directory is nil, in which event
     directory functions will simply forgo listing files inside the real
     root directory.  */

  dir = xmalloc (sizeof *dir);
  dir->vdir.readdir = android_root_readdir;
  dir->vdir.closedir = android_root_closedir;
  dir->vdir.dirfd = android_root_dirfd;
  dir->directory = directory;
  dir->index = 0;

  /* Allocate a temporary file descriptor for this ersatz root.  This is
     required regardless of the value of DIRECTORY, as android_fstatat
     and co. will not defer to the VFS layer if a directory file
     descriptor is not known to be special.  */
  if (root_fd < 0)
    root_fd = open ("/dev/null", O_RDONLY | O_CLOEXEC);
  root_fd_references++;

  return &dir->vdir;
}



/* File system lookup.  */

/* Look up the vnode that designates NAME, a file name that is at least
   N bytes, converting between different file name coding systems as
   necessary.

   NAME may be either an absolute file name or a name relative to the
   current working directory.  It must not be longer than EMACS_PATH_MAX
   bytes.

   Value is NULL upon failure with errno set accordingly, or the
   vnode.  */

static struct android_vnode *
android_name_file (const char *name)
{
  char buffer[EMACS_PATH_MAX + 1], *head;
  const char *end;
  size_t len;
  int nslash, c;
  struct android_vnode *vp;

  len = strlen (name);
  if (len > EMACS_PATH_MAX)
    {
      errno = ENAMETOOLONG;
      return NULL;
    }

  /* Now, try to ``normalize'' the file name by removing consecutive
     slash characters while copying it to BUFFER.  */

  head = buffer;
  nslash = 0;
  for (end = name + len; name < end; ++name)
    {
      c = *name;

      switch (c)
	{
	case '/':
	  /* This is a directory separator character.  Two consecutive
	     separator characters should be replaced by a single
	     character; more than three in a row means that the
	     section of the file name before the last slash character
	     should be discarded.  */

	  if (!nslash)
	    *head++ = '/';

	  nslash++;

	  if (nslash >= 3)
	    /* Return to the root directory.  */
	    head = buffer, *head++ = '/', nslash = 0;
	  break;

	default:
	  /* Otherwise, copy the file name over.  */
	  nslash = 0;
	  *head++ = *name;
	  break;
	}
    }

  /* Terminate the file name.  */
  *head = '\0';

  /* If HEAD is a relative file name, it can't reside inside the
     virtual mounts; create a Unix vnode instead.  */

  if (head == buffer || buffer[0] != '/')
    return android_unix_vnode (buffer);

  /* Start looking from the root vnode.  */
  vp = &root_vnode.vnode;

  /* If buffer is empty, this will create a duplicate of the root
     vnode.  */
  return (*vp->ops->name) (vp, buffer + 1, head - buffer - 1);
}



/* Initialize the virtual filesystem layer.  Load the directory tree
   from the given asset MANAGER (which should be a local reference
   within ENV) that will be used to access assets in the future, and
   create the root vnode.

   ENV should be a JNI environment valid for future calls to VFS
   functions.  */

void
android_vfs_init (JNIEnv *env, jobject manager)
{
  jclass old;

  android_init_assets (env, manager);

  /* Create the root vnode, which is used to locate all other
     vnodes.  */
  root_vnode.vnode.ops = &root_vfs_ops;
  root_vnode.vnode.type = ANDROID_VNODE_UNIX;
  root_vnode.vnode.flags = 0;
  root_vnode.name_length = 1;
  root_vnode.name = (char *) "/";

  /* Initialize some required classes.  */
  java_string_class = (*env)->FindClass (env, "java/lang/String");
  eassert (java_string_class);

  old = java_string_class;
  java_string_class = (jclass) (*env)->NewGlobalRef (env,
						     java_string_class);
  eassert (java_string_class);
  (*env)->DeleteLocalRef (env, old);

  if (android_get_current_api_level () < 19)
    return;

  /* Initialize each of the exception classes used by
     `android_saf_exception_check'.  */

  old = (*env)->FindClass (env, "java/io/FileNotFoundException");
  file_not_found_exception = (*env)->NewGlobalRef (env, old);
  (*env)->DeleteLocalRef (env, old);
  eassert (file_not_found_exception);

  old = (*env)->FindClass (env, "java/lang/SecurityException");
  security_exception = (*env)->NewGlobalRef (env, old);
  (*env)->DeleteLocalRef (env, old);
  eassert (security_exception);

  old = (*env)->FindClass (env, "android/os/OperationCanceledException");
  operation_canceled_exception = (*env)->NewGlobalRef (env, old);
  (*env)->DeleteLocalRef (env, old);
  eassert (operation_canceled_exception);

  old = (*env)->FindClass (env, "java/lang/UnsupportedOperationException");
  unsupported_operation_exception = (*env)->NewGlobalRef (env, old);
  (*env)->DeleteLocalRef (env, old);
  eassert (unsupported_operation_exception);

  old = (*env)->FindClass (env, "java/lang/OutOfMemoryError");
  out_of_memory_error = (*env)->NewGlobalRef (env, old);
  (*env)->DeleteLocalRef (env, old);
  eassert (out_of_memory_error);

  /* And initialize those used on Android 5.0 and later.  */

  if (android_get_current_api_level () < 21)
    return;

  android_init_cursor_class (env);
  android_init_entry_class (env);
  android_init_fd_class (env);

  /* Initialize the semaphore used to wait for SAF operations to
     complete.  */

  if (sem_init (&saf_completion_sem, 0, 0) < 0)
    emacs_abort ();
}

/* The replacement functions that follow have several major
   drawbacks:

   The first is that CWD relative file names will always be Unix
   vnodes, and looking up their parents will always return another
   Unix vnode.  For example, with the working directory set to
   /sdcard:

     ../content/storage

   will find /sdcard/../content/storage on the Unix filesystem,
   opposed to /content/storage within the ``content'' VFS.

   Emacs only uses file names expanded through `expand-file-name', so
   this is unproblematic in practice.

   The second is that `..' components do not usually check that their
   preceding component is a directory.  This is a side effect of their
   removal from file names as part of a pre-processing step before
   they are opened.  So, even if:

     /sdcard/foo.txt

   is a file, opening the directory:

     /sdcard/foo.txt/..

   will be successful.

   The third is that the handling of `..' components relative to
   another vnode hasn't been tested and is only assumed to work
   because the code has been written.  It does not pose a practical
   problem, however, as Emacs only names files starting from the root
   vnode.

   The fourth is that errno values from vnode operations don't always
   reflect what the Unix system calls they emulate can return: for
   example, `open' may return EIO, while trying to `mkdir' within
   /content will return ENOENT instead of EROFS.  This is a
   consequence of how accessing a non-existent file may fail at vnode
   lookup, instead of when a vop is used.  This problem hasn't made a
   sufficient nuisance of itself to justify its fix yet.

   The fifth is that trailing directory separators may be lost when
   naming files relative to another vnode, as a consequence of an
   optimization used to avoid allocating too much stack or heap
   space.

   The sixth is that flags and other argument checking is nowhere near
   exhaustive on vnode types other than Unix vnodes.

   The seventh is that certain vnode types may read async input and
   return EINTR not upon the arrival of a signal itself, but instead
   if subsequently read input causes Vquit_flag to be set.  These
   vnodes may not be reentrant, but operating on them from within an
   async input handler will at worst cause an error to be returned.

   The eight is that some vnode types do not support O_APPEND.

   And the final drawback is that directories cannot be directly
   opened.  Instead, `dirfd' must be called on a directory stream used
   by `openat'.

   Caveat emptor! */

/* Open the VFS node designated by NAME, taking into account FLAGS and
   MODE, both of which mean the same as they do in a call to `open'.

   Value is -1 upon failure with errno set accordingly, and a file
   descriptor otherwise.  */

int
android_open (const char *name, int flags, mode_t mode)
{
  struct android_vnode *vp;
  int fd, rc;

  vp = android_name_file (name);
  if (!vp)
    return -1;

  rc = (*vp->ops->open) (vp, flags, mode, false, &fd, NULL);
  (*vp->ops->close) (vp);

  if (rc < 0)
    return -1;

  /* If rc is 1, then an asset file descriptor has been returned.
     This is impossible, so assert that it doesn't transpire.  */
  assert (rc != 1);
  return fd;
}

/* Unlink the VFS node designated by the specified FILE.
   Value is -1 upon failure with errno set, and 0 otherwise.  */

int
android_unlink (const char *name)
{
  struct android_vnode *vp;
  int rc;

  vp = android_name_file (name);
  if (!vp)
    return -1;

  rc = (*vp->ops->unlink) (vp);
  (*vp->ops->close) (vp);
  return rc;
}

/* Symlink the VFS node designated by LINKPATH to TARGET.
   Value is -1 upon failure with errno set, and 0 otherwise.  */

int
android_symlink (const char *target, const char *linkpath)
{
  struct android_vnode *vp;
  int rc;

  vp = android_name_file (linkpath);
  if (!vp)
    return -1;

  rc = (*vp->ops->symlink) (target, vp);
  (*vp->ops->close) (vp);
  return rc;
}

/* Remove the empty directory at the VFS node designated by NAME.
   Value is -1 upon failure with errno set, and 0 otherwise.  */

int
android_rmdir (const char *name)
{
  struct android_vnode *vp;
  int rc;

  vp = android_name_file (name);
  if (!vp)
    return -1;

  rc = (*vp->ops->rmdir) (vp);
  (*vp->ops->close) (vp);
  return rc;
}

/* Create a directory at the VFS node designated by NAME and the given
   access MODE.  Value is -1 upon failure with errno set, 0
   otherwise.  */

int
android_mkdir (const char *name, mode_t mode)
{
  struct android_vnode *vp;
  int rc;

  vp = android_name_file (name);
  if (!vp)
    return -1;

  rc = (*vp->ops->mkdir) (vp, mode);
  (*vp->ops->close) (vp);
  return rc;
}

/* Rename the vnode designated by SRC to the vnode designated by DST.
   If DST already exists, return -1 and set errno to EEXIST.

   SRCFD and DSTFD should be AT_FDCWD, or else value is -1 and errno
   is ENOSYS.

   If the filesystem or vnodes containing either DST or SRC does not
   support rename operations that also check for a preexisting
   destination, return -1 and set errno to ENOSYS.

   Otherwise, value and errno are identical to that of Unix
   `rename' with the same arguments.  */

int
android_renameat_noreplace (int srcfd, const char *src,
			    int dstfd, const char *dst)
{
  struct android_vnode *vp, *vdst;
  int rc;

  if (srcfd != AT_FDCWD || dstfd != AT_FDCWD)
    {
      errno = ENOSYS;
      return -1;
    }

  /* Find vnodes for both src and dst.  */

  vp = android_name_file (src);
  if (!vp)
    goto error;

  vdst = android_name_file (dst);
  if (!vdst)
    goto error1;

  /* Now try to rename vp to vdst.  */
  rc = (*vp->ops->rename) (vp, vdst, true);
  (*vp->ops->close) (vp);
  (*vdst->ops->close) (vdst);
  return rc;

 error1:
  (*vp->ops->close) (vp);
 error:
  return -1;
}

/* Like `android_renameat_noreplace', but don't check for DST's
   existence and don't accept placeholder SRCFD and DSTFD
   arguments.  */

int
android_rename (const char *src, const char *dst)
{
  struct android_vnode *vp, *vdst;
  int rc;

  /* Find vnodes for both src and dst.  */

  vp = android_name_file (src);
  if (!vp)
    goto error;

  vdst = android_name_file (dst);
  if (!vdst)
    goto error1;

  /* Now try to rename vp to vdst.  */
  rc = (*vp->ops->rename) (vp, vdst, false);
  (*vp->ops->close) (vp);
  (*vdst->ops->close) (vdst);
  return rc;

 error1:
  (*vp->ops->close) (vp);
 error:
  return -1;
}



/* fstat, fstatat, faccessat, close/fclose etc.  These functions are
   somewhat tricky to wrap: they (at least partially) operate on file
   descriptors, which sometimes provide a base directory for the
   filesystem operations they perform.  VFS nodes aren't mapped to
   file descriptors opened through them, which makes this troublesome.

   openat is not wrapped at all; uses are defined out when Emacs is
   being built for Android.  The other functions fall back to directly
   making Unix system calls when their base directory arguments are
   not AT_FDCWD and no directory stream returned from
   `android_opendir' ever returned that file descriptor, which is
   enough to satisfy Emacs's current requirements for those functions
   when a directory file descriptor is supplied.

   fclose and close are finally wrapped because they need to erase
   information used to link file descriptors with file statistics from
   their origins; fstat is also wrapped to take this information into
   account, so that it can return correct file statistics for asset
   directory files.  */

/* Like fstat.  However, look up the asset corresponding to the file
   descriptor.  If it exists, return the right information.  */

int
android_fstat (int fd, struct stat *statb)
{
  struct android_afs_open_fd *tem;
  struct android_parcel_fd *parcel_fd;
  int rc;

  for (tem = afs_file_descriptors; tem; tem = tem->next)
    {
      if (tem->fd == fd)
	{
	  memcpy (statb, &tem->statb, sizeof *statb);
	  return 0;
	}
    }

  rc = fstat (fd, statb);

  /* Now look for a matching parcel file descriptor and use its
     mtime if available.  */

  parcel_fd = open_parcel_fds;
  for (; parcel_fd; parcel_fd = parcel_fd->next)
    {
      if (parcel_fd->fd == fd)
	/* Set STATB->st_dev to a negative device number, signifying
	   that it's contained within a content provider.  */
	statb->st_dev = -4;

      if (parcel_fd->fd == fd
	  && timespec_valid_p (parcel_fd->mtime))
	{
#ifdef STAT_TIMESPEC
	  STAT_TIMESPEC (statb, st_mtim) = parcel_fd->mtime;
#else /* !STAT_TIMESPEC */
	  statb->st_mtime = parcel_fd->mtime.tv_sec;
	  statb->st_mtime_nsec = parcel_fd->mtime.tv_nsec;
#endif /* STAT_TIMESPEC */
	  break;
	}
    }

  return rc;
}

/* If DIRFD is a file descriptor returned by `android_readdir' for a
   non-Unix file stream, return FILENAME relative to the file name of
   the directory represented by that stream within BUFFER, a buffer
   SIZE bytes long.

   Value is 0 if a file name is returned, 1 otherwise.  */

static int
android_fstatat_1 (int dirfd, const char *filename,
		   char *restrict buffer, size_t size)
{
  char *dir_name;
  struct android_saf_root_vdir *vdir;
  struct android_saf_tree_vdir *vdir1;

  /* Now establish whether DIRFD is a file descriptor corresponding to
     an open asset directory stream.  */

  dir_name = android_afs_get_directory_name (dirfd);

  if (dir_name)
    {
      /* Look for PATHNAME relative to this directory within an asset
	 vnode.  */
      snprintf (buffer, size, "/assets%s%s", dir_name,
		filename);
      return 0;
    }

  /* Do the same, but for /content directories instead.  */

  dir_name = android_content_get_directory_name (dirfd);

  if (dir_name)
    {
      /* Look for PATHNAME relative to this directory within an asset
	 vnode.  */
      snprintf (buffer, size, "%s/%s", dir_name,
	        filename);
      return 0;
    }

  /* And for /content/storage.  */

  vdir = android_saf_root_get_directory (dirfd);

  if (vdir)
    {
      if (vdir->authority)
	snprintf (buffer, size, "/content/storage/%s/%s",
		  vdir->authority, filename);
      else
	snprintf (buffer, size, "/content/storage/%s",
		  filename);

      return 0;
    }

  /* /content/storage/foo/... */

  vdir1 = android_saf_tree_get_directory (dirfd);

  if (vdir1)
    {
      snprintf (buffer, size, "%s%s", vdir1->name, filename);
      return 0;
    }

  /* /foo... */

  if (root_fd >= 0 && dirfd == root_fd)
    {
      snprintf (buffer, size, "/%s", filename);
      return 0;
    }

  return 1;
}

/* If DIRFD is AT_FDCWD or a file descriptor returned by
   `android_dirfd', or PATHNAME is an absolute file name, return the
   file status of the VFS node designated by PATHNAME relative to the
   VFS node corresponding to DIRFD, or relative to the current working
   directory if DIRFD is AT_FDCWD.

   Otherwise, call `fstatat' with DIRFD, PATHNAME, STATBUF and
   FLAGS.  */

int
android_fstatat (int dirfd, const char *restrict pathname,
		 struct stat *restrict statbuf, int flags)
{
  char buffer[EMACS_PATH_MAX + 1];
  struct android_vnode *vp;
  int rc;

  /* Emacs uses AT_SYMLINK_NOFOLLOW, but fortunately (?) DIRFD is
     never known to Emacs or AT_FDCWD when it originates from a VFS
     node representing a filesystem that supports symlinks.  */

  if (dirfd == AT_FDCWD || pathname[0] == '/')
    goto vfs;

  /* Now establish whether DIRFD is a file descriptor corresponding to
     an open VFS directory stream.  */

  if (!android_fstatat_1 (dirfd, pathname, buffer, EMACS_PATH_MAX + 1))
    {
      pathname = buffer;
      goto vfs;
    }

  /* Fall back to fstatat.  */
  return fstatat (dirfd, pathname, statbuf, flags);

 vfs:
  vp = android_name_file (pathname);
  if (!vp)
    return -1;

  rc = (*vp->ops->stat) (vp, statbuf, flags);
  (*vp->ops->close) (vp);
  return rc;
}

/* Like `android_fstatat', but check file accessibility instead of
   status.  */

int
android_faccessat (int dirfd, const char *restrict pathname,
		   int mode, int flags)
{
  char buffer[EMACS_PATH_MAX + 1];
  struct android_vnode *vp;
  int rc;

  /* Emacs uses AT_SYMLINK_NOFOLLOW, but fortunately (?) DIRFD is
     never known to Emacs or AT_FDCWD when it originates from a VFS
     node representing a filesystem that supports symlinks.  */

  if (dirfd == AT_FDCWD || pathname[0] == '/')
    goto vfs;

  /* Now establish whether DIRFD is a file descriptor corresponding to
     an open VFS directory stream.  */

  if (!android_fstatat_1 (dirfd, pathname, buffer, EMACS_PATH_MAX + 1))
    {
      pathname = buffer;
      goto vfs;
    }

  /* Fall back to faccessat.  */
  return faccessat (dirfd, pathname, mode, flags);

 vfs:
  vp = android_name_file (pathname);
  if (!vp)
    return -1;

  rc = (*vp->ops->access) (vp, mode);
  (*vp->ops->close) (vp);
  return rc;
}

/* Like `android_fstatat', but set file modes instead of
   checking file status and respect FLAGS.  */

int
android_fchmodat (int dirfd, const char *pathname, mode_t mode,
		  int flags)
{
  char buffer[EMACS_PATH_MAX + 1];
  struct android_vnode *vp;
  int rc;

  if (dirfd == AT_FDCWD || pathname[0] == '/')
    goto vfs;

  /* Now establish whether DIRFD is a file descriptor corresponding to
     an open VFS directory stream.  */

  if (!android_fstatat_1 (dirfd, pathname, buffer, EMACS_PATH_MAX + 1))
    {
      pathname = buffer;
      goto vfs;
    }

  /* Fall back to fchmodat.  */
  return fchmodat (dirfd, pathname, mode, flags);

 vfs:
  vp = android_name_file (pathname);
  if (!vp)
    return -1;

  rc = (*vp->ops->chmod) (vp, mode, flags);
  (*vp->ops->close) (vp);
  return rc;
}

/* Like `android_fstatat', but return the target of any symbolic link
   at PATHNAME instead of checking file status.  */

ssize_t
android_readlinkat (int dirfd, const char *restrict pathname,
		    char *restrict buf, size_t bufsiz)
{
  char buffer[EMACS_PATH_MAX + 1];
  struct android_vnode *vp;
  ssize_t rc;

  if (dirfd == AT_FDCWD || pathname[0] == '/')
    goto vfs;

  /* Now establish whether DIRFD is a file descriptor corresponding to
     an open VFS directory stream.  */

  if (!android_fstatat_1 (dirfd, pathname, buffer, EMACS_PATH_MAX + 1))
    {
      pathname = buffer;
      goto vfs;
    }

  /* Fall back to readlinkat.  */
  return readlinkat (dirfd, pathname, buf, bufsiz);

 vfs:
  vp = android_name_file (pathname);
  if (!vp)
    return -1;

  rc = (*vp->ops->readlink) (vp, buf, bufsiz);
  (*vp->ops->close) (vp);
  return rc;
}

/* Like `fdopen', but if FD is a parcel file descriptor, ``detach'' it
   from the original.

   This is necessary because ownership over parcel file descriptors is
   retained by the ParcelFileDescriptor objects that return them,
   while file streams also require ownership over file descriptors
   they are created on behalf of.

   Detaching the parcel file descriptor linked to FD consequently
   prevents the owner from being notified when it is eventually
   closed, but for now that hasn't been demonstrated to be problematic
   yet, as Emacs doesn't write to file streams.  */

FILE *
android_fdopen (int fd, const char *mode)
{
  struct android_parcel_fd *tem, **next, *temp;
  int new_fd;

  for (next = &open_parcel_fds; (tem = *next);)
    {
      if (tem->fd == fd)
	{
	  new_fd
	    = (*android_java_env)->CallIntMethod (android_java_env,
						  tem->descriptor,
						  fd_class.detach_fd);
	  temp = tem->next;
	  xfree (tem);
	  *next = temp;
	  android_exception_check ();

	  /* Assert that FD (returned from `getFd') is identical to
	     the file descriptor returned by `detachFd'.  */

	  if (fd != new_fd)
	    emacs_abort ();

	  break;
	}
      else
	next = &(*next)->next;
    }

  return fdopen (fd, mode);
}

/* Like close.  However, remove the file descriptor from the asset
   table as well.  */

int
android_close (int fd)
{
  struct android_afs_open_fd *tem, **next, *temp;

  if (android_close_parcel_fd (fd))
    return 0;

  for (next = &afs_file_descriptors; (tem = *next);)
    {
      if (tem->fd == fd)
	{
	  temp = tem->next;
	  xfree (tem);
	  *next = temp;

	  break;
	}
      else
	next = &(*next)->next;
    }

  return close (fd);
}

/* Like fclose.  However, remove any information associated with
   FILE's file descriptor from the asset table as well.  */

int
android_fclose (FILE *stream)
{
  int fd;
  struct android_afs_open_fd *tem, **next, *temp;

  fd = fileno (stream);

  if (fd == -1)
    goto skip;

  for (next = &afs_file_descriptors; (tem = *next);)
    {
      if (tem->fd == fd)
	{
	  temp = tem->next;
	  xfree (*next);
	  *next = temp;

	  break;
	}
      else
	next = &(*next)->next;
    }

 skip:
  return fclose (stream);
}



/* External asset management interface.  By using functions here
   to read and write from files, Emacs can avoid opening a
   shared memory file descriptor for each ``asset'' file.  */

/* Like android_open.  However, return a structure that can
   either directly hold an AAsset or a file descriptor.

   Value is the structure upon success.  Upon failure, value
   consists of an uninitialized file descriptor, but its asset
   field is set to -1, and errno is set accordingly.  */

struct android_fd_or_asset
android_open_asset (const char *filename, int oflag, mode_t mode)
{
  struct android_fd_or_asset fd;
  AAsset *asset;
  int rc;
  struct android_vnode *vp;

  /* Now name this file.  */
  vp = android_name_file (filename);
  if (!vp)
    goto failure;

  rc = (*vp->ops->open) (vp, oflag, mode, true, &fd.fd,
			 &asset);
  (*vp->ops->close) (vp);

  /* Upon failure, return fd with its asset field set to (void *)
     -1.  */

  if (rc < 0)
    {
    failure:
      fd.asset = (void *) -1;
      fd.fd = -1;
      return fd;
    }

  if (rc == 1)
    {
      /* An asset file was returned.  Return the structure containing
	 an asset.  */
      fd.asset = asset;
      fd.fd = -1;
      return fd;
    }

  /* Otherwise, a file descriptor has been returned.  Set fd.asset to
     NULL, signifying that it is a file descriptor.  */
  fd.asset = NULL;
  return fd;
}

/* Like android_close.  However, it takes a ``file descriptor''
   opened using android_open_asset.  */

int
android_close_asset (struct android_fd_or_asset asset)
{
  if (!asset.asset)
    return android_close (asset.fd);

  AAsset_close (asset.asset);
  return 0;
}

/* Like `emacs_read_quit'.  However, it handles file descriptors
   opened using `android_open_asset' as well.  */

ssize_t
android_asset_read_quit (struct android_fd_or_asset asset,
			 void *buffer, size_t size)
{
  if (!asset.asset)
    return emacs_read_quit (asset.fd, buffer, size);

  /* It doesn't seem possible to quit from inside AAsset_read,
     sadly.  */
  return AAsset_read (asset.asset, buffer, size);
}

/* Like `read'.  However, it handles file descriptors opened
   using `android_open_asset' as well.  */

ssize_t
android_asset_read (struct android_fd_or_asset asset,
		    void *buffer, size_t size)
{
  if (!asset.asset)
    return read (asset.fd, buffer, size);

  /* It doesn't seem possible to quit from inside AAsset_read,
     sadly.  */
  return AAsset_read (asset.asset, buffer, size);
}

/* Like `lseek', but it handles ``file descriptors'' opened with
   android_open_asset.  */

off_t
android_asset_lseek (struct android_fd_or_asset asset, off_t off,
		     int whence)
{
  if (!asset.asset)
    return lseek (asset.fd, off, whence);

  return AAsset_seek (asset.asset, off, whence);
}

/* Like `fstat'.  */

int
android_asset_fstat (struct android_fd_or_asset asset,
		     struct stat *statb)
{
  if (!asset.asset)
    return android_fstat (asset.fd, statb);

  /* Clear statb.  */
  memset (statb, 0, sizeof *statb);

  /* Set the mode.  */
  statb->st_mode = S_IFREG | S_IRUSR | S_IRGRP | S_IROTH;

  /* Concoct a nonexistent device and an inode number.  */
  statb->st_dev = -1;
  statb->st_ino = 0;

  /* Owned by root.  */
  statb->st_uid = 0;
  statb->st_gid = 0;

  /* If the installation date can be ascertained, return that as the
     file's modification time.  */

  if (timespec_valid_p (emacs_installation_time))
    {
#ifdef STAT_TIMESPEC
      STAT_TIMESPEC (statb, st_mtim) = emacs_installation_time;
#else /* !STAT_TIMESPEC */
      /* Headers supplied by the NDK r10b contain a `struct stat'
	 without POSIX fields for nano-second timestamps.  */
      statb->st_mtime = emacs_installation_time.tv_sec;
      statb->st_mtime_nsec = emacs_installation_time.tv_nsec;
#endif /* STAT_TIMESPEC */
    }

  /* Size of the file.  */
  statb->st_size = AAsset_getLength (asset.asset);
  return 0;
}



/* Directory listing emulation.  */

/* Open a directory stream from the VFS node designated by NAME.
   Value is NULL upon failure with errno set accordingly.  `errno' may
   be set to EINTR.

   The directory stream returned holds local references to JNI objects
   and shouldn't be used after the current local reference frame is
   popped.  */

struct android_vdir *
android_opendir (const char *name)
{
  struct android_vnode *vp;
  struct android_vdir *dir;

  vp = android_name_file (name);
  if (!vp)
    return NULL;

  dir = (*vp->ops->opendir) (vp);
  (*vp->ops->close) (vp);
  return dir;
}

/* Like dirfd.  However, value is not a real directory file descriptor
   if DIR is an asset directory.  */

int
android_dirfd (struct android_vdir *dirp)
{
  return (*dirp->dirfd) (dirp);
}

/* Like readdir, but for VFS directory streams instead.  */

struct dirent *
android_readdir (struct android_vdir *dirp)
{
  return (*dirp->readdir) (dirp);
}

/* Like closedir, but for VFS directory streams instead.  */

void
android_closedir (struct android_vdir *dirp)
{
  return (*dirp->closedir) (dirp);
}



DEFUN ("android-relinquish-directory-access",
       Fandroid_relinquish_directory_access,
       Sandroid_relinquish_directory_access, 1, 1,
       "DDirectory: ",
       doc: /* Relinquish access to the provided directory.
DIRECTORY must be the toplevel directory of an open SAF volume (i.e., a
file under /content/storage), or one of its inferiors.  Once the command
completes, the SAF directory holding this directory will vanish, but no
files will be removed.  */)
  (Lisp_Object file)
{
  struct android_vnode *vp;
  struct android_saf_tree_vnode *saf_tree;
  jstring string;
  jmethodID method;

  if (android_get_current_api_level () < 21)
    error ("Emacs can only access or relinquish application storage on"
	   " Android 5.0 and later");

  if (!android_init_gui)
    return Qnil;

  file = ENCODE_FILE (Fexpand_file_name (file, Qnil));

  if (!NILP (calln (Qfile_remote_p, file)))
    signal_error ("Cannot relinquish access to remote file", file);

  vp = android_name_file (SSDATA (file));

  if (!vp)
    report_file_error ("Relinquishing directory", file);

  if (vp->type != ANDROID_VNODE_SAF_TREE)
    {
      (*vp->ops->close) (vp);
      signal_error ("Access to this directory cannot be relinquished",
		    file);
    }

  saf_tree = (struct android_saf_tree_vnode *) vp;
  string   = android_build_jstring (saf_tree->tree_uri);
  method   = service_class.relinquish_uri_rights;
  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 emacs_service,
						 service_class.class,
						 method, string);
  (*vp->ops->close) (vp);
  android_exception_check_1 (string);
  ANDROID_DELETE_LOCAL_REF (string);
  return Qnil;
}



void
syms_of_androidvfs (void)
{
  DEFSYM (Qandroid_jni, "android-jni");

  defsubr (&Sandroid_relinquish_directory_access);
}
