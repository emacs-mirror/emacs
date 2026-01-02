/* Android initialization for GNU Emacs.

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

#include <android/log.h>



/* Forward declarations.  */

static const char *directory_tree;

static const char *android_scan_directory_tree (const char *, size_t *);
static unsigned int android_extract_long (const char *);



/* This file contains an emulation of the Android asset manager API
   used on builds for Android 2.2.  It is included by android.c
   whenever appropriate.

   The replacements in this file are not thread safe and must only be
   called from the creating thread.  */

struct android_asset_manager
{
  /* JNI environment.  */
  JNIEnv *env;

  /* Asset manager class and functions.  */
  jclass class;
  jmethodID open_fd;
  jmethodID open;

  /* Asset file descriptor class and functions.  */
  jclass fd_class;
  jmethodID get_length;
  jmethodID create_input_stream;
  jmethodID close;

  /* Input stream class and functions.  */
  jclass input_stream_class;
  jmethodID read;
  jmethodID stream_close;

  /* Associated asset manager object.  */
  jobject asset_manager;
};

typedef struct android_asset_manager AAssetManager;

struct android_asset
{
  /* The asset manager.  */
  AAssetManager *manager;

  /* The length of the asset, or -1.  */
  jlong length;

  /* The asset file descriptor and input stream.  */
  jobject fd, stream;

  /* Alternatively, the name of the file.  */
  jstring name;

  /* The mode.  */
  int mode;
};

typedef struct android_asset AAsset;

static AAssetManager *
AAssetManager_fromJava (JNIEnv *env, jobject java_manager)
{
  AAssetManager *manager;
  jclass temp;

  manager = malloc (sizeof *manager);

  if (!manager)
    return NULL;

  manager->env = env;
  manager->asset_manager
    = (*env)->NewGlobalRef (env, java_manager);

  if (!manager->asset_manager)
    {
      free (manager);
      return NULL;
    }

  manager->class
    = (*env)->FindClass (env, "android/content/res/AssetManager");
  assert (manager->class);

  manager->open_fd
    = (*env)->GetMethodID (env, manager->class, "openFd",
			   "(Ljava/lang/String;)"
			   "Landroid/content/res/AssetFileDescriptor;");
  assert (manager->open_fd);

  manager->open
    = (*env)->GetMethodID (env, manager->class, "open",
			   "(Ljava/lang/String;)"
			   "Ljava/io/InputStream;");
  assert (manager->open);

  manager->fd_class
    = (*env)->FindClass (env, "android/content/res/AssetFileDescriptor");
  assert (manager->fd_class);

  manager->get_length
    = (*env)->GetMethodID (env, manager->fd_class, "getLength",
			   "()J");
  assert (manager->get_length);

  manager->create_input_stream
    = (*env)->GetMethodID (env, manager->fd_class,
			   "createInputStream",
			   "()Ljava/io/FileInputStream;");
  assert (manager->create_input_stream);

  manager->close
    = (*env)->GetMethodID (env, manager->fd_class,
			   "close", "()V");
  assert (manager->close);

  manager->input_stream_class
    = (*env)->FindClass (env, "java/io/InputStream");
  assert (manager->input_stream_class);

  manager->read
    = (*env)->GetMethodID (env, manager->input_stream_class,
			   "read", "([B)I");
  assert (manager->read);

  manager->stream_close
    = (*env)->GetMethodID (env, manager->input_stream_class,
			   "close", "()V");
  assert (manager->stream_close);

  /* Now convert all the class references to global ones.  */
  temp = manager->class;
  manager->class
    = (*env)->NewGlobalRef (env, temp);
  assert (manager->class);
  (*env)->DeleteLocalRef (env, temp);
  temp = manager->fd_class;
  manager->fd_class
    = (*env)->NewGlobalRef (env, temp);
  assert (manager->fd_class);
  (*env)->DeleteLocalRef (env, temp);
  temp = manager->input_stream_class;
  manager->input_stream_class
    = (*env)->NewGlobalRef (env, temp);
  assert (manager->input_stream_class);
  (*env)->DeleteLocalRef (env, temp);

  /* Return the asset manager.  */
  return manager;
}

enum
  {
    AASSET_MODE_STREAMING = 0,
    AASSET_MODE_BUFFER	  = 1,
  };

static AAsset *
AAssetManager_open (AAssetManager *manager, const char *c_name,
		    int mode)
{
  jobject desc = NULL;
  jstring name;
  AAsset *asset = NULL;
  const char *asset_dir;
  jlong st_size = -1;

  /* Push a local frame.  */
  (*(manager->env))->PushLocalFrame (manager->env, 3);

  if ((*(manager->env))->ExceptionCheck (manager->env))
    goto fail;

  /* If the directory tree has been initialized, it is possible to avoid
     opening an AssetFileDescriptor and thereby access compressed
     assets, without sacrificing the possibility of reading the file
     size.  */
  if (directory_tree)
    {
      /* Search for a matching asset.  */
      asset_dir = android_scan_directory_tree (c_name, NULL);
      if (!asset_dir)
	goto fail;

      /* Extract the size of the asset from this directory.  */
      st_size = android_extract_long (asset_dir - 8);
    }

  /* Encoding issues can be ignored for the time being as there are only
     ASCII file names in Emacs.  */
  name = (*(manager->env))->NewStringUTF (manager->env, c_name);

  if (!name)
    goto fail;

  /* If st_size has been set, it ought to be possible to open an input
     stream directly upon the first attempt to read from the asset,
     sidestepping the intermediate AssetFileDescriptor.  */

  if (st_size < 0)
    /* Otherwise attempt to open an ``AssetFileDescriptor''.  */
    desc = (*(manager->env))->CallObjectMethod (manager->env,
						manager->asset_manager,
						manager->open_fd,
						name);

  /* Allocate the asset.  */
  asset = calloc (1, sizeof *asset);

  if (!asset)
    goto fail;

  if (desc)
    {
      /* Pop the local frame and return desc.  */
      desc = (*(manager->env))->NewGlobalRef (manager->env, desc);

      if (!desc)
	goto fail;

      /* Will be released by PopLocalFrame.  */
      name = NULL;
    }
  else /* if (name) */
    {
      /* Pop the local frame and return name.  */
      name = (*(manager->env))->NewGlobalRef (manager->env, name);

      if (!name)
	goto fail;
    }

  (*(manager->env))->PopLocalFrame (manager->env, NULL);

  asset->manager = manager;
  asset->length = st_size;
  asset->fd = desc;
  asset->name = name;
  asset->mode = mode;

  return asset;

 fail:
  if (desc)
    (*(manager->env))->CallVoidMethod (manager->env,
				       desc,
				       manager->close);

  (*(manager->env))->ExceptionClear (manager->env);
  (*(manager->env))->PopLocalFrame (manager->env, NULL);
  free (asset);
  return NULL;
}

static AAsset *
AAsset_close (AAsset *asset)
{
  JNIEnv *env;

  env = asset->manager->env;

  if (asset->fd)
    {
      (*env)->CallVoidMethod (asset->manager->env,
			      asset->fd,
			      asset->manager->close);
      (*env)->DeleteGlobalRef (asset->manager->env,
			       asset->fd);
    }

  if (asset->stream)
    {
      (*env)->CallVoidMethod (asset->manager->env,
			      asset->stream,
			      asset->manager->stream_close);
      (*env)->DeleteGlobalRef (asset->manager->env,
			       asset->stream);
    }

  if (asset->name)
    (*env)->DeleteGlobalRef (asset->manager->env,
			     asset->name);

  free (asset);
}

/* Create an input stream associated with the given ASSET.  Set
   ASSET->stream to its global reference.

   Value is 1 upon failure, else 0.  ASSET must not already have an
   input stream.  */

static int
android_asset_create_stream (AAsset *asset)
{
  jobject stream;
  JNIEnv *env;

  assert (asset->fd || asset->name);

  env = asset->manager->env;

  if (asset->name)
    stream = (*env)->CallObjectMethod (env, asset->manager->asset_manager,
				       asset->manager->open, asset->name);
  else
    stream
      = (*env)->CallObjectMethod (env, asset->fd,
				  asset->manager->create_input_stream);

  if (!stream)
    {
      (*env)->ExceptionClear (env);
      return 1;
    }

  asset->stream
    = (*env)->NewGlobalRef (env, stream);

  if (!asset->stream)
    {
      (*env)->ExceptionClear (env);
      (*env)->DeleteLocalRef (env, stream);
      return 1;
    }

  (*env)->DeleteLocalRef (env, stream);
  return 0;
}

/* Read NBYTES from the specified asset into the given BUFFER;

   Internally, allocate a Java byte array containing 4096 elements and
   copy the data to and from that array.

   Value is the number of bytes actually read, 0 at EOF, or -1 upon
   failure, in which case errno is set accordingly.  If NBYTES is
   zero, behavior is undefined.  */

static int
android_asset_read_internal (AAsset *asset, int nbytes, char *buffer)
{
  jbyteArray stash;
  JNIEnv *env;
  jint bytes_read, total;

  /* Allocate a suitable amount of storage.  Either nbytes or 4096,
     whichever is larger.  */
  env = asset->manager->env;
  stash = (*env)->NewByteArray (env, MIN (nbytes, 4096));

  if (!stash)
    {
      (*env)->ExceptionClear (env);
      errno = ENOMEM;
      return -1;
    }

  /* Try to create an input stream.  */

  if (!asset->stream
      && android_asset_create_stream (asset))
    {
      (*env)->DeleteLocalRef (env, stash);
      errno = ENOMEM;
      return -1;
    }

  /* Start reading.  */

  total = 0;

  while (nbytes)
    {
      bytes_read = (*env)->CallIntMethod (env, asset->stream,
					  asset->manager->read,
					  stash);

      /* Detect error conditions.  */

      if ((*env)->ExceptionCheck (env))
	goto out_errno;

      /* Detect EOF.  */

      if (bytes_read == -1)
	goto out;

      /* Finally write out the amount that was read.  */
      bytes_read = MIN (bytes_read, nbytes);
      (*env)->GetByteArrayRegion (env, stash, 0, bytes_read, buffer);

      buffer += bytes_read;
      total += bytes_read;
      nbytes -= bytes_read;
    }

  /* Make sure the value of nbytes still makes sense.  */
  assert (nbytes >= 0);

 out:
  (*env)->ExceptionClear (env);
  (*env)->DeleteLocalRef (env, stash);
  return total;

 out_errno:
  /* Return an error indication if an exception arises while the file
     is being read.  */
  (*env)->ExceptionClear (env);
  (*env)->DeleteLocalRef (env, stash);
  errno = EIO;
  return -1;
}

static long
AAsset_getLength (AAsset *asset)
{
  JNIEnv *env;

  if (asset->length != -1)
    return asset->length;
  if (!asset->fd)
    return 0;

  env = asset->manager->env;
  asset->length
    = (*env)->CallLongMethod (env, asset->fd,
			      asset->manager->get_length);
  return asset->length;
}

static char *
AAsset_getBuffer (AAsset *asset)
{
  long length;
  char *buffer;

  length = AAsset_getLength (asset);

  if (!length)
    return NULL;

  buffer = malloc (length);

  if (!buffer)
    return NULL;

  if (android_asset_read_internal (asset, length, buffer)
      != length)
    {
      free (buffer);
      return NULL;
    }

  return buffer;
}

static size_t
AAsset_read (AAsset *asset, void *buffer, size_t size)
{
  return android_asset_read_internal (asset, MIN (size, INT_MAX),
				      buffer);
}

static off_t
AAsset_seek (AAsset *asset, off_t offset, int whence)
{
  /* Java InputStreams don't support seeking at all.  */
  errno = ESPIPE;
  return -1;
}
