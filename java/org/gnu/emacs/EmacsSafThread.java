/* Communication module for Android terminals.  -*- c-file-style: "GNU" -*-

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

package org.gnu.emacs;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;

import java.io.FileNotFoundException;
import java.io.IOException;

import android.content.ContentResolver;
import android.database.Cursor;
import android.net.Uri;

import android.os.Build;
import android.os.CancellationSignal;
import android.os.Handler;
import android.os.HandlerThread;
import android.os.OperationCanceledException;
import android.os.ParcelFileDescriptor;
import android.os.SystemClock;

import android.util.Log;

import android.provider.DocumentsContract;
import android.provider.DocumentsContract.Document;



/* Emacs runs long-running SAF operations on a second thread running
   its own handler.  These operations include opening files and
   maintaining the path to document ID cache.

   Because Emacs paths are based on file display names, while Android
   document identifiers have no discernible hierarchy of their own,
   each file name lookup must carry out a repeated search for
   directory documents with the names of all of the file name's
   constituent components, where each iteration searches within the
   directory document identified by the previous iteration.

   A time limited cache tying components to document IDs is maintained
   in order to speed up consecutive searches for file names sharing
   the same components.  Since listening for changes to each document
   in the cache is prohibitively expensive, Emacs instead elects to
   periodically remove entries that are older than a predetermined
   amount of a time.

   The cache is split into two levels: the first caches the
   relationships between display names and document IDs, while the
   second caches individual document IDs and their contents (children,
   type, etc.)

   Long-running operations are also run on this thread for another
   reason: Android uses special cancellation objects to terminate
   ongoing IPC operations.  However, the functions that perform these
   operations block instead of providing mechanisms for the caller to
   wait for their completion while also reading async input, as a
   consequence of which the calling thread is unable to signal the
   cancellation objects that it provides.  Performing the blocking
   operations in this auxiliary thread enables the main thread to wait
   for completion itself, signaling the cancellation objects when it
   deems necessary.  */



public final class EmacsSafThread extends HandlerThread
{
  private static final String TAG = "EmacsSafThread";

  /* The content resolver used by this thread.  */
  private final ContentResolver resolver;

  /* Map between tree URIs and the cache entry representing its
     toplevel directory.  */
  private final HashMap<Uri, CacheToplevel> cacheToplevels;

  /* Handler for this thread's main loop.  */
  private Handler handler;

  /* File access mode constants.  See `man 7 inode'.  */
  public static final int S_IRUSR = 0000400;
  public static final int S_IWUSR = 0000200;
  public static final int S_IXUSR = 0000100;
  public static final int S_IFCHR = 0020000;
  public static final int S_IFDIR = 0040000;
  public static final int S_IFREG = 0100000;

  /* Number of seconds in between each attempt to prune the storage
     cache.  */
  public static final int CACHE_PRUNE_TIME = 10;

  /* Number of seconds after which an entry in the cache is to be
     considered invalid.  */
  public static final int CACHE_INVALID_TIME = 10;

  public
  EmacsSafThread (ContentResolver resolver)
  {
    super ("Document provider access thread");
    this.resolver = resolver;
    this.cacheToplevels = new HashMap<Uri, CacheToplevel> ();
  }



  @Override
  public void
  start ()
  {
    super.start ();

    /* Set up the handler after the thread starts.  */
    handler = new Handler (getLooper ());

    /* And start periodically pruning the cache.  */
    postPruneMessage ();
  }


  private static final class CacheToplevel
  {
    /* Map between document names and children.  */
    HashMap<String, DocIdEntry> children;

    /* Map between document names and file status.  */
    HashMap<String, StatCacheEntry> statCache;

    /* Map between document IDs and cache items.  */
    HashMap<String, CacheEntry> idCache;
  };

  private static final class StatCacheEntry
  {
    /* The time at which this cache entry was created.  */
    long time;

    /* Flags, size, and modification time of this file.  */
    long flags, size, mtime;

    /* Whether or not this file is a directory.  */
    boolean isDirectory;

    public
    StatCacheEntry ()
    {
      time = SystemClock.uptimeMillis ();
    }

    public boolean
    isValid ()
    {
      return ((SystemClock.uptimeMillis () - time)
	      < CACHE_INVALID_TIME * 1000);
    }
  };

  private static final class DocIdEntry
  {
    /* The document ID.  */
    String documentId;

    /* The time this entry was created.  */
    long time;

    public
    DocIdEntry ()
    {
      time = SystemClock.uptimeMillis ();
    }

    /* Return a cache entry comprised of the state of the file
       identified by `documentId'.  TREE is the URI of the tree
       containing this entry, and TOPLEVEL is the toplevel
       representing it.  SIGNAL is a cancellation signal.

       RESOLVER is the content provider used to retrieve file
       information.

       Value is NULL if the file cannot be found.  */

    public CacheEntry
    getCacheEntry (ContentResolver resolver, Uri tree,
		   CacheToplevel toplevel,
		   CancellationSignal signal)
    {
      Uri uri;
      String[] projection;
      String type;
      Cursor cursor;
      int column;
      CacheEntry entry;

      /* Create a document URI representing DOCUMENTID within URI's
	 authority.  */

      uri = DocumentsContract.buildDocumentUriUsingTree (tree,
							 documentId);
      projection = new String[] {
	Document.COLUMN_MIME_TYPE,
      };

      cursor = null;

      try
	{
	  cursor = resolver.query (uri, projection, null,
				   null, null, signal);

	  if (!cursor.moveToFirst ())
	    return null;

	  column = cursor.getColumnIndex (Document.COLUMN_MIME_TYPE);

	  if (column < 0)
	    return null;

	  type = cursor.getString (column);

	  if (type == null)
	    return null;

	  entry = new CacheEntry ();
	  entry.type = type;
	  toplevel.idCache.put (documentId, entry);
	  return entry;
	}
      catch (OperationCanceledException e)
	{
	  throw e;
	}
      catch (Throwable e)
	{
	  return null;
	}
      finally
	{
	  if (cursor != null)
	    cursor.close ();
	}
    }

    public boolean
    isValid ()
    {
      return ((SystemClock.uptimeMillis () - time)
	      < CACHE_INVALID_TIME * 1000);
    }
  };

  private static final class CacheEntry
  {
    /* The type of this document.  */
    String type;

    /* Map between document names and children.  */
    HashMap<String, DocIdEntry> children;

    /* The time this entry was created.  */
    long time;

    public
    CacheEntry ()
    {
      children = new HashMap<String, DocIdEntry> ();
      time = SystemClock.uptimeMillis ();
    }

    public boolean
    isValid ()
    {
      return ((SystemClock.uptimeMillis () - time)
	      < CACHE_INVALID_TIME * 1000);
    }
  };

  /* Create or return a toplevel for the given tree URI.  */

  private CacheToplevel
  getCache (Uri uri)
  {
    CacheToplevel toplevel;

    toplevel = cacheToplevels.get (uri);

    if (toplevel != null)
      return toplevel;

    toplevel = new CacheToplevel ();
    toplevel.children = new HashMap<String, DocIdEntry> ();
    toplevel.statCache = new HashMap<String, StatCacheEntry> ();
    toplevel.idCache = new HashMap<String, CacheEntry> ();
    cacheToplevels.put (uri, toplevel);
    return toplevel;
  }

  /* Remove each cache entry within COLLECTION older than
     CACHE_INVALID_TIME.  */

  private void
  pruneCache1 (Collection<DocIdEntry> collection)
  {
    Iterator<DocIdEntry> iter;
    DocIdEntry tem;

    iter = collection.iterator ();
    while (iter.hasNext ())
      {
	/* Get the cache entry.  */
	tem = iter.next ();

	/* If it's not valid anymore, remove it.  Iterating over a
	   collection whose contents are being removed is undefined
	   unless the removal is performed using the iterator's own
	   `remove' function, so tem.remove cannot be used here.  */

	if (tem.isValid ())
	  continue;

	iter.remove ();
      }
  }

  /* Remove every entry older than CACHE_INVALID_TIME from each
     toplevel inside `cachedToplevels'.  */

  private void
  pruneCache ()
  {
    Iterator<CacheEntry> iter;
    Iterator<StatCacheEntry> statIter;
    CacheEntry tem;
    StatCacheEntry stat;

    for (CacheToplevel toplevel : cacheToplevels.values ())
      {
	/* First, clean up expired cache entries.  */
	iter = toplevel.idCache.values ().iterator ();

	while (iter.hasNext ())
	  {
	    /* Get the cache entry.  */
	    tem = iter.next ();

	    /* If it's not valid anymore, remove it.  Iterating over a
	       collection whose contents are being removed is
	       undefined unless the removal is performed using the
	       iterator's own `remove' function, so tem.remove cannot
	       be used here.  */

	    if (tem.isValid ())
	      {
		/* Otherwise, clean up expired items in its document
		   ID cache.  */
		pruneCache1 (tem.children.values ());
		continue;
	      }

	    iter.remove ();
	  }

	statIter = toplevel.statCache.values ().iterator ();

	while (statIter.hasNext ())
	  {
	    /* Get the cache entry.  */
	    stat = statIter.next ();

	    /* If it's not valid anymore, remove it.  Iterating over a
	       collection whose contents are being removed is
	       undefined unless the removal is performed using the
	       iterator's own `remove' function, so tem.remove cannot
	       be used here.  */

	    if (stat.isValid ())
	      continue;

	    statIter.remove ();
	  }
      }

    postPruneMessage ();
  }

  /* Cache file information within TOPLEVEL, under the list of
     children CHILDREN.

     NAME, ID, and TYPE should respectively be the display name of the
     document within its parent document (the CacheEntry whose
     `children' field is CHILDREN), its document ID, and its MIME
     type.

     If ID_ENTRY_EXISTS, don't create a new document ID entry within
     CHILDREN indexed by NAME.

     Value is the cache entry saved for the document ID.  */

  private CacheEntry
  cacheChild (CacheToplevel toplevel,
	      HashMap<String, DocIdEntry> children,
	      String name, String id, String type,
	      boolean id_entry_exists)
  {
    DocIdEntry idEntry;
    CacheEntry cacheEntry;

    if (!id_entry_exists)
      {
	idEntry = new DocIdEntry ();
	idEntry.documentId = id;
	children.put (name, idEntry);
      }

    cacheEntry = new CacheEntry ();
    cacheEntry.type = type;
    toplevel.idCache.put (id, cacheEntry);
    return cacheEntry;
  }

  /* Cache file status for DOCUMENTID within TOPLEVEL.  Value is the
     new cache entry.  CURSOR is the cursor from where to retrieve the
     file status, in the form of the columns COLUMN_FLAGS,
     COLUMN_SIZE, COLUMN_MIME_TYPE and COLUMN_LAST_MODIFIED.

     If NO_CACHE, don't cache the file status; just return the
     entry.  */

  private StatCacheEntry
  cacheFileStatus (String documentId, CacheToplevel toplevel,
		   Cursor cursor, boolean no_cache)
  {
    StatCacheEntry entry;
    int flagsIndex, columnIndex, typeIndex;
    int sizeIndex, mtimeIndex;
    String type;

    /* Obtain the indices for columns wanted from this cursor.  */
    flagsIndex = cursor.getColumnIndex (Document.COLUMN_FLAGS);
    sizeIndex = cursor.getColumnIndex (Document.COLUMN_SIZE);
    typeIndex = cursor.getColumnIndex (Document.COLUMN_MIME_TYPE);
    mtimeIndex = cursor.getColumnIndex (Document.COLUMN_LAST_MODIFIED);

    /* COLUMN_LAST_MODIFIED is allowed to be absent in a
       conforming documents provider.  */
    if (flagsIndex < 0 || sizeIndex < 0 || typeIndex < 0)
      return null;

    /* Get the file status from CURSOR.  */
    entry = new StatCacheEntry ();
    entry.flags = cursor.getInt (flagsIndex);
    type = cursor.getString (typeIndex);

    if (type == null)
      return null;

    entry.isDirectory = type.equals (Document.MIME_TYPE_DIR);

    if (cursor.isNull (sizeIndex))
      /* The size is unknown.  */
      entry.size = -1;
    else
      entry.size = cursor.getLong (sizeIndex);

    /* mtimeIndex is potentially unset, since document providers
       aren't obligated to provide modification times.  */

    if (mtimeIndex >= 0 && !cursor.isNull (mtimeIndex))
      entry.mtime = cursor.getLong (mtimeIndex);

    /* Finally, add this entry to the cache and return.  */
    if (!no_cache)
      toplevel.statCache.put (documentId, entry);
    return entry;
  }

  /* Cache the type and as many of the children of the directory
     designated by DOCUMENTID as possible into TOPLEVEL.

     CURSOR should be a cursor representing an open directory stream,
     with its projection consisting of at least the display name,
     document ID and MIME type columns.

     Rewind the position of CURSOR to before its first element after
     completion.  */

  private void
  cacheDirectoryFromCursor (CacheToplevel toplevel, String documentId,
			    Cursor cursor)
  {
    CacheEntry entry, constituent;
    int nameColumn, idColumn, typeColumn;
    String id, name, type;
    DocIdEntry idEntry;

    /* Find the numbers of the columns wanted.  */

    nameColumn
      = cursor.getColumnIndex (Document.COLUMN_DISPLAY_NAME);
    idColumn
      = cursor.getColumnIndex (Document.COLUMN_DOCUMENT_ID);
    typeColumn
      = cursor.getColumnIndex (Document.COLUMN_MIME_TYPE);

    if (nameColumn < 0 || idColumn < 0 || typeColumn < 0)
      return;

    entry = new CacheEntry ();

    /* We know this is a directory already.  */
    entry.type = Document.MIME_TYPE_DIR;
    toplevel.idCache.put (documentId, entry);

    /* Now, try to cache each of its constituents.  */

    while (cursor.moveToNext ())
      {
	try
	  {
	    name = cursor.getString (nameColumn);
	    id = cursor.getString (idColumn);
	    type = cursor.getString (typeColumn);

	    if (name == null || id == null || type == null)
	      continue;

	    /* First, add the name and ID to ENTRY's map of
	       children.  */
	    idEntry = new DocIdEntry ();
	    idEntry.documentId = id;
	    entry.children.put (id, idEntry);

	    /* Cache the file status for ID within TOPELVEL too; if a
	       directory listing is being requested, it's very likely
	       that a series of calls for file status will follow.  */

	    cacheFileStatus (id, toplevel, cursor, false);

	    /* If this constituent is a directory, don't cache any
	       information about it.  It cannot be cached without
	       knowing its children.  */

	    if (type.equals (Document.MIME_TYPE_DIR))
	      continue;

	    /* Otherwise, create a new cache entry comprised of its
	       type.  */
	    constituent = new CacheEntry ();
	    constituent.type = type;
	    toplevel.idCache.put (documentId, entry);
	  }
	catch (Exception e)
	  {
	    e.printStackTrace ();
	    continue;
	  }
      }

    /* Rewind cursor back to the beginning.  */
    cursor.moveToPosition (-1);
  }

  /* Post a message to run `pruneCache' every CACHE_PRUNE_TIME
     seconds.  */

  private void
  postPruneMessage ()
  {
    handler.postDelayed (new Runnable () {
	@Override
	public void
	run ()
	{
	  pruneCache ();
	}
      }, CACHE_PRUNE_TIME * 1000);
  }

  /* Invalidate the cache entry denoted by DOCUMENT_ID, within the
     document tree URI.
     Call this after deleting a document or directory.

     At the same time, remove the final component within the file name
     CACHENAME from the cache if it exists.  */

  public void
  postInvalidateCache (final Uri uri, final String documentId,
		       final String cacheName)
  {
    handler.post (new Runnable () {
	@Override
	public void
	run ()
	{
	  CacheToplevel toplevel;
	  HashMap<String, DocIdEntry> children;
	  String[] components;
	  CacheEntry entry;
	  DocIdEntry idEntry;

	  toplevel = getCache (uri);
	  toplevel.idCache.remove (documentId);
	  toplevel.statCache.remove (documentId);

	  /* If the parent of CACHENAME is cached, remove it.  */

	  children = toplevel.children;
	  components = cacheName.split ("/");

	  for (String component : components)
	    {
	      /* Java `split' removes trailing empty matches but not
		 leading or intermediary ones.  */
	      if (component.isEmpty ())
		continue;

	      if (component == components[components.length - 1])
		{
		  /* This is the last component, so remove it from
		     children.  */
		  children.remove (component);
		  return;
		}
	      else
		{
		  /* Search for this component within the last level
		     of the cache.  */

		  idEntry = children.get (component);

		  if (idEntry == null)
		    /* Not cached, so return.  */
		    return;

		  entry = toplevel.idCache.get (idEntry.documentId);

		  if (entry == null)
		    /* Not cached, so return.  */
		    return;

		  /* Locate the next component within this
		     directory.  */
		  children = entry.children;
		}
	    }
	}
      });
  }

  /* Invalidate the cache entry denoted by DOCUMENT_ID, within the
     document tree URI.
     Call this after deleting a document or directory.

     At the same time, remove the child referring to DOCUMENTID from
     within CACHENAME's cache entry if it exists.  */

  public void
  postInvalidateCacheDir (final Uri uri, final String documentId,
			  final String cacheName)
  {
    handler.post (new Runnable () {
	@Override
	public void
	run ()
	{
	  CacheToplevel toplevel;
	  HashMap<String, DocIdEntry> children;
	  String[] components;
	  CacheEntry entry;
	  DocIdEntry idEntry;
	  Iterator<DocIdEntry> iter;

	  toplevel = getCache (uri);
	  toplevel.idCache.remove (documentId);
	  toplevel.statCache.remove (documentId);

	  /* Now remove DOCUMENTID from CACHENAME's cache entry, if
	     any.  */

	  children = toplevel.children;
	  components = cacheName.split ("/");

	  for (String component : components)
	    {
	      /* Java `split' removes trailing empty matches but not
		 leading or intermediary ones.  */
	      if (component.isEmpty ())
		continue;

	      /* Search for this component within the last level
		 of the cache.  */

	      idEntry = children.get (component);

	      if (idEntry == null)
		/* Not cached, so return.  */
		return;

	      entry = toplevel.idCache.get (idEntry.documentId);

	      if (entry == null)
		/* Not cached, so return.  */
		return;

	      /* Locate the next component within this
		 directory.  */
	      children = entry.children;
	    }

	  iter = children.values ().iterator ();
	  while (iter.hasNext ())
	    {
	      idEntry = iter.next ();

	      if (idEntry.documentId.equals (documentId))
		{
		  iter.remove ();
		  break;
		}
	    }
	}
      });
  }

  /* Invalidate the file status cache entry for DOCUMENTID within URI.
     Call this when the contents of a file (i.e. the constituents of a
     directory file) may have changed, but the document's display name
     has not.  */

  public void
  postInvalidateStat (final Uri uri, final String documentId)
  {
    handler.post (new Runnable () {
	@Override
	public void
	run ()
	{
	  CacheToplevel toplevel;

	  toplevel = getCache (uri);
	  toplevel.statCache.remove (documentId);
	}
      });
  }



  /* ``Prototypes'' for nested functions that are run within the SAF
     thread and accepts a cancellation signal.  They differ in their
     return types.  */

  private abstract class SafIntFunction
  {
    /* The ``throws Throwable'' here is a Java idiosyncrasy that tells
       the compiler to allow arbitrary error objects to be signaled
       from within this function.

       Later, runIntFunction will try to re-throw any error object
       generated by this function in the Emacs thread, using a trick
       to avoid the compiler requirement to expressly declare that an
       error (and which types of errors) will be signaled.  */

    public abstract int runInt (CancellationSignal signal)
      throws Throwable;
  };

  private abstract class SafObjectFunction
  {
    /* The ``throws Throwable'' here is a Java idiosyncrasy that tells
       the compiler to allow arbitrary error objects to be signaled
       from within this function.

       Later, runObjectFunction will try to re-throw any error object
       generated by this function in the Emacs thread, using a trick
       to avoid the compiler requirement to expressly declare that an
       error (and which types of errors) will be signaled.  */

    public abstract Object runObject (CancellationSignal signal)
      throws Throwable;
  };



  /* Functions that run cancel-able queries.  These functions are
     internally run within the SAF thread.  */

  /* Throw the specified EXCEPTION.  The type template T is erased by
     the compiler before the object is compiled, so the compiled code
     simply throws EXCEPTION without the cast being verified.

     T should be RuntimeException to obtain the desired effect of
     throwing an exception without a compiler check.  */

  @SuppressWarnings("unchecked")
  private static <T extends Throwable> void
  throwException (Throwable exception)
    throws T
  {
    throw (T) exception;
  }

  /* Run the given function (or rather, its `runInt' field) within the
     SAF thread, waiting for it to complete.

     If async input arrives in the meantime and sets Vquit_flag,
     signal the cancellation signal supplied to that function.

     Rethrow any exception thrown from that function, and return its
     value otherwise.  */

  private int
  runIntFunction (final SafIntFunction function)
  {
    final EmacsHolder<Object> result;
    final CancellationSignal signal;
    Throwable throwable;

    result = new EmacsHolder<Object> ();
    signal = new CancellationSignal ();

    handler.post (new Runnable () {
	@Override
	public void
	run ()
	{
	  try
	    {
	      result.thing
		= Integer.valueOf (function.runInt (signal));
	    }
	  catch (Throwable throwable)
	    {
	      result.thing = throwable;
	    }

	  EmacsNative.safPostRequest ();
	}
      });

    if (EmacsNative.safSyncAndReadInput () != 0)
      {
	signal.cancel ();

	/* Now wait for the function to finish.  Either the signal has
	   arrived after the query took place, in which case it will
	   finish normally, or an OperationCanceledException will be
	   thrown.  */

	EmacsNative.safSync ();
      }

    if (result.thing instanceof Throwable)
      {
	throwable = (Throwable) result.thing;
	EmacsSafThread.<RuntimeException>throwException (throwable);
      }

    return (Integer) result.thing;
  }

  /* Run the given function (or rather, its `runObject' field) within
     the SAF thread, waiting for it to complete.

     If async input arrives in the meantime and sets Vquit_flag,
     signal the cancellation signal supplied to that function.

     Rethrow any exception thrown from that function, and return its
     value otherwise.  */

  private Object
  runObjectFunction (final SafObjectFunction function)
  {
    final EmacsHolder<Object> result;
    final CancellationSignal signal;
    Throwable throwable;

    result = new EmacsHolder<Object> ();
    signal = new CancellationSignal ();

    handler.post (new Runnable () {
	@Override
	public void
	run ()
	{
	  try
	    {
	      result.thing = function.runObject (signal);
	    }
	  catch (Throwable throwable)
	    {
	      result.thing = throwable;
	    }

	  EmacsNative.safPostRequest ();
	}
      });

    if (EmacsNative.safSyncAndReadInput () != 0)
      {
	signal.cancel ();

	/* Now wait for the function to finish.  Either the signal has
	   arrived after the query took place, in which case it will
	   finish normally, or an OperationCanceledException will be
	   thrown.  */

	EmacsNative.safSync ();
      }

    if (result.thing instanceof Throwable)
      {
	throwable = (Throwable) result.thing;
	EmacsSafThread.<RuntimeException>throwException (throwable);
      }

    return result.thing;
  }

  /* The crux of `documentIdFromName1', run within the SAF thread.
     SIGNAL should be a cancellation signal run upon quitting.  */

  private int
  documentIdFromName1 (String tree_uri, String name,
		       String[] id_return, CancellationSignal signal)
  {
    Uri uri, treeUri;
    String id, type, newId, newType;
    String[] components, projection;
    Cursor cursor;
    int nameColumn, idColumn, typeColumn;
    CacheToplevel toplevel;
    DocIdEntry idEntry;
    HashMap<String, DocIdEntry> children, next;
    CacheEntry cache;

    projection = new String[] {
      Document.COLUMN_DISPLAY_NAME,
      Document.COLUMN_DOCUMENT_ID,
      Document.COLUMN_MIME_TYPE,
    };

    /* Parse the URI identifying the tree first.  */
    uri = Uri.parse (tree_uri);

    /* Now, split NAME into its individual components.  */
    components = name.split ("/");

    /* Set id and type to the value at the root of the tree.  */
    type = id = null;
    cursor = null;

    /* Obtain the top level of this cache.  */
    toplevel = getCache (uri);

    /* Set the current map of children to this top level.  */
    children = toplevel.children;

    /* For each component... */

    try
      {
	for (String component : components)
	  {
	    /* Java split doesn't behave very much like strtok when it
	       comes to trailing and leading delimiters...  */
	    if (component.isEmpty ())
	      continue;

	    /* Search for component within the currently cached list
	       of children.  */

	    idEntry = children.get (component);

	    if (idEntry != null)
	      {
		/* The document ID is known.  Now find the
		   corresponding document ID cache.  */

		cache = toplevel.idCache.get (idEntry.documentId);

		/* Fetch just the information for this document.  */

		if (cache == null)
		  cache = idEntry.getCacheEntry (resolver, uri, toplevel,
						 signal);

		if (cache == null)
		  {
		    /* File status matching idEntry could not be
		       obtained.  Treat this as if the file does not
		       exist.  */

		    children.remove (component);

		    if (id == null)
		      id = DocumentsContract.getTreeDocumentId (uri);

		    id_return[0] = id;

		    if ((type == null
			 || type.equals (Document.MIME_TYPE_DIR))
			/* ... and type and id currently represent the
			   penultimate component.  */
			&& component == components[components.length  - 1])
		      return -2;

		    return -1;
		  }

		/* Otherwise, use the cached information.  */
		id = idEntry.documentId;
		type = cache.type;
		children = cache.children;
		continue;
	      }

	    /* Create the tree URI for URI from ID if it exists, or
	       the root otherwise.  */

	    if (id == null)
	      id = DocumentsContract.getTreeDocumentId (uri);

	    treeUri
	      = DocumentsContract.buildChildDocumentsUriUsingTree (uri, id);

	    /* Look for a file in this directory by the name of
	       component.  */

	    cursor = resolver.query (treeUri, projection,
				     (Document.COLUMN_DISPLAY_NAME
				      + " = ?"),
				     new String[] { component, },
				     null, signal);

	    if (cursor == null)
	      return -1;

	    /* Find the column numbers for each of the columns that
	       are wanted.  */

	    nameColumn
	      = cursor.getColumnIndex (Document.COLUMN_DISPLAY_NAME);
	    idColumn
	      = cursor.getColumnIndex (Document.COLUMN_DOCUMENT_ID);
	    typeColumn
	      = cursor.getColumnIndex (Document.COLUMN_MIME_TYPE);

	    if (nameColumn < 0 || idColumn < 0 || typeColumn < 0)
	      return -1;

	    next = null;

	    while (true)
	      {
		/* Even though the query selects for a specific
		   display name, some content providers nevertheless
		   return every file within the directory.  */

		if (!cursor.moveToNext ())
		  {
		    /* If a component has been found, break out of the
		       loop.  */

		    if (next != null)
		      break;

		    /* If the last component considered is a
		       directory... */
		    if ((type == null
			 || type.equals (Document.MIME_TYPE_DIR))
			/* ... and type and id currently represent the
			   penultimate component.  */
			&& component == components[components.length  - 1])
		      {
			/* The cursor is empty.  In this case, return
			   -2 and the current document ID (belonging
			   to the previous component) in
			   ID_RETURN.  */

			id_return[0] = id;

			/* But return -1 on the off chance that id is
			   null.  */

			if (id == null)
			  return -1;

			return -2;
		      }

		    /* The last component found is not a directory, so
		       return -1.  */
		    return -1;
		  }

		/* So move CURSOR to a row with the right display
		   name.  */

		name = cursor.getString (nameColumn);
		newId = cursor.getString (idColumn);
		newType = cursor.getString (typeColumn);

		/* Any of the three variables above may be NULL if the
		   column data is of the wrong type depending on how
		   the Cursor returned is implemented.  */

		if (name == null || newId == null || newType == null)
		  return -1;

		/* Cache this name, even if it isn't the document
		   that's being searched for.  */

		cache = cacheChild (toplevel, children, name,
				    newId, newType,
				    idEntry != null);

		/* Record the desired component once it is located,
		   but continue reading and caching items from the
		   cursor.  */

		if (name.equals (component))
		  {
		    id = newId;
		    next = cache.children;
		    type = newType;
		  }
	      }

	    children = next;

	    /* Now close the cursor.  */
	    cursor.close ();
	    cursor = null;

	    /* ID may have become NULL if the data is in an invalid
	       format.  */
	    if (id == null)
	      return -1;
	  }
      }
    finally
      {
	/* If an error is thrown within the block above, let
	   android_saf_exception_check handle it, but make sure the
	   cursor is closed.  */

	if (cursor != null)
	  cursor.close ();
      }

    /* Here, id is either NULL (meaning the same as TREE_URI), and
       type is either NULL (in which case id should also be NULL) or
       the MIME type of the file.  */

    /* First return the ID.  */

    if (id == null)
      id_return[0] = DocumentsContract.getTreeDocumentId (uri);
    else
      id_return[0] = id;

    /* Next, return whether or not this is a directory.  */
    if (type == null || type.equals (Document.MIME_TYPE_DIR))
      return 1;

    return 0;
  }

  /* Find the document ID of the file within TREE_URI designated by
     NAME.

     NAME is a ``file name'' comprised of the display names of
     individual files.  Each constituent component prior to the last
     must name a directory file within TREE_URI.

     Upon success, return 0 or 1 (contingent upon whether or not the
     last component within NAME is a directory) and place the document
     ID of the named file in ID_RETURN[0].

     If the designated file can't be located, but each component of
     NAME up to the last component can and is a directory, return -2
     and the ID of the last component located in ID_RETURN[0].

     If the designated file can't be located, return -1, or signal one
     of OperationCanceledException, SecurityException,
     FileNotFoundException, or UnsupportedOperationException.  */

  public int
  documentIdFromName (final String tree_uri, final String name,
		      final String[] id_return)
  {
    return runIntFunction (new SafIntFunction () {
	@Override
	public int
	runInt (CancellationSignal signal)
	{
	  return documentIdFromName1 (tree_uri, name, id_return,
				      signal);
	}
      });
  }

  /* The bulk of `statDocument'.  SIGNAL should be a cancellation
     signal.  */

  private long[]
  statDocument1 (String uri, String documentId,
		 CancellationSignal signal, boolean noCache)
  {
    Uri uriObject, tree;
    String[] projection;
    long[] stat;
    Cursor cursor;
    CacheToplevel toplevel;
    StatCacheEntry cache;

    tree = Uri.parse (uri);

    if (documentId == null)
      documentId = DocumentsContract.getTreeDocumentId (tree);

    /* Create a document URI representing DOCUMENTID within URI's
       authority.  */

    uriObject
      = DocumentsContract.buildDocumentUriUsingTree (tree, documentId);

    /* See if the file status cache currently contains this
       document.  */

    toplevel = getCache (tree);
    cache = toplevel.statCache.get (documentId);

    if (cache == null || !cache.isValid ())
      {
	/* Stat this document and enter its information into the
	   cache.  */

	projection = new String[] {
	  Document.COLUMN_FLAGS,
	  Document.COLUMN_LAST_MODIFIED,
	  Document.COLUMN_MIME_TYPE,
	  Document.COLUMN_SIZE,
	};

	cursor = resolver.query (uriObject, projection, null,
				 null, null, signal);

	if (cursor == null)
	  return null;

	try
	  {
	    if (!cursor.moveToFirst ())
	      return null;

	    cache = cacheFileStatus (documentId, toplevel, cursor,
				     noCache);
	  }
	finally
	  {
	    cursor.close ();
	  }

	/* If cache is still null, return null.  */

	if (cache == null)
	  return null;
      }

    /* Create the array of file status and populate it with the
       information within cache.  */
    stat = new long[3];

    stat[0] |= S_IRUSR;
    if ((cache.flags & Document.FLAG_SUPPORTS_WRITE) != 0)
      stat[0] |= S_IWUSR;

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N
	&& (cache.flags & Document.FLAG_VIRTUAL_DOCUMENT) != 0)
      stat[0] |= S_IFCHR;

    stat[1] = cache.size;

    /* Check if this is a directory file.  */
    if (cache.isDirectory
	/* Files shouldn't be specials and directories at the same
	   time, but Android doesn't forbid document providers
	   from returning this information.  */
	&& (stat[0] & S_IFCHR) == 0)
      {
	/* Since FLAG_SUPPORTS_WRITE doesn't apply to directories,
	   just assume they're writable.  */
	stat[0] |= S_IFDIR | S_IWUSR | S_IXUSR;

	/* Directory files cannot be modified if
	   FLAG_DIR_SUPPORTS_CREATE is not set.  */

	if ((cache.flags & Document.FLAG_DIR_SUPPORTS_CREATE) == 0)
	  stat[0] &= ~S_IWUSR;
      }

    /* If this file is neither a character special nor a
       directory, indicate that it's a regular file.  */

    if ((stat[0] & (S_IFDIR | S_IFCHR)) == 0)
      stat[0] |= S_IFREG;

    stat[2] = cache.mtime;
    return stat;
  }

  /* Return file status for the document designated by the given
     DOCUMENTID and tree URI.  If DOCUMENTID is NULL, use the document
     ID in URI itself.

     Value is null upon failure, or an array of longs [MODE, SIZE,
     MTIM] upon success, where MODE contains the file type and access
     modes of the file as in `struct stat', SIZE is the size of the
     file in BYTES or -1 if not known, and MTIM is the time of the
     last modification to this file in milliseconds since 00:00,
     January 1st, 1970.

     If NOCACHE, refrain from placing the file status within the
     status cache.

     OperationCanceledException and other typical exceptions may be
     signaled upon receiving async input or other errors.  */

  public long[]
  statDocument (final String uri, final String documentId,
		final boolean noCache)
  {
    return (long[]) runObjectFunction (new SafObjectFunction () {
	@Override
	public Object
	runObject (CancellationSignal signal)
	{
	  return statDocument1 (uri, documentId, signal, noCache);
	}
      });
  }

  /* The bulk of `accessDocument'.  SIGNAL should be a cancellation
     signal.  */

  private int
  accessDocument1 (String uri, String documentId, boolean writable,
		   CancellationSignal signal)
  {
    Uri uriObject;
    String[] projection;
    int tem, index;
    String tem1;
    Cursor cursor;
    CacheToplevel toplevel;
    CacheEntry entry;

    uriObject = Uri.parse (uri);

    if (documentId == null)
      documentId = DocumentsContract.getTreeDocumentId (uriObject);

    /* If WRITABLE is false and the document ID is cached, use its
       cached value instead.  This speeds up
       `directory-files-with-attributes' a little.  */

    if (!writable)
      {
	toplevel = getCache (uriObject);
	entry = toplevel.idCache.get (documentId);

	if (entry != null)
	  return 0;
      }

    /* Create a document URI representing DOCUMENTID within URI's
       authority.  */

    uriObject
      = DocumentsContract.buildDocumentUriUsingTree (uriObject, documentId);

    /* Now stat this document.  */

    projection = new String[] {
      Document.COLUMN_FLAGS,
      Document.COLUMN_MIME_TYPE,
    };

    cursor = resolver.query (uriObject, projection, null,
			     null, null, signal);

    if (cursor == null)
      return -1;

    try
      {
	if (!cursor.moveToFirst ())
	  return -1;

	if (!writable)
	  return 0;

	index = cursor.getColumnIndex (Document.COLUMN_MIME_TYPE);
	if (index < 0)
	  return -3;

	/* Get the type of this file to check if it's a directory.  */
	tem1 = cursor.getString (index);

	/* Check if this is a directory file.  */
	if (tem1.equals (Document.MIME_TYPE_DIR))
	  {
	    /* If so, don't check for FLAG_SUPPORTS_WRITE.
	       Check for FLAG_DIR_SUPPORTS_CREATE instead.  */

	    index = cursor.getColumnIndex (Document.COLUMN_FLAGS);
	    if (index < 0)
	      return -3;

	    tem = cursor.getInt (index);
	    if ((tem & Document.FLAG_DIR_SUPPORTS_CREATE) == 0)
	      return -3;

	    return 0;
	  }

	index = cursor.getColumnIndex (Document.COLUMN_FLAGS);
	if (index < 0)
	  return -3;

	tem = cursor.getInt (index);
	if (writable && (tem & Document.FLAG_SUPPORTS_WRITE) == 0)
	  return -3;
      }
    finally
      {
	/* Close the cursor if an exception occurs.  */
	cursor.close ();
      }

    return 0;
  }

  /* Find out whether Emacs has access to the document designated by
     the specified DOCUMENTID within the tree URI.  If DOCUMENTID is
     NULL, use the document ID in URI itself.

     If WRITABLE, also check that the file is writable, which is true
     if it is either a directory or its flags contains
     FLAG_SUPPORTS_WRITE.

     Value is 0 if the file is accessible, and one of the following if
     not:

     -1, if the file does not exist.
     -2, if WRITABLE and the file is not writable.
     -3, upon any other error.

     In addition, arbitrary runtime exceptions (such as
     SecurityException or UnsupportedOperationException) may be
     thrown.  */

  public int
  accessDocument (final String uri, final String documentId,
		  final boolean writable)
  {
    return runIntFunction (new SafIntFunction () {
	@Override
	public int
	runInt (CancellationSignal signal)
	{
	  return accessDocument1 (uri, documentId, writable,
				  signal);
	}
      });
  }

  /* The crux of openDocumentDirectory.  SIGNAL must be a cancellation
     signal.  */

  private Cursor
  openDocumentDirectory1 (String uri, String documentId,
			  CancellationSignal signal)
  {
    Uri uriObject, tree;
    Cursor cursor;
    String projection[];
    CacheToplevel toplevel;

    tree = uriObject = Uri.parse (uri);

    /* If documentId is not set, use the document ID of the tree URI
       itself.  */

    if (documentId == null)
      documentId = DocumentsContract.getTreeDocumentId (uriObject);

    /* Build a URI representing each directory entry within
       DOCUMENTID.  */

    uriObject
      = DocumentsContract.buildChildDocumentsUriUsingTree (uriObject,
							   documentId);

    projection = new String [] {
      Document.COLUMN_DISPLAY_NAME,
      Document.COLUMN_DOCUMENT_ID,
      Document.COLUMN_MIME_TYPE,
      Document.COLUMN_FLAGS,
      Document.COLUMN_LAST_MODIFIED,
      Document.COLUMN_SIZE,
    };

    cursor = resolver.query (uriObject, projection, null, null,
			     null, signal);

    /* Create a new cache entry tied to this document ID.  */

    if (cursor != null)
      {
	toplevel = getCache (tree);
	cacheDirectoryFromCursor (toplevel, documentId,
				  cursor);
      }

    /* Return the cursor.  */
    return cursor;
  }

  /* Open a cursor representing each entry within the directory
     designated by the specified DOCUMENTID within the tree URI.

     If DOCUMENTID is NULL, use the document ID within URI itself.
     Value is NULL upon failure.

     In addition, arbitrary runtime exceptions (such as
     SecurityException or UnsupportedOperationException) may be
     thrown.  */

  public Cursor
  openDocumentDirectory (final String uri, final String documentId)
  {
    return (Cursor) runObjectFunction (new SafObjectFunction () {
	@Override
	public Object
	runObject (CancellationSignal signal)
	{
	  return openDocumentDirectory1 (uri, documentId, signal);
	}
      });
  }

  /* The crux of `openDocument'.  SIGNAL must be a cancellation
     signal.  */

  public ParcelFileDescriptor
  openDocument1 (String uri, String documentId, boolean read,
		 boolean write, boolean truncate,
		 CancellationSignal signal)
    throws Throwable
  {
    Uri treeUri, documentUri;
    String mode;
    ParcelFileDescriptor fileDescriptor;
    CacheToplevel toplevel;

    treeUri = Uri.parse (uri);

    /* documentId must be set for this request, since it doesn't make
       sense to ``open'' the root of the directory tree.  */

    documentUri
      = DocumentsContract.buildDocumentUriUsingTree (treeUri, documentId);

    /* Select the mode used to open the file.  */

    if (write)
      {
	if (read)
	  {
	    if (truncate)
	      mode = "rwt";
	    else
	      mode = "rw";
	  }
	else
	  /* Set mode to w when WRITE && !READ, disregarding TRUNCATE.
	     In contradiction with the ContentResolver documentation,
	     document providers seem to truncate files whenever w is
	     specified, at least superficially.  (But see below.)  */
	  mode = "w";
      }
    else
      mode = "r";

    fileDescriptor
      = resolver.openFileDescriptor (documentUri, mode,
				     signal);

    /* If a writable on-disk file descriptor is requested and TRUNCATE
       is set, then probe the file descriptor to detect if it is
       actually readable.  If not, close this file descriptor and
       reopen it with MODE set to rw; some document providers granting
       access to Samba shares don't implement rwt, but these document
       providers invariably truncate the file opened even when the
       mode is merely w.

       This may be ascribed to a mix-up in Android's documentation
       regarding DocumentsProvider: the `openDocument' function is only
       documented to accept r or rw, whereas the default implementation
       of the `openFile' function (which documents rwt) delegates to
       `openDocument'.  */

    if (read && write && truncate && fileDescriptor != null
	&& !EmacsNative.ftruncate (fileDescriptor.getFd ()))
      {
	try
	  {
	    fileDescriptor.closeWithError ("File descriptor requested"
					   + " is not writable");
	  }
	catch (IOException e)
	  {
	    Log.w (TAG, "Leaking unclosed file descriptor " + e);
	  }

	fileDescriptor
	  = resolver.openFileDescriptor (documentUri, "rw", signal);

	/* Try to truncate fileDescriptor just to stay on the safe
	   side.  */
	if (fileDescriptor != null)
	  EmacsNative.ftruncate (fileDescriptor.getFd ());
      }
    else if (!read && write && truncate && fileDescriptor != null)
      /* Moreover, document providers that return actual seekable
	 files characteristically neglect to truncate the file
	 returned when the access mode is merely w, so attempt to
	 truncate it by hand.  */
      EmacsNative.ftruncate (fileDescriptor.getFd ());

    /* Every time a document is opened, remove it from the file status
       cache.  */
    toplevel = getCache (treeUri);
    toplevel.statCache.remove (documentId);

    return fileDescriptor;
  }

  /* Open a file descriptor for a file document designated by
     DOCUMENTID within the document tree identified by URI.  If
     TRUNCATE and the document already exists, truncate its contents
     before returning.

     If READ && WRITE, open the file under either the `rw' or `rwt'
     access mode, which implies that the value must be a seekable
     on-disk file.  If WRITE && !READ or TRUNC && WRITE, also truncate
     the file after it is opened.

     If only READ or WRITE is set, value may be a non-seekable FIFO or
     one end of a socket pair.

     Value is NULL upon failure or a parcel file descriptor upon
     success.  Call `ParcelFileDescriptor.close' on this file
     descriptor instead of using the `close' system call.

     FileNotFoundException and/or SecurityException and/or
     UnsupportedOperationException and/or OperationCanceledException
     may be thrown upon failure.  */

  public ParcelFileDescriptor
  openDocument (final String uri, final String documentId,
		final boolean read, final boolean write,
		final boolean truncate)
  {
    Object tem;

    tem = runObjectFunction (new SafObjectFunction () {
	@Override
	public Object
	runObject (CancellationSignal signal)
	  throws Throwable
	{
	  return openDocument1 (uri, documentId, read,
				write, truncate, signal);
	}
      });

    return (ParcelFileDescriptor) tem;
  }
};
