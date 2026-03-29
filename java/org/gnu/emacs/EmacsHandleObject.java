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

import java.lang.IllegalStateException;

/* This defines something that is a so-called ``handle''.  Handles
   must be created by C code, and will remain existing until
   destroyHandle is called.  C code then refers to the handle by a
   number which maps into the Java object representing the handle.

   All handle operations must be done from the Emacs thread.  */

public abstract class EmacsHandleObject
{
  /* Whether or not this handle has been destroyed.  */
  volatile boolean destroyed;

  /* The handle associated with this object, set in
     android_globalize_reference.  */
  public long handle;

  public void
  destroyHandle () throws IllegalStateException
  {
    synchronized (this)
      {
	destroyed = true;
      }
  }

  public boolean
  isDestroyed ()
  {
    return destroyed;
  }
};
