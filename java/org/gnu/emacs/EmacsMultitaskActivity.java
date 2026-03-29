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

import android.content.Intent;

import android.os.Bundle;

/* In large measure, this class only exists because EmacsActivity is
   already defined as an activity, and the system requires that every
   new activity be defined by a new class.  */

public final class EmacsMultitaskActivity extends EmacsActivity
{
  /* Token provided by the creator.  */
  private long activityToken;

  @Override
  public final void
  onCreate (Bundle savedInstanceState)
  {
    Intent intent;
    String token;

    intent = getIntent ();
    token  = EmacsWindowManager.ACTIVITY_TOKEN;

    if (intent != null)
      activityToken = intent.getLongExtra (token, -2);

    super.onCreate (savedInstanceState);
  }

  @Override
  public final long
  getAttachmentToken ()
  {
    return activityToken;
  }
};
