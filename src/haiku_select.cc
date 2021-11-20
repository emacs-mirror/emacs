/* Haiku window system selection support. Hey Emacs, this is -*- C++ -*-
   Copyright (C) 2021 Free Software Foundation, Inc.

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

#include <Clipboard.h>

#include <cstdlib>
#include <cstring>

#include "haikuselect.h"


static BClipboard *primary = NULL;
static BClipboard *secondary = NULL;
static BClipboard *system_clipboard = NULL;

int selection_state_flag;

static char *
BClipboard_find_data (BClipboard *cb, const char *type, ssize_t *len)
{
  if (!cb->Lock ())
    return 0;

  BMessage *dat = cb->Data ();
  if (!dat)
    {
      cb->Unlock ();
      return 0;
    }

  const char *ptr;
  ssize_t bt;
  dat->FindData (type, B_MIME_TYPE, (const void **) &ptr, &bt);

  if (!ptr)
    {
      cb->Unlock ();
      return NULL;
    }

  if (len)
    *len = bt;

  cb->Unlock ();

  return strndup (ptr, bt);
}

static void
BClipboard_set_data (BClipboard *cb, const char *type, const char *dat,
		     ssize_t len)
{
  if (!cb->Lock ())
    return;
  cb->Clear ();
  BMessage *mdat = cb->Data ();
  if (!mdat)
    {
      cb->Unlock ();
      return;
    }

  if (dat)
    mdat->AddData (type, B_MIME_TYPE, dat, len);
  cb->Commit ();
  cb->Unlock ();
}

char *
BClipboard_find_system_data (const char *type, ssize_t *len)
{
  if (!system_clipboard)
    return 0;

  return BClipboard_find_data (system_clipboard, type, len);
}

char *
BClipboard_find_primary_selection_data (const char *type, ssize_t *len)
{
  if (!primary)
    return 0;

  return BClipboard_find_data (primary, type, len);
}

char *
BClipboard_find_secondary_selection_data (const char *type, ssize_t *len)
{
  if (!secondary)
    return 0;

  return BClipboard_find_data (secondary, type, len);
}

void
BClipboard_set_system_data (const char *type, const char *data,
			    ssize_t len)
{
  if (!system_clipboard)
    return;

  BClipboard_set_data (system_clipboard, type, data, len);
}

void
BClipboard_set_primary_selection_data (const char *type, const char *data,
				       ssize_t len)
{
  if (!primary)
    return;

  BClipboard_set_data (primary, type, data, len);
}

void
BClipboard_set_secondary_selection_data (const char *type, const char *data,
					 ssize_t len)
{
  if (!secondary)
    return;

  BClipboard_set_data (secondary, type, data, len);
}

void
BClipboard_free_data (void *ptr)
{
  std::free (ptr);
}

void
init_haiku_select (void)
{
  system_clipboard = new BClipboard ("system");
  primary = new BClipboard ("primary");
  secondary = new BClipboard ("secondary");
}
