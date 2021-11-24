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
BClipboard_get_targets (BClipboard *cb, char **buf, int buf_size)
{
  BMessage *data;
  char *name;
  int32 count_found;
  type_code type;
  int32 i;
  int index;

  if (!cb->Lock ())
    {
      buf[0] = NULL;
      return;
    }

  data = cb->Data ();
  index = 0;

  if (!data)
    {
      buf[0] = NULL;
      cb->Unlock ();
      return;
    }

  for (i = 0; (data->GetInfo (B_ANY_TYPE, i, &name,
			     &type, &count_found)
	       == B_OK); ++i)
    {
      if (type == B_MIME_TYPE)
	{
	  if (index < (buf_size - 1))
	    {
	      buf[index++] = strdup (name);

	      if (!buf[index - 1])
		break;
	    }
	}
    }

  buf[index] = NULL;

  cb->Unlock ();
}

static void
BClipboard_set_data (BClipboard *cb, const char *type, const char *dat,
		     ssize_t len, bool clear)
{
  if (!cb->Lock ())
    return;

  if (clear)
    cb->Clear ();

  BMessage *mdat = cb->Data ();
  if (!mdat)
    {
      cb->Unlock ();
      return;
    }

  if (dat)
    {
      if (mdat->ReplaceData (type, B_MIME_TYPE, dat, len)
	  == B_NAME_NOT_FOUND)
	mdat->AddData (type, B_MIME_TYPE, dat, len);
    }
  else
    mdat->RemoveName (type);
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
			    ssize_t len, bool clear)
{
  if (!system_clipboard)
    return;

  BClipboard_set_data (system_clipboard, type, data, len, clear);
}

void
BClipboard_set_primary_selection_data (const char *type, const char *data,
				       ssize_t len, bool clear)
{
  if (!primary)
    return;

  BClipboard_set_data (primary, type, data, len, clear);
}

void
BClipboard_set_secondary_selection_data (const char *type, const char *data,
					 ssize_t len, bool clear)
{
  if (!secondary)
    return;

  BClipboard_set_data (secondary, type, data, len, clear);
}

void
BClipboard_free_data (void *ptr)
{
  std::free (ptr);
}

void
BClipboard_system_targets (char **buf, int len)
{
  BClipboard_get_targets (system_clipboard, buf, len);
}

void
BClipboard_primary_targets (char **buf, int len)
{
  BClipboard_get_targets (primary, buf, len);
}

void
BClipboard_secondary_targets (char **buf, int len)
{
  BClipboard_get_targets (secondary, buf, len);
}

void
init_haiku_select (void)
{
  system_clipboard = new BClipboard ("system");
  primary = new BClipboard ("primary");
  secondary = new BClipboard ("secondary");
}
