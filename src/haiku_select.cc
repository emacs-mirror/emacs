/* Haiku window system selection support. Hey Emacs, this is -*- C++ -*-
   Copyright (C) 2021-2022 Free Software Foundation, Inc.

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
#include <Message.h>
#include <Path.h>
#include <Entry.h>

#include <cstdlib>
#include <cstring>

#include "haikuselect.h"

static BClipboard *primary = NULL;
static BClipboard *secondary = NULL;
static BClipboard *system_clipboard = NULL;
static int64 count_clipboard = -1;
static int64 count_primary = -1;
static int64 count_secondary = -1;

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

  void *data = malloc (bt);

  if (!data)
    {
      cb->Unlock ();
      return NULL;
    }

  memcpy (data, ptr, bt);
  cb->Unlock ();
  return (char *) data;
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

  count_clipboard = system_clipboard->SystemCount ();
  BClipboard_set_data (system_clipboard, type, data, len, clear);
}

void
BClipboard_set_primary_selection_data (const char *type, const char *data,
				       ssize_t len, bool clear)
{
  if (!primary)
    return;

  count_primary = primary->SystemCount ();
  BClipboard_set_data (primary, type, data, len, clear);
}

void
BClipboard_set_secondary_selection_data (const char *type, const char *data,
					 ssize_t len, bool clear)
{
  if (!secondary)
    return;

  count_secondary = secondary->SystemCount ();
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

bool
BClipboard_owns_clipboard (void)
{
  return (count_clipboard >= 0
	  && (count_clipboard + 1
	      == system_clipboard->SystemCount ()));
}

bool
BClipboard_owns_primary (void)
{
  return (count_primary >= 0
	  && (count_primary + 1
	      == primary->SystemCount ()));
}

bool
BClipboard_owns_secondary (void)
{
  return (count_secondary >= 0
	  && (count_secondary + 1
	      == secondary->SystemCount ()));
}

void
init_haiku_select (void)
{
  system_clipboard = new BClipboard ("system");
  primary = new BClipboard ("primary");
  secondary = new BClipboard ("secondary");
}

int
be_enum_message (void *message, int32 *tc, int32 index,
		 int32 *count, const char **name_return)
{
  BMessage *msg = (BMessage *) message;
  type_code type;
  char *name;
  status_t rc;

  rc = msg->GetInfo (B_ANY_TYPE, index, &name, &type, count);

  if (rc != B_OK)
    return 1;

  *tc = type;
  *name_return = name;
  return 0;
}

int
be_get_refs_data (void *message, const char *name,
		  int32 index, char **path_buffer)
{
  status_t rc;
  BEntry entry;
  BPath path;
  entry_ref ref;
  BMessage *msg;

  msg = (BMessage *) message;
  rc = msg->FindRef (name, index, &ref);

  if (rc != B_OK)
    return 1;

  rc = entry.SetTo (&ref, 0);

  if (rc != B_OK)
    return 1;

  rc = entry.GetPath (&path);

  if (rc != B_OK)
    return 1;

  *path_buffer = strdup (path.Path ());
  return 0;
}

int
be_get_point_data (void *message, const char *name,
		   int32 index, float *x, float *y)
{
  status_t rc;
  BMessage *msg;
  BPoint point;

  msg = (BMessage *) message;
  rc = msg->FindPoint (name, index, &point);

  if (rc != B_OK)
    return 1;

  *x = point.x;
  *y = point.y;

  return 0;
}

int
be_get_message_data (void *message, const char *name,
		     int32 type_code, int32 index,
		     const void **buf_return,
		     ssize_t *size_return)
{
  BMessage *msg = (BMessage *) message;

  return msg->FindData (name, type_code,
			index, buf_return, size_return) != B_OK;
}

uint32
be_get_message_type (void *message)
{
  BMessage *msg = (BMessage *) message;

  return msg->what;
}

void
be_set_message_type (void *message, uint32 what)
{
  BMessage *msg = (BMessage *) message;

  msg->what = what;
}

void *
be_get_message_message (void *message, const char *name,
			int32 index)
{
  BMessage *msg = (BMessage *) message;
  BMessage *out = new (std::nothrow) BMessage;

  if (!out)
    return NULL;

  if (msg->FindMessage (name, index, out) != B_OK)
    {
      delete out;
      return NULL;
    }

  return out;
}

void *
be_create_simple_message (void)
{
  return new BMessage (B_SIMPLE_DATA);
}

int
be_add_message_data (void *message, const char *name,
		     int32 type_code, const void *buf,
		     ssize_t buf_size)
{
  BMessage *msg = (BMessage *) message;

  return msg->AddData (name, type_code, buf, buf_size) != B_OK;
}

int
be_add_refs_data (void *message, const char *name,
		  const char *filename)
{
  BEntry entry (filename);
  entry_ref ref;
  BMessage *msg = (BMessage *) message;

  if (entry.InitCheck () != B_OK)
    return 1;

  if (entry.GetRef (&ref) != B_OK)
    return 1;

  return msg->AddRef (name, &ref) != B_OK;
}

int
be_add_point_data (void *message, const char *name,
		   float x, float y)
{
  BMessage *msg = (BMessage *) message;

  return msg->AddPoint (name, BPoint (x, y)) != B_OK;
}

int
be_add_message_message (void *message, const char *name,
			void *data)
{
  BMessage *msg = (BMessage *) message;
  BMessage *data_message = (BMessage *) data;

  if (msg->AddMessage (name, data_message) != B_OK)
    return 1;

  return 0;
}

int
be_lock_clipboard_message (enum haiku_clipboard clipboard,
			   void **message_return, bool clear)
{
  BClipboard *board;

  if (clipboard == CLIPBOARD_PRIMARY)
    board = primary;
  else if (clipboard == CLIPBOARD_SECONDARY)
    board = secondary;
  else
    board = system_clipboard;

  if (!board->Lock ())
    return 1;

  if (clear)
    board->Clear ();

  *message_return = board->Data ();
  return 0;
}

void
be_unlock_clipboard (enum haiku_clipboard clipboard, bool discard)
{
  BClipboard *board;

  if (clipboard == CLIPBOARD_PRIMARY)
    board = primary;
  else if (clipboard == CLIPBOARD_SECONDARY)
    board = secondary;
  else
    board = system_clipboard;

  if (discard)
    board->Revert ();
  else
    board->Commit ();

  board->Unlock ();
}
