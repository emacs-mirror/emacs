/* Haiku window system selection support. Hey Emacs, this is -*- C++ -*-
   Copyright (C) 2021-2024 Free Software Foundation, Inc.

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

#include <Application.h>
#include <Clipboard.h>
#include <Message.h>
#include <Path.h>
#include <Entry.h>

#include <cstdlib>
#include <cstring>

#include "haikuselect.h"

/* The clipboard object representing the primary selection.  */
static BClipboard *primary = NULL;

/* The clipboard object representing the secondary selection.  */
static BClipboard *secondary = NULL;

/* The clipboard object used by other programs, representing the
   clipboard.  */
static BClipboard *system_clipboard = NULL;

/* The number of times the system clipboard has changed.  */
static int64 count_clipboard = -1;

/* The number of times the primary selection has changed.  */
static int64 count_primary = -1;

/* The number of times the secondary selection has changed.  */
static int64 count_secondary = -1;

/* Whether or not we currently think Emacs owns the primary
   selection.  */
static bool owned_primary;

/* Likewise for the secondary selection.  */
static bool owned_secondary;

/* And the clipboard.  */
static bool owned_clipboard;

static BClipboard *
get_clipboard_object (enum haiku_clipboard clipboard)
{
  switch (clipboard)
    {
    case CLIPBOARD_PRIMARY:
      return primary;

    case CLIPBOARD_SECONDARY:
      return secondary;

    case CLIPBOARD_CLIPBOARD:
      return system_clipboard;
    }

  abort ();
}

static char *
be_find_clipboard_data_1 (BClipboard *cb, const char *type, ssize_t *len)
{
  BMessage *data;
  const char *ptr;
  ssize_t nbytes;
  void *value;

  if (!cb->Lock ())
    return NULL;

  data = cb->Data ();

  if (!data)
    {
      cb->Unlock ();
      return NULL;
    }

  data->FindData (type, B_MIME_TYPE, (const void **) &ptr,
		  &nbytes);

  if (!ptr)
    {
      cb->Unlock ();
      return NULL;
    }

  if (len)
    *len = nbytes;

  value = malloc (nbytes);

  if (!data)
    {
      cb->Unlock ();
      return NULL;
    }

  memcpy (value, ptr, nbytes);
  cb->Unlock ();

  return (char *) value;
}

static void
be_set_clipboard_data_1 (BClipboard *cb, const char *type, const char *data,
			 ssize_t len, bool clear)
{
  BMessage *message_data;

  if (!cb->Lock ())
    return;

  if (clear)
    cb->Clear ();

  message_data = cb->Data ();

  if (!message_data)
    {
      cb->Unlock ();
      return;
    }

  if (data)
    {
      if (message_data->ReplaceData (type, B_MIME_TYPE, data, len)
	  == B_NAME_NOT_FOUND)
	message_data->AddData (type, B_MIME_TYPE, data, len);
    }
  else
    message_data->RemoveName (type);

  cb->Commit ();
  cb->Unlock ();
}

void
be_update_clipboard_count (enum haiku_clipboard id)
{
  switch (id)
    {
    case CLIPBOARD_CLIPBOARD:
      count_clipboard = system_clipboard->SystemCount ();
      owned_clipboard = true;
      break;

    case CLIPBOARD_PRIMARY:
      count_primary = primary->SystemCount ();
      owned_primary = true;
      break;

    case CLIPBOARD_SECONDARY:
      count_secondary = secondary->SystemCount ();
      owned_secondary = true;
      break;
    }
}

char *
be_find_clipboard_data (enum haiku_clipboard id, const char *type,
			ssize_t *len)
{
  return be_find_clipboard_data_1 (get_clipboard_object (id),
				   type, len);
}

void
be_set_clipboard_data (enum haiku_clipboard id, const char *type,
		       const char *data, ssize_t len, bool clear)
{
  be_update_clipboard_count (id);

  be_set_clipboard_data_1 (get_clipboard_object (id), type,
			   data, len, clear);
}

static bool
clipboard_owner_p (void)
{
  return (count_clipboard >= 0
	  && (count_clipboard + 1
	      == system_clipboard->SystemCount ()));
}

static bool
primary_owner_p (void)
{
  return (count_primary >= 0
	  && (count_primary + 1
	      == primary->SystemCount ()));
}

static bool
secondary_owner_p (void)
{
  return (count_secondary >= 0
	  && (count_secondary + 1
	      == secondary->SystemCount ()));
}

bool
be_clipboard_owner_p (enum haiku_clipboard clipboard)
{
  switch (clipboard)
    {
    case CLIPBOARD_PRIMARY:
      return primary_owner_p ();

    case CLIPBOARD_SECONDARY:
      return secondary_owner_p ();

    case CLIPBOARD_CLIPBOARD:
      return clipboard_owner_p ();
    }

  abort ();
}

void
be_clipboard_init (void)
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

  board = get_clipboard_object (clipboard);

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

  board = get_clipboard_object (clipboard);

  if (discard)
    board->Revert ();
  else
    board->Commit ();

  board->Unlock ();
}

void
be_handle_clipboard_changed_message (void)
{
  int64 n_clipboard, n_primary, n_secondary;

  n_clipboard = system_clipboard->SystemCount ();
  n_primary = primary->SystemCount ();
  n_secondary = secondary->SystemCount ();

  if (count_clipboard != -1
      && (n_clipboard > count_clipboard + 1)
      && owned_clipboard)
    {
      owned_clipboard = false;
      haiku_selection_disowned (CLIPBOARD_CLIPBOARD,
				n_clipboard);
    }

  if (count_primary != -1
      && (n_primary > count_primary + 1)
      && owned_primary)
    {
      owned_primary = false;
      haiku_selection_disowned (CLIPBOARD_PRIMARY,
				n_primary);
    }

  if (count_secondary != -1
      && (n_secondary > count_secondary + 1)
      && owned_secondary)
    {
      owned_secondary = false;
      haiku_selection_disowned (CLIPBOARD_SECONDARY,
				n_secondary);
    }
}

void
be_start_watching_selection (enum haiku_clipboard id)
{
  BClipboard *clipboard;

  clipboard = get_clipboard_object (id);
  clipboard->StartWatching (be_app);
}

bool
be_selection_outdated_p (enum haiku_clipboard id, int64 count)
{
  if (id == CLIPBOARD_CLIPBOARD && count_clipboard > count)
    return true;

  if (id == CLIPBOARD_PRIMARY && count_primary > count)
    return true;

  if (id == CLIPBOARD_SECONDARY && count_secondary > count)
    return true;

  return false;
}

int64
be_get_clipboard_count (enum haiku_clipboard id)
{
  BClipboard *clipboard;

  clipboard = get_clipboard_object (id);
  return clipboard->SystemCount ();
}
