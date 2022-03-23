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

#ifndef _HAIKU_SELECT_H_
#define _HAIKU_SELECT_H_

#ifdef __cplusplus
#include <cstdio>
#endif

#include <SupportDefs.h>

enum haiku_clipboard
  {
    CLIPBOARD_PRIMARY,
    CLIPBOARD_SECONDARY,
    CLIPBOARD_CLIPBOARD
  };

#ifdef __cplusplus
#include <stdio.h>
extern "C"
{
  extern void init_haiku_select (void);
#endif

  /* Whether or not the selection was recently changed.  */
  extern int selection_state_flag;

  /* Find a string with the MIME type TYPE in the system clipboard.  */
  extern char *
  BClipboard_find_system_data (const char *type, ssize_t *len);

  /* Ditto, but for the primary selection and not clipboard.  */
  extern char *
  BClipboard_find_primary_selection_data (const char *type, ssize_t *len);

  /* Ditto, this time for the secondary selection.  */
  extern char *
  BClipboard_find_secondary_selection_data (const char *type, ssize_t *len);

  extern void
  BClipboard_set_system_data (const char *type, const char *data, ssize_t len,
			      bool clear);

  extern void
  BClipboard_set_primary_selection_data (const char *type, const char *data,
					 ssize_t len, bool clear);

  extern void
  BClipboard_set_secondary_selection_data (const char *type, const char *data,
					   ssize_t len, bool clear);

  extern void
  BClipboard_system_targets (char **buf, int len);

  extern void
  BClipboard_primary_targets (char **buf, int len);

  extern void
  BClipboard_secondary_targets (char **buf, int len);

  extern bool
  BClipboard_owns_clipboard (void);

  extern bool
  BClipboard_owns_primary (void);

  extern bool BClipboard_owns_secondary (void);

  /* Free the returned data.  */
  extern void BClipboard_free_data (void *ptr);

  extern int be_enum_message (void *message, int32 *tc, int32 index,
			      int32 *count, const char **name_return);
  extern int be_get_message_data (void *message, const char *name,
				  int32 type_code, int32 index,
				  const void **buf_return,
				  ssize_t *size_return);
  extern int be_get_refs_data (void *message, const char *name,
			       int32 index, char **path_buffer);
  extern uint32 be_get_message_type (void *message);
  extern void be_set_message_type (void *message, uint32 what);
  extern void *be_get_message_message (void *message, const char *name,
				       int32 index);
  extern void *be_create_simple_message (void);
  extern int be_add_message_data (void *message, const char *name,
				  int32 type_code, const void *buf,
				  ssize_t buf_size);
  extern int be_add_refs_data (void *message, const char *name,
			       const char *filename);
  extern int be_add_message_message (void *message, const char *name,
				     void *data);
  extern int be_lock_clipboard_message (enum haiku_clipboard clipboard,
					void **message_return);
  extern void be_unlock_clipboard (enum haiku_clipboard clipboard);
#ifdef __cplusplus
};
#endif
#endif /* _HAIKU_SELECT_H_ */
