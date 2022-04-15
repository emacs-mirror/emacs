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
extern char *BClipboard_find_system_data (const char *, ssize_t *);
extern char *BClipboard_find_primary_selection_data (const char *, ssize_t *);
extern char *BClipboard_find_secondary_selection_data (const char *, ssize_t *);

extern void BClipboard_set_system_data (const char *, const char *, ssize_t, bool);
extern void BClipboard_set_primary_selection_data (const char *, const char *,
						   ssize_t, bool);
extern void BClipboard_set_secondary_selection_data (const char *, const char *,
						     ssize_t, bool);

extern void BClipboard_system_targets (char **, int);
extern void BClipboard_primary_targets (char **, int);
extern void BClipboard_secondary_targets (char **, int);

extern bool BClipboard_owns_clipboard (void);
extern bool BClipboard_owns_primary (void);
extern bool BClipboard_owns_secondary (void);

/* Free the returned data.  */
extern void BClipboard_free_data (void *);

extern int be_enum_message (void *, int32 *, int32, int32 *, const char **);
extern int be_get_message_data (void *, const char *, int32, int32,
				const void **, ssize_t *);
extern int be_get_refs_data (void *, const char *, int32, char **);
extern int be_get_point_data (void *, const char *, int32, float *, float *);
extern uint32 be_get_message_type (void *);
extern void be_set_message_type (void *, uint32);
extern void *be_get_message_message (void *, const char *, int32);
extern void *be_create_simple_message (void);
extern int be_add_message_data (void *, const char *, int32, const void *, ssize_t);
extern int be_add_refs_data (void *, const char *, const char *);
extern int be_add_point_data (void *, const char *, float, float);
extern int be_add_message_message (void *, const char *, void *);
extern int be_lock_clipboard_message (enum haiku_clipboard, void **, bool);
extern void be_unlock_clipboard (enum haiku_clipboard, bool);
#ifdef __cplusplus
};
#endif
#endif /* _HAIKU_SELECT_H_ */

// Local Variables:
// eval: (setf (alist-get 'inextern-lang c-offsets-alist) 0)
// End:
