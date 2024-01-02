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

#ifndef _HAIKU_SELECT_H_
#define _HAIKU_SELECT_H_

#ifdef __cplusplus
#include <cstdio>
#else
#include <stdio.h>
#endif

#include <SupportDefs.h>

enum haiku_clipboard
  {
    CLIPBOARD_PRIMARY,
    CLIPBOARD_SECONDARY,
    CLIPBOARD_CLIPBOARD
  };

#ifdef __cplusplus
extern "C"
{
#endif
/* Defined in haikuselect.c.  */
extern void haiku_selection_disowned (enum haiku_clipboard, int64);

/* Defined in haiku_select.cc.  */
extern void be_clipboard_init (void);
extern char *be_find_clipboard_data (enum haiku_clipboard, const char *, ssize_t *);
extern void be_set_clipboard_data (enum haiku_clipboard, const char *, const char *,
				   ssize_t, bool);
extern bool be_clipboard_owner_p (enum haiku_clipboard);
extern void be_update_clipboard_count (enum haiku_clipboard);

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
extern void be_handle_clipboard_changed_message (void);
extern void be_start_watching_selection (enum haiku_clipboard);
extern bool be_selection_outdated_p (enum haiku_clipboard, int64);
extern int64 be_get_clipboard_count (enum haiku_clipboard);

#ifdef __cplusplus
};
#endif
#endif /* _HAIKU_SELECT_H_ */

// Local Variables:
// eval: (setf (alist-get 'inextern-lang c-offsets-alist) 0)
// End:
