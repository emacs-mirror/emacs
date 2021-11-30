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

#ifndef _HAIKU_SELECT_H_
#define _HAIKU_SELECT_H_

#ifdef __cplusplus
#include <cstdio>
#endif

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

  /* Free the returned data.  */
  extern void BClipboard_free_data (void *ptr);
#ifdef __cplusplus
};
#endif
#endif /* _HAIKU_SELECT_H_ */
