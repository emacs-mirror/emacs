/* An OLIT menubar widget, by Chuck Thompson <cthomp@cs.uiuc.edu>
   Copyright (C) 1993 Lucid, Inc.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software; you can redistribute it and/or 
modify it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

The Lucid Widget Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of 
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifndef LW_MENUBARP_H
#define LW_MENUBARP_H

typedef struct _lwMenuBarClassPart
{
  int ignore;
} lwMenuBarClassPart;

typedef struct _lwMenuBarClassRec
{
  CoreClassPart		core_class;
  CompositeClassPart	composite_class;
  lwMenuBarClassPart	menubar_class;
} lwMenuBarClassRec;

extern lwMenuBarClassRec lwMenubarClassRec;

typedef struct
{
  int empty;
} lwMenuBarPart;

typedef struct _lwMenuBarRec
{
  CorePart		core;
  CompositePart		composite;
  lwMenuBarPart		menubar;
} lwMenuBarRec;

#endif /* LW_MENUBARP_H */
