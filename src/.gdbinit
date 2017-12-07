# Copyright (C) 1992-1998, 2000-2017 Free Software Foundation, Inc.
#
# This file is part of GNU Emacs.
#
# GNU Emacs is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

# Force loading of symbols, enough to give us VALBITS etc.
set $dummy = main + 8
# With some compilers, we need this to give us struct Lisp_Symbol etc.:
set $dummy = Fmake_symbol + 8

# Find lwlib source files too.
dir ../lwlib
#dir /gd/gnu/lesstif-0.89.9/lib/Xm

# Don't enter GDB when user types C-g to quit.
# This has one unfortunate effect: you can't type C-c
# at the GDB to stop Emacs, when using X.
# However, C-z works just as well in that case.
handle 2 noprint pass

# Make it work like SIGINT normally does.
handle SIGTSTP nopass

# Pass on user signals
handle SIGUSR1 noprint pass
handle SIGUSR2 noprint pass

# Don't pass SIGALRM to Emacs.  This makes problems when
# debugging.
handle SIGALRM ignore

# Use $bugfix so that the value isn't a constant.
# Using a constant runs into GDB bugs sometimes.
define xgetptr
  if (CHECK_LISP_OBJECT_TYPE)
    set $bugfix = $arg0.i
  else
    set $bugfix = $arg0
  end
  set $ptr = $bugfix & VALMASK
end

define xgetint
  if (CHECK_LISP_OBJECT_TYPE)
    set $bugfix = $arg0.i
  else
    set $bugfix = $arg0
  end
  set $int = $bugfix << (USE_LSB_TAG ? 0 : INTTYPEBITS) >> INTTYPEBITS
end

define xgettype
  if (CHECK_LISP_OBJECT_TYPE)
    set $bugfix = $arg0.i
  else
    set $bugfix = $arg0
  end
  set $type = (enum Lisp_Type) (USE_LSB_TAG ? $bugfix & (1 << GCTYPEBITS) - 1 : (EMACS_UINT) $bugfix >> VALBITS)
end

define xgetsym
  xgetptr $arg0
  set $ptr = ((struct Lisp_Symbol *) ((char *)lispsym + $ptr))
end

# Access the name of a symbol
define xsymname
  xgetsym $arg0
  set $symname = $ptr->u.s.name
end

# Set up something to print out s-expressions.
# We save and restore print_output_debug_flag to prevent the w32 port
# from calling OutputDebugString, which causes GDB to display each
# character twice (yuk!).
define pr
  pp $
end
document pr
Print the emacs s-expression which is $.
Works only when an inferior emacs is executing.
end

# Print out s-expressions
define pp
  set $tmp = $arg0
  set $output_debug = print_output_debug_flag
  set print_output_debug_flag = 0
  call safe_debug_print ($tmp)
  set print_output_debug_flag = $output_debug
end
document pp
Print the argument as an emacs s-expression
Works only when an inferior emacs is executing.
end

# Print value of lisp variable
define pv
  set $tmp = "$arg0"
  set $output_debug = print_output_debug_flag
  set print_output_debug_flag = 0
  call safe_debug_print (find_symbol_value (intern ($tmp)))
  set print_output_debug_flag = $output_debug
end
document pv
Print the value of the lisp variable given as argument.
Works only when an inferior emacs is executing.
end

# Print out current buffer point and boundaries
define ppt
  set $b = current_buffer
  set $t = $b->text
  printf "BUF PT: %d", $b->pt
  if ($b->pt != $b->pt_byte)
    printf "[%d]", $b->pt_byte
  end
  printf " of 1..%d", $t->z
  if ($t->z != $t->z_byte)
    printf "[%d]", $t->z_byte
  end
  if ($b->begv != 1 || $b->zv != $t->z)
    printf " NARROW=%d..%d", $b->begv, $b->zv
    if ($b->begv != $b->begv_byte || $b->zv != $b->zv_byte)
      printf " [%d..%d]", $b->begv_byte, $b->zv_byte
    end
  end
  printf " GAP: %d", $t->gpt
  if ($t->gpt != $t->gpt_byte)
    printf "[%d]", $t->gpt_byte
  end
  printf " SZ=%d\n", $t->gap_size
end
document ppt
Print current buffer's point and boundaries.
Prints values of point, beg, end, narrow, and gap for current buffer.
end

define pitmethod
  set $itmethod = $arg0
  # output $itmethod
  if ($itmethod == 0)
    printf "GET_FROM_BUFFER"
  end
  if ($itmethod == 1)
    printf "GET_FROM_DISPLAY_VECTOR"
  end
  if ($itmethod == 2)
    printf "GET_FROM_STRING"
  end
  if ($itmethod == 3)
    printf "GET_FROM_C_STRING"
  end
  if ($itmethod == 4)
    printf "GET_FROM_IMAGE"
  end
  if ($itmethod == 5)
    printf "GET_FROM_STRETCH"
  end
  if ($itmethod < 0 || $itmethod > 5)
    output $itmethod
  end
end
document pitmethod
Pretty print it->method given as first arg
end

# Print out iterator given as first arg
define pitx
  set $it = $arg0
  printf "cur=%d", $it->current.pos.charpos
  if ($it->current.pos.charpos != $it->current.pos.bytepos)
    printf "[%d]", $it->current.pos.bytepos
  end
  printf " pos=%d", $it->position.charpos
  if ($it->position.charpos != $it->position.bytepos)
    printf "[%d]", $it->position.bytepos
  end
  printf " start=%d", $it->start.pos.charpos
  if ($it->start.pos.charpos != $it->start.pos.bytepos)
    printf "[%d]", $it->start.pos.bytepos
  end
  printf " end=%d", $it->end_charpos
  printf " stop=%d", $it->stop_charpos
  printf " face=%d", $it->face_id
  if ($it->multibyte_p)
    printf " MB"
  end
  if ($it->header_line_p)
    printf " HL"
  end
  if ($it->n_overlay_strings > 0)
    printf " nov=%d", $it->n_overlay_strings
  end
  if ($it->sp != 0)
    printf " sp=%d", $it->sp
  end
  # IT_CHARACTER
  if ($it->what == 0)
    if ($it->len == 1 && $it->c >= ' ' && it->c < 255)
      printf " ch='%c'", $it->c
    else
      printf " ch=[%d,%d]", $it->c, $it->len
    end
  else
    printf " "
    # output $it->what
    if ($it->what == 0)
      printf "IT_CHARACTER"
    end
    if ($it->what == 1)
      printf "IT_COMPOSITION"
    end
    if ($it->what == 2)
      printf "IT_IMAGE"
    end
    if ($it->what == 3)
      printf "IT_STRETCH"
    end
    if ($it->what == 4)
      printf "IT_EOB"
    end
    if ($it->what == 5)
      printf "IT_TRUNCATION"
    end
    if ($it->what == 6)
      printf "IT_CONTINUATION"
    end
    if ($it->what < 0 || $it->what > 6)
      output $it->what
    end
  end
  if ($it->method != 0)
    # !GET_FROM_BUFFER
    printf " next="
    pitmethod $it->method
    if ($it->method == 2)
      # GET_FROM_STRING
      printf "[%d]", $it->current.string_pos.charpos
    end
    if ($it->method == 4)
      # GET_FROM_IMAGE
      printf "[%d]", $it->image_id
    end
  end
  printf "\n"
  if ($it->bidi_p)
    printf "BIDI: base_stop=%d prev_stop=%d level=%d\n", $it->base_level_stop, $it->prev_stop, $it->bidi_it.resolved_level
  end
  if ($it->region_beg_charpos >= 0)
    printf "reg=%d-%d ", $it->region_beg_charpos, $it->region_end_charpos
  end
  printf "vpos=%d hpos=%d", $it->vpos, $it->hpos,
  printf " y=%d lvy=%d", $it->current_y, $it->last_visible_y
  printf " x=%d vx=%d-%d", $it->current_x, $it->first_visible_x, $it->last_visible_x
  printf " w=%d", $it->pixel_width
  printf " a+d=%d+%d=%d", $it->ascent, $it->descent, $it->ascent+$it->descent
  printf " max=%d+%d=%d", $it->max_ascent, $it->max_descent, $it->max_ascent+$it->max_descent
  printf "\n"
  set $i = 0
  while ($i < $it->sp && $i < 4)
    set $e = $it->stack[$i]
    printf "stack[%d]: ", $i
    pitmethod $e.method
    printf "[%d]", $e.position.charpos
    printf "\n"
    set $i = $i + 1
  end
end
document pitx
Pretty print a display iterator.
Take one arg, an iterator object or pointer.
end

define pit
  pitx it
end
document pit
Pretty print the display iterator it.
end

define prowx
  set $row = $arg0
  printf "y=%d x=%d pwid=%d", $row->y, $row->x, $row->pixel_width
  printf " a+d=%d+%d=%d", $row->ascent, $row->height-$row->ascent, $row->height
  printf " phys=%d+%d=%d", $row->phys_ascent, $row->phys_height-$row->phys_ascent, $row->phys_height
  printf " vis=%d\n", $row->visible_height
  printf "used=(LMargin=%d,Text=%d,RMargin=%d) Hash=%d\n", $row->used[0], $row->used[1], $row->used[2], $row->hash
  printf "start=%d end=%d", $row->start.pos.charpos, $row->end.pos.charpos
  if ($row->enabled_p)
    printf " ENA"
  end
  if ($row->displays_text_p)
    printf " DISP"
  end
  if ($row->mode_line_p)
    printf " MODEL"
  end
  if ($row->continued_p)
    printf " CONT"
  end
  if ($row-> truncated_on_left_p)
    printf " TRUNC:L"
  end
  if ($row-> truncated_on_right_p)
    printf " TRUNC:R"
  end
  if ($row->starts_in_middle_of_char_p)
    printf " STARTMID"
  end
  if ($row->ends_in_middle_of_char_p)
    printf " ENDMID"
  end
  if ($row->ends_in_newline_from_string_p)
    printf " ENDNLFS"
  end
  if ($row->ends_at_zv_p)
    printf " ENDZV"
  end
  if ($row->overlapped_p)
    printf " OLAPD"
  end
  if ($row->overlapping_p)
    printf " OLAPNG"
  end
  printf "\n"
end
document prowx
Pretty print information about glyph_row.
Takes one argument, a row object or pointer.
end

define prow
  prowx row
end
document prow
Pretty print information about glyph_row in row.
end


define pcursorx
  set $cp = $arg0
  printf "y=%d x=%d vpos=%d hpos=%d", $cp.y, $cp.x, $cp.vpos, $cp.hpos
end
document pcursorx
Pretty print a window cursor.
end

define pcursor
  printf "output: "
  pcursorx output_cursor
  printf "\n"
end
document pcursor
Pretty print the output_cursor.
end

define pwinx
  set $w = $arg0
  if ($w->mini != 0)
    printf "Mini "
  end
  printf "Window %d ", $w->sequence_number
  xgetptr $w->contents
  set $tem = (struct buffer *) $ptr
  xgetptr $tem->name_
  printf "%s", ((struct Lisp_String *) $ptr)->u.s.data
  printf "\n"
  xgetptr $w->start
  set $tem = (struct Lisp_Marker *) $ptr
  printf "start=%d end:", $tem->charpos
  if ($w->window_end_valid != 0)
    printf "pos=%d", $w->window_end_pos
    printf " vpos=%d", $w->window_end_vpos
  else
    printf "invalid"
  end
  printf " vscroll=%d", $w->vscroll
  if ($w->force_start != 0)
    printf " FORCE_START"
  end
  if ($w->must_be_updated_p)
    printf " MUST_UPD"
  end
  printf "\n"
  printf "cursor: "
  pcursorx $w->cursor
  printf "  phys: "
  pcursorx $w->phys_cursor
  if ($w->phys_cursor_on_p)
    printf " ON"
  else
    printf " OFF"
  end
  printf " blk="
  if ($w->last_cursor_off_p != $w->cursor_off_p)
    if ($w->last_cursor_off_p)
      printf "ON->"
    else
      printf "OFF->"
    end
  end
  if ($w->cursor_off_p)
    printf "ON"
  else
    printf "OFF"
  end
  printf "\n"
end
document pwinx
Pretty print a window structure.
Takes one argument, a pointer to a window structure.
end

define pwin
  pwinx w
end
document pwin
Pretty print window structure w.
end

define pbiditype
  if ($arg0 == 0)
    printf "UNDEF"
  end
  if ($arg0 == 1)
    printf "L"
  end
  if ($arg0 == 2)
    printf "R"
  end
  if ($arg0 == 3)
    printf "EN"
  end
  if ($arg0 == 4)
    printf "AN"
  end
  if ($arg0 == 5)
    printf "BN"
  end
  if ($arg0 == 6)
    printf "B"
  end
  if ($arg0 < 0 || $arg0 > 6)
    printf "%d??", $arg0
  end
end
document pbiditype
Print textual description of bidi type given as first argument.
end

define pgx
  set $g = $arg0
  # CHAR_GLYPH
  if ($g.type == 0)
    if ($g.u.ch >= ' ' && $g.u.ch < 127)
      printf "CHAR[%c]", $g.u.ch
    else
      printf "CHAR[0x%x]", $g.u.ch
    end
  end
  # COMPOSITE_GLYPH
  if ($g.type == 1)
    printf "COMP[%d (%d..%d)]", $g.u.cmp.id, $g.slice.cmp.from, $g.slice.cmp.to
  end
  # GLYPHLESS_GLYPH
  if ($g.type == 2)
    printf "G-LESS["
    if ($g.u.glyphless.method == 0)
      printf "THIN;0x%x]", $g.u.glyphless.ch
    end
    if ($g.u.glyphless.method == 1)
      printf "EMPTY;0x%x]", $g.u.glyphless.ch
    end
    if ($g.u.glyphless.method == 2)
      printf "ACRO;0x%x]", $g.u.glyphless.ch
    end
    if ($g.u.glyphless.method == 3)
      printf "HEX;0x%x]", $g.u.glyphless.ch
    end
  end
  # IMAGE_GLYPH
  if ($g.type == 3)
    printf "IMAGE[%d]", $g.u.img_id
  end
  # STRETCH_GLYPH
  if ($g.type == 4)
    printf "STRETCH[%d+%d]", $g.u.stretch.height, $g.u.stretch.ascent
  end
  xgettype ($g.object)
  if ($type == Lisp_String)
    xgetptr $g.object
    printf " str=0x%x[%d]", ((struct Lisp_String *)$ptr)->u.s.data, $g.charpos
  else
    printf " pos=%d", $g.charpos
  end
  # For characters, print their resolved level and bidi type
  if ($g.type == 0 || $g.type == 2)
    printf " blev=%d,btyp=", $g.resolved_level
    pbiditype $g.bidi_type
  end
  printf " w=%d a+d=%d+%d", $g.pixel_width, $g.ascent, $g.descent
  # If not DEFAULT_FACE_ID
  if ($g.face_id != 0)
    printf " face=%d", $g.face_id
  end
  if ($g.voffset)
    printf " vof=%d", $g.voffset
  end
  if ($g.multibyte_p)
    printf " MB"
  end
  if ($g.padding_p)
    printf " PAD"
  end
  if ($g.glyph_not_available_p)
    printf " N/A"
  end
  if ($g.overlaps_vertically_p)
    printf " OVL"
  end
  if ($g.avoid_cursor_p)
    printf " AVOID"
  end
  if ($g.left_box_line_p)
    printf " ["
  end
  if ($g.right_box_line_p)
    printf " ]"
  end
  if ($g.slice.img.x || $g.slice.img.y || $g.slice.img.width || $g.slice.img.height)
    printf " slice=%d,%d,%d,%d" ,$g.slice.img.x, $g.slice.img.y, $g.slice.img.width, $g.slice.img.height
  end
  printf "\n"
end
document pgx
Pretty print a glyph structure.
Takes one argument, a pointer to a glyph structure.
end

define pg
  set $pgidx = 0
  pgx glyph
end
document pg
Pretty print glyph structure glyph.
end

define pgi
  set $pgidx = $arg0
  pgx (&glyph[$pgidx])
end
document pgi
Pretty print glyph structure glyph[I].
Takes one argument, a integer I.
end

define pgn
  set $pgidx = $pgidx + 1
  pgx (&glyph[$pgidx])
end
document pgn
Pretty print next glyph structure.
end

define pgrowx
  set $row = $arg0
  set $area = 0
  set $xofs = $row->x
  while ($area < 3)
    set $used = $row->used[$area]
    if ($used > 0)
      set $gl0 = $row->glyphs[$area]
      set $pgidx = 0
      printf "%s: %d glyphs\n", ($area == 0 ? "LEFT" : $area == 2 ? "RIGHT" : "TEXT"), $used
      while ($pgidx < $used)
	printf "%3d %4d: ", $pgidx, $xofs
	pgx $gl0[$pgidx]
	set $xofs = $xofs + $gl0[$pgidx]->pixel_width
	set $pgidx = $pgidx + 1
      end
    end
    set $area = $area + 1
  end
end
document pgrowx
Pretty print all glyphs in a row structure.
Takes one argument, a pointer to a row structure.
end

define pgrow
  pgrowx row
end
document pgrow
Pretty print all glyphs in row structure row.
end

define pgrowit
  pgrowx it->glyph_row
end
document pgrowit
Pretty print all glyphs in it->glyph_row.
end

define prowlims
  printf "edges=(%d,%d),enb=%d,r2l=%d,cont=%d,trunc=(%d,%d),at_zv=%d\n", $arg0->minpos.charpos, $arg0->maxpos.charpos, $arg0->enabled_p, $arg0->reversed_p, $arg0->continued_p, $arg0->truncated_on_left_p, $arg0->truncated_on_right_p, $arg0->ends_at_zv_p
end
document prowlims
Print important attributes of a glyph_row structure.
Takes one argument, a pointer to a glyph_row structure.
end

define pmtxrows
  set $mtx = $arg0
  set $gl = $mtx->rows
  set $glend = $mtx->rows + $mtx->nrows - 1
  set $i = 0
  while ($gl < $glend)
    printf "%d: ", $i
    prowlims $gl
    set $gl = $gl + 1
    set $i = $i + 1
  end
end
document pmtxrows
Print data about glyph rows in a glyph matrix.
Takes one argument, a pointer to a glyph_matrix structure.
end

define xtype
  xgettype $
  output $type
  echo \n
  if $type == Lisp_Misc
    xmisctype
  else
    if $type == Lisp_Vectorlike
      xvectype
    end
  end
end
document xtype
Print the type of $, assuming it is an Emacs Lisp value.
If the first type printed is Lisp_Vector or Lisp_Misc,
a second line gives the more precise type.
end

define pvectype
  set $size = ((struct Lisp_Vector *) $arg0)->header.size
  if ($size & PSEUDOVECTOR_FLAG)
    output (enum pvec_type) (($size & PVEC_TYPE_MASK) >> PSEUDOVECTOR_AREA_BITS)
  else
    output PVEC_NORMAL_VECTOR
  end
  echo \n
end
document pvectype
Print the subtype of vectorlike object.
Takes one argument, a pointer to an object.
end

define xvectype
  xgetptr $
  pvectype $ptr
end
document xvectype
Print the subtype of vectorlike object.
This command assumes that $ is a Lisp_Object.
end

define pvecsize
  set $size = ((struct Lisp_Vector *) $arg0)->header.size
  if ($size & PSEUDOVECTOR_FLAG)
    output ($size & PSEUDOVECTOR_SIZE_MASK)
    echo \n
    output (($size & PSEUDOVECTOR_REST_MASK) >> PSEUDOVECTOR_SIZE_BITS)
  else
    output ($size & ~ARRAY_MARK_FLAG)
  end
  echo \n
end
document pvecsize
Print the size of vectorlike object.
Takes one argument, a pointer to an object.
end

define xvecsize
  xgetptr $
  pvecsize $ptr
end
document xvecsize
Print the size of $
This command assumes that $ is a Lisp_Object.
end

define xmisctype
  xgetptr $
  output (enum Lisp_Misc_Type) (((struct Lisp_Free *) $ptr)->type)
  echo \n
end
document xmisctype
Assume that $ is some misc type and print its specific type.
end

define xint
  xgetint $
  print $int
end
document xint
Print $ as an Emacs Lisp integer.  This gets the sign right.
end

define xptr
  xgetptr $
  print (void *) $ptr
end
document xptr
Print the pointer portion of an Emacs Lisp value in $.
end

define xmarker
  xgetptr $
  print (struct Lisp_Marker *) $ptr
end
document xmarker
Print $ as a marker pointer.
This command assumes that $ is an Emacs Lisp marker value.
end

define xoverlay
  xgetptr $
  print (struct Lisp_Overlay *) $ptr
end
document xoverlay
Print $ as a overlay pointer.
This command assumes that $ is an Emacs Lisp overlay value.
end

define xmiscfree
  xgetptr $
  print (struct Lisp_Free *) $ptr
end
document xmiscfree
Print $ as a misc free-cell pointer.
This command assumes that $ is an Emacs Lisp Misc value.
end

define xsymbol
  set $sym = $
  xgetsym $sym
  print (struct Lisp_Symbol *) $ptr
  xprintsym $sym
  echo \n
end
document xsymbol
Print the name and address of the symbol $.
This command assumes that $ is an Emacs Lisp symbol value.
end

define xstring
  xgetptr $
  print (struct Lisp_String *) $ptr
  xprintstr $
  echo \n
end
document xstring
Print the contents and address of the string $.
This command assumes that $ is an Emacs Lisp string value.
end

define xvector
  xgetptr $
  print (struct Lisp_Vector *) $ptr
  output ($->header.size > 50) ? 0 : ($->contents[0])@($->header.size & ~ARRAY_MARK_FLAG)
echo \n
end
document xvector
Print the contents and address of the vector $.
This command assumes that $ is an Emacs Lisp vector value.
end

define xprocess
  xgetptr $
  print (struct Lisp_Process *) $ptr
  output *$
  echo \n
end
document xprocess
Print the address of the struct Lisp_process to which $ points.
This command assumes that $ is a Lisp_Object.
end

define xframe
  xgetptr $
  print (struct frame *) $ptr
  xgetptr $->name
  set $ptr = (struct Lisp_String *) $ptr
  xprintstr $ptr
  echo \n
end
document xframe
Print $ as a frame pointer.
This command assumes $ is an Emacs Lisp frame value.
end

define xcompiled
  xgetptr $
  print (struct Lisp_Vector *) $ptr
  output ($->contents[0])@($->header.size & 0xff)
end
document xcompiled
Print $ as a compiled function pointer.
This command assumes that $ is an Emacs Lisp compiled value.
end

define xwindow
  xgetptr $
  print (struct window *) $ptr
  set $window = (struct window *) $ptr
  printf "%dx%d+%d+%d\n", $window->total_cols, $window->total_lines, $window->left_col, $window->top_line
end
document xwindow
Print $ as a window pointer, assuming it is an Emacs Lisp window value.
Print the window's position as "WIDTHxHEIGHT+LEFT+TOP".
end

define xwinconfig
  xgetptr $
  print (struct save_window_data *) $ptr
end
document xwinconfig
Print $ as a window configuration pointer.
This command assumes that $ is an Emacs Lisp window configuration value.
end

define xsubr
  xgetptr $
  print (struct Lisp_Subr *) $ptr
  output *$
  echo \n
end
document xsubr
Print the address of the subr which the Lisp_Object $ points to.
end

define xchartable
  xgetptr $
  print (struct Lisp_Char_Table *) $ptr
  printf "Purpose: "
  xprintsym $->purpose
  printf "  %d extra slots", ($->header.size & 0x1ff) - 68
  echo \n
end
document xchartable
Print the address of the char-table $, and its purpose.
This command assumes that $ is an Emacs Lisp char-table value.
end

define xsubchartable
  xgetptr $
  print (struct Lisp_Sub_Char_Table *) $ptr
  set $subchartab = (struct Lisp_Sub_Char_Table *) $ptr
  printf "Depth: %d, Min char: %d (0x%x)\n", $subchartab->depth, $subchartab->min_char, $subchartab->min_char
end
document xsubchartable
Print the address of the sub-char-table $, its depth and min-char.
This command assumes that $ is an Emacs Lisp sub-char-table value.
end

define xboolvector
  xgetptr $
  print (struct Lisp_Bool_Vector *) $ptr
  output ($->size > 256) ? 0 : ($->data[0])@(($->size + BOOL_VECTOR_BITS_PER_CHAR - 1)/ BOOL_VECTOR_BITS_PER_CHAR)
  echo \n
end
document xboolvector
Print the contents and address of the bool-vector $.
This command assumes that $ is an Emacs Lisp bool-vector value.
end

define xbuffer
  xgetptr $
  print (struct buffer *) $ptr
  xgetptr $->name_
  output ((struct Lisp_String *) $ptr)->u.s.data
  echo \n
end
document xbuffer
Set $ as a buffer pointer and the name of the buffer.
This command assumes $ is an Emacs Lisp buffer value.
end

define xhashtable
  xgetptr $
  print (struct Lisp_Hash_Table *) $ptr
end
document xhashtable
Set $ as a hash table pointer.
This command assumes that $ is an Emacs Lisp hash table value.
end

define xcons
  xgetptr $
  print (struct Lisp_Cons *) $ptr
  output/x *$
  echo \n
end
document xcons
Print the contents of $ as an Emacs Lisp cons.
end

define nextcons
  p $.u.cdr
  xcons
end
document nextcons
Print the contents of the next cell in a list.
This command assumes that the last thing you printed was a cons cell contents
(type struct Lisp_Cons) or a pointer to one.
end
define xcar
  xgetptr $
  xgettype $
  print/x ($type == Lisp_Cons ? ((struct Lisp_Cons *) $ptr)->u.s.car : 0)
end
document xcar
Assume that $ is an Emacs Lisp pair and print its car.
end

define xcdr
  xgetptr $
  xgettype $
  print/x ($type == Lisp_Cons ? ((struct Lisp_Cons *) $ptr)->u.s.u.cdr : 0)
end
document xcdr
Assume that $ is an Emacs Lisp pair and print its cdr.
end

define xlist
  xgetptr $
  set $cons = (struct Lisp_Cons *) $ptr
  xgetptr Qnil
  set $nil = $ptr
  set $i = 0
  while $cons != $nil && $i < 10
    p/x $cons->u.s.car
    xpr
    xgetptr $cons->u.s.u.cdr
    set $cons = (struct Lisp_Cons *) $ptr
    set $i = $i + 1
    printf "---\n"
  end
  if $cons == $nil
    printf "nil\n"
  else
    printf "...\n"
    p $ptr
  end
end
document xlist
Print $ assuming it is a list.
end

define xfloat
  xgetptr $
  print ((struct Lisp_Float *) $ptr)->u.data
end
document xfloat
Print $ assuming it is a lisp floating-point number.
end

define xscrollbar
  xgetptr $
  print (struct scrollbar *) $ptr
output *$
echo \n
end
document xscrollbar
Print $ as a scrollbar pointer.
end

define xpr
  xtype
  if $type == Lisp_Int0 || $type == Lisp_Int1
    xint
  end
  if $type == Lisp_Symbol
    xsymbol
  end
  if $type == Lisp_String
    xstring
  end
  if $type == Lisp_Cons
    xcons
  end
  if $type == Lisp_Float
    xfloat
  end
  if $type == Lisp_Misc
    set $misc = (enum Lisp_Misc_Type) (((struct Lisp_Free *) $ptr)->type)
    if $misc == Lisp_Misc_Free
      xmiscfree
    end
    if $misc == Lisp_Misc_Marker
      xmarker
    end
    if $misc == Lisp_Misc_Overlay
      xoverlay
    end
#    if $misc == Lisp_Misc_Save_Value
#      xsavevalue
#    end
  end
  if $type == Lisp_Vectorlike
    set $size = ((struct Lisp_Vector *) $ptr)->header.size
    if ($size & PSEUDOVECTOR_FLAG)
      set $vec = (enum pvec_type) (($size & PVEC_TYPE_MASK) >> PSEUDOVECTOR_AREA_BITS)
      if $vec == PVEC_NORMAL_VECTOR
	xvector
      end
      if $vec == PVEC_PROCESS
	xprocess
      end
      if $vec == PVEC_FRAME
	xframe
      end
      if $vec == PVEC_COMPILED
	xcompiled
      end
      if $vec == PVEC_WINDOW
	xwindow
      end
      if $vec == PVEC_WINDOW_CONFIGURATION
	xwinconfig
      end
      if $vec == PVEC_SUBR
	xsubr
      end
      if $vec == PVEC_CHAR_TABLE
	xchartable
      end
      if $vec == PVEC_BOOL_VECTOR
	xboolvector
      end
      if $vec == PVEC_BUFFER
	xbuffer
      end
      if $vec == PVEC_HASH_TABLE
	xhashtable
      end
    else
      xvector
    end
  end
end
document xpr
Print $ as a lisp object of any type.
end

define xprintstr
  set $data = (char *) $arg0->u.s.data
  set $strsize = ($arg0->u.s.size_byte < 0) ? ($arg0->u.s.size & ~ARRAY_MARK_FLAG) : $arg0->u.s.size_byte
  # GDB doesn't like zero repetition counts
  if $strsize == 0
    output ""
  else
    output ($arg0->u.s.size > 1000) ? 0 : ($data[0])@($strsize)
  end
end

define xprintsym
  xsymname $arg0
  xgetptr $symname
  set $sym_name = (struct Lisp_String *) $ptr
  xprintstr $sym_name
end
document xprintsym
  Print argument as a symbol.
end

define xcoding
  set $tmp = (struct Lisp_Hash_Table *) (Vcoding_system_hash_table & VALMASK)
  set $tmp = (struct Lisp_Vector *) ($tmp->key_and_value & VALMASK)
  set $name = $tmp->contents[$arg0 * 2]
  print $name
  pr
  print $tmp->contents[$arg0 * 2 + 1]
  pr
end
document xcoding
  Print the name and attributes of coding system that has ID (argument).
end

define xcharset
  set $tmp = (struct Lisp_Hash_Table *) (Vcharset_hash_table & VALMASK)
  set $tmp = (struct Lisp_Vector *) ($tmp->key_and_value & VALMASK)
  p $tmp->contents[charset_table[$arg0].hash_index * 2]
  pr
end
document xcharset
  Print the name of charset that has ID (argument).
end

define xfontset
  xgetptr $
  set $tbl = (struct Lisp_Char_Table *) $ptr
  print $tbl
  xgetint $tbl->extras[0]
  printf " ID:%d", $int
  xgettype $tbl->extras[1]
  xgetptr $tbl->extras[1]
  if $type == Lisp_String
    set $ptr = (struct Lisp_String *) $ptr
    printf " Name:"
    xprintstr $ptr
  else
    xgetptr $tbl->extras[2]
    set $ptr = (struct Lisp_Char_Table *) $ptr
    xgetptr $ptr->extras[1]
    set $ptr = (struct Lisp_String *) $ptr
    printf " Realized from:"
    xprintstr $ptr
  end
  echo \n
end

define xfont
  xgetptr $
  set $size = (((struct Lisp_Vector *) $ptr)->header.size & 0x1FF)
  if $size == FONT_SPEC_MAX
    print (struct font_spec *) $ptr
  else
    if $size == FONT_ENTITY_MAX
      print (struct font_entity *) $ptr
    else
      print (struct font *) $ptr
    end
  end
end
document xfont
Print $ assuming it is a list font (font-spec, font-entity, or font-object).
end

define xbacktrace
  set $bt = backtrace_top ()
  while backtrace_p ($bt)
    set $fun = backtrace_function ($bt)
    xgettype $fun
    if $type == Lisp_Symbol
      xprintsym $fun
      printf " (0x%x)\n", backtrace_args ($bt)
    else
      xgetptr $fun
      printf "0x%x ", $ptr
      if $type == Lisp_Vectorlike
	xgetptr $fun
        set $size = ((struct Lisp_Vector *) $ptr)->header.size
        if ($size & PSEUDOVECTOR_FLAG)
	  output (enum pvec_type) (($size & PVEC_TYPE_MASK) >> PSEUDOVECTOR_AREA_BITS)
	else
	  output $size & ~ARRAY_MARK_FLAG
	end
      else
        printf "Lisp type %d", $type
      end
      echo \n
    end
    set $bt = backtrace_next ($bt)
  end
end
document xbacktrace
  Print a backtrace of Lisp function calls from backtrace_list.
  Set a breakpoint at Fsignal and call this to see from where
  an error was signaled.
end

define xprintbytestr
  set $data = (char *) $arg0->data
  set $bstrsize = ($arg0->size_byte < 0) ? ($arg0->size & ~ARRAY_MARK_FLAG) : $arg0->size_byte
  printf "Bytecode: "
  if $bstrsize > 0
    output/u ($arg0->size > 1000) ? 0 : ($data[0])@($bvsize)
  else
    printf ""
  end
end
document xprintbytestr
  Print a string of byte code.
end

define xwhichsymbols
  set $output_debug = print_output_debug_flag
  set print_output_debug_flag = 0
  call safe_debug_print (which_symbols ($arg0, $arg1))
  set print_output_debug_flag = $output_debug
end
document xwhichsymbols
  Print symbols which references a given lisp object
  either as its symbol value or symbol function.
  Call with two arguments: the lisp object and the
  maximum number of symbols referencing it to produce.
end

# Show Lisp backtrace after normal backtrace.
define hookpost-backtrace
  set $bt = backtrace_top ()
  if backtrace_p ($bt)
    echo \n
    echo Lisp Backtrace:\n
    xbacktrace
  end
end

# Flush display (X only)
define ff
  set x_flush (0)
end
document ff
Flush pending X window display updates to screen.
Works only when an inferior emacs is executing.
end


set print pretty on
set print sevenbit-strings

show environment DISPLAY
show environment TERM

# When debugging, it is handy to be able to "return" from
# terminate_due_to_signal when an assertion failure is non-fatal.
break terminate_due_to_signal

# x_error_quitter is defined only on X.  But window-system is set up
# only at run time, during Emacs startup, so we need to defer setting
# the breakpoint.  init_sys_modes is the first function called on
# every platform after init_display, where window-system is set.
tbreak init_sys_modes
commands
  silent
  xsymname globals.f_Vinitial_window_system
  xgetptr $symname
  set $tem = (struct Lisp_String *) $ptr
  set $tem = (char *) $tem->u.s.data
  # If we are running in synchronous mode, we want a chance to look
  # around before Emacs exits.  Perhaps we should put the break
  # somewhere else instead...
  if $tem[0] == 'x' && $tem[1] == '\0'
    break x_error_quitter
  end
  continue
end


# Put the Python code at the end of .gdbinit so that if GDB does not
# support Python, GDB will do all the above initializations before
# reporting an error.

python

# Omit pretty-printing in older (pre-7.3) GDBs that lack it.
if hasattr(gdb, 'printing'):

  class Emacs_Pretty_Printers (gdb.printing.RegexpCollectionPrettyPrinter):
    """A collection of pretty-printers.  This is like GDB's
       RegexpCollectionPrettyPrinter except when printing Lisp_Object."""
    def __call__ (self, val):
      """Look up the pretty-printer for the provided value."""
      type = val.type.unqualified ()
      typename = type.tag or type.name
      basic_type = gdb.types.get_basic_type (type)
      basic_typename = basic_type.tag or basic_type.name
      for printer in self.subprinters:
        if (printer.enabled
            and ((printer.regexp == '^Lisp_Object$'
                  and typename == 'Lisp_Object')
                 or (basic_typename
                     and printer.compiled_re.search (basic_typename)))):
          return printer.gen_printer (val)
      return None

  class Lisp_Object_Printer:
    "A printer for Lisp_Object values."
    def __init__ (self, val):
      self.val = val

    def to_string (self):
      "Yield a string that can be fed back into GDB."

      # This implementation should work regardless of C compiler, and
      # it should not attempt to run any code in the inferior.

      # If the macros EMACS_INT_WIDTH and USE_LSB_TAG are not in the
      # symbol table, guess reasonable defaults.
      sym = gdb.lookup_symbol ("EMACS_INT_WIDTH")[0]
      if sym:
        EMACS_INT_WIDTH = int (sym.value ())
      else:
        sym = gdb.lookup_symbol ("EMACS_INT")[0]
        EMACS_INT_WIDTH = 8 * sym.type.sizeof
      sym = gdb.lookup_symbol ("USE_LSB_TAG")[0]
      if sym:
        USE_LSB_TAG = int (sym.value ())
      else:
        USE_LSB_TAG = 1

      GCTYPEBITS = 3
      VALBITS = EMACS_INT_WIDTH - GCTYPEBITS
      Lisp_Int0 = 2
      Lisp_Int1 = 6 if USE_LSB_TAG else 3

      # Unpack the Lisp value from its containing structure, if necessary.
      val = self.val
      basic_type = gdb.types.get_basic_type (val.type)
      if (basic_type.code == gdb.TYPE_CODE_STRUCT
          and gdb.types.has_field (basic_type, "i")):
        val = val["i"]

      # For nil, yield "XIL(0)", which is easier to read than "XIL(0x0)".
      if not val:
        return "XIL(0)"

      # Extract the integer representation of the value and its Lisp type.
      ival = int(val)
      itype = ival >> (0 if USE_LSB_TAG else VALBITS)
      itype = itype & ((1 << GCTYPEBITS) - 1)

      # For a Lisp integer N, yield "make_number(N)".
      if itype == Lisp_Int0 or itype == Lisp_Int1:
        if USE_LSB_TAG:
          ival = ival >> (GCTYPEBITS - 1)
        elif (ival >> VALBITS) & 1:
          ival = ival | (-1 << VALBITS)
        else:
          ival = ival & ((1 << VALBITS) - 1)
        return "make_number(%d)" % ival

      # For non-integers other than nil yield "XIL(N)", where N is a C integer.
      # This helps humans distinguish Lisp_Object values from ordinary
      # integers even when Lisp_Object is an integer.
      # Perhaps some day the pretty-printing could be fancier.
      # Prefer the unsigned representation to negative values, converting
      # by hand as val.cast(gdb.lookup_type("EMACS_UINT") does not work in
      # GDB 7.12.1; see <http://patchwork.sourceware.org/patch/11557/>.
      if ival < 0:
        ival = ival + (1 << EMACS_INT_WIDTH)
      return "XIL(0x%x)" % ival

  def build_pretty_printer ():
    pp = Emacs_Pretty_Printers ("Emacs")
    pp.add_printer ('Lisp_Object', '^Lisp_Object$', Lisp_Object_Printer)
    return pp

  gdb.printing.register_pretty_printer (gdb.current_objfile (),
                                        build_pretty_printer (), True)
end

# GDB mishandles indentation with leading tabs when feeding it to Python.
# Local Variables:
# indent-tabs-mode: nil
# End:
