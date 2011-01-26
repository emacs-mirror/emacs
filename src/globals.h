/* Declare all global lisp variables.

   Copyright (C) 2011  Free Software Foundation, Inc.

   This file is part of GNU Emacs.

   GNU Emacs is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   GNU Emacs is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

struct emacs_globals
{

  /* Count the amount of consing of various sorts of space.  */
  EMACS_INT f_cons_cells_consed;

  EMACS_INT f_floats_consed;

  EMACS_INT f_vector_cells_consed;

  EMACS_INT f_symbols_consed;

  EMACS_INT f_string_chars_consed;

  EMACS_INT f_misc_objects_consed;

  EMACS_INT f_intervals_consed;

  EMACS_INT f_strings_consed;

  /* Minimum number of bytes of consing since GC before next GC. */
  EMACS_INT f_gc_cons_threshold;

  Lisp_Object f_Vgc_cons_percentage;

  /* Nonzero means display messages at beginning and end of GC.  */
  int f_garbage_collection_messages;

  /* Non-nil means defun should do purecopy on the function definition.  */
  Lisp_Object f_Vpurify_flag;

  /* Non-nil means we are handling a memory-full error.  */
  Lisp_Object f_Vmemory_full;

  /* Total number of bytes allocated in pure storage. */
  EMACS_INT f_pure_bytes_used;

  /* Pre-computed signal argument for use when memory is exhausted.  */
  Lisp_Object f_Vmemory_signal_data;

  Lisp_Object f_Vpost_gc_hook;

  Lisp_Object f_Vgc_elapsed;

  EMACS_INT f_gcs_done;

  /* Functions to call before and after each text change. */
  Lisp_Object f_Vbefore_change_functions;

  Lisp_Object f_Vafter_change_functions;

  Lisp_Object f_Vtransient_mark_mode;

  /* t means ignore all read-only text properties.
     A list means ignore such a property if its value is a member of the list.
     Any non-nil value means ignore buffer-read-only.  */
  Lisp_Object f_Vinhibit_read_only;

  /* List of functions to call that can query about killing a buffer.
     If any of these functions returns nil, we don't kill it.  */
  Lisp_Object f_Vkill_buffer_query_functions;

  Lisp_Object f_Vchange_major_mode_hook;

  /* List of functions to call before changing an unmodified buffer.  */
  Lisp_Object f_Vfirst_change_hook;

  /* If nonzero, all modification hooks are suppressed.  */
  int f_inhibit_modification_hooks;

  Lisp_Object f_Vbyte_code_meter;

  int f_byte_metering_on;

  Lisp_Object f_Vcurrent_prefix_arg;

  Lisp_Object f_Vcommand_history;

  Lisp_Object f_Vcommand_debug_status;

  /* Non-nil means treat the mark as active
     even if mark_active is 0.  */
  Lisp_Object f_Vmark_even_if_inactive;

  Lisp_Object f_Vmouse_leave_buffer_hook;

  Lisp_Object f_Vexec_path;
  Lisp_Object f_Vexec_directory;
  Lisp_Object f_Vexec_suffixes;

  Lisp_Object f_Vdata_directory;
  Lisp_Object f_Vdoc_directory;

  Lisp_Object f_Vconfigure_info_directory;
  Lisp_Object f_Vshared_game_score_directory;

  Lisp_Object f_Vshell_file_name;

  Lisp_Object f_Vprocess_environment;
  Lisp_Object f_Vinitial_environment;

  /* Variables to determine word boundary.  */
  Lisp_Object f_Vword_combining_categories;
  Lisp_Object f_Vword_separating_categories;

  /* This contains all code conversion map available to CCL.  */
  Lisp_Object f_Vcode_conversion_map_vector;

  /* Alist of fontname patterns vs corresponding CCL program.  */
  Lisp_Object f_Vfont_ccl_encoder_alist;

  /* Vector of registered hash tables for translation.  */
  Lisp_Object f_Vtranslation_hash_table_vector;

  /* Vector of translation table ever defined.
     ID of a translation table is used to index this vector.  */
  Lisp_Object f_Vtranslation_table_vector;

  /* A char-table for characters which may invoke auto-filling.  */
  Lisp_Object f_Vauto_fill_chars;

  /* A char-table.  An element is non-nil iff the corresponding
     character has a printable glyph.  */
  Lisp_Object f_Vprintable_chars;

  /* A char-table.  An elemnent is a column-width of the corresponding
     character.  */
  Lisp_Object f_Vchar_width_table;

  /* A char-table.  An element is a symbol indicating the direction
     property of corresponding character.  */
  Lisp_Object f_Vchar_direction_table;

  /* Char table of scripts.  */
  Lisp_Object f_Vchar_script_table;

  /* Alist of scripts vs representative characters.  */
  Lisp_Object f_Vscript_representative_chars;

  Lisp_Object f_Vunicode_category_table;

  /* List of all charsets.  This variable is used only from Emacs
     Lisp.  */
  Lisp_Object f_Vcharset_list;

  Lisp_Object f_Vcharset_map_path;

  /* If nonzero, don't load charset maps.  */
  int f_inhibit_load_charset_map;

  Lisp_Object f_Vcurrent_iso639_language;

  Lisp_Object f_Vpost_self_insert_hook;

  int f_coding_system_require_warning;

  Lisp_Object f_Vselect_safe_coding_system_function;

  /* Mnemonic string for each format of end-of-line.  */
  Lisp_Object f_eol_mnemonic_unix;
  Lisp_Object f_eol_mnemonic_dos;
  Lisp_Object f_eol_mnemonic_mac;

  /* Mnemonic string to indicate format of end-of-line is not yet
     decided.  */
  Lisp_Object f_eol_mnemonic_undecided;

  Lisp_Object f_Vcoding_system_list;
  Lisp_Object f_Vcoding_system_alist;

  /* Coding-system for reading files and receiving data from process.  */
  Lisp_Object f_Vcoding_system_for_read;

  /* Coding-system for writing files and sending data to process.  */
  Lisp_Object f_Vcoding_system_for_write;

  /* Coding-system actually used in the latest I/O.  */
  Lisp_Object f_Vlast_coding_system_used;

  /* Set to non-nil when an error is detected while code conversion.  */
  Lisp_Object f_Vlast_code_conversion_error;

  /* A vector of length 256 which contains information about special
     Latin codes (especially for dealing with Microsoft codes).  */
  Lisp_Object f_Vlatin_extra_code_table;

  /* Flag to inhibit code conversion of end-of-line format.  */
  int f_inhibit_eol_conversion;

  /* Flag to inhibit ISO2022 escape sequence detection.  */
  int f_inhibit_iso_escape_detection;

  /* Flag to inhibit detection of binary files through null bytes.  */
  int f_inhibit_null_byte_detection;

  /* Flag to make buffer-file-coding-system inherit from process-coding.  */
  int f_inherit_process_coding_system;

  Lisp_Object f_Vfile_coding_system_alist;

  Lisp_Object f_Vprocess_coding_system_alist;

  Lisp_Object f_Vnetwork_coding_system_alist;

  Lisp_Object f_Vlocale_coding_system;

  /* Flag to tell if we look up translation table on character code
     conversion.  */
  Lisp_Object f_Venable_character_translation;

  /* Standard translation table to look up on decoding (reading).  */
  Lisp_Object f_Vstandard_translation_table_for_decode;

  /* Standard translation table to look up on encoding (writing).  */
  Lisp_Object f_Vstandard_translation_table_for_encode;

  /* Alist of charsets vs revision number.  */
  Lisp_Object f_Vcharset_revision_table;

  /* Default coding systems used for process I/O.  */
  Lisp_Object f_Vdefault_process_coding_system;

  /* Char table for translating Quail and self-inserting input.  */
  Lisp_Object f_Vtranslation_table_for_input;

  /* List of symbols `coding-category-xxx' ordered by priority.  This
     variable is exposed to Emacs Lisp.  */
  Lisp_Object f_Vcoding_category_list;

  /* Function to call to adjust composition.  */
  Lisp_Object f_Vcompose_chars_after_function;

  Lisp_Object f_Vauto_composition_mode;

  Lisp_Object f_Vauto_composition_function;

  Lisp_Object f_Vcomposition_function_table;

  Lisp_Object f_Vmost_positive_fixnum;
  Lisp_Object f_Vmost_negative_fixnum;

  /* Registered buses.  */
  Lisp_Object f_Vdbus_registered_buses;

  /* Hash table which keeps function definitions.  */
  Lisp_Object f_Vdbus_registered_objects_table;

  /* Whether to debug D-Bus.  */
  Lisp_Object f_Vdbus_debug;

  Lisp_Object f_Vcompletion_ignored_extensions;

  /* Non-zero means don't pause redisplay for pending input.  (This is
     for debugging and for a future implementation of EDT-like
     scrolling.  */
  int f_redisplay_dont_pause;

  /* If a number (float), check for user input every N seconds.  */
  Lisp_Object f_Vredisplay_preemption_period;

  /* Lisp variable visible-bell; enables use of screen-flash instead of
     audible bell.  */
  int f_visible_bell;

  /* Invert the color of the whole frame, at a low level.  */
  int f_inverse_video;

  /* Line speed of the terminal.  */
  EMACS_INT f_baud_rate;

  /* Either nil or a symbol naming the window system under which Emacs
     creates the first frame.  */
  Lisp_Object f_Vinitial_window_system;

  /* Version number of X windows: 10, 11 or nil.  */
  Lisp_Object f_Vwindow_system_version;

  /* Vector of glyph definitions.  Indexed by glyph number, the contents
     are a string which is how to output the glyph.

     If Vglyph_table is nil, a glyph is output by using its low 8 bits
     as a character code.

     This is an obsolete feature that is no longer used.  The variable
     is retained for compatibility.  */
  Lisp_Object f_Vglyph_table;

  /* Display table to use for vectors that don't specify their own.  */
  Lisp_Object f_Vstandard_display_table;

  /* Nonzero means reading single-character input with prompt so put
     cursor on mini-buffer after the prompt.  Positive means at end of
     text in echo area; negative means at beginning of line.  */
  int f_cursor_in_echo_area;

  Lisp_Object f_Vdoc_file_name;

  /* A list of files used to build this Emacs binary.  */
  Lisp_Object f_Vbuild_files;

  /* country info */
  EMACS_INT f_dos_country_code;

  EMACS_INT f_dos_codepage;

  EMACS_INT f_dos_timezone_offset;

  EMACS_INT f_dos_decimal_point;

  EMACS_INT f_dos_keyboard_layout;

  EMACS_INT f_dos_hyper_key;

  EMACS_INT f_dos_super_key;

  EMACS_INT f_dos_keypad_mode;

  Lisp_Object f_Vdos_version;

  Lisp_Object f_Vdos_display_scancodes;

  Lisp_Object f_Vdos_windows_version;

  Lisp_Object f_Vbuffer_access_fontify_functions;

  Lisp_Object f_Vbuffer_access_fontified_property;

  /* Non-nil means don't stop at field boundary in text motion commands.  */
  Lisp_Object f_Vinhibit_field_text_motion;

  /* Some static data, and a function to initialize it for each run */
  Lisp_Object f_Vsystem_name;

  Lisp_Object f_Vuser_real_login_name;

  Lisp_Object f_Vuser_full_name;

  Lisp_Object f_Vuser_login_name;

  Lisp_Object f_Voperating_system_release;

  /* Command line args from shell, as list of strings.  */
  Lisp_Object f_Vcommand_line_args;

  /* The name under which Emacs was invoked, with any leading directory
     names discarded.  */
  Lisp_Object f_Vinvocation_name;

  /* The directory name from which Emacs was invoked.  */
  Lisp_Object f_Vinvocation_directory;

  /* The directory name in which to find subdirs such as lisp and etc.
     nil means get them only from PATH_LOADSEARCH.  */
  Lisp_Object f_Vinstallation_directory;

  /* The values of `current-time' before and after Emacs initialization.  */
  Lisp_Object f_Vbefore_init_time;
  Lisp_Object f_Vafter_init_time;

  /* Hook run by `kill-emacs' before it does really anything.  */
  Lisp_Object f_Vkill_emacs_hook;

  /* Search path separator.  */
  Lisp_Object f_Vpath_separator;

  /* Variable whose value is symbol giving operating system type.  */
  Lisp_Object f_Vsystem_type;

  /* Variable whose value is string giving configuration built for.  */
  Lisp_Object f_Vsystem_configuration;

  /* Variable whose value is string giving configuration options,
     for use when reporting bugs.  */
  Lisp_Object f_Vsystem_configuration_options;

  /* Current and previous system locales for messages and time.  */
  Lisp_Object f_Vsystem_messages_locale;

  Lisp_Object f_Vprevious_system_messages_locale;

  Lisp_Object f_Vsystem_time_locale;

  Lisp_Object f_Vprevious_system_time_locale;

  /* Copyright and version info.  The version number may be updated by
     Lisp code.  */
  Lisp_Object f_Vemacs_copyright;
  Lisp_Object f_Vemacs_version;

  /* Alist of external libraries and files implementing them.  */
  Lisp_Object f_Vdynamic_library_alist;

  /* Value of Lisp variable `noninteractive'.
     Normally same as C variable `noninteractive'
     but nothing terrible happens if user sets this one.  */
  int f_noninteractive1;

  /* Nonzero means Emacs was run in --quick mode.  */
  int f_inhibit_x_resources;

  Lisp_Object f_Vinhibit_quit;
  Lisp_Object f_Vquit_flag;

  /* Maximum size allowed for specpdl allocation */
  EMACS_INT f_max_specpdl_size;

  /* Maximum allowed depth in Lisp evaluations and function calls.  */
  EMACS_INT f_max_lisp_eval_depth;

  /* Nonzero means enter debugger before next function call */
  int f_debug_on_next_call;

  /* Non-zero means debugger may continue.  This is zero when the
     debugger is called during redisplay, where it might not be safe to
     continue the interrupted redisplay. */
  int f_debugger_may_continue;

  /* List of conditions (non-nil atom means all) which enter the debugger
     if an error is handled by the command loop's error handler.  */
  Lisp_Object f_Vdebug_on_error;

  /* List of conditions and regexps specifying error messages which
     do not enter the debugger even if Vdebug_on_error says they should.  */
  Lisp_Object f_Vdebug_ignored_errors;

  /* Non-nil means call the debugger even if the error will be handled.  */
  Lisp_Object f_Vdebug_on_signal;

  /* Hook for edebug to use.  */
  Lisp_Object f_Vsignal_hook_function;

  /* Nonzero means enter debugger if a quit signal
     is handled by the command loop's error handler. */
  int f_debug_on_quit;

  Lisp_Object f_Vdebugger;

  /* Function to process declarations in defmacro forms.  */
  Lisp_Object f_Vmacro_declaration_function;

  /* Coding system for file names, or nil if none.  */
  Lisp_Object f_Vfile_name_coding_system;

  /* Coding system for file names used only when
     Vfile_name_coding_system is nil.  */
  Lisp_Object f_Vdefault_file_name_coding_system;

  /* Alist of elements (REGEXP . HANDLER) for file names
     whose I/O is done with a special handler.  */
  Lisp_Object f_Vfile_name_handler_alist;

  /* Function to be called to decide a coding system of a reading file.  */
  Lisp_Object f_Vset_auto_coding_function;

  /* Functions to be called to process text properties in inserted file.  */
  Lisp_Object f_Vafter_insert_file_functions;

  /* Functions to be called to create text property annotations for file.  */
  Lisp_Object f_Vwrite_region_annotate_functions;

  Lisp_Object f_Vwrite_region_post_annotation_function;

  /* During build_annotations, each time an annotation function is called,
     this holds the annotations made by the previous functions.  */
  Lisp_Object f_Vwrite_region_annotations_so_far;

  /* File name in which we write a list of all our auto save files.  */
  Lisp_Object f_Vauto_save_list_file_name;

  /* Whether or not files are auto-saved into themselves.  */
  Lisp_Object f_Vauto_save_visited_file_name;

  /* Whether or not to continue auto-saving after a large deletion.  */
  Lisp_Object f_Vauto_save_include_big_deletions;

  /* Nonzero means skip the call to fsync in Fwrite-region.  */
  int f_write_region_inhibit_fsync;

  /* Non-zero means call move-file-to-trash in Fdelete_file or
     Fdelete_directory_internal.  */
  int f_delete_by_moving_to_trash;

  /* These variables describe handlers that have "already" had a chance
     to handle the current operation.

     Vinhibit_file_name_handlers is a list of file name handlers.
     Vinhibit_file_name_operation is the operation being handled.
     If we try to handle that operation, we ignore those handlers.  */
  Lisp_Object f_Vinhibit_file_name_handlers;

  Lisp_Object f_Vinhibit_file_name_operation;

  /* The directory for writing temporary files.  */
  Lisp_Object f_Vtemporary_file_directory;

  /* Nonzero enables use of dialog boxes for questions
     asked by mouse commands.  */
  int f_use_dialog_box;

  /* Nonzero enables use of a file dialog for file name
     questions asked by mouse commands.  */
  int f_use_file_dialog;

  Lisp_Object f_Vfeatures;

  Lisp_Object f_Vfont_weight_table;
  Lisp_Object f_Vfont_slant_table;
  Lisp_Object f_Vfont_width_table;

  Lisp_Object f_Vfont_encoding_alist;

  Lisp_Object f_Vfont_log;

  Lisp_Object f_Vfont_encoding_charset_alist;

  Lisp_Object f_Vuse_default_ascent;

  Lisp_Object f_Vignore_relative_composition;

  Lisp_Object f_Valternate_fontname_alist;

  Lisp_Object f_Vfontset_alias_alist;

  Lisp_Object f_Vvertical_centering_font_regexp;

  Lisp_Object f_Votf_script_alist;

  /* If we shall make pointer invisible when typing or not.  */
  Lisp_Object f_Vmake_pointer_invisible;

  /* The name we're using in resource queries.  Most often "emacs".  */
  Lisp_Object f_Vx_resource_name;

  /* The application class we're using in resource queries.
     Normally "Emacs".  */
  Lisp_Object f_Vx_resource_class;

  /* Lower limit value of the frame opacity (alpha transparency).  */
  Lisp_Object f_Vframe_alpha_lower_limit;

  Lisp_Object f_Vmenu_bar_mode;
  Lisp_Object f_Vtool_bar_mode;

  Lisp_Object f_Vterminal_frame;

  Lisp_Object f_Vdefault_frame_alist;

  Lisp_Object f_Vdefault_frame_scroll_bars;

  Lisp_Object f_Vmouse_position_function;

  Lisp_Object f_Vmouse_highlight;

  Lisp_Object f_Vdelete_frame_functions;

  int f_focus_follows_mouse;

  /* Non-nil means that newline may flow into the right fringe.  */
  Lisp_Object f_Voverflow_newline_into_fringe;

  /* List of known fringe bitmap symbols.

     The fringe bitmap number is stored in the `fringe' property on
     those symbols.  Names for the built-in bitmaps are installed by
     loading fringe.el.
  */
  Lisp_Object f_Vfringe_bitmaps;

  /* Search path for bitmap files.  */
  Lisp_Object f_Vx_bitmap_file_path;

  /* A list of symbols, one for each supported image type.  */
  Lisp_Object f_Vimage_types;

  /* Time in seconds after which images should be removed from the cache
     if not displayed.  */
  Lisp_Object f_Vimage_cache_eviction_delay;

  Lisp_Object f_Vmax_image_size;

  /* Non-zero means draw a cross on images having `:conversion
     disabled'.  */
  int f_cross_disabled_images;

  Lisp_Object f_Vimagemagick_render_type;

  /* Indentation can insert tabs if this is non-zero;
     otherwise always uses spaces.  */
  int f_indent_tabs_mode;

  /* Non-nil means don't call the after-change-functions right away,
     just record an element in combine_after_change_list.  */
  Lisp_Object f_Vcombine_after_change_calls;

  /* Check all markers in the current buffer, looking for something invalid.  */
  int f_check_markers_debug_flag;

  /* Non-nil if the present key sequence was obtained by shift translation.  */
  Lisp_Object f_Vthis_command_keys_shift_translated;

  /* If non-nil, the function that implements the display of help.
     It's called with one argument, the help string to display.  */
  Lisp_Object f_Vshow_help_function;

  /* Nonzero means do menu prompting.  */
  int f_menu_prompting;

  /* Character to see next line of menu prompt.  */
  Lisp_Object f_menu_prompt_more_char;

  /* Nonzero means disregard local maps for the menu bar.  */
  int f_inhibit_local_menu_bar_menus;

  /* The user's hook function for outputting an error message.  */
  Lisp_Object f_Vcommand_error_function;

  /* The user's ERASE setting.  */
  Lisp_Object f_Vtty_erase_char;

  /* Character to recognize as the help char.  */
  Lisp_Object f_Vhelp_char;

  /* List of other event types to recognize as meaning "help".  */
  Lisp_Object f_Vhelp_event_list;

  /* Form to execute when help char is typed.  */
  Lisp_Object f_Vhelp_form;

  /* Command to run when the help character follows a prefix key.  */
  Lisp_Object f_Vprefix_help_command;

  /* List of items that should move to the end of the menu bar.  */
  Lisp_Object f_Vmenu_bar_final_items;

  /* Expression to evaluate for the tool bar separator image.
     This is used for build_desired_tool_bar_string only.  For GTK, we
     use GTK tool bar seperators.  */
  Lisp_Object f_Vtool_bar_separator_image_expression;

  /* Non-nil means show the equivalent key-binding for
     any M-x command that has one.
     The value can be a length of time to show the message for.
     If the value is non-nil and not a number, we wait 2 seconds.  */
  Lisp_Object f_Vsuggest_key_bindings;

  /* How long to display an echo-area message when the minibuffer is active.
     If the value is not a number, such messages don't time out.  */
  Lisp_Object f_Vminibuffer_message_timeout;

  /* If non-nil, this is a map that overrides all other local maps.  */
  Lisp_Object f_Voverriding_local_map;

  /* If non-nil, Voverriding_local_map applies to the menu bar.  */
  Lisp_Object f_Voverriding_local_map_menu_flag;

  /* Keymap that defines special misc events that should
     be processed immediately at a low level.  */
  Lisp_Object f_Vspecial_event_map;

  /* Total number of times command_loop has read a key sequence.  */
  EMACS_INT f_num_input_keys;

  /* Last input event read as a command.  */
  Lisp_Object f_last_command_event;

  /* Last input character read as a command, not counting menus
     reached by the mouse.  */
  Lisp_Object f_last_nonmenu_event;

  /* Last input event read for any purpose.  */
  Lisp_Object f_last_input_event;

  /* If not Qnil, a list of objects to be read as subsequent command input.  */
  Lisp_Object f_Vunread_command_events;

  /* If not Qnil, a list of objects to be read as subsequent command input
     including input method processing.  */
  Lisp_Object f_Vunread_input_method_events;

  /* If not Qnil, a list of objects to be read as subsequent command input
     but NOT including input method processing.  */
  Lisp_Object f_Vunread_post_input_method_events;

  /* If not -1, an event to be read as subsequent command input.  */
  EMACS_INT f_unread_command_char;

  /* A mask of extra modifier bits to put into every keyboard char.  */
  EMACS_INT f_extra_keyboard_modifiers;

  /* Char to use as prefix when a meta character is typed in.
     This is bound on entry to minibuffer in case ESC is changed there.  */
  Lisp_Object f_meta_prefix_char;

  /* Number of idle seconds before an auto-save and garbage collection.  */
  Lisp_Object f_Vauto_save_timeout;

  /* Total number of times read_char has returned, outside of macros.  */
  EMACS_INT f_num_nonmacro_input_events;

  /* Auto-save automatically when this many characters have been typed
     since the last time.  */
  EMACS_INT f_auto_save_interval;

  /* The command being executed by the command loop.
     Commands may set this, and the value set will be copied into
     current_kboard->Vlast_command instead of the actual command.  */
  Lisp_Object f_Vthis_command;

  /* If the lookup of the command returns a binding, the original
     command is stored in this-original-command.  It is nil otherwise.  */
  Lisp_Object f_Vthis_original_command;

  /* A user-visible version of the above, intended to allow users to
     figure out where the last event came from, if the event doesn't
     carry that information itself (i.e. if it was a character).  */
  Lisp_Object f_Vlast_event_frame;

  /* If non-nil, active regions automatically become the window selection.  */
  Lisp_Object f_Vselect_active_regions;

  /* The text in the active region prior to modifying the buffer.
     Used by the `select-active-regions' feature.  */
  Lisp_Object f_Vsaved_region_selection;

  /* Echo unfinished commands after this many seconds of pause.  */
  Lisp_Object f_Vecho_keystrokes;

  /* Form to evaluate (if non-nil) when Emacs is started.  */
  Lisp_Object f_Vtop_level;

  /* If non-nil, this implements the current input method.  */
  Lisp_Object f_Vinput_method_function;

  /* When we call Vinput_method_function,
     this holds the echo area message that was just erased.  */
  Lisp_Object f_Vinput_method_previous_message;

  /* Non-nil means deactivate the mark at end of this command.  */
  Lisp_Object f_Vdeactivate_mark;

  /* Menu bar specified in Lucid Emacs fashion.  */
  Lisp_Object f_Vlucid_menu_bar_dirty_flag;

  Lisp_Object f_Vpre_command_hook;

  Lisp_Object f_Vpost_command_hook;

  Lisp_Object f_Vcommand_hook_internal;

  /* Parent keymap of terminal-local function-key-map instances.  */
  Lisp_Object f_Vfunction_key_map;

  /* Keymap of key translations that can override keymaps.  */
  Lisp_Object f_Vkey_translation_map;

  /* List of deferred actions to be performed at a later time.
     The precise format isn't relevant here; we just check whether it is nil.  */
  Lisp_Object f_Vdeferred_action_list;

  /* Function to call to handle deferred actions, when there are any.  */
  Lisp_Object f_Vdeferred_action_function;

  /* If this flag is non-nil, we check mouse_moved to see when the
     mouse moves, and motion events will appear in the input stream.
     Otherwise, mouse motion is ignored.  */
  Lisp_Object f_do_mouse_tracking;

  /* List of absolute timers.  Appears in order of next scheduled event.  */
  Lisp_Object f_Vtimer_list;

  /* List of idle time timers.  Appears in order of next scheduled event.  */
  Lisp_Object f_Vtimer_idle_list;

  /* After a command is executed, if point is moved into a region that
     has specific properties (e.g. composition, display), we adjust
     point to the boundary of the region.  But, if a command sets this
     variable to non-nil, we suppress this point adjustment.  This
     variable is set to nil before reading a command.  */
  Lisp_Object f_Vdisable_point_adjustment;

  /* If non-nil, always disable point adjustment.  */
  Lisp_Object f_Vglobal_disable_point_adjustment;

  /* If non-nil, events produced by disabled menu items and tool-bar
     buttons are not ignored.  Help functions bind this to allow help on
     those items and buttons.  */
  Lisp_Object f_Venable_disabled_menus_and_buttons;

  /* Nonzero means don't try to suspend even if the operating system seems
     to support it.  */
  int f_cannot_suspend;

  /* Number of seconds between polling for input.  This is a Lisp
     variable that can be bound.  */
  EMACS_INT f_polling_period;

  /* subprocesses */
  Lisp_Object f_Vthrow_on_input;

  /* The maximum time between clicks to make a double-click, or Qnil to
     disable double-click detection, or Qt for no time limit.  */
  Lisp_Object f_Vdouble_click_time;

  /* Maximum number of pixels the mouse may be moved between clicks
     to make a double-click.  */
  EMACS_INT f_double_click_fuzz;

  /* was MinibufLocalMap */
  Lisp_Object f_Vminibuffer_local_map;

  /* was MinibufLocalNSMap */
  Lisp_Object f_Vminibuffer_local_ns_map;

  /* was MinibufLocalCompletionMap */
  Lisp_Object f_Vminibuffer_local_completion_map;

  /* keymap used for minibuffers when doing completion in filenames */
  Lisp_Object f_Vminibuffer_local_filename_completion_map;

  /* keymap used for minibuffers when doing completion in filenames
     with require-match*/
  Lisp_Object f_Vminibuffer_local_filename_must_match_map;

  /* was MinibufLocalMustMatchMap */
  Lisp_Object f_Vminibuffer_local_must_match_map;

  /* Alist of minor mode variables and keymaps.  */
  Lisp_Object f_Vminor_mode_map_alist;

  /* Alist of major-mode-specific overrides for
     minor mode variables and keymaps.  */
  Lisp_Object f_Vminor_mode_overriding_map_alist;

  /* List of emulation mode keymap alists.  */
  Lisp_Object f_Vemulation_mode_map_alists;

  /* A list of all commands given new bindings since a certain time
     when nil was stored here.
     This is used to speed up recomputation of menu key equivalents
     when Emacs starts up.   t means don't record anything here.  */
  Lisp_Object f_Vdefine_key_rebound_commands;

  Lisp_Object f_Vwhere_is_preferred_modifier;

  Lisp_Object f_Vvalues;
  Lisp_Object f_Vstandard_input;
  Lisp_Object f_Vafter_load_alist;

  Lisp_Object f_Veval_buffer_list;

  /* non-zero if inside `load' */
  int f_load_in_progress;

  /* Directory in which the sources were found.  */
  Lisp_Object f_Vsource_directory;

  /* Search path and suffixes for files to be loaded. */
  Lisp_Object f_Vload_path;
  Lisp_Object f_Vload_suffixes;
  Lisp_Object f_Vload_file_rep_suffixes;

  /* File name of user's init file.  */
  Lisp_Object f_Vuser_init_file;

  /* This is the user-visible association list that maps features to
     lists of defs in their load files. */
  Lisp_Object f_Vload_history;

  /* This is used to build the load history. */
  Lisp_Object f_Vcurrent_load_list;

  /* List of files that were preloaded.  */
  Lisp_Object f_Vpreloaded_file_list;

  /* Name of file actually being read by `load'.  */
  Lisp_Object f_Vload_file_name;

  /* Function to use for reading, in `load' and friends.  */
  Lisp_Object f_Vload_read_function;

  /* Non-nil means read recursive structures using #n= and #n# syntax.  */
  Lisp_Object f_Vread_circle;

  /* Nonzero means load should forcibly load all dynamic doc strings.  */
  int f_load_force_doc_strings;

  /* Nonzero means read should convert strings to unibyte.  */
  int f_load_convert_to_unibyte;

  /* Function to use for loading an Emacs Lisp source file (not
     compiled) instead of readevalloop.  */
  Lisp_Object f_Vload_source_file_function;

  /* List of all DEFVAR_BOOL variables.  Used by the byte optimizer.  */
  Lisp_Object f_Vbyte_boolean_vars;

  /* Whether or not to add a `read-positions' property to symbols
     read. */
  Lisp_Object f_Vread_with_symbol_positions;

  /* List of (SYMBOL . POSITION) accumulated so far. */
  Lisp_Object f_Vread_symbol_positions_list;

  Lisp_Object f_Vold_style_backquotes;

  /* Non-zero means load dangerous compiled Lisp files.  */
  int f_load_dangerous_libraries;

  /* Non-zero means force printing messages when loading Lisp files.  */
  int f_force_load_messages;

  /* A regular expression used to detect files compiled with Emacs.  */
  Lisp_Object f_Vbytecomp_version_regexp;

  Lisp_Object f_Vobarray;

  /* Normal hook run whenever a keyboard macro terminates.  */
  Lisp_Object f_Vkbd_macro_termination_hook;

  /* Kbd macro currently being executed (a string or vector).  */
  Lisp_Object f_Vexecuting_kbd_macro;

  /* Index of next character to fetch from that macro.  */
  EMACS_INT f_executing_kbd_macro_index;

  /* Nonzero means enable debugging checks on byte/char correspondences.  */
  int f_byte_debug_flag;

  Lisp_Object f_Vhistory_length;

  /* No duplicates in history.  */
  int f_history_delete_duplicates;

  /* Non-nil means add new input to history.  */
  Lisp_Object f_Vhistory_add_new_input;

  /* Nonzero means let functions called when within a minibuffer
     invoke recursive minibuffers (to read arguments, or whatever) */
  int f_enable_recursive_minibuffers;

  /* Nonzero means don't ignore text properties
     in Fread_from_minibuffer.  */
  int f_minibuffer_allow_text_properties;

  /* help-form is bound to this while in the minibuffer.  */
  Lisp_Object f_Vminibuffer_help_form;

  /* Variable which is the history list to add minibuffer values to.  */
  Lisp_Object f_Vminibuffer_history_variable;

  /* Current position in the history list (adjusted by M-n and M-p).  */
  Lisp_Object f_Vminibuffer_history_position;

  /* Text properties that are added to minibuffer prompts.
     These are in addition to the basic `field' property, and stickiness
     properties.  */
  Lisp_Object f_Vminibuffer_prompt_properties;

  Lisp_Object f_Vminibuffer_setup_hook;

  Lisp_Object f_Vminibuffer_exit_hook;

  Lisp_Object f_Vread_expression_history;

  /* Function to call to read a buffer name.  */
  Lisp_Object f_Vread_buffer_function;

  /* Nonzero means completion ignores case.  */
  int f_completion_ignore_case;

  int f_read_buffer_completion_ignore_case;

  /* List of regexps that should restrict possible completions.  */
  Lisp_Object f_Vcompletion_regexp_list;

  /* Nonzero means raise the minibuffer frame when the minibuffer
     is entered.  */
  int f_minibuffer_auto_raise;

  /* Keymap for reading expressions.  */
  Lisp_Object f_Vread_expression_map;

  Lisp_Object f_Vminibuffer_completion_table;

  Lisp_Object f_Vminibuffer_completion_predicate;

  Lisp_Object f_Vminibuffer_completion_confirm;

  Lisp_Object f_Vminibuffer_completing_file_name;

  Lisp_Object f_Vdos_unsupported_char_glyph;

  Lisp_Object f_Vstandard_output;

  Lisp_Object f_Vfloat_output_format;

  /* Maximum length of list to print in full; noninteger means
     effectively infinity */
  Lisp_Object f_Vprint_length;

  /* Maximum depth of list to print in full; noninteger means
     effectively infinity.  */
  Lisp_Object f_Vprint_level;

  /* Nonzero means print newlines in strings as \n.  */
  int f_print_escape_newlines;

  /* Nonzero means to print single-byte non-ascii characters in strings as
     octal escapes.  */
  int f_print_escape_nonascii;

  /* Nonzero means to print multibyte characters in strings as hex escapes.  */
  int f_print_escape_multibyte;

  /* Nonzero means print (quote foo) forms as 'foo, etc.  */
  int f_print_quoted;

  /* Non-nil means print #: before uninterned symbols.  */
  Lisp_Object f_Vprint_gensym;

  /* Non-nil means print recursive structures using #n= and #n# syntax.  */
  Lisp_Object f_Vprint_circle;

  /* Non-nil means keep continuous number for #n= and #n# syntax
     between several print functions.  */
  Lisp_Object f_Vprint_continuous_numbering;

  Lisp_Object f_Vprint_number_table;

  /* A flag to control printing of `charset' text property.
     The default value is Qdefault. */
  Lisp_Object f_Vprint_charset_text_property;

  /* Nonzero means delete a process right away if it exits.  */
  int f_delete_exited_processes;

  /* t means use pty, nil means use a pipe,
     maybe other values to come.  */
  Lisp_Object f_Vprocess_connection_type;

  /* Non-nil means to delay reading process output to improve buffering.
     A value of t means that delay is reset after each send, any other
     non-nil value does not reset the delay.  A value of nil disables
     adaptive read buffering completely.  */
  Lisp_Object f_Vprocess_adaptive_read_buffering;

  Lisp_Object f_Vsearch_spaces_regexp;

  /* If non-nil, the match data will not be changed during call to
     searching or matching functions.  This variable is for internal use
     only.  */
  Lisp_Object f_Vinhibit_changing_match_data;

  int f_words_include_escapes;

  int f_parse_sexp_lookup_properties;

  /* Nonzero means `scan-sexps' treat all multibyte characters as symbol.  */
  int f_multibyte_syntax_as_symbol;

  /* Non-zero means an open parenthesis in column 0 is always considered
     to be the start of a defun.  Zero means an open parenthesis in
     column 0 has no special meaning.  */
  int f_open_paren_in_column_0_is_defun_start;

  int f_parse_sexp_ignore_comments;

  /* Char-table of functions that find the next or previous word
     boundary.  */
  Lisp_Object f_Vfind_word_boundary_function_table;

  /* If true, use "vs", otherwise use "ve" to make the cursor visible.  */
  int f_visible_cursor;

  /* Functions to call after suspending a tty. */
  Lisp_Object f_Vsuspend_tty_functions;

  /* Functions to call after resuming a tty. */
  Lisp_Object f_Vresume_tty_functions;

  /* Nonzero means no need to redraw the entire frame on resuming a
     suspended Emacs.  This is useful on terminals with multiple
     pages, where one page is used for Emacs and another for all
     else. */
  int f_no_redraw_on_reenter;

  /* Provided for lisp packages.  */
  int f_system_uses_terminfo;

  /* Function to use to ring the bell.  */
  Lisp_Object f_Vring_bell_function;

  Lisp_Object f_Vdelete_terminal_functions;

  Lisp_Object f_Vinhibit_point_motion_hooks;

  Lisp_Object f_Vdefault_text_properties;

  Lisp_Object f_Vchar_property_alias_alist;

  Lisp_Object f_Vtext_property_default_nonsticky;

  /* Limits controlling how much undo information to keep.  */
  EMACS_INT f_undo_limit;

  EMACS_INT f_undo_strong_limit;

  Lisp_Object f_Vundo_outer_limit;

  /* Function to call when undo_outer_limit is exceeded.  */
  Lisp_Object f_Vundo_outer_limit_function;

  /* Nonzero means do not record point in record_point.  */
  int f_undo_inhibit_record_point;

  /* Coding system for communicating with other Windows programs via the
     clipboard.  */
  Lisp_Object f_Vselection_coding_system;

  /* Coding system for the next communicating with other Windows programs.  */
  Lisp_Object f_Vnext_selection_coding_system;

  /* Determine whether to make frame dimensions match the screen buffer,
     or the current window size.  The former is desirable when running
     over telnet, while the latter is more useful when working directly at
     the console with a large scroll-back buffer.  */
  int f_w32_use_full_screen_buffer;

  /* The colormap for converting color names to RGB values */
  Lisp_Object f_Vw32_color_map;

  /* Non nil if alt key presses are passed on to Windows.  */
  Lisp_Object f_Vw32_pass_alt_to_system;

  /* Non nil if alt key is translated to meta_modifier, nil if it is translated
     to alt_modifier.  */
  Lisp_Object f_Vw32_alt_is_meta;

  /* If non-zero, the windows virtual key code for an alternative quit key. */
  int f_w32_quit_key;

  /* Non nil if left window key events are passed on to Windows (this only
     affects whether "tapping" the key opens the Start menu).  */
  Lisp_Object f_Vw32_pass_lwindow_to_system;

  /* Non nil if right window key events are passed on to Windows (this
     only affects whether "tapping" the key opens the Start menu).  */
  Lisp_Object f_Vw32_pass_rwindow_to_system;

  /* Virtual key code used to generate "phantom" key presses in order
     to stop system from acting on Windows key events.  */
  Lisp_Object f_Vw32_phantom_key_code;

  /* Modifier associated with the left "Windows" key, or nil to act as a
     normal key.  */
  Lisp_Object f_Vw32_lwindow_modifier;

  /* Modifier associated with the right "Windows" key, or nil to act as a
     normal key.  */
  Lisp_Object f_Vw32_rwindow_modifier;

  /* Modifier associated with the "Apps" key, or nil to act as a normal
     key.  */
  Lisp_Object f_Vw32_apps_modifier;

  /* Value is nil if Num Lock acts as a function key.  */
  Lisp_Object f_Vw32_enable_num_lock;

  /* Value is nil if Caps Lock acts as a function key.  */
  Lisp_Object f_Vw32_enable_caps_lock;

  /* Modifier associated with Scroll Lock, or nil to act as a normal key.  */
  Lisp_Object f_Vw32_scroll_lock_modifier;

  /* Switch to control whether we inhibit requests for synthesized bold
     and italic versions of fonts.  */
  int f_w32_enable_synthesized_fonts;

  /* Enable palette management. */
  Lisp_Object f_Vw32_enable_palette;

  /* Control how close left/right button down events must be to
     be converted to a middle button down event. */
  int f_w32_mouse_button_tolerance;

  /* Minimum interval between mouse movement (and scroll bar drag)
     events that are passed on to the event loop. */
  int f_w32_mouse_move_interval;

  /* Flag to indicate if XBUTTON events should be passed on to Windows.  */
  int f_w32_pass_extra_mouse_buttons_to_system;

  /* Flag to indicate if media keys should be passed on to Windows.  */
  int f_w32_pass_multimedia_buttons_to_system;

  /* Non nil if no window manager is in use.  */
  Lisp_Object f_Vx_no_window_manager;

  /* The background and shape of the mouse pointer, and shape when not
     over text or in the modeline.  */
  Lisp_Object f_Vx_pointer_shape;
  Lisp_Object f_Vx_nontext_pointer_shape;
  Lisp_Object f_Vx_mode_pointer_shape;

  /* TODO: Mouse cursor customization.  */
  Lisp_Object f_Vx_hourglass_pointer_shape;
  Lisp_Object f_Vx_window_horizontal_drag_shape;

  /* The shape when over mouse-sensitive text.  */
  Lisp_Object f_Vx_sensitive_text_pointer_shape;

  /* Color of chars displayed in cursor box.  */
  Lisp_Object f_Vx_cursor_fore_pixel;

  /* Regexp matching a font name whose width is the same as `PIXEL_SIZE'.  */
  Lisp_Object f_Vx_pixel_size_width_font_regexp;

  /* Alist of bdf fonts and the files that define them.  */
  Lisp_Object f_Vw32_bdf_filename_alist;

  /* A flag to control whether fonts are matched strictly or not.  */
  int f_w32_strict_fontnames;

  /* A flag to control whether we should only repaint if GetUpdateRect
     indicates there is an update region.  */
  int f_w32_strict_painting;

  /* The ANSI codepage.  */
  int f_w32_ansi_code_page;

  /* Maximum size for tooltips; a cons (COLUMNS . ROWS).  */
  Lisp_Object f_Vx_max_tooltip_size;

  /* Associative list linking character set strings to Windows codepages. */
  Lisp_Object f_Vw32_charset_info_alist;

  /* Control whether spawnve quotes arguments as necessary to ensure
     correct parsing by child process.  Because not all uses of spawnve
     are careful about constructing argv arrays, we make this behavior
     conditional (off by default). */
  Lisp_Object f_Vw32_quote_process_args;

  /* Control whether create_child causes the process' window to be
     hidden.  The default is nil. */
  Lisp_Object f_Vw32_start_process_show_window;

  /* Control whether create_child causes the process to inherit Emacs'
     console window, or be given a new one of its own.  The default is
     nil, to allow multiple DOS programs to run on Win95.  Having separate
     consoles also allows Emacs to cleanly terminate process groups.  */
  Lisp_Object f_Vw32_start_process_share_console;

  /* Control whether create_child cause the process to inherit Emacs'
     error mode setting.  The default is t, to minimize the possibility of
     subprocesses blocking when accessing unmounted drives.  */
  Lisp_Object f_Vw32_start_process_inherit_error_mode;

  /* Time to sleep before reading from a subprocess output pipe - this
     avoids the inefficiency of frequently reading small amounts of data.
     This is primarily necessary for handling DOS processes on Windows 95,
     but is useful for W32 processes on both Windows 95 and NT as well.  */
  int f_w32_pipe_read_delay;

  /* Control conversion of upper case file names to lower case.
     nil means no, t means yes. */
  Lisp_Object f_Vw32_downcase_file_names;

  /* Control whether stat() attempts to generate fake but hopefully
     "accurate" inode values, by hashing the absolute truenames of files.
     This should detect aliasing between long and short names, but still
     allows the possibility of hash collisions.  */
  Lisp_Object f_Vw32_generate_fake_inodes;

  /* Control whether stat() attempts to determine file type and link count
     exactly, at the expense of slower operation.  Since true hard links
     are supported on NTFS volumes, this is only relevant on NT.  */
  Lisp_Object f_Vw32_get_true_file_attributes;

  /* Coding system for communicating with other programs via the
     clipboard.  */

  /* Coding system for the next communication with other programs.  */

  /* Non-nil means Emacs uses toolkit scroll bars.  */
  Lisp_Object f_Vx_toolkit_scroll_bars;

  /* Non-zero means make use of UNDERLINE_POSITION font properties.  */
  int f_x_use_underline_position_properties;

  /* Non-zero means to draw the underline at the same place as the descent line.  */
  int f_x_underline_at_descent_line;

  int f_w32_use_visible_system_caret;

  int f_w32_num_mouse_buttons;

  Lisp_Object f_Vw32_swap_mouse_buttons;

  /* Control whether x_raise_frame also sets input focus.  */
  Lisp_Object f_Vw32_grab_focus_on_raise;

  /* Control whether Caps Lock affects non-ascii characters.  */
  Lisp_Object f_Vw32_capslock_is_shiftlock;

  /* Control whether right-alt and left-ctrl should be recognized as AltGr.  */
  Lisp_Object f_Vw32_recognize_altgr;

  /* Non-nil means it is the window for C-M-v to scroll
     when the mini-buffer is selected.  */
  Lisp_Object f_Vminibuf_scroll_window;

  /* Non-nil means this is the buffer whose window C-M-v should scroll.  */
  Lisp_Object f_Vother_window_scroll_buffer;

  /* Non-nil means it's function to call to display temp buffers.  */
  Lisp_Object f_Vtemp_buffer_show_function;

  /* Non-zero means line and page scrolling on tall lines (with images)
     does partial scrolling by modifying window-vscroll.  */
  int f_auto_window_vscroll_p;

  /* Non-zero means to use mode-line-inactive face in all windows but the
     selected-window and the minibuffer-scroll-window when the
     minibuffer is active.  */
  int f_mode_line_in_non_selected_windows;

  /* If a window gets smaller than either of these, it is removed. */
  EMACS_INT f_window_min_height;

  EMACS_INT f_window_min_width;

  /* Number of lines of continuity in scrolling by screenfuls.  */
  EMACS_INT f_next_screen_context_lines;

  Lisp_Object f_Vwindow_configuration_change_hook;

  /* Non-nil means scroll commands try to put point
     at the same screen height as previously.  */
  Lisp_Object f_Vscroll_preserve_screen_position;

  /* Non-nil means that text is inserted before window's markers.  */
  Lisp_Object f_Vwindow_point_insertion_type;

  /* If non-nil, then the `recenter' command with a nil argument
     the entire frame to be redrawn; the special value `tty' causes the
     frame to be redrawn only if it is a tty frame.  */
  Lisp_Object f_Vrecenter_redisplay;

  Lisp_Object f_Vwindow_scroll_functions;

  Lisp_Object f_Vwindow_text_change_functions;

  Lisp_Object f_Vredisplay_end_trigger_functions;

  /* Functions called to fontify regions of text.  */
  Lisp_Object f_Vfontification_functions;

  /* Non-nil means automatically select any window when the mouse
     cursor moves into it.  */
  Lisp_Object f_Vmouse_autoselect_window;

  Lisp_Object f_Vwrap_prefix;

  Lisp_Object f_Vline_prefix;

  /* Non-zero means draw tool bar buttons raised when the mouse moves
     over them.  */
  int f_auto_raise_tool_bar_buttons_p;

  /* Non-zero means to reposition window if cursor line is only partially visible.  */
  int f_make_cursor_line_fully_visible_p;

  /* Margin below tool bar in pixels.  0 or nil means no margin.
     If value is `internal-border-width' or `border-width',
     the corresponding frame parameter is used.  */
  Lisp_Object f_Vtool_bar_border;

  /* Margin around tool bar buttons in pixels.  */
  Lisp_Object f_Vtool_bar_button_margin;

  /* Thickness of shadow to draw around tool bar buttons.  */
  EMACS_INT f_tool_bar_button_relief;

  /* Non-nil means automatically resize tool-bars so that all tool-bar
     items are visible, and no blank lines remain.

     If value is `grow-only', only make tool-bar bigger.  */
  Lisp_Object f_Vauto_resize_tool_bars;

  /* Type of tool bar.  Can be symbols image, text, both or both-hroiz.  */
  Lisp_Object f_Vtool_bar_style;

  /* Maximum number of characters a label can have to be shown.  */
  EMACS_INT f_tool_bar_max_label_size;

  /* Non-zero means draw block and hollow cursor as wide as the glyph
     under it.  For example, if a block cursor is over a tab, it will be
     drawn as wide as that tab on the display.  */
  int f_x_stretch_cursor_p;

  Lisp_Object f_Vinhibit_redisplay;

  /* Non-zero means Lisp evaluation during redisplay is inhibited.  */
  int f_inhibit_eval_during_redisplay;

  /* Symbols used in text property values.  */
  Lisp_Object f_Vdisplay_pixels_per_inch;

  /* Non-nil means highlight trailing whitespace.  */
  Lisp_Object f_Vshow_trailing_whitespace;

  /* Non-nil means escape non-break space and hyphens.  */
  Lisp_Object f_Vnobreak_char_display;

  /* Non-nil means show the text cursor in void text areas
     i.e. in blank areas after eol and eob.  This used to be
     the default in 21.3.  */
  Lisp_Object f_Vvoid_text_area_pointer;

  /* Nonzero means truncate lines in all windows less wide than the
     frame.  */
  Lisp_Object f_Vtruncate_partial_width_windows;

  /* A flag to control how to display unibyte 8-bit character.  */
  int f_unibyte_display_via_language_environment;

  /* Nonzero means we have more than one non-mini-buffer-only frame.
     Not guaranteed to be accurate except while parsing
     frame-title-format.  */
  int f_multiple_frames;

  Lisp_Object f_Vglobal_mode_string;

  /* List of variables (symbols) which hold markers for overlay arrows.
     The symbols on this list are examined during redisplay to determine
     where to display overlay arrows.  */
  Lisp_Object f_Voverlay_arrow_variable_list;

  /* Marker for where to display an arrow on top of the buffer text.  */
  Lisp_Object f_Voverlay_arrow_position;

  /* String to display for the arrow.  Only used on terminal frames.  */
  Lisp_Object f_Voverlay_arrow_string;

  /* Like mode-line-format, but for the title bar on a visible frame.  */
  Lisp_Object f_Vframe_title_format;

  /* Like mode-line-format, but for the title bar on an iconified frame.  */
  Lisp_Object f_Vicon_title_format;

  /* List of functions to call when a window's size changes.  These
     functions get one arg, a frame on which one or more windows' sizes
     have changed.  */
  Lisp_Object f_Vwindow_size_change_functions;

  Lisp_Object f_Vmenu_bar_update_hook;

  /* Nonzero means highlight the region even in nonselected windows.  */
  int f_highlight_nonselected_windows;

  /* If cursor motion alone moves point off frame, try scrolling this
     many lines up or down if that will bring it back.  */
  EMACS_INT f_emacs_scroll_step;

  /* Nonzero means scroll just far enough to bring point back on the
     screen, when appropriate.  */
  EMACS_INT f_scroll_conservatively;

  /* Recenter the window whenever point gets within this many lines of
     the top or bottom of the window.  This value is translated into a
     pixel value by multiplying it with FRAME_LINE_HEIGHT, which means
     that there is really a fixed pixel height scroll margin.  */
  EMACS_INT f_scroll_margin;

  /* Zero means display the mode-line/header-line/menu-bar in the default face
     (this slightly odd definition is for compatibility with previous versions
     of emacs), non-zero means display them using their respective faces.

     This variable is deprecated.  */
  int f_mode_line_inverse_video;

  /* Maximum buffer size for which to display line numbers.  */
  Lisp_Object f_Vline_number_display_limit;

  /* Line width to consider when repositioning for line number display.  */
  EMACS_INT f_line_number_display_limit_width;

  /* Number of lines to keep in the message log buffer.  t means
     infinite.  nil means don't log at all.  */
  Lisp_Object f_Vmessage_log_max;

  int f_inhibit_menubar_update;

  /* When evaluating expressions from menu bar items (enable conditions,
     for instance), this is the frame they are being processed for.  */
  Lisp_Object f_Vmenu_updating_frame;

  /* Maximum height for resizing mini-windows.  Either a float
     specifying a fraction of the available height, or an integer
     specifying a number of lines.  */
  Lisp_Object f_Vmax_mini_window_height;

  /* Non-zero means messages should be displayed with truncated
     lines instead of being continued.  */
  int f_message_truncate_lines;

  /* How to blink the default frame cursor off.  */
  Lisp_Object f_Vblink_cursor_alist;

  /* Variables to turn off display optimizations from Lisp.  */
  int f_inhibit_try_window_id;
  int f_inhibit_try_window_reusing;

  int f_inhibit_try_cursor_movement;

  /* Non-zero means automatically scroll windows horizontally to make
     point visible.  */
  int f_automatic_hscrolling_p;

  /* How close to the margin can point get before the window is scrolled
     horizontally.  */
  EMACS_INT f_hscroll_margin;

  /* How much to scroll horizontally when point is inside the above margin.  */
  Lisp_Object f_Vhscroll_step;

  /* The variable `resize-mini-windows'.  If nil, don't resize
     mini-windows.  If t, always resize them to fit the text they
     display.  If `grow-only', let mini-windows grow only until they
     become empty.  */
  Lisp_Object f_Vresize_mini_windows;

  /* Space between overline and text. */
  EMACS_INT f_overline_margin;

  /* Require underline to be at least this many screen pixels below baseline
     This to avoid underline "merging" with the base of letters at small
     font sizes, particularly when x_use_underline_position_properties is on. */
  EMACS_INT f_underline_minimum_offset;

  /* Non-zero means don't free realized faces.  Bound while freeing
     realized faces is dangerous because glyph matrices might still
     reference them.  */
  int f_inhibit_free_realized_faces;

  /* Non-zero means we're allowed to display a hourglass pointer.  */
  int f_display_hourglass_p;

  /* Number of seconds to wait before displaying an hourglass cursor.  */
  Lisp_Object f_Vhourglass_delay;

  /* Char-table to control the display of glyphless characters.  */
  Lisp_Object f_Vglyphless_char_display;

  EMACS_INT f_debug_end_pos;

  /* Default stipple pattern used on monochrome displays.  This stipple
     pattern is used on monochrome displays instead of shades of gray
     for a face background color.  See `set-face-stipple' for possible
     values for this variable.  */
  Lisp_Object f_Vface_default_stipple;

  Lisp_Object f_Vscalable_fonts_allowed;

  /* List of regular expressions that matches names of fonts to ignore. */
  Lisp_Object f_Vface_ignored_fonts;

  /* Alist of font name patterns vs the rescaling factor.  */
  Lisp_Object f_Vface_font_rescale_alist;

  /* Maximum number of fonts to consider in font_list.  If not an
     integer > 0, DEFAULT_FONT_LIST_LIMIT is used instead.  */
  Lisp_Object f_Vfont_list_limit;

  /* Alist of global face definitions.  Each element is of the form
     (FACE . LFACE) where FACE is a symbol naming a face and LFACE
     is a Lisp vector of face attributes.  These faces are used
     to initialize faces for new frames.  */
  Lisp_Object f_Vface_new_frame_defaults;

  /* Alist of face remappings.  Each element is of the form:
     (FACE REPLACEMENT...) which causes display of the face FACE to use
     REPLACEMENT... instead.  REPLACEMENT... is interpreted the same way
     the value of a `face' text property is: it may be (1) A face name,
     (2) A list of face names, (3) A property-list of face attribute/value
     pairs, or (4) A list of face names intermixed with lists containing
     face attribute/value pairs.

     Multiple entries in REPLACEMENT... are merged together to form the final
     result, with faces or attributes earlier in the list taking precedence
     over those that are later.

     Face-name remapping cycles are suppressed; recursive references use
     the underlying face instead of the remapped face.  */
  Lisp_Object f_Vface_remapping_alist;

  /* An alist of defined terminal colors and their RGB values.  */
  Lisp_Object f_Vtty_defined_color_alist;

  /* LessTif/Motif version info.  */
  Lisp_Object f_Vmotif_version_string;

  /* GTK+ version info */
  Lisp_Object f_Vgtk_version_string;

  /* Non-zero means prompt with the old GTK file selection dialog.  */
  int f_x_gtk_use_old_file_dialog;

  /* If non-zero, by default show hidden files in the GTK file chooser.  */
  int f_x_gtk_show_hidden_files;

  /* If non-zero, don't show additional help text in the GTK file chooser.  */
  int f_x_gtk_file_dialog_help_text;

  /* If non-zero, don't collapse to tool bar when it is detached.  */
  int f_x_gtk_whole_detached_tool_bar;

  /* If non-zero, use Gtk+ tooltips.  */
  int f_x_gtk_use_system_tooltips;

  /* The background and shape of the mouse pointer, and shape when not
     over text or in the modeline.  */

  /* The shape when over mouse-sensitive text.  */

  /* If non-nil, the pointer shape to indicate that windows can be
     dragged horizontally.  */

  /* Color of chars displayed in cursor box.  */

  /* Non nil if no window manager is in use.  */

  /* Regexp matching a font name whose width is the same as `PIXEL_SIZE'.  */

  /* Maximum size for tooltips; a cons (COLUMNS . ROWS).  */

  Lisp_Object f_Vx_lost_selection_functions;

  Lisp_Object f_Vx_sent_selection_functions;

  /* This is an alist whose CARs are selection-types (whose names are the same
     as the names of X Atoms) and whose CDRs are the names of Lisp functions to
     call to convert the given Emacs selection value to a string representing
     the given selection type.  This is for Lisp-level extension of the emacs
     selection handling.  */
  Lisp_Object f_Vselection_converter_alist;

  /* If the selection owner takes too long to reply to a selection request,
     we give up on it.  This is in milliseconds (0 = no timeout.)  */
  EMACS_INT f_x_selection_timeout;

  int f_use_system_font;

  Lisp_Object f_Vxft_settings;

  /* The client session id for this session as a lisp object.  */
  Lisp_Object f_Vx_session_id;

  /* The id we had the previous session.  This is only available if we
     have been started by the session manager with SMID_OPT.  */
  Lisp_Object f_Vx_session_previous_id;

  /* Non-nil means Emacs uses toolkit scroll bars.  */

  /* Non-zero means make use of UNDERLINE_POSITION font properties.  */

  /* Non-zero means to draw the underline at the same place as the descent line.  */

  /* Non-zero means to not move point as a result of clicking on a
     frame to focus it (when focus-follows-mouse is nil).  */
  int f_x_mouse_click_focus_ignore_position;

  /* The keysyms to use for the various modifiers.  */
  Lisp_Object f_Vx_alt_keysym;
  Lisp_Object f_Vx_hyper_keysym;
  Lisp_Object f_Vx_meta_keysym;
  Lisp_Object f_Vx_super_keysym;

  Lisp_Object f_Vx_keysym_table;

  /* Lisp communications */
  Lisp_Object f_ns_input_file, f_ns_input_font, f_ns_input_fontsize,
    f_ns_input_line;
  Lisp_Object f_ns_input_color, f_ns_input_text, f_ns_working_text;
  Lisp_Object f_ns_input_spi_name, f_ns_input_spi_arg;

  /* Specifies which emacs modifier should be generated when NS receives
     the Alternate modifier.  May be Qnone or any of the modifier lisp symbols.
  */
  Lisp_Object f_ns_alternate_modifier;

  /* Specifies which emacs modifier should be generated when NS receives
     the right Alternate modifier.  Has same values as ns_alternate_modifier
     plus the value Qleft which means whatever value ns_alternate_modifier has.
  */
  Lisp_Object f_ns_right_alternate_modifier;

  /* Specifies which emacs modifier should be generated when NS receives
     the Command modifier.  May be any of the modifier lisp symbols. */
  Lisp_Object f_ns_command_modifier;

  /* Specifies which emacs modifier should be generated when NS receives
     the right Command modifier.  Has same values as ns_command_modifier plus
     the value Qleft which means whatever value ns_command_modifier has.  */
  Lisp_Object f_ns_right_command_modifier;

  /* Specifies which emacs modifier should be generated when NS receives
     the Control modifier.  May be any of the modifier lisp symbols. */
  Lisp_Object f_ns_control_modifier;

  /* Specifies which emacs modifier should be generated when NS receives
     the right Control modifier.  Has same values as ns_control_modifier plus
     the value Qleft which means whatever value ns_control_modifier has.  */
  Lisp_Object f_ns_right_control_modifier;

  /* Specifies which emacs modifier should be generated when NS receives
     the Function modifier (laptops).  May be any of the modifier lisp symbols.
  */
  Lisp_Object f_ns_function_modifier;

  /* Control via default 'GSFontAntiAlias' on OS X and GNUstep. */
  Lisp_Object f_ns_antialias_text;

  /* Confirm on exit. */
  Lisp_Object f_ns_confirm_quit;

  /* Alist of elements (REGEXP . IMAGE) for images of icons associated
     to frames.*/
  Lisp_Object f_Vns_icon_type_alist;

  /* Toolkit version support. */
  Lisp_Object f_Vns_version_string;

  Lisp_Object f_Vns_sent_selection_hooks;
  Lisp_Object f_Vns_lost_selection_hooks;

  /* This is an association list whose elements are of the form
       ( SELECTION-NAME SELECTION-VALUE SELECTION-TIMESTAMP FRAME)
     SELECTION-NAME is a lisp symbol, whose name is the name of an X Atom.
     SELECTION-VALUE is the value that emacs owns for that selection.
       It may be any kind of Lisp object.
     SELECTION-TIMESTAMP is the time at which emacs began owning this
       selection, as a cons of two 16-bit numbers (making a 32 bit time.)
     FRAME is the frame for which we made the selection.
     If there is an entry in this alist, then it can be assumed that Emacs owns
      that selection.
     The only (eq) parts of this list that are visible from Lisp are the
      selection-values.  */
  Lisp_Object f_Vselection_alist;

  Lisp_Object f_Vns_reg_to_script;


};

extern struct emacs_globals globals;

#define Vafter_change_functions \
    globals.f_Vafter_change_functions
#define Vafter_init_time \
    globals.f_Vafter_init_time
#define Vafter_insert_file_functions \
    globals.f_Vafter_insert_file_functions
#define Vafter_load_alist \
    globals.f_Vafter_load_alist
#define Valternate_fontname_alist \
    globals.f_Valternate_fontname_alist
#define Vauto_composition_function \
    globals.f_Vauto_composition_function
#define Vauto_composition_mode \
    globals.f_Vauto_composition_mode
#define Vauto_fill_chars \
    globals.f_Vauto_fill_chars
#define Vauto_resize_tool_bars \
    globals.f_Vauto_resize_tool_bars
#define Vauto_save_include_big_deletions \
    globals.f_Vauto_save_include_big_deletions
#define Vauto_save_list_file_name \
    globals.f_Vauto_save_list_file_name
#define Vauto_save_timeout \
    globals.f_Vauto_save_timeout
#define Vauto_save_visited_file_name \
    globals.f_Vauto_save_visited_file_name
#define Vbefore_change_functions \
    globals.f_Vbefore_change_functions
#define Vbefore_init_time \
    globals.f_Vbefore_init_time
#define Vblink_cursor_alist \
    globals.f_Vblink_cursor_alist
#define Vbuffer_access_fontified_property \
    globals.f_Vbuffer_access_fontified_property
#define Vbuffer_access_fontify_functions \
    globals.f_Vbuffer_access_fontify_functions
#define Vbuild_files \
    globals.f_Vbuild_files
#define Vbyte_boolean_vars \
    globals.f_Vbyte_boolean_vars
#define Vbyte_code_meter \
    globals.f_Vbyte_code_meter
#define Vbytecomp_version_regexp \
    globals.f_Vbytecomp_version_regexp
#define Vchange_major_mode_hook \
    globals.f_Vchange_major_mode_hook
#define Vchar_direction_table \
    globals.f_Vchar_direction_table
#define Vchar_property_alias_alist \
    globals.f_Vchar_property_alias_alist
#define Vchar_script_table \
    globals.f_Vchar_script_table
#define Vchar_width_table \
    globals.f_Vchar_width_table
#define Vcharset_list \
    globals.f_Vcharset_list
#define Vcharset_map_path \
    globals.f_Vcharset_map_path
#define Vcharset_revision_table \
    globals.f_Vcharset_revision_table
#define Vcode_conversion_map_vector \
    globals.f_Vcode_conversion_map_vector
#define Vcoding_category_list \
    globals.f_Vcoding_category_list
#define Vcoding_system_alist \
    globals.f_Vcoding_system_alist
#define Vcoding_system_for_read \
    globals.f_Vcoding_system_for_read
#define Vcoding_system_for_write \
    globals.f_Vcoding_system_for_write
#define Vcoding_system_list \
    globals.f_Vcoding_system_list
#define Vcombine_after_change_calls \
    globals.f_Vcombine_after_change_calls
#define Vcommand_debug_status \
    globals.f_Vcommand_debug_status
#define Vcommand_error_function \
    globals.f_Vcommand_error_function
#define Vcommand_history \
    globals.f_Vcommand_history
#define Vcommand_hook_internal \
    globals.f_Vcommand_hook_internal
#define Vcommand_line_args \
    globals.f_Vcommand_line_args
#define Vcompletion_ignored_extensions \
    globals.f_Vcompletion_ignored_extensions
#define Vcompletion_regexp_list \
    globals.f_Vcompletion_regexp_list
#define Vcompose_chars_after_function \
    globals.f_Vcompose_chars_after_function
#define Vcomposition_function_table \
    globals.f_Vcomposition_function_table
#define Vconfigure_info_directory \
    globals.f_Vconfigure_info_directory
#define Vcurrent_iso639_language \
    globals.f_Vcurrent_iso639_language
#define Vcurrent_load_list \
    globals.f_Vcurrent_load_list
#define Vcurrent_prefix_arg \
    globals.f_Vcurrent_prefix_arg
#define Vdata_directory \
    globals.f_Vdata_directory
#define Vdbus_debug \
    globals.f_Vdbus_debug
#define Vdbus_registered_buses \
    globals.f_Vdbus_registered_buses
#define Vdbus_registered_objects_table \
    globals.f_Vdbus_registered_objects_table
#define Vdeactivate_mark \
    globals.f_Vdeactivate_mark
#define Vdebug_ignored_errors \
    globals.f_Vdebug_ignored_errors
#define Vdebug_on_error \
    globals.f_Vdebug_on_error
#define Vdebug_on_signal \
    globals.f_Vdebug_on_signal
#define Vdebugger \
    globals.f_Vdebugger
#define Vdefault_file_name_coding_system \
    globals.f_Vdefault_file_name_coding_system
#define Vdefault_frame_alist \
    globals.f_Vdefault_frame_alist
#define Vdefault_frame_scroll_bars \
    globals.f_Vdefault_frame_scroll_bars
#define Vdefault_process_coding_system \
    globals.f_Vdefault_process_coding_system
#define Vdefault_text_properties \
    globals.f_Vdefault_text_properties
#define Vdeferred_action_function \
    globals.f_Vdeferred_action_function
#define Vdeferred_action_list \
    globals.f_Vdeferred_action_list
#define Vdefine_key_rebound_commands \
    globals.f_Vdefine_key_rebound_commands
#define Vdelete_frame_functions \
    globals.f_Vdelete_frame_functions
#define Vdelete_terminal_functions \
    globals.f_Vdelete_terminal_functions
#define Vdisable_point_adjustment \
    globals.f_Vdisable_point_adjustment
#define Vdisplay_pixels_per_inch \
    globals.f_Vdisplay_pixels_per_inch
#define Vdoc_directory \
    globals.f_Vdoc_directory
#define Vdoc_file_name \
    globals.f_Vdoc_file_name
#define Vdos_display_scancodes \
    globals.f_Vdos_display_scancodes
#define Vdos_unsupported_char_glyph \
    globals.f_Vdos_unsupported_char_glyph
#define Vdos_version \
    globals.f_Vdos_version
#define Vdos_windows_version \
    globals.f_Vdos_windows_version
#define Vdouble_click_time \
    globals.f_Vdouble_click_time
#define Vdynamic_library_alist \
    globals.f_Vdynamic_library_alist
#define Vecho_keystrokes \
    globals.f_Vecho_keystrokes
#define Vemacs_copyright \
    globals.f_Vemacs_copyright
#define Vemacs_version \
    globals.f_Vemacs_version
#define Vemulation_mode_map_alists \
    globals.f_Vemulation_mode_map_alists
#define Venable_character_translation \
    globals.f_Venable_character_translation
#define Venable_disabled_menus_and_buttons \
    globals.f_Venable_disabled_menus_and_buttons
#define Veval_buffer_list \
    globals.f_Veval_buffer_list
#define Vexec_directory \
    globals.f_Vexec_directory
#define Vexec_path \
    globals.f_Vexec_path
#define Vexec_suffixes \
    globals.f_Vexec_suffixes
#define Vkbd_macro_termination_hook \
    globals.f_Vkbd_macro_termination_hook
#define Vexecuting_kbd_macro \
    globals.f_Vexecuting_kbd_macro
#define Vface_default_stipple \
    globals.f_Vface_default_stipple
#define Vface_font_rescale_alist \
    globals.f_Vface_font_rescale_alist
#define Vface_ignored_fonts \
    globals.f_Vface_ignored_fonts
#define Vface_new_frame_defaults \
    globals.f_Vface_new_frame_defaults
#define Vface_remapping_alist \
    globals.f_Vface_remapping_alist
#define Vfeatures \
    globals.f_Vfeatures
#define Vfile_coding_system_alist \
    globals.f_Vfile_coding_system_alist
#define Vfile_name_coding_system \
    globals.f_Vfile_name_coding_system
#define Vfile_name_handler_alist \
    globals.f_Vfile_name_handler_alist
#define Vfind_word_boundary_function_table \
    globals.f_Vfind_word_boundary_function_table
#define Vfirst_change_hook \
    globals.f_Vfirst_change_hook
#define Vfloat_output_format \
    globals.f_Vfloat_output_format
#define Vfont_ccl_encoder_alist \
    globals.f_Vfont_ccl_encoder_alist
#define Vfont_encoding_alist \
    globals.f_Vfont_encoding_alist
#define Vfont_encoding_charset_alist \
    globals.f_Vfont_encoding_charset_alist
#define Vfont_list_limit \
    globals.f_Vfont_list_limit
#define Vfont_log \
    globals.f_Vfont_log
#define Vfont_slant_table \
    globals.f_Vfont_slant_table
#define Vfont_weight_table \
    globals.f_Vfont_weight_table
#define Vfont_width_table \
    globals.f_Vfont_width_table
#define Vfontification_functions \
    globals.f_Vfontification_functions
#define Vfontset_alias_alist \
    globals.f_Vfontset_alias_alist
#define Vframe_alpha_lower_limit \
    globals.f_Vframe_alpha_lower_limit
#define Vframe_title_format \
    globals.f_Vframe_title_format
#define Vfringe_bitmaps \
    globals.f_Vfringe_bitmaps
#define Vfunction_key_map \
    globals.f_Vfunction_key_map
#define Vgc_cons_percentage \
    globals.f_Vgc_cons_percentage
#define Vgc_elapsed \
    globals.f_Vgc_elapsed
#define Vglobal_disable_point_adjustment \
    globals.f_Vglobal_disable_point_adjustment
#define Vglobal_mode_string \
    globals.f_Vglobal_mode_string
#define Vglyph_table \
    globals.f_Vglyph_table
#define Vglyphless_char_display \
    globals.f_Vglyphless_char_display
#define Vgtk_version_string \
    globals.f_Vgtk_version_string
#define Vhelp_char \
    globals.f_Vhelp_char
#define Vhelp_event_list \
    globals.f_Vhelp_event_list
#define Vhelp_form \
    globals.f_Vhelp_form
#define Vhistory_add_new_input \
    globals.f_Vhistory_add_new_input
#define Vhistory_length \
    globals.f_Vhistory_length
#define Vhourglass_delay \
    globals.f_Vhourglass_delay
#define Vhscroll_step \
    globals.f_Vhscroll_step
#define Vicon_title_format \
    globals.f_Vicon_title_format
#define Vignore_relative_composition \
    globals.f_Vignore_relative_composition
#define Vimage_cache_eviction_delay \
    globals.f_Vimage_cache_eviction_delay
#define Vimage_types \
    globals.f_Vimage_types
#define Vimagemagick_render_type \
    globals.f_Vimagemagick_render_type
#define Vinhibit_changing_match_data \
    globals.f_Vinhibit_changing_match_data
#define Vinhibit_field_text_motion \
    globals.f_Vinhibit_field_text_motion
#define Vinhibit_file_name_handlers \
    globals.f_Vinhibit_file_name_handlers
#define Vinhibit_file_name_operation \
    globals.f_Vinhibit_file_name_operation
#define Vinhibit_point_motion_hooks \
    globals.f_Vinhibit_point_motion_hooks
#define Vinhibit_quit \
    globals.f_Vinhibit_quit
#define Vinhibit_read_only \
    globals.f_Vinhibit_read_only
#define Vinhibit_redisplay \
    globals.f_Vinhibit_redisplay
#define Vinitial_environment \
    globals.f_Vinitial_environment
#define Vinitial_window_system \
    globals.f_Vinitial_window_system
#define Vinput_method_function \
    globals.f_Vinput_method_function
#define Vinput_method_previous_message \
    globals.f_Vinput_method_previous_message
#define Vinstallation_directory \
    globals.f_Vinstallation_directory
#define Vinvocation_directory \
    globals.f_Vinvocation_directory
#define Vinvocation_name \
    globals.f_Vinvocation_name
#define Vkey_translation_map \
    globals.f_Vkey_translation_map
#define Vkill_buffer_query_functions \
    globals.f_Vkill_buffer_query_functions
#define Vkill_emacs_hook \
    globals.f_Vkill_emacs_hook
#define Vlast_code_conversion_error \
    globals.f_Vlast_code_conversion_error
#define Vlast_coding_system_used \
    globals.f_Vlast_coding_system_used
#define Vlast_event_frame \
    globals.f_Vlast_event_frame
#define Vlatin_extra_code_table \
    globals.f_Vlatin_extra_code_table
#define Vline_number_display_limit \
    globals.f_Vline_number_display_limit
#define Vline_prefix \
    globals.f_Vline_prefix
#define Vload_file_name \
    globals.f_Vload_file_name
#define Vload_file_rep_suffixes \
    globals.f_Vload_file_rep_suffixes
#define Vload_history \
    globals.f_Vload_history
#define Vload_path \
    globals.f_Vload_path
#define Vload_read_function \
    globals.f_Vload_read_function
#define Vload_source_file_function \
    globals.f_Vload_source_file_function
#define Vload_suffixes \
    globals.f_Vload_suffixes
#define Vlocale_coding_system \
    globals.f_Vlocale_coding_system
#define Vlucid_menu_bar_dirty_flag \
    globals.f_Vlucid_menu_bar_dirty_flag
#define Vmacro_declaration_function \
    globals.f_Vmacro_declaration_function
#define Vmake_pointer_invisible \
    globals.f_Vmake_pointer_invisible
#define Vmark_even_if_inactive \
    globals.f_Vmark_even_if_inactive
#define Vmax_image_size \
    globals.f_Vmax_image_size
#define Vmax_mini_window_height \
    globals.f_Vmax_mini_window_height
#define Vmemory_full \
    globals.f_Vmemory_full
#define Vmemory_signal_data \
    globals.f_Vmemory_signal_data
#define Vmenu_bar_final_items \
    globals.f_Vmenu_bar_final_items
#define Vmenu_bar_mode \
    globals.f_Vmenu_bar_mode
#define Vmenu_bar_update_hook \
    globals.f_Vmenu_bar_update_hook
#define Vmenu_updating_frame \
    globals.f_Vmenu_updating_frame
#define Vmessage_log_max \
    globals.f_Vmessage_log_max
#define Vminibuf_scroll_window \
    globals.f_Vminibuf_scroll_window
#define Vminibuffer_completing_file_name \
    globals.f_Vminibuffer_completing_file_name
#define Vminibuffer_completion_confirm \
    globals.f_Vminibuffer_completion_confirm
#define Vminibuffer_completion_predicate \
    globals.f_Vminibuffer_completion_predicate
#define Vminibuffer_completion_table \
    globals.f_Vminibuffer_completion_table
#define Vminibuffer_exit_hook \
    globals.f_Vminibuffer_exit_hook
#define Vminibuffer_help_form \
    globals.f_Vminibuffer_help_form
#define Vminibuffer_history_position \
    globals.f_Vminibuffer_history_position
#define Vminibuffer_history_variable \
    globals.f_Vminibuffer_history_variable
#define Vminibuffer_local_completion_map \
    globals.f_Vminibuffer_local_completion_map
#define Vminibuffer_local_filename_completion_map \
    globals.f_Vminibuffer_local_filename_completion_map
#define Vminibuffer_local_filename_must_match_map \
    globals.f_Vminibuffer_local_filename_must_match_map
#define Vminibuffer_local_map \
    globals.f_Vminibuffer_local_map
#define Vminibuffer_local_must_match_map \
    globals.f_Vminibuffer_local_must_match_map
#define Vminibuffer_local_ns_map \
    globals.f_Vminibuffer_local_ns_map
#define Vminibuffer_message_timeout \
    globals.f_Vminibuffer_message_timeout
#define Vminibuffer_prompt_properties \
    globals.f_Vminibuffer_prompt_properties
#define Vminibuffer_setup_hook \
    globals.f_Vminibuffer_setup_hook
#define Vminor_mode_map_alist \
    globals.f_Vminor_mode_map_alist
#define Vminor_mode_overriding_map_alist \
    globals.f_Vminor_mode_overriding_map_alist
#define Vmost_negative_fixnum \
    globals.f_Vmost_negative_fixnum
#define Vmost_positive_fixnum \
    globals.f_Vmost_positive_fixnum
#define Vmotif_version_string \
    globals.f_Vmotif_version_string
#define Vmouse_autoselect_window \
    globals.f_Vmouse_autoselect_window
#define Vmouse_highlight \
    globals.f_Vmouse_highlight
#define Vmouse_leave_buffer_hook \
    globals.f_Vmouse_leave_buffer_hook
#define Vmouse_position_function \
    globals.f_Vmouse_position_function
#define Vnetwork_coding_system_alist \
    globals.f_Vnetwork_coding_system_alist
#define Vnext_selection_coding_system \
    globals.f_Vnext_selection_coding_system
#define Vnobreak_char_display \
    globals.f_Vnobreak_char_display
#define Vobarray \
    globals.f_Vobarray
#define Vold_style_backquotes \
    globals.f_Vold_style_backquotes
#define Voperating_system_release \
    globals.f_Voperating_system_release
#define Votf_script_alist \
    globals.f_Votf_script_alist
#define Vother_window_scroll_buffer \
    globals.f_Vother_window_scroll_buffer
#define Voverflow_newline_into_fringe \
    globals.f_Voverflow_newline_into_fringe
#define Voverlay_arrow_position \
    globals.f_Voverlay_arrow_position
#define Voverlay_arrow_string \
    globals.f_Voverlay_arrow_string
#define Voverlay_arrow_variable_list \
    globals.f_Voverlay_arrow_variable_list
#define Voverriding_local_map \
    globals.f_Voverriding_local_map
#define Voverriding_local_map_menu_flag \
    globals.f_Voverriding_local_map_menu_flag
#define Vpath_separator \
    globals.f_Vpath_separator
#define Vpost_command_hook \
    globals.f_Vpost_command_hook
#define Vpost_gc_hook \
    globals.f_Vpost_gc_hook
#define Vpost_self_insert_hook \
    globals.f_Vpost_self_insert_hook
#define Vpre_command_hook \
    globals.f_Vpre_command_hook
#define Vprefix_help_command \
    globals.f_Vprefix_help_command
#define Vpreloaded_file_list \
    globals.f_Vpreloaded_file_list
#define Vprevious_system_messages_locale \
    globals.f_Vprevious_system_messages_locale
#define Vprevious_system_time_locale \
    globals.f_Vprevious_system_time_locale
#define Vprint_charset_text_property \
    globals.f_Vprint_charset_text_property
#define Vprint_circle \
    globals.f_Vprint_circle
#define Vprint_continuous_numbering \
    globals.f_Vprint_continuous_numbering
#define Vprint_gensym \
    globals.f_Vprint_gensym
#define Vprint_length \
    globals.f_Vprint_length
#define Vprint_level \
    globals.f_Vprint_level
#define Vprint_number_table \
    globals.f_Vprint_number_table
#define Vprintable_chars \
    globals.f_Vprintable_chars
#define Vprocess_adaptive_read_buffering \
    globals.f_Vprocess_adaptive_read_buffering
#define Vprocess_coding_system_alist \
    globals.f_Vprocess_coding_system_alist
#define Vprocess_connection_type \
    globals.f_Vprocess_connection_type
#define Vprocess_environment \
    globals.f_Vprocess_environment
#define Vpurify_flag \
    globals.f_Vpurify_flag
#define Vquit_flag \
    globals.f_Vquit_flag
#define Vread_buffer_function \
    globals.f_Vread_buffer_function
#define Vread_expression_history \
    globals.f_Vread_expression_history
#define Vread_circle \
    globals.f_Vread_circle
#define Vread_expression_map \
    globals.f_Vread_expression_map
#define Vread_symbol_positions_list \
    globals.f_Vread_symbol_positions_list
#define Vread_with_symbol_positions \
    globals.f_Vread_with_symbol_positions
#define Vrecenter_redisplay \
    globals.f_Vrecenter_redisplay
#define Vredisplay_end_trigger_functions \
    globals.f_Vredisplay_end_trigger_functions
#define Vredisplay_preemption_period \
    globals.f_Vredisplay_preemption_period
#define Vresize_mini_windows \
    globals.f_Vresize_mini_windows
#define Vresume_tty_functions \
    globals.f_Vresume_tty_functions
#define Vring_bell_function \
    globals.f_Vring_bell_function
#define Vsaved_region_selection \
    globals.f_Vsaved_region_selection
#define Vscalable_fonts_allowed \
    globals.f_Vscalable_fonts_allowed
#define Vscript_representative_chars \
    globals.f_Vscript_representative_chars
#define Vscroll_preserve_screen_position \
    globals.f_Vscroll_preserve_screen_position
#define Vsearch_spaces_regexp \
    globals.f_Vsearch_spaces_regexp
#define Vselect_active_regions \
    globals.f_Vselect_active_regions
#define Vselect_safe_coding_system_function \
    globals.f_Vselect_safe_coding_system_function
#define Vselection_coding_system \
    globals.f_Vselection_coding_system
#define Vselection_converter_alist \
    globals.f_Vselection_converter_alist
#define Vset_auto_coding_function \
    globals.f_Vset_auto_coding_function
#define Vshared_game_score_directory \
    globals.f_Vshared_game_score_directory
#define Vshell_file_name \
    globals.f_Vshell_file_name
#define Vshow_help_function \
    globals.f_Vshow_help_function
#define Vshow_trailing_whitespace \
    globals.f_Vshow_trailing_whitespace
#define Vsignal_hook_function \
    globals.f_Vsignal_hook_function
#define Vsource_directory \
    globals.f_Vsource_directory
#define Vspecial_event_map \
    globals.f_Vspecial_event_map
#define Vstandard_display_table \
    globals.f_Vstandard_display_table
#define Vstandard_input \
    globals.f_Vstandard_input
#define Vstandard_output \
    globals.f_Vstandard_output
#define Vstandard_translation_table_for_decode \
    globals.f_Vstandard_translation_table_for_decode
#define Vstandard_translation_table_for_encode \
    globals.f_Vstandard_translation_table_for_encode
#define Vsuggest_key_bindings \
    globals.f_Vsuggest_key_bindings
#define Vsuspend_tty_functions \
    globals.f_Vsuspend_tty_functions
#define Vsystem_configuration \
    globals.f_Vsystem_configuration
#define Vsystem_configuration_options \
    globals.f_Vsystem_configuration_options
#define Vsystem_messages_locale \
    globals.f_Vsystem_messages_locale
#define Vsystem_name \
    globals.f_Vsystem_name
#define Vsystem_time_locale \
    globals.f_Vsystem_time_locale
#define Vsystem_type \
    globals.f_Vsystem_type
#define Vtemp_buffer_show_function \
    globals.f_Vtemp_buffer_show_function
#define Vtemporary_file_directory \
    globals.f_Vtemporary_file_directory
#define Vterminal_frame \
    globals.f_Vterminal_frame
#define Vtext_property_default_nonsticky \
    globals.f_Vtext_property_default_nonsticky
#define Vthis_command \
    globals.f_Vthis_command
#define Vthis_command_keys_shift_translated \
    globals.f_Vthis_command_keys_shift_translated
#define Vthis_original_command \
    globals.f_Vthis_original_command
#define Vthrow_on_input \
    globals.f_Vthrow_on_input
#define Vtimer_idle_list \
    globals.f_Vtimer_idle_list
#define Vtimer_list \
    globals.f_Vtimer_list
#define Vtool_bar_border \
    globals.f_Vtool_bar_border
#define Vtool_bar_button_margin \
    globals.f_Vtool_bar_button_margin
#define Vtool_bar_mode \
    globals.f_Vtool_bar_mode
#define Vtool_bar_separator_image_expression \
    globals.f_Vtool_bar_separator_image_expression
#define Vtool_bar_style \
    globals.f_Vtool_bar_style
#define Vtop_level \
    globals.f_Vtop_level
#define Vtransient_mark_mode \
    globals.f_Vtransient_mark_mode
#define Vtranslation_hash_table_vector \
    globals.f_Vtranslation_hash_table_vector
#define Vtranslation_table_for_input \
    globals.f_Vtranslation_table_for_input
#define Vtranslation_table_vector \
    globals.f_Vtranslation_table_vector
#define Vtruncate_partial_width_windows \
    globals.f_Vtruncate_partial_width_windows
#define Vtty_defined_color_alist \
    globals.f_Vtty_defined_color_alist
#define Vtty_erase_char \
    globals.f_Vtty_erase_char
#define Vundo_outer_limit \
    globals.f_Vundo_outer_limit
#define Vundo_outer_limit_function \
    globals.f_Vundo_outer_limit_function
#define Vunicode_category_table \
    globals.f_Vunicode_category_table
#define Vunread_command_events \
    globals.f_Vunread_command_events
#define Vunread_input_method_events \
    globals.f_Vunread_input_method_events
#define Vunread_post_input_method_events \
    globals.f_Vunread_post_input_method_events
#define Vuse_default_ascent \
    globals.f_Vuse_default_ascent
#define Vuser_full_name \
    globals.f_Vuser_full_name
#define Vuser_init_file \
    globals.f_Vuser_init_file
#define Vuser_login_name \
    globals.f_Vuser_login_name
#define Vuser_real_login_name \
    globals.f_Vuser_real_login_name
#define Vvalues \
    globals.f_Vvalues
#define Vvertical_centering_font_regexp \
    globals.f_Vvertical_centering_font_regexp
#define Vvoid_text_area_pointer \
    globals.f_Vvoid_text_area_pointer
#define Vw32_alt_is_meta \
    globals.f_Vw32_alt_is_meta
#define Vw32_apps_modifier \
    globals.f_Vw32_apps_modifier
#define Vw32_bdf_filename_alist \
    globals.f_Vw32_bdf_filename_alist
#define Vw32_capslock_is_shiftlock \
    globals.f_Vw32_capslock_is_shiftlock
#define Vw32_charset_info_alist \
    globals.f_Vw32_charset_info_alist
#define Vw32_color_map \
    globals.f_Vw32_color_map
#define Vw32_downcase_file_names \
    globals.f_Vw32_downcase_file_names
#define Vw32_enable_caps_lock \
    globals.f_Vw32_enable_caps_lock
#define Vw32_enable_num_lock \
    globals.f_Vw32_enable_num_lock
#define Vw32_enable_palette \
    globals.f_Vw32_enable_palette
#define Vw32_generate_fake_inodes \
    globals.f_Vw32_generate_fake_inodes
#define Vw32_get_true_file_attributes \
    globals.f_Vw32_get_true_file_attributes
#define Vw32_grab_focus_on_raise \
    globals.f_Vw32_grab_focus_on_raise
#define Vw32_lwindow_modifier \
    globals.f_Vw32_lwindow_modifier
#define Vw32_pass_alt_to_system \
    globals.f_Vw32_pass_alt_to_system
#define Vw32_pass_lwindow_to_system \
    globals.f_Vw32_pass_lwindow_to_system
#define Vw32_pass_rwindow_to_system \
    globals.f_Vw32_pass_rwindow_to_system
#define Vw32_phantom_key_code \
    globals.f_Vw32_phantom_key_code
#define Vw32_quote_process_args \
    globals.f_Vw32_quote_process_args
#define Vw32_recognize_altgr \
    globals.f_Vw32_recognize_altgr
#define Vw32_rwindow_modifier \
    globals.f_Vw32_rwindow_modifier
#define Vw32_scroll_lock_modifier \
    globals.f_Vw32_scroll_lock_modifier
#define Vw32_start_process_inherit_error_mode \
    globals.f_Vw32_start_process_inherit_error_mode
#define Vw32_start_process_share_console \
    globals.f_Vw32_start_process_share_console
#define Vw32_start_process_show_window \
    globals.f_Vw32_start_process_show_window
#define Vw32_swap_mouse_buttons \
    globals.f_Vw32_swap_mouse_buttons
#define Vwhere_is_preferred_modifier \
    globals.f_Vwhere_is_preferred_modifier
#define Vwindow_configuration_change_hook \
    globals.f_Vwindow_configuration_change_hook
#define Vwindow_point_insertion_type \
    globals.f_Vwindow_point_insertion_type
#define Vwindow_scroll_functions \
    globals.f_Vwindow_scroll_functions
#define Vwindow_size_change_functions \
    globals.f_Vwindow_size_change_functions
#define Vwindow_system_version \
    globals.f_Vwindow_system_version
#define Vwindow_text_change_functions \
    globals.f_Vwindow_text_change_functions
#define Vword_combining_categories \
    globals.f_Vword_combining_categories
#define Vword_separating_categories \
    globals.f_Vword_separating_categories
#define Vwrap_prefix \
    globals.f_Vwrap_prefix
#define Vwrite_region_annotate_functions \
    globals.f_Vwrite_region_annotate_functions
#define Vwrite_region_annotations_so_far \
    globals.f_Vwrite_region_annotations_so_far
#define Vwrite_region_post_annotation_function \
    globals.f_Vwrite_region_post_annotation_function
#define Vx_alt_keysym \
    globals.f_Vx_alt_keysym
#define Vx_bitmap_file_path \
    globals.f_Vx_bitmap_file_path
#define Vx_cursor_fore_pixel \
    globals.f_Vx_cursor_fore_pixel
#define Vx_hourglass_pointer_shape \
    globals.f_Vx_hourglass_pointer_shape
#define Vx_hyper_keysym \
    globals.f_Vx_hyper_keysym
#define Vx_keysym_table \
    globals.f_Vx_keysym_table
#define Vx_lost_selection_functions \
    globals.f_Vx_lost_selection_functions
#define Vx_max_tooltip_size \
    globals.f_Vx_max_tooltip_size
#define Vx_meta_keysym \
    globals.f_Vx_meta_keysym
#define Vx_mode_pointer_shape \
    globals.f_Vx_mode_pointer_shape
#define Vx_no_window_manager \
    globals.f_Vx_no_window_manager
#define Vx_nontext_pointer_shape \
    globals.f_Vx_nontext_pointer_shape
#define Vx_pixel_size_width_font_regexp \
    globals.f_Vx_pixel_size_width_font_regexp
#define Vx_pointer_shape \
    globals.f_Vx_pointer_shape
#define Vx_resource_class \
    globals.f_Vx_resource_class
#define Vx_resource_name \
    globals.f_Vx_resource_name
#define Vx_sensitive_text_pointer_shape \
    globals.f_Vx_sensitive_text_pointer_shape
#define Vx_sent_selection_functions \
    globals.f_Vx_sent_selection_functions
#define Vx_session_id \
    globals.f_Vx_session_id
#define Vx_session_previous_id \
    globals.f_Vx_session_previous_id
#define Vx_super_keysym \
    globals.f_Vx_super_keysym
#define Vx_toolkit_scroll_bars \
    globals.f_Vx_toolkit_scroll_bars
#define Vx_window_horizontal_drag_shape \
    globals.f_Vx_window_horizontal_drag_shape
#define Vxft_settings \
    globals.f_Vxft_settings
#define auto_raise_tool_bar_buttons_p \
    globals.f_auto_raise_tool_bar_buttons_p
#define auto_save_interval \
    globals.f_auto_save_interval
#define auto_window_vscroll_p \
    globals.f_auto_window_vscroll_p
#define automatic_hscrolling_p \
    globals.f_automatic_hscrolling_p
#define baud_rate \
    globals.f_baud_rate
#define byte_debug_flag \
    globals.f_byte_debug_flag
#define byte_metering_on \
    globals.f_byte_metering_on
#define cannot_suspend \
    globals.f_cannot_suspend
#define check_markers_debug_flag \
    globals.f_check_markers_debug_flag
#define coding_system_require_warning \
    globals.f_coding_system_require_warning
#define completion_ignore_case \
    globals.f_completion_ignore_case
#define cons_cells_consed \
    globals.f_cons_cells_consed
#define cross_disabled_images \
    globals.f_cross_disabled_images
#define cursor_in_echo_area \
    globals.f_cursor_in_echo_area
#define debug_end_pos \
    globals.f_debug_end_pos
#define debug_on_next_call \
    globals.f_debug_on_next_call
#define debug_on_quit \
    globals.f_debug_on_quit
#define debugger_may_continue \
    globals.f_debugger_may_continue
#define delete_by_moving_to_trash \
    globals.f_delete_by_moving_to_trash
#define delete_exited_processes \
    globals.f_delete_exited_processes
#define display_hourglass_p \
    globals.f_display_hourglass_p
#define do_mouse_tracking \
    globals.f_do_mouse_tracking
#define dos_codepage \
    globals.f_dos_codepage
#define dos_country_code \
    globals.f_dos_country_code
#define dos_decimal_point \
    globals.f_dos_decimal_point
#define dos_hyper_key \
    globals.f_dos_hyper_key
#define dos_keyboard_layout \
    globals.f_dos_keyboard_layout
#define dos_keypad_mode \
    globals.f_dos_keypad_mode
#define dos_super_key \
    globals.f_dos_super_key
#define dos_timezone_offset \
    globals.f_dos_timezone_offset
#define double_click_fuzz \
    globals.f_double_click_fuzz
#define emacs_scroll_step \
    globals.f_emacs_scroll_step
#define enable_recursive_minibuffers \
    globals.f_enable_recursive_minibuffers
#define eol_mnemonic_dos \
    globals.f_eol_mnemonic_dos
#define eol_mnemonic_mac \
    globals.f_eol_mnemonic_mac
#define eol_mnemonic_undecided \
    globals.f_eol_mnemonic_undecided
#define eol_mnemonic_unix \
    globals.f_eol_mnemonic_unix
#define executing_kbd_macro_index \
    globals.f_executing_kbd_macro_index
#define extra_keyboard_modifiers \
    globals.f_extra_keyboard_modifiers
#define floats_consed \
    globals.f_floats_consed
#define focus_follows_mouse \
    globals.f_focus_follows_mouse
#define force_load_messages \
    globals.f_force_load_messages
#define garbage_collection_messages \
    globals.f_garbage_collection_messages
#define gc_cons_threshold \
    globals.f_gc_cons_threshold
#define gcs_done \
    globals.f_gcs_done
#define highlight_nonselected_windows \
    globals.f_highlight_nonselected_windows
#define history_delete_duplicates \
    globals.f_history_delete_duplicates
#define hscroll_margin \
    globals.f_hscroll_margin
#define indent_tabs_mode \
    globals.f_indent_tabs_mode
#define inherit_process_coding_system \
    globals.f_inherit_process_coding_system
#define inhibit_eol_conversion \
    globals.f_inhibit_eol_conversion
#define inhibit_eval_during_redisplay \
    globals.f_inhibit_eval_during_redisplay
#define inhibit_free_realized_faces \
    globals.f_inhibit_free_realized_faces
#define inhibit_iso_escape_detection \
    globals.f_inhibit_iso_escape_detection
#define inhibit_load_charset_map \
    globals.f_inhibit_load_charset_map
#define inhibit_local_menu_bar_menus \
    globals.f_inhibit_local_menu_bar_menus
#define inhibit_menubar_update \
    globals.f_inhibit_menubar_update
#define inhibit_modification_hooks \
    globals.f_inhibit_modification_hooks
#define inhibit_null_byte_detection \
    globals.f_inhibit_null_byte_detection
#define inhibit_try_cursor_movement \
    globals.f_inhibit_try_cursor_movement
#define inhibit_try_window_id \
    globals.f_inhibit_try_window_id
#define inhibit_try_window_reusing \
    globals.f_inhibit_try_window_reusing
#define inhibit_x_resources \
    globals.f_inhibit_x_resources
#define intervals_consed \
    globals.f_intervals_consed
#define inverse_video \
    globals.f_inverse_video
#define last_command_event \
    globals.f_last_command_event
#define last_input_event \
    globals.f_last_input_event
#define last_nonmenu_event \
    globals.f_last_nonmenu_event
#define line_number_display_limit_width \
    globals.f_line_number_display_limit_width
#define load_convert_to_unibyte \
    globals.f_load_convert_to_unibyte
#define load_dangerous_libraries \
    globals.f_load_dangerous_libraries
#define load_force_doc_strings \
    globals.f_load_force_doc_strings
#define load_in_progress \
    globals.f_load_in_progress
#define make_cursor_line_fully_visible_p \
    globals.f_make_cursor_line_fully_visible_p
#define max_lisp_eval_depth \
    globals.f_max_lisp_eval_depth
#define max_specpdl_size \
    globals.f_max_specpdl_size
#define menu_prompt_more_char \
    globals.f_menu_prompt_more_char
#define menu_prompting \
    globals.f_menu_prompting
#define message_truncate_lines \
    globals.f_message_truncate_lines
#define meta_prefix_char \
    globals.f_meta_prefix_char
#define minibuffer_allow_text_properties \
    globals.f_minibuffer_allow_text_properties
#define minibuffer_auto_raise \
    globals.f_minibuffer_auto_raise
#define misc_objects_consed \
    globals.f_misc_objects_consed
#define mode_line_in_non_selected_windows \
    globals.f_mode_line_in_non_selected_windows
#define mode_line_inverse_video \
    globals.f_mode_line_inverse_video
#define multibyte_syntax_as_symbol \
    globals.f_multibyte_syntax_as_symbol
#define multiple_frames \
    globals.f_multiple_frames
#define next_screen_context_lines \
    globals.f_next_screen_context_lines
#define no_redraw_on_reenter \
    globals.f_no_redraw_on_reenter
#define noninteractive1 \
    globals.f_noninteractive1
#define num_input_keys \
    globals.f_num_input_keys
#define num_nonmacro_input_events \
    globals.f_num_nonmacro_input_events
#define open_paren_in_column_0_is_defun_start \
    globals.f_open_paren_in_column_0_is_defun_start
#define overline_margin \
    globals.f_overline_margin
#define parse_sexp_ignore_comments \
    globals.f_parse_sexp_ignore_comments
#define parse_sexp_lookup_properties \
    globals.f_parse_sexp_lookup_properties
#define polling_period \
    globals.f_polling_period
#define print_escape_multibyte \
    globals.f_print_escape_multibyte
#define print_escape_newlines \
    globals.f_print_escape_newlines
#define print_escape_nonascii \
    globals.f_print_escape_nonascii
#define print_quoted \
    globals.f_print_quoted
#define pure_bytes_used \
    globals.f_pure_bytes_used
#define read_buffer_completion_ignore_case \
    globals.f_read_buffer_completion_ignore_case
#define redisplay_dont_pause \
    globals.f_redisplay_dont_pause
#define scroll_conservatively \
    globals.f_scroll_conservatively
#define scroll_margin \
    globals.f_scroll_margin
#define string_chars_consed \
    globals.f_string_chars_consed
#define strings_consed \
    globals.f_strings_consed
#define symbols_consed \
    globals.f_symbols_consed
#define system_uses_terminfo \
    globals.f_system_uses_terminfo
#define tool_bar_button_relief \
    globals.f_tool_bar_button_relief
#define tool_bar_max_label_size \
    globals.f_tool_bar_max_label_size
#define underline_minimum_offset \
    globals.f_underline_minimum_offset
#define undo_inhibit_record_point \
    globals.f_undo_inhibit_record_point
#define undo_limit \
    globals.f_undo_limit
#define undo_strong_limit \
    globals.f_undo_strong_limit
#define unibyte_display_via_language_environment \
    globals.f_unibyte_display_via_language_environment
#define unread_command_char \
    globals.f_unread_command_char
#define use_dialog_box \
    globals.f_use_dialog_box
#define use_file_dialog \
    globals.f_use_file_dialog
#define use_system_font \
    globals.f_use_system_font
#define vector_cells_consed \
    globals.f_vector_cells_consed
#define visible_bell \
    globals.f_visible_bell
#define visible_cursor \
    globals.f_visible_cursor
#define w32_ansi_code_page \
    globals.f_w32_ansi_code_page
#define w32_enable_synthesized_fonts \
    globals.f_w32_enable_synthesized_fonts
#define w32_mouse_button_tolerance \
    globals.f_w32_mouse_button_tolerance
#define w32_mouse_move_interval \
    globals.f_w32_mouse_move_interval
#define w32_num_mouse_buttons \
    globals.f_w32_num_mouse_buttons
#define w32_pass_extra_mouse_buttons_to_system \
    globals.f_w32_pass_extra_mouse_buttons_to_system
#define w32_pass_multimedia_buttons_to_system \
    globals.f_w32_pass_multimedia_buttons_to_system
#define w32_pipe_read_delay \
    globals.f_w32_pipe_read_delay
#define w32_quit_key \
    globals.f_w32_quit_key
#define w32_strict_fontnames \
    globals.f_w32_strict_fontnames
#define w32_strict_painting \
    globals.f_w32_strict_painting
#define w32_use_full_screen_buffer \
    globals.f_w32_use_full_screen_buffer
#define w32_use_visible_system_caret \
    globals.f_w32_use_visible_system_caret
#define window_min_height \
    globals.f_window_min_height
#define window_min_width \
    globals.f_window_min_width
#define words_include_escapes \
    globals.f_words_include_escapes
#define write_region_inhibit_fsync \
    globals.f_write_region_inhibit_fsync
#define x_gtk_file_dialog_help_text \
    globals.f_x_gtk_file_dialog_help_text
#define x_gtk_show_hidden_files \
    globals.f_x_gtk_show_hidden_files
#define x_gtk_use_old_file_dialog \
    globals.f_x_gtk_use_old_file_dialog
#define x_gtk_use_system_tooltips \
    globals.f_x_gtk_use_system_tooltips
#define x_gtk_whole_detached_tool_bar \
    globals.f_x_gtk_whole_detached_tool_bar
#define x_mouse_click_focus_ignore_position \
    globals.f_x_mouse_click_focus_ignore_position
#define x_selection_timeout \
    globals.f_x_selection_timeout
#define x_stretch_cursor_p \
    globals.f_x_stretch_cursor_p
#define x_underline_at_descent_line \
    globals.f_x_underline_at_descent_line
#define x_use_underline_position_properties \
    globals.f_x_use_underline_position_properties
#define ns_input_file \
    globals.f_ns_input_file
#define ns_input_font \
    globals.f_ns_input_font
#define ns_input_fontsize \
    globals.f_ns_input_fontsize
#define ns_input_line \
    globals.f_ns_input_line
#define ns_input_color \
    globals.f_ns_input_color
#define ns_input_text \
    globals.f_ns_input_text
#define ns_working_text \
    globals.f_ns_working_text
#define ns_input_spi_name \
    globals.f_ns_input_spi_name
#define ns_input_spi_arg \
    globals.f_ns_input_spi_arg
#define ns_alternate_modifier \
    globals.f_ns_alternate_modifier
#define ns_right_alternate_modifier \
    globals.f_ns_right_alternate_modifier
#define ns_command_modifier \
    globals.f_ns_command_modifier
#define ns_right_command_modifier \
    globals.f_ns_right_command_modifier
#define ns_control_modifier \
    globals.f_ns_control_modifier
#define ns_right_control_modifier \
    globals.f_ns_right_control_modifier
#define ns_function_modifier \
    globals.f_ns_function_modifier
#define ns_antialias_text \
    globals.f_ns_antialias_text
#define ns_confirm_quit \
    globals.f_ns_confirm_quit
#define Vns_icon_type_alist \
    globals.f_Vns_icon_type_alist
#define Vns_version_string \
    globals.f_Vns_version_string
#define Vns_sent_selection_hooks \
    globals.f_Vns_sent_selection_hooks
#define Vns_lost_selection_hooks \
    globals.f_Vns_lost_selection_hooks
#define Vselection_alist \
    globals.f_Vselection_alist
#define Vns_reg_to_script \
    globals.f_Vns_reg_to_script
