// Change direct access to Lisp_Object fields of struct frame to FVAR.
@@
expression F;
@@
(
- F->icon_name
+ FVAR (F, icon_name)
|
- F->title
+ FVAR (F, title)
|
- F->focus_frame
+ FVAR (F, focus_frame)
|
- F->root_window
+ FVAR (F, root_window)
|
- F->selected_window
+ FVAR (F, selected_window)
|
- F->minibuffer_window
+ FVAR (F, minibuffer_window)
|
- F->param_alist
+ FVAR (F, param_alist)
|
- F->scroll_bars
+ FVAR (F, scroll_bars)
|
- F->condemned_scroll_bars
+ FVAR (F, condemned_scroll_bars)
|
- F->menu_bar_items
+ FVAR (F, menu_bar_items)
|
- F->face_alist
+ FVAR (F, face_alist)
|
- F->menu_bar_vector
+ FVAR (F, menu_bar_vector)
|
- F->buffer_predicate
+ FVAR (F, buffer_predicate)
|
- F->buffer_list
+ FVAR (F, buffer_list)
|
- F->buried_buffer_list
+ FVAR (F, buried_buffer_list)
|
- F->menu_bar_window
+ FVAR (F, menu_bar_window)
|
- F->tool_bar_window
+ FVAR (F, tool_bar_window)
|
- F->tool_bar_items
+ FVAR (F, tool_bar_items)
|
- F->tool_bar_position
+ FVAR (F, tool_bar_position)
|
- F->desired_tool_bar_string
+ FVAR (F, desired_tool_bar_string)
|
- F->current_tool_bar_string
+ FVAR (F, current_tool_bar_string)

|

- XFRAME (F)->icon_name
+ FVAR (XFRAME (F), icon_name)
|
- XFRAME (F)->title
+ FVAR (XFRAME (F), title)
|
- XFRAME (F)->focus_frame
+ FVAR (XFRAME (F), focus_frame)
|
- XFRAME (F)->root_window
+ FVAR (XFRAME (F), root_window)
|
- XFRAME (F)->selected_window
+ FVAR (XFRAME (F), selected_window)
|
- XFRAME (F)->minibuffer_window
+ FVAR (XFRAME (F), minibuffer_window)
|
- XFRAME (F)->param_alist
+ FVAR (XFRAME (F), param_alist)
|
- XFRAME (F)->scroll_bars
+ FVAR (XFRAME (F), scroll_bars)
|
- XFRAME (F)->condemned_scroll_bars
+ FVAR (XFRAME (F), condemned_scroll_bars)
|
- XFRAME (F)->menu_bar_items
+ FVAR (XFRAME (F), menu_bar_items)
|
- XFRAME (F)->face_alist
+ FVAR (XFRAME (F), face_alist)
|
- XFRAME (F)->menu_bar_vector
+ FVAR (XFRAME (F), menu_bar_vector)
|
- XFRAME (F)->buffer_predicate
+ FVAR (XFRAME (F), buffer_predicate)
|
- XFRAME (F)->buffer_list
+ FVAR (XFRAME (F), buffer_list)
|
- XFRAME (F)->buried_buffer_list
+ FVAR (XFRAME (F), buried_buffer_list)
|
- XFRAME (F)->menu_bar_window
+ FVAR (XFRAME (F), menu_bar_window)
|
- XFRAME (F)->tool_bar_window
+ FVAR (XFRAME (F), tool_bar_window)
|
- XFRAME (F)->tool_bar_items
+ FVAR (XFRAME (F), tool_bar_items)
|
- XFRAME (F)->tool_bar_position
+ FVAR (XFRAME (F), tool_bar_position)
|
- XFRAME (F)->desired_tool_bar_string
+ FVAR (XFRAME (F), desired_tool_bar_string)
|
- XFRAME (F)->current_tool_bar_string
+ FVAR (XFRAME (F), current_tool_bar_string)
)
