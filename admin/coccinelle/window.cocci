// Change direct access to Lisp_Object fields of struct window to WVAR.
@@
struct window *W;
Lisp_Object O;
@@
(
- W->frame
+ WVAR (W, frame)
|
- W->next
+ WVAR (W, next)
|
- W->prev
+ WVAR (W, prev)
|
- W->hchild
+ WVAR (W, hchild)
|
- W->vchild
+ WVAR (W, vchild)
|
- W->parent
+ WVAR (W, parent)
|
- W->left_col
+ WVAR (W, left_col)
|
- W->top_line
+ WVAR (W, top_line)
|
- W->total_lines
+ WVAR (W, total_lines)
|
- W->total_cols
+ WVAR (W, total_cols)
|
- W->normal_lines
+ WVAR (W, normal_lines)
|
- W->normal_cols
+ WVAR (W, normal_cols)
|
- W->new_total
+ WVAR (W, new_total)
|
- W->new_normal
+ WVAR (W, new_normal)
|
- W->buffer
+ WVAR (W, buffer)
|
- W->start
+ WVAR (W, start)
|
- W->pointm
+ WVAR (W, pointm)
|
- W->temslot
+ WVAR (W, temslot)
|
- W->vertical_scroll_bar
+ WVAR (W, vertical_scroll_bar)
|
- W->left_margin_cols
+ WVAR (W, left_margin_cols)
|
- W->right_margin_cols
+ WVAR (W, right_margin_cols)
|
- W->left_fringe_width
+ WVAR (W, left_fringe_width)
|
- W->right_fringe_width
+ WVAR (W, right_fringe_width)
|
- W->scroll_bar_width
+ WVAR (W, scroll_bar_width)
|
- W->vertical_scroll_bar_type
+ WVAR (W, vertical_scroll_bar_type)
|
- W->window_end_pos
+ WVAR (W, window_end_pos)
|
- W->window_end_vpos
+ WVAR (W, window_end_vpos)
|
- W->window_end_valid
+ WVAR (W, window_end_valid)
|
- W->display_table
+ WVAR (W, display_table)
|
- W->dedicated
+ WVAR (W, dedicated)
|
- W->base_line_number
+ WVAR (W, base_line_number)
|
- W->base_line_pos
+ WVAR (W, base_line_pos)
|
- W->region_showing
+ WVAR (W, region_showing)
|
- W->column_number_displayed
+ WVAR (W, column_number_displayed)
|
- W->redisplay_end_trigger
+ WVAR (W, redisplay_end_trigger)
|
- W->combination_limit
+ WVAR (W, combination_limit)
|
- W->prev_buffers
+ WVAR (W, prev_buffers)
|
- W->next_buffers
+ WVAR (W, next_buffers)
|
- W->window_parameters
+ WVAR (W, window_parameters)

|

- XWINDOW (O)->frame
+ WVAR (XWINDOW (O), frame)
|
- XWINDOW (O)->next
+ WVAR (XWINDOW (O), next)
|
- XWINDOW (O)->prev
+ WVAR (XWINDOW (O), prev)
|
- XWINDOW (O)->hchild
+ WVAR (XWINDOW (O), hchild)
|
- XWINDOW (O)->vchild
+ WVAR (XWINDOW (O), vchild)
|
- XWINDOW (O)->parent
+ WVAR (XWINDOW (O), parent)
|
- XWINDOW (O)->left_col
+ WVAR (XWINDOW (O), left_col)
|
- XWINDOW (O)->top_line
+ WVAR (XWINDOW (O), top_line)
|
- XWINDOW (O)->total_lines
+ WVAR (XWINDOW (O), total_lines)
|
- XWINDOW (O)->total_cols
+ WVAR (XWINDOW (O), total_cols)
|
- XWINDOW (O)->normal_lines
+ WVAR (XWINDOW (O), normal_lines)
|
- XWINDOW (O)->normal_cols
+ WVAR (XWINDOW (O), normal_cols)
|
- XWINDOW (O)->new_total
+ WVAR (XWINDOW (O), new_total)
|
- XWINDOW (O)->new_normal
+ WVAR (XWINDOW (O), new_normal)
|
- XWINDOW (O)->buffer
+ WVAR (XWINDOW (O), buffer)
|
- XWINDOW (O)->start
+ WVAR (XWINDOW (O), start)
|
- XWINDOW (O)->pointm
+ WVAR (XWINDOW (O), pointm)
|
- XWINDOW (O)->temslot
+ WVAR (XWINDOW (O), temslot)
|
- XWINDOW (O)->vertical_scroll_bar
+ WVAR (XWINDOW (O), vertical_scroll_bar)
|
- XWINDOW (O)->left_margin_cols
+ WVAR (XWINDOW (O), left_margin_cols)
|
- XWINDOW (O)->right_margin_cols
+ WVAR (XWINDOW (O), right_margin_cols)
|
- XWINDOW (O)->left_fringe_width
+ WVAR (XWINDOW (O), left_fringe_width)
|
- XWINDOW (O)->right_fringe_width
+ WVAR (XWINDOW (O), right_fringe_width)
|
- XWINDOW (O)->scroll_bar_width
+ WVAR (XWINDOW (O), scroll_bar_width)
|
- XWINDOW (O)->vertical_scroll_bar_type
+ WVAR (XWINDOW (O), vertical_scroll_bar_type)
|
- XWINDOW (O)->window_end_pos
+ WVAR (XWINDOW (O), window_end_pos)
|
- XWINDOW (O)->window_end_vpos
+ WVAR (XWINDOW (O), window_end_vpos)
|
- XWINDOW (O)->window_end_valid
+ WVAR (XWINDOW (O), window_end_valid)
|
- XWINDOW (O)->display_table
+ WVAR (XWINDOW (O), display_table)
|
- XWINDOW (O)->dedicated
+ WVAR (XWINDOW (O), dedicated)
|
- XWINDOW (O)->base_line_number
+ WVAR (XWINDOW (O), base_line_number)
|
- XWINDOW (O)->base_line_pos
+ WVAR (XWINDOW (O), base_line_pos)
|
- XWINDOW (O)->region_showing
+ WVAR (XWINDOW (O), region_showing)
|
- XWINDOW (O)->column_number_displayed
+ WVAR (XWINDOW (O), column_number_displayed)
|
- XWINDOW (O)->redisplay_end_trigger
+ WVAR (XWINDOW (O), redisplay_end_trigger)
|
- XWINDOW (O)->combination_limit
+ WVAR (XWINDOW (O), combination_limit)
|
- XWINDOW (O)->prev_buffers
+ WVAR (XWINDOW (O), prev_buffers)
|
- XWINDOW (O)->next_buffers
+ WVAR (XWINDOW (O), next_buffers)
|
- XWINDOW (O)->window_parameters
+ WVAR (XWINDOW (O), window_parameters)
)
