(defpackage :lem-erlang-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:import-from #:ppcre
                #:parse-string)
  (:export :*erlang-mode-hook*
           :erlang-mode
           :*erlang-syntax-table*
           :*erlang-mode-keymap*))

(in-package :lem-erlang-mode)

; Defvars are taken from Erlang OTP's "erlang.el"
; to follow official naming.
; (https://github.com/erlang/otp/tree/master/lib/tools/emacs)

(defvar *erlang-keywords*
  '(
    "after"
    "begin"
    "catch"
    "case"
    "cond"
    "end"
    "fun"
    "if"
    "let"
    "of"
    "receive"
    "try"
    "maybe"
    "else"
    "when"
    ))

(defvar *erlang-keywords2*
  '("module" "behaviour" "behavior" "export" "define" "record" "type" "include" "include_lib" "spec"))

(defvar *erlang-boolean-literals*
  '("true" "false"))

(defvar *erlang-null-literal*
  '("nil"))

(defvar *erlang-operators*
    '("and"
      "andalso"
      "band"
      "bnot"
      "bor"
      "bsl"
      "bsr"
      "bxor"
      "div"
      "not"
      "or"
      "orelse"
      "rem"
      "xor"))

(defvar *erlang-operators2*
   '(
    "+" "-" "*" "/" ">" ">=" "<" "=<" "=:=" "==" "=/=" "/=" "||" "!" ":=" "=>" "|"))

(defvar *erlang-guards*
    '("is_atom"
      "is_binary"
      "is_bitstring"
      "is_boolean"
      "is_float"
      "is_function"
      "is_integer"
      "is_list"
      "is_map"
      "is_number"
      "is_pid"
      "is_port"
      "is_record"
      "is_reference"
      "is_tuple"
      "atom"
      "binary"
      "bitstring"
      "boolean"
      ;;"float" ; Not included to avoid clashes with the bif float/1
      "function"
      "integer"
      "list"
      "number"
      "pid"
      "port"
      "record"
      "reference"
      "tuple"))

(defvar *erlang-predefined-types*
    '("any"
      "arity"
      "boolean"
      "byte"
      "char"
      "cons"
      "deep_string"
      "iodata"
      "iolist"
      "maybe_improper_list"
      "module"
      "mfa"
      "nil"
      "neg_integer"
      "none"
      "non_neg_integer"
      "nonempty_list"
      "nonempty_improper_list"
      "nonempty_maybe_improper_list"
      "nonempty_string"
      "no_return"
      "pos_integer"
      "string"
      "term"
      "timeout"
      "map"))

(defvar *erlang-int-bifs*
    '("abs"
      "alias"
      "apply"
      "atom_to_binary"
      "atom_to_list"
      "binary_to_atom"
      "binary_to_existing_atom"
      "binary_to_float"
      "binary_to_integer"
      "binary_to_list"
      "binary_to_term"
      "binary_part"
      "bit_size"
      "bitstring_to_list"
      "byte_size"
      "ceil"
      "check_old_code"
      "check_process_code"
      "date"
      "delete_module"
      "demonitor"
      "disconnect_node"
      "element"
      "erase"
      "error"
      "exit"
      "floor"
      "float"
      "float_to_binary"
      "float_to_list"
      "garbage_collect"
      "get"
      "get_keys"
      "group_leader"
      "halt"
      "hd"
      "integer_to_list"
      "integer_to_binary"
      "iolist_size"
      "iolist_to_binary"
      "is_alive"
      "is_atom"
      "is_binary"
      "is_bitstring"
      "is_boolean"
      "is_float"
      "is_function"
      "is_integer"
      "is_list"
      "is_map"
      "is_map_key"
      "is_number"
      "is_pid"
      "is_port"
      "is_process_alive"
      "is_record"
      "is_reference"
      "is_tuple"
      "length"
      "link"
      "list_to_atom"
      "list_to_binary"
      "list_to_bitstring"
      "list_to_existing_atom"
      "list_to_float"
      "list_to_integer"
      "list_to_pid"
      "list_to_port"
      "list_to_ref"
      "list_to_tuple"
      "load_module"
      "make_ref"
      "map_get"
      "map_size"
      "max"
      "min"
      "module_loaded"
      "monitor"
      "monitor_node"
      "node"
      "nodes"
      "now"
      "open_port"
      "pid_to_list"
      "port_close"
      "port_command"
      "port_connect"
      "port_control"
      "port_to_list"
      "pre_loaded"
      "process_flag"
      "process_info"
      "processes"
      "purge_module"
      "put"
      "ref_to_list"
      "register"
      "registered"
      "round"
      "self"
      "setelement"
      "size"
      "spawn"
      "spawn_link"
      "spawn_monitor"
      "spawn_opt"
      "spawn_request"
      "spawn_request_abandon"
      "split_binary"
      "statistics"
      "term_to_binary"
      "term_to_iovec"
      "time"
      "throw"
      "tl"
      "trunc"
      "tuple_size"
      "tuple_to_list"
      "unalias"
      "unlink"
      "unregister"
      "whereis"))

(defvar *erlang-ext-bifs*
    '("adler32"
      "adler32_combine"
      "alloc_info"
      "alloc_sizes"
      "append"
      "append_element"
      "bump_reductions"
      "call_on_load_function"
      "cancel_timer"
      "crc32"
      "crc32_combine"
      "decode_packet"
      "delay_trap"
      "delete_element"
      "display"
      "display_string"
      "dist_get_stat"
      "dist_ctrl_get_data"
      "dist_ctrl_get_data_notification"
      "dist_ctrl_get_opt"
      "dist_ctrl_input_handler"
      "dist_ctrl_put_data"
      "dist_ctrl_set_opt"
      "dmonitor_node"
      "dt_append_vm_tag_data"
      "dt_get_tag"
      "dt_get_tag_data"
      "dt_prepend_vm_tag_data"
      "dt_put_tag"
      "dt_restore_tag"
      "dt_spread_tag"
      "convert_time_unit"
      "exit_signal"
      "external_size"
      "finish_after_on_load"
      "finish_loading"
      "format_cpu_topology"
      "fun_info"
      "fun_info_mfa"
      "fun_to_list"
      "function_exported"
      "garbage_collect_message_area"
      "gather_gc_info_result"
      "get_cookie"
      "get_module_info"
      "has_prepared_code_on_load"
      "hibernate"
      "insert_element"
      "iolist_to_iovec"
      "is_builtin"
      "load_nif"
      "loaded"
      "localtime"
      "localtime_to_universaltime"
      "make_fun"
      "make_tuple"
      "match_spec_test"
      "md5"
      "md5_final"
      "md5_init"
      "md5_update"
      "memory"
      "module_info"
      "monitor_node"
      "monotonic_time"
      "nif_error"
      "phash"
      "phash2"
      "port_call"
      "port_get_data"
      "port_info"
      "port_set_data"
      "ports"
      "posixtime_to_universaltime"
      "prepare_loading"
      "process_display"
      "raise"
      "read_timer"
      "resume_process"
      "send"
      "send_after"
      "send_nosuspend"
      "seq_trace"
      "seq_trace_info"
      "seq_trace_print"
      "set_cookie"
      "set_cpu_topology"
      "setnode"
      "start_timer"
      "subtract"
      "suspend_process"
      "system_flag"
      "system_info"
      "system_monitor"
      "system_profile"
      "system_time"
      "trace"
      "trace_delivered"
      "trace_info"
      "trace_pattern"
      "time_offset"
      "timestamp"
      "universaltime"
      "universaltime_to_localtime"
      "universaltime_to_posixtime"
      "unique_integer"
      "yield"))

(defun tokens (boundary strings)
  (let ((alternation
          `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun word-length-sort (&rest words)
  (sort (copy-list words) #'> :key #'length))

(defun wrap-symbol-names (&rest names)
  `(:sequence
    (:register
     (:group :case-insensitive-p
      ,(let ((args (apply #'word-length-sort names)))
         (if (null (rest args)) (first args) `(:alternation ,@args)))))
    (:alternation
     (:greedy-repetition 1 nil :whitespace-char-class)
     :whitespace-char-class :end-anchor #\( #\))))

; custom attributes

(define-attribute syntax-guard-attribute
  (:light :foreground "#00875f")
  (:dark :foreground "SpringGreen"))

(defun make-tmlanguage-erlang ()
  (let* ((patterns (make-tm-patterns
                    (make-tm-match
                              "%[+-]"
                              :name 'syntax-comment-attribute)
                    (make-tm-match "[\\b]*[A-Z]+[A-Za-z0-9_-]*"
                                      :name 'syntax-variable-attribute)
                    (make-tm-match "[\\b]*[\?]+[\\w]+\\b"
                                      :name 'syntax-constant-attribute)
                    (make-tm-line-comment-region "%")
                    (make-tm-string-region "\"")
                    (make-tm-string-region "'")
                    (make-tm-string-region "\"\"\"")
                    (make-tm-match (tokens :word-boundary
                                           (append *erlang-boolean-literals*
                                                   *erlang-null-literal*))
                                   :name 'syntax-constant-attribute)
                    (make-tm-match (tokens :word-boundary *erlang-keywords*)
                                   :name 'syntax-keyword-attribute)
                    (make-tm-match
                    `(:sequence
                      "-" ,(lem-lisp-mode/grammar::wrap-symbol-names
                      "module" "behaviour" "behavior" "export" "export_type" "define" "record" "type" "include" "include_lib" "spec"))
                    :captures (vector nil (make-tm-name 'syntax-keyword-attribute)))
                    (make-tm-match "^[\n]*[a-z_]+"
                          :name 'syntax-function-name-attribute)
                    (make-tm-match (tokens :word-boundary (append '("when") *erlang-guards*))
                                    :name 'syntax-guard-attribute)
                    (make-tm-match (tokens :word-boundary *erlang-int-bifs*)
                                    :name 'syntax-builtin-attribute)
                    (make-tm-match (tokens :word-boundary *erlang-ext-bifs*)
                                    :name 'syntax-builtin-attribute)
                    (make-tm-match (tokens :word-boundary *erlang-operators*)
                                   :name 'syntax-builtin-attribute)
                    (make-tm-match (tokens :word-boundary *erlang-predefined-types*)
                                    :name 'syntax-type-attribute)
                    (make-tm-match (tokens nil *erlang-operators2*)
                                   :name 'syntax-builtin-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *erlang-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\')
                :block-string-pairs '(("\"\"\"" . "\"\"\"")
                                      ("'''" . "'''"))
                :line-comment-string "%"))
        (tmlanguage (make-tmlanguage-erlang)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode erlang-mode language-mode
    (:name "Erlang"
     :keymap *erlang-mode-keymap*
     :syntax-table *erlang-syntax-table*
     :mode-hook *erlang-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'calc-indent-function) 'erlang-calc-indent
        (variable-value 'indent-tabs-mode) t
        (variable-value 'tab-width) 4
        (variable-value 'line-comment) "%"
        (variable-value 'beginning-of-defun-function) 'beginning-of-defun
        (variable-value 'end-of-defun-function) 'end-of-defun))

(defun beginning-of-defun (point n)
  (loop :with regex = "^[a-z_\-]+\([A-Za-z\s\S_]*\)\s*->"
        :repeat n
        :do (search-backward-regexp point regex)))

(defun end-of-defun (point n)
  (with-point ((p point))
    (loop :repeat n
          :do (line-offset p 1)
              (unless (search-forward-regexp p "$[\).]|$[\);]") (return)))
    (line-start p)
    (move-point point p)))

(define-file-type ("erl" "hrl") erlang-mode)

; as in markdown mode
(defun erlang-calc-indent (point)
  (with-point ((point point))
    (let ((tab-width (variable-value 'tab-width :default point))
          (column (point-column point)))
      (+ column (- tab-width (rem column tab-width))))))
