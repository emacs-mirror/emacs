// Change direct access to Lisp_Object fields of struct Lisp_Process to PVAR.
@@
struct Lisp_Process *P;
Lisp_Object O;
@@
(
- P->tty_name
+ PVAR (P, tty_name)
|
- P->name
+ PVAR (P, name)
|
- P->command
+ PVAR (P, command)
|
- P->filter
+ PVAR (P, filter)
|
- P->sentinel
+ PVAR (P, sentinel)
|
- P->log
+ PVAR (P, log)
|
- P->buffer
+ PVAR (P, buffer)
|
- P->childp
+ PVAR (P, childp)
|
- P->plist
+ PVAR (P, plist)
|
- P->type
+ PVAR (P, type)
|
- P->mark
+ PVAR (P, mark)
|
- P->status
+ PVAR (P, status)
|
- P->decode_coding_system
+ PVAR (P, decode_coding_system)
|
- P->decoding_buf
+ PVAR (P, decoding_buf)
|
- P->encode_coding_system
+ PVAR (P, encode_coding_system)
|
- P->encoding_buf
+ PVAR (P, encoding_buf)
|
- P->write_queue
+ PVAR (P, write_queue)

|

- XPROCESS (O)->tty_name
+ PVAR (XPROCESS (O), tty_name)
|
- XPROCESS (O)->name
+ PVAR (XPROCESS (O), name)
|
- XPROCESS (O)->command
+ PVAR (XPROCESS (O), command)
|
- XPROCESS (O)->filter
+ PVAR (XPROCESS (O), filter)
|
- XPROCESS (O)->sentinel
+ PVAR (XPROCESS (O), sentinel)
|
- XPROCESS (O)->log
+ PVAR (XPROCESS (O), log)
|
- XPROCESS (O)->buffer
+ PVAR (XPROCESS (O), buffer)
|
- XPROCESS (O)->childp
+ PVAR (XPROCESS (O), childp)
|
- XPROCESS (O)->plist
+ PVAR (XPROCESS (O), plist)
|
- XPROCESS (O)->type
+ PVAR (XPROCESS (O), type)
|
- XPROCESS (O)->mark
+ PVAR (XPROCESS (O), mark)
|
- XPROCESS (O)->status
+ PVAR (XPROCESS (O), status)
|
- XPROCESS (O)->decode_coding_system
+ PVAR (XPROCESS (O), decode_coding_system)
|
- XPROCESS (O)->decoding_buf
+ PVAR (XPROCESS (O), decoding_buf)
|
- XPROCESS (O)->encode_coding_system
+ PVAR (XPROCESS (O), encode_coding_system)
|
- XPROCESS (O)->encoding_buf
+ PVAR (XPROCESS (O), encoding_buf)
|
- XPROCESS (O)->write_queue
+ PVAR (XPROCESS (O), write_queue)
)
