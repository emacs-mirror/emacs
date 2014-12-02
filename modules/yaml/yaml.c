#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <yaml.h>


#include <config.h>
#include <lisp.h>

#include <character.h> /* buffer.h needs it */
#include <buffer.h>

int plugin_is_GPL_compatible;
static Lisp_Object Qyaml;

typedef unsigned char uchar;

struct context
{
  yaml_parser_t p;
  int error;
  Lisp_Object anchors; /* hashtable mapping alias to values */
};

static Lisp_Object parse_scalar (struct context *ctx, yaml_event_t *e);
static Lisp_Object parse_sequence (struct context *ctx, yaml_event_t *e);
static Lisp_Object parse_mapping (struct context *ctx, yaml_event_t *e);

static Lisp_Object
parse_element (struct context *ctx)
{
  Lisp_Object res = Qnil;
  yaml_event_t e;

 redo:
  yaml_parser_parse (&ctx->p, &e);
  const char *s = (char*)e.data.alias.anchor;

  switch (e.type)
    {
    case YAML_STREAM_START_EVENT:
      /* a stream is a sequence of documents */
      res = parse_sequence (ctx, &e);
      break;

    case YAML_DOCUMENT_START_EVENT:
    case YAML_DOCUMENT_END_EVENT:
      /* keep reading */
      yaml_event_delete (&e);
      goto redo;

    case YAML_ALIAS_EVENT:
      res = Fgethash (make_string (s, strlen (s)), ctx->anchors, Qnil);
      break;

    case YAML_SCALAR_EVENT:
      res = parse_scalar (ctx, &e);
      if (s)
        Fputhash (make_string (s, strlen (s)), res, ctx->anchors);
      break;

    case YAML_SEQUENCE_START_EVENT:
      res = parse_sequence (ctx, &e);
      if (s)
        Fputhash (make_string (s, strlen (s)), res, ctx->anchors);
      break;

    case YAML_MAPPING_START_EVENT:
      res = parse_mapping (ctx, &e);
      if (s)
        Fputhash (make_string (s, strlen (s)), res, ctx->anchors);
      break;

    case YAML_NO_EVENT:
    case YAML_MAPPING_END_EVENT:
    case YAML_SEQUENCE_END_EVENT:
    case YAML_STREAM_END_EVENT:
      res = Qnil;
      break;
    }

  yaml_event_delete (&e);
  return res;
}

static Lisp_Object
parse_scalar (struct context *ctx, yaml_event_t *e)
{
  return make_string ((char*)e->data.scalar.value, e->data.scalar.length);
}

static Lisp_Object
parse_sequence (struct context *ctx, yaml_event_t *e)
{
  /* always >= 1 elements in sequence */
  Lisp_Object cons = Fcons (parse_element (ctx), Qnil);
  Lisp_Object res = cons;

  while (1)
    {
      Lisp_Object e = parse_element (ctx);

      if (NILP (e))
        break;

      XSETCDR (cons, Fcons(e, Qnil));
      cons = XCDR (cons);
    }

  return res;
}

static Lisp_Object
parse_mapping (struct context *ctx, yaml_event_t *e)
{
  Lisp_Object args[2];
  args[0] = QCtest;
  args[1] = Qequal;
  Lisp_Object res = Fmake_hash_table (2, args);

  while (1)
    {
      Lisp_Object key = parse_element (ctx);

      if (NILP (key))
        break;

      Lisp_Object val = parse_element (ctx);

      Fputhash (key, val, res);
    }

  return res;
}

static void
context_init (struct context *ctx)
{
  memset (ctx, 0, sizeof (*ctx));
  Lisp_Object args[2];
  args[0] = QCtest;
  args[1] = Qequal;
  ctx->anchors = Fmake_hash_table (2, args);
}

EXFUN (Fyaml_parse_string, 1);
DEFUN ("yaml-parse-string", Fyaml_parse_string, Syaml_parse_string, 1, 1, 0,
       doc: "Parse STRING as yaml.")
  (Lisp_Object string)
{
  struct context ctx;
  Lisp_Object res = Qnil;

  context_init (&ctx);

  yaml_parser_initialize (&ctx.p);
  yaml_parser_set_input_string (&ctx.p, SDATA (string), SBYTES (string));
  res = parse_element (&ctx);
  yaml_parser_delete (&ctx.p);

  return res;
}


EXFUN (Fyaml_parse_buffer, 0);
DEFUN ("yaml-parse-buffer", Fyaml_parse_buffer, Syaml_parse_buffer, 0, 0, 0,
       doc: "Parse current buffer as yaml.")
  (void)
{
  struct context ctx;
  Lisp_Object res = Qnil;

  context_init (&ctx);

  yaml_parser_initialize (&ctx.p);
  yaml_parser_set_input_string (&ctx.p, BYTE_POS_ADDR (BEGV_BYTE), ZV_BYTE - BEGV_BYTE);
  res = parse_element (&ctx);
  yaml_parser_delete (&ctx.p);

  return res;
}


EXFUN (Fyaml_parse_file, 1);
DEFUN ("yaml-parse-file", Fyaml_parse_file, Syaml_parse_file, 1, 1, 0,
       doc: "Parse FILE as yaml.")
  (Lisp_Object file)
{
  struct gcpro gcpro1;
  struct context ctx;

  context_init (&ctx);

  int r;
  FILE *fh;
  Lisp_Object res = Qnil;

  fh = fopen((char*)SDATA (file), "r");

  if (!fh)
    goto out;

  r = yaml_parser_initialize (&ctx.p);

  if (!r)
    goto out_close;

  yaml_parser_set_input_file (&ctx.p, fh);

  GCPRO1 (ctx.anchors);
  res = parse_element (&ctx);
  UNGCPRO;

  yaml_parser_delete (&ctx.p);

 out_close:
  fclose (fh);

 out:
  return res;
}

void init ()
{
  DEFSYM (Qyaml, "yaml");

  defsubr (&Syaml_parse_file);
  defsubr (&Syaml_parse_string);
  defsubr (&Syaml_parse_buffer);

  Fprovide (Qyaml, Qnil);
}
