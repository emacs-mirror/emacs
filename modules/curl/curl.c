#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <curl/curl.h>

#include <config.h>
#include <lisp.h>

int plugin_is_GPL_compatible;
static Lisp_Object Qcurl;

struct buffer
{
  char *p;
  size_t size, capacity;
};

struct Lisp_CURL
{
  struct buffer buf;
  CURL *curl;
};

#define XCURL(x) ((struct Lisp_CURL*)XSAVE_POINTER (x, 0))

/* curl write callback */
static size_t
write_cb (void *src, size_t size, size_t nb, void *userp)
{
  struct buffer *buf = userp;
  size_t total = size*nb;

  if (buf->size + total > buf->capacity)
    {
      buf->capacity = 2 * (buf->size + total);
      buf->p = realloc (buf->p, buf->capacity);
    }

  memcpy (buf->p + buf->size, src, total);
  buf->size += total;
  buf->p[buf->size] = 0;

  return total;
}


EXFUN (Fcurl_make, 0);
DEFUN ("curl-make", Fcurl_make, Scurl_make, 0, 0, 0,
       doc: "Return a new CURL handle.")
  (void)
{
  struct Lisp_CURL *p = calloc (sizeof (*p), 1);
  p->buf.p = calloc (1, 1); /* so that realloc always work */
  p->buf.capacity = 0;
  p->curl = curl_easy_init ();
  return make_save_ptr ((void*)p);
}


EXFUN (Fcurl_fetch_url, 2);
DEFUN ("curl-fetch-url", Fcurl_fetch_url, Scurl_fetch_url, 2, 2, 0,
       doc: "Fetch and store the content of URL using HANDLE.\n"
       "Return t if successful otherwise return an error string.")
  (Lisp_Object handle, Lisp_Object url)
{
  CURLcode res;
  struct Lisp_CURL *c = XCURL (handle);

  curl_easy_setopt (c->curl, CURLOPT_URL, SSDATA (url));
  curl_easy_setopt (c->curl, CURLOPT_WRITEFUNCTION, write_cb);
  curl_easy_setopt (c->curl, CURLOPT_WRITEDATA, (void*)&c->buf);
  curl_easy_setopt (c->curl, CURLOPT_USERAGENT, "curl-in-emacs/1.0");
  res = curl_easy_perform (c->curl);

  if (res != CURLE_OK)
    {
      const char* error = curl_easy_strerror (res);
      return make_string (error, strlen (error));
    }

  return Qt;
}

EXFUN (Fcurl_content, 1);
DEFUN ("curl-content", Fcurl_content, Scurl_content, 1, 1, 0,
       doc: "Return the content of a successful fetch made in HANDLE.")
  (Lisp_Object handle)
{
  struct Lisp_CURL *c = XCURL (handle);
  return make_string (c->buf.p, c->buf.size);
}

EXFUN (Fcurl_free, 1);
DEFUN ("curl-free", Fcurl_free, Scurl_free, 1, 1, 0,
       doc: "Free curl HANDLE.")
  (Lisp_Object handle)
{
  struct Lisp_CURL *c = XCURL (handle);
  free (c->buf.p);
  curl_easy_cleanup (c->curl);

  return Qt;
}

void init ()
{
  curl_global_init (CURL_GLOBAL_ALL);
  /* when unloading: curl_global_cleanup(); */

  DEFSYM (Qcurl, "curl");

  defsubr (&Scurl_make);
  defsubr (&Scurl_fetch_url);
  defsubr (&Scurl_content);
  defsubr (&Scurl_free);

  Fprovide (Qcurl, Qnil);
}
