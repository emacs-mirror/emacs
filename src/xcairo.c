#include <math.h>
#include <string.h>
#include <cairo.h>
#include "xcairo.h"

typedef unsigned char v4uc __attribute__((vector_size (4)));
typedef float v4f __attribute__((vector_size (16)));

#define NUM_BOXES 3

static int *
boxes_for_gaussian (double sigma)
{
  static double last_sigma = -0.0;
  static int sizes[NUM_BOXES];
  double wIdeal, mIdeal;
  int wl, wu, m;
  int n = NUM_BOXES;

  if (sigma == last_sigma)
    return sizes;
  else
    last_sigma = sigma;

  wIdeal = sqrt ((12 * sigma * sigma / n) + 1);
  wl = floor (wIdeal);
  if (wl % 2 == 0)
    wl --;
  wu = wl + 2;

  mIdeal = (12 * sigma * sigma - n * wl * wl - 4 * n * wl - 3 * n) / (- 4 * wl - 4);
  m = round(mIdeal);

  for (int i = 0; i < n; ++i)
    sizes[i] = i < m ? wl : wu;
  return sizes;
}

#define uc2f(v) (__builtin_convertvector((v), v4f) / 128.0f)
#define f2uc(v) (__builtin_convertvector((v) * 128.0f, v4uc))
#define scaled_mult4(v,x) f2uc(uc2f((v))*x)

static void
box_blur_h (v4uc *s, v4uc *t, int w, int h, int r)
{
  float iarr = 1.0f / (r + r + 1.0f);
  for (int i = 0; i < h; ++i)
    {
      int ti = i * w, li = ti, ri = ti + r;
      v4uc fv = s[ti], lv = s[ti+w-1];
      v4f val = uc2f(fv) * (float)(r+1);
      for(int j=0;j<r;++j)
        val += uc2f(s[ti+j]);
      for(int j=0;j<=r;++j)
        {
          val += uc2f(s[ri++])-uc2f(fv);
          t[ti++] = f2uc(val*iarr);
        }
      for(int j=r+1;j<w-r;++j)
        {
          val += uc2f(s[ri++])-uc2f(s[li++]);
          t[ti++] = f2uc(val*iarr);
        }
      for(int j=w-r;j<w-1;++j)
        {
          val += uc2f(lv)-uc2f(s[li++]);
          t[ti++] = f2uc(val*iarr);
        }
    }
}

static void
box_blur_t (v4uc *s, v4uc *t, int w, int h, int r)
{
  float iarr = 1.0f / (r + r + 1.0f);
  for (int i = 0; i < w; ++i)
    {
      int ti = i, li = ti, ri = ti+r*w;
      v4uc fv = s[ti], lv = s[ti+w*(h-1)];
      v4f val = uc2f(fv) * (float)(r+1);
      for(int j=0; j<r; ++j)
        val += uc2f(s[ti+j*w]);
      for(int j=0; j<=r; ++j)
        {
          val += uc2f(s[ri]) - uc2f(fv);
          t[ti] = f2uc(val*iarr);
          ri+=w; ti+=w;
        }
      for(int j=r+1; j<h-r; ++j)
        {
          val += uc2f(s[ri]) - uc2f(s[li]);
          t[ti] = f2uc(val*iarr);
          li+=w; ri+=w; ti+=w;
        }
      for(int j=h-r; j<h-1; ++j)
        {
          val += uc2f(lv) - uc2f(s[li]);
          t[ti] = f2uc(val*iarr);
          li+=w; ti+=w;
        }
    }
}

void
box_blur (cairo_surface_t *s, cairo_surface_t *t, int w, int h, int r)
{
  v4uc *sdata, *tdata;

  sdata = (v4uc *) cairo_image_surface_get_data (s);
  tdata = (v4uc *) cairo_image_surface_get_data (t);

  memcpy (tdata, sdata, w * h * 4);

  box_blur_h (tdata, sdata, w, h, r);
  box_blur_t (sdata, tdata, w, h, r);
}

void
gaussian_blur (cairo_surface_t *s, int w, int h, double r)
{
  int *boxes = boxes_for_gaussian (r);

  cairo_surface_t *t = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, w, h);
  v4uc *sdata, *tdata;

  sdata = cairo_image_surface_get_data (s);
  tdata = cairo_image_surface_get_data (t);

  /* Inline box_blur to reduce memory copying. */
  box_blur_h (sdata, tdata, w, h, (boxes[0] - 1.0) / 2.0);
  box_blur_t (tdata, sdata, w, h, (boxes[0] - 1.0) / 2.0);
  box_blur_h (sdata, tdata, w, h, (boxes[1] - 1.0) / 2.0);
  box_blur_t (tdata, sdata, w, h, (boxes[1] - 1.0) / 2.0);
  box_blur_h (sdata, tdata, w, h, (boxes[2] - 1.0) / 2.0);
  box_blur_t (tdata, sdata, w, h, (boxes[2] - 1.0) / 2.0);

  cairo_surface_destroy (t);
  cairo_surface_mark_dirty (s);
}
