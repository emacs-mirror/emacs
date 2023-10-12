#ifndef EMACS_XCAIRO_H
#define EMACS_XCAIRO_H

#include <cairo.h>

void box_blur (cairo_surface_t *s, cairo_surface_t *t, int w, int h, int r);
void gaussian_blur (cairo_surface_t *s, int w, int h, double r);

#endif /* EMACS_XCAIRO_H */
