#ifndef EMACS_OPENCV_H
#define EMACS_OPENCV_H

#ifdef __cplusplus
extern "C" {
#endif

void opencv_gaussian_blur (void *src, void *dst, int w, int r);

#ifdef __cplusplus
}
#endif

#endif /* EMACS_OPENCV_H */
