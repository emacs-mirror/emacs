#include <string.h>
#include <opencv2/imgproc.hpp>

#include "opencv.h"

/* srcdata must be obtained from a cairo_image_surface whose format is
   ARGB4, and has size w * h. */
void
opencv_gaussian_blur (void *srcdata,
                      int w, int h,
                      double sigma)
{
  cv::Mat src(h, w, CV_8UC4, srcdata);
  cv::Mat dst(h, w, CV_8UC4);
  cv::GaussianBlur(src, dst, cv::Size(0,0), sigma);
  memcpy(srcdata, dst.data, w*h*4);
}
