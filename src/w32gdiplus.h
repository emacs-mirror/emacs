#ifdef WINDOWSNT
typedef GpStatus (WINGDIPAPI *GdiplusStartup_Proc)
  (ULONG_PTR *, GdiplusStartupInput *, GdiplusStartupOutput *);
typedef VOID (WINGDIPAPI *GdiplusShutdown_Proc) (ULONG_PTR);
typedef GpStatus (WINGDIPAPI *GdipCreateFromHDC_Proc)
  (HDC hdc, GpGraphics **graphics);
typedef GpStatus (WINGDIPAPI *GdipDeleteGraphics_Proc) (GpGraphics *graphics);
typedef GpStatus (WINGDIPAPI *GdipGetPropertyItemSize_Proc)
  (GpImage *, PROPID, UINT *);
typedef GpStatus (WINGDIPAPI *GdipGetPropertyItem_Proc)
  (GpImage *, PROPID, UINT, PropertyItem *);
typedef GpStatus (WINGDIPAPI *GdipImageGetFrameDimensionsCount_Proc)
  (GpImage *, UINT *);
typedef GpStatus (WINGDIPAPI *GdipImageGetFrameDimensionsList_Proc)
  (GpImage *, GUID *, UINT);
typedef GpStatus (WINGDIPAPI *GdipImageGetFrameCount_Proc)
  (GpImage *, GDIPCONST GUID *, UINT *);
typedef GpStatus (WINGDIPAPI *GdipImageSelectActiveFrame_Proc)
  (GpImage*, GDIPCONST GUID *, UINT);
typedef GpStatus (WINGDIPAPI *GdipCreateBitmapFromFile_Proc)
  (WCHAR *, GpBitmap **);
typedef GpStatus (WINGDIPAPI *GdipCreateBitmapFromStream_Proc)
  (IStream *, GpBitmap **);
typedef GpStatus (WINGDIPAPI *GdipCreateBitmapFromScan0_Proc)
  (INT, INT, INT, PixelFormat, BYTE*, GpBitmap**);
typedef GpStatus (WINGDIPAPI *GdipCreateBitmapFromHBITMAP_Proc)
  (HBITMAP hbm, HPALETTE hpal, GpBitmap** bitmap);
typedef GpStatus (WINGDIPAPI *GdipSetInterpolationMode_Proc)
  (GpGraphics *graphics, InterpolationMode interpolationMode);
typedef GpStatus (WINGDIPAPI *GdipDrawImageRectRectI_Proc)
  (GpGraphics *graphics, GpImage *image, INT dstx, INT dsty, INT dstwidth,
   INT dstheight, INT srcx, INT srcy, INT srcwidth, INT srcheight,
   GpUnit srcUnit, GDIPCONST GpImageAttributes* imageAttributes,
   DrawImageAbort callback, VOID * callbackData);
typedef IStream * (WINAPI *SHCreateMemStream_Proc) (const BYTE *, UINT);
typedef GpStatus (WINGDIPAPI *GdipCreateHBITMAPFromBitmap_Proc)
  (GpBitmap *, HBITMAP *, ARGB);
typedef GpStatus (WINGDIPAPI *GdipDisposeImage_Proc) (GpImage *);
typedef GpStatus (WINGDIPAPI *GdipGetImageHeight_Proc) (GpImage *, UINT *);
typedef GpStatus (WINGDIPAPI *GdipGetImageWidth_Proc) (GpImage *, UINT *);
typedef GpStatus (WINGDIPAPI *GdipGetImageEncodersSize_Proc) (UINT *, UINT *);
typedef GpStatus (WINGDIPAPI *GdipGetImageEncoders_Proc)
 (UINT, UINT, ImageCodecInfo *);
typedef GpStatus (WINGDIPAPI *GdipLoadImageFromFile_Proc)
 (GDIPCONST WCHAR *,GpImage **);
typedef GpStatus (WINGDIPAPI *GdipGetImageThumbnail_Proc)
 (GpImage *, UINT, UINT, GpImage**, GetThumbnailImageAbort, VOID *);
typedef GpStatus (WINGDIPAPI *GdipSaveImageToFile_Proc)
 (GpImage *, GDIPCONST WCHAR *, GDIPCONST CLSID *,
 GDIPCONST EncoderParameters *);
typedef GpStatus (WINGDIPAPI *GdipImageRotateFlip_Proc)
  (GpImage *image, RotateFlipType rfType);

extern GdiplusStartup_Proc fn_GdiplusStartup;
extern GdiplusShutdown_Proc fn_GdiplusShutdown;
extern GdipCreateFromHDC_Proc fn_GdipCreateFromHDC;
extern GdipDeleteGraphics_Proc fn_GdipDeleteGraphics;
extern GdipGetPropertyItemSize_Proc fn_GdipGetPropertyItemSize;
extern GdipGetPropertyItem_Proc fn_GdipGetPropertyItem;
extern GdipImageGetFrameDimensionsCount_Proc fn_GdipImageGetFrameDimensionsCount;
extern GdipImageGetFrameDimensionsList_Proc fn_GdipImageGetFrameDimensionsList;
extern GdipImageGetFrameCount_Proc fn_GdipImageGetFrameCount;
extern GdipImageSelectActiveFrame_Proc fn_GdipImageSelectActiveFrame;
extern GdipCreateBitmapFromFile_Proc fn_GdipCreateBitmapFromFile;
extern GdipCreateBitmapFromStream_Proc fn_GdipCreateBitmapFromStream;
extern GdipCreateBitmapFromHBITMAP_Proc fn_GdipCreateBitmapFromHBITMAP;
extern GdipDrawImageRectRectI_Proc fn_GdipDrawImageRectRectI;
extern GdipSetInterpolationMode_Proc fn_GdipSetInterpolationMode;
extern GdipCreateBitmapFromScan0_Proc fn_GdipCreateBitmapFromScan0;
extern SHCreateMemStream_Proc fn_SHCreateMemStream;
extern GdipCreateHBITMAPFromBitmap_Proc fn_GdipCreateHBITMAPFromBitmap;
extern GdipDisposeImage_Proc fn_GdipDisposeImage;
extern GdipGetImageHeight_Proc fn_GdipGetImageHeight;
extern GdipGetImageWidth_Proc fn_GdipGetImageWidth;
extern GdipGetImageEncodersSize_Proc fn_GdipGetImageEncodersSize;
extern GdipGetImageEncoders_Proc fn_GdipGetImageEncoders;
extern GdipLoadImageFromFile_Proc fn_GdipLoadImageFromFile;
extern GdipGetImageThumbnail_Proc fn_GdipGetImageThumbnail;
extern GdipSaveImageToFile_Proc fn_GdipSaveImageToFile;
extern GdipImageRotateFlip_Proc fn_GdipImageRotateFlip;

# undef GdiplusStartup
# undef GdiplusShutdown
# undef GdipGetPropertyItemSize
# undef GdipGetPropertyItem
# undef GdipImageGetFrameDimensionsCount
# undef GdipImageGetFrameDimensionsList
# undef GdipImageGetFrameCount
# undef GdipImageSelectActiveFrame
# undef GdipCreateBitmapFromFile
# undef GdipCreateBitmapFromStream
# undef GdipCreateBitmapFromScan0
# undef GdipCreateBitmapFromHBITMAP
# undef GdipCreateFromHDC
# undef GdipDrawImageRectRectI
# undef GdipSetInterpolationMode
# undef GdipDeleteGraphics
# undef SHCreateMemStream
# undef GdipCreateHBITMAPFromBitmap
# undef GdipDisposeImage
# undef GdipGetImageHeight
# undef GdipGetImageWidth
# undef GdipGetImageEncodersSize
# undef GdipGetImageEncoders
# undef GdipLoadImageFromFile
# undef GdipGetImageThumbnail
# undef GdipSaveImageToFile
# undef GdipSaveImageRotateFlip

# define GdiplusStartup fn_GdiplusStartup
# define GdiplusShutdown fn_GdiplusShutdown
# define GdipGetPropertyItemSize fn_GdipGetPropertyItemSize
# define GdipGetPropertyItem fn_GdipGetPropertyItem
# define GdipImageGetFrameDimensionsCount fn_GdipImageGetFrameDimensionsCount
# define GdipImageGetFrameDimensionsList fn_GdipImageGetFrameDimensionsList
# define GdipImageGetFrameCount fn_GdipImageGetFrameCount
# define GdipImageSelectActiveFrame fn_GdipImageSelectActiveFrame
# define GdipCreateBitmapFromFile fn_GdipCreateBitmapFromFile
# define GdipCreateBitmapFromStream fn_GdipCreateBitmapFromStream
# define GdipCreateBitmapFromScan0 fn_GdipCreateBitmapFromScan0
# define GdipCreateBitmapFromHBITMAP fn_GdipCreateBitmapFromHBITMAP
# define GdipCreateFromHDC fn_GdipCreateFromHDC
# define GdipDrawImageRectRectI fn_GdipDrawImageRectRectI
# define GdipSetInterpolationMode fn_GdipSetInterpolationMode
# define GdipDeleteGraphics  fn_GdipDeleteGraphics
# define SHCreateMemStream fn_SHCreateMemStream
# define GdipCreateHBITMAPFromBitmap fn_GdipCreateHBITMAPFromBitmap
# define GdipDisposeImage fn_GdipDisposeImage
# define GdipGetImageHeight fn_GdipGetImageHeight
# define GdipGetImageWidth fn_GdipGetImageWidth
# define GdipGetImageEncodersSize fn_GdipGetImageEncodersSize
# define GdipGetImageEncoders fn_GdipGetImageEncoders
# define GdipLoadImageFromFile fn_GdipLoadImageFromFile
# define GdipGetImageThumbnail fn_GdipGetImageThumbnail
# define GdipSaveImageToFile fn_GdipSaveImageToFile
# define GdipImageRotateFlip fn_GdipImageRotateFlip
#endif

int w32_gdip_get_encoder_clsid (const char *type, CLSID *clsid);
