/* Support for using DirectWrite on MS-Windows to draw text.  This
   allows for color fonts.
   Copyright (C) 2024-2026 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

/* This requires the HarfBuzz font backend to be available.

   It works by modifying the HarfBuzz backend to use DirectWrite at
   some points, if it is available:

   - When encoding characters: w32hb_encode_char
   - When measuring text: w32font_text_extents
   - When drawing text: w32font_draw

   DirectWrite is setup by calling w32_initialize_direct_write.  From
   that point, the function w32_use_direct_write will return true if
   DirectWrite is to be used.

   DirectWrite is available since Windows 7, but we don't activate it on
   versions before 8.1, because color fonts are only available since that.  */

#include <config.h>
#include <math.h>
#include <windows.h>

#if !defined MINGW_W64 && !defined CYGWIN
# define INITGUID
#endif
#include <initguid.h>
#include <ole2.h>
#include <unknwn.h>

#include "frame.h"
#include "w32font.h"
#include "w32common.h"
#include "w32term.h"

#ifndef MINGW_W64

/* The following definitions would be included from dwrite_3.h, but it
   is not available when building with mingw.org's MinGW.  Methods that
   we don't use are declared with the EMACS_DWRITE_UNUSED macro, to
   avoid bringing in more types that would need to be declared.  */

#define EMACS_DWRITE_UNUSED(name) void (STDMETHODCALLTYPE *name) (void)

#define DWRITE_E_NOCOLOR _HRESULT_TYPEDEF_(0x8898500CL)

typedef enum DWRITE_PIXEL_GEOMETRY {
  DWRITE_PIXEL_GEOMETRY_FLAT = 0,
  DWRITE_PIXEL_GEOMETRY_RGB = 1,
  DWRITE_PIXEL_GEOMETRY_BGR = 2
} DWRITE_PIXEL_GEOMETRY;

typedef enum DWRITE_RENDERING_MODE {
  DWRITE_RENDERING_MODE_DEFAULT = 0,
  DWRITE_RENDERING_MODE_ALIASED = 1,
  DWRITE_RENDERING_MODE_GDI_CLASSIC = 2,
  DWRITE_RENDERING_MODE_GDI_NATURAL = 3,
  DWRITE_RENDERING_MODE_NATURAL = 4,
  DWRITE_RENDERING_MODE_NATURAL_SYMMETRIC = 5,
  DWRITE_RENDERING_MODE_OUTLINE = 6
} DWRITE_RENDERING_MODE;

typedef enum DWRITE_MEASURING_MODE {
  DWRITE_MEASURING_MODE_NATURAL = 0,
  DWRITE_MEASURING_MODE_GDI_CLASSIC = 1,
  DWRITE_MEASURING_MODE_GDI_NATURAL = 2
} DWRITE_MEASURING_MODE;

typedef enum DWRITE_TEXT_ANTIALIAS_MODE {
  DWRITE_TEXT_ANTIALIAS_MODE_CLEARTYPE = 0,
  DWRITE_TEXT_ANTIALIAS_MODE_GRAYSCALE = 1
} DWRITE_TEXT_ANTIALIAS_MODE;

typedef enum DWRITE_FACTORY_TYPE {
  DWRITE_FACTORY_TYPE_SHARED = 0,
  DWRITE_FACTORY_TYPE_ISOLATED = 1
} DWRITE_FACTORY_TYPE;

typedef struct DWRITE_FONT_METRICS {
  UINT16 designUnitsPerEm;
  UINT16 ascent;
  UINT16 descent;
  INT16 lineGap;
  UINT16 capHeight;
  UINT16 xHeight;
  INT16 underlinePosition;
  UINT16 underlineThickness;
  INT16 strikethroughPosition;
  UINT16 strikethroughThickness;
} DWRITE_FONT_METRICS;

typedef struct DWRITE_GLYPH_METRICS {
  INT32 leftSideBearing;
  UINT32 advanceWidth;
  INT32 rightSideBearing;
  INT32 topSideBearing;
  UINT32 advanceHeight;
  INT32 bottomSideBearing;
  INT32 verticalOriginY;
} DWRITE_GLYPH_METRICS;

typedef struct D2D1_POINT_2F {
    float x;
    float y;
} D2D1_POINT_2F;

typedef struct D2D1_BEZIER_SEGMENT {
  D2D1_POINT_2F point1;
  D2D1_POINT_2F point2;
  D2D1_POINT_2F point3;
} D2D1_BEZIER_SEGMENT;

typedef enum D2D1_FILL_MODE {
  D2D1_FILL_MODE_ALTERNATE   = 0,
  D2D1_FILL_MODE_WINDING     = 1,
  D2D1_FILL_MODE_FORCE_DWORD = 0xffffffff
} D2D1_FILL_MODE;

typedef enum D2D1_PATH_SEGMENT {
  D2D1_PATH_SEGMENT_NONE                    = 0x00000000,
  D2D1_PATH_SEGMENT_FORCE_UNSTROKED         = 0x00000001,
  D2D1_PATH_SEGMENT_FORCE_ROUND_LINE_JOIN   = 0x00000002,
  D2D1_PATH_SEGMENT_FORCE_DWORD             = 0xffffffff
} D2D1_PATH_SEGMENT;

typedef enum D2D1_FIGURE_BEGIN {
  D2D1_FIGURE_BEGIN_FILLED      = 0,
  D2D1_FIGURE_BEGIN_HOLLOW      = 1,
  D2D1_FIGURE_BEGIN_FORCE_DWORD = 0xffffffff
} D2D1_FIGURE_BEGIN;

typedef enum D2D1_FIGURE_END {
  D2D1_FIGURE_END_OPEN        = 0,
  D2D1_FIGURE_END_CLOSED      = 1,
  D2D1_FIGURE_END_FORCE_DWORD = 0xffffffff
} D2D1_FIGURE_END;

typedef interface IDWriteRenderingParams IDWriteRenderingParams;
typedef interface IDWriteFont IDWriteFont;
typedef interface IDWriteGdiInterop IDWriteGdiInterop;
typedef interface IDWriteFactory IDWriteFactory;
typedef interface IDWriteFactory2 IDWriteFactory2;
typedef interface IDWriteFontFace IDWriteFontFace;
typedef interface IDWriteBitmapRenderTarget IDWriteBitmapRenderTarget;
typedef interface IDWriteBitmapRenderTarget1 IDWriteBitmapRenderTarget1;
typedef interface IDWriteColorGlyphRunEnumerator IDWriteColorGlyphRunEnumerator;
typedef interface ID2D1SimplifiedGeometrySink ID2D1SimplifiedGeometrySink;

DEFINE_GUID (IID_IDWriteBitmapRenderTarget1, 0x791e8298, 0x3ef3, 0x4230, 0x98,
	     0x80, 0xc9, 0xbd, 0xec, 0xc4, 0x20, 0x64);
DEFINE_GUID (IID_IDWriteFactory2, 0x0439fc60, 0xca44, 0x4994, 0x8d, 0xee,
	     0x3a, 0x9a, 0xf7, 0xb7, 0x32, 0xec);
DEFINE_GUID (IID_IDWriteFactory, 0xb859ee5a, 0xd838, 0x4b5b, 0xa2, 0xe8, 0x1a,
	     0xdc, 0x7d, 0x93, 0xdb, 0x48);

typedef struct DWRITE_GLYPH_OFFSET {
  FLOAT advanceOffset;
  FLOAT ascenderOffset;
} DWRITE_GLYPH_OFFSET;

typedef struct DWRITE_GLYPH_RUN {
  IDWriteFontFace *fontFace;
  FLOAT fontEmSize;
  UINT32 glyphCount;
  const UINT16 *glyphIndices;
  const FLOAT *glyphAdvances;
  const DWRITE_GLYPH_OFFSET *glyphOffsets;
  WINBOOL isSideways;
  UINT32 bidiLevel;
}  DWRITE_GLYPH_RUN;

typedef struct _D3DCOLORVALUE {
  float r;
  float g;
  float b;
  float a;
} D3DCOLORVALUE;

typedef D3DCOLORVALUE DWRITE_COLOR_F;

typedef struct DWRITE_COLOR_GLYPH_RUN {
  DWRITE_GLYPH_RUN glyphRun;
  void *glyphRunDescription;
  FLOAT baselineOriginX;
  FLOAT baselineOriginY;
  DWRITE_COLOR_F runColor;
  UINT16 paletteIndex;
} DWRITE_COLOR_GLYPH_RUN;

typedef struct IDWriteFontFaceVtbl {
  BEGIN_INTERFACE

  HRESULT (STDMETHODCALLTYPE *QueryInterface)
    (IDWriteFontFace *This, REFIID riid, void **ppvObject);
  ULONG (STDMETHODCALLTYPE *AddRef) (IDWriteFontFace *This);
  ULONG (STDMETHODCALLTYPE *Release) (IDWriteFontFace *This);

  EMACS_DWRITE_UNUSED (GetType);
  EMACS_DWRITE_UNUSED (GetFiles);
  EMACS_DWRITE_UNUSED (GetIndex);
  EMACS_DWRITE_UNUSED (GetSimulations);
  EMACS_DWRITE_UNUSED (IsSymbolFont);

  void (STDMETHODCALLTYPE *GetMetrics)
    (IDWriteFontFace *This, DWRITE_FONT_METRICS *metrics);

  EMACS_DWRITE_UNUSED (GetGlyphCount);
  EMACS_DWRITE_UNUSED (GetDesignGlyphMetrics);

  HRESULT (STDMETHODCALLTYPE *GetGlyphIndices)
    (IDWriteFontFace *This, const UINT32 *codepoints, UINT32 count,
     UINT16 *glyph_indices);

  EMACS_DWRITE_UNUSED (TryGetFontTable);
  EMACS_DWRITE_UNUSED (ReleaseFontTable);
  HRESULT (STDMETHODCALLTYPE *GetGlyphRunOutline)
    (IDWriteFontFace *This,
     FLOAT emSize,
     const UINT16 *glyph_indices,
     const DWRITE_GLYPH_OFFSET *glyph_offsets,
     const FLOAT *glyph_advances,
     UINT32 glyph_count,
     WINBOOL is_sideways,
     WINBOOL is_right_to_left,
     ID2D1SimplifiedGeometrySink *geometry_sink);
  EMACS_DWRITE_UNUSED (GetRecommendedRenderingMode);
  EMACS_DWRITE_UNUSED (GetGdiCompatibleMetrics);

  HRESULT (STDMETHODCALLTYPE *GetGdiCompatibleGlyphMetrics)
    (IDWriteFontFace *This,
     FLOAT emSize,
     FLOAT pixels_per_dip,
     void *transform,
     WINBOOL use_gdi_natural,
     const UINT16 *glyph_indices,
     UINT32 glyph_count,
     DWRITE_GLYPH_METRICS *metrics,
     WINBOOL is_sideways);
  END_INTERFACE
} IDWriteFontFaceVtbl;

interface IDWriteFontFace {
    CONST_VTBL IDWriteFontFaceVtbl *lpVtbl;
};

typedef struct IDWriteRenderingParamsVtbl {
  BEGIN_INTERFACE

  HRESULT (STDMETHODCALLTYPE *QueryInterface)
    (IDWriteRenderingParams *This, REFIID riid, void **ppvObject);
  ULONG (STDMETHODCALLTYPE *AddRef) (IDWriteRenderingParams *This);
  ULONG (STDMETHODCALLTYPE *Release) (IDWriteRenderingParams *This);

  FLOAT (STDMETHODCALLTYPE *GetGamma)
    (IDWriteRenderingParams *This);
  FLOAT (STDMETHODCALLTYPE *GetEnhancedContrast)
    (IDWriteRenderingParams *This);
  FLOAT (STDMETHODCALLTYPE *GetClearTypeLevel)
    (IDWriteRenderingParams *This);
  int (STDMETHODCALLTYPE *GetPixelGeometry)
    (IDWriteRenderingParams *This);
  END_INTERFACE
} IDWriteRenderingParamsVtbl;

interface IDWriteRenderingParams {
    CONST_VTBL IDWriteRenderingParamsVtbl *lpVtbl;
};

typedef struct IDWriteFontVtbl {
  BEGIN_INTERFACE

  HRESULT (STDMETHODCALLTYPE *QueryInterface)
    (IDWriteFont *This, REFIID riid, void **ppvObject);
  ULONG (STDMETHODCALLTYPE *AddRef) (IDWriteFont *This);
  ULONG (STDMETHODCALLTYPE *Release) (IDWriteFont *This);

  EMACS_DWRITE_UNUSED (GetFontFamily);
  EMACS_DWRITE_UNUSED (GetWeight);
  EMACS_DWRITE_UNUSED (GetStretch);
  EMACS_DWRITE_UNUSED (GetStyle);
  EMACS_DWRITE_UNUSED (IsSymbolFont);
  EMACS_DWRITE_UNUSED (GetFaceNames);
  EMACS_DWRITE_UNUSED (GetInformationalStrings);
  EMACS_DWRITE_UNUSED (GetSimulations);

  void (STDMETHODCALLTYPE *GetMetrics)
    (IDWriteFont *This, DWRITE_FONT_METRICS *metrics);

  EMACS_DWRITE_UNUSED (HasCharacter);

  HRESULT (STDMETHODCALLTYPE *CreateFontFace)
    (IDWriteFont *This, IDWriteFontFace **face);

  END_INTERFACE
} IDWriteFontVtbl;

interface IDWriteFont {
  CONST_VTBL IDWriteFontVtbl *lpVtbl;
};

typedef struct IDWriteBitmapRenderTargetVtbl {
  BEGIN_INTERFACE

  HRESULT (STDMETHODCALLTYPE *QueryInterface)
    (IDWriteBitmapRenderTarget *This, REFIID riid, void **ppvObject);
  ULONG (STDMETHODCALLTYPE *AddRef) (IDWriteBitmapRenderTarget *This);
  ULONG (STDMETHODCALLTYPE *Release) (IDWriteBitmapRenderTarget *This);

  HRESULT (STDMETHODCALLTYPE *DrawGlyphRun)
    (IDWriteBitmapRenderTarget *This,
     FLOAT baselineOriginX,
     FLOAT baselineOriginY,
     DWRITE_MEASURING_MODE measuring_mode,
     const DWRITE_GLYPH_RUN *glyph_run,
     IDWriteRenderingParams *params,
     COLORREF textColor,
     RECT *blackbox_rect);

  HDC (STDMETHODCALLTYPE *GetMemoryDC) (IDWriteBitmapRenderTarget *This);

  EMACS_DWRITE_UNUSED (GetPixelsPerDip);

  HRESULT (STDMETHODCALLTYPE *SetPixelsPerDip)
    (IDWriteBitmapRenderTarget *This, FLOAT pixels_per_dip);

  EMACS_DWRITE_UNUSED (GetCurrentTransform);
  EMACS_DWRITE_UNUSED (SetCurrentTransform);
  EMACS_DWRITE_UNUSED (GetSize);
  EMACS_DWRITE_UNUSED (Resize);
  END_INTERFACE
} IDWriteBitmapRenderTargetVtbl;

interface IDWriteBitmapRenderTarget {
  CONST_VTBL IDWriteBitmapRenderTargetVtbl *lpVtbl;
};

typedef struct IDWriteBitmapRenderTarget1Vtbl {
  BEGIN_INTERFACE

  HRESULT (STDMETHODCALLTYPE *QueryInterface)
    (IDWriteBitmapRenderTarget1 *This, REFIID riid, void **ppvObject);
  ULONG (STDMETHODCALLTYPE *AddRef) (IDWriteBitmapRenderTarget1 *This);
  ULONG (STDMETHODCALLTYPE *Release) (IDWriteBitmapRenderTarget1 *This);

  EMACS_DWRITE_UNUSED (DrawGlyphRun);
  EMACS_DWRITE_UNUSED (GetMemoryDC);
  EMACS_DWRITE_UNUSED (GetPixelsPerDip);
  EMACS_DWRITE_UNUSED (SetPixelsPerDip);
  EMACS_DWRITE_UNUSED (GetCurrentTransform);
  EMACS_DWRITE_UNUSED (SetCurrentTransform);
  EMACS_DWRITE_UNUSED (GetSize);
  EMACS_DWRITE_UNUSED (Resize);
  EMACS_DWRITE_UNUSED (GetTextAntialiasMode);

  HRESULT (STDMETHODCALLTYPE *SetTextAntialiasMode)
    (IDWriteBitmapRenderTarget1 *This, DWRITE_TEXT_ANTIALIAS_MODE mode);

  END_INTERFACE
} IDWriteBitmapRenderTarget1Vtbl;

interface IDWriteBitmapRenderTarget1 {
  CONST_VTBL IDWriteBitmapRenderTarget1Vtbl *lpVtbl;
};

typedef struct IDWriteGdiInteropVtbl {
  BEGIN_INTERFACE

  HRESULT (STDMETHODCALLTYPE *QueryInterface)
    (IDWriteGdiInterop *This, REFIID riid, void **ppvObject);
  ULONG (STDMETHODCALLTYPE *AddRef) (IDWriteGdiInterop *This);
  ULONG (STDMETHODCALLTYPE *Release) (IDWriteGdiInterop *This);

  HRESULT (STDMETHODCALLTYPE *CreateFontFromLOGFONT)
    (IDWriteGdiInterop *This, const LOGFONTW *logfont,
     IDWriteFont **font);

  EMACS_DWRITE_UNUSED (ConvertFontToLOGFONT);
  EMACS_DWRITE_UNUSED (ConvertFontFaceToLOGFONT);
  EMACS_DWRITE_UNUSED (CreateFontFaceFromHdc);

  HRESULT (STDMETHODCALLTYPE *CreateBitmapRenderTarget)
    (IDWriteGdiInterop *This, HDC hdc, UINT32 width, UINT32 height,
     IDWriteBitmapRenderTarget **target);
  END_INTERFACE
} IDWriteGdiInteropVtbl;

interface IDWriteGdiInterop {
  CONST_VTBL IDWriteGdiInteropVtbl *lpVtbl;
};

typedef struct IDWriteFactoryVtbl {
  BEGIN_INTERFACE

  HRESULT (STDMETHODCALLTYPE *QueryInterface)
    (IDWriteFactory *This, REFIID riid, void **ppvObject);
  ULONG (STDMETHODCALLTYPE *AddRef) (IDWriteFactory *This);
  ULONG (STDMETHODCALLTYPE *Release) (IDWriteFactory *This);

  EMACS_DWRITE_UNUSED (GetSystemFontCollection);
  EMACS_DWRITE_UNUSED (CreateCustomFontCollection);
  EMACS_DWRITE_UNUSED (RegisterFontCollectionLoader);
  EMACS_DWRITE_UNUSED (UnregisterFontCollectionLoader);
  EMACS_DWRITE_UNUSED (CreateFontFileReference);
  EMACS_DWRITE_UNUSED (CreateCustomFontFileReference);
  EMACS_DWRITE_UNUSED (CreateFontFace);
  HRESULT (STDMETHODCALLTYPE *CreateRenderingParams)
    (IDWriteFactory *This, IDWriteRenderingParams **params);
  EMACS_DWRITE_UNUSED (CreateMonitorRenderingParams);
  HRESULT (STDMETHODCALLTYPE *CreateCustomRenderingParams)
    (IDWriteFactory *This, FLOAT gamma, FLOAT enhancedContrast,
     FLOAT cleartype_level, DWRITE_PIXEL_GEOMETRY geometry,
     DWRITE_RENDERING_MODE mode, IDWriteRenderingParams **params);
  EMACS_DWRITE_UNUSED (RegisterFontFileLoader);
  EMACS_DWRITE_UNUSED (UnregisterFontFileLoader);
  EMACS_DWRITE_UNUSED (CreateTextFormat);
  EMACS_DWRITE_UNUSED (CreateTypography);
  HRESULT (STDMETHODCALLTYPE *GetGdiInterop)
    (IDWriteFactory *This, IDWriteGdiInterop **gdi_interop);
  EMACS_DWRITE_UNUSED (CreateTextLayout);
  EMACS_DWRITE_UNUSED (CreateGdiCompatibleTextLayout);
  EMACS_DWRITE_UNUSED (CreateEllipsisTrimmingSign);
  EMACS_DWRITE_UNUSED (CreateTextAnalyzer);
  EMACS_DWRITE_UNUSED (CreateNumberSubstitution);
  EMACS_DWRITE_UNUSED (CreateGlyphRunAnalysis);
  END_INTERFACE
} IDWriteFactoryVtbl;

interface IDWriteFactory { CONST_VTBL IDWriteFactoryVtbl *lpVtbl; };

typedef struct IDWriteColorGlyphRunEnumeratorVtbl {
  BEGIN_INTERFACE

  HRESULT (STDMETHODCALLTYPE *QueryInterface)
    (IDWriteColorGlyphRunEnumerator *This, REFIID riid, void **ppvObject);
  ULONG (STDMETHODCALLTYPE *AddRef) (IDWriteColorGlyphRunEnumerator *This);
  ULONG (STDMETHODCALLTYPE *Release) (IDWriteColorGlyphRunEnumerator *This);

  HRESULT (STDMETHODCALLTYPE *MoveNext) (IDWriteColorGlyphRunEnumerator *This,
					 WINBOOL *hasRun);

  HRESULT (STDMETHODCALLTYPE *GetCurrentRun) (IDWriteColorGlyphRunEnumerator *This,
					      const DWRITE_COLOR_GLYPH_RUN **run);

  END_INTERFACE
} IDWriteColorGlyphRunEnumeratorVtbl;

interface IDWriteColorGlyphRunEnumerator {
  CONST_VTBL IDWriteColorGlyphRunEnumeratorVtbl *lpVtbl;
};

typedef struct IDWriteFactory2Vtbl {
  BEGIN_INTERFACE
  HRESULT (STDMETHODCALLTYPE *QueryInterface)
    (IDWriteFactory2 *This, REFIID riid, void **ppvObject);
  ULONG (STDMETHODCALLTYPE *AddRef) (IDWriteFactory2 *This);
  ULONG (STDMETHODCALLTYPE *Release) (IDWriteFactory2 *This);
  EMACS_DWRITE_UNUSED (GetSystemFontCollection);
  EMACS_DWRITE_UNUSED (CreateCustomFontCollection);
  EMACS_DWRITE_UNUSED (RegisterFontCollectionLoader);
  EMACS_DWRITE_UNUSED (UnregisterFontCollectionLoader);
  EMACS_DWRITE_UNUSED (CreateFontFileReference);
  EMACS_DWRITE_UNUSED (CreateCustomFontFileReference);
  EMACS_DWRITE_UNUSED (CreateFontFace);
  EMACS_DWRITE_UNUSED (CreateRenderingParams);
  EMACS_DWRITE_UNUSED (CreateMonitorRenderingParams);
  EMACS_DWRITE_UNUSED (CreateCustomRenderingParams);
  EMACS_DWRITE_UNUSED (RegisterFontFileLoader);
  EMACS_DWRITE_UNUSED (UnregisterFontFileLoader);
  EMACS_DWRITE_UNUSED (CreateTextFormat);
  EMACS_DWRITE_UNUSED (CreateTypography);
  EMACS_DWRITE_UNUSED (GetGdiInterop);
  EMACS_DWRITE_UNUSED (CreateTextLayout);
  EMACS_DWRITE_UNUSED (CreateGdiCompatibleTextLayout);
  EMACS_DWRITE_UNUSED (CreateEllipsisTrimmingSign);
  EMACS_DWRITE_UNUSED (CreateTextAnalyzer);
  EMACS_DWRITE_UNUSED (CreateNumberSubstitution);
  EMACS_DWRITE_UNUSED (CreateGlyphRunAnalysis);

  EMACS_DWRITE_UNUSED (GetEudcFontCollection);
  EMACS_DWRITE_UNUSED (IDWriteFactory1_CreateCustomRenderingParams);

  EMACS_DWRITE_UNUSED (GetSystemFontFallback);
  EMACS_DWRITE_UNUSED (CreateFontFallbackBuilder);
  HRESULT (STDMETHODCALLTYPE *TranslateColorGlyphRun)
    (IDWriteFactory2 *This,
     FLOAT originX,
     FLOAT originY,
     const DWRITE_GLYPH_RUN *run,
     void *rundescr,
     DWRITE_MEASURING_MODE mode,
     void *transform,
     UINT32 palette_index,
     IDWriteColorGlyphRunEnumerator **colorlayers);

  EMACS_DWRITE_UNUSED (IDWriteFactory2_CreateCustomRenderingParams);
  EMACS_DWRITE_UNUSED (IDWriteFactory2_CreateGlyphRunAnalysis);
  END_INTERFACE
} IDWriteFactory2Vtbl;

interface IDWriteFactory2 {
  CONST_VTBL IDWriteFactory2Vtbl *lpVtbl;
};

typedef struct ID2D1SimplifiedGeometrySinkVtbl {
  BEGIN_INTERFACE

  HRESULT (STDMETHODCALLTYPE *QueryInterface)
    (ID2D1SimplifiedGeometrySink *This, REFIID riid, void **ppvObject);
  ULONG (STDMETHODCALLTYPE *AddRef) (ID2D1SimplifiedGeometrySink *This);
  ULONG (STDMETHODCALLTYPE *Release) (ID2D1SimplifiedGeometrySink *This);

    VOID (STDMETHODCALLTYPE *SetFillMode) (ID2D1SimplifiedGeometrySink *This, D2D1_FILL_MODE fillMode);
    VOID (STDMETHODCALLTYPE *SetSegmentFlags) (ID2D1SimplifiedGeometrySink *This, D2D1_PATH_SEGMENT vertexFlags);
    VOID (STDMETHODCALLTYPE *BeginFigure) (ID2D1SimplifiedGeometrySink *This, D2D1_POINT_2F startPoint, D2D1_FIGURE_BEGIN figureBegin);
    VOID (STDMETHODCALLTYPE *AddLines) (ID2D1SimplifiedGeometrySink *This, const D2D1_POINT_2F *points, UINT pointsCount);
    VOID (STDMETHODCALLTYPE *AddBeziers) (ID2D1SimplifiedGeometrySink *This, const D2D1_BEZIER_SEGMENT *beziers, UINT beziersCount);
    VOID (STDMETHODCALLTYPE *EndFigure) (ID2D1SimplifiedGeometrySink *This, D2D1_FIGURE_END figureEnd);
    HRESULT (STDMETHODCALLTYPE *Close) (ID2D1SimplifiedGeometrySink *This);
  END_INTERFACE
} ID2D1SimplifiedGeometrySinkVtbl;

interface ID2D1SimplifiedGeometrySink {
    const ID2D1SimplifiedGeometrySinkVtbl *lpVtbl;
};

typedef ID2D1SimplifiedGeometrySink IDWriteGeometrySink;

#else /* MINGW_W64 */
# include <dwrite_3.h>
# include <d2d1.h>
#endif

/* User configurable variables.  If they are smaller than 0, use
   DirectWrite's defaults, or our defaults.  To set them, the user calls
   'w32-dwrite-reinit' */
static float config_enhanced_contrast = -1.0f;
static float config_clear_type_level = -1.0f;
static float config_gamma = -1.0f;

/* Values to use for DirectWrite rendering.  */
#define MEASURING_MODE DWRITE_MEASURING_MODE_NATURAL
#define RENDERING_MODE DWRITE_RENDERING_MODE_NATURAL_SYMMETRIC
#define ANTIALIAS_MODE DWRITE_TEXT_ANTIALIAS_MODE_CLEARTYPE

static void
release_com (IUnknown **i)
{
  if ( *i )
    {
      ((IUnknown *) (*i))->lpVtbl->Release (*i);
      *i = NULL;
    }
}

#define RELEASE_COM(i) release_com ((IUnknown **) &i)

/* Implementation of IDWriteGeometrySink, used to the get bounding
   vertical coordinates of glyphs (ascent/descent).  The methods that
   affect the bounding box are BeginFigure (which gives a start point),
   AddBeziers and AddLines.

   Normal procedures to get text extents fail to give correct
   ascent/descent metrics for individual glyphs, using a default value
   for an entire font.  That is not acceptable, specially for fonts like
   "Sans Serif Collection", which include glyphs for many different
   scripts and have a huge default value.

   Because of that, we need to use the GetGlyphRunOutline and examine
   the glyph's geometry.
*/

struct geometry_sink
{
  IDWriteGeometrySink sink;
  int empty;
  float min_y, max_y;
};

static HRESULT STDMETHODCALLTYPE
geometry_sink_QueryInterface (IUnknown *This, REFIID ri, void **r)
{
  return E_NOINTERFACE;
}

/* There is nothing to allocate of free heres, so we can safely skip ref counting.  */
static ULONG STDMETHODCALLTYPE
geometry_sink_AddRef (IUnknown *This)
{
  return 1;
}

static ULONG STDMETHODCALLTYPE
geometry_sink_Release (IUnknown *This)
{
  return 1;
}

static void STDMETHODCALLTYPE
geometry_sink_AddBeziers (IDWriteGeometrySink *This,
			  const D2D1_BEZIER_SEGMENT *beziers, UINT32 count)
{
  struct geometry_sink *sink = (struct geometry_sink *) This;
  for (UINT32 i = 0; i < count; i++)
    {
      if (sink->min_y > beziers[i].point1.y)
	sink->min_y = beziers[i].point1.y;
      if (sink->max_y < beziers[i].point1.y)
	sink->max_y = beziers[i].point1.y;
      if (sink->min_y > beziers[i].point2.y)
	sink->min_y = beziers[i].point2.y;
      if (sink->max_y < beziers[i].point2.y)
	sink->max_y = beziers[i].point2.y;
      if (sink->min_y > beziers[i].point3.y)
	sink->min_y = beziers[i].point3.y;
      if (sink->max_y < beziers[i].point3.y)
	sink->max_y = beziers[i].point3.y;
    }
}

static void STDMETHODCALLTYPE
geometry_sink_AddLines (IDWriteGeometrySink *This,
			const D2D1_POINT_2F *points, UINT32 count)
{
  struct geometry_sink *sink = (struct geometry_sink *) This;
  for (UINT32 i = 0; i < count; i++)
    {
      if (sink->min_y > points[i].y)
	sink->min_y = points[i].y;
      if (sink->max_y < points[i].y)
	sink->max_y = points[i].y;
    }
}

static void STDMETHODCALLTYPE
geometry_sink_BeginFigure (IDWriteGeometrySink *This,
			   D2D1_POINT_2F startPoint,
			   D2D1_FIGURE_BEGIN figureBegin)
{
  struct geometry_sink *sink = (struct geometry_sink *) This;
  if (sink->min_y > startPoint.y)
    sink->min_y = startPoint.y;
  if (sink->max_y < startPoint.y)
    sink->max_y = startPoint.y;
  sink->empty = 0;
}

static void STDMETHODCALLTYPE
geometry_sink_EndFigure (IDWriteGeometrySink *This, D2D1_FIGURE_END figureEnd)
{
}

static HRESULT STDMETHODCALLTYPE
geometry_sink_Close (IDWriteGeometrySink *This)
{
  return S_OK;
}

static void STDMETHODCALLTYPE
geometry_sink_SetFillMode (IDWriteGeometrySink *This,
			   D2D1_FILL_MODE fillMode)
{
}

static void STDMETHODCALLTYPE
geometry_sink_SetSegmentFlags (IDWriteGeometrySink *This,
			       D2D1_PATH_SEGMENT vertexFlags)
{
}

/* Global variables for DirectWrite.  */
static bool direct_write_available = false;
static IDWriteFactory *dwrite_factory = NULL;
static IDWriteFactory2 *dwrite_factory2 = NULL;
static IDWriteGdiInterop *gdi_interop = NULL;
static IDWriteRenderingParams *rendering_params = NULL;
static struct geometry_sink dwrite_geometry_sink;
static ID2D1SimplifiedGeometrySinkVtbl dwrite_geometry_sink_vtbl;

static bool
verify_hr (HRESULT hr, const char *msg)
{
  if (FAILED (hr))
    {
      DebPrint (("DirectWrite HRESULT failed: (%d) %s\n", hr, msg));
      eassert (SUCCEEDED (hr));
      return false;
    }
  return true;
}

/* Gets a IDWriteFontFace from a struct font (its HFONT).  Returns the
   font size in points.  It may fail to get a DirectWrite font, and face
   will be NULL on return.  This happens for some fonts like Courier.

   Never call Release on the result, as it is cached for reuse on the
   struct font.  */
static float
get_font_face (struct font *infont, IDWriteFontFace **face)
{
  HRESULT hr;
  LOGFONTW logfont;
  IDWriteFont *font;

  struct uniscribe_font_info *uniscribe_font
    = (struct uniscribe_font_info *) infont;

  /* Check the cache.  */
  *face = uniscribe_font->dwrite_cache;
  if (*face)
    return uniscribe_font->dwrite_font_size;

  GetObjectW (FONT_HANDLE (infont), sizeof (LOGFONTW), &logfont);

  hr = gdi_interop->lpVtbl->CreateFontFromLOGFONT (gdi_interop,
						   (const LOGFONTW *) &logfont,
						   &font);

  if (!verify_hr (hr, "Failed to CreateFontFromLOGFONT"))
    {
      uniscribe_font->dwrite_skip_font = true;
      *face = NULL;
      return 0.0;
    }

  hr = font->lpVtbl->CreateFontFace (font, face);
  RELEASE_COM (font);
  if (!verify_hr (hr, "Failed to create DWriteFontFace"))
    {
      uniscribe_font->dwrite_skip_font = true;
      *face = NULL;
      return 0.0;
    }

  /* Cache this FontFace.  */
  uniscribe_font->dwrite_font_size = eabs (logfont.lfHeight);
  uniscribe_font->dwrite_cache = *face;

  return eabs (logfont.lfHeight);
}

void
w32_dwrite_free_cached_face (void *cache)
{
  if (cache)
    RELEASE_COM (cache);
}

static float
convert_metrics_sz (int sz, float font_size, int units_per_em)
{
  return (float) sz * font_size / units_per_em;
}


/* If the caller does not need ascent/descent information, it should pass
   NEED_ASCENT_DESCENT = false.  This is used to avoid the overhead of
   calling GetGlyphRunOutline.  */

static bool
text_extents_internal (IDWriteFontFace *dwrite_font_face,
		       bool need_ascent_descent,
		       float font_size, const unsigned *code,
		       int nglyphs, struct font_metrics *metrics)
{
  HRESULT hr;

  USE_SAFE_ALLOCA;

  DWRITE_FONT_METRICS dwrite_font_metrics;
  dwrite_font_face->lpVtbl->GetMetrics (dwrite_font_face,
					&dwrite_font_metrics);

  UINT16 *indices = SAFE_ALLOCA (nglyphs * sizeof (UINT16));
  for (int i = 0; i < nglyphs; i++)
    indices[i] = code[i];

  DWRITE_GLYPH_METRICS *gmetrics
    = SAFE_ALLOCA (nglyphs * sizeof (DWRITE_GLYPH_METRICS));

  hr = dwrite_font_face->lpVtbl->GetGdiCompatibleGlyphMetrics (dwrite_font_face,
							       font_size,
							       1.0,
							       NULL,
							       TRUE,
							       indices,
							       nglyphs,
							       gmetrics,
							       false);

  /* E_INVALIDARG means some of the glyphs index is out of bounds for the font.  */
  if (hr == E_INVALIDARG)
    {
      SAFE_FREE ();
      return false;
    }

  if (!verify_hr (hr, "Failed to GetGdiCompatibleGlyphMetrics"))
    {
      SAFE_FREE ();
      return false;
    }

  float width = 0;
  int du_per_em = dwrite_font_metrics.designUnitsPerEm;

  for (int i = 0; i < nglyphs; i++)
    {
      float advance
	= convert_metrics_sz (gmetrics[i].advanceWidth, font_size, du_per_em);

      width += advance;

      float lbearing
	= round (convert_metrics_sz (gmetrics[i].leftSideBearing, font_size,
				     du_per_em));
      float rbearing
	= round (advance -
		 convert_metrics_sz (gmetrics[i].rightSideBearing,
				     font_size, du_per_em));
      if (i == 0)
	{
	  metrics->lbearing = lbearing;
	  metrics->rbearing = rbearing;
	}
      if (metrics->lbearing > lbearing)
	metrics->lbearing = lbearing;
      if (metrics->rbearing < rbearing)
	metrics->rbearing = rbearing;
    }
  metrics->width = round (width);

  if (need_ascent_descent)
    {
      dwrite_geometry_sink.min_y = FLT_MAX;
      dwrite_geometry_sink.max_y = -FLT_MAX;
      dwrite_geometry_sink.empty = 1;

      hr = dwrite_font_face->lpVtbl->GetGlyphRunOutline (dwrite_font_face,
							 font_size,
							 indices,
							 NULL,
							 NULL,
							 nglyphs,
							 FALSE,
							 FALSE,
							 &dwrite_geometry_sink.sink);

      if (!verify_hr (hr, "Failed to GetGlyhRunOutline"))
	{
	  SAFE_FREE ();
	  return false;
	}

      if (dwrite_geometry_sink.empty)
	{
	  dwrite_geometry_sink.min_y = 0;
	  dwrite_geometry_sink.max_y = 0;
	}

      metrics->ascent = (int) round (-dwrite_geometry_sink.min_y);
      metrics->descent = (int) round (dwrite_geometry_sink.max_y);
    }

  SAFE_FREE ();
  return true;
}

unsigned
w32_dwrite_encode_char (struct font *font, int c)
{
  HRESULT hr;
  IDWriteFontFace *dwrite_font_face;
  UINT16 index;

  get_font_face (font, &dwrite_font_face);
  if (dwrite_font_face == NULL)
    return FONT_INVALID_CODE;
  hr = dwrite_font_face->lpVtbl->GetGlyphIndices (dwrite_font_face,
						  (UINT32 *) &c, 1, &index);
  if (verify_hr (hr, "Failed to GetGlyphIndices"))
    {
      if (index == 0)
	return FONT_INVALID_CODE;
      return index;
    }
  ((struct uniscribe_font_info *) font)->dwrite_skip_font = true;
  return FONT_INVALID_CODE;
}

bool
w32_dwrite_text_extents (struct font *font, const unsigned *code, int nglyphs,
			 struct font_metrics *metrics)
{
  IDWriteFontFace *dwrite_font_face;

  float font_size = get_font_face (font, &dwrite_font_face);

  if (dwrite_font_face == NULL)
    return false;

  /* We can get fonts with a size of 0.  GDI handles this by using a default
     size.  We do the same.  */
  if (font_size <= 0.0f)
    font_size = FRAME_LINE_HEIGHT (SELECTED_FRAME ());

  metrics->ascent = font->ascent;
  metrics->descent = font->descent;

  return text_extents_internal (dwrite_font_face, true,
				font_size, code, nglyphs,
				metrics);
}

/* Never call Release on the value returned by this function, as it is
   reused.  */
static IDWriteBitmapRenderTarget *
get_bitmap_render_target (HDC hdc, int width, int height)
{
  HRESULT hr;
  static IDWriteBitmapRenderTarget *brt = NULL;
  static SIZE size = {0, 0};

  if (brt)
    {
      /* Check if we need to make a bigger one.  */
      if (width <= size.cx && height <= size.cy)
	return brt;
      RELEASE_COM (brt);
    }

  if (width > size.cx)
    size.cx = width;
  if (height > size.cy)
    size.cy = height;

  hr = gdi_interop->lpVtbl->CreateBitmapRenderTarget (gdi_interop,
						      hdc,
						      size.cx, size.cy,
						      &brt);
  if (!verify_hr (hr, "Failed to CreateBitmapRenderTarget"))
    return NULL;

  /* We handle high dpi displays by increasing font size, so override
     PixelsPerDip.  */
  brt->lpVtbl->SetPixelsPerDip (brt, 1.0);

  /* The SetTextAntialiasMode method is only available in
     IDWriteBitmapRenderTarget1.  */
  IDWriteBitmapRenderTarget1 *brt1;
  hr = brt->lpVtbl->QueryInterface (brt,
				    &IID_IDWriteBitmapRenderTarget1,
				    (void **) &brt1);
  /* This error should not happen, but is not catastrofic  */
  if (verify_hr (hr, "Failed to QueryInterface for IDWriteBitmapRenderTarget1"))
    {
      brt1->lpVtbl->SetTextAntialiasMode (brt1, ANTIALIAS_MODE);
      RELEASE_COM (brt1);
    }

  return brt;
}

void
w32_initialize_direct_write (void)
{
  direct_write_available = false;

  if (dwrite_factory)
    {
      RELEASE_COM (dwrite_factory);
      RELEASE_COM (dwrite_factory2);
      RELEASE_COM (gdi_interop);
      RELEASE_COM (rendering_params);
    }

  HMODULE direct_write = LoadLibrary ("dwrite.dll");
  if (!direct_write)
    return;

  /* This is only used here, no need to define it globally.  */
  typedef HRESULT (WINAPI *DWCreateFactory) (DWRITE_FACTORY_TYPE,
					     REFIID, IUnknown **);

  DWCreateFactory dw_create_factory
    = (DWCreateFactory) get_proc_addr (direct_write,
				       "DWriteCreateFactory");

  if (!dw_create_factory)
    {
      FreeLibrary (direct_write);
      return;
    }

  HRESULT hr = dw_create_factory (DWRITE_FACTORY_TYPE_SHARED,
				  &IID_IDWriteFactory,
				  (IUnknown **) &dwrite_factory);
  if (FAILED (hr))
    {
      DebPrint (("DirectWrite HRESULT failed: (%d) CreateFactory\n", hr));
      FreeLibrary (direct_write);
      eassert (SUCCEEDED (hr));
      return;
    }

  /* IDWriteFactory2 is only available on Windows 8.1 and later.
     Without this, we can't use color fonts.  So we disable DirectWrite
     if it is not available.  */
  hr = dwrite_factory->lpVtbl->QueryInterface (dwrite_factory,
					       &IID_IDWriteFactory2,
					       (void **) &dwrite_factory2);

  if (FAILED (hr))
    {
      DebPrint (("DirectWrite HRESULT failed: (%d) QueryInterface IDWriteFactory2\n", hr));
      RELEASE_COM (dwrite_factory);
      FreeLibrary (direct_write);
      return;
    }

  hr = dwrite_factory->lpVtbl->GetGdiInterop (dwrite_factory,
					      &gdi_interop);
  if (FAILED (hr))
    {
      DebPrint (("DirectWrite HRESULT failed: (%d) GetGdiInterop\n", hr));
      RELEASE_COM (dwrite_factory);
      RELEASE_COM (dwrite_factory2);
      FreeLibrary (direct_write);
      eassert (SUCCEEDED (hr));
      return;
    }

  IDWriteRenderingParams *def;

  hr = dwrite_factory->lpVtbl->CreateRenderingParams (dwrite_factory,
						      &def);
  if (FAILED (hr))
    {
      DebPrint (("DirectWrite HRESULT failed: (%d) CreateRenderingParams\n", hr));
      RELEASE_COM (dwrite_factory);
      RELEASE_COM (dwrite_factory2);
      RELEASE_COM (gdi_interop);
      FreeLibrary (direct_write);
      eassert (SUCCEEDED (hr));
      return;
    }

  /* range: [0.0, 1.0] */
  if (config_enhanced_contrast < 0.0f || config_enhanced_contrast > 1.0f)
    config_enhanced_contrast = def->lpVtbl->GetEnhancedContrast (def);

  /* range: [0.0, 1.0]  */
  if (config_clear_type_level < 0.0f || config_clear_type_level > 1.0f)
     config_clear_type_level = def->lpVtbl->GetClearTypeLevel (def);

  /* range: (0.0, 256.0] */
  /* We change the default value of 2.2 for gamma to 1.4, that looks
     very similar to GDI.  The default looks too dim for emacs,
     subjectively.  */
  if (config_gamma <= 0.0f || config_gamma > 256.0f)
    config_gamma = 1.4; /* def->lpVtbl->GetGamma (def);  */

  hr = dwrite_factory->lpVtbl->CreateCustomRenderingParams (dwrite_factory,
							    config_gamma,
							    config_enhanced_contrast,
							    config_clear_type_level,
							    def->lpVtbl->GetPixelGeometry (def),
							    RENDERING_MODE,
							    &rendering_params);

  RELEASE_COM (def);

  if (FAILED (hr))
    {
      DebPrint (("DirectWrite HRESULT failed: (%d)"
		 " CreateCustomRenderingParams\n", hr));
      RELEASE_COM (dwrite_factory);
      RELEASE_COM (dwrite_factory2);
      RELEASE_COM (gdi_interop);
      FreeLibrary (direct_write);
      eassert (SUCCEEDED (hr));
      return;
    }

  dwrite_geometry_sink.sink.lpVtbl = &dwrite_geometry_sink_vtbl;

#ifdef MINGW_W64
  dwrite_geometry_sink_vtbl.Base.AddRef = geometry_sink_AddRef;
  dwrite_geometry_sink_vtbl.Base.Release = geometry_sink_Release;
  dwrite_geometry_sink_vtbl.Base.QueryInterface =
    geometry_sink_QueryInterface;
#else
  dwrite_geometry_sink_vtbl.AddRef = (void *) geometry_sink_AddRef;
  dwrite_geometry_sink_vtbl.Release = (void *) geometry_sink_Release;
  dwrite_geometry_sink_vtbl.QueryInterface
    = (void *) geometry_sink_QueryInterface;
#endif

  dwrite_geometry_sink_vtbl.AddBeziers = geometry_sink_AddBeziers;
  dwrite_geometry_sink_vtbl.AddLines = geometry_sink_AddLines;
  dwrite_geometry_sink_vtbl.BeginFigure = geometry_sink_BeginFigure;
  dwrite_geometry_sink_vtbl.EndFigure = geometry_sink_EndFigure;
  dwrite_geometry_sink_vtbl.Close = geometry_sink_Close;
  dwrite_geometry_sink_vtbl.SetFillMode = geometry_sink_SetFillMode;
  dwrite_geometry_sink_vtbl.SetSegmentFlags = geometry_sink_SetSegmentFlags;

  direct_write_available = true;
  w32_inhibit_dwrite = false;
}

bool
w32_dwrite_draw (HDC hdc, int x, int y, unsigned *glyphs, int len,
		 COLORREF color, struct font *font)
{
  HRESULT hr;
  IDWriteFontFace *dwrite_font_face;

  USE_SAFE_ALLOCA;

  struct uniscribe_font_info *uniscribe_font
    = (struct uniscribe_font_info *) font;

  /* What we get as y is the baseline position.  */
  y -= font->ascent;

  float font_size = get_font_face (font, &dwrite_font_face);
  if (dwrite_font_face == NULL)
    return false;

  struct font_metrics metrics;
  if (!text_extents_internal (dwrite_font_face, false, font_size, glyphs, len,
			      &metrics))
    {
      uniscribe_font->dwrite_skip_font = true;
      return false;
    }

  int left_margin = metrics.lbearing < 0 ? -metrics.lbearing : 0;

  int bitmap_width = left_margin + metrics.width + metrics.rbearing;
  int bitmap_height = font->ascent + font->descent;

  /* We never release this, get_bitmap_render_target reuses it.  */
  IDWriteBitmapRenderTarget *bitmap_render_target =
    get_bitmap_render_target (hdc, bitmap_width, bitmap_height);

  /* If this fails, completely disable DirectWrite.  */
  if (bitmap_render_target == NULL)
    {
      direct_write_available = false;
      return false;
    }

  /* This DC can't be released.  */
  HDC text_dc
    = bitmap_render_target->lpVtbl->GetMemoryDC (bitmap_render_target);

  /* Copy the background pixel to the render target bitmap.  */
  BitBlt (text_dc, 0, 0, bitmap_width, bitmap_height, hdc, x - left_margin, y, SRCCOPY);

  UINT16 *indices = SAFE_ALLOCA (len * sizeof (UINT16));

  for (int i = 0; i < len; i++)
    indices[i] = glyphs[i];

  FLOAT *advances = SAFE_ALLOCA (len * sizeof (FLOAT));

  for (int i = 0; i < len; i++)
    {
      if (!text_extents_internal (dwrite_font_face, false, font_size, glyphs + i, 1,
				  &metrics))
	{
	  uniscribe_font->dwrite_skip_font = true;
	  SAFE_FREE ();
	  return false;
	}
      advances[i] = metrics.width;
    }

  DWRITE_GLYPH_RUN glyph_run;
  glyph_run.fontFace = dwrite_font_face;
  glyph_run.fontEmSize = font_size;
  glyph_run.glyphIndices = indices;
  glyph_run.glyphCount = len;
  glyph_run.isSideways = false;
  glyph_run.bidiLevel = 0;	/* we reorder bidi text ourselves */
  glyph_run.glyphOffsets = NULL;
  glyph_run.glyphAdvances = advances;

  IDWriteColorGlyphRunEnumerator *layers;
  /* This call will tell us if we have to handle any color glyphs.  */
  hr = dwrite_factory2->lpVtbl->TranslateColorGlyphRun (dwrite_factory2,
							left_margin, font->ascent,
							&glyph_run,
							NULL,
							MEASURING_MODE,
							NULL,
							0,
							&layers);

  /* No color.  Just draw the GlyphRun.  */
  if (hr == DWRITE_E_NOCOLOR)
    bitmap_render_target->lpVtbl->DrawGlyphRun (bitmap_render_target,
						left_margin, font->ascent,
						MEASURING_MODE,
						&glyph_run,
						rendering_params,
						color,
						NULL);
  else
    {
      /* If there were color glyphs, 'layers' contains a list of
	 GlyphRun with a color and a position for each.  We draw them
	 individually.  */
      if (!verify_hr (hr, "Failed at TranslateColorGlyphRun"))
	{
	  uniscribe_font->dwrite_skip_font = true;
	  RELEASE_COM (layers);
	  SAFE_FREE ();
	  return false;
	}
      for (;;)
	{
	  HRESULT hr;
	  BOOL more_layers;
	  const DWRITE_COLOR_GLYPH_RUN *layer;

	  hr = layers->lpVtbl->MoveNext (layers, &more_layers);
	  if (!verify_hr (hr, "Failed at MoveNext"))
	    {
	      uniscribe_font->dwrite_skip_font = true;
	      RELEASE_COM (layers);
	      SAFE_FREE ();
	      return false;
	    }
	  if (!more_layers)
	    break;
	  hr = layers->lpVtbl->GetCurrentRun (layers, &layer);
	  if (!verify_hr (hr, "Failed at GetCurrentRun"))
	    {
	      uniscribe_font->dwrite_skip_font = true;
	      RELEASE_COM (layers);
	      SAFE_FREE ();
	      return false;
	    }
	  hr = bitmap_render_target->lpVtbl->DrawGlyphRun
	    (bitmap_render_target,
	     layer->baselineOriginX,
	     layer->baselineOriginY,
	     MEASURING_MODE,
	     &layer->glyphRun,
	     rendering_params,
	     RGB (layer->runColor.r * 255,
		  layer->runColor.g * 255,
		  layer->runColor.b * 255),
	     NULL);
	  if (!verify_hr (hr, "Failed at GetCurrentRun"))
	    {
	      uniscribe_font->dwrite_skip_font = true;
	      RELEASE_COM (layers);
	      SAFE_FREE ();
	      return false;
	    }
	}
      RELEASE_COM (layers);
    }

  /* Finally, copy the rendered text back to the original DC.  */
  BitBlt (hdc, x - left_margin, y, bitmap_width, bitmap_height, text_dc, 0, 0, SRCCOPY);
  SAFE_FREE ();
  return true;
}

/* Returns true if DirectWrite is to be used:
   - It is available.
   - The font is handled by HarfBuzz.
   - w32-inhibit-dwrite is false.
   - The font has not been marked after a failed DirectWrite operation.
*/
bool
w32_use_direct_write (struct w32font_info *w32font)
{
#ifdef HAVE_HARFBUZZ
  return (direct_write_available
	  && w32font->font.driver == &harfbuzz_font_driver
	  && !w32_inhibit_dwrite
	  && !((struct uniscribe_font_info *) w32font)->dwrite_skip_font);
#else
  return false;
#endif
}

DEFUN ("w32-dwrite-available", Fw32_dwrite_available, Sw32_dwrite_available, 0, 0, 0,
       doc: /* Returns t if DirectWrite is available.
DirectWrite will be used if it is available and 'w32-inhibit-dwrite' is nil.  */)
  (void)
{
  return direct_write_available ? Qt : Qnil;
}

DEFUN ("w32-dwrite-reinit", Fw32_dwrite_reinit, Sw32_dwrite_reinit, 0, 3, 0,
       doc: /* Reinitialize DirectWrite with the given parameters.
If a parameter is not specified, or is out of range, it will take a default
value.

Return value is nil.

ENHANCED_CONTRAST is in the range [0.0, 1.0], and defaults to 0.5.
CLEAR_TYPE_LEVEL is in the range [0.0, 1.0], and defaults to 1.0.
GAMMA is in the range (0.0, 256.0], and defaults to a system-dependent value
      around 2.0 (sometimes 1.8, sometimes 2.2).  */)
  (Lisp_Object enhanced_contrast, Lisp_Object clear_type_level,
   Lisp_Object gamma)
{
  config_enhanced_contrast = -1.0f;
  if (FLOATP (enhanced_contrast))
    config_enhanced_contrast = XFLOAT_DATA (enhanced_contrast);
  if (FIXNUMP (enhanced_contrast))
    config_enhanced_contrast = XFIXNUM (enhanced_contrast);

  config_clear_type_level = -1.0f;
  if (FLOATP (clear_type_level))
    config_clear_type_level = XFLOAT_DATA (clear_type_level);
  if (FIXNUMP (clear_type_level))
    config_clear_type_level = XFIXNUM (clear_type_level);

  config_gamma = -1.0f;
  if (FLOATP (gamma))
    config_gamma = XFLOAT_DATA (gamma);
  if (FIXNUMP (gamma))
    config_gamma = XFIXNUM (gamma);

  w32_initialize_direct_write ();

  return Qnil;
}

void
syms_of_w32dwrite (void)
{
  DEFVAR_BOOL ("w32-inhibit-dwrite", w32_inhibit_dwrite,
	       doc: /* If t, don't use DirectWrite.  */);
  /* The actual value is determined at startup in
     w32_initialize_direct_write, which is called from
     syms_of_w32uniscribe_for_pdumper.  */
  w32_inhibit_dwrite = false;

  defsubr (&Sw32_dwrite_reinit);
  defsubr (&Sw32_dwrite_available);
}
