{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2011-2022 Google LLC.                                    }
{ Copyright (c) 2021-2022 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by a BSD-style license that can be }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Skia.API;

interface

{$ALIGN ON}
{$MINENUMSIZE 4}

{$IF NOT DEFINED(IOS) or NOT DEFINED(CPUARM)}
  {$DEFINE SK_DYNAMIC_LOADING}
{$ENDIF}

{$IFDEF SK_DYNAMIC_LOADING}

uses
  { Delphi }
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  System.SysUtils;

{$ENDIF}

type
  {$REGION 'C types'}

  bool       = System.Boolean;
  {$IF CompilerVersion < 31}
  char       = System.AnsiChar;
  {$ELSE}
  char       = System.UTF8Char;
  {$ENDIF}
  char16_t   = System.Char;
  double     = System.Double;
  float      = System.Single;
  int16_t    = System.SmallInt;
  int32_t    = System.Integer;
  int64_t    = System.Int64;
  int8_t     = System.ShortInt;
  intptr_t   = System.NativeInt;
  long       = System.LongInt;
  size_t     = System.NativeUInt;
  uint16_t   = System.Word;
  uint32_t   = System.Cardinal;
  uint64_t   = System.UInt64;
  uint8_t    = System.Byte;
  uintptr_t  = System.NativeUInt;

  pbool      = ^bool;
  pchar      = ^char;
  pchar16_t  = ^char16_t;
  pdouble    = ^double;
  pfloat     = ^float;
  pint16_t   = ^int16_t;
  pint32_t   = ^int32_t;
  pint64_t   = ^int64_t;
  pint8_t    = ^int8_t;
  pintptr_t  = ^intptr_t;
  plong      = ^long;
  psize_t    = ^size_t;
  puint16_t  = ^uint16_t;
  puint32_t  = ^uint32_t;
  puint64_t  = ^uint64_t;
  puint8_t   = ^uint8_t;
  puintptr_t = ^uintptr_t;

  {$ENDREGION}

  {$REGION 'Skia types'}

  {$REGION 'include/c/sk4d_types.h'}
  gr_backendformat_t            = THandle;
  gr_backendrendertarget_t      = THandle;
  gr_backendtexture_t           = THandle;
  gr_directcontext_t            = THandle;
  sk_animcodecplayer_t          = THandle;
  sk_blender_t                  = THandle;
  sk_canvas_t                   = THandle;
  sk_codec_t                    = THandle;
  sk_colorfilter_t              = THandle;
  sk_colorspace_t               = THandle;
  sk_colorspaceiccprofile_t     = THandle;
  sk_data_t                     = THandle;
  sk_document_t                 = THandle;
  sk_dynamicmemorywstream_t     = THandle;
  sk_filestream_t               = THandle;
  sk_filewstream_t              = THandle;
  sk_font_t                     = THandle;
  sk_fontmgr_t                  = THandle;
  sk_image_t                    = THandle;
  sk_imagefilter_t              = THandle;
  sk_maskfilter_t               = THandle;
  sk_memorystream_t             = THandle;
  sk_opbuilder_t                = THandle;
  sk_paint_t                    = THandle;
  sk_path_t                     = THandle;
  sk_pathbuilder_t              = THandle;
  sk_patheffect_t               = THandle;
  sk_pathiterator_t             = THandle;
  sk_pathmeasure_t              = THandle;
  sk_pathrawiter_t              = THandle;
  sk_picture_t                  = THandle;
  sk_picturerecorder_t          = THandle;
  sk_pixmap_t                   = THandle;
  sk_refcnt_t                   = THandle;
  sk_region_t                   = THandle;
  sk_regioncliperator_t         = THandle;
  sk_regioniterator_t           = THandle;
  sk_regionspanerator_t         = THandle;
  sk_rrect_t                    = THandle;
  sk_runtimeeffect_t            = THandle;
  sk_shader_t                   = THandle;
  sk_stream_t                   = THandle;
  sk_streamadapter_t            = THandle;
  sk_string_t                   = THandle;
  sk_surface_t                  = THandle;
  sk_textblob_t                 = THandle;
  sk_tracememorydump_t          = THandle;
  sk_tracememorydumpbaseclass_t = THandle;
  sk_typeface_t                 = THandle;
  sk_vertices_t                 = THandle;
  sk_wstream_t                  = THandle;
  sk_wstreamadapter_t           = THandle;

  pgr_backendformat_t            = ^gr_backendformat_t;
  pgr_backendrendertarget_t      = ^gr_backendrendertarget_t;
  pgr_backendtexture_t           = ^gr_backendtexture_t;
  pgr_directcontext_t            = ^gr_directcontext_t;
  psk_animcodecplayer_t          = ^sk_animcodecplayer_t;
  psk_blender_t                  = ^sk_blender_t;
  psk_canvas_t                   = ^sk_canvas_t;
  psk_codec_t                    = ^sk_codec_t;
  psk_colorfilter_t              = ^sk_colorfilter_t;
  psk_colorspace_t               = ^sk_colorspace_t;
  psk_colorspaceiccprofile_t     = ^sk_colorspaceiccprofile_t;
  psk_data_t                     = ^sk_data_t;
  psk_document_t                 = ^sk_document_t;
  psk_dynamicmemorywstream_t     = ^sk_dynamicmemorywstream_t;
  psk_filestream_t               = ^sk_filestream_t;
  psk_filewstream_t              = ^sk_filewstream_t;
  psk_font_t                     = ^sk_font_t;
  psk_fontmgr_t                  = ^sk_fontmgr_t;
  psk_image_t                    = ^sk_image_t;
  psk_imagefilter_t              = ^sk_imagefilter_t;
  psk_maskfilter_t               = ^sk_maskfilter_t;
  psk_memorystream_t             = ^sk_memorystream_t;
  psk_opbuilder_t                = ^sk_opbuilder_t;
  psk_paint_t                    = ^sk_paint_t;
  psk_path_t                     = ^sk_path_t;
  psk_pathbuilder_t              = ^sk_pathbuilder_t;
  psk_patheffect_t               = ^sk_patheffect_t;
  psk_pathiterator_t             = ^sk_pathiterator_t;
  psk_pathmeasure_t              = ^sk_pathmeasure_t;
  psk_pathrawiter_t              = ^sk_pathrawiter_t;
  psk_picture_t                  = ^sk_picture_t;
  psk_picturerecorder_t          = ^sk_picturerecorder_t;
  psk_pixmap_t                   = ^sk_pixmap_t;
  psk_refcnt_t                   = ^sk_refcnt_t;
  psk_region_t                   = ^sk_region_t;
  psk_regioncliperator_t         = ^sk_regioncliperator_t;
  psk_regioniterator_t           = ^sk_regioniterator_t;
  psk_regionspanerator_t         = ^sk_regionspanerator_t;
  psk_rrect_t                    = ^sk_rrect_t;
  psk_runtimeeffect_t            = ^sk_runtimeeffect_t;
  psk_shader_t                   = ^sk_shader_t;
  psk_stream_t                   = ^sk_stream_t;
  psk_streamadapter_t            = ^sk_streamadapter_t;
  psk_string_t                   = ^sk_string_t;
  psk_surface_t                  = ^sk_surface_t;
  psk_textblob_t                 = ^sk_textblob_t;
  psk_tracememorydump_t          = ^sk_tracememorydump_t;
  psk_tracememorydumpbaseclass_t = ^sk_tracememorydumpbaseclass_t;
  psk_typeface_t                 = ^sk_typeface_t;
  psk_vertices_t                 = ^sk_vertices_t;
  psk_wstream_t                  = ^sk_wstream_t;
  psk_wstreamadapter_t           = ^sk_wstreamadapter_t;

  gr_backendapi_t = (
    OPEN_GL_GR_BACKENDAPI,
    METAL_GR_BACKENDAPI = 2
  );
  pgr_backendapi_t = ^gr_backendapi_t;

  gr_surfaceorigin_t = (
    TOP_LEFT_GR_SURFACEORIGIN,
    BOTTOM_LEFT_GR_SURFACEORIGIN
  );
  pgr_surfaceorigin_t = ^gr_surfaceorigin_t;

  sk_alphatype_t = (
    UNKNOWN_SK_ALPHATYPE,
    OPAQUE_SK_ALPHATYPE,
    PREMUL_SK_ALPHATYPE,
    UNPREMUL_SK_ALPHATYPE
  );
  psk_alphatype_t = ^sk_alphatype_t;

  sk_blendmode_t = (
    CLEAR_SK_BLENDMODE,
    SRC_SK_BLENDMODE,
    DEST_SK_BLENDMODE,
    SRC_OVER_SK_BLENDMODE,
    DEST_OVER_SK_BLENDMODE,
    SRC_IN_SK_BLENDMODE,
    DEST_IN_SK_BLENDMODE,
    SRC_OUT_SK_BLENDMODE,
    DEST_OUT_SK_BLENDMODE,
    SRC_A_TOP_SK_BLENDMODE,
    DEST_A_TOP_SK_BLENDMODE,
    XOR_SK_BLENDMODE,
    PLUS_SK_BLENDMODE,
    MODULATE_SK_BLENDMODE,
    SCREEN_SK_BLENDMODE,
    OVERLAY_SK_BLENDMODE,
    DARKEN_SK_BLENDMODE,
    LIGHTEN_SK_BLENDMODE,
    COLOR_DODGE_SK_BLENDMODE,
    COLOR_BURN_SK_BLENDMODE,
    HARD_LIGHT_SK_BLENDMODE,
    SOFT_LIGHT_SK_BLENDMODE,
    DIFFERENCE_SK_BLENDMODE,
    EXCLUSION_SK_BLENDMODE,
    MULTIPLY_SK_BLENDMODE,
    HUE_SK_BLENDMODE,
    SATURATION_SK_BLENDMODE,
    COLOR_SK_BLENDMODE,
    LUMINOSITY_SK_BLENDMODE
  );
  psk_blendmode_t = ^sk_blendmode_t;

  sk_blurstyle_t = (
    NORMAL_SK_BLURSTYLE,
    SOLID_SK_BLURSTYLE,
    OUTER_SK_BLURSTYLE,
    INNER_SK_BLURSTYLE
  );
  psk_blurstyle_t = ^sk_blurstyle_t;

  sk_clipop_t = (
    DIFFERENCE_SK_CLIPOP,
    INTERSECT_SK_CLIPOP
  );
  psk_clipop_t = ^sk_clipop_t;

  sk_colorchannel_t = (
    R_SK_COLORCHANNEL,
    G_SK_COLORCHANNEL,
    B_SK_COLORCHANNEL,
    A_SK_COLORCHANNEL
  );
  psk_colorchannel_t = ^sk_colorchannel_t;

  sk_colortype_t = (
    UNKNOWN_SK_COLORTYPE,
    ALPHA8_SK_COLORTYPE,
    RGB565_SK_COLORTYPE,
    ARGB4444_SK_COLORTYPE,
    RGBA8888_SK_COLORTYPE,
    RGB888X_SK_COLORTYPE,
    BGRA8888_SK_COLORTYPE,
    RGBA1010102_SK_COLORTYPE,
    BGRA1010102_SK_COLORTYPE,
    RGB101010X_SK_COLORTYPE,
    BGR101010X_SK_COLORTYPE,
    GRAY8_SK_COLORTYPE,
    RGBAF16_SK_COLORTYPE,
    RGBAF16CLAMPED_SK_COLORTYPE,
    RGBAF32_SK_COLORTYPE,
    RG88_SK_COLORTYPE,
    ALPHAF16_SK_COLORTYPE,
    RGF16_SK_COLORTYPE,
    ALPHA16_SK_COLORTYPE,
    RG1616_SK_COLORTYPE,
    RGBA16161616_SK_COLORTYPE,
    SRGBA8888_SK_COLORTYPE
  );
  psk_colortype_t = ^sk_colortype_t;

  sk_drawpointsmode_t = (
    POINTS_SK_DRAWPOINTSMODE,
    LINES_SK_DRAWPOINTSMODE,
    POLYGON_DRAWPOINTSMODE
  );
  psk_drawpointsmode_t = ^sk_drawpointsmode_t;

  sk_encodedimageformat_t = (
    BMP_SK_ENCODEDIMAGEFORMAT,
    GIF_SK_ENCODEDIMAGEFORMAT,
    ICO_SK_ENCODEDIMAGEFORMAT,
    JPEG_SK_ENCODEDIMAGEFORMAT,
    PNG_SK_ENCODEDIMAGEFORMAT,
    WBMP_SK_ENCODEDIMAGEFORMAT,
    WEBP_SK_ENCODEDIMAGEFORMAT,
    PKM_SK_ENCODEDIMAGEFORMAT,
    KTX_SK_ENCODEDIMAGEFORMAT,
    ASTC_SK_ENCODEDIMAGEFORMAT,
    DNG_SK_ENCODEDIMAGEFORMAT,
    HEIF_SK_ENCODEDIMAGEFORMAT,
    AVIF_SK_ENCODEDIMAGEFORMAT
  );
  psk_encodedimageformat_t = ^sk_encodedimageformat_t;

  sk_filtermode_t = (
    NEAREST_SK_FILTERMODE,
    LINEAR_SK_FILTERMODE
  );
  psk_filtermode_t = ^sk_filtermode_t;

  sk_fontedging_t = (
    ALIAS_SK_FONTEDGING,
    ANTI_ALIAS_SK_FONTEDGING,
    SUBPIXEL_ANTI_ALIAS_SK_FONTEDGING
  );
  psk_fontedging_t = ^sk_fontedging_t;

  sk_fonthinting_t = (
    NONE_SK_FONTHINTING,
    SLIGHT_SK_FONTHINTING,
    NORMAL_SK_FONTHINTING,
    FULL_SK_FONTHINTING
  );
  psk_fonthinting_t = ^sk_fonthinting_t;

  sk_fontslant_t = (
    UPRIGHT_SK_FONTSLANT,
    ITALIC_SK_FONTSLANT,
    OBLIQUE_SK_FONTSLANT
  );
  psk_fontslant_t = ^sk_fontslant_t;

  sk_contrastinvertstyle_t = (
    NO_INVERT_SK_CONTRASTINVERTSTYLE,
    INVERT_BRIGHTNESS_SK_CONTRASTINVERTSTYLE,
    INVERT_LIGHTNESS_SK_CONTRASTINVERTSTYLE
  );
  psk_contrastinvertstyle_t = ^sk_contrastinvertstyle_t;

  sk_imagecachinghint_t = (
    ALLOW_SK_IMAGECACHINGHINT,
    DISALLOW_SK_IMAGECACHINGHINT
  );
  psk_imagecachinghint_t = ^sk_imagecachinghint_t;

  sk_latticerecttype_t = (
    DEFAULT_SK_LATTICERECTTYPE,
    TRANSPARENT_SK_LATTICERECTTYPE,
    FIXED_COLOR_SK_LATTICERECTTYPE
  );
  psk_latticerecttype_t = ^sk_latticerecttype_t;

  sk_mipmapmode_t = (
    NONE_SK_MIPMAPMODE,
    NEAREST_SK_MIPMAPMODE,
    LINEAR_SK_MIPMAPMODE
  );
  psk_mipmapmode_t = ^sk_mipmapmode_t;

  sk_paintstyle_t = (
    FILL_SK_PAINTSTYLE,
    STROKE_SK_PAINTSTYLE,
    STROKE_AND_FILL_SK_PAINTSTYLE
  );
  psk_paintstyle_t = ^sk_paintstyle_t;

  sk_patharcsize_t = (
    SMALL_SK_ARCSIZE,
    LARGE_SK_ARCSIZE
  );
  psk_patharcsize_t = ^sk_patharcsize_t;

  sk_pathdirection_t = (
    CW_SK_PATHDIRECTION,
    CCW_SK_PATHDIRECTION
  );
  psk_pathdirection_t = ^sk_pathdirection_t;

  sk_patheffect1dstyle_t = (
    TRANSLATE_SK_PATHEFFECT1DSTYLE,
    ROTATE_SK_PATHEFFECT1DSTYLE,
    MORPH_SK_PATHEFFECT1DSTYLE
  );
  psk_patheffect1dstyle_t = ^sk_patheffect1dstyle_t;

  sk_patheffecttrimmode_t = (
    NORMAL_SK_PATHEFFECTTRIMMODE,
    INVERTED_SK_PATHEFFECTTRIMMODE
  );
  psk_patheffecttrimmode_t = ^sk_patheffecttrimmode_t;

  sk_pathfilltype_t = (
    WINDING_SK_PATHFILLTYPE,
    EVEN_ODD_SK_PATHFILLTYPE,
    INVERSE_WINDING_SK_PATHFILLTYPE,
    INVERSE_EVEN_ODD_SK_PATHFILLTYPE
  );
  psk_pathfilltype_t = ^sk_pathfilltype_t;

  sk_pathop_t = (
    DIFFERENCE_SK_PATHOP,
    INTERSECT_SK_PATHOP,
    UNION_SK_PATHOP,
    XOR_SK_PATHOP,
    REVERSE_DIFFERENCE_SK_PATHOP
  );
  psk_pathop_t = ^sk_pathop_t;

  sk_pathverb_t = (
    MOVE_SK_PATHVERB,
    LINE_SK_PATHVERB,
    QUAD_SK_PATHVERB,
    CONIC_SK_PATHVERB,
    CUBIC_SK_PATHVERB,
    CLOSE_SK_PATHVERB
  );
  psk_pathverb_t = ^sk_pathverb_t;

  sk_pixelgeometry_t = (
    UNKNOWN_SK_PIXELGEOMETRY,
    RGB_HORIZONTAL_SK_PIXELGEOMETRY,
    BGR_HORIZONTAL_SK_PIXELGEOMETRY,
    RGB_VERTICAL_SK_PIXELGEOMETRY,
    BGR_VERTICAL_SK_PIXELGEOMETRY
  );
  psk_pixelgeometry_t = ^sk_pixelgeometry_t;

  sk_regionop_t = (
    DIFFERENCE_SK_REGIONOP,
    INTERSECT_SK_REGIONOP,
    UNION_SK_REGIONOP,
    XOR_SK_REGIONOP,
    REVERSE_DIFFERENCE_SK_REGIONOP,
    REPLACE_SK_REGIONOP
  );
  psk_regionop_t = ^sk_regionop_t;

  sk_rrectcorner_t = (
    UPPER_LEFT_SK_RRECTCORNER,
    UPPER_RIGHT_SK_RRECTCORNER,
    LOWER_RIGHT_SK_RRECTCORNER,
    LOWER_LEFT_SK_RRECTCORNER
  );
  psk_rrectcorner_t = ^sk_rrectcorner_t;

  sk_runtimeeffectchildtype_t = (
    SHADER_SK_RUNTIMEEFFECTCHILDTYPE,
    COLOR_FILTER_SK_RUNTIMEEFFECTCHILDTYPE,
    BLENDER_FILTER_SK_RUNTIMEEFFECTCHILDTYPE
  );
  psk_runtimeeffectchildtype_t = ^sk_runtimeeffectchildtype_t;

  sk_runtimeeffectuniformtype_t = (
    FLOAT_SK_RUNTIMEEFFECTUNIFORMTYPE,
    FLOAT2_SK_RUNTIMEEFFECTUNIFORMTYPE,
    FLOAT3_SK_RUNTIMEEFFECTUNIFORMTYPE,
    FLOAT4_SK_RUNTIMEEFFECTUNIFORMTYPE,
    FLOAT2X2_SK_RUNTIMEEFFECTUNIFORMTYPE,
    FLOAT3X3_SK_RUNTIMEEFFECTUNIFORMTYPE,
    FLOAT4X4_SK_RUNTIMEEFFECTUNIFORMTYPE,
    INT_SK_RUNTIMEEFFECTUNIFORMTYPE,
    INT2_SK_RUNTIMEEFFECTUNIFORMTYPE,
    INT3_SK_RUNTIMEEFFECTUNIFORMTYPE,
    INT4_SK_RUNTIMEEFFECTUNIFORMTYPE
  );
  psk_runtimeeffectuniformtype_t = ^sk_runtimeeffectuniformtype_t;

  sk_srcrectconstraint_t = (
    CLOSE_SK_SRCRECTCONSTRAINT,
    FAST_SK_SRCRECTCONSTRAINT
  );
  psk_srcrectconstraint_t = ^sk_srcrectconstraint_t;

  sk_strokecap_t = (
    BUTT_SK_STROKECAP,
    ROUND_SK_STROKECAP,
    SQUARE_SK_STROKECAP
  );
  psk_strokecap_t = ^sk_strokecap_t;

  sk_strokejoin_t = (
    MITER_SK_STROKEJOIN,
    ROUND_SK_STROKEJOIN,
    BEVEL_SK_STROKEJOIN
  );
  psk_strokejoin_t = ^sk_strokejoin_t;

  sk_textencoding_t = (
    UTF8_SK_TEXTENCODING,
    UTF16_SK_TEXTENCODING,
    UTF32_SK_TEXTENCODING,
    GLYPH_ID_SK_TEXTENCODING
  );
  psk_textencoding_t = ^sk_textencoding_t;

  sk_tilemode_t = (
    CLAMP_SK_TILEMODE,
    REPEAT_SK_TILEMODE,
    MIRROR_SK_TILEMODE,
    DECAL_SK_TILEMODE
  );
  psk_tilemode_t = ^sk_tilemode_t;

  sk_vertexmode_t = (
    TRIANGLES_SK_VERTEXMODE,
    TRIANGLE_STRIP_SK_VERTEXMODE,
    TRIANGLE_FAN_SK_VERTEXMODE
  );
  psk_vertexmode_t = ^sk_vertexmode_t;

  sk_color_t   = uint32_t;
  sk_glyphid_t = uint16_t;
  sk_unichar_t = int32_t;

  psk_color_t   = ^sk_color_t;
  psk_glyphid_t = ^sk_glyphid_t;
  psk_unichar_t = ^sk_unichar_t;

  sk_color4f_t = record
    r: float;
    g: float;
    b: float;
    a: float;
  end;
  psk_color4f_t = ^sk_color4f_t;

	sk_colormatrix_t = record
	  m_11: float;
	  m_12: float;
	  m_13: float;
	  m_14: float;
	  m_15: float;
	  m_21: float;
	  m_22: float;
	  m_23: float;
	  m_24: float;
	  m_25: float;
	  m_31: float;
	  m_32: float;
	  m_33: float;
	  m_34: float;
	  m_35: float;
	  m_41: float;
	  m_42: float;
	  m_43: float;
	  m_44: float;
	  m_45: float;
	end;
	psk_colormatrix_t = ^sk_colormatrix_t;

  sk_colorspaceprimaries_t = record
    rx: float;
    ry: float;
    gx: float;
    gy: float;
    bx: float;
    by: float;
    wx: float;
    wy: float;
  end;
  psk_colorspaceprimaries_t = ^sk_colorspaceprimaries_t;

  sk_colorspacetransferfn_t = record
    g: float;
    a: float;
    b: float;
    c: float;
    d: float;
    e: float;
    f: float;
  end;
  psk_colorspacetransferfn_t = ^sk_colorspacetransferfn_t;

  sk_colorspacexyz_t = record
    m_11: float;
    m_12: float;
    m_13: float;
    m_21: float;
    m_22: float;
    m_23: float;
    m_31: float;
    m_32: float;
    m_33: float;
  end;
  psk_colorspacexyz_t = ^sk_colorspacexyz_t;

  sk_cubicresampler_t = record
    b: float;
    c: float;
  end;
  psk_cubicresampler_t = ^sk_cubicresampler_t;

  sk_datetime_t = record
    time_zone_minutes : int16_t;
    year              : uint16_t;
    month             : uint8_t;
    day_of_week       : uint8_t;
    day               : uint8_t;
    hour              : uint8_t;
    minute            : uint8_t;
    second            : uint8_t;
  end;
  psk_datetime_t = ^sk_datetime_t;

  sk_fontmetrics_t = record
    flags               : uint32_t;
    top                 : float;
    ascent              : float;
    descent             : float;
    bottom              : float;
    leading             : float;
    avg_char_width      : float;
    max_char_width      : float;
    x_min               : float;
    x_max               : float;
    x_height            : float;
    cap_height          : float;
    underline_thickness : float;
    underline_position  : float;
    strikeout_thickness : float;
    strikeout_position  : float;
  end;
  psk_fontmetrics_t = ^sk_fontmetrics_t;

  sk_fontstyle_t = record
    weight : int32_t;
    width  : int32_t;
    slant  : sk_fontslant_t;
  end;
  psk_fontstyle_t = ^sk_fontstyle_t;

  sk_highcontrastconfig_t = record
    grayscale    : bool;
    invert_style : sk_contrastinvertstyle_t;
    contrast     : float;
  end;
  psk_highcontrastconfig_t = ^sk_highcontrastconfig_t;

  sk_imageinfo_t = record
    width       : int32_t;
    height      : int32_t;
    color_type  : sk_colortype_t;
    alpha_type  : sk_alphatype_t;
    color_space : sk_colorspace_t;
  end;
  psk_imageinfo_t = ^sk_imageinfo_t;

  sk_ipoint_t = record
    x: int32_t;
    y: int32_t;
  end;
  psk_ipoint_t = ^sk_ipoint_t;

  sk_irect_t = record
    left   : int32_t;
    top    : int32_t;
    right  : int32_t;
    bottom : int32_t;
  end;
  psk_irect_t = ^sk_irect_t;

  sk_isize_t = record
    width  : int32_t;
    height : int32_t;
  end;
  psk_isize_t = ^sk_isize_t;

  sk_lattice_t = record
    x_divs     : pint32_t;
    y_divs     : pint32_t;
    rect_types : psk_latticerecttype_t;
    x_count    : int32_t;
    y_count    : int32_t;
    bounds     : psk_irect_t;
    colors     : psk_color_t;
  end;
  psk_lattice_t = ^sk_lattice_t;

  sk_matrix_t = record
    m_11: float;
    m_12: float;
    m_13: float;
    m_21: float;
    m_22: float;
    m_23: float;
    m_31: float;
    m_32: float;
    m_33: float;
  end;
  psk_matrix_t = ^sk_matrix_t;

  sk_matrix44_t = record
    m_11: float;
    m_12: float;
    m_13: float;
    m_14: float;
    m_21: float;
    m_22: float;
    m_23: float;
    m_24: float;
    m_31: float;
    m_32: float;
    m_33: float;
    m_34: float;
    m_41: float;
    m_42: float;
    m_43: float;
    m_44: float;
  end;
  psk_matrix44_t = ^sk_matrix44_t;

  sk_point_t = record
    x: float;
    y: float;
  end;
  psk_point_t = ^sk_point_t;

  sk_pathiteratorelem_t = record
    verb         : sk_pathverb_t;
    points       : array[0..3] of sk_point_t;
    conic_weight : float;
  end;
  psk_pathiteratorelem_t = ^sk_pathiteratorelem_t;

  sk_pdfmetadata_t = record
    title            : MarshaledAString;
    author           : MarshaledAString;
    subject          : MarshaledAString;
    keywords         : MarshaledAString;
    creator          : MarshaledAString;
    producer         : MarshaledAString;
    creation         : sk_datetime_t;
    modified         : sk_datetime_t;
    raster_dpi       : float;
    pdfa             : bool;
    encoding_quality : int32_t;
  end;
  psk_pdfmetadata_t = ^sk_pdfmetadata_t;

  sk_point3_t = record
    x: float;
    y: float;
    z: float;
  end;
  psk_point3_t = ^sk_point3_t;

  sk_rect_t = record
    left   : float;
    top    : float;
    right  : float;
    bottom : float;
  end;
  psk_rect_t = ^sk_rect_t;

  sk_rotationscalematrix_t = record
    s_cos : float;
    s_sin : float;
    t_x   : float;
    t_y   : float;
  end;
  psk_rotationscalematrix_t = ^sk_rotationscalematrix_t;

  sk_samplingoptions_t = record
    use_cubic : bool;
    cubic     : sk_cubicresampler_t;
    filter    : sk_filtermode_t;
    mipmap    : sk_mipmapmode_t;
  end;
  psk_samplingoptions_t = ^sk_samplingoptions_t;

  sk_size_t = record
    width  : float;
    height : float;
  end;
  psk_size_t = ^sk_size_t;

  sk_surfaceprops_t = record
    flags          : uint32_t;
    pixel_geometry : sk_pixelgeometry_t;
  end;
  psk_surfaceprops_t = ^sk_surfaceprops_t;

  sk_vector_t  = sk_point_t;
  psk_vector_t = ^sk_vector_t;

  sk_font_path_proc              = procedure (const path: sk_path_t; const matrix: psk_matrix_t; context: Pointer); cdecl;
  sk_image_raster_release_proc   = procedure (const pixels: Pointer; context: Pointer); cdecl;
  sk_image_texture_release_proc  = procedure (context: Pointer); cdecl;
  sk_surface_raster_release_proc = procedure (pixels, context: Pointer); cdecl;

  sk_streamadapter_procs_t = record
    get_length   : function (context: Pointer): size_t; cdecl;
    get_position : function (context: Pointer): size_t; cdecl;
    read         : function (context: Pointer; buffer: Pointer; size: size_t): size_t; cdecl;
    seek         : function (context: Pointer; position: size_t): bool; cdecl;
  end;
  psk_streamadapter_procs_t = ^sk_streamadapter_procs_t;

  sk_wstreamadapter_procs_t = record
    write: function  (context: Pointer; const buffer: Pointer; size: size_t): bool; cdecl;
  end;
  psk_wstreamadapter_procs_t = ^sk_wstreamadapter_procs_t;

  sk_tracememorydumpbaseclass_procs_t = record
    dump_numeric_value : procedure (context: Pointer; const dump_name, value_name, units: MarshaledAString; value: uint64_t); cdecl;
    dump_string_value  : procedure (context: Pointer; const dump_name, value_name, value: MarshaledAString); cdecl;
  end;
  psk_tracememorydumpbaseclass_procs_t = ^sk_tracememorydumpbaseclass_procs_t;

  // GPU

  gr_contextoptions_t = record
    buffer_map_threshold              : int32_t;
    do_manual_mipmapping              : bool;
    allow_path_mask_caching           : bool;
    glyph_cache_texture_maximum_bytes : size_t;
    avoid_stencil_buffers             : bool;
    runtime_program_cache_size        : int32_t;
  end;
  pgr_contextoptions_t = ^gr_contextoptions_t;

  // GPU - OpenGL

  gr_gl_interface_t = THandle;

  pgr_gl_interface_t = ^gr_gl_interface_t;

  gl_enum_t = uint32_t;
  gl_uint_t = uint32_t;

  pgl_enum_t = ^gl_enum_t;
  pgl_uint_t = ^gl_uint_t;

  gr_gl_framebufferinfo_t = record
    fboid  : gl_uint_t;
    format : gl_enum_t;
  end;
  pgr_gl_framebufferinfo_t = ^gr_gl_framebufferinfo_t;

  gr_gl_textureinfo_t = record
    target : gl_enum_t;
    id     : gl_uint_t;
    format : gl_enum_t;
  end;
  pgr_gl_textureinfo_t = ^gr_gl_textureinfo_t;

  gr_gl_get_proc = function (context: Pointer; const name: MarshaledAString): Pointer; cdecl;

  // GPU - Metal

  gr_mtl_handle_t = Pointer;

  gr_mtl_textureinfo_t = record
    texture: gr_mtl_handle_t;
  end;
  pgr_mtl_textureinfo_t = ^gr_mtl_textureinfo_t;

  gr_mtl_backendcontext_t = record
    device         : gr_mtl_handle_t;
    queue          : gr_mtl_handle_t;
    binary_archive : gr_mtl_handle_t;
  end;
  pgr_mtl_backendcontext_t = ^gr_mtl_backendcontext_t;
  {$ENDREGION}

  {$REGION 'modules/particles/include/sk4d_particles_types.h'}
  sk_particleeffect_t = THandle;

  psk_particleeffect_t = ^sk_particleeffect_t;

  sk_particleuniform_t = record
    columns : int32_t;
    rows    : int32_t;
    slot    : int32_t;
  end;
  psk_particleuniform_t = ^sk_particleuniform_t;
  {$ENDREGION}

  {$REGION 'modules/skottie/include/sk4d_skottie_types.h'}
  sk_skottieanimation_t = THandle;

  psk_skottieanimation_t = ^sk_skottieanimation_t;
  {$ENDREGION}

  {$REGION 'modules/skparagraph/include/sk4d_paragraph_types.h'}
  sk_paragraph_t            = THandle;
  sk_paragraphbuilder_t     = THandle;
  sk_paragraphstyle_t       = THandle;
  sk_strutstyle_t           = THandle;
  sk_textstyle_t            = THandle;
  sk_typefacefontprovider_t = THandle;

  psk_paragraph_t            = ^sk_paragraph_t;
  psk_paragraphbuilder_t     = ^sk_paragraphbuilder_t;
  psk_paragraphstyle_t       = ^sk_paragraphstyle_t;
  psk_strutstyle_t           = ^sk_strutstyle_t;
  psk_textstyle_t            = ^sk_textstyle_t;
  psk_typefacefontprovider_t = ^sk_typefacefontprovider_t;

  sk_affinity_t = (
    UPSTREAM_SK_AFFINITY,
    DOWNSTREAM_SK_AFFINITY
  );
  psk_affinity_t = ^sk_affinity_t;

  sk_placeholderalignment_t = (
    BASELINE_SK_PLACEHOLDERALIGNMENT,
    ABOVE_BASELINE_SK_PLACEHOLDERALIGNMENT,
    BELOW_BASELINE_SK_PLACEHOLDERALIGNMENT,
    TOP_SK_PLACEHOLDERALIGNMENT,
    BOTTOM_SK_PLACEHOLDERALIGNMENT,
    MIDDLE_SK_PLACEHOLDERALIGNMENT
  );
  psk_placeholderalignment_t = ^sk_placeholderalignment_t;

  sk_rectheightstyle_t = (
    TIGHT_SK_RECTHEIGHTSTYLE,
    MAX_SK_RECTHEIGHTSTYLE,
    INCLUDE_LINE_SPACING_MIDDLE_SK_RECTHEIGHTSTYLE,
    INCLUDE_LINE_SPACING_TOP_SK_RECTHEIGHTSTYLE,
    INCLUDE_LINE_SPACING_BOTTOM_SK_RECTHEIGHTSTYLE,
    STRUT_SK_RECTHEIGHTSTYLE
  );
  psk_rectheightstyle_t = ^sk_rectheightstyle_t;

  sk_rectwidthstyle_t = (
    TIGHT_SK_RECTWIDTHSTYLE,
    MAX_SK_RECTWIDTHSTYLE
  );
  psk_rectwidthstyle_t = ^sk_rectwidthstyle_t;

  sk_textalign_t = (
    LEFT_SK_TEXTALIGN,
    RIGHT_SK_TEXTALIGN,
    CENTER_SK_TEXTALIGN,
    JUSTIFY_SK_TEXTALIGN,
    START_SK_TEXTALIGN,
    TERMINATE_SK_TEXTALIGN
  );
  psk_textalign_t = ^sk_textalign_t;

  sk_textbaseline_t = (
    ALPHABETIC_SK_TEXTBASELINE,
    IDEOGRAPHIC_SK_TEXTBASELINE
  );
  psk_textbaseline_t = ^sk_textbaseline_t;

  sk_textdecorationstyle_t = (
    SOLID_SK_TEXTDECORATIONSTYLE,
    DOUBLE_SK_TEXTDECORATIONSTYLE,
    DOTTED_SK_TEXTDECORATIONSTYLE,
    DASHED_SK_TEXTDECORATIONSTYLE,
    WAVY_SK_TEXTDECORATIONSTYLE
  );
  psk_textdecorationstyle_t = ^sk_textdecorationstyle_t;

  sk_textdirection_t = (
    RIGHT_TO_LEFT_SK_TEXTDIRECTION,
    LEFT_TO_RIGHT_SK_TEXTDIRECTION
  );
  psk_textdirection_t = ^sk_textdirection_t;

  sk_metrics_t = record
    start_index               : size_t;
    end_index                 : size_t;
    end_excluding_whitespaces : size_t;
    end_including_newline     : size_t;
    is_hard_break             : bool;
    ascent                    : double;
    descent                   : double;
    height                    : double;
    width                     : double;
    left                      : double;
    baseline                  : double;
    line_number               : size_t;
  end;
  psk_metrics_t = ^sk_metrics_t;

  sk_placeholderstyle_t = record
    width           : float;
    height          : float;
    alignment       : sk_placeholderalignment_t;
    baseline        : sk_textbaseline_t;
    baseline_offset : float;
  end;
  psk_placeholderstyle_t = ^sk_placeholderstyle_t;

  sk_positionaffinity_t = record
    position: int32_t;
    affinity: sk_affinity_t;
  end;
  psk_positionaffinity_t = ^sk_positionaffinity_t;

  sk_textbox_t = record
    rect      : sk_rect_t;
    direction : sk_textdirection_t;
  end;
  psk_textbox_t = ^sk_textbox_t;

  sk_textshadow_t = record
    color       : sk_color_t;
    offset      : sk_point_t;
    blur_radius : double;
  end;
  psk_textshadow_t = ^sk_textshadow_t;
  {$ENDREGION}

  {$REGION 'modules/skresources/include/sk4d_resources_types.h'}
  sk_resourceprovider_t          = THandle;
  sk_resourceproviderbaseclass_t = THandle;

  psk_resourceprovider_t          = ^sk_resourceprovider_t;
  psk_resourceproviderbaseclass_t = ^sk_resourceproviderbaseclass_t;

  sk_resourceproviderbaseclass_procs_t = record
    load: function (context: Pointer; const path, name: MarshaledAString): sk_data_t; cdecl;
  end;
  psk_resourceproviderbaseclass_procs_t = ^sk_resourceproviderbaseclass_procs_t;
  {$ENDREGION}

  {$REGION 'modules/skshaper/include/sk4d_shaper_types.h'}
  sk_shaper_t = THandle;

  psk_shaper_t = ^sk_shaper_t;
  {$ENDREGION}

  {$REGION 'modules/skunicode/include/sk4d_unicode_types.h'}
  sk_unicode_t  = THandle;

  psk_unicode_t = ^sk_unicode_t;

  sk_breaktype_t = (
    WORDS_SK_BREAKTYPE,
    GRAPHEMES_SK_BREAKTYPE,
    LINES_SK_BREAKTYPE
  );
  psk_breaktype_t = ^sk_breaktype_t;

  sk_direction_t = (
    LEFT_TO_RIGHT_SK_DIRECTION,
    RIGHT_TO_LEFT_SK_DIRECTION
  );
  psk_direction_t = ^sk_direction_t;

  sk_unicode_bidi_region_proc = procedure (start, &end: int32_t; level: uint8_t; context: Pointer); cdecl;
  sk_unicode_break_proc       = procedure (position, status: int32_t; context: Pointer); cdecl;
  sk_unicode_codepoint_proc   = procedure (unichar: sk_unichar_t; start, &end: int32_t; context: Pointer); cdecl;
  {$ENDREGION}

  {$REGION 'modules/svg/include/sk4d_svg_types.h'}
  sk_svgdom_t  = THandle;
  sk_svgsvg_t  = THandle;
  sk_svgnode_t = THandle;

  psk_svgdom_t  = ^sk_svgdom_t;
  psk_svgsvg_t  = ^sk_svgsvg_t;
  psk_svgnode_t = ^sk_svgnode_t;

  sk_svglengthunit_t = (
    UNKNOWN_SK_SVGLENGTHUNIT,
    NUMBER_SK_SVGLENGTHUNIT,
    PERCENTAGE_SK_SVGLENGTHUNIT,
    EMS_SK_SVGLENGTHUNIT,
    EXS_SK_SVGLENGTHUNIT,
    PX_SK_SVGLENGTHUNIT,
    CM_SK_SVGLENGTHUNIT,
    MM_SK_SVGLENGTHUNIT,
    IN_SK_SVGLENGTHUNIT,
    PT_SK_SVGLENGTHUNIT,
    PC_SK_SVGLENGTHUNIT
  );
  psk_svglengthunit_t = ^sk_svglengthunit_t;

  sk_svgaspectalign_t = (
    X_MIN_Y_MIN_SK_SVGASPECTALIGN,
    X_MID_Y_MIN_SK_SVGASPECTALIGN,
    X_MAX_Y_MIN_SK_SVGASPECTALIGN,
    X_MIN_Y_MID_SK_SVGASPECTALIGN = 4,
    X_MID_Y_MID_SK_SVGASPECTALIGN,
    X_MAX_Y_MID_SK_SVGASPECTALIGN,
    X_MIN_Y_MAX_SK_SVGASPECTALIGN = 8,
    X_MID_Y_MAX_SK_SVGASPECTALIGN,
    X_MAX_Y_MAX_SK_SVGASPECTALIGN,
    NONE_SK_SVGASPECTALIGN = 16
  );
  psk_svgaspectalign_t = ^sk_svgaspectalign_t;

  sk_svgaspectscale_t = (
    MEET_SK_SVGASPECTSCALE,
    SLICE_SK_SVGASPECTSCALE
  );
  psk_svgaspectscale_t = ^sk_svgaspectscale_t;

  sk_svglength_t = record
    value: float;
    &unit: sk_svglengthunit_t;
  end;
  psk_svglength_t = ^sk_svglength_t;

  sk_svgpreserveaspectratio_t = record
    align: sk_svgaspectalign_t;
    scale: sk_svgaspectscale_t;
  end;
  psk_svgpreserveaspectratio_t = ^sk_svgpreserveaspectratio_t;
  {$ENDREGION}

  {$ENDREGION}

  {$REGION 'Skia API'}

  {$IFDEF SK_DYNAMIC_LOADING}

  ESkiaAPI = class(Exception);

  {$ENDIF}

  { TSkiaAPI }

  TSkiaAPI = class sealed
  public const
    {$IF DEFINED(MSWINDOWS)}
    LibName = 'sk4d.dll';
    {$ELSEIF DEFINED(MACOS)}
      {$IF DEFINED(IOS) and DEFINED(CPUARM)}
      LibName = 'sk4d.a';
      {$ELSE}
      LibName = 'sk4d.dylib';
      {$ENDIF}
    {$ELSE}
    LibName = 'libsk4d.so';
    {$ENDIF}
  {$IFDEF SK_DYNAMIC_LOADING}
  strict private class var
    [Volatile] FRefCount: Integer;
    FLibHandle: HMODULE;
  {$ENDIF}
  strict private
    class constructor Create;
    {$IFDEF SK_DYNAMIC_LOADING}
    class destructor Destroy;
    {$ENDIF}
  public
    class procedure Initialize;
    class procedure Terminate;

    {$REGION 'include/c/gr4d_backendsurface.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendrendertarget_create_gl         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(width, height, sample_count, stencil_bits: int32_t; const framebuffer_info: pgr_gl_framebufferinfo_t): gr_backendrendertarget_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendrendertarget_create_mtl        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(width, height: int32_t; const texture_info: pgr_mtl_textureinfo_t): gr_backendrendertarget_t; cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_backendrendertarget_destroy           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_backendrendertarget_t); cdecl;                                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendrendertarget_get_backend_api   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendrendertarget_t): gr_backendapi_t; cdecl;                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendrendertarget_get_height        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendrendertarget_t): int32_t; cdecl;                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendrendertarget_get_sample_count  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendrendertarget_t): int32_t; cdecl;                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendrendertarget_get_stencil_bits  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendrendertarget_t): int32_t; cdecl;                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendrendertarget_get_width         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendrendertarget_t): int32_t; cdecl;                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendrendertarget_is_valid          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendrendertarget_t): bool; cdecl;                                                                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendtexture_create_gl              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(width, height: int32_t; is_mipmapped: bool; const texture_info: pgr_gl_textureinfo_t): gr_backendtexture_t; cdecl;                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendtexture_create_mtl             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(width, height: int32_t; is_mipmapped: bool; const texture_info: pgr_mtl_textureinfo_t): gr_backendtexture_t; cdecl;                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_backendtexture_destroy                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_backendtexture_t); cdecl;                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendtexture_get_backend_api        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendtexture_t): gr_backendapi_t; cdecl;                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendtexture_get_gl_framebuffer_info{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendtexture_t; out texture_info: gr_gl_textureinfo_t): bool; cdecl;                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendtexture_get_height             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendtexture_t): int32_t; cdecl;                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendtexture_get_width              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendtexture_t): int32_t; cdecl;                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendtexture_has_mipmaps            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendtexture_t): bool; cdecl;                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendtexture_is_valid               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendtexture_t): bool; cdecl;                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/gr4d_directcontext.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_abandon_context                            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t); cdecl;                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_dump_memory_statistics                     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: gr_directcontext_t; trace_memory_dump: sk_tracememorydump_t); cdecl;                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_flush                                      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t); cdecl;                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_flush_and_submit                           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t; sync_cpu: bool); cdecl;                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_free_gpu_resources                         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t); cdecl;                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_directcontext_get_backend_api                            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_directcontext_t): gr_backendapi_t; cdecl;                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_directcontext_get_max_surface_sample_count_for_color_type{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_directcontext_t; color_type: sk_colortype_t): int32_t; cdecl;                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_directcontext_get_resource_cache_limit                   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_directcontext_t): size_t; cdecl;                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_get_resource_cache_usage                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: gr_directcontext_t; out resources: int32_t; out resources_bytes: size_t); cdecl;                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_directcontext_is_abandoned                               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: gr_directcontext_t): bool; cdecl;                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_directcontext_make_gl                                    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const gl_interface: gr_gl_interface_t; const options: pgr_contextoptions_t): gr_directcontext_t; cdecl;           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_directcontext_make_metal                                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const backend_context: pgr_mtl_backendcontext_t; const options: pgr_contextoptions_t): gr_directcontext_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_perform_deferred_cleanup                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t; milliseconds: int64_t); cdecl;                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_purge_unlocked_resources                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t; scratch_resources_only: bool); cdecl;                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_purge_unlocked_resources2                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t; bytes_to_purge: size_t; prefer_scratch_resources: bool); cdecl;                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_release_resources_and_abandon_context      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t); cdecl;                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_reset_context                              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t); cdecl;                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_set_resource_cache_limit                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t; value: size_t); cdecl;                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_directcontext_submit                                     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: gr_directcontext_t; sync_cpu: bool): bool; cdecl;                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/gr4d_gl_interface.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}gr4d_gl_interface_has_extension       {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: gr_gl_interface_t; const name: MarshaledAString): bool; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}gr4d_gl_interface_make_assembled      {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(context: Pointer; proc: gr_gl_get_proc): gr_gl_interface_t; cdecl;         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}gr4d_gl_interface_make_assembled_gl   {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(context: Pointer; proc: gr_gl_get_proc): gr_gl_interface_t; cdecl;         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}gr4d_gl_interface_make_assembled_gles {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(context: Pointer; proc: gr_gl_get_proc): gr_gl_interface_t; cdecl;         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}gr4d_gl_interface_make_assembled_webgl{$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(context: Pointer; proc: gr_gl_get_proc): gr_gl_interface_t; cdecl;         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}gr4d_gl_interface_make_native         {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(): gr_gl_interface_t; cdecl;                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}gr4d_gl_interface_validate            {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: gr_gl_interface_t): bool; cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_blender.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_blender_make_arithmetic{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(k1, k2, k3, k4: float; enforce_premultiplied_color: bool): sk_blender_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_blender_make_mode      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(mode: sk_blendmode_t): sk_blender_t; cdecl;                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_canvas.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_clear                     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; color: sk_color_t); cdecl;                                                                                                                                                                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_clear2                    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const color: psk_color4f_t); cdecl;                                                                                                                                                                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_destroy                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t); cdecl;                                                                                                                                                                                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_discard                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t); cdecl;                                                                                                                                                                                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_clip_path                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const path: sk_path_t; op: sk_clipop_t; anti_alias: bool); cdecl;                                                                                                                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_clip_rect                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const rect: psk_rect_t; op: sk_clipop_t; anti_alias: bool); cdecl;                                                                                                                                                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_clip_region               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const region: sk_region_t; op: sk_clipop_t); cdecl;                                                                                                                                                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_clip_rrect                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const rrect: sk_rrect_t; op: sk_clipop_t; anti_alias: bool); cdecl;                                                                                                                                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_clip_shader               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; shader: sk_shader_t; op: sk_clipop_t); cdecl;                                                                                                                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_concat                    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const matrix: psk_matrix44_t); cdecl;                                                                                                                                                                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_concat2                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const matrix: psk_matrix_t); cdecl;                                                                                                                                                                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_annotation           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const rect: psk_rect_t; const key: MarshaledAString; const value: Pointer; size: size_t); cdecl;                                                                                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_arc                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const oval: psk_rect_t; start_angle, sweep_angle: float; use_center: bool; const paint: sk_paint_t); cdecl;                                                                                                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_atlas                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const atlas: sk_image_t; const transforms: psk_rotationscalematrix_t; const sprites: psk_rect_t; const colors: psk_color_t; count: int32_t; blend_mode: sk_blendmode_t; const sampling: psk_samplingoptions_t; const cull_rect: psk_rect_t; const paint: sk_paint_t); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_circle               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const center: psk_point_t; radius: float; const paint: sk_paint_t); cdecl;                                                                                                                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_color                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; color: sk_color_t; blend_mode: sk_blendmode_t); cdecl;                                                                                                                                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_color2               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const color: psk_color4f_t; blend_mode: sk_blendmode_t); cdecl;                                                                                                                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_glyphs               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; count: int32_t; const glyphs: psk_glyphid_t; const positions: psk_point_t; const origin: psk_point_t; const font: sk_font_t; const paint: sk_paint_t); cdecl;                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_glyphs2              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; count: int32_t; const glyphs: psk_glyphid_t; const matrices: psk_rotationscalematrix_t; const origin: psk_point_t; const font: sk_font_t; const paint: sk_paint_t); cdecl;                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_image                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const image: sk_image_t; x, y: float; const sampling: psk_samplingoptions_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_image_lattice        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const image: sk_image_t; const lattice: psk_lattice_t; const dest: psk_rect_t; filter_mode: sk_filtermode_t; const paint: sk_paint_t); cdecl;                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_image_nine           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const image: sk_image_t; const center: psk_irect_t; const dest: psk_rect_t; filter_mode: sk_filtermode_t; const paint: sk_paint_t); cdecl;                                                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_image_rect           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const image: sk_image_t; const src, dest: psk_rect_t; const sampling: psk_samplingoptions_t; const paint: sk_paint_t; constraint: sk_srcrectconstraint_t); cdecl;                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_line                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const point1, point2: psk_point_t; paint: sk_paint_t); cdecl;                                                                                                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_oval                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const oval: psk_rect_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                                                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_paint                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_patch                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const cubics: psk_point_t; const colors: psk_color_t; const tex_coords: psk_point_t; blend_mode: sk_blendmode_t; const paint: sk_paint_t); cdecl;                                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_path                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const path: sk_path_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_picture              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const picture: sk_picture_t; const matrix: psk_matrix_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_point                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const point: psk_point_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_points               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; mode: sk_drawpointsmode_t; count: size_t; const points: psk_point_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_rect                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const rect: psk_rect_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                                                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_region               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const region: sk_region_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_rrect                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const rrect: sk_rrect_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_rrect2               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const rect: psk_rect_t; radius_x, radius_y: float; const paint: sk_paint_t); cdecl;                                                                                                                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_rrect_difference     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const outer, inner: sk_rrect_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_simple_text          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const text: Pointer; size: size_t; encoding: sk_textencoding_t; x, y: float; const font: sk_font_t; const paint: sk_paint_t); cdecl;                                                                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_text_blob            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const text_blob: sk_textblob_t; x, y: float; const paint: sk_paint_t); cdecl;                                                                                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_vertices             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const vertices: sk_vertices_t; blend_mode: sk_blendmode_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_get_device_clip_bounds    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_canvas_t; out result: sk_irect_t); cdecl;                                                                                                                                                                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_get_local_clip_bounds     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_canvas_t; out result: sk_rect_t); cdecl;                                                                                                                                                                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_get_local_to_device       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_canvas_t; out result: sk_matrix44_t); cdecl;                                                                                                                                                                                                                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_get_local_to_device_as_3x3{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_canvas_t; out result: sk_matrix_t); cdecl;                                                                                                                                                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_canvas_get_save_count            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_canvas_t): int32_t; cdecl;                                                                                                                                                                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_canvas_quick_reject              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_canvas_t; const rect: psk_rect_t): bool; cdecl;                                                                                                                                                                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_canvas_quick_reject2             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_canvas_t; const path: sk_path_t): bool; cdecl;                                                                                                                                                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_reset_matrix              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t); cdecl;                                                                                                                                                                                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_restore                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t); cdecl;                                                                                                                                                                                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_restore_to_count          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; save_count: int32_t); cdecl;                                                                                                                                                                                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_rotate                    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; degrees: float); cdecl;                                                                                                                                                                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_rotate2                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; degrees, px, py: float); cdecl;                                                                                                                                                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_canvas_save                      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_canvas_t): int32_t; cdecl;                                                                                                                                                                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_canvas_save_layer                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_canvas_t; const rect: psk_rect_t; const paint: sk_paint_t): int32_t; cdecl;                                                                                                                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_canvas_save_layer_alpha          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_canvas_t; const rect: psk_rect_t; alpha: uint8_t): int32_t; cdecl;                                                                                                                                                                                                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_scale                     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; sx, sy: float); cdecl;                                                                                                                                                                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_set_matrix                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const matrix: psk_matrix44_t); cdecl;                                                                                                                                                                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_set_matrix2               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const matrix: psk_matrix_t); cdecl;                                                                                                                                                                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_skew                      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; kx, ky: float); cdecl;                                                                                                                                                                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_translate                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; dx, dy: float); cdecl;                                                                                                                                                                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_codec.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_codec_destroy                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(codec: sk_codec_t); cdecl;                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_codec_get_dimensions            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_codec_t; out result: sk_isize_t); cdecl;                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_codec_get_image                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_codec_t; color_type: sk_colortype_t; alpha_type: sk_alphatype_t; color_space: sk_colorspace_t): sk_image_t; cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_codec_get_pixels                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_codec_t; pixels: Pointer; row_bytes: size_t; color_type: sk_colortype_t; alpha_type: sk_alphatype_t; color_space: sk_colorspace_t): bool; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_codec_make_from_file            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const file_name: MarshaledAString): sk_codec_t; cdecl;                                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_codec_make_with_copy            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const data: Pointer; size: size_t): sk_codec_t; cdecl;                                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_codec_make_without_copy         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const data: Pointer; size: size_t): sk_codec_t; cdecl;                                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_animcodecplayer_destroy         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_animcodecplayer_t); cdecl;                                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_animcodecplayer_get_dimensions  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_animcodecplayer_t; out result: sk_isize_t); cdecl;                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_animcodecplayer_get_duration    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_animcodecplayer_t): uint32_t; cdecl;                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_animcodecplayer_get_frame       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_animcodecplayer_t): sk_image_t; cdecl;                                                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_animcodecplayer_make_from_file  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const file_name: MarshaledAString): sk_animcodecplayer_t; cdecl;                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_animcodecplayer_make_from_stream{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(stream: sk_stream_t): sk_animcodecplayer_t; cdecl;                                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_animcodecplayer_seek            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_animcodecplayer_t; milliseconds: uint32_t): bool; cdecl;                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_colorfilter.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_colorfilter_make_blend               {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(color: sk_color_t; mode: sk_blendmode_t): sk_colorfilter_t; cdecl;                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_colorfilter_make_compose             {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(outer, inner: sk_colorfilter_t): sk_colorfilter_t; cdecl;                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_colorfilter_make_high_contrast       {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const config: psk_highcontrastconfig_t): sk_colorfilter_t; cdecl;                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_colorfilter_make_hsla_matrix         {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const matrix: psk_colormatrix_t): sk_colorfilter_t; cdecl;                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_colorfilter_make_lighting            {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(multiply, add: sk_color_t): sk_colorfilter_t; cdecl;                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_colorfilter_make_linear_to_srgb_gamma{$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(): sk_colorfilter_t; cdecl;                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_colorfilter_make_luma_color          {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(): sk_colorfilter_t; cdecl;                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_colorfilter_make_matrix              {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const matrix: psk_colormatrix_t): sk_colorfilter_t; cdecl;                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_colorfilter_make_overdraw            {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const colors: psk_color_t): sk_colorfilter_t; cdecl;                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_colorfilter_make_table               {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const tablea_a, tablea_r, tablea_g, tablea_b: puint8_t): sk_colorfilter_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_colorspace.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_gamma_close_to_srgb      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspace_t): bool; cdecl;                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_gamma_is_linear          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspace_t): bool; cdecl;                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_is_equal                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, color_space: sk_colorspace_t): bool; cdecl;                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_is_numerical_transfer_fn {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspace_t; out transfer_function: sk_colorspacetransferfn_t): bool; cdecl;                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_is_srgb                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspace_t): bool; cdecl;                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_make                     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const profile: sk_colorspaceiccprofile_t): sk_colorspace_t; cdecl;                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_make_linear_gamma        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspace_t): sk_colorspace_t; cdecl;                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_make_rgb                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const transfer_function: psk_colorspacetransferfn_t; const xyz: psk_colorspacexyz_t): sk_colorspace_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_make_srgb                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_colorspace_t; cdecl;                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_make_srgb_gamma          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspace_t): sk_colorspace_t; cdecl;                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_make_srgb_linear         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_colorspace_t; cdecl;                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_colorspace_ref                      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_colorspace_t); cdecl;                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_to_profile               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspace_t): sk_colorspaceiccprofile_t; cdecl;                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_to_xyz                   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspace_t; out xyz: sk_colorspacexyz_t): bool; cdecl;                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_colorspace_unref                    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_colorspace_t); cdecl;                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_colorspaceiccprofile_destroy        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_colorspaceiccprofile_t); cdecl;                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspaceiccprofile_get_buffer     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspaceiccprofile_t; size: puint32_t): puint8_t; cdecl;                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspaceiccprofile_make_with_parse{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const buffer: Pointer; size: size_t): sk_colorspaceiccprofile_t; cdecl;                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspaceiccprofile_to_xyz         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspaceiccprofile_t; out dest: sk_colorspacexyz_t): bool; cdecl;                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspaceprimaries_to_xyz          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: psk_colorspaceprimaries_t; out xyz: sk_colorspacexyz_t): bool; cdecl;                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspacetransferfn_invert         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: psk_colorspacetransferfn_t; out transfer_function: sk_colorspacetransferfn_t): bool; cdecl;       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspacetransferfn_transform      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: psk_colorspacetransferfn_t; x: float): float; cdecl;                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_data.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_data_make_empty    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_data_t; cdecl;                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_data_make_with_copy{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const data: Pointer; size: size_t): sk_data_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_document.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_document_begin_page{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_document_t; width, height: float; const content: psk_rect_t): sk_canvas_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_document_close     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_document_t); cdecl;                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_document_end_page  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_document_t); cdecl;                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_document_make_pdf  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(w_stream: sk_wstream_t): sk_document_t; cdecl;                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_document_make_pdf2 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(w_stream: sk_wstream_t; const metadata: psk_pdfmetadata_t): sk_document_t; cdecl;          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_document_make_xps  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(w_stream: sk_wstream_t; dpi: float): sk_document_t; cdecl;                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_document_terminate {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_document_t); cdecl;                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_font.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_create                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(typeface: sk_typeface_t; size, sx, kx: float): sk_font_t; cdecl;                                                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_create2                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const font: sk_font_t): sk_font_t; cdecl;                                                                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_destroy                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t); cdecl;                                                                                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_baseline_snap       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): bool; cdecl;                                                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_edging              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): sk_fontedging_t; cdecl;                                                                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_embedded_bitmaps    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): bool; cdecl;                                                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_embolden            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): bool; cdecl;                                                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_force_auto_hinting  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): bool; cdecl;                                                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_glyphs              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t; const text: Pointer; size: size_t; encoding: sk_textencoding_t; result: psk_glyphid_t; max_count: int32_t): int32_t; cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_glyphs_count        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t; const text: Pointer; size: size_t; encoding: sk_textencoding_t): int32_t; cdecl;                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_hinting             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): sk_fonthinting_t; cdecl;                                                                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_get_horizontal_positions{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; result: pfloat; origin: float); cdecl;                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_intercepts          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; const positions: psk_point_t; const bounds: pfloat; result: pfloat; const paint: sk_paint_t): size_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_linear_metrics      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): bool; cdecl;                                                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_metrics             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t; metrics: psk_fontmetrics_t): float; cdecl;                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_path                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t; glyph: sk_glyphid_t): sk_path_t; cdecl;                                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_get_paths               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; proc: sk_font_path_proc; proc_context: Pointer); cdecl;                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_get_positions           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; result: psk_point_t; const origin: psk_point_t); cdecl;                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_scale_x             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): float; cdecl;                                                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_size                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): float; cdecl;                                                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_skew_x              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): float; cdecl;                                                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_subpixel            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): bool; cdecl;                                                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_typeface            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): sk_typeface_t; cdecl;                                                                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_typeface_or_default {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): sk_typeface_t; cdecl;                                                                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_get_widths_bounds       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; widths: pfloat; bounds: psk_rect_t; const paint: sk_paint_t); cdecl;                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_is_equal                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, font: sk_font_t): bool; cdecl;                                                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_measure_text            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t; const text: Pointer; size: size_t; encoding: sk_textencoding_t; bounds: psk_rect_t; const paint: sk_paint_t): float; cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_set_baseline_snap       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: bool); cdecl;                                                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_set_edging              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: sk_fontedging_t); cdecl;                                                                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_set_embedded_bitmaps    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: bool); cdecl;                                                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_set_embolden            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: bool); cdecl;                                                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_set_force_auto_hinting  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: bool); cdecl;                                                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_set_hinting             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: sk_fonthinting_t); cdecl;                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_set_linear_metrics      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: bool); cdecl;                                                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_set_scale_x             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: float); cdecl;                                                                                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_set_size                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: float); cdecl;                                                                                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_set_skew_x              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: float); cdecl;                                                                                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_set_subpixel            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: bool); cdecl;                                                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_set_typeface            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; typeface: sk_typeface_t); cdecl;                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_unichar_to_glyph        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t; unichar: sk_unichar_t): sk_glyphid_t; cdecl;                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_unichars_to_glyphs      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_font_t; const unichars: psk_unichar_t; count: int32_t; result: psk_glyphid_t); cdecl;                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_graphics.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_graphics_allow_jit                                      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(); cdecl;                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_graphics_dump_memory_statistics                         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(trace_memory_dump: sk_tracememorydump_t); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_graphics_get_font_cache_count_limit                     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): int32_t; cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_graphics_get_font_cache_count_used                      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): int32_t; cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_graphics_get_font_cache_limit                           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): size_t; cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_graphics_get_font_cache_used                            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): size_t; cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_graphics_get_resource_cache_single_allocation_byte_limit{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): size_t; cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_graphics_get_resource_cache_total_byte_limit            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): size_t; cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_graphics_get_resource_cache_total_bytes_used            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): size_t; cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_graphics_init                                           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(); cdecl;                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_graphics_purge_all_caches                               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(); cdecl;                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_graphics_purge_font_cache                               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(); cdecl;                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_graphics_purge_resource_cache                           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(); cdecl;                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_graphics_set_font_cache_count_limit                     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(value: int32_t): int32_t; cdecl;                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_graphics_set_font_cache_limit                           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(value: size_t): size_t; cdecl;                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_graphics_set_resource_cache_single_allocation_byte_limit{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(value: size_t): size_t; cdecl;                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_graphics_set_resource_cache_total_byte_limit            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(value: size_t): size_t; cdecl;                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_image.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_image_encode_to_file           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_image_t; const file_name: MarshaledAString; format: sk_encodedimageformat_t; quality: int32_t); cdecl;                                                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_image_encode_to_stream         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_image_t; w_stream: sk_wstream_t; format: sk_encodedimageformat_t; quality: int32_t); cdecl;                                                                                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_get_alpha_type           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): sk_alphatype_t; cdecl;                                                                                                                                                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_get_color_space          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): sk_colorspace_t; cdecl;                                                                                                                                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_get_color_type           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): sk_colortype_t; cdecl;                                                                                                                                                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_get_height               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): int32_t; cdecl;                                                                                                                                                                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_image_get_image_info           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_image_t; out result: sk_imageinfo_t); cdecl;                                                                                                                                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_get_unique_id            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): uint32_t; cdecl;                                                                                                                                                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_get_width                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): int32_t; cdecl;                                                                                                                                                                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_is_lazy_generated        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): bool; cdecl;                                                                                                                                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_is_texture_backed        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): bool; cdecl;                                                                                                                                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_is_valid                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t; context: gr_directcontext_t): bool; cdecl;                                                                                                                                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_make_from_adopted_texture{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: gr_directcontext_t; const texture: gr_backendtexture_t; origin: gr_surfaceorigin_t; color_type: sk_colortype_t; alpha_type: sk_alphatype_t; color_space: sk_colorspace_t): sk_image_t; cdecl;                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_make_from_encoded_file   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const file_name: MarshaledAString): sk_image_t; cdecl;                                                                                                                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_make_from_encoded_stream {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(stream: sk_stream_t): sk_image_t; cdecl;                                                                                                                                                                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_make_from_raster         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const pixmap: sk_pixmap_t; proc: sk_image_raster_release_proc; proc_context: Pointer): sk_image_t; cdecl;                                                                                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_make_from_texture        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: gr_directcontext_t; const texture: gr_backendtexture_t; origin: gr_surfaceorigin_t; color_type: sk_colortype_t; alpha_type: sk_alphatype_t; color_space: sk_colorspace_t; proc: sk_image_texture_release_proc; proc_context: Pointer): sk_image_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_make_non_texture_image   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): sk_image_t; cdecl;                                                                                                                                                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_make_raster_copy         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const pixmap: sk_pixmap_t): sk_image_t; cdecl;                                                                                                                                                                                                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_make_raster_image        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): sk_image_t; cdecl;                                                                                                                                                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_make_shader              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t; tile_mode_x, tile_mode_y: sk_tilemode_t; const sampling: psk_samplingoptions_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;                                                                                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_make_subset              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t; const subset: psk_irect_t; context: gr_directcontext_t): sk_image_t; cdecl;                                                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_make_texture_image       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t; context: gr_directcontext_t; is_mipmapped: bool): sk_image_t; cdecl;                                                                                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_make_with_filter         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t; context: gr_directcontext_t; const filter: sk_imagefilter_t; const subset, clip_bounds: psk_irect_t; out out_subset: sk_irect_t; out offset: sk_ipoint_t): sk_image_t; cdecl;                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_peek_pixels              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): sk_pixmap_t; cdecl;                                                                                                                                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_read_pixels              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t; context: gr_directcontext_t; const dest: sk_pixmap_t; src_x, src_y: int32_t; caching_hint: sk_imagecachinghint_t): bool; cdecl;                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_scale_pixels             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t; const dest: sk_pixmap_t; const sampling: psk_samplingoptions_t; caching_hint: sk_imagecachinghint_t): bool; cdecl;                                                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_imageencoder.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imageencoder_encode_to_file  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const file_name: MarshaledAString; const src: sk_pixmap_t; format: sk_encodedimageformat_t; quality: int32_t): bool; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imageencoder_encode_to_stream{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(w_stream: sk_wstream_t; const src: sk_pixmap_t; format: sk_encodedimageformat_t; quality: int32_t): bool; cdecl;            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_imagefilter.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_alpha_threshold     {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const region: sk_region_t; inner_min, outer_max: float; input: sk_imagefilter_t): sk_imagefilter_t; cdecl;                                                                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_arithmetic          {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(k1, k2, k3, k4: float; enforce_premultiplied_color: bool; background, foreground: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_blend               {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(mode: sk_blendmode_t; background, foreground: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_blur                {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(sigma_x, sigma_y: float; tile_mode: sk_tilemode_t; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_colorfilter         {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(color_filter: sk_colorfilter_t; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_compose             {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(inner, outer: sk_imagefilter_t): sk_imagefilter_t; cdecl;                                                                                                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_dilate              {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(radius_x, radius_y: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_displacement_map    {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(x_channel_selector, y_channel_selector: sk_colorchannel_t; scale: float; displacement, input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_distant_lit_diffuse {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const direction: psk_point3_t; light_color: sk_color_t; surface_scale, kd: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_distant_lit_specular{$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const direction: psk_point3_t; light_color: sk_color_t; surface_scale, ks, shininess: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_drop_shadow         {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(dx, dy, sigma_x, sigma_y: float; color: sk_color_t; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_drop_shadow_only    {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(dx, dy, sigma_x, sigma_y: float; color: sk_color_t; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_erode               {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(radius_x, radius_y: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_image               {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(image: sk_image_t; const src, dest: psk_rect_t; const sampling: psk_samplingoptions_t): sk_imagefilter_t; cdecl;                                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_magnifier           {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const src: psk_rect_t; inset: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_matrix_convolution  {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const kernel_size: psk_isize_t; const kernel: pfloat; gain, bias: float; const kernel_offset: psk_ipoint_t; tile_mode: sk_tilemode_t; convolve_alpha: bool; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_matrix_transform    {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const matrix: psk_matrix_t; const sampling: psk_samplingoptions_t; input: sk_imagefilter_t): sk_imagefilter_t; cdecl;                                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_merge               {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const filters: psk_imagefilter_t; count: int32_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_offset              {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(dx, dy: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_picture             {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(picture: sk_picture_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_point_lit_diffuse   {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const location: psk_point3_t; light_color: sk_color_t; surface_scale, kd: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_point_lit_specular  {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const location: psk_point3_t; light_color: sk_color_t; surface_scale, ks, shininess: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_shader              {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(shader: sk_shader_t; dither: bool; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_spot_lit_diffuse    {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const location, target: psk_point3_t; falloff_exponent, cutoff_angle: float; light_color: sk_color_t; surface_scale, kd: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_spot_lit_specular   {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const location, target: psk_point3_t; falloff_exponent, cutoff_angle: float; light_color: sk_color_t; surface_scale, ks, shininess: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_tile                {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const src, dest: psk_rect_t; input: sk_imagefilter_t): sk_imagefilter_t; cdecl;                                                                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_with_local_matrix   {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_imagefilter_t; const local_matrix: psk_matrix_t): sk_imagefilter_t; cdecl;                                                                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}

    {$ENDREGION}

    {$REGION 'include/c/sk4d_maskfilter.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_maskfilter_make_blur       {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(style: sk_blurstyle_t; sigma: float; respect_ctm: bool): sk_maskfilter_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_maskfilter_make_shader     {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(shader: sk_shader_t): sk_maskfilter_t; cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_maskfilter_make_table      {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const table: puint8_t): sk_maskfilter_t; cdecl;                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_maskfilter_make_table_clip {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(min, max: uint8_t): sk_maskfilter_t; cdecl;                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_maskfilter_make_table_gamma{$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(gamma: float): sk_maskfilter_t; cdecl;                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_paint.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_create            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_paint_t; cdecl;                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_create2           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const paint: sk_paint_t): sk_paint_t; cdecl;                                                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_destroy           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t); cdecl;                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_alpha         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): uint8_t; cdecl;                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_alphaf        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): float; cdecl;                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_anti_alias    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): bool; cdecl;                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_blender       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_blender_t; cdecl;                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_color         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_color_t; cdecl;                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_get_colorf        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_paint_t; out result: sk_color4f_t); cdecl;                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_color_filter  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_colorfilter_t; cdecl;                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_dither        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): bool; cdecl;                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_fill_path     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t; const path: sk_path_t; const cull_rect: psk_rect_t; res_scale: float): sk_path_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_image_filter  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_imagefilter_t; cdecl;                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_mask_filter   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_maskfilter_t; cdecl;                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_path_effect   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_patheffect_t; cdecl;                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_shader        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_shader_t; cdecl;                                                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_stroke_cap    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_strokecap_t; cdecl;                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_stroke_join   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_strokejoin_t; cdecl;                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_stroke_miter  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): float; cdecl;                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_stroke_width  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): float; cdecl;                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_style         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_paintstyle_t; cdecl;                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_reset             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t); cdecl;                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_alpha         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: uint8_t); cdecl;                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_alphaf        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: float); cdecl;                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_antialias     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: bool); cdecl;                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_argb          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; a, r, g, b: uint8_t); cdecl;                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_blender       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_blender_t); cdecl;                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_color         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_color_t); cdecl;                                                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_colorf        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; const value: psk_color4f_t; color_space: sk_colorspace_t); cdecl;                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_color_filter  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_colorfilter_t); cdecl;                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_dither        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: bool); cdecl;                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_image_filter  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_imagefilter_t); cdecl;                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_mask_filter   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_maskfilter_t); cdecl;                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_path_effect   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_patheffect_t); cdecl;                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_shader        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_shader_t); cdecl;                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_stroke_cap    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_strokecap_t); cdecl;                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_stroke_join   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_strokejoin_t); cdecl;                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_stroke_miter  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: float); cdecl;                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_stroke_width  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: float); cdecl;                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_style         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_paintstyle_t); cdecl;                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_path.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_opbuilder_add              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_opbuilder_t; const path: sk_path_t; op: sk_pathop_t); cdecl;                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_opbuilder_create           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_opbuilder_t; cdecl;                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_opbuilder_destroy          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_opbuilder_t); cdecl;                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_opbuilder_detach           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_opbuilder_t): sk_path_t; cdecl;                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_contains              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t; x, y: float): bool; cdecl;                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_convert_conic_to_quads{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const point1, point2, point3: psk_point_t; weight: float; points: psk_point_t; power2: int32_t): int32_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_create                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const svg: MarshaledAString): sk_path_t; cdecl;                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_create2               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(stream: sk_stream_t): sk_path_t; cdecl;                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_path_destroy               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_path_t); cdecl;                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_path_get_bounds            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_path_t; out result: sk_rect_t); cdecl;                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_get_fill_type         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t): sk_pathfilltype_t; cdecl;                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_get_last_point        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t; out result: sk_point_t): bool; cdecl;                                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_get_segment_masks     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t): uint32_t; cdecl;                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_path_get_tight_bounds      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_path_t; out result: sk_rect_t); cdecl;                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_interpolate           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, cending: sk_path_t; weight: float): sk_path_t; cdecl;                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_is_convex             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t): bool; cdecl;                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_is_empty              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t): bool; cdecl;                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_is_finite             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t): bool; cdecl;                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_is_interpolatable     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, path: sk_path_t): bool; cdecl;                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_is_last_contour_closed{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t): bool; cdecl;                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_is_line               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t; lines: psk_point_t): bool; cdecl;                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_is_oval               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t; oval: psk_rect_t): bool; cdecl;                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_is_rect               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t; rect: psk_rect_t): bool; cdecl;                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_is_rrect              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t; rrect: sk_rrect_t): bool; cdecl;                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_path_serialize_to_stream   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_path_t; w_stream: sk_wstream_t); cdecl;                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_to_svg                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t): sk_string_t; cdecl;                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_transform             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t; const matrix: psk_matrix_t): sk_path_t; cdecl;                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pathiterator_create        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const path: sk_path_t; force_close: bool): sk_pathiterator_t; cdecl;                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathiterator_destroy       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathiterator_t); cdecl;                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pathiterator_next          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_pathiterator_t; out elem: sk_pathiteratorelem_t): bool; cdecl;                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_pathbuilder.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_add_arc                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const oval: psk_rect_t; start_angle, sweep_angle: float); cdecl;                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_add_circle             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; center_x, center_y, radius: float; direction: sk_pathdirection_t); cdecl;                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_add_oval               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const oval: psk_rect_t; direction: sk_pathdirection_t; start_index: uint32_t); cdecl;                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_add_path               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const path: sk_path_t); cdecl;                                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_add_polygon            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; polygon: psk_point_t; count: int32_t; is_closed: bool); cdecl;                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_add_rect               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const rect: psk_rect_t; direction: sk_pathdirection_t; start_index: uint32_t); cdecl;                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_add_rrect              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const rrect: sk_rrect_t; direction: sk_pathdirection_t; start_index: uint32_t); cdecl;                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_arc_to                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const radius: psk_point_t; x_axis_rotate: float; large_arc: sk_patharcsize_t; sweep: sk_pathdirection_t; const xy: psk_point_t); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_arc_to2                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const oval: psk_rect_t; start_angle, sweep_angle: float; force_move_to: bool); cdecl;                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_arc_to3                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const point1, point2: psk_point_t; radius: float); cdecl;                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_close                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t); cdecl;                                                                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_conic_to               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const point1, point2: psk_point_t; weight: float); cdecl;                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pathbuilder_create                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_pathbuilder_t; cdecl;                                                                                                                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pathbuilder_create2                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const path_builder: sk_pathbuilder_t): sk_pathbuilder_t; cdecl;                                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_cubic_to               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const point1, point2, point3: psk_point_t); cdecl;                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_destroy                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t); cdecl;                                                                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pathbuilder_detach                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_pathbuilder_t): sk_path_t; cdecl;                                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_get_bounds             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_pathbuilder_t; out result: sk_rect_t); cdecl;                                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pathbuilder_get_fill_type          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pathbuilder_t): sk_pathfilltype_t; cdecl;                                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_inc_reserve            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; extra_point_count, extra_verb_count: int32_t); cdecl;                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_line_to                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const cpoint: psk_point_t); cdecl;                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_move_to                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const cpoint: psk_point_t); cdecl;                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_offset                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; dx, dy: float); cdecl;                                                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_polyline_to            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const points: psk_point_t; count: int32_t); cdecl;                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_quad_to                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const point1, point2: psk_point_t); cdecl;                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_r_conic_to             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const point1, point2: psk_point_t; weight: float); cdecl;                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_r_cubic_to             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const point1, point2, point3: psk_point_t); cdecl;                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_r_line_to              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const point: psk_point_t); cdecl;                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_r_quad_to              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const point1, point2: psk_point_t); cdecl;                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_reset                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t); cdecl;                                                                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_set_filltype           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; value: sk_pathfilltype_t); cdecl;                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pathbuilder_snapshot               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pathbuilder_t): sk_path_t; cdecl;                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_toggle_inverse_filltype{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t); cdecl;                                                                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_patheffect.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_patheffect_make_1dpath         {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const path: sk_path_t; advance, phase: float; style: sk_patheffect1dstyle_t): sk_patheffect_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_patheffect_make_2dline         {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(width: float; const matrix: psk_matrix_t): sk_patheffect_t; cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_patheffect_make_2dpath         {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const matrix: psk_matrix_t; const path: sk_path_t): sk_patheffect_t; cdecl;                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_patheffect_make_compose        {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(outer, inner: sk_patheffect_t): sk_patheffect_t; cdecl;                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_patheffect_make_corner         {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(radius: float): sk_patheffect_t; cdecl;                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_patheffect_make_dash           {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const intervals: pfloat; count: int32_t; phase: float): sk_patheffect_t; cdecl;                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_patheffect_make_discrete       {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(seg_length, deviation: float; seed_assist: uint32_t): sk_patheffect_t; cdecl;                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_patheffect_make_matrix         {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const matrix: psk_matrix_t): sk_patheffect_t; cdecl;                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_patheffect_make_merge          {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(effect1, effect2: sk_patheffect_t; op: sk_pathop_t): sk_patheffect_t; cdecl;                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_patheffect_make_stroke         {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(width: float; join: sk_strokejoin_t; cap: sk_strokecap_t; miter: float): sk_patheffect_t; cdecl;      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_patheffect_make_stroke_and_fill{$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(): sk_patheffect_t; cdecl;                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_patheffect_make_sum            {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(effect1, effect2: sk_patheffect_t): sk_patheffect_t; cdecl;                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_patheffect_make_translate      {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(dx, dy: float): sk_patheffect_t; cdecl;                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_patheffect_make_trim           {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(start, stop: float; mode: sk_patheffecttrimmode_t): sk_patheffect_t; cdecl;                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_pathmeasure.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pathmeasure_create                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const path: sk_path_t; force_closed: bool; res_scale: float): sk_pathmeasure_t; cdecl;                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathmeasure_destroy                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathmeasure_t); cdecl;                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pathmeasure_get_length              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_pathmeasure_t): float; cdecl;                                                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pathmeasure_get_matrix              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_pathmeasure_t; distance: float; out matrix: sk_matrix_t; matrix_flags: uint32_t): bool; cdecl;    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pathmeasure_get_position_and_tangent{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_pathmeasure_t; distance: float; out position: sk_point_t; out tangent: sk_vector_t): bool; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pathmeasure_get_segment             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_pathmeasure_t; start, stop: float; start_with_move_to: bool): sk_path_t; cdecl;                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pathmeasure_is_closed               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_pathmeasure_t): bool; cdecl;                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pathmeasure_next_contour            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_pathmeasure_t): bool; cdecl;                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_picture.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_picture_get_cull_rect      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_picture_t; out result: sk_rect_t); cdecl;                                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_picture_make_from_stream   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(stream: sk_stream_t): sk_picture_t; cdecl;                                                                                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_picture_make_shader        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_picture_t; tile_mode_x, tile_mode_y: sk_tilemode_t; filter_mode: sk_filtermode_t; const local_matrix: psk_matrix_t; const tile_rect: psk_rect_t): sk_shader_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_picture_playback           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_picture_t; canvas: sk_canvas_t); cdecl;                                                                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_picture_serialize_to_stream{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_picture_t; w_stream: sk_wstream_t); cdecl;                                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_picturerecorder.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_picturerecorder_begin_recording  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_picturerecorder_t; const bounds: psk_rect_t): sk_canvas_t; cdecl;     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_picturerecorder_create           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_picturerecorder_t; cdecl;                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_picturerecorder_destroy          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_picturerecorder_t); cdecl;                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_picturerecorder_finish_recording {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_picturerecorder_t): sk_picture_t; cdecl;                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_picturerecorder_finish_recording2{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_picturerecorder_t; const cull_rect: psk_rect_t): sk_picture_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_pixmap.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pixmap_create         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const image_info: psk_imageinfo_t; const pixels: Pointer; row_bytes: size_t): sk_pixmap_t; cdecl;                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pixmap_destroy        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pixmap_t); cdecl;                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pixmap_erase          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t; color: sk_color_t; const area: psk_irect_t): bool; cdecl;                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pixmap_erase2         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t; const color: psk_color4f_t; color_space: sk_colorspace_t; const area: psk_irect_t): bool; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pixmap_extract_subset {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t; dest: sk_pixmap_t; const area: psk_irect_t): bool; cdecl;                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pixmap_get_alpha      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t; x, y: int32_t): float; cdecl;                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pixmap_get_alpha_type {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t): sk_alphatype_t; cdecl;                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pixmap_get_color      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t; x, y: int32_t): sk_color_t; cdecl;                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pixmap_get_color_space{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t): sk_colorspace_t; cdecl;                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pixmap_get_color_type {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t): sk_colortype_t; cdecl;                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pixmap_get_height     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t): int32_t; cdecl;                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pixmap_get_image_info {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_pixmap_t; out result: sk_imageinfo_t); cdecl;                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pixmap_get_pixel_addr {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t; x, y: int32_t): Pointer; cdecl;                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pixmap_get_pixels     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t): Pointer; cdecl;                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pixmap_get_row_bytes  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t): size_t; cdecl;                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pixmap_get_width      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t): int32_t; cdecl;                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pixmap_read_pixels    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, dest: sk_pixmap_t; src_x, src_y: int32_t): bool; cdecl;                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pixmap_scale_pixels   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, dest: sk_pixmap_t; const sampling: psk_samplingoptions_t): bool; cdecl;                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pixmap_set_colorspace {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pixmap_t; value: sk_colorspace_t); cdecl;                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_refcnt.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_refcnt_ref  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_refcnt_t); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_refcnt_unref{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_refcnt_t); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_region.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_contains             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, region: sk_region_t): bool; cdecl;                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_contains2            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_region_t; const rect: psk_irect_t): bool; cdecl;                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_contains3            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_region_t; x, y: int32_t): bool; cdecl;                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_create               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_region_t; cdecl;                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_create2              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const region: sk_region_t): sk_region_t; cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_region_destroy              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_region_t); cdecl;                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_get_boundary_path    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_region_t): sk_path_t; cdecl;                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_region_get_bounds           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_region_t; out result: sk_irect_t); cdecl;                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_intersects           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, region: sk_region_t): bool; cdecl;                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_intersects2          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_region_t; const rect: psk_irect_t): bool; cdecl;                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_is_complex           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_region_t): bool; cdecl;                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_is_empty             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_region_t): bool; cdecl;                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_is_equal             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, region: sk_region_t): bool; cdecl;                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_is_rect              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_region_t): bool; cdecl;                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_op                   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_region_t; const region: sk_region_t; op: sk_regionop_t): bool; cdecl;     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_op2                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_region_t; const rect: psk_irect_t; op: sk_regionop_t): bool; cdecl;       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_quick_contains       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_region_t; const rect: psk_irect_t): bool; cdecl;                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_quick_reject         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, region: sk_region_t): bool; cdecl;                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_quick_reject2        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_region_t; const rect: psk_irect_t): bool; cdecl;                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_region_set_empty            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_region_t); cdecl;                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_set_path             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_region_t; const path: sk_path_t; const clip: sk_region_t): bool; cdecl;   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_set_rect             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_region_t; const rect: psk_irect_t): bool; cdecl;                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_set_rects            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_region_t; const rects: psk_irect_t; count: int32_t): bool; cdecl;         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_region_translate            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_region_t; x, y: int32_t); cdecl;                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_regioncliperator_create     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const region: sk_region_t; const clip: psk_irect_t): sk_regioncliperator_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_regioncliperator_destroy    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_regioncliperator_t); cdecl;                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_regioncliperator_get_current{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_regioncliperator_t; out result: sk_irect_t); cdecl;                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_regioncliperator_move_next  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_regioncliperator_t): bool; cdecl;                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_regioniterator_create       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const region: sk_region_t): sk_regioniterator_t; cdecl;                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_regioniterator_destroy      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_regioniterator_t); cdecl;                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_regioniterator_get_current  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_regioniterator_t; out result: sk_irect_t); cdecl;                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_regioniterator_move_next    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_regioniterator_t): bool; cdecl;                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_regioniterator_reset        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_regioniterator_t); cdecl;                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_regionspanerator_create     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const region: sk_region_t; y, left, right: int32_t): sk_regionspanerator_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_regionspanerator_destroy    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_regionspanerator_t); cdecl;                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_regionspanerator_next       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_regionspanerator_t; out elem: sk_ipoint_t): bool; cdecl;                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_rrect.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_rrect_contains        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_rrect_t; const rect: psk_rect_t): bool; cdecl;                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_rrect_create          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_rrect_t; cdecl;                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_rrect_create2         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const rrect: sk_rrect_t): sk_rrect_t; cdecl;                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_deflate         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; dx, dy: float); cdecl;                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_destroy         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t); cdecl;                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_rrect_get_height      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_rrect_t): float; cdecl;                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_get_radii       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_rrect_t; corner: sk_rrectcorner_t; out result: sk_vector_t); cdecl;                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_get_rect        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_rrect_t; out result: sk_rect_t); cdecl;                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_get_simple_radii{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_rrect_t; out result: sk_vector_t); cdecl;                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_rrect_get_width       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_rrect_t): float; cdecl;                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_inflate         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; dx, dy: float); cdecl;                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_rrect_is_complex      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_rrect_t): bool; cdecl;                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_rrect_is_empty        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_rrect_t): bool; cdecl;                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_rrect_is_equal        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, rrect: sk_rrect_t): bool; cdecl;                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_rrect_is_nine_patch   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_rrect_t): bool; cdecl;                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_rrect_is_oval         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_rrect_t): bool; cdecl;                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_rrect_is_rect         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_rrect_t): bool; cdecl;                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_rrect_is_simple       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_rrect_t): bool; cdecl;                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_rrect_is_valid        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_rrect_t): bool; cdecl;                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_offset          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; dx, dy: float); cdecl;                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_set_empty       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t); cdecl;                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_set_nine_patch  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; const rect: psk_rect_t; radius_left, radius_top, radius_right, radius_bottom: float); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_set_oval        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; const rect: psk_rect_t); cdecl;                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_set_rect        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; const rect: psk_rect_t); cdecl;                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_set_rect2       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; const rect: psk_rect_t; const radii: psk_vector_t); cdecl;                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_set_rect3       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; const rect: psk_rect_t; radius_x, radius_y: float); cdecl;                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_rrect_transform       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_rrect_t; const matrix: psk_matrix_t): sk_rrect_t; cdecl;                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_runtimeeffect.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_get_child_count       {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_runtimeeffect_t): int32_t; cdecl;                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_get_child_name        {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_runtimeeffect_t; index: int32_t): MarshaledAString; cdecl;                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_get_child_type        {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_runtimeeffect_t; index: int32_t): sk_runtimeeffectchildtype_t; cdecl;                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_get_uniform_count     {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_runtimeeffect_t): int32_t; cdecl;                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_get_uniform_data_size {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_runtimeeffect_t): size_t; cdecl;                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_get_uniform_name      {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_runtimeeffect_t; index: int32_t): MarshaledAString; cdecl;                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_get_uniform_offset    {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_runtimeeffect_t; index: int32_t): size_t; cdecl;                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_get_uniform_type      {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_runtimeeffect_t; index: int32_t): sk_runtimeeffectuniformtype_t; cdecl;                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_get_uniform_type_count{$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_runtimeeffect_t; index: int32_t): int32_t; cdecl;                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_index_of_child        {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_runtimeeffect_t; const name: MarshaledAString): int32_t; cdecl;                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_index_of_uniform      {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_runtimeeffect_t; const name: MarshaledAString): int32_t; cdecl;                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_make_blender          {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_runtimeeffect_t; const uniforms: Pointer; children: psk_blender_t): sk_blender_t; cdecl;                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_make_color_filter     {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_runtimeeffect_t; const uniforms: Pointer; children: psk_colorfilter_t): sk_colorfilter_t; cdecl;                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_make_for_blender      {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const sksl: MarshaledAString; error_text: sk_string_t): sk_runtimeeffect_t; cdecl;                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_make_for_color_filter {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const sksl: MarshaledAString; error_text: sk_string_t): sk_runtimeeffect_t; cdecl;                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_make_for_shader       {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const sksl: MarshaledAString; error_text: sk_string_t): sk_runtimeeffect_t; cdecl;                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_make_shader           {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_runtimeeffect_t; const uniforms: Pointer; children: psk_shader_t; const local_matrix: psk_matrix_t; opaque: bool): sk_shader_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_shader.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_blend                      {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(mode: sk_blendmode_t; dest, src: sk_shader_t): sk_shader_t; cdecl;                                                                                                                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_color                      {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(color: sk_color_t): sk_shader_t; cdecl;                                                                                                                                                                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_color2                     {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const color: psk_color4f_t; color_space: sk_colorspace_t): sk_shader_t; cdecl;                                                                                                                                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_gradient_linear            {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const points: psk_point_t; const colors: psk_color_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;                                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_gradient_linear2           {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const points: psk_point_t; const colors: psk_color4f_t; color_space: sk_colorspace_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_gradient_radial            {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const center: psk_point_t; radius: float; const colors: psk_color_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_gradient_radial2           {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const center: psk_point_t; radius: float; const colors: psk_color4f_t; color_space: sk_colorspace_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_gradient_sweep             {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(center_x, center_y: float; const colors: psk_color_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; start_angle, end_angle: float; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_gradient_sweep2            {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(center_x, center_y: float; const colors: psk_color4f_t; color_space: sk_colorspace_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; start_angle, end_angle: float; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_gradient_two_point_conical {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const start: psk_point_t; start_radius: float; const &end: psk_point_t; end_radius: float; const colors: psk_color_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_gradient_two_point_conical2{$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const start: psk_point_t; start_radius: float; const &end: psk_point_t; end_radius: float; const colors: psk_color4f_t; color_space: sk_colorspace_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_perlin_noise_fractal_noise {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(base_frequency_x, base_frequency_y: float; num_octaves: int32_t; seed: float; const tile_size: psk_isize_t): sk_shader_t; cdecl;                                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_perlin_noise_turbulence    {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(base_frequency_x, base_frequency_y: float; num_octaves: int32_t; seed: float; const tile_size: psk_isize_t): sk_shader_t; cdecl;                                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_with_color_filter          {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_shader_t; color_filter: sk_colorfilter_t): sk_shader_t; cdecl;                                                                                                                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_with_local_matrix          {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_shader_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;                                                                                                                                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_stream.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_streamadapter_create    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: Pointer): sk_streamadapter_t; cdecl;    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_streamadapter_destroy   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_streamadapter_t); cdecl;                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_streamadapter_set_procs {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const procs: psk_streamadapter_procs_t); cdecl;  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_wstreamadapter_create   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: Pointer): sk_wstreamadapter_t; cdecl;   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_wstreamadapter_destroy  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_wstreamadapter_t); cdecl;               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_wstreamadapter_set_procs{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const procs: psk_wstreamadapter_procs_t); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_string.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_string_create  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_string_t; cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_string_destroy {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_string_t); cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_string_get_text{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_string_t): MarshaledAString; cdecl;        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_string_set_text{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_string_t; const value: MarshaledAString); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_surface.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_surface_draw                    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_surface_t; canvas: sk_canvas_t; x, y: float; paint: sk_paint_t); cdecl;                                                                                                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_surface_flush                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_surface_t); cdecl;                                                                                                                                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_surface_flush_and_submit        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_surface_t; sync_cpu: bool); cdecl;                                                                                                                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_get_canvas              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_surface_t): sk_canvas_t; cdecl;                                                                                                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_surface_get_props               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_surface_t; out result: sk_surfaceprops_t); cdecl;                                                                                                                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_make_from_ca_metal_layer{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: gr_directcontext_t; layer: gr_mtl_handle_t; origin: gr_surfaceorigin_t; sample_count: int32_t; color_type: sk_colortype_t; color_space: sk_colorspace_t; const props: psk_surfaceprops_t; out drawable: gr_mtl_handle_t): sk_surface_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_make_from_mtk_view      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: gr_directcontext_t; layer: gr_mtl_handle_t; origin: gr_surfaceorigin_t; sample_count: int32_t; color_type: sk_colortype_t; color_space: sk_colorspace_t; const props: psk_surfaceprops_t): sk_surface_t; cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_make_from_render_target {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: gr_directcontext_t; const render_target: gr_backendrendertarget_t; origin: gr_surfaceorigin_t; color_type: sk_colortype_t; color_space: sk_colorspace_t; const props: psk_surfaceprops_t): sk_surface_t; cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_make_from_texture       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: gr_directcontext_t; const texture: gr_backendtexture_t; origin: gr_surfaceorigin_t; sample_count: int32_t; color_type: sk_colortype_t; color_space: sk_colorspace_t; const props: psk_surfaceprops_t): sk_surface_t; cdecl;                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_make_image_snapshot     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_surface_t): sk_image_t; cdecl;                                                                                                                                                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_make_image_snapshot2    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_surface_t; const bounds: psk_irect_t): sk_image_t; cdecl;                                                                                                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_make_raster             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const image_info: psk_imageinfo_t; row_bytes: size_t; const props: psk_surfaceprops_t): sk_surface_t; cdecl;                                                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_make_raster_direct      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const pixmap: sk_pixmap_t; proc: sk_surface_raster_release_proc; proc_context: Pointer; const props: psk_surfaceprops_t): sk_surface_t; cdecl;                                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_make_render_target      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: gr_directcontext_t; is_budgeted: bool; const image_info: psk_imageinfo_t; sample_count: int32_t; origin: gr_surfaceorigin_t; const props: psk_surfaceprops_t; should_create_with_mips: bool): sk_surface_t; cdecl;                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_peek_pixels             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_surface_t): sk_pixmap_t; cdecl;                                                                                                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_read_pixels             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_surface_t; const dest: sk_pixmap_t; src_x, src_y: int32_t): bool; cdecl;                                                                                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}                                                                                                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}                                                                                                                                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_svgcanvas.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_svgcanvas_make{$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const bounds: psk_rect_t; w_stream: sk_wstream_t; flags: uint32_t): sk_canvas_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}                                                                                                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}                                                                                                                                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_textblob.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textblob_get_intercepts                        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textblob_t; const bounds: pfloat; result: pfloat; const paint: sk_paint_t): int32_t; cdecl;                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textblob_make_from_text                        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const text: Pointer; size: size_t; const font: sk_font_t; encoding: sk_textencoding_t): sk_textblob_t; cdecl;                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textblob_make_from_text_horizontally_positioned{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const text: Pointer; size: size_t; const x_positions: pfloat; y: float; const font: sk_font_t; encoding: sk_textencoding_t): sk_textblob_t; cdecl;       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textblob_make_from_text_positioned             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const text: Pointer; size: size_t; const positions: psk_point_t; const font: sk_font_t; encoding: sk_textencoding_t): sk_textblob_t; cdecl;              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textblob_make_from_text_transform              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const text: Pointer; size: size_t; const matrices: psk_rotationscalematrix_t; const font: sk_font_t; encoding: sk_textencoding_t): sk_textblob_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textblob_ref                                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_textblob_t); cdecl;                                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textblob_unref                                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_textblob_t); cdecl;                                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_tracememorydump.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_tracememorydumpbaseclass_create   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(detailed_dump, dump_wrapped_objects: bool; context: Pointer): sk_tracememorydumpbaseclass_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_tracememorydumpbaseclass_destroy  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_tracememorydumpbaseclass_t); cdecl;                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_tracememorydumpbaseclass_set_procs{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const procs: psk_tracememorydumpbaseclass_procs_t); cdecl;                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_typeface.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_typeface_get_family_name {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_typeface_t): sk_string_t; cdecl;                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_typeface_get_slant       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_typeface_t): sk_fontslant_t; cdecl;                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_typeface_get_style       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_typeface_t; out result: sk_fontstyle_t); cdecl;                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_typeface_get_weight      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_typeface_t): int32_t; cdecl;                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_typeface_get_width       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_typeface_t): int32_t; cdecl;                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_typeface_make_default    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_typeface_t; cdecl;                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_typeface_make_from_file  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const file_name: MarshaledAString; ttc_index: int32_t): sk_typeface_t; cdecl;             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_typeface_make_from_stream{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(stream: sk_stream_t; ttc_index: int32_t): sk_typeface_t; cdecl;                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_typeface_make_from_name  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const family_name: MarshaledAString; const style: psk_fontstyle_t): sk_typeface_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_version.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_version_get_build    {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(): int32_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_version_get_major    {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(): int32_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_version_get_milestone{$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(): int32_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_version_get_minor    {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(): int32_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_vertices.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_vertices_make_copy{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(vertex_mode: sk_vertexmode_t; vertex_count: int32_t; const positions, textures: psk_point_t; const colors: psk_color_t; index_count: int32_t; const indices: puint16_t): sk_vertices_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_vertices_ref      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_vertices_t); cdecl;                                                                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_vertices_unref    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_vertices_t); cdecl;                                                                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/particles/include/sk4d_particleeffect.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_particleeffect_get_position          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_particleeffect_t; out result: sk_point_t); cdecl;                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_particleeffect_get_rate              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_particleeffect_t): float; cdecl;                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_particleeffect_get_uniform           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_particleeffect_t; index: size_t; out result: sk_particleuniform_t); cdecl;                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_particleeffect_get_uniform_count     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_particleeffect_t): size_t; cdecl;                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_particleeffect_get_uniform_data      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_particleeffect_t): pfloat; cdecl;                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_particleeffect_get_uniform_data_count{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_particleeffect_t): int32_t; cdecl;                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_particleeffect_get_uniform_name      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_particleeffect_t; index: size_t): sk_string_t; cdecl;                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_particleeffect_init                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(); cdecl;                                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_particleeffect_make_from_file        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const file_name: MarshaledAString): sk_particleeffect_t; cdecl;                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_particleeffect_make_from_stream      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(stream: sk_stream_t; resource_provider: sk_resourceprovider_t): sk_particleeffect_t; cdecl;                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_particleeffect_render                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_particleeffect_t; canvas: sk_canvas_t); cdecl;                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_particleeffect_set_position          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_particleeffect_t; const value: psk_point_t); cdecl;                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_particleeffect_set_rate              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_particleeffect_t; value: float); cdecl;                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_particleeffect_set_uniform           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_particleeffect_t; const name: MarshaledAString; const data: pfloat; count: int32_t): bool; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_particleeffect_start                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_particleeffect_t; now: double; looping: bool); cdecl;                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_particleeffect_update                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_particleeffect_t; now: double); cdecl;                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skottie/include/sk4d_skottie.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_skottieanimation_get_duration    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_skottieanimation_t): double; cdecl;                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_skottieanimation_get_fps         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_skottieanimation_t): double; cdecl;                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_skottieanimation_get_in_point    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_skottieanimation_t): double; cdecl;                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_skottieanimation_get_out_point   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_skottieanimation_t): double; cdecl;                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_skottieanimation_get_size        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_skottieanimation_t; out result: sk_size_t); cdecl;                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_skottieanimation_get_version     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_skottieanimation_t): MarshaledAString; cdecl;                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_skottieanimation_make_from_file  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const file_name: MarshaledAString): sk_skottieanimation_t; cdecl;                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_skottieanimation_make_from_stream{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(stream: sk_stream_t; resource_provider: sk_resourceprovider_t): sk_skottieanimation_t; cdecl;                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_skottieanimation_ref             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_skottieanimation_t); cdecl;                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_skottieanimation_render          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_skottieanimation_t; canvas: sk_canvas_t; const dest: psk_rect_t; render_flags: uint32_t); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_skottieanimation_seek_frame      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_skottieanimation_t; tick: double); cdecl;                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_skottieanimation_seek_frame_time {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_skottieanimation_t; tick: double); cdecl;                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_skottieanimation_unref           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_skottieanimation_t); cdecl;                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skparagraph/include/sk4d_paragraph.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraph_destroy                         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraph_t); cdecl;                                                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraph_did_exceed_max_lines            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_paragraph_t): bool; cdecl;                                                                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraph_get_alphabetic_baseline         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_paragraph_t): float; cdecl;                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraph_get_glyph_position_at_coordinate{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraph_t; dx, dy: float; out result: sk_positionaffinity_t); cdecl;                                                                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraph_get_height                      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_paragraph_t): float; cdecl;                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraph_get_ideographic_baseline        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_paragraph_t): float; cdecl;                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraph_get_line_metrics                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_paragraph_t; result: psk_metrics_t): size_t; cdecl;                                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraph_get_longest_line                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_paragraph_t): float; cdecl;                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraph_get_max_intrinsic_width         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_paragraph_t): float; cdecl;                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraph_get_max_width                   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_paragraph_t): float; cdecl;                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraph_get_min_intrinsic_width         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_paragraph_t): float; cdecl;                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraph_get_rects_for_placeholders      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_paragraph_t; result: psk_textbox_t): size_t; cdecl;                                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraph_get_rects_for_range             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_paragraph_t; start, &end: uint32_t; rect_height_style: sk_rectheightstyle_t; rect_width_style: sk_rectwidthstyle_t; result: psk_textbox_t): size_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraph_get_word_boundary               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraph_t; offset: uint32_t; out start, &end: uint32_t); cdecl;                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraph_layout                          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraph_t; width: float); cdecl;                                                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraph_paint                           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraph_t; canvas: sk_canvas_t; x, y: float); cdecl;                                                                                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraph_to_path                         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_paragraph_t): sk_path_t; cdecl;                                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skparagraph/include/sk4d_paragraphbuilder.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphbuilder_add_placeholder{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphbuilder_t; const placeholder: psk_placeholderstyle_t); cdecl;                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphbuilder_add_text       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphbuilder_t; const text: pchar; size: size_t); cdecl;                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphbuilder_build          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_paragraphbuilder_t): sk_paragraph_t; cdecl;                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphbuilder_create         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const paragraph_style: sk_paragraphstyle_t): sk_paragraphbuilder_t; cdecl;                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphbuilder_create2        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const paragraph_style: sk_paragraphstyle_t; font_provider: sk_typefacefontprovider_t; enable_font_fallback: bool): sk_paragraphbuilder_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphbuilder_destroy        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphbuilder_t); cdecl;                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphbuilder_pop            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphbuilder_t); cdecl;                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphbuilder_push_style     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphbuilder_t; const text_style: sk_textstyle_t); cdecl;                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skparagraph/include/sk4d_paragraphstyle.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_create                   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_paragraphstyle_t; cdecl;                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_destroy                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphstyle_t); cdecl;                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_disable_hinting          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphstyle_t); cdecl;                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_get_ellipsis             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paragraphstyle_t): sk_string_t; cdecl;                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_get_height               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paragraphstyle_t): float; cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_get_max_lines            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paragraphstyle_t): size_t; cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_get_strut_style          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paragraphstyle_t): sk_strutstyle_t; cdecl;                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_get_text_align           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paragraphstyle_t): sk_textalign_t; cdecl;                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_get_text_direction       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paragraphstyle_t): sk_textdirection_t; cdecl;                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_get_text_height_behaviors{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paragraphstyle_t): uint32_t; cdecl;                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_get_text_style           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paragraphstyle_t): sk_textstyle_t; cdecl;                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_set_ellipsis             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphstyle_t; const value: MarshaledAString); cdecl;              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_set_height               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphstyle_t; value: float); cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_set_max_lines            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphstyle_t; value: size_t); cdecl;                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_set_strut_style          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphstyle_t; const value: sk_strutstyle_t); cdecl;               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_set_text_align           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphstyle_t; value: sk_textalign_t); cdecl;                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_set_text_direction       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphstyle_t; value: sk_textdirection_t); cdecl;                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_set_text_height_behaviors{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphstyle_t; value: uint32_t); cdecl;                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_set_text_style           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphstyle_t; value: sk_textstyle_t); cdecl;                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_strutstyle_create                       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_strutstyle_t; cdecl;                                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_strutstyle_destroy                      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_strutstyle_t); cdecl;                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_strutstyle_get_enabled                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_strutstyle_t): bool; cdecl;                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_strutstyle_get_font_families            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_strutstyle_t; const result: PMarshaledAString): size_t; cdecl;  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_strutstyle_get_font_size                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_strutstyle_t): float; cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_strutstyle_get_font_style               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_strutstyle_t; out result: sk_fontstyle_t); cdecl;               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_strutstyle_get_force_height             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_strutstyle_t): bool; cdecl;                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_strutstyle_get_half_leading             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_strutstyle_t): bool; cdecl;                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_strutstyle_get_height_multiplier        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_strutstyle_t): float; cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_strutstyle_get_leading                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_strutstyle_t): float; cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_strutstyle_is_equal                     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_strutstyle_t; const strut_style: sk_strutstyle_t): bool; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_strutstyle_set_enabled                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_strutstyle_t; value: bool); cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_strutstyle_set_font_families            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_strutstyle_t; const values: PMarshaledAString; count: size_t); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_strutstyle_set_font_size                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_strutstyle_t; value: float); cdecl;                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_strutstyle_set_font_style               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_strutstyle_t; value: psk_fontstyle_t); cdecl;                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_strutstyle_set_force_height             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_strutstyle_t; value: bool); cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_strutstyle_set_half_leading             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_strutstyle_t; value: bool); cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_strutstyle_set_height_multiplier        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_strutstyle_t; value: float); cdecl;                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_strutstyle_set_leading                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_strutstyle_t; value: float); cdecl;                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skparagraph/include/sk4d_textstyle.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_add_font_feature        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; const feature: MarshaledAString; value: int32_t); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_add_shadow              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; const shadow: psk_textshadow_t); cdecl;                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_clear_background_color  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t); cdecl;                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_clear_foreground_color  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t); cdecl;                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_create                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_textstyle_t; cdecl;                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_destroy                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t); cdecl;                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_background          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): sk_paint_t; cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_color               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): sk_color_t; cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_decoration_color    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): sk_color_t; cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_decoration_style    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): sk_textdecorationstyle_t; cdecl;                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_decoration_thickness{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): float; cdecl;                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_decorations         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): uint32_t; cdecl;                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_font_families       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t; const result: PMarshaledAString): size_t; cdecl;   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_get_font_metrics        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_textstyle_t; out result: sk_fontmetrics_t); cdecl;              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_font_size           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): float; cdecl;                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_get_font_style          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_textstyle_t; out result: sk_fontstyle_t); cdecl;                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_foreground          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): sk_paint_t; cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_half_leading        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): bool; cdecl;                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_height_multiplier   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): float; cdecl;                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_letter_spacing      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): float; cdecl;                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_locale              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): sk_string_t; cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_word_spacing        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): float; cdecl;                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_is_equal                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, text_style: sk_textstyle_t): bool; cdecl;                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_reset_font_features     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t); cdecl;                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_reset_shadows           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t); cdecl;                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_background_color    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; paint: sk_paint_t); cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_color               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; value: sk_color_t); cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_decoration_color    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; value: sk_color_t); cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_decoration_style    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; value: sk_textdecorationstyle_t); cdecl;                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_decoration_thickness{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; value: float); cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_decorations         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; value: uint32_t); cdecl;                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_font_families       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; const values: PMarshaledAString; count: size_t); cdecl;  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_font_size           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; value: float); cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_font_style          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; const value: psk_fontstyle_t); cdecl;                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_foreground_color    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; paint: sk_paint_t); cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_half_leading        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; value: bool); cdecl;                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_height_multiplier   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; value: float); cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_letter_spacing      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; value: float); cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_locale              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; const value: MarshaledAString); cdecl;                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_word_spacing        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; value: float); cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skparagraph/include/sk4d_typefacefontprovider.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_typefacefontprovider_create            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_typefacefontprovider_t; cdecl;                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_typefacefontprovider_register_typeface {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_typefacefontprovider_t; typeface: sk_typeface_t); cdecl;                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_typefacefontprovider_register_typeface2{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_typefacefontprovider_t; typeface: sk_typeface_t; const family_name: MarshaledAString); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skresources/include/sk4d_resources.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_resourceproviderbaseclass_create   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(predecode: bool; context: Pointer): sk_resourceproviderbaseclass_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_resourceproviderbaseclass_set_procs{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const procs: psk_resourceproviderbaseclass_procs_t); cdecl;                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skshaper/include/sk4d_shaper.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_shaper_create {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_shaper_t; cdecl;                                                                                                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_shaper_destroy{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_shaper_t); cdecl;                                                                                                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_shaper_shape  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_shaper_t; const text: pchar; size: size_t; const font: sk_font_t; left_to_right: bool; width: float; const offset: psk_point_t; end_point: psk_point_t): sk_textblob_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skunicode/include/sk4d_unicode.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_unicode_create              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_unicode_t; cdecl;                                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_unicode_destroy             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_unicode_t); cdecl;                                                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_unicode_for_each_bidi_region{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_unicode_t; const text: puint16_t; units: int32_t; direction: sk_direction_t; proc: sk_unicode_bidi_region_proc; context: Pointer); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_unicode_for_each_break      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_unicode_t; const text: pchar16_t; units: int32_t; &type: sk_breaktype_t; proc: sk_unicode_break_proc; context: Pointer); cdecl;           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_unicode_for_each_codepoint  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_unicode_t; const text: pchar16_t; units: int32_t; proc: sk_unicode_codepoint_proc; context: Pointer); cdecl;                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/svg/include/sk4d_svgdom.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_svgdom_find_node_by_id   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_svgdom_t; const id: MarshaledAString): sk_svgnode_t; cdecl;                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_svgdom_get_root          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_svgdom_t): sk_svgsvg_t; cdecl;                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_svgdom_make_from_file    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const file_name: MarshaledAString): sk_svgdom_t; cdecl;                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_svgdom_make_from_stream  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(stream: sk_stream_t; resource_provider: sk_resourceprovider_t): sk_svgdom_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_svgdom_render            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_svgdom_t; canvas: sk_canvas_t); cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_svgdom_set_container_size{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_svgdom_t; const size: psk_size_t); cdecl;                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/svg/include/sk4d_svgnode.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_svgnode_set_attribute{$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(self: sk_svgnode_t; const name, value: MarshaledAString): bool; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/svg/include/sk4d_svgsvg.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_svgsvg_get_height               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_svgsvg_t; out result: sk_svglength_t); cdecl;                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_svgsvg_get_intrinsic_size       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_svgsvg_t; const view_port: psk_size_t; dpi: float; out result: sk_size_t); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_svgsvg_get_preserve_aspect_ratio{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_svgsvg_t; out result: sk_svgpreserveaspectratio_t); cdecl;                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_svgsvg_get_view_box             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_svgsvg_t; out result: sk_rect_t): bool; cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_svgsvg_get_width                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_svgsvg_t; out result: sk_svglength_t); cdecl;                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_svgsvg_get_x                    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_svgsvg_t; out result: sk_svglength_t); cdecl;                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_svgsvg_get_y                    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_svgsvg_t; out result: sk_svglength_t); cdecl;                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_svgsvg_set_height               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_svgsvg_t; value: psk_svglength_t); cdecl;                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_svgsvg_set_preserve_aspect_ratio{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_svgsvg_t; value: psk_svgpreserveaspectratio_t); cdecl;                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_svgsvg_set_view_box             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_svgsvg_t; view_box: psk_rect_t); cdecl;                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_svgsvg_set_width                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_svgsvg_t; value: psk_svglength_t); cdecl;                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_svgsvg_set_x                    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_svgsvg_t; value: psk_svglength_t); cdecl;                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_svgsvg_set_y                    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_svgsvg_t; value: psk_svglength_t); cdecl;                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$IFDEF SK_DYNAMIC_LOADING}
    class property LibHandle: HMODULE read FLibHandle;
    {$ENDIF}
  end;

  {$ENDREGION}

implementation

uses
  { Delphi }
  {$IF DEFINED(ANDROID) and DEFINED(SK_DYNAMIC_LOADING)}
  System.IOUtils,
  {$ENDIF}
  System.Math;

{$REGION 'Dynamic loading utils'}

{$IFDEF SK_DYNAMIC_LOADING}

function GetProcAddress(AModule: HMODULE; AName: System.PChar): Pointer;
begin
  {$IFDEF MSWINDOWS}
  Result := Winapi.Windows.GetProcAddress(AModule, AName);
  {$ELSE}
  Result := System.SysUtils.GetProcAddress(AModule, AName);
  {$ENDIF}
  if Result = nil then
    raise ESkiaAPI.CreateFmt('"%s" function address could not be retrieved from Skia library', [AName]) at ReturnAddress;
end;

{$ENDIF}

{$ENDREGION}

{$REGION 'Skia API'}

{ TSkiaAPI }

class constructor TSkiaAPI.Create;
begin
  SetExceptionMask(exAllArithmeticExceptions);
{$IFDEF SK_DYNAMIC_LOADING}
  Initialize;
end;

class destructor TSkiaAPI.Destroy;
begin
  Terminate;
{$ENDIF}
end;

class procedure TSkiaAPI.Initialize;
begin
{$IFDEF SK_DYNAMIC_LOADING}
  if AtomicIncrement(FRefCount) = 1 then
  begin
    // Some Android devices, normally old, need the full path of the library,
    // and other devices, normally new, do not accept the full path.
    {$IFDEF ANDROID}
    FLibHandle := SafeLoadLibrary(TSkiaAPI.LibName);
    if FLibHandle = 0 then
      FLibHandle := SafeLoadLibrary(TPath.Combine(TPath.GetLibraryPath, TSkiaAPI.LibName));
    {$ELSE}
    FLibHandle := SafeLoadLibrary(TSkiaAPI.LibName);
    {$ENDIF}
    if FLibHandle = 0 then
      raise ESkiaAPI.Create('Skia library could not be loaded');
{$ELSE}
end;
{$ENDIF}

    {$REGION 'include/c/gr4d_backendsurface.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    gr4d_backendrendertarget_create_gl          := GetProcAddress(FLibHandle, 'gr4d_backendrendertarget_create_gl');
    gr4d_backendrendertarget_create_mtl         := GetProcAddress(FLibHandle, 'gr4d_backendrendertarget_create_mtl');
    gr4d_backendrendertarget_destroy            := GetProcAddress(FLibHandle, 'gr4d_backendrendertarget_destroy');
    gr4d_backendrendertarget_get_backend_api    := GetProcAddress(FLibHandle, 'gr4d_backendrendertarget_get_backend_api');
    gr4d_backendrendertarget_get_height         := GetProcAddress(FLibHandle, 'gr4d_backendrendertarget_get_height');
    gr4d_backendrendertarget_get_sample_count   := GetProcAddress(FLibHandle, 'gr4d_backendrendertarget_get_sample_count');
    gr4d_backendrendertarget_get_stencil_bits   := GetProcAddress(FLibHandle, 'gr4d_backendrendertarget_get_stencil_bits');
    gr4d_backendrendertarget_get_width          := GetProcAddress(FLibHandle, 'gr4d_backendrendertarget_get_width');
    gr4d_backendrendertarget_is_valid           := GetProcAddress(FLibHandle, 'gr4d_backendrendertarget_is_valid');
    gr4d_backendtexture_create_gl               := GetProcAddress(FLibHandle, 'gr4d_backendtexture_create_gl');
    gr4d_backendtexture_create_mtl              := GetProcAddress(FLibHandle, 'gr4d_backendtexture_create_mtl');
    gr4d_backendtexture_destroy                 := GetProcAddress(FLibHandle, 'gr4d_backendtexture_destroy');
    gr4d_backendtexture_get_backend_api         := GetProcAddress(FLibHandle, 'gr4d_backendtexture_get_backend_api');
    gr4d_backendtexture_get_gl_framebuffer_info := GetProcAddress(FLibHandle, 'gr4d_backendtexture_get_gl_framebuffer_info');
    gr4d_backendtexture_get_height              := GetProcAddress(FLibHandle, 'gr4d_backendtexture_get_height');
    gr4d_backendtexture_get_width               := GetProcAddress(FLibHandle, 'gr4d_backendtexture_get_width');
    gr4d_backendtexture_has_mipmaps             := GetProcAddress(FLibHandle, 'gr4d_backendtexture_has_mipmaps');
    gr4d_backendtexture_is_valid                := GetProcAddress(FLibHandle, 'gr4d_backendtexture_is_valid');
    {$ELSE}
    class function  TSkiaAPI.gr4d_backendrendertarget_create_gl;          external TSkiaAPI.LibName name 'gr4d_backendrendertarget_create_gl';
    class function  TSkiaAPI.gr4d_backendrendertarget_create_mtl;         external TSkiaAPI.LibName name 'gr4d_backendrendertarget_create_mtl';
    class procedure TSkiaAPI.gr4d_backendrendertarget_destroy;            external TSkiaAPI.LibName name 'gr4d_backendrendertarget_destroy';
    class function  TSkiaAPI.gr4d_backendrendertarget_get_backend_api;    external TSkiaAPI.LibName name 'gr4d_backendrendertarget_get_backend_api';
    class function  TSkiaAPI.gr4d_backendrendertarget_get_height;         external TSkiaAPI.LibName name 'gr4d_backendrendertarget_get_height';
    class function  TSkiaAPI.gr4d_backendrendertarget_get_sample_count;   external TSkiaAPI.LibName name 'gr4d_backendrendertarget_get_sample_count';
    class function  TSkiaAPI.gr4d_backendrendertarget_get_stencil_bits;   external TSkiaAPI.LibName name 'gr4d_backendrendertarget_get_stencil_bits';
    class function  TSkiaAPI.gr4d_backendrendertarget_get_width;          external TSkiaAPI.LibName name 'gr4d_backendrendertarget_get_width';
    class function  TSkiaAPI.gr4d_backendrendertarget_is_valid;           external TSkiaAPI.LibName name 'gr4d_backendrendertarget_is_valid';
    class function  TSkiaAPI.gr4d_backendtexture_create_gl;               external TSkiaAPI.LibName name 'gr4d_backendtexture_create_gl';
    class function  TSkiaAPI.gr4d_backendtexture_create_mtl;              external TSkiaAPI.LibName name 'gr4d_backendtexture_create_mtl';
    class procedure TSkiaAPI.gr4d_backendtexture_destroy;                 external TSkiaAPI.LibName name 'gr4d_backendtexture_destroy';
    class function  TSkiaAPI.gr4d_backendtexture_get_backend_api;         external TSkiaAPI.LibName name 'gr4d_backendtexture_get_backend_api';
    class function  TSkiaAPI.gr4d_backendtexture_get_gl_framebuffer_info; external TSkiaAPI.LibName name 'gr4d_backendtexture_get_gl_framebuffer_info';
    class function  TSkiaAPI.gr4d_backendtexture_get_height;              external TSkiaAPI.LibName name 'gr4d_backendtexture_get_height';
    class function  TSkiaAPI.gr4d_backendtexture_get_width;               external TSkiaAPI.LibName name 'gr4d_backendtexture_get_width';
    class function  TSkiaAPI.gr4d_backendtexture_has_mipmaps;             external TSkiaAPI.LibName name 'gr4d_backendtexture_has_mipmaps';
    class function  TSkiaAPI.gr4d_backendtexture_is_valid;                external TSkiaAPI.LibName name 'gr4d_backendtexture_is_valid';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/gr4d_directcontext.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    gr4d_directcontext_abandon_context                             := GetProcAddress(FLibHandle, 'gr4d_directcontext_abandon_context');
    gr4d_directcontext_dump_memory_statistics                      := GetProcAddress(FLibHandle, 'gr4d_directcontext_dump_memory_statistics');
    gr4d_directcontext_flush                                       := GetProcAddress(FLibHandle, 'gr4d_directcontext_flush');
    gr4d_directcontext_flush_and_submit                            := GetProcAddress(FLibHandle, 'gr4d_directcontext_flush_and_submit');
    gr4d_directcontext_free_gpu_resources                          := GetProcAddress(FLibHandle, 'gr4d_directcontext_free_gpu_resources');
    gr4d_directcontext_get_backend_api                             := GetProcAddress(FLibHandle, 'gr4d_directcontext_get_backend_api');
    gr4d_directcontext_get_max_surface_sample_count_for_color_type := GetProcAddress(FLibHandle, 'gr4d_directcontext_get_max_surface_sample_count_for_color_type');
    gr4d_directcontext_get_resource_cache_limit                    := GetProcAddress(FLibHandle, 'gr4d_directcontext_get_resource_cache_limit');
    gr4d_directcontext_get_resource_cache_usage                    := GetProcAddress(FLibHandle, 'gr4d_directcontext_get_resource_cache_usage');
    gr4d_directcontext_is_abandoned                                := GetProcAddress(FLibHandle, 'gr4d_directcontext_is_abandoned');
    gr4d_directcontext_make_gl                                     := GetProcAddress(FLibHandle, 'gr4d_directcontext_make_gl');
    gr4d_directcontext_make_metal                                  := GetProcAddress(FLibHandle, 'gr4d_directcontext_make_metal');
    gr4d_directcontext_perform_deferred_cleanup                    := GetProcAddress(FLibHandle, 'gr4d_directcontext_perform_deferred_cleanup');
    gr4d_directcontext_purge_unlocked_resources                    := GetProcAddress(FLibHandle, 'gr4d_directcontext_purge_unlocked_resources');
    gr4d_directcontext_purge_unlocked_resources2                   := GetProcAddress(FLibHandle, 'gr4d_directcontext_purge_unlocked_resources2');
    gr4d_directcontext_release_resources_and_abandon_context       := GetProcAddress(FLibHandle, 'gr4d_directcontext_release_resources_and_abandon_context');
    gr4d_directcontext_reset_context                               := GetProcAddress(FLibHandle, 'gr4d_directcontext_reset_context');
    gr4d_directcontext_set_resource_cache_limit                    := GetProcAddress(FLibHandle, 'gr4d_directcontext_set_resource_cache_limit');
    gr4d_directcontext_submit                                      := GetProcAddress(FLibHandle, 'gr4d_directcontext_submit');
    {$ELSE}
    class procedure TSkiaAPI.gr4d_directcontext_abandon_context;                             external TSkiaAPI.LibName name 'gr4d_directcontext_abandon_context';
    class procedure TSkiaAPI.gr4d_directcontext_dump_memory_statistics;                      external TSkiaAPI.LibName name 'gr4d_directcontext_dump_memory_statistics';
    class procedure TSkiaAPI.gr4d_directcontext_flush;                                       external TSkiaAPI.LibName name 'gr4d_directcontext_flush';
    class procedure TSkiaAPI.gr4d_directcontext_flush_and_submit;                            external TSkiaAPI.LibName name 'gr4d_directcontext_flush_and_submit';
    class procedure TSkiaAPI.gr4d_directcontext_free_gpu_resources;                          external TSkiaAPI.LibName name 'gr4d_directcontext_free_gpu_resources';
    class function  TSkiaAPI.gr4d_directcontext_get_backend_api;                             external TSkiaAPI.LibName name 'gr4d_directcontext_get_backend_api';
    class function  TSkiaAPI.gr4d_directcontext_get_max_surface_sample_count_for_color_type; external TSkiaAPI.LibName name 'gr4d_directcontext_get_max_surface_sample_count_for_color_type';
    class function  TSkiaAPI.gr4d_directcontext_get_resource_cache_limit;                    external TSkiaAPI.LibName name 'gr4d_directcontext_get_resource_cache_limit';
    class procedure TSkiaAPI.gr4d_directcontext_get_resource_cache_usage;                    external TSkiaAPI.LibName name 'gr4d_directcontext_get_resource_cache_usage';
    class function  TSkiaAPI.gr4d_directcontext_is_abandoned;                                external TSkiaAPI.LibName name 'gr4d_directcontext_is_abandoned';
    class function  TSkiaAPI.gr4d_directcontext_make_gl;                                     external TSkiaAPI.LibName name 'gr4d_directcontext_make_gl';
    class function  TSkiaAPI.gr4d_directcontext_make_metal;                                  external TSkiaAPI.LibName name 'gr4d_directcontext_make_metal';
    class procedure TSkiaAPI.gr4d_directcontext_perform_deferred_cleanup;                    external TSkiaAPI.LibName name 'gr4d_directcontext_perform_deferred_cleanup';
    class procedure TSkiaAPI.gr4d_directcontext_purge_unlocked_resources;                    external TSkiaAPI.LibName name 'gr4d_directcontext_purge_unlocked_resources';
    class procedure TSkiaAPI.gr4d_directcontext_purge_unlocked_resources2;                   external TSkiaAPI.LibName name 'gr4d_directcontext_purge_unlocked_resources2';
    class procedure TSkiaAPI.gr4d_directcontext_release_resources_and_abandon_context;       external TSkiaAPI.LibName name 'gr4d_directcontext_release_resources_and_abandon_context';
    class procedure TSkiaAPI.gr4d_directcontext_reset_context;                               external TSkiaAPI.LibName name 'gr4d_directcontext_reset_context';
    class procedure TSkiaAPI.gr4d_directcontext_set_resource_cache_limit;                    external TSkiaAPI.LibName name 'gr4d_directcontext_set_resource_cache_limit';
    class function  TSkiaAPI.gr4d_directcontext_submit;                                      external TSkiaAPI.LibName name 'gr4d_directcontext_submit';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/gr4d_gl_interface.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    gr4d_gl_interface_has_extension        := GetProcAddress(FLibHandle, 'gr4d_gl_interface_has_extension');
    gr4d_gl_interface_make_assembled       := GetProcAddress(FLibHandle, 'gr4d_gl_interface_make_assembled');
    gr4d_gl_interface_make_assembled_gl    := GetProcAddress(FLibHandle, 'gr4d_gl_interface_make_assembled_gl');
    gr4d_gl_interface_make_assembled_gles  := GetProcAddress(FLibHandle, 'gr4d_gl_interface_make_assembled_gles');
    gr4d_gl_interface_make_assembled_webgl := GetProcAddress(FLibHandle, 'gr4d_gl_interface_make_assembled_webgl');
    gr4d_gl_interface_make_native          := GetProcAddress(FLibHandle, 'gr4d_gl_interface_make_native');
    gr4d_gl_interface_validate             := GetProcAddress(FLibHandle, 'gr4d_gl_interface_validate');
    {$ELSE}
    class function TSkiaAPI.gr4d_gl_interface_has_extension;        external TSkiaAPI.LibName name 'gr4d_gl_interface_has_extension';
    class function TSkiaAPI.gr4d_gl_interface_make_assembled;       external TSkiaAPI.LibName name 'gr4d_gl_interface_make_assembled';
    class function TSkiaAPI.gr4d_gl_interface_make_assembled_gl;    external TSkiaAPI.LibName name 'gr4d_gl_interface_make_assembled_gl';
    class function TSkiaAPI.gr4d_gl_interface_make_assembled_gles;  external TSkiaAPI.LibName name 'gr4d_gl_interface_make_assembled_gles';
    class function TSkiaAPI.gr4d_gl_interface_make_assembled_webgl; external TSkiaAPI.LibName name 'gr4d_gl_interface_make_assembled_webgl';
    class function TSkiaAPI.gr4d_gl_interface_make_native;          external TSkiaAPI.LibName name 'gr4d_gl_interface_make_native';
    class function TSkiaAPI.gr4d_gl_interface_validate;             external TSkiaAPI.LibName name 'gr4d_gl_interface_validate';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_blender.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_blender_make_arithmetic := GetProcAddress(FLibHandle, 'sk4d_blender_make_arithmetic');
    sk4d_blender_make_mode       := GetProcAddress(FLibHandle, 'sk4d_blender_make_mode');
    {$ELSE}
    class function  TSkiaAPI.sk4d_blender_make_arithmetic; external TSkiaAPI.LibName name 'sk4d_blender_make_arithmetic';
    class function  TSkiaAPI.sk4d_blender_make_mode;       external TSkiaAPI.LibName name 'sk4d_blender_make_mode';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_canvas.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_canvas_clear                      := GetProcAddress(FLibHandle, 'sk4d_canvas_clear');
    sk4d_canvas_clear2                     := GetProcAddress(FLibHandle, 'sk4d_canvas_clear2');
    sk4d_canvas_destroy                    := GetProcAddress(FLibHandle, 'sk4d_canvas_destroy');
    sk4d_canvas_discard                    := GetProcAddress(FLibHandle, 'sk4d_canvas_discard');
    sk4d_canvas_clip_path                  := GetProcAddress(FLibHandle, 'sk4d_canvas_clip_path');
    sk4d_canvas_clip_rect                  := GetProcAddress(FLibHandle, 'sk4d_canvas_clip_rect');
    sk4d_canvas_clip_region                := GetProcAddress(FLibHandle, 'sk4d_canvas_clip_region');
    sk4d_canvas_clip_rrect                 := GetProcAddress(FLibHandle, 'sk4d_canvas_clip_rrect');
    sk4d_canvas_clip_shader                := GetProcAddress(FLibHandle, 'sk4d_canvas_clip_shader');
    sk4d_canvas_concat                     := GetProcAddress(FLibHandle, 'sk4d_canvas_concat');
    sk4d_canvas_concat2                    := GetProcAddress(FLibHandle, 'sk4d_canvas_concat2');
    sk4d_canvas_draw_annotation            := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_annotation');
    sk4d_canvas_draw_arc                   := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_arc');
    sk4d_canvas_draw_atlas                 := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_atlas');
    sk4d_canvas_draw_circle                := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_circle');
    sk4d_canvas_draw_color                 := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_color');
    sk4d_canvas_draw_color2                := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_color2');
    sk4d_canvas_draw_glyphs                := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_glyphs');
    sk4d_canvas_draw_glyphs2               := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_glyphs2');
    sk4d_canvas_draw_image                 := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_image');
    sk4d_canvas_draw_image_lattice         := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_image_lattice');
    sk4d_canvas_draw_image_nine            := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_image_nine');
    sk4d_canvas_draw_image_rect            := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_image_rect');
    sk4d_canvas_draw_line                  := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_line');
    sk4d_canvas_draw_oval                  := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_oval');
    sk4d_canvas_draw_paint                 := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_paint');
    sk4d_canvas_draw_patch                 := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_patch');
    sk4d_canvas_draw_path                  := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_path');
    sk4d_canvas_draw_picture               := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_picture');
    sk4d_canvas_draw_point                 := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_point');
    sk4d_canvas_draw_points                := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_points');
    sk4d_canvas_draw_rect                  := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_rect');
    sk4d_canvas_draw_region                := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_region');
    sk4d_canvas_draw_rrect                 := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_rrect');
    sk4d_canvas_draw_rrect2                := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_rrect2');
    sk4d_canvas_draw_rrect_difference      := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_rrect_difference');
    sk4d_canvas_draw_simple_text           := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_simple_text');
    sk4d_canvas_draw_text_blob             := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_text_blob');
    sk4d_canvas_draw_vertices              := GetProcAddress(FLibHandle, 'sk4d_canvas_draw_vertices');
    sk4d_canvas_get_device_clip_bounds     := GetProcAddress(FLibHandle, 'sk4d_canvas_get_device_clip_bounds');
    sk4d_canvas_get_local_clip_bounds      := GetProcAddress(FLibHandle, 'sk4d_canvas_get_local_clip_bounds');
    sk4d_canvas_get_local_to_device        := GetProcAddress(FLibHandle, 'sk4d_canvas_get_local_to_device');
    sk4d_canvas_get_local_to_device_as_3x3 := GetProcAddress(FLibHandle, 'sk4d_canvas_get_local_to_device_as_3x3');
    sk4d_canvas_get_save_count             := GetProcAddress(FLibHandle, 'sk4d_canvas_get_save_count');
    sk4d_canvas_quick_reject               := GetProcAddress(FLibHandle, 'sk4d_canvas_quick_reject');
    sk4d_canvas_quick_reject2              := GetProcAddress(FLibHandle, 'sk4d_canvas_quick_reject2');
    sk4d_canvas_reset_matrix               := GetProcAddress(FLibHandle, 'sk4d_canvas_reset_matrix');
    sk4d_canvas_restore                    := GetProcAddress(FLibHandle, 'sk4d_canvas_restore');
    sk4d_canvas_restore_to_count           := GetProcAddress(FLibHandle, 'sk4d_canvas_restore_to_count');
    sk4d_canvas_rotate                     := GetProcAddress(FLibHandle, 'sk4d_canvas_rotate');
    sk4d_canvas_rotate2                    := GetProcAddress(FLibHandle, 'sk4d_canvas_rotate2');
    sk4d_canvas_save                       := GetProcAddress(FLibHandle, 'sk4d_canvas_save');
    sk4d_canvas_save_layer                 := GetProcAddress(FLibHandle, 'sk4d_canvas_save_layer');
    sk4d_canvas_save_layer_alpha           := GetProcAddress(FLibHandle, 'sk4d_canvas_save_layer_alpha');
    sk4d_canvas_scale                      := GetProcAddress(FLibHandle, 'sk4d_canvas_scale');
    sk4d_canvas_set_matrix                 := GetProcAddress(FLibHandle, 'sk4d_canvas_set_matrix');
    sk4d_canvas_set_matrix2                := GetProcAddress(FLibHandle, 'sk4d_canvas_set_matrix2');
    sk4d_canvas_skew                       := GetProcAddress(FLibHandle, 'sk4d_canvas_skew');
    sk4d_canvas_translate                  := GetProcAddress(FLibHandle, 'sk4d_canvas_translate');
    {$ELSE}
    class procedure TSkiaAPI.sk4d_canvas_clear;                      external TSkiaAPI.LibName name 'sk4d_canvas_clear';
    class procedure TSkiaAPI.sk4d_canvas_clear2;                     external TSkiaAPI.LibName name 'sk4d_canvas_clear2';
    class procedure TSkiaAPI.sk4d_canvas_destroy;                    external TSkiaAPI.LibName name 'sk4d_canvas_destroy';
    class procedure TSkiaAPI.sk4d_canvas_discard;                    external TSkiaAPI.LibName name 'sk4d_canvas_discard';
    class procedure TSkiaAPI.sk4d_canvas_clip_path;                  external TSkiaAPI.LibName name 'sk4d_canvas_clip_path';
    class procedure TSkiaAPI.sk4d_canvas_clip_rect;                  external TSkiaAPI.LibName name 'sk4d_canvas_clip_rect';
    class procedure TSkiaAPI.sk4d_canvas_clip_region;                external TSkiaAPI.LibName name 'sk4d_canvas_clip_region';
    class procedure TSkiaAPI.sk4d_canvas_clip_rrect;                 external TSkiaAPI.LibName name 'sk4d_canvas_clip_rrect';
    class procedure TSkiaAPI.sk4d_canvas_clip_shader;                external TSkiaAPI.LibName name 'sk4d_canvas_clip_shader';
    class procedure TSkiaAPI.sk4d_canvas_concat;                     external TSkiaAPI.LibName name 'sk4d_canvas_concat';
    class procedure TSkiaAPI.sk4d_canvas_concat2;                    external TSkiaAPI.LibName name 'sk4d_canvas_concat2';
    class procedure TSkiaAPI.sk4d_canvas_draw_annotation;            external TSkiaAPI.LibName name 'sk4d_canvas_draw_annotation';
    class procedure TSkiaAPI.sk4d_canvas_draw_arc;                   external TSkiaAPI.LibName name 'sk4d_canvas_draw_arc';
    class procedure TSkiaAPI.sk4d_canvas_draw_atlas;                 external TSkiaAPI.LibName name 'sk4d_canvas_draw_atlas';
    class procedure TSkiaAPI.sk4d_canvas_draw_circle;                external TSkiaAPI.LibName name 'sk4d_canvas_draw_circle';
    class procedure TSkiaAPI.sk4d_canvas_draw_color;                 external TSkiaAPI.LibName name 'sk4d_canvas_draw_color';
    class procedure TSkiaAPI.sk4d_canvas_draw_color2;                external TSkiaAPI.LibName name 'sk4d_canvas_draw_color2';
    class procedure TSkiaAPI.sk4d_canvas_draw_glyphs;                external TSkiaAPI.LibName name 'sk4d_canvas_draw_glyphs';
    class procedure TSkiaAPI.sk4d_canvas_draw_glyphs2;               external TSkiaAPI.LibName name 'sk4d_canvas_draw_glyphs2';
    class procedure TSkiaAPI.sk4d_canvas_draw_image;                 external TSkiaAPI.LibName name 'sk4d_canvas_draw_image';
    class procedure TSkiaAPI.sk4d_canvas_draw_image_lattice;         external TSkiaAPI.LibName name 'sk4d_canvas_draw_image_lattice';
    class procedure TSkiaAPI.sk4d_canvas_draw_image_nine;            external TSkiaAPI.LibName name 'sk4d_canvas_draw_image_nine';
    class procedure TSkiaAPI.sk4d_canvas_draw_image_rect;            external TSkiaAPI.LibName name 'sk4d_canvas_draw_image_rect';
    class procedure TSkiaAPI.sk4d_canvas_draw_line;                  external TSkiaAPI.LibName name 'sk4d_canvas_draw_line';
    class procedure TSkiaAPI.sk4d_canvas_draw_oval;                  external TSkiaAPI.LibName name 'sk4d_canvas_draw_oval';
    class procedure TSkiaAPI.sk4d_canvas_draw_paint;                 external TSkiaAPI.LibName name 'sk4d_canvas_draw_paint';
    class procedure TSkiaAPI.sk4d_canvas_draw_patch;                 external TSkiaAPI.LibName name 'sk4d_canvas_draw_patch';
    class procedure TSkiaAPI.sk4d_canvas_draw_path;                  external TSkiaAPI.LibName name 'sk4d_canvas_draw_path';
    class procedure TSkiaAPI.sk4d_canvas_draw_picture;               external TSkiaAPI.LibName name 'sk4d_canvas_draw_picture';
    class procedure TSkiaAPI.sk4d_canvas_draw_point;                 external TSkiaAPI.LibName name 'sk4d_canvas_draw_point';
    class procedure TSkiaAPI.sk4d_canvas_draw_points;                external TSkiaAPI.LibName name 'sk4d_canvas_draw_points';
    class procedure TSkiaAPI.sk4d_canvas_draw_rect;                  external TSkiaAPI.LibName name 'sk4d_canvas_draw_rect';
    class procedure TSkiaAPI.sk4d_canvas_draw_region;                external TSkiaAPI.LibName name 'sk4d_canvas_draw_region';
    class procedure TSkiaAPI.sk4d_canvas_draw_rrect;                 external TSkiaAPI.LibName name 'sk4d_canvas_draw_rrect';
    class procedure TSkiaAPI.sk4d_canvas_draw_rrect2;                external TSkiaAPI.LibName name 'sk4d_canvas_draw_rrect2';
    class procedure TSkiaAPI.sk4d_canvas_draw_rrect_difference;      external TSkiaAPI.LibName name 'sk4d_canvas_draw_rrect_difference';
    class procedure TSkiaAPI.sk4d_canvas_draw_simple_text;           external TSkiaAPI.LibName name 'sk4d_canvas_draw_simple_text';
    class procedure TSkiaAPI.sk4d_canvas_draw_text_blob;             external TSkiaAPI.LibName name 'sk4d_canvas_draw_text_blob';
    class procedure TSkiaAPI.sk4d_canvas_draw_vertices;              external TSkiaAPI.LibName name 'sk4d_canvas_draw_vertices';
    class procedure TSkiaAPI.sk4d_canvas_get_device_clip_bounds;     external TSkiaAPI.LibName name 'sk4d_canvas_get_device_clip_bounds';
    class procedure TSkiaAPI.sk4d_canvas_get_local_clip_bounds;      external TSkiaAPI.LibName name 'sk4d_canvas_get_local_clip_bounds';
    class procedure TSkiaAPI.sk4d_canvas_get_local_to_device;        external TSkiaAPI.LibName name 'sk4d_canvas_get_local_to_device';
    class procedure TSkiaAPI.sk4d_canvas_get_local_to_device_as_3x3; external TSkiaAPI.LibName name 'sk4d_canvas_get_local_to_device_as_3x3';
    class function  TSkiaAPI.sk4d_canvas_get_save_count;             external TSkiaAPI.LibName name 'sk4d_canvas_get_save_count';
    class function  TSkiaAPI.sk4d_canvas_quick_reject;               external TSkiaAPI.LibName name 'sk4d_canvas_quick_reject';
    class function  TSkiaAPI.sk4d_canvas_quick_reject2;              external TSkiaAPI.LibName name 'sk4d_canvas_quick_reject2';
    class procedure TSkiaAPI.sk4d_canvas_reset_matrix;               external TSkiaAPI.LibName name 'sk4d_canvas_reset_matrix';
    class procedure TSkiaAPI.sk4d_canvas_restore;                    external TSkiaAPI.LibName name 'sk4d_canvas_restore';
    class procedure TSkiaAPI.sk4d_canvas_restore_to_count;           external TSkiaAPI.LibName name 'sk4d_canvas_restore_to_count';
    class procedure TSkiaAPI.sk4d_canvas_rotate;                     external TSkiaAPI.LibName name 'sk4d_canvas_rotate';
    class procedure TSkiaAPI.sk4d_canvas_rotate2;                    external TSkiaAPI.LibName name 'sk4d_canvas_rotate2';
    class function  TSkiaAPI.sk4d_canvas_save;                       external TSkiaAPI.LibName name 'sk4d_canvas_save';
    class function  TSkiaAPI.sk4d_canvas_save_layer;                 external TSkiaAPI.LibName name 'sk4d_canvas_save_layer';
    class function  TSkiaAPI.sk4d_canvas_save_layer_alpha;           external TSkiaAPI.LibName name 'sk4d_canvas_save_layer_alpha';
    class procedure TSkiaAPI.sk4d_canvas_scale;                      external TSkiaAPI.LibName name 'sk4d_canvas_scale';
    class procedure TSkiaAPI.sk4d_canvas_set_matrix;                 external TSkiaAPI.LibName name 'sk4d_canvas_set_matrix';
    class procedure TSkiaAPI.sk4d_canvas_set_matrix2;                external TSkiaAPI.LibName name 'sk4d_canvas_set_matrix2';
    class procedure TSkiaAPI.sk4d_canvas_skew;                       external TSkiaAPI.LibName name 'sk4d_canvas_skew';
    class procedure TSkiaAPI.sk4d_canvas_translate;                  external TSkiaAPI.LibName name 'sk4d_canvas_translate';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_codec.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_codec_destroy                    := GetProcAddress(FLibHandle, 'sk4d_codec_destroy');
    sk4d_codec_get_dimensions             := GetProcAddress(FLibHandle, 'sk4d_codec_get_dimensions');
    sk4d_codec_get_image                  := GetProcAddress(FLibHandle, 'sk4d_codec_get_image');
    sk4d_codec_get_pixels                 := GetProcAddress(FLibHandle, 'sk4d_codec_get_pixels');
    sk4d_codec_make_from_file             := GetProcAddress(FLibHandle, 'sk4d_codec_make_from_file');
    sk4d_codec_make_with_copy             := GetProcAddress(FLibHandle, 'sk4d_codec_make_with_copy');
    sk4d_codec_make_without_copy          := GetProcAddress(FLibHandle, 'sk4d_codec_make_without_copy');
    sk4d_animcodecplayer_destroy          := GetProcAddress(FLibHandle, 'sk4d_animcodecplayer_destroy');
    sk4d_animcodecplayer_get_dimensions   := GetProcAddress(FLibHandle, 'sk4d_animcodecplayer_get_dimensions');
    sk4d_animcodecplayer_get_duration     := GetProcAddress(FLibHandle, 'sk4d_animcodecplayer_get_duration');
    sk4d_animcodecplayer_get_frame        := GetProcAddress(FLibHandle, 'sk4d_animcodecplayer_get_frame');
    sk4d_animcodecplayer_make_from_file   := GetProcAddress(FLibHandle, 'sk4d_animcodecplayer_make_from_file');
    sk4d_animcodecplayer_make_from_stream := GetProcAddress(FLibHandle, 'sk4d_animcodecplayer_make_from_stream');
    sk4d_animcodecplayer_seek             := GetProcAddress(FLibHandle, 'sk4d_animcodecplayer_seek');
    {$ELSE}
    class procedure TSkiaAPI.sk4d_codec_destroy;                    external TSkiaAPI.LibName name 'sk4d_codec_destroy';
    class procedure TSkiaAPI.sk4d_codec_get_dimensions;             external TSkiaAPI.LibName name 'sk4d_codec_get_dimensions';
    class function  TSkiaAPI.sk4d_codec_get_image;                  external TSkiaAPI.LibName name 'sk4d_codec_get_image';
    class function  TSkiaAPI.sk4d_codec_get_pixels;                 external TSkiaAPI.LibName name 'sk4d_codec_get_pixels';
    class function  TSkiaAPI.sk4d_codec_make_from_file;             external TSkiaAPI.LibName name 'sk4d_codec_make_from_file';
    class function  TSkiaAPI.sk4d_codec_make_with_copy;             external TSkiaAPI.LibName name 'sk4d_codec_make_with_copy';
    class function  TSkiaAPI.sk4d_codec_make_without_copy;          external TSkiaAPI.LibName name 'sk4d_codec_make_without_copy';
    class procedure TSkiaAPI.sk4d_animcodecplayer_destroy;          external TSkiaAPI.LibName name 'sk4d_animcodecplayer_destroy';
    class procedure TSkiaAPI.sk4d_animcodecplayer_get_dimensions;   external TSkiaAPI.LibName name 'sk4d_animcodecplayer_get_dimensions';
    class function  TSkiaAPI.sk4d_animcodecplayer_get_duration;     external TSkiaAPI.LibName name 'sk4d_animcodecplayer_get_duration';
    class function  TSkiaAPI.sk4d_animcodecplayer_get_frame;        external TSkiaAPI.LibName name 'sk4d_animcodecplayer_get_frame';
    class function  TSkiaAPI.sk4d_animcodecplayer_make_from_file;   external TSkiaAPI.LibName name 'sk4d_animcodecplayer_make_from_file';
    class function  TSkiaAPI.sk4d_animcodecplayer_make_from_stream; external TSkiaAPI.LibName name 'sk4d_animcodecplayer_make_from_stream';
    class function  TSkiaAPI.sk4d_animcodecplayer_seek;             external TSkiaAPI.LibName name 'sk4d_animcodecplayer_seek';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_colorfilter.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_colorfilter_make_blend                := GetProcAddress(FLibHandle, 'sk4d_colorfilter_make_blend');
    sk4d_colorfilter_make_compose              := GetProcAddress(FLibHandle, 'sk4d_colorfilter_make_compose');
    sk4d_colorfilter_make_high_contrast        := GetProcAddress(FLibHandle, 'sk4d_colorfilter_make_high_contrast');
    sk4d_colorfilter_make_hsla_matrix          := GetProcAddress(FLibHandle, 'sk4d_colorfilter_make_hsla_matrix');
    sk4d_colorfilter_make_lighting             := GetProcAddress(FLibHandle, 'sk4d_colorfilter_make_lighting');
    sk4d_colorfilter_make_linear_to_srgb_gamma := GetProcAddress(FLibHandle, 'sk4d_colorfilter_make_linear_to_srgb_gamma');
    sk4d_colorfilter_make_luma_color           := GetProcAddress(FLibHandle, 'sk4d_colorfilter_make_luma_color');
    sk4d_colorfilter_make_matrix               := GetProcAddress(FLibHandle, 'sk4d_colorfilter_make_matrix');
    sk4d_colorfilter_make_overdraw             := GetProcAddress(FLibHandle, 'sk4d_colorfilter_make_overdraw');
    sk4d_colorfilter_make_table                := GetProcAddress(FLibHandle, 'sk4d_colorfilter_make_table');
    {$ELSE}
    class function TSkiaAPI.sk4d_colorfilter_make_blend;                external TSkiaAPI.LibName name 'sk4d_colorfilter_make_blend';
    class function TSkiaAPI.sk4d_colorfilter_make_compose;              external TSkiaAPI.LibName name 'sk4d_colorfilter_make_compose';
    class function TSkiaAPI.sk4d_colorfilter_make_high_contrast;        external TSkiaAPI.LibName name 'sk4d_colorfilter_make_high_contrast';
    class function TSkiaAPI.sk4d_colorfilter_make_hsla_matrix;          external TSkiaAPI.LibName name 'sk4d_colorfilter_make_hsla_matrix';
    class function TSkiaAPI.sk4d_colorfilter_make_lighting;             external TSkiaAPI.LibName name 'sk4d_colorfilter_make_lighting';
    class function TSkiaAPI.sk4d_colorfilter_make_linear_to_srgb_gamma; external TSkiaAPI.LibName name 'sk4d_colorfilter_make_linear_to_srgb_gamma';
    class function TSkiaAPI.sk4d_colorfilter_make_luma_color;           external TSkiaAPI.LibName name 'sk4d_colorfilter_make_luma_color';
    class function TSkiaAPI.sk4d_colorfilter_make_matrix;               external TSkiaAPI.LibName name 'sk4d_colorfilter_make_matrix';
    class function TSkiaAPI.sk4d_colorfilter_make_overdraw;             external TSkiaAPI.LibName name 'sk4d_colorfilter_make_overdraw';
    class function TSkiaAPI.sk4d_colorfilter_make_table;                external TSkiaAPI.LibName name 'sk4d_colorfilter_make_table';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_colorspace.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_colorspace_gamma_close_to_srgb       := GetProcAddress(FLibHandle, 'sk4d_colorspace_gamma_close_to_srgb');
    sk4d_colorspace_gamma_is_linear           := GetProcAddress(FLibHandle, 'sk4d_colorspace_gamma_is_linear');
    sk4d_colorspace_is_equal                  := GetProcAddress(FLibHandle, 'sk4d_colorspace_is_equal');
    sk4d_colorspace_is_numerical_transfer_fn  := GetProcAddress(FLibHandle, 'sk4d_colorspace_is_numerical_transfer_fn');
    sk4d_colorspace_is_srgb                   := GetProcAddress(FLibHandle, 'sk4d_colorspace_is_srgb');
    sk4d_colorspace_make                      := GetProcAddress(FLibHandle, 'sk4d_colorspace_make');
    sk4d_colorspace_make_linear_gamma         := GetProcAddress(FLibHandle, 'sk4d_colorspace_make_linear_gamma');
    sk4d_colorspace_make_rgb                  := GetProcAddress(FLibHandle, 'sk4d_colorspace_make_rgb');
    sk4d_colorspace_make_srgb                 := GetProcAddress(FLibHandle, 'sk4d_colorspace_make_srgb');
    sk4d_colorspace_make_srgb_gamma           := GetProcAddress(FLibHandle, 'sk4d_colorspace_make_srgb_gamma');
    sk4d_colorspace_make_srgb_linear          := GetProcAddress(FLibHandle, 'sk4d_colorspace_make_srgb_linear');
    sk4d_colorspace_ref                       := GetProcAddress(FLibHandle, 'sk4d_colorspace_ref');
    sk4d_colorspace_to_profile                := GetProcAddress(FLibHandle, 'sk4d_colorspace_to_profile');
    sk4d_colorspace_to_xyz                    := GetProcAddress(FLibHandle, 'sk4d_colorspace_to_xyz');
    sk4d_colorspace_unref                     := GetProcAddress(FLibHandle, 'sk4d_colorspace_unref');
    sk4d_colorspaceiccprofile_destroy         := GetProcAddress(FLibHandle, 'sk4d_colorspaceiccprofile_destroy');
    sk4d_colorspaceiccprofile_get_buffer      := GetProcAddress(FLibHandle, 'sk4d_colorspaceiccprofile_get_buffer');
    sk4d_colorspaceiccprofile_make_with_parse := GetProcAddress(FLibHandle, 'sk4d_colorspaceiccprofile_make_with_parse');
    sk4d_colorspaceiccprofile_to_xyz          := GetProcAddress(FLibHandle, 'sk4d_colorspaceiccprofile_to_xyz');
    sk4d_colorspaceprimaries_to_xyz           := GetProcAddress(FLibHandle, 'sk4d_colorspaceprimaries_to_xyz');
    sk4d_colorspacetransferfn_invert          := GetProcAddress(FLibHandle, 'sk4d_colorspacetransferfn_invert');
    sk4d_colorspacetransferfn_transform       := GetProcAddress(FLibHandle, 'sk4d_colorspacetransferfn_transform');
    {$ELSE}
    class function  TSkiaAPI.sk4d_colorspace_gamma_close_to_srgb;       external TSkiaAPI.LibName name 'sk4d_colorspace_gamma_close_to_srgb';
    class function  TSkiaAPI.sk4d_colorspace_gamma_is_linear;           external TSkiaAPI.LibName name 'sk4d_colorspace_gamma_is_linear';
    class function  TSkiaAPI.sk4d_colorspace_is_equal;                  external TSkiaAPI.LibName name 'sk4d_colorspace_is_equal';
    class function  TSkiaAPI.sk4d_colorspace_is_numerical_transfer_fn;  external TSkiaAPI.LibName name 'sk4d_colorspace_is_numerical_transfer_fn';
    class function  TSkiaAPI.sk4d_colorspace_is_srgb;                   external TSkiaAPI.LibName name 'sk4d_colorspace_is_srgb';
    class function  TSkiaAPI.sk4d_colorspace_make;                      external TSkiaAPI.LibName name 'sk4d_colorspace_make';
    class function  TSkiaAPI.sk4d_colorspace_make_linear_gamma;         external TSkiaAPI.LibName name 'sk4d_colorspace_make_linear_gamma';
    class function  TSkiaAPI.sk4d_colorspace_make_rgb;                  external TSkiaAPI.LibName name 'sk4d_colorspace_make_rgb';
    class function  TSkiaAPI.sk4d_colorspace_make_srgb;                 external TSkiaAPI.LibName name 'sk4d_colorspace_make_srgb';
    class function  TSkiaAPI.sk4d_colorspace_make_srgb_gamma;           external TSkiaAPI.LibName name 'sk4d_colorspace_make_srgb_gamma';
    class function  TSkiaAPI.sk4d_colorspace_make_srgb_linear;          external TSkiaAPI.LibName name 'sk4d_colorspace_make_srgb_linear';
    class procedure TSkiaAPI.sk4d_colorspace_ref;                       external TSkiaAPI.LibName name 'sk4d_colorspace_ref';
    class function  TSkiaAPI.sk4d_colorspace_to_profile;                external TSkiaAPI.LibName name 'sk4d_colorspace_to_profile';
    class function  TSkiaAPI.sk4d_colorspace_to_xyz;                    external TSkiaAPI.LibName name 'sk4d_colorspace_to_xyz';
    class procedure TSkiaAPI.sk4d_colorspace_unref;                     external TSkiaAPI.LibName name 'sk4d_colorspace_unref';
    class procedure TSkiaAPI.sk4d_colorspaceiccprofile_destroy;         external TSkiaAPI.LibName name 'sk4d_colorspaceiccprofile_destroy';
    class function  TSkiaAPI.sk4d_colorspaceiccprofile_get_buffer;      external TSkiaAPI.LibName name 'sk4d_colorspaceiccprofile_get_buffer';
    class function  TSkiaAPI.sk4d_colorspaceiccprofile_make_with_parse; external TSkiaAPI.LibName name 'sk4d_colorspaceiccprofile_make_with_parse';
    class function  TSkiaAPI.sk4d_colorspaceiccprofile_to_xyz;          external TSkiaAPI.LibName name 'sk4d_colorspaceiccprofile_to_xyz';
    class function  TSkiaAPI.sk4d_colorspaceprimaries_to_xyz;           external TSkiaAPI.LibName name 'sk4d_colorspaceprimaries_to_xyz';
    class function  TSkiaAPI.sk4d_colorspacetransferfn_invert;          external TSkiaAPI.LibName name 'sk4d_colorspacetransferfn_invert';
    class function  TSkiaAPI.sk4d_colorspacetransferfn_transform;       external TSkiaAPI.LibName name 'sk4d_colorspacetransferfn_transform';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_data.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_data_make_empty     := GetProcAddress(FLibHandle, 'sk4d_data_make_empty');
    sk4d_data_make_with_copy := GetProcAddress(FLibHandle, 'sk4d_data_make_with_copy');
    {$ELSE}
    class function TSkiaAPI.sk4d_data_make_empty;     external TSkiaAPI.LibName name 'sk4d_data_make_empty';
    class function TSkiaAPI.sk4d_data_make_with_copy; external TSkiaAPI.LibName name 'sk4d_data_make_with_copy';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_document.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_document_begin_page := GetProcAddress(FLibHandle, 'sk4d_document_begin_page');
    sk4d_document_close      := GetProcAddress(FLibHandle, 'sk4d_document_close');
    sk4d_document_end_page   := GetProcAddress(FLibHandle, 'sk4d_document_end_page');
    sk4d_document_make_pdf   := GetProcAddress(FLibHandle, 'sk4d_document_make_pdf');
    sk4d_document_make_pdf2  := GetProcAddress(FLibHandle, 'sk4d_document_make_pdf2');
    sk4d_document_make_xps   := GetProcAddress(FLibHandle, 'sk4d_document_make_xps');
    sk4d_document_terminate  := GetProcAddress(FLibHandle, 'sk4d_document_terminate');
    {$ELSE}
    class function  TSkiaAPI.sk4d_document_begin_page; external TSkiaAPI.LibName name 'sk4d_document_begin_page';
    class procedure TSkiaAPI.sk4d_document_close;      external TSkiaAPI.LibName name 'sk4d_document_close';
    class procedure TSkiaAPI.sk4d_document_end_page;   external TSkiaAPI.LibName name 'sk4d_document_end_page';
    class function  TSkiaAPI.sk4d_document_make_pdf;   external TSkiaAPI.LibName name 'sk4d_document_make_pdf';
    class function  TSkiaAPI.sk4d_document_make_pdf2;  external TSkiaAPI.LibName name 'sk4d_document_make_pdf2';
    class function  TSkiaAPI.sk4d_document_make_xps;   external TSkiaAPI.LibName name 'sk4d_document_make_xps';
    class procedure TSkiaAPI.sk4d_document_terminate;  external TSkiaAPI.LibName name 'sk4d_document_terminate';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_font.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_font_create                   := GetProcAddress(FLibHandle, 'sk4d_font_create');
    sk4d_font_create2                  := GetProcAddress(FLibHandle, 'sk4d_font_create2');
    sk4d_font_destroy                  := GetProcAddress(FLibHandle, 'sk4d_font_destroy');
    sk4d_font_get_baseline_snap        := GetProcAddress(FLibHandle, 'sk4d_font_get_baseline_snap');
    sk4d_font_get_edging               := GetProcAddress(FLibHandle, 'sk4d_font_get_edging');
    sk4d_font_get_embedded_bitmaps     := GetProcAddress(FLibHandle, 'sk4d_font_get_embedded_bitmaps');
    sk4d_font_get_embolden             := GetProcAddress(FLibHandle, 'sk4d_font_get_embolden');
    sk4d_font_get_force_auto_hinting   := GetProcAddress(FLibHandle, 'sk4d_font_get_force_auto_hinting');
    sk4d_font_get_glyphs               := GetProcAddress(FLibHandle, 'sk4d_font_get_glyphs');
    sk4d_font_get_glyphs_count         := GetProcAddress(FLibHandle, 'sk4d_font_get_glyphs_count');
    sk4d_font_get_hinting              := GetProcAddress(FLibHandle, 'sk4d_font_get_hinting');
    sk4d_font_get_horizontal_positions := GetProcAddress(FLibHandle, 'sk4d_font_get_horizontal_positions');
    sk4d_font_get_intercepts           := GetProcAddress(FLibHandle, 'sk4d_font_get_intercepts');
    sk4d_font_get_linear_metrics       := GetProcAddress(FLibHandle, 'sk4d_font_get_linear_metrics');
    sk4d_font_get_metrics              := GetProcAddress(FLibHandle, 'sk4d_font_get_metrics');
    sk4d_font_get_path                 := GetProcAddress(FLibHandle, 'sk4d_font_get_path');
    sk4d_font_get_paths                := GetProcAddress(FLibHandle, 'sk4d_font_get_paths');
    sk4d_font_get_positions            := GetProcAddress(FLibHandle, 'sk4d_font_get_positions');
    sk4d_font_get_scale_x              := GetProcAddress(FLibHandle, 'sk4d_font_get_scale_x');
    sk4d_font_get_size                 := GetProcAddress(FLibHandle, 'sk4d_font_get_size');
    sk4d_font_get_skew_x               := GetProcAddress(FLibHandle, 'sk4d_font_get_skew_x');
    sk4d_font_get_subpixel             := GetProcAddress(FLibHandle, 'sk4d_font_get_subpixel');
    sk4d_font_get_typeface             := GetProcAddress(FLibHandle, 'sk4d_font_get_typeface');
    sk4d_font_get_typeface_or_default  := GetProcAddress(FLibHandle, 'sk4d_font_get_typeface_or_default');
    sk4d_font_get_widths_bounds        := GetProcAddress(FLibHandle, 'sk4d_font_get_widths_bounds');
    sk4d_font_is_equal                 := GetProcAddress(FLibHandle, 'sk4d_font_is_equal');
    sk4d_font_measure_text             := GetProcAddress(FLibHandle, 'sk4d_font_measure_text');
    sk4d_font_set_baseline_snap        := GetProcAddress(FLibHandle, 'sk4d_font_set_baseline_snap');
    sk4d_font_set_edging               := GetProcAddress(FLibHandle, 'sk4d_font_set_edging');
    sk4d_font_set_embedded_bitmaps     := GetProcAddress(FLibHandle, 'sk4d_font_set_embedded_bitmaps');
    sk4d_font_set_embolden             := GetProcAddress(FLibHandle, 'sk4d_font_set_embolden');
    sk4d_font_set_force_auto_hinting   := GetProcAddress(FLibHandle, 'sk4d_font_set_force_auto_hinting');
    sk4d_font_set_hinting              := GetProcAddress(FLibHandle, 'sk4d_font_set_hinting');
    sk4d_font_set_linear_metrics       := GetProcAddress(FLibHandle, 'sk4d_font_set_linear_metrics');
    sk4d_font_set_scale_x              := GetProcAddress(FLibHandle, 'sk4d_font_set_scale_x');
    sk4d_font_set_size                 := GetProcAddress(FLibHandle, 'sk4d_font_set_size');
    sk4d_font_set_skew_x               := GetProcAddress(FLibHandle, 'sk4d_font_set_skew_x');
    sk4d_font_set_subpixel             := GetProcAddress(FLibHandle, 'sk4d_font_set_subpixel');
    sk4d_font_set_typeface             := GetProcAddress(FLibHandle, 'sk4d_font_set_typeface');
    sk4d_font_unichar_to_glyph         := GetProcAddress(FLibHandle, 'sk4d_font_unichar_to_glyph');
    sk4d_font_unichars_to_glyphs       := GetProcAddress(FLibHandle, 'sk4d_font_unichars_to_glyphs');
    {$ELSE}
    class function  TSkiaAPI.sk4d_font_create;                   external TSkiaAPI.LibName name 'sk4d_font_create';
    class function  TSkiaAPI.sk4d_font_create2;                  external TSkiaAPI.LibName name 'sk4d_font_create2';
    class procedure TSkiaAPI.sk4d_font_destroy;                  external TSkiaAPI.LibName name 'sk4d_font_destroy';
    class function  TSkiaAPI.sk4d_font_get_baseline_snap;        external TSkiaAPI.LibName name 'sk4d_font_get_baseline_snap';
    class function  TSkiaAPI.sk4d_font_get_edging;               external TSkiaAPI.LibName name 'sk4d_font_get_edging';
    class function  TSkiaAPI.sk4d_font_get_embedded_bitmaps;     external TSkiaAPI.LibName name 'sk4d_font_get_embedded_bitmaps';
    class function  TSkiaAPI.sk4d_font_get_embolden;             external TSkiaAPI.LibName name 'sk4d_font_get_embolden';
    class function  TSkiaAPI.sk4d_font_get_force_auto_hinting;   external TSkiaAPI.LibName name 'sk4d_font_get_force_auto_hinting';
    class function  TSkiaAPI.sk4d_font_get_glyphs;               external TSkiaAPI.LibName name 'sk4d_font_get_glyphs';
    class function  TSkiaAPI.sk4d_font_get_glyphs_count;         external TSkiaAPI.LibName name 'sk4d_font_get_glyphs_count';
    class function  TSkiaAPI.sk4d_font_get_hinting;              external TSkiaAPI.LibName name 'sk4d_font_get_hinting';
    class procedure TSkiaAPI.sk4d_font_get_horizontal_positions; external TSkiaAPI.LibName name 'sk4d_font_get_horizontal_positions';
    class function  TSkiaAPI.sk4d_font_get_intercepts;           external TSkiaAPI.LibName name 'sk4d_font_get_intercepts';
    class function  TSkiaAPI.sk4d_font_get_linear_metrics;       external TSkiaAPI.LibName name 'sk4d_font_get_linear_metrics';
    class function  TSkiaAPI.sk4d_font_get_metrics;              external TSkiaAPI.LibName name 'sk4d_font_get_metrics';
    class function  TSkiaAPI.sk4d_font_get_path;                 external TSkiaAPI.LibName name 'sk4d_font_get_path';
    class procedure TSkiaAPI.sk4d_font_get_paths;                external TSkiaAPI.LibName name 'sk4d_font_get_paths';
    class procedure TSkiaAPI.sk4d_font_get_positions;            external TSkiaAPI.LibName name 'sk4d_font_get_positions';
    class function  TSkiaAPI.sk4d_font_get_scale_x;              external TSkiaAPI.LibName name 'sk4d_font_get_scale_x';
    class function  TSkiaAPI.sk4d_font_get_size;                 external TSkiaAPI.LibName name 'sk4d_font_get_size';
    class function  TSkiaAPI.sk4d_font_get_skew_x;               external TSkiaAPI.LibName name 'sk4d_font_get_skew_x';
    class function  TSkiaAPI.sk4d_font_get_subpixel;             external TSkiaAPI.LibName name 'sk4d_font_get_subpixel';
    class function  TSkiaAPI.sk4d_font_get_typeface;             external TSkiaAPI.LibName name 'sk4d_font_get_typeface';
    class function  TSkiaAPI.sk4d_font_get_typeface_or_default;  external TSkiaAPI.LibName name 'sk4d_font_get_typeface_or_default';
    class procedure TSkiaAPI.sk4d_font_get_widths_bounds;        external TSkiaAPI.LibName name 'sk4d_font_get_widths_bounds';
    class function  TSkiaAPI.sk4d_font_is_equal;                 external TSkiaAPI.LibName name 'sk4d_font_is_equal';
    class function  TSkiaAPI.sk4d_font_measure_text;             external TSkiaAPI.LibName name 'sk4d_font_measure_text';
    class procedure TSkiaAPI.sk4d_font_set_baseline_snap;        external TSkiaAPI.LibName name 'sk4d_font_set_baseline_snap';
    class procedure TSkiaAPI.sk4d_font_set_edging;               external TSkiaAPI.LibName name 'sk4d_font_set_edging';
    class procedure TSkiaAPI.sk4d_font_set_embedded_bitmaps;     external TSkiaAPI.LibName name 'sk4d_font_set_embedded_bitmaps';
    class procedure TSkiaAPI.sk4d_font_set_embolden;             external TSkiaAPI.LibName name 'sk4d_font_set_embolden';
    class procedure TSkiaAPI.sk4d_font_set_force_auto_hinting;   external TSkiaAPI.LibName name 'sk4d_font_set_force_auto_hinting';
    class procedure TSkiaAPI.sk4d_font_set_hinting;              external TSkiaAPI.LibName name 'sk4d_font_set_hinting';
    class procedure TSkiaAPI.sk4d_font_set_linear_metrics;       external TSkiaAPI.LibName name 'sk4d_font_set_linear_metrics';
    class procedure TSkiaAPI.sk4d_font_set_scale_x;              external TSkiaAPI.LibName name 'sk4d_font_set_scale_x';
    class procedure TSkiaAPI.sk4d_font_set_size;                 external TSkiaAPI.LibName name 'sk4d_font_set_size';
    class procedure TSkiaAPI.sk4d_font_set_skew_x;               external TSkiaAPI.LibName name 'sk4d_font_set_skew_x';
    class procedure TSkiaAPI.sk4d_font_set_subpixel;             external TSkiaAPI.LibName name 'sk4d_font_set_subpixel';
    class procedure TSkiaAPI.sk4d_font_set_typeface;             external TSkiaAPI.LibName name 'sk4d_font_set_typeface';
    class function  TSkiaAPI.sk4d_font_unichar_to_glyph;         external TSkiaAPI.LibName name 'sk4d_font_unichar_to_glyph';
    class procedure TSkiaAPI.sk4d_font_unichars_to_glyphs;       external TSkiaAPI.LibName name 'sk4d_font_unichars_to_glyphs';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_graphics.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_graphics_allow_jit                                       := GetProcAddress(FLibHandle, 'sk4d_graphics_allow_jit');
    sk4d_graphics_dump_memory_statistics                          := GetProcAddress(FLibHandle, 'sk4d_graphics_dump_memory_statistics');
    sk4d_graphics_get_font_cache_count_limit                      := GetProcAddress(FLibHandle, 'sk4d_graphics_get_font_cache_count_limit');
    sk4d_graphics_get_font_cache_count_used                       := GetProcAddress(FLibHandle, 'sk4d_graphics_get_font_cache_count_used');
    sk4d_graphics_get_font_cache_limit                            := GetProcAddress(FLibHandle, 'sk4d_graphics_get_font_cache_limit');
    sk4d_graphics_get_font_cache_used                             := GetProcAddress(FLibHandle, 'sk4d_graphics_get_font_cache_used');
    sk4d_graphics_get_resource_cache_single_allocation_byte_limit := GetProcAddress(FLibHandle, 'sk4d_graphics_get_resource_cache_single_allocation_byte_limit');
    sk4d_graphics_get_resource_cache_total_byte_limit             := GetProcAddress(FLibHandle, 'sk4d_graphics_get_resource_cache_total_byte_limit');
    sk4d_graphics_get_resource_cache_total_bytes_used             := GetProcAddress(FLibHandle, 'sk4d_graphics_get_resource_cache_total_bytes_used');
    sk4d_graphics_init                                            := GetProcAddress(FLibHandle, 'sk4d_graphics_init');
    sk4d_graphics_purge_all_caches                                := GetProcAddress(FLibHandle, 'sk4d_graphics_purge_all_caches');
    sk4d_graphics_purge_font_cache                                := GetProcAddress(FLibHandle, 'sk4d_graphics_purge_font_cache');
    sk4d_graphics_purge_resource_cache                            := GetProcAddress(FLibHandle, 'sk4d_graphics_purge_resource_cache');
    sk4d_graphics_set_font_cache_count_limit                      := GetProcAddress(FLibHandle, 'sk4d_graphics_set_font_cache_count_limit');
    sk4d_graphics_set_font_cache_limit                            := GetProcAddress(FLibHandle, 'sk4d_graphics_set_font_cache_limit');
    sk4d_graphics_set_resource_cache_single_allocation_byte_limit := GetProcAddress(FLibHandle, 'sk4d_graphics_set_resource_cache_single_allocation_byte_limit');
    sk4d_graphics_set_resource_cache_total_byte_limit             := GetProcAddress(FLibHandle, 'sk4d_graphics_set_resource_cache_total_byte_limit');
    {$ELSE}
    class procedure TSkiaAPI.sk4d_graphics_allow_jit;                                       external TSkiaAPI.LibName name 'sk4d_graphics_allow_jit';
    class procedure TSkiaAPI.sk4d_graphics_dump_memory_statistics;                          external TSkiaAPI.LibName name 'sk4d_graphics_dump_memory_statistics';
    class function  TSkiaAPI.sk4d_graphics_get_font_cache_count_limit;                      external TSkiaAPI.LibName name 'sk4d_graphics_get_font_cache_count_limit';
    class function  TSkiaAPI.sk4d_graphics_get_font_cache_count_used;                       external TSkiaAPI.LibName name 'sk4d_graphics_get_font_cache_count_used';
    class function  TSkiaAPI.sk4d_graphics_get_font_cache_limit;                            external TSkiaAPI.LibName name 'sk4d_graphics_get_font_cache_limit';
    class function  TSkiaAPI.sk4d_graphics_get_font_cache_used;                             external TSkiaAPI.LibName name 'sk4d_graphics_get_font_cache_used';
    class function  TSkiaAPI.sk4d_graphics_get_resource_cache_single_allocation_byte_limit; external TSkiaAPI.LibName name 'sk4d_graphics_get_resource_cache_single_allocation_byte_limit';
    class function  TSkiaAPI.sk4d_graphics_get_resource_cache_total_byte_limit;             external TSkiaAPI.LibName name 'sk4d_graphics_get_resource_cache_total_byte_limit';
    class function  TSkiaAPI.sk4d_graphics_get_resource_cache_total_bytes_used;             external TSkiaAPI.LibName name 'sk4d_graphics_get_resource_cache_total_bytes_used';
    class procedure TSkiaAPI.sk4d_graphics_init;                                            external TSkiaAPI.LibName name 'sk4d_graphics_init';
    class procedure TSkiaAPI.sk4d_graphics_purge_all_caches;                                external TSkiaAPI.LibName name 'sk4d_graphics_purge_all_caches';
    class procedure TSkiaAPI.sk4d_graphics_purge_font_cache;                                external TSkiaAPI.LibName name 'sk4d_graphics_purge_font_cache';
    class procedure TSkiaAPI.sk4d_graphics_purge_resource_cache;                            external TSkiaAPI.LibName name 'sk4d_graphics_purge_resource_cache';
    class function  TSkiaAPI.sk4d_graphics_set_font_cache_count_limit;                      external TSkiaAPI.LibName name 'sk4d_graphics_set_font_cache_count_limit';
    class function  TSkiaAPI.sk4d_graphics_set_font_cache_limit;                            external TSkiaAPI.LibName name 'sk4d_graphics_set_font_cache_limit';
    class function  TSkiaAPI.sk4d_graphics_set_resource_cache_single_allocation_byte_limit; external TSkiaAPI.LibName name 'sk4d_graphics_set_resource_cache_single_allocation_byte_limit';
    class function  TSkiaAPI.sk4d_graphics_set_resource_cache_total_byte_limit;             external TSkiaAPI.LibName name 'sk4d_graphics_set_resource_cache_total_byte_limit';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_image.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_image_encode_to_file            := GetProcAddress(FLibHandle, 'sk4d_image_encode_to_file');
    sk4d_image_encode_to_stream          := GetProcAddress(FLibHandle, 'sk4d_image_encode_to_stream');
    sk4d_image_get_alpha_type            := GetProcAddress(FLibHandle, 'sk4d_image_get_alpha_type');
    sk4d_image_get_color_space           := GetProcAddress(FLibHandle, 'sk4d_image_get_color_space');
    sk4d_image_get_color_type            := GetProcAddress(FLibHandle, 'sk4d_image_get_color_type');
    sk4d_image_get_height                := GetProcAddress(FLibHandle, 'sk4d_image_get_height');
    sk4d_image_get_image_info            := GetProcAddress(FLibHandle, 'sk4d_image_get_image_info');
    sk4d_image_get_unique_id             := GetProcAddress(FLibHandle, 'sk4d_image_get_unique_id');
    sk4d_image_get_width                 := GetProcAddress(FLibHandle, 'sk4d_image_get_width');
    sk4d_image_is_lazy_generated         := GetProcAddress(FLibHandle, 'sk4d_image_is_lazy_generated');
    sk4d_image_is_texture_backed         := GetProcAddress(FLibHandle, 'sk4d_image_is_texture_backed');
    sk4d_image_is_valid                  := GetProcAddress(FLibHandle, 'sk4d_image_is_valid');
    sk4d_image_make_from_adopted_texture := GetProcAddress(FLibHandle, 'sk4d_image_make_from_adopted_texture');
    sk4d_image_make_from_encoded_file    := GetProcAddress(FLibHandle, 'sk4d_image_make_from_encoded_file');
    sk4d_image_make_from_encoded_stream  := GetProcAddress(FLibHandle, 'sk4d_image_make_from_encoded_stream');
    sk4d_image_make_from_raster          := GetProcAddress(FLibHandle, 'sk4d_image_make_from_raster');
    sk4d_image_make_from_texture         := GetProcAddress(FLibHandle, 'sk4d_image_make_from_texture');
    sk4d_image_make_non_texture_image    := GetProcAddress(FLibHandle, 'sk4d_image_make_non_texture_image');
    sk4d_image_make_raster_copy          := GetProcAddress(FLibHandle, 'sk4d_image_make_raster_copy');
    sk4d_image_make_raster_image         := GetProcAddress(FLibHandle, 'sk4d_image_make_raster_image');
    sk4d_image_make_shader               := GetProcAddress(FLibHandle, 'sk4d_image_make_shader');
    sk4d_image_make_subset               := GetProcAddress(FLibHandle, 'sk4d_image_make_subset');
    sk4d_image_make_texture_image        := GetProcAddress(FLibHandle, 'sk4d_image_make_texture_image');
    sk4d_image_make_with_filter          := GetProcAddress(FLibHandle, 'sk4d_image_make_with_filter');
    sk4d_image_peek_pixels               := GetProcAddress(FLibHandle, 'sk4d_image_peek_pixels');
    sk4d_image_read_pixels               := GetProcAddress(FLibHandle, 'sk4d_image_read_pixels');
    sk4d_image_scale_pixels              := GetProcAddress(FLibHandle, 'sk4d_image_scale_pixels');
    {$ELSE}
    class procedure TSkiaAPI.sk4d_image_encode_to_file;            external TSkiaAPI.LibName name 'sk4d_image_encode_to_file';
    class procedure TSkiaAPI.sk4d_image_encode_to_stream;          external TSkiaAPI.LibName name 'sk4d_image_encode_to_stream';
    class function  TSkiaAPI.sk4d_image_get_alpha_type;            external TSkiaAPI.LibName name 'sk4d_image_get_alpha_type';
    class function  TSkiaAPI.sk4d_image_get_color_space;           external TSkiaAPI.LibName name 'sk4d_image_get_color_space';
    class function  TSkiaAPI.sk4d_image_get_color_type;            external TSkiaAPI.LibName name 'sk4d_image_get_color_type';
    class function  TSkiaAPI.sk4d_image_get_height;                external TSkiaAPI.LibName name 'sk4d_image_get_height';
    class procedure TSkiaAPI.sk4d_image_get_image_info;            external TSkiaAPI.LibName name 'sk4d_image_get_image_info';
    class function  TSkiaAPI.sk4d_image_get_unique_id;             external TSkiaAPI.LibName name 'sk4d_image_get_unique_id';
    class function  TSkiaAPI.sk4d_image_get_width;                 external TSkiaAPI.LibName name 'sk4d_image_get_width';
    class function  TSkiaAPI.sk4d_image_is_lazy_generated;         external TSkiaAPI.LibName name 'sk4d_image_is_lazy_generated';
    class function  TSkiaAPI.sk4d_image_is_texture_backed;         external TSkiaAPI.LibName name 'sk4d_image_is_texture_backed';
    class function  TSkiaAPI.sk4d_image_is_valid;                  external TSkiaAPI.LibName name 'sk4d_image_is_valid';
    class function  TSkiaAPI.sk4d_image_make_from_adopted_texture; external TSkiaAPI.LibName name 'sk4d_image_make_from_adopted_texture';
    class function  TSkiaAPI.sk4d_image_make_from_encoded_file;    external TSkiaAPI.LibName name 'sk4d_image_make_from_encoded_file';
    class function  TSkiaAPI.sk4d_image_make_from_encoded_stream;  external TSkiaAPI.LibName name 'sk4d_image_make_from_encoded_stream';
    class function  TSkiaAPI.sk4d_image_make_from_raster;          external TSkiaAPI.LibName name 'sk4d_image_make_from_raster';
    class function  TSkiaAPI.sk4d_image_make_from_texture;         external TSkiaAPI.LibName name 'sk4d_image_make_from_texture';
    class function  TSkiaAPI.sk4d_image_make_non_texture_image;    external TSkiaAPI.LibName name 'sk4d_image_make_non_texture_image';
    class function  TSkiaAPI.sk4d_image_make_raster_copy;          external TSkiaAPI.LibName name 'sk4d_image_make_raster_copy';
    class function  TSkiaAPI.sk4d_image_make_raster_image;         external TSkiaAPI.LibName name 'sk4d_image_make_raster_image';
    class function  TSkiaAPI.sk4d_image_make_shader;               external TSkiaAPI.LibName name 'sk4d_image_make_shader';
    class function  TSkiaAPI.sk4d_image_make_subset;               external TSkiaAPI.LibName name 'sk4d_image_make_subset';
    class function  TSkiaAPI.sk4d_image_make_texture_image;        external TSkiaAPI.LibName name 'sk4d_image_make_texture_image';
    class function  TSkiaAPI.sk4d_image_make_with_filter;          external TSkiaAPI.LibName name 'sk4d_image_make_with_filter';
    class function  TSkiaAPI.sk4d_image_peek_pixels;               external TSkiaAPI.LibName name 'sk4d_image_peek_pixels';
    class function  TSkiaAPI.sk4d_image_read_pixels;               external TSkiaAPI.LibName name 'sk4d_image_read_pixels';
    class function  TSkiaAPI.sk4d_image_scale_pixels;              external TSkiaAPI.LibName name 'sk4d_image_scale_pixels';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_imageencoder.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_imageencoder_encode_to_file   := GetProcAddress(FLibHandle, 'sk4d_imageencoder_encode_to_file');
    sk4d_imageencoder_encode_to_stream := GetProcAddress(FLibHandle, 'sk4d_imageencoder_encode_to_stream');
    {$ELSE}
    class function TSkiaAPI.sk4d_imageencoder_encode_to_file;   external TSkiaAPI.LibName name 'sk4d_imageencoder_encode_to_file';
    class function TSkiaAPI.sk4d_imageencoder_encode_to_stream; external TSkiaAPI.LibName name 'sk4d_imageencoder_encode_to_stream';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_imagefilter.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_imagefilter_make_alpha_threshold      := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_alpha_threshold');
    sk4d_imagefilter_make_arithmetic           := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_arithmetic');
    sk4d_imagefilter_make_blend                := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_blend');
    sk4d_imagefilter_make_blur                 := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_blur');
    sk4d_imagefilter_make_colorfilter          := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_colorfilter');
    sk4d_imagefilter_make_compose              := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_compose');
    sk4d_imagefilter_make_dilate               := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_dilate');
    sk4d_imagefilter_make_displacement_map     := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_displacement_map');
    sk4d_imagefilter_make_distant_lit_diffuse  := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_distant_lit_diffuse');
    sk4d_imagefilter_make_distant_lit_specular := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_distant_lit_specular');
    sk4d_imagefilter_make_drop_shadow          := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_drop_shadow');
    sk4d_imagefilter_make_drop_shadow_only     := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_drop_shadow_only');
    sk4d_imagefilter_make_erode                := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_erode');
    sk4d_imagefilter_make_image                := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_image');
    sk4d_imagefilter_make_magnifier            := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_magnifier');
    sk4d_imagefilter_make_matrix_convolution   := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_matrix_convolution');
    sk4d_imagefilter_make_matrix_transform     := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_matrix_transform');
    sk4d_imagefilter_make_merge                := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_merge');
    sk4d_imagefilter_make_offset               := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_offset');
    sk4d_imagefilter_make_picture              := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_picture');
    sk4d_imagefilter_make_point_lit_diffuse    := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_point_lit_diffuse');
    sk4d_imagefilter_make_point_lit_specular   := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_point_lit_specular');
    sk4d_imagefilter_make_shader               := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_shader');
    sk4d_imagefilter_make_spot_lit_diffuse     := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_spot_lit_diffuse');
    sk4d_imagefilter_make_spot_lit_specular    := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_spot_lit_specular');
    sk4d_imagefilter_make_tile                 := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_tile');
    sk4d_imagefilter_make_with_local_matrix    := GetProcAddress(FLibHandle, 'sk4d_imagefilter_make_with_local_matrix');
    {$ELSE}
    class function TSkiaAPI.sk4d_imagefilter_make_alpha_threshold;      external TSkiaAPI.LibName name 'sk4d_imagefilter_make_alpha_threshold';
    class function TSkiaAPI.sk4d_imagefilter_make_arithmetic;           external TSkiaAPI.LibName name 'sk4d_imagefilter_make_arithmetic';
    class function TSkiaAPI.sk4d_imagefilter_make_blend;                external TSkiaAPI.LibName name 'sk4d_imagefilter_make_blend';
    class function TSkiaAPI.sk4d_imagefilter_make_blur;                 external TSkiaAPI.LibName name 'sk4d_imagefilter_make_blur';
    class function TSkiaAPI.sk4d_imagefilter_make_colorfilter;          external TSkiaAPI.LibName name 'sk4d_imagefilter_make_colorfilter';
    class function TSkiaAPI.sk4d_imagefilter_make_compose;              external TSkiaAPI.LibName name 'sk4d_imagefilter_make_compose';
    class function TSkiaAPI.sk4d_imagefilter_make_dilate;               external TSkiaAPI.LibName name 'sk4d_imagefilter_make_dilate';
    class function TSkiaAPI.sk4d_imagefilter_make_displacement_map;     external TSkiaAPI.LibName name 'sk4d_imagefilter_make_displacement_map';
    class function TSkiaAPI.sk4d_imagefilter_make_distant_lit_diffuse;  external TSkiaAPI.LibName name 'sk4d_imagefilter_make_distant_lit_diffuse';
    class function TSkiaAPI.sk4d_imagefilter_make_distant_lit_specular; external TSkiaAPI.LibName name 'sk4d_imagefilter_make_distant_lit_specular';
    class function TSkiaAPI.sk4d_imagefilter_make_drop_shadow;          external TSkiaAPI.LibName name 'sk4d_imagefilter_make_drop_shadow';
    class function TSkiaAPI.sk4d_imagefilter_make_drop_shadow_only;     external TSkiaAPI.LibName name 'sk4d_imagefilter_make_drop_shadow_only';
    class function TSkiaAPI.sk4d_imagefilter_make_erode;                external TSkiaAPI.LibName name 'sk4d_imagefilter_make_erode';
    class function TSkiaAPI.sk4d_imagefilter_make_image;                external TSkiaAPI.LibName name 'sk4d_imagefilter_make_image';
    class function TSkiaAPI.sk4d_imagefilter_make_magnifier;            external TSkiaAPI.LibName name 'sk4d_imagefilter_make_magnifier';
    class function TSkiaAPI.sk4d_imagefilter_make_matrix_convolution;   external TSkiaAPI.LibName name 'sk4d_imagefilter_make_matrix_convolution';
    class function TSkiaAPI.sk4d_imagefilter_make_matrix_transform;     external TSkiaAPI.LibName name 'sk4d_imagefilter_make_matrix_transform';
    class function TSkiaAPI.sk4d_imagefilter_make_merge;                external TSkiaAPI.LibName name 'sk4d_imagefilter_make_merge';
    class function TSkiaAPI.sk4d_imagefilter_make_offset;               external TSkiaAPI.LibName name 'sk4d_imagefilter_make_offset';
    class function TSkiaAPI.sk4d_imagefilter_make_picture;              external TSkiaAPI.LibName name 'sk4d_imagefilter_make_picture';
    class function TSkiaAPI.sk4d_imagefilter_make_point_lit_diffuse;    external TSkiaAPI.LibName name 'sk4d_imagefilter_make_point_lit_diffuse';
    class function TSkiaAPI.sk4d_imagefilter_make_point_lit_specular;   external TSkiaAPI.LibName name 'sk4d_imagefilter_make_point_lit_specular';
    class function TSkiaAPI.sk4d_imagefilter_make_shader;               external TSkiaAPI.LibName name 'sk4d_imagefilter_make_shader';
    class function TSkiaAPI.sk4d_imagefilter_make_spot_lit_diffuse;     external TSkiaAPI.LibName name 'sk4d_imagefilter_make_spot_lit_diffuse';
    class function TSkiaAPI.sk4d_imagefilter_make_spot_lit_specular;    external TSkiaAPI.LibName name 'sk4d_imagefilter_make_spot_lit_specular';
    class function TSkiaAPI.sk4d_imagefilter_make_tile;                 external TSkiaAPI.LibName name 'sk4d_imagefilter_make_tile';
    class function TSkiaAPI.sk4d_imagefilter_make_with_local_matrix;    external TSkiaAPI.LibName name 'sk4d_imagefilter_make_with_local_matrix';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_maskfilter.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_maskfilter_make_blur        := GetProcAddress(FLibHandle, 'sk4d_maskfilter_make_blur');
    sk4d_maskfilter_make_shader      := GetProcAddress(FLibHandle, 'sk4d_maskfilter_make_shader');
    sk4d_maskfilter_make_table       := GetProcAddress(FLibHandle, 'sk4d_maskfilter_make_table');
    sk4d_maskfilter_make_table_clip  := GetProcAddress(FLibHandle, 'sk4d_maskfilter_make_table_clip');
    sk4d_maskfilter_make_table_gamma := GetProcAddress(FLibHandle, 'sk4d_maskfilter_make_table_gamma');
    {$ELSE}
    class function TSkiaAPI.sk4d_maskfilter_make_blur;        external TSkiaAPI.LibName name 'sk4d_maskfilter_make_blur';
    class function TSkiaAPI.sk4d_maskfilter_make_shader;      external TSkiaAPI.LibName name 'sk4d_maskfilter_make_shader';
    class function TSkiaAPI.sk4d_maskfilter_make_table;       external TSkiaAPI.LibName name 'sk4d_maskfilter_make_table';
    class function TSkiaAPI.sk4d_maskfilter_make_table_clip;  external TSkiaAPI.LibName name 'sk4d_maskfilter_make_table_clip';
    class function TSkiaAPI.sk4d_maskfilter_make_table_gamma; external TSkiaAPI.LibName name 'sk4d_maskfilter_make_table_gamma';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_paint.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_paint_create             := GetProcAddress(FLibHandle, 'sk4d_paint_create');
    sk4d_paint_create2            := GetProcAddress(FLibHandle, 'sk4d_paint_create2');
    sk4d_paint_destroy            := GetProcAddress(FLibHandle, 'sk4d_paint_destroy');
    sk4d_paint_get_alpha          := GetProcAddress(FLibHandle, 'sk4d_paint_get_alpha');
    sk4d_paint_get_alphaf         := GetProcAddress(FLibHandle, 'sk4d_paint_get_alphaf');
    sk4d_paint_get_anti_alias     := GetProcAddress(FLibHandle, 'sk4d_paint_get_anti_alias');
    sk4d_paint_get_blender        := GetProcAddress(FLibHandle, 'sk4d_paint_get_blender');
    sk4d_paint_get_color          := GetProcAddress(FLibHandle, 'sk4d_paint_get_color');
    sk4d_paint_get_colorf         := GetProcAddress(FLibHandle, 'sk4d_paint_get_colorf');
    sk4d_paint_get_color_filter   := GetProcAddress(FLibHandle, 'sk4d_paint_get_color_filter');
    sk4d_paint_get_dither         := GetProcAddress(FLibHandle, 'sk4d_paint_get_dither');
    sk4d_paint_get_fill_path      := GetProcAddress(FLibHandle, 'sk4d_paint_get_fill_path');
    sk4d_paint_get_image_filter   := GetProcAddress(FLibHandle, 'sk4d_paint_get_image_filter');
    sk4d_paint_get_mask_filter    := GetProcAddress(FLibHandle, 'sk4d_paint_get_mask_filter');
    sk4d_paint_get_path_effect    := GetProcAddress(FLibHandle, 'sk4d_paint_get_path_effect');
    sk4d_paint_get_shader         := GetProcAddress(FLibHandle, 'sk4d_paint_get_shader');
    sk4d_paint_get_stroke_cap     := GetProcAddress(FLibHandle, 'sk4d_paint_get_stroke_cap');
    sk4d_paint_get_stroke_join    := GetProcAddress(FLibHandle, 'sk4d_paint_get_stroke_join');
    sk4d_paint_get_stroke_miter   := GetProcAddress(FLibHandle, 'sk4d_paint_get_stroke_miter');
    sk4d_paint_get_stroke_width   := GetProcAddress(FLibHandle, 'sk4d_paint_get_stroke_width');
    sk4d_paint_get_style          := GetProcAddress(FLibHandle, 'sk4d_paint_get_style');
    sk4d_paint_reset              := GetProcAddress(FLibHandle, 'sk4d_paint_reset');
    sk4d_paint_set_alpha          := GetProcAddress(FLibHandle, 'sk4d_paint_set_alpha');
    sk4d_paint_set_alphaf         := GetProcAddress(FLibHandle, 'sk4d_paint_set_alphaf');
    sk4d_paint_set_antialias      := GetProcAddress(FLibHandle, 'sk4d_paint_set_antialias');
    sk4d_paint_set_argb           := GetProcAddress(FLibHandle, 'sk4d_paint_set_argb');
    sk4d_paint_set_blender        := GetProcAddress(FLibHandle, 'sk4d_paint_set_blender');
    sk4d_paint_set_color          := GetProcAddress(FLibHandle, 'sk4d_paint_set_color');
    sk4d_paint_set_colorf         := GetProcAddress(FLibHandle, 'sk4d_paint_set_colorf');
    sk4d_paint_set_color_filter   := GetProcAddress(FLibHandle, 'sk4d_paint_set_color_filter');
    sk4d_paint_set_dither         := GetProcAddress(FLibHandle, 'sk4d_paint_set_dither');
    sk4d_paint_set_image_filter   := GetProcAddress(FLibHandle, 'sk4d_paint_set_image_filter');
    sk4d_paint_set_mask_filter    := GetProcAddress(FLibHandle, 'sk4d_paint_set_mask_filter');
    sk4d_paint_set_path_effect    := GetProcAddress(FLibHandle, 'sk4d_paint_set_path_effect');
    sk4d_paint_set_shader         := GetProcAddress(FLibHandle, 'sk4d_paint_set_shader');
    sk4d_paint_set_stroke_cap     := GetProcAddress(FLibHandle, 'sk4d_paint_set_stroke_cap');
    sk4d_paint_set_stroke_join    := GetProcAddress(FLibHandle, 'sk4d_paint_set_stroke_join');
    sk4d_paint_set_stroke_miter   := GetProcAddress(FLibHandle, 'sk4d_paint_set_stroke_miter');
    sk4d_paint_set_stroke_width   := GetProcAddress(FLibHandle, 'sk4d_paint_set_stroke_width');
    sk4d_paint_set_style          := GetProcAddress(FLibHandle, 'sk4d_paint_set_style');
    {$ELSE}
    class function  TSkiaAPI.sk4d_paint_create;             external TSkiaAPI.LibName name 'sk4d_paint_create';
    class function  TSkiaAPI.sk4d_paint_create2;            external TSkiaAPI.LibName name 'sk4d_paint_create2';
    class procedure TSkiaAPI.sk4d_paint_destroy;            external TSkiaAPI.LibName name 'sk4d_paint_destroy';
    class function  TSkiaAPI.sk4d_paint_get_alpha;          external TSkiaAPI.LibName name 'sk4d_paint_get_alpha';
    class function  TSkiaAPI.sk4d_paint_get_alphaf;         external TSkiaAPI.LibName name 'sk4d_paint_get_alphaf';
    class function  TSkiaAPI.sk4d_paint_get_anti_alias;     external TSkiaAPI.LibName name 'sk4d_paint_get_anti_alias';
    class function  TSkiaAPI.sk4d_paint_get_blender;        external TSkiaAPI.LibName name 'sk4d_paint_get_blender';
    class function  TSkiaAPI.sk4d_paint_get_color;          external TSkiaAPI.LibName name 'sk4d_paint_get_color';
    class procedure TSkiaAPI.sk4d_paint_get_colorf;         external TSkiaAPI.LibName name 'sk4d_paint_get_colorf';
    class function  TSkiaAPI.sk4d_paint_get_color_filter;   external TSkiaAPI.LibName name 'sk4d_paint_get_color_filter';
    class function  TSkiaAPI.sk4d_paint_get_dither;         external TSkiaAPI.LibName name 'sk4d_paint_get_dither';
    class function  TSkiaAPI.sk4d_paint_get_fill_path;      external TSkiaAPI.LibName name 'sk4d_paint_get_fill_path';
    class function  TSkiaAPI.sk4d_paint_get_image_filter;   external TSkiaAPI.LibName name 'sk4d_paint_get_image_filter';
    class function  TSkiaAPI.sk4d_paint_get_mask_filter;    external TSkiaAPI.LibName name 'sk4d_paint_get_mask_filter';
    class function  TSkiaAPI.sk4d_paint_get_path_effect;    external TSkiaAPI.LibName name 'sk4d_paint_get_path_effect';
    class function  TSkiaAPI.sk4d_paint_get_shader;         external TSkiaAPI.LibName name 'sk4d_paint_get_shader';
    class function  TSkiaAPI.sk4d_paint_get_stroke_cap;     external TSkiaAPI.LibName name 'sk4d_paint_get_stroke_cap';
    class function  TSkiaAPI.sk4d_paint_get_stroke_join;    external TSkiaAPI.LibName name 'sk4d_paint_get_stroke_join';
    class function  TSkiaAPI.sk4d_paint_get_stroke_miter;   external TSkiaAPI.LibName name 'sk4d_paint_get_stroke_miter';
    class function  TSkiaAPI.sk4d_paint_get_stroke_width;   external TSkiaAPI.LibName name 'sk4d_paint_get_stroke_width';
    class function  TSkiaAPI.sk4d_paint_get_style;          external TSkiaAPI.LibName name 'sk4d_paint_get_style';
    class procedure TSkiaAPI.sk4d_paint_reset;              external TSkiaAPI.LibName name 'sk4d_paint_reset';
    class procedure TSkiaAPI.sk4d_paint_set_alpha;          external TSkiaAPI.LibName name 'sk4d_paint_set_alpha';
    class procedure TSkiaAPI.sk4d_paint_set_alphaf;         external TSkiaAPI.LibName name 'sk4d_paint_set_alphaf';
    class procedure TSkiaAPI.sk4d_paint_set_antialias;      external TSkiaAPI.LibName name 'sk4d_paint_set_antialias';
    class procedure TSkiaAPI.sk4d_paint_set_argb;           external TSkiaAPI.LibName name 'sk4d_paint_set_argb';
    class procedure TSkiaAPI.sk4d_paint_set_blender;        external TSkiaAPI.LibName name 'sk4d_paint_set_blender';
    class procedure TSkiaAPI.sk4d_paint_set_color;          external TSkiaAPI.LibName name 'sk4d_paint_set_color';
    class procedure TSkiaAPI.sk4d_paint_set_colorf;         external TSkiaAPI.LibName name 'sk4d_paint_set_colorf';
    class procedure TSkiaAPI.sk4d_paint_set_color_filter;   external TSkiaAPI.LibName name 'sk4d_paint_set_color_filter';
    class procedure TSkiaAPI.sk4d_paint_set_dither;         external TSkiaAPI.LibName name 'sk4d_paint_set_dither';
    class procedure TSkiaAPI.sk4d_paint_set_image_filter;   external TSkiaAPI.LibName name 'sk4d_paint_set_image_filter';
    class procedure TSkiaAPI.sk4d_paint_set_mask_filter;    external TSkiaAPI.LibName name 'sk4d_paint_set_mask_filter';
    class procedure TSkiaAPI.sk4d_paint_set_path_effect;    external TSkiaAPI.LibName name 'sk4d_paint_set_path_effect';
    class procedure TSkiaAPI.sk4d_paint_set_shader;         external TSkiaAPI.LibName name 'sk4d_paint_set_shader';
    class procedure TSkiaAPI.sk4d_paint_set_stroke_cap;     external TSkiaAPI.LibName name 'sk4d_paint_set_stroke_cap';
    class procedure TSkiaAPI.sk4d_paint_set_stroke_join;    external TSkiaAPI.LibName name 'sk4d_paint_set_stroke_join';
    class procedure TSkiaAPI.sk4d_paint_set_stroke_miter;   external TSkiaAPI.LibName name 'sk4d_paint_set_stroke_miter';
    class procedure TSkiaAPI.sk4d_paint_set_stroke_width;   external TSkiaAPI.LibName name 'sk4d_paint_set_stroke_width';
    class procedure TSkiaAPI.sk4d_paint_set_style;          external TSkiaAPI.LibName name 'sk4d_paint_set_style';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_path.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_opbuilder_add               := GetProcAddress(FLibHandle, 'sk4d_opbuilder_add');
    sk4d_opbuilder_create            := GetProcAddress(FLibHandle, 'sk4d_opbuilder_create');
    sk4d_opbuilder_destroy           := GetProcAddress(FLibHandle, 'sk4d_opbuilder_destroy');
    sk4d_opbuilder_detach            := GetProcAddress(FLibHandle, 'sk4d_opbuilder_detach');
    sk4d_path_contains               := GetProcAddress(FLibHandle, 'sk4d_path_contains');
    sk4d_path_convert_conic_to_quads := GetProcAddress(FLibHandle, 'sk4d_path_convert_conic_to_quads');
    sk4d_path_create                 := GetProcAddress(FLibHandle, 'sk4d_path_create');
    sk4d_path_create2                := GetProcAddress(FLibHandle, 'sk4d_path_create2');
    sk4d_path_destroy                := GetProcAddress(FLibHandle, 'sk4d_path_destroy');
    sk4d_path_get_bounds             := GetProcAddress(FLibHandle, 'sk4d_path_get_bounds');
    sk4d_path_get_fill_type          := GetProcAddress(FLibHandle, 'sk4d_path_get_fill_type');
    sk4d_path_get_last_point         := GetProcAddress(FLibHandle, 'sk4d_path_get_last_point');
    sk4d_path_get_segment_masks      := GetProcAddress(FLibHandle, 'sk4d_path_get_segment_masks');
    sk4d_path_get_tight_bounds       := GetProcAddress(FLibHandle, 'sk4d_path_get_tight_bounds');
    sk4d_path_interpolate            := GetProcAddress(FLibHandle, 'sk4d_path_interpolate');
    sk4d_path_is_convex              := GetProcAddress(FLibHandle, 'sk4d_path_is_convex');
    sk4d_path_is_empty               := GetProcAddress(FLibHandle, 'sk4d_path_is_empty');
    sk4d_path_is_finite              := GetProcAddress(FLibHandle, 'sk4d_path_is_finite');
    sk4d_path_is_interpolatable      := GetProcAddress(FLibHandle, 'sk4d_path_is_interpolatable');
    sk4d_path_is_last_contour_closed := GetProcAddress(FLibHandle, 'sk4d_path_is_last_contour_closed');
    sk4d_path_is_line                := GetProcAddress(FLibHandle, 'sk4d_path_is_line');
    sk4d_path_is_oval                := GetProcAddress(FLibHandle, 'sk4d_path_is_oval');
    sk4d_path_is_rect                := GetProcAddress(FLibHandle, 'sk4d_path_is_rect');
    sk4d_path_is_rrect               := GetProcAddress(FLibHandle, 'sk4d_path_is_rrect');
    sk4d_path_serialize_to_stream    := GetProcAddress(FLibHandle, 'sk4d_path_serialize_to_stream');
    sk4d_path_to_svg                 := GetProcAddress(FLibHandle, 'sk4d_path_to_svg');
    sk4d_path_transform              := GetProcAddress(FLibHandle, 'sk4d_path_transform');
    sk4d_pathiterator_create         := GetProcAddress(FLibHandle, 'sk4d_pathiterator_create');
    sk4d_pathiterator_destroy        := GetProcAddress(FLibHandle, 'sk4d_pathiterator_destroy');
    sk4d_pathiterator_next           := GetProcAddress(FLibHandle, 'sk4d_pathiterator_next');
    {$ELSE}
    class procedure TSkiaAPI.sk4d_opbuilder_add;               external TSkiaAPI.LibName name 'sk4d_opbuilder_add';
    class function  TSkiaAPI.sk4d_opbuilder_create;            external TSkiaAPI.LibName name 'sk4d_opbuilder_create';
    class procedure TSkiaAPI.sk4d_opbuilder_destroy;           external TSkiaAPI.LibName name 'sk4d_opbuilder_destroy';
    class function  TSkiaAPI.sk4d_opbuilder_detach;            external TSkiaAPI.LibName name 'sk4d_opbuilder_detach';
    class function  TSkiaAPI.sk4d_path_contains;               external TSkiaAPI.LibName name 'sk4d_path_contains';
    class function  TSkiaAPI.sk4d_path_convert_conic_to_quads; external TSkiaAPI.LibName name 'sk4d_path_convert_conic_to_quads';
    class function  TSkiaAPI.sk4d_path_create;                 external TSkiaAPI.LibName name 'sk4d_path_create';
    class function  TSkiaAPI.sk4d_path_create2;                external TSkiaAPI.LibName name 'sk4d_path_create2';
    class procedure TSkiaAPI.sk4d_path_destroy;                external TSkiaAPI.LibName name 'sk4d_path_destroy';
    class procedure TSkiaAPI.sk4d_path_get_bounds;             external TSkiaAPI.LibName name 'sk4d_path_get_bounds';
    class function  TSkiaAPI.sk4d_path_get_fill_type;          external TSkiaAPI.LibName name 'sk4d_path_get_fill_type';
    class function  TSkiaAPI.sk4d_path_get_last_point;         external TSkiaAPI.LibName name 'sk4d_path_get_last_point';
    class function  TSkiaAPI.sk4d_path_get_segment_masks;      external TSkiaAPI.LibName name 'sk4d_path_get_segment_masks';
    class procedure TSkiaAPI.sk4d_path_get_tight_bounds;       external TSkiaAPI.LibName name 'sk4d_path_get_tight_bounds';
    class function  TSkiaAPI.sk4d_path_interpolate;            external TSkiaAPI.LibName name 'sk4d_path_interpolate';
    class function  TSkiaAPI.sk4d_path_is_convex;              external TSkiaAPI.LibName name 'sk4d_path_is_convex';
    class function  TSkiaAPI.sk4d_path_is_empty;               external TSkiaAPI.LibName name 'sk4d_path_is_empty';
    class function  TSkiaAPI.sk4d_path_is_finite;              external TSkiaAPI.LibName name 'sk4d_path_is_finite';
    class function  TSkiaAPI.sk4d_path_is_interpolatable;      external TSkiaAPI.LibName name 'sk4d_path_is_interpolatable';
    class function  TSkiaAPI.sk4d_path_is_last_contour_closed; external TSkiaAPI.LibName name 'sk4d_path_is_last_contour_closed';
    class function  TSkiaAPI.sk4d_path_is_line;                external TSkiaAPI.LibName name 'sk4d_path_is_line';
    class function  TSkiaAPI.sk4d_path_is_oval;                external TSkiaAPI.LibName name 'sk4d_path_is_oval';
    class function  TSkiaAPI.sk4d_path_is_rect;                external TSkiaAPI.LibName name 'sk4d_path_is_rect';
    class function  TSkiaAPI.sk4d_path_is_rrect;               external TSkiaAPI.LibName name 'sk4d_path_is_rrect';
    class procedure TSkiaAPI.sk4d_path_serialize_to_stream;    external TSkiaAPI.LibName name 'sk4d_path_serialize_to_stream';
    class function  TSkiaAPI.sk4d_path_to_svg;                 external TSkiaAPI.LibName name 'sk4d_path_to_svg';
    class function  TSkiaAPI.sk4d_path_transform;              external TSkiaAPI.LibName name 'sk4d_path_transform';
    class function  TSkiaAPI.sk4d_pathiterator_create;         external TSkiaAPI.LibName name 'sk4d_pathiterator_create';
    class procedure TSkiaAPI.sk4d_pathiterator_destroy;        external TSkiaAPI.LibName name 'sk4d_pathiterator_destroy';
    class function  TSkiaAPI.sk4d_pathiterator_next;           external TSkiaAPI.LibName name 'sk4d_pathiterator_next';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_pathbuilder.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_pathbuilder_add_arc                 := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_add_arc');
    sk4d_pathbuilder_add_circle              := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_add_circle');
    sk4d_pathbuilder_add_oval                := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_add_oval');
    sk4d_pathbuilder_add_path                := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_add_path');
    sk4d_pathbuilder_add_polygon             := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_add_polygon');
    sk4d_pathbuilder_add_rect                := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_add_rect');
    sk4d_pathbuilder_add_rrect               := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_add_rrect');
    sk4d_pathbuilder_arc_to                  := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_arc_to');
    sk4d_pathbuilder_arc_to2                 := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_arc_to2');
    sk4d_pathbuilder_arc_to3                 := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_arc_to3');
    sk4d_pathbuilder_close                   := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_close');
    sk4d_pathbuilder_conic_to                := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_conic_to');
    sk4d_pathbuilder_create                  := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_create');
    sk4d_pathbuilder_create2                 := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_create2');
    sk4d_pathbuilder_cubic_to                := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_cubic_to');
    sk4d_pathbuilder_destroy                 := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_destroy');
    sk4d_pathbuilder_detach                  := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_detach');
    sk4d_pathbuilder_get_bounds              := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_get_bounds');
    sk4d_pathbuilder_get_fill_type           := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_get_fill_type');
    sk4d_pathbuilder_inc_reserve             := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_inc_reserve');
    sk4d_pathbuilder_line_to                 := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_line_to');
    sk4d_pathbuilder_move_to                 := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_move_to');
    sk4d_pathbuilder_offset                  := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_offset');
    sk4d_pathbuilder_polyline_to             := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_polyline_to');
    sk4d_pathbuilder_quad_to                 := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_quad_to');
    sk4d_pathbuilder_r_conic_to              := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_r_conic_to');
    sk4d_pathbuilder_r_cubic_to              := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_r_cubic_to');
    sk4d_pathbuilder_r_line_to               := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_r_line_to');
    sk4d_pathbuilder_r_quad_to               := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_r_quad_to');
    sk4d_pathbuilder_reset                   := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_reset');
    sk4d_pathbuilder_set_filltype            := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_set_filltype');
    sk4d_pathbuilder_snapshot                := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_snapshot');
    sk4d_pathbuilder_toggle_inverse_filltype := GetProcAddress(FLibHandle, 'sk4d_pathbuilder_toggle_inverse_filltype');
    {$ELSE}
    class procedure TSkiaAPI.sk4d_pathbuilder_add_arc;                 external TSkiaAPI.LibName name 'sk4d_pathbuilder_add_arc';
    class procedure TSkiaAPI.sk4d_pathbuilder_add_circle;              external TSkiaAPI.LibName name 'sk4d_pathbuilder_add_circle';
    class procedure TSkiaAPI.sk4d_pathbuilder_add_oval;                external TSkiaAPI.LibName name 'sk4d_pathbuilder_add_oval';
    class procedure TSkiaAPI.sk4d_pathbuilder_add_path;                external TSkiaAPI.LibName name 'sk4d_pathbuilder_add_path';
    class procedure TSkiaAPI.sk4d_pathbuilder_add_polygon;             external TSkiaAPI.LibName name 'sk4d_pathbuilder_add_polygon';
    class procedure TSkiaAPI.sk4d_pathbuilder_add_rect;                external TSkiaAPI.LibName name 'sk4d_pathbuilder_add_rect';
    class procedure TSkiaAPI.sk4d_pathbuilder_add_rrect;               external TSkiaAPI.LibName name 'sk4d_pathbuilder_add_rrect';
    class procedure TSkiaAPI.sk4d_pathbuilder_arc_to;                  external TSkiaAPI.LibName name 'sk4d_pathbuilder_arc_to';
    class procedure TSkiaAPI.sk4d_pathbuilder_arc_to2;                 external TSkiaAPI.LibName name 'sk4d_pathbuilder_arc_to2';
    class procedure TSkiaAPI.sk4d_pathbuilder_arc_to3;                 external TSkiaAPI.LibName name 'sk4d_pathbuilder_arc_to3';
    class procedure TSkiaAPI.sk4d_pathbuilder_close;                   external TSkiaAPI.LibName name 'sk4d_pathbuilder_close';
    class procedure TSkiaAPI.sk4d_pathbuilder_conic_to;                external TSkiaAPI.LibName name 'sk4d_pathbuilder_conic_to';
    class function  TSkiaAPI.sk4d_pathbuilder_create;                  external TSkiaAPI.LibName name 'sk4d_pathbuilder_create';
    class function  TSkiaAPI.sk4d_pathbuilder_create2;                 external TSkiaAPI.LibName name 'sk4d_pathbuilder_create2';
    class procedure TSkiaAPI.sk4d_pathbuilder_cubic_to;                external TSkiaAPI.LibName name 'sk4d_pathbuilder_cubic_to';
    class procedure TSkiaAPI.sk4d_pathbuilder_destroy;                 external TSkiaAPI.LibName name 'sk4d_pathbuilder_destroy';
    class function  TSkiaAPI.sk4d_pathbuilder_detach;                  external TSkiaAPI.LibName name 'sk4d_pathbuilder_detach';
    class procedure TSkiaAPI.sk4d_pathbuilder_get_bounds;              external TSkiaAPI.LibName name 'sk4d_pathbuilder_get_bounds';
    class function  TSkiaAPI.sk4d_pathbuilder_get_fill_type;           external TSkiaAPI.LibName name 'sk4d_pathbuilder_get_fill_type';
    class procedure TSkiaAPI.sk4d_pathbuilder_inc_reserve;             external TSkiaAPI.LibName name 'sk4d_pathbuilder_inc_reserve';
    class procedure TSkiaAPI.sk4d_pathbuilder_line_to;                 external TSkiaAPI.LibName name 'sk4d_pathbuilder_line_to';
    class procedure TSkiaAPI.sk4d_pathbuilder_move_to;                 external TSkiaAPI.LibName name 'sk4d_pathbuilder_move_to';
    class procedure TSkiaAPI.sk4d_pathbuilder_offset;                  external TSkiaAPI.LibName name 'sk4d_pathbuilder_offset';
    class procedure TSkiaAPI.sk4d_pathbuilder_polyline_to;             external TSkiaAPI.LibName name 'sk4d_pathbuilder_polyline_to';
    class procedure TSkiaAPI.sk4d_pathbuilder_quad_to;                 external TSkiaAPI.LibName name 'sk4d_pathbuilder_quad_to';
    class procedure TSkiaAPI.sk4d_pathbuilder_r_conic_to;              external TSkiaAPI.LibName name 'sk4d_pathbuilder_r_conic_to';
    class procedure TSkiaAPI.sk4d_pathbuilder_r_cubic_to;              external TSkiaAPI.LibName name 'sk4d_pathbuilder_r_cubic_to';
    class procedure TSkiaAPI.sk4d_pathbuilder_r_line_to;               external TSkiaAPI.LibName name 'sk4d_pathbuilder_r_line_to';
    class procedure TSkiaAPI.sk4d_pathbuilder_r_quad_to;               external TSkiaAPI.LibName name 'sk4d_pathbuilder_r_quad_to';
    class procedure TSkiaAPI.sk4d_pathbuilder_reset;                   external TSkiaAPI.LibName name 'sk4d_pathbuilder_reset';
    class procedure TSkiaAPI.sk4d_pathbuilder_set_filltype;            external TSkiaAPI.LibName name 'sk4d_pathbuilder_set_filltype';
    class function  TSkiaAPI.sk4d_pathbuilder_snapshot;                external TSkiaAPI.LibName name 'sk4d_pathbuilder_snapshot';
    class procedure TSkiaAPI.sk4d_pathbuilder_toggle_inverse_filltype; external TSkiaAPI.LibName name 'sk4d_pathbuilder_toggle_inverse_filltype';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_patheffect.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_patheffect_make_1dpath          := GetProcAddress(FLibHandle, 'sk4d_patheffect_make_1dpath');
    sk4d_patheffect_make_2dline          := GetProcAddress(FLibHandle, 'sk4d_patheffect_make_2dline');
    sk4d_patheffect_make_2dpath          := GetProcAddress(FLibHandle, 'sk4d_patheffect_make_2dpath');
    sk4d_patheffect_make_compose         := GetProcAddress(FLibHandle, 'sk4d_patheffect_make_compose');
    sk4d_patheffect_make_corner          := GetProcAddress(FLibHandle, 'sk4d_patheffect_make_corner');
    sk4d_patheffect_make_dash            := GetProcAddress(FLibHandle, 'sk4d_patheffect_make_dash');
    sk4d_patheffect_make_discrete        := GetProcAddress(FLibHandle, 'sk4d_patheffect_make_discrete');
    sk4d_patheffect_make_matrix          := GetProcAddress(FLibHandle, 'sk4d_patheffect_make_matrix');
    sk4d_patheffect_make_merge           := GetProcAddress(FLibHandle, 'sk4d_patheffect_make_merge');
    sk4d_patheffect_make_stroke          := GetProcAddress(FLibHandle, 'sk4d_patheffect_make_stroke');
    sk4d_patheffect_make_stroke_and_fill := GetProcAddress(FLibHandle, 'sk4d_patheffect_make_stroke_and_fill');
    sk4d_patheffect_make_sum             := GetProcAddress(FLibHandle, 'sk4d_patheffect_make_sum');
    sk4d_patheffect_make_translate       := GetProcAddress(FLibHandle, 'sk4d_patheffect_make_translate');
    sk4d_patheffect_make_trim            := GetProcAddress(FLibHandle, 'sk4d_patheffect_make_trim');
    {$ELSE}
    class function TSkiaAPI.sk4d_patheffect_make_1dpath;          external TSkiaAPI.LibName name 'sk4d_patheffect_make_1dpath';
    class function TSkiaAPI.sk4d_patheffect_make_2dline;          external TSkiaAPI.LibName name 'sk4d_patheffect_make_2dline';
    class function TSkiaAPI.sk4d_patheffect_make_2dpath;          external TSkiaAPI.LibName name 'sk4d_patheffect_make_2dpath';
    class function TSkiaAPI.sk4d_patheffect_make_compose;         external TSkiaAPI.LibName name 'sk4d_patheffect_make_compose';
    class function TSkiaAPI.sk4d_patheffect_make_corner;          external TSkiaAPI.LibName name 'sk4d_patheffect_make_corner';
    class function TSkiaAPI.sk4d_patheffect_make_dash;            external TSkiaAPI.LibName name 'sk4d_patheffect_make_dash';
    class function TSkiaAPI.sk4d_patheffect_make_discrete;        external TSkiaAPI.LibName name 'sk4d_patheffect_make_discrete';
    class function TSkiaAPI.sk4d_patheffect_make_matrix;          external TSkiaAPI.LibName name 'sk4d_patheffect_make_matrix';
    class function TSkiaAPI.sk4d_patheffect_make_merge;           external TSkiaAPI.LibName name 'sk4d_patheffect_make_merge';
    class function TSkiaAPI.sk4d_patheffect_make_stroke;          external TSkiaAPI.LibName name 'sk4d_patheffect_make_stroke';
    class function TSkiaAPI.sk4d_patheffect_make_stroke_and_fill; external TSkiaAPI.LibName name 'sk4d_patheffect_make_stroke_and_fill';
    class function TSkiaAPI.sk4d_patheffect_make_sum;             external TSkiaAPI.LibName name 'sk4d_patheffect_make_sum';
    class function TSkiaAPI.sk4d_patheffect_make_translate;       external TSkiaAPI.LibName name 'sk4d_patheffect_make_translate';
    class function TSkiaAPI.sk4d_patheffect_make_trim;            external TSkiaAPI.LibName name 'sk4d_patheffect_make_trim';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_pathmeasure.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_pathmeasure_create                   := GetProcAddress(FLibHandle, 'sk4d_pathmeasure_create');
    sk4d_pathmeasure_destroy                  := GetProcAddress(FLibHandle, 'sk4d_pathmeasure_destroy');
    sk4d_pathmeasure_get_length               := GetProcAddress(FLibHandle, 'sk4d_pathmeasure_get_length');
    sk4d_pathmeasure_get_matrix               := GetProcAddress(FLibHandle, 'sk4d_pathmeasure_get_matrix');
    sk4d_pathmeasure_get_position_and_tangent := GetProcAddress(FLibHandle, 'sk4d_pathmeasure_get_position_and_tangent');
    sk4d_pathmeasure_get_segment              := GetProcAddress(FLibHandle, 'sk4d_pathmeasure_get_segment');
    sk4d_pathmeasure_is_closed                := GetProcAddress(FLibHandle, 'sk4d_pathmeasure_is_closed');
    sk4d_pathmeasure_next_contour             := GetProcAddress(FLibHandle, 'sk4d_pathmeasure_next_contour');
    {$ELSE}
    class function  TSkiaAPI.sk4d_pathmeasure_create;                   external TSkiaAPI.LibName name 'sk4d_pathmeasure_create';
    class procedure TSkiaAPI.sk4d_pathmeasure_destroy;                  external TSkiaAPI.LibName name 'sk4d_pathmeasure_destroy';
    class function  TSkiaAPI.sk4d_pathmeasure_get_length;               external TSkiaAPI.LibName name 'sk4d_pathmeasure_get_length';
    class function  TSkiaAPI.sk4d_pathmeasure_get_matrix;               external TSkiaAPI.LibName name 'sk4d_pathmeasure_get_matrix';
    class function  TSkiaAPI.sk4d_pathmeasure_get_position_and_tangent; external TSkiaAPI.LibName name 'sk4d_pathmeasure_get_position_and_tangent';
    class function  TSkiaAPI.sk4d_pathmeasure_get_segment;              external TSkiaAPI.LibName name 'sk4d_pathmeasure_get_segment';
    class function  TSkiaAPI.sk4d_pathmeasure_is_closed;                external TSkiaAPI.LibName name 'sk4d_pathmeasure_is_closed';
    class function  TSkiaAPI.sk4d_pathmeasure_next_contour;             external TSkiaAPI.LibName name 'sk4d_pathmeasure_next_contour';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_picture.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_picture_get_cull_rect       := GetProcAddress(FLibHandle, 'sk4d_picture_get_cull_rect');
    sk4d_picture_make_from_stream    := GetProcAddress(FLibHandle, 'sk4d_picture_make_from_stream');
    sk4d_picture_make_shader         := GetProcAddress(FLibHandle, 'sk4d_picture_make_shader');
    sk4d_picture_playback            := GetProcAddress(FLibHandle, 'sk4d_picture_playback');
    sk4d_picture_serialize_to_stream := GetProcAddress(FLibHandle, 'sk4d_picture_serialize_to_stream');
    {$ELSE}
    class procedure TSkiaAPI.sk4d_picture_get_cull_rect;       external TSkiaAPI.LibName name 'sk4d_picture_get_cull_rect';
    class function  TSkiaAPI.sk4d_picture_make_from_stream;    external TSkiaAPI.LibName name 'sk4d_picture_make_from_stream';
    class function  TSkiaAPI.sk4d_picture_make_shader;         external TSkiaAPI.LibName name 'sk4d_picture_make_shader';
    class procedure TSkiaAPI.sk4d_picture_playback;            external TSkiaAPI.LibName name 'sk4d_picture_playback';
    class procedure TSkiaAPI.sk4d_picture_serialize_to_stream; external TSkiaAPI.LibName name 'sk4d_picture_serialize_to_stream';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_picturerecorder.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_picturerecorder_begin_recording   := GetProcAddress(FLibHandle, 'sk4d_picturerecorder_begin_recording');
    sk4d_picturerecorder_create            := GetProcAddress(FLibHandle, 'sk4d_picturerecorder_create');
    sk4d_picturerecorder_destroy           := GetProcAddress(FLibHandle, 'sk4d_picturerecorder_destroy');
    sk4d_picturerecorder_finish_recording  := GetProcAddress(FLibHandle, 'sk4d_picturerecorder_finish_recording');
    sk4d_picturerecorder_finish_recording2 := GetProcAddress(FLibHandle, 'sk4d_picturerecorder_finish_recording2');
    {$ELSE}
    class function  TSkiaAPI.sk4d_picturerecorder_begin_recording;   external TSkiaAPI.LibName name 'sk4d_picturerecorder_begin_recording';
    class function  TSkiaAPI.sk4d_picturerecorder_create;            external TSkiaAPI.LibName name 'sk4d_picturerecorder_create';
    class procedure TSkiaAPI.sk4d_picturerecorder_destroy;           external TSkiaAPI.LibName name 'sk4d_picturerecorder_destroy';
    class function  TSkiaAPI.sk4d_picturerecorder_finish_recording;  external TSkiaAPI.LibName name 'sk4d_picturerecorder_finish_recording';
    class function  TSkiaAPI.sk4d_picturerecorder_finish_recording2; external TSkiaAPI.LibName name 'sk4d_picturerecorder_finish_recording2';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_pixmap.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_pixmap_create          := GetProcAddress(FLibHandle, 'sk4d_pixmap_create');
    sk4d_pixmap_destroy         := GetProcAddress(FLibHandle, 'sk4d_pixmap_destroy');
    sk4d_pixmap_erase           := GetProcAddress(FLibHandle, 'sk4d_pixmap_erase');
    sk4d_pixmap_erase2          := GetProcAddress(FLibHandle, 'sk4d_pixmap_erase2');
    sk4d_pixmap_extract_subset  := GetProcAddress(FLibHandle, 'sk4d_pixmap_extract_subset');
    sk4d_pixmap_get_alpha       := GetProcAddress(FLibHandle, 'sk4d_pixmap_get_alpha');
    sk4d_pixmap_get_alpha_type  := GetProcAddress(FLibHandle, 'sk4d_pixmap_get_alpha_type');
    sk4d_pixmap_get_color       := GetProcAddress(FLibHandle, 'sk4d_pixmap_get_color');
    sk4d_pixmap_get_color_space := GetProcAddress(FLibHandle, 'sk4d_pixmap_get_color_space');
    sk4d_pixmap_get_color_type  := GetProcAddress(FLibHandle, 'sk4d_pixmap_get_color_type');
    sk4d_pixmap_get_height      := GetProcAddress(FLibHandle, 'sk4d_pixmap_get_height');
    sk4d_pixmap_get_image_info  := GetProcAddress(FLibHandle, 'sk4d_pixmap_get_image_info');
    sk4d_pixmap_get_pixel_addr  := GetProcAddress(FLibHandle, 'sk4d_pixmap_get_pixel_addr');
    sk4d_pixmap_get_pixels      := GetProcAddress(FLibHandle, 'sk4d_pixmap_get_pixels');
    sk4d_pixmap_get_row_bytes   := GetProcAddress(FLibHandle, 'sk4d_pixmap_get_row_bytes');
    sk4d_pixmap_get_width       := GetProcAddress(FLibHandle, 'sk4d_pixmap_get_width');
    sk4d_pixmap_read_pixels     := GetProcAddress(FLibHandle, 'sk4d_pixmap_read_pixels');
    sk4d_pixmap_scale_pixels    := GetProcAddress(FLibHandle, 'sk4d_pixmap_scale_pixels');
    sk4d_pixmap_set_colorspace  := GetProcAddress(FLibHandle, 'sk4d_pixmap_set_colorspace');
    {$ELSE}
    class function  TSkiaAPI.sk4d_pixmap_create;          external TSkiaAPI.LibName name 'sk4d_pixmap_create';
    class procedure TSkiaAPI.sk4d_pixmap_destroy;         external TSkiaAPI.LibName name 'sk4d_pixmap_destroy';
    class function  TSkiaAPI.sk4d_pixmap_erase;           external TSkiaAPI.LibName name 'sk4d_pixmap_erase';
    class function  TSkiaAPI.sk4d_pixmap_erase2;          external TSkiaAPI.LibName name 'sk4d_pixmap_erase2';
    class function  TSkiaAPI.sk4d_pixmap_extract_subset;  external TSkiaAPI.LibName name 'sk4d_pixmap_extract_subset';
    class function  TSkiaAPI.sk4d_pixmap_get_alpha;       external TSkiaAPI.LibName name 'sk4d_pixmap_get_alpha';
    class function  TSkiaAPI.sk4d_pixmap_get_alpha_type;  external TSkiaAPI.LibName name 'sk4d_pixmap_get_alpha_type';
    class function  TSkiaAPI.sk4d_pixmap_get_color;       external TSkiaAPI.LibName name 'sk4d_pixmap_get_color';
    class function  TSkiaAPI.sk4d_pixmap_get_color_space; external TSkiaAPI.LibName name 'sk4d_pixmap_get_color_space';
    class function  TSkiaAPI.sk4d_pixmap_get_color_type;  external TSkiaAPI.LibName name 'sk4d_pixmap_get_color_type';
    class function  TSkiaAPI.sk4d_pixmap_get_height;      external TSkiaAPI.LibName name 'sk4d_pixmap_get_height';
    class procedure TSkiaAPI.sk4d_pixmap_get_image_info;  external TSkiaAPI.LibName name 'sk4d_pixmap_get_image_info';
    class function  TSkiaAPI.sk4d_pixmap_get_pixel_addr;  external TSkiaAPI.LibName name 'sk4d_pixmap_get_pixel_addr';
    class function  TSkiaAPI.sk4d_pixmap_get_pixels;      external TSkiaAPI.LibName name 'sk4d_pixmap_get_pixels';
    class function  TSkiaAPI.sk4d_pixmap_get_row_bytes;   external TSkiaAPI.LibName name 'sk4d_pixmap_get_row_bytes';
    class function  TSkiaAPI.sk4d_pixmap_get_width;       external TSkiaAPI.LibName name 'sk4d_pixmap_get_width';
    class function  TSkiaAPI.sk4d_pixmap_read_pixels;     external TSkiaAPI.LibName name 'sk4d_pixmap_read_pixels';
    class function  TSkiaAPI.sk4d_pixmap_scale_pixels;    external TSkiaAPI.LibName name 'sk4d_pixmap_scale_pixels';
    class procedure TSkiaAPI.sk4d_pixmap_set_colorspace;  external TSkiaAPI.LibName name 'sk4d_pixmap_set_colorspace';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_refcnt.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_refcnt_ref   := GetProcAddress(FLibHandle, 'sk4d_refcnt_ref');
    sk4d_refcnt_unref := GetProcAddress(FLibHandle, 'sk4d_refcnt_unref');
    {$ELSE}
    class procedure TSkiaAPI.sk4d_refcnt_ref;   external TSkiaAPI.LibName name 'sk4d_refcnt_ref';
    class procedure TSkiaAPI.sk4d_refcnt_unref; external TSkiaAPI.LibName name 'sk4d_refcnt_unref';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_region.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_region_contains              := GetProcAddress(FLibHandle, 'sk4d_region_contains');
    sk4d_region_contains2             := GetProcAddress(FLibHandle, 'sk4d_region_contains2');
    sk4d_region_contains3             := GetProcAddress(FLibHandle, 'sk4d_region_contains3');
    sk4d_region_create                := GetProcAddress(FLibHandle, 'sk4d_region_create');
    sk4d_region_create2               := GetProcAddress(FLibHandle, 'sk4d_region_create2');
    sk4d_region_destroy               := GetProcAddress(FLibHandle, 'sk4d_region_destroy');
    sk4d_region_get_boundary_path     := GetProcAddress(FLibHandle, 'sk4d_region_get_boundary_path');
    sk4d_region_get_bounds            := GetProcAddress(FLibHandle, 'sk4d_region_get_bounds');
    sk4d_region_intersects            := GetProcAddress(FLibHandle, 'sk4d_region_intersects');
    sk4d_region_intersects2           := GetProcAddress(FLibHandle, 'sk4d_region_intersects2');
    sk4d_region_is_complex            := GetProcAddress(FLibHandle, 'sk4d_region_is_complex');
    sk4d_region_is_empty              := GetProcAddress(FLibHandle, 'sk4d_region_is_empty');
    sk4d_region_is_equal              := GetProcAddress(FLibHandle, 'sk4d_region_is_equal');
    sk4d_region_is_rect               := GetProcAddress(FLibHandle, 'sk4d_region_is_rect');
    sk4d_region_op                    := GetProcAddress(FLibHandle, 'sk4d_region_op');
    sk4d_region_op2                   := GetProcAddress(FLibHandle, 'sk4d_region_op2');
    sk4d_region_quick_contains        := GetProcAddress(FLibHandle, 'sk4d_region_quick_contains');
    sk4d_region_quick_reject          := GetProcAddress(FLibHandle, 'sk4d_region_quick_reject');
    sk4d_region_quick_reject2         := GetProcAddress(FLibHandle, 'sk4d_region_quick_reject2');
    sk4d_region_set_empty             := GetProcAddress(FLibHandle, 'sk4d_region_set_empty');
    sk4d_region_set_path              := GetProcAddress(FLibHandle, 'sk4d_region_set_path');
    sk4d_region_set_rect              := GetProcAddress(FLibHandle, 'sk4d_region_set_rect');
    sk4d_region_set_rects             := GetProcAddress(FLibHandle, 'sk4d_region_set_rects');
    sk4d_region_translate             := GetProcAddress(FLibHandle, 'sk4d_region_translate');
    sk4d_regioncliperator_create      := GetProcAddress(FLibHandle, 'sk4d_regioncliperator_create');
    sk4d_regioncliperator_destroy     := GetProcAddress(FLibHandle, 'sk4d_regioncliperator_destroy');
    sk4d_regioncliperator_get_current := GetProcAddress(FLibHandle, 'sk4d_regioncliperator_get_current');
    sk4d_regioncliperator_move_next   := GetProcAddress(FLibHandle, 'sk4d_regioncliperator_move_next');
    sk4d_regioniterator_create        := GetProcAddress(FLibHandle, 'sk4d_regioniterator_create');
    sk4d_regioniterator_destroy       := GetProcAddress(FLibHandle, 'sk4d_regioniterator_destroy');
    sk4d_regioniterator_get_current   := GetProcAddress(FLibHandle, 'sk4d_regioniterator_get_current');
    sk4d_regioniterator_move_next     := GetProcAddress(FLibHandle, 'sk4d_regioniterator_move_next');
    sk4d_regioniterator_reset         := GetProcAddress(FLibHandle, 'sk4d_regioniterator_reset');
    sk4d_regionspanerator_create      := GetProcAddress(FLibHandle, 'sk4d_regionspanerator_create');
    sk4d_regionspanerator_destroy     := GetProcAddress(FLibHandle, 'sk4d_regionspanerator_destroy');
    sk4d_regionspanerator_next        := GetProcAddress(FLibHandle, 'sk4d_regionspanerator_next');
    {$ELSE}
    class function  TSkiaAPI.sk4d_region_contains;              external TSkiaAPI.LibName name 'sk4d_region_contains';
    class function  TSkiaAPI.sk4d_region_contains2;             external TSkiaAPI.LibName name 'sk4d_region_contains2';
    class function  TSkiaAPI.sk4d_region_contains3;             external TSkiaAPI.LibName name 'sk4d_region_contains3';
    class function  TSkiaAPI.sk4d_region_create;                external TSkiaAPI.LibName name 'sk4d_region_create';
    class function  TSkiaAPI.sk4d_region_create2;               external TSkiaAPI.LibName name 'sk4d_region_create2';
    class procedure TSkiaAPI.sk4d_region_destroy;               external TSkiaAPI.LibName name 'sk4d_region_destroy';
    class function  TSkiaAPI.sk4d_region_get_boundary_path;     external TSkiaAPI.LibName name 'sk4d_region_get_boundary_path';
    class procedure TSkiaAPI.sk4d_region_get_bounds;            external TSkiaAPI.LibName name 'sk4d_region_get_bounds';
    class function  TSkiaAPI.sk4d_region_intersects;            external TSkiaAPI.LibName name 'sk4d_region_intersects';
    class function  TSkiaAPI.sk4d_region_intersects2;           external TSkiaAPI.LibName name 'sk4d_region_intersects2';
    class function  TSkiaAPI.sk4d_region_is_complex;            external TSkiaAPI.LibName name 'sk4d_region_is_complex';
    class function  TSkiaAPI.sk4d_region_is_empty;              external TSkiaAPI.LibName name 'sk4d_region_is_empty';
    class function  TSkiaAPI.sk4d_region_is_equal;              external TSkiaAPI.LibName name 'sk4d_region_is_equal';
    class function  TSkiaAPI.sk4d_region_is_rect;               external TSkiaAPI.LibName name 'sk4d_region_is_rect';
    class function  TSkiaAPI.sk4d_region_op;                    external TSkiaAPI.LibName name 'sk4d_region_op';
    class function  TSkiaAPI.sk4d_region_op2;                   external TSkiaAPI.LibName name 'sk4d_region_op2';
    class function  TSkiaAPI.sk4d_region_quick_contains;        external TSkiaAPI.LibName name 'sk4d_region_quick_contains';
    class function  TSkiaAPI.sk4d_region_quick_reject;          external TSkiaAPI.LibName name 'sk4d_region_quick_reject';
    class function  TSkiaAPI.sk4d_region_quick_reject2;         external TSkiaAPI.LibName name 'sk4d_region_quick_reject2';
    class procedure TSkiaAPI.sk4d_region_set_empty;             external TSkiaAPI.LibName name 'sk4d_region_set_empty';
    class function  TSkiaAPI.sk4d_region_set_path;              external TSkiaAPI.LibName name 'sk4d_region_set_path';
    class function  TSkiaAPI.sk4d_region_set_rect;              external TSkiaAPI.LibName name 'sk4d_region_set_rect';
    class function  TSkiaAPI.sk4d_region_set_rects;             external TSkiaAPI.LibName name 'sk4d_region_set_rects';
    class procedure TSkiaAPI.sk4d_region_translate;             external TSkiaAPI.LibName name 'sk4d_region_translate';
    class function  TSkiaAPI.sk4d_regioncliperator_create;      external TSkiaAPI.LibName name 'sk4d_regioncliperator_create';
    class procedure TSkiaAPI.sk4d_regioncliperator_destroy;     external TSkiaAPI.LibName name 'sk4d_regioncliperator_destroy';
    class procedure TSkiaAPI.sk4d_regioncliperator_get_current; external TSkiaAPI.LibName name 'sk4d_regioncliperator_get_current';
    class function  TSkiaAPI.sk4d_regioncliperator_move_next;   external TSkiaAPI.LibName name 'sk4d_regioncliperator_move_next';
    class function  TSkiaAPI.sk4d_regioniterator_create;        external TSkiaAPI.LibName name 'sk4d_regioniterator_create';
    class procedure TSkiaAPI.sk4d_regioniterator_destroy;       external TSkiaAPI.LibName name 'sk4d_regioniterator_destroy';
    class procedure TSkiaAPI.sk4d_regioniterator_get_current;   external TSkiaAPI.LibName name 'sk4d_regioniterator_get_current';
    class function  TSkiaAPI.sk4d_regioniterator_move_next;     external TSkiaAPI.LibName name 'sk4d_regioniterator_move_next';
    class procedure TSkiaAPI.sk4d_regioniterator_reset;         external TSkiaAPI.LibName name 'sk4d_regioniterator_reset';
    class function  TSkiaAPI.sk4d_regionspanerator_create;      external TSkiaAPI.LibName name 'sk4d_regionspanerator_create';
    class procedure TSkiaAPI.sk4d_regionspanerator_destroy;     external TSkiaAPI.LibName name 'sk4d_regionspanerator_destroy';
    class function  TSkiaAPI.sk4d_regionspanerator_next;        external TSkiaAPI.LibName name 'sk4d_regionspanerator_next';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_rrect.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_rrect_contains         := GetProcAddress(FLibHandle, 'sk4d_rrect_contains');
    sk4d_rrect_create           := GetProcAddress(FLibHandle, 'sk4d_rrect_create');
    sk4d_rrect_create2          := GetProcAddress(FLibHandle, 'sk4d_rrect_create2');
    sk4d_rrect_deflate          := GetProcAddress(FLibHandle, 'sk4d_rrect_deflate');
    sk4d_rrect_destroy          := GetProcAddress(FLibHandle, 'sk4d_rrect_destroy');
    sk4d_rrect_get_height       := GetProcAddress(FLibHandle, 'sk4d_rrect_get_height');
    sk4d_rrect_get_radii        := GetProcAddress(FLibHandle, 'sk4d_rrect_get_radii');
    sk4d_rrect_get_rect         := GetProcAddress(FLibHandle, 'sk4d_rrect_get_rect');
    sk4d_rrect_get_simple_radii := GetProcAddress(FLibHandle, 'sk4d_rrect_get_simple_radii');
    sk4d_rrect_get_width        := GetProcAddress(FLibHandle, 'sk4d_rrect_get_width');
    sk4d_rrect_inflate          := GetProcAddress(FLibHandle, 'sk4d_rrect_inflate');
    sk4d_rrect_is_complex       := GetProcAddress(FLibHandle, 'sk4d_rrect_is_complex');
    sk4d_rrect_is_empty         := GetProcAddress(FLibHandle, 'sk4d_rrect_is_empty');
    sk4d_rrect_is_equal         := GetProcAddress(FLibHandle, 'sk4d_rrect_is_equal');
    sk4d_rrect_is_nine_patch    := GetProcAddress(FLibHandle, 'sk4d_rrect_is_nine_patch');
    sk4d_rrect_is_oval          := GetProcAddress(FLibHandle, 'sk4d_rrect_is_oval');
    sk4d_rrect_is_rect          := GetProcAddress(FLibHandle, 'sk4d_rrect_is_rect');
    sk4d_rrect_is_simple        := GetProcAddress(FLibHandle, 'sk4d_rrect_is_simple');
    sk4d_rrect_is_valid         := GetProcAddress(FLibHandle, 'sk4d_rrect_is_valid');
    sk4d_rrect_offset           := GetProcAddress(FLibHandle, 'sk4d_rrect_offset');
    sk4d_rrect_set_empty        := GetProcAddress(FLibHandle, 'sk4d_rrect_set_empty');
    sk4d_rrect_set_nine_patch   := GetProcAddress(FLibHandle, 'sk4d_rrect_set_nine_patch');
    sk4d_rrect_set_oval         := GetProcAddress(FLibHandle, 'sk4d_rrect_set_oval');
    sk4d_rrect_set_rect         := GetProcAddress(FLibHandle, 'sk4d_rrect_set_rect');
    sk4d_rrect_set_rect2        := GetProcAddress(FLibHandle, 'sk4d_rrect_set_rect2');
    sk4d_rrect_set_rect3        := GetProcAddress(FLibHandle, 'sk4d_rrect_set_rect3');
    sk4d_rrect_transform        := GetProcAddress(FLibHandle, 'sk4d_rrect_transform');
    {$ELSE}
    class function  TSkiaAPI.sk4d_rrect_contains;         external TSkiaAPI.LibName name 'sk4d_rrect_contains';
    class function  TSkiaAPI.sk4d_rrect_create;           external TSkiaAPI.LibName name 'sk4d_rrect_create';
    class function  TSkiaAPI.sk4d_rrect_create2;          external TSkiaAPI.LibName name 'sk4d_rrect_create2';
    class procedure TSkiaAPI.sk4d_rrect_deflate;          external TSkiaAPI.LibName name 'sk4d_rrect_deflate';
    class procedure TSkiaAPI.sk4d_rrect_destroy;          external TSkiaAPI.LibName name 'sk4d_rrect_destroy';
    class function  TSkiaAPI.sk4d_rrect_get_height;       external TSkiaAPI.LibName name 'sk4d_rrect_get_height';
    class procedure TSkiaAPI.sk4d_rrect_get_radii;        external TSkiaAPI.LibName name 'sk4d_rrect_get_radii';
    class procedure TSkiaAPI.sk4d_rrect_get_rect;         external TSkiaAPI.LibName name 'sk4d_rrect_get_rect';
    class procedure TSkiaAPI.sk4d_rrect_get_simple_radii; external TSkiaAPI.LibName name 'sk4d_rrect_get_simple_radii';
    class function  TSkiaAPI.sk4d_rrect_get_width;        external TSkiaAPI.LibName name 'sk4d_rrect_get_width';
    class procedure TSkiaAPI.sk4d_rrect_inflate;          external TSkiaAPI.LibName name 'sk4d_rrect_inflate';
    class function  TSkiaAPI.sk4d_rrect_is_complex;       external TSkiaAPI.LibName name 'sk4d_rrect_is_complex';
    class function  TSkiaAPI.sk4d_rrect_is_empty;         external TSkiaAPI.LibName name 'sk4d_rrect_is_empty';
    class function  TSkiaAPI.sk4d_rrect_is_equal;         external TSkiaAPI.LibName name 'sk4d_rrect_is_equal';
    class function  TSkiaAPI.sk4d_rrect_is_nine_patch;    external TSkiaAPI.LibName name 'sk4d_rrect_is_nine_patch';
    class function  TSkiaAPI.sk4d_rrect_is_oval;          external TSkiaAPI.LibName name 'sk4d_rrect_is_oval';
    class function  TSkiaAPI.sk4d_rrect_is_rect;          external TSkiaAPI.LibName name 'sk4d_rrect_is_rect';
    class function  TSkiaAPI.sk4d_rrect_is_simple;        external TSkiaAPI.LibName name 'sk4d_rrect_is_simple';
    class function  TSkiaAPI.sk4d_rrect_is_valid;         external TSkiaAPI.LibName name 'sk4d_rrect_is_valid';
    class procedure TSkiaAPI.sk4d_rrect_offset;           external TSkiaAPI.LibName name 'sk4d_rrect_offset';
    class procedure TSkiaAPI.sk4d_rrect_set_empty;        external TSkiaAPI.LibName name 'sk4d_rrect_set_empty';
    class procedure TSkiaAPI.sk4d_rrect_set_nine_patch;   external TSkiaAPI.LibName name 'sk4d_rrect_set_nine_patch';
    class procedure TSkiaAPI.sk4d_rrect_set_oval;         external TSkiaAPI.LibName name 'sk4d_rrect_set_oval';
    class procedure TSkiaAPI.sk4d_rrect_set_rect;         external TSkiaAPI.LibName name 'sk4d_rrect_set_rect';
    class procedure TSkiaAPI.sk4d_rrect_set_rect2;        external TSkiaAPI.LibName name 'sk4d_rrect_set_rect2';
    class procedure TSkiaAPI.sk4d_rrect_set_rect3;        external TSkiaAPI.LibName name 'sk4d_rrect_set_rect3';
    class function  TSkiaAPI.sk4d_rrect_transform;        external TSkiaAPI.LibName name 'sk4d_rrect_transform';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_runtimeeffect.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_runtimeeffect_get_child_count        := GetProcAddress(FLibHandle, 'sk4d_runtimeeffect_get_child_count');
    sk4d_runtimeeffect_get_child_name         := GetProcAddress(FLibHandle, 'sk4d_runtimeeffect_get_child_name');
    sk4d_runtimeeffect_get_child_type         := GetProcAddress(FLibHandle, 'sk4d_runtimeeffect_get_child_type');
    sk4d_runtimeeffect_get_uniform_count      := GetProcAddress(FLibHandle, 'sk4d_runtimeeffect_get_uniform_count');
    sk4d_runtimeeffect_get_uniform_data_size  := GetProcAddress(FLibHandle, 'sk4d_runtimeeffect_get_uniform_data_size');
    sk4d_runtimeeffect_get_uniform_name       := GetProcAddress(FLibHandle, 'sk4d_runtimeeffect_get_uniform_name');
    sk4d_runtimeeffect_get_uniform_offset     := GetProcAddress(FLibHandle, 'sk4d_runtimeeffect_get_uniform_offset');
    sk4d_runtimeeffect_get_uniform_type       := GetProcAddress(FLibHandle, 'sk4d_runtimeeffect_get_uniform_type');
    sk4d_runtimeeffect_get_uniform_type_count := GetProcAddress(FLibHandle, 'sk4d_runtimeeffect_get_uniform_type_count');
    sk4d_runtimeeffect_index_of_child         := GetProcAddress(FLibHandle, 'sk4d_runtimeeffect_index_of_child');
    sk4d_runtimeeffect_index_of_uniform       := GetProcAddress(FLibHandle, 'sk4d_runtimeeffect_index_of_uniform');
    sk4d_runtimeeffect_make_blender           := GetProcAddress(FLibHandle, 'sk4d_runtimeeffect_make_blender');
    sk4d_runtimeeffect_make_color_filter      := GetProcAddress(FLibHandle, 'sk4d_runtimeeffect_make_color_filter');
    sk4d_runtimeeffect_make_for_blender       := GetProcAddress(FLibHandle, 'sk4d_runtimeeffect_make_for_blender');
    sk4d_runtimeeffect_make_for_color_filter  := GetProcAddress(FLibHandle, 'sk4d_runtimeeffect_make_for_color_filter');
    sk4d_runtimeeffect_make_for_shader        := GetProcAddress(FLibHandle, 'sk4d_runtimeeffect_make_for_shader');
    sk4d_runtimeeffect_make_shader            := GetProcAddress(FLibHandle, 'sk4d_runtimeeffect_make_shader');
    {$ELSE}
    class function TSkiaAPI.sk4d_runtimeeffect_get_child_count;        external TSkiaAPI.LibName name 'sk4d_runtimeeffect_get_child_count';
    class function TSkiaAPI.sk4d_runtimeeffect_get_child_name;         external TSkiaAPI.LibName name 'sk4d_runtimeeffect_get_child_name';
    class function TSkiaAPI.sk4d_runtimeeffect_get_child_type;         external TSkiaAPI.LibName name 'sk4d_runtimeeffect_get_child_type';
    class function TSkiaAPI.sk4d_runtimeeffect_get_uniform_count;      external TSkiaAPI.LibName name 'sk4d_runtimeeffect_get_uniform_count';
    class function TSkiaAPI.sk4d_runtimeeffect_get_uniform_data_size;  external TSkiaAPI.LibName name 'sk4d_runtimeeffect_get_uniform_data_size';
    class function TSkiaAPI.sk4d_runtimeeffect_get_uniform_name;       external TSkiaAPI.LibName name 'sk4d_runtimeeffect_get_uniform_name';
    class function TSkiaAPI.sk4d_runtimeeffect_get_uniform_offset;     external TSkiaAPI.LibName name 'sk4d_runtimeeffect_get_uniform_offset';
    class function TSkiaAPI.sk4d_runtimeeffect_get_uniform_type;       external TSkiaAPI.LibName name 'sk4d_runtimeeffect_get_uniform_type';
    class function TSkiaAPI.sk4d_runtimeeffect_get_uniform_type_count; external TSkiaAPI.LibName name 'sk4d_runtimeeffect_get_uniform_type_count';
    class function TSkiaAPI.sk4d_runtimeeffect_index_of_child;         external TSkiaAPI.LibName name 'sk4d_runtimeeffect_index_of_child';
    class function TSkiaAPI.sk4d_runtimeeffect_index_of_uniform;       external TSkiaAPI.LibName name 'sk4d_runtimeeffect_index_of_uniform';
    class function TSkiaAPI.sk4d_runtimeeffect_make_blender;           external TSkiaAPI.LibName name 'sk4d_runtimeeffect_make_blender';
    class function TSkiaAPI.sk4d_runtimeeffect_make_color_filter;      external TSkiaAPI.LibName name 'sk4d_runtimeeffect_make_color_filter';
    class function TSkiaAPI.sk4d_runtimeeffect_make_for_blender;       external TSkiaAPI.LibName name 'sk4d_runtimeeffect_make_for_blender';
    class function TSkiaAPI.sk4d_runtimeeffect_make_for_color_filter;  external TSkiaAPI.LibName name 'sk4d_runtimeeffect_make_for_color_filter';
    class function TSkiaAPI.sk4d_runtimeeffect_make_for_shader;        external TSkiaAPI.LibName name 'sk4d_runtimeeffect_make_for_shader';
    class function TSkiaAPI.sk4d_runtimeeffect_make_shader;            external TSkiaAPI.LibName name 'sk4d_runtimeeffect_make_shader';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_shader.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_shader_make_blend                       := GetProcAddress(FLibHandle, 'sk4d_shader_make_blend');
    sk4d_shader_make_color                       := GetProcAddress(FLibHandle, 'sk4d_shader_make_color');
    sk4d_shader_make_color2                      := GetProcAddress(FLibHandle, 'sk4d_shader_make_color2');
    sk4d_shader_make_gradient_linear             := GetProcAddress(FLibHandle, 'sk4d_shader_make_gradient_linear');
    sk4d_shader_make_gradient_linear2            := GetProcAddress(FLibHandle, 'sk4d_shader_make_gradient_linear2');
    sk4d_shader_make_gradient_radial             := GetProcAddress(FLibHandle, 'sk4d_shader_make_gradient_radial');
    sk4d_shader_make_gradient_radial2            := GetProcAddress(FLibHandle, 'sk4d_shader_make_gradient_radial2');
    sk4d_shader_make_gradient_sweep              := GetProcAddress(FLibHandle, 'sk4d_shader_make_gradient_sweep');
    sk4d_shader_make_gradient_sweep2             := GetProcAddress(FLibHandle, 'sk4d_shader_make_gradient_sweep2');
    sk4d_shader_make_gradient_two_point_conical  := GetProcAddress(FLibHandle, 'sk4d_shader_make_gradient_two_point_conical');
    sk4d_shader_make_gradient_two_point_conical2 := GetProcAddress(FLibHandle, 'sk4d_shader_make_gradient_two_point_conical2');
    sk4d_shader_make_perlin_noise_fractal_noise  := GetProcAddress(FLibHandle, 'sk4d_shader_make_perlin_noise_fractal_noise');
    sk4d_shader_make_perlin_noise_turbulence     := GetProcAddress(FLibHandle, 'sk4d_shader_make_perlin_noise_turbulence');
    sk4d_shader_make_with_color_filter           := GetProcAddress(FLibHandle, 'sk4d_shader_make_with_color_filter');
    sk4d_shader_make_with_local_matrix           := GetProcAddress(FLibHandle, 'sk4d_shader_make_with_local_matrix');
    {$ELSE}
    class function TSkiaAPI.sk4d_shader_make_blend;                       external TSkiaAPI.LibName name 'sk4d_shader_make_blend';
    class function TSkiaAPI.sk4d_shader_make_color;                       external TSkiaAPI.LibName name 'sk4d_shader_make_color';
    class function TSkiaAPI.sk4d_shader_make_color2;                      external TSkiaAPI.LibName name 'sk4d_shader_make_color2';
    class function TSkiaAPI.sk4d_shader_make_gradient_linear;             external TSkiaAPI.LibName name 'sk4d_shader_make_gradient_linear';
    class function TSkiaAPI.sk4d_shader_make_gradient_linear2;            external TSkiaAPI.LibName name 'sk4d_shader_make_gradient_linear2';
    class function TSkiaAPI.sk4d_shader_make_gradient_radial;             external TSkiaAPI.LibName name 'sk4d_shader_make_gradient_radial';
    class function TSkiaAPI.sk4d_shader_make_gradient_radial2;            external TSkiaAPI.LibName name 'sk4d_shader_make_gradient_radial2';
    class function TSkiaAPI.sk4d_shader_make_gradient_sweep;              external TSkiaAPI.LibName name 'sk4d_shader_make_gradient_sweep';
    class function TSkiaAPI.sk4d_shader_make_gradient_sweep2;             external TSkiaAPI.LibName name 'sk4d_shader_make_gradient_sweep2';
    class function TSkiaAPI.sk4d_shader_make_gradient_two_point_conical;  external TSkiaAPI.LibName name 'sk4d_shader_make_gradient_two_point_conical';
    class function TSkiaAPI.sk4d_shader_make_gradient_two_point_conical2; external TSkiaAPI.LibName name 'sk4d_shader_make_gradient_two_point_conical2';
    class function TSkiaAPI.sk4d_shader_make_perlin_noise_fractal_noise;  external TSkiaAPI.LibName name 'sk4d_shader_make_perlin_noise_fractal_noise';
    class function TSkiaAPI.sk4d_shader_make_perlin_noise_turbulence;     external TSkiaAPI.LibName name 'sk4d_shader_make_perlin_noise_turbulence';
    class function TSkiaAPI.sk4d_shader_make_with_color_filter;           external TSkiaAPI.LibName name 'sk4d_shader_make_with_color_filter';
    class function TSkiaAPI.sk4d_shader_make_with_local_matrix;           external TSkiaAPI.LibName name 'sk4d_shader_make_with_local_matrix';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_stream.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_streamadapter_create     := GetProcAddress(FLibHandle, 'sk4d_streamadapter_create');
    sk4d_streamadapter_destroy    := GetProcAddress(FLibHandle, 'sk4d_streamadapter_destroy');
    sk4d_streamadapter_set_procs  := GetProcAddress(FLibHandle, 'sk4d_streamadapter_set_procs');
    sk4d_wstreamadapter_create    := GetProcAddress(FLibHandle, 'sk4d_wstreamadapter_create');
    sk4d_wstreamadapter_destroy   := GetProcAddress(FLibHandle, 'sk4d_wstreamadapter_destroy');
    sk4d_wstreamadapter_set_procs := GetProcAddress(FLibHandle, 'sk4d_wstreamadapter_set_procs');
    {$ELSE}
    class function  TSkiaAPI.sk4d_streamadapter_create;     external TSkiaAPI.LibName name 'sk4d_streamadapter_create';
    class procedure TSkiaAPI.sk4d_streamadapter_destroy;    external TSkiaAPI.LibName name 'sk4d_streamadapter_destroy';
    class procedure TSkiaAPI.sk4d_streamadapter_set_procs;  external TSkiaAPI.LibName name 'sk4d_streamadapter_set_procs';
    class function  TSkiaAPI.sk4d_wstreamadapter_create;    external TSkiaAPI.LibName name 'sk4d_wstreamadapter_create';
    class procedure TSkiaAPI.sk4d_wstreamadapter_destroy;   external TSkiaAPI.LibName name 'sk4d_wstreamadapter_destroy';
    class procedure TSkiaAPI.sk4d_wstreamadapter_set_procs; external TSkiaAPI.LibName name 'sk4d_wstreamadapter_set_procs';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_string.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_string_create   := GetProcAddress(FLibHandle, 'sk4d_string_create');
    sk4d_string_destroy  := GetProcAddress(FLibHandle, 'sk4d_string_destroy');
    sk4d_string_get_text := GetProcAddress(FLibHandle, 'sk4d_string_get_text');
    sk4d_string_set_text := GetProcAddress(FLibHandle, 'sk4d_string_set_text');
    {$ELSE}
    class function  TSkiaAPI.sk4d_string_create;   external TSkiaAPI.LibName name 'sk4d_string_create';
    class procedure TSkiaAPI.sk4d_string_destroy;  external TSkiaAPI.LibName name 'sk4d_string_destroy';
    class function  TSkiaAPI.sk4d_string_get_text; external TSkiaAPI.LibName name 'sk4d_string_get_text';
    class procedure TSkiaAPI.sk4d_string_set_text; external TSkiaAPI.LibName name 'sk4d_string_set_text';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_surface.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_surface_draw                     := GetProcAddress(FLibHandle, 'sk4d_surface_draw');
    sk4d_surface_flush                    := GetProcAddress(FLibHandle, 'sk4d_surface_flush');
    sk4d_surface_flush_and_submit         := GetProcAddress(FLibHandle, 'sk4d_surface_flush_and_submit');
    sk4d_surface_get_canvas               := GetProcAddress(FLibHandle, 'sk4d_surface_get_canvas');
    sk4d_surface_get_props                := GetProcAddress(FLibHandle, 'sk4d_surface_get_props');
    sk4d_surface_make_from_ca_metal_layer := GetProcAddress(FLibHandle, 'sk4d_surface_make_from_ca_metal_layer');
    sk4d_surface_make_from_mtk_view       := GetProcAddress(FLibHandle, 'sk4d_surface_make_from_mtk_view');
    sk4d_surface_make_from_render_target  := GetProcAddress(FLibHandle, 'sk4d_surface_make_from_render_target');
    sk4d_surface_make_from_texture        := GetProcAddress(FLibHandle, 'sk4d_surface_make_from_texture');
    sk4d_surface_make_image_snapshot      := GetProcAddress(FLibHandle, 'sk4d_surface_make_image_snapshot');
    sk4d_surface_make_image_snapshot2     := GetProcAddress(FLibHandle, 'sk4d_surface_make_image_snapshot2');
    sk4d_surface_make_raster              := GetProcAddress(FLibHandle, 'sk4d_surface_make_raster');
    sk4d_surface_make_raster_direct       := GetProcAddress(FLibHandle, 'sk4d_surface_make_raster_direct');
    sk4d_surface_make_render_target       := GetProcAddress(FLibHandle, 'sk4d_surface_make_render_target');
    sk4d_surface_peek_pixels              := GetProcAddress(FLibHandle, 'sk4d_surface_peek_pixels');
    sk4d_surface_read_pixels              := GetProcAddress(FLibHandle, 'sk4d_surface_read_pixels');
    {$ELSE}
    class procedure TSkiaAPI.sk4d_surface_draw;                     external TSkiaAPI.LibName name 'sk4d_surface_draw';
    class procedure TSkiaAPI.sk4d_surface_flush;                    external TSkiaAPI.LibName name 'sk4d_surface_flush';
    class procedure TSkiaAPI.sk4d_surface_flush_and_submit;         external TSkiaAPI.LibName name 'sk4d_surface_flush_and_submit';
    class function  TSkiaAPI.sk4d_surface_get_canvas;               external TSkiaAPI.LibName name 'sk4d_surface_get_canvas';
    class procedure TSkiaAPI.sk4d_surface_get_props;                external TSkiaAPI.LibName name 'sk4d_surface_get_props';
    class function  TSkiaAPI.sk4d_surface_make_from_ca_metal_layer; external TSkiaAPI.LibName name 'sk4d_surface_make_from_ca_metal_layer';
    class function  TSkiaAPI.sk4d_surface_make_from_mtk_view;       external TSkiaAPI.LibName name 'sk4d_surface_make_from_mtk_view';
    class function  TSkiaAPI.sk4d_surface_make_from_render_target;  external TSkiaAPI.LibName name 'sk4d_surface_make_from_render_target';
    class function  TSkiaAPI.sk4d_surface_make_from_texture;        external TSkiaAPI.LibName name 'sk4d_surface_make_from_texture';
    class function  TSkiaAPI.sk4d_surface_make_image_snapshot;      external TSkiaAPI.LibName name 'sk4d_surface_make_image_snapshot';
    class function  TSkiaAPI.sk4d_surface_make_image_snapshot2;     external TSkiaAPI.LibName name 'sk4d_surface_make_image_snapshot2';
    class function  TSkiaAPI.sk4d_surface_make_raster;              external TSkiaAPI.LibName name 'sk4d_surface_make_raster';
    class function  TSkiaAPI.sk4d_surface_make_raster_direct;       external TSkiaAPI.LibName name 'sk4d_surface_make_raster_direct';
    class function  TSkiaAPI.sk4d_surface_make_render_target;       external TSkiaAPI.LibName name 'sk4d_surface_make_render_target';
    class function  TSkiaAPI.sk4d_surface_peek_pixels;              external TSkiaAPI.LibName name 'sk4d_surface_peek_pixels';
    class function  TSkiaAPI.sk4d_surface_read_pixels;              external TSkiaAPI.LibName name 'sk4d_surface_read_pixels';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_svgcanvas.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_svgcanvas_make := GetProcAddress(FLibHandle, 'sk4d_svgcanvas_make');
    {$ELSE}
    class function TSkiaAPI.sk4d_svgcanvas_make; external TSkiaAPI.LibName name 'sk4d_svgcanvas_make';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_textblob.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_textblob_get_intercepts                         := GetProcAddress(FLibHandle, 'sk4d_textblob_get_intercepts');
    sk4d_textblob_make_from_text                         := GetProcAddress(FLibHandle, 'sk4d_textblob_make_from_text');
    sk4d_textblob_make_from_text_horizontally_positioned := GetProcAddress(FLibHandle, 'sk4d_textblob_make_from_text_horizontally_positioned');
    sk4d_textblob_make_from_text_positioned              := GetProcAddress(FLibHandle, 'sk4d_textblob_make_from_text_positioned');
    sk4d_textblob_make_from_text_transform               := GetProcAddress(FLibHandle, 'sk4d_textblob_make_from_text_transform');
    sk4d_textblob_ref                                    := GetProcAddress(FLibHandle, 'sk4d_textblob_ref');
    sk4d_textblob_unref                                  := GetProcAddress(FLibHandle, 'sk4d_textblob_unref');
    {$ELSE}
    class function  TSkiaAPI.sk4d_textblob_get_intercepts;                         external TSkiaAPI.LibName name 'sk4d_textblob_get_intercepts';
    class function  TSkiaAPI.sk4d_textblob_make_from_text;                         external TSkiaAPI.LibName name 'sk4d_textblob_make_from_text';
    class function  TSkiaAPI.sk4d_textblob_make_from_text_horizontally_positioned; external TSkiaAPI.LibName name 'sk4d_textblob_make_from_text_horizontally_positioned';
    class function  TSkiaAPI.sk4d_textblob_make_from_text_positioned;              external TSkiaAPI.LibName name 'sk4d_textblob_make_from_text_positioned';
    class function  TSkiaAPI.sk4d_textblob_make_from_text_transform;               external TSkiaAPI.LibName name 'sk4d_textblob_make_from_text_transform';
    class procedure TSkiaAPI.sk4d_textblob_ref;                                    external TSkiaAPI.LibName name 'sk4d_textblob_ref';
    class procedure TSkiaAPI.sk4d_textblob_unref;                                  external TSkiaAPI.LibName name 'sk4d_textblob_unref';

    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_tracememorydump.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_tracememorydumpbaseclass_create    := GetProcAddress(FLibHandle, 'sk4d_tracememorydumpbaseclass_create');
    sk4d_tracememorydumpbaseclass_destroy   := GetProcAddress(FLibHandle, 'sk4d_tracememorydumpbaseclass_destroy');
    sk4d_tracememorydumpbaseclass_set_procs := GetProcAddress(FLibHandle, 'sk4d_tracememorydumpbaseclass_set_procs');
    {$ELSE}
    class function  TSkiaAPI.sk4d_tracememorydumpbaseclass_create;    external TSkiaAPI.LibName name 'sk4d_tracememorydumpbaseclass_create';
    class procedure TSkiaAPI.sk4d_tracememorydumpbaseclass_destroy;   external TSkiaAPI.LibName name 'sk4d_tracememorydumpbaseclass_destroy';
    class procedure TSkiaAPI.sk4d_tracememorydumpbaseclass_set_procs; external TSkiaAPI.LibName name 'sk4d_tracememorydumpbaseclass_set_procs';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_typeface.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_typeface_get_family_name  := GetProcAddress(FLibHandle, 'sk4d_typeface_get_family_name');
    sk4d_typeface_get_slant        := GetProcAddress(FLibHandle, 'sk4d_typeface_get_slant');
    sk4d_typeface_get_style        := GetProcAddress(FLibHandle, 'sk4d_typeface_get_style');
    sk4d_typeface_get_weight       := GetProcAddress(FLibHandle, 'sk4d_typeface_get_weight');
    sk4d_typeface_get_width        := GetProcAddress(FLibHandle, 'sk4d_typeface_get_width');
    sk4d_typeface_make_default     := GetProcAddress(FLibHandle, 'sk4d_typeface_make_default');
    sk4d_typeface_make_from_file   := GetProcAddress(FLibHandle, 'sk4d_typeface_make_from_file');
    sk4d_typeface_make_from_stream := GetProcAddress(FLibHandle, 'sk4d_typeface_make_from_stream');
    sk4d_typeface_make_from_name   := GetProcAddress(FLibHandle, 'sk4d_typeface_make_from_name');
    {$ELSE}
    class function  TSkiaAPI.sk4d_typeface_get_family_name;  external TSkiaAPI.LibName name 'sk4d_typeface_get_family_name';
    class function  TSkiaAPI.sk4d_typeface_get_slant;        external TSkiaAPI.LibName name 'sk4d_typeface_get_slant';
    class procedure TSkiaAPI.sk4d_typeface_get_style;        external TSkiaAPI.LibName name 'sk4d_typeface_get_style';
    class function  TSkiaAPI.sk4d_typeface_get_weight;       external TSkiaAPI.LibName name 'sk4d_typeface_get_weight';
    class function  TSkiaAPI.sk4d_typeface_get_width;        external TSkiaAPI.LibName name 'sk4d_typeface_get_width';
    class function  TSkiaAPI.sk4d_typeface_make_default;     external TSkiaAPI.LibName name 'sk4d_typeface_make_default';
    class function  TSkiaAPI.sk4d_typeface_make_from_file;   external TSkiaAPI.LibName name 'sk4d_typeface_make_from_file';
    class function  TSkiaAPI.sk4d_typeface_make_from_stream; external TSkiaAPI.LibName name 'sk4d_typeface_make_from_stream';
    class function  TSkiaAPI.sk4d_typeface_make_from_name;   external TSkiaAPI.LibName name 'sk4d_typeface_make_from_name';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_version.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_version_get_build     := GetProcAddress(FLibHandle, 'sk4d_version_get_build');
    sk4d_version_get_major     := GetProcAddress(FLibHandle, 'sk4d_version_get_major');
    sk4d_version_get_milestone := GetProcAddress(FLibHandle, 'sk4d_version_get_milestone');
    sk4d_version_get_minor     := GetProcAddress(FLibHandle, 'sk4d_version_get_minor');
    {$ELSE}
    class function TSkiaAPI.sk4d_version_get_build;     external TSkiaAPI.LibName name 'sk4d_version_get_build';
    class function TSkiaAPI.sk4d_version_get_major;     external TSkiaAPI.LibName name 'sk4d_version_get_major';
    class function TSkiaAPI.sk4d_version_get_milestone; external TSkiaAPI.LibName name 'sk4d_version_get_milestone';
    class function TSkiaAPI.sk4d_version_get_minor;     external TSkiaAPI.LibName name 'sk4d_version_get_minor';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_vertices.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_vertices_make_copy := GetProcAddress(FLibHandle, 'sk4d_vertices_make_copy');
    sk4d_vertices_ref       := GetProcAddress(FLibHandle, 'sk4d_vertices_ref');
    sk4d_vertices_unref     := GetProcAddress(FLibHandle, 'sk4d_vertices_unref');
    {$ELSE}
    class function  TSkiaAPI.sk4d_vertices_make_copy; external TSkiaAPI.LibName name 'sk4d_vertices_make_copy';
    class procedure TSkiaAPI.sk4d_vertices_ref;       external TSkiaAPI.LibName name 'sk4d_vertices_ref';
    class procedure TSkiaAPI.sk4d_vertices_unref;     external TSkiaAPI.LibName name 'sk4d_vertices_unref';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/particles/include/sk4d_particleeffect.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_particleeffect_get_position           := GetProcAddress(FLibHandle, 'sk4d_particleeffect_get_position');
    sk4d_particleeffect_get_rate               := GetProcAddress(FLibHandle, 'sk4d_particleeffect_get_rate');
    sk4d_particleeffect_get_uniform            := GetProcAddress(FLibHandle, 'sk4d_particleeffect_get_uniform');
    sk4d_particleeffect_get_uniform_count      := GetProcAddress(FLibHandle, 'sk4d_particleeffect_get_uniform_count');
    sk4d_particleeffect_get_uniform_data       := GetProcAddress(FLibHandle, 'sk4d_particleeffect_get_uniform_data');
    sk4d_particleeffect_get_uniform_data_count := GetProcAddress(FLibHandle, 'sk4d_particleeffect_get_uniform_data_count');
    sk4d_particleeffect_get_uniform_name       := GetProcAddress(FLibHandle, 'sk4d_particleeffect_get_uniform_name');
    sk4d_particleeffect_init                   := GetProcAddress(FLibHandle, 'sk4d_particleeffect_init');
    sk4d_particleeffect_make_from_file         := GetProcAddress(FLibHandle, 'sk4d_particleeffect_make_from_file');
    sk4d_particleeffect_make_from_stream       := GetProcAddress(FLibHandle, 'sk4d_particleeffect_make_from_stream');
    sk4d_particleeffect_render                 := GetProcAddress(FLibHandle, 'sk4d_particleeffect_render');
    sk4d_particleeffect_set_position           := GetProcAddress(FLibHandle, 'sk4d_particleeffect_set_position');
    sk4d_particleeffect_set_rate               := GetProcAddress(FLibHandle, 'sk4d_particleeffect_set_rate');
    sk4d_particleeffect_set_uniform            := GetProcAddress(FLibHandle, 'sk4d_particleeffect_set_uniform');
    sk4d_particleeffect_start                  := GetProcAddress(FLibHandle, 'sk4d_particleeffect_start');
    sk4d_particleeffect_update                 := GetProcAddress(FLibHandle, 'sk4d_particleeffect_update');
    {$ELSE}
    class procedure TSkiaAPI.sk4d_particleeffect_get_position;           external TSkiaAPI.LibName name 'sk4d_particleeffect_get_position';
    class function  TSkiaAPI.sk4d_particleeffect_get_rate;               external TSkiaAPI.LibName name 'sk4d_particleeffect_get_rate';
    class procedure TSkiaAPI.sk4d_particleeffect_get_uniform;            external TSkiaAPI.LibName name 'sk4d_particleeffect_get_uniform';
    class function  TSkiaAPI.sk4d_particleeffect_get_uniform_count;      external TSkiaAPI.LibName name 'sk4d_particleeffect_get_uniform_count';
    class function  TSkiaAPI.sk4d_particleeffect_get_uniform_data;       external TSkiaAPI.LibName name 'sk4d_particleeffect_get_uniform_data';
    class function  TSkiaAPI.sk4d_particleeffect_get_uniform_data_count; external TSkiaAPI.LibName name 'sk4d_particleeffect_get_uniform_data_count';
    class function  TSkiaAPI.sk4d_particleeffect_get_uniform_name;       external TSkiaAPI.LibName name 'sk4d_particleeffect_get_uniform_name';
    class procedure TSkiaAPI.sk4d_particleeffect_init;                   external TSkiaAPI.LibName name 'sk4d_particleeffect_init';
    class function  TSkiaAPI.sk4d_particleeffect_make_from_file;         external TSkiaAPI.LibName name 'sk4d_particleeffect_make_from_file';
    class function  TSkiaAPI.sk4d_particleeffect_make_from_stream;       external TSkiaAPI.LibName name 'sk4d_particleeffect_make_from_stream';
    class procedure TSkiaAPI.sk4d_particleeffect_render;                 external TSkiaAPI.LibName name 'sk4d_particleeffect_render';
    class procedure TSkiaAPI.sk4d_particleeffect_set_position;           external TSkiaAPI.LibName name 'sk4d_particleeffect_set_position';
    class procedure TSkiaAPI.sk4d_particleeffect_set_rate;               external TSkiaAPI.LibName name 'sk4d_particleeffect_set_rate';
    class function  TSkiaAPI.sk4d_particleeffect_set_uniform;            external TSkiaAPI.LibName name 'sk4d_particleeffect_set_uniform';
    class procedure TSkiaAPI.sk4d_particleeffect_start;                  external TSkiaAPI.LibName name 'sk4d_particleeffect_start';
    class procedure TSkiaAPI.sk4d_particleeffect_update;                 external TSkiaAPI.LibName name 'sk4d_particleeffect_update';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skottie/include/sk4d_skottie.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_skottieanimation_get_duration     := GetProcAddress(FLibHandle, 'sk4d_skottieanimation_get_duration');
    sk4d_skottieanimation_get_fps          := GetProcAddress(FLibHandle, 'sk4d_skottieanimation_get_fps');
    sk4d_skottieanimation_get_in_point     := GetProcAddress(FLibHandle, 'sk4d_skottieanimation_get_in_point');
    sk4d_skottieanimation_get_out_point    := GetProcAddress(FLibHandle, 'sk4d_skottieanimation_get_out_point');
    sk4d_skottieanimation_get_size         := GetProcAddress(FLibHandle, 'sk4d_skottieanimation_get_size');
    sk4d_skottieanimation_get_version      := GetProcAddress(FLibHandle, 'sk4d_skottieanimation_get_version');
    sk4d_skottieanimation_make_from_file   := GetProcAddress(FLibHandle, 'sk4d_skottieanimation_make_from_file');
    sk4d_skottieanimation_make_from_stream := GetProcAddress(FLibHandle, 'sk4d_skottieanimation_make_from_stream');
    sk4d_skottieanimation_ref              := GetProcAddress(FLibHandle, 'sk4d_skottieanimation_ref');
    sk4d_skottieanimation_render           := GetProcAddress(FLibHandle, 'sk4d_skottieanimation_render');
    sk4d_skottieanimation_seek_frame       := GetProcAddress(FLibHandle, 'sk4d_skottieanimation_seek_frame');
    sk4d_skottieanimation_seek_frame_time  := GetProcAddress(FLibHandle, 'sk4d_skottieanimation_seek_frame_time');
    sk4d_skottieanimation_unref            := GetProcAddress(FLibHandle, 'sk4d_skottieanimation_unref');
    {$ELSE}
    class function  TSkiaAPI.sk4d_skottieanimation_get_duration;     external TSkiaAPI.LibName name 'sk4d_skottieanimation_get_duration';
    class function  TSkiaAPI.sk4d_skottieanimation_get_fps;          external TSkiaAPI.LibName name 'sk4d_skottieanimation_get_fps';
    class function  TSkiaAPI.sk4d_skottieanimation_get_in_point;     external TSkiaAPI.LibName name 'sk4d_skottieanimation_get_in_point';
    class function  TSkiaAPI.sk4d_skottieanimation_get_out_point;    external TSkiaAPI.LibName name 'sk4d_skottieanimation_get_out_point';
    class procedure TSkiaAPI.sk4d_skottieanimation_get_size;         external TSkiaAPI.LibName name 'sk4d_skottieanimation_get_size';
    class function  TSkiaAPI.sk4d_skottieanimation_get_version;      external TSkiaAPI.LibName name 'sk4d_skottieanimation_get_version';
    class function  TSkiaAPI.sk4d_skottieanimation_make_from_file;   external TSkiaAPI.LibName name 'sk4d_skottieanimation_make_from_file';
    class function  TSkiaAPI.sk4d_skottieanimation_make_from_stream; external TSkiaAPI.LibName name 'sk4d_skottieanimation_make_from_stream';
    class procedure TSkiaAPI.sk4d_skottieanimation_ref;              external TSkiaAPI.LibName name 'sk4d_skottieanimation_ref';
    class procedure TSkiaAPI.sk4d_skottieanimation_render;           external TSkiaAPI.LibName name 'sk4d_skottieanimation_render';
    class procedure TSkiaAPI.sk4d_skottieanimation_seek_frame;       external TSkiaAPI.LibName name 'sk4d_skottieanimation_seek_frame';
    class procedure TSkiaAPI.sk4d_skottieanimation_seek_frame_time;  external TSkiaAPI.LibName name 'sk4d_skottieanimation_seek_frame_time';
    class procedure TSkiaAPI.sk4d_skottieanimation_unref;            external TSkiaAPI.LibName name 'sk4d_skottieanimation_unref';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skparagraph/include/sk4d_paragraph.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_paragraph_destroy                          := GetProcAddress(FLibHandle, 'sk4d_paragraph_destroy');
    sk4d_paragraph_did_exceed_max_lines             := GetProcAddress(FLibHandle, 'sk4d_paragraph_did_exceed_max_lines');
    sk4d_paragraph_get_alphabetic_baseline          := GetProcAddress(FLibHandle, 'sk4d_paragraph_get_alphabetic_baseline');
    sk4d_paragraph_get_glyph_position_at_coordinate := GetProcAddress(FLibHandle, 'sk4d_paragraph_get_glyph_position_at_coordinate');
    sk4d_paragraph_get_height                       := GetProcAddress(FLibHandle, 'sk4d_paragraph_get_height');
    sk4d_paragraph_get_ideographic_baseline         := GetProcAddress(FLibHandle, 'sk4d_paragraph_get_ideographic_baseline');
    sk4d_paragraph_get_line_metrics                 := GetProcAddress(FLibHandle, 'sk4d_paragraph_get_line_metrics');
    sk4d_paragraph_get_longest_line                 := GetProcAddress(FLibHandle, 'sk4d_paragraph_get_longest_line');
    sk4d_paragraph_get_max_intrinsic_width          := GetProcAddress(FLibHandle, 'sk4d_paragraph_get_max_intrinsic_width');
    sk4d_paragraph_get_max_width                    := GetProcAddress(FLibHandle, 'sk4d_paragraph_get_max_width');
    sk4d_paragraph_get_min_intrinsic_width          := GetProcAddress(FLibHandle, 'sk4d_paragraph_get_min_intrinsic_width');
    sk4d_paragraph_get_rects_for_placeholders       := GetProcAddress(FLibHandle, 'sk4d_paragraph_get_rects_for_placeholders');
    sk4d_paragraph_get_rects_for_range              := GetProcAddress(FLibHandle, 'sk4d_paragraph_get_rects_for_range');
    sk4d_paragraph_get_word_boundary                := GetProcAddress(FLibHandle, 'sk4d_paragraph_get_word_boundary');
    sk4d_paragraph_layout                           := GetProcAddress(FLibHandle, 'sk4d_paragraph_layout');
    sk4d_paragraph_paint                            := GetProcAddress(FLibHandle, 'sk4d_paragraph_paint');
    sk4d_paragraph_to_path                          := GetProcAddress(FLibHandle, 'sk4d_paragraph_to_path');
    {$ELSE}
    class procedure TSkiaAPI.sk4d_paragraph_destroy;                          external TSkiaAPI.LibName name 'sk4d_paragraph_destroy';
    class function  TSkiaAPI.sk4d_paragraph_did_exceed_max_lines;             external TSkiaAPI.LibName name 'sk4d_paragraph_did_exceed_max_lines';
    class function  TSkiaAPI.sk4d_paragraph_get_alphabetic_baseline;          external TSkiaAPI.LibName name 'sk4d_paragraph_get_alphabetic_baseline';
    class procedure TSkiaAPI.sk4d_paragraph_get_glyph_position_at_coordinate; external TSkiaAPI.LibName name 'sk4d_paragraph_get_glyph_position_at_coordinate';
    class function  TSkiaAPI.sk4d_paragraph_get_height;                       external TSkiaAPI.LibName name 'sk4d_paragraph_get_height';
    class function  TSkiaAPI.sk4d_paragraph_get_ideographic_baseline;         external TSkiaAPI.LibName name 'sk4d_paragraph_get_ideographic_baseline';
    class function  TSkiaAPI.sk4d_paragraph_get_line_metrics;                 external TSkiaAPI.LibName name 'sk4d_paragraph_get_line_metrics';
    class function  TSkiaAPI.sk4d_paragraph_get_longest_line;                 external TSkiaAPI.LibName name 'sk4d_paragraph_get_longest_line';
    class function  TSkiaAPI.sk4d_paragraph_get_max_intrinsic_width;          external TSkiaAPI.LibName name 'sk4d_paragraph_get_max_intrinsic_width';
    class function  TSkiaAPI.sk4d_paragraph_get_max_width;                    external TSkiaAPI.LibName name 'sk4d_paragraph_get_max_width';
    class function  TSkiaAPI.sk4d_paragraph_get_min_intrinsic_width;          external TSkiaAPI.LibName name 'sk4d_paragraph_get_min_intrinsic_width';
    class function  TSkiaAPI.sk4d_paragraph_get_rects_for_placeholders;       external TSkiaAPI.LibName name 'sk4d_paragraph_get_rects_for_placeholders';
    class function  TSkiaAPI.sk4d_paragraph_get_rects_for_range;              external TSkiaAPI.LibName name 'sk4d_paragraph_get_rects_for_range';
    class procedure TSkiaAPI.sk4d_paragraph_get_word_boundary;                external TSkiaAPI.LibName name 'sk4d_paragraph_get_word_boundary';
    class procedure TSkiaAPI.sk4d_paragraph_layout;                           external TSkiaAPI.LibName name 'sk4d_paragraph_layout';
    class procedure TSkiaAPI.sk4d_paragraph_paint;                            external TSkiaAPI.LibName name 'sk4d_paragraph_paint';
    class function  TSkiaAPI.sk4d_paragraph_to_path;                          external TSkiaAPI.LibName name 'sk4d_paragraph_to_path';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skparagraph/include/sk4d_paragraphbuilder.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_paragraphbuilder_add_placeholder := GetProcAddress(FLibHandle, 'sk4d_paragraphbuilder_add_placeholder');
    sk4d_paragraphbuilder_add_text        := GetProcAddress(FLibHandle, 'sk4d_paragraphbuilder_add_text');
    sk4d_paragraphbuilder_build           := GetProcAddress(FLibHandle, 'sk4d_paragraphbuilder_build');
    sk4d_paragraphbuilder_create          := GetProcAddress(FLibHandle, 'sk4d_paragraphbuilder_create');
    sk4d_paragraphbuilder_create2         := GetProcAddress(FLibHandle, 'sk4d_paragraphbuilder_create2');
    sk4d_paragraphbuilder_destroy         := GetProcAddress(FLibHandle, 'sk4d_paragraphbuilder_destroy');
    sk4d_paragraphbuilder_pop             := GetProcAddress(FLibHandle, 'sk4d_paragraphbuilder_pop');
    sk4d_paragraphbuilder_push_style      := GetProcAddress(FLibHandle, 'sk4d_paragraphbuilder_push_style');
    {$ELSE}
    class procedure TSkiaAPI.sk4d_paragraphbuilder_add_placeholder; external TSkiaAPI.LibName name 'sk4d_paragraphbuilder_add_placeholder';
    class procedure TSkiaAPI.sk4d_paragraphbuilder_add_text;        external TSkiaAPI.LibName name 'sk4d_paragraphbuilder_add_text';
    class function  TSkiaAPI.sk4d_paragraphbuilder_build;           external TSkiaAPI.LibName name 'sk4d_paragraphbuilder_build';
    class function  TSkiaAPI.sk4d_paragraphbuilder_create;          external TSkiaAPI.LibName name 'sk4d_paragraphbuilder_create';
    class function  TSkiaAPI.sk4d_paragraphbuilder_create2;         external TSkiaAPI.LibName name 'sk4d_paragraphbuilder_create2';
    class procedure TSkiaAPI.sk4d_paragraphbuilder_destroy;         external TSkiaAPI.LibName name 'sk4d_paragraphbuilder_destroy';
    class procedure TSkiaAPI.sk4d_paragraphbuilder_pop;             external TSkiaAPI.LibName name 'sk4d_paragraphbuilder_pop';
    class procedure TSkiaAPI.sk4d_paragraphbuilder_push_style;      external TSkiaAPI.LibName name 'sk4d_paragraphbuilder_push_style';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skparagraph/include/sk4d_paragraphstyle.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_paragraphstyle_create                    := GetProcAddress(FLibHandle, 'sk4d_paragraphstyle_create');
    sk4d_paragraphstyle_destroy                   := GetProcAddress(FLibHandle, 'sk4d_paragraphstyle_destroy');
    sk4d_paragraphstyle_disable_hinting           := GetProcAddress(FLibHandle, 'sk4d_paragraphstyle_disable_hinting');
    sk4d_paragraphstyle_get_ellipsis              := GetProcAddress(FLibHandle, 'sk4d_paragraphstyle_get_ellipsis');
    sk4d_paragraphstyle_get_height                := GetProcAddress(FLibHandle, 'sk4d_paragraphstyle_get_height');
    sk4d_paragraphstyle_get_max_lines             := GetProcAddress(FLibHandle, 'sk4d_paragraphstyle_get_max_lines');
    sk4d_paragraphstyle_get_strut_style           := GetProcAddress(FLibHandle, 'sk4d_paragraphstyle_get_strut_style');
    sk4d_paragraphstyle_get_text_align            := GetProcAddress(FLibHandle, 'sk4d_paragraphstyle_get_text_align');
    sk4d_paragraphstyle_get_text_direction        := GetProcAddress(FLibHandle, 'sk4d_paragraphstyle_get_text_direction');
    sk4d_paragraphstyle_get_text_height_behaviors := GetProcAddress(FLibHandle, 'sk4d_paragraphstyle_get_text_height_behaviors');
    sk4d_paragraphstyle_get_text_style            := GetProcAddress(FLibHandle, 'sk4d_paragraphstyle_get_text_style');
    sk4d_paragraphstyle_set_ellipsis              := GetProcAddress(FLibHandle, 'sk4d_paragraphstyle_set_ellipsis');
    sk4d_paragraphstyle_set_height                := GetProcAddress(FLibHandle, 'sk4d_paragraphstyle_set_height');
    sk4d_paragraphstyle_set_max_lines             := GetProcAddress(FLibHandle, 'sk4d_paragraphstyle_set_max_lines');
    sk4d_paragraphstyle_set_strut_style           := GetProcAddress(FLibHandle, 'sk4d_paragraphstyle_set_strut_style');
    sk4d_paragraphstyle_set_text_align            := GetProcAddress(FLibHandle, 'sk4d_paragraphstyle_set_text_align');
    sk4d_paragraphstyle_set_text_direction        := GetProcAddress(FLibHandle, 'sk4d_paragraphstyle_set_text_direction');
    sk4d_paragraphstyle_set_text_height_behaviors := GetProcAddress(FLibHandle, 'sk4d_paragraphstyle_set_text_height_behaviors');
    sk4d_paragraphstyle_set_text_style            := GetProcAddress(FLibHandle, 'sk4d_paragraphstyle_set_text_style');
    sk4d_strutstyle_create                        := GetProcAddress(FLibHandle, 'sk4d_strutstyle_create');
    sk4d_strutstyle_destroy                       := GetProcAddress(FLibHandle, 'sk4d_strutstyle_destroy');
    sk4d_strutstyle_get_enabled                   := GetProcAddress(FLibHandle, 'sk4d_strutstyle_get_enabled');
    sk4d_strutstyle_get_font_families             := GetProcAddress(FLibHandle, 'sk4d_strutstyle_get_font_families');
    sk4d_strutstyle_get_font_size                 := GetProcAddress(FLibHandle, 'sk4d_strutstyle_get_font_size');
    sk4d_strutstyle_get_font_style                := GetProcAddress(FLibHandle, 'sk4d_strutstyle_get_font_style');
    sk4d_strutstyle_get_force_height              := GetProcAddress(FLibHandle, 'sk4d_strutstyle_get_force_height');
    sk4d_strutstyle_get_half_leading              := GetProcAddress(FLibHandle, 'sk4d_strutstyle_get_half_leading');
    sk4d_strutstyle_get_height_multiplier         := GetProcAddress(FLibHandle, 'sk4d_strutstyle_get_height_multiplier');
    sk4d_strutstyle_get_leading                   := GetProcAddress(FLibHandle, 'sk4d_strutstyle_get_leading');
    sk4d_strutstyle_is_equal                      := GetProcAddress(FLibHandle, 'sk4d_strutstyle_is_equal');
    sk4d_strutstyle_set_enabled                   := GetProcAddress(FLibHandle, 'sk4d_strutstyle_set_enabled');
    sk4d_strutstyle_set_font_families             := GetProcAddress(FLibHandle, 'sk4d_strutstyle_set_font_families');
    sk4d_strutstyle_set_font_size                 := GetProcAddress(FLibHandle, 'sk4d_strutstyle_set_font_size');
    sk4d_strutstyle_set_font_style                := GetProcAddress(FLibHandle, 'sk4d_strutstyle_set_font_style');
    sk4d_strutstyle_set_force_height              := GetProcAddress(FLibHandle, 'sk4d_strutstyle_set_force_height');
    sk4d_strutstyle_set_half_leading              := GetProcAddress(FLibHandle, 'sk4d_strutstyle_set_half_leading');
    sk4d_strutstyle_set_height_multiplier         := GetProcAddress(FLibHandle, 'sk4d_strutstyle_set_height_multiplier');
    sk4d_strutstyle_set_leading                   := GetProcAddress(FLibHandle, 'sk4d_strutstyle_set_leading');
    {$ELSE}
    class function  TSkiaAPI.sk4d_paragraphstyle_create;                    external TSkiaAPI.LibName name 'sk4d_paragraphstyle_create';
    class procedure TSkiaAPI.sk4d_paragraphstyle_destroy;                   external TSkiaAPI.LibName name 'sk4d_paragraphstyle_destroy';
    class procedure TSkiaAPI.sk4d_paragraphstyle_disable_hinting;           external TSkiaAPI.LibName name 'sk4d_paragraphstyle_disable_hinting';
    class function  TSkiaAPI.sk4d_paragraphstyle_get_ellipsis;              external TSkiaAPI.LibName name 'sk4d_paragraphstyle_get_ellipsis';
    class function  TSkiaAPI.sk4d_paragraphstyle_get_height;                external TSkiaAPI.LibName name 'sk4d_paragraphstyle_get_height';
    class function  TSkiaAPI.sk4d_paragraphstyle_get_max_lines;             external TSkiaAPI.LibName name 'sk4d_paragraphstyle_get_max_lines';
    class function  TSkiaAPI.sk4d_paragraphstyle_get_strut_style;           external TSkiaAPI.LibName name 'sk4d_paragraphstyle_get_strut_style';
    class function  TSkiaAPI.sk4d_paragraphstyle_get_text_align;            external TSkiaAPI.LibName name 'sk4d_paragraphstyle_get_text_align';
    class function  TSkiaAPI.sk4d_paragraphstyle_get_text_direction;        external TSkiaAPI.LibName name 'sk4d_paragraphstyle_get_text_direction';
    class function  TSkiaAPI.sk4d_paragraphstyle_get_text_height_behaviors; external TSkiaAPI.LibName name 'sk4d_paragraphstyle_get_text_height_behaviors';
    class function  TSkiaAPI.sk4d_paragraphstyle_get_text_style;            external TSkiaAPI.LibName name 'sk4d_paragraphstyle_get_text_style';
    class procedure TSkiaAPI.sk4d_paragraphstyle_set_ellipsis;              external TSkiaAPI.LibName name 'sk4d_paragraphstyle_set_ellipsis';
    class procedure TSkiaAPI.sk4d_paragraphstyle_set_height;                external TSkiaAPI.LibName name 'sk4d_paragraphstyle_set_height';
    class procedure TSkiaAPI.sk4d_paragraphstyle_set_max_lines;             external TSkiaAPI.LibName name 'sk4d_paragraphstyle_set_max_lines';
    class procedure TSkiaAPI.sk4d_paragraphstyle_set_strut_style;           external TSkiaAPI.LibName name 'sk4d_paragraphstyle_set_strut_style';
    class procedure TSkiaAPI.sk4d_paragraphstyle_set_text_align;            external TSkiaAPI.LibName name 'sk4d_paragraphstyle_set_text_align';
    class procedure TSkiaAPI.sk4d_paragraphstyle_set_text_direction;        external TSkiaAPI.LibName name 'sk4d_paragraphstyle_set_text_direction';
    class procedure TSkiaAPI.sk4d_paragraphstyle_set_text_height_behaviors; external TSkiaAPI.LibName name 'sk4d_paragraphstyle_set_text_height_behaviors';
    class procedure TSkiaAPI.sk4d_paragraphstyle_set_text_style;            external TSkiaAPI.LibName name 'sk4d_paragraphstyle_set_text_style';
    class function  TSkiaAPI.sk4d_strutstyle_create;                        external TSkiaAPI.LibName name 'sk4d_strutstyle_create';
    class procedure TSkiaAPI.sk4d_strutstyle_destroy;                       external TSkiaAPI.LibName name 'sk4d_strutstyle_destroy';
    class function  TSkiaAPI.sk4d_strutstyle_get_enabled;                   external TSkiaAPI.LibName name 'sk4d_strutstyle_get_enabled';
    class function  TSkiaAPI.sk4d_strutstyle_get_font_families;             external TSkiaAPI.LibName name 'sk4d_strutstyle_get_font_families';
    class function  TSkiaAPI.sk4d_strutstyle_get_font_size;                 external TSkiaAPI.LibName name 'sk4d_strutstyle_get_font_size';
    class procedure TSkiaAPI.sk4d_strutstyle_get_font_style;                external TSkiaAPI.LibName name 'sk4d_strutstyle_get_font_style';
    class function  TSkiaAPI.sk4d_strutstyle_get_force_height;              external TSkiaAPI.LibName name 'sk4d_strutstyle_get_force_height';
    class function  TSkiaAPI.sk4d_strutstyle_get_half_leading;              external TSkiaAPI.LibName name 'sk4d_strutstyle_get_half_leading';
    class function  TSkiaAPI.sk4d_strutstyle_get_height_multiplier;         external TSkiaAPI.LibName name 'sk4d_strutstyle_get_height_multiplier';
    class function  TSkiaAPI.sk4d_strutstyle_get_leading;                   external TSkiaAPI.LibName name 'sk4d_strutstyle_get_leading';
    class function  TSkiaAPI.sk4d_strutstyle_is_equal;                      external TSkiaAPI.LibName name 'sk4d_strutstyle_is_equal';
    class procedure TSkiaAPI.sk4d_strutstyle_set_enabled;                   external TSkiaAPI.LibName name 'sk4d_strutstyle_set_enabled';
    class procedure TSkiaAPI.sk4d_strutstyle_set_font_families;             external TSkiaAPI.LibName name 'sk4d_strutstyle_set_font_families';
    class procedure TSkiaAPI.sk4d_strutstyle_set_font_size;                 external TSkiaAPI.LibName name 'sk4d_strutstyle_set_font_size';
    class procedure TSkiaAPI.sk4d_strutstyle_set_font_style;                external TSkiaAPI.LibName name 'sk4d_strutstyle_set_font_style';
    class procedure TSkiaAPI.sk4d_strutstyle_set_force_height;              external TSkiaAPI.LibName name 'sk4d_strutstyle_set_force_height';
    class procedure TSkiaAPI.sk4d_strutstyle_set_half_leading;              external TSkiaAPI.LibName name 'sk4d_strutstyle_set_half_leading';
    class procedure TSkiaAPI.sk4d_strutstyle_set_height_multiplier;         external TSkiaAPI.LibName name 'sk4d_strutstyle_set_height_multiplier';
    class procedure TSkiaAPI.sk4d_strutstyle_set_leading;                   external TSkiaAPI.LibName name 'sk4d_strutstyle_set_leading';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skparagraph/include/sk4d_textstyle.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_textstyle_add_font_feature         := GetProcAddress(FLibHandle, 'sk4d_textstyle_add_font_feature');
    sk4d_textstyle_add_shadow               := GetProcAddress(FLibHandle, 'sk4d_textstyle_add_shadow');
    sk4d_textstyle_clear_background_color   := GetProcAddress(FLibHandle, 'sk4d_textstyle_clear_background_color');
    sk4d_textstyle_clear_foreground_color   := GetProcAddress(FLibHandle, 'sk4d_textstyle_clear_foreground_color');
    sk4d_textstyle_create                   := GetProcAddress(FLibHandle, 'sk4d_textstyle_create');
    sk4d_textstyle_destroy                  := GetProcAddress(FLibHandle, 'sk4d_textstyle_destroy');
    sk4d_textstyle_get_background           := GetProcAddress(FLibHandle, 'sk4d_textstyle_get_background');
    sk4d_textstyle_get_color                := GetProcAddress(FLibHandle, 'sk4d_textstyle_get_color');
    sk4d_textstyle_get_decoration_color     := GetProcAddress(FLibHandle, 'sk4d_textstyle_get_decoration_color');
    sk4d_textstyle_get_decoration_style     := GetProcAddress(FLibHandle, 'sk4d_textstyle_get_decoration_style');
    sk4d_textstyle_get_decoration_thickness := GetProcAddress(FLibHandle, 'sk4d_textstyle_get_decoration_thickness');
    sk4d_textstyle_get_decorations          := GetProcAddress(FLibHandle, 'sk4d_textstyle_get_decorations');
    sk4d_textstyle_get_font_families        := GetProcAddress(FLibHandle, 'sk4d_textstyle_get_font_families');
    sk4d_textstyle_get_font_metrics         := GetProcAddress(FLibHandle, 'sk4d_textstyle_get_font_metrics');
    sk4d_textstyle_get_font_size            := GetProcAddress(FLibHandle, 'sk4d_textstyle_get_font_size');
    sk4d_textstyle_get_font_style           := GetProcAddress(FLibHandle, 'sk4d_textstyle_get_font_style');
    sk4d_textstyle_get_foreground           := GetProcAddress(FLibHandle, 'sk4d_textstyle_get_foreground');
    sk4d_textstyle_get_half_leading         := GetProcAddress(FLibHandle, 'sk4d_textstyle_get_half_leading');
    sk4d_textstyle_get_height_multiplier    := GetProcAddress(FLibHandle, 'sk4d_textstyle_get_height_multiplier');
    sk4d_textstyle_get_letter_spacing       := GetProcAddress(FLibHandle, 'sk4d_textstyle_get_letter_spacing');
    sk4d_textstyle_get_locale               := GetProcAddress(FLibHandle, 'sk4d_textstyle_get_locale');
    sk4d_textstyle_get_word_spacing         := GetProcAddress(FLibHandle, 'sk4d_textstyle_get_word_spacing');
    sk4d_textstyle_is_equal                 := GetProcAddress(FLibHandle, 'sk4d_textstyle_is_equal');
    sk4d_textstyle_reset_font_features      := GetProcAddress(FLibHandle, 'sk4d_textstyle_reset_font_features');
    sk4d_textstyle_reset_shadows            := GetProcAddress(FLibHandle, 'sk4d_textstyle_reset_shadows');
    sk4d_textstyle_set_background_color     := GetProcAddress(FLibHandle, 'sk4d_textstyle_set_background_color');
    sk4d_textstyle_set_color                := GetProcAddress(FLibHandle, 'sk4d_textstyle_set_color');
    sk4d_textstyle_set_decoration_color     := GetProcAddress(FLibHandle, 'sk4d_textstyle_set_decoration_color');
    sk4d_textstyle_set_decoration_style     := GetProcAddress(FLibHandle, 'sk4d_textstyle_set_decoration_style');
    sk4d_textstyle_set_decoration_thickness := GetProcAddress(FLibHandle, 'sk4d_textstyle_set_decoration_thickness');
    sk4d_textstyle_set_decorations          := GetProcAddress(FLibHandle, 'sk4d_textstyle_set_decorations');
    sk4d_textstyle_set_font_families        := GetProcAddress(FLibHandle, 'sk4d_textstyle_set_font_families');
    sk4d_textstyle_set_font_size            := GetProcAddress(FLibHandle, 'sk4d_textstyle_set_font_size');
    sk4d_textstyle_set_font_style           := GetProcAddress(FLibHandle, 'sk4d_textstyle_set_font_style');
    sk4d_textstyle_set_foreground_color     := GetProcAddress(FLibHandle, 'sk4d_textstyle_set_foreground_color');
    sk4d_textstyle_set_half_leading         := GetProcAddress(FLibHandle, 'sk4d_textstyle_set_half_leading');
    sk4d_textstyle_set_height_multiplier    := GetProcAddress(FLibHandle, 'sk4d_textstyle_set_height_multiplier');
    sk4d_textstyle_set_letter_spacing       := GetProcAddress(FLibHandle, 'sk4d_textstyle_set_letter_spacing');
    sk4d_textstyle_set_locale               := GetProcAddress(FLibHandle, 'sk4d_textstyle_set_locale');
    sk4d_textstyle_set_word_spacing         := GetProcAddress(FLibHandle, 'sk4d_textstyle_set_word_spacing');
    {$ELSE}
    class procedure TSkiaAPI.sk4d_textstyle_add_font_feature;         external TSkiaAPI.LibName name 'sk4d_textstyle_add_font_feature';
    class procedure TSkiaAPI.sk4d_textstyle_add_shadow;               external TSkiaAPI.LibName name 'sk4d_textstyle_add_shadow';
    class procedure TSkiaAPI.sk4d_textstyle_clear_background_color;   external TSkiaAPI.LibName name 'sk4d_textstyle_clear_background_color';
    class procedure TSkiaAPI.sk4d_textstyle_clear_foreground_color;   external TSkiaAPI.LibName name 'sk4d_textstyle_clear_foreground_color';
    class function  TSkiaAPI.sk4d_textstyle_create;                   external TSkiaAPI.LibName name 'sk4d_textstyle_create';
    class procedure TSkiaAPI.sk4d_textstyle_destroy;                  external TSkiaAPI.LibName name 'sk4d_textstyle_destroy';
    class function  TSkiaAPI.sk4d_textstyle_get_background;           external TSkiaAPI.LibName name 'sk4d_textstyle_get_background';
    class function  TSkiaAPI.sk4d_textstyle_get_color;                external TSkiaAPI.LibName name 'sk4d_textstyle_get_color';
    class function  TSkiaAPI.sk4d_textstyle_get_decoration_color;     external TSkiaAPI.LibName name 'sk4d_textstyle_get_decoration_color';
    class function  TSkiaAPI.sk4d_textstyle_get_decoration_style;     external TSkiaAPI.LibName name 'sk4d_textstyle_get_decoration_style';
    class function  TSkiaAPI.sk4d_textstyle_get_decoration_thickness; external TSkiaAPI.LibName name 'sk4d_textstyle_get_decoration_thickness';
    class function  TSkiaAPI.sk4d_textstyle_get_decorations;          external TSkiaAPI.LibName name 'sk4d_textstyle_get_decorations';
    class function  TSkiaAPI.sk4d_textstyle_get_font_families;        external TSkiaAPI.LibName name 'sk4d_textstyle_get_font_families';
    class procedure TSkiaAPI.sk4d_textstyle_get_font_metrics;         external TSkiaAPI.LibName name 'sk4d_textstyle_get_font_metrics';
    class function  TSkiaAPI.sk4d_textstyle_get_font_size;            external TSkiaAPI.LibName name 'sk4d_textstyle_get_font_size';
    class procedure TSkiaAPI.sk4d_textstyle_get_font_style;           external TSkiaAPI.LibName name 'sk4d_textstyle_get_font_style';
    class function  TSkiaAPI.sk4d_textstyle_get_foreground;           external TSkiaAPI.LibName name 'sk4d_textstyle_get_foreground';
    class function  TSkiaAPI.sk4d_textstyle_get_half_leading;         external TSkiaAPI.LibName name 'sk4d_textstyle_get_half_leading';
    class function  TSkiaAPI.sk4d_textstyle_get_height_multiplier;    external TSkiaAPI.LibName name 'sk4d_textstyle_get_height_multiplier';
    class function  TSkiaAPI.sk4d_textstyle_get_letter_spacing;       external TSkiaAPI.LibName name 'sk4d_textstyle_get_letter_spacing';
    class function  TSkiaAPI.sk4d_textstyle_get_locale;               external TSkiaAPI.LibName name 'sk4d_textstyle_get_locale';
    class function  TSkiaAPI.sk4d_textstyle_get_word_spacing;         external TSkiaAPI.LibName name 'sk4d_textstyle_get_word_spacing';
    class function  TSkiaAPI.sk4d_textstyle_is_equal;                 external TSkiaAPI.LibName name 'sk4d_textstyle_is_equal';
    class procedure TSkiaAPI.sk4d_textstyle_reset_font_features;      external TSkiaAPI.LibName name 'sk4d_textstyle_reset_font_features';
    class procedure TSkiaAPI.sk4d_textstyle_reset_shadows;            external TSkiaAPI.LibName name 'sk4d_textstyle_reset_shadows';
    class procedure TSkiaAPI.sk4d_textstyle_set_background_color;     external TSkiaAPI.LibName name 'sk4d_textstyle_set_background_color';
    class procedure TSkiaAPI.sk4d_textstyle_set_color;                external TSkiaAPI.LibName name 'sk4d_textstyle_set_color';
    class procedure TSkiaAPI.sk4d_textstyle_set_decoration_color;     external TSkiaAPI.LibName name 'sk4d_textstyle_set_decoration_color';
    class procedure TSkiaAPI.sk4d_textstyle_set_decoration_style;     external TSkiaAPI.LibName name 'sk4d_textstyle_set_decoration_style';
    class procedure TSkiaAPI.sk4d_textstyle_set_decoration_thickness; external TSkiaAPI.LibName name 'sk4d_textstyle_set_decoration_thickness';
    class procedure TSkiaAPI.sk4d_textstyle_set_decorations;          external TSkiaAPI.LibName name 'sk4d_textstyle_set_decorations';
    class procedure TSkiaAPI.sk4d_textstyle_set_font_families;        external TSkiaAPI.LibName name 'sk4d_textstyle_set_font_families';
    class procedure TSkiaAPI.sk4d_textstyle_set_font_size;            external TSkiaAPI.LibName name 'sk4d_textstyle_set_font_size';
    class procedure TSkiaAPI.sk4d_textstyle_set_font_style;           external TSkiaAPI.LibName name 'sk4d_textstyle_set_font_style';
    class procedure TSkiaAPI.sk4d_textstyle_set_foreground_color;     external TSkiaAPI.LibName name 'sk4d_textstyle_set_foreground_color';
    class procedure TSkiaAPI.sk4d_textstyle_set_half_leading;         external TSkiaAPI.LibName name 'sk4d_textstyle_set_half_leading';
    class procedure TSkiaAPI.sk4d_textstyle_set_height_multiplier;    external TSkiaAPI.LibName name 'sk4d_textstyle_set_height_multiplier';
    class procedure TSkiaAPI.sk4d_textstyle_set_letter_spacing;       external TSkiaAPI.LibName name 'sk4d_textstyle_set_letter_spacing';
    class procedure TSkiaAPI.sk4d_textstyle_set_locale;               external TSkiaAPI.LibName name 'sk4d_textstyle_set_locale';
    class procedure TSkiaAPI.sk4d_textstyle_set_word_spacing;         external TSkiaAPI.LibName name 'sk4d_textstyle_set_word_spacing';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skparagraph/include/sk4d_typefacefontprovider.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_typefacefontprovider_create             := GetProcAddress(FLibHandle, 'sk4d_typefacefontprovider_create');
    sk4d_typefacefontprovider_register_typeface  := GetProcAddress(FLibHandle, 'sk4d_typefacefontprovider_register_typeface');
    sk4d_typefacefontprovider_register_typeface2 := GetProcAddress(FLibHandle, 'sk4d_typefacefontprovider_register_typeface2');
    {$ELSE}
    class function  TSkiaAPI.sk4d_typefacefontprovider_create;             external TSkiaAPI.LibName name 'sk4d_typefacefontprovider_create';
    class procedure TSkiaAPI.sk4d_typefacefontprovider_register_typeface;  external TSkiaAPI.LibName name 'sk4d_typefacefontprovider_register_typeface';
    class procedure TSkiaAPI.sk4d_typefacefontprovider_register_typeface2; external TSkiaAPI.LibName name 'sk4d_typefacefontprovider_register_typeface2';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skresources/include/sk4d_resources.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_resourceproviderbaseclass_create    := GetProcAddress(FLibHandle, 'sk4d_resourceproviderbaseclass_create');
    sk4d_resourceproviderbaseclass_set_procs := GetProcAddress(FLibHandle, 'sk4d_resourceproviderbaseclass_set_procs');
    {$ELSE}
    class function  TSkiaAPI.sk4d_resourceproviderbaseclass_create;    external TSkiaAPI.LibName name 'sk4d_resourceproviderbaseclass_create';
    class procedure TSkiaAPI.sk4d_resourceproviderbaseclass_set_procs; external TSkiaAPI.LibName name 'sk4d_resourceproviderbaseclass_set_procs';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skshaper/include/sk4d_shaper.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_shaper_create  := GetProcAddress(FLibHandle, 'sk4d_shaper_create');
    sk4d_shaper_destroy := GetProcAddress(FLibHandle, 'sk4d_shaper_destroy');
    sk4d_shaper_shape   := GetProcAddress(FLibHandle, 'sk4d_shaper_shape');
    {$ELSE}
    class function  TSkiaAPI.sk4d_shaper_create;  external TSkiaAPI.LibName name 'sk4d_shaper_create';
    class procedure TSkiaAPI.sk4d_shaper_destroy; external TSkiaAPI.LibName name 'sk4d_shaper_destroy';
    class function  TSkiaAPI.sk4d_shaper_shape;   external TSkiaAPI.LibName name 'sk4d_shaper_shape';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skunicode/include/sk4d_unicode.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_unicode_create               := GetProcAddress(FLibHandle, 'sk4d_unicode_create');
    sk4d_unicode_destroy              := GetProcAddress(FLibHandle, 'sk4d_unicode_destroy');
    sk4d_unicode_for_each_bidi_region := GetProcAddress(FLibHandle, 'sk4d_unicode_for_each_bidi_region');
    sk4d_unicode_for_each_break       := GetProcAddress(FLibHandle, 'sk4d_unicode_for_each_break');
    sk4d_unicode_for_each_codepoint   := GetProcAddress(FLibHandle, 'sk4d_unicode_for_each_codepoint');
    {$ELSE}
    class function  TSkiaAPI.sk4d_unicode_create;               external TSkiaAPI.LibName name 'sk4d_unicode_create';
    class procedure TSkiaAPI.sk4d_unicode_destroy;              external TSkiaAPI.LibName name 'sk4d_unicode_destroy';
    class procedure TSkiaAPI.sk4d_unicode_for_each_bidi_region; external TSkiaAPI.LibName name 'sk4d_unicode_for_each_bidi_region';
    class procedure TSkiaAPI.sk4d_unicode_for_each_break;       external TSkiaAPI.LibName name 'sk4d_unicode_for_each_break';
    class procedure TSkiaAPI.sk4d_unicode_for_each_codepoint;   external TSkiaAPI.LibName name 'sk4d_unicode_for_each_codepoint';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/svg/include/sk4d_svgdom.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_svgdom_find_node_by_id    := GetProcAddress(FLibHandle, 'sk4d_svgdom_find_node_by_id');
    sk4d_svgdom_get_root           := GetProcAddress(FLibHandle, 'sk4d_svgdom_get_root');
    sk4d_svgdom_make_from_file     := GetProcAddress(FLibHandle, 'sk4d_svgdom_make_from_file');
    sk4d_svgdom_make_from_stream   := GetProcAddress(FLibHandle, 'sk4d_svgdom_make_from_stream');
    sk4d_svgdom_render             := GetProcAddress(FLibHandle, 'sk4d_svgdom_render');
    sk4d_svgdom_set_container_size := GetProcAddress(FLibHandle, 'sk4d_svgdom_set_container_size');
    {$ELSE}
    class function  TSkiaAPI.sk4d_svgdom_find_node_by_id;    external TSkiaAPI.LibName name 'sk4d_svgdom_find_node_by_id';
    class function  TSkiaAPI.sk4d_svgdom_get_root;           external TSkiaAPI.LibName name 'sk4d_svgdom_get_root';
    class function  TSkiaAPI.sk4d_svgdom_make_from_file;     external TSkiaAPI.LibName name 'sk4d_svgdom_make_from_file';
    class function  TSkiaAPI.sk4d_svgdom_make_from_stream;   external TSkiaAPI.LibName name 'sk4d_svgdom_make_from_stream';
    class procedure TSkiaAPI.sk4d_svgdom_render;             external TSkiaAPI.LibName name 'sk4d_svgdom_render';
    class procedure TSkiaAPI.sk4d_svgdom_set_container_size; external TSkiaAPI.LibName name 'sk4d_svgdom_set_container_size';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/svg/include/sk4d_svgnode.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_svgnode_set_attribute := GetProcAddress(FLibHandle, 'sk4d_svgnode_set_attribute');
    {$ELSE}
    class function TSkiaAPI.sk4d_svgnode_set_attribute; external TSkiaAPI.LibName name 'sk4d_svgnode_set_attribute';
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/svg/include/sk4d_svgsvg.h'}
    {$IFDEF SK_DYNAMIC_LOADING}
    sk4d_svgsvg_get_height                := GetProcAddress(FLibHandle, 'sk4d_svgsvg_get_height');
    sk4d_svgsvg_get_intrinsic_size        := GetProcAddress(FLibHandle, 'sk4d_svgsvg_get_intrinsic_size');
    sk4d_svgsvg_get_preserve_aspect_ratio := GetProcAddress(FLibHandle, 'sk4d_svgsvg_get_preserve_aspect_ratio');
    sk4d_svgsvg_get_view_box              := GetProcAddress(FLibHandle, 'sk4d_svgsvg_get_view_box');
    sk4d_svgsvg_get_width                 := GetProcAddress(FLibHandle, 'sk4d_svgsvg_get_width');
    sk4d_svgsvg_get_x                     := GetProcAddress(FLibHandle, 'sk4d_svgsvg_get_x');
    sk4d_svgsvg_get_y                     := GetProcAddress(FLibHandle, 'sk4d_svgsvg_get_y');
    sk4d_svgsvg_set_height                := GetProcAddress(FLibHandle, 'sk4d_svgsvg_set_height');
    sk4d_svgsvg_set_preserve_aspect_ratio := GetProcAddress(FLibHandle, 'sk4d_svgsvg_set_preserve_aspect_ratio');
    sk4d_svgsvg_set_view_box              := GetProcAddress(FLibHandle, 'sk4d_svgsvg_set_view_box');
    sk4d_svgsvg_set_width                 := GetProcAddress(FLibHandle, 'sk4d_svgsvg_set_width');
    sk4d_svgsvg_set_x                     := GetProcAddress(FLibHandle, 'sk4d_svgsvg_set_x');
    sk4d_svgsvg_set_y                     := GetProcAddress(FLibHandle, 'sk4d_svgsvg_set_y');
    {$ELSE}
    class procedure TSkiaAPI.sk4d_svgsvg_get_height;                external TSkiaAPI.LibName name 'sk4d_svgsvg_get_height';
    class procedure TSkiaAPI.sk4d_svgsvg_get_intrinsic_size;        external TSkiaAPI.LibName name 'sk4d_svgsvg_get_intrinsic_size';
    class procedure TSkiaAPI.sk4d_svgsvg_get_preserve_aspect_ratio; external TSkiaAPI.LibName name 'sk4d_svgsvg_get_preserve_aspect_ratio';
    class function  TSkiaAPI.sk4d_svgsvg_get_view_box;              external TSkiaAPI.LibName name 'sk4d_svgsvg_get_view_box';
    class procedure TSkiaAPI.sk4d_svgsvg_get_width;                 external TSkiaAPI.LibName name 'sk4d_svgsvg_get_width';
    class procedure TSkiaAPI.sk4d_svgsvg_get_x;                     external TSkiaAPI.LibName name 'sk4d_svgsvg_get_x';
    class procedure TSkiaAPI.sk4d_svgsvg_get_y;                     external TSkiaAPI.LibName name 'sk4d_svgsvg_get_y';
    class procedure TSkiaAPI.sk4d_svgsvg_set_height;                external TSkiaAPI.LibName name 'sk4d_svgsvg_set_height';
    class procedure TSkiaAPI.sk4d_svgsvg_set_preserve_aspect_ratio; external TSkiaAPI.LibName name 'sk4d_svgsvg_set_preserve_aspect_ratio';
    class procedure TSkiaAPI.sk4d_svgsvg_set_view_box;              external TSkiaAPI.LibName name 'sk4d_svgsvg_set_view_box';
    class procedure TSkiaAPI.sk4d_svgsvg_set_width;                 external TSkiaAPI.LibName name 'sk4d_svgsvg_set_width';
    class procedure TSkiaAPI.sk4d_svgsvg_set_x;                     external TSkiaAPI.LibName name 'sk4d_svgsvg_set_x';
    class procedure TSkiaAPI.sk4d_svgsvg_set_y;                     external TSkiaAPI.LibName name 'sk4d_svgsvg_set_y';
    {$ENDIF}
    {$ENDREGION}

{$IFDEF SK_DYNAMIC_LOADING}
  end;
end;
{$ENDIF}

class procedure TSkiaAPI.Terminate;
begin
  {$IFDEF SK_DYNAMIC_LOADING}
  if AtomicDecrement(FRefCount) = 0 then
    FreeLibrary(FLibHandle);
  {$ENDIF}
end;

{$ENDREGION}

{$REGION 'Static linking loaders'}

{$IFDEF IOS}
  procedure libcpp; external '/usr/lib/libc++.dylib';
  procedure libcompiler_rt; external '/usr/lib/clang/lib/darwin/libclang_rt.ios.a';
{$ENDIF}

{$ENDREGION}

end.
