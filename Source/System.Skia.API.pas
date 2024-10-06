{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2011-2024 Google LLC.                                    }
{ Copyright (c) 2021-2024 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by a BSD-style license that can be }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit System.Skia.API;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
  {$IFDEF DARWIN}
    {$DEFINE MACOS}
  {$ENDIF}
{$ENDIF}

{$HPPEMIT NOUSINGNAMESPACE}
{$IFDEF MSWINDOWS}
  {$HPPEMIT '#ifdef USEPACKAGES'}
  {$HPPEMIT '  #pragma link "Skia.Package.RTL.bpi"'}
  {$HPPEMIT '#elif defined(__WIN32__)'}
  {$HPPEMIT '  #pragma link "Skia.Package.RTL.lib"'}
  {$HPPEMIT '#elif defined(_WIN64)'}
  {$HPPEMIT '  #if (__clang_major__ >= 15)'}
  {$HPPEMIT '    #pragma link "Skia.Package.RTL.lib"'}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma link "Skia.Package.RTL.a"'}
  {$HPPEMIT '  #endif'}
  {$HPPEMIT '#endif'}
{$ENDIF}

{$ALIGN ON}
{$MINENUMSIZE 4}

uses
  {$IFDEF FPC}
  { FPC }
  SysUtils;
  {$ELSE}
  { Delphi }
  System.SysUtils;
  {$ENDIF}

{$IFDEF IOS}
  {$DEFINE SK_STATIC_LIBRARY}
{$ENDIF}

{$IF DEFINED(ANDROID) or DEFINED(OSX)}
  {$IF CompilerVersion <= 36}
    {$DEFINE WORKAROUND_RS123846}
  {$ELSE}
    {$DEFINE SK_STATIC_LIBRARY}
  {$ENDIF}
{$ENDIF}

type
  _bool     = Boolean;
  {$IF DECLARED(UTF8Char)}
  _char     = UTF8Char;
  {$ELSE}
  _char     = AnsiChar;
  {$ENDIF}
  char16_t  = Char;
  _double   = Double;
  float     = Single;
  int16_t   = SmallInt;
  {$IF DECLARED(FixedInt)}
  int32_t   = FixedInt;
  {$ELSEIF DEFINED(FPC) or DEFINED(MSWINDOWS)}
  int32_t   = Longint;
  {$ELSE}
  int32_t   = Integer;
  {$ENDIF}
  int64_t   = Int64;
  int8_t    = ShortInt;
  intptr_t  = NativeInt;
  size_t    = NativeUInt;
  uint16_t  = Word;
  {$IF DECLARED(FixedUInt)}
  uint32_t  = FixedUInt;
  {$ELSEIF DEFINED(FPC) or DEFINED(MSWINDOWS)}
  uint32_t  = Longword;
  {$ELSE}
  uint32_t  = Cardinal;
  {$ENDIF}
  uint64_t  = UInt64;
  uint8_t   = Byte;
  uintptr_t = NativeUInt;

  _pbool     = ^_bool;
  _pchar     = ^_char;
  pchar16_t  = ^char16_t;
  _pdouble   = ^_double;
  pfloat     = ^float;
  pint16_t   = ^int16_t;
  pint32_t   = ^int32_t;
  pint64_t   = ^int64_t;
  pint8_t    = ^int8_t;
  pintptr_t  = ^intptr_t;
  psize_t    = ^size_t;
  puint16_t  = ^uint16_t;
  puint32_t  = ^uint32_t;
  puint64_t  = ^uint64_t;
  puint8_t   = ^uint8_t;
  puintptr_t = ^uintptr_t;


  { include/c/sk4d_types.h }

  sk_handle_t                   = uintptr_t;

  sk_animcodecplayer_t          = sk_handle_t;
  sk_blender_t                  = sk_handle_t;
  sk_canvas_t                   = sk_handle_t;
  sk_codec_t                    = sk_handle_t;
  sk_colorfilter_t              = sk_handle_t;
  sk_colorspace_t               = sk_handle_t;
  sk_colorspaceiccprofile_t     = sk_handle_t;
  sk_data_t                     = sk_handle_t;
  sk_document_t                 = sk_handle_t;
  sk_flattenable_t              = sk_handle_t;
  sk_font_t                     = sk_handle_t;
  sk_fontmgr_t                  = sk_handle_t;
  sk_image_t                    = sk_handle_t;
  sk_imagefilter_t              = sk_handle_t;
  sk_maskfilter_t               = sk_handle_t;
  sk_opbuilder_t                = sk_handle_t;
  sk_paint_t                    = sk_handle_t;
  sk_path_t                     = sk_handle_t;
  sk_pathbuilder_t              = sk_handle_t;
  sk_patheffect_t               = sk_handle_t;
  sk_pathiterator_t             = sk_handle_t;
  sk_pathmeasure_t              = sk_handle_t;
  sk_pathrawiter_t              = sk_handle_t;
  sk_picture_t                  = sk_handle_t;
  sk_picturerecorder_t          = sk_handle_t;
  sk_pixmap_t                   = sk_handle_t;
  sk_refcnt_t                   = sk_handle_t;
  sk_region_t                   = sk_handle_t;
  sk_regioncliperator_t         = sk_handle_t;
  sk_regioniterator_t           = sk_handle_t;
  sk_regionspanerator_t         = sk_handle_t;
  sk_rrect_t                    = sk_handle_t;
  sk_runtimeblendbuilder_t      = sk_handle_t;
  sk_runtimeeffect_t            = sk_handle_t;
  sk_runtimeeffectbuilder_t     = sk_handle_t;
  sk_runtimeshaderbuilder_t     = sk_handle_t;
  sk_shader_t                   = sk_handle_t;
  sk_stream_t                   = sk_handle_t;
  sk_streamadapter_t            = sk_handle_t;
  sk_string_t                   = sk_handle_t;
  sk_surface_t                  = sk_handle_t;
  sk_textblob_t                 = sk_handle_t;
  sk_tracememorydump_t          = sk_handle_t;
  sk_tracememorydumpbaseclass_t = sk_handle_t;
  sk_typeface_t                 = sk_handle_t;
  sk_vertices_t                 = sk_handle_t;
  sk_wstream_t                  = sk_handle_t;
  sk_wstreamadapter_t           = sk_handle_t;

  psk_animcodecplayer_t          = ^sk_animcodecplayer_t;
  psk_blender_t                  = ^sk_blender_t;
  psk_canvas_t                   = ^sk_canvas_t;
  psk_codec_t                    = ^sk_codec_t;
  psk_colorfilter_t              = ^sk_colorfilter_t;
  psk_colorspace_t               = ^sk_colorspace_t;
  psk_colorspaceiccprofile_t     = ^sk_colorspaceiccprofile_t;
  psk_data_t                     = ^sk_data_t;
  psk_document_t                 = ^sk_document_t;
  psk_flattenable_t              = ^sk_flattenable_t;
  psk_font_t                     = ^sk_font_t;
  psk_fontmgr_t                  = ^sk_fontmgr_t;
  psk_image_t                    = ^sk_image_t;
  psk_imagefilter_t              = ^sk_imagefilter_t;
  psk_maskfilter_t               = ^sk_maskfilter_t;
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
  psk_runtimeblendbuilder_t      = ^sk_runtimeblendbuilder_t;
  psk_runtimeeffect_t            = ^sk_runtimeeffect_t;
  psk_runtimeeffectbuilder_t     = ^sk_runtimeeffectbuilder_t;
  psk_runtimeshaderbuilder_t     = ^sk_runtimeshaderbuilder_t;
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
    SRGBA8888_SK_COLORTYPE,
    R8_SK_COLORTYPE
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

  sk_frame_t = record
    pixmap   : sk_pixmap_t;
    duration : int32_t;
  end;
  psk_frame_t = ^sk_frame_t;

  sk_highcontrastconfig_t = record
    grayscale    : _bool;
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
    pdfa             : _bool;
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
    max_anisotropic : int32_t;
    use_cubic       : _bool;
    cubic           : sk_cubicresampler_t;
    filter          : sk_filtermode_t;
    mipmap          : sk_mipmapmode_t;
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
    seek         : function (context: Pointer; position: size_t): _bool; cdecl;
  end;
  psk_streamadapter_procs_t = ^sk_streamadapter_procs_t;

  sk_wstreamadapter_procs_t = record
    write: function  (context: Pointer; const buffer: Pointer; size: size_t): _bool; cdecl;
  end;
  psk_wstreamadapter_procs_t = ^sk_wstreamadapter_procs_t;

  sk_tracememorydumpbaseclass_procs_t = record
    dump_numeric_value : procedure (context: Pointer; const dump_name, value_name, units: MarshaledAString; value: uint64_t); cdecl;
    dump_string_value  : procedure (context: Pointer; const dump_name, value_name, value: MarshaledAString); cdecl;
  end;
  psk_tracememorydumpbaseclass_procs_t = ^sk_tracememorydumpbaseclass_procs_t;

  // Ganesh

  gr_backendrendertarget_t         = sk_handle_t;
  gr_backendsemaphore_t            = sk_handle_t;
  gr_backendsurfacemutablestate_t  = sk_handle_t;
  gr_backendtexture_t              = sk_handle_t;
  gr_directcontext_t               = sk_handle_t;
  gr_persistentcache_t             = sk_handle_t;
  gr_persistentcachebaseclass_t    = sk_handle_t;
  gr_shadererrorhandler_t          = sk_handle_t;
  gr_shadererrorhandlerbaseclass_t = sk_handle_t;

  pgr_backendrendertarget_t         = ^gr_backendrendertarget_t;
  pgr_backendsemaphore_t            = ^gr_backendsemaphore_t;
  pgr_backendsurfacemutablestate_t  = ^gr_backendsurfacemutablestate_t;
  pgr_backendtexture_t              = ^gr_backendtexture_t;
  pgr_directcontext_t               = ^gr_directcontext_t;
  pgr_persistentcache_t             = ^gr_persistentcache_t;
  pgr_persistentcachebaseclass_t    = ^gr_persistentcachebaseclass_t;
  pgr_shadererrorhandler_t          = ^gr_shadererrorhandler_t;
  pgr_shadererrorhandlerbaseclass_t = ^gr_shadererrorhandlerbaseclass_t;

  gr_backendapi_t = (
    OPEN_GL_GR_BACKENDAPI,
    VULKAN_GR_BACKENDAPI,
    METAL_GR_BACKENDAPI
  );
  pgr_backendapi_t = ^gr_backendapi_t;

  gr_shadercachestrategy_t = (
    SKSL_GR_SHADERCACHESTRATEGY,
    BACKEND_SOURCE_GR_SHADERCACHESTRATEGY,
    BACKEND_BINARY_GR_SHADERCACHESTRATEGY
  );
  pgr_shadercachestrategy_t = ^gr_shadercachestrategy_t;

  gr_surfaceorigin_t = (
    TOP_LEFT_GR_SURFACEORIGIN,
    BOTTOM_LEFT_GR_SURFACEORIGIN
  );
  pgr_surfaceorigin_t = ^gr_surfaceorigin_t;

  gr_contextoptions_t = record
    buffer_map_threshold              : int32_t;
    do_manual_mipmapping              : _bool;
    allow_path_mask_caching           : _bool;
    glyph_cache_texture_maximum_bytes : size_t;
    avoid_stencil_buffers             : _bool;
    runtime_program_cache_size        : int32_t;
    persistent_cache                  : gr_persistentcache_t;
    shader_cache_strategy             : gr_shadercachestrategy_t;
    shader_error_handler              : gr_shadererrorhandler_t;
  end;
  pgr_contextoptions_t = ^gr_contextoptions_t;

  gr_persistentcachebaseclass_procs_t = record
    load  : function  (context: Pointer; const key_data: Pointer; key_size: size_t): sk_data_t; cdecl;
    store : procedure (context: Pointer; const key_data: Pointer; key_size: size_t; const data: Pointer; size: size_t); cdecl;
  end;
  pgr_persistentcachebaseclass_procs_t = ^gr_persistentcachebaseclass_procs_t;

  gr_shadererrorhandlerbaseclass_procs_t = record
    compile_error : procedure (context: Pointer; const shader, errors: MarshaledAString); cdecl;
  end;
  pgr_shadererrorhandlerbaseclass_procs_t = ^gr_shadererrorhandlerbaseclass_procs_t;

  // Ganesh - OpenGL

  gr_gl_interface_t = sk_handle_t;

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

  // Ganesh - Metal

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

  // Ganesh - Vulkan

  gr_vk_extensions_t  = sk_handle_t;

  pgr_vk_extensions_t = ^gr_vk_extensions_t;

  gr_vk_bool32_t                      = uint32_t;
  gr_vk_chromalocation_t              = int32_t;
  gr_vk_device_t                      = intptr_t;
  gr_vk_devicememory_t                = uint64_t;
  gr_vk_devicesize_t                  = uint64_t;
  gr_vk_filter_t                      = int32_t;
  gr_vk_format_t                      = int32_t;
  gr_vk_formatfeatureflags_t          = uint32_t;
  gr_vk_image_t                       = uint64_t;
  gr_vk_imagelayout_t                 = int32_t;
  gr_vk_imagetiling_t                 = int32_t;
  gr_vk_imageusageflags_t             = uint32_t;
  gr_vk_instance_t                    = intptr_t;
  gr_vk_physicaldevice_t              = intptr_t;
  gr_vk_queue_t                       = intptr_t;
  gr_vk_samplerycbcrmodelconversion_t = int32_t;
  gr_vk_samplerycbcrrange_t           = int32_t;
  gr_vk_semaphore_t                   = uint64_t;
  gr_vk_sharingmode_t                 = int32_t;

  pgr_vk_bool32_t                      = ^gr_vk_bool32_t;
  pgr_vk_chromalocation_t              = ^gr_vk_chromalocation_t;
  pgr_vk_device_t                      = ^gr_vk_device_t;
  pgr_vk_devicememory_t                = ^gr_vk_devicememory_t;
  pgr_vk_devicesize_t                  = ^gr_vk_devicesize_t;
  pgr_vk_filter_t                      = ^gr_vk_filter_t;
  pgr_vk_format_t                      = ^gr_vk_format_t;
  pgr_vk_formatfeatureflags_t          = ^gr_vk_formatfeatureflags_t;
  pgr_vk_image_t                       = ^gr_vk_image_t;
  pgr_vk_imagelayout_t                 = ^gr_vk_imagelayout_t;
  pgr_vk_imagetiling_t                 = ^gr_vk_imagetiling_t;
  pgr_vk_imageusageflags_t             = ^gr_vk_imageusageflags_t;
  pgr_vk_instance_t                    = ^gr_vk_instance_t;
  pgr_vk_physicaldevice_t              = ^gr_vk_physicaldevice_t;
  pgr_vk_queue_t                       = ^gr_vk_queue_t;
  pgr_vk_samplerycbcrmodelconversion_t = ^gr_vk_samplerycbcrmodelconversion_t;
  pgr_vk_samplerycbcrrange_t           = ^gr_vk_samplerycbcrrange_t;
  pgr_vk_semaphore_t                   = ^gr_vk_semaphore_t;
  pgr_vk_sharingmode_t                 = ^gr_vk_sharingmode_t;

  gr_vk_physicaldevicefeatures_t  = record end;
  gr_vk_physicaldevicefeatures2_t = record end;

  pgr_vk_physicaldevicefeatures_t  = ^gr_vk_physicaldevicefeatures_t;
  pgr_vk_physicaldevicefeatures2_t = ^gr_vk_physicaldevicefeatures2_t;

  gr_vk_get_proc = function (context: Pointer; const name: MarshaledAString; instance: gr_vk_instance_t; device: gr_vk_device_t): Pointer; cdecl;

  gr_vk_alloc_t = record
    device_memory : gr_vk_devicememory_t;
    offset        : gr_vk_devicesize_t;
    size          : gr_vk_devicesize_t;
    flags         : uint32_t;
    memory        : intptr_t;
  end;
  pgr_vk_alloc_t = ^gr_vk_alloc_t;


  gr_vk_backendcontext_t = record
    instance                  : gr_vk_instance_t;
    physical_device           : gr_vk_physicaldevice_t;
    device                    : gr_vk_device_t;
    queue                     : gr_vk_queue_t;
    graphics_queue_index      : uint32_t;
    max_api_version           : uint32_t;
    extensions                : gr_vk_extensions_t;
    physical_device_features  : pgr_vk_physicaldevicefeatures_t;
    physical_device_features2 : pgr_vk_physicaldevicefeatures2_t;
    get_context               : Pointer;
    get_proc                  : gr_vk_get_proc;
    protected_context         : _bool;
  end;
  pgr_vk_backendcontext_t = ^gr_vk_backendcontext_t;

  gr_vk_ycbcrconversioninfo_t = record
    format                        : gr_vk_format_t;
    external_format               : uint64_t;
    ycbcr_model                   : gr_vk_samplerycbcrmodelconversion_t;
    ycbcr_range                   : gr_vk_samplerycbcrrange_t;
    x_chroma_offset               : gr_vk_chromalocation_t;
    y_chroma_offset               : gr_vk_chromalocation_t;
    chroma_filter                 : gr_vk_filter_t;
    force_explicit_reconstruction : gr_vk_bool32_t;
    format_features               : gr_vk_formatfeatureflags_t;
  end;
  pgr_vk_ycbcrconversioninfo_t = ^gr_vk_ycbcrconversioninfo_t;

  gr_vk_imageinfo_t = record
    image                 : gr_vk_image_t;
    alloc                 : gr_vk_alloc_t;
    image_tiling          : gr_vk_imagetiling_t;
    image_layout          : gr_vk_imagelayout_t;
    format                : gr_vk_format_t;
    image_usage_flags     : gr_vk_imageusageflags_t;
    sample_count          : uint32_t;
    level_count           : uint32_t;
    current_queue_family  : uint32_t;
    protected_image       : _bool;
    ycbcr_conversion_info : gr_vk_ycbcrconversioninfo_t;
    sharing_mode          : gr_vk_sharingmode_t;
  end;
  pgr_vk_imageinfo_t = ^gr_vk_imageinfo_t;

  { modules/particles/include/sk4d_particles_types.h }

  sk_particleeffect_t = sk_handle_t;

  psk_particleeffect_t = ^sk_particleeffect_t;

  sk_particleuniform_t = record
    columns : int32_t;
    rows    : int32_t;
    slot    : int32_t;
  end;
  psk_particleuniform_t = ^sk_particleuniform_t;


  { modules/skottie/include/sk4d_skottie_types.h }

  sk_skottieanimation_t = sk_handle_t;

  psk_skottieanimation_t = ^sk_skottieanimation_t;


  { modules/skparagraph/include/sk4d_paragraph_types.h }

  sk_paragraph_t            = sk_handle_t;
  sk_paragraphbuilder_t     = sk_handle_t;
  sk_paragraphstyle_t       = sk_handle_t;
  sk_strutstyle_t           = sk_handle_t;
  sk_textstyle_t            = sk_handle_t;
  sk_typefacefontprovider_t = sk_handle_t;

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
    is_hard_break             : _bool;
    ascent                    : _double;
    descent                   : _double;
    height                    : _double;
    width                     : _double;
    left                      : _double;
    baseline                  : _double;
    line_number               : size_t;
  end;
  psk_metrics_t = ^sk_metrics_t;

  sk_paragraphvisitorinfo_t = record
    font        : sk_font_t;
    origin      : sk_point_t;
    advance_x   : float;
    count       : int32_t;
    glyphs      : puint16_t;
    positions   : psk_point_t;
    utf8_starts : puint32_t;
    flags       : uint32_t;
  end;
  psk_paragraphvisitorinfo_t = ^sk_paragraphvisitorinfo_t;

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
    blur_radius : _double;
  end;
  psk_textshadow_t = ^sk_textshadow_t;

  sk_paragraph_visit_proc = procedure (line_number: int32_t; const info: psk_paragraphvisitorinfo_t; context: Pointer); cdecl;


  { modules/skresources/include/sk4d_resources_types.h }

  sk_resourceprovider_t          = sk_handle_t;
  sk_resourceproviderbaseclass_t = sk_handle_t;

  psk_resourceprovider_t          = ^sk_resourceprovider_t;
  psk_resourceproviderbaseclass_t = ^sk_resourceproviderbaseclass_t;

  sk_resourceproviderbaseclass_procs_t = record
    load: function (context: Pointer; const path, name: MarshaledAString): sk_data_t; cdecl;
  end;
  psk_resourceproviderbaseclass_procs_t = ^sk_resourceproviderbaseclass_procs_t;


  { modules/skshaper/include/sk4d_shaper_types.h }

  sk_shaper_t = sk_handle_t;

  psk_shaper_t = ^sk_shaper_t;


  { modules/skunicode/include/sk4d_unicode_types.h }

  sk_unicode_t              = sk_handle_t;
  sk_unicodebreakiterator_t = sk_handle_t;

  psk_unicode_t              = ^sk_unicode_t;
  psk_unicodebreakiterator_t = ^sk_unicodebreakiterator_t;

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

  sk_unicodebreakiteratorelem_t = record
    position: int32_t;
    status: int32_t;
  end;
  psk_unicodebreakiteratorelem_t = ^sk_unicodebreakiteratorelem_t;

  sk_unicode_bidi_region_proc = procedure (start, &end: int32_t; level: uint8_t; context: Pointer); cdecl;
  sk_unicode_break_proc       = procedure (position, status: int32_t; context: Pointer); cdecl;
  sk_unicode_codepoint_proc   = procedure (uni_char: sk_unichar_t; start, &end: int32_t; context: Pointer); cdecl;


  { modules/svg/include/sk4d_svg_types.h }

  sk_svgdom_t  = sk_handle_t;
  sk_svgsvg_t  = sk_handle_t;
  sk_svgnode_t = sk_handle_t;

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



{$IFNDEF SK_STATIC_LIBRARY}
var
{$ENDIF}

  { include/c/gr4d_backendsemaphore.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_backendsemaphore_create     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): gr_backendsemaphore_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}gr4d_backendsemaphore_destroy    {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: gr_backendsemaphore_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}gr4d_backendsemaphore_init_vulkan{$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: gr_backendsemaphore_t; semaphore: gr_vk_semaphore_t); cdecl;


  { include/c/gr4d_backendsurface.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_backendrendertarget_create_gl         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(width, height, sample_count, stencil_bits: int32_t; const framebuffer_info: pgr_gl_framebufferinfo_t): gr_backendrendertarget_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_backendrendertarget_create_mtl        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(width, height: int32_t; const texture_info: pgr_mtl_textureinfo_t): gr_backendrendertarget_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_backendrendertarget_create_vk         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(width, height: int32_t; const image_info: pgr_vk_imageinfo_t): gr_backendrendertarget_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}gr4d_backendrendertarget_destroy           {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: gr_backendrendertarget_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_backendrendertarget_get_backend_api   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: gr_backendrendertarget_t): gr_backendapi_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_backendrendertarget_get_height        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: gr_backendrendertarget_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_backendrendertarget_get_sample_count  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: gr_backendrendertarget_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_backendrendertarget_get_stencil_bits  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: gr_backendrendertarget_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_backendrendertarget_get_width         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: gr_backendrendertarget_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_backendrendertarget_is_valid          {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: gr_backendrendertarget_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_backendtexture_create_gl              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(width, height: int32_t; is_mipmapped: _bool; const texture_info: pgr_gl_textureinfo_t): gr_backendtexture_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_backendtexture_create_mtl             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(width, height: int32_t; is_mipmapped: _bool; const texture_info: pgr_mtl_textureinfo_t): gr_backendtexture_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_backendtexture_create_vk              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(width, height: int32_t; const image_info: pgr_vk_imageinfo_t): gr_backendtexture_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}gr4d_backendtexture_destroy                {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: gr_backendtexture_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_backendtexture_get_backend_api        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: gr_backendtexture_t): gr_backendapi_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_backendtexture_get_gl_framebuffer_info{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: gr_backendtexture_t; out texture_info: gr_gl_textureinfo_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_backendtexture_get_height             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: gr_backendtexture_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_backendtexture_get_width              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: gr_backendtexture_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_backendtexture_has_mipmaps            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: gr_backendtexture_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_backendtexture_is_valid               {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: gr_backendtexture_t): _bool; cdecl;


  { include/c/gr4d_backendsurfacemutablestate.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_backendsurfacemutablestate_create {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(image_layout: gr_vk_imagelayout_t; queue_family_index: uint32_t): gr_backendsurfacemutablestate_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}gr4d_backendsurfacemutablestate_destroy{$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: gr_backendsurfacemutablestate_t); cdecl;


  { include/c/gr4d_contextoptions.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_persistentcachebaseclass_create   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(context: Pointer): gr_persistentcachebaseclass_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}gr4d_persistentcachebaseclass_destroy  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: gr_persistentcachebaseclass_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}gr4d_persistentcachebaseclass_set_procs{$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const procs: pgr_persistentcachebaseclass_procs_t); cdecl;


  { include/c/gr4d_directcontext.h }

  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}gr4d_directcontext_abandon_context                            {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: gr_directcontext_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_directcontext_create_texture                             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: gr_directcontext_t; width, height: int32_t; color_type: sk_colortype_t; is_mipmapped, is_renderable, is_protected: _bool): gr_backendtexture_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_directcontext_create_texture2                            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: gr_directcontext_t; width, height: int32_t; color_type: sk_colortype_t; color: sk_color_t; is_mipmapped, is_renderable, is_protected: _bool): gr_backendtexture_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_directcontext_create_texture3                            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: gr_directcontext_t; width, height: int32_t; color_type: sk_colortype_t; const color: psk_color4f_t; is_mipmapped, is_renderable, is_protected: _bool): gr_backendtexture_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}gr4d_directcontext_delete_texture                             {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: gr_directcontext_t; texture: gr_backendtexture_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}gr4d_directcontext_dump_memory_statistics                     {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: gr_directcontext_t; trace_memory_dump: sk_tracememorydump_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}gr4d_directcontext_flush                                      {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: gr_directcontext_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}gr4d_directcontext_flush_and_submit                           {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: gr_directcontext_t; sync_cpu: _bool); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}gr4d_directcontext_free_gpu_resources                         {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: gr_directcontext_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_directcontext_get_backend_api                            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: gr_directcontext_t): gr_backendapi_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_directcontext_get_max_surface_sample_count_for_color_type{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: gr_directcontext_t; color_type: sk_colortype_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_directcontext_get_resource_cache_limit                   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: gr_directcontext_t): size_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}gr4d_directcontext_get_resource_cache_usage                   {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: gr_directcontext_t; out resources: int32_t; out resources_bytes: size_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_directcontext_is_abandoned                               {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: gr_directcontext_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_directcontext_make_gl                                    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const gl_interface: gr_gl_interface_t; const options: pgr_contextoptions_t): gr_directcontext_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_directcontext_make_metal                                 {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const backend_context: pgr_mtl_backendcontext_t; const options: pgr_contextoptions_t): gr_directcontext_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_directcontext_make_vulkan                                {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const backend_context: pgr_vk_backendcontext_t; const options: pgr_contextoptions_t): gr_directcontext_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}gr4d_directcontext_perform_deferred_cleanup                   {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: gr_directcontext_t; milliseconds: int64_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}gr4d_directcontext_purge_unlocked_resources                   {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: gr_directcontext_t; scratch_resources_only: _bool); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}gr4d_directcontext_purge_unlocked_resources2                  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: gr_directcontext_t; bytes_to_purge: size_t; prefer_scratch_resources: _bool); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}gr4d_directcontext_release_resources_and_abandon_context      {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: gr_directcontext_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}gr4d_directcontext_reset_context                              {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: gr_directcontext_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}gr4d_directcontext_set_resource_cache_limit                   {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: gr_directcontext_t; value: size_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_directcontext_submit                                     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: gr_directcontext_t; sync_cpu: _bool): _bool; cdecl;


  { include/c/gr4d_gl_interface.h }

  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}gr4d_gl_interface_has_extension       {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const self: gr_gl_interface_t; const name: MarshaledAString): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}gr4d_gl_interface_make_assembled      {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(context: Pointer; proc: gr_gl_get_proc): gr_gl_interface_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}gr4d_gl_interface_make_assembled_gl   {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(context: Pointer; proc: gr_gl_get_proc): gr_gl_interface_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}gr4d_gl_interface_make_assembled_gles {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(context: Pointer; proc: gr_gl_get_proc): gr_gl_interface_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}gr4d_gl_interface_make_assembled_webgl{$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(context: Pointer; proc: gr_gl_get_proc): gr_gl_interface_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}gr4d_gl_interface_make_native         {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(): gr_gl_interface_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}gr4d_gl_interface_validate            {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const self: gr_gl_interface_t): _bool; cdecl;


  { include/c/gr4d_shadererrorhandler.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_shadererrorhandlerbaseclass_create   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(context: Pointer): gr_shadererrorhandlerbaseclass_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}gr4d_shadererrorhandlerbaseclass_destroy  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: gr_shadererrorhandlerbaseclass_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}gr4d_shadererrorhandlerbaseclass_set_procs{$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const procs: pgr_shadererrorhandlerbaseclass_procs_t); cdecl;


  { include/c/gr4d_vk_extensions.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_vk_extensions_create       {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): gr_vk_extensions_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}gr4d_vk_extensions_destroy      {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: gr_vk_extensions_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}gr4d_vk_extensions_has_extension{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: gr_vk_extensions_t; const name: MarshaledAString; min_api_version: uint32_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}gr4d_vk_extensions_init         {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: gr_vk_extensions_t; context: Pointer; proc: gr_vk_get_proc; instance: gr_vk_instance_t; physical_device: gr_vk_physicaldevice_t; instance_extension_count: int32_t; const instance_extensions: PMarshaledAString; device_extension_count: int32_t; const device_extensions: PMarshaledAString); cdecl;


  { include/c/sk4d_animatedwebpencoder.h }

  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_animatedwebpencoder_encode_to_file  {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const file_name: MarshaledAString; const src: psk_frame_t; count: size_t; quality: int32_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_animatedwebpencoder_encode_to_stream{$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(w_stream: sk_wstream_t; const src: psk_frame_t; count: size_t; quality: int32_t): _bool; cdecl;


  { include/c/sk4d_blender.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_blender_make_arithmetic{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(k1, k2, k3, k4: float; enforce_premultiplied_color: _bool): sk_blender_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_blender_make_mode      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(mode: sk_blendmode_t): sk_blender_t; cdecl;


  { include/c/sk4d_canvas.h }

  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_clear                     {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; color: sk_color_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_clear2                    {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const color: psk_color4f_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_destroy                   {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_discard                   {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_clip_path                 {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const path: sk_path_t; op: sk_clipop_t; anti_alias: _bool); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_clip_rect                 {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const rect: psk_rect_t; op: sk_clipop_t; anti_alias: _bool); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_clip_region               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const region: sk_region_t; op: sk_clipop_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_clip_rrect                {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const rrect: sk_rrect_t; op: sk_clipop_t; anti_alias: _bool); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_clip_shader               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; shader: sk_shader_t; op: sk_clipop_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_concat                    {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const matrix: psk_matrix44_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_concat2                   {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const matrix: psk_matrix_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_annotation           {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const rect: psk_rect_t; const key: MarshaledAString; const value: Pointer; size: size_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_arc                  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const oval: psk_rect_t; start_angle, sweep_angle: float; use_center: _bool; const paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_atlas                {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const atlas: sk_image_t; const transforms: psk_rotationscalematrix_t; const sprites: psk_rect_t; const colors: psk_color_t; count: int32_t; blend_mode: sk_blendmode_t; const sampling: psk_samplingoptions_t; const cull_rect: psk_rect_t; const paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_circle               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const center: psk_point_t; radius: float; const paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_color                {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; color: sk_color_t; blend_mode: sk_blendmode_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_color2               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const color: psk_color4f_t; blend_mode: sk_blendmode_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_glyphs               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; count: int32_t; const glyphs: psk_glyphid_t; const positions: psk_point_t; const origin: psk_point_t; const font: sk_font_t; const paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_glyphs2              {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; count: int32_t; const glyphs: psk_glyphid_t; const matrices: psk_rotationscalematrix_t; const origin: psk_point_t; const font: sk_font_t; const paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_image                {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const image: sk_image_t; x, y: float; const sampling: psk_samplingoptions_t; const paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_image_lattice        {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const image: sk_image_t; const lattice: psk_lattice_t; const dest: psk_rect_t; filter_mode: sk_filtermode_t; const paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_image_nine           {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const image: sk_image_t; const center: psk_irect_t; const dest: psk_rect_t; filter_mode: sk_filtermode_t; const paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_image_rect           {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const image: sk_image_t; const src, dest: psk_rect_t; const sampling: psk_samplingoptions_t; const paint: sk_paint_t; constraint: sk_srcrectconstraint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_line                 {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const point1, point2: psk_point_t; paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_oval                 {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const oval: psk_rect_t; const paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_paint                {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_patch                {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const cubics: psk_point_t; const colors: psk_color_t; const tex_coords: psk_point_t; blend_mode: sk_blendmode_t; const paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_path                 {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const path: sk_path_t; const paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_picture              {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const picture: sk_picture_t; const matrix: psk_matrix_t; const paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_point                {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const point: psk_point_t; const paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_points               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; mode: sk_drawpointsmode_t; count: size_t; const points: psk_point_t; const paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_rect                 {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const rect: psk_rect_t; const paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_region               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const region: sk_region_t; const paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_rrect                {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const rrect: sk_rrect_t; const paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_rrect2               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const rect: psk_rect_t; radius_x, radius_y: float; const paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_rrect_difference     {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const outer, inner: sk_rrect_t; const paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_simple_text          {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const text: Pointer; size: size_t; encoding: sk_textencoding_t; x, y: float; const font: sk_font_t; const paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_text_blob            {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const text_blob: sk_textblob_t; x, y: float; const paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_draw_vertices             {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const vertices: sk_vertices_t; blend_mode: sk_blendmode_t; const paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_get_base_props            {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_canvas_t; out result: sk_surfaceprops_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_get_device_clip_bounds    {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_canvas_t; out result: sk_irect_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_get_local_clip_bounds     {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_canvas_t; out result: sk_rect_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_get_local_to_device       {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_canvas_t; out result: sk_matrix44_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_get_local_to_device_as_3x3{$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_canvas_t; out result: sk_matrix_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_get_top_props             {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_canvas_t; out result: sk_surfaceprops_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_canvas_get_save_count            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_canvas_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_canvas_make_surface              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_canvas_t; const image_info: psk_imageinfo_t; const props: psk_surfaceprops_t): sk_surface_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_canvas_quick_reject              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_canvas_t; const rect: psk_rect_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_canvas_quick_reject2             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_canvas_t; const path: sk_path_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_reset_matrix              {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_restore                   {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_restore_to_count          {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; save_count: int32_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_rotate                    {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; degrees: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_rotate2                   {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; degrees, px, py: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_canvas_save                      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_canvas_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_canvas_save_layer                {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_canvas_t; const bounds: psk_rect_t; const paint: sk_paint_t; const backdrop: sk_imagefilter_t; flags: uint32_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_canvas_save_layer_alpha          {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_canvas_t; const bounds: psk_rect_t; alpha: uint8_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_scale                     {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; sx, sy: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_set_matrix                {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const matrix: psk_matrix44_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_set_matrix2               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; const matrix: psk_matrix_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_skew                      {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; kx, ky: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_canvas_translate                 {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_canvas_t; dx, dy: float); cdecl;


  { include/c/sk4d_codec.h }

  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_codec_destroy                   {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(codec: sk_codec_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_codec_get_dimensions            {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_codec_t; out result: sk_isize_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_codec_get_encoded_image_format  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_codec_t): sk_encodedimageformat_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_codec_get_image                 {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_codec_t; color_type: sk_colortype_t; alpha_type: sk_alphatype_t; color_space: sk_colorspace_t): sk_image_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_codec_get_pixels                {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_codec_t; pixels: Pointer; row_bytes: size_t; color_type: sk_colortype_t; alpha_type: sk_alphatype_t; color_space: sk_colorspace_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_codec_make_from_file            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const file_name: MarshaledAString): sk_codec_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_codec_make_from_stream          {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(stream: sk_stream_t): sk_codec_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_codec_make_with_copy            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const data: Pointer; size: size_t): sk_codec_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_codec_make_without_copy         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const data: Pointer; size: size_t): sk_codec_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_animcodecplayer_destroy         {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_animcodecplayer_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_animcodecplayer_get_dimensions  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_animcodecplayer_t; out result: sk_isize_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_animcodecplayer_get_duration    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_animcodecplayer_t): uint32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_animcodecplayer_get_frame       {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_animcodecplayer_t): sk_image_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_animcodecplayer_make_from_file  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const file_name: MarshaledAString): sk_animcodecplayer_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_animcodecplayer_make_from_stream{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(stream: sk_stream_t): sk_animcodecplayer_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_animcodecplayer_seek            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_animcodecplayer_t; milliseconds: uint32_t): _bool; cdecl;


  { include/c/sk4d_colorfilter.h }

  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_colorfilter_make_blend               {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(color: sk_color_t; mode: sk_blendmode_t): sk_colorfilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_colorfilter_make_blend2              {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const color: psk_color4f_t; color_space: sk_colorspace_t; mode: sk_blendmode_t): sk_colorfilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_colorfilter_make_compose             {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(outer, inner: sk_colorfilter_t): sk_colorfilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_colorfilter_make_high_contrast       {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const config: psk_highcontrastconfig_t): sk_colorfilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_colorfilter_make_hsla_matrix         {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const matrix: psk_colormatrix_t): sk_colorfilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_colorfilter_make_lighting            {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(multiply, add: sk_color_t): sk_colorfilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_colorfilter_make_linear_to_srgb_gamma{$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(): sk_colorfilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_colorfilter_make_luma_color          {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(): sk_colorfilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_colorfilter_make_matrix              {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const matrix: psk_colormatrix_t): sk_colorfilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_colorfilter_make_overdraw            {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const colors: psk_color_t): sk_colorfilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_colorfilter_make_table               {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const tablea_a, tablea_r, tablea_g, tablea_b: puint8_t): sk_colorfilter_t; cdecl;


  { include/c/sk4d_colorspace.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_colorspace_gamma_close_to_srgb      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_colorspace_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_colorspace_gamma_is_linear          {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_colorspace_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_colorspace_is_equal                 {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self, color_space: sk_colorspace_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_colorspace_is_numerical_transfer_fn {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_colorspace_t; out transfer_function: sk_colorspacetransferfn_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_colorspace_is_srgb                  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_colorspace_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_colorspace_make                     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const profile: sk_colorspaceiccprofile_t): sk_colorspace_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_colorspace_make_linear_gamma        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_colorspace_t): sk_colorspace_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_colorspace_make_rgb                 {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const transfer_function: psk_colorspacetransferfn_t; const xyz: psk_colorspacexyz_t): sk_colorspace_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_colorspace_make_srgb                {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): sk_colorspace_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_colorspace_make_srgb_gamma          {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_colorspace_t): sk_colorspace_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_colorspace_make_srgb_linear         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): sk_colorspace_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_colorspace_ref                      {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_colorspace_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_colorspace_to_profile               {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_colorspace_t): sk_colorspaceiccprofile_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_colorspace_to_xyz                   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_colorspace_t; out xyz: sk_colorspacexyz_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_colorspace_unref                    {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_colorspace_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_colorspaceiccprofile_destroy        {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_colorspaceiccprofile_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_colorspaceiccprofile_get_buffer     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_colorspaceiccprofile_t; size: puint32_t): puint8_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_colorspaceiccprofile_make_with_parse{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const buffer: Pointer; size: size_t): sk_colorspaceiccprofile_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_colorspaceiccprofile_to_xyz         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_colorspaceiccprofile_t; out dest: sk_colorspacexyz_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_colorspaceprimaries_to_xyz          {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: psk_colorspaceprimaries_t; out xyz: sk_colorspacexyz_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_colorspacetransferfn_invert         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: psk_colorspacetransferfn_t; out transfer_function: sk_colorspacetransferfn_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_colorspacetransferfn_transform      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: psk_colorspacetransferfn_t; x: float): float; cdecl;


  { include/c/sk4d_data.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_data_make_empty    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): sk_data_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_data_make_with_copy{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const data: Pointer; size: size_t): sk_data_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_data_ref           {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_data_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_data_unref         {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_data_t); cdecl;


  { include/c/sk4d_document.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_document_begin_page{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_document_t; width, height: float; const content: psk_rect_t): sk_canvas_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_document_close     {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_document_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_document_end_page  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_document_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_document_make_pdf  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(w_stream: sk_wstream_t): sk_document_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_document_make_pdf2 {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(w_stream: sk_wstream_t; const metadata: psk_pdfmetadata_t): sk_document_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_document_make_xps  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(w_stream: sk_wstream_t; dpi: float): sk_document_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_document_terminate {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_document_t); cdecl;


  { include/c/sk4d_font.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_font_create                  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(typeface: sk_typeface_t; size, sx, kx: float): sk_font_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_font_create2                 {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const font: sk_font_t): sk_font_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_font_destroy                 {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_font_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_font_get_baseline_snap       {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_font_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_font_get_edging              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_font_t): sk_fontedging_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_font_get_embedded_bitmaps    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_font_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_font_get_embolden            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_font_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_font_get_force_auto_hinting  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_font_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_font_get_glyphs              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_font_t; const text: Pointer; size: size_t; encoding: sk_textencoding_t; result: psk_glyphid_t; max_count: int32_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_font_get_glyphs_count        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_font_t; const text: Pointer; size: size_t; encoding: sk_textencoding_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_font_get_hinting             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_font_t): sk_fonthinting_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_font_get_horizontal_positions{$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; result: pfloat; origin: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_font_get_intercepts          {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; const positions: psk_point_t; const bounds: pfloat; result: pfloat; const paint: sk_paint_t): size_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_font_get_linear_metrics      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_font_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_font_get_metrics             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_font_t; metrics: psk_fontmetrics_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_font_get_path                {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_font_t; glyph: sk_glyphid_t): sk_path_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_font_get_paths               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; proc: sk_font_path_proc; proc_context: Pointer); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_font_get_positions           {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; result: psk_point_t; const origin: psk_point_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_font_get_scale_x             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_font_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_font_get_size                {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_font_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_font_get_skew_x              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_font_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_font_get_subpixel            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_font_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_font_get_typeface            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_font_t): sk_typeface_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_font_get_typeface_or_default {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_font_t): sk_typeface_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_font_get_widths_bounds       {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; widths: pfloat; bounds: psk_rect_t; const paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_font_is_equal                {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self, font: sk_font_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_font_measure_text            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_font_t; const text: Pointer; size: size_t; encoding: sk_textencoding_t; bounds: psk_rect_t; const paint: sk_paint_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_font_set_baseline_snap       {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_font_t; value: _bool); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_font_set_edging              {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_font_t; value: sk_fontedging_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_font_set_embedded_bitmaps    {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_font_t; value: _bool); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_font_set_embolden            {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_font_t; value: _bool); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_font_set_force_auto_hinting  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_font_t; value: _bool); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_font_set_hinting             {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_font_t; value: sk_fonthinting_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_font_set_linear_metrics      {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_font_t; value: _bool); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_font_set_scale_x             {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_font_t; value: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_font_set_size                {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_font_t; value: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_font_set_skew_x              {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_font_t; value: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_font_set_subpixel            {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_font_t; value: _bool); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_font_set_typeface            {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_font_t; typeface: sk_typeface_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_font_unichar_to_glyph        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_font_t; uni_char: sk_unichar_t): sk_glyphid_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_font_unichars_to_glyphs      {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_font_t; const uni_chars: psk_unichar_t; count: int32_t; result: psk_glyphid_t); cdecl;


  { include/c/sk4d_graphics.h }

  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_graphics_allow_jit                                      {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_graphics_dump_memory_statistics                         {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(trace_memory_dump: sk_tracememorydump_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_graphics_get_font_cache_count_limit                     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_graphics_get_font_cache_count_used                      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_graphics_get_font_cache_limit                           {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): size_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_graphics_get_font_cache_used                            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): size_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_graphics_get_resource_cache_single_allocation_byte_limit{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): size_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_graphics_get_resource_cache_total_byte_limit            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): size_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_graphics_get_resource_cache_total_bytes_used            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): size_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_graphics_init                                           {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_graphics_purge_all_caches                               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_graphics_purge_font_cache                               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_graphics_purge_resource_cache                           {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_graphics_set_font_cache_count_limit                     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(value: int32_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_graphics_set_font_cache_limit                           {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(value: size_t): size_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_graphics_set_resource_cache_single_allocation_byte_limit{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(value: size_t): size_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_graphics_set_resource_cache_total_byte_limit            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(value: size_t): size_t; cdecl;


  { include/c/sk4d_image.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_encode_to_file           {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_image_t; const file_name: MarshaledAString; format: sk_encodedimageformat_t; quality: int32_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_encode_to_stream         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_image_t; w_stream: sk_wstream_t; format: sk_encodedimageformat_t; quality: int32_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_get_alpha_type           {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_image_t): sk_alphatype_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_get_color_space          {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_image_t): sk_colorspace_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_get_color_type           {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_image_t): sk_colortype_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_get_height               {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_image_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_image_get_image_info           {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_image_t; out result: sk_imageinfo_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_get_unique_id            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_image_t): uint32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_get_width                {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_image_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_is_lazy_generated        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_image_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_is_texture_backed        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_image_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_is_valid                 {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_image_t; context: gr_directcontext_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_make_cross_context       {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(context: gr_directcontext_t; const pixmap: sk_pixmap_t; build_mips, limit_to_max_texture_size: _bool): sk_image_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_make_from_adopted_texture{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(context: gr_directcontext_t; const texture: gr_backendtexture_t; origin: gr_surfaceorigin_t; color_type: sk_colortype_t; alpha_type: sk_alphatype_t; color_space: sk_colorspace_t): sk_image_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_make_from_encoded_file   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const file_name: MarshaledAString): sk_image_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_make_from_encoded_stream {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(stream: sk_stream_t): sk_image_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_make_from_picture        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(picture: sk_picture_t; const dimensions: psk_isize_t; const matrix: psk_matrix_t; const paint: sk_paint_t; color_space: sk_colorspace_t; const props: psk_surfaceprops_t): sk_image_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_make_from_raster         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const pixmap: sk_pixmap_t; proc: sk_image_raster_release_proc; proc_context: Pointer): sk_image_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_make_from_texture        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(context: gr_directcontext_t; const texture: gr_backendtexture_t; origin: gr_surfaceorigin_t; color_type: sk_colortype_t; alpha_type: sk_alphatype_t; color_space: sk_colorspace_t; proc: sk_image_texture_release_proc; proc_context: Pointer): sk_image_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_make_non_texture_image   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_image_t): sk_image_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_make_raster_copy         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const pixmap: sk_pixmap_t): sk_image_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_make_raster_image        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_image_t): sk_image_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_make_raw_shader          {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_image_t; tile_mode_x, tile_mode_y: sk_tilemode_t; const sampling: psk_samplingoptions_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_make_shader              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_image_t; tile_mode_x, tile_mode_y: sk_tilemode_t; const sampling: psk_samplingoptions_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_make_subset              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_image_t; const subset: psk_irect_t; context: gr_directcontext_t): sk_image_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_make_texture_image       {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_image_t; context: gr_directcontext_t; is_mipmapped: _bool): sk_image_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_make_with_filter         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_image_t; context: gr_directcontext_t; const filter: sk_imagefilter_t; const subset, clip_bounds: psk_irect_t; out out_subset: sk_irect_t; out offset: sk_ipoint_t): sk_image_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_peek_pixels              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_image_t): sk_pixmap_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_read_pixels              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_image_t; context: gr_directcontext_t; const dest: sk_pixmap_t; src_x, src_y: int32_t; caching_hint: sk_imagecachinghint_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_image_scale_pixels             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_image_t; const dest: sk_pixmap_t; const sampling: psk_samplingoptions_t; caching_hint: sk_imagecachinghint_t): _bool; cdecl;


  { include/c/sk4d_imageencoder.h }

  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_imageencoder_encode_to_file  {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const file_name: MarshaledAString; const src: sk_pixmap_t; format: sk_encodedimageformat_t; quality: int32_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_imageencoder_encode_to_stream{$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(w_stream: sk_wstream_t; const src: sk_pixmap_t; format: sk_encodedimageformat_t; quality: int32_t): _bool; cdecl;


  { include/c/sk4d_imagefilter.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_can_compute_fast_bounds  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_imagefilter_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_imagefilter_compute_fast_bounds      {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_imagefilter_t; const bounds: psk_rect_t; out result: sk_rect_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_alpha_threshold     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const region: sk_region_t; inner_min, outer_max: float; input: sk_imagefilter_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_arithmetic          {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(k1, k2, k3, k4: float; enforce_premultiplied_color: _bool; background, foreground: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_blend               {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(mode: sk_blendmode_t; background, foreground: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_blur                {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(sigma_x, sigma_y: float; tile_mode: sk_tilemode_t; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_colorfilter         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(color_filter: sk_colorfilter_t; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_compose             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(inner, outer: sk_imagefilter_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_dilate              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(radius_x, radius_y: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_displacement_map    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(x_channel_selector, y_channel_selector: sk_colorchannel_t; scale: float; displacement, input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_distant_lit_diffuse {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const direction: psk_point3_t; light_color: sk_color_t; surface_scale, kd: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_distant_lit_specular{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const direction: psk_point3_t; light_color: sk_color_t; surface_scale, ks, shininess: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_drop_shadow         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(dx, dy, sigma_x, sigma_y: float; color: sk_color_t; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_drop_shadow_only    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(dx, dy, sigma_x, sigma_y: float; color: sk_color_t; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_erode               {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(radius_x, radius_y: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_image               {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(image: sk_image_t; const src, dest: psk_rect_t; const sampling: psk_samplingoptions_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_magnifier           {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const src: psk_rect_t; inset: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_matrix_convolution  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const kernel_size: psk_isize_t; const kernel: pfloat; gain, bias: float; const kernel_offset: psk_ipoint_t; tile_mode: sk_tilemode_t; convolve_alpha: _bool; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_matrix_transform    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const matrix: psk_matrix_t; const sampling: psk_samplingoptions_t; input: sk_imagefilter_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_merge               {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const filters: psk_imagefilter_t; count: int32_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_offset              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(dx, dy: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_picture             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(picture: sk_picture_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_point_lit_diffuse   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const location: psk_point3_t; light_color: sk_color_t; surface_scale, kd: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_point_lit_specular  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const location: psk_point3_t; light_color: sk_color_t; surface_scale, ks, shininess: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_runtime_shader      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const effect_builder: sk_runtimeshaderbuilder_t; const child: MarshaledAString; input: sk_imagefilter_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_runtime_shader2     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const effect_builder: sk_runtimeshaderbuilder_t; const children: PMarshaledAString; inputs: psk_imagefilter_t; count: int32_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_shader              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(shader: sk_shader_t; dither: _bool; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_spot_lit_diffuse    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const location, target: psk_point3_t; falloff_exponent, cutoff_angle: float; light_color: sk_color_t; surface_scale, kd: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_spot_lit_specular   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const location, target: psk_point3_t; falloff_exponent, cutoff_angle: float; light_color: sk_color_t; surface_scale, ks, shininess: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_tile                {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const src, dest: psk_rect_t; input: sk_imagefilter_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_imagefilter_make_with_local_matrix   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_imagefilter_t; const local_matrix: psk_matrix_t): sk_imagefilter_t; cdecl;



  { include/c/sk4d_maskfilter.h }

  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_maskfilter_make_blur       {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(style: sk_blurstyle_t; sigma: float; respect_ctm: _bool): sk_maskfilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_maskfilter_make_shader     {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(shader: sk_shader_t): sk_maskfilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_maskfilter_make_table      {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const table: puint8_t): sk_maskfilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_maskfilter_make_table_clip {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(min, max: uint8_t): sk_maskfilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_maskfilter_make_table_gamma{$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(gamma: float): sk_maskfilter_t; cdecl;


  { include/c/sk4d_paint.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paint_create            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): sk_paint_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paint_create2           {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const paint: sk_paint_t): sk_paint_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paint_destroy           {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paint_get_alpha         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paint_t): uint8_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paint_get_alphaf        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paint_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paint_get_anti_alias    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paint_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paint_get_blender       {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paint_t): sk_blender_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paint_get_color         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paint_t): sk_color_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paint_get_colorf        {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_paint_t; out result: sk_color4f_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paint_get_color_filter  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paint_t): sk_colorfilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paint_get_dither        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paint_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paint_get_fill_path     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paint_t; const path: sk_path_t; const cull_rect: psk_rect_t; res_scale: float): sk_path_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paint_get_image_filter  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paint_t): sk_imagefilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paint_get_mask_filter   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paint_t): sk_maskfilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paint_get_path_effect   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paint_t): sk_patheffect_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paint_get_shader        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paint_t): sk_shader_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paint_get_stroke_cap    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paint_t): sk_strokecap_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paint_get_stroke_join   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paint_t): sk_strokejoin_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paint_get_stroke_miter  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paint_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paint_get_stroke_width  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paint_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paint_get_style         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paint_t): sk_paintstyle_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paint_reset             {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paint_set_alpha         {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paint_t; value: uint8_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paint_set_alphaf        {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paint_t; value: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paint_set_antialias     {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paint_t; value: _bool); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paint_set_argb          {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paint_t; a, r, g, b: uint8_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paint_set_blender       {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paint_t; value: sk_blender_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paint_set_color         {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paint_t; value: sk_color_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paint_set_colorf        {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paint_t; const value: psk_color4f_t; color_space: sk_colorspace_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paint_set_color_filter  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paint_t; value: sk_colorfilter_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paint_set_dither        {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paint_t; value: _bool); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paint_set_image_filter  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paint_t; value: sk_imagefilter_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paint_set_mask_filter   {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paint_t; value: sk_maskfilter_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paint_set_path_effect   {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paint_t; value: sk_patheffect_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paint_set_shader        {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paint_t; value: sk_shader_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paint_set_stroke_cap    {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paint_t; value: sk_strokecap_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paint_set_stroke_join   {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paint_t; value: sk_strokejoin_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paint_set_stroke_miter  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paint_t; value: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paint_set_stroke_width  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paint_t; value: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paint_set_style         {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paint_t; value: sk_paintstyle_t); cdecl;


  { include/c/sk4d_path.h }

  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_opbuilder_add              {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_opbuilder_t; const path: sk_path_t; op: sk_pathop_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_opbuilder_create           {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): sk_opbuilder_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_opbuilder_destroy          {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_opbuilder_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_opbuilder_detach           {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_opbuilder_t): sk_path_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_path_contains              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_path_t; x, y: float): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_path_convert_conic_to_quads{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const point1, point2, point3: psk_point_t; weight: float; points: psk_point_t; power2: int32_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_path_create                {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const svg: MarshaledAString): sk_path_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_path_create2               {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(stream: sk_stream_t): sk_path_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_path_destroy               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_path_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_path_get_bounds            {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_path_t; out result: sk_rect_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_path_get_fill_type         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_path_t): sk_pathfilltype_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_path_get_last_point        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_path_t; out result: sk_point_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_path_get_segment_masks     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_path_t): uint32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_path_get_tight_bounds      {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_path_t; out result: sk_rect_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_path_interpolate           {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self, ending: sk_path_t; weight: float): sk_path_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_path_is_convex             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_path_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_path_is_empty              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_path_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_path_is_finite             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_path_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_path_is_interpolatable     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self, path: sk_path_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_path_is_last_contour_closed{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_path_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_path_is_line               {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_path_t; lines: psk_point_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_path_is_oval               {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_path_t; oval: psk_rect_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_path_is_rect               {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_path_t; rect: psk_rect_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_path_is_rrect              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_path_t; rrect: sk_rrect_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_path_op                    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self, path: sk_path_t; op: sk_pathop_t): sk_path_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_path_serialize_to_stream   {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_path_t; w_stream: sk_wstream_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_path_to_svg                {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_path_t): sk_string_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_path_transform             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_path_t; const matrix: psk_matrix_t): sk_path_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pathiterator_create        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const path: sk_path_t; force_close: _bool): sk_pathiterator_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathiterator_destroy       {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathiterator_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pathiterator_next          {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_pathiterator_t; out elem: sk_pathiteratorelem_t): _bool; cdecl;


  { include/c/sk4d_pathbuilder.h }

  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_add_arc                {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t; const oval: psk_rect_t; start_angle, sweep_angle: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_add_circle             {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t; center_x, center_y, radius: float; direction: sk_pathdirection_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_add_oval               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t; const oval: psk_rect_t; direction: sk_pathdirection_t; start_index: uint32_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_add_path               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t; const path: sk_path_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_add_polygon            {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t; polygon: psk_point_t; count: int32_t; is_closed: _bool); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_add_rect               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t; const rect: psk_rect_t; direction: sk_pathdirection_t; start_index: uint32_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_add_rrect              {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t; const rrect: sk_rrect_t; direction: sk_pathdirection_t; start_index: uint32_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_arc_to                 {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t; const radius: psk_point_t; x_axis_rotate: float; large_arc: sk_patharcsize_t; sweep: sk_pathdirection_t; const xy: psk_point_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_arc_to2                {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t; const oval: psk_rect_t; start_angle, sweep_angle: float; force_move_to: _bool); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_arc_to3                {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t; const point1, point2: psk_point_t; radius: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_close                  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_conic_to               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t; const point1, point2: psk_point_t; weight: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pathbuilder_create                 {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): sk_pathbuilder_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pathbuilder_create2                {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const path_builder: sk_pathbuilder_t): sk_pathbuilder_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_cubic_to               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t; const point1, point2, point3: psk_point_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_destroy                {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pathbuilder_detach                 {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_pathbuilder_t): sk_path_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_get_bounds             {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_pathbuilder_t; out result: sk_rect_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pathbuilder_get_fill_type          {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_pathbuilder_t): sk_pathfilltype_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_inc_reserve            {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t; extra_point_count, extra_verb_count: int32_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_line_to                {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t; const cpoint: psk_point_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_move_to                {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t; const cpoint: psk_point_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_offset                 {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t; dx, dy: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_polyline_to            {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t; const points: psk_point_t; count: int32_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_quad_to                {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t; const point1, point2: psk_point_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_r_conic_to             {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t; const point1, point2: psk_point_t; weight: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_r_cubic_to             {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t; const point1, point2, point3: psk_point_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_r_line_to              {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t; const point: psk_point_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_r_quad_to              {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t; const point1, point2: psk_point_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_reset                  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_set_filltype           {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t; value: sk_pathfilltype_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pathbuilder_snapshot               {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_pathbuilder_t): sk_path_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathbuilder_toggle_inverse_filltype{$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathbuilder_t); cdecl;


  { include/c/sk4d_patheffect.h }

  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_patheffect_make_1dpath         {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const path: sk_path_t; advance, phase: float; style: sk_patheffect1dstyle_t): sk_patheffect_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_patheffect_make_2dline         {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(width: float; const matrix: psk_matrix_t): sk_patheffect_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_patheffect_make_2dpath         {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const matrix: psk_matrix_t; const path: sk_path_t): sk_patheffect_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_patheffect_make_compose        {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(outer, inner: sk_patheffect_t): sk_patheffect_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_patheffect_make_corner         {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(radius: float): sk_patheffect_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_patheffect_make_dash           {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const intervals: pfloat; count: int32_t; phase: float): sk_patheffect_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_patheffect_make_discrete       {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(seg_length, deviation: float; seed_assist: uint32_t): sk_patheffect_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_patheffect_make_matrix         {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const matrix: psk_matrix_t): sk_patheffect_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_patheffect_make_merge          {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(effect1, effect2: sk_patheffect_t; op: sk_pathop_t): sk_patheffect_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_patheffect_make_stroke         {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(width: float; join: sk_strokejoin_t; cap: sk_strokecap_t; miter: float): sk_patheffect_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_patheffect_make_stroke_and_fill{$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(): sk_patheffect_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_patheffect_make_sum            {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(effect1, effect2: sk_patheffect_t): sk_patheffect_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_patheffect_make_translate      {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(dx, dy: float): sk_patheffect_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_patheffect_make_trim           {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(start, stop: float; mode: sk_patheffecttrimmode_t): sk_patheffect_t; cdecl;


  { include/c/sk4d_pathmeasure.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pathmeasure_create                  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const path: sk_path_t; force_closed: _bool; res_scale: float): sk_pathmeasure_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pathmeasure_destroy                 {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pathmeasure_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pathmeasure_get_length              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_pathmeasure_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pathmeasure_get_matrix              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_pathmeasure_t; distance: float; out matrix: sk_matrix_t; matrix_flags: uint32_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pathmeasure_get_position_and_tangent{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_pathmeasure_t; distance: float; out position: sk_point_t; out tangent: sk_vector_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pathmeasure_get_segment             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_pathmeasure_t; start, stop: float; start_with_move_to: _bool): sk_path_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pathmeasure_is_closed               {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_pathmeasure_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pathmeasure_next_contour            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_pathmeasure_t): _bool; cdecl;


  { include/c/sk4d_picture.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_picture_approximate_bytes_used{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_picture_t): size_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_picture_approximate_op_count  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_picture_t; nested: _bool): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_picture_get_cull_rect         {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_picture_t; out result: sk_rect_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_picture_make_from_stream      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(stream: sk_stream_t): sk_picture_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_picture_make_shader           {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_picture_t; tile_mode_x, tile_mode_y: sk_tilemode_t; filter_mode: sk_filtermode_t; const local_matrix: psk_matrix_t; const tile_rect: psk_rect_t): sk_shader_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_picture_playback              {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_picture_t; canvas: sk_canvas_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_picture_serialize_to_stream   {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_picture_t; w_stream: sk_wstream_t); cdecl;


  { include/c/sk4d_picturerecorder.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_picturerecorder_begin_recording  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_picturerecorder_t; const bounds: psk_rect_t): sk_canvas_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_picturerecorder_create           {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): sk_picturerecorder_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_picturerecorder_destroy          {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_picturerecorder_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_picturerecorder_finish_recording {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_picturerecorder_t): sk_picture_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_picturerecorder_finish_recording2{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_picturerecorder_t; const cull_rect: psk_rect_t): sk_picture_t; cdecl;


  { include/c/sk4d_pixmap.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pixmap_create         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const image_info: psk_imageinfo_t; const pixels: Pointer; row_bytes: size_t): sk_pixmap_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pixmap_destroy        {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pixmap_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pixmap_erase          {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_pixmap_t; color: sk_color_t; const area: psk_irect_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pixmap_erase2         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_pixmap_t; const color: psk_color4f_t; color_space: sk_colorspace_t; const area: psk_irect_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pixmap_extract_subset {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_pixmap_t; dest: sk_pixmap_t; const area: psk_irect_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pixmap_get_alpha      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_pixmap_t; x, y: int32_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pixmap_get_alpha_type {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_pixmap_t): sk_alphatype_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pixmap_get_color      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_pixmap_t; x, y: int32_t): sk_color_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pixmap_get_color_space{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_pixmap_t): sk_colorspace_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pixmap_get_color_type {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_pixmap_t): sk_colortype_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pixmap_get_colorf     {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_pixmap_t; x, y: int32_t; out result: sk_color4f_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pixmap_get_height     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_pixmap_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pixmap_get_image_info {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_pixmap_t; out result: sk_imageinfo_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pixmap_get_pixel_addr {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_pixmap_t; x, y: int32_t): Pointer; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pixmap_get_pixels     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_pixmap_t): Pointer; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pixmap_get_row_bytes  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_pixmap_t): size_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pixmap_get_width      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_pixmap_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pixmap_read_pixels    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self, dest: sk_pixmap_t; src_x, src_y: int32_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_pixmap_scale_pixels   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self, dest: sk_pixmap_t; const sampling: psk_samplingoptions_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_pixmap_set_colorspace {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_pixmap_t; value: sk_colorspace_t); cdecl;


  { include/c/sk4d_refcnt.h }

  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_refcnt_ref  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_refcnt_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_refcnt_unref{$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_refcnt_t); cdecl;


  { include/c/sk4d_region.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_region_contains             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self, region: sk_region_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_region_contains2            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_region_t; const rect: psk_irect_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_region_contains3            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_region_t; x, y: int32_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_region_create               {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): sk_region_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_region_create2              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const region: sk_region_t): sk_region_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_region_destroy              {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_region_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_region_get_boundary_path    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_region_t): sk_path_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_region_get_bounds           {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_region_t; out result: sk_irect_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_region_intersects           {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self, region: sk_region_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_region_intersects2          {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_region_t; const rect: psk_irect_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_region_is_complex           {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_region_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_region_is_empty             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_region_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_region_is_equal             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self, region: sk_region_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_region_is_rect              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_region_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_region_op                   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_region_t; const region: sk_region_t; op: sk_regionop_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_region_op2                  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_region_t; const rect: psk_irect_t; op: sk_regionop_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_region_quick_contains       {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_region_t; const rect: psk_irect_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_region_quick_reject         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self, region: sk_region_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_region_quick_reject2        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_region_t; const rect: psk_irect_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_region_set_empty            {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_region_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_region_set_path             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_region_t; const path: sk_path_t; const clip: sk_region_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_region_set_rect             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_region_t; const rect: psk_irect_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_region_set_rects            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_region_t; const rects: psk_irect_t; count: int32_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_region_translate            {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_region_t; x, y: int32_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_regioncliperator_create     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const region: sk_region_t; const clip: psk_irect_t): sk_regioncliperator_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_regioncliperator_destroy    {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_regioncliperator_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_regioncliperator_get_current{$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_regioncliperator_t; out result: sk_irect_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_regioncliperator_move_next  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_regioncliperator_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_regioniterator_create       {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const region: sk_region_t): sk_regioniterator_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_regioniterator_destroy      {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_regioniterator_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_regioniterator_get_current  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_regioniterator_t; out result: sk_irect_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_regioniterator_move_next    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_regioniterator_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_regionspanerator_create     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const region: sk_region_t; y, left, right: int32_t): sk_regionspanerator_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_regionspanerator_destroy    {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_regionspanerator_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_regionspanerator_next       {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_regionspanerator_t; out elem: sk_ipoint_t): _bool; cdecl;


  { include/c/sk4d_rrect.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_rrect_contains        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_rrect_t; const rect: psk_rect_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_rrect_create          {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): sk_rrect_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_rrect_create2         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const rrect: sk_rrect_t): sk_rrect_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_rrect_deflate         {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_rrect_t; dx, dy: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_rrect_destroy         {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_rrect_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_rrect_get_height      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_rrect_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_rrect_get_radii       {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_rrect_t; corner: sk_rrectcorner_t; out result: sk_vector_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_rrect_get_rect        {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_rrect_t; out result: sk_rect_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_rrect_get_simple_radii{$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_rrect_t; out result: sk_vector_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_rrect_get_width       {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_rrect_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_rrect_inflate         {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_rrect_t; dx, dy: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_rrect_is_complex      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_rrect_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_rrect_is_empty        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_rrect_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_rrect_is_equal        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self, rrect: sk_rrect_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_rrect_is_nine_patch   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_rrect_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_rrect_is_oval         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_rrect_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_rrect_is_rect         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_rrect_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_rrect_is_simple       {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_rrect_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_rrect_is_valid        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_rrect_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_rrect_offset          {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_rrect_t; dx, dy: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_rrect_set_empty       {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_rrect_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_rrect_set_nine_patch  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_rrect_t; const rect: psk_rect_t; radius_left, radius_top, radius_right, radius_bottom: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_rrect_set_oval        {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_rrect_t; const rect: psk_rect_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_rrect_set_rect        {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_rrect_t; const rect: psk_rect_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_rrect_set_rect2       {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_rrect_t; const rect: psk_rect_t; const radii: psk_vector_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_rrect_set_rect3       {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_rrect_t; const rect: psk_rect_t; radius_x, radius_y: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_rrect_transform       {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_rrect_t; const matrix: psk_matrix_t): sk_rrect_t; cdecl;


  { include/c/sk4d_runtimeeffect.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_runtimeblendbuilder_create          {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(effect: sk_runtimeeffect_t): sk_runtimeblendbuilder_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_runtimeblendbuilder_destroy         {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_runtimeblendbuilder_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_runtimeblendbuilder_make_blender    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_runtimeblendbuilder_t): sk_blender_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_runtimeeffect_get_child_count       {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_runtimeeffect_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_runtimeeffect_get_child_name        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_runtimeeffect_t; index: int32_t): sk_string_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_runtimeeffect_get_child_type        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_runtimeeffect_t; index: int32_t): sk_runtimeeffectchildtype_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_runtimeeffect_get_uniform_count     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_runtimeeffect_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_runtimeeffect_get_uniform_data_size {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_runtimeeffect_t): size_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_runtimeeffect_get_uniform_name      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_runtimeeffect_t; index: int32_t): sk_string_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_runtimeeffect_get_uniform_offset    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_runtimeeffect_t; index: int32_t): size_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_runtimeeffect_get_uniform_type      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_runtimeeffect_t; index: int32_t): sk_runtimeeffectuniformtype_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_runtimeeffect_get_uniform_type_count{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_runtimeeffect_t; index: int32_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_runtimeeffect_index_of_child        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_runtimeeffect_t; const name: MarshaledAString): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_runtimeeffect_index_of_uniform      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_runtimeeffect_t; const name: MarshaledAString): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_runtimeeffect_make_blender          {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_runtimeeffect_t; const uniforms: Pointer; children: psk_flattenable_t): sk_blender_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_runtimeeffect_make_color_filter     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_runtimeeffect_t; const uniforms: Pointer; children: psk_flattenable_t): sk_colorfilter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_runtimeeffect_make_for_blender      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const sksl: MarshaledAString; error_text: sk_string_t): sk_runtimeeffect_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_runtimeeffect_make_for_color_filter {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const sksl: MarshaledAString; error_text: sk_string_t): sk_runtimeeffect_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_runtimeeffect_make_for_shader       {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const sksl: MarshaledAString; error_text: sk_string_t): sk_runtimeeffect_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_runtimeeffect_make_image            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_runtimeeffect_t; context: gr_directcontext_t; const uniforms: Pointer; children: psk_flattenable_t; const local_matrix: psk_matrix_t; const image_info: psk_imageinfo_t; mipmapped: _bool): sk_image_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_runtimeeffect_make_shader           {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_runtimeeffect_t; const uniforms: Pointer; children: psk_flattenable_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_runtimeeffectbuilder_set_child      {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_runtimeeffectbuilder_t; const name: MarshaledAString; shader: sk_shader_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_runtimeeffectbuilder_set_child2     {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_runtimeeffectbuilder_t; const name: MarshaledAString; color_filter: sk_colorfilter_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_runtimeeffectbuilder_set_child3     {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_runtimeeffectbuilder_t; const name: MarshaledAString; blender: sk_blender_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_runtimeeffectbuilder_set_uniform    {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_runtimeeffectbuilder_t; const name: MarshaledAString; const data: Pointer); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_runtimeeffectbuilder_get_effect     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_runtimeeffectbuilder_t): sk_runtimeeffect_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_runtimeshaderbuilder_create         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(effect: sk_runtimeeffect_t): sk_runtimeshaderbuilder_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_runtimeshaderbuilder_destroy        {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_runtimeshaderbuilder_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_runtimeshaderbuilder_make_image     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_runtimeshaderbuilder_t; context: gr_directcontext_t; const local_matrix: psk_matrix_t; const image_info: psk_imageinfo_t; mipmapped: _bool): sk_image_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_runtimeshaderbuilder_make_shader    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_runtimeshaderbuilder_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;


  { include/c/sk4d_shader.h }

  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_shader_make_blend                      {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(mode: sk_blendmode_t; dest, src: sk_shader_t): sk_shader_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_shader_make_color                      {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(color: sk_color_t): sk_shader_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_shader_make_color2                     {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const color: psk_color4f_t; color_space: sk_colorspace_t): sk_shader_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_shader_make_empty                      {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(): sk_shader_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_shader_make_gradient_linear            {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const points: psk_point_t; const colors: psk_color_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_shader_make_gradient_linear2           {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const points: psk_point_t; const colors: psk_color4f_t; color_space: sk_colorspace_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_shader_make_gradient_radial            {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const center: psk_point_t; radius: float; const colors: psk_color_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_shader_make_gradient_radial2           {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const center: psk_point_t; radius: float; const colors: psk_color4f_t; color_space: sk_colorspace_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_shader_make_gradient_sweep             {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(center_x, center_y: float; const colors: psk_color_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; start_angle, end_angle: float; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_shader_make_gradient_sweep2            {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(center_x, center_y: float; const colors: psk_color4f_t; color_space: sk_colorspace_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; start_angle, end_angle: float; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_shader_make_gradient_two_point_conical {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const start: psk_point_t; start_radius: float; const &end: psk_point_t; end_radius: float; const colors: psk_color_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_shader_make_gradient_two_point_conical2{$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const start: psk_point_t; start_radius: float; const &end: psk_point_t; end_radius: float; const colors: psk_color4f_t; color_space: sk_colorspace_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_shader_make_perlin_noise_fractal_noise {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(base_frequency_x, base_frequency_y: float; num_octaves: int32_t; seed: float; const tile_size: psk_isize_t): sk_shader_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_shader_make_perlin_noise_turbulence    {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(base_frequency_x, base_frequency_y: float; num_octaves: int32_t; seed: float; const tile_size: psk_isize_t): sk_shader_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_shader_make_with_color_filter          {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const self: sk_shader_t; color_filter: sk_colorfilter_t): sk_shader_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_shader_make_with_local_matrix          {$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const self: sk_shader_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;


  { include/c/sk4d_stream.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_streamadapter_create    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(context: Pointer): sk_streamadapter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_streamadapter_destroy   {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_streamadapter_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_streamadapter_set_procs {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const procs: psk_streamadapter_procs_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_wstreamadapter_create   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(context: Pointer): sk_wstreamadapter_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_wstreamadapter_destroy  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_wstreamadapter_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_wstreamadapter_set_procs{$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const procs: psk_wstreamadapter_procs_t); cdecl;


  { include/c/sk4d_string.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_string_create  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): sk_string_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_string_destroy {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_string_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_string_get_text{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_string_t): MarshaledAString; cdecl;


  { include/c/sk4d_surface.h }

  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_surface_draw                   {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_surface_t; canvas: sk_canvas_t; x, y: float; paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_surface_flush                  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_surface_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_surface_flush_and_submit       {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_surface_t; semaphores: pgr_backendsemaphore_t; count: int32_t; const new_state: gr_backendsurfacemutablestate_t; sync_cpu: _bool); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_surface_get_canvas             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_surface_t): sk_canvas_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_surface_get_props              {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_surface_t; out result: sk_surfaceprops_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_surface_make_from_mtk_view     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(context: gr_directcontext_t; layer: gr_mtl_handle_t; origin: gr_surfaceorigin_t; sample_count: int32_t; color_type: sk_colortype_t; color_space: sk_colorspace_t; const props: psk_surfaceprops_t): sk_surface_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_surface_make_from_render_target{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(context: gr_directcontext_t; const render_target: gr_backendrendertarget_t; origin: gr_surfaceorigin_t; color_type: sk_colortype_t; color_space: sk_colorspace_t; const props: psk_surfaceprops_t): sk_surface_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_surface_make_from_texture      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(context: gr_directcontext_t; const texture: gr_backendtexture_t; origin: gr_surfaceorigin_t; sample_count: int32_t; color_type: sk_colortype_t; color_space: sk_colorspace_t; const props: psk_surfaceprops_t): sk_surface_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_surface_make_image_snapshot    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_surface_t): sk_image_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_surface_make_image_snapshot2   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_surface_t; const bounds: psk_irect_t): sk_image_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_surface_make_raster            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const image_info: psk_imageinfo_t; row_bytes: size_t; const props: psk_surfaceprops_t): sk_surface_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_surface_make_raster_direct     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const pixmap: sk_pixmap_t; proc: sk_surface_raster_release_proc; proc_context: Pointer; const props: psk_surfaceprops_t): sk_surface_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_surface_make_render_target     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(context: gr_directcontext_t; is_budgeted: _bool; const image_info: psk_imageinfo_t; sample_count: int32_t; origin: gr_surfaceorigin_t; const props: psk_surfaceprops_t; should_create_with_mips: _bool): sk_surface_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_surface_peek_pixels            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_surface_t): sk_pixmap_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_surface_read_pixels            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_surface_t; const dest: sk_pixmap_t; src_x, src_y: int32_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_surface_wait                   {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_surface_t; const semaphores: pgr_backendsemaphore_t; count: int32_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_surface_write_pixels           {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_surface_t; const src: sk_pixmap_t; dest_x, dest_y: int32_t); cdecl;

  { include/c/sk4d_svgcanvas.h }

  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_svgcanvas_make{$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(const bounds: psk_rect_t; w_stream: sk_wstream_t; flags: uint32_t): sk_canvas_t; cdecl;


  { include/c/sk4d_textblob.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_textblob_get_intercepts                        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_textblob_t; const bounds: pfloat; result: pfloat; const paint: sk_paint_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_textblob_make_from_text                        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const text: Pointer; size: size_t; const font: sk_font_t; encoding: sk_textencoding_t): sk_textblob_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_textblob_make_from_text_horizontally_positioned{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const text: Pointer; size: size_t; const x_positions: pfloat; y: float; const font: sk_font_t; encoding: sk_textencoding_t): sk_textblob_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_textblob_make_from_text_positioned             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const text: Pointer; size: size_t; const positions: psk_point_t; const font: sk_font_t; encoding: sk_textencoding_t): sk_textblob_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_textblob_make_from_text_transform              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const text: Pointer; size: size_t; const matrices: psk_rotationscalematrix_t; const font: sk_font_t; encoding: sk_textencoding_t): sk_textblob_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textblob_ref                                   {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_textblob_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textblob_unref                                 {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_textblob_t); cdecl;


  { include/c/sk4d_tracememorydump.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_tracememorydumpbaseclass_create   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(detailed_dump, dump_wrapped_objects: _bool; context: Pointer): sk_tracememorydumpbaseclass_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_tracememorydumpbaseclass_destroy  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_tracememorydumpbaseclass_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_tracememorydumpbaseclass_set_procs{$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const procs: psk_tracememorydumpbaseclass_procs_t); cdecl;


  { include/c/sk4d_typeface.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_typeface_get_family_name {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_typeface_t): sk_string_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_typeface_get_slant       {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_typeface_t): sk_fontslant_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_typeface_get_style       {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_typeface_t; out result: sk_fontstyle_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_typeface_get_weight      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_typeface_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_typeface_get_width       {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_typeface_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_typeface_make_default    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): sk_typeface_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_typeface_make_from_file  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const file_name: MarshaledAString; ttc_index: int32_t): sk_typeface_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_typeface_make_from_stream{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(stream: sk_stream_t; ttc_index: int32_t): sk_typeface_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_typeface_make_from_name  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const family_name: MarshaledAString; const style: psk_fontstyle_t): sk_typeface_t; cdecl;


  { include/c/sk4d_vertices.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_vertices_make_copy{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(vertex_mode: sk_vertexmode_t; vertex_count: int32_t; const positions, textures: psk_point_t; const colors: psk_color_t; index_count: int32_t; const indices: puint16_t): sk_vertices_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_vertices_ref      {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_vertices_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_vertices_unref    {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_vertices_t); cdecl;


  { modules/particles/include/sk4d_particleeffect.h }

  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_particleeffect_get_position          {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_particleeffect_t; out result: sk_point_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_particleeffect_get_rate              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_particleeffect_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_particleeffect_get_uniform           {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_particleeffect_t; index: size_t; out result: sk_particleuniform_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_particleeffect_get_uniform_count     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_particleeffect_t): size_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_particleeffect_get_uniform_data      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_particleeffect_t): pfloat; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_particleeffect_get_uniform_data_count{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_particleeffect_t): int32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_particleeffect_get_uniform_name      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_particleeffect_t; index: size_t): sk_string_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_particleeffect_init                  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_particleeffect_make_from_file        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const file_name: MarshaledAString): sk_particleeffect_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_particleeffect_make_from_stream      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(stream: sk_stream_t; resource_provider: sk_resourceprovider_t): sk_particleeffect_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_particleeffect_render                {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_particleeffect_t; canvas: sk_canvas_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_particleeffect_set_position          {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_particleeffect_t; const value: psk_point_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_particleeffect_set_rate              {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_particleeffect_t; value: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_particleeffect_set_uniform           {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_particleeffect_t; const name: MarshaledAString; const data: pfloat; count: int32_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_particleeffect_start                 {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_particleeffect_t; now: _double; looping: _bool); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_particleeffect_update                {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_particleeffect_t; now: _double); cdecl;


  { modules/skottie/include/sk4d_skottie.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_skottieanimation_get_duration    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_skottieanimation_t): _double; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_skottieanimation_get_fps         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_skottieanimation_t): _double; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_skottieanimation_get_in_point    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_skottieanimation_t): _double; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_skottieanimation_get_out_point   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_skottieanimation_t): _double; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_skottieanimation_get_size        {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_skottieanimation_t; out result: sk_size_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_skottieanimation_get_version     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_skottieanimation_t): MarshaledAString; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_skottieanimation_make_from_file  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const file_name: MarshaledAString; font_provider: sk_fontmgr_t): sk_skottieanimation_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_skottieanimation_make_from_stream{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(stream: sk_stream_t; resource_provider: sk_resourceprovider_t; font_provider: sk_fontmgr_t): sk_skottieanimation_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_skottieanimation_ref             {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_skottieanimation_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_skottieanimation_render          {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_skottieanimation_t; canvas: sk_canvas_t; const dest: psk_rect_t; render_flags: uint32_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_skottieanimation_seek_frame      {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_skottieanimation_t; tick: _double); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_skottieanimation_seek_frame_time {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_skottieanimation_t; tick: _double); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_skottieanimation_unref           {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_skottieanimation_t); cdecl;


  { modules/skparagraph/include/sk4d_paragraph.h }

  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paragraph_destroy                         {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paragraph_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paragraph_did_exceed_max_lines            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_paragraph_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paragraph_get_alphabetic_baseline         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_paragraph_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paragraph_get_glyph_position_at_coordinate{$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paragraph_t; dx, dy: float; out result: sk_positionaffinity_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paragraph_get_height                      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_paragraph_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paragraph_get_ideographic_baseline        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_paragraph_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paragraph_get_line_metrics                {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_paragraph_t; result: psk_metrics_t): size_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paragraph_get_longest_line                {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_paragraph_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paragraph_get_max_intrinsic_width         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_paragraph_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paragraph_get_max_width                   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_paragraph_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paragraph_get_min_intrinsic_width         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_paragraph_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paragraph_get_rects_for_placeholders      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_paragraph_t; result: psk_textbox_t): size_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paragraph_get_rects_for_range             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_paragraph_t; start, &end: uint32_t; rect_height_style: sk_rectheightstyle_t; rect_width_style: sk_rectwidthstyle_t; result: psk_textbox_t): size_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paragraph_get_word_boundary               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paragraph_t; offset: uint32_t; out start, &end: uint32_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paragraph_layout                          {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paragraph_t; width: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paragraph_paint                           {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paragraph_t; canvas: sk_canvas_t; x, y: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paragraph_to_path                         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_paragraph_t): sk_path_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paragraph_visit                           {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paragraph_t; proc: sk_paragraph_visit_proc; proc_context: Pointer); cdecl;


  { modules/skparagraph/include/sk4d_paragraphbuilder.h }

  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paragraphbuilder_add_placeholder{$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paragraphbuilder_t; const placeholder: psk_placeholderstyle_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paragraphbuilder_add_text       {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paragraphbuilder_t; const text: MarshaledAString); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paragraphbuilder_build          {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_paragraphbuilder_t): sk_paragraph_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paragraphbuilder_create         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const paragraph_style: sk_paragraphstyle_t): sk_paragraphbuilder_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paragraphbuilder_create2        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const paragraph_style: sk_paragraphstyle_t; font_provider: sk_fontmgr_t; enable_font_fallback: _bool): sk_paragraphbuilder_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paragraphbuilder_destroy        {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paragraphbuilder_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paragraphbuilder_pop            {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paragraphbuilder_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paragraphbuilder_push_style     {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paragraphbuilder_t; const text_style: sk_textstyle_t); cdecl;


  { modules/skparagraph/include/sk4d_paragraphstyle.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paragraphstyle_create                   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): sk_paragraphstyle_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paragraphstyle_destroy                  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paragraphstyle_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paragraphstyle_disable_hinting          {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paragraphstyle_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paragraphstyle_get_ellipsis             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paragraphstyle_t): sk_string_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paragraphstyle_get_height               {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paragraphstyle_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paragraphstyle_get_max_lines            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paragraphstyle_t): size_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paragraphstyle_get_strut_style          {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paragraphstyle_t): sk_strutstyle_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paragraphstyle_get_text_align           {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paragraphstyle_t): sk_textalign_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paragraphstyle_get_text_direction       {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paragraphstyle_t): sk_textdirection_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paragraphstyle_get_text_height_behaviors{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paragraphstyle_t): uint32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_paragraphstyle_get_text_style           {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_paragraphstyle_t): sk_textstyle_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paragraphstyle_set_ellipsis             {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paragraphstyle_t; const value: MarshaledAString); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paragraphstyle_set_height               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paragraphstyle_t; value: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paragraphstyle_set_max_lines            {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paragraphstyle_t; value: size_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paragraphstyle_set_strut_style          {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paragraphstyle_t; const value: sk_strutstyle_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paragraphstyle_set_text_align           {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paragraphstyle_t; value: sk_textalign_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paragraphstyle_set_text_direction       {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paragraphstyle_t; value: sk_textdirection_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paragraphstyle_set_text_height_behaviors{$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paragraphstyle_t; value: uint32_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_paragraphstyle_set_text_style           {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_paragraphstyle_t; value: sk_textstyle_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_strutstyle_create                       {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): sk_strutstyle_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_strutstyle_destroy                      {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_strutstyle_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_strutstyle_get_enabled                  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_strutstyle_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_strutstyle_get_font_families            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_strutstyle_t; const result: PMarshaledAString): size_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_strutstyle_get_font_size                {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_strutstyle_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_strutstyle_get_font_style               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_strutstyle_t; out result: sk_fontstyle_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_strutstyle_get_force_height             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_strutstyle_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_strutstyle_get_half_leading             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_strutstyle_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_strutstyle_get_height_multiplier        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_strutstyle_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_strutstyle_get_leading                  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_strutstyle_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_strutstyle_is_equal                     {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_strutstyle_t; const strut_style: sk_strutstyle_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_strutstyle_set_enabled                  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_strutstyle_t; value: _bool); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_strutstyle_set_font_families            {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_strutstyle_t; const values: PMarshaledAString; count: size_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_strutstyle_set_font_size                {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_strutstyle_t; value: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_strutstyle_set_font_style               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_strutstyle_t; value: psk_fontstyle_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_strutstyle_set_force_height             {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_strutstyle_t; value: _bool); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_strutstyle_set_half_leading             {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_strutstyle_t; value: _bool); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_strutstyle_set_height_multiplier        {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_strutstyle_t; value: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_strutstyle_set_leading                  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_strutstyle_t; value: float); cdecl;


  { modules/skparagraph/include/sk4d_textstyle.h }

  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textstyle_add_font_feature        {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_textstyle_t; const feature: MarshaledAString; value: int32_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textstyle_add_shadow              {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_textstyle_t; const shadow: psk_textshadow_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textstyle_clear_background_color  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_textstyle_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textstyle_clear_foreground_color  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_textstyle_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_textstyle_create                  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): sk_textstyle_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textstyle_destroy                 {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_textstyle_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_textstyle_get_background          {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_textstyle_t): sk_paint_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_textstyle_get_color               {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_textstyle_t): sk_color_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_textstyle_get_decoration_color    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_textstyle_t): sk_color_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_textstyle_get_decoration_style    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_textstyle_t): sk_textdecorationstyle_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_textstyle_get_decoration_thickness{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_textstyle_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_textstyle_get_decorations         {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_textstyle_t): uint32_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_textstyle_get_font_families       {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_textstyle_t; const result: PMarshaledAString): size_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textstyle_get_font_metrics        {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_textstyle_t; out result: sk_fontmetrics_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_textstyle_get_font_size           {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_textstyle_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textstyle_get_font_style          {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_textstyle_t; out result: sk_fontstyle_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_textstyle_get_foreground          {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_textstyle_t): sk_paint_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_textstyle_get_half_leading        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_textstyle_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_textstyle_get_height_multiplier   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_textstyle_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_textstyle_get_letter_spacing      {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_textstyle_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_textstyle_get_locale              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_textstyle_t): sk_string_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_textstyle_get_word_spacing        {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_textstyle_t): float; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_textstyle_is_equal                {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self, text_style: sk_textstyle_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textstyle_reset_font_features     {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_textstyle_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textstyle_reset_shadows           {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_textstyle_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textstyle_set_background_color    {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_textstyle_t; paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textstyle_set_color               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_textstyle_t; value: sk_color_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textstyle_set_decoration_color    {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_textstyle_t; value: sk_color_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textstyle_set_decoration_style    {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_textstyle_t; value: sk_textdecorationstyle_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textstyle_set_decoration_thickness{$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_textstyle_t; value: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textstyle_set_decorations         {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_textstyle_t; value: uint32_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textstyle_set_font_families       {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_textstyle_t; const values: PMarshaledAString; count: size_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textstyle_set_font_size           {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_textstyle_t; value: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textstyle_set_font_style          {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_textstyle_t; const value: psk_fontstyle_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textstyle_set_foreground_color    {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_textstyle_t; paint: sk_paint_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textstyle_set_half_leading        {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_textstyle_t; value: _bool); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textstyle_set_height_multiplier   {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_textstyle_t; value: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textstyle_set_letter_spacing      {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_textstyle_t; value: float); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textstyle_set_locale              {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_textstyle_t; const value: MarshaledAString); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_textstyle_set_word_spacing        {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_textstyle_t; value: float); cdecl;


  { modules/skparagraph/include/sk4d_typefacefontprovider.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_typefacefontprovider_create            {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): sk_typefacefontprovider_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_typefacefontprovider_register_typeface {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_typefacefontprovider_t; typeface: sk_typeface_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_typefacefontprovider_register_typeface2{$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_typefacefontprovider_t; typeface: sk_typeface_t; const family_name: MarshaledAString); cdecl;


  { modules/skresources/include/sk4d_resources.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_resourceproviderbaseclass_create   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(predecode: _bool; context: Pointer): sk_resourceproviderbaseclass_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_resourceproviderbaseclass_set_procs{$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const procs: psk_resourceproviderbaseclass_procs_t); cdecl;


  { modules/skshaper/include/sk4d_shaper.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_shaper_create {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): sk_shaper_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_shaper_destroy{$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_shaper_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_shaper_shape  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_shaper_t; const text: MarshaledAString; const font: sk_font_t; left_to_right: _bool; width: float; const offset: psk_point_t; end_point: psk_point_t): sk_textblob_t; cdecl;


  { modules/skunicode/include/sk4d_unicode.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_unicode_create              {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(): sk_unicode_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_unicode_destroy             {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_unicode_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_unicode_for_each_bidi_region{$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_unicode_t; const utf16_text: puint16_t; utf16_units: int32_t; direction: sk_direction_t; proc: sk_unicode_bidi_region_proc; context: Pointer); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_unicode_for_each_break      {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_unicode_t; const utf16_text: pchar16_t; utf16_units: int32_t; &type: sk_breaktype_t; proc: sk_unicode_break_proc; context: Pointer); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_unicode_for_each_codepoint  {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_unicode_t; const utf16_text: pchar16_t; utf16_units: int32_t; proc: sk_unicode_codepoint_proc; context: Pointer); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_unicodebreakiterator_create {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(unicode: sk_unicode_t; &type: sk_breaktype_t; const text: _pchar; units: int32_t): sk_unicodebreakiterator_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_unicodebreakiterator_create2{$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(unicode: sk_unicode_t; &type: sk_breaktype_t; const utf16_text: pchar16_t; utf16_units: int32_t): sk_unicodebreakiterator_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_unicodebreakiterator_destroy{$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_unicodebreakiterator_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_unicodebreakiterator_next   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_unicodebreakiterator_t; out elem: sk_unicodebreakiteratorelem_t): _bool; cdecl;

  { modules/svg/include/sk4d_svgdom.h }

  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_svgdom_find_node_by_id   {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(self: sk_svgdom_t; const id: MarshaledAString): sk_svgnode_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_svgdom_get_root          {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_svgdom_t): sk_svgsvg_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_svgdom_make_from_file    {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const file_name: MarshaledAString; font_provider: sk_fontmgr_t): sk_svgdom_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_svgdom_make_from_stream  {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(stream: sk_stream_t; resource_provider: sk_resourceprovider_t; font_provider: sk_fontmgr_t): sk_svgdom_t; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_svgdom_render            {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_svgdom_t; canvas: sk_canvas_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_svgdom_set_container_size{$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_svgdom_t; const size: psk_size_t); cdecl;


  { modules/svg/include/sk4d_svgnode.h }

  {$IFDEF SK_STATIC_LIBRARY}function {$ENDIF}sk4d_svgnode_set_attribute{$IFNDEF SK_STATIC_LIBRARY}: function {$ENDIF}(self: sk_svgnode_t; const name, value: MarshaledAString): _bool; cdecl;


  { modules/svg/include/sk4d_svgsvg.h }

  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_svgsvg_get_height               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_svgsvg_t; out result: sk_svglength_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_svgsvg_get_intrinsic_size       {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_svgsvg_t; const view_port: psk_size_t; dpi: float; out result: sk_size_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_svgsvg_get_preserve_aspect_ratio{$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_svgsvg_t; out result: sk_svgpreserveaspectratio_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}function  {$ENDIF}sk4d_svgsvg_get_view_box             {$IFNDEF SK_STATIC_LIBRARY}: function  {$ENDIF}(const self: sk_svgsvg_t; out result: sk_rect_t): _bool; cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_svgsvg_get_width                {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_svgsvg_t; out result: sk_svglength_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_svgsvg_get_x                    {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_svgsvg_t; out result: sk_svglength_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_svgsvg_get_y                    {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(const self: sk_svgsvg_t; out result: sk_svglength_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_svgsvg_set_height               {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_svgsvg_t; value: psk_svglength_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_svgsvg_set_preserve_aspect_ratio{$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_svgsvg_t; value: psk_svgpreserveaspectratio_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_svgsvg_set_view_box             {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_svgsvg_t; view_box: psk_rect_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_svgsvg_set_width                {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_svgsvg_t; value: psk_svglength_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_svgsvg_set_x                    {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_svgsvg_t; value: psk_svglength_t); cdecl;
  {$IFDEF SK_STATIC_LIBRARY}procedure {$ENDIF}sk4d_svgsvg_set_y                    {$IFNDEF SK_STATIC_LIBRARY}: procedure {$ENDIF}(self: sk_svgsvg_t; value: psk_svglength_t); cdecl;

procedure SkInitialize;
procedure SkFinalize;

implementation

{.$DEFINE SK_DEBUG}

{$IF DEFINED(MSWINDOWS)}

uses
  {$IFDEF FPC}
  { FPC }
  Windows,
  Math;
  {$ELSE}
  { Delphi }
  Winapi.Windows,
  System.Math;
  {$ENDIF}

{$ELSEIF NOT DEFINED(SK_STATIC_LIBRARY) and NOT DEFINED(FPC) and DEFINED(ANDROID)}

uses
  { Delphi }
  System.IOUtils;

{$ENDIF}

{$IF DEFINED(SK_STATIC_LIBRARY) or DEFINED(WORKAROUND_RS123846)}
  {$IFDEF FPC}
    {$IF DEFINED(MACOS)}
      {$IFDEF IOS)}
        {$LINKFRAMEWORK CoreFoundation}
        {$LINKFRAMEWORK CoreGraphics}
        {$LINKFRAMEWORK CoreText}
        {$LINKFRAMEWORK UIKit}
      {$ELSE}
        {$LINKFRAMEWORK ApplicationServices}
      {$ENDIF}
      {$LINKFRAMEWORK Foundation}
      {$LINKFRAMEWORK Metal}
      {$LINKLIB 'c++'}
      {$LINKLIB 'c++abi'}
    {$ELSEIF DEFINED(ANDROID)}
      {$LINKLIB 'EGL'}
      {$LINKLIB 'GLESv2'}
      {$LINKLIB 'log'}
      {$LINKLIB 'c++_static'}
      {$LINKLIB 'c++abi'}
    {$ENDIF}
    {$LINKLIB 'sk4d'}
  {$ELSE}
    const
      LibraryName = 'libsk4d.a';

    {$IF DEFINED(MACOS)}
      {$IFDEF IOS)}
        procedure LINKFRAMEWORK_CoreFoundation;      external '/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation';
        procedure LINKFRAMEWORK_CoreGraphics;        external '/System/Library/Frameworks/CoreGraphics.framework/CoreGraphics';
        procedure LINKFRAMEWORK_CoreText;            external '/System/Library/Frameworks/CoreText.framework/CoreText';
        procedure LINKFRAMEWORK_UIKit;               external '/System/Library/Frameworks/UIKit.framework/UIKit';
      {$ELSE}
        procedure LINKFRAMEWORK_ApplicationServices; external '/System/Library/Frameworks/ApplicationServices.framework/ApplicationServices';
      {$ENDIF}
      procedure LINKFRAMEWORK_Foundation;            external '/System/Library/Frameworks/Foundation.framework/Foundation';
      procedure LINKFRAMEWORK_Metal;                 external '/System/Library/Frameworks/Metal.framework/Metal';
      procedure LINKLIB_cxx;                         external '/usr/lib/libc++.dylib';
      procedure LINKLIB_cxxabi;                      external '/usr/lib/libc++abi.dylib';
    {$ELSEIF DEFINED(ANDROID)}
      procedure eglGetProcAddress;                   external 'libEGL.so';
      procedure glCreateShader;                      external 'libGLESv2.so';
      procedure __android_log_vprint;                external 'liblog.so';
      procedure _ZNSt6__ndk16locale7classicEv;       external 'libc++_static.a';
      procedure __cxa_guard_abort;                   external 'libc++abi.a';
    {$ENDIF}
  {$ENDIF}
{$ELSE}
const
  {$IF DEFINED(MSWINDOWS)}
  LibraryName = 'sk4d.dll';
  {$ELSEIF DEFINED(MACOS)}
  LibraryName = 'libsk4d.dylib';
  {$ELSE}
  LibraryName = 'libsk4d.so';
  {$ENDIF}
{$ENDIF}

{$REGION ' - Workaround RS-123846'}
{$IFDEF WORKAROUND_RS123846}
// - ---------------------------------------------------------------------------
// - WORKAROUND
// - ---------------------------------------------------------------------------
// -
// - Description:
// -   This code is a workaround intended to fix a bug involving interface
// -   breakage between RAD 11.1 and RAD 12.2 when the static library was
// -   implemented on Android and macOS.
// -
// - Bug report:
// -   https://embt.atlassian.net/browse/RS-123846
// -
// - ---------------------------------------------------------------------------

 { include/c/gr4d_backendsemaphore.h }

  function  _gr4d_backendsemaphore_create(): gr_backendsemaphore_t; cdecl; external LibraryName name 'gr4d_backendsemaphore_create';
  procedure _gr4d_backendsemaphore_destroy(self: gr_backendsemaphore_t); cdecl; external LibraryName name 'gr4d_backendsemaphore_destroy';
  procedure _gr4d_backendsemaphore_init_vulkan(self: gr_backendsemaphore_t; semaphore: gr_vk_semaphore_t); cdecl; external LibraryName name 'gr4d_backendsemaphore_init_vulkan';


 { include/c/gr4d_backendsurface.h }

  function  _gr4d_backendrendertarget_create_gl(width, height, sample_count, stencil_bits: int32_t; const framebuffer_info: pgr_gl_framebufferinfo_t): gr_backendrendertarget_t; cdecl; external LibraryName name 'gr4d_backendrendertarget_create_gl';
  function  _gr4d_backendrendertarget_create_mtl(width, height: int32_t; const texture_info: pgr_mtl_textureinfo_t): gr_backendrendertarget_t; cdecl; external LibraryName name 'gr4d_backendrendertarget_create_mtl';
  function  _gr4d_backendrendertarget_create_vk(width, height: int32_t; const image_info: pgr_vk_imageinfo_t): gr_backendrendertarget_t; cdecl; external LibraryName name 'gr4d_backendrendertarget_create_vk';
  procedure _gr4d_backendrendertarget_destroy(self: gr_backendrendertarget_t); cdecl; external LibraryName name 'gr4d_backendrendertarget_destroy';
  function  _gr4d_backendrendertarget_get_backend_api(const self: gr_backendrendertarget_t): gr_backendapi_t; cdecl; external LibraryName name 'gr4d_backendrendertarget_get_backend_api';
  function  _gr4d_backendrendertarget_get_height(const self: gr_backendrendertarget_t): int32_t; cdecl; external LibraryName name 'gr4d_backendrendertarget_get_height';
  function  _gr4d_backendrendertarget_get_sample_count(const self: gr_backendrendertarget_t): int32_t; cdecl; external LibraryName name 'gr4d_backendrendertarget_get_sample_count';
  function  _gr4d_backendrendertarget_get_stencil_bits(const self: gr_backendrendertarget_t): int32_t; cdecl; external LibraryName name 'gr4d_backendrendertarget_get_stencil_bits';
  function  _gr4d_backendrendertarget_get_width(const self: gr_backendrendertarget_t): int32_t; cdecl; external LibraryName name 'gr4d_backendrendertarget_get_width';
  function  _gr4d_backendrendertarget_is_valid(const self: gr_backendrendertarget_t): _bool; cdecl; external LibraryName name 'gr4d_backendrendertarget_is_valid';
  function  _gr4d_backendtexture_create_gl(width, height: int32_t; is_mipmapped: _bool; const texture_info: pgr_gl_textureinfo_t): gr_backendtexture_t; cdecl; external LibraryName name 'gr4d_backendtexture_create_gl';
  function  _gr4d_backendtexture_create_mtl(width, height: int32_t; is_mipmapped: _bool; const texture_info: pgr_mtl_textureinfo_t): gr_backendtexture_t; cdecl; external LibraryName name 'gr4d_backendtexture_create_mtl';
  function  _gr4d_backendtexture_create_vk(width, height: int32_t; const image_info: pgr_vk_imageinfo_t): gr_backendtexture_t; cdecl; external LibraryName name 'gr4d_backendtexture_create_vk';
  procedure _gr4d_backendtexture_destroy(self: gr_backendtexture_t); cdecl; external LibraryName name 'gr4d_backendtexture_destroy';
  function  _gr4d_backendtexture_get_backend_api(const self: gr_backendtexture_t): gr_backendapi_t; cdecl; external LibraryName name 'gr4d_backendtexture_get_backend_api';
  function  _gr4d_backendtexture_get_gl_framebuffer_info(const self: gr_backendtexture_t; out texture_info: gr_gl_textureinfo_t): _bool; cdecl; external LibraryName name 'gr4d_backendtexture_get_gl_framebuffer_info';
  function  _gr4d_backendtexture_get_height(const self: gr_backendtexture_t): int32_t; cdecl; external LibraryName name 'gr4d_backendtexture_get_height';
  function  _gr4d_backendtexture_get_width(const self: gr_backendtexture_t): int32_t; cdecl; external LibraryName name 'gr4d_backendtexture_get_width';
  function  _gr4d_backendtexture_has_mipmaps(const self: gr_backendtexture_t): _bool; cdecl; external LibraryName name 'gr4d_backendtexture_has_mipmaps';
  function  _gr4d_backendtexture_is_valid(const self: gr_backendtexture_t): _bool; cdecl; external LibraryName name 'gr4d_backendtexture_is_valid';


 { include/c/gr4d_backendsurfacemutablestate.h }

  function  _gr4d_backendsurfacemutablestate_create(image_layout: gr_vk_imagelayout_t; queue_family_index: uint32_t): gr_backendsurfacemutablestate_t; cdecl; external LibraryName name 'gr4d_backendsurfacemutablestate_create';
  procedure _gr4d_backendsurfacemutablestate_destroy(self: gr_backendsurfacemutablestate_t); cdecl; external LibraryName name 'gr4d_backendsurfacemutablestate_destroy';


 { include/c/gr4d_contextoptions.h }

  function  _gr4d_persistentcachebaseclass_create(context: Pointer): gr_persistentcachebaseclass_t; cdecl; external LibraryName name 'gr4d_persistentcachebaseclass_create';
  procedure _gr4d_persistentcachebaseclass_destroy(self: gr_persistentcachebaseclass_t); cdecl; external LibraryName name 'gr4d_persistentcachebaseclass_destroy';
  procedure _gr4d_persistentcachebaseclass_set_procs(const procs: pgr_persistentcachebaseclass_procs_t); cdecl; external LibraryName name 'gr4d_persistentcachebaseclass_set_procs';


 { include/c/gr4d_directcontext.h }

  procedure _gr4d_directcontext_abandon_context(self: gr_directcontext_t); cdecl; external LibraryName name 'gr4d_directcontext_abandon_context';
  function  _gr4d_directcontext_create_texture(self: gr_directcontext_t; width, height: int32_t; color_type: sk_colortype_t; is_mipmapped, is_renderable, is_protected: _bool): gr_backendtexture_t; cdecl; external LibraryName name 'gr4d_directcontext_create_texture';
  function  _gr4d_directcontext_create_texture2(self: gr_directcontext_t; width, height: int32_t; color_type: sk_colortype_t; color: sk_color_t; is_mipmapped, is_renderable, is_protected: _bool): gr_backendtexture_t; cdecl; external LibraryName name 'gr4d_directcontext_create_texture2';
  function  _gr4d_directcontext_create_texture3(self: gr_directcontext_t; width, height: int32_t; color_type: sk_colortype_t; const color: psk_color4f_t; is_mipmapped, is_renderable, is_protected: _bool): gr_backendtexture_t; cdecl; external LibraryName name 'gr4d_directcontext_create_texture3';
  procedure _gr4d_directcontext_delete_texture(self: gr_directcontext_t; texture: gr_backendtexture_t); cdecl; external LibraryName name 'gr4d_directcontext_delete_texture';
  procedure _gr4d_directcontext_dump_memory_statistics(const self: gr_directcontext_t; trace_memory_dump: sk_tracememorydump_t); cdecl; external LibraryName name 'gr4d_directcontext_dump_memory_statistics';
  procedure _gr4d_directcontext_flush(self: gr_directcontext_t); cdecl; external LibraryName name 'gr4d_directcontext_flush';
  procedure _gr4d_directcontext_flush_and_submit(self: gr_directcontext_t; sync_cpu: _bool); cdecl; external LibraryName name 'gr4d_directcontext_flush_and_submit';
  procedure _gr4d_directcontext_free_gpu_resources(self: gr_directcontext_t); cdecl; external LibraryName name 'gr4d_directcontext_free_gpu_resources';
  function  _gr4d_directcontext_get_backend_api(const self: gr_directcontext_t): gr_backendapi_t; cdecl; external LibraryName name 'gr4d_directcontext_get_backend_api';
  function  _gr4d_directcontext_get_max_surface_sample_count_for_color_type(const self: gr_directcontext_t; color_type: sk_colortype_t): int32_t; cdecl; external LibraryName name 'gr4d_directcontext_get_max_surface_sample_count_for_color_type';
  function  _gr4d_directcontext_get_resource_cache_limit(const self: gr_directcontext_t): size_t; cdecl; external LibraryName name 'gr4d_directcontext_get_resource_cache_limit';
  procedure _gr4d_directcontext_get_resource_cache_usage(const self: gr_directcontext_t; out resources: int32_t; out resources_bytes: size_t); cdecl; external LibraryName name 'gr4d_directcontext_get_resource_cache_usage';
  function  _gr4d_directcontext_is_abandoned(self: gr_directcontext_t): _bool; cdecl; external LibraryName name 'gr4d_directcontext_is_abandoned';
  function  _gr4d_directcontext_make_gl(const gl_interface: gr_gl_interface_t; const options: pgr_contextoptions_t): gr_directcontext_t; cdecl; external LibraryName name 'gr4d_directcontext_make_gl';
  function  _gr4d_directcontext_make_metal(const backend_context: pgr_mtl_backendcontext_t; const options: pgr_contextoptions_t): gr_directcontext_t; cdecl; external LibraryName name 'gr4d_directcontext_make_metal';
  function  _gr4d_directcontext_make_vulkan(const backend_context: pgr_vk_backendcontext_t; const options: pgr_contextoptions_t): gr_directcontext_t; cdecl; external LibraryName name 'gr4d_directcontext_make_vulkan';
  procedure _gr4d_directcontext_perform_deferred_cleanup(self: gr_directcontext_t; milliseconds: int64_t); cdecl; external LibraryName name 'gr4d_directcontext_perform_deferred_cleanup';
  procedure _gr4d_directcontext_purge_unlocked_resources(self: gr_directcontext_t; scratch_resources_only: _bool); cdecl; external LibraryName name 'gr4d_directcontext_purge_unlocked_resources';
  procedure _gr4d_directcontext_purge_unlocked_resources2(self: gr_directcontext_t; bytes_to_purge: size_t; prefer_scratch_resources: _bool); cdecl; external LibraryName name 'gr4d_directcontext_purge_unlocked_resources2';
  procedure _gr4d_directcontext_release_resources_and_abandon_context(self: gr_directcontext_t); cdecl; external LibraryName name 'gr4d_directcontext_release_resources_and_abandon_context';
  procedure _gr4d_directcontext_reset_context(self: gr_directcontext_t); cdecl; external LibraryName name 'gr4d_directcontext_reset_context';
  procedure _gr4d_directcontext_set_resource_cache_limit(self: gr_directcontext_t; value: size_t); cdecl; external LibraryName name 'gr4d_directcontext_set_resource_cache_limit';
  function  _gr4d_directcontext_submit(self: gr_directcontext_t; sync_cpu: _bool): _bool; cdecl; external LibraryName name 'gr4d_directcontext_submit';


 { include/c/gr4d_gl_interface.h }

  function  _gr4d_gl_interface_has_extension(const self: gr_gl_interface_t; const name: MarshaledAString): _bool; cdecl; external LibraryName name 'gr4d_gl_interface_has_extension';
  function  _gr4d_gl_interface_make_assembled(context: Pointer; proc: gr_gl_get_proc): gr_gl_interface_t; cdecl; external LibraryName name 'gr4d_gl_interface_make_assembled';
  function  _gr4d_gl_interface_make_assembled_gl(context: Pointer; proc: gr_gl_get_proc): gr_gl_interface_t; cdecl; external LibraryName name 'gr4d_gl_interface_make_assembled_gl';
  function  _gr4d_gl_interface_make_assembled_gles(context: Pointer; proc: gr_gl_get_proc): gr_gl_interface_t; cdecl; external LibraryName name 'gr4d_gl_interface_make_assembled_gles';
  function  _gr4d_gl_interface_make_assembled_webgl(context: Pointer; proc: gr_gl_get_proc): gr_gl_interface_t; cdecl; external LibraryName name 'gr4d_gl_interface_make_assembled_webgl';
  function  _gr4d_gl_interface_make_native(): gr_gl_interface_t; cdecl; external LibraryName name 'gr4d_gl_interface_make_native';
  function  _gr4d_gl_interface_validate(const self: gr_gl_interface_t): _bool; cdecl; external LibraryName name 'gr4d_gl_interface_validate';


 { include/c/gr4d_shadererrorhandler.h }

  function  _gr4d_shadererrorhandlerbaseclass_create(context: Pointer): gr_shadererrorhandlerbaseclass_t; cdecl; external LibraryName name 'gr4d_shadererrorhandlerbaseclass_create';
  procedure _gr4d_shadererrorhandlerbaseclass_destroy(self: gr_shadererrorhandlerbaseclass_t); cdecl; external LibraryName name 'gr4d_shadererrorhandlerbaseclass_destroy';
  procedure _gr4d_shadererrorhandlerbaseclass_set_procs(const procs: pgr_shadererrorhandlerbaseclass_procs_t); cdecl; external LibraryName name 'gr4d_shadererrorhandlerbaseclass_set_procs';


 { include/c/gr4d_vk_extensions.h }

  function  _gr4d_vk_extensions_create(): gr_vk_extensions_t; cdecl; external LibraryName name 'gr4d_vk_extensions_create';
  procedure _gr4d_vk_extensions_destroy(self: gr_vk_extensions_t); cdecl; external LibraryName name 'gr4d_vk_extensions_destroy';
  function  _gr4d_vk_extensions_has_extension(const self: gr_vk_extensions_t; const name: MarshaledAString; min_api_version: uint32_t): _bool; cdecl; external LibraryName name 'gr4d_vk_extensions_has_extension';
  procedure _gr4d_vk_extensions_init(self: gr_vk_extensions_t; context: Pointer; proc: gr_vk_get_proc; instance: gr_vk_instance_t; physical_device: gr_vk_physicaldevice_t; instance_extension_count: int32_t; const instance_extensions: PMarshaledAString; device_extension_count: int32_t; const device_extensions: PMarshaledAString); cdecl; external LibraryName name 'gr4d_vk_extensions_init';


 { include/c/sk4d_animatedwebpencoder.h }

  function  _sk4d_animatedwebpencoder_encode_to_file(const file_name: MarshaledAString; const src: psk_frame_t; count: size_t; quality: int32_t): _bool; cdecl; external LibraryName name 'sk4d_animatedwebpencoder_encode_to_file';
  function  _sk4d_animatedwebpencoder_encode_to_stream(w_stream: sk_wstream_t; const src: psk_frame_t; count: size_t; quality: int32_t): _bool; cdecl; external LibraryName name 'sk4d_animatedwebpencoder_encode_to_stream';


 { include/c/sk4d_blender.h }

  function  _sk4d_blender_make_arithmetic(k1, k2, k3, k4: float; enforce_premultiplied_color: _bool): sk_blender_t; cdecl; external LibraryName name 'sk4d_blender_make_arithmetic';
  function  _sk4d_blender_make_mode(mode: sk_blendmode_t): sk_blender_t; cdecl; external LibraryName name 'sk4d_blender_make_mode';


 { include/c/sk4d_canvas.h }

  procedure _sk4d_canvas_clear(self: sk_canvas_t; color: sk_color_t); cdecl; external LibraryName name 'sk4d_canvas_clear';
  procedure _sk4d_canvas_clear2(self: sk_canvas_t; const color: psk_color4f_t); cdecl; external LibraryName name 'sk4d_canvas_clear2';
  procedure _sk4d_canvas_destroy(self: sk_canvas_t); cdecl; external LibraryName name 'sk4d_canvas_destroy';
  procedure _sk4d_canvas_discard(self: sk_canvas_t); cdecl; external LibraryName name 'sk4d_canvas_discard';
  procedure _sk4d_canvas_clip_path(self: sk_canvas_t; const path: sk_path_t; op: sk_clipop_t; anti_alias: _bool); cdecl; external LibraryName name 'sk4d_canvas_clip_path';
  procedure _sk4d_canvas_clip_rect(self: sk_canvas_t; const rect: psk_rect_t; op: sk_clipop_t; anti_alias: _bool); cdecl; external LibraryName name 'sk4d_canvas_clip_rect';
  procedure _sk4d_canvas_clip_region(self: sk_canvas_t; const region: sk_region_t; op: sk_clipop_t); cdecl; external LibraryName name 'sk4d_canvas_clip_region';
  procedure _sk4d_canvas_clip_rrect(self: sk_canvas_t; const rrect: sk_rrect_t; op: sk_clipop_t; anti_alias: _bool); cdecl; external LibraryName name 'sk4d_canvas_clip_rrect';
  procedure _sk4d_canvas_clip_shader(self: sk_canvas_t; shader: sk_shader_t; op: sk_clipop_t); cdecl; external LibraryName name 'sk4d_canvas_clip_shader';
  procedure _sk4d_canvas_concat(self: sk_canvas_t; const matrix: psk_matrix44_t); cdecl; external LibraryName name 'sk4d_canvas_concat';
  procedure _sk4d_canvas_concat2(self: sk_canvas_t; const matrix: psk_matrix_t); cdecl; external LibraryName name 'sk4d_canvas_concat2';
  procedure _sk4d_canvas_draw_annotation(self: sk_canvas_t; const rect: psk_rect_t; const key: MarshaledAString; const value: Pointer; size: size_t); cdecl; external LibraryName name 'sk4d_canvas_draw_annotation';
  procedure _sk4d_canvas_draw_arc(self: sk_canvas_t; const oval: psk_rect_t; start_angle, sweep_angle: float; use_center: _bool; const paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_arc';
  procedure _sk4d_canvas_draw_atlas(self: sk_canvas_t; const atlas: sk_image_t; const transforms: psk_rotationscalematrix_t; const sprites: psk_rect_t; const colors: psk_color_t; count: int32_t; blend_mode: sk_blendmode_t; const sampling: psk_samplingoptions_t; const cull_rect: psk_rect_t; const paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_atlas';
  procedure _sk4d_canvas_draw_circle(self: sk_canvas_t; const center: psk_point_t; radius: float; const paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_circle';
  procedure _sk4d_canvas_draw_color(self: sk_canvas_t; color: sk_color_t; blend_mode: sk_blendmode_t); cdecl; external LibraryName name 'sk4d_canvas_draw_color';
  procedure _sk4d_canvas_draw_color2(self: sk_canvas_t; const color: psk_color4f_t; blend_mode: sk_blendmode_t); cdecl; external LibraryName name 'sk4d_canvas_draw_color2';
  procedure _sk4d_canvas_draw_glyphs(self: sk_canvas_t; count: int32_t; const glyphs: psk_glyphid_t; const positions: psk_point_t; const origin: psk_point_t; const font: sk_font_t; const paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_glyphs';
  procedure _sk4d_canvas_draw_glyphs2(self: sk_canvas_t; count: int32_t; const glyphs: psk_glyphid_t; const matrices: psk_rotationscalematrix_t; const origin: psk_point_t; const font: sk_font_t; const paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_glyphs2';
  procedure _sk4d_canvas_draw_image(self: sk_canvas_t; const image: sk_image_t; x, y: float; const sampling: psk_samplingoptions_t; const paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_image';
  procedure _sk4d_canvas_draw_image_lattice(self: sk_canvas_t; const image: sk_image_t; const lattice: psk_lattice_t; const dest: psk_rect_t; filter_mode: sk_filtermode_t; const paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_image_lattice';
  procedure _sk4d_canvas_draw_image_nine(self: sk_canvas_t; const image: sk_image_t; const center: psk_irect_t; const dest: psk_rect_t; filter_mode: sk_filtermode_t; const paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_image_nine';
  procedure _sk4d_canvas_draw_image_rect(self: sk_canvas_t; const image: sk_image_t; const src, dest: psk_rect_t; const sampling: psk_samplingoptions_t; const paint: sk_paint_t; constraint: sk_srcrectconstraint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_image_rect';
  procedure _sk4d_canvas_draw_line(self: sk_canvas_t; const point1, point2: psk_point_t; paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_line';
  procedure _sk4d_canvas_draw_oval(self: sk_canvas_t; const oval: psk_rect_t; const paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_oval';
  procedure _sk4d_canvas_draw_paint(self: sk_canvas_t; const paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_paint';
  procedure _sk4d_canvas_draw_patch(self: sk_canvas_t; const cubics: psk_point_t; const colors: psk_color_t; const tex_coords: psk_point_t; blend_mode: sk_blendmode_t; const paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_patch';
  procedure _sk4d_canvas_draw_path(self: sk_canvas_t; const path: sk_path_t; const paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_path';
  procedure _sk4d_canvas_draw_picture(self: sk_canvas_t; const picture: sk_picture_t; const matrix: psk_matrix_t; const paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_picture';
  procedure _sk4d_canvas_draw_point(self: sk_canvas_t; const point: psk_point_t; const paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_point';
  procedure _sk4d_canvas_draw_points(self: sk_canvas_t; mode: sk_drawpointsmode_t; count: size_t; const points: psk_point_t; const paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_points';
  procedure _sk4d_canvas_draw_rect(self: sk_canvas_t; const rect: psk_rect_t; const paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_rect';
  procedure _sk4d_canvas_draw_region(self: sk_canvas_t; const region: sk_region_t; const paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_region';
  procedure _sk4d_canvas_draw_rrect(self: sk_canvas_t; const rrect: sk_rrect_t; const paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_rrect';
  procedure _sk4d_canvas_draw_rrect2(self: sk_canvas_t; const rect: psk_rect_t; radius_x, radius_y: float; const paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_rrect2';
  procedure _sk4d_canvas_draw_rrect_difference(self: sk_canvas_t; const outer, inner: sk_rrect_t; const paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_rrect_difference';
  procedure _sk4d_canvas_draw_simple_text(self: sk_canvas_t; const text: Pointer; size: size_t; encoding: sk_textencoding_t; x, y: float; const font: sk_font_t; const paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_simple_text';
  procedure _sk4d_canvas_draw_text_blob(self: sk_canvas_t; const text_blob: sk_textblob_t; x, y: float; const paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_text_blob';
  procedure _sk4d_canvas_draw_vertices(self: sk_canvas_t; const vertices: sk_vertices_t; blend_mode: sk_blendmode_t; const paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_canvas_draw_vertices';
  procedure _sk4d_canvas_get_base_props(const self: sk_canvas_t; out result: sk_surfaceprops_t); cdecl; external LibraryName name 'sk4d_canvas_get_base_props';
  procedure _sk4d_canvas_get_device_clip_bounds(const self: sk_canvas_t; out result: sk_irect_t); cdecl; external LibraryName name 'sk4d_canvas_get_device_clip_bounds';
  procedure _sk4d_canvas_get_local_clip_bounds(const self: sk_canvas_t; out result: sk_rect_t); cdecl; external LibraryName name 'sk4d_canvas_get_local_clip_bounds';
  procedure _sk4d_canvas_get_local_to_device(const self: sk_canvas_t; out result: sk_matrix44_t); cdecl; external LibraryName name 'sk4d_canvas_get_local_to_device';
  procedure _sk4d_canvas_get_local_to_device_as_3x3(const self: sk_canvas_t; out result: sk_matrix_t); cdecl; external LibraryName name 'sk4d_canvas_get_local_to_device_as_3x3';
  procedure _sk4d_canvas_get_top_props(const self: sk_canvas_t; out result: sk_surfaceprops_t); cdecl; external LibraryName name 'sk4d_canvas_get_top_props';
  function  _sk4d_canvas_get_save_count(const self: sk_canvas_t): int32_t; cdecl; external LibraryName name 'sk4d_canvas_get_save_count';
  function  _sk4d_canvas_make_surface(self: sk_canvas_t; const image_info: psk_imageinfo_t; const props: psk_surfaceprops_t): sk_surface_t; cdecl; external LibraryName name 'sk4d_canvas_make_surface';
  function  _sk4d_canvas_quick_reject(const self: sk_canvas_t; const rect: psk_rect_t): _bool; cdecl; external LibraryName name 'sk4d_canvas_quick_reject';
  function  _sk4d_canvas_quick_reject2(const self: sk_canvas_t; const path: sk_path_t): _bool; cdecl; external LibraryName name 'sk4d_canvas_quick_reject2';
  procedure _sk4d_canvas_reset_matrix(self: sk_canvas_t); cdecl; external LibraryName name 'sk4d_canvas_reset_matrix';
  procedure _sk4d_canvas_restore(self: sk_canvas_t); cdecl; external LibraryName name 'sk4d_canvas_restore';
  procedure _sk4d_canvas_restore_to_count(self: sk_canvas_t; save_count: int32_t); cdecl; external LibraryName name 'sk4d_canvas_restore_to_count';
  procedure _sk4d_canvas_rotate(self: sk_canvas_t; degrees: float); cdecl; external LibraryName name 'sk4d_canvas_rotate';
  procedure _sk4d_canvas_rotate2(self: sk_canvas_t; degrees, px, py: float); cdecl; external LibraryName name 'sk4d_canvas_rotate2';
  function  _sk4d_canvas_save(self: sk_canvas_t): int32_t; cdecl; external LibraryName name 'sk4d_canvas_save';
  function  _sk4d_canvas_save_layer(self: sk_canvas_t; const bounds: psk_rect_t; const paint: sk_paint_t; const backdrop: sk_imagefilter_t; flags: uint32_t): int32_t; cdecl; external LibraryName name 'sk4d_canvas_save_layer';
  function  _sk4d_canvas_save_layer_alpha(self: sk_canvas_t; const bounds: psk_rect_t; alpha: uint8_t): int32_t; cdecl; external LibraryName name 'sk4d_canvas_save_layer_alpha';
  procedure _sk4d_canvas_scale(self: sk_canvas_t; sx, sy: float); cdecl; external LibraryName name 'sk4d_canvas_scale';
  procedure _sk4d_canvas_set_matrix(self: sk_canvas_t; const matrix: psk_matrix44_t); cdecl; external LibraryName name 'sk4d_canvas_set_matrix';
  procedure _sk4d_canvas_set_matrix2(self: sk_canvas_t; const matrix: psk_matrix_t); cdecl; external LibraryName name 'sk4d_canvas_set_matrix2';
  procedure _sk4d_canvas_skew(self: sk_canvas_t; kx, ky: float); cdecl; external LibraryName name 'sk4d_canvas_skew';
  procedure _sk4d_canvas_translate(self: sk_canvas_t; dx, dy: float); cdecl; external LibraryName name 'sk4d_canvas_translate';


 { include/c/sk4d_codec.h }

  procedure _sk4d_codec_destroy(codec: sk_codec_t); cdecl; external LibraryName name 'sk4d_codec_destroy';
  procedure _sk4d_codec_get_dimensions(const self: sk_codec_t; out result: sk_isize_t); cdecl; external LibraryName name 'sk4d_codec_get_dimensions';
  function  _sk4d_codec_get_encoded_image_format(const self: sk_codec_t): sk_encodedimageformat_t; cdecl; external LibraryName name 'sk4d_codec_get_encoded_image_format';
  function  _sk4d_codec_get_image(self: sk_codec_t; color_type: sk_colortype_t; alpha_type: sk_alphatype_t; color_space: sk_colorspace_t): sk_image_t; cdecl; external LibraryName name 'sk4d_codec_get_image';
  function  _sk4d_codec_get_pixels(self: sk_codec_t; pixels: Pointer; row_bytes: size_t; color_type: sk_colortype_t; alpha_type: sk_alphatype_t; color_space: sk_colorspace_t): _bool; cdecl; external LibraryName name 'sk4d_codec_get_pixels';
  function  _sk4d_codec_make_from_file(const file_name: MarshaledAString): sk_codec_t; cdecl; external LibraryName name 'sk4d_codec_make_from_file';
  function  _sk4d_codec_make_from_stream(stream: sk_stream_t): sk_codec_t; cdecl; external LibraryName name 'sk4d_codec_make_from_stream';
  function  _sk4d_codec_make_with_copy(const data: Pointer; size: size_t): sk_codec_t; cdecl; external LibraryName name 'sk4d_codec_make_with_copy';
  function  _sk4d_codec_make_without_copy(const data: Pointer; size: size_t): sk_codec_t; cdecl; external LibraryName name 'sk4d_codec_make_without_copy';
  procedure _sk4d_animcodecplayer_destroy(self: sk_animcodecplayer_t); cdecl; external LibraryName name 'sk4d_animcodecplayer_destroy';
  procedure _sk4d_animcodecplayer_get_dimensions(const self: sk_animcodecplayer_t; out result: sk_isize_t); cdecl; external LibraryName name 'sk4d_animcodecplayer_get_dimensions';
  function  _sk4d_animcodecplayer_get_duration(const self: sk_animcodecplayer_t): uint32_t; cdecl; external LibraryName name 'sk4d_animcodecplayer_get_duration';
  function  _sk4d_animcodecplayer_get_frame(self: sk_animcodecplayer_t): sk_image_t; cdecl; external LibraryName name 'sk4d_animcodecplayer_get_frame';
  function  _sk4d_animcodecplayer_make_from_file(const file_name: MarshaledAString): sk_animcodecplayer_t; cdecl; external LibraryName name 'sk4d_animcodecplayer_make_from_file';
  function  _sk4d_animcodecplayer_make_from_stream(stream: sk_stream_t): sk_animcodecplayer_t; cdecl; external LibraryName name 'sk4d_animcodecplayer_make_from_stream';
  function  _sk4d_animcodecplayer_seek(self: sk_animcodecplayer_t; milliseconds: uint32_t): _bool; cdecl; external LibraryName name 'sk4d_animcodecplayer_seek';


 { include/c/sk4d_colorfilter.h }

  function  _sk4d_colorfilter_make_blend(color: sk_color_t; mode: sk_blendmode_t): sk_colorfilter_t; cdecl; external LibraryName name 'sk4d_colorfilter_make_blend';
  function  _sk4d_colorfilter_make_blend2(const color: psk_color4f_t; color_space: sk_colorspace_t; mode: sk_blendmode_t): sk_colorfilter_t; cdecl; external LibraryName name 'sk4d_colorfilter_make_blend2';
  function  _sk4d_colorfilter_make_compose(outer, inner: sk_colorfilter_t): sk_colorfilter_t; cdecl; external LibraryName name 'sk4d_colorfilter_make_compose';
  function  _sk4d_colorfilter_make_high_contrast(const config: psk_highcontrastconfig_t): sk_colorfilter_t; cdecl; external LibraryName name 'sk4d_colorfilter_make_high_contrast';
  function  _sk4d_colorfilter_make_hsla_matrix(const matrix: psk_colormatrix_t): sk_colorfilter_t; cdecl; external LibraryName name 'sk4d_colorfilter_make_hsla_matrix';
  function  _sk4d_colorfilter_make_lighting(multiply, add: sk_color_t): sk_colorfilter_t; cdecl; external LibraryName name 'sk4d_colorfilter_make_lighting';
  function  _sk4d_colorfilter_make_linear_to_srgb_gamma(): sk_colorfilter_t; cdecl; external LibraryName name 'sk4d_colorfilter_make_linear_to_srgb_gamma';
  function  _sk4d_colorfilter_make_luma_color(): sk_colorfilter_t; cdecl; external LibraryName name 'sk4d_colorfilter_make_luma_color';
  function  _sk4d_colorfilter_make_matrix(const matrix: psk_colormatrix_t): sk_colorfilter_t; cdecl; external LibraryName name 'sk4d_colorfilter_make_matrix';
  function  _sk4d_colorfilter_make_overdraw(const colors: psk_color_t): sk_colorfilter_t; cdecl; external LibraryName name 'sk4d_colorfilter_make_overdraw';
  function  _sk4d_colorfilter_make_table(const tablea_a, tablea_r, tablea_g, tablea_b: puint8_t): sk_colorfilter_t; cdecl; external LibraryName name 'sk4d_colorfilter_make_table';


 { include/c/sk4d_colorspace.h }

  function  _sk4d_colorspace_gamma_close_to_srgb(const self: sk_colorspace_t): _bool; cdecl; external LibraryName name 'sk4d_colorspace_gamma_close_to_srgb';
  function  _sk4d_colorspace_gamma_is_linear(const self: sk_colorspace_t): _bool; cdecl; external LibraryName name 'sk4d_colorspace_gamma_is_linear';
  function  _sk4d_colorspace_is_equal(const self, color_space: sk_colorspace_t): _bool; cdecl; external LibraryName name 'sk4d_colorspace_is_equal';
  function  _sk4d_colorspace_is_numerical_transfer_fn(const self: sk_colorspace_t; out transfer_function: sk_colorspacetransferfn_t): _bool; cdecl; external LibraryName name 'sk4d_colorspace_is_numerical_transfer_fn';
  function  _sk4d_colorspace_is_srgb(const self: sk_colorspace_t): _bool; cdecl; external LibraryName name 'sk4d_colorspace_is_srgb';
  function  _sk4d_colorspace_make(const profile: sk_colorspaceiccprofile_t): sk_colorspace_t; cdecl; external LibraryName name 'sk4d_colorspace_make';
  function  _sk4d_colorspace_make_linear_gamma(const self: sk_colorspace_t): sk_colorspace_t; cdecl; external LibraryName name 'sk4d_colorspace_make_linear_gamma';
  function  _sk4d_colorspace_make_rgb(const transfer_function: psk_colorspacetransferfn_t; const xyz: psk_colorspacexyz_t): sk_colorspace_t; cdecl; external LibraryName name 'sk4d_colorspace_make_rgb';
  function  _sk4d_colorspace_make_srgb(): sk_colorspace_t; cdecl; external LibraryName name 'sk4d_colorspace_make_srgb';
  function  _sk4d_colorspace_make_srgb_gamma(const self: sk_colorspace_t): sk_colorspace_t; cdecl; external LibraryName name 'sk4d_colorspace_make_srgb_gamma';
  function  _sk4d_colorspace_make_srgb_linear(): sk_colorspace_t; cdecl; external LibraryName name 'sk4d_colorspace_make_srgb_linear';
  procedure _sk4d_colorspace_ref(const self: sk_colorspace_t); cdecl; external LibraryName name 'sk4d_colorspace_ref';
  function  _sk4d_colorspace_to_profile(const self: sk_colorspace_t): sk_colorspaceiccprofile_t; cdecl; external LibraryName name 'sk4d_colorspace_to_profile';
  function  _sk4d_colorspace_to_xyz(const self: sk_colorspace_t; out xyz: sk_colorspacexyz_t): _bool; cdecl; external LibraryName name 'sk4d_colorspace_to_xyz';
  procedure _sk4d_colorspace_unref(const self: sk_colorspace_t); cdecl; external LibraryName name 'sk4d_colorspace_unref';
  procedure _sk4d_colorspaceiccprofile_destroy(self: sk_colorspaceiccprofile_t); cdecl; external LibraryName name 'sk4d_colorspaceiccprofile_destroy';
  function  _sk4d_colorspaceiccprofile_get_buffer(const self: sk_colorspaceiccprofile_t; size: puint32_t): puint8_t; cdecl; external LibraryName name 'sk4d_colorspaceiccprofile_get_buffer';
  function  _sk4d_colorspaceiccprofile_make_with_parse(const buffer: Pointer; size: size_t): sk_colorspaceiccprofile_t; cdecl; external LibraryName name 'sk4d_colorspaceiccprofile_make_with_parse';
  function  _sk4d_colorspaceiccprofile_to_xyz(const self: sk_colorspaceiccprofile_t; out dest: sk_colorspacexyz_t): _bool; cdecl; external LibraryName name 'sk4d_colorspaceiccprofile_to_xyz';
  function  _sk4d_colorspaceprimaries_to_xyz(const self: psk_colorspaceprimaries_t; out xyz: sk_colorspacexyz_t): _bool; cdecl; external LibraryName name 'sk4d_colorspaceprimaries_to_xyz';
  function  _sk4d_colorspacetransferfn_invert(const self: psk_colorspacetransferfn_t; out transfer_function: sk_colorspacetransferfn_t): _bool; cdecl; external LibraryName name 'sk4d_colorspacetransferfn_invert';
  function  _sk4d_colorspacetransferfn_transform(const self: psk_colorspacetransferfn_t; x: float): float; cdecl; external LibraryName name 'sk4d_colorspacetransferfn_transform';


 { include/c/sk4d_data.h }

  function  _sk4d_data_make_empty(): sk_data_t; cdecl; external LibraryName name 'sk4d_data_make_empty';
  function  _sk4d_data_make_with_copy(const data: Pointer; size: size_t): sk_data_t; cdecl; external LibraryName name 'sk4d_data_make_with_copy';
  procedure _sk4d_data_ref(const self: sk_data_t); cdecl; external LibraryName name 'sk4d_data_ref';
  procedure _sk4d_data_unref(const self: sk_data_t); cdecl; external LibraryName name 'sk4d_data_unref';


 { include/c/sk4d_document.h }

  function  _sk4d_document_begin_page(self: sk_document_t; width, height: float; const content: psk_rect_t): sk_canvas_t; cdecl; external LibraryName name 'sk4d_document_begin_page';
  procedure _sk4d_document_close(self: sk_document_t); cdecl; external LibraryName name 'sk4d_document_close';
  procedure _sk4d_document_end_page(self: sk_document_t); cdecl; external LibraryName name 'sk4d_document_end_page';
  function  _sk4d_document_make_pdf(w_stream: sk_wstream_t): sk_document_t; cdecl; external LibraryName name 'sk4d_document_make_pdf';
  function  _sk4d_document_make_pdf2(w_stream: sk_wstream_t; const metadata: psk_pdfmetadata_t): sk_document_t; cdecl; external LibraryName name 'sk4d_document_make_pdf2';
  function  _sk4d_document_make_xps(w_stream: sk_wstream_t; dpi: float): sk_document_t; cdecl; external LibraryName name 'sk4d_document_make_xps';
  procedure _sk4d_document_terminate(self: sk_document_t); cdecl; external LibraryName name 'sk4d_document_terminate';


 { include/c/sk4d_font.h }

  function  _sk4d_font_create(typeface: sk_typeface_t; size, sx, kx: float): sk_font_t; cdecl; external LibraryName name 'sk4d_font_create';
  function  _sk4d_font_create2(const font: sk_font_t): sk_font_t; cdecl; external LibraryName name 'sk4d_font_create2';
  procedure _sk4d_font_destroy(self: sk_font_t); cdecl; external LibraryName name 'sk4d_font_destroy';
  function  _sk4d_font_get_baseline_snap(const self: sk_font_t): _bool; cdecl; external LibraryName name 'sk4d_font_get_baseline_snap';
  function  _sk4d_font_get_edging(const self: sk_font_t): sk_fontedging_t; cdecl; external LibraryName name 'sk4d_font_get_edging';
  function  _sk4d_font_get_embedded_bitmaps(const self: sk_font_t): _bool; cdecl; external LibraryName name 'sk4d_font_get_embedded_bitmaps';
  function  _sk4d_font_get_embolden(const self: sk_font_t): _bool; cdecl; external LibraryName name 'sk4d_font_get_embolden';
  function  _sk4d_font_get_force_auto_hinting(const self: sk_font_t): _bool; cdecl; external LibraryName name 'sk4d_font_get_force_auto_hinting';
  function  _sk4d_font_get_glyphs(const self: sk_font_t; const text: Pointer; size: size_t; encoding: sk_textencoding_t; result: psk_glyphid_t; max_count: int32_t): int32_t; cdecl; external LibraryName name 'sk4d_font_get_glyphs';
  function  _sk4d_font_get_glyphs_count(const self: sk_font_t; const text: Pointer; size: size_t; encoding: sk_textencoding_t): int32_t; cdecl; external LibraryName name 'sk4d_font_get_glyphs_count';
  function  _sk4d_font_get_hinting(const self: sk_font_t): sk_fonthinting_t; cdecl; external LibraryName name 'sk4d_font_get_hinting';
  procedure _sk4d_font_get_horizontal_positions(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; result: pfloat; origin: float); cdecl; external LibraryName name 'sk4d_font_get_horizontal_positions';
  function  _sk4d_font_get_intercepts(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; const positions: psk_point_t; const bounds: pfloat; result: pfloat; const paint: sk_paint_t): size_t; cdecl; external LibraryName name 'sk4d_font_get_intercepts';
  function  _sk4d_font_get_linear_metrics(const self: sk_font_t): _bool; cdecl; external LibraryName name 'sk4d_font_get_linear_metrics';
  function  _sk4d_font_get_metrics(const self: sk_font_t; metrics: psk_fontmetrics_t): float; cdecl; external LibraryName name 'sk4d_font_get_metrics';
  function  _sk4d_font_get_path(const self: sk_font_t; glyph: sk_glyphid_t): sk_path_t; cdecl; external LibraryName name 'sk4d_font_get_path';
  procedure _sk4d_font_get_paths(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; proc: sk_font_path_proc; proc_context: Pointer); cdecl; external LibraryName name 'sk4d_font_get_paths';
  procedure _sk4d_font_get_positions(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; result: psk_point_t; const origin: psk_point_t); cdecl; external LibraryName name 'sk4d_font_get_positions';
  function  _sk4d_font_get_scale_x(const self: sk_font_t): float; cdecl; external LibraryName name 'sk4d_font_get_scale_x';
  function  _sk4d_font_get_size(const self: sk_font_t): float; cdecl; external LibraryName name 'sk4d_font_get_size';
  function  _sk4d_font_get_skew_x(const self: sk_font_t): float; cdecl; external LibraryName name 'sk4d_font_get_skew_x';
  function  _sk4d_font_get_subpixel(const self: sk_font_t): _bool; cdecl; external LibraryName name 'sk4d_font_get_subpixel';
  function  _sk4d_font_get_typeface(const self: sk_font_t): sk_typeface_t; cdecl; external LibraryName name 'sk4d_font_get_typeface';
  function  _sk4d_font_get_typeface_or_default(const self: sk_font_t): sk_typeface_t; cdecl; external LibraryName name 'sk4d_font_get_typeface_or_default';
  procedure _sk4d_font_get_widths_bounds(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; widths: pfloat; bounds: psk_rect_t; const paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_font_get_widths_bounds';
  function  _sk4d_font_is_equal(const self, font: sk_font_t): _bool; cdecl; external LibraryName name 'sk4d_font_is_equal';
  function  _sk4d_font_measure_text(const self: sk_font_t; const text: Pointer; size: size_t; encoding: sk_textencoding_t; bounds: psk_rect_t; const paint: sk_paint_t): float; cdecl; external LibraryName name 'sk4d_font_measure_text';
  procedure _sk4d_font_set_baseline_snap(self: sk_font_t; value: _bool); cdecl; external LibraryName name 'sk4d_font_set_baseline_snap';
  procedure _sk4d_font_set_edging(self: sk_font_t; value: sk_fontedging_t); cdecl; external LibraryName name 'sk4d_font_set_edging';
  procedure _sk4d_font_set_embedded_bitmaps(self: sk_font_t; value: _bool); cdecl; external LibraryName name 'sk4d_font_set_embedded_bitmaps';
  procedure _sk4d_font_set_embolden(self: sk_font_t; value: _bool); cdecl; external LibraryName name 'sk4d_font_set_embolden';
  procedure _sk4d_font_set_force_auto_hinting(self: sk_font_t; value: _bool); cdecl; external LibraryName name 'sk4d_font_set_force_auto_hinting';
  procedure _sk4d_font_set_hinting(self: sk_font_t; value: sk_fonthinting_t); cdecl; external LibraryName name 'sk4d_font_set_hinting';
  procedure _sk4d_font_set_linear_metrics(self: sk_font_t; value: _bool); cdecl; external LibraryName name 'sk4d_font_set_linear_metrics';
  procedure _sk4d_font_set_scale_x(self: sk_font_t; value: float); cdecl; external LibraryName name 'sk4d_font_set_scale_x';
  procedure _sk4d_font_set_size(self: sk_font_t; value: float); cdecl; external LibraryName name 'sk4d_font_set_size';
  procedure _sk4d_font_set_skew_x(self: sk_font_t; value: float); cdecl; external LibraryName name 'sk4d_font_set_skew_x';
  procedure _sk4d_font_set_subpixel(self: sk_font_t; value: _bool); cdecl; external LibraryName name 'sk4d_font_set_subpixel';
  procedure _sk4d_font_set_typeface(self: sk_font_t; typeface: sk_typeface_t); cdecl; external LibraryName name 'sk4d_font_set_typeface';
  function  _sk4d_font_unichar_to_glyph(const self: sk_font_t; uni_char: sk_unichar_t): sk_glyphid_t; cdecl; external LibraryName name 'sk4d_font_unichar_to_glyph';
  procedure _sk4d_font_unichars_to_glyphs(const self: sk_font_t; const uni_chars: psk_unichar_t; count: int32_t; result: psk_glyphid_t); cdecl; external LibraryName name 'sk4d_font_unichars_to_glyphs';


 { include/c/sk4d_graphics.h }

  procedure _sk4d_graphics_allow_jit(); cdecl; external LibraryName name 'sk4d_graphics_allow_jit';
  procedure _sk4d_graphics_dump_memory_statistics(trace_memory_dump: sk_tracememorydump_t); cdecl; external LibraryName name 'sk4d_graphics_dump_memory_statistics';
  function  _sk4d_graphics_get_font_cache_count_limit(): int32_t; cdecl; external LibraryName name 'sk4d_graphics_get_font_cache_count_limit';
  function  _sk4d_graphics_get_font_cache_count_used(): int32_t; cdecl; external LibraryName name 'sk4d_graphics_get_font_cache_count_used';
  function  _sk4d_graphics_get_font_cache_limit(): size_t; cdecl; external LibraryName name 'sk4d_graphics_get_font_cache_limit';
  function  _sk4d_graphics_get_font_cache_used(): size_t; cdecl; external LibraryName name 'sk4d_graphics_get_font_cache_used';
  function  _sk4d_graphics_get_resource_cache_single_allocation_byte_limit(): size_t; cdecl; external LibraryName name 'sk4d_graphics_get_resource_cache_single_allocation_byte_limit';
  function  _sk4d_graphics_get_resource_cache_total_byte_limit(): size_t; cdecl; external LibraryName name 'sk4d_graphics_get_resource_cache_total_byte_limit';
  function  _sk4d_graphics_get_resource_cache_total_bytes_used(): size_t; cdecl; external LibraryName name 'sk4d_graphics_get_resource_cache_total_bytes_used';
  procedure _sk4d_graphics_init(); cdecl; external LibraryName name 'sk4d_graphics_init';
  procedure _sk4d_graphics_purge_all_caches(); cdecl; external LibraryName name 'sk4d_graphics_purge_all_caches';
  procedure _sk4d_graphics_purge_font_cache(); cdecl; external LibraryName name 'sk4d_graphics_purge_font_cache';
  procedure _sk4d_graphics_purge_resource_cache(); cdecl; external LibraryName name 'sk4d_graphics_purge_resource_cache';
  function  _sk4d_graphics_set_font_cache_count_limit(value: int32_t): int32_t; cdecl; external LibraryName name 'sk4d_graphics_set_font_cache_count_limit';
  function  _sk4d_graphics_set_font_cache_limit(value: size_t): size_t; cdecl; external LibraryName name 'sk4d_graphics_set_font_cache_limit';
  function  _sk4d_graphics_set_resource_cache_single_allocation_byte_limit(value: size_t): size_t; cdecl; external LibraryName name 'sk4d_graphics_set_resource_cache_single_allocation_byte_limit';
  function  _sk4d_graphics_set_resource_cache_total_byte_limit(value: size_t): size_t; cdecl; external LibraryName name 'sk4d_graphics_set_resource_cache_total_byte_limit';


 { include/c/sk4d_image.h }

  function  _sk4d_image_encode_to_file(const self: sk_image_t; const file_name: MarshaledAString; format: sk_encodedimageformat_t; quality: int32_t): _bool; cdecl; external LibraryName name 'sk4d_image_encode_to_file';
  function  _sk4d_image_encode_to_stream(const self: sk_image_t; w_stream: sk_wstream_t; format: sk_encodedimageformat_t; quality: int32_t): _bool; cdecl; external LibraryName name 'sk4d_image_encode_to_stream';
  function  _sk4d_image_get_alpha_type(const self: sk_image_t): sk_alphatype_t; cdecl; external LibraryName name 'sk4d_image_get_alpha_type';
  function  _sk4d_image_get_color_space(const self: sk_image_t): sk_colorspace_t; cdecl; external LibraryName name 'sk4d_image_get_color_space';
  function  _sk4d_image_get_color_type(const self: sk_image_t): sk_colortype_t; cdecl; external LibraryName name 'sk4d_image_get_color_type';
  function  _sk4d_image_get_height(const self: sk_image_t): int32_t; cdecl; external LibraryName name 'sk4d_image_get_height';
  procedure _sk4d_image_get_image_info(const self: sk_image_t; out result: sk_imageinfo_t); cdecl; external LibraryName name 'sk4d_image_get_image_info';
  function  _sk4d_image_get_unique_id(const self: sk_image_t): uint32_t; cdecl; external LibraryName name 'sk4d_image_get_unique_id';
  function  _sk4d_image_get_width(const self: sk_image_t): int32_t; cdecl; external LibraryName name 'sk4d_image_get_width';
  function  _sk4d_image_is_lazy_generated(const self: sk_image_t): _bool; cdecl; external LibraryName name 'sk4d_image_is_lazy_generated';
  function  _sk4d_image_is_texture_backed(const self: sk_image_t): _bool; cdecl; external LibraryName name 'sk4d_image_is_texture_backed';
  function  _sk4d_image_is_valid(const self: sk_image_t; context: gr_directcontext_t): _bool; cdecl; external LibraryName name 'sk4d_image_is_valid';
  function  _sk4d_image_make_cross_context(context: gr_directcontext_t; const pixmap: sk_pixmap_t; build_mips, limit_to_max_texture_size: _bool): sk_image_t; cdecl; external LibraryName name 'sk4d_image_make_cross_context';
  function  _sk4d_image_make_from_adopted_texture(context: gr_directcontext_t; const texture: gr_backendtexture_t; origin: gr_surfaceorigin_t; color_type: sk_colortype_t; alpha_type: sk_alphatype_t; color_space: sk_colorspace_t): sk_image_t; cdecl; external LibraryName name 'sk4d_image_make_from_adopted_texture';
  function  _sk4d_image_make_from_encoded_file(const file_name: MarshaledAString): sk_image_t; cdecl; external LibraryName name 'sk4d_image_make_from_encoded_file';
  function  _sk4d_image_make_from_encoded_stream(stream: sk_stream_t): sk_image_t; cdecl; external LibraryName name 'sk4d_image_make_from_encoded_stream';
  function  _sk4d_image_make_from_picture(picture: sk_picture_t; const dimensions: psk_isize_t; const matrix: psk_matrix_t; const paint: sk_paint_t; color_space: sk_colorspace_t; const props: psk_surfaceprops_t): sk_image_t; cdecl; external LibraryName name 'sk4d_image_make_from_picture';
  function  _sk4d_image_make_from_raster(const pixmap: sk_pixmap_t; proc: sk_image_raster_release_proc; proc_context: Pointer): sk_image_t; cdecl; external LibraryName name 'sk4d_image_make_from_raster';
  function  _sk4d_image_make_from_texture(context: gr_directcontext_t; const texture: gr_backendtexture_t; origin: gr_surfaceorigin_t; color_type: sk_colortype_t; alpha_type: sk_alphatype_t; color_space: sk_colorspace_t; proc: sk_image_texture_release_proc; proc_context: Pointer): sk_image_t; cdecl; external LibraryName name 'sk4d_image_make_from_texture';
  function  _sk4d_image_make_non_texture_image(const self: sk_image_t): sk_image_t; cdecl; external LibraryName name 'sk4d_image_make_non_texture_image';
  function  _sk4d_image_make_raster_copy(const pixmap: sk_pixmap_t): sk_image_t; cdecl; external LibraryName name 'sk4d_image_make_raster_copy';
  function  _sk4d_image_make_raster_image(const self: sk_image_t): sk_image_t; cdecl; external LibraryName name 'sk4d_image_make_raster_image';
  function  _sk4d_image_make_raw_shader(const self: sk_image_t; tile_mode_x, tile_mode_y: sk_tilemode_t; const sampling: psk_samplingoptions_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl; external LibraryName name 'sk4d_image_make_raw_shader';
  function  _sk4d_image_make_shader(const self: sk_image_t; tile_mode_x, tile_mode_y: sk_tilemode_t; const sampling: psk_samplingoptions_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl; external LibraryName name 'sk4d_image_make_shader';
  function  _sk4d_image_make_subset(const self: sk_image_t; const subset: psk_irect_t; context: gr_directcontext_t): sk_image_t; cdecl; external LibraryName name 'sk4d_image_make_subset';
  function  _sk4d_image_make_texture_image(const self: sk_image_t; context: gr_directcontext_t; is_mipmapped: _bool): sk_image_t; cdecl; external LibraryName name 'sk4d_image_make_texture_image';
  function  _sk4d_image_make_with_filter(const self: sk_image_t; context: gr_directcontext_t; const filter: sk_imagefilter_t; const subset, clip_bounds: psk_irect_t; out out_subset: sk_irect_t; out offset: sk_ipoint_t): sk_image_t; cdecl; external LibraryName name 'sk4d_image_make_with_filter';
  function  _sk4d_image_peek_pixels(const self: sk_image_t): sk_pixmap_t; cdecl; external LibraryName name 'sk4d_image_peek_pixels';
  function  _sk4d_image_read_pixels(const self: sk_image_t; context: gr_directcontext_t; const dest: sk_pixmap_t; src_x, src_y: int32_t; caching_hint: sk_imagecachinghint_t): _bool; cdecl; external LibraryName name 'sk4d_image_read_pixels';
  function  _sk4d_image_scale_pixels(const self: sk_image_t; const dest: sk_pixmap_t; const sampling: psk_samplingoptions_t; caching_hint: sk_imagecachinghint_t): _bool; cdecl; external LibraryName name 'sk4d_image_scale_pixels';


 { include/c/sk4d_imageencoder.h }

  function  _sk4d_imageencoder_encode_to_file(const file_name: MarshaledAString; const src: sk_pixmap_t; format: sk_encodedimageformat_t; quality: int32_t): _bool; cdecl; external LibraryName name 'sk4d_imageencoder_encode_to_file';
  function  _sk4d_imageencoder_encode_to_stream(w_stream: sk_wstream_t; const src: sk_pixmap_t; format: sk_encodedimageformat_t; quality: int32_t): _bool; cdecl; external LibraryName name 'sk4d_imageencoder_encode_to_stream';


 { include/c/sk4d_imagefilter.h }

  function  _sk4d_imagefilter_can_compute_fast_bounds(const self: sk_imagefilter_t): _bool; cdecl; external LibraryName name 'sk4d_imagefilter_can_compute_fast_bounds';
  procedure _sk4d_imagefilter_compute_fast_bounds(const self: sk_imagefilter_t; const bounds: psk_rect_t; out result: sk_rect_t); cdecl; external LibraryName name 'sk4d_imagefilter_compute_fast_bounds';
  function  _sk4d_imagefilter_make_alpha_threshold(const region: sk_region_t; inner_min, outer_max: float; input: sk_imagefilter_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_alpha_threshold';
  function  _sk4d_imagefilter_make_arithmetic(k1, k2, k3, k4: float; enforce_premultiplied_color: _bool; background, foreground: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_arithmetic';
  function  _sk4d_imagefilter_make_blend(mode: sk_blendmode_t; background, foreground: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_blend';
  function  _sk4d_imagefilter_make_blur(sigma_x, sigma_y: float; tile_mode: sk_tilemode_t; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_blur';
  function  _sk4d_imagefilter_make_colorfilter(color_filter: sk_colorfilter_t; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_colorfilter';
  function  _sk4d_imagefilter_make_compose(inner, outer: sk_imagefilter_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_compose';
  function  _sk4d_imagefilter_make_dilate(radius_x, radius_y: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_dilate';
  function  _sk4d_imagefilter_make_displacement_map(x_channel_selector, y_channel_selector: sk_colorchannel_t; scale: float; displacement, input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_displacement_map';
  function  _sk4d_imagefilter_make_distant_lit_diffuse(const direction: psk_point3_t; light_color: sk_color_t; surface_scale, kd: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_distant_lit_diffuse';
  function  _sk4d_imagefilter_make_distant_lit_specular(const direction: psk_point3_t; light_color: sk_color_t; surface_scale, ks, shininess: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_distant_lit_specular';
  function  _sk4d_imagefilter_make_drop_shadow(dx, dy, sigma_x, sigma_y: float; color: sk_color_t; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_drop_shadow';
  function  _sk4d_imagefilter_make_drop_shadow_only(dx, dy, sigma_x, sigma_y: float; color: sk_color_t; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_drop_shadow_only';
  function  _sk4d_imagefilter_make_erode(radius_x, radius_y: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_erode';
  function  _sk4d_imagefilter_make_image(image: sk_image_t; const src, dest: psk_rect_t; const sampling: psk_samplingoptions_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_image';
  function  _sk4d_imagefilter_make_magnifier(const src: psk_rect_t; inset: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_magnifier';
  function  _sk4d_imagefilter_make_matrix_convolution(const kernel_size: psk_isize_t; const kernel: pfloat; gain, bias: float; const kernel_offset: psk_ipoint_t; tile_mode: sk_tilemode_t; convolve_alpha: _bool; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_matrix_convolution';
  function  _sk4d_imagefilter_make_matrix_transform(const matrix: psk_matrix_t; const sampling: psk_samplingoptions_t; input: sk_imagefilter_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_matrix_transform';
  function  _sk4d_imagefilter_make_merge(const filters: psk_imagefilter_t; count: int32_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_merge';
  function  _sk4d_imagefilter_make_offset(dx, dy: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_offset';
  function  _sk4d_imagefilter_make_picture(picture: sk_picture_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_picture';
  function  _sk4d_imagefilter_make_point_lit_diffuse(const location: psk_point3_t; light_color: sk_color_t; surface_scale, kd: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_point_lit_diffuse';
  function  _sk4d_imagefilter_make_point_lit_specular(const location: psk_point3_t; light_color: sk_color_t; surface_scale, ks, shininess: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_point_lit_specular';
  function  _sk4d_imagefilter_make_runtime_shader(const effect_builder: sk_runtimeshaderbuilder_t; const child: MarshaledAString; input: sk_imagefilter_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_runtime_shader';
  function  _sk4d_imagefilter_make_runtime_shader2(const effect_builder: sk_runtimeshaderbuilder_t; const children: PMarshaledAString; inputs: psk_imagefilter_t; count: int32_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_runtime_shader2';
  function  _sk4d_imagefilter_make_shader(shader: sk_shader_t; dither: _bool; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_shader';
  function  _sk4d_imagefilter_make_spot_lit_diffuse(const location, target: psk_point3_t; falloff_exponent, cutoff_angle: float; light_color: sk_color_t; surface_scale, kd: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_spot_lit_diffuse';
  function  _sk4d_imagefilter_make_spot_lit_specular(const location, target: psk_point3_t; falloff_exponent, cutoff_angle: float; light_color: sk_color_t; surface_scale, ks, shininess: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_spot_lit_specular';
  function  _sk4d_imagefilter_make_tile(const src, dest: psk_rect_t; input: sk_imagefilter_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_tile';
  function  _sk4d_imagefilter_make_with_local_matrix(const self: sk_imagefilter_t; const local_matrix: psk_matrix_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_imagefilter_make_with_local_matrix';



 { include/c/sk4d_maskfilter.h }

  function  _sk4d_maskfilter_make_blur(style: sk_blurstyle_t; sigma: float; respect_ctm: _bool): sk_maskfilter_t; cdecl; external LibraryName name 'sk4d_maskfilter_make_blur';
  function  _sk4d_maskfilter_make_shader(shader: sk_shader_t): sk_maskfilter_t; cdecl; external LibraryName name 'sk4d_maskfilter_make_shader';
  function  _sk4d_maskfilter_make_table(const table: puint8_t): sk_maskfilter_t; cdecl; external LibraryName name 'sk4d_maskfilter_make_table';
  function  _sk4d_maskfilter_make_table_clip(min, max: uint8_t): sk_maskfilter_t; cdecl; external LibraryName name 'sk4d_maskfilter_make_table_clip';
  function  _sk4d_maskfilter_make_table_gamma(gamma: float): sk_maskfilter_t; cdecl; external LibraryName name 'sk4d_maskfilter_make_table_gamma';


 { include/c/sk4d_paint.h }

  function  _sk4d_paint_create(): sk_paint_t; cdecl; external LibraryName name 'sk4d_paint_create';
  function  _sk4d_paint_create2(const paint: sk_paint_t): sk_paint_t; cdecl; external LibraryName name 'sk4d_paint_create2';
  procedure _sk4d_paint_destroy(self: sk_paint_t); cdecl; external LibraryName name 'sk4d_paint_destroy';
  function  _sk4d_paint_get_alpha(const self: sk_paint_t): uint8_t; cdecl; external LibraryName name 'sk4d_paint_get_alpha';
  function  _sk4d_paint_get_alphaf(const self: sk_paint_t): float; cdecl; external LibraryName name 'sk4d_paint_get_alphaf';
  function  _sk4d_paint_get_anti_alias(const self: sk_paint_t): _bool; cdecl; external LibraryName name 'sk4d_paint_get_anti_alias';
  function  _sk4d_paint_get_blender(const self: sk_paint_t): sk_blender_t; cdecl; external LibraryName name 'sk4d_paint_get_blender';
  function  _sk4d_paint_get_color(const self: sk_paint_t): sk_color_t; cdecl; external LibraryName name 'sk4d_paint_get_color';
  procedure _sk4d_paint_get_colorf(const self: sk_paint_t; out result: sk_color4f_t); cdecl; external LibraryName name 'sk4d_paint_get_colorf';
  function  _sk4d_paint_get_color_filter(const self: sk_paint_t): sk_colorfilter_t; cdecl; external LibraryName name 'sk4d_paint_get_color_filter';
  function  _sk4d_paint_get_dither(const self: sk_paint_t): _bool; cdecl; external LibraryName name 'sk4d_paint_get_dither';
  function  _sk4d_paint_get_fill_path(const self: sk_paint_t; const path: sk_path_t; const cull_rect: psk_rect_t; res_scale: float): sk_path_t; cdecl; external LibraryName name 'sk4d_paint_get_fill_path';
  function  _sk4d_paint_get_image_filter(const self: sk_paint_t): sk_imagefilter_t; cdecl; external LibraryName name 'sk4d_paint_get_image_filter';
  function  _sk4d_paint_get_mask_filter(const self: sk_paint_t): sk_maskfilter_t; cdecl; external LibraryName name 'sk4d_paint_get_mask_filter';
  function  _sk4d_paint_get_path_effect(const self: sk_paint_t): sk_patheffect_t; cdecl; external LibraryName name 'sk4d_paint_get_path_effect';
  function  _sk4d_paint_get_shader(const self: sk_paint_t): sk_shader_t; cdecl; external LibraryName name 'sk4d_paint_get_shader';
  function  _sk4d_paint_get_stroke_cap(const self: sk_paint_t): sk_strokecap_t; cdecl; external LibraryName name 'sk4d_paint_get_stroke_cap';
  function  _sk4d_paint_get_stroke_join(const self: sk_paint_t): sk_strokejoin_t; cdecl; external LibraryName name 'sk4d_paint_get_stroke_join';
  function  _sk4d_paint_get_stroke_miter(const self: sk_paint_t): float; cdecl; external LibraryName name 'sk4d_paint_get_stroke_miter';
  function  _sk4d_paint_get_stroke_width(const self: sk_paint_t): float; cdecl; external LibraryName name 'sk4d_paint_get_stroke_width';
  function  _sk4d_paint_get_style(const self: sk_paint_t): sk_paintstyle_t; cdecl; external LibraryName name 'sk4d_paint_get_style';
  procedure _sk4d_paint_reset(self: sk_paint_t); cdecl; external LibraryName name 'sk4d_paint_reset';
  procedure _sk4d_paint_set_alpha(self: sk_paint_t; value: uint8_t); cdecl; external LibraryName name 'sk4d_paint_set_alpha';
  procedure _sk4d_paint_set_alphaf(self: sk_paint_t; value: float); cdecl; external LibraryName name 'sk4d_paint_set_alphaf';
  procedure _sk4d_paint_set_antialias(self: sk_paint_t; value: _bool); cdecl; external LibraryName name 'sk4d_paint_set_antialias';
  procedure _sk4d_paint_set_argb(self: sk_paint_t; a, r, g, b: uint8_t); cdecl; external LibraryName name 'sk4d_paint_set_argb';
  procedure _sk4d_paint_set_blender(self: sk_paint_t; value: sk_blender_t); cdecl; external LibraryName name 'sk4d_paint_set_blender';
  procedure _sk4d_paint_set_color(self: sk_paint_t; value: sk_color_t); cdecl; external LibraryName name 'sk4d_paint_set_color';
  procedure _sk4d_paint_set_colorf(self: sk_paint_t; const value: psk_color4f_t; color_space: sk_colorspace_t); cdecl; external LibraryName name 'sk4d_paint_set_colorf';
  procedure _sk4d_paint_set_color_filter(self: sk_paint_t; value: sk_colorfilter_t); cdecl; external LibraryName name 'sk4d_paint_set_color_filter';
  procedure _sk4d_paint_set_dither(self: sk_paint_t; value: _bool); cdecl; external LibraryName name 'sk4d_paint_set_dither';
  procedure _sk4d_paint_set_image_filter(self: sk_paint_t; value: sk_imagefilter_t); cdecl; external LibraryName name 'sk4d_paint_set_image_filter';
  procedure _sk4d_paint_set_mask_filter(self: sk_paint_t; value: sk_maskfilter_t); cdecl; external LibraryName name 'sk4d_paint_set_mask_filter';
  procedure _sk4d_paint_set_path_effect(self: sk_paint_t; value: sk_patheffect_t); cdecl; external LibraryName name 'sk4d_paint_set_path_effect';
  procedure _sk4d_paint_set_shader(self: sk_paint_t; value: sk_shader_t); cdecl; external LibraryName name 'sk4d_paint_set_shader';
  procedure _sk4d_paint_set_stroke_cap(self: sk_paint_t; value: sk_strokecap_t); cdecl; external LibraryName name 'sk4d_paint_set_stroke_cap';
  procedure _sk4d_paint_set_stroke_join(self: sk_paint_t; value: sk_strokejoin_t); cdecl; external LibraryName name 'sk4d_paint_set_stroke_join';
  procedure _sk4d_paint_set_stroke_miter(self: sk_paint_t; value: float); cdecl; external LibraryName name 'sk4d_paint_set_stroke_miter';
  procedure _sk4d_paint_set_stroke_width(self: sk_paint_t; value: float); cdecl; external LibraryName name 'sk4d_paint_set_stroke_width';
  procedure _sk4d_paint_set_style(self: sk_paint_t; value: sk_paintstyle_t); cdecl; external LibraryName name 'sk4d_paint_set_style';


 { include/c/sk4d_path.h }

  procedure _sk4d_opbuilder_add(self: sk_opbuilder_t; const path: sk_path_t; op: sk_pathop_t); cdecl; external LibraryName name 'sk4d_opbuilder_add';
  function  _sk4d_opbuilder_create(): sk_opbuilder_t; cdecl; external LibraryName name 'sk4d_opbuilder_create';
  procedure _sk4d_opbuilder_destroy(self: sk_opbuilder_t); cdecl; external LibraryName name 'sk4d_opbuilder_destroy';
  function  _sk4d_opbuilder_detach(self: sk_opbuilder_t): sk_path_t; cdecl; external LibraryName name 'sk4d_opbuilder_detach';
  function  _sk4d_path_contains(const self: sk_path_t; x, y: float): _bool; cdecl; external LibraryName name 'sk4d_path_contains';
  function  _sk4d_path_convert_conic_to_quads(const point1, point2, point3: psk_point_t; weight: float; points: psk_point_t; power2: int32_t): int32_t; cdecl; external LibraryName name 'sk4d_path_convert_conic_to_quads';
  function  _sk4d_path_create(const svg: MarshaledAString): sk_path_t; cdecl; external LibraryName name 'sk4d_path_create';
  function  _sk4d_path_create2(stream: sk_stream_t): sk_path_t; cdecl; external LibraryName name 'sk4d_path_create2';
  procedure _sk4d_path_destroy(self: sk_path_t); cdecl; external LibraryName name 'sk4d_path_destroy';
  procedure _sk4d_path_get_bounds(const self: sk_path_t; out result: sk_rect_t); cdecl; external LibraryName name 'sk4d_path_get_bounds';
  function  _sk4d_path_get_fill_type(const self: sk_path_t): sk_pathfilltype_t; cdecl; external LibraryName name 'sk4d_path_get_fill_type';
  function  _sk4d_path_get_last_point(const self: sk_path_t; out result: sk_point_t): _bool; cdecl; external LibraryName name 'sk4d_path_get_last_point';
  function  _sk4d_path_get_segment_masks(const self: sk_path_t): uint32_t; cdecl; external LibraryName name 'sk4d_path_get_segment_masks';
  procedure _sk4d_path_get_tight_bounds(const self: sk_path_t; out result: sk_rect_t); cdecl; external LibraryName name 'sk4d_path_get_tight_bounds';
  function  _sk4d_path_interpolate(const self, ending: sk_path_t; weight: float): sk_path_t; cdecl; external LibraryName name 'sk4d_path_interpolate';
  function  _sk4d_path_is_convex(const self: sk_path_t): _bool; cdecl; external LibraryName name 'sk4d_path_is_convex';
  function  _sk4d_path_is_empty(const self: sk_path_t): _bool; cdecl; external LibraryName name 'sk4d_path_is_empty';
  function  _sk4d_path_is_finite(const self: sk_path_t): _bool; cdecl; external LibraryName name 'sk4d_path_is_finite';
  function  _sk4d_path_is_interpolatable(const self, path: sk_path_t): _bool; cdecl; external LibraryName name 'sk4d_path_is_interpolatable';
  function  _sk4d_path_is_last_contour_closed(const self: sk_path_t): _bool; cdecl; external LibraryName name 'sk4d_path_is_last_contour_closed';
  function  _sk4d_path_is_line(const self: sk_path_t; lines: psk_point_t): _bool; cdecl; external LibraryName name 'sk4d_path_is_line';
  function  _sk4d_path_is_oval(const self: sk_path_t; oval: psk_rect_t): _bool; cdecl; external LibraryName name 'sk4d_path_is_oval';
  function  _sk4d_path_is_rect(const self: sk_path_t; rect: psk_rect_t): _bool; cdecl; external LibraryName name 'sk4d_path_is_rect';
  function  _sk4d_path_is_rrect(const self: sk_path_t; rrect: sk_rrect_t): _bool; cdecl; external LibraryName name 'sk4d_path_is_rrect';
  function  _sk4d_path_op(const self, path: sk_path_t; op: sk_pathop_t): sk_path_t; cdecl; external LibraryName name 'sk4d_path_op';
  procedure _sk4d_path_serialize_to_stream(const self: sk_path_t; w_stream: sk_wstream_t); cdecl; external LibraryName name 'sk4d_path_serialize_to_stream';
  function  _sk4d_path_to_svg(const self: sk_path_t): sk_string_t; cdecl; external LibraryName name 'sk4d_path_to_svg';
  function  _sk4d_path_transform(const self: sk_path_t; const matrix: psk_matrix_t): sk_path_t; cdecl; external LibraryName name 'sk4d_path_transform';
  function  _sk4d_pathiterator_create(const path: sk_path_t; force_close: _bool): sk_pathiterator_t; cdecl; external LibraryName name 'sk4d_pathiterator_create';
  procedure _sk4d_pathiterator_destroy(self: sk_pathiterator_t); cdecl; external LibraryName name 'sk4d_pathiterator_destroy';
  function  _sk4d_pathiterator_next(self: sk_pathiterator_t; out elem: sk_pathiteratorelem_t): _bool; cdecl; external LibraryName name 'sk4d_pathiterator_next';


 { include/c/sk4d_pathbuilder.h }

  procedure _sk4d_pathbuilder_add_arc(self: sk_pathbuilder_t; const oval: psk_rect_t; start_angle, sweep_angle: float); cdecl; external LibraryName name 'sk4d_pathbuilder_add_arc';
  procedure _sk4d_pathbuilder_add_circle(self: sk_pathbuilder_t; center_x, center_y, radius: float; direction: sk_pathdirection_t); cdecl; external LibraryName name 'sk4d_pathbuilder_add_circle';
  procedure _sk4d_pathbuilder_add_oval(self: sk_pathbuilder_t; const oval: psk_rect_t; direction: sk_pathdirection_t; start_index: uint32_t); cdecl; external LibraryName name 'sk4d_pathbuilder_add_oval';
  procedure _sk4d_pathbuilder_add_path(self: sk_pathbuilder_t; const path: sk_path_t); cdecl; external LibraryName name 'sk4d_pathbuilder_add_path';
  procedure _sk4d_pathbuilder_add_polygon(self: sk_pathbuilder_t; polygon: psk_point_t; count: int32_t; is_closed: _bool); cdecl; external LibraryName name 'sk4d_pathbuilder_add_polygon';
  procedure _sk4d_pathbuilder_add_rect(self: sk_pathbuilder_t; const rect: psk_rect_t; direction: sk_pathdirection_t; start_index: uint32_t); cdecl; external LibraryName name 'sk4d_pathbuilder_add_rect';
  procedure _sk4d_pathbuilder_add_rrect(self: sk_pathbuilder_t; const rrect: sk_rrect_t; direction: sk_pathdirection_t; start_index: uint32_t); cdecl; external LibraryName name 'sk4d_pathbuilder_add_rrect';
  procedure _sk4d_pathbuilder_arc_to(self: sk_pathbuilder_t; const radius: psk_point_t; x_axis_rotate: float; large_arc: sk_patharcsize_t; sweep: sk_pathdirection_t; const xy: psk_point_t); cdecl; external LibraryName name 'sk4d_pathbuilder_arc_to';
  procedure _sk4d_pathbuilder_arc_to2(self: sk_pathbuilder_t; const oval: psk_rect_t; start_angle, sweep_angle: float; force_move_to: _bool); cdecl; external LibraryName name 'sk4d_pathbuilder_arc_to2';
  procedure _sk4d_pathbuilder_arc_to3(self: sk_pathbuilder_t; const point1, point2: psk_point_t; radius: float); cdecl; external LibraryName name 'sk4d_pathbuilder_arc_to3';
  procedure _sk4d_pathbuilder_close(self: sk_pathbuilder_t); cdecl; external LibraryName name 'sk4d_pathbuilder_close';
  procedure _sk4d_pathbuilder_conic_to(self: sk_pathbuilder_t; const point1, point2: psk_point_t; weight: float); cdecl; external LibraryName name 'sk4d_pathbuilder_conic_to';
  function  _sk4d_pathbuilder_create(): sk_pathbuilder_t; cdecl; external LibraryName name 'sk4d_pathbuilder_create';
  function  _sk4d_pathbuilder_create2(const path_builder: sk_pathbuilder_t): sk_pathbuilder_t; cdecl; external LibraryName name 'sk4d_pathbuilder_create2';
  procedure _sk4d_pathbuilder_cubic_to(self: sk_pathbuilder_t; const point1, point2, point3: psk_point_t); cdecl; external LibraryName name 'sk4d_pathbuilder_cubic_to';
  procedure _sk4d_pathbuilder_destroy(self: sk_pathbuilder_t); cdecl; external LibraryName name 'sk4d_pathbuilder_destroy';
  function  _sk4d_pathbuilder_detach(self: sk_pathbuilder_t): sk_path_t; cdecl; external LibraryName name 'sk4d_pathbuilder_detach';
  procedure _sk4d_pathbuilder_get_bounds(const self: sk_pathbuilder_t; out result: sk_rect_t); cdecl; external LibraryName name 'sk4d_pathbuilder_get_bounds';
  function  _sk4d_pathbuilder_get_fill_type(const self: sk_pathbuilder_t): sk_pathfilltype_t; cdecl; external LibraryName name 'sk4d_pathbuilder_get_fill_type';
  procedure _sk4d_pathbuilder_inc_reserve(self: sk_pathbuilder_t; extra_point_count, extra_verb_count: int32_t); cdecl; external LibraryName name 'sk4d_pathbuilder_inc_reserve';
  procedure _sk4d_pathbuilder_line_to(self: sk_pathbuilder_t; const cpoint: psk_point_t); cdecl; external LibraryName name 'sk4d_pathbuilder_line_to';
  procedure _sk4d_pathbuilder_move_to(self: sk_pathbuilder_t; const cpoint: psk_point_t); cdecl; external LibraryName name 'sk4d_pathbuilder_move_to';
  procedure _sk4d_pathbuilder_offset(self: sk_pathbuilder_t; dx, dy: float); cdecl; external LibraryName name 'sk4d_pathbuilder_offset';
  procedure _sk4d_pathbuilder_polyline_to(self: sk_pathbuilder_t; const points: psk_point_t; count: int32_t); cdecl; external LibraryName name 'sk4d_pathbuilder_polyline_to';
  procedure _sk4d_pathbuilder_quad_to(self: sk_pathbuilder_t; const point1, point2: psk_point_t); cdecl; external LibraryName name 'sk4d_pathbuilder_quad_to';
  procedure _sk4d_pathbuilder_r_conic_to(self: sk_pathbuilder_t; const point1, point2: psk_point_t; weight: float); cdecl; external LibraryName name 'sk4d_pathbuilder_r_conic_to';
  procedure _sk4d_pathbuilder_r_cubic_to(self: sk_pathbuilder_t; const point1, point2, point3: psk_point_t); cdecl; external LibraryName name 'sk4d_pathbuilder_r_cubic_to';
  procedure _sk4d_pathbuilder_r_line_to(self: sk_pathbuilder_t; const point: psk_point_t); cdecl; external LibraryName name 'sk4d_pathbuilder_r_line_to';
  procedure _sk4d_pathbuilder_r_quad_to(self: sk_pathbuilder_t; const point1, point2: psk_point_t); cdecl; external LibraryName name 'sk4d_pathbuilder_r_quad_to';
  procedure _sk4d_pathbuilder_reset(self: sk_pathbuilder_t); cdecl; external LibraryName name 'sk4d_pathbuilder_reset';
  procedure _sk4d_pathbuilder_set_filltype(self: sk_pathbuilder_t; value: sk_pathfilltype_t); cdecl; external LibraryName name 'sk4d_pathbuilder_set_filltype';
  function  _sk4d_pathbuilder_snapshot(const self: sk_pathbuilder_t): sk_path_t; cdecl; external LibraryName name 'sk4d_pathbuilder_snapshot';
  procedure _sk4d_pathbuilder_toggle_inverse_filltype(self: sk_pathbuilder_t); cdecl; external LibraryName name 'sk4d_pathbuilder_toggle_inverse_filltype';


 { include/c/sk4d_patheffect.h }

  function  _sk4d_patheffect_make_1dpath(const path: sk_path_t; advance, phase: float; style: sk_patheffect1dstyle_t): sk_patheffect_t; cdecl; external LibraryName name 'sk4d_patheffect_make_1dpath';
  function  _sk4d_patheffect_make_2dline(width: float; const matrix: psk_matrix_t): sk_patheffect_t; cdecl; external LibraryName name 'sk4d_patheffect_make_2dline';
  function  _sk4d_patheffect_make_2dpath(const matrix: psk_matrix_t; const path: sk_path_t): sk_patheffect_t; cdecl; external LibraryName name 'sk4d_patheffect_make_2dpath';
  function  _sk4d_patheffect_make_compose(outer, inner: sk_patheffect_t): sk_patheffect_t; cdecl; external LibraryName name 'sk4d_patheffect_make_compose';
  function  _sk4d_patheffect_make_corner(radius: float): sk_patheffect_t; cdecl; external LibraryName name 'sk4d_patheffect_make_corner';
  function  _sk4d_patheffect_make_dash(const intervals: pfloat; count: int32_t; phase: float): sk_patheffect_t; cdecl; external LibraryName name 'sk4d_patheffect_make_dash';
  function  _sk4d_patheffect_make_discrete(seg_length, deviation: float; seed_assist: uint32_t): sk_patheffect_t; cdecl; external LibraryName name 'sk4d_patheffect_make_discrete';
  function  _sk4d_patheffect_make_matrix(const matrix: psk_matrix_t): sk_patheffect_t; cdecl; external LibraryName name 'sk4d_patheffect_make_matrix';
  function  _sk4d_patheffect_make_merge(effect1, effect2: sk_patheffect_t; op: sk_pathop_t): sk_patheffect_t; cdecl; external LibraryName name 'sk4d_patheffect_make_merge';
  function  _sk4d_patheffect_make_stroke(width: float; join: sk_strokejoin_t; cap: sk_strokecap_t; miter: float): sk_patheffect_t; cdecl; external LibraryName name 'sk4d_patheffect_make_stroke';
  function  _sk4d_patheffect_make_stroke_and_fill(): sk_patheffect_t; cdecl; external LibraryName name 'sk4d_patheffect_make_stroke_and_fill';
  function  _sk4d_patheffect_make_sum(effect1, effect2: sk_patheffect_t): sk_patheffect_t; cdecl; external LibraryName name 'sk4d_patheffect_make_sum';
  function  _sk4d_patheffect_make_translate(dx, dy: float): sk_patheffect_t; cdecl; external LibraryName name 'sk4d_patheffect_make_translate';
  function  _sk4d_patheffect_make_trim(start, stop: float; mode: sk_patheffecttrimmode_t): sk_patheffect_t; cdecl; external LibraryName name 'sk4d_patheffect_make_trim';


 { include/c/sk4d_pathmeasure.h }

  function  _sk4d_pathmeasure_create(const path: sk_path_t; force_closed: _bool; res_scale: float): sk_pathmeasure_t; cdecl; external LibraryName name 'sk4d_pathmeasure_create';
  procedure _sk4d_pathmeasure_destroy(self: sk_pathmeasure_t); cdecl; external LibraryName name 'sk4d_pathmeasure_destroy';
  function  _sk4d_pathmeasure_get_length(self: sk_pathmeasure_t): float; cdecl; external LibraryName name 'sk4d_pathmeasure_get_length';
  function  _sk4d_pathmeasure_get_matrix(self: sk_pathmeasure_t; distance: float; out matrix: sk_matrix_t; matrix_flags: uint32_t): _bool; cdecl; external LibraryName name 'sk4d_pathmeasure_get_matrix';
  function  _sk4d_pathmeasure_get_position_and_tangent(self: sk_pathmeasure_t; distance: float; out position: sk_point_t; out tangent: sk_vector_t): _bool; cdecl; external LibraryName name 'sk4d_pathmeasure_get_position_and_tangent';
  function  _sk4d_pathmeasure_get_segment(self: sk_pathmeasure_t; start, stop: float; start_with_move_to: _bool): sk_path_t; cdecl; external LibraryName name 'sk4d_pathmeasure_get_segment';
  function  _sk4d_pathmeasure_is_closed(self: sk_pathmeasure_t): _bool; cdecl; external LibraryName name 'sk4d_pathmeasure_is_closed';
  function  _sk4d_pathmeasure_next_contour(self: sk_pathmeasure_t): _bool; cdecl; external LibraryName name 'sk4d_pathmeasure_next_contour';


 { include/c/sk4d_picture.h }

  function  _sk4d_picture_approximate_bytes_used(const self: sk_picture_t): size_t; cdecl; external LibraryName name 'sk4d_picture_approximate_bytes_used';
  function  _sk4d_picture_approximate_op_count(const self: sk_picture_t; nested: _bool): int32_t; cdecl; external LibraryName name 'sk4d_picture_approximate_op_count';
  procedure _sk4d_picture_get_cull_rect(const self: sk_picture_t; out result: sk_rect_t); cdecl; external LibraryName name 'sk4d_picture_get_cull_rect';
  function  _sk4d_picture_make_from_stream(stream: sk_stream_t): sk_picture_t; cdecl; external LibraryName name 'sk4d_picture_make_from_stream';
  function  _sk4d_picture_make_shader(const self: sk_picture_t; tile_mode_x, tile_mode_y: sk_tilemode_t; filter_mode: sk_filtermode_t; const local_matrix: psk_matrix_t; const tile_rect: psk_rect_t): sk_shader_t; cdecl; external LibraryName name 'sk4d_picture_make_shader';
  procedure _sk4d_picture_playback(const self: sk_picture_t; canvas: sk_canvas_t); cdecl; external LibraryName name 'sk4d_picture_playback';
  procedure _sk4d_picture_serialize_to_stream(const self: sk_picture_t; w_stream: sk_wstream_t); cdecl; external LibraryName name 'sk4d_picture_serialize_to_stream';


 { include/c/sk4d_picturerecorder.h }

  function  _sk4d_picturerecorder_begin_recording(self: sk_picturerecorder_t; const bounds: psk_rect_t): sk_canvas_t; cdecl; external LibraryName name 'sk4d_picturerecorder_begin_recording';
  function  _sk4d_picturerecorder_create(): sk_picturerecorder_t; cdecl; external LibraryName name 'sk4d_picturerecorder_create';
  procedure _sk4d_picturerecorder_destroy(self: sk_picturerecorder_t); cdecl; external LibraryName name 'sk4d_picturerecorder_destroy';
  function  _sk4d_picturerecorder_finish_recording(self: sk_picturerecorder_t): sk_picture_t; cdecl; external LibraryName name 'sk4d_picturerecorder_finish_recording';
  function  _sk4d_picturerecorder_finish_recording2(self: sk_picturerecorder_t; const cull_rect: psk_rect_t): sk_picture_t; cdecl; external LibraryName name 'sk4d_picturerecorder_finish_recording2';


 { include/c/sk4d_pixmap.h }

  function  _sk4d_pixmap_create(const image_info: psk_imageinfo_t; const pixels: Pointer; row_bytes: size_t): sk_pixmap_t; cdecl; external LibraryName name 'sk4d_pixmap_create';
  procedure _sk4d_pixmap_destroy(self: sk_pixmap_t); cdecl; external LibraryName name 'sk4d_pixmap_destroy';
  function  _sk4d_pixmap_erase(const self: sk_pixmap_t; color: sk_color_t; const area: psk_irect_t): _bool; cdecl; external LibraryName name 'sk4d_pixmap_erase';
  function  _sk4d_pixmap_erase2(const self: sk_pixmap_t; const color: psk_color4f_t; color_space: sk_colorspace_t; const area: psk_irect_t): _bool; cdecl; external LibraryName name 'sk4d_pixmap_erase2';
  function  _sk4d_pixmap_extract_subset(const self: sk_pixmap_t; dest: sk_pixmap_t; const area: psk_irect_t): _bool; cdecl; external LibraryName name 'sk4d_pixmap_extract_subset';
  function  _sk4d_pixmap_get_alpha(const self: sk_pixmap_t; x, y: int32_t): float; cdecl; external LibraryName name 'sk4d_pixmap_get_alpha';
  function  _sk4d_pixmap_get_alpha_type(const self: sk_pixmap_t): sk_alphatype_t; cdecl; external LibraryName name 'sk4d_pixmap_get_alpha_type';
  function  _sk4d_pixmap_get_color(const self: sk_pixmap_t; x, y: int32_t): sk_color_t; cdecl; external LibraryName name 'sk4d_pixmap_get_color';
  function  _sk4d_pixmap_get_color_space(const self: sk_pixmap_t): sk_colorspace_t; cdecl; external LibraryName name 'sk4d_pixmap_get_color_space';
  function  _sk4d_pixmap_get_color_type(const self: sk_pixmap_t): sk_colortype_t; cdecl; external LibraryName name 'sk4d_pixmap_get_color_type';
  procedure _sk4d_pixmap_get_colorf(const self: sk_pixmap_t; x, y: int32_t; out result: sk_color4f_t); cdecl; external LibraryName name 'sk4d_pixmap_get_colorf';
  function  _sk4d_pixmap_get_height(const self: sk_pixmap_t): int32_t; cdecl; external LibraryName name 'sk4d_pixmap_get_height';
  procedure _sk4d_pixmap_get_image_info(const self: sk_pixmap_t; out result: sk_imageinfo_t); cdecl; external LibraryName name 'sk4d_pixmap_get_image_info';
  function  _sk4d_pixmap_get_pixel_addr(const self: sk_pixmap_t; x, y: int32_t): Pointer; cdecl; external LibraryName name 'sk4d_pixmap_get_pixel_addr';
  function  _sk4d_pixmap_get_pixels(const self: sk_pixmap_t): Pointer; cdecl; external LibraryName name 'sk4d_pixmap_get_pixels';
  function  _sk4d_pixmap_get_row_bytes(const self: sk_pixmap_t): size_t; cdecl; external LibraryName name 'sk4d_pixmap_get_row_bytes';
  function  _sk4d_pixmap_get_width(const self: sk_pixmap_t): int32_t; cdecl; external LibraryName name 'sk4d_pixmap_get_width';
  function  _sk4d_pixmap_read_pixels(const self, dest: sk_pixmap_t; src_x, src_y: int32_t): _bool; cdecl; external LibraryName name 'sk4d_pixmap_read_pixels';
  function  _sk4d_pixmap_scale_pixels(const self, dest: sk_pixmap_t; const sampling: psk_samplingoptions_t): _bool; cdecl; external LibraryName name 'sk4d_pixmap_scale_pixels';
  procedure _sk4d_pixmap_set_colorspace(self: sk_pixmap_t; value: sk_colorspace_t); cdecl; external LibraryName name 'sk4d_pixmap_set_colorspace';


 { include/c/sk4d_refcnt.h }

  procedure _sk4d_refcnt_ref(const self: sk_refcnt_t); cdecl; external LibraryName name 'sk4d_refcnt_ref';
  procedure _sk4d_refcnt_unref(const self: sk_refcnt_t); cdecl; external LibraryName name 'sk4d_refcnt_unref';


 { include/c/sk4d_region.h }

  function  _sk4d_region_contains(const self, region: sk_region_t): _bool; cdecl; external LibraryName name 'sk4d_region_contains';
  function  _sk4d_region_contains2(const self: sk_region_t; const rect: psk_irect_t): _bool; cdecl; external LibraryName name 'sk4d_region_contains2';
  function  _sk4d_region_contains3(const self: sk_region_t; x, y: int32_t): _bool; cdecl; external LibraryName name 'sk4d_region_contains3';
  function  _sk4d_region_create(): sk_region_t; cdecl; external LibraryName name 'sk4d_region_create';
  function  _sk4d_region_create2(const region: sk_region_t): sk_region_t; cdecl; external LibraryName name 'sk4d_region_create2';
  procedure _sk4d_region_destroy(self: sk_region_t); cdecl; external LibraryName name 'sk4d_region_destroy';
  function  _sk4d_region_get_boundary_path(const self: sk_region_t): sk_path_t; cdecl; external LibraryName name 'sk4d_region_get_boundary_path';
  procedure _sk4d_region_get_bounds(const self: sk_region_t; out result: sk_irect_t); cdecl; external LibraryName name 'sk4d_region_get_bounds';
  function  _sk4d_region_intersects(const self, region: sk_region_t): _bool; cdecl; external LibraryName name 'sk4d_region_intersects';
  function  _sk4d_region_intersects2(const self: sk_region_t; const rect: psk_irect_t): _bool; cdecl; external LibraryName name 'sk4d_region_intersects2';
  function  _sk4d_region_is_complex(const self: sk_region_t): _bool; cdecl; external LibraryName name 'sk4d_region_is_complex';
  function  _sk4d_region_is_empty(const self: sk_region_t): _bool; cdecl; external LibraryName name 'sk4d_region_is_empty';
  function  _sk4d_region_is_equal(const self, region: sk_region_t): _bool; cdecl; external LibraryName name 'sk4d_region_is_equal';
  function  _sk4d_region_is_rect(const self: sk_region_t): _bool; cdecl; external LibraryName name 'sk4d_region_is_rect';
  function  _sk4d_region_op(self: sk_region_t; const region: sk_region_t; op: sk_regionop_t): _bool; cdecl; external LibraryName name 'sk4d_region_op';
  function  _sk4d_region_op2(self: sk_region_t; const rect: psk_irect_t; op: sk_regionop_t): _bool; cdecl; external LibraryName name 'sk4d_region_op2';
  function  _sk4d_region_quick_contains(const self: sk_region_t; const rect: psk_irect_t): _bool; cdecl; external LibraryName name 'sk4d_region_quick_contains';
  function  _sk4d_region_quick_reject(const self, region: sk_region_t): _bool; cdecl; external LibraryName name 'sk4d_region_quick_reject';
  function  _sk4d_region_quick_reject2(const self: sk_region_t; const rect: psk_irect_t): _bool; cdecl; external LibraryName name 'sk4d_region_quick_reject2';
  procedure _sk4d_region_set_empty(self: sk_region_t); cdecl; external LibraryName name 'sk4d_region_set_empty';
  function  _sk4d_region_set_path(self: sk_region_t; const path: sk_path_t; const clip: sk_region_t): _bool; cdecl; external LibraryName name 'sk4d_region_set_path';
  function  _sk4d_region_set_rect(self: sk_region_t; const rect: psk_irect_t): _bool; cdecl; external LibraryName name 'sk4d_region_set_rect';
  function  _sk4d_region_set_rects(self: sk_region_t; const rects: psk_irect_t; count: int32_t): _bool; cdecl; external LibraryName name 'sk4d_region_set_rects';
  procedure _sk4d_region_translate(self: sk_region_t; x, y: int32_t); cdecl; external LibraryName name 'sk4d_region_translate';
  function  _sk4d_regioncliperator_create(const region: sk_region_t; const clip: psk_irect_t): sk_regioncliperator_t; cdecl; external LibraryName name 'sk4d_regioncliperator_create';
  procedure _sk4d_regioncliperator_destroy(self: sk_regioncliperator_t); cdecl; external LibraryName name 'sk4d_regioncliperator_destroy';
  procedure _sk4d_regioncliperator_get_current(const self: sk_regioncliperator_t; out result: sk_irect_t); cdecl; external LibraryName name 'sk4d_regioncliperator_get_current';
  function  _sk4d_regioncliperator_move_next(self: sk_regioncliperator_t): _bool; cdecl; external LibraryName name 'sk4d_regioncliperator_move_next';
  function  _sk4d_regioniterator_create(const region: sk_region_t): sk_regioniterator_t; cdecl; external LibraryName name 'sk4d_regioniterator_create';
  procedure _sk4d_regioniterator_destroy(self: sk_regioniterator_t); cdecl; external LibraryName name 'sk4d_regioniterator_destroy';
  procedure _sk4d_regioniterator_get_current(const self: sk_regioniterator_t; out result: sk_irect_t); cdecl; external LibraryName name 'sk4d_regioniterator_get_current';
  function  _sk4d_regioniterator_move_next(self: sk_regioniterator_t): _bool; cdecl; external LibraryName name 'sk4d_regioniterator_move_next';
  function  _sk4d_regionspanerator_create(const region: sk_region_t; y, left, right: int32_t): sk_regionspanerator_t; cdecl; external LibraryName name 'sk4d_regionspanerator_create';
  procedure _sk4d_regionspanerator_destroy(self: sk_regionspanerator_t); cdecl; external LibraryName name 'sk4d_regionspanerator_destroy';
  function  _sk4d_regionspanerator_next(self: sk_regionspanerator_t; out elem: sk_ipoint_t): _bool; cdecl; external LibraryName name 'sk4d_regionspanerator_next';


 { include/c/sk4d_rrect.h }

  function  _sk4d_rrect_contains(const self: sk_rrect_t; const rect: psk_rect_t): _bool; cdecl; external LibraryName name 'sk4d_rrect_contains';
  function  _sk4d_rrect_create(): sk_rrect_t; cdecl; external LibraryName name 'sk4d_rrect_create';
  function  _sk4d_rrect_create2(const rrect: sk_rrect_t): sk_rrect_t; cdecl; external LibraryName name 'sk4d_rrect_create2';
  procedure _sk4d_rrect_deflate(self: sk_rrect_t; dx, dy: float); cdecl; external LibraryName name 'sk4d_rrect_deflate';
  procedure _sk4d_rrect_destroy(self: sk_rrect_t); cdecl; external LibraryName name 'sk4d_rrect_destroy';
  function  _sk4d_rrect_get_height(const self: sk_rrect_t): float; cdecl; external LibraryName name 'sk4d_rrect_get_height';
  procedure _sk4d_rrect_get_radii(const self: sk_rrect_t; corner: sk_rrectcorner_t; out result: sk_vector_t); cdecl; external LibraryName name 'sk4d_rrect_get_radii';
  procedure _sk4d_rrect_get_rect(const self: sk_rrect_t; out result: sk_rect_t); cdecl; external LibraryName name 'sk4d_rrect_get_rect';
  procedure _sk4d_rrect_get_simple_radii(const self: sk_rrect_t; out result: sk_vector_t); cdecl; external LibraryName name 'sk4d_rrect_get_simple_radii';
  function  _sk4d_rrect_get_width(const self: sk_rrect_t): float; cdecl; external LibraryName name 'sk4d_rrect_get_width';
  procedure _sk4d_rrect_inflate(self: sk_rrect_t; dx, dy: float); cdecl; external LibraryName name 'sk4d_rrect_inflate';
  function  _sk4d_rrect_is_complex(const self: sk_rrect_t): _bool; cdecl; external LibraryName name 'sk4d_rrect_is_complex';
  function  _sk4d_rrect_is_empty(const self: sk_rrect_t): _bool; cdecl; external LibraryName name 'sk4d_rrect_is_empty';
  function  _sk4d_rrect_is_equal(const self, rrect: sk_rrect_t): _bool; cdecl; external LibraryName name 'sk4d_rrect_is_equal';
  function  _sk4d_rrect_is_nine_patch(const self: sk_rrect_t): _bool; cdecl; external LibraryName name 'sk4d_rrect_is_nine_patch';
  function  _sk4d_rrect_is_oval(const self: sk_rrect_t): _bool; cdecl; external LibraryName name 'sk4d_rrect_is_oval';
  function  _sk4d_rrect_is_rect(const self: sk_rrect_t): _bool; cdecl; external LibraryName name 'sk4d_rrect_is_rect';
  function  _sk4d_rrect_is_simple(const self: sk_rrect_t): _bool; cdecl; external LibraryName name 'sk4d_rrect_is_simple';
  function  _sk4d_rrect_is_valid(const self: sk_rrect_t): _bool; cdecl; external LibraryName name 'sk4d_rrect_is_valid';
  procedure _sk4d_rrect_offset(self: sk_rrect_t; dx, dy: float); cdecl; external LibraryName name 'sk4d_rrect_offset';
  procedure _sk4d_rrect_set_empty(self: sk_rrect_t); cdecl; external LibraryName name 'sk4d_rrect_set_empty';
  procedure _sk4d_rrect_set_nine_patch(self: sk_rrect_t; const rect: psk_rect_t; radius_left, radius_top, radius_right, radius_bottom: float); cdecl; external LibraryName name 'sk4d_rrect_set_nine_patch';
  procedure _sk4d_rrect_set_oval(self: sk_rrect_t; const rect: psk_rect_t); cdecl; external LibraryName name 'sk4d_rrect_set_oval';
  procedure _sk4d_rrect_set_rect(self: sk_rrect_t; const rect: psk_rect_t); cdecl; external LibraryName name 'sk4d_rrect_set_rect';
  procedure _sk4d_rrect_set_rect2(self: sk_rrect_t; const rect: psk_rect_t; const radii: psk_vector_t); cdecl; external LibraryName name 'sk4d_rrect_set_rect2';
  procedure _sk4d_rrect_set_rect3(self: sk_rrect_t; const rect: psk_rect_t; radius_x, radius_y: float); cdecl; external LibraryName name 'sk4d_rrect_set_rect3';
  function  _sk4d_rrect_transform(const self: sk_rrect_t; const matrix: psk_matrix_t): sk_rrect_t; cdecl; external LibraryName name 'sk4d_rrect_transform';


 { include/c/sk4d_runtimeeffect.h }

  function  _sk4d_runtimeblendbuilder_create(effect: sk_runtimeeffect_t): sk_runtimeblendbuilder_t; cdecl; external LibraryName name 'sk4d_runtimeblendbuilder_create';
  procedure _sk4d_runtimeblendbuilder_destroy(self: sk_runtimeblendbuilder_t); cdecl; external LibraryName name 'sk4d_runtimeblendbuilder_destroy';
  function  _sk4d_runtimeblendbuilder_make_blender(self: sk_runtimeblendbuilder_t): sk_blender_t; cdecl; external LibraryName name 'sk4d_runtimeblendbuilder_make_blender';
  function  _sk4d_runtimeeffect_get_child_count(const self: sk_runtimeeffect_t): int32_t; cdecl; external LibraryName name 'sk4d_runtimeeffect_get_child_count';
  function  _sk4d_runtimeeffect_get_child_name(const self: sk_runtimeeffect_t; index: int32_t): sk_string_t; cdecl; external LibraryName name 'sk4d_runtimeeffect_get_child_name';
  function  _sk4d_runtimeeffect_get_child_type(const self: sk_runtimeeffect_t; index: int32_t): sk_runtimeeffectchildtype_t; cdecl; external LibraryName name 'sk4d_runtimeeffect_get_child_type';
  function  _sk4d_runtimeeffect_get_uniform_count(const self: sk_runtimeeffect_t): int32_t; cdecl; external LibraryName name 'sk4d_runtimeeffect_get_uniform_count';
  function  _sk4d_runtimeeffect_get_uniform_data_size(const self: sk_runtimeeffect_t): size_t; cdecl; external LibraryName name 'sk4d_runtimeeffect_get_uniform_data_size';
  function  _sk4d_runtimeeffect_get_uniform_name(const self: sk_runtimeeffect_t; index: int32_t): sk_string_t; cdecl; external LibraryName name 'sk4d_runtimeeffect_get_uniform_name';
  function  _sk4d_runtimeeffect_get_uniform_offset(const self: sk_runtimeeffect_t; index: int32_t): size_t; cdecl; external LibraryName name 'sk4d_runtimeeffect_get_uniform_offset';
  function  _sk4d_runtimeeffect_get_uniform_type(const self: sk_runtimeeffect_t; index: int32_t): sk_runtimeeffectuniformtype_t; cdecl; external LibraryName name 'sk4d_runtimeeffect_get_uniform_type';
  function  _sk4d_runtimeeffect_get_uniform_type_count(const self: sk_runtimeeffect_t; index: int32_t): int32_t; cdecl; external LibraryName name 'sk4d_runtimeeffect_get_uniform_type_count';
  function  _sk4d_runtimeeffect_index_of_child(const self: sk_runtimeeffect_t; const name: MarshaledAString): int32_t; cdecl; external LibraryName name 'sk4d_runtimeeffect_index_of_child';
  function  _sk4d_runtimeeffect_index_of_uniform(const self: sk_runtimeeffect_t; const name: MarshaledAString): int32_t; cdecl; external LibraryName name 'sk4d_runtimeeffect_index_of_uniform';
  function  _sk4d_runtimeeffect_make_blender(const self: sk_runtimeeffect_t; const uniforms: Pointer; children: psk_flattenable_t): sk_blender_t; cdecl; external LibraryName name 'sk4d_runtimeeffect_make_blender';
  function  _sk4d_runtimeeffect_make_color_filter(const self: sk_runtimeeffect_t; const uniforms: Pointer; children: psk_flattenable_t): sk_colorfilter_t; cdecl; external LibraryName name 'sk4d_runtimeeffect_make_color_filter';
  function  _sk4d_runtimeeffect_make_for_blender(const sksl: MarshaledAString; error_text: sk_string_t): sk_runtimeeffect_t; cdecl; external LibraryName name 'sk4d_runtimeeffect_make_for_blender';
  function  _sk4d_runtimeeffect_make_for_color_filter(const sksl: MarshaledAString; error_text: sk_string_t): sk_runtimeeffect_t; cdecl; external LibraryName name 'sk4d_runtimeeffect_make_for_color_filter';
  function  _sk4d_runtimeeffect_make_for_shader(const sksl: MarshaledAString; error_text: sk_string_t): sk_runtimeeffect_t; cdecl; external LibraryName name 'sk4d_runtimeeffect_make_for_shader';
  function  _sk4d_runtimeeffect_make_image(const self: sk_runtimeeffect_t; context: gr_directcontext_t; const uniforms: Pointer; children: psk_flattenable_t; const local_matrix: psk_matrix_t; const image_info: psk_imageinfo_t; mipmapped: _bool): sk_image_t; cdecl; external LibraryName name 'sk4d_runtimeeffect_make_image';
  function  _sk4d_runtimeeffect_make_shader(const self: sk_runtimeeffect_t; const uniforms: Pointer; children: psk_flattenable_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl; external LibraryName name 'sk4d_runtimeeffect_make_shader';
  procedure _sk4d_runtimeeffectbuilder_set_child(self: sk_runtimeeffectbuilder_t; const name: MarshaledAString; shader: sk_shader_t); cdecl; external LibraryName name 'sk4d_runtimeeffectbuilder_set_child';
  procedure _sk4d_runtimeeffectbuilder_set_child2(self: sk_runtimeeffectbuilder_t; const name: MarshaledAString; color_filter: sk_colorfilter_t); cdecl; external LibraryName name 'sk4d_runtimeeffectbuilder_set_child2';
  procedure _sk4d_runtimeeffectbuilder_set_child3(self: sk_runtimeeffectbuilder_t; const name: MarshaledAString; blender: sk_blender_t); cdecl; external LibraryName name 'sk4d_runtimeeffectbuilder_set_child3';
  procedure _sk4d_runtimeeffectbuilder_set_uniform(self: sk_runtimeeffectbuilder_t; const name: MarshaledAString; const data: Pointer); cdecl; external LibraryName name 'sk4d_runtimeeffectbuilder_set_uniform';
  function  _sk4d_runtimeeffectbuilder_get_effect(const self: sk_runtimeeffectbuilder_t): sk_runtimeeffect_t; cdecl; external LibraryName name 'sk4d_runtimeeffectbuilder_get_effect';
  function  _sk4d_runtimeshaderbuilder_create(effect: sk_runtimeeffect_t): sk_runtimeshaderbuilder_t; cdecl; external LibraryName name 'sk4d_runtimeshaderbuilder_create';
  procedure _sk4d_runtimeshaderbuilder_destroy(self: sk_runtimeshaderbuilder_t); cdecl; external LibraryName name 'sk4d_runtimeshaderbuilder_destroy';
  function  _sk4d_runtimeshaderbuilder_make_image(self: sk_runtimeshaderbuilder_t; context: gr_directcontext_t; const local_matrix: psk_matrix_t; const image_info: psk_imageinfo_t; mipmapped: _bool): sk_image_t; cdecl; external LibraryName name 'sk4d_runtimeshaderbuilder_make_image';
  function  _sk4d_runtimeshaderbuilder_make_shader(self: sk_runtimeshaderbuilder_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl; external LibraryName name 'sk4d_runtimeshaderbuilder_make_shader';


 { include/c/sk4d_shader.h }

  function  _sk4d_shader_make_blend(mode: sk_blendmode_t; dest, src: sk_shader_t): sk_shader_t; cdecl; external LibraryName name 'sk4d_shader_make_blend';
  function  _sk4d_shader_make_color(color: sk_color_t): sk_shader_t; cdecl; external LibraryName name 'sk4d_shader_make_color';
  function  _sk4d_shader_make_color2(const color: psk_color4f_t; color_space: sk_colorspace_t): sk_shader_t; cdecl; external LibraryName name 'sk4d_shader_make_color2';
  function  _sk4d_shader_make_empty(): sk_shader_t; cdecl; external LibraryName name 'sk4d_shader_make_empty';
  function  _sk4d_shader_make_gradient_linear(const points: psk_point_t; const colors: psk_color_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl; external LibraryName name 'sk4d_shader_make_gradient_linear';
  function  _sk4d_shader_make_gradient_linear2(const points: psk_point_t; const colors: psk_color4f_t; color_space: sk_colorspace_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl; external LibraryName name 'sk4d_shader_make_gradient_linear2';
  function  _sk4d_shader_make_gradient_radial(const center: psk_point_t; radius: float; const colors: psk_color_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl; external LibraryName name 'sk4d_shader_make_gradient_radial';
  function  _sk4d_shader_make_gradient_radial2(const center: psk_point_t; radius: float; const colors: psk_color4f_t; color_space: sk_colorspace_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl; external LibraryName name 'sk4d_shader_make_gradient_radial2';
  function  _sk4d_shader_make_gradient_sweep(center_x, center_y: float; const colors: psk_color_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; start_angle, end_angle: float; const local_matrix: psk_matrix_t): sk_shader_t; cdecl; external LibraryName name 'sk4d_shader_make_gradient_sweep';
  function  _sk4d_shader_make_gradient_sweep2(center_x, center_y: float; const colors: psk_color4f_t; color_space: sk_colorspace_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; start_angle, end_angle: float; const local_matrix: psk_matrix_t): sk_shader_t; cdecl; external LibraryName name 'sk4d_shader_make_gradient_sweep2';
  function  _sk4d_shader_make_gradient_two_point_conical(const start: psk_point_t; start_radius: float; const &end: psk_point_t; end_radius: float; const colors: psk_color_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl; external LibraryName name 'sk4d_shader_make_gradient_two_point_conical';
  function  _sk4d_shader_make_gradient_two_point_conical2(const start: psk_point_t; start_radius: float; const &end: psk_point_t; end_radius: float; const colors: psk_color4f_t; color_space: sk_colorspace_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl; external LibraryName name 'sk4d_shader_make_gradient_two_point_conical2';
  function  _sk4d_shader_make_perlin_noise_fractal_noise(base_frequency_x, base_frequency_y: float; num_octaves: int32_t; seed: float; const tile_size: psk_isize_t): sk_shader_t; cdecl; external LibraryName name 'sk4d_shader_make_perlin_noise_fractal_noise';
  function  _sk4d_shader_make_perlin_noise_turbulence(base_frequency_x, base_frequency_y: float; num_octaves: int32_t; seed: float; const tile_size: psk_isize_t): sk_shader_t; cdecl; external LibraryName name 'sk4d_shader_make_perlin_noise_turbulence';
  function  _sk4d_shader_make_with_color_filter(const self: sk_shader_t; color_filter: sk_colorfilter_t): sk_shader_t; cdecl; external LibraryName name 'sk4d_shader_make_with_color_filter';
  function  _sk4d_shader_make_with_local_matrix(const self: sk_shader_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl; external LibraryName name 'sk4d_shader_make_with_local_matrix';


 { include/c/sk4d_stream.h }

  function  _sk4d_streamadapter_create(context: Pointer): sk_streamadapter_t; cdecl; external LibraryName name 'sk4d_streamadapter_create';
  procedure _sk4d_streamadapter_destroy(self: sk_streamadapter_t); cdecl; external LibraryName name 'sk4d_streamadapter_destroy';
  procedure _sk4d_streamadapter_set_procs(const procs: psk_streamadapter_procs_t); cdecl; external LibraryName name 'sk4d_streamadapter_set_procs';
  function  _sk4d_wstreamadapter_create(context: Pointer): sk_wstreamadapter_t; cdecl; external LibraryName name 'sk4d_wstreamadapter_create';
  procedure _sk4d_wstreamadapter_destroy(self: sk_wstreamadapter_t); cdecl; external LibraryName name 'sk4d_wstreamadapter_destroy';
  procedure _sk4d_wstreamadapter_set_procs(const procs: psk_wstreamadapter_procs_t); cdecl; external LibraryName name 'sk4d_wstreamadapter_set_procs';


 { include/c/sk4d_string.h }

  function  _sk4d_string_create(): sk_string_t; cdecl; external LibraryName name 'sk4d_string_create';
  procedure _sk4d_string_destroy(self: sk_string_t); cdecl; external LibraryName name 'sk4d_string_destroy';
  function  _sk4d_string_get_text(const self: sk_string_t): MarshaledAString; cdecl; external LibraryName name 'sk4d_string_get_text';


 { include/c/sk4d_surface.h }

  procedure _sk4d_surface_draw(self: sk_surface_t; canvas: sk_canvas_t; x, y: float; paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_surface_draw';
  procedure _sk4d_surface_flush(self: sk_surface_t); cdecl; external LibraryName name 'sk4d_surface_flush';
  procedure _sk4d_surface_flush_and_submit(self: sk_surface_t; semaphores: pgr_backendsemaphore_t; count: int32_t; const new_state: gr_backendsurfacemutablestate_t; sync_cpu: _bool); cdecl; external LibraryName name 'sk4d_surface_flush_and_submit';
  function  _sk4d_surface_get_canvas(self: sk_surface_t): sk_canvas_t; cdecl; external LibraryName name 'sk4d_surface_get_canvas';
  procedure _sk4d_surface_get_props(const self: sk_surface_t; out result: sk_surfaceprops_t); cdecl; external LibraryName name 'sk4d_surface_get_props';
  function  _sk4d_surface_make_from_mtk_view(context: gr_directcontext_t; layer: gr_mtl_handle_t; origin: gr_surfaceorigin_t; sample_count: int32_t; color_type: sk_colortype_t; color_space: sk_colorspace_t; const props: psk_surfaceprops_t): sk_surface_t; cdecl; external LibraryName name 'sk4d_surface_make_from_mtk_view';
  function  _sk4d_surface_make_from_render_target(context: gr_directcontext_t; const render_target: gr_backendrendertarget_t; origin: gr_surfaceorigin_t; color_type: sk_colortype_t; color_space: sk_colorspace_t; const props: psk_surfaceprops_t): sk_surface_t; cdecl; external LibraryName name 'sk4d_surface_make_from_render_target';
  function  _sk4d_surface_make_from_texture(context: gr_directcontext_t; const texture: gr_backendtexture_t; origin: gr_surfaceorigin_t; sample_count: int32_t; color_type: sk_colortype_t; color_space: sk_colorspace_t; const props: psk_surfaceprops_t): sk_surface_t; cdecl; external LibraryName name 'sk4d_surface_make_from_texture';
  function  _sk4d_surface_make_image_snapshot(self: sk_surface_t): sk_image_t; cdecl; external LibraryName name 'sk4d_surface_make_image_snapshot';
  function  _sk4d_surface_make_image_snapshot2(self: sk_surface_t; const bounds: psk_irect_t): sk_image_t; cdecl; external LibraryName name 'sk4d_surface_make_image_snapshot2';
  function  _sk4d_surface_make_raster(const image_info: psk_imageinfo_t; row_bytes: size_t; const props: psk_surfaceprops_t): sk_surface_t; cdecl; external LibraryName name 'sk4d_surface_make_raster';
  function  _sk4d_surface_make_raster_direct(const pixmap: sk_pixmap_t; proc: sk_surface_raster_release_proc; proc_context: Pointer; const props: psk_surfaceprops_t): sk_surface_t; cdecl; external LibraryName name 'sk4d_surface_make_raster_direct';
  function  _sk4d_surface_make_render_target(context: gr_directcontext_t; is_budgeted: _bool; const image_info: psk_imageinfo_t; sample_count: int32_t; origin: gr_surfaceorigin_t; const props: psk_surfaceprops_t; should_create_with_mips: _bool): sk_surface_t; cdecl; external LibraryName name 'sk4d_surface_make_render_target';
  function  _sk4d_surface_peek_pixels(self: sk_surface_t): sk_pixmap_t; cdecl; external LibraryName name 'sk4d_surface_peek_pixels';
  function  _sk4d_surface_read_pixels(self: sk_surface_t; const dest: sk_pixmap_t; src_x, src_y: int32_t): _bool; cdecl; external LibraryName name 'sk4d_surface_read_pixels';
  procedure _sk4d_surface_wait(self: sk_surface_t; const semaphores: pgr_backendsemaphore_t; count: int32_t); cdecl; external LibraryName name 'sk4d_surface_wait';
  procedure _sk4d_surface_write_pixels(self: sk_surface_t; const src: sk_pixmap_t; dest_x, dest_y: int32_t); cdecl; external LibraryName name 'sk4d_surface_write_pixels';


 { include/c/sk4d_svgcanvas.h }

  function  _sk4d_svgcanvas_make(const bounds: psk_rect_t; w_stream: sk_wstream_t; flags: uint32_t): sk_canvas_t; cdecl; external LibraryName name 'sk4d_svgcanvas_make';


 { include/c/sk4d_textblob.h }

  function  _sk4d_textblob_get_intercepts(const self: sk_textblob_t; const bounds: pfloat; result: pfloat; const paint: sk_paint_t): int32_t; cdecl; external LibraryName name 'sk4d_textblob_get_intercepts';
  function  _sk4d_textblob_make_from_text(const text: Pointer; size: size_t; const font: sk_font_t; encoding: sk_textencoding_t): sk_textblob_t; cdecl; external LibraryName name 'sk4d_textblob_make_from_text';
  function  _sk4d_textblob_make_from_text_horizontally_positioned(const text: Pointer; size: size_t; const x_positions: pfloat; y: float; const font: sk_font_t; encoding: sk_textencoding_t): sk_textblob_t; cdecl; external LibraryName name 'sk4d_textblob_make_from_text_horizontally_positioned';
  function  _sk4d_textblob_make_from_text_positioned(const text: Pointer; size: size_t; const positions: psk_point_t; const font: sk_font_t; encoding: sk_textencoding_t): sk_textblob_t; cdecl; external LibraryName name 'sk4d_textblob_make_from_text_positioned';
  function  _sk4d_textblob_make_from_text_transform(const text: Pointer; size: size_t; const matrices: psk_rotationscalematrix_t; const font: sk_font_t; encoding: sk_textencoding_t): sk_textblob_t; cdecl; external LibraryName name 'sk4d_textblob_make_from_text_transform';
  procedure _sk4d_textblob_ref(const self: sk_textblob_t); cdecl; external LibraryName name 'sk4d_textblob_ref';
  procedure _sk4d_textblob_unref(const self: sk_textblob_t); cdecl; external LibraryName name 'sk4d_textblob_unref';


 { include/c/sk4d_tracememorydump.h }

  function  _sk4d_tracememorydumpbaseclass_create(detailed_dump, dump_wrapped_objects: _bool; context: Pointer): sk_tracememorydumpbaseclass_t; cdecl; external LibraryName name 'sk4d_tracememorydumpbaseclass_create';
  procedure _sk4d_tracememorydumpbaseclass_destroy(self: sk_tracememorydumpbaseclass_t); cdecl; external LibraryName name 'sk4d_tracememorydumpbaseclass_destroy';
  procedure _sk4d_tracememorydumpbaseclass_set_procs(const procs: psk_tracememorydumpbaseclass_procs_t); cdecl; external LibraryName name 'sk4d_tracememorydumpbaseclass_set_procs';


 { include/c/sk4d_typeface.h }

  function  _sk4d_typeface_get_family_name(const self: sk_typeface_t): sk_string_t; cdecl; external LibraryName name 'sk4d_typeface_get_family_name';
  function  _sk4d_typeface_get_slant(const self: sk_typeface_t): sk_fontslant_t; cdecl; external LibraryName name 'sk4d_typeface_get_slant';
  procedure _sk4d_typeface_get_style(const self: sk_typeface_t; out result: sk_fontstyle_t); cdecl; external LibraryName name 'sk4d_typeface_get_style';
  function  _sk4d_typeface_get_weight(const self: sk_typeface_t): int32_t; cdecl; external LibraryName name 'sk4d_typeface_get_weight';
  function  _sk4d_typeface_get_width(const self: sk_typeface_t): int32_t; cdecl; external LibraryName name 'sk4d_typeface_get_width';
  function  _sk4d_typeface_make_default(): sk_typeface_t; cdecl; external LibraryName name 'sk4d_typeface_make_default';
  function  _sk4d_typeface_make_from_file(const file_name: MarshaledAString; ttc_index: int32_t): sk_typeface_t; cdecl; external LibraryName name 'sk4d_typeface_make_from_file';
  function  _sk4d_typeface_make_from_stream(stream: sk_stream_t; ttc_index: int32_t): sk_typeface_t; cdecl; external LibraryName name 'sk4d_typeface_make_from_stream';
  function  _sk4d_typeface_make_from_name(const family_name: MarshaledAString; const style: psk_fontstyle_t): sk_typeface_t; cdecl; external LibraryName name 'sk4d_typeface_make_from_name';


 { include/c/sk4d_vertices.h }

  function  _sk4d_vertices_make_copy(vertex_mode: sk_vertexmode_t; vertex_count: int32_t; const positions, textures: psk_point_t; const colors: psk_color_t; index_count: int32_t; const indices: puint16_t): sk_vertices_t; cdecl; external LibraryName name 'sk4d_vertices_make_copy';
  procedure _sk4d_vertices_ref(const self: sk_vertices_t); cdecl; external LibraryName name 'sk4d_vertices_ref';
  procedure _sk4d_vertices_unref(const self: sk_vertices_t); cdecl; external LibraryName name 'sk4d_vertices_unref';


 { modules/particles/include/sk4d_particleeffect.h }

  procedure _sk4d_particleeffect_get_position(const self: sk_particleeffect_t; out result: sk_point_t); cdecl; external LibraryName name 'sk4d_particleeffect_get_position';
  function  _sk4d_particleeffect_get_rate(const self: sk_particleeffect_t): float; cdecl; external LibraryName name 'sk4d_particleeffect_get_rate';
  procedure _sk4d_particleeffect_get_uniform(const self: sk_particleeffect_t; index: size_t; out result: sk_particleuniform_t); cdecl; external LibraryName name 'sk4d_particleeffect_get_uniform';
  function  _sk4d_particleeffect_get_uniform_count(const self: sk_particleeffect_t): size_t; cdecl; external LibraryName name 'sk4d_particleeffect_get_uniform_count';
  function  _sk4d_particleeffect_get_uniform_data(self: sk_particleeffect_t): pfloat; cdecl; external LibraryName name 'sk4d_particleeffect_get_uniform_data';
  function  _sk4d_particleeffect_get_uniform_data_count(const self: sk_particleeffect_t): int32_t; cdecl; external LibraryName name 'sk4d_particleeffect_get_uniform_data_count';
  function  _sk4d_particleeffect_get_uniform_name(const self: sk_particleeffect_t; index: size_t): sk_string_t; cdecl; external LibraryName name 'sk4d_particleeffect_get_uniform_name';
  procedure _sk4d_particleeffect_init(); cdecl; external LibraryName name 'sk4d_particleeffect_init';
  function  _sk4d_particleeffect_make_from_file(const file_name: MarshaledAString): sk_particleeffect_t; cdecl; external LibraryName name 'sk4d_particleeffect_make_from_file';
  function  _sk4d_particleeffect_make_from_stream(stream: sk_stream_t; resource_provider: sk_resourceprovider_t): sk_particleeffect_t; cdecl; external LibraryName name 'sk4d_particleeffect_make_from_stream';
  procedure _sk4d_particleeffect_render(self: sk_particleeffect_t; canvas: sk_canvas_t); cdecl; external LibraryName name 'sk4d_particleeffect_render';
  procedure _sk4d_particleeffect_set_position(self: sk_particleeffect_t; const value: psk_point_t); cdecl; external LibraryName name 'sk4d_particleeffect_set_position';
  procedure _sk4d_particleeffect_set_rate(self: sk_particleeffect_t; value: float); cdecl; external LibraryName name 'sk4d_particleeffect_set_rate';
  function  _sk4d_particleeffect_set_uniform(self: sk_particleeffect_t; const name: MarshaledAString; const data: pfloat; count: int32_t): _bool; cdecl; external LibraryName name 'sk4d_particleeffect_set_uniform';
  procedure _sk4d_particleeffect_start(self: sk_particleeffect_t; now: _double; looping: _bool); cdecl; external LibraryName name 'sk4d_particleeffect_start';
  procedure _sk4d_particleeffect_update(self: sk_particleeffect_t; now: _double); cdecl; external LibraryName name 'sk4d_particleeffect_update';


 { modules/skottie/include/sk4d_skottie.h }

  function  _sk4d_skottieanimation_get_duration(const self: sk_skottieanimation_t): _double; cdecl; external LibraryName name 'sk4d_skottieanimation_get_duration';
  function  _sk4d_skottieanimation_get_fps(const self: sk_skottieanimation_t): _double; cdecl; external LibraryName name 'sk4d_skottieanimation_get_fps';
  function  _sk4d_skottieanimation_get_in_point(const self: sk_skottieanimation_t): _double; cdecl; external LibraryName name 'sk4d_skottieanimation_get_in_point';
  function  _sk4d_skottieanimation_get_out_point(const self: sk_skottieanimation_t): _double; cdecl; external LibraryName name 'sk4d_skottieanimation_get_out_point';
  procedure _sk4d_skottieanimation_get_size(const self: sk_skottieanimation_t; out result: sk_size_t); cdecl; external LibraryName name 'sk4d_skottieanimation_get_size';
  function  _sk4d_skottieanimation_get_version(const self: sk_skottieanimation_t): MarshaledAString; cdecl; external LibraryName name 'sk4d_skottieanimation_get_version';
  function  _sk4d_skottieanimation_make_from_file(const file_name: MarshaledAString; font_provider: sk_fontmgr_t): sk_skottieanimation_t; cdecl; external LibraryName name 'sk4d_skottieanimation_make_from_file';
  function  _sk4d_skottieanimation_make_from_stream(stream: sk_stream_t; resource_provider: sk_resourceprovider_t; font_provider: sk_fontmgr_t): sk_skottieanimation_t; cdecl; external LibraryName name 'sk4d_skottieanimation_make_from_stream';
  procedure _sk4d_skottieanimation_ref(const self: sk_skottieanimation_t); cdecl; external LibraryName name 'sk4d_skottieanimation_ref';
  procedure _sk4d_skottieanimation_render(const self: sk_skottieanimation_t; canvas: sk_canvas_t; const dest: psk_rect_t; render_flags: uint32_t); cdecl; external LibraryName name 'sk4d_skottieanimation_render';
  procedure _sk4d_skottieanimation_seek_frame(self: sk_skottieanimation_t; tick: _double); cdecl; external LibraryName name 'sk4d_skottieanimation_seek_frame';
  procedure _sk4d_skottieanimation_seek_frame_time(self: sk_skottieanimation_t; tick: _double); cdecl; external LibraryName name 'sk4d_skottieanimation_seek_frame_time';
  procedure _sk4d_skottieanimation_unref(const self: sk_skottieanimation_t); cdecl; external LibraryName name 'sk4d_skottieanimation_unref';


 { modules/skparagraph/include/sk4d_paragraph.h }

  procedure _sk4d_paragraph_destroy(self: sk_paragraph_t); cdecl; external LibraryName name 'sk4d_paragraph_destroy';
  function  _sk4d_paragraph_did_exceed_max_lines(self: sk_paragraph_t): _bool; cdecl; external LibraryName name 'sk4d_paragraph_did_exceed_max_lines';
  function  _sk4d_paragraph_get_alphabetic_baseline(self: sk_paragraph_t): float; cdecl; external LibraryName name 'sk4d_paragraph_get_alphabetic_baseline';
  procedure _sk4d_paragraph_get_glyph_position_at_coordinate(self: sk_paragraph_t; dx, dy: float; out result: sk_positionaffinity_t); cdecl; external LibraryName name 'sk4d_paragraph_get_glyph_position_at_coordinate';
  function  _sk4d_paragraph_get_height(self: sk_paragraph_t): float; cdecl; external LibraryName name 'sk4d_paragraph_get_height';
  function  _sk4d_paragraph_get_ideographic_baseline(self: sk_paragraph_t): float; cdecl; external LibraryName name 'sk4d_paragraph_get_ideographic_baseline';
  function  _sk4d_paragraph_get_line_metrics(self: sk_paragraph_t; result: psk_metrics_t): size_t; cdecl; external LibraryName name 'sk4d_paragraph_get_line_metrics';
  function  _sk4d_paragraph_get_longest_line(self: sk_paragraph_t): float; cdecl; external LibraryName name 'sk4d_paragraph_get_longest_line';
  function  _sk4d_paragraph_get_max_intrinsic_width(self: sk_paragraph_t): float; cdecl; external LibraryName name 'sk4d_paragraph_get_max_intrinsic_width';
  function  _sk4d_paragraph_get_max_width(self: sk_paragraph_t): float; cdecl; external LibraryName name 'sk4d_paragraph_get_max_width';
  function  _sk4d_paragraph_get_min_intrinsic_width(self: sk_paragraph_t): float; cdecl; external LibraryName name 'sk4d_paragraph_get_min_intrinsic_width';
  function  _sk4d_paragraph_get_rects_for_placeholders(self: sk_paragraph_t; result: psk_textbox_t): size_t; cdecl; external LibraryName name 'sk4d_paragraph_get_rects_for_placeholders';
  function  _sk4d_paragraph_get_rects_for_range(self: sk_paragraph_t; start, &end: uint32_t; rect_height_style: sk_rectheightstyle_t; rect_width_style: sk_rectwidthstyle_t; result: psk_textbox_t): size_t; cdecl; external LibraryName name 'sk4d_paragraph_get_rects_for_range';
  procedure _sk4d_paragraph_get_word_boundary(self: sk_paragraph_t; offset: uint32_t; out start, &end: uint32_t); cdecl; external LibraryName name 'sk4d_paragraph_get_word_boundary';
  procedure _sk4d_paragraph_layout(self: sk_paragraph_t; width: float); cdecl; external LibraryName name 'sk4d_paragraph_layout';
  procedure _sk4d_paragraph_paint(self: sk_paragraph_t; canvas: sk_canvas_t; x, y: float); cdecl; external LibraryName name 'sk4d_paragraph_paint';
  function  _sk4d_paragraph_to_path(self: sk_paragraph_t): sk_path_t; cdecl; external LibraryName name 'sk4d_paragraph_to_path';
  procedure _sk4d_paragraph_visit(self: sk_paragraph_t; proc: sk_paragraph_visit_proc; proc_context: Pointer); cdecl; external LibraryName name 'sk4d_paragraph_visit';


 { modules/skparagraph/include/sk4d_paragraphbuilder.h }

  procedure _sk4d_paragraphbuilder_add_placeholder(self: sk_paragraphbuilder_t; const placeholder: psk_placeholderstyle_t); cdecl; external LibraryName name 'sk4d_paragraphbuilder_add_placeholder';
  procedure _sk4d_paragraphbuilder_add_text(self: sk_paragraphbuilder_t; const text: MarshaledAString); cdecl; external LibraryName name 'sk4d_paragraphbuilder_add_text';
  function  _sk4d_paragraphbuilder_build(self: sk_paragraphbuilder_t): sk_paragraph_t; cdecl; external LibraryName name 'sk4d_paragraphbuilder_build';
  function  _sk4d_paragraphbuilder_create(const paragraph_style: sk_paragraphstyle_t): sk_paragraphbuilder_t; cdecl; external LibraryName name 'sk4d_paragraphbuilder_create';
  function  _sk4d_paragraphbuilder_create2(const paragraph_style: sk_paragraphstyle_t; font_provider: sk_fontmgr_t; enable_font_fallback: _bool): sk_paragraphbuilder_t; cdecl; external LibraryName name 'sk4d_paragraphbuilder_create2';
  procedure _sk4d_paragraphbuilder_destroy(self: sk_paragraphbuilder_t); cdecl; external LibraryName name 'sk4d_paragraphbuilder_destroy';
  procedure _sk4d_paragraphbuilder_pop(self: sk_paragraphbuilder_t); cdecl; external LibraryName name 'sk4d_paragraphbuilder_pop';
  procedure _sk4d_paragraphbuilder_push_style(self: sk_paragraphbuilder_t; const text_style: sk_textstyle_t); cdecl; external LibraryName name 'sk4d_paragraphbuilder_push_style';


 { modules/skparagraph/include/sk4d_paragraphstyle.h }

  function  _sk4d_paragraphstyle_create(): sk_paragraphstyle_t; cdecl; external LibraryName name 'sk4d_paragraphstyle_create';
  procedure _sk4d_paragraphstyle_destroy(self: sk_paragraphstyle_t); cdecl; external LibraryName name 'sk4d_paragraphstyle_destroy';
  procedure _sk4d_paragraphstyle_disable_hinting(self: sk_paragraphstyle_t); cdecl; external LibraryName name 'sk4d_paragraphstyle_disable_hinting';
  function  _sk4d_paragraphstyle_get_ellipsis(const self: sk_paragraphstyle_t): sk_string_t; cdecl; external LibraryName name 'sk4d_paragraphstyle_get_ellipsis';
  function  _sk4d_paragraphstyle_get_height(const self: sk_paragraphstyle_t): float; cdecl; external LibraryName name 'sk4d_paragraphstyle_get_height';
  function  _sk4d_paragraphstyle_get_max_lines(const self: sk_paragraphstyle_t): size_t; cdecl; external LibraryName name 'sk4d_paragraphstyle_get_max_lines';
  function  _sk4d_paragraphstyle_get_strut_style(const self: sk_paragraphstyle_t): sk_strutstyle_t; cdecl; external LibraryName name 'sk4d_paragraphstyle_get_strut_style';
  function  _sk4d_paragraphstyle_get_text_align(const self: sk_paragraphstyle_t): sk_textalign_t; cdecl; external LibraryName name 'sk4d_paragraphstyle_get_text_align';
  function  _sk4d_paragraphstyle_get_text_direction(const self: sk_paragraphstyle_t): sk_textdirection_t; cdecl; external LibraryName name 'sk4d_paragraphstyle_get_text_direction';
  function  _sk4d_paragraphstyle_get_text_height_behaviors(const self: sk_paragraphstyle_t): uint32_t; cdecl; external LibraryName name 'sk4d_paragraphstyle_get_text_height_behaviors';
  function  _sk4d_paragraphstyle_get_text_style(const self: sk_paragraphstyle_t): sk_textstyle_t; cdecl; external LibraryName name 'sk4d_paragraphstyle_get_text_style';
  procedure _sk4d_paragraphstyle_set_ellipsis(self: sk_paragraphstyle_t; const value: MarshaledAString); cdecl; external LibraryName name 'sk4d_paragraphstyle_set_ellipsis';
  procedure _sk4d_paragraphstyle_set_height(self: sk_paragraphstyle_t; value: float); cdecl; external LibraryName name 'sk4d_paragraphstyle_set_height';
  procedure _sk4d_paragraphstyle_set_max_lines(self: sk_paragraphstyle_t; value: size_t); cdecl; external LibraryName name 'sk4d_paragraphstyle_set_max_lines';
  procedure _sk4d_paragraphstyle_set_strut_style(self: sk_paragraphstyle_t; const value: sk_strutstyle_t); cdecl; external LibraryName name 'sk4d_paragraphstyle_set_strut_style';
  procedure _sk4d_paragraphstyle_set_text_align(self: sk_paragraphstyle_t; value: sk_textalign_t); cdecl; external LibraryName name 'sk4d_paragraphstyle_set_text_align';
  procedure _sk4d_paragraphstyle_set_text_direction(self: sk_paragraphstyle_t; value: sk_textdirection_t); cdecl; external LibraryName name 'sk4d_paragraphstyle_set_text_direction';
  procedure _sk4d_paragraphstyle_set_text_height_behaviors(self: sk_paragraphstyle_t; value: uint32_t); cdecl; external LibraryName name 'sk4d_paragraphstyle_set_text_height_behaviors';
  procedure _sk4d_paragraphstyle_set_text_style(self: sk_paragraphstyle_t; value: sk_textstyle_t); cdecl; external LibraryName name 'sk4d_paragraphstyle_set_text_style';
  function  _sk4d_strutstyle_create(): sk_strutstyle_t; cdecl; external LibraryName name 'sk4d_strutstyle_create';
  procedure _sk4d_strutstyle_destroy(self: sk_strutstyle_t); cdecl; external LibraryName name 'sk4d_strutstyle_destroy';
  function  _sk4d_strutstyle_get_enabled(const self: sk_strutstyle_t): _bool; cdecl; external LibraryName name 'sk4d_strutstyle_get_enabled';
  function  _sk4d_strutstyle_get_font_families(const self: sk_strutstyle_t; const result: PMarshaledAString): size_t; cdecl; external LibraryName name 'sk4d_strutstyle_get_font_families';
  function  _sk4d_strutstyle_get_font_size(const self: sk_strutstyle_t): float; cdecl; external LibraryName name 'sk4d_strutstyle_get_font_size';
  procedure _sk4d_strutstyle_get_font_style(const self: sk_strutstyle_t; out result: sk_fontstyle_t); cdecl; external LibraryName name 'sk4d_strutstyle_get_font_style';
  function  _sk4d_strutstyle_get_force_height(const self: sk_strutstyle_t): _bool; cdecl; external LibraryName name 'sk4d_strutstyle_get_force_height';
  function  _sk4d_strutstyle_get_half_leading(const self: sk_strutstyle_t): _bool; cdecl; external LibraryName name 'sk4d_strutstyle_get_half_leading';
  function  _sk4d_strutstyle_get_height_multiplier(const self: sk_strutstyle_t): float; cdecl; external LibraryName name 'sk4d_strutstyle_get_height_multiplier';
  function  _sk4d_strutstyle_get_leading(const self: sk_strutstyle_t): float; cdecl; external LibraryName name 'sk4d_strutstyle_get_leading';
  function  _sk4d_strutstyle_is_equal(const self: sk_strutstyle_t; const strut_style: sk_strutstyle_t): _bool; cdecl; external LibraryName name 'sk4d_strutstyle_is_equal';
  procedure _sk4d_strutstyle_set_enabled(self: sk_strutstyle_t; value: _bool); cdecl; external LibraryName name 'sk4d_strutstyle_set_enabled';
  procedure _sk4d_strutstyle_set_font_families(self: sk_strutstyle_t; const values: PMarshaledAString; count: size_t); cdecl; external LibraryName name 'sk4d_strutstyle_set_font_families';
  procedure _sk4d_strutstyle_set_font_size(self: sk_strutstyle_t; value: float); cdecl; external LibraryName name 'sk4d_strutstyle_set_font_size';
  procedure _sk4d_strutstyle_set_font_style(self: sk_strutstyle_t; value: psk_fontstyle_t); cdecl; external LibraryName name 'sk4d_strutstyle_set_font_style';
  procedure _sk4d_strutstyle_set_force_height(self: sk_strutstyle_t; value: _bool); cdecl; external LibraryName name 'sk4d_strutstyle_set_force_height';
  procedure _sk4d_strutstyle_set_half_leading(self: sk_strutstyle_t; value: _bool); cdecl; external LibraryName name 'sk4d_strutstyle_set_half_leading';
  procedure _sk4d_strutstyle_set_height_multiplier(self: sk_strutstyle_t; value: float); cdecl; external LibraryName name 'sk4d_strutstyle_set_height_multiplier';
  procedure _sk4d_strutstyle_set_leading(self: sk_strutstyle_t; value: float); cdecl; external LibraryName name 'sk4d_strutstyle_set_leading';


 { modules/skparagraph/include/sk4d_textstyle.h }

  procedure _sk4d_textstyle_add_font_feature(self: sk_textstyle_t; const feature: MarshaledAString; value: int32_t); cdecl; external LibraryName name 'sk4d_textstyle_add_font_feature';
  procedure _sk4d_textstyle_add_shadow(self: sk_textstyle_t; const shadow: psk_textshadow_t); cdecl; external LibraryName name 'sk4d_textstyle_add_shadow';
  procedure _sk4d_textstyle_clear_background_color(self: sk_textstyle_t); cdecl; external LibraryName name 'sk4d_textstyle_clear_background_color';
  procedure _sk4d_textstyle_clear_foreground_color(self: sk_textstyle_t); cdecl; external LibraryName name 'sk4d_textstyle_clear_foreground_color';
  function  _sk4d_textstyle_create(): sk_textstyle_t; cdecl; external LibraryName name 'sk4d_textstyle_create';
  procedure _sk4d_textstyle_destroy(self: sk_textstyle_t); cdecl; external LibraryName name 'sk4d_textstyle_destroy';
  function  _sk4d_textstyle_get_background(const self: sk_textstyle_t): sk_paint_t; cdecl; external LibraryName name 'sk4d_textstyle_get_background';
  function  _sk4d_textstyle_get_color(const self: sk_textstyle_t): sk_color_t; cdecl; external LibraryName name 'sk4d_textstyle_get_color';
  function  _sk4d_textstyle_get_decoration_color(const self: sk_textstyle_t): sk_color_t; cdecl; external LibraryName name 'sk4d_textstyle_get_decoration_color';
  function  _sk4d_textstyle_get_decoration_style(const self: sk_textstyle_t): sk_textdecorationstyle_t; cdecl; external LibraryName name 'sk4d_textstyle_get_decoration_style';
  function  _sk4d_textstyle_get_decoration_thickness(const self: sk_textstyle_t): float; cdecl; external LibraryName name 'sk4d_textstyle_get_decoration_thickness';
  function  _sk4d_textstyle_get_decorations(const self: sk_textstyle_t): uint32_t; cdecl; external LibraryName name 'sk4d_textstyle_get_decorations';
  function  _sk4d_textstyle_get_font_families(const self: sk_textstyle_t; const result: PMarshaledAString): size_t; cdecl; external LibraryName name 'sk4d_textstyle_get_font_families';
  procedure _sk4d_textstyle_get_font_metrics(const self: sk_textstyle_t; out result: sk_fontmetrics_t); cdecl; external LibraryName name 'sk4d_textstyle_get_font_metrics';
  function  _sk4d_textstyle_get_font_size(const self: sk_textstyle_t): float; cdecl; external LibraryName name 'sk4d_textstyle_get_font_size';
  procedure _sk4d_textstyle_get_font_style(const self: sk_textstyle_t; out result: sk_fontstyle_t); cdecl; external LibraryName name 'sk4d_textstyle_get_font_style';
  function  _sk4d_textstyle_get_foreground(const self: sk_textstyle_t): sk_paint_t; cdecl; external LibraryName name 'sk4d_textstyle_get_foreground';
  function  _sk4d_textstyle_get_half_leading(const self: sk_textstyle_t): _bool; cdecl; external LibraryName name 'sk4d_textstyle_get_half_leading';
  function  _sk4d_textstyle_get_height_multiplier(const self: sk_textstyle_t): float; cdecl; external LibraryName name 'sk4d_textstyle_get_height_multiplier';
  function  _sk4d_textstyle_get_letter_spacing(const self: sk_textstyle_t): float; cdecl; external LibraryName name 'sk4d_textstyle_get_letter_spacing';
  function  _sk4d_textstyle_get_locale(const self: sk_textstyle_t): sk_string_t; cdecl; external LibraryName name 'sk4d_textstyle_get_locale';
  function  _sk4d_textstyle_get_word_spacing(const self: sk_textstyle_t): float; cdecl; external LibraryName name 'sk4d_textstyle_get_word_spacing';
  function  _sk4d_textstyle_is_equal(const self, text_style: sk_textstyle_t): _bool; cdecl; external LibraryName name 'sk4d_textstyle_is_equal';
  procedure _sk4d_textstyle_reset_font_features(self: sk_textstyle_t); cdecl; external LibraryName name 'sk4d_textstyle_reset_font_features';
  procedure _sk4d_textstyle_reset_shadows(self: sk_textstyle_t); cdecl; external LibraryName name 'sk4d_textstyle_reset_shadows';
  procedure _sk4d_textstyle_set_background_color(self: sk_textstyle_t; paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_textstyle_set_background_color';
  procedure _sk4d_textstyle_set_color(self: sk_textstyle_t; value: sk_color_t); cdecl; external LibraryName name 'sk4d_textstyle_set_color';
  procedure _sk4d_textstyle_set_decoration_color(self: sk_textstyle_t; value: sk_color_t); cdecl; external LibraryName name 'sk4d_textstyle_set_decoration_color';
  procedure _sk4d_textstyle_set_decoration_style(self: sk_textstyle_t; value: sk_textdecorationstyle_t); cdecl; external LibraryName name 'sk4d_textstyle_set_decoration_style';
  procedure _sk4d_textstyle_set_decoration_thickness(self: sk_textstyle_t; value: float); cdecl; external LibraryName name 'sk4d_textstyle_set_decoration_thickness';
  procedure _sk4d_textstyle_set_decorations(self: sk_textstyle_t; value: uint32_t); cdecl; external LibraryName name 'sk4d_textstyle_set_decorations';
  procedure _sk4d_textstyle_set_font_families(self: sk_textstyle_t; const values: PMarshaledAString; count: size_t); cdecl; external LibraryName name 'sk4d_textstyle_set_font_families';
  procedure _sk4d_textstyle_set_font_size(self: sk_textstyle_t; value: float); cdecl; external LibraryName name 'sk4d_textstyle_set_font_size';
  procedure _sk4d_textstyle_set_font_style(self: sk_textstyle_t; const value: psk_fontstyle_t); cdecl; external LibraryName name 'sk4d_textstyle_set_font_style';
  procedure _sk4d_textstyle_set_foreground_color(self: sk_textstyle_t; paint: sk_paint_t); cdecl; external LibraryName name 'sk4d_textstyle_set_foreground_color';
  procedure _sk4d_textstyle_set_half_leading(self: sk_textstyle_t; value: _bool); cdecl; external LibraryName name 'sk4d_textstyle_set_half_leading';
  procedure _sk4d_textstyle_set_height_multiplier(self: sk_textstyle_t; value: float); cdecl; external LibraryName name 'sk4d_textstyle_set_height_multiplier';
  procedure _sk4d_textstyle_set_letter_spacing(self: sk_textstyle_t; value: float); cdecl; external LibraryName name 'sk4d_textstyle_set_letter_spacing';
  procedure _sk4d_textstyle_set_locale(self: sk_textstyle_t; const value: MarshaledAString); cdecl; external LibraryName name 'sk4d_textstyle_set_locale';
  procedure _sk4d_textstyle_set_word_spacing(self: sk_textstyle_t; value: float); cdecl; external LibraryName name 'sk4d_textstyle_set_word_spacing';


 { modules/skparagraph/include/sk4d_typefacefontprovider.h }

  function  _sk4d_typefacefontprovider_create(): sk_typefacefontprovider_t; cdecl; external LibraryName name 'sk4d_typefacefontprovider_create';
  procedure _sk4d_typefacefontprovider_register_typeface(self: sk_typefacefontprovider_t; typeface: sk_typeface_t); cdecl; external LibraryName name 'sk4d_typefacefontprovider_register_typeface';
  procedure _sk4d_typefacefontprovider_register_typeface2(self: sk_typefacefontprovider_t; typeface: sk_typeface_t; const family_name: MarshaledAString); cdecl; external LibraryName name 'sk4d_typefacefontprovider_register_typeface2';


 { modules/skresources/include/sk4d_resources.h }

  function  _sk4d_resourceproviderbaseclass_create(predecode: _bool; context: Pointer): sk_resourceproviderbaseclass_t; cdecl; external LibraryName name 'sk4d_resourceproviderbaseclass_create';
  procedure _sk4d_resourceproviderbaseclass_set_procs(const procs: psk_resourceproviderbaseclass_procs_t); cdecl; external LibraryName name 'sk4d_resourceproviderbaseclass_set_procs';


 { modules/skshaper/include/sk4d_shaper.h }

  function  _sk4d_shaper_create(): sk_shaper_t; cdecl; external LibraryName name 'sk4d_shaper_create';
  procedure _sk4d_shaper_destroy(self: sk_shaper_t); cdecl; external LibraryName name 'sk4d_shaper_destroy';
  function  _sk4d_shaper_shape(const self: sk_shaper_t; const text: MarshaledAString; const font: sk_font_t; left_to_right: _bool; width: float; const offset: psk_point_t; end_point: psk_point_t): sk_textblob_t; cdecl; external LibraryName name 'sk4d_shaper_shape';


 { modules/skunicode/include/sk4d_unicode.h }

  function  _sk4d_unicode_create(): sk_unicode_t; cdecl; external LibraryName name 'sk4d_unicode_create';
  procedure _sk4d_unicode_destroy(self: sk_unicode_t); cdecl; external LibraryName name 'sk4d_unicode_destroy';
  procedure _sk4d_unicode_for_each_bidi_region(self: sk_unicode_t; const utf16_text: puint16_t; utf16_units: int32_t; direction: sk_direction_t; proc: sk_unicode_bidi_region_proc; context: Pointer); cdecl; external LibraryName name 'sk4d_unicode_for_each_bidi_region';
  procedure _sk4d_unicode_for_each_break(self: sk_unicode_t; const utf16_text: pchar16_t; utf16_units: int32_t; &type: sk_breaktype_t; proc: sk_unicode_break_proc; context: Pointer); cdecl; external LibraryName name 'sk4d_unicode_for_each_break';
  procedure _sk4d_unicode_for_each_codepoint(self: sk_unicode_t; const utf16_text: pchar16_t; utf16_units: int32_t; proc: sk_unicode_codepoint_proc; context: Pointer); cdecl; external LibraryName name 'sk4d_unicode_for_each_codepoint';
  function  _sk4d_unicodebreakiterator_create(unicode: sk_unicode_t; &type: sk_breaktype_t; const text: _pchar; units: int32_t): sk_unicodebreakiterator_t; cdecl; external LibraryName name 'sk4d_unicodebreakiterator_create';
  function  _sk4d_unicodebreakiterator_create2(unicode: sk_unicode_t; &type: sk_breaktype_t; const utf16_text: pchar16_t; utf16_units: int32_t): sk_unicodebreakiterator_t; cdecl; external LibraryName name 'sk4d_unicodebreakiterator_create2';
  procedure _sk4d_unicodebreakiterator_destroy(self: sk_unicodebreakiterator_t); cdecl; external LibraryName name 'sk4d_unicodebreakiterator_destroy';
  function  _sk4d_unicodebreakiterator_next(self: sk_unicodebreakiterator_t; out elem: sk_unicodebreakiteratorelem_t): _bool; cdecl; external LibraryName name 'sk4d_unicodebreakiterator_next';

 { modules/svg/include/sk4d_svgdom.h }

  function  _sk4d_svgdom_find_node_by_id(self: sk_svgdom_t; const id: MarshaledAString): sk_svgnode_t; cdecl; external LibraryName name 'sk4d_svgdom_find_node_by_id';
  function  _sk4d_svgdom_get_root(const self: sk_svgdom_t): sk_svgsvg_t; cdecl; external LibraryName name 'sk4d_svgdom_get_root';
  function  _sk4d_svgdom_make_from_file(const file_name: MarshaledAString; font_provider: sk_fontmgr_t): sk_svgdom_t; cdecl; external LibraryName name 'sk4d_svgdom_make_from_file';
  function  _sk4d_svgdom_make_from_stream(stream: sk_stream_t; resource_provider: sk_resourceprovider_t; font_provider: sk_fontmgr_t): sk_svgdom_t; cdecl; external LibraryName name 'sk4d_svgdom_make_from_stream';
  procedure _sk4d_svgdom_render(const self: sk_svgdom_t; canvas: sk_canvas_t); cdecl; external LibraryName name 'sk4d_svgdom_render';
  procedure _sk4d_svgdom_set_container_size(self: sk_svgdom_t; const size: psk_size_t); cdecl; external LibraryName name 'sk4d_svgdom_set_container_size';


 { modules/svg/include/sk4d_svgnode.h }

  function  _sk4d_svgnode_set_attribute(self: sk_svgnode_t; const name, value: MarshaledAString): _bool; cdecl; external LibraryName name 'sk4d_svgnode_set_attribute';


 { modules/svg/include/sk4d_svgsvg.h }

  procedure _sk4d_svgsvg_get_height(const self: sk_svgsvg_t; out result: sk_svglength_t); cdecl; external LibraryName name 'sk4d_svgsvg_get_height';
  procedure _sk4d_svgsvg_get_intrinsic_size(const self: sk_svgsvg_t; const view_port: psk_size_t; dpi: float; out result: sk_size_t); cdecl; external LibraryName name 'sk4d_svgsvg_get_intrinsic_size';
  procedure _sk4d_svgsvg_get_preserve_aspect_ratio(const self: sk_svgsvg_t; out result: sk_svgpreserveaspectratio_t); cdecl; external LibraryName name 'sk4d_svgsvg_get_preserve_aspect_ratio';
  function  _sk4d_svgsvg_get_view_box(const self: sk_svgsvg_t; out result: sk_rect_t): _bool; cdecl; external LibraryName name 'sk4d_svgsvg_get_view_box';
  procedure _sk4d_svgsvg_get_width(const self: sk_svgsvg_t; out result: sk_svglength_t); cdecl; external LibraryName name 'sk4d_svgsvg_get_width';
  procedure _sk4d_svgsvg_get_x(const self: sk_svgsvg_t; out result: sk_svglength_t); cdecl; external LibraryName name 'sk4d_svgsvg_get_x';
  procedure _sk4d_svgsvg_get_y(const self: sk_svgsvg_t; out result: sk_svglength_t); cdecl; external LibraryName name 'sk4d_svgsvg_get_y';
  procedure _sk4d_svgsvg_set_height(self: sk_svgsvg_t; value: psk_svglength_t); cdecl; external LibraryName name 'sk4d_svgsvg_set_height';
  procedure _sk4d_svgsvg_set_preserve_aspect_ratio(self: sk_svgsvg_t; value: psk_svgpreserveaspectratio_t); cdecl; external LibraryName name 'sk4d_svgsvg_set_preserve_aspect_ratio';
  procedure _sk4d_svgsvg_set_view_box(self: sk_svgsvg_t; view_box: psk_rect_t); cdecl; external LibraryName name 'sk4d_svgsvg_set_view_box';
  procedure _sk4d_svgsvg_set_width(self: sk_svgsvg_t; value: psk_svglength_t); cdecl; external LibraryName name 'sk4d_svgsvg_set_width';
  procedure _sk4d_svgsvg_set_x(self: sk_svgsvg_t; value: psk_svglength_t); cdecl; external LibraryName name 'sk4d_svgsvg_set_x';
  procedure _sk4d_svgsvg_set_y(self: sk_svgsvg_t; value: psk_svglength_t); cdecl; external LibraryName name 'sk4d_svgsvg_set_y';

var
  {$IFNDEF FPC}[Volatile]{$ENDIF} InitCount: Integer;

procedure SkInitialize;
begin
  if AtomicIncrement(InitCount) <> 1 then
    Exit;

 { include/c/gr4d_backendsemaphore.h }

  gr4d_backendsemaphore_create := Addr(_gr4d_backendsemaphore_create);
  gr4d_backendsemaphore_destroy := Addr(_gr4d_backendsemaphore_destroy);
  gr4d_backendsemaphore_init_vulkan := Addr(_gr4d_backendsemaphore_init_vulkan);


 { include/c/gr4d_backendsurface.h }

  gr4d_backendrendertarget_create_gl := Addr(_gr4d_backendrendertarget_create_gl);
  gr4d_backendrendertarget_create_mtl := Addr(_gr4d_backendrendertarget_create_mtl);
  gr4d_backendrendertarget_create_vk := Addr(_gr4d_backendrendertarget_create_vk);
  gr4d_backendrendertarget_destroy := Addr(_gr4d_backendrendertarget_destroy);
  gr4d_backendrendertarget_get_backend_api := Addr(_gr4d_backendrendertarget_get_backend_api);
  gr4d_backendrendertarget_get_height := Addr(_gr4d_backendrendertarget_get_height);
  gr4d_backendrendertarget_get_sample_count := Addr(_gr4d_backendrendertarget_get_sample_count);
  gr4d_backendrendertarget_get_stencil_bits := Addr(_gr4d_backendrendertarget_get_stencil_bits);
  gr4d_backendrendertarget_get_width := Addr(_gr4d_backendrendertarget_get_width);
  gr4d_backendrendertarget_is_valid := Addr(_gr4d_backendrendertarget_is_valid);
  gr4d_backendtexture_create_gl := Addr(_gr4d_backendtexture_create_gl);
  gr4d_backendtexture_create_mtl := Addr(_gr4d_backendtexture_create_mtl);
  gr4d_backendtexture_create_vk := Addr(_gr4d_backendtexture_create_vk);
  gr4d_backendtexture_destroy := Addr(_gr4d_backendtexture_destroy);
  gr4d_backendtexture_get_backend_api := Addr(_gr4d_backendtexture_get_backend_api);
  gr4d_backendtexture_get_gl_framebuffer_info := Addr(_gr4d_backendtexture_get_gl_framebuffer_info);
  gr4d_backendtexture_get_height := Addr(_gr4d_backendtexture_get_height);
  gr4d_backendtexture_get_width := Addr(_gr4d_backendtexture_get_width);
  gr4d_backendtexture_has_mipmaps := Addr(_gr4d_backendtexture_has_mipmaps);
  gr4d_backendtexture_is_valid := Addr(_gr4d_backendtexture_is_valid);


 { include/c/gr4d_backendsurfacemutablestate.h }

  gr4d_backendsurfacemutablestate_create := Addr(_gr4d_backendsurfacemutablestate_create);
  gr4d_backendsurfacemutablestate_destroy := Addr(_gr4d_backendsurfacemutablestate_destroy);


 { include/c/gr4d_contextoptions.h }

  gr4d_persistentcachebaseclass_create := Addr(_gr4d_persistentcachebaseclass_create);
  gr4d_persistentcachebaseclass_destroy := Addr(_gr4d_persistentcachebaseclass_destroy);
  gr4d_persistentcachebaseclass_set_procs := Addr(_gr4d_persistentcachebaseclass_set_procs);


 { include/c/gr4d_directcontext.h }

  gr4d_directcontext_abandon_context := Addr(_gr4d_directcontext_abandon_context);
  gr4d_directcontext_create_texture := Addr(_gr4d_directcontext_create_texture);
  gr4d_directcontext_create_texture2 := Addr(_gr4d_directcontext_create_texture2);
  gr4d_directcontext_create_texture3 := Addr(_gr4d_directcontext_create_texture3);
  gr4d_directcontext_delete_texture := Addr(_gr4d_directcontext_delete_texture);
  gr4d_directcontext_dump_memory_statistics := Addr(_gr4d_directcontext_dump_memory_statistics);
  gr4d_directcontext_flush := Addr(_gr4d_directcontext_flush);
  gr4d_directcontext_flush_and_submit := Addr(_gr4d_directcontext_flush_and_submit);
  gr4d_directcontext_free_gpu_resources := Addr(_gr4d_directcontext_free_gpu_resources);
  gr4d_directcontext_get_backend_api := Addr(_gr4d_directcontext_get_backend_api);
  gr4d_directcontext_get_max_surface_sample_count_for_color_type := Addr(_gr4d_directcontext_get_max_surface_sample_count_for_color_type);
  gr4d_directcontext_get_resource_cache_limit := Addr(_gr4d_directcontext_get_resource_cache_limit);
  gr4d_directcontext_get_resource_cache_usage := Addr(_gr4d_directcontext_get_resource_cache_usage);
  gr4d_directcontext_is_abandoned := Addr(_gr4d_directcontext_is_abandoned);
  gr4d_directcontext_make_gl := Addr(_gr4d_directcontext_make_gl);
  gr4d_directcontext_make_metal := Addr(_gr4d_directcontext_make_metal);
  gr4d_directcontext_make_vulkan := Addr(_gr4d_directcontext_make_vulkan);
  gr4d_directcontext_perform_deferred_cleanup := Addr(_gr4d_directcontext_perform_deferred_cleanup);
  gr4d_directcontext_purge_unlocked_resources := Addr(_gr4d_directcontext_purge_unlocked_resources);
  gr4d_directcontext_purge_unlocked_resources2 := Addr(_gr4d_directcontext_purge_unlocked_resources2);
  gr4d_directcontext_release_resources_and_abandon_context := Addr(_gr4d_directcontext_release_resources_and_abandon_context);
  gr4d_directcontext_reset_context := Addr(_gr4d_directcontext_reset_context);
  gr4d_directcontext_set_resource_cache_limit := Addr(_gr4d_directcontext_set_resource_cache_limit);
  gr4d_directcontext_submit := Addr(_gr4d_directcontext_submit);


 { include/c/gr4d_gl_interface.h }

  gr4d_gl_interface_has_extension := Addr(_gr4d_gl_interface_has_extension);
  gr4d_gl_interface_make_assembled := Addr(_gr4d_gl_interface_make_assembled);
  gr4d_gl_interface_make_assembled_gl := Addr(_gr4d_gl_interface_make_assembled_gl);
  gr4d_gl_interface_make_assembled_gles := Addr(_gr4d_gl_interface_make_assembled_gles);
  gr4d_gl_interface_make_assembled_webgl := Addr(_gr4d_gl_interface_make_assembled_webgl);
  gr4d_gl_interface_make_native := Addr(_gr4d_gl_interface_make_native);
  gr4d_gl_interface_validate := Addr(_gr4d_gl_interface_validate);


 { include/c/gr4d_shadererrorhandler.h }

  gr4d_shadererrorhandlerbaseclass_create := Addr(_gr4d_shadererrorhandlerbaseclass_create);
  gr4d_shadererrorhandlerbaseclass_destroy := Addr(_gr4d_shadererrorhandlerbaseclass_destroy);
  gr4d_shadererrorhandlerbaseclass_set_procs := Addr(_gr4d_shadererrorhandlerbaseclass_set_procs);


 { include/c/gr4d_vk_extensions.h }

  gr4d_vk_extensions_create := Addr(_gr4d_vk_extensions_create);
  gr4d_vk_extensions_destroy := Addr(_gr4d_vk_extensions_destroy);
  gr4d_vk_extensions_has_extension := Addr(_gr4d_vk_extensions_has_extension);
  gr4d_vk_extensions_init := Addr(_gr4d_vk_extensions_init);


 { include/c/sk4d_animatedwebpencoder.h }

  sk4d_animatedwebpencoder_encode_to_file := Addr(_sk4d_animatedwebpencoder_encode_to_file);
  sk4d_animatedwebpencoder_encode_to_stream := Addr(_sk4d_animatedwebpencoder_encode_to_stream);


 { include/c/sk4d_blender.h }

  sk4d_blender_make_arithmetic := Addr(_sk4d_blender_make_arithmetic);
  sk4d_blender_make_mode := Addr(_sk4d_blender_make_mode);


 { include/c/sk4d_canvas.h }

  sk4d_canvas_clear := Addr(_sk4d_canvas_clear);
  sk4d_canvas_clear2 := Addr(_sk4d_canvas_clear2);
  sk4d_canvas_destroy := Addr(_sk4d_canvas_destroy);
  sk4d_canvas_discard := Addr(_sk4d_canvas_discard);
  sk4d_canvas_clip_path := Addr(_sk4d_canvas_clip_path);
  sk4d_canvas_clip_rect := Addr(_sk4d_canvas_clip_rect);
  sk4d_canvas_clip_region := Addr(_sk4d_canvas_clip_region);
  sk4d_canvas_clip_rrect := Addr(_sk4d_canvas_clip_rrect);
  sk4d_canvas_clip_shader := Addr(_sk4d_canvas_clip_shader);
  sk4d_canvas_concat := Addr(_sk4d_canvas_concat);
  sk4d_canvas_concat2 := Addr(_sk4d_canvas_concat2);
  sk4d_canvas_draw_annotation := Addr(_sk4d_canvas_draw_annotation);
  sk4d_canvas_draw_arc := Addr(_sk4d_canvas_draw_arc);
  sk4d_canvas_draw_atlas := Addr(_sk4d_canvas_draw_atlas);
  sk4d_canvas_draw_circle := Addr(_sk4d_canvas_draw_circle);
  sk4d_canvas_draw_color := Addr(_sk4d_canvas_draw_color);
  sk4d_canvas_draw_color2 := Addr(_sk4d_canvas_draw_color2);
  sk4d_canvas_draw_glyphs := Addr(_sk4d_canvas_draw_glyphs);
  sk4d_canvas_draw_glyphs2 := Addr(_sk4d_canvas_draw_glyphs2);
  sk4d_canvas_draw_image := Addr(_sk4d_canvas_draw_image);
  sk4d_canvas_draw_image_lattice := Addr(_sk4d_canvas_draw_image_lattice);
  sk4d_canvas_draw_image_nine := Addr(_sk4d_canvas_draw_image_nine);
  sk4d_canvas_draw_image_rect := Addr(_sk4d_canvas_draw_image_rect);
  sk4d_canvas_draw_line := Addr(_sk4d_canvas_draw_line);
  sk4d_canvas_draw_oval := Addr(_sk4d_canvas_draw_oval);
  sk4d_canvas_draw_paint := Addr(_sk4d_canvas_draw_paint);
  sk4d_canvas_draw_patch := Addr(_sk4d_canvas_draw_patch);
  sk4d_canvas_draw_path := Addr(_sk4d_canvas_draw_path);
  sk4d_canvas_draw_picture := Addr(_sk4d_canvas_draw_picture);
  sk4d_canvas_draw_point := Addr(_sk4d_canvas_draw_point);
  sk4d_canvas_draw_points := Addr(_sk4d_canvas_draw_points);
  sk4d_canvas_draw_rect := Addr(_sk4d_canvas_draw_rect);
  sk4d_canvas_draw_region := Addr(_sk4d_canvas_draw_region);
  sk4d_canvas_draw_rrect := Addr(_sk4d_canvas_draw_rrect);
  sk4d_canvas_draw_rrect2 := Addr(_sk4d_canvas_draw_rrect2);
  sk4d_canvas_draw_rrect_difference := Addr(_sk4d_canvas_draw_rrect_difference);
  sk4d_canvas_draw_simple_text := Addr(_sk4d_canvas_draw_simple_text);
  sk4d_canvas_draw_text_blob := Addr(_sk4d_canvas_draw_text_blob);
  sk4d_canvas_draw_vertices := Addr(_sk4d_canvas_draw_vertices);
  sk4d_canvas_get_base_props := Addr(_sk4d_canvas_get_base_props);
  sk4d_canvas_get_device_clip_bounds := Addr(_sk4d_canvas_get_device_clip_bounds);
  sk4d_canvas_get_local_clip_bounds := Addr(_sk4d_canvas_get_local_clip_bounds);
  sk4d_canvas_get_local_to_device := Addr(_sk4d_canvas_get_local_to_device);
  sk4d_canvas_get_local_to_device_as_3x3 := Addr(_sk4d_canvas_get_local_to_device_as_3x3);
  sk4d_canvas_get_top_props := Addr(_sk4d_canvas_get_top_props);
  sk4d_canvas_get_save_count := Addr(_sk4d_canvas_get_save_count);
  sk4d_canvas_make_surface := Addr(_sk4d_canvas_make_surface);
  sk4d_canvas_quick_reject := Addr(_sk4d_canvas_quick_reject);
  sk4d_canvas_quick_reject2 := Addr(_sk4d_canvas_quick_reject2);
  sk4d_canvas_reset_matrix := Addr(_sk4d_canvas_reset_matrix);
  sk4d_canvas_restore := Addr(_sk4d_canvas_restore);
  sk4d_canvas_restore_to_count := Addr(_sk4d_canvas_restore_to_count);
  sk4d_canvas_rotate := Addr(_sk4d_canvas_rotate);
  sk4d_canvas_rotate2 := Addr(_sk4d_canvas_rotate2);
  sk4d_canvas_save := Addr(_sk4d_canvas_save);
  sk4d_canvas_save_layer := Addr(_sk4d_canvas_save_layer);
  sk4d_canvas_save_layer_alpha := Addr(_sk4d_canvas_save_layer_alpha);
  sk4d_canvas_scale := Addr(_sk4d_canvas_scale);
  sk4d_canvas_set_matrix := Addr(_sk4d_canvas_set_matrix);
  sk4d_canvas_set_matrix2 := Addr(_sk4d_canvas_set_matrix2);
  sk4d_canvas_skew := Addr(_sk4d_canvas_skew);
  sk4d_canvas_translate := Addr(_sk4d_canvas_translate);


 { include/c/sk4d_codec.h }

  sk4d_codec_destroy := Addr(_sk4d_codec_destroy);
  sk4d_codec_get_dimensions := Addr(_sk4d_codec_get_dimensions);
  sk4d_codec_get_encoded_image_format := Addr(_sk4d_codec_get_encoded_image_format);
  sk4d_codec_get_image := Addr(_sk4d_codec_get_image);
  sk4d_codec_get_pixels := Addr(_sk4d_codec_get_pixels);
  sk4d_codec_make_from_file := Addr(_sk4d_codec_make_from_file);
  sk4d_codec_make_from_stream := Addr(_sk4d_codec_make_from_stream);
  sk4d_codec_make_with_copy := Addr(_sk4d_codec_make_with_copy);
  sk4d_codec_make_without_copy := Addr(_sk4d_codec_make_without_copy);
  sk4d_animcodecplayer_destroy := Addr(_sk4d_animcodecplayer_destroy);
  sk4d_animcodecplayer_get_dimensions := Addr(_sk4d_animcodecplayer_get_dimensions);
  sk4d_animcodecplayer_get_duration := Addr(_sk4d_animcodecplayer_get_duration);
  sk4d_animcodecplayer_get_frame := Addr(_sk4d_animcodecplayer_get_frame);
  sk4d_animcodecplayer_make_from_file := Addr(_sk4d_animcodecplayer_make_from_file);
  sk4d_animcodecplayer_make_from_stream := Addr(_sk4d_animcodecplayer_make_from_stream);
  sk4d_animcodecplayer_seek := Addr(_sk4d_animcodecplayer_seek);


 { include/c/sk4d_colorfilter.h }

  sk4d_colorfilter_make_blend := Addr(_sk4d_colorfilter_make_blend);
  sk4d_colorfilter_make_blend2 := Addr(_sk4d_colorfilter_make_blend2);
  sk4d_colorfilter_make_compose := Addr(_sk4d_colorfilter_make_compose);
  sk4d_colorfilter_make_high_contrast := Addr(_sk4d_colorfilter_make_high_contrast);
  sk4d_colorfilter_make_hsla_matrix := Addr(_sk4d_colorfilter_make_hsla_matrix);
  sk4d_colorfilter_make_lighting := Addr(_sk4d_colorfilter_make_lighting);
  sk4d_colorfilter_make_linear_to_srgb_gamma := Addr(_sk4d_colorfilter_make_linear_to_srgb_gamma);
  sk4d_colorfilter_make_luma_color := Addr(_sk4d_colorfilter_make_luma_color);
  sk4d_colorfilter_make_matrix := Addr(_sk4d_colorfilter_make_matrix);
  sk4d_colorfilter_make_overdraw := Addr(_sk4d_colorfilter_make_overdraw);
  sk4d_colorfilter_make_table := Addr(_sk4d_colorfilter_make_table);


 { include/c/sk4d_colorspace.h }

  sk4d_colorspace_gamma_close_to_srgb := Addr(_sk4d_colorspace_gamma_close_to_srgb);
  sk4d_colorspace_gamma_is_linear := Addr(_sk4d_colorspace_gamma_is_linear);
  sk4d_colorspace_is_equal := Addr(_sk4d_colorspace_is_equal);
  sk4d_colorspace_is_numerical_transfer_fn := Addr(_sk4d_colorspace_is_numerical_transfer_fn);
  sk4d_colorspace_is_srgb := Addr(_sk4d_colorspace_is_srgb);
  sk4d_colorspace_make := Addr(_sk4d_colorspace_make);
  sk4d_colorspace_make_linear_gamma := Addr(_sk4d_colorspace_make_linear_gamma);
  sk4d_colorspace_make_rgb := Addr(_sk4d_colorspace_make_rgb);
  sk4d_colorspace_make_srgb := Addr(_sk4d_colorspace_make_srgb);
  sk4d_colorspace_make_srgb_gamma := Addr(_sk4d_colorspace_make_srgb_gamma);
  sk4d_colorspace_make_srgb_linear := Addr(_sk4d_colorspace_make_srgb_linear);
  sk4d_colorspace_ref := Addr(_sk4d_colorspace_ref);
  sk4d_colorspace_to_profile := Addr(_sk4d_colorspace_to_profile);
  sk4d_colorspace_to_xyz := Addr(_sk4d_colorspace_to_xyz);
  sk4d_colorspace_unref := Addr(_sk4d_colorspace_unref);
  sk4d_colorspaceiccprofile_destroy := Addr(_sk4d_colorspaceiccprofile_destroy);
  sk4d_colorspaceiccprofile_get_buffer := Addr(_sk4d_colorspaceiccprofile_get_buffer);
  sk4d_colorspaceiccprofile_make_with_parse := Addr(_sk4d_colorspaceiccprofile_make_with_parse);
  sk4d_colorspaceiccprofile_to_xyz := Addr(_sk4d_colorspaceiccprofile_to_xyz);
  sk4d_colorspaceprimaries_to_xyz := Addr(_sk4d_colorspaceprimaries_to_xyz);
  sk4d_colorspacetransferfn_invert := Addr(_sk4d_colorspacetransferfn_invert);
  sk4d_colorspacetransferfn_transform := Addr(_sk4d_colorspacetransferfn_transform);


 { include/c/sk4d_data.h }

  sk4d_data_make_empty := Addr(_sk4d_data_make_empty);
  sk4d_data_make_with_copy := Addr(_sk4d_data_make_with_copy);
  sk4d_data_ref := Addr(_sk4d_data_ref);
  sk4d_data_unref := Addr(_sk4d_data_unref);


 { include/c/sk4d_document.h }

  sk4d_document_begin_page := Addr(_sk4d_document_begin_page);
  sk4d_document_close := Addr(_sk4d_document_close);
  sk4d_document_end_page := Addr(_sk4d_document_end_page);
  sk4d_document_make_pdf := Addr(_sk4d_document_make_pdf);
  sk4d_document_make_pdf2 := Addr(_sk4d_document_make_pdf2);
  sk4d_document_make_xps := Addr(_sk4d_document_make_xps);
  sk4d_document_terminate := Addr(_sk4d_document_terminate);


 { include/c/sk4d_font.h }

  sk4d_font_create := Addr(_sk4d_font_create);
  sk4d_font_create2 := Addr(_sk4d_font_create2);
  sk4d_font_destroy := Addr(_sk4d_font_destroy);
  sk4d_font_get_baseline_snap := Addr(_sk4d_font_get_baseline_snap);
  sk4d_font_get_edging := Addr(_sk4d_font_get_edging);
  sk4d_font_get_embedded_bitmaps := Addr(_sk4d_font_get_embedded_bitmaps);
  sk4d_font_get_embolden := Addr(_sk4d_font_get_embolden);
  sk4d_font_get_force_auto_hinting := Addr(_sk4d_font_get_force_auto_hinting);
  sk4d_font_get_glyphs := Addr(_sk4d_font_get_glyphs);
  sk4d_font_get_glyphs_count := Addr(_sk4d_font_get_glyphs_count);
  sk4d_font_get_hinting := Addr(_sk4d_font_get_hinting);
  sk4d_font_get_horizontal_positions := Addr(_sk4d_font_get_horizontal_positions);
  sk4d_font_get_intercepts := Addr(_sk4d_font_get_intercepts);
  sk4d_font_get_linear_metrics := Addr(_sk4d_font_get_linear_metrics);
  sk4d_font_get_metrics := Addr(_sk4d_font_get_metrics);
  sk4d_font_get_path := Addr(_sk4d_font_get_path);
  sk4d_font_get_paths := Addr(_sk4d_font_get_paths);
  sk4d_font_get_positions := Addr(_sk4d_font_get_positions);
  sk4d_font_get_scale_x := Addr(_sk4d_font_get_scale_x);
  sk4d_font_get_size := Addr(_sk4d_font_get_size);
  sk4d_font_get_skew_x := Addr(_sk4d_font_get_skew_x);
  sk4d_font_get_subpixel := Addr(_sk4d_font_get_subpixel);
  sk4d_font_get_typeface := Addr(_sk4d_font_get_typeface);
  sk4d_font_get_typeface_or_default := Addr(_sk4d_font_get_typeface_or_default);
  sk4d_font_get_widths_bounds := Addr(_sk4d_font_get_widths_bounds);
  sk4d_font_is_equal := Addr(_sk4d_font_is_equal);
  sk4d_font_measure_text := Addr(_sk4d_font_measure_text);
  sk4d_font_set_baseline_snap := Addr(_sk4d_font_set_baseline_snap);
  sk4d_font_set_edging := Addr(_sk4d_font_set_edging);
  sk4d_font_set_embedded_bitmaps := Addr(_sk4d_font_set_embedded_bitmaps);
  sk4d_font_set_embolden := Addr(_sk4d_font_set_embolden);
  sk4d_font_set_force_auto_hinting := Addr(_sk4d_font_set_force_auto_hinting);
  sk4d_font_set_hinting := Addr(_sk4d_font_set_hinting);
  sk4d_font_set_linear_metrics := Addr(_sk4d_font_set_linear_metrics);
  sk4d_font_set_scale_x := Addr(_sk4d_font_set_scale_x);
  sk4d_font_set_size := Addr(_sk4d_font_set_size);
  sk4d_font_set_skew_x := Addr(_sk4d_font_set_skew_x);
  sk4d_font_set_subpixel := Addr(_sk4d_font_set_subpixel);
  sk4d_font_set_typeface := Addr(_sk4d_font_set_typeface);
  sk4d_font_unichar_to_glyph := Addr(_sk4d_font_unichar_to_glyph);
  sk4d_font_unichars_to_glyphs := Addr(_sk4d_font_unichars_to_glyphs);


 { include/c/sk4d_graphics.h }

  sk4d_graphics_allow_jit := Addr(_sk4d_graphics_allow_jit);
  sk4d_graphics_dump_memory_statistics := Addr(_sk4d_graphics_dump_memory_statistics);
  sk4d_graphics_get_font_cache_count_limit := Addr(_sk4d_graphics_get_font_cache_count_limit);
  sk4d_graphics_get_font_cache_count_used := Addr(_sk4d_graphics_get_font_cache_count_used);
  sk4d_graphics_get_font_cache_limit := Addr(_sk4d_graphics_get_font_cache_limit);
  sk4d_graphics_get_font_cache_used := Addr(_sk4d_graphics_get_font_cache_used);
  sk4d_graphics_get_resource_cache_single_allocation_byte_limit := Addr(_sk4d_graphics_get_resource_cache_single_allocation_byte_limit);
  sk4d_graphics_get_resource_cache_total_byte_limit := Addr(_sk4d_graphics_get_resource_cache_total_byte_limit);
  sk4d_graphics_get_resource_cache_total_bytes_used := Addr(_sk4d_graphics_get_resource_cache_total_bytes_used);
  sk4d_graphics_init := Addr(_sk4d_graphics_init);
  sk4d_graphics_purge_all_caches := Addr(_sk4d_graphics_purge_all_caches);
  sk4d_graphics_purge_font_cache := Addr(_sk4d_graphics_purge_font_cache);
  sk4d_graphics_purge_resource_cache := Addr(_sk4d_graphics_purge_resource_cache);
  sk4d_graphics_set_font_cache_count_limit := Addr(_sk4d_graphics_set_font_cache_count_limit);
  sk4d_graphics_set_font_cache_limit := Addr(_sk4d_graphics_set_font_cache_limit);
  sk4d_graphics_set_resource_cache_single_allocation_byte_limit := Addr(_sk4d_graphics_set_resource_cache_single_allocation_byte_limit);
  sk4d_graphics_set_resource_cache_total_byte_limit := Addr(_sk4d_graphics_set_resource_cache_total_byte_limit);


 { include/c/sk4d_image.h }

  sk4d_image_encode_to_file := Addr(_sk4d_image_encode_to_file);
  sk4d_image_encode_to_stream := Addr(_sk4d_image_encode_to_stream);
  sk4d_image_get_alpha_type := Addr(_sk4d_image_get_alpha_type);
  sk4d_image_get_color_space := Addr(_sk4d_image_get_color_space);
  sk4d_image_get_color_type := Addr(_sk4d_image_get_color_type);
  sk4d_image_get_height := Addr(_sk4d_image_get_height);
  sk4d_image_get_image_info := Addr(_sk4d_image_get_image_info);
  sk4d_image_get_unique_id := Addr(_sk4d_image_get_unique_id);
  sk4d_image_get_width := Addr(_sk4d_image_get_width);
  sk4d_image_is_lazy_generated := Addr(_sk4d_image_is_lazy_generated);
  sk4d_image_is_texture_backed := Addr(_sk4d_image_is_texture_backed);
  sk4d_image_is_valid := Addr(_sk4d_image_is_valid);
  sk4d_image_make_cross_context := Addr(_sk4d_image_make_cross_context);
  sk4d_image_make_from_adopted_texture := Addr(_sk4d_image_make_from_adopted_texture);
  sk4d_image_make_from_encoded_file := Addr(_sk4d_image_make_from_encoded_file);
  sk4d_image_make_from_encoded_stream := Addr(_sk4d_image_make_from_encoded_stream);
  sk4d_image_make_from_picture := Addr(_sk4d_image_make_from_picture);
  sk4d_image_make_from_raster := Addr(_sk4d_image_make_from_raster);
  sk4d_image_make_from_texture := Addr(_sk4d_image_make_from_texture);
  sk4d_image_make_non_texture_image := Addr(_sk4d_image_make_non_texture_image);
  sk4d_image_make_raster_copy := Addr(_sk4d_image_make_raster_copy);
  sk4d_image_make_raster_image := Addr(_sk4d_image_make_raster_image);
  sk4d_image_make_raw_shader := Addr(_sk4d_image_make_raw_shader);
  sk4d_image_make_shader := Addr(_sk4d_image_make_shader);
  sk4d_image_make_subset := Addr(_sk4d_image_make_subset);
  sk4d_image_make_texture_image := Addr(_sk4d_image_make_texture_image);
  sk4d_image_make_with_filter := Addr(_sk4d_image_make_with_filter);
  sk4d_image_peek_pixels := Addr(_sk4d_image_peek_pixels);
  sk4d_image_read_pixels := Addr(_sk4d_image_read_pixels);
  sk4d_image_scale_pixels := Addr(_sk4d_image_scale_pixels);


 { include/c/sk4d_imageencoder.h }

  sk4d_imageencoder_encode_to_file := Addr(_sk4d_imageencoder_encode_to_file);
  sk4d_imageencoder_encode_to_stream := Addr(_sk4d_imageencoder_encode_to_stream);


 { include/c/sk4d_imagefilter.h }

  sk4d_imagefilter_can_compute_fast_bounds := Addr(_sk4d_imagefilter_can_compute_fast_bounds);
  sk4d_imagefilter_compute_fast_bounds := Addr(_sk4d_imagefilter_compute_fast_bounds);
  sk4d_imagefilter_make_alpha_threshold := Addr(_sk4d_imagefilter_make_alpha_threshold);
  sk4d_imagefilter_make_arithmetic := Addr(_sk4d_imagefilter_make_arithmetic);
  sk4d_imagefilter_make_blend := Addr(_sk4d_imagefilter_make_blend);
  sk4d_imagefilter_make_blur := Addr(_sk4d_imagefilter_make_blur);
  sk4d_imagefilter_make_colorfilter := Addr(_sk4d_imagefilter_make_colorfilter);
  sk4d_imagefilter_make_compose := Addr(_sk4d_imagefilter_make_compose);
  sk4d_imagefilter_make_dilate := Addr(_sk4d_imagefilter_make_dilate);
  sk4d_imagefilter_make_displacement_map := Addr(_sk4d_imagefilter_make_displacement_map);
  sk4d_imagefilter_make_distant_lit_diffuse := Addr(_sk4d_imagefilter_make_distant_lit_diffuse);
  sk4d_imagefilter_make_distant_lit_specular := Addr(_sk4d_imagefilter_make_distant_lit_specular);
  sk4d_imagefilter_make_drop_shadow := Addr(_sk4d_imagefilter_make_drop_shadow);
  sk4d_imagefilter_make_drop_shadow_only := Addr(_sk4d_imagefilter_make_drop_shadow_only);
  sk4d_imagefilter_make_erode := Addr(_sk4d_imagefilter_make_erode);
  sk4d_imagefilter_make_image := Addr(_sk4d_imagefilter_make_image);
  sk4d_imagefilter_make_magnifier := Addr(_sk4d_imagefilter_make_magnifier);
  sk4d_imagefilter_make_matrix_convolution := Addr(_sk4d_imagefilter_make_matrix_convolution);
  sk4d_imagefilter_make_matrix_transform := Addr(_sk4d_imagefilter_make_matrix_transform);
  sk4d_imagefilter_make_merge := Addr(_sk4d_imagefilter_make_merge);
  sk4d_imagefilter_make_offset := Addr(_sk4d_imagefilter_make_offset);
  sk4d_imagefilter_make_picture := Addr(_sk4d_imagefilter_make_picture);
  sk4d_imagefilter_make_point_lit_diffuse := Addr(_sk4d_imagefilter_make_point_lit_diffuse);
  sk4d_imagefilter_make_point_lit_specular := Addr(_sk4d_imagefilter_make_point_lit_specular);
  sk4d_imagefilter_make_runtime_shader := Addr(_sk4d_imagefilter_make_runtime_shader);
  sk4d_imagefilter_make_runtime_shader2 := Addr(_sk4d_imagefilter_make_runtime_shader2);
  sk4d_imagefilter_make_shader := Addr(_sk4d_imagefilter_make_shader);
  sk4d_imagefilter_make_spot_lit_diffuse := Addr(_sk4d_imagefilter_make_spot_lit_diffuse);
  sk4d_imagefilter_make_spot_lit_specular := Addr(_sk4d_imagefilter_make_spot_lit_specular);
  sk4d_imagefilter_make_tile := Addr(_sk4d_imagefilter_make_tile);
  sk4d_imagefilter_make_with_local_matrix := Addr(_sk4d_imagefilter_make_with_local_matrix);



 { include/c/sk4d_maskfilter.h }

  sk4d_maskfilter_make_blur := Addr(_sk4d_maskfilter_make_blur);
  sk4d_maskfilter_make_shader := Addr(_sk4d_maskfilter_make_shader);
  sk4d_maskfilter_make_table := Addr(_sk4d_maskfilter_make_table);
  sk4d_maskfilter_make_table_clip := Addr(_sk4d_maskfilter_make_table_clip);
  sk4d_maskfilter_make_table_gamma := Addr(_sk4d_maskfilter_make_table_gamma);


 { include/c/sk4d_paint.h }

  sk4d_paint_create := Addr(_sk4d_paint_create);
  sk4d_paint_create2 := Addr(_sk4d_paint_create2);
  sk4d_paint_destroy := Addr(_sk4d_paint_destroy);
  sk4d_paint_get_alpha := Addr(_sk4d_paint_get_alpha);
  sk4d_paint_get_alphaf := Addr(_sk4d_paint_get_alphaf);
  sk4d_paint_get_anti_alias := Addr(_sk4d_paint_get_anti_alias);
  sk4d_paint_get_blender := Addr(_sk4d_paint_get_blender);
  sk4d_paint_get_color := Addr(_sk4d_paint_get_color);
  sk4d_paint_get_colorf := Addr(_sk4d_paint_get_colorf);
  sk4d_paint_get_color_filter := Addr(_sk4d_paint_get_color_filter);
  sk4d_paint_get_dither := Addr(_sk4d_paint_get_dither);
  sk4d_paint_get_fill_path := Addr(_sk4d_paint_get_fill_path);
  sk4d_paint_get_image_filter := Addr(_sk4d_paint_get_image_filter);
  sk4d_paint_get_mask_filter := Addr(_sk4d_paint_get_mask_filter);
  sk4d_paint_get_path_effect := Addr(_sk4d_paint_get_path_effect);
  sk4d_paint_get_shader := Addr(_sk4d_paint_get_shader);
  sk4d_paint_get_stroke_cap := Addr(_sk4d_paint_get_stroke_cap);
  sk4d_paint_get_stroke_join := Addr(_sk4d_paint_get_stroke_join);
  sk4d_paint_get_stroke_miter := Addr(_sk4d_paint_get_stroke_miter);
  sk4d_paint_get_stroke_width := Addr(_sk4d_paint_get_stroke_width);
  sk4d_paint_get_style := Addr(_sk4d_paint_get_style);
  sk4d_paint_reset := Addr(_sk4d_paint_reset);
  sk4d_paint_set_alpha := Addr(_sk4d_paint_set_alpha);
  sk4d_paint_set_alphaf := Addr(_sk4d_paint_set_alphaf);
  sk4d_paint_set_antialias := Addr(_sk4d_paint_set_antialias);
  sk4d_paint_set_argb := Addr(_sk4d_paint_set_argb);
  sk4d_paint_set_blender := Addr(_sk4d_paint_set_blender);
  sk4d_paint_set_color := Addr(_sk4d_paint_set_color);
  sk4d_paint_set_colorf := Addr(_sk4d_paint_set_colorf);
  sk4d_paint_set_color_filter := Addr(_sk4d_paint_set_color_filter);
  sk4d_paint_set_dither := Addr(_sk4d_paint_set_dither);
  sk4d_paint_set_image_filter := Addr(_sk4d_paint_set_image_filter);
  sk4d_paint_set_mask_filter := Addr(_sk4d_paint_set_mask_filter);
  sk4d_paint_set_path_effect := Addr(_sk4d_paint_set_path_effect);
  sk4d_paint_set_shader := Addr(_sk4d_paint_set_shader);
  sk4d_paint_set_stroke_cap := Addr(_sk4d_paint_set_stroke_cap);
  sk4d_paint_set_stroke_join := Addr(_sk4d_paint_set_stroke_join);
  sk4d_paint_set_stroke_miter := Addr(_sk4d_paint_set_stroke_miter);
  sk4d_paint_set_stroke_width := Addr(_sk4d_paint_set_stroke_width);
  sk4d_paint_set_style := Addr(_sk4d_paint_set_style);


 { include/c/sk4d_path.h }

  sk4d_opbuilder_add := Addr(_sk4d_opbuilder_add);
  sk4d_opbuilder_create := Addr(_sk4d_opbuilder_create);
  sk4d_opbuilder_destroy := Addr(_sk4d_opbuilder_destroy);
  sk4d_opbuilder_detach := Addr(_sk4d_opbuilder_detach);
  sk4d_path_contains := Addr(_sk4d_path_contains);
  sk4d_path_convert_conic_to_quads := Addr(_sk4d_path_convert_conic_to_quads);
  sk4d_path_create := Addr(_sk4d_path_create);
  sk4d_path_create2 := Addr(_sk4d_path_create2);
  sk4d_path_destroy := Addr(_sk4d_path_destroy);
  sk4d_path_get_bounds := Addr(_sk4d_path_get_bounds);
  sk4d_path_get_fill_type := Addr(_sk4d_path_get_fill_type);
  sk4d_path_get_last_point := Addr(_sk4d_path_get_last_point);
  sk4d_path_get_segment_masks := Addr(_sk4d_path_get_segment_masks);
  sk4d_path_get_tight_bounds := Addr(_sk4d_path_get_tight_bounds);
  sk4d_path_interpolate := Addr(_sk4d_path_interpolate);
  sk4d_path_is_convex := Addr(_sk4d_path_is_convex);
  sk4d_path_is_empty := Addr(_sk4d_path_is_empty);
  sk4d_path_is_finite := Addr(_sk4d_path_is_finite);
  sk4d_path_is_interpolatable := Addr(_sk4d_path_is_interpolatable);
  sk4d_path_is_last_contour_closed := Addr(_sk4d_path_is_last_contour_closed);
  sk4d_path_is_line := Addr(_sk4d_path_is_line);
  sk4d_path_is_oval := Addr(_sk4d_path_is_oval);
  sk4d_path_is_rect := Addr(_sk4d_path_is_rect);
  sk4d_path_is_rrect := Addr(_sk4d_path_is_rrect);
  sk4d_path_op := Addr(_sk4d_path_op);
  sk4d_path_serialize_to_stream := Addr(_sk4d_path_serialize_to_stream);
  sk4d_path_to_svg := Addr(_sk4d_path_to_svg);
  sk4d_path_transform := Addr(_sk4d_path_transform);
  sk4d_pathiterator_create := Addr(_sk4d_pathiterator_create);
  sk4d_pathiterator_destroy := Addr(_sk4d_pathiterator_destroy);
  sk4d_pathiterator_next := Addr(_sk4d_pathiterator_next);


 { include/c/sk4d_pathbuilder.h }

  sk4d_pathbuilder_add_arc := Addr(_sk4d_pathbuilder_add_arc);
  sk4d_pathbuilder_add_circle := Addr(_sk4d_pathbuilder_add_circle);
  sk4d_pathbuilder_add_oval := Addr(_sk4d_pathbuilder_add_oval);
  sk4d_pathbuilder_add_path := Addr(_sk4d_pathbuilder_add_path);
  sk4d_pathbuilder_add_polygon := Addr(_sk4d_pathbuilder_add_polygon);
  sk4d_pathbuilder_add_rect := Addr(_sk4d_pathbuilder_add_rect);
  sk4d_pathbuilder_add_rrect := Addr(_sk4d_pathbuilder_add_rrect);
  sk4d_pathbuilder_arc_to := Addr(_sk4d_pathbuilder_arc_to);
  sk4d_pathbuilder_arc_to2 := Addr(_sk4d_pathbuilder_arc_to2);
  sk4d_pathbuilder_arc_to3 := Addr(_sk4d_pathbuilder_arc_to3);
  sk4d_pathbuilder_close := Addr(_sk4d_pathbuilder_close);
  sk4d_pathbuilder_conic_to := Addr(_sk4d_pathbuilder_conic_to);
  sk4d_pathbuilder_create := Addr(_sk4d_pathbuilder_create);
  sk4d_pathbuilder_create2 := Addr(_sk4d_pathbuilder_create2);
  sk4d_pathbuilder_cubic_to := Addr(_sk4d_pathbuilder_cubic_to);
  sk4d_pathbuilder_destroy := Addr(_sk4d_pathbuilder_destroy);
  sk4d_pathbuilder_detach := Addr(_sk4d_pathbuilder_detach);
  sk4d_pathbuilder_get_bounds := Addr(_sk4d_pathbuilder_get_bounds);
  sk4d_pathbuilder_get_fill_type := Addr(_sk4d_pathbuilder_get_fill_type);
  sk4d_pathbuilder_inc_reserve := Addr(_sk4d_pathbuilder_inc_reserve);
  sk4d_pathbuilder_line_to := Addr(_sk4d_pathbuilder_line_to);
  sk4d_pathbuilder_move_to := Addr(_sk4d_pathbuilder_move_to);
  sk4d_pathbuilder_offset := Addr(_sk4d_pathbuilder_offset);
  sk4d_pathbuilder_polyline_to := Addr(_sk4d_pathbuilder_polyline_to);
  sk4d_pathbuilder_quad_to := Addr(_sk4d_pathbuilder_quad_to);
  sk4d_pathbuilder_r_conic_to := Addr(_sk4d_pathbuilder_r_conic_to);
  sk4d_pathbuilder_r_cubic_to := Addr(_sk4d_pathbuilder_r_cubic_to);
  sk4d_pathbuilder_r_line_to := Addr(_sk4d_pathbuilder_r_line_to);
  sk4d_pathbuilder_r_quad_to := Addr(_sk4d_pathbuilder_r_quad_to);
  sk4d_pathbuilder_reset := Addr(_sk4d_pathbuilder_reset);
  sk4d_pathbuilder_set_filltype := Addr(_sk4d_pathbuilder_set_filltype);
  sk4d_pathbuilder_snapshot := Addr(_sk4d_pathbuilder_snapshot);
  sk4d_pathbuilder_toggle_inverse_filltype := Addr(_sk4d_pathbuilder_toggle_inverse_filltype);


 { include/c/sk4d_patheffect.h }

  sk4d_patheffect_make_1dpath := Addr(_sk4d_patheffect_make_1dpath);
  sk4d_patheffect_make_2dline := Addr(_sk4d_patheffect_make_2dline);
  sk4d_patheffect_make_2dpath := Addr(_sk4d_patheffect_make_2dpath);
  sk4d_patheffect_make_compose := Addr(_sk4d_patheffect_make_compose);
  sk4d_patheffect_make_corner := Addr(_sk4d_patheffect_make_corner);
  sk4d_patheffect_make_dash := Addr(_sk4d_patheffect_make_dash);
  sk4d_patheffect_make_discrete := Addr(_sk4d_patheffect_make_discrete);
  sk4d_patheffect_make_matrix := Addr(_sk4d_patheffect_make_matrix);
  sk4d_patheffect_make_merge := Addr(_sk4d_patheffect_make_merge);
  sk4d_patheffect_make_stroke := Addr(_sk4d_patheffect_make_stroke);
  sk4d_patheffect_make_stroke_and_fill := Addr(_sk4d_patheffect_make_stroke_and_fill);
  sk4d_patheffect_make_sum := Addr(_sk4d_patheffect_make_sum);
  sk4d_patheffect_make_translate := Addr(_sk4d_patheffect_make_translate);
  sk4d_patheffect_make_trim := Addr(_sk4d_patheffect_make_trim);


 { include/c/sk4d_pathmeasure.h }

  sk4d_pathmeasure_create := Addr(_sk4d_pathmeasure_create);
  sk4d_pathmeasure_destroy := Addr(_sk4d_pathmeasure_destroy);
  sk4d_pathmeasure_get_length := Addr(_sk4d_pathmeasure_get_length);
  sk4d_pathmeasure_get_matrix := Addr(_sk4d_pathmeasure_get_matrix);
  sk4d_pathmeasure_get_position_and_tangent := Addr(_sk4d_pathmeasure_get_position_and_tangent);
  sk4d_pathmeasure_get_segment := Addr(_sk4d_pathmeasure_get_segment);
  sk4d_pathmeasure_is_closed := Addr(_sk4d_pathmeasure_is_closed);
  sk4d_pathmeasure_next_contour := Addr(_sk4d_pathmeasure_next_contour);


 { include/c/sk4d_picture.h }

  sk4d_picture_approximate_bytes_used := Addr(_sk4d_picture_approximate_bytes_used);
  sk4d_picture_approximate_op_count := Addr(_sk4d_picture_approximate_op_count);
  sk4d_picture_get_cull_rect := Addr(_sk4d_picture_get_cull_rect);
  sk4d_picture_make_from_stream := Addr(_sk4d_picture_make_from_stream);
  sk4d_picture_make_shader := Addr(_sk4d_picture_make_shader);
  sk4d_picture_playback := Addr(_sk4d_picture_playback);
  sk4d_picture_serialize_to_stream := Addr(_sk4d_picture_serialize_to_stream);


 { include/c/sk4d_picturerecorder.h }

  sk4d_picturerecorder_begin_recording := Addr(_sk4d_picturerecorder_begin_recording);
  sk4d_picturerecorder_create := Addr(_sk4d_picturerecorder_create);
  sk4d_picturerecorder_destroy := Addr(_sk4d_picturerecorder_destroy);
  sk4d_picturerecorder_finish_recording := Addr(_sk4d_picturerecorder_finish_recording);
  sk4d_picturerecorder_finish_recording2 := Addr(_sk4d_picturerecorder_finish_recording2);


 { include/c/sk4d_pixmap.h }

  sk4d_pixmap_create := Addr(_sk4d_pixmap_create);
  sk4d_pixmap_destroy := Addr(_sk4d_pixmap_destroy);
  sk4d_pixmap_erase := Addr(_sk4d_pixmap_erase);
  sk4d_pixmap_erase2 := Addr(_sk4d_pixmap_erase2);
  sk4d_pixmap_extract_subset := Addr(_sk4d_pixmap_extract_subset);
  sk4d_pixmap_get_alpha := Addr(_sk4d_pixmap_get_alpha);
  sk4d_pixmap_get_alpha_type := Addr(_sk4d_pixmap_get_alpha_type);
  sk4d_pixmap_get_color := Addr(_sk4d_pixmap_get_color);
  sk4d_pixmap_get_color_space := Addr(_sk4d_pixmap_get_color_space);
  sk4d_pixmap_get_color_type := Addr(_sk4d_pixmap_get_color_type);
  sk4d_pixmap_get_colorf := Addr(_sk4d_pixmap_get_colorf);
  sk4d_pixmap_get_height := Addr(_sk4d_pixmap_get_height);
  sk4d_pixmap_get_image_info := Addr(_sk4d_pixmap_get_image_info);
  sk4d_pixmap_get_pixel_addr := Addr(_sk4d_pixmap_get_pixel_addr);
  sk4d_pixmap_get_pixels := Addr(_sk4d_pixmap_get_pixels);
  sk4d_pixmap_get_row_bytes := Addr(_sk4d_pixmap_get_row_bytes);
  sk4d_pixmap_get_width := Addr(_sk4d_pixmap_get_width);
  sk4d_pixmap_read_pixels := Addr(_sk4d_pixmap_read_pixels);
  sk4d_pixmap_scale_pixels := Addr(_sk4d_pixmap_scale_pixels);
  sk4d_pixmap_set_colorspace := Addr(_sk4d_pixmap_set_colorspace);


 { include/c/sk4d_refcnt.h }

  sk4d_refcnt_ref := Addr(_sk4d_refcnt_ref);
  sk4d_refcnt_unref := Addr(_sk4d_refcnt_unref);


 { include/c/sk4d_region.h }

  sk4d_region_contains := Addr(_sk4d_region_contains);
  sk4d_region_contains2 := Addr(_sk4d_region_contains2);
  sk4d_region_contains3 := Addr(_sk4d_region_contains3);
  sk4d_region_create := Addr(_sk4d_region_create);
  sk4d_region_create2 := Addr(_sk4d_region_create2);
  sk4d_region_destroy := Addr(_sk4d_region_destroy);
  sk4d_region_get_boundary_path := Addr(_sk4d_region_get_boundary_path);
  sk4d_region_get_bounds := Addr(_sk4d_region_get_bounds);
  sk4d_region_intersects := Addr(_sk4d_region_intersects);
  sk4d_region_intersects2 := Addr(_sk4d_region_intersects2);
  sk4d_region_is_complex := Addr(_sk4d_region_is_complex);
  sk4d_region_is_empty := Addr(_sk4d_region_is_empty);
  sk4d_region_is_equal := Addr(_sk4d_region_is_equal);
  sk4d_region_is_rect := Addr(_sk4d_region_is_rect);
  sk4d_region_op := Addr(_sk4d_region_op);
  sk4d_region_op2 := Addr(_sk4d_region_op2);
  sk4d_region_quick_contains := Addr(_sk4d_region_quick_contains);
  sk4d_region_quick_reject := Addr(_sk4d_region_quick_reject);
  sk4d_region_quick_reject2 := Addr(_sk4d_region_quick_reject2);
  sk4d_region_set_empty := Addr(_sk4d_region_set_empty);
  sk4d_region_set_path := Addr(_sk4d_region_set_path);
  sk4d_region_set_rect := Addr(_sk4d_region_set_rect);
  sk4d_region_set_rects := Addr(_sk4d_region_set_rects);
  sk4d_region_translate := Addr(_sk4d_region_translate);
  sk4d_regioncliperator_create := Addr(_sk4d_regioncliperator_create);
  sk4d_regioncliperator_destroy := Addr(_sk4d_regioncliperator_destroy);
  sk4d_regioncliperator_get_current := Addr(_sk4d_regioncliperator_get_current);
  sk4d_regioncliperator_move_next := Addr(_sk4d_regioncliperator_move_next);
  sk4d_regioniterator_create := Addr(_sk4d_regioniterator_create);
  sk4d_regioniterator_destroy := Addr(_sk4d_regioniterator_destroy);
  sk4d_regioniterator_get_current := Addr(_sk4d_regioniterator_get_current);
  sk4d_regioniterator_move_next := Addr(_sk4d_regioniterator_move_next);
  sk4d_regionspanerator_create := Addr(_sk4d_regionspanerator_create);
  sk4d_regionspanerator_destroy := Addr(_sk4d_regionspanerator_destroy);
  sk4d_regionspanerator_next := Addr(_sk4d_regionspanerator_next);


 { include/c/sk4d_rrect.h }

  sk4d_rrect_contains := Addr(_sk4d_rrect_contains);
  sk4d_rrect_create := Addr(_sk4d_rrect_create);
  sk4d_rrect_create2 := Addr(_sk4d_rrect_create2);
  sk4d_rrect_deflate := Addr(_sk4d_rrect_deflate);
  sk4d_rrect_destroy := Addr(_sk4d_rrect_destroy);
  sk4d_rrect_get_height := Addr(_sk4d_rrect_get_height);
  sk4d_rrect_get_radii := Addr(_sk4d_rrect_get_radii);
  sk4d_rrect_get_rect := Addr(_sk4d_rrect_get_rect);
  sk4d_rrect_get_simple_radii := Addr(_sk4d_rrect_get_simple_radii);
  sk4d_rrect_get_width := Addr(_sk4d_rrect_get_width);
  sk4d_rrect_inflate := Addr(_sk4d_rrect_inflate);
  sk4d_rrect_is_complex := Addr(_sk4d_rrect_is_complex);
  sk4d_rrect_is_empty := Addr(_sk4d_rrect_is_empty);
  sk4d_rrect_is_equal := Addr(_sk4d_rrect_is_equal);
  sk4d_rrect_is_nine_patch := Addr(_sk4d_rrect_is_nine_patch);
  sk4d_rrect_is_oval := Addr(_sk4d_rrect_is_oval);
  sk4d_rrect_is_rect := Addr(_sk4d_rrect_is_rect);
  sk4d_rrect_is_simple := Addr(_sk4d_rrect_is_simple);
  sk4d_rrect_is_valid := Addr(_sk4d_rrect_is_valid);
  sk4d_rrect_offset := Addr(_sk4d_rrect_offset);
  sk4d_rrect_set_empty := Addr(_sk4d_rrect_set_empty);
  sk4d_rrect_set_nine_patch := Addr(_sk4d_rrect_set_nine_patch);
  sk4d_rrect_set_oval := Addr(_sk4d_rrect_set_oval);
  sk4d_rrect_set_rect := Addr(_sk4d_rrect_set_rect);
  sk4d_rrect_set_rect2 := Addr(_sk4d_rrect_set_rect2);
  sk4d_rrect_set_rect3 := Addr(_sk4d_rrect_set_rect3);
  sk4d_rrect_transform := Addr(_sk4d_rrect_transform);


 { include/c/sk4d_runtimeeffect.h }

  sk4d_runtimeblendbuilder_create := Addr(_sk4d_runtimeblendbuilder_create);
  sk4d_runtimeblendbuilder_destroy := Addr(_sk4d_runtimeblendbuilder_destroy);
  sk4d_runtimeblendbuilder_make_blender := Addr(_sk4d_runtimeblendbuilder_make_blender);
  sk4d_runtimeeffect_get_child_count := Addr(_sk4d_runtimeeffect_get_child_count);
  sk4d_runtimeeffect_get_child_name := Addr(_sk4d_runtimeeffect_get_child_name);
  sk4d_runtimeeffect_get_child_type := Addr(_sk4d_runtimeeffect_get_child_type);
  sk4d_runtimeeffect_get_uniform_count := Addr(_sk4d_runtimeeffect_get_uniform_count);
  sk4d_runtimeeffect_get_uniform_data_size := Addr(_sk4d_runtimeeffect_get_uniform_data_size);
  sk4d_runtimeeffect_get_uniform_name := Addr(_sk4d_runtimeeffect_get_uniform_name);
  sk4d_runtimeeffect_get_uniform_offset := Addr(_sk4d_runtimeeffect_get_uniform_offset);
  sk4d_runtimeeffect_get_uniform_type := Addr(_sk4d_runtimeeffect_get_uniform_type);
  sk4d_runtimeeffect_get_uniform_type_count := Addr(_sk4d_runtimeeffect_get_uniform_type_count);
  sk4d_runtimeeffect_index_of_child := Addr(_sk4d_runtimeeffect_index_of_child);
  sk4d_runtimeeffect_index_of_uniform := Addr(_sk4d_runtimeeffect_index_of_uniform);
  sk4d_runtimeeffect_make_blender := Addr(_sk4d_runtimeeffect_make_blender);
  sk4d_runtimeeffect_make_color_filter := Addr(_sk4d_runtimeeffect_make_color_filter);
  sk4d_runtimeeffect_make_for_blender := Addr(_sk4d_runtimeeffect_make_for_blender);
  sk4d_runtimeeffect_make_for_color_filter := Addr(_sk4d_runtimeeffect_make_for_color_filter);
  sk4d_runtimeeffect_make_for_shader := Addr(_sk4d_runtimeeffect_make_for_shader);
  sk4d_runtimeeffect_make_image := Addr(_sk4d_runtimeeffect_make_image);
  sk4d_runtimeeffect_make_shader := Addr(_sk4d_runtimeeffect_make_shader);
  sk4d_runtimeeffectbuilder_set_child := Addr(_sk4d_runtimeeffectbuilder_set_child);
  sk4d_runtimeeffectbuilder_set_child2 := Addr(_sk4d_runtimeeffectbuilder_set_child2);
  sk4d_runtimeeffectbuilder_set_child3 := Addr(_sk4d_runtimeeffectbuilder_set_child3);
  sk4d_runtimeeffectbuilder_set_uniform := Addr(_sk4d_runtimeeffectbuilder_set_uniform);
  sk4d_runtimeeffectbuilder_get_effect := Addr(_sk4d_runtimeeffectbuilder_get_effect);
  sk4d_runtimeshaderbuilder_create := Addr(_sk4d_runtimeshaderbuilder_create);
  sk4d_runtimeshaderbuilder_destroy := Addr(_sk4d_runtimeshaderbuilder_destroy);
  sk4d_runtimeshaderbuilder_make_image := Addr(_sk4d_runtimeshaderbuilder_make_image);
  sk4d_runtimeshaderbuilder_make_shader := Addr(_sk4d_runtimeshaderbuilder_make_shader);


 { include/c/sk4d_shader.h }

  sk4d_shader_make_blend := Addr(_sk4d_shader_make_blend);
  sk4d_shader_make_color := Addr(_sk4d_shader_make_color);
  sk4d_shader_make_color2 := Addr(_sk4d_shader_make_color2);
  sk4d_shader_make_empty := Addr(_sk4d_shader_make_empty);
  sk4d_shader_make_gradient_linear := Addr(_sk4d_shader_make_gradient_linear);
  sk4d_shader_make_gradient_linear2 := Addr(_sk4d_shader_make_gradient_linear2);
  sk4d_shader_make_gradient_radial := Addr(_sk4d_shader_make_gradient_radial);
  sk4d_shader_make_gradient_radial2 := Addr(_sk4d_shader_make_gradient_radial2);
  sk4d_shader_make_gradient_sweep := Addr(_sk4d_shader_make_gradient_sweep);
  sk4d_shader_make_gradient_sweep2 := Addr(_sk4d_shader_make_gradient_sweep2);
  sk4d_shader_make_gradient_two_point_conical := Addr(_sk4d_shader_make_gradient_two_point_conical);
  sk4d_shader_make_gradient_two_point_conical2 := Addr(_sk4d_shader_make_gradient_two_point_conical2);
  sk4d_shader_make_perlin_noise_fractal_noise := Addr(_sk4d_shader_make_perlin_noise_fractal_noise);
  sk4d_shader_make_perlin_noise_turbulence := Addr(_sk4d_shader_make_perlin_noise_turbulence);
  sk4d_shader_make_with_color_filter := Addr(_sk4d_shader_make_with_color_filter);
  sk4d_shader_make_with_local_matrix := Addr(_sk4d_shader_make_with_local_matrix);


 { include/c/sk4d_stream.h }

  sk4d_streamadapter_create := Addr(_sk4d_streamadapter_create);
  sk4d_streamadapter_destroy := Addr(_sk4d_streamadapter_destroy);
  sk4d_streamadapter_set_procs := Addr(_sk4d_streamadapter_set_procs);
  sk4d_wstreamadapter_create := Addr(_sk4d_wstreamadapter_create);
  sk4d_wstreamadapter_destroy := Addr(_sk4d_wstreamadapter_destroy);
  sk4d_wstreamadapter_set_procs := Addr(_sk4d_wstreamadapter_set_procs);


 { include/c/sk4d_string.h }

  sk4d_string_create := Addr(_sk4d_string_create);
  sk4d_string_destroy := Addr(_sk4d_string_destroy);
  sk4d_string_get_text := Addr(_sk4d_string_get_text);


 { include/c/sk4d_surface.h }

  sk4d_surface_draw := Addr(_sk4d_surface_draw);
  sk4d_surface_flush := Addr(_sk4d_surface_flush);
  sk4d_surface_flush_and_submit := Addr(_sk4d_surface_flush_and_submit);
  sk4d_surface_get_canvas := Addr(_sk4d_surface_get_canvas);
  sk4d_surface_get_props := Addr(_sk4d_surface_get_props);
  sk4d_surface_make_from_mtk_view := Addr(_sk4d_surface_make_from_mtk_view);
  sk4d_surface_make_from_render_target := Addr(_sk4d_surface_make_from_render_target);
  sk4d_surface_make_from_texture := Addr(_sk4d_surface_make_from_texture);
  sk4d_surface_make_image_snapshot := Addr(_sk4d_surface_make_image_snapshot);
  sk4d_surface_make_image_snapshot2 := Addr(_sk4d_surface_make_image_snapshot2);
  sk4d_surface_make_raster := Addr(_sk4d_surface_make_raster);
  sk4d_surface_make_raster_direct := Addr(_sk4d_surface_make_raster_direct);
  sk4d_surface_make_render_target := Addr(_sk4d_surface_make_render_target);
  sk4d_surface_peek_pixels := Addr(_sk4d_surface_peek_pixels);
  sk4d_surface_read_pixels := Addr(_sk4d_surface_read_pixels);
  sk4d_surface_wait := Addr(_sk4d_surface_wait);
  sk4d_surface_write_pixels := Addr(_sk4d_surface_write_pixels);


 { include/c/sk4d_svgcanvas.h }

  sk4d_svgcanvas_make := Addr(_sk4d_svgcanvas_make);


 { include/c/sk4d_textblob.h }

  sk4d_textblob_get_intercepts := Addr(_sk4d_textblob_get_intercepts);
  sk4d_textblob_make_from_text := Addr(_sk4d_textblob_make_from_text);
  sk4d_textblob_make_from_text_horizontally_positioned := Addr(_sk4d_textblob_make_from_text_horizontally_positioned);
  sk4d_textblob_make_from_text_positioned := Addr(_sk4d_textblob_make_from_text_positioned);
  sk4d_textblob_make_from_text_transform := Addr(_sk4d_textblob_make_from_text_transform);
  sk4d_textblob_ref := Addr(_sk4d_textblob_ref);
  sk4d_textblob_unref := Addr(_sk4d_textblob_unref);


 { include/c/sk4d_tracememorydump.h }

  sk4d_tracememorydumpbaseclass_create := Addr(_sk4d_tracememorydumpbaseclass_create);
  sk4d_tracememorydumpbaseclass_destroy := Addr(_sk4d_tracememorydumpbaseclass_destroy);
  sk4d_tracememorydumpbaseclass_set_procs := Addr(_sk4d_tracememorydumpbaseclass_set_procs);


 { include/c/sk4d_typeface.h }

  sk4d_typeface_get_family_name := Addr(_sk4d_typeface_get_family_name);
  sk4d_typeface_get_slant := Addr(_sk4d_typeface_get_slant);
  sk4d_typeface_get_style := Addr(_sk4d_typeface_get_style);
  sk4d_typeface_get_weight := Addr(_sk4d_typeface_get_weight);
  sk4d_typeface_get_width := Addr(_sk4d_typeface_get_width);
  sk4d_typeface_make_default := Addr(_sk4d_typeface_make_default);
  sk4d_typeface_make_from_file := Addr(_sk4d_typeface_make_from_file);
  sk4d_typeface_make_from_stream := Addr(_sk4d_typeface_make_from_stream);
  sk4d_typeface_make_from_name := Addr(_sk4d_typeface_make_from_name);


 { include/c/sk4d_vertices.h }

  sk4d_vertices_make_copy := Addr(_sk4d_vertices_make_copy);
  sk4d_vertices_ref := Addr(_sk4d_vertices_ref);
  sk4d_vertices_unref := Addr(_sk4d_vertices_unref);


 { modules/particles/include/sk4d_particleeffect.h }

  sk4d_particleeffect_get_position := Addr(_sk4d_particleeffect_get_position);
  sk4d_particleeffect_get_rate := Addr(_sk4d_particleeffect_get_rate);
  sk4d_particleeffect_get_uniform := Addr(_sk4d_particleeffect_get_uniform);
  sk4d_particleeffect_get_uniform_count := Addr(_sk4d_particleeffect_get_uniform_count);
  sk4d_particleeffect_get_uniform_data := Addr(_sk4d_particleeffect_get_uniform_data);
  sk4d_particleeffect_get_uniform_data_count := Addr(_sk4d_particleeffect_get_uniform_data_count);
  sk4d_particleeffect_get_uniform_name := Addr(_sk4d_particleeffect_get_uniform_name);
  sk4d_particleeffect_init := Addr(_sk4d_particleeffect_init);
  sk4d_particleeffect_make_from_file := Addr(_sk4d_particleeffect_make_from_file);
  sk4d_particleeffect_make_from_stream := Addr(_sk4d_particleeffect_make_from_stream);
  sk4d_particleeffect_render := Addr(_sk4d_particleeffect_render);
  sk4d_particleeffect_set_position := Addr(_sk4d_particleeffect_set_position);
  sk4d_particleeffect_set_rate := Addr(_sk4d_particleeffect_set_rate);
  sk4d_particleeffect_set_uniform := Addr(_sk4d_particleeffect_set_uniform);
  sk4d_particleeffect_start := Addr(_sk4d_particleeffect_start);
  sk4d_particleeffect_update := Addr(_sk4d_particleeffect_update);


 { modules/skottie/include/sk4d_skottie.h }

  sk4d_skottieanimation_get_duration := Addr(_sk4d_skottieanimation_get_duration);
  sk4d_skottieanimation_get_fps := Addr(_sk4d_skottieanimation_get_fps);
  sk4d_skottieanimation_get_in_point := Addr(_sk4d_skottieanimation_get_in_point);
  sk4d_skottieanimation_get_out_point := Addr(_sk4d_skottieanimation_get_out_point);
  sk4d_skottieanimation_get_size := Addr(_sk4d_skottieanimation_get_size);
  sk4d_skottieanimation_get_version := Addr(_sk4d_skottieanimation_get_version);
  sk4d_skottieanimation_make_from_file := Addr(_sk4d_skottieanimation_make_from_file);
  sk4d_skottieanimation_make_from_stream := Addr(_sk4d_skottieanimation_make_from_stream);
  sk4d_skottieanimation_ref := Addr(_sk4d_skottieanimation_ref);
  sk4d_skottieanimation_render := Addr(_sk4d_skottieanimation_render);
  sk4d_skottieanimation_seek_frame := Addr(_sk4d_skottieanimation_seek_frame);
  sk4d_skottieanimation_seek_frame_time := Addr(_sk4d_skottieanimation_seek_frame_time);
  sk4d_skottieanimation_unref := Addr(_sk4d_skottieanimation_unref);


 { modules/skparagraph/include/sk4d_paragraph.h }

  sk4d_paragraph_destroy := Addr(_sk4d_paragraph_destroy);
  sk4d_paragraph_did_exceed_max_lines := Addr(_sk4d_paragraph_did_exceed_max_lines);
  sk4d_paragraph_get_alphabetic_baseline := Addr(_sk4d_paragraph_get_alphabetic_baseline);
  sk4d_paragraph_get_glyph_position_at_coordinate := Addr(_sk4d_paragraph_get_glyph_position_at_coordinate);
  sk4d_paragraph_get_height := Addr(_sk4d_paragraph_get_height);
  sk4d_paragraph_get_ideographic_baseline := Addr(_sk4d_paragraph_get_ideographic_baseline);
  sk4d_paragraph_get_line_metrics := Addr(_sk4d_paragraph_get_line_metrics);
  sk4d_paragraph_get_longest_line := Addr(_sk4d_paragraph_get_longest_line);
  sk4d_paragraph_get_max_intrinsic_width := Addr(_sk4d_paragraph_get_max_intrinsic_width);
  sk4d_paragraph_get_max_width := Addr(_sk4d_paragraph_get_max_width);
  sk4d_paragraph_get_min_intrinsic_width := Addr(_sk4d_paragraph_get_min_intrinsic_width);
  sk4d_paragraph_get_rects_for_placeholders := Addr(_sk4d_paragraph_get_rects_for_placeholders);
  sk4d_paragraph_get_rects_for_range := Addr(_sk4d_paragraph_get_rects_for_range);
  sk4d_paragraph_get_word_boundary := Addr(_sk4d_paragraph_get_word_boundary);
  sk4d_paragraph_layout := Addr(_sk4d_paragraph_layout);
  sk4d_paragraph_paint := Addr(_sk4d_paragraph_paint);
  sk4d_paragraph_to_path := Addr(_sk4d_paragraph_to_path);
  sk4d_paragraph_visit := Addr(_sk4d_paragraph_visit);


 { modules/skparagraph/include/sk4d_paragraphbuilder.h }

  sk4d_paragraphbuilder_add_placeholder := Addr(_sk4d_paragraphbuilder_add_placeholder);
  sk4d_paragraphbuilder_add_text := Addr(_sk4d_paragraphbuilder_add_text);
  sk4d_paragraphbuilder_build := Addr(_sk4d_paragraphbuilder_build);
  sk4d_paragraphbuilder_create := Addr(_sk4d_paragraphbuilder_create);
  sk4d_paragraphbuilder_create2 := Addr(_sk4d_paragraphbuilder_create2);
  sk4d_paragraphbuilder_destroy := Addr(_sk4d_paragraphbuilder_destroy);
  sk4d_paragraphbuilder_pop := Addr(_sk4d_paragraphbuilder_pop);
  sk4d_paragraphbuilder_push_style := Addr(_sk4d_paragraphbuilder_push_style);


 { modules/skparagraph/include/sk4d_paragraphstyle.h }

  sk4d_paragraphstyle_create := Addr(_sk4d_paragraphstyle_create);
  sk4d_paragraphstyle_destroy := Addr(_sk4d_paragraphstyle_destroy);
  sk4d_paragraphstyle_disable_hinting := Addr(_sk4d_paragraphstyle_disable_hinting);
  sk4d_paragraphstyle_get_ellipsis := Addr(_sk4d_paragraphstyle_get_ellipsis);
  sk4d_paragraphstyle_get_height := Addr(_sk4d_paragraphstyle_get_height);
  sk4d_paragraphstyle_get_max_lines := Addr(_sk4d_paragraphstyle_get_max_lines);
  sk4d_paragraphstyle_get_strut_style := Addr(_sk4d_paragraphstyle_get_strut_style);
  sk4d_paragraphstyle_get_text_align := Addr(_sk4d_paragraphstyle_get_text_align);
  sk4d_paragraphstyle_get_text_direction := Addr(_sk4d_paragraphstyle_get_text_direction);
  sk4d_paragraphstyle_get_text_height_behaviors := Addr(_sk4d_paragraphstyle_get_text_height_behaviors);
  sk4d_paragraphstyle_get_text_style := Addr(_sk4d_paragraphstyle_get_text_style);
  sk4d_paragraphstyle_set_ellipsis := Addr(_sk4d_paragraphstyle_set_ellipsis);
  sk4d_paragraphstyle_set_height := Addr(_sk4d_paragraphstyle_set_height);
  sk4d_paragraphstyle_set_max_lines := Addr(_sk4d_paragraphstyle_set_max_lines);
  sk4d_paragraphstyle_set_strut_style := Addr(_sk4d_paragraphstyle_set_strut_style);
  sk4d_paragraphstyle_set_text_align := Addr(_sk4d_paragraphstyle_set_text_align);
  sk4d_paragraphstyle_set_text_direction := Addr(_sk4d_paragraphstyle_set_text_direction);
  sk4d_paragraphstyle_set_text_height_behaviors := Addr(_sk4d_paragraphstyle_set_text_height_behaviors);
  sk4d_paragraphstyle_set_text_style := Addr(_sk4d_paragraphstyle_set_text_style);
  sk4d_strutstyle_create := Addr(_sk4d_strutstyle_create);
  sk4d_strutstyle_destroy := Addr(_sk4d_strutstyle_destroy);
  sk4d_strutstyle_get_enabled := Addr(_sk4d_strutstyle_get_enabled);
  sk4d_strutstyle_get_font_families := Addr(_sk4d_strutstyle_get_font_families);
  sk4d_strutstyle_get_font_size := Addr(_sk4d_strutstyle_get_font_size);
  sk4d_strutstyle_get_font_style := Addr(_sk4d_strutstyle_get_font_style);
  sk4d_strutstyle_get_force_height := Addr(_sk4d_strutstyle_get_force_height);
  sk4d_strutstyle_get_half_leading := Addr(_sk4d_strutstyle_get_half_leading);
  sk4d_strutstyle_get_height_multiplier := Addr(_sk4d_strutstyle_get_height_multiplier);
  sk4d_strutstyle_get_leading := Addr(_sk4d_strutstyle_get_leading);
  sk4d_strutstyle_is_equal := Addr(_sk4d_strutstyle_is_equal);
  sk4d_strutstyle_set_enabled := Addr(_sk4d_strutstyle_set_enabled);
  sk4d_strutstyle_set_font_families := Addr(_sk4d_strutstyle_set_font_families);
  sk4d_strutstyle_set_font_size := Addr(_sk4d_strutstyle_set_font_size);
  sk4d_strutstyle_set_font_style := Addr(_sk4d_strutstyle_set_font_style);
  sk4d_strutstyle_set_force_height := Addr(_sk4d_strutstyle_set_force_height);
  sk4d_strutstyle_set_half_leading := Addr(_sk4d_strutstyle_set_half_leading);
  sk4d_strutstyle_set_height_multiplier := Addr(_sk4d_strutstyle_set_height_multiplier);
  sk4d_strutstyle_set_leading := Addr(_sk4d_strutstyle_set_leading);


 { modules/skparagraph/include/sk4d_textstyle.h }

  sk4d_textstyle_add_font_feature := Addr(_sk4d_textstyle_add_font_feature);
  sk4d_textstyle_add_shadow := Addr(_sk4d_textstyle_add_shadow);
  sk4d_textstyle_clear_background_color := Addr(_sk4d_textstyle_clear_background_color);
  sk4d_textstyle_clear_foreground_color := Addr(_sk4d_textstyle_clear_foreground_color);
  sk4d_textstyle_create := Addr(_sk4d_textstyle_create);
  sk4d_textstyle_destroy := Addr(_sk4d_textstyle_destroy);
  sk4d_textstyle_get_background := Addr(_sk4d_textstyle_get_background);
  sk4d_textstyle_get_color := Addr(_sk4d_textstyle_get_color);
  sk4d_textstyle_get_decoration_color := Addr(_sk4d_textstyle_get_decoration_color);
  sk4d_textstyle_get_decoration_style := Addr(_sk4d_textstyle_get_decoration_style);
  sk4d_textstyle_get_decoration_thickness := Addr(_sk4d_textstyle_get_decoration_thickness);
  sk4d_textstyle_get_decorations := Addr(_sk4d_textstyle_get_decorations);
  sk4d_textstyle_get_font_families := Addr(_sk4d_textstyle_get_font_families);
  sk4d_textstyle_get_font_metrics := Addr(_sk4d_textstyle_get_font_metrics);
  sk4d_textstyle_get_font_size := Addr(_sk4d_textstyle_get_font_size);
  sk4d_textstyle_get_font_style := Addr(_sk4d_textstyle_get_font_style);
  sk4d_textstyle_get_foreground := Addr(_sk4d_textstyle_get_foreground);
  sk4d_textstyle_get_half_leading := Addr(_sk4d_textstyle_get_half_leading);
  sk4d_textstyle_get_height_multiplier := Addr(_sk4d_textstyle_get_height_multiplier);
  sk4d_textstyle_get_letter_spacing := Addr(_sk4d_textstyle_get_letter_spacing);
  sk4d_textstyle_get_locale := Addr(_sk4d_textstyle_get_locale);
  sk4d_textstyle_get_word_spacing := Addr(_sk4d_textstyle_get_word_spacing);
  sk4d_textstyle_is_equal := Addr(_sk4d_textstyle_is_equal);
  sk4d_textstyle_reset_font_features := Addr(_sk4d_textstyle_reset_font_features);
  sk4d_textstyle_reset_shadows := Addr(_sk4d_textstyle_reset_shadows);
  sk4d_textstyle_set_background_color := Addr(_sk4d_textstyle_set_background_color);
  sk4d_textstyle_set_color := Addr(_sk4d_textstyle_set_color);
  sk4d_textstyle_set_decoration_color := Addr(_sk4d_textstyle_set_decoration_color);
  sk4d_textstyle_set_decoration_style := Addr(_sk4d_textstyle_set_decoration_style);
  sk4d_textstyle_set_decoration_thickness := Addr(_sk4d_textstyle_set_decoration_thickness);
  sk4d_textstyle_set_decorations := Addr(_sk4d_textstyle_set_decorations);
  sk4d_textstyle_set_font_families := Addr(_sk4d_textstyle_set_font_families);
  sk4d_textstyle_set_font_size := Addr(_sk4d_textstyle_set_font_size);
  sk4d_textstyle_set_font_style := Addr(_sk4d_textstyle_set_font_style);
  sk4d_textstyle_set_foreground_color := Addr(_sk4d_textstyle_set_foreground_color);
  sk4d_textstyle_set_half_leading := Addr(_sk4d_textstyle_set_half_leading);
  sk4d_textstyle_set_height_multiplier := Addr(_sk4d_textstyle_set_height_multiplier);
  sk4d_textstyle_set_letter_spacing := Addr(_sk4d_textstyle_set_letter_spacing);
  sk4d_textstyle_set_locale := Addr(_sk4d_textstyle_set_locale);
  sk4d_textstyle_set_word_spacing := Addr(_sk4d_textstyle_set_word_spacing);


 { modules/skparagraph/include/sk4d_typefacefontprovider.h }

  sk4d_typefacefontprovider_create := Addr(_sk4d_typefacefontprovider_create);
  sk4d_typefacefontprovider_register_typeface := Addr(_sk4d_typefacefontprovider_register_typeface);
  sk4d_typefacefontprovider_register_typeface2 := Addr(_sk4d_typefacefontprovider_register_typeface2);


 { modules/skresources/include/sk4d_resources.h }

  sk4d_resourceproviderbaseclass_create := Addr(_sk4d_resourceproviderbaseclass_create);
  sk4d_resourceproviderbaseclass_set_procs := Addr(_sk4d_resourceproviderbaseclass_set_procs);


 { modules/skshaper/include/sk4d_shaper.h }

  sk4d_shaper_create := Addr(_sk4d_shaper_create);
  sk4d_shaper_destroy := Addr(_sk4d_shaper_destroy);
  sk4d_shaper_shape := Addr(_sk4d_shaper_shape);


 { modules/skunicode/include/sk4d_unicode.h }

  sk4d_unicode_create := Addr(_sk4d_unicode_create);
  sk4d_unicode_destroy := Addr(_sk4d_unicode_destroy);
  sk4d_unicode_for_each_bidi_region := Addr(_sk4d_unicode_for_each_bidi_region);
  sk4d_unicode_for_each_break := Addr(_sk4d_unicode_for_each_break);
  sk4d_unicode_for_each_codepoint := Addr(_sk4d_unicode_for_each_codepoint);
  sk4d_unicodebreakiterator_create := Addr(_sk4d_unicodebreakiterator_create);
  sk4d_unicodebreakiterator_create2 := Addr(_sk4d_unicodebreakiterator_create2);
  sk4d_unicodebreakiterator_destroy := Addr(_sk4d_unicodebreakiterator_destroy);
  sk4d_unicodebreakiterator_next := Addr(_sk4d_unicodebreakiterator_next);


 { modules/svg/include/sk4d_svgdom.h }

  sk4d_svgdom_find_node_by_id := Addr(_sk4d_svgdom_find_node_by_id);
  sk4d_svgdom_get_root := Addr(_sk4d_svgdom_get_root);
  sk4d_svgdom_make_from_file := Addr(_sk4d_svgdom_make_from_file);
  sk4d_svgdom_make_from_stream := Addr(_sk4d_svgdom_make_from_stream);
  sk4d_svgdom_render := Addr(_sk4d_svgdom_render);
  sk4d_svgdom_set_container_size := Addr(_sk4d_svgdom_set_container_size);


 { modules/svg/include/sk4d_svgnode.h }

  sk4d_svgnode_set_attribute := Addr(_sk4d_svgnode_set_attribute);


 { modules/svg/include/sk4d_svgsvg.h }

  sk4d_svgsvg_get_height := Addr(_sk4d_svgsvg_get_height);
  sk4d_svgsvg_get_intrinsic_size := Addr(_sk4d_svgsvg_get_intrinsic_size);
  sk4d_svgsvg_get_preserve_aspect_ratio := Addr(_sk4d_svgsvg_get_preserve_aspect_ratio);
  sk4d_svgsvg_get_view_box := Addr(_sk4d_svgsvg_get_view_box);
  sk4d_svgsvg_get_width := Addr(_sk4d_svgsvg_get_width);
  sk4d_svgsvg_get_x := Addr(_sk4d_svgsvg_get_x);
  sk4d_svgsvg_get_y := Addr(_sk4d_svgsvg_get_y);
  sk4d_svgsvg_set_height := Addr(_sk4d_svgsvg_set_height);
  sk4d_svgsvg_set_preserve_aspect_ratio := Addr(_sk4d_svgsvg_set_preserve_aspect_ratio);
  sk4d_svgsvg_set_view_box := Addr(_sk4d_svgsvg_set_view_box);
  sk4d_svgsvg_set_width := Addr(_sk4d_svgsvg_set_width);
  sk4d_svgsvg_set_x := Addr(_sk4d_svgsvg_set_x);
  sk4d_svgsvg_set_y := Addr(_sk4d_svgsvg_set_y);
end;

procedure SkFinalize;
begin
  AtomicDecrement(InitCount);
end;

{$ELSE WORKAROUND_RS123846}
{$ENDREGION}

{$IFNDEF SK_STATIC_LIBRARY}
var
  {$IFNDEF FPC}[Volatile]{$ENDIF} InitCount: Integer;
  LibraryHandle: HMODULE;
{$ENDIF}

{$IF DEFINED(SK_DEBUG) and DEFINED(MSWINDOWS)}
function GetProcAddress(AModule: HMODULE; AProcName: PChar): Pointer;
begin
  Result := Winapi.Windows.GetProcAddress(AModule, AProcName);
  if Result = nil then
    raise Exception.CreateFmt('Function "%s" not found in Skia library.', [AProcName]);
end;
{$ENDIF}

{$IFNDEF SK_STATIC_LIBRARY}
procedure SkInitialize;
begin
  if AtomicIncrement(InitCount) <> 1 then
    Exit;
  {$IF DEFINED(ANDROID) and NOT DEFINED(FPC)}
  // Some Android devices, normally old, need the full path of the library,
  // and other devices, normally new, do not accept the full path.
  LibraryHandle := SafeLoadLibrary(LibraryName);
  if LibraryHandle = 0 then
    LibraryHandle := SafeLoadLibrary(TPath.Combine(TPath.GetLibraryPath, LibraryName));
  {$ELSE}
  LibraryHandle := SafeLoadLibrary(LibraryName);
  {$ENDIF}
  if LibraryHandle = 0 then
    Abort;
  {$IFDEF MSWINDOWS}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  {$ENDIF}
{$ENDIF}

{ include/c/gr4d_backendsemaphore.h }

{$IFNDEF SK_STATIC_LIBRARY}
  gr4d_backendsemaphore_create      := GetProcAddress(LibraryHandle, PChar('gr4d_backendsemaphore_create'));
  gr4d_backendsemaphore_destroy     := GetProcAddress(LibraryHandle, PChar('gr4d_backendsemaphore_destroy'));
  gr4d_backendsemaphore_init_vulkan := GetProcAddress(LibraryHandle, PChar('gr4d_backendsemaphore_init_vulkan'));
{$ELSE}
function  gr4d_backendsemaphore_create(): gr_backendsemaphore_t; cdecl;                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure gr4d_backendsemaphore_destroy(self: gr_backendsemaphore_t); cdecl;                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure gr4d_backendsemaphore_init_vulkan(self: gr_backendsemaphore_t; semaphore: gr_vk_semaphore_t); cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/gr4d_backendsurface.h }

{$IFNDEF SK_STATIC_LIBRARY}
  gr4d_backendrendertarget_create_gl          := GetProcAddress(LibraryHandle, PChar('gr4d_backendrendertarget_create_gl'));
  gr4d_backendrendertarget_create_mtl         := GetProcAddress(LibraryHandle, PChar('gr4d_backendrendertarget_create_mtl'));
  gr4d_backendrendertarget_create_vk          := GetProcAddress(LibraryHandle, PChar('gr4d_backendrendertarget_create_vk'));
  gr4d_backendrendertarget_destroy            := GetProcAddress(LibraryHandle, PChar('gr4d_backendrendertarget_destroy'));
  gr4d_backendrendertarget_get_backend_api    := GetProcAddress(LibraryHandle, PChar('gr4d_backendrendertarget_get_backend_api'));
  gr4d_backendrendertarget_get_height         := GetProcAddress(LibraryHandle, PChar('gr4d_backendrendertarget_get_height'));
  gr4d_backendrendertarget_get_sample_count   := GetProcAddress(LibraryHandle, PChar('gr4d_backendrendertarget_get_sample_count'));
  gr4d_backendrendertarget_get_stencil_bits   := GetProcAddress(LibraryHandle, PChar('gr4d_backendrendertarget_get_stencil_bits'));
  gr4d_backendrendertarget_get_width          := GetProcAddress(LibraryHandle, PChar('gr4d_backendrendertarget_get_width'));
  gr4d_backendrendertarget_is_valid           := GetProcAddress(LibraryHandle, PChar('gr4d_backendrendertarget_is_valid'));
  gr4d_backendtexture_create_gl               := GetProcAddress(LibraryHandle, PChar('gr4d_backendtexture_create_gl'));
  gr4d_backendtexture_create_mtl              := GetProcAddress(LibraryHandle, PChar('gr4d_backendtexture_create_mtl'));
  gr4d_backendtexture_create_vk               := GetProcAddress(LibraryHandle, PChar('gr4d_backendtexture_create_vk'));
  gr4d_backendtexture_destroy                 := GetProcAddress(LibraryHandle, PChar('gr4d_backendtexture_destroy'));
  gr4d_backendtexture_get_backend_api         := GetProcAddress(LibraryHandle, PChar('gr4d_backendtexture_get_backend_api'));
  gr4d_backendtexture_get_gl_framebuffer_info := GetProcAddress(LibraryHandle, PChar('gr4d_backendtexture_get_gl_framebuffer_info'));
  gr4d_backendtexture_get_height              := GetProcAddress(LibraryHandle, PChar('gr4d_backendtexture_get_height'));
  gr4d_backendtexture_get_width               := GetProcAddress(LibraryHandle, PChar('gr4d_backendtexture_get_width'));
  gr4d_backendtexture_has_mipmaps             := GetProcAddress(LibraryHandle, PChar('gr4d_backendtexture_has_mipmaps'));
  gr4d_backendtexture_is_valid                := GetProcAddress(LibraryHandle, PChar('gr4d_backendtexture_is_valid'));
{$ELSE}
function  gr4d_backendrendertarget_create_gl(width, height, sample_count, stencil_bits: int32_t; const framebuffer_info: pgr_gl_framebufferinfo_t): gr_backendrendertarget_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_backendrendertarget_create_mtl(width, height: int32_t; const texture_info: pgr_mtl_textureinfo_t): gr_backendrendertarget_t; cdecl;                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_backendrendertarget_create_vk(width, height: int32_t; const image_info: pgr_vk_imageinfo_t): gr_backendrendertarget_t; cdecl;                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure gr4d_backendrendertarget_destroy(self: gr_backendrendertarget_t); cdecl;                                                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_backendrendertarget_get_backend_api(const self: gr_backendrendertarget_t): gr_backendapi_t; cdecl;                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_backendrendertarget_get_height(const self: gr_backendrendertarget_t): int32_t; cdecl;                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_backendrendertarget_get_sample_count(const self: gr_backendrendertarget_t): int32_t; cdecl;                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_backendrendertarget_get_stencil_bits(const self: gr_backendrendertarget_t): int32_t; cdecl;                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_backendrendertarget_get_width(const self: gr_backendrendertarget_t): int32_t; cdecl;                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_backendrendertarget_is_valid(const self: gr_backendrendertarget_t): _bool; cdecl;                                                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_backendtexture_create_gl(width, height: int32_t; is_mipmapped: _bool; const texture_info: pgr_gl_textureinfo_t): gr_backendtexture_t; cdecl;                          external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_backendtexture_create_mtl(width, height: int32_t; is_mipmapped: _bool; const texture_info: pgr_mtl_textureinfo_t): gr_backendtexture_t; cdecl;                        external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_backendtexture_create_vk(width, height: int32_t; const image_info: pgr_vk_imageinfo_t): gr_backendtexture_t; cdecl;                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure gr4d_backendtexture_destroy(self: gr_backendtexture_t); cdecl;                                                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_backendtexture_get_backend_api(const self: gr_backendtexture_t): gr_backendapi_t; cdecl;                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_backendtexture_get_gl_framebuffer_info(const self: gr_backendtexture_t; out texture_info: gr_gl_textureinfo_t): _bool; cdecl;                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_backendtexture_get_height(const self: gr_backendtexture_t): int32_t; cdecl;                                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_backendtexture_get_width(const self: gr_backendtexture_t): int32_t; cdecl;                                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_backendtexture_has_mipmaps(const self: gr_backendtexture_t): _bool; cdecl;                                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_backendtexture_is_valid(const self: gr_backendtexture_t): _bool; cdecl;                                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/gr4d_backendsurfacemutablestate.h }

{$IFNDEF SK_STATIC_LIBRARY}
  gr4d_backendsurfacemutablestate_create  := GetProcAddress(LibraryHandle, PChar('gr4d_backendsurfacemutablestate_create'));
  gr4d_backendsurfacemutablestate_destroy := GetProcAddress(LibraryHandle, PChar('gr4d_backendsurfacemutablestate_destroy'));
{$ELSE}
function  gr4d_backendsurfacemutablestate_create(image_layout: gr_vk_imagelayout_t; queue_family_index: uint32_t): gr_backendsurfacemutablestate_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure gr4d_backendsurfacemutablestate_destroy(self: gr_backendsurfacemutablestate_t); cdecl;                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/gr4d_contextoptions.h }

{$IFNDEF SK_STATIC_LIBRARY}
gr4d_persistentcachebaseclass_create    := GetProcAddress(LibraryHandle, PChar('gr4d_persistentcachebaseclass_create'));
gr4d_persistentcachebaseclass_destroy   := GetProcAddress(LibraryHandle, PChar('gr4d_persistentcachebaseclass_destroy'));
gr4d_persistentcachebaseclass_set_procs := GetProcAddress(LibraryHandle, PChar('gr4d_persistentcachebaseclass_set_procs'));
{$ELSE}
function  gr4d_persistentcachebaseclass_create(context: Pointer): gr_persistentcachebaseclass_t; cdecl;      external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure gr4d_persistentcachebaseclass_destroy(self: gr_persistentcachebaseclass_t); cdecl;                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure gr4d_persistentcachebaseclass_set_procs(const procs: pgr_persistentcachebaseclass_procs_t); cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/gr4d_directcontext.h }

{$IFNDEF SK_STATIC_LIBRARY}
  gr4d_directcontext_abandon_context                             := GetProcAddress(LibraryHandle, PChar('gr4d_directcontext_abandon_context'));
  gr4d_directcontext_create_texture                              := GetProcAddress(LibraryHandle, PChar('gr4d_directcontext_create_texture'));
  gr4d_directcontext_create_texture2                             := GetProcAddress(LibraryHandle, PChar('gr4d_directcontext_create_texture2'));
  gr4d_directcontext_create_texture3                             := GetProcAddress(LibraryHandle, PChar('gr4d_directcontext_create_texture3'));
  gr4d_directcontext_delete_texture                              := GetProcAddress(LibraryHandle, PChar('gr4d_directcontext_delete_texture'));
  gr4d_directcontext_dump_memory_statistics                      := GetProcAddress(LibraryHandle, PChar('gr4d_directcontext_dump_memory_statistics'));
  gr4d_directcontext_flush                                       := GetProcAddress(LibraryHandle, PChar('gr4d_directcontext_flush'));
  gr4d_directcontext_flush_and_submit                            := GetProcAddress(LibraryHandle, PChar('gr4d_directcontext_flush_and_submit'));
  gr4d_directcontext_free_gpu_resources                          := GetProcAddress(LibraryHandle, PChar('gr4d_directcontext_free_gpu_resources'));
  gr4d_directcontext_get_backend_api                             := GetProcAddress(LibraryHandle, PChar('gr4d_directcontext_get_backend_api'));
  gr4d_directcontext_get_max_surface_sample_count_for_color_type := GetProcAddress(LibraryHandle, PChar('gr4d_directcontext_get_max_surface_sample_count_for_color_type'));
  gr4d_directcontext_get_resource_cache_limit                    := GetProcAddress(LibraryHandle, PChar('gr4d_directcontext_get_resource_cache_limit'));
  gr4d_directcontext_get_resource_cache_usage                    := GetProcAddress(LibraryHandle, PChar('gr4d_directcontext_get_resource_cache_usage'));
  gr4d_directcontext_is_abandoned                                := GetProcAddress(LibraryHandle, PChar('gr4d_directcontext_is_abandoned'));
  gr4d_directcontext_make_gl                                     := GetProcAddress(LibraryHandle, PChar('gr4d_directcontext_make_gl'));
  gr4d_directcontext_make_metal                                  := GetProcAddress(LibraryHandle, PChar('gr4d_directcontext_make_metal'));
  gr4d_directcontext_make_vulkan                                 := GetProcAddress(LibraryHandle, PChar('gr4d_directcontext_make_vulkan'));
  gr4d_directcontext_perform_deferred_cleanup                    := GetProcAddress(LibraryHandle, PChar('gr4d_directcontext_perform_deferred_cleanup'));
  gr4d_directcontext_purge_unlocked_resources                    := GetProcAddress(LibraryHandle, PChar('gr4d_directcontext_purge_unlocked_resources'));
  gr4d_directcontext_purge_unlocked_resources2                   := GetProcAddress(LibraryHandle, PChar('gr4d_directcontext_purge_unlocked_resources2'));
  gr4d_directcontext_release_resources_and_abandon_context       := GetProcAddress(LibraryHandle, PChar('gr4d_directcontext_release_resources_and_abandon_context'));
  gr4d_directcontext_reset_context                               := GetProcAddress(LibraryHandle, PChar('gr4d_directcontext_reset_context'));
  gr4d_directcontext_set_resource_cache_limit                    := GetProcAddress(LibraryHandle, PChar('gr4d_directcontext_set_resource_cache_limit'));
  gr4d_directcontext_submit                                      := GetProcAddress(LibraryHandle, PChar('gr4d_directcontext_submit'));
{$ELSE}
procedure gr4d_directcontext_abandon_context(self: gr_directcontext_t); cdecl;                                                                                                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_directcontext_create_texture(self: gr_directcontext_t; width, height: int32_t; color_type: sk_colortype_t; is_mipmapped, is_renderable, is_protected: _bool): gr_backendtexture_t; cdecl;                              external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_directcontext_create_texture2(self: gr_directcontext_t; width, height: int32_t; color_type: sk_colortype_t; color: sk_color_t; is_mipmapped, is_renderable, is_protected: _bool): gr_backendtexture_t; cdecl;          external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_directcontext_create_texture3(self: gr_directcontext_t; width, height: int32_t; color_type: sk_colortype_t; const color: psk_color4f_t; is_mipmapped, is_renderable, is_protected: _bool): gr_backendtexture_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure gr4d_directcontext_delete_texture(self: gr_directcontext_t; texture: gr_backendtexture_t); cdecl;                                                                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure gr4d_directcontext_dump_memory_statistics(const self: gr_directcontext_t; trace_memory_dump: sk_tracememorydump_t); cdecl;                                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure gr4d_directcontext_flush(self: gr_directcontext_t); cdecl;                                                                                                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure gr4d_directcontext_flush_and_submit(self: gr_directcontext_t; sync_cpu: _bool); cdecl;                                                                                                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure gr4d_directcontext_free_gpu_resources(self: gr_directcontext_t); cdecl;                                                                                                                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_directcontext_get_backend_api(const self: gr_directcontext_t): gr_backendapi_t; cdecl;                                                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_directcontext_get_max_surface_sample_count_for_color_type(const self: gr_directcontext_t; color_type: sk_colortype_t): int32_t; cdecl;                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_directcontext_get_resource_cache_limit(const self: gr_directcontext_t): size_t; cdecl;                                                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure gr4d_directcontext_get_resource_cache_usage(const self: gr_directcontext_t; out resources: int32_t; out resources_bytes: size_t); cdecl;                                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_directcontext_is_abandoned(self: gr_directcontext_t): _bool; cdecl;                                                                                                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_directcontext_make_gl(const gl_interface: gr_gl_interface_t; const options: pgr_contextoptions_t): gr_directcontext_t; cdecl;                                                                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_directcontext_make_metal(const backend_context: pgr_mtl_backendcontext_t; const options: pgr_contextoptions_t): gr_directcontext_t; cdecl;                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_directcontext_make_vulkan(const backend_context: pgr_vk_backendcontext_t; const options: pgr_contextoptions_t): gr_directcontext_t; cdecl;                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure gr4d_directcontext_perform_deferred_cleanup(self: gr_directcontext_t; milliseconds: int64_t); cdecl;                                                                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure gr4d_directcontext_purge_unlocked_resources(self: gr_directcontext_t; scratch_resources_only: _bool); cdecl;                                                                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure gr4d_directcontext_purge_unlocked_resources2(self: gr_directcontext_t; bytes_to_purge: size_t; prefer_scratch_resources: _bool); cdecl;                                                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure gr4d_directcontext_release_resources_and_abandon_context(self: gr_directcontext_t); cdecl;                                                                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure gr4d_directcontext_reset_context(self: gr_directcontext_t); cdecl;                                                                                                                                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure gr4d_directcontext_set_resource_cache_limit(self: gr_directcontext_t; value: size_t); cdecl;                                                                                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_directcontext_submit(self: gr_directcontext_t; sync_cpu: _bool): _bool; cdecl;                                                                                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/gr4d_gl_interface.h }

{$IFNDEF SK_STATIC_LIBRARY}
  gr4d_gl_interface_has_extension        := GetProcAddress(LibraryHandle, PChar('gr4d_gl_interface_has_extension'));
  gr4d_gl_interface_make_assembled       := GetProcAddress(LibraryHandle, PChar('gr4d_gl_interface_make_assembled'));
  gr4d_gl_interface_make_assembled_gl    := GetProcAddress(LibraryHandle, PChar('gr4d_gl_interface_make_assembled_gl'));
  gr4d_gl_interface_make_assembled_gles  := GetProcAddress(LibraryHandle, PChar('gr4d_gl_interface_make_assembled_gles'));
  gr4d_gl_interface_make_assembled_webgl := GetProcAddress(LibraryHandle, PChar('gr4d_gl_interface_make_assembled_webgl'));
  gr4d_gl_interface_make_native          := GetProcAddress(LibraryHandle, PChar('gr4d_gl_interface_make_native'));
  gr4d_gl_interface_validate             := GetProcAddress(LibraryHandle, PChar('gr4d_gl_interface_validate'));
{$ELSE}
function gr4d_gl_interface_has_extension(const self: gr_gl_interface_t; const name: MarshaledAString): _bool; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
function gr4d_gl_interface_make_assembled(context: Pointer; proc: gr_gl_get_proc): gr_gl_interface_t; cdecl;         external{$IFNDEF FPC} LibraryName{$ENDIF};
function gr4d_gl_interface_make_assembled_gl(context: Pointer; proc: gr_gl_get_proc): gr_gl_interface_t; cdecl;      external{$IFNDEF FPC} LibraryName{$ENDIF};
function gr4d_gl_interface_make_assembled_gles(context: Pointer; proc: gr_gl_get_proc): gr_gl_interface_t; cdecl;    external{$IFNDEF FPC} LibraryName{$ENDIF};
function gr4d_gl_interface_make_assembled_webgl(context: Pointer; proc: gr_gl_get_proc): gr_gl_interface_t; cdecl;   external{$IFNDEF FPC} LibraryName{$ENDIF};
function gr4d_gl_interface_make_native(): gr_gl_interface_t; cdecl;                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function gr4d_gl_interface_validate(const self: gr_gl_interface_t): _bool; cdecl;                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/gr4d_shadererrorhandler.h }

{$IFNDEF SK_STATIC_LIBRARY}
gr4d_shadererrorhandlerbaseclass_create    := GetProcAddress(LibraryHandle, PChar('gr4d_shadererrorhandlerbaseclass_create'));
gr4d_shadererrorhandlerbaseclass_destroy   := GetProcAddress(LibraryHandle, PChar('gr4d_shadererrorhandlerbaseclass_destroy'));
gr4d_shadererrorhandlerbaseclass_set_procs := GetProcAddress(LibraryHandle, PChar('gr4d_shadererrorhandlerbaseclass_set_procs'));
{$ELSE}
function  gr4d_shadererrorhandlerbaseclass_create(context: Pointer): gr_shadererrorhandlerbaseclass_t; cdecl;      external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure gr4d_shadererrorhandlerbaseclass_destroy(self: gr_shadererrorhandlerbaseclass_t); cdecl;                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure gr4d_shadererrorhandlerbaseclass_set_procs(const procs: pgr_shadererrorhandlerbaseclass_procs_t); cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/gr4d_vk_extensions.h }

{$IFNDEF SK_STATIC_LIBRARY}
gr4d_vk_extensions_create        := GetProcAddress(LibraryHandle, PChar('gr4d_vk_extensions_create'));
gr4d_vk_extensions_destroy       := GetProcAddress(LibraryHandle, PChar('gr4d_vk_extensions_destroy'));
gr4d_vk_extensions_has_extension := GetProcAddress(LibraryHandle, PChar('gr4d_vk_extensions_has_extension'));
gr4d_vk_extensions_init          := GetProcAddress(LibraryHandle, PChar('gr4d_vk_extensions_init'));
{$ELSE}
function  gr4d_vk_extensions_create(): gr_vk_extensions_t; cdecl;                                                                                                                                                                                                                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure gr4d_vk_extensions_destroy(self: gr_vk_extensions_t); cdecl;                                                                                                                                                                                                                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function  gr4d_vk_extensions_has_extension(const self: gr_vk_extensions_t; const name: MarshaledAString; min_api_version: uint32_t): _bool; cdecl;                                                                                                                                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure gr4d_vk_extensions_init(self: gr_vk_extensions_t; context: Pointer; proc: gr_vk_get_proc; instance: gr_vk_instance_t; physical_device: gr_vk_physicaldevice_t; instance_extension_count: int32_t; const instance_extensions: PMarshaledAString; device_extension_count: int32_t; const device_extensions: PMarshaledAString); cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_animatedwebpencoder.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_animatedwebpencoder_encode_to_file   := GetProcAddress(LibraryHandle, PChar('sk4d_animatedwebpencoder_encode_to_file'));
  sk4d_animatedwebpencoder_encode_to_stream := GetProcAddress(LibraryHandle, PChar('sk4d_animatedwebpencoder_encode_to_stream'));
{$ELSE}
function sk4d_animatedwebpencoder_encode_to_file(const file_name: MarshaledAString; const src: psk_frame_t; count: size_t; quality: int32_t): _bool; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_animatedwebpencoder_encode_to_stream(w_stream: sk_wstream_t; const src: psk_frame_t; count: size_t; quality: int32_t): _bool; cdecl;          external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_blender.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_blender_make_arithmetic := GetProcAddress(LibraryHandle, PChar('sk4d_blender_make_arithmetic'));
  sk4d_blender_make_mode       := GetProcAddress(LibraryHandle, PChar('sk4d_blender_make_mode'));
{$ELSE}
function  sk4d_blender_make_arithmetic(k1, k2, k3, k4: float; enforce_premultiplied_color: _bool): sk_blender_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_blender_make_mode(mode: sk_blendmode_t): sk_blender_t; cdecl;                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_canvas.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_canvas_clear                      := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_clear'));
  sk4d_canvas_clear2                     := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_clear2'));
  sk4d_canvas_destroy                    := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_destroy'));
  sk4d_canvas_discard                    := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_discard'));
  sk4d_canvas_clip_path                  := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_clip_path'));
  sk4d_canvas_clip_rect                  := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_clip_rect'));
  sk4d_canvas_clip_region                := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_clip_region'));
  sk4d_canvas_clip_rrect                 := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_clip_rrect'));
  sk4d_canvas_clip_shader                := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_clip_shader'));
  sk4d_canvas_concat                     := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_concat'));
  sk4d_canvas_concat2                    := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_concat2'));
  sk4d_canvas_draw_annotation            := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_annotation'));
  sk4d_canvas_draw_arc                   := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_arc'));
  sk4d_canvas_draw_atlas                 := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_atlas'));
  sk4d_canvas_draw_circle                := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_circle'));
  sk4d_canvas_draw_color                 := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_color'));
  sk4d_canvas_draw_color2                := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_color2'));
  sk4d_canvas_draw_glyphs                := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_glyphs'));
  sk4d_canvas_draw_glyphs2               := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_glyphs2'));
  sk4d_canvas_draw_image                 := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_image'));
  sk4d_canvas_draw_image_lattice         := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_image_lattice'));
  sk4d_canvas_draw_image_nine            := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_image_nine'));
  sk4d_canvas_draw_image_rect            := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_image_rect'));
  sk4d_canvas_draw_line                  := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_line'));
  sk4d_canvas_draw_oval                  := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_oval'));
  sk4d_canvas_draw_paint                 := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_paint'));
  sk4d_canvas_draw_patch                 := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_patch'));
  sk4d_canvas_draw_path                  := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_path'));
  sk4d_canvas_draw_picture               := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_picture'));
  sk4d_canvas_draw_point                 := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_point'));
  sk4d_canvas_draw_points                := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_points'));
  sk4d_canvas_draw_rect                  := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_rect'));
  sk4d_canvas_draw_region                := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_region'));
  sk4d_canvas_draw_rrect                 := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_rrect'));
  sk4d_canvas_draw_rrect2                := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_rrect2'));
  sk4d_canvas_draw_rrect_difference      := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_rrect_difference'));
  sk4d_canvas_draw_simple_text           := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_simple_text'));
  sk4d_canvas_draw_text_blob             := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_text_blob'));
  sk4d_canvas_draw_vertices              := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_draw_vertices'));
  sk4d_canvas_get_base_props             := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_get_base_props'));
  sk4d_canvas_get_device_clip_bounds     := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_get_device_clip_bounds'));
  sk4d_canvas_get_local_clip_bounds      := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_get_local_clip_bounds'));
  sk4d_canvas_get_local_to_device        := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_get_local_to_device'));
  sk4d_canvas_get_local_to_device_as_3x3 := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_get_local_to_device_as_3x3'));
  sk4d_canvas_get_top_props              := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_get_top_props'));
  sk4d_canvas_get_save_count             := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_get_save_count'));
  sk4d_canvas_make_surface               := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_make_surface'));
  sk4d_canvas_quick_reject               := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_quick_reject'));
  sk4d_canvas_quick_reject2              := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_quick_reject2'));
  sk4d_canvas_reset_matrix               := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_reset_matrix'));
  sk4d_canvas_restore                    := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_restore'));
  sk4d_canvas_restore_to_count           := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_restore_to_count'));
  sk4d_canvas_rotate                     := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_rotate'));
  sk4d_canvas_rotate2                    := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_rotate2'));
  sk4d_canvas_save                       := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_save'));
  sk4d_canvas_save_layer                 := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_save_layer'));
  sk4d_canvas_save_layer_alpha           := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_save_layer_alpha'));
  sk4d_canvas_scale                      := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_scale'));
  sk4d_canvas_set_matrix                 := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_set_matrix'));
  sk4d_canvas_set_matrix2                := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_set_matrix2'));
  sk4d_canvas_skew                       := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_skew'));
  sk4d_canvas_translate                  := GetProcAddress(LibraryHandle, PChar('sk4d_canvas_translate'));
{$ELSE}
procedure sk4d_canvas_clear(self: sk_canvas_t; color: sk_color_t); cdecl;                                                                                                                                                                                                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_clear2(self: sk_canvas_t; const color: psk_color4f_t); cdecl;                                                                                                                                                                                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_destroy(self: sk_canvas_t); cdecl;                                                                                                                                                                                                                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_discard(self: sk_canvas_t); cdecl;                                                                                                                                                                                                                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_clip_path(self: sk_canvas_t; const path: sk_path_t; op: sk_clipop_t; anti_alias: _bool); cdecl;                                                                                                                                                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_clip_rect(self: sk_canvas_t; const rect: psk_rect_t; op: sk_clipop_t; anti_alias: _bool); cdecl;                                                                                                                                                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_clip_region(self: sk_canvas_t; const region: sk_region_t; op: sk_clipop_t); cdecl;                                                                                                                                                                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_clip_rrect(self: sk_canvas_t; const rrect: sk_rrect_t; op: sk_clipop_t; anti_alias: _bool); cdecl;                                                                                                                                                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_clip_shader(self: sk_canvas_t; shader: sk_shader_t; op: sk_clipop_t); cdecl;                                                                                                                                                                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_concat(self: sk_canvas_t; const matrix: psk_matrix44_t); cdecl;                                                                                                                                                                                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_concat2(self: sk_canvas_t; const matrix: psk_matrix_t); cdecl;                                                                                                                                                                                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_annotation(self: sk_canvas_t; const rect: psk_rect_t; const key: MarshaledAString; const value: Pointer; size: size_t); cdecl;                                                                                                                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_arc(self: sk_canvas_t; const oval: psk_rect_t; start_angle, sweep_angle: float; use_center: _bool; const paint: sk_paint_t); cdecl;                                                                                                                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_atlas(self: sk_canvas_t; const atlas: sk_image_t; const transforms: psk_rotationscalematrix_t; const sprites: psk_rect_t; const colors: psk_color_t; count: int32_t; blend_mode: sk_blendmode_t; const sampling: psk_samplingoptions_t; const cull_rect: psk_rect_t; const paint: sk_paint_t); cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_circle(self: sk_canvas_t; const center: psk_point_t; radius: float; const paint: sk_paint_t); cdecl;                                                                                                                                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_color(self: sk_canvas_t; color: sk_color_t; blend_mode: sk_blendmode_t); cdecl;                                                                                                                                                                                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_color2(self: sk_canvas_t; const color: psk_color4f_t; blend_mode: sk_blendmode_t); cdecl;                                                                                                                                                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_glyphs(self: sk_canvas_t; count: int32_t; const glyphs: psk_glyphid_t; const positions: psk_point_t; const origin: psk_point_t; const font: sk_font_t; const paint: sk_paint_t); cdecl;                                                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_glyphs2(self: sk_canvas_t; count: int32_t; const glyphs: psk_glyphid_t; const matrices: psk_rotationscalematrix_t; const origin: psk_point_t; const font: sk_font_t; const paint: sk_paint_t); cdecl;                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_image(self: sk_canvas_t; const image: sk_image_t; x, y: float; const sampling: psk_samplingoptions_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_image_lattice(self: sk_canvas_t; const image: sk_image_t; const lattice: psk_lattice_t; const dest: psk_rect_t; filter_mode: sk_filtermode_t; const paint: sk_paint_t); cdecl;                                                                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_image_nine(self: sk_canvas_t; const image: sk_image_t; const center: psk_irect_t; const dest: psk_rect_t; filter_mode: sk_filtermode_t; const paint: sk_paint_t); cdecl;                                                                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_image_rect(self: sk_canvas_t; const image: sk_image_t; const src, dest: psk_rect_t; const sampling: psk_samplingoptions_t; const paint: sk_paint_t; constraint: sk_srcrectconstraint_t); cdecl;                                                                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_line(self: sk_canvas_t; const point1, point2: psk_point_t; paint: sk_paint_t); cdecl;                                                                                                                                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_oval(self: sk_canvas_t; const oval: psk_rect_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_paint(self: sk_canvas_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_patch(self: sk_canvas_t; const cubics: psk_point_t; const colors: psk_color_t; const tex_coords: psk_point_t; blend_mode: sk_blendmode_t; const paint: sk_paint_t); cdecl;                                                                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_path(self: sk_canvas_t; const path: sk_path_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_picture(self: sk_canvas_t; const picture: sk_picture_t; const matrix: psk_matrix_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_point(self: sk_canvas_t; const point: psk_point_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_points(self: sk_canvas_t; mode: sk_drawpointsmode_t; count: size_t; const points: psk_point_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_rect(self: sk_canvas_t; const rect: psk_rect_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_region(self: sk_canvas_t; const region: sk_region_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_rrect(self: sk_canvas_t; const rrect: sk_rrect_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_rrect2(self: sk_canvas_t; const rect: psk_rect_t; radius_x, radius_y: float; const paint: sk_paint_t); cdecl;                                                                                                                                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_rrect_difference(self: sk_canvas_t; const outer, inner: sk_rrect_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_simple_text(self: sk_canvas_t; const text: Pointer; size: size_t; encoding: sk_textencoding_t; x, y: float; const font: sk_font_t; const paint: sk_paint_t); cdecl;                                                                                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_text_blob(self: sk_canvas_t; const text_blob: sk_textblob_t; x, y: float; const paint: sk_paint_t); cdecl;                                                                                                                                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_draw_vertices(self: sk_canvas_t; const vertices: sk_vertices_t; blend_mode: sk_blendmode_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_get_base_props(const self: sk_canvas_t; out result: sk_surfaceprops_t); cdecl;                                                                                                                                                                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_get_device_clip_bounds(const self: sk_canvas_t; out result: sk_irect_t); cdecl;                                                                                                                                                                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_get_local_clip_bounds(const self: sk_canvas_t; out result: sk_rect_t); cdecl;                                                                                                                                                                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_get_local_to_device(const self: sk_canvas_t; out result: sk_matrix44_t); cdecl;                                                                                                                                                                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_get_local_to_device_as_3x3(const self: sk_canvas_t; out result: sk_matrix_t); cdecl;                                                                                                                                                                                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_get_top_props(const self: sk_canvas_t; out result: sk_surfaceprops_t); cdecl;                                                                                                                                                                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_canvas_get_save_count(const self: sk_canvas_t): int32_t; cdecl;                                                                                                                                                                                                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_canvas_make_surface(self: sk_canvas_t; const image_info: psk_imageinfo_t; const props: psk_surfaceprops_t): sk_surface_t; cdecl;                                                                                                                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_canvas_quick_reject(const self: sk_canvas_t; const rect: psk_rect_t): _bool; cdecl;                                                                                                                                                                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_canvas_quick_reject2(const self: sk_canvas_t; const path: sk_path_t): _bool; cdecl;                                                                                                                                                                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_reset_matrix(self: sk_canvas_t); cdecl;                                                                                                                                                                                                                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_restore(self: sk_canvas_t); cdecl;                                                                                                                                                                                                                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_restore_to_count(self: sk_canvas_t; save_count: int32_t); cdecl;                                                                                                                                                                                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_rotate(self: sk_canvas_t; degrees: float); cdecl;                                                                                                                                                                                                                                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_rotate2(self: sk_canvas_t; degrees, px, py: float); cdecl;                                                                                                                                                                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_canvas_save(self: sk_canvas_t): int32_t; cdecl;                                                                                                                                                                                                                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_canvas_save_layer(self: sk_canvas_t; const bounds: psk_rect_t; const paint: sk_paint_t; const backdrop: sk_imagefilter_t; flags: uint32_t): int32_t; cdecl;                                                                                                                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_canvas_save_layer_alpha(self: sk_canvas_t; const bounds: psk_rect_t; alpha: uint8_t): int32_t; cdecl;                                                                                                                                                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_scale(self: sk_canvas_t; sx, sy: float); cdecl;                                                                                                                                                                                                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_set_matrix(self: sk_canvas_t; const matrix: psk_matrix44_t); cdecl;                                                                                                                                                                                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_set_matrix2(self: sk_canvas_t; const matrix: psk_matrix_t); cdecl;                                                                                                                                                                                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_skew(self: sk_canvas_t; kx, ky: float); cdecl;                                                                                                                                                                                                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_canvas_translate(self: sk_canvas_t; dx, dy: float); cdecl;                                                                                                                                                                                                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_codec.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_codec_destroy                    := GetProcAddress(LibraryHandle, PChar('sk4d_codec_destroy'));
  sk4d_codec_get_dimensions             := GetProcAddress(LibraryHandle, PChar('sk4d_codec_get_dimensions'));
  sk4d_codec_get_encoded_image_format   := GetProcAddress(LibraryHandle, PChar('sk4d_codec_get_encoded_image_format'));
  sk4d_codec_get_image                  := GetProcAddress(LibraryHandle, PChar('sk4d_codec_get_image'));
  sk4d_codec_get_pixels                 := GetProcAddress(LibraryHandle, PChar('sk4d_codec_get_pixels'));
  sk4d_codec_make_from_file             := GetProcAddress(LibraryHandle, PChar('sk4d_codec_make_from_file'));
  sk4d_codec_make_from_stream           := GetProcAddress(LibraryHandle, PChar('sk4d_codec_make_from_stream'));
  sk4d_codec_make_with_copy             := GetProcAddress(LibraryHandle, PChar('sk4d_codec_make_with_copy'));
  sk4d_codec_make_without_copy          := GetProcAddress(LibraryHandle, PChar('sk4d_codec_make_without_copy'));
  sk4d_animcodecplayer_destroy          := GetProcAddress(LibraryHandle, PChar('sk4d_animcodecplayer_destroy'));
  sk4d_animcodecplayer_get_dimensions   := GetProcAddress(LibraryHandle, PChar('sk4d_animcodecplayer_get_dimensions'));
  sk4d_animcodecplayer_get_duration     := GetProcAddress(LibraryHandle, PChar('sk4d_animcodecplayer_get_duration'));
  sk4d_animcodecplayer_get_frame        := GetProcAddress(LibraryHandle, PChar('sk4d_animcodecplayer_get_frame'));
  sk4d_animcodecplayer_make_from_file   := GetProcAddress(LibraryHandle, PChar('sk4d_animcodecplayer_make_from_file'));
  sk4d_animcodecplayer_make_from_stream := GetProcAddress(LibraryHandle, PChar('sk4d_animcodecplayer_make_from_stream'));
  sk4d_animcodecplayer_seek             := GetProcAddress(LibraryHandle, PChar('sk4d_animcodecplayer_seek'));
{$ELSE}
procedure sk4d_codec_destroy(codec: sk_codec_t); cdecl;                                                                                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_codec_get_dimensions(const self: sk_codec_t; out result: sk_isize_t); cdecl;                                                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_codec_get_encoded_image_format(const self: sk_codec_t): sk_encodedimageformat_t; cdecl;                                                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_codec_get_image(self: sk_codec_t; color_type: sk_colortype_t; alpha_type: sk_alphatype_t; color_space: sk_colorspace_t): sk_image_t; cdecl;                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_codec_get_pixels(self: sk_codec_t; pixels: Pointer; row_bytes: size_t; color_type: sk_colortype_t; alpha_type: sk_alphatype_t; color_space: sk_colorspace_t): _bool; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_codec_make_from_file(const file_name: MarshaledAString): sk_codec_t; cdecl;                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_codec_make_from_stream(stream: sk_stream_t): sk_codec_t; cdecl;                                                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_codec_make_with_copy(const data: Pointer; size: size_t): sk_codec_t; cdecl;                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_codec_make_without_copy(const data: Pointer; size: size_t): sk_codec_t; cdecl;                                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_animcodecplayer_destroy(self: sk_animcodecplayer_t); cdecl;                                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_animcodecplayer_get_dimensions(const self: sk_animcodecplayer_t; out result: sk_isize_t); cdecl;                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_animcodecplayer_get_duration(const self: sk_animcodecplayer_t): uint32_t; cdecl;                                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_animcodecplayer_get_frame(self: sk_animcodecplayer_t): sk_image_t; cdecl;                                                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_animcodecplayer_make_from_file(const file_name: MarshaledAString): sk_animcodecplayer_t; cdecl;                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_animcodecplayer_make_from_stream(stream: sk_stream_t): sk_animcodecplayer_t; cdecl;                                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_animcodecplayer_seek(self: sk_animcodecplayer_t; milliseconds: uint32_t): _bool; cdecl;                                                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_colorfilter.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_colorfilter_make_blend                := GetProcAddress(LibraryHandle, PChar('sk4d_colorfilter_make_blend'));
  sk4d_colorfilter_make_blend2               := GetProcAddress(LibraryHandle, PChar('sk4d_colorfilter_make_blend2'));
  sk4d_colorfilter_make_compose              := GetProcAddress(LibraryHandle, PChar('sk4d_colorfilter_make_compose'));
  sk4d_colorfilter_make_high_contrast        := GetProcAddress(LibraryHandle, PChar('sk4d_colorfilter_make_high_contrast'));
  sk4d_colorfilter_make_hsla_matrix          := GetProcAddress(LibraryHandle, PChar('sk4d_colorfilter_make_hsla_matrix'));
  sk4d_colorfilter_make_lighting             := GetProcAddress(LibraryHandle, PChar('sk4d_colorfilter_make_lighting'));
  sk4d_colorfilter_make_linear_to_srgb_gamma := GetProcAddress(LibraryHandle, PChar('sk4d_colorfilter_make_linear_to_srgb_gamma'));
  sk4d_colorfilter_make_luma_color           := GetProcAddress(LibraryHandle, PChar('sk4d_colorfilter_make_luma_color'));
  sk4d_colorfilter_make_matrix               := GetProcAddress(LibraryHandle, PChar('sk4d_colorfilter_make_matrix'));
  sk4d_colorfilter_make_overdraw             := GetProcAddress(LibraryHandle, PChar('sk4d_colorfilter_make_overdraw'));
  sk4d_colorfilter_make_table                := GetProcAddress(LibraryHandle, PChar('sk4d_colorfilter_make_table'));
{$ELSE}
function sk4d_colorfilter_make_blend(color: sk_color_t; mode: sk_blendmode_t): sk_colorfilter_t; cdecl;                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_colorfilter_make_blend2(const color: psk_color4f_t; color_space: sk_colorspace_t; mode: sk_blendmode_t): sk_colorfilter_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_colorfilter_make_compose(outer, inner: sk_colorfilter_t): sk_colorfilter_t; cdecl;                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_colorfilter_make_high_contrast(const config: psk_highcontrastconfig_t): sk_colorfilter_t; cdecl;                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_colorfilter_make_hsla_matrix(const matrix: psk_colormatrix_t): sk_colorfilter_t; cdecl;                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_colorfilter_make_lighting(multiply, add: sk_color_t): sk_colorfilter_t; cdecl;                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_colorfilter_make_linear_to_srgb_gamma(): sk_colorfilter_t; cdecl;                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_colorfilter_make_luma_color(): sk_colorfilter_t; cdecl;                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_colorfilter_make_matrix(const matrix: psk_colormatrix_t): sk_colorfilter_t; cdecl;                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_colorfilter_make_overdraw(const colors: psk_color_t): sk_colorfilter_t; cdecl;                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_colorfilter_make_table(const tablea_a, tablea_r, tablea_g, tablea_b: puint8_t): sk_colorfilter_t; cdecl;                          external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_colorspace.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_colorspace_gamma_close_to_srgb       := GetProcAddress(LibraryHandle, PChar('sk4d_colorspace_gamma_close_to_srgb'));
  sk4d_colorspace_gamma_is_linear           := GetProcAddress(LibraryHandle, PChar('sk4d_colorspace_gamma_is_linear'));
  sk4d_colorspace_is_equal                  := GetProcAddress(LibraryHandle, PChar('sk4d_colorspace_is_equal'));
  sk4d_colorspace_is_numerical_transfer_fn  := GetProcAddress(LibraryHandle, PChar('sk4d_colorspace_is_numerical_transfer_fn'));
  sk4d_colorspace_is_srgb                   := GetProcAddress(LibraryHandle, PChar('sk4d_colorspace_is_srgb'));
  sk4d_colorspace_make                      := GetProcAddress(LibraryHandle, PChar('sk4d_colorspace_make'));
  sk4d_colorspace_make_linear_gamma         := GetProcAddress(LibraryHandle, PChar('sk4d_colorspace_make_linear_gamma'));
  sk4d_colorspace_make_rgb                  := GetProcAddress(LibraryHandle, PChar('sk4d_colorspace_make_rgb'));
  sk4d_colorspace_make_srgb                 := GetProcAddress(LibraryHandle, PChar('sk4d_colorspace_make_srgb'));
  sk4d_colorspace_make_srgb_gamma           := GetProcAddress(LibraryHandle, PChar('sk4d_colorspace_make_srgb_gamma'));
  sk4d_colorspace_make_srgb_linear          := GetProcAddress(LibraryHandle, PChar('sk4d_colorspace_make_srgb_linear'));
  sk4d_colorspace_ref                       := GetProcAddress(LibraryHandle, PChar('sk4d_colorspace_ref'));
  sk4d_colorspace_to_profile                := GetProcAddress(LibraryHandle, PChar('sk4d_colorspace_to_profile'));
  sk4d_colorspace_to_xyz                    := GetProcAddress(LibraryHandle, PChar('sk4d_colorspace_to_xyz'));
  sk4d_colorspace_unref                     := GetProcAddress(LibraryHandle, PChar('sk4d_colorspace_unref'));
  sk4d_colorspaceiccprofile_destroy         := GetProcAddress(LibraryHandle, PChar('sk4d_colorspaceiccprofile_destroy'));
  sk4d_colorspaceiccprofile_get_buffer      := GetProcAddress(LibraryHandle, PChar('sk4d_colorspaceiccprofile_get_buffer'));
  sk4d_colorspaceiccprofile_make_with_parse := GetProcAddress(LibraryHandle, PChar('sk4d_colorspaceiccprofile_make_with_parse'));
  sk4d_colorspaceiccprofile_to_xyz          := GetProcAddress(LibraryHandle, PChar('sk4d_colorspaceiccprofile_to_xyz'));
  sk4d_colorspaceprimaries_to_xyz           := GetProcAddress(LibraryHandle, PChar('sk4d_colorspaceprimaries_to_xyz'));
  sk4d_colorspacetransferfn_invert          := GetProcAddress(LibraryHandle, PChar('sk4d_colorspacetransferfn_invert'));
  sk4d_colorspacetransferfn_transform       := GetProcAddress(LibraryHandle, PChar('sk4d_colorspacetransferfn_transform'));
{$ELSE}
function  sk4d_colorspace_gamma_close_to_srgb(const self: sk_colorspace_t): _bool; cdecl;                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_colorspace_gamma_is_linear(const self: sk_colorspace_t): _bool; cdecl;                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_colorspace_is_equal(const self, color_space: sk_colorspace_t): _bool; cdecl;                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_colorspace_is_numerical_transfer_fn(const self: sk_colorspace_t; out transfer_function: sk_colorspacetransferfn_t): _bool; cdecl;    external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_colorspace_is_srgb(const self: sk_colorspace_t): _bool; cdecl;                                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_colorspace_make(const profile: sk_colorspaceiccprofile_t): sk_colorspace_t; cdecl;                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_colorspace_make_linear_gamma(const self: sk_colorspace_t): sk_colorspace_t; cdecl;                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_colorspace_make_rgb(const transfer_function: psk_colorspacetransferfn_t; const xyz: psk_colorspacexyz_t): sk_colorspace_t; cdecl;    external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_colorspace_make_srgb(): sk_colorspace_t; cdecl;                                                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_colorspace_make_srgb_gamma(const self: sk_colorspace_t): sk_colorspace_t; cdecl;                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_colorspace_make_srgb_linear(): sk_colorspace_t; cdecl;                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_colorspace_ref(const self: sk_colorspace_t); cdecl;                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_colorspace_to_profile(const self: sk_colorspace_t): sk_colorspaceiccprofile_t; cdecl;                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_colorspace_to_xyz(const self: sk_colorspace_t; out xyz: sk_colorspacexyz_t): _bool; cdecl;                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_colorspace_unref(const self: sk_colorspace_t); cdecl;                                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_colorspaceiccprofile_destroy(self: sk_colorspaceiccprofile_t); cdecl;                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_colorspaceiccprofile_get_buffer(const self: sk_colorspaceiccprofile_t; size: puint32_t): puint8_t; cdecl;                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_colorspaceiccprofile_make_with_parse(const buffer: Pointer; size: size_t): sk_colorspaceiccprofile_t; cdecl;                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_colorspaceiccprofile_to_xyz(const self: sk_colorspaceiccprofile_t; out dest: sk_colorspacexyz_t): _bool; cdecl;                      external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_colorspaceprimaries_to_xyz(const self: psk_colorspaceprimaries_t; out xyz: sk_colorspacexyz_t): _bool; cdecl;                        external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_colorspacetransferfn_invert(const self: psk_colorspacetransferfn_t; out transfer_function: sk_colorspacetransferfn_t): _bool; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_colorspacetransferfn_transform(const self: psk_colorspacetransferfn_t; x: float): float; cdecl;                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_data.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_data_make_empty     := GetProcAddress(LibraryHandle, PChar('sk4d_data_make_empty'));
  sk4d_data_make_with_copy := GetProcAddress(LibraryHandle, PChar('sk4d_data_make_with_copy'));
  sk4d_data_ref            := GetProcAddress(LibraryHandle, PChar('sk4d_data_ref'));
  sk4d_data_unref          := GetProcAddress(LibraryHandle, PChar('sk4d_data_unref'));
{$ELSE}
function  sk4d_data_make_empty(): sk_data_t; cdecl;                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_data_make_with_copy(const data: Pointer; size: size_t): sk_data_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_data_ref(const self: sk_data_t); cdecl;                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_data_unref(const self: sk_data_t); cdecl;                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_document.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_document_begin_page := GetProcAddress(LibraryHandle, PChar('sk4d_document_begin_page'));
  sk4d_document_close      := GetProcAddress(LibraryHandle, PChar('sk4d_document_close'));
  sk4d_document_end_page   := GetProcAddress(LibraryHandle, PChar('sk4d_document_end_page'));
  sk4d_document_make_pdf   := GetProcAddress(LibraryHandle, PChar('sk4d_document_make_pdf'));
  sk4d_document_make_pdf2  := GetProcAddress(LibraryHandle, PChar('sk4d_document_make_pdf2'));
  sk4d_document_make_xps   := GetProcAddress(LibraryHandle, PChar('sk4d_document_make_xps'));
  sk4d_document_terminate  := GetProcAddress(LibraryHandle, PChar('sk4d_document_terminate'));
{$ELSE}
function  sk4d_document_begin_page(self: sk_document_t; width, height: float; const content: psk_rect_t): sk_canvas_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_document_close(self: sk_document_t); cdecl;                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_document_end_page(self: sk_document_t); cdecl;                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_document_make_pdf(w_stream: sk_wstream_t): sk_document_t; cdecl;                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_document_make_pdf2(w_stream: sk_wstream_t; const metadata: psk_pdfmetadata_t): sk_document_t; cdecl;           external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_document_make_xps(w_stream: sk_wstream_t; dpi: float): sk_document_t; cdecl;                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_document_terminate(self: sk_document_t); cdecl;                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_font.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_font_create                   := GetProcAddress(LibraryHandle, PChar('sk4d_font_create'));
  sk4d_font_create2                  := GetProcAddress(LibraryHandle, PChar('sk4d_font_create2'));
  sk4d_font_destroy                  := GetProcAddress(LibraryHandle, PChar('sk4d_font_destroy'));
  sk4d_font_get_baseline_snap        := GetProcAddress(LibraryHandle, PChar('sk4d_font_get_baseline_snap'));
  sk4d_font_get_edging               := GetProcAddress(LibraryHandle, PChar('sk4d_font_get_edging'));
  sk4d_font_get_embedded_bitmaps     := GetProcAddress(LibraryHandle, PChar('sk4d_font_get_embedded_bitmaps'));
  sk4d_font_get_embolden             := GetProcAddress(LibraryHandle, PChar('sk4d_font_get_embolden'));
  sk4d_font_get_force_auto_hinting   := GetProcAddress(LibraryHandle, PChar('sk4d_font_get_force_auto_hinting'));
  sk4d_font_get_glyphs               := GetProcAddress(LibraryHandle, PChar('sk4d_font_get_glyphs'));
  sk4d_font_get_glyphs_count         := GetProcAddress(LibraryHandle, PChar('sk4d_font_get_glyphs_count'));
  sk4d_font_get_hinting              := GetProcAddress(LibraryHandle, PChar('sk4d_font_get_hinting'));
  sk4d_font_get_horizontal_positions := GetProcAddress(LibraryHandle, PChar('sk4d_font_get_horizontal_positions'));
  sk4d_font_get_intercepts           := GetProcAddress(LibraryHandle, PChar('sk4d_font_get_intercepts'));
  sk4d_font_get_linear_metrics       := GetProcAddress(LibraryHandle, PChar('sk4d_font_get_linear_metrics'));
  sk4d_font_get_metrics              := GetProcAddress(LibraryHandle, PChar('sk4d_font_get_metrics'));
  sk4d_font_get_path                 := GetProcAddress(LibraryHandle, PChar('sk4d_font_get_path'));
  sk4d_font_get_paths                := GetProcAddress(LibraryHandle, PChar('sk4d_font_get_paths'));
  sk4d_font_get_positions            := GetProcAddress(LibraryHandle, PChar('sk4d_font_get_positions'));
  sk4d_font_get_scale_x              := GetProcAddress(LibraryHandle, PChar('sk4d_font_get_scale_x'));
  sk4d_font_get_size                 := GetProcAddress(LibraryHandle, PChar('sk4d_font_get_size'));
  sk4d_font_get_skew_x               := GetProcAddress(LibraryHandle, PChar('sk4d_font_get_skew_x'));
  sk4d_font_get_subpixel             := GetProcAddress(LibraryHandle, PChar('sk4d_font_get_subpixel'));
  sk4d_font_get_typeface             := GetProcAddress(LibraryHandle, PChar('sk4d_font_get_typeface'));
  sk4d_font_get_typeface_or_default  := GetProcAddress(LibraryHandle, PChar('sk4d_font_get_typeface_or_default'));
  sk4d_font_get_widths_bounds        := GetProcAddress(LibraryHandle, PChar('sk4d_font_get_widths_bounds'));
  sk4d_font_is_equal                 := GetProcAddress(LibraryHandle, PChar('sk4d_font_is_equal'));
  sk4d_font_measure_text             := GetProcAddress(LibraryHandle, PChar('sk4d_font_measure_text'));
  sk4d_font_set_baseline_snap        := GetProcAddress(LibraryHandle, PChar('sk4d_font_set_baseline_snap'));
  sk4d_font_set_edging               := GetProcAddress(LibraryHandle, PChar('sk4d_font_set_edging'));
  sk4d_font_set_embedded_bitmaps     := GetProcAddress(LibraryHandle, PChar('sk4d_font_set_embedded_bitmaps'));
  sk4d_font_set_embolden             := GetProcAddress(LibraryHandle, PChar('sk4d_font_set_embolden'));
  sk4d_font_set_force_auto_hinting   := GetProcAddress(LibraryHandle, PChar('sk4d_font_set_force_auto_hinting'));
  sk4d_font_set_hinting              := GetProcAddress(LibraryHandle, PChar('sk4d_font_set_hinting'));
  sk4d_font_set_linear_metrics       := GetProcAddress(LibraryHandle, PChar('sk4d_font_set_linear_metrics'));
  sk4d_font_set_scale_x              := GetProcAddress(LibraryHandle, PChar('sk4d_font_set_scale_x'));
  sk4d_font_set_size                 := GetProcAddress(LibraryHandle, PChar('sk4d_font_set_size'));
  sk4d_font_set_skew_x               := GetProcAddress(LibraryHandle, PChar('sk4d_font_set_skew_x'));
  sk4d_font_set_subpixel             := GetProcAddress(LibraryHandle, PChar('sk4d_font_set_subpixel'));
  sk4d_font_set_typeface             := GetProcAddress(LibraryHandle, PChar('sk4d_font_set_typeface'));
  sk4d_font_unichar_to_glyph         := GetProcAddress(LibraryHandle, PChar('sk4d_font_unichar_to_glyph'));
  sk4d_font_unichars_to_glyphs       := GetProcAddress(LibraryHandle, PChar('sk4d_font_unichars_to_glyphs'));
{$ELSE}
function  sk4d_font_create(typeface: sk_typeface_t; size, sx, kx: float): sk_font_t; cdecl;                                                                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_font_create2(const font: sk_font_t): sk_font_t; cdecl;                                                                                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_font_destroy(self: sk_font_t); cdecl;                                                                                                                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_font_get_baseline_snap(const self: sk_font_t): _bool; cdecl;                                                                                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_font_get_edging(const self: sk_font_t): sk_fontedging_t; cdecl;                                                                                                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_font_get_embedded_bitmaps(const self: sk_font_t): _bool; cdecl;                                                                                                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_font_get_embolden(const self: sk_font_t): _bool; cdecl;                                                                                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_font_get_force_auto_hinting(const self: sk_font_t): _bool; cdecl;                                                                                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_font_get_glyphs(const self: sk_font_t; const text: Pointer; size: size_t; encoding: sk_textencoding_t; result: psk_glyphid_t; max_count: int32_t): int32_t; cdecl;                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_font_get_glyphs_count(const self: sk_font_t; const text: Pointer; size: size_t; encoding: sk_textencoding_t): int32_t; cdecl;                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_font_get_hinting(const self: sk_font_t): sk_fonthinting_t; cdecl;                                                                                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_font_get_horizontal_positions(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; result: pfloat; origin: float); cdecl;                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_font_get_intercepts(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; const positions: psk_point_t; const bounds: pfloat; result: pfloat; const paint: sk_paint_t): size_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_font_get_linear_metrics(const self: sk_font_t): _bool; cdecl;                                                                                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_font_get_metrics(const self: sk_font_t; metrics: psk_fontmetrics_t): float; cdecl;                                                                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_font_get_path(const self: sk_font_t; glyph: sk_glyphid_t): sk_path_t; cdecl;                                                                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_font_get_paths(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; proc: sk_font_path_proc; proc_context: Pointer); cdecl;                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_font_get_positions(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; result: psk_point_t; const origin: psk_point_t); cdecl;                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_font_get_scale_x(const self: sk_font_t): float; cdecl;                                                                                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_font_get_size(const self: sk_font_t): float; cdecl;                                                                                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_font_get_skew_x(const self: sk_font_t): float; cdecl;                                                                                                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_font_get_subpixel(const self: sk_font_t): _bool; cdecl;                                                                                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_font_get_typeface(const self: sk_font_t): sk_typeface_t; cdecl;                                                                                                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_font_get_typeface_or_default(const self: sk_font_t): sk_typeface_t; cdecl;                                                                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_font_get_widths_bounds(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; widths: pfloat; bounds: psk_rect_t; const paint: sk_paint_t); cdecl;                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_font_is_equal(const self, font: sk_font_t): _bool; cdecl;                                                                                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_font_measure_text(const self: sk_font_t; const text: Pointer; size: size_t; encoding: sk_textencoding_t; bounds: psk_rect_t; const paint: sk_paint_t): float; cdecl;                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_font_set_baseline_snap(self: sk_font_t; value: _bool); cdecl;                                                                                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_font_set_edging(self: sk_font_t; value: sk_fontedging_t); cdecl;                                                                                                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_font_set_embedded_bitmaps(self: sk_font_t; value: _bool); cdecl;                                                                                                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_font_set_embolden(self: sk_font_t; value: _bool); cdecl;                                                                                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_font_set_force_auto_hinting(self: sk_font_t; value: _bool); cdecl;                                                                                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_font_set_hinting(self: sk_font_t; value: sk_fonthinting_t); cdecl;                                                                                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_font_set_linear_metrics(self: sk_font_t; value: _bool); cdecl;                                                                                                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_font_set_scale_x(self: sk_font_t; value: float); cdecl;                                                                                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_font_set_size(self: sk_font_t; value: float); cdecl;                                                                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_font_set_skew_x(self: sk_font_t; value: float); cdecl;                                                                                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_font_set_subpixel(self: sk_font_t; value: _bool); cdecl;                                                                                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_font_set_typeface(self: sk_font_t; typeface: sk_typeface_t); cdecl;                                                                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_font_unichar_to_glyph(const self: sk_font_t; uni_char: sk_unichar_t): sk_glyphid_t; cdecl;                                                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_font_unichars_to_glyphs(const self: sk_font_t; const uni_chars: psk_unichar_t; count: int32_t; result: psk_glyphid_t); cdecl;                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_graphics.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_graphics_allow_jit                                       := GetProcAddress(LibraryHandle, PChar('sk4d_graphics_allow_jit'));
  sk4d_graphics_dump_memory_statistics                          := GetProcAddress(LibraryHandle, PChar('sk4d_graphics_dump_memory_statistics'));
  sk4d_graphics_get_font_cache_count_limit                      := GetProcAddress(LibraryHandle, PChar('sk4d_graphics_get_font_cache_count_limit'));
  sk4d_graphics_get_font_cache_count_used                       := GetProcAddress(LibraryHandle, PChar('sk4d_graphics_get_font_cache_count_used'));
  sk4d_graphics_get_font_cache_limit                            := GetProcAddress(LibraryHandle, PChar('sk4d_graphics_get_font_cache_limit'));
  sk4d_graphics_get_font_cache_used                             := GetProcAddress(LibraryHandle, PChar('sk4d_graphics_get_font_cache_used'));
  sk4d_graphics_get_resource_cache_single_allocation_byte_limit := GetProcAddress(LibraryHandle, PChar('sk4d_graphics_get_resource_cache_single_allocation_byte_limit'));
  sk4d_graphics_get_resource_cache_total_byte_limit             := GetProcAddress(LibraryHandle, PChar('sk4d_graphics_get_resource_cache_total_byte_limit'));
  sk4d_graphics_get_resource_cache_total_bytes_used             := GetProcAddress(LibraryHandle, PChar('sk4d_graphics_get_resource_cache_total_bytes_used'));
  sk4d_graphics_init                                            := GetProcAddress(LibraryHandle, PChar('sk4d_graphics_init'));
  sk4d_graphics_purge_all_caches                                := GetProcAddress(LibraryHandle, PChar('sk4d_graphics_purge_all_caches'));
  sk4d_graphics_purge_font_cache                                := GetProcAddress(LibraryHandle, PChar('sk4d_graphics_purge_font_cache'));
  sk4d_graphics_purge_resource_cache                            := GetProcAddress(LibraryHandle, PChar('sk4d_graphics_purge_resource_cache'));
  sk4d_graphics_set_font_cache_count_limit                      := GetProcAddress(LibraryHandle, PChar('sk4d_graphics_set_font_cache_count_limit'));
  sk4d_graphics_set_font_cache_limit                            := GetProcAddress(LibraryHandle, PChar('sk4d_graphics_set_font_cache_limit'));
  sk4d_graphics_set_resource_cache_single_allocation_byte_limit := GetProcAddress(LibraryHandle, PChar('sk4d_graphics_set_resource_cache_single_allocation_byte_limit'));
  sk4d_graphics_set_resource_cache_total_byte_limit             := GetProcAddress(LibraryHandle, PChar('sk4d_graphics_set_resource_cache_total_byte_limit'));
{$ELSE}
procedure sk4d_graphics_allow_jit(); cdecl;                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_graphics_dump_memory_statistics(trace_memory_dump: sk_tracememorydump_t); cdecl;        external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_graphics_get_font_cache_count_limit(): int32_t; cdecl;                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_graphics_get_font_cache_count_used(): int32_t; cdecl;                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_graphics_get_font_cache_limit(): size_t; cdecl;                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_graphics_get_font_cache_used(): size_t; cdecl;                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_graphics_get_resource_cache_single_allocation_byte_limit(): size_t; cdecl;              external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_graphics_get_resource_cache_total_byte_limit(): size_t; cdecl;                          external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_graphics_get_resource_cache_total_bytes_used(): size_t; cdecl;                          external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_graphics_init(); cdecl;                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_graphics_purge_all_caches(); cdecl;                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_graphics_purge_font_cache(); cdecl;                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_graphics_purge_resource_cache(); cdecl;                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_graphics_set_font_cache_count_limit(value: int32_t): int32_t; cdecl;                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_graphics_set_font_cache_limit(value: size_t): size_t; cdecl;                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_graphics_set_resource_cache_single_allocation_byte_limit(value: size_t): size_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_graphics_set_resource_cache_total_byte_limit(value: size_t): size_t; cdecl;             external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_image.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_image_encode_to_file            := GetProcAddress(LibraryHandle, PChar('sk4d_image_encode_to_file'));
  sk4d_image_encode_to_stream          := GetProcAddress(LibraryHandle, PChar('sk4d_image_encode_to_stream'));
  sk4d_image_get_alpha_type            := GetProcAddress(LibraryHandle, PChar('sk4d_image_get_alpha_type'));
  sk4d_image_get_color_space           := GetProcAddress(LibraryHandle, PChar('sk4d_image_get_color_space'));
  sk4d_image_get_color_type            := GetProcAddress(LibraryHandle, PChar('sk4d_image_get_color_type'));
  sk4d_image_get_height                := GetProcAddress(LibraryHandle, PChar('sk4d_image_get_height'));
  sk4d_image_get_image_info            := GetProcAddress(LibraryHandle, PChar('sk4d_image_get_image_info'));
  sk4d_image_get_unique_id             := GetProcAddress(LibraryHandle, PChar('sk4d_image_get_unique_id'));
  sk4d_image_get_width                 := GetProcAddress(LibraryHandle, PChar('sk4d_image_get_width'));
  sk4d_image_is_lazy_generated         := GetProcAddress(LibraryHandle, PChar('sk4d_image_is_lazy_generated'));
  sk4d_image_is_texture_backed         := GetProcAddress(LibraryHandle, PChar('sk4d_image_is_texture_backed'));
  sk4d_image_is_valid                  := GetProcAddress(LibraryHandle, PChar('sk4d_image_is_valid'));
  sk4d_image_make_cross_context        := GetProcAddress(LibraryHandle, PChar('sk4d_image_make_cross_context'));
  sk4d_image_make_from_adopted_texture := GetProcAddress(LibraryHandle, PChar('sk4d_image_make_from_adopted_texture'));
  sk4d_image_make_from_encoded_file    := GetProcAddress(LibraryHandle, PChar('sk4d_image_make_from_encoded_file'));
  sk4d_image_make_from_encoded_stream  := GetProcAddress(LibraryHandle, PChar('sk4d_image_make_from_encoded_stream'));
  sk4d_image_make_from_picture         := GetProcAddress(LibraryHandle, PChar('sk4d_image_make_from_picture'));
  sk4d_image_make_from_raster          := GetProcAddress(LibraryHandle, PChar('sk4d_image_make_from_raster'));
  sk4d_image_make_from_texture         := GetProcAddress(LibraryHandle, PChar('sk4d_image_make_from_texture'));
  sk4d_image_make_non_texture_image    := GetProcAddress(LibraryHandle, PChar('sk4d_image_make_non_texture_image'));
  sk4d_image_make_raster_copy          := GetProcAddress(LibraryHandle, PChar('sk4d_image_make_raster_copy'));
  sk4d_image_make_raster_image         := GetProcAddress(LibraryHandle, PChar('sk4d_image_make_raster_image'));
  sk4d_image_make_raw_shader           := GetProcAddress(LibraryHandle, PChar('sk4d_image_make_raw_shader'));
  sk4d_image_make_shader               := GetProcAddress(LibraryHandle, PChar('sk4d_image_make_shader'));
  sk4d_image_make_subset               := GetProcAddress(LibraryHandle, PChar('sk4d_image_make_subset'));
  sk4d_image_make_texture_image        := GetProcAddress(LibraryHandle, PChar('sk4d_image_make_texture_image'));
  sk4d_image_make_with_filter          := GetProcAddress(LibraryHandle, PChar('sk4d_image_make_with_filter'));
  sk4d_image_peek_pixels               := GetProcAddress(LibraryHandle, PChar('sk4d_image_peek_pixels'));
  sk4d_image_read_pixels               := GetProcAddress(LibraryHandle, PChar('sk4d_image_read_pixels'));
  sk4d_image_scale_pixels              := GetProcAddress(LibraryHandle, PChar('sk4d_image_scale_pixels'));
{$ELSE}
function  sk4d_image_encode_to_file(const self: sk_image_t; const file_name: MarshaledAString; format: sk_encodedimageformat_t; quality: int32_t): _bool; cdecl;                                                                                                                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_encode_to_stream(const self: sk_image_t; w_stream: sk_wstream_t; format: sk_encodedimageformat_t; quality: int32_t): _bool; cdecl;                                                                                                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_get_alpha_type(const self: sk_image_t): sk_alphatype_t; cdecl;                                                                                                                                                                                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_get_color_space(const self: sk_image_t): sk_colorspace_t; cdecl;                                                                                                                                                                                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_get_color_type(const self: sk_image_t): sk_colortype_t; cdecl;                                                                                                                                                                                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_get_height(const self: sk_image_t): int32_t; cdecl;                                                                                                                                                                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_image_get_image_info(const self: sk_image_t; out result: sk_imageinfo_t); cdecl;                                                                                                                                                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_get_unique_id(const self: sk_image_t): uint32_t; cdecl;                                                                                                                                                                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_get_width(const self: sk_image_t): int32_t; cdecl;                                                                                                                                                                                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_is_lazy_generated(const self: sk_image_t): _bool; cdecl;                                                                                                                                                                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_is_texture_backed(const self: sk_image_t): _bool; cdecl;                                                                                                                                                                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_is_valid(const self: sk_image_t; context: gr_directcontext_t): _bool; cdecl;                                                                                                                                                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_make_cross_context(context: gr_directcontext_t; const pixmap: sk_pixmap_t; build_mips, limit_to_max_texture_size: _bool): sk_image_t; cdecl;                                                                                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_make_from_adopted_texture(context: gr_directcontext_t; const texture: gr_backendtexture_t; origin: gr_surfaceorigin_t; color_type: sk_colortype_t; alpha_type: sk_alphatype_t; color_space: sk_colorspace_t): sk_image_t; cdecl;                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_make_from_encoded_file(const file_name: MarshaledAString): sk_image_t; cdecl;                                                                                                                                                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_make_from_encoded_stream(stream: sk_stream_t): sk_image_t; cdecl;                                                                                                                                                                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_make_from_picture(picture: sk_picture_t; const dimensions: psk_isize_t; const matrix: psk_matrix_t; const paint: sk_paint_t; color_space: sk_colorspace_t; const props: psk_surfaceprops_t): sk_image_t; cdecl;                                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_make_from_raster(const pixmap: sk_pixmap_t; proc: sk_image_raster_release_proc; proc_context: Pointer): sk_image_t; cdecl;                                                                                                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_make_from_texture(context: gr_directcontext_t; const texture: gr_backendtexture_t; origin: gr_surfaceorigin_t; color_type: sk_colortype_t; alpha_type: sk_alphatype_t; color_space: sk_colorspace_t; proc: sk_image_texture_release_proc; proc_context: Pointer): sk_image_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_make_non_texture_image(const self: sk_image_t): sk_image_t; cdecl;                                                                                                                                                                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_make_raster_copy(const pixmap: sk_pixmap_t): sk_image_t; cdecl;                                                                                                                                                                                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_make_raster_image(const self: sk_image_t): sk_image_t; cdecl;                                                                                                                                                                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_make_raw_shader(const self: sk_image_t; tile_mode_x, tile_mode_y: sk_tilemode_t; const sampling: psk_samplingoptions_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;                                                                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_make_shader(const self: sk_image_t; tile_mode_x, tile_mode_y: sk_tilemode_t; const sampling: psk_samplingoptions_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;                                                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_make_subset(const self: sk_image_t; const subset: psk_irect_t; context: gr_directcontext_t): sk_image_t; cdecl;                                                                                                                                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_make_texture_image(const self: sk_image_t; context: gr_directcontext_t; is_mipmapped: _bool): sk_image_t; cdecl;                                                                                                                                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_make_with_filter(const self: sk_image_t; context: gr_directcontext_t; const filter: sk_imagefilter_t; const subset, clip_bounds: psk_irect_t; out out_subset: sk_irect_t; out offset: sk_ipoint_t): sk_image_t; cdecl;                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_peek_pixels(const self: sk_image_t): sk_pixmap_t; cdecl;                                                                                                                                                                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_read_pixels(const self: sk_image_t; context: gr_directcontext_t; const dest: sk_pixmap_t; src_x, src_y: int32_t; caching_hint: sk_imagecachinghint_t): _bool; cdecl;                                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_image_scale_pixels(const self: sk_image_t; const dest: sk_pixmap_t; const sampling: psk_samplingoptions_t; caching_hint: sk_imagecachinghint_t): _bool; cdecl;                                                                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_imageencoder.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_imageencoder_encode_to_file   := GetProcAddress(LibraryHandle, PChar('sk4d_imageencoder_encode_to_file'));
  sk4d_imageencoder_encode_to_stream := GetProcAddress(LibraryHandle, PChar('sk4d_imageencoder_encode_to_stream'));
{$ELSE}
function sk4d_imageencoder_encode_to_file(const file_name: MarshaledAString; const src: sk_pixmap_t; format: sk_encodedimageformat_t; quality: int32_t): _bool; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_imageencoder_encode_to_stream(w_stream: sk_wstream_t; const src: sk_pixmap_t; format: sk_encodedimageformat_t; quality: int32_t): _bool; cdecl;          external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_imagefilter.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_imagefilter_can_compute_fast_bounds   := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_can_compute_fast_bounds'));
  sk4d_imagefilter_compute_fast_bounds       := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_compute_fast_bounds'));
  sk4d_imagefilter_make_alpha_threshold      := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_alpha_threshold'));
  sk4d_imagefilter_make_arithmetic           := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_arithmetic'));
  sk4d_imagefilter_make_blend                := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_blend'));
  sk4d_imagefilter_make_blur                 := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_blur'));
  sk4d_imagefilter_make_colorfilter          := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_colorfilter'));
  sk4d_imagefilter_make_compose              := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_compose'));
  sk4d_imagefilter_make_dilate               := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_dilate'));
  sk4d_imagefilter_make_displacement_map     := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_displacement_map'));
  sk4d_imagefilter_make_distant_lit_diffuse  := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_distant_lit_diffuse'));
  sk4d_imagefilter_make_distant_lit_specular := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_distant_lit_specular'));
  sk4d_imagefilter_make_drop_shadow          := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_drop_shadow'));
  sk4d_imagefilter_make_drop_shadow_only     := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_drop_shadow_only'));
  sk4d_imagefilter_make_erode                := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_erode'));
  sk4d_imagefilter_make_image                := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_image'));
  sk4d_imagefilter_make_magnifier            := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_magnifier'));
  sk4d_imagefilter_make_matrix_convolution   := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_matrix_convolution'));
  sk4d_imagefilter_make_matrix_transform     := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_matrix_transform'));
  sk4d_imagefilter_make_merge                := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_merge'));
  sk4d_imagefilter_make_offset               := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_offset'));
  sk4d_imagefilter_make_picture              := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_picture'));
  sk4d_imagefilter_make_point_lit_diffuse    := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_point_lit_diffuse'));
  sk4d_imagefilter_make_point_lit_specular   := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_point_lit_specular'));
  sk4d_imagefilter_make_runtime_shader       := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_runtime_shader'));
  sk4d_imagefilter_make_runtime_shader2      := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_runtime_shader2'));
  sk4d_imagefilter_make_shader               := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_shader'));
  sk4d_imagefilter_make_spot_lit_diffuse     := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_spot_lit_diffuse'));
  sk4d_imagefilter_make_spot_lit_specular    := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_spot_lit_specular'));
  sk4d_imagefilter_make_tile                 := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_tile'));
  sk4d_imagefilter_make_with_local_matrix    := GetProcAddress(LibraryHandle, PChar('sk4d_imagefilter_make_with_local_matrix'));
{$ELSE}
function  sk4d_imagefilter_can_compute_fast_bounds(const self: sk_imagefilter_t): _bool; cdecl;                                                                                                                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_imagefilter_compute_fast_bounds(const self: sk_imagefilter_t; const bounds: psk_rect_t; out result: sk_rect_t); cdecl;                                                                                                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_alpha_threshold(const region: sk_region_t; inner_min, outer_max: float; input: sk_imagefilter_t): sk_imagefilter_t; cdecl;                                                                                                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_arithmetic(k1, k2, k3, k4: float; enforce_premultiplied_color: _bool; background, foreground: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_blend(mode: sk_blendmode_t; background, foreground: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_blur(sigma_x, sigma_y: float; tile_mode: sk_tilemode_t; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_colorfilter(color_filter: sk_colorfilter_t; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_compose(inner, outer: sk_imagefilter_t): sk_imagefilter_t; cdecl;                                                                                                                                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_dilate(radius_x, radius_y: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_displacement_map(x_channel_selector, y_channel_selector: sk_colorchannel_t; scale: float; displacement, input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_distant_lit_diffuse(const direction: psk_point3_t; light_color: sk_color_t; surface_scale, kd: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_distant_lit_specular(const direction: psk_point3_t; light_color: sk_color_t; surface_scale, ks, shininess: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_drop_shadow(dx, dy, sigma_x, sigma_y: float; color: sk_color_t; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_drop_shadow_only(dx, dy, sigma_x, sigma_y: float; color: sk_color_t; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_erode(radius_x, radius_y: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_image(image: sk_image_t; const src, dest: psk_rect_t; const sampling: psk_samplingoptions_t): sk_imagefilter_t; cdecl;                                                                                                                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_magnifier(const src: psk_rect_t; inset: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_matrix_convolution(const kernel_size: psk_isize_t; const kernel: pfloat; gain, bias: float; const kernel_offset: psk_ipoint_t; tile_mode: sk_tilemode_t; convolve_alpha: _bool; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_matrix_transform(const matrix: psk_matrix_t; const sampling: psk_samplingoptions_t; input: sk_imagefilter_t): sk_imagefilter_t; cdecl;                                                                                                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_merge(const filters: psk_imagefilter_t; count: int32_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_offset(dx, dy: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_picture(picture: sk_picture_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_point_lit_diffuse(const location: psk_point3_t; light_color: sk_color_t; surface_scale, kd: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_point_lit_specular(const location: psk_point3_t; light_color: sk_color_t; surface_scale, ks, shininess: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_runtime_shader(const effect_builder: sk_runtimeshaderbuilder_t; const child: MarshaledAString; input: sk_imagefilter_t): sk_imagefilter_t; cdecl;                                                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_runtime_shader2(const effect_builder: sk_runtimeshaderbuilder_t; const children: PMarshaledAString; inputs: psk_imagefilter_t; count: int32_t): sk_imagefilter_t; cdecl;                                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_shader(shader: sk_shader_t; dither: _bool; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_spot_lit_diffuse(const location, target: psk_point3_t; falloff_exponent, cutoff_angle: float; light_color: sk_color_t; surface_scale, kd: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_spot_lit_specular(const location, target: psk_point3_t; falloff_exponent, cutoff_angle: float; light_color: sk_color_t; surface_scale, ks, shininess: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_tile(const src, dest: psk_rect_t; input: sk_imagefilter_t): sk_imagefilter_t; cdecl;                                                                                                                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_imagefilter_make_with_local_matrix(const self: sk_imagefilter_t; const local_matrix: psk_matrix_t): sk_imagefilter_t; cdecl;                                                                                                                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_maskfilter.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_maskfilter_make_blur        := GetProcAddress(LibraryHandle, PChar('sk4d_maskfilter_make_blur'));
  sk4d_maskfilter_make_shader      := GetProcAddress(LibraryHandle, PChar('sk4d_maskfilter_make_shader'));
  sk4d_maskfilter_make_table       := GetProcAddress(LibraryHandle, PChar('sk4d_maskfilter_make_table'));
  sk4d_maskfilter_make_table_clip  := GetProcAddress(LibraryHandle, PChar('sk4d_maskfilter_make_table_clip'));
  sk4d_maskfilter_make_table_gamma := GetProcAddress(LibraryHandle, PChar('sk4d_maskfilter_make_table_gamma'));
{$ELSE}
function sk4d_maskfilter_make_blur(style: sk_blurstyle_t; sigma: float; respect_ctm: _bool): sk_maskfilter_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_maskfilter_make_shader(shader: sk_shader_t): sk_maskfilter_t; cdecl;                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_maskfilter_make_table(const table: puint8_t): sk_maskfilter_t; cdecl;                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_maskfilter_make_table_clip(min, max: uint8_t): sk_maskfilter_t; cdecl;                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_maskfilter_make_table_gamma(gamma: float): sk_maskfilter_t; cdecl;                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_paint.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_paint_create             := GetProcAddress(LibraryHandle, PChar('sk4d_paint_create'));
  sk4d_paint_create2            := GetProcAddress(LibraryHandle, PChar('sk4d_paint_create2'));
  sk4d_paint_destroy            := GetProcAddress(LibraryHandle, PChar('sk4d_paint_destroy'));
  sk4d_paint_get_alpha          := GetProcAddress(LibraryHandle, PChar('sk4d_paint_get_alpha'));
  sk4d_paint_get_alphaf         := GetProcAddress(LibraryHandle, PChar('sk4d_paint_get_alphaf'));
  sk4d_paint_get_anti_alias     := GetProcAddress(LibraryHandle, PChar('sk4d_paint_get_anti_alias'));
  sk4d_paint_get_blender        := GetProcAddress(LibraryHandle, PChar('sk4d_paint_get_blender'));
  sk4d_paint_get_color          := GetProcAddress(LibraryHandle, PChar('sk4d_paint_get_color'));
  sk4d_paint_get_colorf         := GetProcAddress(LibraryHandle, PChar('sk4d_paint_get_colorf'));
  sk4d_paint_get_color_filter   := GetProcAddress(LibraryHandle, PChar('sk4d_paint_get_color_filter'));
  sk4d_paint_get_dither         := GetProcAddress(LibraryHandle, PChar('sk4d_paint_get_dither'));
  sk4d_paint_get_fill_path      := GetProcAddress(LibraryHandle, PChar('sk4d_paint_get_fill_path'));
  sk4d_paint_get_image_filter   := GetProcAddress(LibraryHandle, PChar('sk4d_paint_get_image_filter'));
  sk4d_paint_get_mask_filter    := GetProcAddress(LibraryHandle, PChar('sk4d_paint_get_mask_filter'));
  sk4d_paint_get_path_effect    := GetProcAddress(LibraryHandle, PChar('sk4d_paint_get_path_effect'));
  sk4d_paint_get_shader         := GetProcAddress(LibraryHandle, PChar('sk4d_paint_get_shader'));
  sk4d_paint_get_stroke_cap     := GetProcAddress(LibraryHandle, PChar('sk4d_paint_get_stroke_cap'));
  sk4d_paint_get_stroke_join    := GetProcAddress(LibraryHandle, PChar('sk4d_paint_get_stroke_join'));
  sk4d_paint_get_stroke_miter   := GetProcAddress(LibraryHandle, PChar('sk4d_paint_get_stroke_miter'));
  sk4d_paint_get_stroke_width   := GetProcAddress(LibraryHandle, PChar('sk4d_paint_get_stroke_width'));
  sk4d_paint_get_style          := GetProcAddress(LibraryHandle, PChar('sk4d_paint_get_style'));
  sk4d_paint_reset              := GetProcAddress(LibraryHandle, PChar('sk4d_paint_reset'));
  sk4d_paint_set_alpha          := GetProcAddress(LibraryHandle, PChar('sk4d_paint_set_alpha'));
  sk4d_paint_set_alphaf         := GetProcAddress(LibraryHandle, PChar('sk4d_paint_set_alphaf'));
  sk4d_paint_set_antialias      := GetProcAddress(LibraryHandle, PChar('sk4d_paint_set_antialias'));
  sk4d_paint_set_argb           := GetProcAddress(LibraryHandle, PChar('sk4d_paint_set_argb'));
  sk4d_paint_set_blender        := GetProcAddress(LibraryHandle, PChar('sk4d_paint_set_blender'));
  sk4d_paint_set_color          := GetProcAddress(LibraryHandle, PChar('sk4d_paint_set_color'));
  sk4d_paint_set_colorf         := GetProcAddress(LibraryHandle, PChar('sk4d_paint_set_colorf'));
  sk4d_paint_set_color_filter   := GetProcAddress(LibraryHandle, PChar('sk4d_paint_set_color_filter'));
  sk4d_paint_set_dither         := GetProcAddress(LibraryHandle, PChar('sk4d_paint_set_dither'));
  sk4d_paint_set_image_filter   := GetProcAddress(LibraryHandle, PChar('sk4d_paint_set_image_filter'));
  sk4d_paint_set_mask_filter    := GetProcAddress(LibraryHandle, PChar('sk4d_paint_set_mask_filter'));
  sk4d_paint_set_path_effect    := GetProcAddress(LibraryHandle, PChar('sk4d_paint_set_path_effect'));
  sk4d_paint_set_shader         := GetProcAddress(LibraryHandle, PChar('sk4d_paint_set_shader'));
  sk4d_paint_set_stroke_cap     := GetProcAddress(LibraryHandle, PChar('sk4d_paint_set_stroke_cap'));
  sk4d_paint_set_stroke_join    := GetProcAddress(LibraryHandle, PChar('sk4d_paint_set_stroke_join'));
  sk4d_paint_set_stroke_miter   := GetProcAddress(LibraryHandle, PChar('sk4d_paint_set_stroke_miter'));
  sk4d_paint_set_stroke_width   := GetProcAddress(LibraryHandle, PChar('sk4d_paint_set_stroke_width'));
  sk4d_paint_set_style          := GetProcAddress(LibraryHandle, PChar('sk4d_paint_set_style'));
{$ELSE}
function  sk4d_paint_create(): sk_paint_t; cdecl;                                                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paint_create2(const paint: sk_paint_t): sk_paint_t; cdecl;                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paint_destroy(self: sk_paint_t); cdecl;                                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paint_get_alpha(const self: sk_paint_t): uint8_t; cdecl;                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paint_get_alphaf(const self: sk_paint_t): float; cdecl;                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paint_get_anti_alias(const self: sk_paint_t): _bool; cdecl;                                                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paint_get_blender(const self: sk_paint_t): sk_blender_t; cdecl;                                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paint_get_color(const self: sk_paint_t): sk_color_t; cdecl;                                                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paint_get_colorf(const self: sk_paint_t; out result: sk_color4f_t); cdecl;                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paint_get_color_filter(const self: sk_paint_t): sk_colorfilter_t; cdecl;                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paint_get_dither(const self: sk_paint_t): _bool; cdecl;                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paint_get_fill_path(const self: sk_paint_t; const path: sk_path_t; const cull_rect: psk_rect_t; res_scale: float): sk_path_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paint_get_image_filter(const self: sk_paint_t): sk_imagefilter_t; cdecl;                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paint_get_mask_filter(const self: sk_paint_t): sk_maskfilter_t; cdecl;                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paint_get_path_effect(const self: sk_paint_t): sk_patheffect_t; cdecl;                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paint_get_shader(const self: sk_paint_t): sk_shader_t; cdecl;                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paint_get_stroke_cap(const self: sk_paint_t): sk_strokecap_t; cdecl;                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paint_get_stroke_join(const self: sk_paint_t): sk_strokejoin_t; cdecl;                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paint_get_stroke_miter(const self: sk_paint_t): float; cdecl;                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paint_get_stroke_width(const self: sk_paint_t): float; cdecl;                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paint_get_style(const self: sk_paint_t): sk_paintstyle_t; cdecl;                                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paint_reset(self: sk_paint_t); cdecl;                                                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paint_set_alpha(self: sk_paint_t; value: uint8_t); cdecl;                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paint_set_alphaf(self: sk_paint_t; value: float); cdecl;                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paint_set_antialias(self: sk_paint_t; value: _bool); cdecl;                                                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paint_set_argb(self: sk_paint_t; a, r, g, b: uint8_t); cdecl;                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paint_set_blender(self: sk_paint_t; value: sk_blender_t); cdecl;                                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paint_set_color(self: sk_paint_t; value: sk_color_t); cdecl;                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paint_set_colorf(self: sk_paint_t; const value: psk_color4f_t; color_space: sk_colorspace_t); cdecl;                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paint_set_color_filter(self: sk_paint_t; value: sk_colorfilter_t); cdecl;                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paint_set_dither(self: sk_paint_t; value: _bool); cdecl;                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paint_set_image_filter(self: sk_paint_t; value: sk_imagefilter_t); cdecl;                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paint_set_mask_filter(self: sk_paint_t; value: sk_maskfilter_t); cdecl;                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paint_set_path_effect(self: sk_paint_t; value: sk_patheffect_t); cdecl;                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paint_set_shader(self: sk_paint_t; value: sk_shader_t); cdecl;                                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paint_set_stroke_cap(self: sk_paint_t; value: sk_strokecap_t); cdecl;                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paint_set_stroke_join(self: sk_paint_t; value: sk_strokejoin_t); cdecl;                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paint_set_stroke_miter(self: sk_paint_t; value: float); cdecl;                                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paint_set_stroke_width(self: sk_paint_t; value: float); cdecl;                                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paint_set_style(self: sk_paint_t; value: sk_paintstyle_t); cdecl;                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_path.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_opbuilder_add               := GetProcAddress(LibraryHandle, PChar('sk4d_opbuilder_add'));
  sk4d_opbuilder_create            := GetProcAddress(LibraryHandle, PChar('sk4d_opbuilder_create'));
  sk4d_opbuilder_destroy           := GetProcAddress(LibraryHandle, PChar('sk4d_opbuilder_destroy'));
  sk4d_opbuilder_detach            := GetProcAddress(LibraryHandle, PChar('sk4d_opbuilder_detach'));
  sk4d_path_contains               := GetProcAddress(LibraryHandle, PChar('sk4d_path_contains'));
  sk4d_path_convert_conic_to_quads := GetProcAddress(LibraryHandle, PChar('sk4d_path_convert_conic_to_quads'));
  sk4d_path_create                 := GetProcAddress(LibraryHandle, PChar('sk4d_path_create'));
  sk4d_path_create2                := GetProcAddress(LibraryHandle, PChar('sk4d_path_create2'));
  sk4d_path_destroy                := GetProcAddress(LibraryHandle, PChar('sk4d_path_destroy'));
  sk4d_path_get_bounds             := GetProcAddress(LibraryHandle, PChar('sk4d_path_get_bounds'));
  sk4d_path_get_fill_type          := GetProcAddress(LibraryHandle, PChar('sk4d_path_get_fill_type'));
  sk4d_path_get_last_point         := GetProcAddress(LibraryHandle, PChar('sk4d_path_get_last_point'));
  sk4d_path_get_segment_masks      := GetProcAddress(LibraryHandle, PChar('sk4d_path_get_segment_masks'));
  sk4d_path_get_tight_bounds       := GetProcAddress(LibraryHandle, PChar('sk4d_path_get_tight_bounds'));
  sk4d_path_interpolate            := GetProcAddress(LibraryHandle, PChar('sk4d_path_interpolate'));
  sk4d_path_is_convex              := GetProcAddress(LibraryHandle, PChar('sk4d_path_is_convex'));
  sk4d_path_is_empty               := GetProcAddress(LibraryHandle, PChar('sk4d_path_is_empty'));
  sk4d_path_is_finite              := GetProcAddress(LibraryHandle, PChar('sk4d_path_is_finite'));
  sk4d_path_is_interpolatable      := GetProcAddress(LibraryHandle, PChar('sk4d_path_is_interpolatable'));
  sk4d_path_is_last_contour_closed := GetProcAddress(LibraryHandle, PChar('sk4d_path_is_last_contour_closed'));
  sk4d_path_is_line                := GetProcAddress(LibraryHandle, PChar('sk4d_path_is_line'));
  sk4d_path_is_oval                := GetProcAddress(LibraryHandle, PChar('sk4d_path_is_oval'));
  sk4d_path_is_rect                := GetProcAddress(LibraryHandle, PChar('sk4d_path_is_rect'));
  sk4d_path_is_rrect               := GetProcAddress(LibraryHandle, PChar('sk4d_path_is_rrect'));
  sk4d_path_op                     := GetProcAddress(LibraryHandle, PChar('sk4d_path_op'));
  sk4d_path_serialize_to_stream    := GetProcAddress(LibraryHandle, PChar('sk4d_path_serialize_to_stream'));
  sk4d_path_to_svg                 := GetProcAddress(LibraryHandle, PChar('sk4d_path_to_svg'));
  sk4d_path_transform              := GetProcAddress(LibraryHandle, PChar('sk4d_path_transform'));
  sk4d_pathiterator_create         := GetProcAddress(LibraryHandle, PChar('sk4d_pathiterator_create'));
  sk4d_pathiterator_destroy        := GetProcAddress(LibraryHandle, PChar('sk4d_pathiterator_destroy'));
  sk4d_pathiterator_next           := GetProcAddress(LibraryHandle, PChar('sk4d_pathiterator_next'));
{$ELSE}
procedure sk4d_opbuilder_add(self: sk_opbuilder_t; const path: sk_path_t; op: sk_pathop_t); cdecl;                                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_opbuilder_create(): sk_opbuilder_t; cdecl;                                                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_opbuilder_destroy(self: sk_opbuilder_t); cdecl;                                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_opbuilder_detach(self: sk_opbuilder_t): sk_path_t; cdecl;                                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_path_contains(const self: sk_path_t; x, y: float): _bool; cdecl;                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_path_convert_conic_to_quads(const point1, point2, point3: psk_point_t; weight: float; points: psk_point_t; power2: int32_t): int32_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_path_create(const svg: MarshaledAString): sk_path_t; cdecl;                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_path_create2(stream: sk_stream_t): sk_path_t; cdecl;                                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_path_destroy(self: sk_path_t); cdecl;                                                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_path_get_bounds(const self: sk_path_t; out result: sk_rect_t); cdecl;                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_path_get_fill_type(const self: sk_path_t): sk_pathfilltype_t; cdecl;                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_path_get_last_point(const self: sk_path_t; out result: sk_point_t): _bool; cdecl;                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_path_get_segment_masks(const self: sk_path_t): uint32_t; cdecl;                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_path_get_tight_bounds(const self: sk_path_t; out result: sk_rect_t); cdecl;                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_path_interpolate(const self, ending: sk_path_t; weight: float): sk_path_t; cdecl;                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_path_is_convex(const self: sk_path_t): _bool; cdecl;                                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_path_is_empty(const self: sk_path_t): _bool; cdecl;                                                                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_path_is_finite(const self: sk_path_t): _bool; cdecl;                                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_path_is_interpolatable(const self, path: sk_path_t): _bool; cdecl;                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_path_is_last_contour_closed(const self: sk_path_t): _bool; cdecl;                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_path_is_line(const self: sk_path_t; lines: psk_point_t): _bool; cdecl;                                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_path_is_oval(const self: sk_path_t; oval: psk_rect_t): _bool; cdecl;                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_path_is_rect(const self: sk_path_t; rect: psk_rect_t): _bool; cdecl;                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_path_is_rrect(const self: sk_path_t; rrect: sk_rrect_t): _bool; cdecl;                                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_path_op(const self, path: sk_path_t; op: sk_pathop_t): sk_path_t; cdecl;                                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_path_serialize_to_stream(const self: sk_path_t; w_stream: sk_wstream_t); cdecl;                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_path_to_svg(const self: sk_path_t): sk_string_t; cdecl;                                                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_path_transform(const self: sk_path_t; const matrix: psk_matrix_t): sk_path_t; cdecl;                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pathiterator_create(const path: sk_path_t; force_close: _bool): sk_pathiterator_t; cdecl;                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathiterator_destroy(self: sk_pathiterator_t); cdecl;                                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pathiterator_next(self: sk_pathiterator_t; out elem: sk_pathiteratorelem_t): _bool; cdecl;                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_pathbuilder.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_pathbuilder_add_arc                 := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_add_arc'));
  sk4d_pathbuilder_add_circle              := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_add_circle'));
  sk4d_pathbuilder_add_oval                := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_add_oval'));
  sk4d_pathbuilder_add_path                := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_add_path'));
  sk4d_pathbuilder_add_polygon             := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_add_polygon'));
  sk4d_pathbuilder_add_rect                := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_add_rect'));
  sk4d_pathbuilder_add_rrect               := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_add_rrect'));
  sk4d_pathbuilder_arc_to                  := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_arc_to'));
  sk4d_pathbuilder_arc_to2                 := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_arc_to2'));
  sk4d_pathbuilder_arc_to3                 := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_arc_to3'));
  sk4d_pathbuilder_close                   := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_close'));
  sk4d_pathbuilder_conic_to                := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_conic_to'));
  sk4d_pathbuilder_create                  := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_create'));
  sk4d_pathbuilder_create2                 := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_create2'));
  sk4d_pathbuilder_cubic_to                := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_cubic_to'));
  sk4d_pathbuilder_destroy                 := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_destroy'));
  sk4d_pathbuilder_detach                  := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_detach'));
  sk4d_pathbuilder_get_bounds              := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_get_bounds'));
  sk4d_pathbuilder_get_fill_type           := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_get_fill_type'));
  sk4d_pathbuilder_inc_reserve             := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_inc_reserve'));
  sk4d_pathbuilder_line_to                 := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_line_to'));
  sk4d_pathbuilder_move_to                 := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_move_to'));
  sk4d_pathbuilder_offset                  := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_offset'));
  sk4d_pathbuilder_polyline_to             := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_polyline_to'));
  sk4d_pathbuilder_quad_to                 := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_quad_to'));
  sk4d_pathbuilder_r_conic_to              := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_r_conic_to'));
  sk4d_pathbuilder_r_cubic_to              := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_r_cubic_to'));
  sk4d_pathbuilder_r_line_to               := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_r_line_to'));
  sk4d_pathbuilder_r_quad_to               := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_r_quad_to'));
  sk4d_pathbuilder_reset                   := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_reset'));
  sk4d_pathbuilder_set_filltype            := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_set_filltype'));
  sk4d_pathbuilder_snapshot                := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_snapshot'));
  sk4d_pathbuilder_toggle_inverse_filltype := GetProcAddress(LibraryHandle, PChar('sk4d_pathbuilder_toggle_inverse_filltype'));
{$ELSE}
procedure sk4d_pathbuilder_add_arc(self: sk_pathbuilder_t; const oval: psk_rect_t; start_angle, sweep_angle: float); cdecl;                                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_add_circle(self: sk_pathbuilder_t; center_x, center_y, radius: float; direction: sk_pathdirection_t); cdecl;                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_add_oval(self: sk_pathbuilder_t; const oval: psk_rect_t; direction: sk_pathdirection_t; start_index: uint32_t); cdecl;                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_add_path(self: sk_pathbuilder_t; const path: sk_path_t); cdecl;                                                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_add_polygon(self: sk_pathbuilder_t; polygon: psk_point_t; count: int32_t; is_closed: _bool); cdecl;                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_add_rect(self: sk_pathbuilder_t; const rect: psk_rect_t; direction: sk_pathdirection_t; start_index: uint32_t); cdecl;                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_add_rrect(self: sk_pathbuilder_t; const rrect: sk_rrect_t; direction: sk_pathdirection_t; start_index: uint32_t); cdecl;                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_arc_to(self: sk_pathbuilder_t; const radius: psk_point_t; x_axis_rotate: float; large_arc: sk_patharcsize_t; sweep: sk_pathdirection_t; const xy: psk_point_t); cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_arc_to2(self: sk_pathbuilder_t; const oval: psk_rect_t; start_angle, sweep_angle: float; force_move_to: _bool); cdecl;                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_arc_to3(self: sk_pathbuilder_t; const point1, point2: psk_point_t; radius: float); cdecl;                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_close(self: sk_pathbuilder_t); cdecl;                                                                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_conic_to(self: sk_pathbuilder_t; const point1, point2: psk_point_t; weight: float); cdecl;                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pathbuilder_create(): sk_pathbuilder_t; cdecl;                                                                                                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pathbuilder_create2(const path_builder: sk_pathbuilder_t): sk_pathbuilder_t; cdecl;                                                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_cubic_to(self: sk_pathbuilder_t; const point1, point2, point3: psk_point_t); cdecl;                                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_destroy(self: sk_pathbuilder_t); cdecl;                                                                                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pathbuilder_detach(self: sk_pathbuilder_t): sk_path_t; cdecl;                                                                                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_get_bounds(const self: sk_pathbuilder_t; out result: sk_rect_t); cdecl;                                                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pathbuilder_get_fill_type(const self: sk_pathbuilder_t): sk_pathfilltype_t; cdecl;                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_inc_reserve(self: sk_pathbuilder_t; extra_point_count, extra_verb_count: int32_t); cdecl;                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_line_to(self: sk_pathbuilder_t; const cpoint: psk_point_t); cdecl;                                                                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_move_to(self: sk_pathbuilder_t; const cpoint: psk_point_t); cdecl;                                                                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_offset(self: sk_pathbuilder_t; dx, dy: float); cdecl;                                                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_polyline_to(self: sk_pathbuilder_t; const points: psk_point_t; count: int32_t); cdecl;                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_quad_to(self: sk_pathbuilder_t; const point1, point2: psk_point_t); cdecl;                                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_r_conic_to(self: sk_pathbuilder_t; const point1, point2: psk_point_t; weight: float); cdecl;                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_r_cubic_to(self: sk_pathbuilder_t; const point1, point2, point3: psk_point_t); cdecl;                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_r_line_to(self: sk_pathbuilder_t; const point: psk_point_t); cdecl;                                                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_r_quad_to(self: sk_pathbuilder_t; const point1, point2: psk_point_t); cdecl;                                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_reset(self: sk_pathbuilder_t); cdecl;                                                                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_set_filltype(self: sk_pathbuilder_t; value: sk_pathfilltype_t); cdecl;                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pathbuilder_snapshot(const self: sk_pathbuilder_t): sk_path_t; cdecl;                                                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathbuilder_toggle_inverse_filltype(self: sk_pathbuilder_t); cdecl;                                                                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_patheffect.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_patheffect_make_1dpath          := GetProcAddress(LibraryHandle, PChar('sk4d_patheffect_make_1dpath'));
  sk4d_patheffect_make_2dline          := GetProcAddress(LibraryHandle, PChar('sk4d_patheffect_make_2dline'));
  sk4d_patheffect_make_2dpath          := GetProcAddress(LibraryHandle, PChar('sk4d_patheffect_make_2dpath'));
  sk4d_patheffect_make_compose         := GetProcAddress(LibraryHandle, PChar('sk4d_patheffect_make_compose'));
  sk4d_patheffect_make_corner          := GetProcAddress(LibraryHandle, PChar('sk4d_patheffect_make_corner'));
  sk4d_patheffect_make_dash            := GetProcAddress(LibraryHandle, PChar('sk4d_patheffect_make_dash'));
  sk4d_patheffect_make_discrete        := GetProcAddress(LibraryHandle, PChar('sk4d_patheffect_make_discrete'));
  sk4d_patheffect_make_matrix          := GetProcAddress(LibraryHandle, PChar('sk4d_patheffect_make_matrix'));
  sk4d_patheffect_make_merge           := GetProcAddress(LibraryHandle, PChar('sk4d_patheffect_make_merge'));
  sk4d_patheffect_make_stroke          := GetProcAddress(LibraryHandle, PChar('sk4d_patheffect_make_stroke'));
  sk4d_patheffect_make_stroke_and_fill := GetProcAddress(LibraryHandle, PChar('sk4d_patheffect_make_stroke_and_fill'));
  sk4d_patheffect_make_sum             := GetProcAddress(LibraryHandle, PChar('sk4d_patheffect_make_sum'));
  sk4d_patheffect_make_translate       := GetProcAddress(LibraryHandle, PChar('sk4d_patheffect_make_translate'));
  sk4d_patheffect_make_trim            := GetProcAddress(LibraryHandle, PChar('sk4d_patheffect_make_trim'));
{$ELSE}
function sk4d_patheffect_make_1dpath(const path: sk_path_t; advance, phase: float; style: sk_patheffect1dstyle_t): sk_patheffect_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_patheffect_make_2dline(width: float; const matrix: psk_matrix_t): sk_patheffect_t; cdecl;                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_patheffect_make_2dpath(const matrix: psk_matrix_t; const path: sk_path_t): sk_patheffect_t; cdecl;                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_patheffect_make_compose(outer, inner: sk_patheffect_t): sk_patheffect_t; cdecl;                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_patheffect_make_corner(radius: float): sk_patheffect_t; cdecl;                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_patheffect_make_dash(const intervals: pfloat; count: int32_t; phase: float): sk_patheffect_t; cdecl;                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_patheffect_make_discrete(seg_length, deviation: float; seed_assist: uint32_t): sk_patheffect_t; cdecl;                       external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_patheffect_make_matrix(const matrix: psk_matrix_t): sk_patheffect_t; cdecl;                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_patheffect_make_merge(effect1, effect2: sk_patheffect_t; op: sk_pathop_t): sk_patheffect_t; cdecl;                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_patheffect_make_stroke(width: float; join: sk_strokejoin_t; cap: sk_strokecap_t; miter: float): sk_patheffect_t; cdecl;      external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_patheffect_make_stroke_and_fill(): sk_patheffect_t; cdecl;                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_patheffect_make_sum(effect1, effect2: sk_patheffect_t): sk_patheffect_t; cdecl;                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_patheffect_make_translate(dx, dy: float): sk_patheffect_t; cdecl;                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_patheffect_make_trim(start, stop: float; mode: sk_patheffecttrimmode_t): sk_patheffect_t; cdecl;                             external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_pathmeasure.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_pathmeasure_create                   := GetProcAddress(LibraryHandle, PChar('sk4d_pathmeasure_create'));
  sk4d_pathmeasure_destroy                  := GetProcAddress(LibraryHandle, PChar('sk4d_pathmeasure_destroy'));
  sk4d_pathmeasure_get_length               := GetProcAddress(LibraryHandle, PChar('sk4d_pathmeasure_get_length'));
  sk4d_pathmeasure_get_matrix               := GetProcAddress(LibraryHandle, PChar('sk4d_pathmeasure_get_matrix'));
  sk4d_pathmeasure_get_position_and_tangent := GetProcAddress(LibraryHandle, PChar('sk4d_pathmeasure_get_position_and_tangent'));
  sk4d_pathmeasure_get_segment              := GetProcAddress(LibraryHandle, PChar('sk4d_pathmeasure_get_segment'));
  sk4d_pathmeasure_is_closed                := GetProcAddress(LibraryHandle, PChar('sk4d_pathmeasure_is_closed'));
  sk4d_pathmeasure_next_contour             := GetProcAddress(LibraryHandle, PChar('sk4d_pathmeasure_next_contour'));
{$ELSE}
function  sk4d_pathmeasure_create(const path: sk_path_t; force_closed: _bool; res_scale: float): sk_pathmeasure_t; cdecl;                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pathmeasure_destroy(self: sk_pathmeasure_t); cdecl;                                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pathmeasure_get_length(self: sk_pathmeasure_t): float; cdecl;                                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pathmeasure_get_matrix(self: sk_pathmeasure_t; distance: float; out matrix: sk_matrix_t; matrix_flags: uint32_t): _bool; cdecl;                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pathmeasure_get_position_and_tangent(self: sk_pathmeasure_t; distance: float; out position: sk_point_t; out tangent: sk_vector_t): _bool; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pathmeasure_get_segment(self: sk_pathmeasure_t; start, stop: float; start_with_move_to: _bool): sk_path_t; cdecl;                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pathmeasure_is_closed(self: sk_pathmeasure_t): _bool; cdecl;                                                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pathmeasure_next_contour(self: sk_pathmeasure_t): _bool; cdecl;                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_picture.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_picture_approximate_bytes_used := GetProcAddress(LibraryHandle, PChar('sk4d_picture_approximate_bytes_used'));
  sk4d_picture_approximate_op_count   := GetProcAddress(LibraryHandle, PChar('sk4d_picture_approximate_op_count'));
  sk4d_picture_get_cull_rect          := GetProcAddress(LibraryHandle, PChar('sk4d_picture_get_cull_rect'));
  sk4d_picture_make_from_stream       := GetProcAddress(LibraryHandle, PChar('sk4d_picture_make_from_stream'));
  sk4d_picture_make_shader            := GetProcAddress(LibraryHandle, PChar('sk4d_picture_make_shader'));
  sk4d_picture_playback               := GetProcAddress(LibraryHandle, PChar('sk4d_picture_playback'));
  sk4d_picture_serialize_to_stream    := GetProcAddress(LibraryHandle, PChar('sk4d_picture_serialize_to_stream'));
{$ELSE}
function  sk4d_picture_approximate_bytes_used(const self: sk_picture_t): size_t; cdecl;                                                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_picture_approximate_op_count(const self: sk_picture_t; nested: _bool): int32_t; cdecl;                                                                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_picture_get_cull_rect(const self: sk_picture_t; out result: sk_rect_t); cdecl;                                                                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_picture_make_from_stream(stream: sk_stream_t): sk_picture_t; cdecl;                                                                                                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_picture_make_shader(const self: sk_picture_t; tile_mode_x, tile_mode_y: sk_tilemode_t; filter_mode: sk_filtermode_t; const local_matrix: psk_matrix_t; const tile_rect: psk_rect_t): sk_shader_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_picture_playback(const self: sk_picture_t; canvas: sk_canvas_t); cdecl;                                                                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_picture_serialize_to_stream(const self: sk_picture_t; w_stream: sk_wstream_t); cdecl;                                                                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_picturerecorder.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_picturerecorder_begin_recording   := GetProcAddress(LibraryHandle, PChar('sk4d_picturerecorder_begin_recording'));
  sk4d_picturerecorder_create            := GetProcAddress(LibraryHandle, PChar('sk4d_picturerecorder_create'));
  sk4d_picturerecorder_destroy           := GetProcAddress(LibraryHandle, PChar('sk4d_picturerecorder_destroy'));
  sk4d_picturerecorder_finish_recording  := GetProcAddress(LibraryHandle, PChar('sk4d_picturerecorder_finish_recording'));
  sk4d_picturerecorder_finish_recording2 := GetProcAddress(LibraryHandle, PChar('sk4d_picturerecorder_finish_recording2'));
{$ELSE}
function  sk4d_picturerecorder_begin_recording(self: sk_picturerecorder_t; const bounds: psk_rect_t): sk_canvas_t; cdecl;       external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_picturerecorder_create(): sk_picturerecorder_t; cdecl;                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_picturerecorder_destroy(self: sk_picturerecorder_t); cdecl;                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_picturerecorder_finish_recording(self: sk_picturerecorder_t): sk_picture_t; cdecl;                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_picturerecorder_finish_recording2(self: sk_picturerecorder_t; const cull_rect: psk_rect_t): sk_picture_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_pixmap.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_pixmap_create          := GetProcAddress(LibraryHandle, PChar('sk4d_pixmap_create'));
  sk4d_pixmap_destroy         := GetProcAddress(LibraryHandle, PChar('sk4d_pixmap_destroy'));
  sk4d_pixmap_erase           := GetProcAddress(LibraryHandle, PChar('sk4d_pixmap_erase'));
  sk4d_pixmap_erase2          := GetProcAddress(LibraryHandle, PChar('sk4d_pixmap_erase2'));
  sk4d_pixmap_extract_subset  := GetProcAddress(LibraryHandle, PChar('sk4d_pixmap_extract_subset'));
  sk4d_pixmap_get_alpha       := GetProcAddress(LibraryHandle, PChar('sk4d_pixmap_get_alpha'));
  sk4d_pixmap_get_alpha_type  := GetProcAddress(LibraryHandle, PChar('sk4d_pixmap_get_alpha_type'));
  sk4d_pixmap_get_color       := GetProcAddress(LibraryHandle, PChar('sk4d_pixmap_get_color'));
  sk4d_pixmap_get_color_space := GetProcAddress(LibraryHandle, PChar('sk4d_pixmap_get_color_space'));
  sk4d_pixmap_get_color_type  := GetProcAddress(LibraryHandle, PChar('sk4d_pixmap_get_color_type'));
  sk4d_pixmap_get_colorf      := GetProcAddress(LibraryHandle, PChar('sk4d_pixmap_get_colorf'));
  sk4d_pixmap_get_height      := GetProcAddress(LibraryHandle, PChar('sk4d_pixmap_get_height'));
  sk4d_pixmap_get_image_info  := GetProcAddress(LibraryHandle, PChar('sk4d_pixmap_get_image_info'));
  sk4d_pixmap_get_pixel_addr  := GetProcAddress(LibraryHandle, PChar('sk4d_pixmap_get_pixel_addr'));
  sk4d_pixmap_get_pixels      := GetProcAddress(LibraryHandle, PChar('sk4d_pixmap_get_pixels'));
  sk4d_pixmap_get_row_bytes   := GetProcAddress(LibraryHandle, PChar('sk4d_pixmap_get_row_bytes'));
  sk4d_pixmap_get_width       := GetProcAddress(LibraryHandle, PChar('sk4d_pixmap_get_width'));
  sk4d_pixmap_read_pixels     := GetProcAddress(LibraryHandle, PChar('sk4d_pixmap_read_pixels'));
  sk4d_pixmap_scale_pixels    := GetProcAddress(LibraryHandle, PChar('sk4d_pixmap_scale_pixels'));
  sk4d_pixmap_set_colorspace  := GetProcAddress(LibraryHandle, PChar('sk4d_pixmap_set_colorspace'));
{$ELSE}
function  sk4d_pixmap_create(const image_info: psk_imageinfo_t; const pixels: Pointer; row_bytes: size_t): sk_pixmap_t; cdecl;                          external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pixmap_destroy(self: sk_pixmap_t); cdecl;                                                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pixmap_erase(const self: sk_pixmap_t; color: sk_color_t; const area: psk_irect_t): _bool; cdecl;                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pixmap_erase2(const self: sk_pixmap_t; const color: psk_color4f_t; color_space: sk_colorspace_t; const area: psk_irect_t): _bool; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pixmap_extract_subset(const self: sk_pixmap_t; dest: sk_pixmap_t; const area: psk_irect_t): _bool; cdecl;                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pixmap_get_alpha(const self: sk_pixmap_t; x, y: int32_t): float; cdecl;                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pixmap_get_alpha_type(const self: sk_pixmap_t): sk_alphatype_t; cdecl;                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pixmap_get_color(const self: sk_pixmap_t; x, y: int32_t): sk_color_t; cdecl;                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pixmap_get_color_space(const self: sk_pixmap_t): sk_colorspace_t; cdecl;                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pixmap_get_color_type(const self: sk_pixmap_t): sk_colortype_t; cdecl;                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pixmap_get_colorf(const self: sk_pixmap_t; x, y: int32_t; out result: sk_color4f_t); cdecl;                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pixmap_get_height(const self: sk_pixmap_t): int32_t; cdecl;                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pixmap_get_image_info(const self: sk_pixmap_t; out result: sk_imageinfo_t); cdecl;                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pixmap_get_pixel_addr(const self: sk_pixmap_t; x, y: int32_t): Pointer; cdecl;                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pixmap_get_pixels(const self: sk_pixmap_t): Pointer; cdecl;                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pixmap_get_row_bytes(const self: sk_pixmap_t): size_t; cdecl;                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pixmap_get_width(const self: sk_pixmap_t): int32_t; cdecl;                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pixmap_read_pixels(const self, dest: sk_pixmap_t; src_x, src_y: int32_t): _bool; cdecl;                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_pixmap_scale_pixels(const self, dest: sk_pixmap_t; const sampling: psk_samplingoptions_t): _bool; cdecl;                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_pixmap_set_colorspace(self: sk_pixmap_t; value: sk_colorspace_t); cdecl;                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_refcnt.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_refcnt_ref   := GetProcAddress(LibraryHandle, PChar('sk4d_refcnt_ref'));
  sk4d_refcnt_unref := GetProcAddress(LibraryHandle, PChar('sk4d_refcnt_unref'));
{$ELSE}
procedure sk4d_refcnt_ref(const self: sk_refcnt_t); cdecl;   external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_refcnt_unref(const self: sk_refcnt_t); cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_region.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_region_contains              := GetProcAddress(LibraryHandle, PChar('sk4d_region_contains'));
  sk4d_region_contains2             := GetProcAddress(LibraryHandle, PChar('sk4d_region_contains2'));
  sk4d_region_contains3             := GetProcAddress(LibraryHandle, PChar('sk4d_region_contains3'));
  sk4d_region_create                := GetProcAddress(LibraryHandle, PChar('sk4d_region_create'));
  sk4d_region_create2               := GetProcAddress(LibraryHandle, PChar('sk4d_region_create2'));
  sk4d_region_destroy               := GetProcAddress(LibraryHandle, PChar('sk4d_region_destroy'));
  sk4d_region_get_boundary_path     := GetProcAddress(LibraryHandle, PChar('sk4d_region_get_boundary_path'));
  sk4d_region_get_bounds            := GetProcAddress(LibraryHandle, PChar('sk4d_region_get_bounds'));
  sk4d_region_intersects            := GetProcAddress(LibraryHandle, PChar('sk4d_region_intersects'));
  sk4d_region_intersects2           := GetProcAddress(LibraryHandle, PChar('sk4d_region_intersects2'));
  sk4d_region_is_complex            := GetProcAddress(LibraryHandle, PChar('sk4d_region_is_complex'));
  sk4d_region_is_empty              := GetProcAddress(LibraryHandle, PChar('sk4d_region_is_empty'));
  sk4d_region_is_equal              := GetProcAddress(LibraryHandle, PChar('sk4d_region_is_equal'));
  sk4d_region_is_rect               := GetProcAddress(LibraryHandle, PChar('sk4d_region_is_rect'));
  sk4d_region_op                    := GetProcAddress(LibraryHandle, PChar('sk4d_region_op'));
  sk4d_region_op2                   := GetProcAddress(LibraryHandle, PChar('sk4d_region_op2'));
  sk4d_region_quick_contains        := GetProcAddress(LibraryHandle, PChar('sk4d_region_quick_contains'));
  sk4d_region_quick_reject          := GetProcAddress(LibraryHandle, PChar('sk4d_region_quick_reject'));
  sk4d_region_quick_reject2         := GetProcAddress(LibraryHandle, PChar('sk4d_region_quick_reject2'));
  sk4d_region_set_empty             := GetProcAddress(LibraryHandle, PChar('sk4d_region_set_empty'));
  sk4d_region_set_path              := GetProcAddress(LibraryHandle, PChar('sk4d_region_set_path'));
  sk4d_region_set_rect              := GetProcAddress(LibraryHandle, PChar('sk4d_region_set_rect'));
  sk4d_region_set_rects             := GetProcAddress(LibraryHandle, PChar('sk4d_region_set_rects'));
  sk4d_region_translate             := GetProcAddress(LibraryHandle, PChar('sk4d_region_translate'));
  sk4d_regioncliperator_create      := GetProcAddress(LibraryHandle, PChar('sk4d_regioncliperator_create'));
  sk4d_regioncliperator_destroy     := GetProcAddress(LibraryHandle, PChar('sk4d_regioncliperator_destroy'));
  sk4d_regioncliperator_get_current := GetProcAddress(LibraryHandle, PChar('sk4d_regioncliperator_get_current'));
  sk4d_regioncliperator_move_next   := GetProcAddress(LibraryHandle, PChar('sk4d_regioncliperator_move_next'));
  sk4d_regioniterator_create        := GetProcAddress(LibraryHandle, PChar('sk4d_regioniterator_create'));
  sk4d_regioniterator_destroy       := GetProcAddress(LibraryHandle, PChar('sk4d_regioniterator_destroy'));
  sk4d_regioniterator_get_current   := GetProcAddress(LibraryHandle, PChar('sk4d_regioniterator_get_current'));
  sk4d_regioniterator_move_next     := GetProcAddress(LibraryHandle, PChar('sk4d_regioniterator_move_next'));
  sk4d_regionspanerator_create      := GetProcAddress(LibraryHandle, PChar('sk4d_regionspanerator_create'));
  sk4d_regionspanerator_destroy     := GetProcAddress(LibraryHandle, PChar('sk4d_regionspanerator_destroy'));
  sk4d_regionspanerator_next        := GetProcAddress(LibraryHandle, PChar('sk4d_regionspanerator_next'));
{$ELSE}
function  sk4d_region_contains(const self, region: sk_region_t): _bool; cdecl;                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_region_contains2(const self: sk_region_t; const rect: psk_irect_t): _bool; cdecl;                          external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_region_contains3(const self: sk_region_t; x, y: int32_t): _bool; cdecl;                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_region_create(): sk_region_t; cdecl;                                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_region_create2(const region: sk_region_t): sk_region_t; cdecl;                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_region_destroy(self: sk_region_t); cdecl;                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_region_get_boundary_path(const self: sk_region_t): sk_path_t; cdecl;                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_region_get_bounds(const self: sk_region_t; out result: sk_irect_t); cdecl;                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_region_intersects(const self, region: sk_region_t): _bool; cdecl;                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_region_intersects2(const self: sk_region_t; const rect: psk_irect_t): _bool; cdecl;                        external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_region_is_complex(const self: sk_region_t): _bool; cdecl;                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_region_is_empty(const self: sk_region_t): _bool; cdecl;                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_region_is_equal(const self, region: sk_region_t): _bool; cdecl;                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_region_is_rect(const self: sk_region_t): _bool; cdecl;                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_region_op(self: sk_region_t; const region: sk_region_t; op: sk_regionop_t): _bool; cdecl;                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_region_op2(self: sk_region_t; const rect: psk_irect_t; op: sk_regionop_t): _bool; cdecl;                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_region_quick_contains(const self: sk_region_t; const rect: psk_irect_t): _bool; cdecl;                     external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_region_quick_reject(const self, region: sk_region_t): _bool; cdecl;                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_region_quick_reject2(const self: sk_region_t; const rect: psk_irect_t): _bool; cdecl;                      external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_region_set_empty(self: sk_region_t); cdecl;                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_region_set_path(self: sk_region_t; const path: sk_path_t; const clip: sk_region_t): _bool; cdecl;          external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_region_set_rect(self: sk_region_t; const rect: psk_irect_t): _bool; cdecl;                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_region_set_rects(self: sk_region_t; const rects: psk_irect_t; count: int32_t): _bool; cdecl;               external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_region_translate(self: sk_region_t; x, y: int32_t); cdecl;                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_regioncliperator_create(const region: sk_region_t; const clip: psk_irect_t): sk_regioncliperator_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_regioncliperator_destroy(self: sk_regioncliperator_t); cdecl;                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_regioncliperator_get_current(const self: sk_regioncliperator_t; out result: sk_irect_t); cdecl;            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_regioncliperator_move_next(self: sk_regioncliperator_t): _bool; cdecl;                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_regioniterator_create(const region: sk_region_t): sk_regioniterator_t; cdecl;                              external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_regioniterator_destroy(self: sk_regioniterator_t); cdecl;                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_regioniterator_get_current(const self: sk_regioniterator_t; out result: sk_irect_t); cdecl;                external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_regioniterator_move_next(self: sk_regioniterator_t): _bool; cdecl;                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_regionspanerator_create(const region: sk_region_t; y, left, right: int32_t): sk_regionspanerator_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_regionspanerator_destroy(self: sk_regionspanerator_t); cdecl;                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_regionspanerator_next(self: sk_regionspanerator_t; out elem: sk_ipoint_t): _bool; cdecl;                   external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_rrect.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_rrect_contains         := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_contains'));
  sk4d_rrect_create           := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_create'));
  sk4d_rrect_create2          := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_create2'));
  sk4d_rrect_deflate          := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_deflate'));
  sk4d_rrect_destroy          := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_destroy'));
  sk4d_rrect_get_height       := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_get_height'));
  sk4d_rrect_get_radii        := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_get_radii'));
  sk4d_rrect_get_rect         := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_get_rect'));
  sk4d_rrect_get_simple_radii := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_get_simple_radii'));
  sk4d_rrect_get_width        := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_get_width'));
  sk4d_rrect_inflate          := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_inflate'));
  sk4d_rrect_is_complex       := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_is_complex'));
  sk4d_rrect_is_empty         := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_is_empty'));
  sk4d_rrect_is_equal         := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_is_equal'));
  sk4d_rrect_is_nine_patch    := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_is_nine_patch'));
  sk4d_rrect_is_oval          := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_is_oval'));
  sk4d_rrect_is_rect          := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_is_rect'));
  sk4d_rrect_is_simple        := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_is_simple'));
  sk4d_rrect_is_valid         := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_is_valid'));
  sk4d_rrect_offset           := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_offset'));
  sk4d_rrect_set_empty        := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_set_empty'));
  sk4d_rrect_set_nine_patch   := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_set_nine_patch'));
  sk4d_rrect_set_oval         := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_set_oval'));
  sk4d_rrect_set_rect         := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_set_rect'));
  sk4d_rrect_set_rect2        := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_set_rect2'));
  sk4d_rrect_set_rect3        := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_set_rect3'));
  sk4d_rrect_transform        := GetProcAddress(LibraryHandle, PChar('sk4d_rrect_transform'));
{$ELSE}
function  sk4d_rrect_contains(const self: sk_rrect_t; const rect: psk_rect_t): _bool; cdecl;                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_rrect_create(): sk_rrect_t; cdecl;                                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_rrect_create2(const rrect: sk_rrect_t): sk_rrect_t; cdecl;                                                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_rrect_deflate(self: sk_rrect_t; dx, dy: float); cdecl;                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_rrect_destroy(self: sk_rrect_t); cdecl;                                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_rrect_get_height(const self: sk_rrect_t): float; cdecl;                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_rrect_get_radii(const self: sk_rrect_t; corner: sk_rrectcorner_t; out result: sk_vector_t); cdecl;                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_rrect_get_rect(const self: sk_rrect_t; out result: sk_rect_t); cdecl;                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_rrect_get_simple_radii(const self: sk_rrect_t; out result: sk_vector_t); cdecl;                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_rrect_get_width(const self: sk_rrect_t): float; cdecl;                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_rrect_inflate(self: sk_rrect_t; dx, dy: float); cdecl;                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_rrect_is_complex(const self: sk_rrect_t): _bool; cdecl;                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_rrect_is_empty(const self: sk_rrect_t): _bool; cdecl;                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_rrect_is_equal(const self, rrect: sk_rrect_t): _bool; cdecl;                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_rrect_is_nine_patch(const self: sk_rrect_t): _bool; cdecl;                                                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_rrect_is_oval(const self: sk_rrect_t): _bool; cdecl;                                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_rrect_is_rect(const self: sk_rrect_t): _bool; cdecl;                                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_rrect_is_simple(const self: sk_rrect_t): _bool; cdecl;                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_rrect_is_valid(const self: sk_rrect_t): _bool; cdecl;                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_rrect_offset(self: sk_rrect_t; dx, dy: float); cdecl;                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_rrect_set_empty(self: sk_rrect_t); cdecl;                                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_rrect_set_nine_patch(self: sk_rrect_t; const rect: psk_rect_t; radius_left, radius_top, radius_right, radius_bottom: float); cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_rrect_set_oval(self: sk_rrect_t; const rect: psk_rect_t); cdecl;                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_rrect_set_rect(self: sk_rrect_t; const rect: psk_rect_t); cdecl;                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_rrect_set_rect2(self: sk_rrect_t; const rect: psk_rect_t; const radii: psk_vector_t); cdecl;                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_rrect_set_rect3(self: sk_rrect_t; const rect: psk_rect_t; radius_x, radius_y: float); cdecl;                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_rrect_transform(const self: sk_rrect_t; const matrix: psk_matrix_t): sk_rrect_t; cdecl;                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_runtimeeffect.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_runtimeblendbuilder_create           := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeblendbuilder_create'));
  sk4d_runtimeblendbuilder_destroy          := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeblendbuilder_destroy'));
  sk4d_runtimeblendbuilder_make_blender     := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeblendbuilder_make_blender'));
  sk4d_runtimeeffect_get_child_count        := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeeffect_get_child_count'));
  sk4d_runtimeeffect_get_child_name         := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeeffect_get_child_name'));
  sk4d_runtimeeffect_get_child_type         := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeeffect_get_child_type'));
  sk4d_runtimeeffect_get_uniform_count      := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeeffect_get_uniform_count'));
  sk4d_runtimeeffect_get_uniform_data_size  := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeeffect_get_uniform_data_size'));
  sk4d_runtimeeffect_get_uniform_name       := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeeffect_get_uniform_name'));
  sk4d_runtimeeffect_get_uniform_offset     := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeeffect_get_uniform_offset'));
  sk4d_runtimeeffect_get_uniform_type       := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeeffect_get_uniform_type'));
  sk4d_runtimeeffect_get_uniform_type_count := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeeffect_get_uniform_type_count'));
  sk4d_runtimeeffect_index_of_child         := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeeffect_index_of_child'));
  sk4d_runtimeeffect_index_of_uniform       := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeeffect_index_of_uniform'));
  sk4d_runtimeeffect_make_blender           := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeeffect_make_blender'));
  sk4d_runtimeeffect_make_color_filter      := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeeffect_make_color_filter'));
  sk4d_runtimeeffect_make_for_blender       := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeeffect_make_for_blender'));
  sk4d_runtimeeffect_make_for_color_filter  := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeeffect_make_for_color_filter'));
  sk4d_runtimeeffect_make_for_shader        := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeeffect_make_for_shader'));
  sk4d_runtimeeffect_make_image             := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeeffect_make_image'));
  sk4d_runtimeeffect_make_shader            := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeeffect_make_shader'));
  sk4d_runtimeeffectbuilder_set_child       := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeeffectbuilder_set_child'));
  sk4d_runtimeeffectbuilder_set_child2      := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeeffectbuilder_set_child2'));
  sk4d_runtimeeffectbuilder_set_child3      := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeeffectbuilder_set_child3'));
  sk4d_runtimeeffectbuilder_set_uniform     := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeeffectbuilder_set_uniform'));
  sk4d_runtimeeffectbuilder_get_effect      := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeeffectbuilder_get_effect'));
  sk4d_runtimeshaderbuilder_create          := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeshaderbuilder_create'));
  sk4d_runtimeshaderbuilder_destroy         := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeshaderbuilder_destroy'));
  sk4d_runtimeshaderbuilder_make_image      := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeshaderbuilder_make_image'));
  sk4d_runtimeshaderbuilder_make_shader     := GetProcAddress(LibraryHandle, PChar('sk4d_runtimeshaderbuilder_make_shader'));
{$ELSE}
function  sk4d_runtimeblendbuilder_create(effect: sk_runtimeeffect_t): sk_runtimeblendbuilder_t; cdecl;                                                                                                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_runtimeblendbuilder_destroy(self: sk_runtimeblendbuilder_t); cdecl;                                                                                                                                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_runtimeblendbuilder_make_blender(self: sk_runtimeblendbuilder_t): sk_blender_t; cdecl;                                                                                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_runtimeeffect_get_child_count(const self: sk_runtimeeffect_t): int32_t; cdecl;                                                                                                                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_runtimeeffect_get_child_name(const self: sk_runtimeeffect_t; index: int32_t): sk_string_t; cdecl;                                                                                                                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_runtimeeffect_get_child_type(const self: sk_runtimeeffect_t; index: int32_t): sk_runtimeeffectchildtype_t; cdecl;                                                                                                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_runtimeeffect_get_uniform_count(const self: sk_runtimeeffect_t): int32_t; cdecl;                                                                                                                                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_runtimeeffect_get_uniform_data_size(const self: sk_runtimeeffect_t): size_t; cdecl;                                                                                                                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_runtimeeffect_get_uniform_name(const self: sk_runtimeeffect_t; index: int32_t): sk_string_t; cdecl;                                                                                                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_runtimeeffect_get_uniform_offset(const self: sk_runtimeeffect_t; index: int32_t): size_t; cdecl;                                                                                                                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_runtimeeffect_get_uniform_type(const self: sk_runtimeeffect_t; index: int32_t): sk_runtimeeffectuniformtype_t; cdecl;                                                                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_runtimeeffect_get_uniform_type_count(const self: sk_runtimeeffect_t; index: int32_t): int32_t; cdecl;                                                                                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_runtimeeffect_index_of_child(const self: sk_runtimeeffect_t; const name: MarshaledAString): int32_t; cdecl;                                                                                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_runtimeeffect_index_of_uniform(const self: sk_runtimeeffect_t; const name: MarshaledAString): int32_t; cdecl;                                                                                                                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_runtimeeffect_make_blender(const self: sk_runtimeeffect_t; const uniforms: Pointer; children: psk_flattenable_t): sk_blender_t; cdecl;                                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_runtimeeffect_make_color_filter(const self: sk_runtimeeffect_t; const uniforms: Pointer; children: psk_flattenable_t): sk_colorfilter_t; cdecl;                                                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_runtimeeffect_make_for_blender(const sksl: MarshaledAString; error_text: sk_string_t): sk_runtimeeffect_t; cdecl;                                                                                                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_runtimeeffect_make_for_color_filter(const sksl: MarshaledAString; error_text: sk_string_t): sk_runtimeeffect_t; cdecl;                                                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_runtimeeffect_make_for_shader(const sksl: MarshaledAString; error_text: sk_string_t): sk_runtimeeffect_t; cdecl;                                                                                                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_runtimeeffect_make_image(const self: sk_runtimeeffect_t; context: gr_directcontext_t; const uniforms: Pointer; children: psk_flattenable_t; const local_matrix: psk_matrix_t; const image_info: psk_imageinfo_t; mipmapped: _bool): sk_image_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_runtimeeffect_make_shader(const self: sk_runtimeeffect_t; const uniforms: Pointer; children: psk_flattenable_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_runtimeeffectbuilder_set_child(self: sk_runtimeeffectbuilder_t; const name: MarshaledAString; shader: sk_shader_t); cdecl;                                                                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_runtimeeffectbuilder_set_child2(self: sk_runtimeeffectbuilder_t; const name: MarshaledAString; color_filter: sk_colorfilter_t); cdecl;                                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_runtimeeffectbuilder_set_child3(self: sk_runtimeeffectbuilder_t; const name: MarshaledAString; blender: sk_blender_t); cdecl;                                                                                                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_runtimeeffectbuilder_set_uniform(self: sk_runtimeeffectbuilder_t; const name: MarshaledAString; const data: Pointer); cdecl;                                                                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_runtimeeffectbuilder_get_effect(const self: sk_runtimeeffectbuilder_t): sk_runtimeeffect_t; cdecl;                                                                                                                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_runtimeshaderbuilder_create(effect: sk_runtimeeffect_t): sk_runtimeshaderbuilder_t; cdecl;                                                                                                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_runtimeshaderbuilder_destroy(self: sk_runtimeshaderbuilder_t); cdecl;                                                                                                                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_runtimeshaderbuilder_make_image(self: sk_runtimeshaderbuilder_t; context: gr_directcontext_t; const local_matrix: psk_matrix_t; const image_info: psk_imageinfo_t; mipmapped: _bool): sk_image_t; cdecl;                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_runtimeshaderbuilder_make_shader(self: sk_runtimeshaderbuilder_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;                                                                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_shader.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_shader_make_blend                       := GetProcAddress(LibraryHandle, PChar('sk4d_shader_make_blend'));
  sk4d_shader_make_color                       := GetProcAddress(LibraryHandle, PChar('sk4d_shader_make_color'));
  sk4d_shader_make_color2                      := GetProcAddress(LibraryHandle, PChar('sk4d_shader_make_color2'));
  sk4d_shader_make_empty                       := GetProcAddress(LibraryHandle, PChar('sk4d_shader_make_empty'));
  sk4d_shader_make_gradient_linear             := GetProcAddress(LibraryHandle, PChar('sk4d_shader_make_gradient_linear'));
  sk4d_shader_make_gradient_linear2            := GetProcAddress(LibraryHandle, PChar('sk4d_shader_make_gradient_linear2'));
  sk4d_shader_make_gradient_radial             := GetProcAddress(LibraryHandle, PChar('sk4d_shader_make_gradient_radial'));
  sk4d_shader_make_gradient_radial2            := GetProcAddress(LibraryHandle, PChar('sk4d_shader_make_gradient_radial2'));
  sk4d_shader_make_gradient_sweep              := GetProcAddress(LibraryHandle, PChar('sk4d_shader_make_gradient_sweep'));
  sk4d_shader_make_gradient_sweep2             := GetProcAddress(LibraryHandle, PChar('sk4d_shader_make_gradient_sweep2'));
  sk4d_shader_make_gradient_two_point_conical  := GetProcAddress(LibraryHandle, PChar('sk4d_shader_make_gradient_two_point_conical'));
  sk4d_shader_make_gradient_two_point_conical2 := GetProcAddress(LibraryHandle, PChar('sk4d_shader_make_gradient_two_point_conical2'));
  sk4d_shader_make_perlin_noise_fractal_noise  := GetProcAddress(LibraryHandle, PChar('sk4d_shader_make_perlin_noise_fractal_noise'));
  sk4d_shader_make_perlin_noise_turbulence     := GetProcAddress(LibraryHandle, PChar('sk4d_shader_make_perlin_noise_turbulence'));
  sk4d_shader_make_with_color_filter           := GetProcAddress(LibraryHandle, PChar('sk4d_shader_make_with_color_filter'));
  sk4d_shader_make_with_local_matrix           := GetProcAddress(LibraryHandle, PChar('sk4d_shader_make_with_local_matrix'));
{$ELSE}
function sk4d_shader_make_blend(mode: sk_blendmode_t; dest, src: sk_shader_t): sk_shader_t; cdecl;                                                                                                                                                                                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_shader_make_color(color: sk_color_t): sk_shader_t; cdecl;                                                                                                                                                                                                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_shader_make_color2(const color: psk_color4f_t; color_space: sk_colorspace_t): sk_shader_t; cdecl;                                                                                                                                                                                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_shader_make_empty(): sk_shader_t; cdecl;                                                                                                                                                                                                                                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_shader_make_gradient_linear(const points: psk_point_t; const colors: psk_color_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;                                                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_shader_make_gradient_linear2(const points: psk_point_t; const colors: psk_color4f_t; color_space: sk_colorspace_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_shader_make_gradient_radial(const center: psk_point_t; radius: float; const colors: psk_color_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;                                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_shader_make_gradient_radial2(const center: psk_point_t; radius: float; const colors: psk_color4f_t; color_space: sk_colorspace_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_shader_make_gradient_sweep(center_x, center_y: float; const colors: psk_color_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; start_angle, end_angle: float; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_shader_make_gradient_sweep2(center_x, center_y: float; const colors: psk_color4f_t; color_space: sk_colorspace_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; start_angle, end_angle: float; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_shader_make_gradient_two_point_conical(const start: psk_point_t; start_radius: float; const &end: psk_point_t; end_radius: float; const colors: psk_color_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_shader_make_gradient_two_point_conical2(const start: psk_point_t; start_radius: float; const &end: psk_point_t; end_radius: float; const colors: psk_color4f_t; color_space: sk_colorspace_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_shader_make_perlin_noise_fractal_noise(base_frequency_x, base_frequency_y: float; num_octaves: int32_t; seed: float; const tile_size: psk_isize_t): sk_shader_t; cdecl;                                                                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_shader_make_perlin_noise_turbulence(base_frequency_x, base_frequency_y: float; num_octaves: int32_t; seed: float; const tile_size: psk_isize_t): sk_shader_t; cdecl;                                                                                                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_shader_make_with_color_filter(const self: sk_shader_t; color_filter: sk_colorfilter_t): sk_shader_t; cdecl;                                                                                                                                                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
function sk4d_shader_make_with_local_matrix(const self: sk_shader_t; const local_matrix: psk_matrix_t): sk_shader_t; cdecl;                                                                                                                                                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_stream.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_streamadapter_create     := GetProcAddress(LibraryHandle, PChar('sk4d_streamadapter_create'));
  sk4d_streamadapter_destroy    := GetProcAddress(LibraryHandle, PChar('sk4d_streamadapter_destroy'));
  sk4d_streamadapter_set_procs  := GetProcAddress(LibraryHandle, PChar('sk4d_streamadapter_set_procs'));
  sk4d_wstreamadapter_create    := GetProcAddress(LibraryHandle, PChar('sk4d_wstreamadapter_create'));
  sk4d_wstreamadapter_destroy   := GetProcAddress(LibraryHandle, PChar('sk4d_wstreamadapter_destroy'));
  sk4d_wstreamadapter_set_procs := GetProcAddress(LibraryHandle, PChar('sk4d_wstreamadapter_set_procs'));
{$ELSE}
function  sk4d_streamadapter_create(context: Pointer): sk_streamadapter_t; cdecl;        external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_streamadapter_destroy(self: sk_streamadapter_t); cdecl;                   external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_streamadapter_set_procs(const procs: psk_streamadapter_procs_t); cdecl;   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_wstreamadapter_create(context: Pointer): sk_wstreamadapter_t; cdecl;      external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_wstreamadapter_destroy(self: sk_wstreamadapter_t); cdecl;                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_wstreamadapter_set_procs(const procs: psk_wstreamadapter_procs_t); cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_string.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_string_create   := GetProcAddress(LibraryHandle, PChar('sk4d_string_create'));
  sk4d_string_destroy  := GetProcAddress(LibraryHandle, PChar('sk4d_string_destroy'));
  sk4d_string_get_text := GetProcAddress(LibraryHandle, PChar('sk4d_string_get_text'));
{$ELSE}
function  sk4d_string_create(): sk_string_t; cdecl;                               external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_string_destroy(self: sk_string_t); cdecl;                          external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_string_get_text(const self: sk_string_t): MarshaledAString; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_surface.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_surface_draw                    := GetProcAddress(LibraryHandle, PChar('sk4d_surface_draw'));
  sk4d_surface_flush                   := GetProcAddress(LibraryHandle, PChar('sk4d_surface_flush'));
  sk4d_surface_flush_and_submit        := GetProcAddress(LibraryHandle, PChar('sk4d_surface_flush_and_submit'));
  sk4d_surface_get_canvas              := GetProcAddress(LibraryHandle, PChar('sk4d_surface_get_canvas'));
  sk4d_surface_get_props               := GetProcAddress(LibraryHandle, PChar('sk4d_surface_get_props'));
  sk4d_surface_make_from_mtk_view      := GetProcAddress(LibraryHandle, PChar('sk4d_surface_make_from_mtk_view'));
  sk4d_surface_make_from_render_target := GetProcAddress(LibraryHandle, PChar('sk4d_surface_make_from_render_target'));
  sk4d_surface_make_from_texture       := GetProcAddress(LibraryHandle, PChar('sk4d_surface_make_from_texture'));
  sk4d_surface_make_image_snapshot     := GetProcAddress(LibraryHandle, PChar('sk4d_surface_make_image_snapshot'));
  sk4d_surface_make_image_snapshot2    := GetProcAddress(LibraryHandle, PChar('sk4d_surface_make_image_snapshot2'));
  sk4d_surface_make_raster             := GetProcAddress(LibraryHandle, PChar('sk4d_surface_make_raster'));
  sk4d_surface_make_raster_direct      := GetProcAddress(LibraryHandle, PChar('sk4d_surface_make_raster_direct'));
  sk4d_surface_make_render_target      := GetProcAddress(LibraryHandle, PChar('sk4d_surface_make_render_target'));
  sk4d_surface_peek_pixels             := GetProcAddress(LibraryHandle, PChar('sk4d_surface_peek_pixels'));
  sk4d_surface_read_pixels             := GetProcAddress(LibraryHandle, PChar('sk4d_surface_read_pixels'));
  sk4d_surface_wait                    := GetProcAddress(LibraryHandle, PChar('sk4d_surface_wait'));
  sk4d_surface_write_pixels            := GetProcAddress(LibraryHandle, PChar('sk4d_surface_write_pixels'));
{$ELSE}
procedure sk4d_surface_draw(self: sk_surface_t; canvas: sk_canvas_t; x, y: float; paint: sk_paint_t); cdecl;                                                                                                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_surface_flush(self: sk_surface_t); cdecl;                                                                                                                                                                                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_surface_flush_and_submit(self: sk_surface_t; semaphores: pgr_backendsemaphore_t; count: int32_t; const new_state: gr_backendsurfacemutablestate_t; sync_cpu: _bool); cdecl;                                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_surface_get_canvas(self: sk_surface_t): sk_canvas_t; cdecl;                                                                                                                                                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_surface_get_props(const self: sk_surface_t; out result: sk_surfaceprops_t); cdecl;                                                                                                                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_surface_make_from_mtk_view(context: gr_directcontext_t; layer: gr_mtl_handle_t; origin: gr_surfaceorigin_t; sample_count: int32_t; color_type: sk_colortype_t; color_space: sk_colorspace_t; const props: psk_surfaceprops_t): sk_surface_t; cdecl;            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_surface_make_from_render_target(context: gr_directcontext_t; const render_target: gr_backendrendertarget_t; origin: gr_surfaceorigin_t; color_type: sk_colortype_t; color_space: sk_colorspace_t; const props: psk_surfaceprops_t): sk_surface_t; cdecl;       external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_surface_make_from_texture(context: gr_directcontext_t; const texture: gr_backendtexture_t; origin: gr_surfaceorigin_t; sample_count: int32_t; color_type: sk_colortype_t; color_space: sk_colorspace_t; const props: psk_surfaceprops_t): sk_surface_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_surface_make_image_snapshot(self: sk_surface_t): sk_image_t; cdecl;                                                                                                                                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_surface_make_image_snapshot2(self: sk_surface_t; const bounds: psk_irect_t): sk_image_t; cdecl;                                                                                                                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_surface_make_raster(const image_info: psk_imageinfo_t; row_bytes: size_t; const props: psk_surfaceprops_t): sk_surface_t; cdecl;                                                                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_surface_make_raster_direct(const pixmap: sk_pixmap_t; proc: sk_surface_raster_release_proc; proc_context: Pointer; const props: psk_surfaceprops_t): sk_surface_t; cdecl;                                                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_surface_make_render_target(context: gr_directcontext_t; is_budgeted: _bool; const image_info: psk_imageinfo_t; sample_count: int32_t; origin: gr_surfaceorigin_t; const props: psk_surfaceprops_t; should_create_with_mips: _bool): sk_surface_t; cdecl;       external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_surface_peek_pixels(self: sk_surface_t): sk_pixmap_t; cdecl;                                                                                                                                                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_surface_read_pixels(self: sk_surface_t; const dest: sk_pixmap_t; src_x, src_y: int32_t): _bool; cdecl;                                                                                                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_surface_wait(self: sk_surface_t; const semaphores: pgr_backendsemaphore_t; count: int32_t); cdecl;                                                                                                                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_surface_write_pixels(self: sk_surface_t; const src: sk_pixmap_t; dest_x, dest_y: int32_t); cdecl;                                                                                                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_svgcanvas.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_svgcanvas_make := GetProcAddress(LibraryHandle, PChar('sk4d_svgcanvas_make'));
{$ELSE}
function sk4d_svgcanvas_make(const bounds: psk_rect_t; w_stream: sk_wstream_t; flags: uint32_t): sk_canvas_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_textblob.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_textblob_get_intercepts                         := GetProcAddress(LibraryHandle, PChar('sk4d_textblob_get_intercepts'));
  sk4d_textblob_make_from_text                         := GetProcAddress(LibraryHandle, PChar('sk4d_textblob_make_from_text'));
  sk4d_textblob_make_from_text_horizontally_positioned := GetProcAddress(LibraryHandle, PChar('sk4d_textblob_make_from_text_horizontally_positioned'));
  sk4d_textblob_make_from_text_positioned              := GetProcAddress(LibraryHandle, PChar('sk4d_textblob_make_from_text_positioned'));
  sk4d_textblob_make_from_text_transform               := GetProcAddress(LibraryHandle, PChar('sk4d_textblob_make_from_text_transform'));
  sk4d_textblob_ref                                    := GetProcAddress(LibraryHandle, PChar('sk4d_textblob_ref'));
  sk4d_textblob_unref                                  := GetProcAddress(LibraryHandle, PChar('sk4d_textblob_unref'));
{$ELSE}
function  sk4d_textblob_get_intercepts(const self: sk_textblob_t; const bounds: pfloat; result: pfloat; const paint: sk_paint_t): int32_t; cdecl;                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_textblob_make_from_text(const text: Pointer; size: size_t; const font: sk_font_t; encoding: sk_textencoding_t): sk_textblob_t; cdecl;                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_textblob_make_from_text_horizontally_positioned(const text: Pointer; size: size_t; const x_positions: pfloat; y: float; const font: sk_font_t; encoding: sk_textencoding_t): sk_textblob_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_textblob_make_from_text_positioned(const text: Pointer; size: size_t; const positions: psk_point_t; const font: sk_font_t; encoding: sk_textencoding_t): sk_textblob_t; cdecl;                     external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_textblob_make_from_text_transform(const text: Pointer; size: size_t; const matrices: psk_rotationscalematrix_t; const font: sk_font_t; encoding: sk_textencoding_t): sk_textblob_t; cdecl;         external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textblob_ref(const self: sk_textblob_t); cdecl;                                                                                                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textblob_unref(const self: sk_textblob_t); cdecl;                                                                                                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_tracememorydump.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_tracememorydumpbaseclass_create    := GetProcAddress(LibraryHandle, PChar('sk4d_tracememorydumpbaseclass_create'));
  sk4d_tracememorydumpbaseclass_destroy   := GetProcAddress(LibraryHandle, PChar('sk4d_tracememorydumpbaseclass_destroy'));
  sk4d_tracememorydumpbaseclass_set_procs := GetProcAddress(LibraryHandle, PChar('sk4d_tracememorydumpbaseclass_set_procs'));
{$ELSE}
function  sk4d_tracememorydumpbaseclass_create(detailed_dump, dump_wrapped_objects: _bool; context: Pointer): sk_tracememorydumpbaseclass_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_tracememorydumpbaseclass_destroy(self: sk_tracememorydumpbaseclass_t); cdecl;                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_tracememorydumpbaseclass_set_procs(const procs: psk_tracememorydumpbaseclass_procs_t); cdecl;                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_typeface.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_typeface_get_family_name  := GetProcAddress(LibraryHandle, PChar('sk4d_typeface_get_family_name'));
  sk4d_typeface_get_slant        := GetProcAddress(LibraryHandle, PChar('sk4d_typeface_get_slant'));
  sk4d_typeface_get_style        := GetProcAddress(LibraryHandle, PChar('sk4d_typeface_get_style'));
  sk4d_typeface_get_weight       := GetProcAddress(LibraryHandle, PChar('sk4d_typeface_get_weight'));
  sk4d_typeface_get_width        := GetProcAddress(LibraryHandle, PChar('sk4d_typeface_get_width'));
  sk4d_typeface_make_default     := GetProcAddress(LibraryHandle, PChar('sk4d_typeface_make_default'));
  sk4d_typeface_make_from_file   := GetProcAddress(LibraryHandle, PChar('sk4d_typeface_make_from_file'));
  sk4d_typeface_make_from_stream := GetProcAddress(LibraryHandle, PChar('sk4d_typeface_make_from_stream'));
  sk4d_typeface_make_from_name   := GetProcAddress(LibraryHandle, PChar('sk4d_typeface_make_from_name'));
{$ELSE}
function  sk4d_typeface_get_family_name(const self: sk_typeface_t): sk_string_t; cdecl;                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_typeface_get_slant(const self: sk_typeface_t): sk_fontslant_t; cdecl;                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_typeface_get_style(const self: sk_typeface_t; out result: sk_fontstyle_t); cdecl;                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_typeface_get_weight(const self: sk_typeface_t): int32_t; cdecl;                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_typeface_get_width(const self: sk_typeface_t): int32_t; cdecl;                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_typeface_make_default(): sk_typeface_t; cdecl;                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_typeface_make_from_file(const file_name: MarshaledAString; ttc_index: int32_t): sk_typeface_t; cdecl;             external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_typeface_make_from_stream(stream: sk_stream_t; ttc_index: int32_t): sk_typeface_t; cdecl;                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_typeface_make_from_name(const family_name: MarshaledAString; const style: psk_fontstyle_t): sk_typeface_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ include/c/sk4d_vertices.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_vertices_make_copy := GetProcAddress(LibraryHandle, PChar('sk4d_vertices_make_copy'));
  sk4d_vertices_ref       := GetProcAddress(LibraryHandle, PChar('sk4d_vertices_ref'));
  sk4d_vertices_unref     := GetProcAddress(LibraryHandle, PChar('sk4d_vertices_unref'));
{$ELSE}
function  sk4d_vertices_make_copy(vertex_mode: sk_vertexmode_t; vertex_count: int32_t; const positions, textures: psk_point_t; const colors: psk_color_t; index_count: int32_t; const indices: puint16_t): sk_vertices_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_vertices_ref(const self: sk_vertices_t); cdecl;                                                                                                                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_vertices_unref(const self: sk_vertices_t); cdecl;                                                                                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ modules/particles/include/sk4d_particleeffect.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_particleeffect_get_position           := GetProcAddress(LibraryHandle, PChar('sk4d_particleeffect_get_position'));
  sk4d_particleeffect_get_rate               := GetProcAddress(LibraryHandle, PChar('sk4d_particleeffect_get_rate'));
  sk4d_particleeffect_get_uniform            := GetProcAddress(LibraryHandle, PChar('sk4d_particleeffect_get_uniform'));
  sk4d_particleeffect_get_uniform_count      := GetProcAddress(LibraryHandle, PChar('sk4d_particleeffect_get_uniform_count'));
  sk4d_particleeffect_get_uniform_data       := GetProcAddress(LibraryHandle, PChar('sk4d_particleeffect_get_uniform_data'));
  sk4d_particleeffect_get_uniform_data_count := GetProcAddress(LibraryHandle, PChar('sk4d_particleeffect_get_uniform_data_count'));
  sk4d_particleeffect_get_uniform_name       := GetProcAddress(LibraryHandle, PChar('sk4d_particleeffect_get_uniform_name'));
  sk4d_particleeffect_init                   := GetProcAddress(LibraryHandle, PChar('sk4d_particleeffect_init'));
  sk4d_particleeffect_make_from_file         := GetProcAddress(LibraryHandle, PChar('sk4d_particleeffect_make_from_file'));
  sk4d_particleeffect_make_from_stream       := GetProcAddress(LibraryHandle, PChar('sk4d_particleeffect_make_from_stream'));
  sk4d_particleeffect_render                 := GetProcAddress(LibraryHandle, PChar('sk4d_particleeffect_render'));
  sk4d_particleeffect_set_position           := GetProcAddress(LibraryHandle, PChar('sk4d_particleeffect_set_position'));
  sk4d_particleeffect_set_rate               := GetProcAddress(LibraryHandle, PChar('sk4d_particleeffect_set_rate'));
  sk4d_particleeffect_set_uniform            := GetProcAddress(LibraryHandle, PChar('sk4d_particleeffect_set_uniform'));
  sk4d_particleeffect_start                  := GetProcAddress(LibraryHandle, PChar('sk4d_particleeffect_start'));
  sk4d_particleeffect_update                 := GetProcAddress(LibraryHandle, PChar('sk4d_particleeffect_update'));
{$ELSE}
procedure sk4d_particleeffect_get_position(const self: sk_particleeffect_t; out result: sk_point_t); cdecl;                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_particleeffect_get_rate(const self: sk_particleeffect_t): float; cdecl;                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_particleeffect_get_uniform(const self: sk_particleeffect_t; index: size_t; out result: sk_particleuniform_t); cdecl;                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_particleeffect_get_uniform_count(const self: sk_particleeffect_t): size_t; cdecl;                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_particleeffect_get_uniform_data(self: sk_particleeffect_t): pfloat; cdecl;                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_particleeffect_get_uniform_data_count(const self: sk_particleeffect_t): int32_t; cdecl;                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_particleeffect_get_uniform_name(const self: sk_particleeffect_t; index: size_t): sk_string_t; cdecl;                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_particleeffect_init(); cdecl;                                                                                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_particleeffect_make_from_file(const file_name: MarshaledAString): sk_particleeffect_t; cdecl;                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_particleeffect_make_from_stream(stream: sk_stream_t; resource_provider: sk_resourceprovider_t): sk_particleeffect_t; cdecl;            external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_particleeffect_render(self: sk_particleeffect_t; canvas: sk_canvas_t); cdecl;                                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_particleeffect_set_position(self: sk_particleeffect_t; const value: psk_point_t); cdecl;                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_particleeffect_set_rate(self: sk_particleeffect_t; value: float); cdecl;                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_particleeffect_set_uniform(self: sk_particleeffect_t; const name: MarshaledAString; const data: pfloat; count: int32_t): _bool; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_particleeffect_start(self: sk_particleeffect_t; now: _double; looping: _bool); cdecl;                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_particleeffect_update(self: sk_particleeffect_t; now: _double); cdecl;                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ modules/skottie/include/sk4d_skottie.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_skottieanimation_get_duration     := GetProcAddress(LibraryHandle, PChar('sk4d_skottieanimation_get_duration'));
  sk4d_skottieanimation_get_fps          := GetProcAddress(LibraryHandle, PChar('sk4d_skottieanimation_get_fps'));
  sk4d_skottieanimation_get_in_point     := GetProcAddress(LibraryHandle, PChar('sk4d_skottieanimation_get_in_point'));
  sk4d_skottieanimation_get_out_point    := GetProcAddress(LibraryHandle, PChar('sk4d_skottieanimation_get_out_point'));
  sk4d_skottieanimation_get_size         := GetProcAddress(LibraryHandle, PChar('sk4d_skottieanimation_get_size'));
  sk4d_skottieanimation_get_version      := GetProcAddress(LibraryHandle, PChar('sk4d_skottieanimation_get_version'));
  sk4d_skottieanimation_make_from_file   := GetProcAddress(LibraryHandle, PChar('sk4d_skottieanimation_make_from_file'));
  sk4d_skottieanimation_make_from_stream := GetProcAddress(LibraryHandle, PChar('sk4d_skottieanimation_make_from_stream'));
  sk4d_skottieanimation_ref              := GetProcAddress(LibraryHandle, PChar('sk4d_skottieanimation_ref'));
  sk4d_skottieanimation_render           := GetProcAddress(LibraryHandle, PChar('sk4d_skottieanimation_render'));
  sk4d_skottieanimation_seek_frame       := GetProcAddress(LibraryHandle, PChar('sk4d_skottieanimation_seek_frame'));
  sk4d_skottieanimation_seek_frame_time  := GetProcAddress(LibraryHandle, PChar('sk4d_skottieanimation_seek_frame_time'));
  sk4d_skottieanimation_unref            := GetProcAddress(LibraryHandle, PChar('sk4d_skottieanimation_unref'));
{$ELSE}
function  sk4d_skottieanimation_get_duration(const self: sk_skottieanimation_t): _double; cdecl;                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_skottieanimation_get_fps(const self: sk_skottieanimation_t): _double; cdecl;                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_skottieanimation_get_in_point(const self: sk_skottieanimation_t): _double; cdecl;                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_skottieanimation_get_out_point(const self: sk_skottieanimation_t): _double; cdecl;                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_skottieanimation_get_size(const self: sk_skottieanimation_t; out result: sk_size_t); cdecl;                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_skottieanimation_get_version(const self: sk_skottieanimation_t): MarshaledAString; cdecl;                                                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_skottieanimation_make_from_file(const file_name: MarshaledAString; font_provider: sk_fontmgr_t): sk_skottieanimation_t; cdecl;                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_skottieanimation_make_from_stream(stream: sk_stream_t; resource_provider: sk_resourceprovider_t; font_provider: sk_fontmgr_t): sk_skottieanimation_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_skottieanimation_ref(const self: sk_skottieanimation_t); cdecl;                                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_skottieanimation_render(const self: sk_skottieanimation_t; canvas: sk_canvas_t; const dest: psk_rect_t; render_flags: uint32_t); cdecl;                      external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_skottieanimation_seek_frame(self: sk_skottieanimation_t; tick: _double); cdecl;                                                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_skottieanimation_seek_frame_time(self: sk_skottieanimation_t; tick: _double); cdecl;                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_skottieanimation_unref(const self: sk_skottieanimation_t); cdecl;                                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ modules/skparagraph/include/sk4d_paragraph.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_paragraph_destroy                          := GetProcAddress(LibraryHandle, PChar('sk4d_paragraph_destroy'));
  sk4d_paragraph_did_exceed_max_lines             := GetProcAddress(LibraryHandle, PChar('sk4d_paragraph_did_exceed_max_lines'));
  sk4d_paragraph_get_alphabetic_baseline          := GetProcAddress(LibraryHandle, PChar('sk4d_paragraph_get_alphabetic_baseline'));
  sk4d_paragraph_get_glyph_position_at_coordinate := GetProcAddress(LibraryHandle, PChar('sk4d_paragraph_get_glyph_position_at_coordinate'));
  sk4d_paragraph_get_height                       := GetProcAddress(LibraryHandle, PChar('sk4d_paragraph_get_height'));
  sk4d_paragraph_get_ideographic_baseline         := GetProcAddress(LibraryHandle, PChar('sk4d_paragraph_get_ideographic_baseline'));
  sk4d_paragraph_get_line_metrics                 := GetProcAddress(LibraryHandle, PChar('sk4d_paragraph_get_line_metrics'));
  sk4d_paragraph_get_longest_line                 := GetProcAddress(LibraryHandle, PChar('sk4d_paragraph_get_longest_line'));
  sk4d_paragraph_get_max_intrinsic_width          := GetProcAddress(LibraryHandle, PChar('sk4d_paragraph_get_max_intrinsic_width'));
  sk4d_paragraph_get_max_width                    := GetProcAddress(LibraryHandle, PChar('sk4d_paragraph_get_max_width'));
  sk4d_paragraph_get_min_intrinsic_width          := GetProcAddress(LibraryHandle, PChar('sk4d_paragraph_get_min_intrinsic_width'));
  sk4d_paragraph_get_rects_for_placeholders       := GetProcAddress(LibraryHandle, PChar('sk4d_paragraph_get_rects_for_placeholders'));
  sk4d_paragraph_get_rects_for_range              := GetProcAddress(LibraryHandle, PChar('sk4d_paragraph_get_rects_for_range'));
  sk4d_paragraph_get_word_boundary                := GetProcAddress(LibraryHandle, PChar('sk4d_paragraph_get_word_boundary'));
  sk4d_paragraph_layout                           := GetProcAddress(LibraryHandle, PChar('sk4d_paragraph_layout'));
  sk4d_paragraph_paint                            := GetProcAddress(LibraryHandle, PChar('sk4d_paragraph_paint'));
  sk4d_paragraph_to_path                          := GetProcAddress(LibraryHandle, PChar('sk4d_paragraph_to_path'));
  sk4d_paragraph_visit                            := GetProcAddress(LibraryHandle, PChar('sk4d_paragraph_visit'));
{$ELSE}
procedure sk4d_paragraph_destroy(self: sk_paragraph_t); cdecl;                                                                                                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paragraph_did_exceed_max_lines(self: sk_paragraph_t): _bool; cdecl;                                                                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paragraph_get_alphabetic_baseline(self: sk_paragraph_t): float; cdecl;                                                                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paragraph_get_glyph_position_at_coordinate(self: sk_paragraph_t; dx, dy: float; out result: sk_positionaffinity_t); cdecl;                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paragraph_get_height(self: sk_paragraph_t): float; cdecl;                                                                                                                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paragraph_get_ideographic_baseline(self: sk_paragraph_t): float; cdecl;                                                                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paragraph_get_line_metrics(self: sk_paragraph_t; result: psk_metrics_t): size_t; cdecl;                                                                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paragraph_get_longest_line(self: sk_paragraph_t): float; cdecl;                                                                                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paragraph_get_max_intrinsic_width(self: sk_paragraph_t): float; cdecl;                                                                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paragraph_get_max_width(self: sk_paragraph_t): float; cdecl;                                                                                                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paragraph_get_min_intrinsic_width(self: sk_paragraph_t): float; cdecl;                                                                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paragraph_get_rects_for_placeholders(self: sk_paragraph_t; result: psk_textbox_t): size_t; cdecl;                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paragraph_get_rects_for_range(self: sk_paragraph_t; start, &end: uint32_t; rect_height_style: sk_rectheightstyle_t; rect_width_style: sk_rectwidthstyle_t; result: psk_textbox_t): size_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paragraph_get_word_boundary(self: sk_paragraph_t; offset: uint32_t; out start, &end: uint32_t); cdecl;                                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paragraph_layout(self: sk_paragraph_t; width: float); cdecl;                                                                                                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paragraph_paint(self: sk_paragraph_t; canvas: sk_canvas_t; x, y: float); cdecl;                                                                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paragraph_to_path(self: sk_paragraph_t): sk_path_t; cdecl;                                                                                                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paragraph_visit(self: sk_paragraph_t; proc: sk_paragraph_visit_proc; proc_context: Pointer); cdecl;                                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ modules/skparagraph/include/sk4d_paragraphbuilder.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_paragraphbuilder_add_placeholder := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphbuilder_add_placeholder'));
  sk4d_paragraphbuilder_add_text        := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphbuilder_add_text'));
  sk4d_paragraphbuilder_build           := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphbuilder_build'));
  sk4d_paragraphbuilder_create          := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphbuilder_create'));
  sk4d_paragraphbuilder_create2         := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphbuilder_create2'));
  sk4d_paragraphbuilder_destroy         := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphbuilder_destroy'));
  sk4d_paragraphbuilder_pop             := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphbuilder_pop'));
  sk4d_paragraphbuilder_push_style      := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphbuilder_push_style'));
{$ELSE}
procedure sk4d_paragraphbuilder_add_placeholder(self: sk_paragraphbuilder_t; const placeholder: psk_placeholderstyle_t); cdecl;                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paragraphbuilder_add_text(self: sk_paragraphbuilder_t; const text: MarshaledAString); cdecl;                                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paragraphbuilder_build(self: sk_paragraphbuilder_t): sk_paragraph_t; cdecl;                                                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paragraphbuilder_create(const paragraph_style: sk_paragraphstyle_t): sk_paragraphbuilder_t; cdecl;                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paragraphbuilder_create2(const paragraph_style: sk_paragraphstyle_t; font_provider: sk_fontmgr_t; enable_font_fallback: _bool): sk_paragraphbuilder_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paragraphbuilder_destroy(self: sk_paragraphbuilder_t); cdecl;                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paragraphbuilder_pop(self: sk_paragraphbuilder_t); cdecl;                                                                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paragraphbuilder_push_style(self: sk_paragraphbuilder_t; const text_style: sk_textstyle_t); cdecl;                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ modules/skparagraph/include/sk4d_paragraphstyle.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_paragraphstyle_create                    := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphstyle_create'));
  sk4d_paragraphstyle_destroy                   := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphstyle_destroy'));
  sk4d_paragraphstyle_disable_hinting           := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphstyle_disable_hinting'));
  sk4d_paragraphstyle_get_ellipsis              := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphstyle_get_ellipsis'));
  sk4d_paragraphstyle_get_height                := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphstyle_get_height'));
  sk4d_paragraphstyle_get_max_lines             := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphstyle_get_max_lines'));
  sk4d_paragraphstyle_get_strut_style           := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphstyle_get_strut_style'));
  sk4d_paragraphstyle_get_text_align            := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphstyle_get_text_align'));
  sk4d_paragraphstyle_get_text_direction        := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphstyle_get_text_direction'));
  sk4d_paragraphstyle_get_text_height_behaviors := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphstyle_get_text_height_behaviors'));
  sk4d_paragraphstyle_get_text_style            := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphstyle_get_text_style'));
  sk4d_paragraphstyle_set_ellipsis              := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphstyle_set_ellipsis'));
  sk4d_paragraphstyle_set_height                := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphstyle_set_height'));
  sk4d_paragraphstyle_set_max_lines             := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphstyle_set_max_lines'));
  sk4d_paragraphstyle_set_strut_style           := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphstyle_set_strut_style'));
  sk4d_paragraphstyle_set_text_align            := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphstyle_set_text_align'));
  sk4d_paragraphstyle_set_text_direction        := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphstyle_set_text_direction'));
  sk4d_paragraphstyle_set_text_height_behaviors := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphstyle_set_text_height_behaviors'));
  sk4d_paragraphstyle_set_text_style            := GetProcAddress(LibraryHandle, PChar('sk4d_paragraphstyle_set_text_style'));
  sk4d_strutstyle_create                        := GetProcAddress(LibraryHandle, PChar('sk4d_strutstyle_create'));
  sk4d_strutstyle_destroy                       := GetProcAddress(LibraryHandle, PChar('sk4d_strutstyle_destroy'));
  sk4d_strutstyle_get_enabled                   := GetProcAddress(LibraryHandle, PChar('sk4d_strutstyle_get_enabled'));
  sk4d_strutstyle_get_font_families             := GetProcAddress(LibraryHandle, PChar('sk4d_strutstyle_get_font_families'));
  sk4d_strutstyle_get_font_size                 := GetProcAddress(LibraryHandle, PChar('sk4d_strutstyle_get_font_size'));
  sk4d_strutstyle_get_font_style                := GetProcAddress(LibraryHandle, PChar('sk4d_strutstyle_get_font_style'));
  sk4d_strutstyle_get_force_height              := GetProcAddress(LibraryHandle, PChar('sk4d_strutstyle_get_force_height'));
  sk4d_strutstyle_get_half_leading              := GetProcAddress(LibraryHandle, PChar('sk4d_strutstyle_get_half_leading'));
  sk4d_strutstyle_get_height_multiplier         := GetProcAddress(LibraryHandle, PChar('sk4d_strutstyle_get_height_multiplier'));
  sk4d_strutstyle_get_leading                   := GetProcAddress(LibraryHandle, PChar('sk4d_strutstyle_get_leading'));
  sk4d_strutstyle_is_equal                      := GetProcAddress(LibraryHandle, PChar('sk4d_strutstyle_is_equal'));
  sk4d_strutstyle_set_enabled                   := GetProcAddress(LibraryHandle, PChar('sk4d_strutstyle_set_enabled'));
  sk4d_strutstyle_set_font_families             := GetProcAddress(LibraryHandle, PChar('sk4d_strutstyle_set_font_families'));
  sk4d_strutstyle_set_font_size                 := GetProcAddress(LibraryHandle, PChar('sk4d_strutstyle_set_font_size'));
  sk4d_strutstyle_set_font_style                := GetProcAddress(LibraryHandle, PChar('sk4d_strutstyle_set_font_style'));
  sk4d_strutstyle_set_force_height              := GetProcAddress(LibraryHandle, PChar('sk4d_strutstyle_set_force_height'));
  sk4d_strutstyle_set_half_leading              := GetProcAddress(LibraryHandle, PChar('sk4d_strutstyle_set_half_leading'));
  sk4d_strutstyle_set_height_multiplier         := GetProcAddress(LibraryHandle, PChar('sk4d_strutstyle_set_height_multiplier'));
  sk4d_strutstyle_set_leading                   := GetProcAddress(LibraryHandle, PChar('sk4d_strutstyle_set_leading'));
{$ELSE}
function  sk4d_paragraphstyle_create(): sk_paragraphstyle_t; cdecl;                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paragraphstyle_destroy(self: sk_paragraphstyle_t); cdecl;                                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paragraphstyle_disable_hinting(self: sk_paragraphstyle_t); cdecl;                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paragraphstyle_get_ellipsis(const self: sk_paragraphstyle_t): sk_string_t; cdecl;                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paragraphstyle_get_height(const self: sk_paragraphstyle_t): float; cdecl;                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paragraphstyle_get_max_lines(const self: sk_paragraphstyle_t): size_t; cdecl;                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paragraphstyle_get_strut_style(const self: sk_paragraphstyle_t): sk_strutstyle_t; cdecl;                    external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paragraphstyle_get_text_align(const self: sk_paragraphstyle_t): sk_textalign_t; cdecl;                      external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paragraphstyle_get_text_direction(const self: sk_paragraphstyle_t): sk_textdirection_t; cdecl;              external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paragraphstyle_get_text_height_behaviors(const self: sk_paragraphstyle_t): uint32_t; cdecl;                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_paragraphstyle_get_text_style(const self: sk_paragraphstyle_t): sk_textstyle_t; cdecl;                      external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paragraphstyle_set_ellipsis(self: sk_paragraphstyle_t; const value: MarshaledAString); cdecl;               external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paragraphstyle_set_height(self: sk_paragraphstyle_t; value: float); cdecl;                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paragraphstyle_set_max_lines(self: sk_paragraphstyle_t; value: size_t); cdecl;                              external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paragraphstyle_set_strut_style(self: sk_paragraphstyle_t; const value: sk_strutstyle_t); cdecl;             external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paragraphstyle_set_text_align(self: sk_paragraphstyle_t; value: sk_textalign_t); cdecl;                     external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paragraphstyle_set_text_direction(self: sk_paragraphstyle_t; value: sk_textdirection_t); cdecl;             external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paragraphstyle_set_text_height_behaviors(self: sk_paragraphstyle_t; value: uint32_t); cdecl;                external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_paragraphstyle_set_text_style(self: sk_paragraphstyle_t; value: sk_textstyle_t); cdecl;                     external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_strutstyle_create(): sk_strutstyle_t; cdecl;                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_strutstyle_destroy(self: sk_strutstyle_t); cdecl;                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_strutstyle_get_enabled(const self: sk_strutstyle_t): _bool; cdecl;                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_strutstyle_get_font_families(const self: sk_strutstyle_t; const result: PMarshaledAString): size_t; cdecl;  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_strutstyle_get_font_size(const self: sk_strutstyle_t): float; cdecl;                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_strutstyle_get_font_style(const self: sk_strutstyle_t; out result: sk_fontstyle_t); cdecl;                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_strutstyle_get_force_height(const self: sk_strutstyle_t): _bool; cdecl;                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_strutstyle_get_half_leading(const self: sk_strutstyle_t): _bool; cdecl;                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_strutstyle_get_height_multiplier(const self: sk_strutstyle_t): float; cdecl;                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_strutstyle_get_leading(const self: sk_strutstyle_t): float; cdecl;                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_strutstyle_is_equal(const self: sk_strutstyle_t; const strut_style: sk_strutstyle_t): _bool; cdecl;         external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_strutstyle_set_enabled(self: sk_strutstyle_t; value: _bool); cdecl;                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_strutstyle_set_font_families(self: sk_strutstyle_t; const values: PMarshaledAString; count: size_t); cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_strutstyle_set_font_size(self: sk_strutstyle_t; value: float); cdecl;                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_strutstyle_set_font_style(self: sk_strutstyle_t; value: psk_fontstyle_t); cdecl;                            external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_strutstyle_set_force_height(self: sk_strutstyle_t; value: _bool); cdecl;                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_strutstyle_set_half_leading(self: sk_strutstyle_t; value: _bool); cdecl;                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_strutstyle_set_height_multiplier(self: sk_strutstyle_t; value: float); cdecl;                               external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_strutstyle_set_leading(self: sk_strutstyle_t; value: float); cdecl;                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ modules/skparagraph/include/sk4d_textstyle.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_textstyle_add_font_feature         := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_add_font_feature'));
  sk4d_textstyle_add_shadow               := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_add_shadow'));
  sk4d_textstyle_clear_background_color   := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_clear_background_color'));
  sk4d_textstyle_clear_foreground_color   := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_clear_foreground_color'));
  sk4d_textstyle_create                   := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_create'));
  sk4d_textstyle_destroy                  := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_destroy'));
  sk4d_textstyle_get_background           := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_get_background'));
  sk4d_textstyle_get_color                := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_get_color'));
  sk4d_textstyle_get_decoration_color     := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_get_decoration_color'));
  sk4d_textstyle_get_decoration_style     := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_get_decoration_style'));
  sk4d_textstyle_get_decoration_thickness := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_get_decoration_thickness'));
  sk4d_textstyle_get_decorations          := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_get_decorations'));
  sk4d_textstyle_get_font_families        := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_get_font_families'));
  sk4d_textstyle_get_font_metrics         := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_get_font_metrics'));
  sk4d_textstyle_get_font_size            := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_get_font_size'));
  sk4d_textstyle_get_font_style           := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_get_font_style'));
  sk4d_textstyle_get_foreground           := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_get_foreground'));
  sk4d_textstyle_get_half_leading         := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_get_half_leading'));
  sk4d_textstyle_get_height_multiplier    := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_get_height_multiplier'));
  sk4d_textstyle_get_letter_spacing       := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_get_letter_spacing'));
  sk4d_textstyle_get_locale               := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_get_locale'));
  sk4d_textstyle_get_word_spacing         := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_get_word_spacing'));
  sk4d_textstyle_is_equal                 := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_is_equal'));
  sk4d_textstyle_reset_font_features      := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_reset_font_features'));
  sk4d_textstyle_reset_shadows            := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_reset_shadows'));
  sk4d_textstyle_set_background_color     := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_set_background_color'));
  sk4d_textstyle_set_color                := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_set_color'));
  sk4d_textstyle_set_decoration_color     := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_set_decoration_color'));
  sk4d_textstyle_set_decoration_style     := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_set_decoration_style'));
  sk4d_textstyle_set_decoration_thickness := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_set_decoration_thickness'));
  sk4d_textstyle_set_decorations          := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_set_decorations'));
  sk4d_textstyle_set_font_families        := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_set_font_families'));
  sk4d_textstyle_set_font_size            := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_set_font_size'));
  sk4d_textstyle_set_font_style           := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_set_font_style'));
  sk4d_textstyle_set_foreground_color     := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_set_foreground_color'));
  sk4d_textstyle_set_half_leading         := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_set_half_leading'));
  sk4d_textstyle_set_height_multiplier    := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_set_height_multiplier'));
  sk4d_textstyle_set_letter_spacing       := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_set_letter_spacing'));
  sk4d_textstyle_set_locale               := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_set_locale'));
  sk4d_textstyle_set_word_spacing         := GetProcAddress(LibraryHandle, PChar('sk4d_textstyle_set_word_spacing'));
{$ELSE}
procedure sk4d_textstyle_add_font_feature(self: sk_textstyle_t; const feature: MarshaledAString; value: int32_t); cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textstyle_add_shadow(self: sk_textstyle_t; const shadow: psk_textshadow_t); cdecl;                        external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textstyle_clear_background_color(self: sk_textstyle_t); cdecl;                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textstyle_clear_foreground_color(self: sk_textstyle_t); cdecl;                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_textstyle_create(): sk_textstyle_t; cdecl;                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textstyle_destroy(self: sk_textstyle_t); cdecl;                                                           external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_textstyle_get_background(const self: sk_textstyle_t): sk_paint_t; cdecl;                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_textstyle_get_color(const self: sk_textstyle_t): sk_color_t; cdecl;                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_textstyle_get_decoration_color(const self: sk_textstyle_t): sk_color_t; cdecl;                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_textstyle_get_decoration_style(const self: sk_textstyle_t): sk_textdecorationstyle_t; cdecl;              external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_textstyle_get_decoration_thickness(const self: sk_textstyle_t): float; cdecl;                             external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_textstyle_get_decorations(const self: sk_textstyle_t): uint32_t; cdecl;                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_textstyle_get_font_families(const self: sk_textstyle_t; const result: PMarshaledAString): size_t; cdecl;  external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textstyle_get_font_metrics(const self: sk_textstyle_t; out result: sk_fontmetrics_t); cdecl;              external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_textstyle_get_font_size(const self: sk_textstyle_t): float; cdecl;                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textstyle_get_font_style(const self: sk_textstyle_t; out result: sk_fontstyle_t); cdecl;                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_textstyle_get_foreground(const self: sk_textstyle_t): sk_paint_t; cdecl;                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_textstyle_get_half_leading(const self: sk_textstyle_t): _bool; cdecl;                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_textstyle_get_height_multiplier(const self: sk_textstyle_t): float; cdecl;                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_textstyle_get_letter_spacing(const self: sk_textstyle_t): float; cdecl;                                   external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_textstyle_get_locale(const self: sk_textstyle_t): sk_string_t; cdecl;                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_textstyle_get_word_spacing(const self: sk_textstyle_t): float; cdecl;                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_textstyle_is_equal(const self, text_style: sk_textstyle_t): _bool; cdecl;                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textstyle_reset_font_features(self: sk_textstyle_t); cdecl;                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textstyle_reset_shadows(self: sk_textstyle_t); cdecl;                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textstyle_set_background_color(self: sk_textstyle_t; paint: sk_paint_t); cdecl;                           external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textstyle_set_color(self: sk_textstyle_t; value: sk_color_t); cdecl;                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textstyle_set_decoration_color(self: sk_textstyle_t; value: sk_color_t); cdecl;                           external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textstyle_set_decoration_style(self: sk_textstyle_t; value: sk_textdecorationstyle_t); cdecl;             external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textstyle_set_decoration_thickness(self: sk_textstyle_t; value: float); cdecl;                            external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textstyle_set_decorations(self: sk_textstyle_t; value: uint32_t); cdecl;                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textstyle_set_font_families(self: sk_textstyle_t; const values: PMarshaledAString; count: size_t); cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textstyle_set_font_size(self: sk_textstyle_t; value: float); cdecl;                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textstyle_set_font_style(self: sk_textstyle_t; const value: psk_fontstyle_t); cdecl;                      external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textstyle_set_foreground_color(self: sk_textstyle_t; paint: sk_paint_t); cdecl;                           external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textstyle_set_half_leading(self: sk_textstyle_t; value: _bool); cdecl;                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textstyle_set_height_multiplier(self: sk_textstyle_t; value: float); cdecl;                               external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textstyle_set_letter_spacing(self: sk_textstyle_t; value: float); cdecl;                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textstyle_set_locale(self: sk_textstyle_t; const value: MarshaledAString); cdecl;                         external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_textstyle_set_word_spacing(self: sk_textstyle_t; value: float); cdecl;                                    external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ modules/skparagraph/include/sk4d_typefacefontprovider.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_typefacefontprovider_create             := GetProcAddress(LibraryHandle, PChar('sk4d_typefacefontprovider_create'));
  sk4d_typefacefontprovider_register_typeface  := GetProcAddress(LibraryHandle, PChar('sk4d_typefacefontprovider_register_typeface'));
  sk4d_typefacefontprovider_register_typeface2 := GetProcAddress(LibraryHandle, PChar('sk4d_typefacefontprovider_register_typeface2'));
{$ELSE}
function  sk4d_typefacefontprovider_create(): sk_typefacefontprovider_t; cdecl;                                                                               external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_typefacefontprovider_register_typeface(self: sk_typefacefontprovider_t; typeface: sk_typeface_t); cdecl;                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_typefacefontprovider_register_typeface2(self: sk_typefacefontprovider_t; typeface: sk_typeface_t; const family_name: MarshaledAString); cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ modules/skresources/include/sk4d_resources.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_resourceproviderbaseclass_create    := GetProcAddress(LibraryHandle, PChar('sk4d_resourceproviderbaseclass_create'));
  sk4d_resourceproviderbaseclass_set_procs := GetProcAddress(LibraryHandle, PChar('sk4d_resourceproviderbaseclass_set_procs'));
{$ELSE}
function  sk4d_resourceproviderbaseclass_create(predecode: _bool; context: Pointer): sk_resourceproviderbaseclass_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_resourceproviderbaseclass_set_procs(const procs: psk_resourceproviderbaseclass_procs_t); cdecl;              external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ modules/skshaper/include/sk4d_shaper.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_shaper_create  := GetProcAddress(LibraryHandle, PChar('sk4d_shaper_create'));
  sk4d_shaper_destroy := GetProcAddress(LibraryHandle, PChar('sk4d_shaper_destroy'));
  sk4d_shaper_shape   := GetProcAddress(LibraryHandle, PChar('sk4d_shaper_shape'));
{$ELSE}
function  sk4d_shaper_create(): sk_shaper_t; cdecl;                                                                                                                                                                     external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_shaper_destroy(self: sk_shaper_t); cdecl;                                                                                                                                                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_shaper_shape(const self: sk_shaper_t; const text: MarshaledAString; const font: sk_font_t; left_to_right: _bool; width: float; const offset: psk_point_t; end_point: psk_point_t): sk_textblob_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ modules/skunicode/include/sk4d_unicode.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_unicode_create               := GetProcAddress(LibraryHandle, PChar('sk4d_unicode_create'));
  sk4d_unicode_destroy              := GetProcAddress(LibraryHandle, PChar('sk4d_unicode_destroy'));
  sk4d_unicode_for_each_bidi_region := GetProcAddress(LibraryHandle, PChar('sk4d_unicode_for_each_bidi_region'));
  sk4d_unicode_for_each_break       := GetProcAddress(LibraryHandle, PChar('sk4d_unicode_for_each_break'));
  sk4d_unicode_for_each_codepoint   := GetProcAddress(LibraryHandle, PChar('sk4d_unicode_for_each_codepoint'));
  sk4d_unicodebreakiterator_create  := GetProcAddress(LibraryHandle, PChar('sk4d_unicodebreakiterator_create'));
  sk4d_unicodebreakiterator_create2 := GetProcAddress(LibraryHandle, PChar('sk4d_unicodebreakiterator_create2'));
  sk4d_unicodebreakiterator_destroy := GetProcAddress(LibraryHandle, PChar('sk4d_unicodebreakiterator_destroy'));
  sk4d_unicodebreakiterator_next    := GetProcAddress(LibraryHandle, PChar('sk4d_unicodebreakiterator_next'));
{$ELSE}
function  sk4d_unicode_create(): sk_unicode_t; cdecl;                                                                                                                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_unicode_destroy(self: sk_unicode_t); cdecl;                                                                                                                                                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_unicode_for_each_bidi_region(self: sk_unicode_t; const utf16_text: puint16_t; utf16_units: int32_t; direction: sk_direction_t; proc: sk_unicode_bidi_region_proc; context: Pointer); cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_unicode_for_each_break(self: sk_unicode_t; const utf16_text: pchar16_t; utf16_units: int32_t; &type: sk_breaktype_t; proc: sk_unicode_break_proc; context: Pointer); cdecl;                 external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_unicode_for_each_codepoint(self: sk_unicode_t; const utf16_text: pchar16_t; utf16_units: int32_t; proc: sk_unicode_codepoint_proc; context: Pointer); cdecl;                                external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_unicodebreakiterator_create(unicode: sk_unicode_t; &type: sk_breaktype_t; const text: _pchar; units: int32_t): sk_unicodebreakiterator_t; cdecl;                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_unicodebreakiterator_create2(unicode: sk_unicode_t; &type: sk_breaktype_t; const utf16_text: pchar16_t; utf16_units: int32_t): sk_unicodebreakiterator_t; cdecl;                            external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_unicodebreakiterator_destroy(self: sk_unicodebreakiterator_t); cdecl;                                                                                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_unicodebreakiterator_next(self: sk_unicodebreakiterator_t; out elem: sk_unicodebreakiteratorelem_t): _bool; cdecl;                                                                          external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ modules/svg/include/sk4d_svgdom.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_svgdom_find_node_by_id    := GetProcAddress(LibraryHandle, PChar('sk4d_svgdom_find_node_by_id'));
  sk4d_svgdom_get_root           := GetProcAddress(LibraryHandle, PChar('sk4d_svgdom_get_root'));
  sk4d_svgdom_make_from_file     := GetProcAddress(LibraryHandle, PChar('sk4d_svgdom_make_from_file'));
  sk4d_svgdom_make_from_stream   := GetProcAddress(LibraryHandle, PChar('sk4d_svgdom_make_from_stream'));
  sk4d_svgdom_render             := GetProcAddress(LibraryHandle, PChar('sk4d_svgdom_render'));
  sk4d_svgdom_set_container_size := GetProcAddress(LibraryHandle, PChar('sk4d_svgdom_set_container_size'));
{$ELSE}
function  sk4d_svgdom_find_node_by_id(self: sk_svgdom_t; const id: MarshaledAString): sk_svgnode_t; cdecl;                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_svgdom_get_root(const self: sk_svgdom_t): sk_svgsvg_t; cdecl;                                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_svgdom_make_from_file(const file_name: MarshaledAString; font_provider: sk_fontmgr_t): sk_svgdom_t; cdecl;                               external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_svgdom_make_from_stream(stream: sk_stream_t; resource_provider: sk_resourceprovider_t; font_provider: sk_fontmgr_t): sk_svgdom_t; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_svgdom_render(const self: sk_svgdom_t; canvas: sk_canvas_t); cdecl;                                                                      external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_svgdom_set_container_size(self: sk_svgdom_t; const size: psk_size_t); cdecl;                                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ modules/svg/include/sk4d_svgnode.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_svgnode_set_attribute := GetProcAddress(LibraryHandle, PChar('sk4d_svgnode_set_attribute'));
{$ELSE}
function sk4d_svgnode_set_attribute(self: sk_svgnode_t; const name, value: MarshaledAString): _bool; cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}


{ modules/svg/include/sk4d_svgsvg.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_svgsvg_get_height                := GetProcAddress(LibraryHandle, PChar('sk4d_svgsvg_get_height'));
  sk4d_svgsvg_get_intrinsic_size        := GetProcAddress(LibraryHandle, PChar('sk4d_svgsvg_get_intrinsic_size'));
  sk4d_svgsvg_get_preserve_aspect_ratio := GetProcAddress(LibraryHandle, PChar('sk4d_svgsvg_get_preserve_aspect_ratio'));
  sk4d_svgsvg_get_view_box              := GetProcAddress(LibraryHandle, PChar('sk4d_svgsvg_get_view_box'));
  sk4d_svgsvg_get_width                 := GetProcAddress(LibraryHandle, PChar('sk4d_svgsvg_get_width'));
  sk4d_svgsvg_get_x                     := GetProcAddress(LibraryHandle, PChar('sk4d_svgsvg_get_x'));
  sk4d_svgsvg_get_y                     := GetProcAddress(LibraryHandle, PChar('sk4d_svgsvg_get_y'));
  sk4d_svgsvg_set_height                := GetProcAddress(LibraryHandle, PChar('sk4d_svgsvg_set_height'));
  sk4d_svgsvg_set_preserve_aspect_ratio := GetProcAddress(LibraryHandle, PChar('sk4d_svgsvg_set_preserve_aspect_ratio'));
  sk4d_svgsvg_set_view_box              := GetProcAddress(LibraryHandle, PChar('sk4d_svgsvg_set_view_box'));
  sk4d_svgsvg_set_width                 := GetProcAddress(LibraryHandle, PChar('sk4d_svgsvg_set_width'));
  sk4d_svgsvg_set_x                     := GetProcAddress(LibraryHandle, PChar('sk4d_svgsvg_set_x'));
  sk4d_svgsvg_set_y                     := GetProcAddress(LibraryHandle, PChar('sk4d_svgsvg_set_y'));
{$ELSE}
procedure sk4d_svgsvg_get_height(const self: sk_svgsvg_t; out result: sk_svglength_t); cdecl;                                             external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_svgsvg_get_intrinsic_size(const self: sk_svgsvg_t; const view_port: psk_size_t; dpi: float; out result: sk_size_t); cdecl; external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_svgsvg_get_preserve_aspect_ratio(const self: sk_svgsvg_t; out result: sk_svgpreserveaspectratio_t); cdecl;                 external{$IFNDEF FPC} LibraryName{$ENDIF};
function  sk4d_svgsvg_get_view_box(const self: sk_svgsvg_t; out result: sk_rect_t): _bool; cdecl;                                         external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_svgsvg_get_width(const self: sk_svgsvg_t; out result: sk_svglength_t); cdecl;                                              external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_svgsvg_get_x(const self: sk_svgsvg_t; out result: sk_svglength_t); cdecl;                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_svgsvg_get_y(const self: sk_svgsvg_t; out result: sk_svglength_t); cdecl;                                                  external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_svgsvg_set_height(self: sk_svgsvg_t; value: psk_svglength_t); cdecl;                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_svgsvg_set_preserve_aspect_ratio(self: sk_svgsvg_t; value: psk_svgpreserveaspectratio_t); cdecl;                           external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_svgsvg_set_view_box(self: sk_svgsvg_t; view_box: psk_rect_t); cdecl;                                                       external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_svgsvg_set_width(self: sk_svgsvg_t; value: psk_svglength_t); cdecl;                                                        external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_svgsvg_set_x(self: sk_svgsvg_t; value: psk_svglength_t); cdecl;                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
procedure sk4d_svgsvg_set_y(self: sk_svgsvg_t; value: psk_svglength_t); cdecl;                                                            external{$IFNDEF FPC} LibraryName{$ENDIF};
{$ENDIF}

{$IFNDEF SK_STATIC_LIBRARY}
end;
{$ELSE}
procedure SkInitialize;
begin
end;
{$ENDIF}

{$IFNDEF SK_STATIC_LIBRARY}
procedure SkFinalize;
begin
  if AtomicDecrement(InitCount) = 0 then
    FreeLibrary(LibraryHandle);
end;
{$ELSE}
procedure SkFinalize;
begin
end;
{$ENDIF}

{$ENDIF !WORKAROUND_RS123846}

end.
