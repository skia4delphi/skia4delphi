{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2011-2023 Google LLC.                                    }
{ Copyright (c) 2021-2023 Skia4Delphi Project.                           }
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
  {$HPPEMIT '  #pragma link "Skia.Package.RTL.a"'}
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

{$ELSEIF DEFINED(ANDROID) and not DEFINED(FPC)}

uses
  { Delphi }
  System.IOUtils;

{$ENDIF}


const
{$IFDEF SK_STATIC_LIBRARY}
  {$IFDEF IOS}
    LibraryName = 'libsk4d.a';

    {$IFDEF IOSSIMULATOR}
    procedure libclang_rt; external '/usr/lib/clang/lib/darwin/libclang_rt.iossim.a';
    {$ELSE}
    procedure libclang_rt; external '/usr/lib/clang/lib/darwin/libclang_rt.ios.a';
    {$ENDIF}
    procedure CoreText; external framework 'CoreText';
    procedure UIKit; external framework 'UIKit';
    procedure Metal; external framework 'Metal';
  {$ENDIF}
{$ELSE}
  {$IF DEFINED(MSWINDOWS)}
  LibraryName = 'sk4d.dll';
  {$ELSEIF DEFINED(MACOS)}
  LibraryName = 'libsk4d.dylib';
  {$ELSE}
  LibraryName = 'libsk4d.so';
  {$ENDIF}
{$ENDIF}

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
  if {$IFDEF FPC}InterlockedIncrement{$ELSE}AtomicIncrement{$ENDIF}(InitCount) <> 1 then
    Exit;
  {$IF DEFINED(ANDROID)}
  // Some Android devices, normally old, need the full path of the library,
  // and other devices, normally new, do not accept the full path.
  LibraryHandle := SafeLoadLibrary(LibraryName);
  if LibraryHandle = 0 then
    LibraryHandle := SafeLoadLibrary({$IFDEF FPC}ExtractFilePath(ParamStr(0)) + LibraryName{$ELSE}TPath.Combine(TPath.GetLibraryPath, LibraryName){$ENDIF});
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
function  gr4d_backendsemaphore_create;      external LibraryName name 'gr4d_backendsemaphore_create'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure gr4d_backendsemaphore_destroy;     external LibraryName name 'gr4d_backendsemaphore_destroy'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure gr4d_backendsemaphore_init_vulkan; external LibraryName name 'gr4d_backendsemaphore_init_vulkan'{$IFDEF IOS} dependency 'c++'{$ENDIF};
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
function  gr4d_backendrendertarget_create_gl;          external LibraryName name 'gr4d_backendrendertarget_create_gl'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_backendrendertarget_create_mtl;         external LibraryName name 'gr4d_backendrendertarget_create_mtl'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_backendrendertarget_create_vk;          external LibraryName name 'gr4d_backendrendertarget_create_vk'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure gr4d_backendrendertarget_destroy;            external LibraryName name 'gr4d_backendrendertarget_destroy'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_backendrendertarget_get_backend_api;    external LibraryName name 'gr4d_backendrendertarget_get_backend_api'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_backendrendertarget_get_height;         external LibraryName name 'gr4d_backendrendertarget_get_height'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_backendrendertarget_get_sample_count;   external LibraryName name 'gr4d_backendrendertarget_get_sample_count'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_backendrendertarget_get_stencil_bits;   external LibraryName name 'gr4d_backendrendertarget_get_stencil_bits'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_backendrendertarget_get_width;          external LibraryName name 'gr4d_backendrendertarget_get_width'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_backendrendertarget_is_valid;           external LibraryName name 'gr4d_backendrendertarget_is_valid'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_backendtexture_create_gl;               external LibraryName name 'gr4d_backendtexture_create_gl'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_backendtexture_create_mtl;              external LibraryName name 'gr4d_backendtexture_create_mtl'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_backendtexture_create_vk;               external LibraryName name 'gr4d_backendtexture_create_vk'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure gr4d_backendtexture_destroy;                 external LibraryName name 'gr4d_backendtexture_destroy'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_backendtexture_get_backend_api;         external LibraryName name 'gr4d_backendtexture_get_backend_api'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_backendtexture_get_gl_framebuffer_info; external LibraryName name 'gr4d_backendtexture_get_gl_framebuffer_info'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_backendtexture_get_height;              external LibraryName name 'gr4d_backendtexture_get_height'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_backendtexture_get_width;               external LibraryName name 'gr4d_backendtexture_get_width'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_backendtexture_has_mipmaps;             external LibraryName name 'gr4d_backendtexture_has_mipmaps'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_backendtexture_is_valid;                external LibraryName name 'gr4d_backendtexture_is_valid'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
{$ENDIF}


{ include/c/gr4d_backendsurfacemutablestate.h }

{$IFNDEF SK_STATIC_LIBRARY}
  gr4d_backendsurfacemutablestate_create  := GetProcAddress(LibraryHandle, PChar('gr4d_backendsurfacemutablestate_create'));
  gr4d_backendsurfacemutablestate_destroy := GetProcAddress(LibraryHandle, PChar('gr4d_backendsurfacemutablestate_destroy'));
{$ELSE}
function  gr4d_backendsurfacemutablestate_create;  external LibraryName name 'gr4d_backendsurfacemutablestate_create' {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure gr4d_backendsurfacemutablestate_destroy; external LibraryName name 'gr4d_backendsurfacemutablestate_destroy'{$IFDEF IOS} dependency 'c++'{$ENDIF};
{$ENDIF}


{ include/c/gr4d_contextoptions.h }

{$IFNDEF SK_STATIC_LIBRARY}
gr4d_persistentcachebaseclass_create    := GetProcAddress(LibraryHandle, PChar('gr4d_persistentcachebaseclass_create'));
gr4d_persistentcachebaseclass_destroy   := GetProcAddress(LibraryHandle, PChar('gr4d_persistentcachebaseclass_destroy'));
gr4d_persistentcachebaseclass_set_procs := GetProcAddress(LibraryHandle, PChar('gr4d_persistentcachebaseclass_set_procs'));
{$ELSE}
function  gr4d_persistentcachebaseclass_create;    external LibraryName name 'gr4d_persistentcachebaseclass_create'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure gr4d_persistentcachebaseclass_destroy;   external LibraryName name 'gr4d_persistentcachebaseclass_destroy'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure gr4d_persistentcachebaseclass_set_procs; external LibraryName name 'gr4d_persistentcachebaseclass_set_procs'{$IFDEF IOS} dependency 'c++'{$ENDIF};
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
procedure gr4d_directcontext_abandon_context;                             external LibraryName name 'gr4d_directcontext_abandon_context'                            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_directcontext_create_texture;                              external LibraryName name 'gr4d_directcontext_create_texture'                             {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_directcontext_create_texture2;                             external LibraryName name 'gr4d_directcontext_create_texture2'                            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_directcontext_create_texture3;                             external LibraryName name 'gr4d_directcontext_create_texture3'                            {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure gr4d_directcontext_delete_texture;                              external LibraryName name 'gr4d_directcontext_delete_texture'                             {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure gr4d_directcontext_dump_memory_statistics;                      external LibraryName name 'gr4d_directcontext_dump_memory_statistics'                     {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure gr4d_directcontext_flush;                                       external LibraryName name 'gr4d_directcontext_flush'                                      {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure gr4d_directcontext_flush_and_submit;                            external LibraryName name 'gr4d_directcontext_flush_and_submit'                           {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure gr4d_directcontext_free_gpu_resources;                          external LibraryName name 'gr4d_directcontext_free_gpu_resources'                         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_directcontext_get_backend_api;                             external LibraryName name 'gr4d_directcontext_get_backend_api'                            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_directcontext_get_max_surface_sample_count_for_color_type; external LibraryName name 'gr4d_directcontext_get_max_surface_sample_count_for_color_type'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_directcontext_get_resource_cache_limit;                    external LibraryName name 'gr4d_directcontext_get_resource_cache_limit'                   {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure gr4d_directcontext_get_resource_cache_usage;                    external LibraryName name 'gr4d_directcontext_get_resource_cache_usage'                   {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_directcontext_is_abandoned;                                external LibraryName name 'gr4d_directcontext_is_abandoned'                               {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_directcontext_make_gl;                                     external LibraryName name 'gr4d_directcontext_make_gl'                                    {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_directcontext_make_metal;                                  external LibraryName name 'gr4d_directcontext_make_metal'                                 {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_directcontext_make_vulkan;                                 external LibraryName name 'gr4d_directcontext_make_vulkan'                                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure gr4d_directcontext_perform_deferred_cleanup;                    external LibraryName name 'gr4d_directcontext_perform_deferred_cleanup'                   {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure gr4d_directcontext_purge_unlocked_resources;                    external LibraryName name 'gr4d_directcontext_purge_unlocked_resources'                   {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure gr4d_directcontext_purge_unlocked_resources2;                   external LibraryName name 'gr4d_directcontext_purge_unlocked_resources2'                  {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure gr4d_directcontext_release_resources_and_abandon_context;       external LibraryName name 'gr4d_directcontext_release_resources_and_abandon_context'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure gr4d_directcontext_reset_context;                               external LibraryName name 'gr4d_directcontext_reset_context'                              {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure gr4d_directcontext_set_resource_cache_limit;                    external LibraryName name 'gr4d_directcontext_set_resource_cache_limit'                   {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_directcontext_submit;                                      external LibraryName name 'gr4d_directcontext_submit'                                     {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
function gr4d_gl_interface_has_extension;        external LibraryName name 'gr4d_gl_interface_has_extension'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
function gr4d_gl_interface_make_assembled;       external LibraryName name 'gr4d_gl_interface_make_assembled'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function gr4d_gl_interface_make_assembled_gl;    external LibraryName name 'gr4d_gl_interface_make_assembled_gl'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
function gr4d_gl_interface_make_assembled_gles;  external LibraryName name 'gr4d_gl_interface_make_assembled_gles' {$IFDEF IOS} dependency 'c++'{$ENDIF};
function gr4d_gl_interface_make_assembled_webgl; external LibraryName name 'gr4d_gl_interface_make_assembled_webgl'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function gr4d_gl_interface_make_native;          external LibraryName name 'gr4d_gl_interface_make_native'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function gr4d_gl_interface_validate;             external LibraryName name 'gr4d_gl_interface_validate'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
{$ENDIF}


{ include/c/gr4d_shadererrorhandler.h }

{$IFNDEF SK_STATIC_LIBRARY}
gr4d_shadererrorhandlerbaseclass_create    := GetProcAddress(LibraryHandle, PChar('gr4d_shadererrorhandlerbaseclass_create'));
gr4d_shadererrorhandlerbaseclass_destroy   := GetProcAddress(LibraryHandle, PChar('gr4d_shadererrorhandlerbaseclass_destroy'));
gr4d_shadererrorhandlerbaseclass_set_procs := GetProcAddress(LibraryHandle, PChar('gr4d_shadererrorhandlerbaseclass_set_procs'));
{$ELSE}
function  gr4d_shadererrorhandlerbaseclass_create;    external LibraryName name 'gr4d_shadererrorhandlerbaseclass_create'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure gr4d_shadererrorhandlerbaseclass_destroy;   external LibraryName name 'gr4d_shadererrorhandlerbaseclass_destroy'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure gr4d_shadererrorhandlerbaseclass_set_procs; external LibraryName name 'gr4d_shadererrorhandlerbaseclass_set_procs'{$IFDEF IOS} dependency 'c++'{$ENDIF};
{$ENDIF}


{ include/c/gr4d_vk_extensions.h }

{$IFNDEF SK_STATIC_LIBRARY}
gr4d_vk_extensions_create        := GetProcAddress(LibraryHandle, PChar('gr4d_vk_extensions_create'));
gr4d_vk_extensions_destroy       := GetProcAddress(LibraryHandle, PChar('gr4d_vk_extensions_destroy'));
gr4d_vk_extensions_has_extension := GetProcAddress(LibraryHandle, PChar('gr4d_vk_extensions_has_extension'));
gr4d_vk_extensions_init          := GetProcAddress(LibraryHandle, PChar('gr4d_vk_extensions_init'));
{$ELSE}
function  gr4d_vk_extensions_create;        external LibraryName name 'gr4d_vk_extensions_create'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure gr4d_vk_extensions_destroy;       external LibraryName name 'gr4d_vk_extensions_destroy'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  gr4d_vk_extensions_has_extension; external LibraryName name 'gr4d_vk_extensions_has_extension'{$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure gr4d_vk_extensions_init;          external LibraryName name 'gr4d_vk_extensions_init'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
{$ENDIF}


{ include/c/sk4d_animatedwebpencoder.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_animatedwebpencoder_encode_to_file   := GetProcAddress(LibraryHandle, PChar('sk4d_animatedwebpencoder_encode_to_file'));
  sk4d_animatedwebpencoder_encode_to_stream := GetProcAddress(LibraryHandle, PChar('sk4d_animatedwebpencoder_encode_to_stream'));
{$ELSE}
function sk4d_animatedwebpencoder_encode_to_file;   external LibraryName name 'sk4d_animatedwebpencoder_encode_to_file'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_animatedwebpencoder_encode_to_stream; external LibraryName name 'sk4d_animatedwebpencoder_encode_to_stream'{$IFDEF IOS} dependency 'c++'{$ENDIF};
{$ENDIF}


{ include/c/sk4d_blender.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_blender_make_arithmetic := GetProcAddress(LibraryHandle, PChar('sk4d_blender_make_arithmetic'));
  sk4d_blender_make_mode       := GetProcAddress(LibraryHandle, PChar('sk4d_blender_make_mode'));
{$ELSE}
function  sk4d_blender_make_arithmetic; external LibraryName name 'sk4d_blender_make_arithmetic'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_blender_make_mode;       external LibraryName name 'sk4d_blender_make_mode'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
procedure sk4d_canvas_clear;                      external LibraryName name 'sk4d_canvas_clear'                     {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_clear2;                     external LibraryName name 'sk4d_canvas_clear2'                    {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_destroy;                    external LibraryName name 'sk4d_canvas_destroy'                   {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_discard;                    external LibraryName name 'sk4d_canvas_discard'                   {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_clip_path;                  external LibraryName name 'sk4d_canvas_clip_path'                 {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_clip_rect;                  external LibraryName name 'sk4d_canvas_clip_rect'                 {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_clip_region;                external LibraryName name 'sk4d_canvas_clip_region'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_clip_rrect;                 external LibraryName name 'sk4d_canvas_clip_rrect'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_clip_shader;                external LibraryName name 'sk4d_canvas_clip_shader'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_concat;                     external LibraryName name 'sk4d_canvas_concat'                    {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_concat2;                    external LibraryName name 'sk4d_canvas_concat2'                   {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_annotation;            external LibraryName name 'sk4d_canvas_draw_annotation'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_arc;                   external LibraryName name 'sk4d_canvas_draw_arc'                  {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_atlas;                 external LibraryName name 'sk4d_canvas_draw_atlas'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_circle;                external LibraryName name 'sk4d_canvas_draw_circle'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_color;                 external LibraryName name 'sk4d_canvas_draw_color'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_color2;                external LibraryName name 'sk4d_canvas_draw_color2'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_glyphs;                external LibraryName name 'sk4d_canvas_draw_glyphs'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_glyphs2;               external LibraryName name 'sk4d_canvas_draw_glyphs2'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_image;                 external LibraryName name 'sk4d_canvas_draw_image'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_image_lattice;         external LibraryName name 'sk4d_canvas_draw_image_lattice'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_image_nine;            external LibraryName name 'sk4d_canvas_draw_image_nine'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_image_rect;            external LibraryName name 'sk4d_canvas_draw_image_rect'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_line;                  external LibraryName name 'sk4d_canvas_draw_line'                 {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_oval;                  external LibraryName name 'sk4d_canvas_draw_oval'                 {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_paint;                 external LibraryName name 'sk4d_canvas_draw_paint'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_patch;                 external LibraryName name 'sk4d_canvas_draw_patch'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_path;                  external LibraryName name 'sk4d_canvas_draw_path'                 {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_picture;               external LibraryName name 'sk4d_canvas_draw_picture'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_point;                 external LibraryName name 'sk4d_canvas_draw_point'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_points;                external LibraryName name 'sk4d_canvas_draw_points'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_rect;                  external LibraryName name 'sk4d_canvas_draw_rect'                 {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_region;                external LibraryName name 'sk4d_canvas_draw_region'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_rrect;                 external LibraryName name 'sk4d_canvas_draw_rrect'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_rrect2;                external LibraryName name 'sk4d_canvas_draw_rrect2'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_rrect_difference;      external LibraryName name 'sk4d_canvas_draw_rrect_difference'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_simple_text;           external LibraryName name 'sk4d_canvas_draw_simple_text'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_text_blob;             external LibraryName name 'sk4d_canvas_draw_text_blob'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_draw_vertices;              external LibraryName name 'sk4d_canvas_draw_vertices'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_get_base_props;             external LibraryName name 'sk4d_canvas_get_base_props'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_get_device_clip_bounds;     external LibraryName name 'sk4d_canvas_get_device_clip_bounds'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_get_local_clip_bounds;      external LibraryName name 'sk4d_canvas_get_local_clip_bounds'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_get_local_to_device;        external LibraryName name 'sk4d_canvas_get_local_to_device'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_get_local_to_device_as_3x3; external LibraryName name 'sk4d_canvas_get_local_to_device_as_3x3'{$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_get_top_props;              external LibraryName name 'sk4d_canvas_get_top_props'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_canvas_get_save_count;             external LibraryName name 'sk4d_canvas_get_save_count'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_canvas_make_surface;               external LibraryName name 'sk4d_canvas_make_surface'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_canvas_quick_reject;               external LibraryName name 'sk4d_canvas_quick_reject'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_canvas_quick_reject2;              external LibraryName name 'sk4d_canvas_quick_reject2'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_reset_matrix;               external LibraryName name 'sk4d_canvas_reset_matrix'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_restore;                    external LibraryName name 'sk4d_canvas_restore'                   {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_restore_to_count;           external LibraryName name 'sk4d_canvas_restore_to_count'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_rotate;                     external LibraryName name 'sk4d_canvas_rotate'                    {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_rotate2;                    external LibraryName name 'sk4d_canvas_rotate2'                   {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_canvas_save;                       external LibraryName name 'sk4d_canvas_save'                      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_canvas_save_layer;                 external LibraryName name 'sk4d_canvas_save_layer'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_canvas_save_layer_alpha;           external LibraryName name 'sk4d_canvas_save_layer_alpha'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_scale;                      external LibraryName name 'sk4d_canvas_scale'                     {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_set_matrix;                 external LibraryName name 'sk4d_canvas_set_matrix'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_set_matrix2;                external LibraryName name 'sk4d_canvas_set_matrix2'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_skew;                       external LibraryName name 'sk4d_canvas_skew'                      {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_canvas_translate;                  external LibraryName name 'sk4d_canvas_translate'                 {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
procedure sk4d_codec_destroy;                    external LibraryName name 'sk4d_codec_destroy'                   {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_codec_get_dimensions;             external LibraryName name 'sk4d_codec_get_dimensions'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_codec_get_encoded_image_format;   external LibraryName name 'sk4d_codec_get_encoded_image_format'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_codec_get_image;                  external LibraryName name 'sk4d_codec_get_image'                 {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_codec_get_pixels;                 external LibraryName name 'sk4d_codec_get_pixels'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_codec_make_from_file;             external LibraryName name 'sk4d_codec_make_from_file'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_codec_make_from_stream;           external LibraryName name 'sk4d_codec_make_from_stream'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_codec_make_with_copy;             external LibraryName name 'sk4d_codec_make_with_copy'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_codec_make_without_copy;          external LibraryName name 'sk4d_codec_make_without_copy'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_animcodecplayer_destroy;          external LibraryName name 'sk4d_animcodecplayer_destroy'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_animcodecplayer_get_dimensions;   external LibraryName name 'sk4d_animcodecplayer_get_dimensions'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_animcodecplayer_get_duration;     external LibraryName name 'sk4d_animcodecplayer_get_duration'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_animcodecplayer_get_frame;        external LibraryName name 'sk4d_animcodecplayer_get_frame'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_animcodecplayer_make_from_file;   external LibraryName name 'sk4d_animcodecplayer_make_from_file'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_animcodecplayer_make_from_stream; external LibraryName name 'sk4d_animcodecplayer_make_from_stream'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_animcodecplayer_seek;             external LibraryName name 'sk4d_animcodecplayer_seek'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
function sk4d_colorfilter_make_blend;                external LibraryName name 'sk4d_colorfilter_make_blend'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_colorfilter_make_blend2;               external LibraryName name 'sk4d_colorfilter_make_blend2'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_colorfilter_make_compose;              external LibraryName name 'sk4d_colorfilter_make_compose'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_colorfilter_make_high_contrast;        external LibraryName name 'sk4d_colorfilter_make_high_contrast'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_colorfilter_make_hsla_matrix;          external LibraryName name 'sk4d_colorfilter_make_hsla_matrix'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_colorfilter_make_lighting;             external LibraryName name 'sk4d_colorfilter_make_lighting'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_colorfilter_make_linear_to_srgb_gamma; external LibraryName name 'sk4d_colorfilter_make_linear_to_srgb_gamma'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_colorfilter_make_luma_color;           external LibraryName name 'sk4d_colorfilter_make_luma_color'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_colorfilter_make_matrix;               external LibraryName name 'sk4d_colorfilter_make_matrix'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_colorfilter_make_overdraw;             external LibraryName name 'sk4d_colorfilter_make_overdraw'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_colorfilter_make_table;                external LibraryName name 'sk4d_colorfilter_make_table'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
function  sk4d_colorspace_gamma_close_to_srgb;       external LibraryName name 'sk4d_colorspace_gamma_close_to_srgb'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_colorspace_gamma_is_linear;           external LibraryName name 'sk4d_colorspace_gamma_is_linear'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_colorspace_is_equal;                  external LibraryName name 'sk4d_colorspace_is_equal'                 {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_colorspace_is_numerical_transfer_fn;  external LibraryName name 'sk4d_colorspace_is_numerical_transfer_fn' {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_colorspace_is_srgb;                   external LibraryName name 'sk4d_colorspace_is_srgb'                  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_colorspace_make;                      external LibraryName name 'sk4d_colorspace_make'                     {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_colorspace_make_linear_gamma;         external LibraryName name 'sk4d_colorspace_make_linear_gamma'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_colorspace_make_rgb;                  external LibraryName name 'sk4d_colorspace_make_rgb'                 {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_colorspace_make_srgb;                 external LibraryName name 'sk4d_colorspace_make_srgb'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_colorspace_make_srgb_gamma;           external LibraryName name 'sk4d_colorspace_make_srgb_gamma'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_colorspace_make_srgb_linear;          external LibraryName name 'sk4d_colorspace_make_srgb_linear'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_colorspace_ref;                       external LibraryName name 'sk4d_colorspace_ref'                      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_colorspace_to_profile;                external LibraryName name 'sk4d_colorspace_to_profile'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_colorspace_to_xyz;                    external LibraryName name 'sk4d_colorspace_to_xyz'                   {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_colorspace_unref;                     external LibraryName name 'sk4d_colorspace_unref'                    {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_colorspaceiccprofile_destroy;         external LibraryName name 'sk4d_colorspaceiccprofile_destroy'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_colorspaceiccprofile_get_buffer;      external LibraryName name 'sk4d_colorspaceiccprofile_get_buffer'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_colorspaceiccprofile_make_with_parse; external LibraryName name 'sk4d_colorspaceiccprofile_make_with_parse'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_colorspaceiccprofile_to_xyz;          external LibraryName name 'sk4d_colorspaceiccprofile_to_xyz'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_colorspaceprimaries_to_xyz;           external LibraryName name 'sk4d_colorspaceprimaries_to_xyz'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_colorspacetransferfn_invert;          external LibraryName name 'sk4d_colorspacetransferfn_invert'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_colorspacetransferfn_transform;       external LibraryName name 'sk4d_colorspacetransferfn_transform'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
{$ENDIF}


{ include/c/sk4d_data.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_data_make_empty     := GetProcAddress(LibraryHandle, PChar('sk4d_data_make_empty'));
  sk4d_data_make_with_copy := GetProcAddress(LibraryHandle, PChar('sk4d_data_make_with_copy'));
  sk4d_data_ref            := GetProcAddress(LibraryHandle, PChar('sk4d_data_ref'));
  sk4d_data_unref          := GetProcAddress(LibraryHandle, PChar('sk4d_data_unref'));
{$ELSE}
function  sk4d_data_make_empty;     external LibraryName name 'sk4d_data_make_empty'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_data_make_with_copy; external LibraryName name 'sk4d_data_make_with_copy'{$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_data_ref;            external LibraryName name 'sk4d_data_ref'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_data_unref;          external LibraryName name 'sk4d_data_unref'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
function  sk4d_document_begin_page; external LibraryName name 'sk4d_document_begin_page'{$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_document_close;      external LibraryName name 'sk4d_document_close'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_document_end_page;   external LibraryName name 'sk4d_document_end_page'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_document_make_pdf;   external LibraryName name 'sk4d_document_make_pdf'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_document_make_pdf2;  external LibraryName name 'sk4d_document_make_pdf2' {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_document_make_xps;   external LibraryName name 'sk4d_document_make_xps'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_document_terminate;  external LibraryName name 'sk4d_document_terminate' {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
function  sk4d_font_create;                   external LibraryName name 'sk4d_font_create'                  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_font_create2;                  external LibraryName name 'sk4d_font_create2'                 {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_font_destroy;                  external LibraryName name 'sk4d_font_destroy'                 {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_font_get_baseline_snap;        external LibraryName name 'sk4d_font_get_baseline_snap'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_font_get_edging;               external LibraryName name 'sk4d_font_get_edging'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_font_get_embedded_bitmaps;     external LibraryName name 'sk4d_font_get_embedded_bitmaps'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_font_get_embolden;             external LibraryName name 'sk4d_font_get_embolden'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_font_get_force_auto_hinting;   external LibraryName name 'sk4d_font_get_force_auto_hinting'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_font_get_glyphs;               external LibraryName name 'sk4d_font_get_glyphs'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_font_get_glyphs_count;         external LibraryName name 'sk4d_font_get_glyphs_count'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_font_get_hinting;              external LibraryName name 'sk4d_font_get_hinting'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_font_get_horizontal_positions; external LibraryName name 'sk4d_font_get_horizontal_positions'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_font_get_intercepts;           external LibraryName name 'sk4d_font_get_intercepts'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_font_get_linear_metrics;       external LibraryName name 'sk4d_font_get_linear_metrics'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_font_get_metrics;              external LibraryName name 'sk4d_font_get_metrics'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_font_get_path;                 external LibraryName name 'sk4d_font_get_path'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_font_get_paths;                external LibraryName name 'sk4d_font_get_paths'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_font_get_positions;            external LibraryName name 'sk4d_font_get_positions'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_font_get_scale_x;              external LibraryName name 'sk4d_font_get_scale_x'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_font_get_size;                 external LibraryName name 'sk4d_font_get_size'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_font_get_skew_x;               external LibraryName name 'sk4d_font_get_skew_x'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_font_get_subpixel;             external LibraryName name 'sk4d_font_get_subpixel'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_font_get_typeface;             external LibraryName name 'sk4d_font_get_typeface'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_font_get_typeface_or_default;  external LibraryName name 'sk4d_font_get_typeface_or_default' {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_font_get_widths_bounds;        external LibraryName name 'sk4d_font_get_widths_bounds'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_font_is_equal;                 external LibraryName name 'sk4d_font_is_equal'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_font_measure_text;             external LibraryName name 'sk4d_font_measure_text'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_font_set_baseline_snap;        external LibraryName name 'sk4d_font_set_baseline_snap'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_font_set_edging;               external LibraryName name 'sk4d_font_set_edging'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_font_set_embedded_bitmaps;     external LibraryName name 'sk4d_font_set_embedded_bitmaps'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_font_set_embolden;             external LibraryName name 'sk4d_font_set_embolden'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_font_set_force_auto_hinting;   external LibraryName name 'sk4d_font_set_force_auto_hinting'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_font_set_hinting;              external LibraryName name 'sk4d_font_set_hinting'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_font_set_linear_metrics;       external LibraryName name 'sk4d_font_set_linear_metrics'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_font_set_scale_x;              external LibraryName name 'sk4d_font_set_scale_x'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_font_set_size;                 external LibraryName name 'sk4d_font_set_size'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_font_set_skew_x;               external LibraryName name 'sk4d_font_set_skew_x'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_font_set_subpixel;             external LibraryName name 'sk4d_font_set_subpixel'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_font_set_typeface;             external LibraryName name 'sk4d_font_set_typeface'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_font_unichar_to_glyph;         external LibraryName name 'sk4d_font_unichar_to_glyph'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_font_unichars_to_glyphs;       external LibraryName name 'sk4d_font_unichars_to_glyphs'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
procedure sk4d_graphics_allow_jit;                                       external LibraryName name 'sk4d_graphics_allow_jit'                                      {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_graphics_dump_memory_statistics;                          external LibraryName name 'sk4d_graphics_dump_memory_statistics'                         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_graphics_get_font_cache_count_limit;                      external LibraryName name 'sk4d_graphics_get_font_cache_count_limit'                     {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_graphics_get_font_cache_count_used;                       external LibraryName name 'sk4d_graphics_get_font_cache_count_used'                      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_graphics_get_font_cache_limit;                            external LibraryName name 'sk4d_graphics_get_font_cache_limit'                           {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_graphics_get_font_cache_used;                             external LibraryName name 'sk4d_graphics_get_font_cache_used'                            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_graphics_get_resource_cache_single_allocation_byte_limit; external LibraryName name 'sk4d_graphics_get_resource_cache_single_allocation_byte_limit'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_graphics_get_resource_cache_total_byte_limit;             external LibraryName name 'sk4d_graphics_get_resource_cache_total_byte_limit'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_graphics_get_resource_cache_total_bytes_used;             external LibraryName name 'sk4d_graphics_get_resource_cache_total_bytes_used'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_graphics_init;                                            external LibraryName name 'sk4d_graphics_init'                                           {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_graphics_purge_all_caches;                                external LibraryName name 'sk4d_graphics_purge_all_caches'                               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_graphics_purge_font_cache;                                external LibraryName name 'sk4d_graphics_purge_font_cache'                               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_graphics_purge_resource_cache;                            external LibraryName name 'sk4d_graphics_purge_resource_cache'                           {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_graphics_set_font_cache_count_limit;                      external LibraryName name 'sk4d_graphics_set_font_cache_count_limit'                     {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_graphics_set_font_cache_limit;                            external LibraryName name 'sk4d_graphics_set_font_cache_limit'                           {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_graphics_set_resource_cache_single_allocation_byte_limit; external LibraryName name 'sk4d_graphics_set_resource_cache_single_allocation_byte_limit'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_graphics_set_resource_cache_total_byte_limit;             external LibraryName name 'sk4d_graphics_set_resource_cache_total_byte_limit'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
function  sk4d_image_encode_to_file;            external LibraryName name 'sk4d_image_encode_to_file'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_encode_to_stream;          external LibraryName name 'sk4d_image_encode_to_stream'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_get_alpha_type;            external LibraryName name 'sk4d_image_get_alpha_type'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_get_color_space;           external LibraryName name 'sk4d_image_get_color_space'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_get_color_type;            external LibraryName name 'sk4d_image_get_color_type'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_get_height;                external LibraryName name 'sk4d_image_get_height'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_image_get_image_info;            external LibraryName name 'sk4d_image_get_image_info'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_get_unique_id;             external LibraryName name 'sk4d_image_get_unique_id'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_get_width;                 external LibraryName name 'sk4d_image_get_width'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_is_lazy_generated;         external LibraryName name 'sk4d_image_is_lazy_generated'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_is_texture_backed;         external LibraryName name 'sk4d_image_is_texture_backed'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_is_valid;                  external LibraryName name 'sk4d_image_is_valid'                 {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_make_cross_context;        external LibraryName name 'sk4d_image_make_cross_context'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_make_from_adopted_texture; external LibraryName name 'sk4d_image_make_from_adopted_texture'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_make_from_encoded_file;    external LibraryName name 'sk4d_image_make_from_encoded_file'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_make_from_encoded_stream;  external LibraryName name 'sk4d_image_make_from_encoded_stream' {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_make_from_picture;         external LibraryName name 'sk4d_image_make_from_picture'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_make_from_raster;          external LibraryName name 'sk4d_image_make_from_raster'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_make_from_texture;         external LibraryName name 'sk4d_image_make_from_texture'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_make_non_texture_image;    external LibraryName name 'sk4d_image_make_non_texture_image'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_make_raster_copy;          external LibraryName name 'sk4d_image_make_raster_copy'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_make_raster_image;         external LibraryName name 'sk4d_image_make_raster_image'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_make_raw_shader;           external LibraryName name 'sk4d_image_make_raw_shader'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_make_shader;               external LibraryName name 'sk4d_image_make_shader'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_make_subset;               external LibraryName name 'sk4d_image_make_subset'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_make_texture_image;        external LibraryName name 'sk4d_image_make_texture_image'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_make_with_filter;          external LibraryName name 'sk4d_image_make_with_filter'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_peek_pixels;               external LibraryName name 'sk4d_image_peek_pixels'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_read_pixels;               external LibraryName name 'sk4d_image_read_pixels'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_image_scale_pixels;              external LibraryName name 'sk4d_image_scale_pixels'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
{$ENDIF}


{ include/c/sk4d_imageencoder.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_imageencoder_encode_to_file   := GetProcAddress(LibraryHandle, PChar('sk4d_imageencoder_encode_to_file'));
  sk4d_imageencoder_encode_to_stream := GetProcAddress(LibraryHandle, PChar('sk4d_imageencoder_encode_to_stream'));
{$ELSE}
function sk4d_imageencoder_encode_to_file;   external LibraryName name 'sk4d_imageencoder_encode_to_file'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_imageencoder_encode_to_stream; external LibraryName name 'sk4d_imageencoder_encode_to_stream'{$IFDEF IOS} dependency 'c++'{$ENDIF};
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
function  sk4d_imagefilter_can_compute_fast_bounds;   external LibraryName name 'sk4d_imagefilter_can_compute_fast_bounds'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_imagefilter_compute_fast_bounds;       external LibraryName name 'sk4d_imagefilter_compute_fast_bounds'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_alpha_threshold;      external LibraryName name 'sk4d_imagefilter_make_alpha_threshold'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_arithmetic;           external LibraryName name 'sk4d_imagefilter_make_arithmetic'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_blend;                external LibraryName name 'sk4d_imagefilter_make_blend'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_blur;                 external LibraryName name 'sk4d_imagefilter_make_blur'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_colorfilter;          external LibraryName name 'sk4d_imagefilter_make_colorfilter'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_compose;              external LibraryName name 'sk4d_imagefilter_make_compose'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_dilate;               external LibraryName name 'sk4d_imagefilter_make_dilate'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_displacement_map;     external LibraryName name 'sk4d_imagefilter_make_displacement_map'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_distant_lit_diffuse;  external LibraryName name 'sk4d_imagefilter_make_distant_lit_diffuse' {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_distant_lit_specular; external LibraryName name 'sk4d_imagefilter_make_distant_lit_specular'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_drop_shadow;          external LibraryName name 'sk4d_imagefilter_make_drop_shadow'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_drop_shadow_only;     external LibraryName name 'sk4d_imagefilter_make_drop_shadow_only'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_erode;                external LibraryName name 'sk4d_imagefilter_make_erode'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_image;                external LibraryName name 'sk4d_imagefilter_make_image'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_magnifier;            external LibraryName name 'sk4d_imagefilter_make_magnifier'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_matrix_convolution;   external LibraryName name 'sk4d_imagefilter_make_matrix_convolution'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_matrix_transform;     external LibraryName name 'sk4d_imagefilter_make_matrix_transform'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_merge;                external LibraryName name 'sk4d_imagefilter_make_merge'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_offset;               external LibraryName name 'sk4d_imagefilter_make_offset'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_picture;              external LibraryName name 'sk4d_imagefilter_make_picture'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_point_lit_diffuse;    external LibraryName name 'sk4d_imagefilter_make_point_lit_diffuse'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_point_lit_specular;   external LibraryName name 'sk4d_imagefilter_make_point_lit_specular'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_runtime_shader;       external LibraryName name 'sk4d_imagefilter_make_runtime_shader'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_runtime_shader2;      external LibraryName name 'sk4d_imagefilter_make_runtime_shader2'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_shader;               external LibraryName name 'sk4d_imagefilter_make_shader'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_spot_lit_diffuse;     external LibraryName name 'sk4d_imagefilter_make_spot_lit_diffuse'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_spot_lit_specular;    external LibraryName name 'sk4d_imagefilter_make_spot_lit_specular'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_tile;                 external LibraryName name 'sk4d_imagefilter_make_tile'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_imagefilter_make_with_local_matrix;    external LibraryName name 'sk4d_imagefilter_make_with_local_matrix'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
{$ENDIF}


{ include/c/sk4d_maskfilter.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_maskfilter_make_blur        := GetProcAddress(LibraryHandle, PChar('sk4d_maskfilter_make_blur'));
  sk4d_maskfilter_make_shader      := GetProcAddress(LibraryHandle, PChar('sk4d_maskfilter_make_shader'));
  sk4d_maskfilter_make_table       := GetProcAddress(LibraryHandle, PChar('sk4d_maskfilter_make_table'));
  sk4d_maskfilter_make_table_clip  := GetProcAddress(LibraryHandle, PChar('sk4d_maskfilter_make_table_clip'));
  sk4d_maskfilter_make_table_gamma := GetProcAddress(LibraryHandle, PChar('sk4d_maskfilter_make_table_gamma'));
{$ELSE}
function sk4d_maskfilter_make_blur;        external LibraryName name 'sk4d_maskfilter_make_blur'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_maskfilter_make_shader;      external LibraryName name 'sk4d_maskfilter_make_shader'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_maskfilter_make_table;       external LibraryName name 'sk4d_maskfilter_make_table'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_maskfilter_make_table_clip;  external LibraryName name 'sk4d_maskfilter_make_table_clip' {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_maskfilter_make_table_gamma; external LibraryName name 'sk4d_maskfilter_make_table_gamma'{$IFDEF IOS} dependency 'c++'{$ENDIF};
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
function  sk4d_paint_create;             external LibraryName name 'sk4d_paint_create'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paint_create2;            external LibraryName name 'sk4d_paint_create2'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paint_destroy;            external LibraryName name 'sk4d_paint_destroy'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paint_get_alpha;          external LibraryName name 'sk4d_paint_get_alpha'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paint_get_alphaf;         external LibraryName name 'sk4d_paint_get_alphaf'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paint_get_anti_alias;     external LibraryName name 'sk4d_paint_get_anti_alias'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paint_get_blender;        external LibraryName name 'sk4d_paint_get_blender'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paint_get_color;          external LibraryName name 'sk4d_paint_get_color'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paint_get_colorf;         external LibraryName name 'sk4d_paint_get_colorf'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paint_get_color_filter;   external LibraryName name 'sk4d_paint_get_color_filter'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paint_get_dither;         external LibraryName name 'sk4d_paint_get_dither'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paint_get_fill_path;      external LibraryName name 'sk4d_paint_get_fill_path'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paint_get_image_filter;   external LibraryName name 'sk4d_paint_get_image_filter'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paint_get_mask_filter;    external LibraryName name 'sk4d_paint_get_mask_filter' {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paint_get_path_effect;    external LibraryName name 'sk4d_paint_get_path_effect' {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paint_get_shader;         external LibraryName name 'sk4d_paint_get_shader'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paint_get_stroke_cap;     external LibraryName name 'sk4d_paint_get_stroke_cap'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paint_get_stroke_join;    external LibraryName name 'sk4d_paint_get_stroke_join' {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paint_get_stroke_miter;   external LibraryName name 'sk4d_paint_get_stroke_miter'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paint_get_stroke_width;   external LibraryName name 'sk4d_paint_get_stroke_width'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paint_get_style;          external LibraryName name 'sk4d_paint_get_style'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paint_reset;              external LibraryName name 'sk4d_paint_reset'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paint_set_alpha;          external LibraryName name 'sk4d_paint_set_alpha'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paint_set_alphaf;         external LibraryName name 'sk4d_paint_set_alphaf'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paint_set_antialias;      external LibraryName name 'sk4d_paint_set_antialias'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paint_set_argb;           external LibraryName name 'sk4d_paint_set_argb'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paint_set_blender;        external LibraryName name 'sk4d_paint_set_blender'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paint_set_color;          external LibraryName name 'sk4d_paint_set_color'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paint_set_colorf;         external LibraryName name 'sk4d_paint_set_colorf'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paint_set_color_filter;   external LibraryName name 'sk4d_paint_set_color_filter'{$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paint_set_dither;         external LibraryName name 'sk4d_paint_set_dither'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paint_set_image_filter;   external LibraryName name 'sk4d_paint_set_image_filter'{$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paint_set_mask_filter;    external LibraryName name 'sk4d_paint_set_mask_filter' {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paint_set_path_effect;    external LibraryName name 'sk4d_paint_set_path_effect' {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paint_set_shader;         external LibraryName name 'sk4d_paint_set_shader'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paint_set_stroke_cap;     external LibraryName name 'sk4d_paint_set_stroke_cap'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paint_set_stroke_join;    external LibraryName name 'sk4d_paint_set_stroke_join' {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paint_set_stroke_miter;   external LibraryName name 'sk4d_paint_set_stroke_miter'{$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paint_set_stroke_width;   external LibraryName name 'sk4d_paint_set_stroke_width'{$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paint_set_style;          external LibraryName name 'sk4d_paint_set_style'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
procedure sk4d_opbuilder_add;               external LibraryName name 'sk4d_opbuilder_add'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_opbuilder_create;            external LibraryName name 'sk4d_opbuilder_create'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_opbuilder_destroy;           external LibraryName name 'sk4d_opbuilder_destroy'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_opbuilder_detach;            external LibraryName name 'sk4d_opbuilder_detach'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_path_contains;               external LibraryName name 'sk4d_path_contains'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_path_convert_conic_to_quads; external LibraryName name 'sk4d_path_convert_conic_to_quads'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_path_create;                 external LibraryName name 'sk4d_path_create'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_path_create2;                external LibraryName name 'sk4d_path_create2'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_path_destroy;                external LibraryName name 'sk4d_path_destroy'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_path_get_bounds;             external LibraryName name 'sk4d_path_get_bounds'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_path_get_fill_type;          external LibraryName name 'sk4d_path_get_fill_type'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_path_get_last_point;         external LibraryName name 'sk4d_path_get_last_point'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_path_get_segment_masks;      external LibraryName name 'sk4d_path_get_segment_masks'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_path_get_tight_bounds;       external LibraryName name 'sk4d_path_get_tight_bounds'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_path_interpolate;            external LibraryName name 'sk4d_path_interpolate'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_path_is_convex;              external LibraryName name 'sk4d_path_is_convex'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_path_is_empty;               external LibraryName name 'sk4d_path_is_empty'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_path_is_finite;              external LibraryName name 'sk4d_path_is_finite'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_path_is_interpolatable;      external LibraryName name 'sk4d_path_is_interpolatable'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_path_is_last_contour_closed; external LibraryName name 'sk4d_path_is_last_contour_closed'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_path_is_line;                external LibraryName name 'sk4d_path_is_line'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_path_is_oval;                external LibraryName name 'sk4d_path_is_oval'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_path_is_rect;                external LibraryName name 'sk4d_path_is_rect'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_path_is_rrect;               external LibraryName name 'sk4d_path_is_rrect'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_path_op;                     external LibraryName name 'sk4d_path_op'                    {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_path_serialize_to_stream;    external LibraryName name 'sk4d_path_serialize_to_stream'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_path_to_svg;                 external LibraryName name 'sk4d_path_to_svg'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_path_transform;              external LibraryName name 'sk4d_path_transform'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pathiterator_create;         external LibraryName name 'sk4d_pathiterator_create'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathiterator_destroy;        external LibraryName name 'sk4d_pathiterator_destroy'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pathiterator_next;           external LibraryName name 'sk4d_pathiterator_next'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
procedure sk4d_pathbuilder_add_arc;                 external LibraryName name 'sk4d_pathbuilder_add_arc'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_add_circle;              external LibraryName name 'sk4d_pathbuilder_add_circle'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_add_oval;                external LibraryName name 'sk4d_pathbuilder_add_oval'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_add_path;                external LibraryName name 'sk4d_pathbuilder_add_path'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_add_polygon;             external LibraryName name 'sk4d_pathbuilder_add_polygon'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_add_rect;                external LibraryName name 'sk4d_pathbuilder_add_rect'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_add_rrect;               external LibraryName name 'sk4d_pathbuilder_add_rrect'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_arc_to;                  external LibraryName name 'sk4d_pathbuilder_arc_to'                 {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_arc_to2;                 external LibraryName name 'sk4d_pathbuilder_arc_to2'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_arc_to3;                 external LibraryName name 'sk4d_pathbuilder_arc_to3'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_close;                   external LibraryName name 'sk4d_pathbuilder_close'                  {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_conic_to;                external LibraryName name 'sk4d_pathbuilder_conic_to'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pathbuilder_create;                  external LibraryName name 'sk4d_pathbuilder_create'                 {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pathbuilder_create2;                 external LibraryName name 'sk4d_pathbuilder_create2'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_cubic_to;                external LibraryName name 'sk4d_pathbuilder_cubic_to'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_destroy;                 external LibraryName name 'sk4d_pathbuilder_destroy'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pathbuilder_detach;                  external LibraryName name 'sk4d_pathbuilder_detach'                 {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_get_bounds;              external LibraryName name 'sk4d_pathbuilder_get_bounds'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pathbuilder_get_fill_type;           external LibraryName name 'sk4d_pathbuilder_get_fill_type'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_inc_reserve;             external LibraryName name 'sk4d_pathbuilder_inc_reserve'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_line_to;                 external LibraryName name 'sk4d_pathbuilder_line_to'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_move_to;                 external LibraryName name 'sk4d_pathbuilder_move_to'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_offset;                  external LibraryName name 'sk4d_pathbuilder_offset'                 {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_polyline_to;             external LibraryName name 'sk4d_pathbuilder_polyline_to'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_quad_to;                 external LibraryName name 'sk4d_pathbuilder_quad_to'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_r_conic_to;              external LibraryName name 'sk4d_pathbuilder_r_conic_to'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_r_cubic_to;              external LibraryName name 'sk4d_pathbuilder_r_cubic_to'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_r_line_to;               external LibraryName name 'sk4d_pathbuilder_r_line_to'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_r_quad_to;               external LibraryName name 'sk4d_pathbuilder_r_quad_to'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_reset;                   external LibraryName name 'sk4d_pathbuilder_reset'                  {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_set_filltype;            external LibraryName name 'sk4d_pathbuilder_set_filltype'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pathbuilder_snapshot;                external LibraryName name 'sk4d_pathbuilder_snapshot'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathbuilder_toggle_inverse_filltype; external LibraryName name 'sk4d_pathbuilder_toggle_inverse_filltype'{$IFDEF IOS} dependency 'c++'{$ENDIF};
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
function sk4d_patheffect_make_1dpath;          external LibraryName name 'sk4d_patheffect_make_1dpath'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_patheffect_make_2dline;          external LibraryName name 'sk4d_patheffect_make_2dline'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_patheffect_make_2dpath;          external LibraryName name 'sk4d_patheffect_make_2dpath'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_patheffect_make_compose;         external LibraryName name 'sk4d_patheffect_make_compose'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_patheffect_make_corner;          external LibraryName name 'sk4d_patheffect_make_corner'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_patheffect_make_dash;            external LibraryName name 'sk4d_patheffect_make_dash'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_patheffect_make_discrete;        external LibraryName name 'sk4d_patheffect_make_discrete'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_patheffect_make_matrix;          external LibraryName name 'sk4d_patheffect_make_matrix'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_patheffect_make_merge;           external LibraryName name 'sk4d_patheffect_make_merge'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_patheffect_make_stroke;          external LibraryName name 'sk4d_patheffect_make_stroke'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_patheffect_make_stroke_and_fill; external LibraryName name 'sk4d_patheffect_make_stroke_and_fill'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_patheffect_make_sum;             external LibraryName name 'sk4d_patheffect_make_sum'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_patheffect_make_translate;       external LibraryName name 'sk4d_patheffect_make_translate'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_patheffect_make_trim;            external LibraryName name 'sk4d_patheffect_make_trim'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
function  sk4d_pathmeasure_create;                   external LibraryName name 'sk4d_pathmeasure_create'                  {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pathmeasure_destroy;                  external LibraryName name 'sk4d_pathmeasure_destroy'                 {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pathmeasure_get_length;               external LibraryName name 'sk4d_pathmeasure_get_length'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pathmeasure_get_matrix;               external LibraryName name 'sk4d_pathmeasure_get_matrix'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pathmeasure_get_position_and_tangent; external LibraryName name 'sk4d_pathmeasure_get_position_and_tangent'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pathmeasure_get_segment;              external LibraryName name 'sk4d_pathmeasure_get_segment'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pathmeasure_is_closed;                external LibraryName name 'sk4d_pathmeasure_is_closed'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pathmeasure_next_contour;             external LibraryName name 'sk4d_pathmeasure_next_contour'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
function  sk4d_picture_approximate_bytes_used; external LibraryName name 'sk4d_picture_approximate_bytes_used'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_picture_approximate_op_count;   external LibraryName name 'sk4d_picture_approximate_op_count'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_picture_get_cull_rect;          external LibraryName name 'sk4d_picture_get_cull_rect'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_picture_make_from_stream;       external LibraryName name 'sk4d_picture_make_from_stream'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_picture_make_shader;            external LibraryName name 'sk4d_picture_make_shader'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_picture_playback;               external LibraryName name 'sk4d_picture_playback'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_picture_serialize_to_stream;    external LibraryName name 'sk4d_picture_serialize_to_stream'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
{$ENDIF}


{ include/c/sk4d_picturerecorder.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_picturerecorder_begin_recording   := GetProcAddress(LibraryHandle, PChar('sk4d_picturerecorder_begin_recording'));
  sk4d_picturerecorder_create            := GetProcAddress(LibraryHandle, PChar('sk4d_picturerecorder_create'));
  sk4d_picturerecorder_destroy           := GetProcAddress(LibraryHandle, PChar('sk4d_picturerecorder_destroy'));
  sk4d_picturerecorder_finish_recording  := GetProcAddress(LibraryHandle, PChar('sk4d_picturerecorder_finish_recording'));
  sk4d_picturerecorder_finish_recording2 := GetProcAddress(LibraryHandle, PChar('sk4d_picturerecorder_finish_recording2'));
{$ELSE}
function  sk4d_picturerecorder_begin_recording;   external LibraryName name 'sk4d_picturerecorder_begin_recording'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_picturerecorder_create;            external LibraryName name 'sk4d_picturerecorder_create'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_picturerecorder_destroy;           external LibraryName name 'sk4d_picturerecorder_destroy'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_picturerecorder_finish_recording;  external LibraryName name 'sk4d_picturerecorder_finish_recording' {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_picturerecorder_finish_recording2; external LibraryName name 'sk4d_picturerecorder_finish_recording2'{$IFDEF IOS} dependency 'c++'{$ENDIF};
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
function  sk4d_pixmap_create;          external LibraryName name 'sk4d_pixmap_create'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pixmap_destroy;         external LibraryName name 'sk4d_pixmap_destroy'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pixmap_erase;           external LibraryName name 'sk4d_pixmap_erase'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pixmap_erase2;          external LibraryName name 'sk4d_pixmap_erase2'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pixmap_extract_subset;  external LibraryName name 'sk4d_pixmap_extract_subset' {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pixmap_get_alpha;       external LibraryName name 'sk4d_pixmap_get_alpha'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pixmap_get_alpha_type;  external LibraryName name 'sk4d_pixmap_get_alpha_type' {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pixmap_get_color;       external LibraryName name 'sk4d_pixmap_get_color'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pixmap_get_color_space; external LibraryName name 'sk4d_pixmap_get_color_space'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pixmap_get_color_type;  external LibraryName name 'sk4d_pixmap_get_color_type' {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pixmap_get_colorf;      external LibraryName name 'sk4d_pixmap_get_colorf'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pixmap_get_height;      external LibraryName name 'sk4d_pixmap_get_height'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pixmap_get_image_info;  external LibraryName name 'sk4d_pixmap_get_image_info' {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pixmap_get_pixel_addr;  external LibraryName name 'sk4d_pixmap_get_pixel_addr' {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pixmap_get_pixels;      external LibraryName name 'sk4d_pixmap_get_pixels'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pixmap_get_row_bytes;   external LibraryName name 'sk4d_pixmap_get_row_bytes'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pixmap_get_width;       external LibraryName name 'sk4d_pixmap_get_width'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pixmap_read_pixels;     external LibraryName name 'sk4d_pixmap_read_pixels'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_pixmap_scale_pixels;    external LibraryName name 'sk4d_pixmap_scale_pixels'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_pixmap_set_colorspace;  external LibraryName name 'sk4d_pixmap_set_colorspace' {$IFDEF IOS} dependency 'c++'{$ENDIF};
{$ENDIF}


{ include/c/sk4d_refcnt.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_refcnt_ref   := GetProcAddress(LibraryHandle, PChar('sk4d_refcnt_ref'));
  sk4d_refcnt_unref := GetProcAddress(LibraryHandle, PChar('sk4d_refcnt_unref'));
{$ELSE}
procedure sk4d_refcnt_ref;   external LibraryName name 'sk4d_refcnt_ref'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_refcnt_unref; external LibraryName name 'sk4d_refcnt_unref'{$IFDEF IOS} dependency 'c++'{$ENDIF};
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
function  sk4d_region_contains;              external LibraryName name 'sk4d_region_contains'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_region_contains2;             external LibraryName name 'sk4d_region_contains2'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_region_contains3;             external LibraryName name 'sk4d_region_contains3'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_region_create;                external LibraryName name 'sk4d_region_create'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_region_create2;               external LibraryName name 'sk4d_region_create2'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_region_destroy;               external LibraryName name 'sk4d_region_destroy'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_region_get_boundary_path;     external LibraryName name 'sk4d_region_get_boundary_path'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_region_get_bounds;            external LibraryName name 'sk4d_region_get_bounds'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_region_intersects;            external LibraryName name 'sk4d_region_intersects'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_region_intersects2;           external LibraryName name 'sk4d_region_intersects2'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_region_is_complex;            external LibraryName name 'sk4d_region_is_complex'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_region_is_empty;              external LibraryName name 'sk4d_region_is_empty'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_region_is_equal;              external LibraryName name 'sk4d_region_is_equal'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_region_is_rect;               external LibraryName name 'sk4d_region_is_rect'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_region_op;                    external LibraryName name 'sk4d_region_op'                   {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_region_op2;                   external LibraryName name 'sk4d_region_op2'                  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_region_quick_contains;        external LibraryName name 'sk4d_region_quick_contains'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_region_quick_reject;          external LibraryName name 'sk4d_region_quick_reject'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_region_quick_reject2;         external LibraryName name 'sk4d_region_quick_reject2'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_region_set_empty;             external LibraryName name 'sk4d_region_set_empty'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_region_set_path;              external LibraryName name 'sk4d_region_set_path'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_region_set_rect;              external LibraryName name 'sk4d_region_set_rect'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_region_set_rects;             external LibraryName name 'sk4d_region_set_rects'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_region_translate;             external LibraryName name 'sk4d_region_translate'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_regioncliperator_create;      external LibraryName name 'sk4d_regioncliperator_create'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_regioncliperator_destroy;     external LibraryName name 'sk4d_regioncliperator_destroy'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_regioncliperator_get_current; external LibraryName name 'sk4d_regioncliperator_get_current'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_regioncliperator_move_next;   external LibraryName name 'sk4d_regioncliperator_move_next'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_regioniterator_create;        external LibraryName name 'sk4d_regioniterator_create'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_regioniterator_destroy;       external LibraryName name 'sk4d_regioniterator_destroy'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_regioniterator_get_current;   external LibraryName name 'sk4d_regioniterator_get_current'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_regioniterator_move_next;     external LibraryName name 'sk4d_regioniterator_move_next'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_regionspanerator_create;      external LibraryName name 'sk4d_regionspanerator_create'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_regionspanerator_destroy;     external LibraryName name 'sk4d_regionspanerator_destroy'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_regionspanerator_next;        external LibraryName name 'sk4d_regionspanerator_next'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
function  sk4d_rrect_contains;         external LibraryName name 'sk4d_rrect_contains'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_rrect_create;           external LibraryName name 'sk4d_rrect_create'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_rrect_create2;          external LibraryName name 'sk4d_rrect_create2'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_rrect_deflate;          external LibraryName name 'sk4d_rrect_deflate'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_rrect_destroy;          external LibraryName name 'sk4d_rrect_destroy'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_rrect_get_height;       external LibraryName name 'sk4d_rrect_get_height'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_rrect_get_radii;        external LibraryName name 'sk4d_rrect_get_radii'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_rrect_get_rect;         external LibraryName name 'sk4d_rrect_get_rect'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_rrect_get_simple_radii; external LibraryName name 'sk4d_rrect_get_simple_radii'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_rrect_get_width;        external LibraryName name 'sk4d_rrect_get_width'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_rrect_inflate;          external LibraryName name 'sk4d_rrect_inflate'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_rrect_is_complex;       external LibraryName name 'sk4d_rrect_is_complex'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_rrect_is_empty;         external LibraryName name 'sk4d_rrect_is_empty'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_rrect_is_equal;         external LibraryName name 'sk4d_rrect_is_equal'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_rrect_is_nine_patch;    external LibraryName name 'sk4d_rrect_is_nine_patch'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_rrect_is_oval;          external LibraryName name 'sk4d_rrect_is_oval'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_rrect_is_rect;          external LibraryName name 'sk4d_rrect_is_rect'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_rrect_is_simple;        external LibraryName name 'sk4d_rrect_is_simple'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_rrect_is_valid;         external LibraryName name 'sk4d_rrect_is_valid'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_rrect_offset;           external LibraryName name 'sk4d_rrect_offset'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_rrect_set_empty;        external LibraryName name 'sk4d_rrect_set_empty'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_rrect_set_nine_patch;   external LibraryName name 'sk4d_rrect_set_nine_patch'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_rrect_set_oval;         external LibraryName name 'sk4d_rrect_set_oval'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_rrect_set_rect;         external LibraryName name 'sk4d_rrect_set_rect'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_rrect_set_rect2;        external LibraryName name 'sk4d_rrect_set_rect2'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_rrect_set_rect3;        external LibraryName name 'sk4d_rrect_set_rect3'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_rrect_transform;        external LibraryName name 'sk4d_rrect_transform'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
function  sk4d_runtimeblendbuilder_create;           external LibraryName name 'sk4d_runtimeblendbuilder_create'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_runtimeblendbuilder_destroy;          external LibraryName name 'sk4d_runtimeblendbuilder_destroy'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_runtimeblendbuilder_make_blender;     external LibraryName name 'sk4d_runtimeblendbuilder_make_blender'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_runtimeeffect_get_child_count;        external LibraryName name 'sk4d_runtimeeffect_get_child_count'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_runtimeeffect_get_child_name;         external LibraryName name 'sk4d_runtimeeffect_get_child_name'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_runtimeeffect_get_child_type;         external LibraryName name 'sk4d_runtimeeffect_get_child_type'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_runtimeeffect_get_uniform_count;      external LibraryName name 'sk4d_runtimeeffect_get_uniform_count'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_runtimeeffect_get_uniform_data_size;  external LibraryName name 'sk4d_runtimeeffect_get_uniform_data_size' {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_runtimeeffect_get_uniform_name;       external LibraryName name 'sk4d_runtimeeffect_get_uniform_name'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_runtimeeffect_get_uniform_offset;     external LibraryName name 'sk4d_runtimeeffect_get_uniform_offset'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_runtimeeffect_get_uniform_type;       external LibraryName name 'sk4d_runtimeeffect_get_uniform_type'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_runtimeeffect_get_uniform_type_count; external LibraryName name 'sk4d_runtimeeffect_get_uniform_type_count'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_runtimeeffect_index_of_child;         external LibraryName name 'sk4d_runtimeeffect_index_of_child'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_runtimeeffect_index_of_uniform;       external LibraryName name 'sk4d_runtimeeffect_index_of_uniform'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_runtimeeffect_make_blender;           external LibraryName name 'sk4d_runtimeeffect_make_blender'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_runtimeeffect_make_color_filter;      external LibraryName name 'sk4d_runtimeeffect_make_color_filter'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_runtimeeffect_make_for_blender;       external LibraryName name 'sk4d_runtimeeffect_make_for_blender'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_runtimeeffect_make_for_color_filter;  external LibraryName name 'sk4d_runtimeeffect_make_for_color_filter' {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_runtimeeffect_make_for_shader;        external LibraryName name 'sk4d_runtimeeffect_make_for_shader'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_runtimeeffect_make_image;             external LibraryName name 'sk4d_runtimeeffect_make_image'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_runtimeeffect_make_shader;            external LibraryName name 'sk4d_runtimeeffect_make_shader'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_runtimeeffectbuilder_set_child;       external LibraryName name 'sk4d_runtimeeffectbuilder_set_child'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_runtimeeffectbuilder_set_child2;      external LibraryName name 'sk4d_runtimeeffectbuilder_set_child2'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_runtimeeffectbuilder_set_child3;      external LibraryName name 'sk4d_runtimeeffectbuilder_set_child3'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_runtimeeffectbuilder_set_uniform;     external LibraryName name 'sk4d_runtimeeffectbuilder_set_uniform'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_runtimeeffectbuilder_get_effect;      external LibraryName name 'sk4d_runtimeeffectbuilder_get_effect'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_runtimeshaderbuilder_create;          external LibraryName name 'sk4d_runtimeshaderbuilder_create'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_runtimeshaderbuilder_destroy;         external LibraryName name 'sk4d_runtimeshaderbuilder_destroy'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_runtimeshaderbuilder_make_image;      external LibraryName name 'sk4d_runtimeshaderbuilder_make_image'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_runtimeshaderbuilder_make_shader;     external LibraryName name 'sk4d_runtimeshaderbuilder_make_shader'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
function sk4d_shader_make_blend;                       external LibraryName name 'sk4d_shader_make_blend'                      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_shader_make_color;                       external LibraryName name 'sk4d_shader_make_color'                      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_shader_make_color2;                      external LibraryName name 'sk4d_shader_make_color2'                     {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_shader_make_empty;                       external LibraryName name 'sk4d_shader_make_empty'                      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_shader_make_gradient_linear;             external LibraryName name 'sk4d_shader_make_gradient_linear'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_shader_make_gradient_linear2;            external LibraryName name 'sk4d_shader_make_gradient_linear2'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_shader_make_gradient_radial;             external LibraryName name 'sk4d_shader_make_gradient_radial'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_shader_make_gradient_radial2;            external LibraryName name 'sk4d_shader_make_gradient_radial2'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_shader_make_gradient_sweep;              external LibraryName name 'sk4d_shader_make_gradient_sweep'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_shader_make_gradient_sweep2;             external LibraryName name 'sk4d_shader_make_gradient_sweep2'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_shader_make_gradient_two_point_conical;  external LibraryName name 'sk4d_shader_make_gradient_two_point_conical' {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_shader_make_gradient_two_point_conical2; external LibraryName name 'sk4d_shader_make_gradient_two_point_conical2'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_shader_make_perlin_noise_fractal_noise;  external LibraryName name 'sk4d_shader_make_perlin_noise_fractal_noise' {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_shader_make_perlin_noise_turbulence;     external LibraryName name 'sk4d_shader_make_perlin_noise_turbulence'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_shader_make_with_color_filter;           external LibraryName name 'sk4d_shader_make_with_color_filter'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function sk4d_shader_make_with_local_matrix;           external LibraryName name 'sk4d_shader_make_with_local_matrix'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
function  sk4d_streamadapter_create;     external LibraryName name 'sk4d_streamadapter_create'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_streamadapter_destroy;    external LibraryName name 'sk4d_streamadapter_destroy'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_streamadapter_set_procs;  external LibraryName name 'sk4d_streamadapter_set_procs' {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_wstreamadapter_create;    external LibraryName name 'sk4d_wstreamadapter_create'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_wstreamadapter_destroy;   external LibraryName name 'sk4d_wstreamadapter_destroy'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_wstreamadapter_set_procs; external LibraryName name 'sk4d_wstreamadapter_set_procs'{$IFDEF IOS} dependency 'c++'{$ENDIF};
{$ENDIF}


{ include/c/sk4d_string.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_string_create   := GetProcAddress(LibraryHandle, PChar('sk4d_string_create'));
  sk4d_string_destroy  := GetProcAddress(LibraryHandle, PChar('sk4d_string_destroy'));
  sk4d_string_get_text := GetProcAddress(LibraryHandle, PChar('sk4d_string_get_text'));
{$ELSE}
function  sk4d_string_create;   external LibraryName name 'sk4d_string_create'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_string_destroy;  external LibraryName name 'sk4d_string_destroy' {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_string_get_text; external LibraryName name 'sk4d_string_get_text'{$IFDEF IOS} dependency 'c++'{$ENDIF};
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
procedure sk4d_surface_draw;                    external LibraryName name 'sk4d_surface_draw'                   {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_surface_flush;                   external LibraryName name 'sk4d_surface_flush'                  {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_surface_flush_and_submit;        external LibraryName name 'sk4d_surface_flush_and_submit'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_surface_get_canvas;              external LibraryName name 'sk4d_surface_get_canvas'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_surface_get_props;               external LibraryName name 'sk4d_surface_get_props'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_surface_make_from_mtk_view;      external LibraryName name 'sk4d_surface_make_from_mtk_view'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_surface_make_from_render_target; external LibraryName name 'sk4d_surface_make_from_render_target'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_surface_make_from_texture;       external LibraryName name 'sk4d_surface_make_from_texture'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_surface_make_image_snapshot;     external LibraryName name 'sk4d_surface_make_image_snapshot'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_surface_make_image_snapshot2;    external LibraryName name 'sk4d_surface_make_image_snapshot2'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_surface_make_raster;             external LibraryName name 'sk4d_surface_make_raster'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_surface_make_raster_direct;      external LibraryName name 'sk4d_surface_make_raster_direct'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_surface_make_render_target;      external LibraryName name 'sk4d_surface_make_render_target'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_surface_peek_pixels;             external LibraryName name 'sk4d_surface_peek_pixels'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_surface_read_pixels;             external LibraryName name 'sk4d_surface_read_pixels'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_surface_wait;                    external LibraryName name 'sk4d_surface_wait'                   {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_surface_write_pixels;            external LibraryName name 'sk4d_surface_write_pixels'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
{$ENDIF}


{ include/c/sk4d_svgcanvas.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_svgcanvas_make := GetProcAddress(LibraryHandle, PChar('sk4d_svgcanvas_make'));
{$ELSE}
function sk4d_svgcanvas_make; external LibraryName name 'sk4d_svgcanvas_make'{$IFDEF IOS} dependency 'c++'{$ENDIF};
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
function  sk4d_textblob_get_intercepts;                         external LibraryName name 'sk4d_textblob_get_intercepts'                        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_textblob_make_from_text;                         external LibraryName name 'sk4d_textblob_make_from_text'                        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_textblob_make_from_text_horizontally_positioned; external LibraryName name 'sk4d_textblob_make_from_text_horizontally_positioned'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_textblob_make_from_text_positioned;              external LibraryName name 'sk4d_textblob_make_from_text_positioned'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_textblob_make_from_text_transform;               external LibraryName name 'sk4d_textblob_make_from_text_transform'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textblob_ref;                                    external LibraryName name 'sk4d_textblob_ref'                                   {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textblob_unref;                                  external LibraryName name 'sk4d_textblob_unref'                                 {$IFDEF IOS} dependency 'c++'{$ENDIF};
{$ENDIF}


{ include/c/sk4d_tracememorydump.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_tracememorydumpbaseclass_create    := GetProcAddress(LibraryHandle, PChar('sk4d_tracememorydumpbaseclass_create'));
  sk4d_tracememorydumpbaseclass_destroy   := GetProcAddress(LibraryHandle, PChar('sk4d_tracememorydumpbaseclass_destroy'));
  sk4d_tracememorydumpbaseclass_set_procs := GetProcAddress(LibraryHandle, PChar('sk4d_tracememorydumpbaseclass_set_procs'));
{$ELSE}
function  sk4d_tracememorydumpbaseclass_create;    external LibraryName name 'sk4d_tracememorydumpbaseclass_create'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_tracememorydumpbaseclass_destroy;   external LibraryName name 'sk4d_tracememorydumpbaseclass_destroy'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_tracememorydumpbaseclass_set_procs; external LibraryName name 'sk4d_tracememorydumpbaseclass_set_procs'{$IFDEF IOS} dependency 'c++'{$ENDIF};
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
function  sk4d_typeface_get_family_name;  external LibraryName name 'sk4d_typeface_get_family_name' {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_typeface_get_slant;        external LibraryName name 'sk4d_typeface_get_slant'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_typeface_get_style;        external LibraryName name 'sk4d_typeface_get_style'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_typeface_get_weight;       external LibraryName name 'sk4d_typeface_get_weight'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_typeface_get_width;        external LibraryName name 'sk4d_typeface_get_width'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_typeface_make_default;     external LibraryName name 'sk4d_typeface_make_default'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_typeface_make_from_file;   external LibraryName name 'sk4d_typeface_make_from_file'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_typeface_make_from_stream; external LibraryName name 'sk4d_typeface_make_from_stream'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_typeface_make_from_name;   external LibraryName name 'sk4d_typeface_make_from_name'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
{$ENDIF}


{ include/c/sk4d_vertices.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_vertices_make_copy := GetProcAddress(LibraryHandle, PChar('sk4d_vertices_make_copy'));
  sk4d_vertices_ref       := GetProcAddress(LibraryHandle, PChar('sk4d_vertices_ref'));
  sk4d_vertices_unref     := GetProcAddress(LibraryHandle, PChar('sk4d_vertices_unref'));
{$ELSE}
function  sk4d_vertices_make_copy; external LibraryName name 'sk4d_vertices_make_copy'{$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_vertices_ref;       external LibraryName name 'sk4d_vertices_ref'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_vertices_unref;     external LibraryName name 'sk4d_vertices_unref'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
procedure sk4d_particleeffect_get_position;           external LibraryName name 'sk4d_particleeffect_get_position'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_particleeffect_get_rate;               external LibraryName name 'sk4d_particleeffect_get_rate'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_particleeffect_get_uniform;            external LibraryName name 'sk4d_particleeffect_get_uniform'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_particleeffect_get_uniform_count;      external LibraryName name 'sk4d_particleeffect_get_uniform_count'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_particleeffect_get_uniform_data;       external LibraryName name 'sk4d_particleeffect_get_uniform_data'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_particleeffect_get_uniform_data_count; external LibraryName name 'sk4d_particleeffect_get_uniform_data_count'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_particleeffect_get_uniform_name;       external LibraryName name 'sk4d_particleeffect_get_uniform_name'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_particleeffect_init;                   external LibraryName name 'sk4d_particleeffect_init'                  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_particleeffect_make_from_file;         external LibraryName name 'sk4d_particleeffect_make_from_file'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_particleeffect_make_from_stream;       external LibraryName name 'sk4d_particleeffect_make_from_stream'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_particleeffect_render;                 external LibraryName name 'sk4d_particleeffect_render'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_particleeffect_set_position;           external LibraryName name 'sk4d_particleeffect_set_position'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_particleeffect_set_rate;               external LibraryName name 'sk4d_particleeffect_set_rate'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_particleeffect_set_uniform;            external LibraryName name 'sk4d_particleeffect_set_uniform'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_particleeffect_start;                  external LibraryName name 'sk4d_particleeffect_start'                 {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_particleeffect_update;                 external LibraryName name 'sk4d_particleeffect_update'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
function  sk4d_skottieanimation_get_duration;     external LibraryName name 'sk4d_skottieanimation_get_duration'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_skottieanimation_get_fps;          external LibraryName name 'sk4d_skottieanimation_get_fps'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_skottieanimation_get_in_point;     external LibraryName name 'sk4d_skottieanimation_get_in_point'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_skottieanimation_get_out_point;    external LibraryName name 'sk4d_skottieanimation_get_out_point'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_skottieanimation_get_size;         external LibraryName name 'sk4d_skottieanimation_get_size'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_skottieanimation_get_version;      external LibraryName name 'sk4d_skottieanimation_get_version'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_skottieanimation_make_from_file;   external LibraryName name 'sk4d_skottieanimation_make_from_file'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_skottieanimation_make_from_stream; external LibraryName name 'sk4d_skottieanimation_make_from_stream' {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_skottieanimation_ref;              external LibraryName name 'sk4d_skottieanimation_ref'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_skottieanimation_render;           external LibraryName name 'sk4d_skottieanimation_render'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_skottieanimation_seek_frame;       external LibraryName name 'sk4d_skottieanimation_seek_frame'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_skottieanimation_seek_frame_time;  external LibraryName name 'sk4d_skottieanimation_seek_frame_time'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_skottieanimation_unref;            external LibraryName name 'sk4d_skottieanimation_unref'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
procedure sk4d_paragraph_destroy;                          external LibraryName name 'sk4d_paragraph_destroy'                         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paragraph_did_exceed_max_lines;             external LibraryName name 'sk4d_paragraph_did_exceed_max_lines'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paragraph_get_alphabetic_baseline;          external LibraryName name 'sk4d_paragraph_get_alphabetic_baseline'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paragraph_get_glyph_position_at_coordinate; external LibraryName name 'sk4d_paragraph_get_glyph_position_at_coordinate'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paragraph_get_height;                       external LibraryName name 'sk4d_paragraph_get_height'                      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paragraph_get_ideographic_baseline;         external LibraryName name 'sk4d_paragraph_get_ideographic_baseline'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paragraph_get_line_metrics;                 external LibraryName name 'sk4d_paragraph_get_line_metrics'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paragraph_get_longest_line;                 external LibraryName name 'sk4d_paragraph_get_longest_line'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paragraph_get_max_intrinsic_width;          external LibraryName name 'sk4d_paragraph_get_max_intrinsic_width'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paragraph_get_max_width;                    external LibraryName name 'sk4d_paragraph_get_max_width'                   {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paragraph_get_min_intrinsic_width;          external LibraryName name 'sk4d_paragraph_get_min_intrinsic_width'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paragraph_get_rects_for_placeholders;       external LibraryName name 'sk4d_paragraph_get_rects_for_placeholders'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paragraph_get_rects_for_range;              external LibraryName name 'sk4d_paragraph_get_rects_for_range'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paragraph_get_word_boundary;                external LibraryName name 'sk4d_paragraph_get_word_boundary'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paragraph_layout;                           external LibraryName name 'sk4d_paragraph_layout'                          {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paragraph_paint;                            external LibraryName name 'sk4d_paragraph_paint'                           {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paragraph_to_path;                          external LibraryName name 'sk4d_paragraph_to_path'                         {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paragraph_visit;                            external LibraryName name 'sk4d_paragraph_visit'                           {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
procedure sk4d_paragraphbuilder_add_placeholder; external LibraryName name 'sk4d_paragraphbuilder_add_placeholder'{$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paragraphbuilder_add_text;        external LibraryName name 'sk4d_paragraphbuilder_add_text'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paragraphbuilder_build;           external LibraryName name 'sk4d_paragraphbuilder_build'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paragraphbuilder_create;          external LibraryName name 'sk4d_paragraphbuilder_create'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paragraphbuilder_create2;         external LibraryName name 'sk4d_paragraphbuilder_create2'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paragraphbuilder_destroy;         external LibraryName name 'sk4d_paragraphbuilder_destroy'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paragraphbuilder_pop;             external LibraryName name 'sk4d_paragraphbuilder_pop'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paragraphbuilder_push_style;      external LibraryName name 'sk4d_paragraphbuilder_push_style'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
function  sk4d_paragraphstyle_create;                    external LibraryName name 'sk4d_paragraphstyle_create'                   {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paragraphstyle_destroy;                   external LibraryName name 'sk4d_paragraphstyle_destroy'                  {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paragraphstyle_disable_hinting;           external LibraryName name 'sk4d_paragraphstyle_disable_hinting'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paragraphstyle_get_ellipsis;              external LibraryName name 'sk4d_paragraphstyle_get_ellipsis'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paragraphstyle_get_height;                external LibraryName name 'sk4d_paragraphstyle_get_height'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paragraphstyle_get_max_lines;             external LibraryName name 'sk4d_paragraphstyle_get_max_lines'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paragraphstyle_get_strut_style;           external LibraryName name 'sk4d_paragraphstyle_get_strut_style'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paragraphstyle_get_text_align;            external LibraryName name 'sk4d_paragraphstyle_get_text_align'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paragraphstyle_get_text_direction;        external LibraryName name 'sk4d_paragraphstyle_get_text_direction'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paragraphstyle_get_text_height_behaviors; external LibraryName name 'sk4d_paragraphstyle_get_text_height_behaviors'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_paragraphstyle_get_text_style;            external LibraryName name 'sk4d_paragraphstyle_get_text_style'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paragraphstyle_set_ellipsis;              external LibraryName name 'sk4d_paragraphstyle_set_ellipsis'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paragraphstyle_set_height;                external LibraryName name 'sk4d_paragraphstyle_set_height'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paragraphstyle_set_max_lines;             external LibraryName name 'sk4d_paragraphstyle_set_max_lines'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paragraphstyle_set_strut_style;           external LibraryName name 'sk4d_paragraphstyle_set_strut_style'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paragraphstyle_set_text_align;            external LibraryName name 'sk4d_paragraphstyle_set_text_align'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paragraphstyle_set_text_direction;        external LibraryName name 'sk4d_paragraphstyle_set_text_direction'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paragraphstyle_set_text_height_behaviors; external LibraryName name 'sk4d_paragraphstyle_set_text_height_behaviors'{$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_paragraphstyle_set_text_style;            external LibraryName name 'sk4d_paragraphstyle_set_text_style'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_strutstyle_create;                        external LibraryName name 'sk4d_strutstyle_create'                       {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_strutstyle_destroy;                       external LibraryName name 'sk4d_strutstyle_destroy'                      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_strutstyle_get_enabled;                   external LibraryName name 'sk4d_strutstyle_get_enabled'                  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_strutstyle_get_font_families;             external LibraryName name 'sk4d_strutstyle_get_font_families'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_strutstyle_get_font_size;                 external LibraryName name 'sk4d_strutstyle_get_font_size'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_strutstyle_get_font_style;                external LibraryName name 'sk4d_strutstyle_get_font_style'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_strutstyle_get_force_height;              external LibraryName name 'sk4d_strutstyle_get_force_height'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_strutstyle_get_half_leading;              external LibraryName name 'sk4d_strutstyle_get_half_leading'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_strutstyle_get_height_multiplier;         external LibraryName name 'sk4d_strutstyle_get_height_multiplier'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_strutstyle_get_leading;                   external LibraryName name 'sk4d_strutstyle_get_leading'                  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_strutstyle_is_equal;                      external LibraryName name 'sk4d_strutstyle_is_equal'                     {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_strutstyle_set_enabled;                   external LibraryName name 'sk4d_strutstyle_set_enabled'                  {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_strutstyle_set_font_families;             external LibraryName name 'sk4d_strutstyle_set_font_families'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_strutstyle_set_font_size;                 external LibraryName name 'sk4d_strutstyle_set_font_size'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_strutstyle_set_font_style;                external LibraryName name 'sk4d_strutstyle_set_font_style'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_strutstyle_set_force_height;              external LibraryName name 'sk4d_strutstyle_set_force_height'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_strutstyle_set_half_leading;              external LibraryName name 'sk4d_strutstyle_set_half_leading'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_strutstyle_set_height_multiplier;         external LibraryName name 'sk4d_strutstyle_set_height_multiplier'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_strutstyle_set_leading;                   external LibraryName name 'sk4d_strutstyle_set_leading'                  {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
procedure sk4d_textstyle_add_font_feature;         external LibraryName name 'sk4d_textstyle_add_font_feature'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textstyle_add_shadow;               external LibraryName name 'sk4d_textstyle_add_shadow'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textstyle_clear_background_color;   external LibraryName name 'sk4d_textstyle_clear_background_color'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textstyle_clear_foreground_color;   external LibraryName name 'sk4d_textstyle_clear_foreground_color'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_textstyle_create;                   external LibraryName name 'sk4d_textstyle_create'                  {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textstyle_destroy;                  external LibraryName name 'sk4d_textstyle_destroy'                 {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_textstyle_get_background;           external LibraryName name 'sk4d_textstyle_get_background'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_textstyle_get_color;                external LibraryName name 'sk4d_textstyle_get_color'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_textstyle_get_decoration_color;     external LibraryName name 'sk4d_textstyle_get_decoration_color'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_textstyle_get_decoration_style;     external LibraryName name 'sk4d_textstyle_get_decoration_style'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_textstyle_get_decoration_thickness; external LibraryName name 'sk4d_textstyle_get_decoration_thickness'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_textstyle_get_decorations;          external LibraryName name 'sk4d_textstyle_get_decorations'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_textstyle_get_font_families;        external LibraryName name 'sk4d_textstyle_get_font_families'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textstyle_get_font_metrics;         external LibraryName name 'sk4d_textstyle_get_font_metrics'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_textstyle_get_font_size;            external LibraryName name 'sk4d_textstyle_get_font_size'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textstyle_get_font_style;           external LibraryName name 'sk4d_textstyle_get_font_style'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_textstyle_get_foreground;           external LibraryName name 'sk4d_textstyle_get_foreground'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_textstyle_get_half_leading;         external LibraryName name 'sk4d_textstyle_get_half_leading'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_textstyle_get_height_multiplier;    external LibraryName name 'sk4d_textstyle_get_height_multiplier'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_textstyle_get_letter_spacing;       external LibraryName name 'sk4d_textstyle_get_letter_spacing'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_textstyle_get_locale;               external LibraryName name 'sk4d_textstyle_get_locale'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_textstyle_get_word_spacing;         external LibraryName name 'sk4d_textstyle_get_word_spacing'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_textstyle_is_equal;                 external LibraryName name 'sk4d_textstyle_is_equal'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textstyle_reset_font_features;      external LibraryName name 'sk4d_textstyle_reset_font_features'     {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textstyle_reset_shadows;            external LibraryName name 'sk4d_textstyle_reset_shadows'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textstyle_set_background_color;     external LibraryName name 'sk4d_textstyle_set_background_color'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textstyle_set_color;                external LibraryName name 'sk4d_textstyle_set_color'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textstyle_set_decoration_color;     external LibraryName name 'sk4d_textstyle_set_decoration_color'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textstyle_set_decoration_style;     external LibraryName name 'sk4d_textstyle_set_decoration_style'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textstyle_set_decoration_thickness; external LibraryName name 'sk4d_textstyle_set_decoration_thickness'{$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textstyle_set_decorations;          external LibraryName name 'sk4d_textstyle_set_decorations'         {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textstyle_set_font_families;        external LibraryName name 'sk4d_textstyle_set_font_families'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textstyle_set_font_size;            external LibraryName name 'sk4d_textstyle_set_font_size'           {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textstyle_set_font_style;           external LibraryName name 'sk4d_textstyle_set_font_style'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textstyle_set_foreground_color;     external LibraryName name 'sk4d_textstyle_set_foreground_color'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textstyle_set_half_leading;         external LibraryName name 'sk4d_textstyle_set_half_leading'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textstyle_set_height_multiplier;    external LibraryName name 'sk4d_textstyle_set_height_multiplier'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textstyle_set_letter_spacing;       external LibraryName name 'sk4d_textstyle_set_letter_spacing'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textstyle_set_locale;               external LibraryName name 'sk4d_textstyle_set_locale'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_textstyle_set_word_spacing;         external LibraryName name 'sk4d_textstyle_set_word_spacing'        {$IFDEF IOS} dependency 'c++'{$ENDIF};
{$ENDIF}


{ modules/skparagraph/include/sk4d_typefacefontprovider.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_typefacefontprovider_create             := GetProcAddress(LibraryHandle, PChar('sk4d_typefacefontprovider_create'));
  sk4d_typefacefontprovider_register_typeface  := GetProcAddress(LibraryHandle, PChar('sk4d_typefacefontprovider_register_typeface'));
  sk4d_typefacefontprovider_register_typeface2 := GetProcAddress(LibraryHandle, PChar('sk4d_typefacefontprovider_register_typeface2'));
{$ELSE}
function  sk4d_typefacefontprovider_create;             external LibraryName name 'sk4d_typefacefontprovider_create'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_typefacefontprovider_register_typeface;  external LibraryName name 'sk4d_typefacefontprovider_register_typeface' {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_typefacefontprovider_register_typeface2; external LibraryName name 'sk4d_typefacefontprovider_register_typeface2'{$IFDEF IOS} dependency 'c++'{$ENDIF};
{$ENDIF}


{ modules/skresources/include/sk4d_resources.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_resourceproviderbaseclass_create    := GetProcAddress(LibraryHandle, PChar('sk4d_resourceproviderbaseclass_create'));
  sk4d_resourceproviderbaseclass_set_procs := GetProcAddress(LibraryHandle, PChar('sk4d_resourceproviderbaseclass_set_procs'));
{$ELSE}
function  sk4d_resourceproviderbaseclass_create;    external LibraryName name 'sk4d_resourceproviderbaseclass_create'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_resourceproviderbaseclass_set_procs; external LibraryName name 'sk4d_resourceproviderbaseclass_set_procs'{$IFDEF IOS} dependency 'c++'{$ENDIF};
{$ENDIF}


{ modules/skshaper/include/sk4d_shaper.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_shaper_create  := GetProcAddress(LibraryHandle, PChar('sk4d_shaper_create'));
  sk4d_shaper_destroy := GetProcAddress(LibraryHandle, PChar('sk4d_shaper_destroy'));
  sk4d_shaper_shape   := GetProcAddress(LibraryHandle, PChar('sk4d_shaper_shape'));
{$ELSE}
function  sk4d_shaper_create;  external LibraryName name 'sk4d_shaper_create' {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_shaper_destroy; external LibraryName name 'sk4d_shaper_destroy'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_shaper_shape;   external LibraryName name 'sk4d_shaper_shape'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
function  sk4d_unicode_create;               external LibraryName name 'sk4d_unicode_create'              {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_unicode_destroy;              external LibraryName name 'sk4d_unicode_destroy'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_unicode_for_each_bidi_region; external LibraryName name 'sk4d_unicode_for_each_bidi_region'{$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_unicode_for_each_break;       external LibraryName name 'sk4d_unicode_for_each_break'      {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_unicode_for_each_codepoint;   external LibraryName name 'sk4d_unicode_for_each_codepoint'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_unicodebreakiterator_create;  external LibraryName name 'sk4d_unicodebreakiterator_create' {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_unicodebreakiterator_create2; external LibraryName name 'sk4d_unicodebreakiterator_create2'{$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_unicodebreakiterator_destroy; external LibraryName name 'sk4d_unicodebreakiterator_destroy'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_unicodebreakiterator_next;    external LibraryName name 'sk4d_unicodebreakiterator_next'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
function  sk4d_svgdom_find_node_by_id;    external LibraryName name 'sk4d_svgdom_find_node_by_id'   {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_svgdom_get_root;           external LibraryName name 'sk4d_svgdom_get_root'          {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_svgdom_make_from_file;     external LibraryName name 'sk4d_svgdom_make_from_file'    {$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_svgdom_make_from_stream;   external LibraryName name 'sk4d_svgdom_make_from_stream'  {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_svgdom_render;             external LibraryName name 'sk4d_svgdom_render'            {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_svgdom_set_container_size; external LibraryName name 'sk4d_svgdom_set_container_size'{$IFDEF IOS} dependency 'c++'{$ENDIF};
{$ENDIF}


{ modules/svg/include/sk4d_svgnode.h }

{$IFNDEF SK_STATIC_LIBRARY}
  sk4d_svgnode_set_attribute := GetProcAddress(LibraryHandle, PChar('sk4d_svgnode_set_attribute'));
{$ELSE}
function sk4d_svgnode_set_attribute; external LibraryName name 'sk4d_svgnode_set_attribute'{$IFDEF IOS} dependency 'c++'{$ENDIF};
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
procedure sk4d_svgsvg_get_height;                external LibraryName name 'sk4d_svgsvg_get_height'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_svgsvg_get_intrinsic_size;        external LibraryName name 'sk4d_svgsvg_get_intrinsic_size'       {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_svgsvg_get_preserve_aspect_ratio; external LibraryName name 'sk4d_svgsvg_get_preserve_aspect_ratio'{$IFDEF IOS} dependency 'c++'{$ENDIF};
function  sk4d_svgsvg_get_view_box;              external LibraryName name 'sk4d_svgsvg_get_view_box'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_svgsvg_get_width;                 external LibraryName name 'sk4d_svgsvg_get_width'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_svgsvg_get_x;                     external LibraryName name 'sk4d_svgsvg_get_x'                    {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_svgsvg_get_y;                     external LibraryName name 'sk4d_svgsvg_get_y'                    {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_svgsvg_set_height;                external LibraryName name 'sk4d_svgsvg_set_height'               {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_svgsvg_set_preserve_aspect_ratio; external LibraryName name 'sk4d_svgsvg_set_preserve_aspect_ratio'{$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_svgsvg_set_view_box;              external LibraryName name 'sk4d_svgsvg_set_view_box'             {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_svgsvg_set_width;                 external LibraryName name 'sk4d_svgsvg_set_width'                {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_svgsvg_set_x;                     external LibraryName name 'sk4d_svgsvg_set_x'                    {$IFDEF IOS} dependency 'c++'{$ENDIF};
procedure sk4d_svgsvg_set_y;                     external LibraryName name 'sk4d_svgsvg_set_y'                    {$IFDEF IOS} dependency 'c++'{$ENDIF};
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
  if {$IFDEF FPC}InterlockedDecrement{$ELSE}AtomicDecrement{$ENDIF}(InitCount) = 0 then
    FreeLibrary(LibraryHandle);
end;
{$ELSE}
procedure SkFinalize;
begin
end;
{$ENDIF}

end.
