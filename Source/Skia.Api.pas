{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2011-2021 Google LLC.                                    }
{ Copyright (c) 2021 Skia4Delphi Project.                                }
{                                                                        }
{ Use of this source code is governed by a BSD-style license that can be }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Skia.Api;

interface

{$ALIGN ON}
{$MINENUMSIZE 4}

{$IF defined(IOS)}
  {$IFNDEF CPUARM}
    {$MESSAGE ERROR 'iOS simulator is not supported.'}
  {$ENDIF}
  {$UNDEF SK_DYNAMIC_LOADING}
{$ELSEIF NOT defined(SK_DYNAMIC_LINKING)}
  {$DEFINE SK_DYNAMIC_LOADING}
{$ENDIF}

uses
  { Delphi }

{$IF defined(MSWINDOWS) and defined(SK_DYNAMIC_LOADING)}
  Winapi.Windows,
{$ENDIF}
  System.SysUtils;

type
  {$REGION 'C types'}

  bool       = System.Boolean;
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
  {$IFDEF NEXTGEN}
  char = _AnsiChar;
  {$ELSE}
  char = AnsiChar;
  {$ENDIF}

  pbool      = ^bool;
  pchar      = ^char;
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
  gr_recordingcontext_t         = THandle;
  sk_canvas_t                   = THandle;
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
  sk_runtimeeffectuniform_t     = THandle;
  sk_shader_t                   = THandle;
  sk_stream_t                   = THandle;
  sk_streamasset_t              = THandle;
  sk_string_t                   = THandle;
  sk_surface_t                  = THandle;
  sk_surfaceprops_t             = THandle;
  sk_textblob_t                 = THandle;
  sk_textblobbuilder_t          = THandle;
  sk_tracememorydump_t          = THandle;
  sk_tracememorydumpbaseclass_t = THandle;
  sk_typeface_t                 = THandle;
  sk_vertices_t                 = THandle;
  sk_wstream_t                  = THandle;

  pgr_backendformat_t            = ^gr_backendformat_t;
  pgr_backendrendertarget_t      = ^gr_backendrendertarget_t;
  pgr_backendtexture_t           = ^gr_backendtexture_t;
  pgr_directcontext_t            = ^gr_directcontext_t;
  pgr_recordingcontext_t         = ^gr_recordingcontext_t;
  psk_canvas_t                   = ^sk_canvas_t;
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
  psk_runtimeeffectuniform_t     = ^sk_runtimeeffectuniform_t;
  psk_shader_t                   = ^sk_shader_t;
  psk_stream_t                   = ^sk_stream_t;
  psk_streamasset_t              = ^sk_streamasset_t;
  psk_string_t                   = ^sk_string_t;
  psk_surface_t                  = ^sk_surface_t;
  psk_surfaceprops_t             = ^sk_surfaceprops_t;
  psk_textblob_t                 = ^sk_textblob_t;
  psk_textblobbuilder_t          = ^sk_textblobbuilder_t;
  psk_tracememorydump_t          = ^sk_tracememorydump_t;
  psk_tracememorydumpbaseclass_t = ^sk_tracememorydumpbaseclass_t;
  psk_typeface_t                 = ^sk_typeface_t;
  psk_vertices_t                 = ^sk_vertices_t;
  psk_wstream_t                  = ^sk_wstream_t;

  gr_backendapi_t = (
    OPEN_GL_GR_BACKENDAPI,
    METAL_GL_GR_BACKENDAPI = 2
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
    EOR_SK_BLENDMODE,
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
    RGBA16161616_SK_COLORTYPE
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

  sk_filterquality_t = (
    NONE_SK_FILTERQUALITY,
    LOW_SK_FILTERQUALITY,
    MEDIUM_SK_FILTERQUALITY,
    HIGH_SK_FILTERQUALITY
  );
  psk_filterquality_t = ^sk_filterquality_t;

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

  sk_highcontrastconfiginvertstyle_t = (
    NO_INVERT_SK_HIGHCONTRASTCONFIGINVERTSTYLE,
    INVERT_BRIGHTNESS_SK_HIGHCONTRASTCONFIGINVERTSTYLE,
    INVERT_LIGHTNESS_SK_HIGHCONTRASTCONFIGINVERTSTYLE
  );
  psk_highcontrastconfiginvertstyle_t = ^sk_highcontrastconfiginvertstyle_t;

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
    EOR_SK_PATHOP,
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
    EOR_SK_REGIONOP,
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

  sk_rrecttype_t = (
    EMPTY_SK_RRECTTYPE,
    RECT_SK_RRECTTYPE,
    OVAL_SK_RRECTTYPE,
    SIMPLE_SK_RRECTTYPE,
    NINE_PATCH_SK_RRECTTYPE,
    COMPLEX_SK_RRECTTYPE
  );
  psk_rrecttype_t = ^sk_rrecttype_t;

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
    REPLICATE_SK_TILEMODE,
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
    flags                   : uint32_t;
    top                     : float;
    ascent                  : float;
    descent                 : float;
    bottom                  : float;
    leading                 : float;
    average_character_width : float;
    max_character_width     : float;
    x_min                   : float;
    x_max                   : float;
    x_height                : float;
    cap_height              : float;
    underline_thickness     : float;
    underline_position      : float;
    strikeout_thickness     : float;
    strikeout_position      : float;
  end;
  psk_fontmetrics_t = ^sk_fontmetrics_t;

  sk_fontstyle_t = record
    weight: int32_t;
    width: int32_t;
    slant: sk_fontslant_t;
  end;
  psk_fontstyle_t = ^sk_fontstyle_t;

  sk_highcontrastconfig_t = record
    grayscale    : bool;
    invert_style : sk_highcontrastconfiginvertstyle_t;
    contrast     : float;
  end;
  psk_highcontrastconfig_t = ^sk_highcontrastconfig_t;

  sk_imageinfo_t = record
    colorspace : sk_colorspace_t;
    width      : int32_t;
    height     : int32_t;
    colortype  : sk_colortype_t;
    alphatype  : sk_alphatype_t;
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
    verb   : sk_pathverb_t;
    point1 : sk_point_t;
    point2 : sk_point_t;
    point3 : sk_point_t;
    case Integer of
      0: (point4: sk_point_t);
      1: (weight: float);
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

  sk_runbuffer_t = record
    glyphs    : psk_glyphid_t;
    positions : pfloat;
  end;
  psk_runbuffer_t = ^sk_runbuffer_t;

  sk_size_t = record
    width  : float;
    height : float;
  end;
  psk_size_t = ^sk_size_t;

  sk_vector_t  = sk_point_t;
  psk_vector_t = ^sk_vector_t;

  sk_debug_msg_proc              = procedure (const msg: MarshaledAString); cdecl;
  sk_font_glyph_path_proc        = procedure (const path: sk_path_t; const matrix: psk_matrix_t; context: Pointer); cdecl;
  sk_surface_raster_release_proc = procedure (pixels, context: Pointer); cdecl;

  sk_managedstream_procs_t = record
    get_length   : function  (context: Pointer): size_t; cdecl;
    get_position : function  (context: Pointer): size_t; cdecl;
    read         : function  (context: Pointer; buffer: Pointer; size: size_t): size_t; cdecl;
    seek         : function  (context: Pointer; position: size_t): bool; cdecl;
  end;
  psk_managedstream_procs_t = ^sk_managedstream_procs_t;

  sk_managedwstream_procs_t = record
    write: function  (context: Pointer; const buffer: Pointer; size: size_t): bool; cdecl;
  end;
  psk_managedwstream_procs_t = ^sk_managedwstream_procs_t;

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
    device: Pointer;
    queue: Pointer;
  end;
  pgr_mtl_backendcontext_t = ^gr_mtl_backendcontext_t;
  {$ENDREGION}

  {$REGION 'modules/skottie/include/sk4d_skottie_types.h'}
  sk_skottieanimation_t = THandle;

  psk_skottieanimation_t = ^sk_skottieanimation_t;
  {$ENDREGION}

  {$REGION 'modules/skshaper/include/sk4d_shaper_types.h'}
  sk_shaper_t                    = THandle;
  sk_shaperrunhandler_t          = THandle;
  sk_shaperrunhandlerbaseclass_t = THandle;
  sk_textblobbuilderrunhandler_t = THandle;

  psk_shaper_t                    = ^sk_shaper_t;
  psk_shaperrunhandler_t          = ^sk_shaperrunhandler_t;
  psk_shaperrunhandlerbaseclass_t = ^sk_shaperrunhandlerbaseclass_t;
  psk_textblobbuilderrunhandler_t = ^sk_textblobbuilderrunhandler_t;

  sk_shaperrunrange_t = record
    start : size_t;
    size  : size_t;
  end;
  psk_shaperrunrange_t = ^sk_shaperrunrange_t;

  sk_shaperruninfo_t = record
    font        : sk_font_t;
    bidi_level  : uint8_t;
    advance     : sk_vector_t;
    glyph_count : size_t;
    utf8_range  : sk_shaperrunrange_t;
  end;
  psk_shaperruninfo_t = ^sk_shaperruninfo_t;

  sk_shaperrunbuffer_t = record
    glyphs    : psk_glyphid_t;
    positions : psk_point_t;
    offsets   : psk_point_t;
    clusters  : puint32_t;
    point     : sk_point_t;
  end;
  psk_shaperrunbuffer_t = ^sk_shaperrunbuffer_t;

  sk_shaperrunhandlerbaseclass_procs_t = record
    begin_line        : procedure (context: Pointer); cdecl;
    commit_line       : procedure (context: Pointer); cdecl;
    commit_run_buffer : procedure (context: Pointer; const info: psk_shaperruninfo_t); cdecl;
    commit_run_info   : procedure (context: Pointer); cdecl;
    run_buffer        : procedure (context: Pointer; const info: psk_shaperruninfo_t; out result: sk_shaperrunbuffer_t); cdecl;
    run_info          : procedure (context: Pointer; const info: psk_shaperruninfo_t); cdecl;
  end;
  psk_shaperrunhandlerbaseclass_procs_t = ^sk_shaperrunhandlerbaseclass_procs_t;
  {$ENDREGION}

  {$REGION 'modules/svg/include/sk4d_svg_types.h'}
  sk_svgdom_t = THandle;

  psk_svgdom_t = ^sk_svgdom_t;
  {$ENDREGION}

  {$REGION 'modules/skparagraph/include/sk4d_paragraph_types.h'}
  sk_fontcollection_t   = THandle;
  sk_paragraph_t        = THandle;
  sk_paragraphbuilder_t = THandle;
  sk_paragraphcache_t   = THandle;
  sk_paragraphstyle_t   = THandle;
  sk_strutstyle_t       = THandle;
  sk_textstyle_t        = THandle;

  psk_fontcollection_t   = ^sk_fontcollection_t;
  psk_paragraph_t        = ^sk_paragraph_t;
  psk_paragraphbuilder_t = ^sk_paragraphbuilder_t;
  psk_paragraphcache_t   = ^sk_paragraphcache_t;
  psk_paragraphstyle_t   = ^sk_paragraphstyle_t;
  psk_strutstyle_t       = ^sk_strutstyle_t;
  psk_textstyle_t        = ^sk_textstyle_t;

  sk_drawoptions_t = (
    REGISTRY_SK_DRAWOPTIONS,
    REPLAY_SK_DRAWOPTIONS,
    DIRECT_SK_DRAWOPTIONS
  );
  psk_drawoptions_t = ^sk_drawoptions_t;

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

  sk_textdecorationmode_t = (
    GAPS_SK_TEXTDECORATIONMODE,
    THROUGH_SK_TEXTDECORATIONMODE
  );
  psk_textdecorationmode_t = ^sk_textdecorationmode_t;

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

  sk_placeholderstyle_t = record
    width           : float;
    height          : float;
    alignment       : sk_placeholderalignment_t;
    baseline        : sk_textbaseline_t;
    baseline_offset : float;
  end;
  psk_placeholderstyle_t = ^sk_placeholderstyle_t;

  sk_textbox_t = record
    rect      : sk_rect_t;
    direction : sk_textdirection_t;
  end;
  psk_textbox_t = ^sk_textbox_t;

  sk_textdecoration_t = record
    &type                : uint32_t;
    mode                 : sk_textdecorationmode_t;
    color                : sk_color_t;
    style                : sk_textdecorationstyle_t;
    thickness_multiplier : float;
  end;
  psk_textdecoration_t = ^sk_textdecoration_t;

  sk_textshadow_t = record
    color       : sk_color_t;
    offset      : sk_point_t;
    blur_radius : double;
  end;
  psk_textshadow_t = ^sk_textshadow_t;
  {$ENDREGION}
  
  {$ENDREGION}

  {$REGION 'Skia Api'}

  ESkiaApi = class(Exception);

  { TSkiaApi }
  
  TSkiaApi = class sealed
  private const
    {$IF defined(MSWINDOWS)}
    LibName = 'sk4d.dll';
    {$ELSEIF defined(IOS)}
    LibName = 'sk4d.a';
    {$ELSEIF defined(MACOS)}
    LibName = 'sk4d.dylib';
    {$ELSE}
    LibName = 'sk4d.so';
    {$ENDIF}
  strict private
  {$IF defined(MSWINDOWS) or defined(SK_DYNAMIC_LOADING)}
    class constructor Create;
  {$ENDIF}
  {$IFDEF SK_DYNAMIC_LOADING}
  strict private class var
    FLibHandle: HMODULE;
  strict private
    class destructor Destroy;
    class function GetProc(const AName: System.PChar): Pointer;
  {$ENDIF}
  public

    {$REGION 'include/c/gr4d_backendsurface.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendrendertarget_create_gl              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(width, height, sample_count, stencil_bits: int32_t; const framebuffer_info: pgr_gl_framebufferinfo_t): gr_backendrendertarget_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendrendertarget_create_mtl             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(width, height: int32_t; const texture_info: pgr_mtl_textureinfo_t): gr_backendrendertarget_t; cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_backendrendertarget_destroy                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_backendrendertarget_t); cdecl;                                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendrendertarget_get_backend_api        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendrendertarget_t): gr_backendapi_t; cdecl;                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendrendertarget_get_gl_framebuffer_info{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendrendertarget_t; out framebuffer_info: gr_gl_framebufferinfo_t): bool; cdecl;                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendrendertarget_get_height             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendrendertarget_t): int32_t; cdecl;                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendrendertarget_get_sample_count       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendrendertarget_t): int32_t; cdecl;                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendrendertarget_get_stencil_bits       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendrendertarget_t): int32_t; cdecl;                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendrendertarget_get_width              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendrendertarget_t): int32_t; cdecl;                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendrendertarget_is_valid               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendrendertarget_t): bool; cdecl;                                                                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendtexture_create_gl                   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(width, height: int32_t; mipmapped: bool; const texture_info: pgr_gl_textureinfo_t): gr_backendtexture_t; cdecl;                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendtexture_create_mtl                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(width, height: int32_t; mipmapped: bool; const texture_info: pgr_mtl_textureinfo_t): gr_backendtexture_t; cdecl;                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_backendtexture_destroy                     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_backendtexture_t); cdecl;                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendtexture_get_backend_api             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendtexture_t): gr_backendapi_t; cdecl;                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendtexture_get_gl_texture_info         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendtexture_t; out texture_info: gr_gl_textureinfo_t): bool; cdecl;                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendtexture_get_height                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendtexture_t): int32_t; cdecl;                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendtexture_get_width                   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendtexture_t): int32_t; cdecl;                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendtexture_has_mipmaps                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendtexture_t): bool; cdecl;                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_backendtexture_is_valid                    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendtexture_t): bool; cdecl;                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/gr4d_directcontext.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_abandon_context                      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t); cdecl;                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_dump_memory_statistics               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: gr_directcontext_t; trace_memory_dump: sk_tracememorydump_t); cdecl;                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_flush                                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t); cdecl;                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_flush_and_submit                     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t; sync_cpu: bool); cdecl;                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_free_gpu_resources                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t); cdecl;                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_directcontext_get_resource_cache_limit             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_directcontext_t): size_t; cdecl;                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_get_resource_cache_usage             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: gr_directcontext_t; out max_resources: int32_t; out max_resources_bytes: size_t); cdecl;              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_directcontext_make_gl                              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const gl_interface: gr_gl_interface_t): gr_directcontext_t; cdecl;                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_directcontext_make_gl2                             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const gl_interface: gr_gl_interface_t; const options: pgr_contextoptions_t): gr_directcontext_t; cdecl;           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_directcontext_make_metal                           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const backend_context: pgr_mtl_backendcontext_t): gr_directcontext_t; cdecl;                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_directcontext_make_metal2                          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const backend_context: pgr_mtl_backendcontext_t; const options: pgr_contextoptions_t): gr_directcontext_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_perform_deferred_cleanup             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t; milliseconds: int64_t); cdecl;                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_purge_unlocked_resources             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t; scratch_resources_only: bool); cdecl;                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_purge_unlocked_resources2            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t; bytes_to_purge: size_t; prefer_scratch_resources: bool); cdecl;                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_release_resources_and_abandon_context{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t); cdecl;                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_reset_context                        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t; state: uint32_t); cdecl;                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_reset_gl_texture_bindings            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t); cdecl;                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}gr4d_directcontext_set_resource_cache_limit             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t; value: size_t); cdecl;                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}gr4d_directcontext_submit                               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: gr_directcontext_t; sync_cpu: bool): bool; cdecl;                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
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

    {$REGION 'include/c/gr4d_recordingcontext.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}gr4d_recordingcontext_get_max_surface_sample_count_for_color_type{$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: gr_recordingcontext_t; color_type: sk_colortype_t): int32_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}gr4d_recordingcontext_is_abandoned                               {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(self: gr_recordingcontext_t): bool; cdecl;                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_canvas.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_clear                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; color: sk_color_t); cdecl;                                                                                                                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_clear2                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const color: psk_color4f_t); cdecl;                                                                                                                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_destroy               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t); cdecl;                                                                                                                                                                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_discard               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t); cdecl;                                                                                                                                                                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_clip_path             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const path: sk_path_t; op: sk_clipop_t; do_anti_alias: bool); cdecl;                                                                                                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_clip_rect             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const rect: psk_rect_t; op: sk_clipop_t; do_anti_alias: bool); cdecl;                                                                                                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_clip_region           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const region: sk_region_t; op: sk_clipop_t); cdecl;                                                                                                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_clip_rrect            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const rrect: sk_rrect_t; op: sk_clipop_t; do_anti_alias: bool); cdecl;                                                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_clip_shader           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; shader: sk_shader_t; op: sk_clipop_t); cdecl;                                                                                                                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_concat                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const matrix: psk_matrix44_t); cdecl;                                                                                                                                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_concat2               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const matrix: psk_matrix_t); cdecl;                                                                                                                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_annotation       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const rect: psk_rect_t; const key: MarshaledAString; value: sk_data_t); cdecl;                                                                                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_arc              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const oval: psk_rect_t; start_angle, sweep_angle: float; use_center: bool; const paint: sk_paint_t); cdecl;                                                                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_atlas            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const atlas: sk_image_t; const transforms: psk_rotationscalematrix_t; const sprites: psk_rect_t; const colors: psk_color_t; count: int32_t; blend_mode: sk_blendmode_t; const cull_rect: psk_rect_t; const paint: sk_paint_t); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_circle           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const center: psk_point_t; radius: float; const paint: sk_paint_t); cdecl;                                                                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_color            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; color: sk_color_t; blend_mode: sk_blendmode_t); cdecl;                                                                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_color2           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const color: psk_color4f_t; blend_mode: sk_blendmode_t); cdecl;                                                                                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_image            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const image: sk_image_t; x, y: float; const paint: sk_paint_t); cdecl;                                                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_image_lattice    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const image: sk_image_t; const lattice: psk_lattice_t; const dest: psk_rect_t; const paint: sk_paint_t); cdecl;                                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_image_nine       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const image: sk_image_t; const center: psk_irect_t; const dest: psk_rect_t; const paint: sk_paint_t); cdecl;                                                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_image_rect       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const image: sk_image_t; const src, dest: psk_rect_t; const paint: sk_paint_t; constraint: sk_srcrectconstraint_t); cdecl;                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_line             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const point1, point2: psk_point_t; paint: sk_paint_t); cdecl;                                                                                                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_oval             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const oval: psk_rect_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_paint            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_patch            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const cubics: psk_point_t; const colors: psk_color_t; const tex_coords: psk_point_t; blend_mode: sk_blendmode_t; const paint: sk_paint_t); cdecl;                                                                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_path             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const path: sk_path_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_picture          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const picture: sk_picture_t); cdecl;                                                                                                                                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_picture2         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const picture: sk_picture_t; const matrix: psk_matrix_t; const paint: sk_paint_t); cdecl;                                                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_point            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const point: psk_point_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_points           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; mode: sk_drawpointsmode_t; count: size_t; const points: psk_point_t; const paint: sk_paint_t); cdecl;                                                                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_rect             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const rect: psk_rect_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_region           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const region: sk_region_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_rrect            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const rrect: sk_rrect_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_rrect2           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const rect: psk_rect_t; radius_x, radius_y: float; const paint: sk_paint_t); cdecl;                                                                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_rrect_difference {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const outer, inner: sk_rrect_t; const paint: sk_paint_t); cdecl;                                                                                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_simple_text      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const text: Pointer; size: size_t; encoding: sk_textencoding_t; x, y: float; const font: sk_font_t; const paint: sk_paint_t); cdecl;                                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_text_blob        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const text_blob: sk_textblob_t; x, y: float; const paint: sk_paint_t); cdecl;                                                                                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_draw_vertices         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const vertices: sk_vertices_t; blend_mode: sk_blendmode_t; const paint: sk_paint_t); cdecl;                                                                                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_canvas_find_marked_ctm       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_canvas_t; const name: MarshaledAString; out matrix: sk_matrix44_t): bool; cdecl;                                                                                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_get_device_clip_bounds{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_canvas_t; out result: sk_irect_t); cdecl;                                                                                                                                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_get_local_clip_bounds {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_canvas_t; out result: sk_rect_t); cdecl;                                                                                                                                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_get_local_to_device   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_canvas_t; out result: sk_matrix44_t); cdecl;                                                                                                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_get_local_to_device2  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_canvas_t; out result: sk_matrix_t); cdecl;                                                                                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_canvas_get_save_count        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_canvas_t): int32_t; cdecl;                                                                                                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_canvas_is_clip_empty         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_canvas_t): bool; cdecl;                                                                                                                                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_canvas_is_clip_rect          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_canvas_t): bool; cdecl;                                                                                                                                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_mark_ctm              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const name: MarshaledAString); cdecl;                                                                                                                                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_canvas_quick_reject          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_canvas_t; const rect: psk_rect_t): bool; cdecl;                                                                                                                                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_canvas_quick_reject2         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_canvas_t; const path: sk_path_t): bool; cdecl;                                                                                                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_reset_matrix          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t); cdecl;                                                                                                                                                                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_restore               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t); cdecl;                                                                                                                                                                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_restore_to_count      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; save_count: int32_t); cdecl;                                                                                                                                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_rotate                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; degrees: float); cdecl;                                                                                                                                                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_rotate2               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; degrees, px, py: float); cdecl;                                                                                                                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_canvas_save                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_canvas_t): int32_t; cdecl;                                                                                                                                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_canvas_save_layer            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_canvas_t; const rect: psk_rect_t; const paint: sk_paint_t): int32_t; cdecl;                                                                                                                                                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_canvas_save_layer_alpha      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_canvas_t; const rect: psk_rect_t; alpha: uint8_t): int32_t; cdecl;                                                                                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_scale                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; sx, sy: float); cdecl;                                                                                                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_skew                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; kx, ky: float); cdecl;                                                                                                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_canvas_translate             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; dx, dy: float); cdecl;                                                                                                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_colorfilter.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_colorfilter_make_blend               {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(color: sk_color_t; mode: sk_blendmode_t): sk_colorfilter_t; cdecl;                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_colorfilter_make_compose             {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(outer, inner: sk_colorfilter_t): sk_colorfilter_t; cdecl;                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_colorfilter_make_high_contrast       {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const config: psk_highcontrastconfig_t): sk_colorfilter_t; cdecl;                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_colorfilter_make_hsla_matrix         {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const matrix: pfloat): sk_colorfilter_t; cdecl;                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_colorfilter_make_lerp                {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(weight: float; dest, src: sk_colorfilter_t): sk_colorfilter_t; cdecl;             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_colorfilter_make_lighting            {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(multiply, add: sk_color_t): sk_colorfilter_t; cdecl;                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_colorfilter_make_linear_to_srgb_gamma{$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(): sk_colorfilter_t; cdecl;                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_colorfilter_make_luma_color          {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(): sk_colorfilter_t; cdecl;                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_colorfilter_make_matrix              {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const matrix: pfloat): sk_colorfilter_t; cdecl;                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_colorfilter_make_overdraw            {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const colors: psk_color_t): sk_colorfilter_t; cdecl;                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_colorfilter_make_srgb_to_linear_gamma{$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(): sk_colorfilter_t; cdecl;                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_colorfilter_make_table               {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const tablea_a, tablea_r, tablea_g, tablea_b: puint8_t): sk_colorfilter_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_colorspace.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_gamma_close_to_srgb     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspace_t): bool; cdecl;                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_gamma_is_linear         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspace_t): bool; cdecl;                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_is_equal                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, color_space: sk_colorspace_t): bool; cdecl;                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_is_numerical_transfer_fn{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspace_t; out transfer_function: sk_colorspacetransferfn_t): bool; cdecl;                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_is_srgb                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspace_t): bool; cdecl;                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_is_unique               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspace_t): bool; cdecl;                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_make                    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const profile: sk_colorspaceiccprofile_t): sk_colorspace_t; cdecl;                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_make_linear_gamma       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspace_t): sk_colorspace_t; cdecl;                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_make_rgb                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const transfer_function: psk_colorspacetransferfn_t; const xyz: psk_colorspacexyz_t): sk_colorspace_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_make_srgb               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_colorspace_t; cdecl;                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_make_srgb_gamma         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspace_t): sk_colorspace_t; cdecl;                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_make_srgb_linear        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_colorspace_t; cdecl;                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_colorspace_ref                     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_colorspace_t); cdecl;                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_to_profile              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspace_t): sk_colorspaceiccprofile_t; cdecl;                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspace_to_xyz                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspace_t; out xyz: sk_colorspacexyz_t): bool; cdecl;                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_colorspace_unref                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_colorspace_t); cdecl;                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_colorspaceiccprofile_destroy       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_colorspaceiccprofile_t); cdecl;                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspaceiccprofile_get_buffer    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspaceiccprofile_t; size: puint32_t): puint8_t; cdecl;                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspaceiccprofile_make          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const input: Pointer; size: size_t): sk_colorspaceiccprofile_t; cdecl;                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspaceiccprofile_to_xyz        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspaceiccprofile_t; out dest: sk_colorspacexyz_t): bool; cdecl;                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspaceprimaries_to_xyz         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: psk_colorspaceprimaries_t; out xyz: sk_colorspacexyz_t): bool; cdecl;                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_colorspacetransferfn_hlg           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(out result: sk_colorspacetransferfn_t); cdecl;                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspacetransferfn_invert        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: psk_colorspacetransferfn_t; out transfer_function: sk_colorspacetransferfn_t): bool; cdecl;       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_colorspacetransferfn_linear        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(out result: sk_colorspacetransferfn_t); cdecl;                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_colorspacetransferfn_pq            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(out result: sk_colorspacetransferfn_t); cdecl;                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_colorspacetransferfn_rec2020       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(out result: sk_colorspacetransferfn_t); cdecl;                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_colorspacetransferfn_srgb          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(out result: sk_colorspacetransferfn_t); cdecl;                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspacetransferfn_transform     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: psk_colorspacetransferfn_t; x: float): float; cdecl;                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_colorspacetransferfn_two_dot_two   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(out result: sk_colorspacetransferfn_t); cdecl;                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_colorspacexyz_adobe_rgb            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(out result: sk_colorspacexyz_t); cdecl;                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_colorspacexyz_concat               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: psk_colorspacexyz_t; const xyz: psk_colorspacexyz_t); cdecl;                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_colorspacexyz_display_p3           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(out result: sk_colorspacexyz_t); cdecl;                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_colorspacexyz_invert               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: psk_colorspacexyz_t; out xyz: sk_colorspacexyz_t): bool; cdecl;                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_colorspacexyz_rec_2020             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(out result: sk_colorspacexyz_t); cdecl;                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_colorspacexyz_srgb                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(out result: sk_colorspacexyz_t); cdecl;                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_colorspacexyz_xyz                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(out result: sk_colorspacexyz_t); cdecl;                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_data.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_data_get_data        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_data_t): Pointer; cdecl;                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_data_get_size        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_data_t): size_t; cdecl;                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_data_is_unique       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_data_t): bool; cdecl;                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_data_make            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const data: Pointer; size: size_t): sk_data_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_data_make_from_stream{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(stream: sk_stream_t; size: size_t): sk_data_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_data_ref             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_data_t); cdecl;                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_data_unref           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_data_t); cdecl;                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_debugf.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_set_debug_msg_proc{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(proc: sk_debug_msg_proc); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_document.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_document_begin_page{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_document_t; width, height: float; const content: psk_rect_t): sk_canvas_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_document_close     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_document_t); cdecl;                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_document_end_page  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_document_t); cdecl;                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_document_make_pdf  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(w_stream: sk_wstream_t): sk_document_t; cdecl;                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_document_make_pdf2 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(w_stream: sk_wstream_t; const metadata: psk_pdfmetadata_t): sk_document_t; cdecl;          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_document_terminate {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_document_t); cdecl;                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_font.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_create                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(typeface: sk_typeface_t; size, scale_x, skew_x: float): sk_font_t; cdecl;                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_create2                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const font: sk_font_t): sk_font_t; cdecl;                                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_destroy                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t); cdecl;                                                                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_baseline_snap      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): bool; cdecl;                                                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_edging             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): sk_fontedging_t; cdecl;                                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_embedded_bitmaps   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): bool; cdecl;                                                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_embolden           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): bool; cdecl;                                                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_force_auto_hinting {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): bool; cdecl;                                                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_glyphs             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t; const text: Pointer; size: size_t; encoding: sk_textencoding_t; result: psk_glyphid_t; max_count: int32_t): int32_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_glyphs_count       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t; const text: Pointer; size: size_t; encoding: sk_textencoding_t): int32_t; cdecl;                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_hinting            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): sk_fonthinting_t; cdecl;                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_linear_metrics     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): bool; cdecl;                                                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_metrics            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t; metrics: psk_fontmetrics_t): float; cdecl;                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_get_offsets            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; result: pfloat; origin: float); cdecl;                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_path               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t; glyph: sk_glyphid_t; result: sk_path_t): bool; cdecl;                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_get_paths              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; proc: sk_font_glyph_path_proc; proc_context: Pointer); cdecl;                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_get_positions          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; result: psk_point_t; const origin: psk_point_t); cdecl;                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_scale_x            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): float; cdecl;                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_size               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): float; cdecl;                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_skew_x             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): float; cdecl;                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_subpixel           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): bool; cdecl;                                                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_typeface           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): sk_typeface_t; cdecl;                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_get_typeface_or_default{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): sk_typeface_t; cdecl;                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_get_widths_bounds      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; widths: pfloat; bounds: psk_rect_t; const paint: sk_paint_t); cdecl;           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_is_equal               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, font: sk_font_t): bool; cdecl;                                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_measure_text           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t; const text: Pointer; size: size_t; encoding: sk_textencoding_t; bounds: psk_rect_t; const paint: sk_paint_t): float; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_set_baseline_snap      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: bool); cdecl;                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_set_edging             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: sk_fontedging_t); cdecl;                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_set_embedded_bitmaps   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: bool); cdecl;                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_set_embolden           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: bool); cdecl;                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_set_force_auto_hinting {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: bool); cdecl;                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_set_hinting            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: sk_fonthinting_t); cdecl;                                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_set_linear_metrics     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: bool); cdecl;                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_set_scale_x            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: float); cdecl;                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_set_size               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: float); cdecl;                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_set_skew_x             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: float); cdecl;                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_set_subpixel           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: bool); cdecl;                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_set_typeface           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; typeface: sk_typeface_t); cdecl;                                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_font_unichar_to_glyph       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t; unichar: sk_unichar_t): sk_glyphid_t; cdecl;                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_font_unichars_to_glyphs     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_font_t; const unichars: psk_unichar_t; count: int32_t; result: psk_glyphid_t); cdecl;                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
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
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_encode_to_data        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t; format: sk_encodedimageformat_t; quality: int32_t): sk_data_t; cdecl;                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_get_alpha_type        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): sk_alphatype_t; cdecl;                                                                                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_get_color_space       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): sk_colorspace_t; cdecl;                                                                                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_get_color_type        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): sk_colortype_t; cdecl;                                                                                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_get_height            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): int32_t; cdecl;                                                                                                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_image_get_image_info        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_image_t; out result: sk_imageinfo_t); cdecl;                                                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_get_unique_id         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): uint32_t; cdecl;                                                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_get_width             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): int32_t; cdecl;                                                                                                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_is_lazy_generated     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): bool; cdecl;                                                                                                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_is_texture_backed     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): bool; cdecl;                                                                                                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_is_valid              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t; context: gr_recordingcontext_t): bool; cdecl;                                                                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_make_from_encoded     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(encoded: sk_data_t): sk_image_t; cdecl;                                                                                                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_make_non_texture_image{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): sk_image_t; cdecl;                                                                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_make_raster           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const pixmap: sk_pixmap_t): sk_image_t; cdecl;                                                                                                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_make_raster_image     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): sk_image_t; cdecl;                                                                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_make_shader           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t; tile_mode_x, tile_mode_y: sk_tilemode_t): sk_shader_t; cdecl;                                                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_make_subset           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t; const subset: psk_irect_t; context: gr_directcontext_t): sk_image_t; cdecl;                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_make_texture_image    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t; context: gr_directcontext_t; mipmapped: bool): sk_image_t; cdecl;                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_make_with_filter      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t; context: gr_recordingcontext_t; const filter: sk_imagefilter_t; const subset, clip_bounds: psk_irect_t; out out_subset: sk_irect_t; out offset: sk_ipoint_t): sk_image_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_read_pixels           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t; context: gr_directcontext_t; const dest: sk_pixmap_t; src_x, src_y: int32_t; caching_hint: sk_imagecachinghint_t): bool; cdecl;                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_image_scale_pixels          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t; const dest: sk_pixmap_t; filter_quality: sk_filterquality_t; caching_hint: sk_imagecachinghint_t): bool; cdecl;                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_imagefilter.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_alpha_threshold     {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const region: sk_region_t; inner_min, outer_max: float; input: sk_imagefilter_t): sk_imagefilter_t; cdecl;                                                                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_arithmetic          {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(k1, k2, k3, k4: float; enforce_premultiplied_color: bool; background, foreground: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_blend               {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(cmode: sk_blendmode_t; background, foreground: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
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
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_image               {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(image: sk_image_t; const src, dest: psk_rect_t; filter_quality: sk_filterquality_t): sk_imagefilter_t; cdecl;                                                                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_magnifier           {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const src: psk_rect_t; inset: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_matrix_convolution  {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const kernel_size: psk_isize_t; const kernel: pfloat; gain, bias: float; const kernel_offset: psk_ipoint_t; tile_mode: sk_tilemode_t; convolve_alpha: bool; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_matrix_transform    {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const matrix: psk_matrix_t; filter_quality: sk_filterquality_t; input: sk_imagefilter_t): sk_imagefilter_t; cdecl;                                                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_merge               {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const filters: psk_imagefilter_t; count: int32_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_offset              {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(dx, dy: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_picture             {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(picture: sk_picture_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                                                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_point_lit_diffuse   {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const location: psk_point3_t; light_color: sk_color_t; surface_scale, kd: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_point_lit_specular  {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const location: psk_point3_t; light_color: sk_color_t; surface_scale, ks, shininess: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_spot_lit_diffuse    {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const location, target: psk_point3_t; falloff_exponent, cutoff_angle: float; light_color: sk_color_t; surface_scale, kd: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_spot_lit_specular   {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const location, target: psk_point3_t; falloff_exponent, cutoff_angle: float; light_color: sk_color_t; surface_scale, ks, shininess: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_imagefilter_make_tile                {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const src, dest: psk_rect_t; input: sk_imagefilter_t): sk_imagefilter_t; cdecl;                                                                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_maskfilter.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_maskfilter_make_blur       {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(style: sk_blurstyle_t; sigma: float; respect_ctm: bool): sk_maskfilter_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_maskfilter_make_table      {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const table: puint8_t): sk_maskfilter_t; cdecl;                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_maskfilter_make_table_clip {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(min, max: uint8_t): sk_maskfilter_t; cdecl;                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_maskfilter_make_table_gamma{$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(gamma: float): sk_maskfilter_t; cdecl;                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_paint.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_create            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_paint_t; cdecl;                                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_create2           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const paint: sk_paint_t): sk_paint_t; cdecl;                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_destroy           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t); cdecl;                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_alpha         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): uint8_t; cdecl;                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_alphaf        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): float; cdecl;                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_anti_alias    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): bool; cdecl;                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_blend_mode    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_blendmode_t; cdecl;                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_color         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_color_t; cdecl;                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_get_colorf        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_paint_t; out result: sk_color4f_t); cdecl;                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_color_filter  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_colorfilter_t; cdecl;                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_dither        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): bool; cdecl;                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_fill_path     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t; const path: sk_path_t; const cull_rect: psk_rect_t; res_scale: float; result: sk_path_t): bool; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_filter_quality{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_filterquality_t; cdecl;                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_image_filter  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_imagefilter_t; cdecl;                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_mask_filter   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_maskfilter_t; cdecl;                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_path_effect   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_patheffect_t; cdecl;                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_shader        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_shader_t; cdecl;                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_stroke_cap    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_strokecap_t; cdecl;                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_stroke_join   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_strokejoin_t; cdecl;                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_stroke_miter  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): float; cdecl;                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_stroke_width  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): float; cdecl;                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paint_get_style         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_paintstyle_t; cdecl;                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_reset             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t); cdecl;                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_alpha         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: uint8_t); cdecl;                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_alphaf        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: float); cdecl;                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_antialias     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: bool); cdecl;                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_argb          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; a, r, g, b: uint8_t); cdecl;                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_blend_mode    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; mode: sk_blendmode_t); cdecl;                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_color         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_color_t); cdecl;                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_colorf        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; const value: psk_color4f_t; color_space: sk_colorspace_t); cdecl;                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_color_filter  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_colorfilter_t); cdecl;                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_dither        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: bool); cdecl;                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_filter_quality{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_filterquality_t); cdecl;                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_image_filter  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_imagefilter_t); cdecl;                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_mask_filter   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_maskfilter_t); cdecl;                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_path_effect   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_patheffect_t); cdecl;                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_shader        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_shader_t); cdecl;                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_stroke_cap    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_strokecap_t); cdecl;                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_stroke_join   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_strokejoin_t); cdecl;                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_stroke_miter  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: float); cdecl;                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_stroke_width  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: float); cdecl;                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paint_set_style         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_paintstyle_t); cdecl;                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_path.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_opbuilder_add              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_opbuilder_t; const path: sk_path_t; op: sk_pathop_t); cdecl;           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_opbuilder_create           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_opbuilder_t; cdecl;                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_opbuilder_destroy          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_opbuilder_t); cdecl;                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_opbuilder_detach           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_opbuilder_t; result: sk_path_t): bool; cdecl;                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_contains              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t; x, y: float): bool; cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_create                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_path_t; cdecl;                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_create2               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const svg: MarshaledAString): sk_path_t; cdecl;                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_path_destroy               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_path_t); cdecl;                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_path_get_bounds            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_path_t; out result: sk_rect_t); cdecl;                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_get_fill_type         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t): sk_pathfilltype_t; cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_get_last_point        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t; out result: sk_point_t): bool; cdecl;                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_get_segment_masks     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t): uint32_t; cdecl;                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_path_get_tight_bounds      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_path_t; out result: sk_rect_t); cdecl;                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_interpolate           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, cending: sk_path_t; weight: float; result: sk_path_t): bool; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_is_convex             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t): bool; cdecl;                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_is_empty              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t): bool; cdecl;                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_is_finite             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t): bool; cdecl;                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_is_interpolatable     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, path: sk_path_t): bool; cdecl;                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_is_last_contour_closed{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t): bool; cdecl;                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_is_line               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t; lines: psk_point_t): bool; cdecl;                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_is_oval               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t; oval: psk_rect_t): bool; cdecl;                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_is_rect               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t; rect: psk_rect_t): bool; cdecl;                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_is_rrect              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t; rrect: sk_rrect_t): bool; cdecl;                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_path_offset                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_path_t; dx, dy: float; result: sk_path_t); cdecl;                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_path_op                    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, path: sk_path_t; op: sk_pathop_t; result: sk_path_t): bool; cdecl;  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_path_to_svg                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_path_t; result: sk_string_t); cdecl;                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_path_transform             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_path_t; const matrix: psk_matrix_t; result: sk_path_t); cdecl;   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pathiterator_create        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const path: sk_path_t; force_close: bool): sk_pathiterator_t; cdecl;            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathiterator_destroy       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathiterator_t); cdecl;                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pathiterator_next          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_pathiterator_t; out elem: sk_pathiteratorelem_t): bool; cdecl;         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_pathbuilder.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_add_arc                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const oval: psk_rect_t; start_angle, sweep_angle: float); cdecl;                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_add_circle             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; center_x, center_y, radius: float; direction: sk_pathdirection_t); cdecl;                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathbuilder_add_oval               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const oval: psk_rect_t; direction: sk_pathdirection_t; start_index: uint32_t); cdecl;                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
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
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_patheffect_make_1dpath  {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const path: sk_path_t; advance, phase: float; style: sk_patheffect1dstyle_t): sk_patheffect_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_patheffect_make_2dline  {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(width: float; const matrix: psk_matrix_t): sk_patheffect_t; cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_patheffect_make_2dpath  {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const matrix: psk_matrix_t; const path: sk_path_t): sk_patheffect_t; cdecl;                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_patheffect_make_compose {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(outer, inner: sk_patheffect_t): sk_patheffect_t; cdecl;                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_patheffect_make_corner  {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(radius: float): sk_patheffect_t; cdecl;                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_patheffect_make_dash    {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const intervals: pfloat; count: int32_t; phase: float): sk_patheffect_t; cdecl;                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_patheffect_make_discrete{$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(seg_length, deviation: float; seed_assist: uint32_t): sk_patheffect_t; cdecl;                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_patheffect_make_sum     {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(effect1, effect2: sk_patheffect_t): sk_patheffect_t; cdecl;                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_patheffect_make_trim    {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(start, stop: float; mode: sk_patheffecttrimmode_t): sk_patheffect_t; cdecl;                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_pathmeasure.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pathmeasure_create                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const path: sk_path_t; force_closed: bool; res_scale: float): sk_pathmeasure_t; cdecl;                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pathmeasure_destroy                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathmeasure_t); cdecl;                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pathmeasure_get_length              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_pathmeasure_t): float; cdecl;                                                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pathmeasure_get_matrix              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_pathmeasure_t; distance: float; out matrix: sk_matrix_t; matrix_flags: uint32_t): bool; cdecl;    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pathmeasure_get_position_and_tangent{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_pathmeasure_t; distance: float; out position: sk_point_t; out tangent: sk_vector_t): bool; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pathmeasure_get_segment             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_pathmeasure_t; start, stop: float; result: sk_path_t; start_with_move_to: bool): bool; cdecl;     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pathmeasure_is_closed               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_pathmeasure_t): bool; cdecl;                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pathmeasure_next_contour            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_pathmeasure_t): bool; cdecl;                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_picture.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_picture_get_cull_rect      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_picture_t; out result: sk_rect_t); cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_picture_get_unique_id      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_picture_t): uint32_t; cdecl;                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_picture_make_from_data     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const data: sk_data_t): sk_picture_t; cdecl;                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_picture_make_from_stream   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(stream: sk_stream_t): sk_picture_t; cdecl;                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_picture_make_shader        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_picture_t; tile_mode_x, tile_mode_y: sk_tilemode_t): sk_shader_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_picture_serialize_to_stream{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_picture_t; w_stream: sk_wstream_t); cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_picturerecorder.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_picturerecorder_begin_recording  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_picturerecorder_t; const bounds: psk_rect_t): sk_canvas_t; cdecl;     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_picturerecorder_create           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_picturerecorder_t; cdecl;                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_picturerecorder_destroy          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_picturerecorder_t); cdecl;                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_picturerecorder_finish_recording {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_picturerecorder_t): sk_picture_t; cdecl;                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_picturerecorder_finish_recording2{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_picturerecorder_t; const cull_rect: psk_rect_t): sk_picture_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_pixmap.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pixmap_create         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_pixmap_t; cdecl;                                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pixmap_create2        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const image_info: psk_imageinfo_t; const pixels: Pointer; row_bytes: size_t): sk_pixmap_t; cdecl;                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pixmap_destroy        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pixmap_t); cdecl;                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pixmap_erase          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t; color: sk_color_t; const area: psk_irect_t): bool; cdecl;                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pixmap_erase2         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t; const color: psk_color4f_t; color_space: sk_colorspace_t; const area: psk_irect_t): bool; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pixmap_extract_subset {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(dest: sk_pixmap_t; const self: sk_pixmap_t; const area: psk_irect_t): bool; cdecl;                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
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
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_pixmap_scale_pixels   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, dest: sk_pixmap_t; filter_quality: sk_filterquality_t): bool; cdecl;                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_pixmap_set_colorspace {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pixmap_t; value: sk_colorspace_t); cdecl;                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_refcnt.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_refcnt_is_unique{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_refcnt_t): bool; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_refcnt_ref      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_refcnt_t); cdecl;       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_refcnt_unref    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_refcnt_t); cdecl;       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_region.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_contains             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, region: sk_region_t): bool; cdecl;                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_contains2            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_region_t; const rect: psk_irect_t): bool; cdecl;                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_contains3            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_region_t; x, y: int32_t): bool; cdecl;                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_create               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_region_t; cdecl;                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_create2              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const region: sk_region_t): sk_region_t; cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_region_destroy              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_region_t); cdecl;                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_region_get_boundary_path    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_region_t; result: sk_path_t): bool; cdecl;                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
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
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_region_translate2           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_region_t; x, y: int32_t; result: sk_region_t); cdecl;               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
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
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_deflate2        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; dx, dy: float; result: sk_rrect_t); cdecl;                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_destroy         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t); cdecl;                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_rrect_get_height      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_rrect_t): float; cdecl;                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_get_radii       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_rrect_t; corner: sk_rrectcorner_t; out result: sk_vector_t); cdecl;                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_get_rect        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_rrect_t; out result: sk_rect_t); cdecl;                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_get_simple_radii{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_rrect_t; out result: sk_vector_t); cdecl;                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_rrect_get_type        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_rrect_t): sk_rrecttype_t; cdecl;                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_rrect_get_width       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_rrect_t): float; cdecl;                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_inflate         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; dx, dy: float); cdecl;                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_inflate2        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; dx, dy: float; result: sk_rrect_t); cdecl;                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_rrect_is_equal        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, rrect: sk_rrect_t): bool; cdecl;                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_rrect_is_valid        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_rrect_t): bool; cdecl;                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_rrect_make_offset     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_rrect_t; dx, dy: float): sk_rrect_t; cdecl;                                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_offset          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; dx, dy: float); cdecl;                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_set_empty       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t); cdecl;                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_set_nine_patch  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; const rect: psk_rect_t; radius_left, radius_top, radius_right, radius_bottom: float); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_set_oval        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; const rect: psk_rect_t); cdecl;                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_set_rect        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; const rect: psk_rect_t); cdecl;                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_set_rect2       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; const rect: psk_rect_t; const radii: psk_vector_t); cdecl;                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_rrect_set_rect3       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; const rect: psk_rect_t; radius_x, radius_y: float); cdecl;                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_rrect_transform       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_rrect_t; const matrix: psk_matrix_t; result: sk_rrect_t): bool; cdecl;                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_runtimeeffect.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_find_uniform        {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_runtimeeffect_t; const name: MarshaledAString): sk_runtimeeffectuniform_t; cdecl;                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_get_children        {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_runtimeeffect_t; index: size_t): MarshaledAString; cdecl;                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_get_children_count  {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_runtimeeffect_t): size_t; cdecl;                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_get_uniform         {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_runtimeeffect_t; index: size_t): sk_runtimeeffectuniform_t; cdecl;                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_get_uniform_count   {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_runtimeeffect_t): size_t; cdecl;                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_get_uniform_size    {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_runtimeeffect_t): size_t; cdecl;                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_make                {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const sksl: MarshaledAString; error: sk_string_t): sk_runtimeeffect_t; cdecl;                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_make_color_filter   {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(self: sk_runtimeeffect_t; uniforms: sk_data_t; children: psk_colorfilter_t; count: size_t): sk_colorfilter_t; cdecl;        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffect_make_shader         {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(self: sk_runtimeeffect_t; uniforms: sk_data_t; children: psk_shader_t; count: size_t; is_opaque: bool): sk_shader_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffectuniform_get_byte_size{$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_runtimeeffectuniform_t): size_t; cdecl;                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffectuniform_get_name     {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_runtimeeffectuniform_t): MarshaledAString; cdecl;                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_runtimeeffectuniform_get_offset   {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_runtimeeffectuniform_t): size_t; cdecl;                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_shader.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_blend                     {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(mode: sk_blendmode_t; dest, src: sk_shader_t): sk_shader_t; cdecl;                                                                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_color                     {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(color: sk_color_t): sk_shader_t; cdecl;                                                                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_color2                    {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const color: psk_color4f_t; color_space: sk_colorspace_t): sk_shader_t; cdecl;                                                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_empty                     {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(): sk_shader_t; cdecl;                                                                                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_gradient_linear           {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const points: psk_point_t; const colors: psk_color_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t): sk_shader_t; cdecl;                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_gradient_linear2          {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const points: psk_point_t; const colors: psk_color4f_t; color_space: sk_colorspace_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t): sk_shader_t; cdecl;                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_gradient_radial           {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const center: psk_point_t; radius: float; const colors: psk_color_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t): sk_shader_t; cdecl;                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_gradient_radial2          {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const center: psk_point_t; radius: float; const colors: psk_color4f_t; color_space: sk_colorspace_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t): sk_shader_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_gradient_sweep            {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const center: psk_point_t; const colors: psk_color_t; const positions: pfloat; count: int32_t): sk_shader_t; cdecl;                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_gradient_sweep2           {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const center: psk_point_t; const colors: psk_color4f_t; color_space: sk_colorspace_t; const positions: pfloat; count: int32_t): sk_shader_t; cdecl;                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_perlin_noise_fractal_noise{$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(base_frequency_x, base_frequency_y: float; num_octaves: int32_t; seed: float; const tile_size: psk_isize_t): sk_shader_t; cdecl;                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_perlin_noise_turbulence   {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(base_frequency_x, base_frequency_y: float; num_octaves: int32_t; seed: float; const tile_size: psk_isize_t): sk_shader_t; cdecl;                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_with_color_filter         {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_shader_t; filter: sk_colorfilter_t): sk_shader_t; cdecl;                                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_shader_make_with_local_matrix         {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(const self: sk_shader_t; const matrix: psk_matrix_t): sk_shader_t; cdecl;                                                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_stream.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_managedstream_create    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: Pointer): sk_streamasset_t; cdecl;      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_managedstream_set_procs {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const procs: psk_managedstream_procs_t); cdecl;  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_managedwstream_create   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: Pointer): sk_wstream_t; cdecl;          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_managedwstream_set_procs{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const procs: psk_managedwstream_procs_t); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_stream_destroy          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_stream_t); cdecl;                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_wstream_destroy         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_wstream_t); cdecl;                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_string.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_string_create  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_string_t; cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_string_destroy {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_string_t); cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_string_get_text{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_string_t): MarshaledAString; cdecl;        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_string_set_text{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_string_t; const value: MarshaledAString); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_surface.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_surface_draw                    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_surface_t; canvas: sk_canvas_t; x, y: float; paint: sk_paint_t); cdecl;                                                                                                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_surface_flush                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_surface_t); cdecl;                                                                                                                                                                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_surface_flush_and_submit        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_surface_t; sync_cpu: bool); cdecl;                                                                                                                                                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_get_canvas              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_surface_t): sk_canvas_t; cdecl;                                                                                                                                                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_get_height              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_surface_t): int32_t; cdecl;                                                                                                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_surface_get_image_info          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_surface_t; out result: sk_imageinfo_t); cdecl;                                                                                                                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_get_props               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_surface_t): sk_surfaceprops_t; cdecl;                                                                                                                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_get_recording_context   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_surface_t): gr_recordingcontext_t; cdecl;                                                                                                                                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_get_width               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_surface_t): int32_t; cdecl;                                                                                                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_make_from_ca_metal_layer{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: gr_recordingcontext_t; layer: gr_mtl_handle_t; origin: gr_surfaceorigin_t; sample_count: int32_t; color_type: sk_colortype_t; color_space: sk_colorspace_t; const props: sk_surfaceprops_t; out drawable: gr_mtl_handle_t): sk_surface_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_make_from_mtk_view      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: gr_recordingcontext_t; layer: gr_mtl_handle_t; origin: gr_surfaceorigin_t; sample_count: int32_t; color_type: sk_colortype_t; color_space: sk_colorspace_t; const props: sk_surfaceprops_t): sk_surface_t; cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_make_from_rendertarget  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: gr_recordingcontext_t; const render_target: gr_backendrendertarget_t; origin: gr_surfaceorigin_t; color_type: sk_colortype_t; color_space: sk_colorspace_t; const props: sk_surfaceprops_t): sk_surface_t; cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_make_from_texture       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: gr_recordingcontext_t; const texture: gr_backendtexture_t; origin: gr_surfaceorigin_t; sample_count: int32_t; color_type: sk_colortype_t; color_space: sk_colorspace_t; const props: sk_surfaceprops_t): sk_surface_t; cdecl;                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_make_image_snapshot     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_surface_t): sk_image_t; cdecl;                                                                                                                                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_make_image_snapshot2    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_surface_t; const bounds: psk_irect_t): sk_image_t; cdecl;                                                                                                                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_make_null               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(width, height: int32_t): sk_surface_t; cdecl;                                                                                                                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_make_raster             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const image_info: psk_imageinfo_t; row_bytes: size_t; const props: sk_surfaceprops_t): sk_surface_t; cdecl;                                                                                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_make_raster_direct      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const image_info: psk_imageinfo_t; pixels: Pointer; row_bytes: size_t; proc: sk_surface_raster_release_proc; proc_context: Pointer; const props: sk_surfaceprops_t): sk_surface_t; cdecl;                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_make_render_target      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: gr_recordingcontext_t; budgeted: bool; const image_info: psk_imageinfo_t; sample_count: int32_t; origin: gr_surfaceorigin_t; const props: sk_surfaceprops_t; should_create_with_mips: bool): sk_surface_t; cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_peek_pixels             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_surface_t; pixmap: sk_pixmap_t): bool; cdecl;                                                                                                                                                                                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surface_read_pixels             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_surface_t; const dest: sk_pixmap_t; src_x, src_y: int32_t): bool; cdecl;                                                                                                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_surface_write_pixels            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_surface_t; const src: sk_pixmap_t; dest_x, dest_y: int32_t); cdecl;                                                                                                                                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_sk_surfaceprops_create          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(flags: uint32_t; pixel_geometry: sk_pixelgeometry_t): sk_surfaceprops_t; cdecl;                                                                                                                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_surfaceprops_destroy            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_surfaceprops_t); cdecl;                                                                                                                                                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surfaceprops_get_flags          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_surfaceprops_t): uint32_t; cdecl;                                                                                                                                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surfaceprops_get_pixel_geometry {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_surfaceprops_t): sk_pixelgeometry_t; cdecl;                                                                                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_surfaceprops_is_equal           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, props: sk_surfaceprops_t): bool; cdecl;                                                                                                                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_textblob.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textblob_get_intercepts                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textblob_t; const bounds: pfloat; result: pfloat; const paint: sk_paint_t): int32_t; cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textblob_is_unique                      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textblob_t): bool; cdecl;                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textblob_ref                            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_textblob_t); cdecl;                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textblob_unref                          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_textblob_t); cdecl;                                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textblobbuilder_create                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_textblobbuilder_t; cdecl;                                                                                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textblobbuilder_destroy                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textblobbuilder_t); cdecl;                                                                                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textblobbuilder_alloc_horizontal_run    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textblobbuilder_t; const font: sk_font_t; count: int32_t; y: float; const bounds: psk_rect_t; out result: sk_runbuffer_t); cdecl;    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textblobbuilder_alloc_positioned_run    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textblobbuilder_t; const font: sk_font_t; count: int32_t; const bounds: psk_rect_t; out result: sk_runbuffer_t); cdecl;              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textblobbuilder_alloc_rotation_scale_run{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textblobbuilder_t; const font: sk_font_t; count: int32_t; out result: sk_runbuffer_t); cdecl;                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textblobbuilder_alloc_run               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textblobbuilder_t; const font: sk_font_t; count: int32_t; x, y: float; const bounds: psk_rect_t; out result: sk_runbuffer_t); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textblobbuilder_detach                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_textblobbuilder_t): sk_textblob_t; cdecl;                                                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_tracememorydump.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_tracememorydumpbaseclass_create   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(detailed_dump, dump_wrapped_objects: bool; context: Pointer): sk_tracememorydumpbaseclass_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_tracememorydumpbaseclass_destroy  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_tracememorydumpbaseclass_t); cdecl;                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_tracememorydumpbaseclass_set_procs{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const procs: psk_tracememorydumpbaseclass_procs_t); cdecl;                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_typeface.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_typeface_get_family_name{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_typeface_t; result: sk_string_t); cdecl;                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_typeface_get_slant      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_typeface_t): sk_fontslant_t; cdecl;                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_typeface_get_style      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_typeface_t; out result: sk_fontstyle_t); cdecl;                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_typeface_get_unique_id  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_typeface_t): uint32_t; cdecl;                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_typeface_get_weight     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_typeface_t): int32_t; cdecl;                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_typeface_get_width      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_typeface_t): int32_t; cdecl;                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_typeface_make_default   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_typeface_t; cdecl;                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_typeface_make_from_data {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(data: sk_data_t; ttc_index: int32_t): sk_typeface_t; cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_typeface_make_from_name {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const family_name: MarshaledAString; const style: psk_fontstyle_t): sk_typeface_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_version.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_version_get_major    {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(): int32_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_version_get_milestone{$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(): int32_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function {$ELSE}class var {$ENDIF}sk4d_version_get_minor    {$IFDEF SK_DYNAMIC_LOADING}: function {$ENDIF}(): int32_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'include/c/sk4d_vertices.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_vertices_get_unique_id{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_vertices_t): uint32_t; cdecl;                                                                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_vertices_is_unique    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_vertices_t): bool; cdecl;                                                                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_vertices_make_copy    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(vertex_mode: sk_vertexmode_t; vertex_count: int32_t; const positions, textures: psk_point_t; const colors: psk_color_t; index_count: int32_t; const indices: puint16_t): sk_vertices_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_vertices_ref          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_vertices_t); cdecl;                                                                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_vertices_unref        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_vertices_t); cdecl;                                                                                                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skottie/include/sk4d_skottie.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_skottieanimation_get_duration    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_skottieanimation_t): double; cdecl;                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_skottieanimation_get_fps         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_skottieanimation_t): double; cdecl;                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_skottieanimation_get_in_point    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_skottieanimation_t): double; cdecl;                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_skottieanimation_get_out_point   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_skottieanimation_t): double; cdecl;                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_skottieanimation_get_size        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_skottieanimation_t; out result: sk_size_t); cdecl;                                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_skottieanimation_get_version     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_skottieanimation_t): sk_string_t; cdecl;                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_skottieanimation_is_unique       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_skottieanimation_t): bool; cdecl;                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_skottieanimation_make            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const data: pchar; size: size_t): sk_skottieanimation_t; cdecl;                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_skottieanimation_make_from_stream{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(stream: sk_stream_t): sk_skottieanimation_t; cdecl;                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_skottieanimation_ref             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_skottieanimation_t); cdecl;                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_skottieanimation_render          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_skottieanimation_t; canvas: sk_canvas_t; const dest: psk_rect_t; render_flags: uint32_t); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_skottieanimation_seek_frame      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_skottieanimation_t; tick: double); cdecl;                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_skottieanimation_seek_frame_time {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_skottieanimation_t; tick: double); cdecl;                                                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_skottieanimation_unref           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_skottieanimation_t); cdecl;                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skshaper/include/sk4d_shaper.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_shaper_create                          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_shaper_t; cdecl;                                                                                                                                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_shaper_destroy                         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_shaper_t); cdecl;                                                                                                                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_shaper_shape                           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_shaper_t; const text: pchar; size: size_t; const font: sk_font_t; left_to_right: bool; width: float; handler: sk_shaperrunhandler_t); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_shaperrunhandler_destroy               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_shaperrunhandler_t); cdecl;                                                                                                                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_shaperrunhandlerbaseclass_create       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: Pointer): sk_shaperrunhandlerbaseclass_t; cdecl;                                                                                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_shaperrunhandlerbaseclass_set_procs    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const procs: psk_shaperrunhandlerbaseclass_procs_t); cdecl;                                                                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textblobbuilderrunhandler_create       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const text: MarshaledAString; const offset: psk_point_t): sk_textblobbuilderrunhandler_t; cdecl;                                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textblobbuilderrunhandler_detach       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_textblobbuilderrunhandler_t): sk_textblob_t; cdecl;                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textblobbuilderrunhandler_get_end_point{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textblobbuilderrunhandler_t; out result: sk_point_t); cdecl;                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/svg/include/sk4d_svgdom.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_svgdom_get_container_size{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_svgdom_t; out sk_size_t); cdecl;       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_svgdom_make              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(stream: sk_stream_t): sk_svgdom_t; cdecl;             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_svgdom_render            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_svgdom_t; canvas: sk_canvas_t); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_svgdom_set_container_size{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_svgdom_t; const value: psk_size_t); cdecl;   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skparagraph/include/sk4d_fontcollection.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_fontcollection_create             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_fontcollection_t; cdecl;                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_fontcollection_get_font_fall_back {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_fontcollection_t): bool; cdecl;                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_fontcollection_get_paragraph_cache{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_fontcollection_t): sk_paragraphcache_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_fontcollection_set_font_fall_back {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_fontcollection_t; value: bool); cdecl;         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skparagraph/include/sk4d_paragraphcache.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphcache_reset  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphcache_t); cdecl;              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphcache_turn_on{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphcache_t; value: bool); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skparagraph/include/sk4d_textstyle.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_add_font_feature                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; const feature: sk_string_t; value: int32_t); cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_add_shadow                         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; const shadow: psk_textshadow_t); cdecl;             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_clear_background_color             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t); cdecl;                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_clear_foreground_color             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t); cdecl;                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_create                             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_textstyle_t; cdecl;                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_destroy                            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t); cdecl;                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_background                     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): sk_paint_t; cdecl;                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_color                          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): sk_color_t; cdecl;                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_decoration_color               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): sk_color_t; cdecl;                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_decoration_mode                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): sk_textdecorationmode_t; cdecl;              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_decoration_style               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): sk_textdecorationstyle_t; cdecl;             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_decoration_thickness_multiplier{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): float; cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_decorations                    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): uint32_t; cdecl;                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_font_families                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t; result: psk_string_t): size_t; cdecl;         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_get_font_metrics                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_textstyle_t; metrics: psk_fontmetrics_t); cdecl;           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_font_size                      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): float; cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_get_font_style                     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_textstyle_t; out result: sk_fontstyle_t); cdecl;           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_foreground                     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): sk_paint_t; cdecl;                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_height                         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): float; cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_height_override                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): bool; cdecl;                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_letter_spacing                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): float; cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_locale                         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): sk_string_t; cdecl;                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_text_baseline                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): sk_textbaseline_t; cdecl;                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_typeface                       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): sk_typeface_t; cdecl;                        {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_get_word_spacing                   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): float; cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_has_background                     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): bool; cdecl;                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_has_foreground                     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): bool; cdecl;                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_is_equal                           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, text_style: sk_textstyle_t): bool; cdecl;                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_textstyle_is_placeholder                     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textstyle_t): bool; cdecl;                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_reset_font_features                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t); cdecl;                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_reset_shadows                      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t); cdecl;                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_background_color               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; paint: sk_paint_t); cdecl;                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_color                          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; value: sk_color_t); cdecl;                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_decoration_color               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; value: sk_color_t); cdecl;                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_decoration_mode                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; value: sk_textdecorationmode_t); cdecl;             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_decoration_style               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; value: sk_textdecorationstyle_t); cdecl;            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_decoration_thickness_multiplier{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; value: float); cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_decorations                    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; value: uint32_t); cdecl;                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_font_families                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; const values: psk_string_t; count: size_t); cdecl;  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_font_size                      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; value: float); cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_font_style                     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; const value: psk_fontstyle_t); cdecl;               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_foreground_color               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; paint: sk_paint_t); cdecl;                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_height                         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; value: float); cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_height_override                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; value: bool); cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_letter_spacing                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; value: float); cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_locale                         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; const value: sk_string_t); cdecl;                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_place_holder                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t); cdecl;                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_text_baseline                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; value: sk_textbaseline_t); cdecl;                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_typeface                       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; value: sk_typeface_t); cdecl;                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_textstyle_set_word_spacing                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textstyle_t; value: float); cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skparagraph/include/sk4d_paragraphstyle.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_create                   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_paragraphstyle_t; cdecl;                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_destroy                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphstyle_t); cdecl;                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_get_draw_options         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_paragraphstyle_t): sk_drawoptions_t; cdecl;                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_get_effective_align      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paragraphstyle_t): sk_textalign_t; cdecl;                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_get_ellipsis             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paragraphstyle_t): sk_string_t; cdecl;                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_get_height               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paragraphstyle_t): float; cdecl;                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_get_max_lines            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paragraphstyle_t): size_t; cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_get_strut_style          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paragraphstyle_t): sk_strutstyle_t; cdecl;                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_get_text_align           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paragraphstyle_t): sk_textalign_t; cdecl;                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_get_text_direction       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paragraphstyle_t): sk_textdirection_t; cdecl;                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_get_text_height_behaviors{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paragraphstyle_t): uint32_t; cdecl;                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_get_text_style           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paragraphstyle_t): sk_textstyle_t; cdecl;                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_has_unlimited_lines      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paragraphstyle_t): bool; cdecl;                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_is_ellipsized            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paragraphstyle_t): bool; cdecl;                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_is_equal                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, paragraph_style: sk_paragraphstyle_t): bool; cdecl;                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_is_hinting_on            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paragraphstyle_t): bool; cdecl;                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_set_draw_options         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphstyle_t; value: sk_drawoptions_t); cdecl;                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_set_ellipsis             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphstyle_t; value: sk_string_t); cdecl;                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_set_height               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphstyle_t; value: float); cdecl;                               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_set_max_lines            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphstyle_t; value: size_t); cdecl;                              {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_set_strut_style          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphstyle_t; const value: sk_strutstyle_t); cdecl;               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_set_text_align           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphstyle_t; value: sk_textalign_t); cdecl;                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_set_text_direction       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphstyle_t; value: sk_textdirection_t); cdecl;                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_set_text_height_behaviors{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphstyle_t; value: uint32_t); cdecl;                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_set_text_style           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphstyle_t; value: sk_textstyle_t); cdecl;                      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphstyle_turn_hinting_off         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphstyle_t); cdecl;                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_strutstyle_create                       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_strutstyle_t; cdecl;                                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_strutstyle_destroy                      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_strutstyle_t); cdecl;                                                 {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_strutstyle_get_enabled                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_strutstyle_t): bool; cdecl;                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_strutstyle_get_font_families            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_strutstyle_t; result: psk_string_t): size_t; cdecl;             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_strutstyle_get_font_size                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_strutstyle_t): float; cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_strutstyle_get_font_style               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_strutstyle_t; out result: sk_fontstyle_t); cdecl;               {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_strutstyle_get_force_height             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_strutstyle_t): bool; cdecl;                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_strutstyle_get_height                   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_strutstyle_t): float; cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_strutstyle_get_height_override          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_strutstyle_t): bool; cdecl;                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_strutstyle_get_leading                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_strutstyle_t): float; cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_strutstyle_is_equal                     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_strutstyle_t; const strut_style: sk_strutstyle_t): bool; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_strutstyle_set_enabled                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_strutstyle_t; value: bool); cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_strutstyle_set_font_families            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_strutstyle_t; const values: psk_string_t; count: size_t); cdecl;      {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_strutstyle_set_font_size                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_strutstyle_t; value: float); cdecl;                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_strutstyle_set_font_style               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_strutstyle_t; value: psk_fontstyle_t); cdecl;                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_strutstyle_set_force_height             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_strutstyle_t; value: bool); cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_strutstyle_set_height                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_strutstyle_t; value: float); cdecl;                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_strutstyle_set_height_override          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_strutstyle_t; value: bool); cdecl;                                    {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_strutstyle_set_leading                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_strutstyle_t; value: float); cdecl;                                   {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skparagraph/include/sk4d_paragraph.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraph_destroy            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraph_t); cdecl;                                                                                                                                       {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraph_get_height         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_paragraph_t): float; cdecl;                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraph_get_max_width      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_paragraph_t): float; cdecl;                                                                                                                                {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraph_get_rects_for_range{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_paragraph_t; first, last: uint32_t; rect_height_style: sk_rectheightstyle_t; rect_width_style: sk_rectwidthstyle_t; result: psk_textbox_t): size_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraph_get_word_boundary  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraph_t; offset: uint32_t; out first, last: uint32_t); cdecl;                                                                                          {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraph_layout             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraph_t; width: float); cdecl;                                                                                                                         {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraph_render             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraph_t; canvas: sk_canvas_t; x, y: float); cdecl;                                                                                                     {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}

    {$REGION 'modules/skparagraph/include/sk4d_paragraphbuilder.h'}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphbuilder_add_placeholder{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphbuilder_t; const placeholder: psk_placeholderstyle_t); cdecl;                                  {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphbuilder_add_text       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphbuilder_t; const text: pchar; size: size_t); cdecl;                                            {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphbuilder_create         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const paragraph_style: sk_paragraphstyle_t; font_collection: sk_fontcollection_t): sk_paragraphbuilder_t; cdecl; {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphbuilder_destroy        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphbuilder_t); cdecl;                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class function  {$ELSE}class var {$ENDIF}sk4d_paragraphbuilder_detach         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_paragraphbuilder_t): sk_paragraph_t; cdecl;                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphbuilder_pop            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphbuilder_t); cdecl;                                                                             {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$IFNDEF SK_DYNAMIC_LOADING}class procedure {$ELSE}class var {$ENDIF}sk4d_paragraphbuilder_push_style     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paragraphbuilder_t; const text_style: sk_textstyle_t); cdecl;                                           {$IFNDEF SK_DYNAMIC_LOADING}static;{$ENDIF}
    {$ENDREGION}



  end;

  {$ENDREGION}

implementation

uses
  { Delphi }
  System.Math;

{$REGION 'Skia Api'}

{ TSkiaApi }

{$IF defined(MSWINDOWS) or defined(SK_DYNAMIC_LOADING)}
class constructor TSkiaApi.Create;
begin
  {$IFDEF MSWINDOWS}
  SetExceptionMask(exAllArithmeticExceptions);
  {$ENDIF}
  {$IFDEF SK_DYNAMIC_LOADING}
  FLibHandle := SafeLoadLibrary(TSkiaApi.LibName);
  if FLibHandle = 0 then
    raise ESkiaApi.CreateFmt('Could not load %s library.', [TSkiaApi.LibName]);
  {$ENDIF}
{$ENDIF}

  {$REGION 'include/c/gr4d_backendsurface.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  gr4d_backendrendertarget_create_gl               := GetProc('gr4d_backendrendertarget_create_gl');
  gr4d_backendrendertarget_create_mtl              := GetProc('gr4d_backendrendertarget_create_mtl');
  gr4d_backendrendertarget_destroy                 := GetProc('gr4d_backendrendertarget_destroy');
  gr4d_backendrendertarget_get_backend_api         := GetProc('gr4d_backendrendertarget_get_backend_api');
  gr4d_backendrendertarget_get_gl_framebuffer_info := GetProc('gr4d_backendrendertarget_get_gl_framebuffer_info');
  gr4d_backendrendertarget_get_height              := GetProc('gr4d_backendrendertarget_get_height');
  gr4d_backendrendertarget_get_sample_count        := GetProc('gr4d_backendrendertarget_get_sample_count');
  gr4d_backendrendertarget_get_stencil_bits        := GetProc('gr4d_backendrendertarget_get_stencil_bits');
  gr4d_backendrendertarget_get_width               := GetProc('gr4d_backendrendertarget_get_width');
  gr4d_backendrendertarget_is_valid                := GetProc('gr4d_backendrendertarget_is_valid');
  gr4d_backendtexture_create_gl                    := GetProc('gr4d_backendtexture_create_gl');
  gr4d_backendtexture_create_mtl                   := GetProc('gr4d_backendtexture_create_mtl');
  gr4d_backendtexture_destroy                      := GetProc('gr4d_backendtexture_destroy');
  gr4d_backendtexture_get_backend_api              := GetProc('gr4d_backendtexture_get_backend_api');
  gr4d_backendtexture_get_gl_texture_info          := GetProc('gr4d_backendtexture_get_gl_texture_info');
  gr4d_backendtexture_get_height                   := GetProc('gr4d_backendtexture_get_height');
  gr4d_backendtexture_get_width                    := GetProc('gr4d_backendtexture_get_width');
  gr4d_backendtexture_has_mipmaps                  := GetProc('gr4d_backendtexture_has_mipmaps');
  gr4d_backendtexture_is_valid                     := GetProc('gr4d_backendtexture_is_valid');
  {$ELSE}
  class function  TSkiaApi.gr4d_backendrendertarget_create_gl;               external TSkiaApi.LibName name 'gr4d_backendrendertarget_create_gl';
  class function  TSkiaApi.gr4d_backendrendertarget_create_mtl;              external TSkiaApi.LibName name 'gr4d_backendrendertarget_create_mtl';
  class procedure TSkiaApi.gr4d_backendrendertarget_destroy;                 external TSkiaApi.LibName name 'gr4d_backendrendertarget_destroy';
  class function  TSkiaApi.gr4d_backendrendertarget_get_backend_api;         external TSkiaApi.LibName name 'gr4d_backendrendertarget_get_backend_api';
  class function  TSkiaApi.gr4d_backendrendertarget_get_gl_framebuffer_info; external TSkiaApi.LibName name 'gr4d_backendrendertarget_get_gl_framebuffer_info';
  class function  TSkiaApi.gr4d_backendrendertarget_get_height;              external TSkiaApi.LibName name 'gr4d_backendrendertarget_get_height';
  class function  TSkiaApi.gr4d_backendrendertarget_get_sample_count;        external TSkiaApi.LibName name 'gr4d_backendrendertarget_get_sample_count';
  class function  TSkiaApi.gr4d_backendrendertarget_get_stencil_bits;        external TSkiaApi.LibName name 'gr4d_backendrendertarget_get_stencil_bits';
  class function  TSkiaApi.gr4d_backendrendertarget_get_width;               external TSkiaApi.LibName name 'gr4d_backendrendertarget_get_width';
  class function  TSkiaApi.gr4d_backendrendertarget_is_valid;                external TSkiaApi.LibName name 'gr4d_backendrendertarget_is_valid';
  class function  TSkiaApi.gr4d_backendtexture_create_gl;                    external TSkiaApi.LibName name 'gr4d_backendtexture_create_gl';
  class function  TSkiaApi.gr4d_backendtexture_create_mtl;                   external TSkiaApi.LibName name 'gr4d_backendtexture_create_mtl';
  class procedure TSkiaApi.gr4d_backendtexture_destroy;                      external TSkiaApi.LibName name 'gr4d_backendtexture_destroy';
  class function  TSkiaApi.gr4d_backendtexture_get_backend_api;              external TSkiaApi.LibName name 'gr4d_backendtexture_get_backend_api';
  class function  TSkiaApi.gr4d_backendtexture_get_gl_texture_info;          external TSkiaApi.LibName name 'gr4d_backendtexture_get_gl_texture_info';
  class function  TSkiaApi.gr4d_backendtexture_get_height;                   external TSkiaApi.LibName name 'gr4d_backendtexture_get_height';
  class function  TSkiaApi.gr4d_backendtexture_get_width;                    external TSkiaApi.LibName name 'gr4d_backendtexture_get_width';
  class function  TSkiaApi.gr4d_backendtexture_has_mipmaps;                  external TSkiaApi.LibName name 'gr4d_backendtexture_has_mipmaps';
  class function  TSkiaApi.gr4d_backendtexture_is_valid;                     external TSkiaApi.LibName name 'gr4d_backendtexture_is_valid';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/gr4d_directcontext.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  gr4d_directcontext_abandon_context                       := GetProc('gr4d_directcontext_abandon_context');
  gr4d_directcontext_dump_memory_statistics                := GetProc('gr4d_directcontext_dump_memory_statistics');
  gr4d_directcontext_flush                                 := GetProc('gr4d_directcontext_flush');
  gr4d_directcontext_flush_and_submit                      := GetProc('gr4d_directcontext_flush_and_submit');
  gr4d_directcontext_free_gpu_resources                    := GetProc('gr4d_directcontext_free_gpu_resources');
  gr4d_directcontext_get_resource_cache_limit              := GetProc('gr4d_directcontext_get_resource_cache_limit');
  gr4d_directcontext_get_resource_cache_usage              := GetProc('gr4d_directcontext_get_resource_cache_usage');
  gr4d_directcontext_make_gl                               := GetProc('gr4d_directcontext_make_gl');
  gr4d_directcontext_make_gl2                              := GetProc('gr4d_directcontext_make_gl2');
  gr4d_directcontext_make_metal                            := GetProc('gr4d_directcontext_make_metal');
  gr4d_directcontext_make_metal2                           := GetProc('gr4d_directcontext_make_metal2');
  gr4d_directcontext_perform_deferred_cleanup              := GetProc('gr4d_directcontext_perform_deferred_cleanup');
  gr4d_directcontext_purge_unlocked_resources              := GetProc('gr4d_directcontext_purge_unlocked_resources');
  gr4d_directcontext_purge_unlocked_resources2             := GetProc('gr4d_directcontext_purge_unlocked_resources2');
  gr4d_directcontext_release_resources_and_abandon_context := GetProc('gr4d_directcontext_release_resources_and_abandon_context');
  gr4d_directcontext_reset_context                         := GetProc('gr4d_directcontext_reset_context');
  gr4d_directcontext_reset_gl_texture_bindings             := GetProc('gr4d_directcontext_reset_gl_texture_bindings');
  gr4d_directcontext_set_resource_cache_limit              := GetProc('gr4d_directcontext_set_resource_cache_limit');
  gr4d_directcontext_submit                                := GetProc('gr4d_directcontext_submit');
  {$ELSE}
  class procedure TSkiaApi.gr4d_directcontext_abandon_context;                       external TSkiaApi.LibName name 'gr4d_directcontext_abandon_context';
  class procedure TSkiaApi.gr4d_directcontext_dump_memory_statistics;                external TSkiaApi.LibName name 'gr4d_directcontext_dump_memory_statistics';
  class procedure TSkiaApi.gr4d_directcontext_flush;                                 external TSkiaApi.LibName name 'gr4d_directcontext_flush';
  class procedure TSkiaApi.gr4d_directcontext_flush_and_submit;                      external TSkiaApi.LibName name 'gr4d_directcontext_flush_and_submit';
  class procedure TSkiaApi.gr4d_directcontext_free_gpu_resources;                    external TSkiaApi.LibName name 'gr4d_directcontext_free_gpu_resources';
  class function  TSkiaApi.gr4d_directcontext_get_resource_cache_limit;              external TSkiaApi.LibName name 'gr4d_directcontext_get_resource_cache_limit';
  class procedure TSkiaApi.gr4d_directcontext_get_resource_cache_usage;              external TSkiaApi.LibName name 'gr4d_directcontext_get_resource_cache_usage';
  class function  TSkiaApi.gr4d_directcontext_make_gl;                               external TSkiaApi.LibName name 'gr4d_directcontext_make_gl';
  class function  TSkiaApi.gr4d_directcontext_make_gl2;                              external TSkiaApi.LibName name 'gr4d_directcontext_make_gl2';
  class function  TSkiaApi.gr4d_directcontext_make_metal;                            external TSkiaApi.LibName name 'gr4d_directcontext_make_metal';
  class function  TSkiaApi.gr4d_directcontext_make_metal2;                           external TSkiaApi.LibName name 'gr4d_directcontext_make_metal2';
  class procedure TSkiaApi.gr4d_directcontext_perform_deferred_cleanup;              external TSkiaApi.LibName name 'gr4d_directcontext_perform_deferred_cleanup';
  class procedure TSkiaApi.gr4d_directcontext_purge_unlocked_resources;              external TSkiaApi.LibName name 'gr4d_directcontext_purge_unlocked_resources';
  class procedure TSkiaApi.gr4d_directcontext_purge_unlocked_resources2;             external TSkiaApi.LibName name 'gr4d_directcontext_purge_unlocked_resources2';
  class procedure TSkiaApi.gr4d_directcontext_release_resources_and_abandon_context; external TSkiaApi.LibName name 'gr4d_directcontext_release_resources_and_abandon_context';
  class procedure TSkiaApi.gr4d_directcontext_reset_context;                         external TSkiaApi.LibName name 'gr4d_directcontext_reset_context';
  class procedure TSkiaApi.gr4d_directcontext_reset_gl_texture_bindings;             external TSkiaApi.LibName name 'gr4d_directcontext_reset_gl_texture_bindings';
  class procedure TSkiaApi.gr4d_directcontext_set_resource_cache_limit;              external TSkiaApi.LibName name 'gr4d_directcontext_set_resource_cache_limit';
  class function  TSkiaApi.gr4d_directcontext_submit;                                external TSkiaApi.LibName name 'gr4d_directcontext_submit';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/gr4d_gl_interface.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  gr4d_gl_interface_has_extension        := GetProc('gr4d_gl_interface_has_extension');
  gr4d_gl_interface_make_assembled       := GetProc('gr4d_gl_interface_make_assembled');
  gr4d_gl_interface_make_assembled_gl    := GetProc('gr4d_gl_interface_make_assembled_gl');
  gr4d_gl_interface_make_assembled_gles  := GetProc('gr4d_gl_interface_make_assembled_gles');
  gr4d_gl_interface_make_assembled_webgl := GetProc('gr4d_gl_interface_make_assembled_webgl');
  gr4d_gl_interface_make_native          := GetProc('gr4d_gl_interface_make_native');
  gr4d_gl_interface_validate             := GetProc('gr4d_gl_interface_validate');
  {$ELSE}
  class function TSkiaApi.gr4d_gl_interface_has_extension;        external TSkiaApi.LibName name 'gr4d_gl_interface_has_extension';
  class function TSkiaApi.gr4d_gl_interface_make_assembled;       external TSkiaApi.LibName name 'gr4d_gl_interface_make_assembled';
  class function TSkiaApi.gr4d_gl_interface_make_assembled_gl;    external TSkiaApi.LibName name 'gr4d_gl_interface_make_assembled_gl';
  class function TSkiaApi.gr4d_gl_interface_make_assembled_gles;  external TSkiaApi.LibName name 'gr4d_gl_interface_make_assembled_gles';
  class function TSkiaApi.gr4d_gl_interface_make_assembled_webgl; external TSkiaApi.LibName name 'gr4d_gl_interface_make_assembled_webgl';
  class function TSkiaApi.gr4d_gl_interface_make_native;          external TSkiaApi.LibName name 'gr4d_gl_interface_make_native';
  class function TSkiaApi.gr4d_gl_interface_validate;             external TSkiaApi.LibName name 'gr4d_gl_interface_validate';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/gr4d_recordingcontext.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  gr4d_recordingcontext_get_max_surface_sample_count_for_color_type := GetProc('gr4d_recordingcontext_get_max_surface_sample_count_for_color_type');
  gr4d_recordingcontext_is_abandoned                                := GetProc('gr4d_recordingcontext_is_abandoned');
  {$ELSE}
  class function TSkiaApi.gr4d_recordingcontext_get_max_surface_sample_count_for_color_type; external TSkiaApi.LibName name 'gr4d_recordingcontext_get_max_surface_sample_count_for_color_type';
  class function TSkiaApi.gr4d_recordingcontext_is_abandoned;                                external TSkiaApi.LibName name 'gr4d_recordingcontext_is_abandoned';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_canvas.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_canvas_clear                  := GetProc('sk4d_canvas_clear');
  sk4d_canvas_clear2                 := GetProc('sk4d_canvas_clear2');
  sk4d_canvas_destroy                := GetProc('sk4d_canvas_destroy');
  sk4d_canvas_discard                := GetProc('sk4d_canvas_discard');
  sk4d_canvas_clip_path              := GetProc('sk4d_canvas_clip_path');
  sk4d_canvas_clip_rect              := GetProc('sk4d_canvas_clip_rect');
  sk4d_canvas_clip_region            := GetProc('sk4d_canvas_clip_region');
  sk4d_canvas_clip_rrect             := GetProc('sk4d_canvas_clip_rrect');
  sk4d_canvas_clip_shader            := GetProc('sk4d_canvas_clip_shader');
  sk4d_canvas_concat                 := GetProc('sk4d_canvas_concat');
  sk4d_canvas_concat2                := GetProc('sk4d_canvas_concat2');
  sk4d_canvas_draw_annotation        := GetProc('sk4d_canvas_draw_annotation');
  sk4d_canvas_draw_arc               := GetProc('sk4d_canvas_draw_arc');
  sk4d_canvas_draw_atlas             := GetProc('sk4d_canvas_draw_atlas');
  sk4d_canvas_draw_circle            := GetProc('sk4d_canvas_draw_circle');
  sk4d_canvas_draw_color             := GetProc('sk4d_canvas_draw_color');
  sk4d_canvas_draw_color2            := GetProc('sk4d_canvas_draw_color2');
  sk4d_canvas_draw_image             := GetProc('sk4d_canvas_draw_image');
  sk4d_canvas_draw_image_lattice     := GetProc('sk4d_canvas_draw_image_lattice');
  sk4d_canvas_draw_image_nine        := GetProc('sk4d_canvas_draw_image_nine');
  sk4d_canvas_draw_image_rect        := GetProc('sk4d_canvas_draw_image_rect');
  sk4d_canvas_draw_line              := GetProc('sk4d_canvas_draw_line');
  sk4d_canvas_draw_oval              := GetProc('sk4d_canvas_draw_oval');
  sk4d_canvas_draw_paint             := GetProc('sk4d_canvas_draw_paint');
  sk4d_canvas_draw_patch             := GetProc('sk4d_canvas_draw_patch');
  sk4d_canvas_draw_path              := GetProc('sk4d_canvas_draw_path');
  sk4d_canvas_draw_picture           := GetProc('sk4d_canvas_draw_picture');
  sk4d_canvas_draw_picture2          := GetProc('sk4d_canvas_draw_picture2');
  sk4d_canvas_draw_point             := GetProc('sk4d_canvas_draw_point');
  sk4d_canvas_draw_points            := GetProc('sk4d_canvas_draw_points');
  sk4d_canvas_draw_rect              := GetProc('sk4d_canvas_draw_rect');
  sk4d_canvas_draw_region            := GetProc('sk4d_canvas_draw_region');
  sk4d_canvas_draw_rrect             := GetProc('sk4d_canvas_draw_rrect');
  sk4d_canvas_draw_rrect2            := GetProc('sk4d_canvas_draw_rrect2');
  sk4d_canvas_draw_rrect_difference  := GetProc('sk4d_canvas_draw_rrect_difference');
  sk4d_canvas_draw_simple_text       := GetProc('sk4d_canvas_draw_simple_text');
  sk4d_canvas_draw_text_blob         := GetProc('sk4d_canvas_draw_text_blob');
  sk4d_canvas_draw_vertices          := GetProc('sk4d_canvas_draw_vertices');
  sk4d_canvas_find_marked_ctm        := GetProc('sk4d_canvas_find_marked_ctm');
  sk4d_canvas_get_device_clip_bounds := GetProc('sk4d_canvas_get_device_clip_bounds');
  sk4d_canvas_get_local_clip_bounds  := GetProc('sk4d_canvas_get_local_clip_bounds');
  sk4d_canvas_get_local_to_device    := GetProc('sk4d_canvas_get_local_to_device');
  sk4d_canvas_get_local_to_device2   := GetProc('sk4d_canvas_get_local_to_device2');
  sk4d_canvas_get_save_count         := GetProc('sk4d_canvas_get_save_count');
  sk4d_canvas_is_clip_empty          := GetProc('sk4d_canvas_is_clip_empty');
  sk4d_canvas_is_clip_rect           := GetProc('sk4d_canvas_is_clip_rect');
  sk4d_canvas_mark_ctm               := GetProc('sk4d_canvas_mark_ctm');
  sk4d_canvas_quick_reject           := GetProc('sk4d_canvas_quick_reject');
  sk4d_canvas_quick_reject2          := GetProc('sk4d_canvas_quick_reject2');
  sk4d_canvas_reset_matrix           := GetProc('sk4d_canvas_reset_matrix');
  sk4d_canvas_restore                := GetProc('sk4d_canvas_restore');
  sk4d_canvas_restore_to_count       := GetProc('sk4d_canvas_restore_to_count');
  sk4d_canvas_rotate                 := GetProc('sk4d_canvas_rotate');
  sk4d_canvas_rotate2                := GetProc('sk4d_canvas_rotate2');
  sk4d_canvas_save                   := GetProc('sk4d_canvas_save');
  sk4d_canvas_save_layer             := GetProc('sk4d_canvas_save_layer');
  sk4d_canvas_save_layer_alpha       := GetProc('sk4d_canvas_save_layer_alpha');
  sk4d_canvas_scale                  := GetProc('sk4d_canvas_scale');
  sk4d_canvas_skew                   := GetProc('sk4d_canvas_skew');
  sk4d_canvas_translate              := GetProc('sk4d_canvas_translate');
  {$ELSE}
  class procedure TSkiaApi.sk4d_canvas_clear;                  external TSkiaApi.LibName name 'sk4d_canvas_clear';
  class procedure TSkiaApi.sk4d_canvas_clear2;                 external TSkiaApi.LibName name 'sk4d_canvas_clear2';
  class procedure TSkiaApi.sk4d_canvas_destroy;                external TSkiaApi.LibName name 'sk4d_canvas_destroy';
  class procedure TSkiaApi.sk4d_canvas_discard;                external TSkiaApi.LibName name 'sk4d_canvas_discard';
  class procedure TSkiaApi.sk4d_canvas_clip_path;              external TSkiaApi.LibName name 'sk4d_canvas_clip_path';
  class procedure TSkiaApi.sk4d_canvas_clip_rect;              external TSkiaApi.LibName name 'sk4d_canvas_clip_rect';
  class procedure TSkiaApi.sk4d_canvas_clip_region;            external TSkiaApi.LibName name 'sk4d_canvas_clip_region';
  class procedure TSkiaApi.sk4d_canvas_clip_rrect;             external TSkiaApi.LibName name 'sk4d_canvas_clip_rrect';
  class procedure TSkiaApi.sk4d_canvas_clip_shader;            external TSkiaApi.LibName name 'sk4d_canvas_clip_shader';
  class procedure TSkiaApi.sk4d_canvas_concat;                 external TSkiaApi.LibName name 'sk4d_canvas_concat';
  class procedure TSkiaApi.sk4d_canvas_concat2;                external TSkiaApi.LibName name 'sk4d_canvas_concat2';
  class procedure TSkiaApi.sk4d_canvas_draw_annotation;        external TSkiaApi.LibName name 'sk4d_canvas_draw_annotation';
  class procedure TSkiaApi.sk4d_canvas_draw_arc;               external TSkiaApi.LibName name 'sk4d_canvas_draw_arc';
  class procedure TSkiaApi.sk4d_canvas_draw_atlas;             external TSkiaApi.LibName name 'sk4d_canvas_draw_atlas';
  class procedure TSkiaApi.sk4d_canvas_draw_circle;            external TSkiaApi.LibName name 'sk4d_canvas_draw_circle';
  class procedure TSkiaApi.sk4d_canvas_draw_color;             external TSkiaApi.LibName name 'sk4d_canvas_draw_color';
  class procedure TSkiaApi.sk4d_canvas_draw_color2;            external TSkiaApi.LibName name 'sk4d_canvas_draw_color2';
  class procedure TSkiaApi.sk4d_canvas_draw_image;             external TSkiaApi.LibName name 'sk4d_canvas_draw_image';
  class procedure TSkiaApi.sk4d_canvas_draw_image_lattice;     external TSkiaApi.LibName name 'sk4d_canvas_draw_image_lattice';
  class procedure TSkiaApi.sk4d_canvas_draw_image_nine;        external TSkiaApi.LibName name 'sk4d_canvas_draw_image_nine';
  class procedure TSkiaApi.sk4d_canvas_draw_image_rect;        external TSkiaApi.LibName name 'sk4d_canvas_draw_image_rect';
  class procedure TSkiaApi.sk4d_canvas_draw_line;              external TSkiaApi.LibName name 'sk4d_canvas_draw_line';
  class procedure TSkiaApi.sk4d_canvas_draw_oval;              external TSkiaApi.LibName name 'sk4d_canvas_draw_oval';
  class procedure TSkiaApi.sk4d_canvas_draw_paint;             external TSkiaApi.LibName name 'sk4d_canvas_draw_paint';
  class procedure TSkiaApi.sk4d_canvas_draw_patch;             external TSkiaApi.LibName name 'sk4d_canvas_draw_patch';
  class procedure TSkiaApi.sk4d_canvas_draw_path;              external TSkiaApi.LibName name 'sk4d_canvas_draw_path';
  class procedure TSkiaApi.sk4d_canvas_draw_picture;           external TSkiaApi.LibName name 'sk4d_canvas_draw_picture';
  class procedure TSkiaApi.sk4d_canvas_draw_picture2;          external TSkiaApi.LibName name 'sk4d_canvas_draw_picture2';
  class procedure TSkiaApi.sk4d_canvas_draw_point;             external TSkiaApi.LibName name 'sk4d_canvas_draw_point';
  class procedure TSkiaApi.sk4d_canvas_draw_points;            external TSkiaApi.LibName name 'sk4d_canvas_draw_points';
  class procedure TSkiaApi.sk4d_canvas_draw_rect;              external TSkiaApi.LibName name 'sk4d_canvas_draw_rect';
  class procedure TSkiaApi.sk4d_canvas_draw_region;            external TSkiaApi.LibName name 'sk4d_canvas_draw_region';
  class procedure TSkiaApi.sk4d_canvas_draw_rrect;             external TSkiaApi.LibName name 'sk4d_canvas_draw_rrect';
  class procedure TSkiaApi.sk4d_canvas_draw_rrect2;            external TSkiaApi.LibName name 'sk4d_canvas_draw_rrect2';
  class procedure TSkiaApi.sk4d_canvas_draw_rrect_difference;  external TSkiaApi.LibName name 'sk4d_canvas_draw_rrect_difference';
  class procedure TSkiaApi.sk4d_canvas_draw_simple_text;       external TSkiaApi.LibName name 'sk4d_canvas_draw_simple_text';
  class procedure TSkiaApi.sk4d_canvas_draw_text_blob;         external TSkiaApi.LibName name 'sk4d_canvas_draw_text_blob';
  class procedure TSkiaApi.sk4d_canvas_draw_vertices;          external TSkiaApi.LibName name 'sk4d_canvas_draw_vertices';
  class function  TSkiaApi.sk4d_canvas_find_marked_ctm;        external TSkiaApi.LibName name 'sk4d_canvas_find_marked_ctm';
  class procedure TSkiaApi.sk4d_canvas_get_device_clip_bounds; external TSkiaApi.LibName name 'sk4d_canvas_get_device_clip_bounds';
  class procedure TSkiaApi.sk4d_canvas_get_local_clip_bounds;  external TSkiaApi.LibName name 'sk4d_canvas_get_local_clip_bounds';
  class procedure TSkiaApi.sk4d_canvas_get_local_to_device;    external TSkiaApi.LibName name 'sk4d_canvas_get_local_to_device';
  class procedure TSkiaApi.sk4d_canvas_get_local_to_device2;   external TSkiaApi.LibName name 'sk4d_canvas_get_local_to_device2';
  class function  TSkiaApi.sk4d_canvas_get_save_count;         external TSkiaApi.LibName name 'sk4d_canvas_get_save_count';
  class function  TSkiaApi.sk4d_canvas_is_clip_empty;          external TSkiaApi.LibName name 'sk4d_canvas_is_clip_empty';
  class function  TSkiaApi.sk4d_canvas_is_clip_rect;           external TSkiaApi.LibName name 'sk4d_canvas_is_clip_rect';
  class procedure TSkiaApi.sk4d_canvas_mark_ctm;               external TSkiaApi.LibName name 'sk4d_canvas_mark_ctm';
  class function  TSkiaApi.sk4d_canvas_quick_reject;           external TSkiaApi.LibName name 'sk4d_canvas_quick_reject';
  class function  TSkiaApi.sk4d_canvas_quick_reject2;          external TSkiaApi.LibName name 'sk4d_canvas_quick_reject2';
  class procedure TSkiaApi.sk4d_canvas_reset_matrix;           external TSkiaApi.LibName name 'sk4d_canvas_reset_matrix';
  class procedure TSkiaApi.sk4d_canvas_restore;                external TSkiaApi.LibName name 'sk4d_canvas_restore';
  class procedure TSkiaApi.sk4d_canvas_restore_to_count;       external TSkiaApi.LibName name 'sk4d_canvas_restore_to_count';
  class procedure TSkiaApi.sk4d_canvas_rotate;                 external TSkiaApi.LibName name 'sk4d_canvas_rotate';
  class procedure TSkiaApi.sk4d_canvas_rotate2;                external TSkiaApi.LibName name 'sk4d_canvas_rotate2';
  class function  TSkiaApi.sk4d_canvas_save;                   external TSkiaApi.LibName name 'sk4d_canvas_save';
  class function  TSkiaApi.sk4d_canvas_save_layer;             external TSkiaApi.LibName name 'sk4d_canvas_save_layer';
  class function  TSkiaApi.sk4d_canvas_save_layer_alpha;       external TSkiaApi.LibName name 'sk4d_canvas_save_layer_alpha';
  class procedure TSkiaApi.sk4d_canvas_scale;                  external TSkiaApi.LibName name 'sk4d_canvas_scale';
  class procedure TSkiaApi.sk4d_canvas_skew;                   external TSkiaApi.LibName name 'sk4d_canvas_skew';
  class procedure TSkiaApi.sk4d_canvas_translate;              external TSkiaApi.LibName name 'sk4d_canvas_translate';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_colorfilter.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_colorfilter_make_blend                := GetProc('sk4d_colorfilter_make_blend');
  sk4d_colorfilter_make_compose              := GetProc('sk4d_colorfilter_make_compose');
  sk4d_colorfilter_make_high_contrast        := GetProc('sk4d_colorfilter_make_high_contrast');
  sk4d_colorfilter_make_hsla_matrix          := GetProc('sk4d_colorfilter_make_hsla_matrix');
  sk4d_colorfilter_make_lerp                 := GetProc('sk4d_colorfilter_make_lerp');
  sk4d_colorfilter_make_lighting             := GetProc('sk4d_colorfilter_make_lighting');
  sk4d_colorfilter_make_linear_to_srgb_gamma := GetProc('sk4d_colorfilter_make_linear_to_srgb_gamma');
  sk4d_colorfilter_make_luma_color           := GetProc('sk4d_colorfilter_make_luma_color');
  sk4d_colorfilter_make_matrix               := GetProc('sk4d_colorfilter_make_matrix');
  sk4d_colorfilter_make_overdraw             := GetProc('sk4d_colorfilter_make_overdraw');
  sk4d_colorfilter_make_srgb_to_linear_gamma := GetProc('sk4d_colorfilter_make_srgb_to_linear_gamma');
  sk4d_colorfilter_make_table                := GetProc('sk4d_colorfilter_make_table');
  {$ELSE}
  class function TSkiaApi.sk4d_colorfilter_make_blend;                external TSkiaApi.LibName name 'sk4d_colorfilter_make_blend';
  class function TSkiaApi.sk4d_colorfilter_make_compose;              external TSkiaApi.LibName name 'sk4d_colorfilter_make_compose';
  class function TSkiaApi.sk4d_colorfilter_make_high_contrast;        external TSkiaApi.LibName name 'sk4d_colorfilter_make_high_contrast';
  class function TSkiaApi.sk4d_colorfilter_make_hsla_matrix;          external TSkiaApi.LibName name 'sk4d_colorfilter_make_hsla_matrix';
  class function TSkiaApi.sk4d_colorfilter_make_lerp;                 external TSkiaApi.LibName name 'sk4d_colorfilter_make_lerp';
  class function TSkiaApi.sk4d_colorfilter_make_lighting;             external TSkiaApi.LibName name 'sk4d_colorfilter_make_lighting';
  class function TSkiaApi.sk4d_colorfilter_make_linear_to_srgb_gamma; external TSkiaApi.LibName name 'sk4d_colorfilter_make_linear_to_srgb_gamma';
  class function TSkiaApi.sk4d_colorfilter_make_luma_color;           external TSkiaApi.LibName name 'sk4d_colorfilter_make_luma_color';
  class function TSkiaApi.sk4d_colorfilter_make_matrix;               external TSkiaApi.LibName name 'sk4d_colorfilter_make_matrix';
  class function TSkiaApi.sk4d_colorfilter_make_overdraw;             external TSkiaApi.LibName name 'sk4d_colorfilter_make_overdraw';
  class function TSkiaApi.sk4d_colorfilter_make_srgb_to_linear_gamma; external TSkiaApi.LibName name 'sk4d_colorfilter_make_srgb_to_linear_gamma';
  class function TSkiaApi.sk4d_colorfilter_make_table;                external TSkiaApi.LibName name 'sk4d_colorfilter_make_table';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_colorspace.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_colorspace_gamma_close_to_srgb      := GetProc('sk4d_colorspace_gamma_close_to_srgb');
  sk4d_colorspace_gamma_is_linear          := GetProc('sk4d_colorspace_gamma_is_linear');
  sk4d_colorspace_is_equal                 := GetProc('sk4d_colorspace_is_equal');
  sk4d_colorspace_is_numerical_transfer_fn := GetProc('sk4d_colorspace_is_numerical_transfer_fn');
  sk4d_colorspace_is_srgb                  := GetProc('sk4d_colorspace_is_srgb');
  sk4d_colorspace_is_unique                := GetProc('sk4d_colorspace_is_unique');
  sk4d_colorspace_make                     := GetProc('sk4d_colorspace_make');
  sk4d_colorspace_make_linear_gamma        := GetProc('sk4d_colorspace_make_linear_gamma');
  sk4d_colorspace_make_rgb                 := GetProc('sk4d_colorspace_make_rgb');
  sk4d_colorspace_make_srgb                := GetProc('sk4d_colorspace_make_srgb');
  sk4d_colorspace_make_srgb_gamma          := GetProc('sk4d_colorspace_make_srgb_gamma');
  sk4d_colorspace_make_srgb_linear         := GetProc('sk4d_colorspace_make_srgb_linear');
  sk4d_colorspace_ref                      := GetProc('sk4d_colorspace_ref');
  sk4d_colorspace_to_profile               := GetProc('sk4d_colorspace_to_profile');
  sk4d_colorspace_to_xyz                   := GetProc('sk4d_colorspace_to_xyz');
  sk4d_colorspace_unref                    := GetProc('sk4d_colorspace_unref');
  sk4d_colorspaceiccprofile_destroy        := GetProc('sk4d_colorspaceiccprofile_destroy');
  sk4d_colorspaceiccprofile_get_buffer     := GetProc('sk4d_colorspaceiccprofile_get_buffer');
  sk4d_colorspaceiccprofile_make           := GetProc('sk4d_colorspaceiccprofile_make');
  sk4d_colorspaceiccprofile_to_xyz         := GetProc('sk4d_colorspaceiccprofile_to_xyz');
  sk4d_colorspaceprimaries_to_xyz          := GetProc('sk4d_colorspaceprimaries_to_xyz');
  sk4d_colorspacetransferfn_hlg            := GetProc('sk4d_colorspacetransferfn_hlg');
  sk4d_colorspacetransferfn_invert         := GetProc('sk4d_colorspacetransferfn_invert');
  sk4d_colorspacetransferfn_linear         := GetProc('sk4d_colorspacetransferfn_linear');
  sk4d_colorspacetransferfn_pq             := GetProc('sk4d_colorspacetransferfn_pq');
  sk4d_colorspacetransferfn_rec2020        := GetProc('sk4d_colorspacetransferfn_rec2020');
  sk4d_colorspacetransferfn_srgb           := GetProc('sk4d_colorspacetransferfn_srgb');
  sk4d_colorspacetransferfn_transform      := GetProc('sk4d_colorspacetransferfn_transform');
  sk4d_colorspacetransferfn_two_dot_two    := GetProc('sk4d_colorspacetransferfn_two_dot_two');
  sk4d_colorspacexyz_adobe_rgb             := GetProc('sk4d_colorspacexyz_adobe_rgb');
  sk4d_colorspacexyz_concat                := GetProc('sk4d_colorspacexyz_concat');
  sk4d_colorspacexyz_display_p3            := GetProc('sk4d_colorspacexyz_display_p3');
  sk4d_colorspacexyz_invert                := GetProc('sk4d_colorspacexyz_invert');
  sk4d_colorspacexyz_rec_2020              := GetProc('sk4d_colorspacexyz_rec_2020');
  sk4d_colorspacexyz_srgb                  := GetProc('sk4d_colorspacexyz_srgb');
  sk4d_colorspacexyz_xyz                   := GetProc('sk4d_colorspacexyz_xyz');
  {$ELSE}
  class function  TSkiaApi.sk4d_colorspace_gamma_close_to_srgb;      external TSkiaApi.LibName name 'sk4d_colorspace_gamma_close_to_srgb';
  class function  TSkiaApi.sk4d_colorspace_gamma_is_linear;          external TSkiaApi.LibName name 'sk4d_colorspace_gamma_is_linear';
  class function  TSkiaApi.sk4d_colorspace_is_equal;                 external TSkiaApi.LibName name 'sk4d_colorspace_is_equal';
  class function  TSkiaApi.sk4d_colorspace_is_numerical_transfer_fn; external TSkiaApi.LibName name 'sk4d_colorspace_is_numerical_transfer_fn';
  class function  TSkiaApi.sk4d_colorspace_is_srgb;                  external TSkiaApi.LibName name 'sk4d_colorspace_is_srgb';
  class function  TSkiaApi.sk4d_colorspace_is_unique;                external TSkiaApi.LibName name 'sk4d_colorspace_is_unique';
  class function  TSkiaApi.sk4d_colorspace_make;                     external TSkiaApi.LibName name 'sk4d_colorspace_make';
  class function  TSkiaApi.sk4d_colorspace_make_linear_gamma;        external TSkiaApi.LibName name 'sk4d_colorspace_make_linear_gamma';
  class function  TSkiaApi.sk4d_colorspace_make_rgb;                 external TSkiaApi.LibName name 'sk4d_colorspace_make_rgb';
  class function  TSkiaApi.sk4d_colorspace_make_srgb;                external TSkiaApi.LibName name 'sk4d_colorspace_make_srgb';
  class function  TSkiaApi.sk4d_colorspace_make_srgb_gamma;          external TSkiaApi.LibName name 'sk4d_colorspace_make_srgb_gamma';
  class function  TSkiaApi.sk4d_colorspace_make_srgb_linear;         external TSkiaApi.LibName name 'sk4d_colorspace_make_srgb_linear';
  class procedure TSkiaApi.sk4d_colorspace_ref;                      external TSkiaApi.LibName name 'sk4d_colorspace_ref';
  class function  TSkiaApi.sk4d_colorspace_to_profile;               external TSkiaApi.LibName name 'sk4d_colorspace_to_profile';
  class function  TSkiaApi.sk4d_colorspace_to_xyz;                   external TSkiaApi.LibName name 'sk4d_colorspace_to_xyz';
  class procedure TSkiaApi.sk4d_colorspace_unref;                    external TSkiaApi.LibName name 'sk4d_colorspace_unref';
  class procedure TSkiaApi.sk4d_colorspaceiccprofile_destroy;        external TSkiaApi.LibName name 'sk4d_colorspaceiccprofile_destroy';
  class function  TSkiaApi.sk4d_colorspaceiccprofile_get_buffer;     external TSkiaApi.LibName name 'sk4d_colorspaceiccprofile_get_buffer';
  class function  TSkiaApi.sk4d_colorspaceiccprofile_make;           external TSkiaApi.LibName name 'sk4d_colorspaceiccprofile_make';
  class function  TSkiaApi.sk4d_colorspaceiccprofile_to_xyz;         external TSkiaApi.LibName name 'sk4d_colorspaceiccprofile_to_xyz';
  class function  TSkiaApi.sk4d_colorspaceprimaries_to_xyz;          external TSkiaApi.LibName name 'sk4d_colorspaceprimaries_to_xyz';
  class procedure TSkiaApi.sk4d_colorspacetransferfn_hlg;            external TSkiaApi.LibName name 'sk4d_colorspacetransferfn_hlg';
  class function  TSkiaApi.sk4d_colorspacetransferfn_invert;         external TSkiaApi.LibName name 'sk4d_colorspacetransferfn_invert';
  class procedure TSkiaApi.sk4d_colorspacetransferfn_linear;         external TSkiaApi.LibName name 'sk4d_colorspacetransferfn_linear';
  class procedure TSkiaApi.sk4d_colorspacetransferfn_pq;             external TSkiaApi.LibName name 'sk4d_colorspacetransferfn_pq';
  class procedure TSkiaApi.sk4d_colorspacetransferfn_rec2020;        external TSkiaApi.LibName name 'sk4d_colorspacetransferfn_rec2020';
  class procedure TSkiaApi.sk4d_colorspacetransferfn_srgb;           external TSkiaApi.LibName name 'sk4d_colorspacetransferfn_srgb';
  class function  TSkiaApi.sk4d_colorspacetransferfn_transform;      external TSkiaApi.LibName name 'sk4d_colorspacetransferfn_transform';
  class procedure TSkiaApi.sk4d_colorspacetransferfn_two_dot_two;    external TSkiaApi.LibName name 'sk4d_colorspacetransferfn_two_dot_two';
  class procedure TSkiaApi.sk4d_colorspacexyz_adobe_rgb;             external TSkiaApi.LibName name 'sk4d_colorspacexyz_adobe_rgb';
  class procedure TSkiaApi.sk4d_colorspacexyz_concat;                external TSkiaApi.LibName name 'sk4d_colorspacexyz_concat';
  class procedure TSkiaApi.sk4d_colorspacexyz_display_p3;            external TSkiaApi.LibName name 'sk4d_colorspacexyz_display_p3';
  class function  TSkiaApi.sk4d_colorspacexyz_invert;                external TSkiaApi.LibName name 'sk4d_colorspacexyz_invert';
  class procedure TSkiaApi.sk4d_colorspacexyz_rec_2020;              external TSkiaApi.LibName name 'sk4d_colorspacexyz_rec_2020';
  class procedure TSkiaApi.sk4d_colorspacexyz_srgb;                  external TSkiaApi.LibName name 'sk4d_colorspacexyz_srgb';
  class procedure TSkiaApi.sk4d_colorspacexyz_xyz;                   external TSkiaApi.LibName name 'sk4d_colorspacexyz_xyz';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_data.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_data_get_data         := GetProc('sk4d_data_get_data');
  sk4d_data_get_size         := GetProc('sk4d_data_get_size');
  sk4d_data_is_unique        := GetProc('sk4d_data_is_unique');
  sk4d_data_make             := GetProc('sk4d_data_make');
  sk4d_data_make_from_stream := GetProc('sk4d_data_make_from_stream');
  sk4d_data_ref              := GetProc('sk4d_data_ref');
  sk4d_data_unref            := GetProc('sk4d_data_unref');
  {$ELSE}
  class function  TSkiaApi.sk4d_data_get_data;         external TSkiaApi.LibName name 'sk4d_data_get_data';
  class function  TSkiaApi.sk4d_data_get_size;         external TSkiaApi.LibName name 'sk4d_data_get_size';
  class function  TSkiaApi.sk4d_data_is_unique;        external TSkiaApi.LibName name 'sk4d_data_is_unique';
  class function  TSkiaApi.sk4d_data_make;             external TSkiaApi.LibName name 'sk4d_data_make';
  class function  TSkiaApi.sk4d_data_make_from_stream; external TSkiaApi.LibName name 'sk4d_data_make_from_stream';
  class procedure TSkiaApi.sk4d_data_ref;              external TSkiaApi.LibName name 'sk4d_data_ref';
  class procedure TSkiaApi.sk4d_data_unref;            external TSkiaApi.LibName name 'sk4d_data_unref';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_debugf.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_set_debug_msg_proc := GetProc('sk4d_set_debug_msg_proc');
  {$ELSE}
  class procedure TSkiaApi.sk4d_set_debug_msg_proc; external TSkiaApi.LibName name 'sk4d_set_debug_msg_proc';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_document.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_document_begin_page := GetProc('sk4d_document_begin_page');
  sk4d_document_close      := GetProc('sk4d_document_close');
  sk4d_document_end_page   := GetProc('sk4d_document_end_page');
  sk4d_document_make_pdf   := GetProc('sk4d_document_make_pdf');
  sk4d_document_make_pdf2  := GetProc('sk4d_document_make_pdf2');
  sk4d_document_terminate  := GetProc('sk4d_document_terminate');
  {$ELSE}
  class function  TSkiaApi.sk4d_document_begin_page; external TSkiaApi.LibName name 'sk4d_document_begin_page';
  class procedure TSkiaApi.sk4d_document_close;      external TSkiaApi.LibName name 'sk4d_document_close';
  class procedure TSkiaApi.sk4d_document_end_page;   external TSkiaApi.LibName name 'sk4d_document_end_page';
  class function  TSkiaApi.sk4d_document_make_pdf;   external TSkiaApi.LibName name 'sk4d_document_make_pdf';
  class function  TSkiaApi.sk4d_document_make_pdf2;  external TSkiaApi.LibName name 'sk4d_document_make_pdf2';
  class procedure TSkiaApi.sk4d_document_terminate;  external TSkiaApi.LibName name 'sk4d_document_terminate';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_font.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_font_create                  := GetProc('sk4d_font_create');
  sk4d_font_create2                 := GetProc('sk4d_font_create2');
  sk4d_font_destroy                 := GetProc('sk4d_font_destroy');
  sk4d_font_get_baseline_snap       := GetProc('sk4d_font_get_baseline_snap');
  sk4d_font_get_edging              := GetProc('sk4d_font_get_edging');
  sk4d_font_get_embedded_bitmaps    := GetProc('sk4d_font_get_embedded_bitmaps');
  sk4d_font_get_embolden            := GetProc('sk4d_font_get_embolden');
  sk4d_font_get_force_auto_hinting  := GetProc('sk4d_font_get_force_auto_hinting');
  sk4d_font_get_glyphs              := GetProc('sk4d_font_get_glyphs');
  sk4d_font_get_glyphs_count        := GetProc('sk4d_font_get_glyphs_count');
  sk4d_font_get_hinting             := GetProc('sk4d_font_get_hinting');
  sk4d_font_get_linear_metrics      := GetProc('sk4d_font_get_linear_metrics');
  sk4d_font_get_metrics             := GetProc('sk4d_font_get_metrics');
  sk4d_font_get_offsets             := GetProc('sk4d_font_get_offsets');
  sk4d_font_get_path                := GetProc('sk4d_font_get_path');
  sk4d_font_get_paths               := GetProc('sk4d_font_get_paths');
  sk4d_font_get_positions           := GetProc('sk4d_font_get_positions');
  sk4d_font_get_scale_x             := GetProc('sk4d_font_get_scale_x');
  sk4d_font_get_size                := GetProc('sk4d_font_get_size');
  sk4d_font_get_skew_x              := GetProc('sk4d_font_get_skew_x');
  sk4d_font_get_subpixel            := GetProc('sk4d_font_get_subpixel');
  sk4d_font_get_typeface            := GetProc('sk4d_font_get_typeface');
  sk4d_font_get_typeface_or_default := GetProc('sk4d_font_get_typeface_or_default');
  sk4d_font_get_widths_bounds       := GetProc('sk4d_font_get_widths_bounds');
  sk4d_font_is_equal                := GetProc('sk4d_font_is_equal');
  sk4d_font_measure_text            := GetProc('sk4d_font_measure_text');
  sk4d_font_set_baseline_snap       := GetProc('sk4d_font_set_baseline_snap');
  sk4d_font_set_edging              := GetProc('sk4d_font_set_edging');
  sk4d_font_set_embedded_bitmaps    := GetProc('sk4d_font_set_embedded_bitmaps');
  sk4d_font_set_embolden            := GetProc('sk4d_font_set_embolden');
  sk4d_font_set_force_auto_hinting  := GetProc('sk4d_font_set_force_auto_hinting');
  sk4d_font_set_hinting             := GetProc('sk4d_font_set_hinting');
  sk4d_font_set_linear_metrics      := GetProc('sk4d_font_set_linear_metrics');
  sk4d_font_set_scale_x             := GetProc('sk4d_font_set_scale_x');
  sk4d_font_set_size                := GetProc('sk4d_font_set_size');
  sk4d_font_set_skew_x              := GetProc('sk4d_font_set_skew_x');
  sk4d_font_set_subpixel            := GetProc('sk4d_font_set_subpixel');
  sk4d_font_set_typeface            := GetProc('sk4d_font_set_typeface');
  sk4d_font_unichar_to_glyph        := GetProc('sk4d_font_unichar_to_glyph');
  sk4d_font_unichars_to_glyphs      := GetProc('sk4d_font_unichars_to_glyphs');
  {$ELSE}
  class function  TSkiaApi.sk4d_font_create;                  external TSkiaApi.LibName name 'sk4d_font_create';
  class function  TSkiaApi.sk4d_font_create2;                 external TSkiaApi.LibName name 'sk4d_font_create2';
  class procedure TSkiaApi.sk4d_font_destroy;                 external TSkiaApi.LibName name 'sk4d_font_destroy';
  class function  TSkiaApi.sk4d_font_get_baseline_snap;       external TSkiaApi.LibName name 'sk4d_font_get_baseline_snap';
  class function  TSkiaApi.sk4d_font_get_edging;              external TSkiaApi.LibName name 'sk4d_font_get_edging';
  class function  TSkiaApi.sk4d_font_get_embedded_bitmaps;    external TSkiaApi.LibName name 'sk4d_font_get_embedded_bitmaps';
  class function  TSkiaApi.sk4d_font_get_embolden;            external TSkiaApi.LibName name 'sk4d_font_get_embolden';
  class function  TSkiaApi.sk4d_font_get_force_auto_hinting;  external TSkiaApi.LibName name 'sk4d_font_get_force_auto_hinting';
  class function  TSkiaApi.sk4d_font_get_glyphs;              external TSkiaApi.LibName name 'sk4d_font_get_glyphs';
  class function  TSkiaApi.sk4d_font_get_glyphs_count;        external TSkiaApi.LibName name 'sk4d_font_get_glyphs_count';
  class function  TSkiaApi.sk4d_font_get_hinting;             external TSkiaApi.LibName name 'sk4d_font_get_hinting';
  class function  TSkiaApi.sk4d_font_get_linear_metrics;      external TSkiaApi.LibName name 'sk4d_font_get_linear_metrics';
  class function  TSkiaApi.sk4d_font_get_metrics;             external TSkiaApi.LibName name 'sk4d_font_get_metrics';
  class procedure TSkiaApi.sk4d_font_get_offsets;             external TSkiaApi.LibName name 'sk4d_font_get_offsets';
  class function  TSkiaApi.sk4d_font_get_path;                external TSkiaApi.LibName name 'sk4d_font_get_path';
  class procedure TSkiaApi.sk4d_font_get_paths;               external TSkiaApi.LibName name 'sk4d_font_get_paths';
  class procedure TSkiaApi.sk4d_font_get_positions;           external TSkiaApi.LibName name 'sk4d_font_get_positions';
  class function  TSkiaApi.sk4d_font_get_scale_x;             external TSkiaApi.LibName name 'sk4d_font_get_scale_x';
  class function  TSkiaApi.sk4d_font_get_size;                external TSkiaApi.LibName name 'sk4d_font_get_size';
  class function  TSkiaApi.sk4d_font_get_skew_x;              external TSkiaApi.LibName name 'sk4d_font_get_skew_x';
  class function  TSkiaApi.sk4d_font_get_subpixel;            external TSkiaApi.LibName name 'sk4d_font_get_subpixel';
  class function  TSkiaApi.sk4d_font_get_typeface;            external TSkiaApi.LibName name 'sk4d_font_get_typeface';
  class function  TSkiaApi.sk4d_font_get_typeface_or_default; external TSkiaApi.LibName name 'sk4d_font_get_typeface_or_default';
  class procedure TSkiaApi.sk4d_font_get_widths_bounds;       external TSkiaApi.LibName name 'sk4d_font_get_widths_bounds';
  class function  TSkiaApi.sk4d_font_is_equal;                external TSkiaApi.LibName name 'sk4d_font_is_equal';
  class function  TSkiaApi.sk4d_font_measure_text;            external TSkiaApi.LibName name 'sk4d_font_measure_text';
  class procedure TSkiaApi.sk4d_font_set_baseline_snap;       external TSkiaApi.LibName name 'sk4d_font_set_baseline_snap';
  class procedure TSkiaApi.sk4d_font_set_edging;              external TSkiaApi.LibName name 'sk4d_font_set_edging';
  class procedure TSkiaApi.sk4d_font_set_embedded_bitmaps;    external TSkiaApi.LibName name 'sk4d_font_set_embedded_bitmaps';
  class procedure TSkiaApi.sk4d_font_set_embolden;            external TSkiaApi.LibName name 'sk4d_font_set_embolden';
  class procedure TSkiaApi.sk4d_font_set_force_auto_hinting;  external TSkiaApi.LibName name 'sk4d_font_set_force_auto_hinting';
  class procedure TSkiaApi.sk4d_font_set_hinting;             external TSkiaApi.LibName name 'sk4d_font_set_hinting';
  class procedure TSkiaApi.sk4d_font_set_linear_metrics;      external TSkiaApi.LibName name 'sk4d_font_set_linear_metrics';
  class procedure TSkiaApi.sk4d_font_set_scale_x;             external TSkiaApi.LibName name 'sk4d_font_set_scale_x';
  class procedure TSkiaApi.sk4d_font_set_size;                external TSkiaApi.LibName name 'sk4d_font_set_size';
  class procedure TSkiaApi.sk4d_font_set_skew_x;              external TSkiaApi.LibName name 'sk4d_font_set_skew_x';
  class procedure TSkiaApi.sk4d_font_set_subpixel;            external TSkiaApi.LibName name 'sk4d_font_set_subpixel';
  class procedure TSkiaApi.sk4d_font_set_typeface;            external TSkiaApi.LibName name 'sk4d_font_set_typeface';
  class function  TSkiaApi.sk4d_font_unichar_to_glyph;        external TSkiaApi.LibName name 'sk4d_font_unichar_to_glyph';
  class procedure TSkiaApi.sk4d_font_unichars_to_glyphs;      external TSkiaApi.LibName name 'sk4d_font_unichars_to_glyphs';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_graphics.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_graphics_allow_jit                                       := GetProc('sk4d_graphics_allow_jit');
  sk4d_graphics_dump_memory_statistics                          := GetProc('sk4d_graphics_dump_memory_statistics');
  sk4d_graphics_get_font_cache_count_limit                      := GetProc('sk4d_graphics_get_font_cache_count_limit');
  sk4d_graphics_get_font_cache_count_used                       := GetProc('sk4d_graphics_get_font_cache_count_used');
  sk4d_graphics_get_font_cache_limit                            := GetProc('sk4d_graphics_get_font_cache_limit');
  sk4d_graphics_get_font_cache_used                             := GetProc('sk4d_graphics_get_font_cache_used');
  sk4d_graphics_get_resource_cache_single_allocation_byte_limit := GetProc('sk4d_graphics_get_resource_cache_single_allocation_byte_limit');
  sk4d_graphics_get_resource_cache_total_byte_limit             := GetProc('sk4d_graphics_get_resource_cache_total_byte_limit');
  sk4d_graphics_get_resource_cache_total_bytes_used             := GetProc('sk4d_graphics_get_resource_cache_total_bytes_used');
  sk4d_graphics_init                                            := GetProc('sk4d_graphics_init');
  sk4d_graphics_purge_all_caches                                := GetProc('sk4d_graphics_purge_all_caches');
  sk4d_graphics_purge_font_cache                                := GetProc('sk4d_graphics_purge_font_cache');
  sk4d_graphics_purge_resource_cache                            := GetProc('sk4d_graphics_purge_resource_cache');
  sk4d_graphics_set_font_cache_count_limit                      := GetProc('sk4d_graphics_set_font_cache_count_limit');
  sk4d_graphics_set_font_cache_limit                            := GetProc('sk4d_graphics_set_font_cache_limit');
  sk4d_graphics_set_resource_cache_single_allocation_byte_limit := GetProc('sk4d_graphics_set_resource_cache_single_allocation_byte_limit');
  sk4d_graphics_set_resource_cache_total_byte_limit             := GetProc('sk4d_graphics_set_resource_cache_total_byte_limit');
  {$ELSE}
  class procedure TSkiaApi.sk4d_graphics_allow_jit;                                       external TSkiaApi.LibName name 'sk4d_graphics_allow_jit';
  class procedure TSkiaApi.sk4d_graphics_dump_memory_statistics;                          external TSkiaApi.LibName name 'sk4d_graphics_dump_memory_statistics';
  class function  TSkiaApi.sk4d_graphics_get_font_cache_count_limit;                      external TSkiaApi.LibName name 'sk4d_graphics_get_font_cache_count_limit';
  class function  TSkiaApi.sk4d_graphics_get_font_cache_count_used;                       external TSkiaApi.LibName name 'sk4d_graphics_get_font_cache_count_used';
  class function  TSkiaApi.sk4d_graphics_get_font_cache_limit;                            external TSkiaApi.LibName name 'sk4d_graphics_get_font_cache_limit';
  class function  TSkiaApi.sk4d_graphics_get_font_cache_used;                             external TSkiaApi.LibName name 'sk4d_graphics_get_font_cache_used';
  class function  TSkiaApi.sk4d_graphics_get_resource_cache_single_allocation_byte_limit; external TSkiaApi.LibName name 'sk4d_graphics_get_resource_cache_single_allocation_byte_limit';
  class function  TSkiaApi.sk4d_graphics_get_resource_cache_total_byte_limit;             external TSkiaApi.LibName name 'sk4d_graphics_get_resource_cache_total_byte_limit';
  class function  TSkiaApi.sk4d_graphics_get_resource_cache_total_bytes_used;             external TSkiaApi.LibName name 'sk4d_graphics_get_resource_cache_total_bytes_used';
  class procedure TSkiaApi.sk4d_graphics_init;                                            external TSkiaApi.LibName name 'sk4d_graphics_init';
  class procedure TSkiaApi.sk4d_graphics_purge_all_caches;                                external TSkiaApi.LibName name 'sk4d_graphics_purge_all_caches';
  class procedure TSkiaApi.sk4d_graphics_purge_font_cache;                                external TSkiaApi.LibName name 'sk4d_graphics_purge_font_cache';
  class procedure TSkiaApi.sk4d_graphics_purge_resource_cache;                            external TSkiaApi.LibName name 'sk4d_graphics_purge_resource_cache';
  class function  TSkiaApi.sk4d_graphics_set_font_cache_count_limit;                      external TSkiaApi.LibName name 'sk4d_graphics_set_font_cache_count_limit';
  class function  TSkiaApi.sk4d_graphics_set_font_cache_limit;                            external TSkiaApi.LibName name 'sk4d_graphics_set_font_cache_limit';
  class function  TSkiaApi.sk4d_graphics_set_resource_cache_single_allocation_byte_limit; external TSkiaApi.LibName name 'sk4d_graphics_set_resource_cache_single_allocation_byte_limit';
  class function  TSkiaApi.sk4d_graphics_set_resource_cache_total_byte_limit;             external TSkiaApi.LibName name 'sk4d_graphics_set_resource_cache_total_byte_limit';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_image.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_image_encode_to_data         := GetProc('sk4d_image_encode_to_data');
  sk4d_image_get_alpha_type         := GetProc('sk4d_image_get_alpha_type');
  sk4d_image_get_color_space        := GetProc('sk4d_image_get_color_space');
  sk4d_image_get_color_type         := GetProc('sk4d_image_get_color_type');
  sk4d_image_get_height             := GetProc('sk4d_image_get_height');
  sk4d_image_get_image_info         := GetProc('sk4d_image_get_image_info');
  sk4d_image_get_unique_id          := GetProc('sk4d_image_get_unique_id');
  sk4d_image_get_width              := GetProc('sk4d_image_get_width');
  sk4d_image_is_lazy_generated      := GetProc('sk4d_image_is_lazy_generated');
  sk4d_image_is_texture_backed      := GetProc('sk4d_image_is_texture_backed');
  sk4d_image_is_valid               := GetProc('sk4d_image_is_valid');
  sk4d_image_make_from_encoded      := GetProc('sk4d_image_make_from_encoded');
  sk4d_image_make_non_texture_image := GetProc('sk4d_image_make_non_texture_image');
  sk4d_image_make_raster            := GetProc('sk4d_image_make_raster');
  sk4d_image_make_raster_image      := GetProc('sk4d_image_make_raster_image');
  sk4d_image_make_shader            := GetProc('sk4d_image_make_shader');
  sk4d_image_make_subset            := GetProc('sk4d_image_make_subset');
  sk4d_image_make_texture_image     := GetProc('sk4d_image_make_texture_image');
  sk4d_image_make_with_filter       := GetProc('sk4d_image_make_with_filter');
  sk4d_image_read_pixels            := GetProc('sk4d_image_read_pixels');
  sk4d_image_scale_pixels           := GetProc('sk4d_image_scale_pixels');
  {$ELSE}
  class function  TSkiaApi.sk4d_image_encode_to_data;         external TSkiaApi.LibName name 'sk4d_image_encode_to_data';
  class function  TSkiaApi.sk4d_image_get_alpha_type;         external TSkiaApi.LibName name 'sk4d_image_get_alpha_type';
  class function  TSkiaApi.sk4d_image_get_color_space;        external TSkiaApi.LibName name 'sk4d_image_get_color_space';
  class function  TSkiaApi.sk4d_image_get_color_type;         external TSkiaApi.LibName name 'sk4d_image_get_color_type';
  class function  TSkiaApi.sk4d_image_get_height;             external TSkiaApi.LibName name 'sk4d_image_get_height';
  class procedure TSkiaApi.sk4d_image_get_image_info;         external TSkiaApi.LibName name 'sk4d_image_get_image_info';
  class function  TSkiaApi.sk4d_image_get_unique_id;          external TSkiaApi.LibName name 'sk4d_image_get_unique_id';
  class function  TSkiaApi.sk4d_image_get_width;              external TSkiaApi.LibName name 'sk4d_image_get_width';
  class function  TSkiaApi.sk4d_image_is_lazy_generated;      external TSkiaApi.LibName name 'sk4d_image_is_lazy_generated';
  class function  TSkiaApi.sk4d_image_is_texture_backed;      external TSkiaApi.LibName name 'sk4d_image_is_texture_backed';
  class function  TSkiaApi.sk4d_image_is_valid;               external TSkiaApi.LibName name 'sk4d_image_is_valid';
  class function  TSkiaApi.sk4d_image_make_from_encoded;      external TSkiaApi.LibName name 'sk4d_image_make_from_encoded';
  class function  TSkiaApi.sk4d_image_make_non_texture_image; external TSkiaApi.LibName name 'sk4d_image_make_non_texture_image';
  class function  TSkiaApi.sk4d_image_make_raster;            external TSkiaApi.LibName name 'sk4d_image_make_raster';
  class function  TSkiaApi.sk4d_image_make_raster_image;      external TSkiaApi.LibName name 'sk4d_image_make_raster_image';
  class function  TSkiaApi.sk4d_image_make_shader;            external TSkiaApi.LibName name 'sk4d_image_make_shader';
  class function  TSkiaApi.sk4d_image_make_subset;            external TSkiaApi.LibName name 'sk4d_image_make_subset';
  class function  TSkiaApi.sk4d_image_make_texture_image;     external TSkiaApi.LibName name 'sk4d_image_make_texture_image';
  class function  TSkiaApi.sk4d_image_make_with_filter;       external TSkiaApi.LibName name 'sk4d_image_make_with_filter';
  class function  TSkiaApi.sk4d_image_read_pixels;            external TSkiaApi.LibName name 'sk4d_image_read_pixels';
  class function  TSkiaApi.sk4d_image_scale_pixels;           external TSkiaApi.LibName name 'sk4d_image_scale_pixels';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_imagefilter.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_imagefilter_make_alpha_threshold      := GetProc('sk4d_imagefilter_make_alpha_threshold');
  sk4d_imagefilter_make_arithmetic           := GetProc('sk4d_imagefilter_make_arithmetic');
  sk4d_imagefilter_make_blend                := GetProc('sk4d_imagefilter_make_blend');
  sk4d_imagefilter_make_blur                 := GetProc('sk4d_imagefilter_make_blur');
  sk4d_imagefilter_make_colorfilter          := GetProc('sk4d_imagefilter_make_colorfilter');
  sk4d_imagefilter_make_compose              := GetProc('sk4d_imagefilter_make_compose');
  sk4d_imagefilter_make_dilate               := GetProc('sk4d_imagefilter_make_dilate');
  sk4d_imagefilter_make_displacement_map     := GetProc('sk4d_imagefilter_make_displacement_map');
  sk4d_imagefilter_make_distant_lit_diffuse  := GetProc('sk4d_imagefilter_make_distant_lit_diffuse');
  sk4d_imagefilter_make_distant_lit_specular := GetProc('sk4d_imagefilter_make_distant_lit_specular');
  sk4d_imagefilter_make_drop_shadow          := GetProc('sk4d_imagefilter_make_drop_shadow');
  sk4d_imagefilter_make_drop_shadow_only     := GetProc('sk4d_imagefilter_make_drop_shadow_only');
  sk4d_imagefilter_make_erode                := GetProc('sk4d_imagefilter_make_erode');
  sk4d_imagefilter_make_image                := GetProc('sk4d_imagefilter_make_image');
  sk4d_imagefilter_make_magnifier            := GetProc('sk4d_imagefilter_make_magnifier');
  sk4d_imagefilter_make_matrix_convolution   := GetProc('sk4d_imagefilter_make_matrix_convolution');
  sk4d_imagefilter_make_matrix_transform     := GetProc('sk4d_imagefilter_make_matrix_transform');
  sk4d_imagefilter_make_merge                := GetProc('sk4d_imagefilter_make_merge');
  sk4d_imagefilter_make_offset               := GetProc('sk4d_imagefilter_make_offset');
  sk4d_imagefilter_make_picture              := GetProc('sk4d_imagefilter_make_picture');
  sk4d_imagefilter_make_point_lit_diffuse    := GetProc('sk4d_imagefilter_make_point_lit_diffuse');
  sk4d_imagefilter_make_point_lit_specular   := GetProc('sk4d_imagefilter_make_point_lit_specular');
  sk4d_imagefilter_make_spot_lit_diffuse     := GetProc('sk4d_imagefilter_make_spot_lit_diffuse');
  sk4d_imagefilter_make_spot_lit_specular    := GetProc('sk4d_imagefilter_make_spot_lit_specular');
  sk4d_imagefilter_make_tile                 := GetProc('sk4d_imagefilter_make_tile');
  {$ELSE}
  class function TSkiaApi.sk4d_imagefilter_make_alpha_threshold;      external TSkiaApi.LibName name 'sk4d_imagefilter_make_alpha_threshold';
  class function TSkiaApi.sk4d_imagefilter_make_arithmetic;           external TSkiaApi.LibName name 'sk4d_imagefilter_make_arithmetic';
  class function TSkiaApi.sk4d_imagefilter_make_blend;                external TSkiaApi.LibName name 'sk4d_imagefilter_make_blend';
  class function TSkiaApi.sk4d_imagefilter_make_blur;                 external TSkiaApi.LibName name 'sk4d_imagefilter_make_blur';
  class function TSkiaApi.sk4d_imagefilter_make_colorfilter;          external TSkiaApi.LibName name 'sk4d_imagefilter_make_colorfilter';
  class function TSkiaApi.sk4d_imagefilter_make_compose;              external TSkiaApi.LibName name 'sk4d_imagefilter_make_compose';
  class function TSkiaApi.sk4d_imagefilter_make_dilate;               external TSkiaApi.LibName name 'sk4d_imagefilter_make_dilate';
  class function TSkiaApi.sk4d_imagefilter_make_displacement_map;     external TSkiaApi.LibName name 'sk4d_imagefilter_make_displacement_map';
  class function TSkiaApi.sk4d_imagefilter_make_distant_lit_diffuse;  external TSkiaApi.LibName name 'sk4d_imagefilter_make_distant_lit_diffuse';
  class function TSkiaApi.sk4d_imagefilter_make_distant_lit_specular; external TSkiaApi.LibName name 'sk4d_imagefilter_make_distant_lit_specular';
  class function TSkiaApi.sk4d_imagefilter_make_drop_shadow;          external TSkiaApi.LibName name 'sk4d_imagefilter_make_drop_shadow';
  class function TSkiaApi.sk4d_imagefilter_make_drop_shadow_only;     external TSkiaApi.LibName name 'sk4d_imagefilter_make_drop_shadow_only';
  class function TSkiaApi.sk4d_imagefilter_make_erode;                external TSkiaApi.LibName name 'sk4d_imagefilter_make_erode';
  class function TSkiaApi.sk4d_imagefilter_make_image;                external TSkiaApi.LibName name 'sk4d_imagefilter_make_image';
  class function TSkiaApi.sk4d_imagefilter_make_magnifier;            external TSkiaApi.LibName name 'sk4d_imagefilter_make_magnifier';
  class function TSkiaApi.sk4d_imagefilter_make_matrix_convolution;   external TSkiaApi.LibName name 'sk4d_imagefilter_make_matrix_convolution';
  class function TSkiaApi.sk4d_imagefilter_make_matrix_transform;     external TSkiaApi.LibName name 'sk4d_imagefilter_make_matrix_transform';
  class function TSkiaApi.sk4d_imagefilter_make_merge;                external TSkiaApi.LibName name 'sk4d_imagefilter_make_merge';
  class function TSkiaApi.sk4d_imagefilter_make_offset;               external TSkiaApi.LibName name 'sk4d_imagefilter_make_offset';
  class function TSkiaApi.sk4d_imagefilter_make_picture;              external TSkiaApi.LibName name 'sk4d_imagefilter_make_picture';
  class function TSkiaApi.sk4d_imagefilter_make_point_lit_diffuse;    external TSkiaApi.LibName name 'sk4d_imagefilter_make_point_lit_diffuse';
  class function TSkiaApi.sk4d_imagefilter_make_point_lit_specular;   external TSkiaApi.LibName name 'sk4d_imagefilter_make_point_lit_specular';
  class function TSkiaApi.sk4d_imagefilter_make_spot_lit_diffuse;     external TSkiaApi.LibName name 'sk4d_imagefilter_make_spot_lit_diffuse';
  class function TSkiaApi.sk4d_imagefilter_make_spot_lit_specular;    external TSkiaApi.LibName name 'sk4d_imagefilter_make_spot_lit_specular';
  class function TSkiaApi.sk4d_imagefilter_make_tile;                 external TSkiaApi.LibName name 'sk4d_imagefilter_make_tile';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_maskfilter.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_maskfilter_make_blur        := GetProc('sk4d_maskfilter_make_blur');
  sk4d_maskfilter_make_table       := GetProc('sk4d_maskfilter_make_table');
  sk4d_maskfilter_make_table_clip  := GetProc('sk4d_maskfilter_make_table_clip');
  sk4d_maskfilter_make_table_gamma := GetProc('sk4d_maskfilter_make_table_gamma');
  {$ELSE}
  class function TSkiaApi.sk4d_maskfilter_make_blur;        external TSkiaApi.LibName name 'sk4d_maskfilter_make_blur';
  class function TSkiaApi.sk4d_maskfilter_make_table;       external TSkiaApi.LibName name 'sk4d_maskfilter_make_table';
  class function TSkiaApi.sk4d_maskfilter_make_table_clip;  external TSkiaApi.LibName name 'sk4d_maskfilter_make_table_clip';
  class function TSkiaApi.sk4d_maskfilter_make_table_gamma; external TSkiaApi.LibName name 'sk4d_maskfilter_make_table_gamma';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_paint.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_paint_create             := GetProc('sk4d_paint_create');
  sk4d_paint_create2            := GetProc('sk4d_paint_create2');
  sk4d_paint_destroy            := GetProc('sk4d_paint_destroy');
  sk4d_paint_get_alpha          := GetProc('sk4d_paint_get_alpha');
  sk4d_paint_get_alphaf         := GetProc('sk4d_paint_get_alphaf');
  sk4d_paint_get_anti_alias     := GetProc('sk4d_paint_get_anti_alias');
  sk4d_paint_get_blend_mode     := GetProc('sk4d_paint_get_blend_mode');
  sk4d_paint_get_color          := GetProc('sk4d_paint_get_color');
  sk4d_paint_get_colorf         := GetProc('sk4d_paint_get_colorf');
  sk4d_paint_get_color_filter   := GetProc('sk4d_paint_get_color_filter');
  sk4d_paint_get_dither         := GetProc('sk4d_paint_get_dither');
  sk4d_paint_get_fill_path      := GetProc('sk4d_paint_get_fill_path');
  sk4d_paint_get_filter_quality := GetProc('sk4d_paint_get_filter_quality');
  sk4d_paint_get_image_filter   := GetProc('sk4d_paint_get_image_filter');
  sk4d_paint_get_mask_filter    := GetProc('sk4d_paint_get_mask_filter');
  sk4d_paint_get_path_effect    := GetProc('sk4d_paint_get_path_effect');
  sk4d_paint_get_shader         := GetProc('sk4d_paint_get_shader');
  sk4d_paint_get_stroke_cap     := GetProc('sk4d_paint_get_stroke_cap');
  sk4d_paint_get_stroke_join    := GetProc('sk4d_paint_get_stroke_join');
  sk4d_paint_get_stroke_miter   := GetProc('sk4d_paint_get_stroke_miter');
  sk4d_paint_get_stroke_width   := GetProc('sk4d_paint_get_stroke_width');
  sk4d_paint_get_style          := GetProc('sk4d_paint_get_style');
  sk4d_paint_reset              := GetProc('sk4d_paint_reset');
  sk4d_paint_set_alpha          := GetProc('sk4d_paint_set_alpha');
  sk4d_paint_set_alphaf         := GetProc('sk4d_paint_set_alphaf');
  sk4d_paint_set_antialias      := GetProc('sk4d_paint_set_antialias');
  sk4d_paint_set_argb           := GetProc('sk4d_paint_set_argb');
  sk4d_paint_set_blend_mode     := GetProc('sk4d_paint_set_blend_mode');
  sk4d_paint_set_color          := GetProc('sk4d_paint_set_color');
  sk4d_paint_set_colorf         := GetProc('sk4d_paint_set_colorf');
  sk4d_paint_set_color_filter   := GetProc('sk4d_paint_set_color_filter');
  sk4d_paint_set_dither         := GetProc('sk4d_paint_set_dither');
  sk4d_paint_set_filter_quality := GetProc('sk4d_paint_set_filter_quality');
  sk4d_paint_set_image_filter   := GetProc('sk4d_paint_set_image_filter');
  sk4d_paint_set_mask_filter    := GetProc('sk4d_paint_set_mask_filter');
  sk4d_paint_set_path_effect    := GetProc('sk4d_paint_set_path_effect');
  sk4d_paint_set_shader         := GetProc('sk4d_paint_set_shader');
  sk4d_paint_set_stroke_cap     := GetProc('sk4d_paint_set_stroke_cap');
  sk4d_paint_set_stroke_join    := GetProc('sk4d_paint_set_stroke_join');
  sk4d_paint_set_stroke_miter   := GetProc('sk4d_paint_set_stroke_miter');
  sk4d_paint_set_stroke_width   := GetProc('sk4d_paint_set_stroke_width');
  sk4d_paint_set_style          := GetProc('sk4d_paint_set_style');
  {$ELSE}
  class function  TSkiaApi.sk4d_paint_create;             external TSkiaApi.LibName name 'sk4d_paint_create';
  class function  TSkiaApi.sk4d_paint_create2;            external TSkiaApi.LibName name 'sk4d_paint_create2';
  class procedure TSkiaApi.sk4d_paint_destroy;            external TSkiaApi.LibName name 'sk4d_paint_destroy';
  class function  TSkiaApi.sk4d_paint_get_alpha;          external TSkiaApi.LibName name 'sk4d_paint_get_alpha';
  class function  TSkiaApi.sk4d_paint_get_alphaf;         external TSkiaApi.LibName name 'sk4d_paint_get_alphaf';
  class function  TSkiaApi.sk4d_paint_get_anti_alias;     external TSkiaApi.LibName name 'sk4d_paint_get_anti_alias';
  class function  TSkiaApi.sk4d_paint_get_blend_mode;     external TSkiaApi.LibName name 'sk4d_paint_get_blend_mode';
  class function  TSkiaApi.sk4d_paint_get_color;          external TSkiaApi.LibName name 'sk4d_paint_get_color';
  class procedure TSkiaApi.sk4d_paint_get_colorf;         external TSkiaApi.LibName name 'sk4d_paint_get_colorf';
  class function  TSkiaApi.sk4d_paint_get_color_filter;   external TSkiaApi.LibName name 'sk4d_paint_get_color_filter';
  class function  TSkiaApi.sk4d_paint_get_dither;         external TSkiaApi.LibName name 'sk4d_paint_get_dither';
  class function  TSkiaApi.sk4d_paint_get_fill_path;      external TSkiaApi.LibName name 'sk4d_paint_get_fill_path';
  class function  TSkiaApi.sk4d_paint_get_filter_quality; external TSkiaApi.LibName name 'sk4d_paint_get_filter_quality';
  class function  TSkiaApi.sk4d_paint_get_image_filter;   external TSkiaApi.LibName name 'sk4d_paint_get_image_filter';
  class function  TSkiaApi.sk4d_paint_get_mask_filter;    external TSkiaApi.LibName name 'sk4d_paint_get_mask_filter';
  class function  TSkiaApi.sk4d_paint_get_path_effect;    external TSkiaApi.LibName name 'sk4d_paint_get_path_effect';
  class function  TSkiaApi.sk4d_paint_get_shader;         external TSkiaApi.LibName name 'sk4d_paint_get_shader';
  class function  TSkiaApi.sk4d_paint_get_stroke_cap;     external TSkiaApi.LibName name 'sk4d_paint_get_stroke_cap';
  class function  TSkiaApi.sk4d_paint_get_stroke_join;    external TSkiaApi.LibName name 'sk4d_paint_get_stroke_join';
  class function  TSkiaApi.sk4d_paint_get_stroke_miter;   external TSkiaApi.LibName name 'sk4d_paint_get_stroke_miter';
  class function  TSkiaApi.sk4d_paint_get_stroke_width;   external TSkiaApi.LibName name 'sk4d_paint_get_stroke_width';
  class function  TSkiaApi.sk4d_paint_get_style;          external TSkiaApi.LibName name 'sk4d_paint_get_style';
  class procedure TSkiaApi.sk4d_paint_reset;              external TSkiaApi.LibName name 'sk4d_paint_reset';
  class procedure TSkiaApi.sk4d_paint_set_alpha;          external TSkiaApi.LibName name 'sk4d_paint_set_alpha';
  class procedure TSkiaApi.sk4d_paint_set_alphaf;         external TSkiaApi.LibName name 'sk4d_paint_set_alphaf';
  class procedure TSkiaApi.sk4d_paint_set_antialias;      external TSkiaApi.LibName name 'sk4d_paint_set_antialias';
  class procedure TSkiaApi.sk4d_paint_set_argb;           external TSkiaApi.LibName name 'sk4d_paint_set_argb';
  class procedure TSkiaApi.sk4d_paint_set_blend_mode;     external TSkiaApi.LibName name 'sk4d_paint_set_blend_mode';
  class procedure TSkiaApi.sk4d_paint_set_color;          external TSkiaApi.LibName name 'sk4d_paint_set_color';
  class procedure TSkiaApi.sk4d_paint_set_colorf;         external TSkiaApi.LibName name 'sk4d_paint_set_colorf';
  class procedure TSkiaApi.sk4d_paint_set_color_filter;   external TSkiaApi.LibName name 'sk4d_paint_set_color_filter';
  class procedure TSkiaApi.sk4d_paint_set_dither;         external TSkiaApi.LibName name 'sk4d_paint_set_dither';
  class procedure TSkiaApi.sk4d_paint_set_filter_quality; external TSkiaApi.LibName name 'sk4d_paint_set_filter_quality';
  class procedure TSkiaApi.sk4d_paint_set_image_filter;   external TSkiaApi.LibName name 'sk4d_paint_set_image_filter';
  class procedure TSkiaApi.sk4d_paint_set_mask_filter;    external TSkiaApi.LibName name 'sk4d_paint_set_mask_filter';
  class procedure TSkiaApi.sk4d_paint_set_path_effect;    external TSkiaApi.LibName name 'sk4d_paint_set_path_effect';
  class procedure TSkiaApi.sk4d_paint_set_shader;         external TSkiaApi.LibName name 'sk4d_paint_set_shader';
  class procedure TSkiaApi.sk4d_paint_set_stroke_cap;     external TSkiaApi.LibName name 'sk4d_paint_set_stroke_cap';
  class procedure TSkiaApi.sk4d_paint_set_stroke_join;    external TSkiaApi.LibName name 'sk4d_paint_set_stroke_join';
  class procedure TSkiaApi.sk4d_paint_set_stroke_miter;   external TSkiaApi.LibName name 'sk4d_paint_set_stroke_miter';
  class procedure TSkiaApi.sk4d_paint_set_stroke_width;   external TSkiaApi.LibName name 'sk4d_paint_set_stroke_width';
  class procedure TSkiaApi.sk4d_paint_set_style;          external TSkiaApi.LibName name 'sk4d_paint_set_style';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_path.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_opbuilder_add               := GetProc('sk4d_opbuilder_add');
  sk4d_opbuilder_create            := GetProc('sk4d_opbuilder_create');
  sk4d_opbuilder_destroy           := GetProc('sk4d_opbuilder_destroy');
  sk4d_opbuilder_detach            := GetProc('sk4d_opbuilder_detach');
  sk4d_path_contains               := GetProc('sk4d_path_contains');
  sk4d_path_create                 := GetProc('sk4d_path_create');
  sk4d_path_create2                := GetProc('sk4d_path_create2');
  sk4d_path_destroy                := GetProc('sk4d_path_destroy');
  sk4d_path_get_bounds             := GetProc('sk4d_path_get_bounds');
  sk4d_path_get_fill_type          := GetProc('sk4d_path_get_fill_type');
  sk4d_path_get_last_point         := GetProc('sk4d_path_get_last_point');
  sk4d_path_get_segment_masks      := GetProc('sk4d_path_get_segment_masks');
  sk4d_path_get_tight_bounds       := GetProc('sk4d_path_get_tight_bounds');
  sk4d_path_interpolate            := GetProc('sk4d_path_interpolate');
  sk4d_path_is_convex              := GetProc('sk4d_path_is_convex');
  sk4d_path_is_empty               := GetProc('sk4d_path_is_empty');
  sk4d_path_is_finite              := GetProc('sk4d_path_is_finite');
  sk4d_path_is_interpolatable      := GetProc('sk4d_path_is_interpolatable');
  sk4d_path_is_last_contour_closed := GetProc('sk4d_path_is_last_contour_closed');
  sk4d_path_is_line                := GetProc('sk4d_path_is_line');
  sk4d_path_is_oval                := GetProc('sk4d_path_is_oval');
  sk4d_path_is_rect                := GetProc('sk4d_path_is_rect');
  sk4d_path_is_rrect               := GetProc('sk4d_path_is_rrect');
  sk4d_path_offset                 := GetProc('sk4d_path_offset');
  sk4d_path_op                     := GetProc('sk4d_path_op');
  sk4d_path_to_svg                 := GetProc('sk4d_path_to_svg');
  sk4d_path_transform              := GetProc('sk4d_path_transform');
  sk4d_pathiterator_create         := GetProc('sk4d_pathiterator_create');
  sk4d_pathiterator_destroy        := GetProc('sk4d_pathiterator_destroy');
  sk4d_pathiterator_next           := GetProc('sk4d_pathiterator_next');
  {$ELSE}
  class procedure TSkiaApi.sk4d_opbuilder_add;               external TSkiaApi.LibName name 'sk4d_opbuilder_add';
  class function  TSkiaApi.sk4d_opbuilder_create;            external TSkiaApi.LibName name 'sk4d_opbuilder_create';
  class procedure TSkiaApi.sk4d_opbuilder_destroy;           external TSkiaApi.LibName name 'sk4d_opbuilder_destroy';
  class function  TSkiaApi.sk4d_opbuilder_detach;            external TSkiaApi.LibName name 'sk4d_opbuilder_detach';
  class function  TSkiaApi.sk4d_path_contains;               external TSkiaApi.LibName name 'sk4d_path_contains';
  class function  TSkiaApi.sk4d_path_create;                 external TSkiaApi.LibName name 'sk4d_path_create';
  class function  TSkiaApi.sk4d_path_create2;                external TSkiaApi.LibName name 'sk4d_path_create2';
  class procedure TSkiaApi.sk4d_path_destroy;                external TSkiaApi.LibName name 'sk4d_path_destroy';
  class procedure TSkiaApi.sk4d_path_get_bounds;             external TSkiaApi.LibName name 'sk4d_path_get_bounds';
  class function  TSkiaApi.sk4d_path_get_fill_type;          external TSkiaApi.LibName name 'sk4d_path_get_fill_type';
  class function  TSkiaApi.sk4d_path_get_last_point;         external TSkiaApi.LibName name 'sk4d_path_get_last_point';
  class function  TSkiaApi.sk4d_path_get_segment_masks;      external TSkiaApi.LibName name 'sk4d_path_get_segment_masks';
  class procedure TSkiaApi.sk4d_path_get_tight_bounds;       external TSkiaApi.LibName name 'sk4d_path_get_tight_bounds';
  class function  TSkiaApi.sk4d_path_interpolate;            external TSkiaApi.LibName name 'sk4d_path_interpolate';
  class function  TSkiaApi.sk4d_path_is_convex;              external TSkiaApi.LibName name 'sk4d_path_is_convex';
  class function  TSkiaApi.sk4d_path_is_empty;               external TSkiaApi.LibName name 'sk4d_path_is_empty';
  class function  TSkiaApi.sk4d_path_is_finite;              external TSkiaApi.LibName name 'sk4d_path_is_finite';
  class function  TSkiaApi.sk4d_path_is_interpolatable;      external TSkiaApi.LibName name 'sk4d_path_is_interpolatable';
  class function  TSkiaApi.sk4d_path_is_last_contour_closed; external TSkiaApi.LibName name 'sk4d_path_is_last_contour_closed';
  class function  TSkiaApi.sk4d_path_is_line;                external TSkiaApi.LibName name 'sk4d_path_is_line';
  class function  TSkiaApi.sk4d_path_is_oval;                external TSkiaApi.LibName name 'sk4d_path_is_oval';
  class function  TSkiaApi.sk4d_path_is_rect;                external TSkiaApi.LibName name 'sk4d_path_is_rect';
  class function  TSkiaApi.sk4d_path_is_rrect;               external TSkiaApi.LibName name 'sk4d_path_is_rrect';
  class procedure TSkiaApi.sk4d_path_offset;                 external TSkiaApi.LibName name 'sk4d_path_offset';
  class function  TSkiaApi.sk4d_path_op;                     external TSkiaApi.LibName name 'sk4d_path_op';
  class procedure TSkiaApi.sk4d_path_to_svg;                 external TSkiaApi.LibName name 'sk4d_path_to_svg';
  class procedure TSkiaApi.sk4d_path_transform;              external TSkiaApi.LibName name 'sk4d_path_transform';
  class function  TSkiaApi.sk4d_pathiterator_create;         external TSkiaApi.LibName name 'sk4d_pathiterator_create';
  class procedure TSkiaApi.sk4d_pathiterator_destroy;        external TSkiaApi.LibName name 'sk4d_pathiterator_destroy';
  class function  TSkiaApi.sk4d_pathiterator_next;           external TSkiaApi.LibName name 'sk4d_pathiterator_next';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_pathbuilder.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_pathbuilder_add_arc                 := GetProc('sk4d_pathbuilder_add_arc');
  sk4d_pathbuilder_add_circle              := GetProc('sk4d_pathbuilder_add_circle');
  sk4d_pathbuilder_add_oval                := GetProc('sk4d_pathbuilder_add_oval');
  sk4d_pathbuilder_add_polygon             := GetProc('sk4d_pathbuilder_add_polygon');
  sk4d_pathbuilder_add_rect                := GetProc('sk4d_pathbuilder_add_rect');
  sk4d_pathbuilder_add_rrect               := GetProc('sk4d_pathbuilder_add_rrect');
  sk4d_pathbuilder_arc_to                  := GetProc('sk4d_pathbuilder_arc_to');
  sk4d_pathbuilder_arc_to2                 := GetProc('sk4d_pathbuilder_arc_to2');
  sk4d_pathbuilder_arc_to3                 := GetProc('sk4d_pathbuilder_arc_to3');
  sk4d_pathbuilder_close                   := GetProc('sk4d_pathbuilder_close');
  sk4d_pathbuilder_conic_to                := GetProc('sk4d_pathbuilder_conic_to');
  sk4d_pathbuilder_create                  := GetProc('sk4d_pathbuilder_create');
  sk4d_pathbuilder_create2                 := GetProc('sk4d_pathbuilder_create2');
  sk4d_pathbuilder_cubic_to                := GetProc('sk4d_pathbuilder_cubic_to');
  sk4d_pathbuilder_destroy                 := GetProc('sk4d_pathbuilder_destroy');
  sk4d_pathbuilder_detach                  := GetProc('sk4d_pathbuilder_detach');
  sk4d_pathbuilder_get_bounds              := GetProc('sk4d_pathbuilder_get_bounds');
  sk4d_pathbuilder_get_fill_type           := GetProc('sk4d_pathbuilder_get_fill_type');
  sk4d_pathbuilder_inc_reserve             := GetProc('sk4d_pathbuilder_inc_reserve');
  sk4d_pathbuilder_line_to                 := GetProc('sk4d_pathbuilder_line_to');
  sk4d_pathbuilder_move_to                 := GetProc('sk4d_pathbuilder_move_to');
  sk4d_pathbuilder_offset                  := GetProc('sk4d_pathbuilder_offset');
  sk4d_pathbuilder_polyline_to             := GetProc('sk4d_pathbuilder_polyline_to');
  sk4d_pathbuilder_quad_to                 := GetProc('sk4d_pathbuilder_quad_to');
  sk4d_pathbuilder_r_conic_to              := GetProc('sk4d_pathbuilder_r_conic_to');
  sk4d_pathbuilder_r_cubic_to              := GetProc('sk4d_pathbuilder_r_cubic_to');
  sk4d_pathbuilder_r_line_to               := GetProc('sk4d_pathbuilder_r_line_to');
  sk4d_pathbuilder_r_quad_to               := GetProc('sk4d_pathbuilder_r_quad_to');
  sk4d_pathbuilder_reset                   := GetProc('sk4d_pathbuilder_reset');
  sk4d_pathbuilder_set_filltype            := GetProc('sk4d_pathbuilder_set_filltype');
  sk4d_pathbuilder_snapshot                := GetProc('sk4d_pathbuilder_snapshot');
  sk4d_pathbuilder_toggle_inverse_filltype := GetProc('sk4d_pathbuilder_toggle_inverse_filltype');
  {$ELSE}
  class procedure TSkiaApi.sk4d_pathbuilder_add_arc;                 external TSkiaApi.LibName name 'sk4d_pathbuilder_add_arc';
  class procedure TSkiaApi.sk4d_pathbuilder_add_circle;              external TSkiaApi.LibName name 'sk4d_pathbuilder_add_circle';
  class procedure TSkiaApi.sk4d_pathbuilder_add_oval;                external TSkiaApi.LibName name 'sk4d_pathbuilder_add_oval';
  class procedure TSkiaApi.sk4d_pathbuilder_add_polygon;             external TSkiaApi.LibName name 'sk4d_pathbuilder_add_polygon';
  class procedure TSkiaApi.sk4d_pathbuilder_add_rect;                external TSkiaApi.LibName name 'sk4d_pathbuilder_add_rect';
  class procedure TSkiaApi.sk4d_pathbuilder_add_rrect;               external TSkiaApi.LibName name 'sk4d_pathbuilder_add_rrect';
  class procedure TSkiaApi.sk4d_pathbuilder_arc_to;                  external TSkiaApi.LibName name 'sk4d_pathbuilder_arc_to';
  class procedure TSkiaApi.sk4d_pathbuilder_arc_to2;                 external TSkiaApi.LibName name 'sk4d_pathbuilder_arc_to2';
  class procedure TSkiaApi.sk4d_pathbuilder_arc_to3;                 external TSkiaApi.LibName name 'sk4d_pathbuilder_arc_to3';
  class procedure TSkiaApi.sk4d_pathbuilder_close;                   external TSkiaApi.LibName name 'sk4d_pathbuilder_close';
  class procedure TSkiaApi.sk4d_pathbuilder_conic_to;                external TSkiaApi.LibName name 'sk4d_pathbuilder_conic_to';
  class function  TSkiaApi.sk4d_pathbuilder_create;                  external TSkiaApi.LibName name 'sk4d_pathbuilder_create';
  class function  TSkiaApi.sk4d_pathbuilder_create2;                 external TSkiaApi.LibName name 'sk4d_pathbuilder_create2';
  class procedure TSkiaApi.sk4d_pathbuilder_cubic_to;                external TSkiaApi.LibName name 'sk4d_pathbuilder_cubic_to';
  class procedure TSkiaApi.sk4d_pathbuilder_destroy;                 external TSkiaApi.LibName name 'sk4d_pathbuilder_destroy';
  class function  TSkiaApi.sk4d_pathbuilder_detach;                  external TSkiaApi.LibName name 'sk4d_pathbuilder_detach';
  class procedure TSkiaApi.sk4d_pathbuilder_get_bounds;              external TSkiaApi.LibName name 'sk4d_pathbuilder_get_bounds';
  class function  TSkiaApi.sk4d_pathbuilder_get_fill_type;           external TSkiaApi.LibName name 'sk4d_pathbuilder_get_fill_type';
  class procedure TSkiaApi.sk4d_pathbuilder_inc_reserve;             external TSkiaApi.LibName name 'sk4d_pathbuilder_inc_reserve';
  class procedure TSkiaApi.sk4d_pathbuilder_line_to;                 external TSkiaApi.LibName name 'sk4d_pathbuilder_line_to';
  class procedure TSkiaApi.sk4d_pathbuilder_move_to;                 external TSkiaApi.LibName name 'sk4d_pathbuilder_move_to';
  class procedure TSkiaApi.sk4d_pathbuilder_offset;                  external TSkiaApi.LibName name 'sk4d_pathbuilder_offset';
  class procedure TSkiaApi.sk4d_pathbuilder_polyline_to;             external TSkiaApi.LibName name 'sk4d_pathbuilder_polyline_to';
  class procedure TSkiaApi.sk4d_pathbuilder_quad_to;                 external TSkiaApi.LibName name 'sk4d_pathbuilder_quad_to';
  class procedure TSkiaApi.sk4d_pathbuilder_r_conic_to;              external TSkiaApi.LibName name 'sk4d_pathbuilder_r_conic_to';
  class procedure TSkiaApi.sk4d_pathbuilder_r_cubic_to;              external TSkiaApi.LibName name 'sk4d_pathbuilder_r_cubic_to';
  class procedure TSkiaApi.sk4d_pathbuilder_r_line_to;               external TSkiaApi.LibName name 'sk4d_pathbuilder_r_line_to';
  class procedure TSkiaApi.sk4d_pathbuilder_r_quad_to;               external TSkiaApi.LibName name 'sk4d_pathbuilder_r_quad_to';
  class procedure TSkiaApi.sk4d_pathbuilder_reset;                   external TSkiaApi.LibName name 'sk4d_pathbuilder_reset';
  class procedure TSkiaApi.sk4d_pathbuilder_set_filltype;            external TSkiaApi.LibName name 'sk4d_pathbuilder_set_filltype';
  class function  TSkiaApi.sk4d_pathbuilder_snapshot;                external TSkiaApi.LibName name 'sk4d_pathbuilder_snapshot';
  class procedure TSkiaApi.sk4d_pathbuilder_toggle_inverse_filltype; external TSkiaApi.LibName name 'sk4d_pathbuilder_toggle_inverse_filltype';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_patheffect.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_patheffect_make_1dpath   := GetProc('sk4d_patheffect_make_1dpath');
  sk4d_patheffect_make_2dline   := GetProc('sk4d_patheffect_make_2dline');
  sk4d_patheffect_make_2dpath   := GetProc('sk4d_patheffect_make_2dpath');
  sk4d_patheffect_make_compose  := GetProc('sk4d_patheffect_make_compose');
  sk4d_patheffect_make_corner   := GetProc('sk4d_patheffect_make_corner');
  sk4d_patheffect_make_dash     := GetProc('sk4d_patheffect_make_dash');
  sk4d_patheffect_make_discrete := GetProc('sk4d_patheffect_make_discrete');
  sk4d_patheffect_make_sum      := GetProc('sk4d_patheffect_make_sum');
  sk4d_patheffect_make_trim     := GetProc('sk4d_patheffect_make_trim');
  {$ELSE}
  class function TSkiaApi.sk4d_patheffect_make_1dpath;   external TSkiaApi.LibName name 'sk4d_patheffect_make_1dpath';
  class function TSkiaApi.sk4d_patheffect_make_2dline;   external TSkiaApi.LibName name 'sk4d_patheffect_make_2dline';
  class function TSkiaApi.sk4d_patheffect_make_2dpath;   external TSkiaApi.LibName name 'sk4d_patheffect_make_2dpath';
  class function TSkiaApi.sk4d_patheffect_make_compose;  external TSkiaApi.LibName name 'sk4d_patheffect_make_compose';
  class function TSkiaApi.sk4d_patheffect_make_corner;   external TSkiaApi.LibName name 'sk4d_patheffect_make_corner';
  class function TSkiaApi.sk4d_patheffect_make_dash;     external TSkiaApi.LibName name 'sk4d_patheffect_make_dash';
  class function TSkiaApi.sk4d_patheffect_make_discrete; external TSkiaApi.LibName name 'sk4d_patheffect_make_discrete';
  class function TSkiaApi.sk4d_patheffect_make_sum;      external TSkiaApi.LibName name 'sk4d_patheffect_make_sum';
  class function TSkiaApi.sk4d_patheffect_make_trim;     external TSkiaApi.LibName name 'sk4d_patheffect_make_trim';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_pathmeasure.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_pathmeasure_create                   := GetProc('sk4d_pathmeasure_create');
  sk4d_pathmeasure_destroy                  := GetProc('sk4d_pathmeasure_destroy');
  sk4d_pathmeasure_get_length               := GetProc('sk4d_pathmeasure_get_length');
  sk4d_pathmeasure_get_matrix               := GetProc('sk4d_pathmeasure_get_matrix');
  sk4d_pathmeasure_get_position_and_tangent := GetProc('sk4d_pathmeasure_get_position_and_tangent');
  sk4d_pathmeasure_get_segment              := GetProc('sk4d_pathmeasure_get_segment');
  sk4d_pathmeasure_is_closed                := GetProc('sk4d_pathmeasure_is_closed');
  sk4d_pathmeasure_next_contour             := GetProc('sk4d_pathmeasure_next_contour');
  {$ELSE}
  class function  TSkiaApi.sk4d_pathmeasure_create;                   external TSkiaApi.LibName name 'sk4d_pathmeasure_create';
  class procedure TSkiaApi.sk4d_pathmeasure_destroy;                  external TSkiaApi.LibName name 'sk4d_pathmeasure_destroy';
  class function  TSkiaApi.sk4d_pathmeasure_get_length;               external TSkiaApi.LibName name 'sk4d_pathmeasure_get_length';
  class function  TSkiaApi.sk4d_pathmeasure_get_matrix;               external TSkiaApi.LibName name 'sk4d_pathmeasure_get_matrix';
  class function  TSkiaApi.sk4d_pathmeasure_get_position_and_tangent; external TSkiaApi.LibName name 'sk4d_pathmeasure_get_position_and_tangent';
  class function  TSkiaApi.sk4d_pathmeasure_get_segment;              external TSkiaApi.LibName name 'sk4d_pathmeasure_get_segment';
  class function  TSkiaApi.sk4d_pathmeasure_is_closed;                external TSkiaApi.LibName name 'sk4d_pathmeasure_is_closed';
  class function  TSkiaApi.sk4d_pathmeasure_next_contour;             external TSkiaApi.LibName name 'sk4d_pathmeasure_next_contour';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_picture.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_picture_get_cull_rect       := GetProc('sk4d_picture_get_cull_rect');
  sk4d_picture_get_unique_id       := GetProc('sk4d_picture_get_unique_id');
  sk4d_picture_make_from_data      := GetProc('sk4d_picture_make_from_data');
  sk4d_picture_make_from_stream    := GetProc('sk4d_picture_make_from_stream');
  sk4d_picture_make_shader         := GetProc('sk4d_picture_make_shader');
  sk4d_picture_serialize_to_stream := GetProc('sk4d_picture_serialize_to_stream');
  {$ELSE}
  class procedure TSkiaApi.sk4d_picture_get_cull_rect;       external TSkiaApi.LibName name 'sk4d_picture_get_cull_rect';
  class function  TSkiaApi.sk4d_picture_get_unique_id;       external TSkiaApi.LibName name 'sk4d_picture_get_unique_id';
  class function  TSkiaApi.sk4d_picture_make_from_data;      external TSkiaApi.LibName name 'sk4d_picture_make_from_data';
  class function  TSkiaApi.sk4d_picture_make_from_stream;    external TSkiaApi.LibName name 'sk4d_picture_make_from_stream';
  class function  TSkiaApi.sk4d_picture_make_shader;         external TSkiaApi.LibName name 'sk4d_picture_make_shader';
  class procedure TSkiaApi.sk4d_picture_serialize_to_stream; external TSkiaApi.LibName name 'sk4d_picture_serialize_to_stream';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_picturerecorder.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_picturerecorder_begin_recording   := GetProc('sk4d_picturerecorder_begin_recording');
  sk4d_picturerecorder_create            := GetProc('sk4d_picturerecorder_create');
  sk4d_picturerecorder_destroy           := GetProc('sk4d_picturerecorder_destroy');
  sk4d_picturerecorder_finish_recording  := GetProc('sk4d_picturerecorder_finish_recording');
  sk4d_picturerecorder_finish_recording2 := GetProc('sk4d_picturerecorder_finish_recording2');
  {$ELSE}
  class function  TSkiaApi.sk4d_picturerecorder_begin_recording;   external TSkiaApi.LibName name 'sk4d_picturerecorder_begin_recording';
  class function  TSkiaApi.sk4d_picturerecorder_create;            external TSkiaApi.LibName name 'sk4d_picturerecorder_create';
  class procedure TSkiaApi.sk4d_picturerecorder_destroy;           external TSkiaApi.LibName name 'sk4d_picturerecorder_destroy';
  class function  TSkiaApi.sk4d_picturerecorder_finish_recording;  external TSkiaApi.LibName name 'sk4d_picturerecorder_finish_recording';
  class function  TSkiaApi.sk4d_picturerecorder_finish_recording2; external TSkiaApi.LibName name 'sk4d_picturerecorder_finish_recording2';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_pixmap.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_pixmap_create          := GetProc('sk4d_pixmap_create');
  sk4d_pixmap_create2         := GetProc('sk4d_pixmap_create2');
  sk4d_pixmap_destroy         := GetProc('sk4d_pixmap_destroy');
  sk4d_pixmap_erase           := GetProc('sk4d_pixmap_erase');
  sk4d_pixmap_erase2          := GetProc('sk4d_pixmap_erase2');
  sk4d_pixmap_extract_subset  := GetProc('sk4d_pixmap_extract_subset');
  sk4d_pixmap_get_alpha       := GetProc('sk4d_pixmap_get_alpha');
  sk4d_pixmap_get_alpha_type  := GetProc('sk4d_pixmap_get_alpha_type');
  sk4d_pixmap_get_color       := GetProc('sk4d_pixmap_get_color');
  sk4d_pixmap_get_color_space := GetProc('sk4d_pixmap_get_color_space');
  sk4d_pixmap_get_color_type  := GetProc('sk4d_pixmap_get_color_type');
  sk4d_pixmap_get_height      := GetProc('sk4d_pixmap_get_height');
  sk4d_pixmap_get_image_info  := GetProc('sk4d_pixmap_get_image_info');
  sk4d_pixmap_get_pixel_addr  := GetProc('sk4d_pixmap_get_pixel_addr');
  sk4d_pixmap_get_pixels      := GetProc('sk4d_pixmap_get_pixels');
  sk4d_pixmap_get_row_bytes   := GetProc('sk4d_pixmap_get_row_bytes');
  sk4d_pixmap_get_width       := GetProc('sk4d_pixmap_get_width');
  sk4d_pixmap_read_pixels     := GetProc('sk4d_pixmap_read_pixels');
  sk4d_pixmap_scale_pixels    := GetProc('sk4d_pixmap_scale_pixels');
  sk4d_pixmap_set_colorspace  := GetProc('sk4d_pixmap_set_colorspace');
  {$ELSE}
  class function  TSkiaApi.sk4d_pixmap_create;          external TSkiaApi.LibName name 'sk4d_pixmap_create';
  class function  TSkiaApi.sk4d_pixmap_create2;         external TSkiaApi.LibName name 'sk4d_pixmap_create2';
  class procedure TSkiaApi.sk4d_pixmap_destroy;         external TSkiaApi.LibName name 'sk4d_pixmap_destroy';
  class function  TSkiaApi.sk4d_pixmap_erase;           external TSkiaApi.LibName name 'sk4d_pixmap_erase';
  class function  TSkiaApi.sk4d_pixmap_erase2;          external TSkiaApi.LibName name 'sk4d_pixmap_erase2';
  class function  TSkiaApi.sk4d_pixmap_extract_subset;  external TSkiaApi.LibName name 'sk4d_pixmap_extract_subset';
  class function  TSkiaApi.sk4d_pixmap_get_alpha;       external TSkiaApi.LibName name 'sk4d_pixmap_get_alpha';
  class function  TSkiaApi.sk4d_pixmap_get_alpha_type;  external TSkiaApi.LibName name 'sk4d_pixmap_get_alpha_type';
  class function  TSkiaApi.sk4d_pixmap_get_color;       external TSkiaApi.LibName name 'sk4d_pixmap_get_color';
  class function  TSkiaApi.sk4d_pixmap_get_color_space; external TSkiaApi.LibName name 'sk4d_pixmap_get_color_space';
  class function  TSkiaApi.sk4d_pixmap_get_color_type;  external TSkiaApi.LibName name 'sk4d_pixmap_get_color_type';
  class function  TSkiaApi.sk4d_pixmap_get_height;      external TSkiaApi.LibName name 'sk4d_pixmap_get_height';
  class procedure TSkiaApi.sk4d_pixmap_get_image_info;  external TSkiaApi.LibName name 'sk4d_pixmap_get_image_info';
  class function  TSkiaApi.sk4d_pixmap_get_pixel_addr;  external TSkiaApi.LibName name 'sk4d_pixmap_get_pixel_addr';
  class function  TSkiaApi.sk4d_pixmap_get_pixels;      external TSkiaApi.LibName name 'sk4d_pixmap_get_pixels';
  class function  TSkiaApi.sk4d_pixmap_get_row_bytes;   external TSkiaApi.LibName name 'sk4d_pixmap_get_row_bytes';
  class function  TSkiaApi.sk4d_pixmap_get_width;       external TSkiaApi.LibName name 'sk4d_pixmap_get_width';
  class function  TSkiaApi.sk4d_pixmap_read_pixels;     external TSkiaApi.LibName name 'sk4d_pixmap_read_pixels';
  class function  TSkiaApi.sk4d_pixmap_scale_pixels;    external TSkiaApi.LibName name 'sk4d_pixmap_scale_pixels';
  class procedure TSkiaApi.sk4d_pixmap_set_colorspace;  external TSkiaApi.LibName name 'sk4d_pixmap_set_colorspace';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_refcnt.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_refcnt_is_unique := GetProc('sk4d_refcnt_is_unique');
  sk4d_refcnt_ref       := GetProc('sk4d_refcnt_ref');
  sk4d_refcnt_unref     := GetProc('sk4d_refcnt_unref');
  {$ELSE}
  class function  TSkiaApi.sk4d_refcnt_is_unique; external TSkiaApi.LibName name 'sk4d_refcnt_is_unique';
  class procedure TSkiaApi.sk4d_refcnt_ref;       external TSkiaApi.LibName name 'sk4d_refcnt_ref';
  class procedure TSkiaApi.sk4d_refcnt_unref;     external TSkiaApi.LibName name 'sk4d_refcnt_unref';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_region.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_region_contains              := GetProc('sk4d_region_contains');
  sk4d_region_contains2             := GetProc('sk4d_region_contains2');
  sk4d_region_contains3             := GetProc('sk4d_region_contains3');
  sk4d_region_create                := GetProc('sk4d_region_create');
  sk4d_region_create2               := GetProc('sk4d_region_create2');
  sk4d_region_destroy               := GetProc('sk4d_region_destroy');
  sk4d_region_get_boundary_path     := GetProc('sk4d_region_get_boundary_path');
  sk4d_region_get_bounds            := GetProc('sk4d_region_get_bounds');
  sk4d_region_intersects            := GetProc('sk4d_region_intersects');
  sk4d_region_intersects2           := GetProc('sk4d_region_intersects2');
  sk4d_region_is_complex            := GetProc('sk4d_region_is_complex');
  sk4d_region_is_empty              := GetProc('sk4d_region_is_empty');
  sk4d_region_is_equal              := GetProc('sk4d_region_is_equal');
  sk4d_region_is_rect               := GetProc('sk4d_region_is_rect');
  sk4d_region_op                    := GetProc('sk4d_region_op');
  sk4d_region_op2                   := GetProc('sk4d_region_op2');
  sk4d_region_quick_contains        := GetProc('sk4d_region_quick_contains');
  sk4d_region_quick_reject          := GetProc('sk4d_region_quick_reject');
  sk4d_region_quick_reject2         := GetProc('sk4d_region_quick_reject2');
  sk4d_region_set_empty             := GetProc('sk4d_region_set_empty');
  sk4d_region_set_path              := GetProc('sk4d_region_set_path');
  sk4d_region_set_rect              := GetProc('sk4d_region_set_rect');
  sk4d_region_set_rects             := GetProc('sk4d_region_set_rects');
  sk4d_region_translate             := GetProc('sk4d_region_translate');
  sk4d_region_translate2            := GetProc('sk4d_region_translate2');
  sk4d_regioncliperator_create      := GetProc('sk4d_regioncliperator_create');
  sk4d_regioncliperator_destroy     := GetProc('sk4d_regioncliperator_destroy');
  sk4d_regioncliperator_get_current := GetProc('sk4d_regioncliperator_get_current');
  sk4d_regioncliperator_move_next   := GetProc('sk4d_regioncliperator_move_next');
  sk4d_regioniterator_create        := GetProc('sk4d_regioniterator_create');
  sk4d_regioniterator_destroy       := GetProc('sk4d_regioniterator_destroy');
  sk4d_regioniterator_get_current   := GetProc('sk4d_regioniterator_get_current');
  sk4d_regioniterator_move_next     := GetProc('sk4d_regioniterator_move_next');
  sk4d_regioniterator_reset         := GetProc('sk4d_regioniterator_reset');
  sk4d_regionspanerator_create      := GetProc('sk4d_regionspanerator_create');
  sk4d_regionspanerator_destroy     := GetProc('sk4d_regionspanerator_destroy');
  sk4d_regionspanerator_next        := GetProc('sk4d_regionspanerator_next');
  {$ELSE}
  class function  TSkiaApi.sk4d_region_contains;              external TSkiaApi.LibName name 'sk4d_region_contains';
  class function  TSkiaApi.sk4d_region_contains2;             external TSkiaApi.LibName name 'sk4d_region_contains2';
  class function  TSkiaApi.sk4d_region_contains3;             external TSkiaApi.LibName name 'sk4d_region_contains3';
  class function  TSkiaApi.sk4d_region_create;                external TSkiaApi.LibName name 'sk4d_region_create';
  class function  TSkiaApi.sk4d_region_create2;               external TSkiaApi.LibName name 'sk4d_region_create2';
  class procedure TSkiaApi.sk4d_region_destroy;               external TSkiaApi.LibName name 'sk4d_region_destroy';
  class function  TSkiaApi.sk4d_region_get_boundary_path;     external TSkiaApi.LibName name 'sk4d_region_get_boundary_path';
  class procedure TSkiaApi.sk4d_region_get_bounds;            external TSkiaApi.LibName name 'sk4d_region_get_bounds';
  class function  TSkiaApi.sk4d_region_intersects;            external TSkiaApi.LibName name 'sk4d_region_intersects';
  class function  TSkiaApi.sk4d_region_intersects2;           external TSkiaApi.LibName name 'sk4d_region_intersects2';
  class function  TSkiaApi.sk4d_region_is_complex;            external TSkiaApi.LibName name 'sk4d_region_is_complex';
  class function  TSkiaApi.sk4d_region_is_empty;              external TSkiaApi.LibName name 'sk4d_region_is_empty';
  class function  TSkiaApi.sk4d_region_is_equal;              external TSkiaApi.LibName name 'sk4d_region_is_equal';
  class function  TSkiaApi.sk4d_region_is_rect;               external TSkiaApi.LibName name 'sk4d_region_is_rect';
  class function  TSkiaApi.sk4d_region_op;                    external TSkiaApi.LibName name 'sk4d_region_op';
  class function  TSkiaApi.sk4d_region_op2;                   external TSkiaApi.LibName name 'sk4d_region_op2';
  class function  TSkiaApi.sk4d_region_quick_contains;        external TSkiaApi.LibName name 'sk4d_region_quick_contains';
  class function  TSkiaApi.sk4d_region_quick_reject;          external TSkiaApi.LibName name 'sk4d_region_quick_reject';
  class function  TSkiaApi.sk4d_region_quick_reject2;         external TSkiaApi.LibName name 'sk4d_region_quick_reject2';
  class procedure TSkiaApi.sk4d_region_set_empty;             external TSkiaApi.LibName name 'sk4d_region_set_empty';
  class function  TSkiaApi.sk4d_region_set_path;              external TSkiaApi.LibName name 'sk4d_region_set_path';
  class function  TSkiaApi.sk4d_region_set_rect;              external TSkiaApi.LibName name 'sk4d_region_set_rect';
  class function  TSkiaApi.sk4d_region_set_rects;             external TSkiaApi.LibName name 'sk4d_region_set_rects';
  class procedure TSkiaApi.sk4d_region_translate;             external TSkiaApi.LibName name 'sk4d_region_translate';
  class procedure TSkiaApi.sk4d_region_translate2;            external TSkiaApi.LibName name 'sk4d_region_translate2';
  class function  TSkiaApi.sk4d_regioncliperator_create;      external TSkiaApi.LibName name 'sk4d_regioncliperator_create';
  class procedure TSkiaApi.sk4d_regioncliperator_destroy;     external TSkiaApi.LibName name 'sk4d_regioncliperator_destroy';
  class procedure TSkiaApi.sk4d_regioncliperator_get_current; external TSkiaApi.LibName name 'sk4d_regioncliperator_get_current';
  class function  TSkiaApi.sk4d_regioncliperator_move_next;   external TSkiaApi.LibName name 'sk4d_regioncliperator_move_next';
  class function  TSkiaApi.sk4d_regioniterator_create;        external TSkiaApi.LibName name 'sk4d_regioniterator_create';
  class procedure TSkiaApi.sk4d_regioniterator_destroy;       external TSkiaApi.LibName name 'sk4d_regioniterator_destroy';
  class procedure TSkiaApi.sk4d_regioniterator_get_current;   external TSkiaApi.LibName name 'sk4d_regioniterator_get_current';
  class function  TSkiaApi.sk4d_regioniterator_move_next;     external TSkiaApi.LibName name 'sk4d_regioniterator_move_next';
  class procedure TSkiaApi.sk4d_regioniterator_reset;         external TSkiaApi.LibName name 'sk4d_regioniterator_reset';
  class function  TSkiaApi.sk4d_regionspanerator_create;      external TSkiaApi.LibName name 'sk4d_regionspanerator_create';
  class procedure TSkiaApi.sk4d_regionspanerator_destroy;     external TSkiaApi.LibName name 'sk4d_regionspanerator_destroy';
  class function  TSkiaApi.sk4d_regionspanerator_next;        external TSkiaApi.LibName name 'sk4d_regionspanerator_next';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_rrect.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_rrect_contains         := GetProc('sk4d_rrect_contains');
  sk4d_rrect_create           := GetProc('sk4d_rrect_create');
  sk4d_rrect_create2          := GetProc('sk4d_rrect_create2');
  sk4d_rrect_deflate          := GetProc('sk4d_rrect_deflate');
  sk4d_rrect_deflate2         := GetProc('sk4d_rrect_deflate2');
  sk4d_rrect_destroy          := GetProc('sk4d_rrect_destroy');
  sk4d_rrect_get_height       := GetProc('sk4d_rrect_get_height');
  sk4d_rrect_get_radii        := GetProc('sk4d_rrect_get_radii');
  sk4d_rrect_get_rect         := GetProc('sk4d_rrect_get_rect');
  sk4d_rrect_get_simple_radii := GetProc('sk4d_rrect_get_simple_radii');
  sk4d_rrect_get_type         := GetProc('sk4d_rrect_get_type');
  sk4d_rrect_get_width        := GetProc('sk4d_rrect_get_width');
  sk4d_rrect_inflate          := GetProc('sk4d_rrect_inflate');
  sk4d_rrect_inflate2         := GetProc('sk4d_rrect_inflate2');
  sk4d_rrect_is_equal         := GetProc('sk4d_rrect_is_equal');
  sk4d_rrect_is_valid         := GetProc('sk4d_rrect_is_valid');
  sk4d_rrect_make_offset      := GetProc('sk4d_rrect_make_offset');
  sk4d_rrect_offset           := GetProc('sk4d_rrect_offset');
  sk4d_rrect_set_empty        := GetProc('sk4d_rrect_set_empty');
  sk4d_rrect_set_nine_patch   := GetProc('sk4d_rrect_set_nine_patch');
  sk4d_rrect_set_oval         := GetProc('sk4d_rrect_set_oval');
  sk4d_rrect_set_rect         := GetProc('sk4d_rrect_set_rect');
  sk4d_rrect_set_rect2        := GetProc('sk4d_rrect_set_rect2');
  sk4d_rrect_set_rect3        := GetProc('sk4d_rrect_set_rect3');
  sk4d_rrect_transform        := GetProc('sk4d_rrect_transform');
  {$ELSE}
  class function  TSkiaApi.sk4d_rrect_contains;         external TSkiaApi.LibName name 'sk4d_rrect_contains';
  class function  TSkiaApi.sk4d_rrect_create;           external TSkiaApi.LibName name 'sk4d_rrect_create';
  class function  TSkiaApi.sk4d_rrect_create2;          external TSkiaApi.LibName name 'sk4d_rrect_create2';
  class procedure TSkiaApi.sk4d_rrect_deflate;          external TSkiaApi.LibName name 'sk4d_rrect_deflate';
  class procedure TSkiaApi.sk4d_rrect_deflate2;         external TSkiaApi.LibName name 'sk4d_rrect_deflate2';
  class procedure TSkiaApi.sk4d_rrect_destroy;          external TSkiaApi.LibName name 'sk4d_rrect_destroy';
  class function  TSkiaApi.sk4d_rrect_get_height;       external TSkiaApi.LibName name 'sk4d_rrect_get_height';
  class procedure TSkiaApi.sk4d_rrect_get_radii;        external TSkiaApi.LibName name 'sk4d_rrect_get_radii';
  class procedure TSkiaApi.sk4d_rrect_get_rect;         external TSkiaApi.LibName name 'sk4d_rrect_get_rect';
  class procedure TSkiaApi.sk4d_rrect_get_simple_radii; external TSkiaApi.LibName name 'sk4d_rrect_get_simple_radii';
  class function  TSkiaApi.sk4d_rrect_get_type;         external TSkiaApi.LibName name 'sk4d_rrect_get_type';
  class function  TSkiaApi.sk4d_rrect_get_width;        external TSkiaApi.LibName name 'sk4d_rrect_get_width';
  class procedure TSkiaApi.sk4d_rrect_inflate;          external TSkiaApi.LibName name 'sk4d_rrect_inflate';
  class procedure TSkiaApi.sk4d_rrect_inflate2;         external TSkiaApi.LibName name 'sk4d_rrect_inflate2';
  class function  TSkiaApi.sk4d_rrect_is_equal;         external TSkiaApi.LibName name 'sk4d_rrect_is_equal';
  class function  TSkiaApi.sk4d_rrect_is_valid;         external TSkiaApi.LibName name 'sk4d_rrect_is_valid';
  class function  TSkiaApi.sk4d_rrect_make_offset;      external TSkiaApi.LibName name 'sk4d_rrect_make_offset';
  class procedure TSkiaApi.sk4d_rrect_offset;           external TSkiaApi.LibName name 'sk4d_rrect_offset';
  class procedure TSkiaApi.sk4d_rrect_set_empty;        external TSkiaApi.LibName name 'sk4d_rrect_set_empty';
  class procedure TSkiaApi.sk4d_rrect_set_nine_patch;   external TSkiaApi.LibName name 'sk4d_rrect_set_nine_patch';
  class procedure TSkiaApi.sk4d_rrect_set_oval;         external TSkiaApi.LibName name 'sk4d_rrect_set_oval';
  class procedure TSkiaApi.sk4d_rrect_set_rect;         external TSkiaApi.LibName name 'sk4d_rrect_set_rect';
  class procedure TSkiaApi.sk4d_rrect_set_rect2;        external TSkiaApi.LibName name 'sk4d_rrect_set_rect2';
  class procedure TSkiaApi.sk4d_rrect_set_rect3;        external TSkiaApi.LibName name 'sk4d_rrect_set_rect3';
  class function  TSkiaApi.sk4d_rrect_transform;        external TSkiaApi.LibName name 'sk4d_rrect_transform';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_runtimeeffect.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_runtimeeffect_find_uniform         := GetProc('sk4d_runtimeeffect_find_uniform');
  sk4d_runtimeeffect_get_children         := GetProc('sk4d_runtimeeffect_get_children');
  sk4d_runtimeeffect_get_children_count   := GetProc('sk4d_runtimeeffect_get_children_count');
  sk4d_runtimeeffect_get_uniform          := GetProc('sk4d_runtimeeffect_get_uniform');
  sk4d_runtimeeffect_get_uniform_count    := GetProc('sk4d_runtimeeffect_get_uniform_count');
  sk4d_runtimeeffect_get_uniform_size     := GetProc('sk4d_runtimeeffect_get_uniform_size');
  sk4d_runtimeeffect_make                 := GetProc('sk4d_runtimeeffect_make');
  sk4d_runtimeeffect_make_color_filter    := GetProc('sk4d_runtimeeffect_make_color_filter');
  sk4d_runtimeeffect_make_shader          := GetProc('sk4d_runtimeeffect_make_shader');
  sk4d_runtimeeffectuniform_get_byte_size := GetProc('sk4d_runtimeeffectuniform_get_byte_size');
  sk4d_runtimeeffectuniform_get_name      := GetProc('sk4d_runtimeeffectuniform_get_name');
  sk4d_runtimeeffectuniform_get_offset    := GetProc('sk4d_runtimeeffectuniform_get_offset');
  {$ELSE}
  class function TSkiaApi.sk4d_runtimeeffect_find_uniform;         external TSkiaApi.LibName name 'sk4d_runtimeeffect_find_uniform';
  class function TSkiaApi.sk4d_runtimeeffect_get_children;         external TSkiaApi.LibName name 'sk4d_runtimeeffect_get_children';
  class function TSkiaApi.sk4d_runtimeeffect_get_children_count;   external TSkiaApi.LibName name 'sk4d_runtimeeffect_get_children_count';
  class function TSkiaApi.sk4d_runtimeeffect_get_uniform;          external TSkiaApi.LibName name 'sk4d_runtimeeffect_get_uniform';
  class function TSkiaApi.sk4d_runtimeeffect_get_uniform_count;    external TSkiaApi.LibName name 'sk4d_runtimeeffect_get_uniform_count';
  class function TSkiaApi.sk4d_runtimeeffect_get_uniform_size;     external TSkiaApi.LibName name 'sk4d_runtimeeffect_get_uniform_size';
  class function TSkiaApi.sk4d_runtimeeffect_make;                 external TSkiaApi.LibName name 'sk4d_runtimeeffect_make';
  class function TSkiaApi.sk4d_runtimeeffect_make_color_filter;    external TSkiaApi.LibName name 'sk4d_runtimeeffect_make_color_filter';
  class function TSkiaApi.sk4d_runtimeeffect_make_shader;          external TSkiaApi.LibName name 'sk4d_runtimeeffect_make_shader';
  class function TSkiaApi.sk4d_runtimeeffectuniform_get_byte_size; external TSkiaApi.LibName name 'sk4d_runtimeeffectuniform_get_byte_size';
  class function TSkiaApi.sk4d_runtimeeffectuniform_get_name;      external TSkiaApi.LibName name 'sk4d_runtimeeffectuniform_get_name';
  class function TSkiaApi.sk4d_runtimeeffectuniform_get_offset;    external TSkiaApi.LibName name 'sk4d_runtimeeffectuniform_get_offset';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_shader.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_shader_make_blend                      := GetProc('sk4d_shader_make_blend');
  sk4d_shader_make_color                      := GetProc('sk4d_shader_make_color');
  sk4d_shader_make_color2                     := GetProc('sk4d_shader_make_color2');
  sk4d_shader_make_empty                      := GetProc('sk4d_shader_make_empty');
  sk4d_shader_make_gradient_linear            := GetProc('sk4d_shader_make_gradient_linear');
  sk4d_shader_make_gradient_linear2           := GetProc('sk4d_shader_make_gradient_linear2');
  sk4d_shader_make_gradient_radial            := GetProc('sk4d_shader_make_gradient_radial');
  sk4d_shader_make_gradient_radial2           := GetProc('sk4d_shader_make_gradient_radial2');
  sk4d_shader_make_gradient_sweep             := GetProc('sk4d_shader_make_gradient_sweep');
  sk4d_shader_make_gradient_sweep2            := GetProc('sk4d_shader_make_gradient_sweep2');
  sk4d_shader_make_perlin_noise_fractal_noise := GetProc('sk4d_shader_make_perlin_noise_fractal_noise');
  sk4d_shader_make_perlin_noise_turbulence    := GetProc('sk4d_shader_make_perlin_noise_turbulence');
  sk4d_shader_make_with_color_filter          := GetProc('sk4d_shader_make_with_color_filter');
  sk4d_shader_make_with_local_matrix          := GetProc('sk4d_shader_make_with_local_matrix');
  {$ELSE}
  class function TSkiaApi.sk4d_shader_make_blend;                      external TSkiaApi.LibName name 'sk4d_shader_make_blend';
  class function TSkiaApi.sk4d_shader_make_color;                      external TSkiaApi.LibName name 'sk4d_shader_make_color';
  class function TSkiaApi.sk4d_shader_make_color2;                     external TSkiaApi.LibName name 'sk4d_shader_make_color2';
  class function TSkiaApi.sk4d_shader_make_empty;                      external TSkiaApi.LibName name 'sk4d_shader_make_empty';
  class function TSkiaApi.sk4d_shader_make_gradient_linear;            external TSkiaApi.LibName name 'sk4d_shader_make_gradient_linear';
  class function TSkiaApi.sk4d_shader_make_gradient_linear2;           external TSkiaApi.LibName name 'sk4d_shader_make_gradient_linear2';
  class function TSkiaApi.sk4d_shader_make_gradient_radial;            external TSkiaApi.LibName name 'sk4d_shader_make_gradient_radial';
  class function TSkiaApi.sk4d_shader_make_gradient_radial2;           external TSkiaApi.LibName name 'sk4d_shader_make_gradient_radial2';
  class function TSkiaApi.sk4d_shader_make_gradient_sweep;             external TSkiaApi.LibName name 'sk4d_shader_make_gradient_sweep';
  class function TSkiaApi.sk4d_shader_make_gradient_sweep2;            external TSkiaApi.LibName name 'sk4d_shader_make_gradient_sweep2';
  class function TSkiaApi.sk4d_shader_make_perlin_noise_fractal_noise; external TSkiaApi.LibName name 'sk4d_shader_make_perlin_noise_fractal_noise';
  class function TSkiaApi.sk4d_shader_make_perlin_noise_turbulence;    external TSkiaApi.LibName name 'sk4d_shader_make_perlin_noise_turbulence';
  class function TSkiaApi.sk4d_shader_make_with_color_filter;          external TSkiaApi.LibName name 'sk4d_shader_make_with_color_filter';
  class function TSkiaApi.sk4d_shader_make_with_local_matrix;          external TSkiaApi.LibName name 'sk4d_shader_make_with_local_matrix';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_stream.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_managedstream_create     := GetProc('sk4d_managedstream_create');
  sk4d_managedstream_set_procs  := GetProc('sk4d_managedstream_set_procs');
  sk4d_managedwstream_create    := GetProc('sk4d_managedwstream_create');
  sk4d_managedwstream_set_procs := GetProc('sk4d_managedwstream_set_procs');
  sk4d_stream_destroy           := GetProc('sk4d_stream_destroy');
  sk4d_wstream_destroy          := GetProc('sk4d_wstream_destroy');
  {$ELSE}
  class function  TSkiaApi.sk4d_managedstream_create;     external TSkiaApi.LibName name 'sk4d_managedstream_create';
  class procedure TSkiaApi.sk4d_managedstream_set_procs;  external TSkiaApi.LibName name 'sk4d_managedstream_set_procs';
  class function  TSkiaApi.sk4d_managedwstream_create;    external TSkiaApi.LibName name 'sk4d_managedwstream_create';
  class procedure TSkiaApi.sk4d_managedwstream_set_procs; external TSkiaApi.LibName name 'sk4d_managedwstream_set_procs';
  class procedure TSkiaApi.sk4d_stream_destroy;           external TSkiaApi.LibName name 'sk4d_stream_destroy';
  class procedure TSkiaApi.sk4d_wstream_destroy;          external TSkiaApi.LibName name 'sk4d_wstream_destroy';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_string.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_string_create   := GetProc('sk4d_string_create');
  sk4d_string_destroy  := GetProc('sk4d_string_destroy');
  sk4d_string_get_text := GetProc('sk4d_string_get_text');
  sk4d_string_set_text := GetProc('sk4d_string_set_text');
  {$ELSE}
  class function  TSkiaApi.sk4d_string_create;   external TSkiaApi.LibName name 'sk4d_string_create';
  class procedure TSkiaApi.sk4d_string_destroy;  external TSkiaApi.LibName name 'sk4d_string_destroy';
  class function  TSkiaApi.sk4d_string_get_text; external TSkiaApi.LibName name 'sk4d_string_get_text';
  class procedure TSkiaApi.sk4d_string_set_text; external TSkiaApi.LibName name 'sk4d_string_set_text';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_surface.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_surface_draw                     := GetProc('sk4d_surface_draw');
  sk4d_surface_flush                    := GetProc('sk4d_surface_flush');
  sk4d_surface_flush_and_submit         := GetProc('sk4d_surface_flush_and_submit');
  sk4d_surface_get_canvas               := GetProc('sk4d_surface_get_canvas');
  sk4d_surface_get_height               := GetProc('sk4d_surface_get_height');
  sk4d_surface_get_image_info           := GetProc('sk4d_surface_get_image_info');
  sk4d_surface_get_props                := GetProc('sk4d_surface_get_props');
  sk4d_surface_get_recording_context    := GetProc('sk4d_surface_get_recording_context');
  sk4d_surface_get_width                := GetProc('sk4d_surface_get_width');
  sk4d_surface_make_from_ca_metal_layer := GetProc('sk4d_surface_make_from_ca_metal_layer');
  sk4d_surface_make_from_mtk_view       := GetProc('sk4d_surface_make_from_mtk_view');
  sk4d_surface_make_from_rendertarget   := GetProc('sk4d_surface_make_from_rendertarget');
  sk4d_surface_make_from_texture        := GetProc('sk4d_surface_make_from_texture');
  sk4d_surface_make_image_snapshot      := GetProc('sk4d_surface_make_image_snapshot');
  sk4d_surface_make_image_snapshot2     := GetProc('sk4d_surface_make_image_snapshot2');
  sk4d_surface_make_null                := GetProc('sk4d_surface_make_null');
  sk4d_surface_make_raster              := GetProc('sk4d_surface_make_raster');
  sk4d_surface_make_raster_direct       := GetProc('sk4d_surface_make_raster_direct');
  sk4d_surface_make_render_target       := GetProc('sk4d_surface_make_render_target');
  sk4d_surface_peek_pixels              := GetProc('sk4d_surface_peek_pixels');
  sk4d_surface_read_pixels              := GetProc('sk4d_surface_read_pixels');
  sk4d_surface_write_pixels             := GetProc('sk4d_surface_write_pixels');
  sk4d_sk_surfaceprops_create           := GetProc('sk4d_sk_surfaceprops_create');
  sk4d_surfaceprops_destroy             := GetProc('sk4d_surfaceprops_destroy');
  sk4d_surfaceprops_get_flags           := GetProc('sk4d_surfaceprops_get_flags');
  sk4d_surfaceprops_get_pixel_geometry  := GetProc('sk4d_surfaceprops_get_pixel_geometry');
  sk4d_surfaceprops_is_equal            := GetProc('sk4d_surfaceprops_is_equal');
  {$ELSE}
  class procedure TSkiaApi.sk4d_surface_draw;                     external TSkiaApi.LibName name 'sk4d_surface_draw';
  class procedure TSkiaApi.sk4d_surface_flush;                    external TSkiaApi.LibName name 'sk4d_surface_flush';
  class procedure TSkiaApi.sk4d_surface_flush_and_submit;         external TSkiaApi.LibName name 'sk4d_surface_flush_and_submit';
  class function  TSkiaApi.sk4d_surface_get_canvas;               external TSkiaApi.LibName name 'sk4d_surface_get_canvas';
  class function  TSkiaApi.sk4d_surface_get_height;               external TSkiaApi.LibName name 'sk4d_surface_get_height';
  class procedure TSkiaApi.sk4d_surface_get_image_info;           external TSkiaApi.LibName name 'sk4d_surface_get_image_info';
  class function  TSkiaApi.sk4d_surface_get_props;                external TSkiaApi.LibName name 'sk4d_surface_get_props';
  class function  TSkiaApi.sk4d_surface_get_recording_context;    external TSkiaApi.LibName name 'sk4d_surface_get_recording_context';
  class function  TSkiaApi.sk4d_surface_get_width;                external TSkiaApi.LibName name 'sk4d_surface_get_width';
  class function  TSkiaApi.sk4d_surface_make_from_ca_metal_layer; external TSkiaApi.LibName name 'sk4d_surface_make_from_ca_metal_layer';
  class function  TSkiaApi.sk4d_surface_make_from_mtk_view;       external TSkiaApi.LibName name 'sk4d_surface_make_from_mtk_view';
  class function  TSkiaApi.sk4d_surface_make_from_rendertarget;   external TSkiaApi.LibName name 'sk4d_surface_make_from_rendertarget';
  class function  TSkiaApi.sk4d_surface_make_from_texture;        external TSkiaApi.LibName name 'sk4d_surface_make_from_texture';
  class function  TSkiaApi.sk4d_surface_make_image_snapshot;      external TSkiaApi.LibName name 'sk4d_surface_make_image_snapshot';
  class function  TSkiaApi.sk4d_surface_make_image_snapshot2;     external TSkiaApi.LibName name 'sk4d_surface_make_image_snapshot2';
  class function  TSkiaApi.sk4d_surface_make_null;                external TSkiaApi.LibName name 'sk4d_surface_make_null';
  class function  TSkiaApi.sk4d_surface_make_raster;              external TSkiaApi.LibName name 'sk4d_surface_make_raster';
  class function  TSkiaApi.sk4d_surface_make_raster_direct;       external TSkiaApi.LibName name 'sk4d_surface_make_raster_direct';
  class function  TSkiaApi.sk4d_surface_make_render_target;       external TSkiaApi.LibName name 'sk4d_surface_make_render_target';
  class function  TSkiaApi.sk4d_surface_peek_pixels;              external TSkiaApi.LibName name 'sk4d_surface_peek_pixels';
  class function  TSkiaApi.sk4d_surface_read_pixels;              external TSkiaApi.LibName name 'sk4d_surface_read_pixels';
  class procedure TSkiaApi.sk4d_surface_write_pixels;             external TSkiaApi.LibName name 'sk4d_surface_write_pixels';
  class function  TSkiaApi.sk4d_sk_surfaceprops_create;           external TSkiaApi.LibName name 'sk4d_sk_surfaceprops_create';
  class procedure TSkiaApi.sk4d_surfaceprops_destroy;             external TSkiaApi.LibName name 'sk4d_surfaceprops_destroy';
  class function  TSkiaApi.sk4d_surfaceprops_get_flags;           external TSkiaApi.LibName name 'sk4d_surfaceprops_get_flags';
  class function  TSkiaApi.sk4d_surfaceprops_get_pixel_geometry;  external TSkiaApi.LibName name 'sk4d_surfaceprops_get_pixel_geometry';
  class function  TSkiaApi.sk4d_surfaceprops_is_equal;            external TSkiaApi.LibName name 'sk4d_surfaceprops_is_equal';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_textblob.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_textblob_get_intercepts                  := GetProc('sk4d_textblob_get_intercepts');
  sk4d_textblob_is_unique                       := GetProc('sk4d_textblob_is_unique');
  sk4d_textblob_ref                             := GetProc('sk4d_textblob_ref');
  sk4d_textblob_unref                           := GetProc('sk4d_textblob_unref');
  sk4d_textblobbuilder_create                   := GetProc('sk4d_textblobbuilder_create');
  sk4d_textblobbuilder_destroy                  := GetProc('sk4d_textblobbuilder_destroy');
  sk4d_textblobbuilder_alloc_horizontal_run     := GetProc('sk4d_textblobbuilder_alloc_horizontal_run');
  sk4d_textblobbuilder_alloc_positioned_run     := GetProc('sk4d_textblobbuilder_alloc_positioned_run');
  sk4d_textblobbuilder_alloc_rotation_scale_run := GetProc('sk4d_textblobbuilder_alloc_rotation_scale_run');
  sk4d_textblobbuilder_alloc_run                := GetProc('sk4d_textblobbuilder_alloc_run');
  sk4d_textblobbuilder_detach                   := GetProc('sk4d_textblobbuilder_detach');
  {$ELSE}
  class function  TSkiaApi.sk4d_textblob_get_intercepts;                  external TSkiaApi.LibName name 'sk4d_textblob_get_intercepts';
  class function  TSkiaApi.sk4d_textblob_is_unique;                       external TSkiaApi.LibName name 'sk4d_textblob_is_unique';
  class procedure TSkiaApi.sk4d_textblob_ref;                             external TSkiaApi.LibName name 'sk4d_textblob_ref';
  class procedure TSkiaApi.sk4d_textblob_unref;                           external TSkiaApi.LibName name 'sk4d_textblob_unref';
  class function  TSkiaApi.sk4d_textblobbuilder_create;                   external TSkiaApi.LibName name 'sk4d_textblobbuilder_create';
  class procedure TSkiaApi.sk4d_textblobbuilder_destroy;                  external TSkiaApi.LibName name 'sk4d_textblobbuilder_destroy';
  class procedure TSkiaApi.sk4d_textblobbuilder_alloc_horizontal_run;     external TSkiaApi.LibName name 'sk4d_textblobbuilder_alloc_horizontal_run';
  class procedure TSkiaApi.sk4d_textblobbuilder_alloc_positioned_run;     external TSkiaApi.LibName name 'sk4d_textblobbuilder_alloc_positioned_run';
  class procedure TSkiaApi.sk4d_textblobbuilder_alloc_rotation_scale_run; external TSkiaApi.LibName name 'sk4d_textblobbuilder_alloc_rotation_scale_run';
  class procedure TSkiaApi.sk4d_textblobbuilder_alloc_run;                external TSkiaApi.LibName name 'sk4d_textblobbuilder_alloc_run';
  class function  TSkiaApi.sk4d_textblobbuilder_detach;                   external TSkiaApi.LibName name 'sk4d_textblobbuilder_detach';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_tracememorydump.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_tracememorydumpbaseclass_create    := GetProc('sk4d_tracememorydumpbaseclass_create');
  sk4d_tracememorydumpbaseclass_destroy   := GetProc('sk4d_tracememorydumpbaseclass_destroy');
  sk4d_tracememorydumpbaseclass_set_procs := GetProc('sk4d_tracememorydumpbaseclass_set_procs');
  {$ELSE}
  class function  TSkiaApi.sk4d_tracememorydumpbaseclass_create;    external TSkiaApi.LibName name 'sk4d_tracememorydumpbaseclass_create';
  class procedure TSkiaApi.sk4d_tracememorydumpbaseclass_destroy;   external TSkiaApi.LibName name 'sk4d_tracememorydumpbaseclass_destroy';
  class procedure TSkiaApi.sk4d_tracememorydumpbaseclass_set_procs; external TSkiaApi.LibName name 'sk4d_tracememorydumpbaseclass_set_procs';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_typeface.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_typeface_get_family_name := GetProc('sk4d_typeface_get_family_name');
  sk4d_typeface_get_slant       := GetProc('sk4d_typeface_get_slant');
  sk4d_typeface_get_style       := GetProc('sk4d_typeface_get_style');
  sk4d_typeface_get_unique_id   := GetProc('sk4d_typeface_get_unique_id');
  sk4d_typeface_get_weight      := GetProc('sk4d_typeface_get_weight');
  sk4d_typeface_get_width       := GetProc('sk4d_typeface_get_width');
  sk4d_typeface_make_default    := GetProc('sk4d_typeface_make_default');
  sk4d_typeface_make_from_data  := GetProc('sk4d_typeface_make_from_data');
  sk4d_typeface_make_from_name  := GetProc('sk4d_typeface_make_from_name');
  {$ELSE}
  class procedure TSkiaApi.sk4d_typeface_get_family_name; external TSkiaApi.LibName name 'sk4d_typeface_get_family_name';
  class function  TSkiaApi.sk4d_typeface_get_slant;       external TSkiaApi.LibName name 'sk4d_typeface_get_slant';
  class procedure TSkiaApi.sk4d_typeface_get_style;       external TSkiaApi.LibName name 'sk4d_typeface_get_style';
  class function  TSkiaApi.sk4d_typeface_get_unique_id;   external TSkiaApi.LibName name 'sk4d_typeface_get_unique_id';
  class function  TSkiaApi.sk4d_typeface_get_weight;      external TSkiaApi.LibName name 'sk4d_typeface_get_weight';
  class function  TSkiaApi.sk4d_typeface_get_width;       external TSkiaApi.LibName name 'sk4d_typeface_get_width';
  class function  TSkiaApi.sk4d_typeface_make_default;    external TSkiaApi.LibName name 'sk4d_typeface_make_default';
  class function  TSkiaApi.sk4d_typeface_make_from_data;  external TSkiaApi.LibName name 'sk4d_typeface_make_from_data';
  class function  TSkiaApi.sk4d_typeface_make_from_name;  external TSkiaApi.LibName name 'sk4d_typeface_make_from_name';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_version.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_version_get_major     := GetProc('sk4d_version_get_major');
  sk4d_version_get_milestone := GetProc('sk4d_version_get_milestone');
  sk4d_version_get_minor     := GetProc('sk4d_version_get_minor');
  {$ELSE}
  class function TSkiaApi.sk4d_version_get_major;     external TSkiaApi.LibName name 'sk4d_version_get_major';
  class function TSkiaApi.sk4d_version_get_milestone; external TSkiaApi.LibName name 'sk4d_version_get_milestone';
  class function TSkiaApi.sk4d_version_get_minor;     external TSkiaApi.LibName name 'sk4d_version_get_minor';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'include/c/sk4d_vertices.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_vertices_get_unique_id := GetProc('sk4d_vertices_get_unique_id');
  sk4d_vertices_is_unique     := GetProc('sk4d_vertices_is_unique');
  sk4d_vertices_make_copy     := GetProc('sk4d_vertices_make_copy');
  sk4d_vertices_ref           := GetProc('sk4d_vertices_ref');
  sk4d_vertices_unref         := GetProc('sk4d_vertices_unref');
  {$ELSE}
  class function  TSkiaApi.sk4d_vertices_get_unique_id; external TSkiaApi.LibName name 'sk4d_vertices_get_unique_id';
  class function  TSkiaApi.sk4d_vertices_is_unique;     external TSkiaApi.LibName name 'sk4d_vertices_is_unique';
  class function  TSkiaApi.sk4d_vertices_make_copy;     external TSkiaApi.LibName name 'sk4d_vertices_make_copy';
  class procedure TSkiaApi.sk4d_vertices_ref;           external TSkiaApi.LibName name 'sk4d_vertices_ref';
  class procedure TSkiaApi.sk4d_vertices_unref;         external TSkiaApi.LibName name 'sk4d_vertices_unref';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'modules/skottie/include/sk4d_skottie.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_skottieanimation_get_duration     := GetProc('sk4d_skottieanimation_get_duration');
  sk4d_skottieanimation_get_fps          := GetProc('sk4d_skottieanimation_get_fps');
  sk4d_skottieanimation_get_in_point     := GetProc('sk4d_skottieanimation_get_in_point');
  sk4d_skottieanimation_get_out_point    := GetProc('sk4d_skottieanimation_get_out_point');
  sk4d_skottieanimation_get_size         := GetProc('sk4d_skottieanimation_get_size');
  sk4d_skottieanimation_get_version      := GetProc('sk4d_skottieanimation_get_version');
  sk4d_skottieanimation_is_unique        := GetProc('sk4d_skottieanimation_is_unique');
  sk4d_skottieanimation_make             := GetProc('sk4d_skottieanimation_make');
  sk4d_skottieanimation_make_from_stream := GetProc('sk4d_skottieanimation_make_from_stream');
  sk4d_skottieanimation_ref              := GetProc('sk4d_skottieanimation_ref');
  sk4d_skottieanimation_render           := GetProc('sk4d_skottieanimation_render');
  sk4d_skottieanimation_seek_frame       := GetProc('sk4d_skottieanimation_seek_frame');
  sk4d_skottieanimation_seek_frame_time  := GetProc('sk4d_skottieanimation_seek_frame_time');
  sk4d_skottieanimation_unref            := GetProc('sk4d_skottieanimation_unref');
  {$ELSE}
  class function  TSkiaApi.sk4d_skottieanimation_get_duration;     external TSkiaApi.LibName name 'sk4d_skottieanimation_get_duration';
  class function  TSkiaApi.sk4d_skottieanimation_get_fps;          external TSkiaApi.LibName name 'sk4d_skottieanimation_get_fps';
  class function  TSkiaApi.sk4d_skottieanimation_get_in_point;     external TSkiaApi.LibName name 'sk4d_skottieanimation_get_in_point';
  class function  TSkiaApi.sk4d_skottieanimation_get_out_point;    external TSkiaApi.LibName name 'sk4d_skottieanimation_get_out_point';
  class procedure TSkiaApi.sk4d_skottieanimation_get_size;         external TSkiaApi.LibName name 'sk4d_skottieanimation_get_size';
  class function  TSkiaApi.sk4d_skottieanimation_get_version;      external TSkiaApi.LibName name 'sk4d_skottieanimation_get_version';
  class function  TSkiaApi.sk4d_skottieanimation_is_unique;        external TSkiaApi.LibName name 'sk4d_skottieanimation_is_unique';
  class function  TSkiaApi.sk4d_skottieanimation_make;             external TSkiaApi.LibName name 'sk4d_skottieanimation_make';
  class function  TSkiaApi.sk4d_skottieanimation_make_from_stream; external TSkiaApi.LibName name 'sk4d_skottieanimation_make_from_stream';
  class procedure TSkiaApi.sk4d_skottieanimation_ref;              external TSkiaApi.LibName name 'sk4d_skottieanimation_ref';
  class procedure TSkiaApi.sk4d_skottieanimation_render;           external TSkiaApi.LibName name 'sk4d_skottieanimation_render';
  class procedure TSkiaApi.sk4d_skottieanimation_seek_frame;       external TSkiaApi.LibName name 'sk4d_skottieanimation_seek_frame';
  class procedure TSkiaApi.sk4d_skottieanimation_seek_frame_time;  external TSkiaApi.LibName name 'sk4d_skottieanimation_seek_frame_time';
  class procedure TSkiaApi.sk4d_skottieanimation_unref;            external TSkiaApi.LibName name 'sk4d_skottieanimation_unref';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'modules/skshaper/include/sk4d_shaper.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_shaper_create                           := GetProc('sk4d_shaper_create');
  sk4d_shaper_destroy                          := GetProc('sk4d_shaper_destroy');
  sk4d_shaper_shape                            := GetProc('sk4d_shaper_shape');
  sk4d_shaperrunhandler_destroy                := GetProc('sk4d_shaperrunhandler_destroy');
  sk4d_shaperrunhandlerbaseclass_create        := GetProc('sk4d_shaperrunhandlerbaseclass_create');
  sk4d_shaperrunhandlerbaseclass_set_procs     := GetProc('sk4d_shaperrunhandlerbaseclass_set_procs');
  sk4d_textblobbuilderrunhandler_create        := GetProc('sk4d_textblobbuilderrunhandler_create');
  sk4d_textblobbuilderrunhandler_detach        := GetProc('sk4d_textblobbuilderrunhandler_detach');
  sk4d_textblobbuilderrunhandler_get_end_point := GetProc('sk4d_textblobbuilderrunhandler_get_end_point');
  {$ELSE}
  class function  TSkiaApi.sk4d_shaper_create;                           external TSkiaApi.LibName name 'sk4d_shaper_create';
  class procedure TSkiaApi.sk4d_shaper_destroy;                          external TSkiaApi.LibName name 'sk4d_shaper_destroy';
  class procedure TSkiaApi.sk4d_shaper_shape;                            external TSkiaApi.LibName name 'sk4d_shaper_shape';
  class procedure TSkiaApi.sk4d_shaperrunhandler_destroy;                external TSkiaApi.LibName name 'sk4d_shaperrunhandler_destroy';
  class function  TSkiaApi.sk4d_shaperrunhandlerbaseclass_create;        external TSkiaApi.LibName name 'sk4d_shaperrunhandlerbaseclass_create';
  class procedure TSkiaApi.sk4d_shaperrunhandlerbaseclass_set_procs;     external TSkiaApi.LibName name 'sk4d_shaperrunhandlerbaseclass_set_procs';
  class function  TSkiaApi.sk4d_textblobbuilderrunhandler_create;        external TSkiaApi.LibName name 'sk4d_textblobbuilderrunhandler_create';
  class function  TSkiaApi.sk4d_textblobbuilderrunhandler_detach;        external TSkiaApi.LibName name 'sk4d_textblobbuilderrunhandler_detach';
  class procedure TSkiaApi.sk4d_textblobbuilderrunhandler_get_end_point; external TSkiaApi.LibName name 'sk4d_textblobbuilderrunhandler_get_end_point';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'modules/svg/include/sk4d_svgdom.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_svgdom_get_container_size := GetProc('sk4d_svgdom_get_container_size');
  sk4d_svgdom_make               := GetProc('sk4d_svgdom_make');
  sk4d_svgdom_render             := GetProc('sk4d_svgdom_render');
  sk4d_svgdom_set_container_size := GetProc('sk4d_svgdom_set_container_size');
  {$ELSE}
  class procedure TSkiaApi.sk4d_svgdom_get_container_size; external TSkiaApi.LibName name 'sk4d_svgdom_get_container_size';
  class function  TSkiaApi.sk4d_svgdom_make;               external TSkiaApi.LibName name 'sk4d_svgdom_make';
  class procedure TSkiaApi.sk4d_svgdom_render;             external TSkiaApi.LibName name 'sk4d_svgdom_render';
  class procedure TSkiaApi.sk4d_svgdom_set_container_size; external TSkiaApi.LibName name 'sk4d_svgdom_set_container_size';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'modules/skparagraph/include/sk4d_fontcollection.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_fontcollection_create              := GetProc('sk4d_fontcollection_create');
  sk4d_fontcollection_get_font_fall_back  := GetProc('sk4d_fontcollection_get_font_fall_back');
  sk4d_fontcollection_get_paragraph_cache := GetProc('sk4d_fontcollection_get_paragraph_cache');
  sk4d_fontcollection_set_font_fall_back  := GetProc('sk4d_fontcollection_set_font_fall_back');
  {$ELSE}
  class function  TSkiaApi.sk4d_fontcollection_create;              external TSkiaApi.LibName name 'sk4d_fontcollection_create';
  class function  TSkiaApi.sk4d_fontcollection_get_font_fall_back;  external TSkiaApi.LibName name 'sk4d_fontcollection_get_font_fall_back';
  class function  TSkiaApi.sk4d_fontcollection_get_paragraph_cache; external TSkiaApi.LibName name 'sk4d_fontcollection_get_paragraph_cache';
  class procedure TSkiaApi.sk4d_fontcollection_set_font_fall_back;  external TSkiaApi.LibName name 'sk4d_fontcollection_set_font_fall_back';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'modules/skparagraph/include/sk4d_paragraphcache.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_paragraphcache_reset   := GetProc('sk4d_paragraphcache_reset');
  sk4d_paragraphcache_turn_on := GetProc('sk4d_paragraphcache_turn_on');
  {$ELSE}
  class procedure TSkiaApi.sk4d_paragraphcache_reset;   external TSkiaApi.LibName name 'sk4d_paragraphcache_reset';
  class procedure TSkiaApi.sk4d_paragraphcache_turn_on; external TSkiaApi.LibName name 'sk4d_paragraphcache_turn_on';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'modules/skparagraph/include/sk4d_textstyle.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_textstyle_add_font_feature                    := GetProc('sk4d_textstyle_add_font_feature');
  sk4d_textstyle_add_shadow                          := GetProc('sk4d_textstyle_add_shadow');
  sk4d_textstyle_clear_background_color              := GetProc('sk4d_textstyle_clear_background_color');
  sk4d_textstyle_clear_foreground_color              := GetProc('sk4d_textstyle_clear_foreground_color');
  sk4d_textstyle_create                              := GetProc('sk4d_textstyle_create');
  sk4d_textstyle_destroy                             := GetProc('sk4d_textstyle_destroy');
  sk4d_textstyle_get_background                      := GetProc('sk4d_textstyle_get_background');
  sk4d_textstyle_get_color                           := GetProc('sk4d_textstyle_get_color');
  sk4d_textstyle_get_decoration_color                := GetProc('sk4d_textstyle_get_decoration_color');
  sk4d_textstyle_get_decoration_mode                 := GetProc('sk4d_textstyle_get_decoration_mode');
  sk4d_textstyle_get_decoration_style                := GetProc('sk4d_textstyle_get_decoration_style');
  sk4d_textstyle_get_decoration_thickness_multiplier := GetProc('sk4d_textstyle_get_decoration_thickness_multiplier');
  sk4d_textstyle_get_decorations                     := GetProc('sk4d_textstyle_get_decorations');
  sk4d_textstyle_get_font_families                   := GetProc('sk4d_textstyle_get_font_families');
  sk4d_textstyle_get_font_metrics                    := GetProc('sk4d_textstyle_get_font_metrics');
  sk4d_textstyle_get_font_size                       := GetProc('sk4d_textstyle_get_font_size');
  sk4d_textstyle_get_font_style                      := GetProc('sk4d_textstyle_get_font_style');
  sk4d_textstyle_get_foreground                      := GetProc('sk4d_textstyle_get_foreground');
  sk4d_textstyle_get_height                          := GetProc('sk4d_textstyle_get_height');
  sk4d_textstyle_get_height_override                 := GetProc('sk4d_textstyle_get_height_override');
  sk4d_textstyle_get_letter_spacing                  := GetProc('sk4d_textstyle_get_letter_spacing');
  sk4d_textstyle_get_locale                          := GetProc('sk4d_textstyle_get_locale');
  sk4d_textstyle_get_text_baseline                   := GetProc('sk4d_textstyle_get_text_baseline');
  sk4d_textstyle_get_typeface                        := GetProc('sk4d_textstyle_get_typeface');
  sk4d_textstyle_get_word_spacing                    := GetProc('sk4d_textstyle_get_word_spacing');
  sk4d_textstyle_has_background                      := GetProc('sk4d_textstyle_has_background');
  sk4d_textstyle_has_foreground                      := GetProc('sk4d_textstyle_has_foreground');
  sk4d_textstyle_is_equal                            := GetProc('sk4d_textstyle_is_equal');
  sk4d_textstyle_is_placeholder                      := GetProc('sk4d_textstyle_is_placeholder');
  sk4d_textstyle_reset_font_features                 := GetProc('sk4d_textstyle_reset_font_features');
  sk4d_textstyle_reset_shadows                       := GetProc('sk4d_textstyle_reset_shadows');
  sk4d_textstyle_set_background_color                := GetProc('sk4d_textstyle_set_background_color');
  sk4d_textstyle_set_color                           := GetProc('sk4d_textstyle_set_color');
  sk4d_textstyle_set_decoration_color                := GetProc('sk4d_textstyle_set_decoration_color');
  sk4d_textstyle_set_decoration_mode                 := GetProc('sk4d_textstyle_set_decoration_mode');
  sk4d_textstyle_set_decoration_style                := GetProc('sk4d_textstyle_set_decoration_style');
  sk4d_textstyle_set_decoration_thickness_multiplier := GetProc('sk4d_textstyle_set_decoration_thickness_multiplier');
  sk4d_textstyle_set_decorations                     := GetProc('sk4d_textstyle_set_decorations');
  sk4d_textstyle_set_font_families                   := GetProc('sk4d_textstyle_set_font_families');
  sk4d_textstyle_set_font_size                       := GetProc('sk4d_textstyle_set_font_size');
  sk4d_textstyle_set_font_style                      := GetProc('sk4d_textstyle_set_font_style');
  sk4d_textstyle_set_foreground_color                := GetProc('sk4d_textstyle_set_foreground_color');
  sk4d_textstyle_set_height                          := GetProc('sk4d_textstyle_set_height');
  sk4d_textstyle_set_height_override                 := GetProc('sk4d_textstyle_set_height_override');
  sk4d_textstyle_set_letter_spacing                  := GetProc('sk4d_textstyle_set_letter_spacing');
  sk4d_textstyle_set_locale                          := GetProc('sk4d_textstyle_set_locale');
  sk4d_textstyle_set_place_holder                    := GetProc('sk4d_textstyle_set_place_holder');
  sk4d_textstyle_set_text_baseline                   := GetProc('sk4d_textstyle_set_text_baseline');
  sk4d_textstyle_set_typeface                        := GetProc('sk4d_textstyle_set_typeface');
  sk4d_textstyle_set_word_spacing                    := GetProc('sk4d_textstyle_set_word_spacing');
  {$ELSE}
  class procedure TSkiaApi.sk4d_textstyle_add_font_feature;                    external TSkiaApi.LibName name 'sk4d_textstyle_add_font_feature';
  class procedure TSkiaApi.sk4d_textstyle_add_shadow;                          external TSkiaApi.LibName name 'sk4d_textstyle_add_shadow';
  class procedure TSkiaApi.sk4d_textstyle_clear_background_color;              external TSkiaApi.LibName name 'sk4d_textstyle_clear_background_color';
  class procedure TSkiaApi.sk4d_textstyle_clear_foreground_color;              external TSkiaApi.LibName name 'sk4d_textstyle_clear_foreground_color';
  class function  TSkiaApi.sk4d_textstyle_create;                              external TSkiaApi.LibName name 'sk4d_textstyle_create';
  class procedure TSkiaApi.sk4d_textstyle_destroy;                             external TSkiaApi.LibName name 'sk4d_textstyle_destroy';
  class function  TSkiaApi.sk4d_textstyle_get_background;                      external TSkiaApi.LibName name 'sk4d_textstyle_get_background';
  class function  TSkiaApi.sk4d_textstyle_get_color;                           external TSkiaApi.LibName name 'sk4d_textstyle_get_color';
  class function  TSkiaApi.sk4d_textstyle_get_decoration_color;                external TSkiaApi.LibName name 'sk4d_textstyle_get_decoration_color';
  class function  TSkiaApi.sk4d_textstyle_get_decoration_mode;                 external TSkiaApi.LibName name 'sk4d_textstyle_get_decoration_mode';
  class function  TSkiaApi.sk4d_textstyle_get_decoration_style;                external TSkiaApi.LibName name 'sk4d_textstyle_get_decoration_style';
  class function  TSkiaApi.sk4d_textstyle_get_decoration_thickness_multiplier; external TSkiaApi.LibName name 'sk4d_textstyle_get_decoration_thickness_multiplier';
  class function  TSkiaApi.sk4d_textstyle_get_decorations;                     external TSkiaApi.LibName name 'sk4d_textstyle_get_decorations';
  class function  TSkiaApi.sk4d_textstyle_get_font_families;                   external TSkiaApi.LibName name 'sk4d_textstyle_get_font_families';
  class procedure TSkiaApi.sk4d_textstyle_get_font_metrics;                    external TSkiaApi.LibName name 'sk4d_textstyle_get_font_metrics';
  class function  TSkiaApi.sk4d_textstyle_get_font_size;                       external TSkiaApi.LibName name 'sk4d_textstyle_get_font_size';
  class procedure TSkiaApi.sk4d_textstyle_get_font_style;                      external TSkiaApi.LibName name 'sk4d_textstyle_get_font_style';
  class function  TSkiaApi.sk4d_textstyle_get_foreground;                      external TSkiaApi.LibName name 'sk4d_textstyle_get_foreground';
  class function  TSkiaApi.sk4d_textstyle_get_height;                          external TSkiaApi.LibName name 'sk4d_textstyle_get_height';
  class function  TSkiaApi.sk4d_textstyle_get_height_override;                 external TSkiaApi.LibName name 'sk4d_textstyle_get_height_override';
  class function  TSkiaApi.sk4d_textstyle_get_letter_spacing;                  external TSkiaApi.LibName name 'sk4d_textstyle_get_letter_spacing';
  class function  TSkiaApi.sk4d_textstyle_get_locale;                          external TSkiaApi.LibName name 'sk4d_textstyle_get_locale';
  class function  TSkiaApi.sk4d_textstyle_get_text_baseline;                   external TSkiaApi.LibName name 'sk4d_textstyle_get_text_baseline';
  class function  TSkiaApi.sk4d_textstyle_get_typeface;                        external TSkiaApi.LibName name 'sk4d_textstyle_get_typeface';
  class function  TSkiaApi.sk4d_textstyle_get_word_spacing;                    external TSkiaApi.LibName name 'sk4d_textstyle_get_word_spacing';
  class function  TSkiaApi.sk4d_textstyle_has_background;                      external TSkiaApi.LibName name 'sk4d_textstyle_has_background';
  class function  TSkiaApi.sk4d_textstyle_has_foreground;                      external TSkiaApi.LibName name 'sk4d_textstyle_has_foreground';
  class function  TSkiaApi.sk4d_textstyle_is_equal;                            external TSkiaApi.LibName name 'sk4d_textstyle_is_equal';
  class function  TSkiaApi.sk4d_textstyle_is_placeholder;                      external TSkiaApi.LibName name 'sk4d_textstyle_is_placeholder';
  class procedure TSkiaApi.sk4d_textstyle_reset_font_features;                 external TSkiaApi.LibName name 'sk4d_textstyle_reset_font_features';
  class procedure TSkiaApi.sk4d_textstyle_reset_shadows;                       external TSkiaApi.LibName name 'sk4d_textstyle_reset_shadows';
  class procedure TSkiaApi.sk4d_textstyle_set_background_color;                external TSkiaApi.LibName name 'sk4d_textstyle_set_background_color';
  class procedure TSkiaApi.sk4d_textstyle_set_color;                           external TSkiaApi.LibName name 'sk4d_textstyle_set_color';
  class procedure TSkiaApi.sk4d_textstyle_set_decoration_color;                external TSkiaApi.LibName name 'sk4d_textstyle_set_decoration_color';
  class procedure TSkiaApi.sk4d_textstyle_set_decoration_mode;                 external TSkiaApi.LibName name 'sk4d_textstyle_set_decoration_mode';
  class procedure TSkiaApi.sk4d_textstyle_set_decoration_style;                external TSkiaApi.LibName name 'sk4d_textstyle_set_decoration_style';
  class procedure TSkiaApi.sk4d_textstyle_set_decoration_thickness_multiplier; external TSkiaApi.LibName name 'sk4d_textstyle_set_decoration_thickness_multiplier';
  class procedure TSkiaApi.sk4d_textstyle_set_decorations;                     external TSkiaApi.LibName name 'sk4d_textstyle_set_decorations';
  class procedure TSkiaApi.sk4d_textstyle_set_font_families;                   external TSkiaApi.LibName name 'sk4d_textstyle_set_font_families';
  class procedure TSkiaApi.sk4d_textstyle_set_font_size;                       external TSkiaApi.LibName name 'sk4d_textstyle_set_font_size';
  class procedure TSkiaApi.sk4d_textstyle_set_font_style;                      external TSkiaApi.LibName name 'sk4d_textstyle_set_font_style';
  class procedure TSkiaApi.sk4d_textstyle_set_foreground_color;                external TSkiaApi.LibName name 'sk4d_textstyle_set_foreground_color';
  class procedure TSkiaApi.sk4d_textstyle_set_height;                          external TSkiaApi.LibName name 'sk4d_textstyle_set_height';
  class procedure TSkiaApi.sk4d_textstyle_set_height_override;                 external TSkiaApi.LibName name 'sk4d_textstyle_set_height_override';
  class procedure TSkiaApi.sk4d_textstyle_set_letter_spacing;                  external TSkiaApi.LibName name 'sk4d_textstyle_set_letter_spacing';
  class procedure TSkiaApi.sk4d_textstyle_set_locale;                          external TSkiaApi.LibName name 'sk4d_textstyle_set_locale';
  class procedure TSkiaApi.sk4d_textstyle_set_place_holder;                    external TSkiaApi.LibName name 'sk4d_textstyle_set_place_holder';
  class procedure TSkiaApi.sk4d_textstyle_set_text_baseline;                   external TSkiaApi.LibName name 'sk4d_textstyle_set_text_baseline';
  class procedure TSkiaApi.sk4d_textstyle_set_typeface;                        external TSkiaApi.LibName name 'sk4d_textstyle_set_typeface';
  class procedure TSkiaApi.sk4d_textstyle_set_word_spacing;                    external TSkiaApi.LibName name 'sk4d_textstyle_set_word_spacing';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'modules/skparagraph/include/sk4d_paragraphstyle.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_paragraphstyle_create                    := GetProc('sk4d_paragraphstyle_create');
  sk4d_paragraphstyle_destroy                   := GetProc('sk4d_paragraphstyle_destroy');
  sk4d_paragraphstyle_get_draw_options          := GetProc('sk4d_paragraphstyle_get_draw_options');
  sk4d_paragraphstyle_get_effective_align       := GetProc('sk4d_paragraphstyle_get_effective_align');
  sk4d_paragraphstyle_get_ellipsis              := GetProc('sk4d_paragraphstyle_get_ellipsis');
  sk4d_paragraphstyle_get_height                := GetProc('sk4d_paragraphstyle_get_height');
  sk4d_paragraphstyle_get_max_lines             := GetProc('sk4d_paragraphstyle_get_max_lines');
  sk4d_paragraphstyle_get_strut_style           := GetProc('sk4d_paragraphstyle_get_strut_style');
  sk4d_paragraphstyle_get_text_align            := GetProc('sk4d_paragraphstyle_get_text_align');
  sk4d_paragraphstyle_get_text_direction        := GetProc('sk4d_paragraphstyle_get_text_direction');
  sk4d_paragraphstyle_get_text_height_behaviors := GetProc('sk4d_paragraphstyle_get_text_height_behaviors');
  sk4d_paragraphstyle_get_text_style            := GetProc('sk4d_paragraphstyle_get_text_style');
  sk4d_paragraphstyle_has_unlimited_lines       := GetProc('sk4d_paragraphstyle_has_unlimited_lines');
  sk4d_paragraphstyle_is_ellipsized             := GetProc('sk4d_paragraphstyle_is_ellipsized');
  sk4d_paragraphstyle_is_equal                  := GetProc('sk4d_paragraphstyle_is_equal');
  sk4d_paragraphstyle_is_hinting_on             := GetProc('sk4d_paragraphstyle_is_hinting_on');
  sk4d_paragraphstyle_set_draw_options          := GetProc('sk4d_paragraphstyle_set_draw_options');
  sk4d_paragraphstyle_set_ellipsis              := GetProc('sk4d_paragraphstyle_set_ellipsis');
  sk4d_paragraphstyle_set_height                := GetProc('sk4d_paragraphstyle_set_height');
  sk4d_paragraphstyle_set_max_lines             := GetProc('sk4d_paragraphstyle_set_max_lines');
  sk4d_paragraphstyle_set_strut_style           := GetProc('sk4d_paragraphstyle_set_strut_style');
  sk4d_paragraphstyle_set_text_align            := GetProc('sk4d_paragraphstyle_set_text_align');
  sk4d_paragraphstyle_set_text_direction        := GetProc('sk4d_paragraphstyle_set_text_direction');
  sk4d_paragraphstyle_set_text_height_behaviors := GetProc('sk4d_paragraphstyle_set_text_height_behaviors');
  sk4d_paragraphstyle_set_text_style            := GetProc('sk4d_paragraphstyle_set_text_style');
  sk4d_paragraphstyle_turn_hinting_off          := GetProc('sk4d_paragraphstyle_turn_hinting_off');
  sk4d_strutstyle_create                        := GetProc('sk4d_strutstyle_create');
  sk4d_strutstyle_destroy                       := GetProc('sk4d_strutstyle_destroy');
  sk4d_strutstyle_get_enabled                   := GetProc('sk4d_strutstyle_get_enabled');
  sk4d_strutstyle_get_font_families             := GetProc('sk4d_strutstyle_get_font_families');
  sk4d_strutstyle_get_font_size                 := GetProc('sk4d_strutstyle_get_font_size');
  sk4d_strutstyle_get_font_style                := GetProc('sk4d_strutstyle_get_font_style');
  sk4d_strutstyle_get_force_height              := GetProc('sk4d_strutstyle_get_force_height');
  sk4d_strutstyle_get_height                    := GetProc('sk4d_strutstyle_get_height');
  sk4d_strutstyle_get_height_override           := GetProc('sk4d_strutstyle_get_height_override');
  sk4d_strutstyle_get_leading                   := GetProc('sk4d_strutstyle_get_leading');
  sk4d_strutstyle_is_equal                      := GetProc('sk4d_strutstyle_is_equal');
  sk4d_strutstyle_set_enabled                   := GetProc('sk4d_strutstyle_set_enabled');
  sk4d_strutstyle_set_font_families             := GetProc('sk4d_strutstyle_set_font_families');
  sk4d_strutstyle_set_font_size                 := GetProc('sk4d_strutstyle_set_font_size');
  sk4d_strutstyle_set_font_style                := GetProc('sk4d_strutstyle_set_font_style');
  sk4d_strutstyle_set_force_height              := GetProc('sk4d_strutstyle_set_force_height');
  sk4d_strutstyle_set_height                    := GetProc('sk4d_strutstyle_set_height');
  sk4d_strutstyle_set_height_override           := GetProc('sk4d_strutstyle_set_height_override');
  sk4d_strutstyle_set_leading                   := GetProc('sk4d_strutstyle_set_leading');
  {$ELSE}
  class function  TSkiaApi.sk4d_paragraphstyle_create;                    external TSkiaApi.LibName name 'sk4d_paragraphstyle_create';
  class procedure TSkiaApi.sk4d_paragraphstyle_destroy;                   external TSkiaApi.LibName name 'sk4d_paragraphstyle_destroy';
  class function  TSkiaApi.sk4d_paragraphstyle_get_draw_options;          external TSkiaApi.LibName name 'sk4d_paragraphstyle_get_draw_options';
  class function  TSkiaApi.sk4d_paragraphstyle_get_effective_align;       external TSkiaApi.LibName name 'sk4d_paragraphstyle_get_effective_align';
  class function  TSkiaApi.sk4d_paragraphstyle_get_ellipsis;              external TSkiaApi.LibName name 'sk4d_paragraphstyle_get_ellipsis';
  class function  TSkiaApi.sk4d_paragraphstyle_get_height;                external TSkiaApi.LibName name 'sk4d_paragraphstyle_get_height';
  class function  TSkiaApi.sk4d_paragraphstyle_get_max_lines;             external TSkiaApi.LibName name 'sk4d_paragraphstyle_get_max_lines';
  class function  TSkiaApi.sk4d_paragraphstyle_get_strut_style;           external TSkiaApi.LibName name 'sk4d_paragraphstyle_get_strut_style';
  class function  TSkiaApi.sk4d_paragraphstyle_get_text_align;            external TSkiaApi.LibName name 'sk4d_paragraphstyle_get_text_align';
  class function  TSkiaApi.sk4d_paragraphstyle_get_text_direction;        external TSkiaApi.LibName name 'sk4d_paragraphstyle_get_text_direction';
  class function  TSkiaApi.sk4d_paragraphstyle_get_text_height_behaviors; external TSkiaApi.LibName name 'sk4d_paragraphstyle_get_text_height_behaviors';
  class function  TSkiaApi.sk4d_paragraphstyle_get_text_style;            external TSkiaApi.LibName name 'sk4d_paragraphstyle_get_text_style';
  class function  TSkiaApi.sk4d_paragraphstyle_has_unlimited_lines;       external TSkiaApi.LibName name 'sk4d_paragraphstyle_has_unlimited_lines';
  class function  TSkiaApi.sk4d_paragraphstyle_is_ellipsized;             external TSkiaApi.LibName name 'sk4d_paragraphstyle_is_ellipsized';
  class function  TSkiaApi.sk4d_paragraphstyle_is_equal;                  external TSkiaApi.LibName name 'sk4d_paragraphstyle_is_equal';
  class function  TSkiaApi.sk4d_paragraphstyle_is_hinting_on;             external TSkiaApi.LibName name 'sk4d_paragraphstyle_is_hinting_on';
  class procedure TSkiaApi.sk4d_paragraphstyle_set_draw_options;          external TSkiaApi.LibName name 'sk4d_paragraphstyle_set_draw_options';
  class procedure TSkiaApi.sk4d_paragraphstyle_set_ellipsis;              external TSkiaApi.LibName name 'sk4d_paragraphstyle_set_ellipsis';
  class procedure TSkiaApi.sk4d_paragraphstyle_set_height;                external TSkiaApi.LibName name 'sk4d_paragraphstyle_set_height';
  class procedure TSkiaApi.sk4d_paragraphstyle_set_max_lines;             external TSkiaApi.LibName name 'sk4d_paragraphstyle_set_max_lines';
  class procedure TSkiaApi.sk4d_paragraphstyle_set_strut_style;           external TSkiaApi.LibName name 'sk4d_paragraphstyle_set_strut_style';
  class procedure TSkiaApi.sk4d_paragraphstyle_set_text_align;            external TSkiaApi.LibName name 'sk4d_paragraphstyle_set_text_align';
  class procedure TSkiaApi.sk4d_paragraphstyle_set_text_direction;        external TSkiaApi.LibName name 'sk4d_paragraphstyle_set_text_direction';
  class procedure TSkiaApi.sk4d_paragraphstyle_set_text_height_behaviors; external TSkiaApi.LibName name 'sk4d_paragraphstyle_set_text_height_behaviors';
  class procedure TSkiaApi.sk4d_paragraphstyle_set_text_style;            external TSkiaApi.LibName name 'sk4d_paragraphstyle_set_text_style';
  class procedure TSkiaApi.sk4d_paragraphstyle_turn_hinting_off;          external TSkiaApi.LibName name 'sk4d_paragraphstyle_turn_hinting_off';
  class function  TSkiaApi.sk4d_strutstyle_create;                        external TSkiaApi.LibName name 'sk4d_strutstyle_create';
  class procedure TSkiaApi.sk4d_strutstyle_destroy;                       external TSkiaApi.LibName name 'sk4d_strutstyle_destroy';
  class function  TSkiaApi.sk4d_strutstyle_get_enabled;                   external TSkiaApi.LibName name 'sk4d_strutstyle_get_enabled';
  class function  TSkiaApi.sk4d_strutstyle_get_font_families;             external TSkiaApi.LibName name 'sk4d_strutstyle_get_font_families';
  class function  TSkiaApi.sk4d_strutstyle_get_font_size;                 external TSkiaApi.LibName name 'sk4d_strutstyle_get_font_size';
  class procedure TSkiaApi.sk4d_strutstyle_get_font_style;                external TSkiaApi.LibName name 'sk4d_strutstyle_get_font_style';
  class function  TSkiaApi.sk4d_strutstyle_get_force_height;              external TSkiaApi.LibName name 'sk4d_strutstyle_get_force_height';
  class function  TSkiaApi.sk4d_strutstyle_get_height;                    external TSkiaApi.LibName name 'sk4d_strutstyle_get_height';
  class function  TSkiaApi.sk4d_strutstyle_get_height_override;           external TSkiaApi.LibName name 'sk4d_strutstyle_get_height_override';
  class function  TSkiaApi.sk4d_strutstyle_get_leading;                   external TSkiaApi.LibName name 'sk4d_strutstyle_get_leading';
  class function  TSkiaApi.sk4d_strutstyle_is_equal;                      external TSkiaApi.LibName name 'sk4d_strutstyle_is_equal';
  class procedure TSkiaApi.sk4d_strutstyle_set_enabled;                   external TSkiaApi.LibName name 'sk4d_strutstyle_set_enabled';
  class procedure TSkiaApi.sk4d_strutstyle_set_font_families;             external TSkiaApi.LibName name 'sk4d_strutstyle_set_font_families';
  class procedure TSkiaApi.sk4d_strutstyle_set_font_size;                 external TSkiaApi.LibName name 'sk4d_strutstyle_set_font_size';
  class procedure TSkiaApi.sk4d_strutstyle_set_font_style;                external TSkiaApi.LibName name 'sk4d_strutstyle_set_font_style';
  class procedure TSkiaApi.sk4d_strutstyle_set_force_height;              external TSkiaApi.LibName name 'sk4d_strutstyle_set_force_height';
  class procedure TSkiaApi.sk4d_strutstyle_set_height;                    external TSkiaApi.LibName name 'sk4d_strutstyle_set_height';
  class procedure TSkiaApi.sk4d_strutstyle_set_height_override;           external TSkiaApi.LibName name 'sk4d_strutstyle_set_height_override';
  class procedure TSkiaApi.sk4d_strutstyle_set_leading;                   external TSkiaApi.LibName name 'sk4d_strutstyle_set_leading';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'modules/skparagraph/include/sk4d_paragraph.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_paragraph_destroy             := GetProc('sk4d_paragraph_destroy');
  sk4d_paragraph_get_height          := GetProc('sk4d_paragraph_get_height');
  sk4d_paragraph_get_max_width       := GetProc('sk4d_paragraph_get_max_width');
  sk4d_paragraph_get_rects_for_range := GetProc('sk4d_paragraph_get_rects_for_range');
  sk4d_paragraph_get_word_boundary   := GetProc('sk4d_paragraph_get_word_boundary');
  sk4d_paragraph_layout              := GetProc('sk4d_paragraph_layout');
  sk4d_paragraph_render              := GetProc('sk4d_paragraph_render');
  {$ELSE}
  class procedure TSkiaApi.sk4d_paragraph_destroy;             external TSkiaApi.LibName name 'sk4d_paragraph_destroy';
  class function  TSkiaApi.sk4d_paragraph_get_height;          external TSkiaApi.LibName name 'sk4d_paragraph_get_height';
  class function  TSkiaApi.sk4d_paragraph_get_max_width;       external TSkiaApi.LibName name 'sk4d_paragraph_get_max_width';
  class function  TSkiaApi.sk4d_paragraph_get_rects_for_range; external TSkiaApi.LibName name 'sk4d_paragraph_get_rects_for_range';
  class procedure TSkiaApi.sk4d_paragraph_get_word_boundary;   external TSkiaApi.LibName name 'sk4d_paragraph_get_word_boundary';
  class procedure TSkiaApi.sk4d_paragraph_layout;              external TSkiaApi.LibName name 'sk4d_paragraph_layout';
  class procedure TSkiaApi.sk4d_paragraph_render;              external TSkiaApi.LibName name 'sk4d_paragraph_render';
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'modules/skparagraph/include/sk4d_paragraphbuilder.h'}
  {$IFDEF SK_DYNAMIC_LOADING}
  sk4d_paragraphbuilder_add_placeholder := GetProc('sk4d_paragraphbuilder_add_placeholder');
  sk4d_paragraphbuilder_add_text        := GetProc('sk4d_paragraphbuilder_add_text');
  sk4d_paragraphbuilder_create          := GetProc('sk4d_paragraphbuilder_create');
  sk4d_paragraphbuilder_destroy         := GetProc('sk4d_paragraphbuilder_destroy');
  sk4d_paragraphbuilder_detach          := GetProc('sk4d_paragraphbuilder_detach');
  sk4d_paragraphbuilder_pop             := GetProc('sk4d_paragraphbuilder_pop');
  sk4d_paragraphbuilder_push_style      := GetProc('sk4d_paragraphbuilder_push_style');
  {$ELSE}
  class procedure TSkiaApi.sk4d_paragraphbuilder_add_placeholder; external TSkiaApi.LibName name 'sk4d_paragraphbuilder_add_placeholder';
  class procedure TSkiaApi.sk4d_paragraphbuilder_add_text;        external TSkiaApi.LibName name 'sk4d_paragraphbuilder_add_text';
  class function  TSkiaApi.sk4d_paragraphbuilder_create;          external TSkiaApi.LibName name 'sk4d_paragraphbuilder_create';
  class procedure TSkiaApi.sk4d_paragraphbuilder_destroy;         external TSkiaApi.LibName name 'sk4d_paragraphbuilder_destroy';
  class function  TSkiaApi.sk4d_paragraphbuilder_detach;          external TSkiaApi.LibName name 'sk4d_paragraphbuilder_detach';
  class procedure TSkiaApi.sk4d_paragraphbuilder_pop;             external TSkiaApi.LibName name 'sk4d_paragraphbuilder_pop';
  class procedure TSkiaApi.sk4d_paragraphbuilder_push_style;      external TSkiaApi.LibName name 'sk4d_paragraphbuilder_push_style';
  {$ENDIF}
  {$ENDREGION}

{$IF defined(MSWINDOWS) or defined(SK_DYNAMIC_LOADING)}
end;
{$ENDIF}

{$IFDEF SK_DYNAMIC_LOADING}

class destructor TSkiaApi.Destroy;
begin
  FreeLibrary(FLibHandle);
end;

class function TSkiaApi.GetProc(const AName: System.PChar): Pointer;
begin
  Result := GetProcAddress(FLibHandle, AName);
  if Result = nil then
    raise ESkiaApi.CreateFmt('Could not get address of %s function.', [AName]);
end;

{$ENDIF}

{$ENDREGION}

{$REGION 'Static linking'}

{$IFDEF IOS}
procedure libcompiler_rt; external '/usr/lib/clang/lib/darwin/libclang_rt.ios.a';
procedure libcpp; external '/usr/lib/libc++.dylib';
{$ENDIF}

{$ENDREGION}

end.