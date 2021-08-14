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
unit sk4d;

{$ALIGN ON}
{$MINENUMSIZE 4}

interface

{$I sk4d.inc}

{$REGION 'C types'}
type
  bool       = System.Boolean;
  double     = System.Double;
  float      = Single;
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

{$REGION 'include/c/sk4d_types.h'}
type
  gr_backendformat_t            = THandle;
  gr_backendrendertarget_t      = THandle;
  gr_backendtexture_t           = THandle;
  gr_directcontext_t            = THandle;
  sk_canvas_t                   = THandle;
  sk_codec_t                    = THandle;
  sk_colorfilter_t              = THandle;
  sk_colorspace_t               = THandle;
  sk_data_t                     = THandle;
  sk_document_t                 = THandle;
  sk_dynamicmemorywstream_t     = THandle;
  sk_filestream_t               = THandle;
  sk_filewstream_t              = THandle;
  sk_font_t                     = THandle;
  sk_fontmgr_t                  = THandle;
  sk_fontstyle_t                = THandle;
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

  psk_imagefilter_t             = ^sk_imagefilter_t;

  gr_backendapi_t = (
    OPEN_GL_GR_BACKENDAPI,
    METAL_GL_GR_BACKENDAPI = 2
  );

  gr_surfaceorigin_t = (
    TOP_LEFT_GR_SURFACEORIGIN,
    BOTTOM_LEFT_GR_SURFACEORIGIN
  );

  sk_alphatype_t = (
    UNKNOWN_SK_ALPHATYPE,
    OPAQUE_SK_ALPHATYPE,
    PREMUL_SK_ALPHATYPE,
    UNPREMUL_SK_ALPHATYPE
  );

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

  sk_blurstyle_t = (
    NORMAL_SK_BLURSTYLE,
    SOLID_SK_BLURSTYLE,
    OUTER_SK_BLURSTYLE,
    INNER_SK_BLURSTYLE
  );

  sk_clipop_t = (
    DIFFERENCE_SK_CLIPOP,
    INTERSECT_SK_CLIPOP
  );

  sk_colorchannel_t = (
    R_SK_COLORCHANNEL,
    G_SK_COLORCHANNEL,
    B_SK_COLORCHANNEL,
    A_SK_COLORCHANNEL
  );

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

  sk_drawpointsmode_t = (
    POINTS_SK_DRAWPOINTSMODE,
    LINES_SK_DRAWPOINTSMODE,
    POLYGON_DRAWPOINTSMODE
  );

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

  sk_filtermode_t = (
    NEAREST_SK_FILTERMODE,
    LINEAR_SK_FILTERMODE
  );

  sk_fontedging_t = (
    ALIAS_SK_FONTEDGING,
    ANTI_ALIAS_SK_FONTEDGING,
    SUBPIXEL_ANTI_ALIAS_SK_FONTEDGING
  );

  sk_fonthinting_t = (
    NONE_SK_FONTHINTING,
    SLIGHT_SK_FONTHINTING,
    NORMAL_SK_FONTHINTING,
    FULL_SK_FONTHINTING
  );

  sk_fontslant_t = (
    UPRIGHT_SK_FONTSLANT,
    ITALIC_SK_FONTSLANT,
    OBLIQUE_SK_FONTSLANT
  );

  sk_highcontrastconfiginvertstyle_t = (
    NO_INVERT_SK_HIGHCONTRASTCONFIGINVERTSTYLE,
    INVERT_BRIGHTNESS_SK_HIGHCONTRASTCONFIGINVERTSTYLE,
    INVERT_LIGHTNESS_SK_HIGHCONTRASTCONFIGINVERTSTYLE
  );

  sk_imagecachinghint_t = (
    ALLOW_SK_IMAGECACHINGHINT,
    DISALLOW_SK_IMAGECACHINGHINT
  );

  sk_latticerecttype_t = (
    DEFAULT_SK_LATTICERECTTYPE,
    TRANSPARENT_SK_LATTICERECTTYPE,
    FIXED_COLOR_SK_LATTICERECTTYPE
  );

  sk_mipmapmode_t = (
    NONE_SK_MIPMAPMODE,
    NEAREST_SK_MIPMAPMODE,
    LINEAR_SK_MIPMAPMODE
  );

  sk_paintstyle_t = (
    FILL_SK_PAINTSTYLE,
    STROKE_SK_PAINTSTYLE,
    STROKE_AND_FILL_SK_PAINTSTYLE
  );

  sk_patharcsize_t = (
    SMALL_SK_ARCSIZE,
    LARGE_SK_ARCSIZE
  );

  sk_pathdirection_t = (
    CW_SK_PATHDIRECTION,
    CCW_SK_PATHDIRECTION
  );

  sk_patheffect1dstyle_t = (
    TRANSLATE_SK_PATHEFFECT1DSTYLE,
    ROTATE_SK_PATHEFFECT1DSTYLE,
    MORPH_SK_PATHEFFECT1DSTYLE
  );

  sk_patheffecttrimmode_t = (
    NORMAL_SK_PATHEFFECTTRIMMODE,
    INVERTED_SK_PATHEFFECTTRIMMODE
  );

  sk_pathfilltype_t = (
    WINDING_SK_PATHFILLTYPE,
    EVEN_ODD_SK_PATHFILLTYPE,
    INVERSE_WINDING_SK_PATHFILLTYPE,
    INVERSE_EVEN_ODD_SK_PATHFILLTYPE
  );

  sk_pathop_t = (
    DIFFERENCE_SK_PATHOP,
    INTERSECT_SK_PATHOP,
    UNION_SK_PATHOP,
    EOR_SK_PATHOP,
    REVERSE_DIFFERENCE_SK_PATHOP
  );

  sk_pathverb_t = (
    MOVE_SK_PATHVERB,
    LINE_SK_PATHVERB,
    QUAD_SK_PATHVERB,
    CONIC_SK_PATHVERB,
    CUBIC_SK_PATHVERB,
    CLOSE_SK_PATHVERB
  );

  sk_pixelgeometry_t = (
    UNKNOWN_SK_PIXELGEOMETRY,
    RGB_HORIZONTAL_SK_PIXELGEOMETRY,
    BGR_HORIZONTAL_SK_PIXELGEOMETRY,
    RGB_VERTICAL_SK_PIXELGEOMETRY,
    BGR_VERTICAL_SK_PIXELGEOMETRY
  );

  sk_regionop_t = (
    DIFFERENCE_SK_REGIONOP,
    INTERSECT_SK_REGIONOP,
    UNION_SK_REGIONOP,
    EOR_SK_REGIONOP,
    REVERSE_DIFFERENCE_SK_REGIONOP,
    REPLACE_SK_REGIONOP
  );

  sk_rrectcorner_t = (
    UPPER_LEFT_SK_RRECTCORNER,
    UPPER_RIGHT_SK_RRECTCORNER,
    LOWER_RIGHT_SK_RRECTCORNER,
    LOWER_LEFT_SK_RRECTCORNER
  );

  sk_rrecttype_t = (
    EMPTY_SK_RRECTTYPE,
    RECT_SK_RRECTTYPE,
    OVAL_SK_RRECTTYPE,
    SIMPLE_SK_RRECTTYPE,
    NINE_PATCH_SK_RRECTTYPE,
    COMPLEX_SK_RRECTTYPE
  );

  sk_srcrectconstraint_t = (
    CLOSE_SK_SRCRECTCONSTRAINT,
    FAST_SK_SRCRECTCONSTRAINT
  );

  sk_strokecap_t = (
    BUTT_SK_STROKECAP,
    ROUND_SK_STROKECAP,
    SQUARE_SK_STROKECAP
  );

  sk_strokejoin_t = (
    MITER_SK_STROKEJOIN,
    ROUND_SK_STROKEJOIN,
    BEVEL_SK_STROKEJOIN
  );

  sk_textencoding_t = (
    UTF8_SK_TEXTENCODING,
    UTF16_SK_TEXTENCODING,
    UTF32_SK_TEXTENCODING,
    GLYPH_ID_SK_TEXTENCODING
  );

  sk_tilemode_t = (
    CLAMP_SK_TILEMODE,
    REPLICATE_SK_TILEMODE,
    MIRROR_SK_TILEMODE,
    DECAL_SK_TILEMODE
  );

  sk_vertexmode_t = (
    TRIANGLES_SK_VERTEXMODE,
    TRIANGLE_STRIP_SK_VERTEXMODE,
    TRIANGLE_FAN_SK_VERTEXMODE
  );

  psk_latticerecttype_t = ^sk_latticerecttype_t;

  sk_color_t       = uint32_t;
  sk_fourbytetag_t = uint32_t;
  sk_glyphid_t     = uint16_t;
  sk_unichar_t     = int32_t;

  psk_color_t       = ^sk_color_t;
  psk_fourbytetag_t = ^sk_fourbytetag_t;
  psk_glyphid_t     = ^sk_glyphid_t;
  psk_unichar_t     = ^sk_unichar_t;

  sk_color4f_t = record
    r: float;
    g: float;
    b: float;
    a: float;
  end;
  psk_color4f_t = ^sk_color4f_t;

  sk_colorspacematrix33_t = record
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
  psk_colorspacematrix33_t = ^sk_colorspacematrix33_t;

  sk_colorspacematrix34_t = record
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
  end;
  psk_colorspacematrix34_t = ^sk_colorspacematrix34_t;

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

  sk_colorspacecurve_t = record
  case Integer of
    0: (alias_of_table_entries : uint32_t;
        parametric             : sk_colorspacetransferfn_t);
    1:
      (table_entries : uint32_t;
       table_8       : puint8_t;
       table_16      : puint8_t);
  end;
  psk_colorspacecurve_t = ^sk_colorspacecurve_t;

  sk_colorspacea2b_t = record
    input_channels  : uint32_t;
    input_curves    : array[0..3] of sk_colorspacecurve_t;
    grid_points     : array[0..3] of uint8_t;
    grid_8          : puint8_t;
    grid_16         : puint8_t;
    matrix_channels : uint32_t;
    matrix_curves   : array[0..2] of sk_colorspacecurve_t;
    matrix          : sk_colorspacematrix34_t;
    output_channels : uint32_t;
    output_curves   : array[0..2] of sk_colorspacecurve_t;
  end;
  psk_colorspacea2b_t = ^sk_colorspacea2b_t;

  sk_colorspaceb2a_t = record
    input_channels  : uint32_t;
    input_curves    : array[0..2] of sk_colorspacecurve_t;
    matrix_channels : uint32_t;
    matrix          : sk_colorspacematrix34_t;
    matrix_curves   : array[0..2] of sk_colorspacecurve_t;
    output_channels : uint32_t;
    grid_points     : array[0..3] of uint8_t;
    grid_8          : puint8_t;
    grid_16         : puint8_t;
    output_curves   : array[0..3] of sk_colorspacecurve_t;
  end;
  psk_colorspaceb2a_t = ^sk_colorspaceb2a_t;

  sk_colorspaceiccprofile_t = record
    buffer           : puint8_t;
    size             : uint32_t;
    data_color_space : uint32_t;
    pcs              : uint32_t;
    tag_count        : uint32_t;
    has_trc          : bool;
    trc              : array[0..2] of sk_colorspacecurve_t;
    has_to_xyz_d50   : bool;
    to_xyz_d50       : sk_colorspacematrix33_t;
    has_a2b          : bool;
    a2b              : sk_colorspacea2b_t;
    has_b2a          : bool;
    b2a              : sk_colorspaceb2a_t;
  end;
  psk_colorspaceiccprofile_t = ^sk_colorspaceiccprofile_t;

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

  sk_pdfmetadata_t = record
    title            : sk_string_t;
    author           : sk_string_t;
    subject          : sk_string_t;
    keywords         : sk_string_t;
    creator          : sk_string_t;
    producer         : sk_string_t;
    creation         : sk_datetime_t;
    modified         : sk_datetime_t;
    raster_dpi       : float;
    pdfa             : bool;
    encoding_quality : int32_t;
  end;
  psk_pdfmetadata_t = ^sk_pdfmetadata_t;

  sk_point_t = record
    x: float;
    y: float;
  end;
  psk_point_t = ^sk_point_t;

  sk_vector_t  = sk_point_t;
  psk_vector_t = ^sk_vector_t;

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
    text      : MarshaledAString;
    clusters  : puint32_t;
  end;
  psk_runbuffer_t = ^sk_runbuffer_t;

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

  sk_data_release_proc           = procedure (const data: Pointer; context: Pointer); cdecl;
  sk_debug_msg_proc              = procedure (const msg: MarshaledAString); cdecl;
  sk_font_glyph_path_proc        = procedure (const path: sk_path_t; const matrix: psk_matrix_t; context: Pointer); cdecl;
  sk_image_raster_release_proc   = procedure (const pixels: Pointer; context: Pointer); cdecl;
  sk_image_texture_release_proc  = procedure (context: Pointer); cdecl;
  sk_surface_raster_release_proc = procedure (pixels, context: Pointer); cdecl;

  sk_managedstream_procs_t = record
    get_length   : function  (context: Pointer): size_t; cdecl;
    get_position : function  (context: Pointer): size_t; cdecl;
    read         : function  (context: Pointer; buffer: Pointer; size: size_t): size_t; cdecl;
    release      : procedure (context: Pointer); cdecl;
    seek         : function  (context: Pointer; position: size_t): bool; cdecl;
  end;
  psk_managedstream_procs_t = ^sk_managedstream_procs_t;

  sk_managedwstream_procs_t = record
    release : procedure (context: Pointer); cdecl;
    write   : function  (context: Pointer; const buffer: Pointer; size: size_t): bool; cdecl;
  end;
  psk_managedwstream_procs_t = ^sk_managedwstream_procs_t;

  sk_tracememorydumpbaseclass_procs_t = record
    dump_numeric_value : procedure (context: Pointer; const dump_name, value_name, units: MarshaledAString; value: uint64_t); cdecl;
    dump_string_value  : procedure (context: Pointer; const dump_name, value_name, value: MarshaledAString); cdecl;
  end;
  psk_tracememorydumpbaseclass_procs_t = ^sk_tracememorydumpbaseclass_procs_t;

// GPU - OpenGL

  gr_gl_interface_t = THandle;

  gl_enum_t = uint32_t;
  gl_uint_t = uint32_t;

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
    device: gr_mtl_handle_t;
    queue: gr_mtl_handle_t;
    binary_archive: gr_mtl_handle_t;
  end;
  pgr_mtl_backendcontext_t = ^gr_mtl_backendcontext_t;
{$ENDREGION}

{$REGION 'include/c/gr4d_backendsurface.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_backendrendertarget_create_gl              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(width, height, sample_count, stencil_bits: int32_t; const framebuffer_info: pgr_gl_framebufferinfo_t): gr_backendrendertarget_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_backendrendertarget_create_mtl             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(width, height: int32_t; const texture_info: pgr_mtl_textureinfo_t): gr_backendrendertarget_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}gr4d_backendrendertarget_destroy                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_backendrendertarget_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_backendrendertarget_get_backend_api        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendrendertarget_t): gr_backendapi_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_backendrendertarget_get_gl_framebuffer_info{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendrendertarget_t; var framebuffer_info: gr_gl_framebufferinfo_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_backendrendertarget_get_height             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendrendertarget_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_backendrendertarget_get_sample_count       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendrendertarget_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_backendrendertarget_get_stencil_bits       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendrendertarget_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_backendrendertarget_get_width              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendrendertarget_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_backendrendertarget_is_valid               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendrendertarget_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_backendtexture_create_gl                   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(width, height: int32_t; mipmapped: bool; const texture_info: pgr_gl_textureinfo_t): gr_backendtexture_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_backendtexture_create_mtl                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(width, height: int32_t; mipmapped: bool; const texture_info: pgr_mtl_textureinfo_t): gr_backendtexture_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}gr4d_backendtexture_destroy                     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_backendtexture_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_backendtexture_get_backend_api             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendtexture_t): gr_backendapi_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_backendtexture_get_gl_texture_info         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendtexture_t; var texture_info: gr_gl_textureinfo_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_backendtexture_get_height                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendtexture_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_backendtexture_get_width                   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendtexture_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_backendtexture_has_mipmaps                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendtexture_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_backendtexture_is_valid                    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_backendtexture_t): bool; cdecl;
{$ENDREGION}

{$REGION 'include/c/gr4d_directcontext.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}gr4d_directcontext_abandon_context                            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}gr4d_directcontext_dump_memory_statistics                     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: gr_directcontext_t; trace_memory_dump: sk_tracememorydump_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}gr4d_directcontext_flush                                      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}gr4d_directcontext_free_gpu_resources                         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_directcontext_get_max_surface_sample_count_for_color_type{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_directcontext_t; color_type: sk_colortype_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_directcontext_get_resource_cache_limit                   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_directcontext_t): size_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}gr4d_directcontext_get_resource_cache_usage                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: gr_directcontext_t; var max_resources: int32_t; var max_resources_bytes: size_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_directcontext_make_gl                                    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const gl_interface: gr_gl_interface_t): gr_directcontext_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_directcontext_make_metal                                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const backend_context: pgr_mtl_backendcontext_t): gr_directcontext_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}gr4d_directcontext_perform_deferred_cleanup                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t; milliseconds: int64_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}gr4d_directcontext_purge_unlocked_resources                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t; scratch_resources_only: bool); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}gr4d_directcontext_purge_unlocked_resources2                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t; bytes_to_purge: size_t; prefer_scratch_resources: bool); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}gr4d_directcontext_release_resources_and_abandon_context      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}gr4d_directcontext_reset_context                              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t; state: uint32_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}gr4d_directcontext_set_resource_cache_limit                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: gr_directcontext_t; value: size_t); cdecl;
{$ENDREGION}

{$REGION 'include/c/gr4d_gl_interface.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_gl_interface_has_extension       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_gl_interface_t; const name: MarshaledAString): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_gl_interface_make_assembled      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: Pointer; proc: gr_gl_get_proc): gr_gl_interface_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_gl_interface_make_assembled_gl   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: Pointer; proc: gr_gl_get_proc): gr_gl_interface_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_gl_interface_make_assembled_gles {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: Pointer; proc: gr_gl_get_proc): gr_gl_interface_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_gl_interface_make_assembled_webgl{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: Pointer; proc: gr_gl_get_proc): gr_gl_interface_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}gr4d_gl_interface_validate            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: gr_gl_interface_t): bool; cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_canvas.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_clear                        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; color: sk_color_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_clear2                       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const color: psk_color4f_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_destroy                      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_discard                      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_clip_path                    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const path: sk_path_t; op: sk_clipop_t; do_anti_alias: bool); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_clip_rect                    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const rect: psk_rect_t; op: sk_clipop_t; do_anti_alias: bool); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_clip_region                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const region: sk_region_t; op: sk_clipop_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_clip_rrect                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const rrect: sk_rrect_t; op: sk_clipop_t; do_anti_alias: bool); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_clip_shader                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; shader: sk_shader_t; op: sk_clipop_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_concat                       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const matrix: psk_matrix44_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_concat2                      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const matrix: psk_matrix_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_annotation              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const rect: psk_rect_t; const key: MarshaledAString; value: sk_data_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_arc                     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const oval: psk_rect_t; start_angle, sweep_angle: float; use_center: bool; const paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_atlas                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const atlas: sk_image_t; const transforms: psk_rotationscalematrix_t; const sprites: psk_rect_t; const colors: psk_color_t; count: int32_t; blend_mode: sk_blendmode_t; const sampling: psk_samplingoptions_t; const cull_rect: psk_rect_t; const paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_circle                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const center: psk_point_t; radius: float; const paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_color                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; color: sk_color_t; blend_mode: sk_blendmode_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_color2                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const color: psk_color4f_t; blend_mode: sk_blendmode_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_glyphs                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; count: int32_t; const glyphs: psk_glyphid_t; const positions, origin: psk_point_t; const font: sk_font_t; const paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_glyphs2                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; count: int32_t; const glyphs: psk_glyphid_t; const positions: psk_rotationscalematrix_t; const origin: psk_point_t; const font: sk_font_t; const paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_image                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const image: sk_image_t; x, y: float; const sampling: psk_samplingoptions_t; const paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_image_lattice           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const image: sk_image_t; const lattice: psk_lattice_t; const dest: psk_rect_t; filter_mode: sk_filtermode_t; const paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_image_nine              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const image: sk_image_t; const center: psk_irect_t; const dest: psk_rect_t; filter_mode: sk_filtermode_t; const paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_image_rect              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const image: sk_image_t; const src, dest: psk_rect_t; const sampling: psk_samplingoptions_t; const paint: sk_paint_t; constraint: sk_srcrectconstraint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_line                    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const point1, point2: psk_point_t; paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_oval                    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const oval: psk_rect_t; const paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_paint                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_patch                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const cubics: psk_point_t; const colors: psk_color_t; const tex_coords: psk_point_t; blend_mode: sk_blendmode_t; const paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_path                    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const path: sk_path_t; const paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_picture                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const picture: sk_picture_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_picture2                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const picture: sk_picture_t; const matrix: psk_matrix_t; const paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_point                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const point: psk_point_t; const paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_points                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; mode: sk_drawpointsmode_t; count: size_t; const points: psk_point_t; const paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_rect                    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const rect: psk_rect_t; const paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_region                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const region: sk_region_t; const paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_rrect                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const rrect: sk_rrect_t; const paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_rrect2                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const rect: psk_rect_t; radius_x, radius_y: float; const paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_rrect_difference        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const outer, inner: sk_rrect_t; const paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_simple_text             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const text: Pointer; size: size_t; encoding: sk_textencoding_t; x, y: float; const font: sk_font_t; const paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_text_blob               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const text_blob: sk_textblob_t; x, y: float; const paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_draw_vertices                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const vertices: sk_vertices_t; blend_mode: sk_blendmode_t; const paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_canvas_find_marked_ctm              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_canvas_t; const name: MarshaledAString; var matrix: sk_matrix44_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_get_device_clip_bounds       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_canvas_t; var result: sk_irect_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_get_local_clip_bounds        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_canvas_t; var result: sk_rect_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_get_local_to_device          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_canvas_t; var result: sk_matrix44_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_get_local_to_device_as_matrix{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_canvas_t; var result: sk_matrix_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_canvas_get_save_count               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_canvas_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_canvas_is_clip_empty                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_canvas_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_canvas_is_clip_rect                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_canvas_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_mark_ctm                     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const name: MarshaledAString); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_canvas_quick_reject                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_canvas_t; const rect: psk_rect_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_canvas_quick_reject2                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_canvas_t; const path: sk_path_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_reset_matrix                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_restore                      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_restore_to_count             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; save_count: int32_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_rotate                       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; degrees: float); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_rotate2                      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; degrees, px, py: float); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_canvas_save                         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_canvas_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_canvas_save_layer                   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_canvas_t; const rect: psk_rect_t; const paint: sk_paint_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_canvas_save_layer_alpha             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_canvas_t; const rect: psk_rect_t; alpha: uint8_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_scale                        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; sx, sy: float); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_set_matrix                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const matrixmatrix: psk_matrix44_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_set_matrix2                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; const matrix: psk_matrix_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_skew                         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; kx, ky: float); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_canvas_translate                    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_canvas_t; dx, dy: float); cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_codec.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_codec_decode  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(src: sk_data_t; const dest: sk_pixmap_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_codec_encode  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const src: sk_pixmap_t; dest: sk_wstream_t; format: sk_encodedimageformat_t; quality: int32_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_codec_get_info{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(src: sk_data_t; out image_info: sk_imageinfo_t): bool; cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_colorfilter.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorfilter_make_blend               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(color: sk_color_t; mode: sk_blendmode_t): sk_colorfilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorfilter_make_compose             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(outer, inner: sk_colorfilter_t): sk_colorfilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorfilter_make_high_contrast       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const config: psk_highcontrastconfig_t): sk_colorfilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorfilter_make_hsla_matrix         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const matrix: pfloat): sk_colorfilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorfilter_make_lerp                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(weight: float; dest, src: sk_colorfilter_t): sk_colorfilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorfilter_make_lighting            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(multiply, add: sk_color_t): sk_colorfilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorfilter_make_linear_to_srgb_gamma{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_colorfilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorfilter_make_luma_color          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_colorfilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorfilter_make_matrix              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const matrix: pfloat): sk_colorfilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorfilter_make_overdraw            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const colors: psk_color_t): sk_colorfilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorfilter_make_srgb_to_linear_gamma{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_colorfilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorfilter_make_table               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const tablea_a, tablea_r, tablea_g, tablea_b: puint8_t): sk_colorfilter_t; cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_colorspace.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorspace_get_gamma_close_to_srgb {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspace_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorspace_get_gamma_is_linear     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspace_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_colorspace_get_gamut               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self, dest: sk_colorspace_t; var result: sk_colorspacematrix33_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_colorspace_get_inverse_transfer_fn {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_colorspace_t; var result: sk_colorspacetransferfn_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_colorspace_get_to_xyz_d50          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_colorspace_t; var result: sk_colorspacematrix33_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_colorspace_get_transfer_fn         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_colorspace_t; var result: sk_colorspacetransferfn_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorspace_is_equal                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, color_space: sk_colorspace_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorspace_is_numerical_transfer_fn{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspace_t; var transfer_fn: sk_colorspacetransferfn_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorspace_is_srgb                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspace_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorspace_make                    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const profile: psk_colorspaceiccprofile_t): sk_colorspace_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorspace_make_color_spin         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspace_t): sk_colorspace_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorspace_make_linear_gamma       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspace_t): sk_colorspace_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorspace_make_rgb                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const ctransfer_function: psk_colorspacetransferfn_t; const cto_xyz_d50: psk_colorspacematrix33_t): sk_colorspace_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorspace_make_srgb               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_colorspace_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorspace_make_srgb_gamma         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_colorspace_t): sk_colorspace_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorspace_make_srgb_linear        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_colorspace_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_colorspace_ref                     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_colorspace_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_colorspace_to_profile              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_colorspace_t; var result: sk_colorspaceiccprofile_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_colorspace_unref                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_colorspace_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_colorspacematrix33_adobe_rgb_gamut {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(var result: sk_colorspacematrix33_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_colorspacematrix33_display_p3      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(var result: sk_colorspacematrix33_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_colorspacematrix33_rec_2020        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(var result: sk_colorspacematrix33_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_colorspacematrix33_srgb_gamut      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(var result: sk_colorspacematrix33_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_colorspacematrix33_xyz             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(var result: sk_colorspacematrix33_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_colorspaceprimaries_get_to_xyz_d50 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: psk_colorspaceprimaries_t; var dest: sk_colorspacematrix33_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_colorspacetransferfn_hlg           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(var result: sk_colorspacetransferfn_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_colorspacetransferfn_linear        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(var result: sk_colorspacetransferfn_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_colorspacetransferfn_pq            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(var result: sk_colorspacetransferfn_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_colorspacetransferfn_rec2020       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(var result: sk_colorspacetransferfn_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_colorspacetransferfn_srgb          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(var result: sk_colorspacetransferfn_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_colorspacetransferfn_two_dot_two   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(var result: sk_colorspacetransferfn_t); cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_data.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_data_get_data          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_data_t): Pointer; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_data_get_size          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_data_t): size_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_data_make              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const data: Pointer; size: size_t; proc: sk_data_release_proc; proc_context: Pointer): sk_data_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_data_make_from_file    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const file_name: MarshaledAString): sk_data_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_data_make_from_stream  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(stream: sk_stream_t; size: size_t): sk_data_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_data_make_uninitialized{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(size: size_t): sk_data_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_data_make_with_copy    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const data: Pointer; size: size_t): sk_data_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_data_ref               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_data_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_data_unref             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_data_t); cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_debugf.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_set_debug_msg_proc{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(proc: sk_debug_msg_proc); cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_document.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_document_begin_page{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_document_t; width, height: float; const content: psk_rect_t): sk_canvas_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_document_close     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_document_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_document_end_page  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_document_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_document_make_pdf  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(w_stream: sk_wstream_t): sk_document_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_document_make_pdf2 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(w_stream: sk_wstream_t; const metadata: psk_pdfmetadata_t): sk_document_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_document_terminate {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_document_t); cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_font.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_font_create                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(typeface: sk_typeface_t; size, scale_x, skew_x: float): sk_font_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_font_create2                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const font: sk_font_t): sk_font_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_font_destroy                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_font_get_baseline_snap      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_font_get_edging             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): sk_fontedging_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_font_get_embedded_bitmaps   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_font_get_embolden           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_font_get_force_auto_hinting {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_font_get_glyphs             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t; const text: Pointer; size: size_t; encoding: sk_textencoding_t; result: psk_glyphid_t; max_count: int32_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_font_get_glyphs_count       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t; const text: Pointer; size: size_t; encoding: sk_textencoding_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_font_get_hinting            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): sk_fonthinting_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_font_get_intercepts         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; const positions: psk_point_t; top, bottom: float; result: pfloat; const paint: sk_paint_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_font_get_linear_metrics     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_font_get_metrics            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t; metrics: psk_fontmetrics_t): float; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_font_get_offsets            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; result: pfloat; origin: float); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_font_get_path               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t; glyph: sk_glyphid_t; result: sk_path_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_font_get_paths              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; proc: sk_font_glyph_path_proc; proc_context: Pointer); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_font_get_positions          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; result: psk_point_t; const origin: psk_point_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_font_get_scale_x            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): float; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_font_get_size               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): float; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_font_get_skew_x             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): float; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_font_get_subpixel           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_font_get_typeface           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): sk_typeface_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_font_get_typeface_or_default{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t): sk_typeface_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_font_get_widths_bounds      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_font_t; const glyphs: psk_glyphid_t; count: int32_t; widths: pfloat; bounds: psk_rect_t; const paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_font_is_equal               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, font: sk_font_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_font_measure_text           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t; const text: Pointer; size: size_t; encoding: sk_textencoding_t; bounds: psk_rect_t; const paint: sk_paint_t): float; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_font_set_baseline_snap      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: bool); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_font_set_edging             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: sk_fontedging_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_font_set_embedded_bitmaps   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: bool); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_font_set_embolden           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: bool); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_font_set_force_auto_hinting {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: bool); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_font_set_hinting            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: sk_fonthinting_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_font_set_linear_metrics     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: bool); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_font_set_scale_x            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: float); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_font_set_size               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: float); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_font_set_skew_x             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: float); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_font_set_subpixel           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; value: bool); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_font_set_typeface           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_font_t; typeface: sk_typeface_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_font_unichar_to_glyph       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_font_t; unichar: sk_unichar_t): sk_glyphid_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_font_unichars_to_glyphs     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_font_t; const unichars: psk_unichar_t; count: int32_t; result: psk_glyphid_t); cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_fontstyle.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_fontstyle_create    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(weight, width: int32_t; slant: sk_fontslant_t): sk_fontstyle_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_fontstyle_destroy   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_fontstyle_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_fontstyle_get_slant {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_fontstyle_t): sk_fontslant_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_fontstyle_get_weight{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_fontstyle_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_fontstyle_get_width {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_fontstyle_t): int32_t; cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_graphics.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_graphics_allow_jit                                      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_graphics_dump_memory_statistics                         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(trace_memory_dump: sk_tracememorydump_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_graphics_get_font_cache_count_limit                     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_graphics_get_font_cache_count_used                      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_graphics_get_font_cache_limit                           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): size_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_graphics_get_font_cache_used                            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): size_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_graphics_get_resource_cache_single_allocation_byte_limit{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): size_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_graphics_get_resource_cache_total_byte_limit            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): size_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_graphics_get_resource_cache_total_bytes_used            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): size_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_graphics_init                                           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_graphics_purge_all_caches                               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_graphics_purge_font_cache                               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_graphics_purge_resource_cache                           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_graphics_set_font_cache_count_limit                     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(value: int32_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_graphics_set_font_cache_limit                           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(value: size_t): size_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_graphics_set_resource_cache_single_allocation_byte_limit{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(value: size_t): size_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_graphics_set_resource_cache_total_byte_limit            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(value: size_t): size_t; cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_image.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_encode_to_data           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): sk_data_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_encode_to_data2          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t; format: sk_encodedimageformat_t; quality: int32_t): sk_data_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_get_alpha_type           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): sk_alphatype_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_get_color_space          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): sk_colorspace_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_get_color_type           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): sk_colortype_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_get_encoded_data         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): sk_data_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_get_height               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_image_get_image_info           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_image_t; var result: sk_imageinfo_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_get_unique_id            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): uint32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_get_width                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_is_lazy_generated        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_is_texture_backed        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_is_valid                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t; context: gr_directcontext_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_make_colorspace          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t; colorspace: sk_colorspace_t; context: gr_directcontext_t): sk_image_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_make_from_adopted_texture{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: gr_directcontext_t; const texture: gr_backendtexture_t; origin: gr_surfaceorigin_t; color_type: sk_colortype_t; alpha_type: sk_alphatype_t; color_space: sk_colorspace_t): sk_image_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_make_from_encoded_data   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(encoded: sk_data_t): sk_image_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_make_from_picture        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(picture: sk_picture_t; const dimensions: psk_isize_t): sk_image_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_make_from_picture2       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(picture: sk_picture_t; const dimensions: psk_isize_t; const matrix: psk_matrix_t; const paint: sk_paint_t): sk_image_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_make_from_raster         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const pixmap: sk_pixmap_t; proc: sk_image_raster_release_proc; proc_context: Pointer): sk_image_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_make_from_texture        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: gr_directcontext_t; const texture: gr_backendtexture_t; origin: gr_surfaceorigin_t; color_type: sk_colortype_t; alpha_type: sk_alphatype_t; color_space: sk_colorspace_t; proc: sk_image_texture_release_proc; proc_context: Pointer): sk_image_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_make_non_texture_image   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): sk_image_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_make_raster_copy         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const pixmap: sk_pixmap_t): sk_image_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_make_raster_image        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t): sk_image_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_make_shader              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t; tile_mode_x, tile_mode_y: sk_tilemode_t; const sampling: psk_samplingoptions_t): sk_shader_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_make_subset              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t; const subset: psk_irect_t; context: gr_directcontext_t): sk_image_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_make_texture_image       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t; context: gr_directcontext_t; mipmapped: bool): sk_image_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_make_with_filter         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t; context: gr_directcontext_t; const filter: sk_imagefilter_t; const subset, clip_bounds: psk_irect_t; var out_subset: sk_irect_t; var offset: sk_ipoint_t): sk_image_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_read_pixels              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t; context: gr_directcontext_t; const dest: sk_pixmap_t; src_x, src_y: int32_t; caching_hint: sk_imagecachinghint_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_image_scale_pixels             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_image_t; const dest: sk_pixmap_t; const sampling: psk_samplingoptions_t; caching_hint: sk_imagecachinghint_t): bool; cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_imagefilter.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_alpha_threshold       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const region: sk_region_t; inner_min, outer_max: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_arithmetic            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(k1, k2, k3, k4: float; enforce_premultiplied_color: bool; background, foreground: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_blend                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(cmode: sk_blendmode_t; background, foreground: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_blur                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(sigma_x, sigma_y: float; tile_mode: sk_tilemode_t; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_colorfilter           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(color_filter: sk_colorfilter_t; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_compose               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(inner, outer: sk_imagefilter_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_dilate                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(radius_x, radius_y: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_displacement_map      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(x_channel_selector, y_channel_selector: sk_colorchannel_t; scale: float; displacement, color: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_distant_light_diffuse {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const direction: psk_point3_t; light_color: sk_color_t; surface_scale, kd: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_distant_light_specular{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const direction: psk_point3_t; light_color: sk_color_t; surface_scale, ks, shininess: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_drop_shadow           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(dx, dy, sigma_x, sigma_y: float; color: sk_color_t; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_drop_shadow_only      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(dx, dy, sigma_x, sigma_y: float; color: sk_color_t; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_erode                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(radius_x, radius_y: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_image                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(image: sk_image_t; const src, dest: psk_rect_t; const sampling: psk_samplingoptions_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_magnifier             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const src: psk_rect_t; inset: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_matrix_convolution    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const kernel_size: psk_isize_t; const kernel: pfloat; gain, bias: float; const kernel_offset: psk_ipoint_t; tile_mode: sk_tilemode_t; convolve_alpha: bool; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_matrix_transform      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const matrix: psk_matrix_t; const sampling: psk_samplingoptions_t; input: sk_imagefilter_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_merge                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const filters: psk_imagefilter_t; count: int32_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_offset                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(dx, dy: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_picture               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(picture: sk_picture_t; const target_rect: psk_rect_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_point_light_diffuse   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const location: psk_point3_t; light_color: sk_color_t; surface_scale, kd: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_point_light_specular  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const location: psk_point3_t; light_color: sk_color_t; surface_scale, ks, shininess: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_shader                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(shader: sk_shader_t; dither: bool; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_spot_light_diffuse    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const location, target: psk_point3_t; falloff_exponent, cutoff_angle: float; light_color: sk_color_t; surface_scale, kd: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_spot_light_specular   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const location, target: psk_point3_t; falloff_exponent, cutoff_angle: float; light_color: sk_color_t; surface_scale, ks, shininess: float; input: sk_imagefilter_t; const crop_rect: psk_rect_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_imagefilter_make_tile                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const src, dest: psk_rect_t; input: sk_imagefilter_t): sk_imagefilter_t; cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_maskfilter.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_maskfilter_make_blur       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(style: sk_blurstyle_t; sigma: float; respect_ctm: bool): sk_maskfilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_maskfilter_make_shader     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(shader: sk_shader_t): sk_maskfilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_maskfilter_make_table      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const table: puint8_t): sk_maskfilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_maskfilter_make_table_clip {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(min, max: uint8_t): sk_maskfilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_maskfilter_make_table_gamma{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(gamma: float): sk_maskfilter_t; cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_paint.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_paint_as_blend_mode   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t; out mode: sk_blendmode_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_paint_create          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_paint_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_paint_create2         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const paint: sk_paint_t): sk_paint_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_paint_destroy         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_paint_get_alpha       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): uint8_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_paint_get_alphaf      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): float; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_paint_get_anti_alias  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_paint_get_color       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_color_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_paint_get_colorf      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_paint_t; var result: sk_color4f_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_paint_get_color_filter{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_colorfilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_paint_get_dither      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_paint_get_fill_path   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t; const path: sk_path_t; const cull_rect: psk_rect_t; res_scale: float; result: sk_path_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_paint_get_image_filter{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_imagefilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_paint_get_mask_filter {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_maskfilter_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_paint_get_path_effect {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_patheffect_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_paint_get_shader      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_shader_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_paint_get_stroke_cap  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_strokecap_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_paint_get_stroke_join {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_strokejoin_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_paint_get_stroke_miter{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): float; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_paint_get_stroke_width{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): float; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_paint_get_style       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_paint_t): sk_paintstyle_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_paint_reset           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_paint_set_alpha       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: uint8_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_paint_set_alphaf      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: float); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_paint_set_antialias   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: bool); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_paint_set_argb        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; a, r, g, b: uint8_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_paint_set_blend_mode  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; mode: sk_blendmode_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_paint_set_color       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_color_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_paint_set_colorf      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; const value: psk_color4f_t; color_space: sk_colorspace_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_paint_set_color_filter{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_colorfilter_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_paint_set_dither      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: bool); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_paint_set_image_filter{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_imagefilter_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_paint_set_mask_filter {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_maskfilter_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_paint_set_path_effect {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_patheffect_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_paint_set_shader      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_shader_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_paint_set_stroke_cap  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_strokecap_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_paint_set_stroke_join {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_strokejoin_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_paint_set_stroke_miter{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: float); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_paint_set_stroke_width{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: float); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_paint_set_style       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_paint_t; value: sk_paintstyle_t); cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_path.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_opbuilder_add                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_opbuilder_t; const path: sk_path_t; op: sk_pathop_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_opbuilder_create             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_opbuilder_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_opbuilder_destroy            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_opbuilder_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_opbuilder_detach             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_opbuilder_t; result: sk_path_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_path_contains                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t; x, y: float): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_path_create                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_path_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_path_create2                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const path: sk_path_t): sk_path_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_path_create3                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const svg: MarshaledAString): sk_path_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_path_destroy                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_path_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_path_get_bounds              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_path_t; var result: sk_rect_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_path_get_fill_type           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t): sk_pathfilltype_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_path_get_last_point          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t; var result: sk_point_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_path_get_segment_masks       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t): uint32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_path_get_tight_bounds        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_path_t; var result: sk_rect_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_path_interpolate             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, cending: sk_path_t; weight: float; result: sk_path_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_path_is_convex               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_path_is_empty                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_path_is_equal                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, path: sk_path_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_path_is_finite               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_path_is_interpolatable       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, path: sk_path_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_path_is_last_contour_closed  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_path_is_line                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t; lines: psk_point_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_path_is_oval                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t; oval: psk_rect_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_path_is_rect                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t; rect: psk_rect_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_path_is_rrect                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_path_t; rrect: sk_rrect_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_path_offset                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_path_t; dx, dy: float; result: sk_path_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_path_op                      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, path: sk_path_t; op: sk_pathop_t; result: sk_path_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_path_to_svg                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_path_t; result: sk_string_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_path_transform               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_path_t; const matrix: psk_matrix_t; result: sk_path_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pathiterator_create          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const path: sk_path_t; force_close: bool): sk_pathiterator_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathiterator_destroy         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathiterator_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pathiterator_get_conic_weight{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pathiterator_t): float; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pathiterator_next            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_pathiterator_t; points: psk_point_t; var verb: sk_pathverb_t): bool; cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_pathbuilder.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_add_arc                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const oval: psk_rect_t; start_angle, sweep_angle: float); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_add_circle             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; center_x, center_y, radius: float; direction: sk_pathdirection_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_add_oval               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const oval: psk_rect_t; direction: sk_pathdirection_t; start_index: uint32_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_add_path               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const path: sk_path_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_add_polygon            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; polygon: psk_point_t; count: int32_t; is_closed: bool); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_add_rect               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const rect: psk_rect_t; direction: sk_pathdirection_t; start_index: uint32_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_add_rrect              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const rrect: sk_rrect_t; direction: sk_pathdirection_t; start_index: uint32_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_arc_to                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const radius: psk_point_t; x_axis_rotate: float; large_arc: sk_patharcsize_t; sweep: sk_pathdirection_t; const xy: psk_point_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_arc_to2                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const oval: psk_rect_t; start_angle, sweep_angle: float; force_move_to: bool); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_arc_to3                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const point1, point2: psk_point_t; radius: float); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_close                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_conic_to               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const point1, point2: psk_point_t; weight: float); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pathbuilder_create                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_pathbuilder_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pathbuilder_create2                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const path_builder: sk_pathbuilder_t): sk_pathbuilder_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_cubic_to               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const point1, point2, point3: psk_point_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_destroy                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pathbuilder_detach                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_pathbuilder_t): sk_path_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_get_bounds             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_pathbuilder_t; var result: sk_rect_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pathbuilder_get_fill_type          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pathbuilder_t): sk_pathfilltype_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_inc_reserve            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; extra_point_count, extra_verb_count: int32_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_line_to                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const cpoint: psk_point_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_move_to                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const cpoint: psk_point_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_offset                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; dx, dy: float); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_polyline_to            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const points: psk_point_t; count: int32_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_quad_to                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const point1, point2: psk_point_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_r_conic_to             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const point1, point2: psk_point_t; weight: float); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_r_cubic_to             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const point1, point2, point3: psk_point_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_r_line_to              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const point: psk_point_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_r_quad_to              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; const point1, point2: psk_point_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_reset                  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_set_filltype           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t; value: sk_pathfilltype_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pathbuilder_snapshot               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pathbuilder_t): sk_path_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathbuilder_toggle_inverse_filltype{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathbuilder_t); cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_patheffect.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_patheffect_make_1dpath  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const path: sk_path_t; advance, phase: float; style: sk_patheffect1dstyle_t): sk_patheffect_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_patheffect_make_2dline  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(width: float; const matrix: psk_matrix_t): sk_patheffect_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_patheffect_make_2dpath  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const matrix: psk_matrix_t; const path: sk_path_t): sk_patheffect_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_patheffect_make_compose {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(outer, inner: sk_patheffect_t): sk_patheffect_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_patheffect_make_corner  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(radius: float): sk_patheffect_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_patheffect_make_dash    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const intervals: pfloat; count: int32_t; phase: float): sk_patheffect_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_patheffect_make_discrete{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(seg_length, deviation: float; seed_assist: uint32_t): sk_patheffect_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_patheffect_make_sum     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(effect1, effect2: sk_patheffect_t): sk_patheffect_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_patheffect_make_trim    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(start, stop: float; mode: sk_patheffecttrimmode_t): sk_patheffect_t; cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_pathmeasure.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pathmeasure_create                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const path: sk_path_t; force_closed: bool; res_scale: float): sk_pathmeasure_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pathmeasure_destroy                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pathmeasure_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pathmeasure_get_length              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_pathmeasure_t): float; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pathmeasure_get_matrix              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_pathmeasure_t; distance: float; var matrix: sk_matrix_t; matrix_flags: uint32_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pathmeasure_get_position_and_tangent{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_pathmeasure_t; distance: float; var position: sk_point_t; var tangent: sk_vector_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pathmeasure_get_segment             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_pathmeasure_t; start, stop: float; result: sk_path_t; start_with_move_to: bool): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pathmeasure_is_closed               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_pathmeasure_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pathmeasure_next_contour            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_pathmeasure_t): bool; cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_picture.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_picture_get_cull_rect   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_picture_t; var result: sk_rect_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_picture_get_unique_id   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_picture_t): uint32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_picture_make_from_data  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const data: sk_data_t): sk_picture_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_picture_make_from_stream{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(stream: sk_stream_t): sk_picture_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_picture_make_shader     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_picture_t; tile_mode_x, tile_mode_y: sk_tilemode_t; filter_mode: sk_filtermode_t): sk_shader_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_picture_save_to_data    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_picture_t): sk_data_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_picture_save_to_stream  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_picture_t; w_stream: sk_wstream_t); cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_picturerecorder.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_picturerecorder_begin_recording  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_picturerecorder_t; const bounds: psk_rect_t): sk_canvas_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_picturerecorder_create           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_picturerecorder_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_picturerecorder_destroy          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_picturerecorder_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_picturerecorder_finish_recording {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_picturerecorder_t): sk_picture_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_picturerecorder_finish_recording2{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_picturerecorder_t; const cull_rect: psk_rect_t): sk_picture_t; cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_pixmap.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pixmap_create         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_pixmap_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pixmap_create2        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const image_info: psk_imageinfo_t; const pixels: Pointer; row_bytes: size_t): sk_pixmap_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pixmap_destroy        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pixmap_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pixmap_erase          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t; color: sk_color_t; const area: psk_irect_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pixmap_erase2         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t; const color: psk_color4f_t; color_space: sk_colorspace_t; const area: psk_irect_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pixmap_extract_subset {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(dest: sk_pixmap_t; const self: sk_pixmap_t; const area: psk_irect_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pixmap_get_alpha      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t; x, y: int32_t): float; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pixmap_get_alpha_type {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t): sk_alphatype_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pixmap_get_color      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t; x, y: int32_t): sk_color_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pixmap_get_color_space{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t): sk_colorspace_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pixmap_get_color_type {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t): sk_colortype_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pixmap_get_height     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pixmap_get_image_info {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_pixmap_t; var result: sk_imageinfo_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pixmap_get_pixel_addr {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t; x, y: int32_t): Pointer; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pixmap_get_pixels     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t): Pointer; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pixmap_get_row_bytes  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t): size_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pixmap_get_width      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_pixmap_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pixmap_read_pixels    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, dest: sk_pixmap_t; src_x, src_y: int32_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_pixmap_scale_pixels   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, dest: sk_pixmap_t; const sampling: psk_samplingoptions_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_pixmap_set_colorspace {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_pixmap_t; value: sk_colorspace_t); cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_refcnt.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_refcnt_ref  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_refcnt_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_refcnt_unref{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_refcnt_t); cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_region.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_region_contains          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, region: sk_region_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_region_contains2         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_region_t; const rect: psk_irect_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_region_contains3         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_region_t; x, y: int32_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_region_create            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_region_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_region_create2           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const region: sk_region_t): sk_region_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_region_destroy           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_region_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_region_get_boundary_path {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_region_t; result: sk_path_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_region_get_bounds        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_region_t; var result: sk_irect_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_region_intersects        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, region: sk_region_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_region_intersects2       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_region_t; const rect: psk_irect_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_region_is_complex        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_region_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_region_is_empty          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_region_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_region_is_equal          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, region: sk_region_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_region_is_rect           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_region_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_region_op                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_region_t; const region: sk_region_t; op: sk_regionop_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_region_op2               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_region_t; const rect: psk_irect_t; op: sk_regionop_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_region_quick_contains    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_region_t; const rect: psk_irect_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_region_quick_reject      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, region: sk_region_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_region_quick_reject2     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_region_t; const rect: psk_irect_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_region_set_empty         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_region_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_region_set_path          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_region_t; const path: sk_path_t; const clip: sk_region_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_region_set_rect          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_region_t; const rect: psk_irect_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_region_set_rects         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_region_t; const rects: psk_irect_t; count: int32_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_region_translate         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_region_t; x, y: int32_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_region_translate2        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_region_t; x, y: int32_t; result: sk_region_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_regioncliperator_create  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const region: sk_region_t; const clip: psk_irect_t): sk_regioncliperator_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_regioncliperator_destroy {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_regioncliperator_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_regioncliperator_get_rect{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_regioncliperator_t; var result: sk_irect_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_regioncliperator_next    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_regioncliperator_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_regioniterator_create    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const region: sk_region_t): sk_regioniterator_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_regioniterator_destroy   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_regioniterator_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_regioniterator_get_rect  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_regioniterator_t; var result: sk_irect_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_regioniterator_next      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_regioniterator_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_regioniterator_rewind    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_regioniterator_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_regionspanerator_create  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const region: sk_region_t; y, left, right: int32_t): sk_regionspanerator_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_regionspanerator_destroy {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_regionspanerator_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_regionspanerator_next    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_regionspanerator_t; var left, right: int32_t): bool; cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_rrect.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_rrect_contains        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_rrect_t; const rect: psk_rect_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_rrect_create          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_rrect_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_rrect_create2         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const rrect: sk_rrect_t): sk_rrect_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_rrect_deflate         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; dx, dy: float); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_rrect_deflate2        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; dx, dy: float; result: sk_rrect_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_rrect_destroy         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_rrect_get_height      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_rrect_t): float; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_rrect_get_radii       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_rrect_t; corner: sk_rrectcorner_t; var result: sk_vector_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_rrect_get_rect        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_rrect_t; var result: sk_rect_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_rrect_get_simple_radii{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_rrect_t; var result: sk_vector_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_rrect_get_type        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_rrect_t): sk_rrecttype_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_rrect_get_width       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_rrect_t): float; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_rrect_inflate         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; dx, dy: float); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_rrect_inflate2        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; dx, dy: float; result: sk_rrect_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_rrect_is_equal        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, rrect: sk_rrect_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_rrect_is_valid        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_rrect_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_rrect_make_offset     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_rrect_t; dx, dy: float): sk_rrect_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_rrect_offset          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; dx, dy: float); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_rrect_set_empty       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_rrect_set_nine_patch  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; const rect: psk_rect_t; radius_left, radius_top, radius_right, radius_bottom: float); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_rrect_set_oval        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; const rect: psk_rect_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_rrect_set_rect        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; const rect: psk_rect_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_rrect_set_rect2       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; const rect: psk_rect_t; const radii: psk_vector_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_rrect_set_rect3       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_rrect_t; const rect: psk_rect_t; radius_x, radius_y: float); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_rrect_transform       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_rrect_t; const matrix: psk_matrix_t; result: sk_rrect_t): bool; cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_shader.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_shader_make_blend                     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(mode: sk_blendmode_t; dest, src: sk_shader_t): sk_shader_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_shader_make_color                     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(color: sk_color_t): sk_shader_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_shader_make_color2                    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const color: psk_color4f_t; color_space: sk_colorspace_t): sk_shader_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_shader_make_empty                     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_shader_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_shader_make_gradient_linear           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const points: psk_point_t; const colors: psk_color_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t): sk_shader_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_shader_make_gradient_linear2          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const points: psk_point_t; const colors: psk_color4f_t; color_space: sk_colorspace_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t): sk_shader_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_shader_make_gradient_radial           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const center: psk_point_t; radius: float; const colors: psk_color_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t): sk_shader_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_shader_make_gradient_radial2          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const center: psk_point_t; radius: float; const colors: psk_color4f_t; color_space: sk_colorspace_t; const positions: pfloat; count: int32_t; tile_mode: sk_tilemode_t): sk_shader_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_shader_make_gradient_sweep            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const center: psk_point_t; const colors: psk_color_t; const positions: pfloat; count: int32_t): sk_shader_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_shader_make_gradient_sweep2           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const center: psk_point_t; const colors: psk_color4f_t; color_space: sk_colorspace_t; const positions: pfloat; count: int32_t): sk_shader_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_shader_make_perlin_noise_fractal_noise{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(base_frequency_x, base_frequency_y: float; num_octaves: int32_t; seed: float; const tile_size: psk_isize_t): sk_shader_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_shader_make_perlin_noise_turbulence   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(base_frequency_x, base_frequency_y: float; num_octaves: int32_t; seed: float; const tile_size: psk_isize_t): sk_shader_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_shader_make_with_color_filter         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_shader_t; filter: sk_colorfilter_t): sk_shader_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_shader_make_with_local_matrix         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_shader_t; const matrix: psk_matrix_t): sk_shader_t; cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_stream.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_dynamicmemorywstream_copy_to         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_dynamicmemorywstream_t; dest: Pointer); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_dynamicmemorywstream_create          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_dynamicmemorywstream_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_dynamicmemorywstream_detach_as_data  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_dynamicmemorywstream_t): sk_data_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_dynamicmemorywstream_detach_as_stream{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_dynamicmemorywstream_t): sk_streamasset_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_dynamicmemorywstream_write_to_stream {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_dynamicmemorywstream_t; cdest: sk_wstream_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_filestream_create                    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const file_name: MarshaledAString): sk_filestream_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_filestream_is_valid                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_filestream_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_filewstream_create                   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const file_name: MarshaledAString): sk_filewstream_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_filewstream_is_valid                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_filewstream_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_managedstream_create                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: Pointer; owns_context: bool): sk_streamasset_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_managedstream_set_procs              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const procs: psk_managedstream_procs_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_managedwstream_create                {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: Pointer; owns_context: bool): sk_wstream_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_managedwstream_set_procs             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const procs: psk_managedwstream_procs_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_memorystream_create                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(data: sk_data_t): sk_memorystream_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_stream_destroy                       {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_stream_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_stream_duplicate                     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_stream_t): sk_stream_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_stream_fork                          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_stream_t): sk_stream_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_stream_get_length                    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_stream_t): size_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_stream_get_memory_base               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_stream_t): Pointer; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_stream_get_position                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_stream_t): size_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_stream_has_length                    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_stream_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_stream_has_position                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_stream_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_stream_is_at_end                     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_stream_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_stream_move                          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_stream_t; offset: long): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_stream_peek                          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_stream_t; buffer: Pointer; size: size_t): size_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_stream_read                          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_stream_t; buffer: Pointer; size: size_t): size_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_stream_rewind                        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_stream_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_stream_seek                          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_stream_t; position: size_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_wstream_destroy                      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_wstream_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_wstream_flush                        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_wstream_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_wstream_get_bytes_written            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_wstream_t): size_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_wstream_write                        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_wstream_t; const buffer: Pointer; size: size_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_wstream_write_stream                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_wstream_t; stream: sk_stream_t; size: size_t): bool; cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_string.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_string_append  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_string_t; const src: sk_string_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_string_create  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_string_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_string_create2 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const &string: sk_string_t): sk_string_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_string_destroy {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_string_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_string_get_text{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_string_t): MarshaledAString; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_string_is_equal{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, &string: sk_string_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_string_set_text{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_string_t; const value: MarshaledAString); cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_surface.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_surface_draw                    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_surface_t; canvas: sk_canvas_t; x, y: float; const sampling: psk_samplingoptions_t; paint: sk_paint_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_surface_flush                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_surface_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_surface_get_canvas              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_surface_t): sk_canvas_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_surface_get_height              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_surface_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_surface_get_image_info          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_surface_t; var result: sk_imageinfo_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_surface_get_props               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_surface_t): sk_surfaceprops_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_surface_get_width               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_surface_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_surface_make_from_ca_metal_layer{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: gr_directcontext_t; layer: gr_mtl_handle_t; origin: gr_surfaceorigin_t; sample_count: int32_t; color_type: sk_colortype_t; color_space: sk_colorspace_t; const props: sk_surfaceprops_t; out drawable: gr_mtl_handle_t): sk_surface_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_surface_make_from_rendertarget  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: gr_directcontext_t; const render_target: gr_backendrendertarget_t; origin: gr_surfaceorigin_t; color_type: sk_colortype_t; color_space: sk_colorspace_t; const props: sk_surfaceprops_t): sk_surface_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_surface_make_from_texture       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: gr_directcontext_t; const texture: gr_backendtexture_t; origin: gr_surfaceorigin_t; sample_count: int32_t; color_type: sk_colortype_t; color_space: sk_colorspace_t; const props: sk_surfaceprops_t): sk_surface_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_surface_make_image_snapshot     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_surface_t): sk_image_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_surface_make_image_snapshot2    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_surface_t; const bounds: psk_irect_t): sk_image_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_surface_make_null               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(width, height: int32_t): sk_surface_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_surface_make_raster             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const image_info: psk_imageinfo_t; row_bytes: size_t; const props: sk_surfaceprops_t): sk_surface_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_surface_make_raster_direct      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const image_info: psk_imageinfo_t; pixels: Pointer; row_bytes: size_t; proc: sk_surface_raster_release_proc; proc_context: Pointer; const props: sk_surfaceprops_t): sk_surface_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_surface_make_render_target      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: gr_directcontext_t; budgeted: bool; const image_info: psk_imageinfo_t; sample_count: int32_t; origin: gr_surfaceorigin_t; const props: sk_surfaceprops_t; should_create_with_mips: bool): sk_surface_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_surface_peek_pixels             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_surface_t; pixmap: sk_pixmap_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_surface_read_pixels             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_surface_t; const dest: sk_pixmap_t; src_x, src_y: int32_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_surface_write_pixels            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_surface_t; const src: sk_pixmap_t; dest_x, dest_y: int32_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_sk_surfaceprops_create          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(flags: uint32_t; pixel_geometry: sk_pixelgeometry_t): sk_surfaceprops_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_surfaceprops_destroy            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_surfaceprops_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_surfaceprops_get_flags          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_surfaceprops_t): uint32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_surfaceprops_get_pixel_geometry {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_surfaceprops_t): sk_pixelgeometry_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_surfaceprops_is_equal           {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self, props: sk_surfaceprops_t): bool; cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_svgcanvas.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_svgcanvas_make{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const bounds: psk_rect_t; stream: sk_wstream_t): sk_canvas_t; cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_textblob.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_textblob_get_intercepts                 {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_textblob_t; const bounds: pfloat; result: pfloat; const paint: sk_paint_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_textblob_ref                            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_textblob_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_textblob_unref                          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_textblob_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_textblobbuilder_create                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_textblobbuilder_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_textblobbuilder_destroy                 {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textblobbuilder_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_textblobbuilder_alloc_horizontal_run    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textblobbuilder_t; const font: sk_font_t; count: int32_t; y: float; const bounds: psk_rect_t; var result: sk_runbuffer_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_textblobbuilder_alloc_positioned_run    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textblobbuilder_t; const font: sk_font_t; count: int32_t; const bounds: psk_rect_t; var result: sk_runbuffer_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_textblobbuilder_alloc_rotation_scale_run{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textblobbuilder_t; const font: sk_font_t; count: int32_t; var result: sk_runbuffer_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_textblobbuilder_alloc_run               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textblobbuilder_t; const font: sk_font_t; count: int32_t; x, y: float; const bounds: psk_rect_t; var result: sk_runbuffer_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_textblobbuilder_detach                  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_textblobbuilder_t): sk_textblob_t; cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_time.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_datetime_to_iso8601{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: psk_datetime_t; result: sk_string_t); cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_tracememorydump.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_tracememorydumpbaseclass_create   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(detailed_dump, dump_wrapped_objects: bool; context: Pointer): sk_tracememorydumpbaseclass_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_tracememorydumpbaseclass_destroy  {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_tracememorydumpbaseclass_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_tracememorydumpbaseclass_set_procs{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const procs: psk_tracememorydumpbaseclass_procs_t); cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_typeface.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_typeface_get_family_name{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_typeface_t; result: sk_string_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_typeface_get_slant      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_typeface_t): sk_fontslant_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_typeface_get_style      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_typeface_t): sk_fontstyle_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_typeface_get_unique_id  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_typeface_t): uint32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_typeface_get_weight     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_typeface_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_typeface_get_width      {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_typeface_t): int32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_typeface_make_default   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_typeface_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_typeface_make_from_data {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(data: sk_data_t; ttc_index: int32_t): sk_typeface_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_typeface_make_from_file {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const file_name: MarshaledAString; ttc_index: int32_t): sk_typeface_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_typeface_make_from_name {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const family_name: MarshaledAString; const style: sk_fontstyle_t): sk_typeface_t; cdecl;
{$ENDREGION}

{$REGION 'include/c/sk4d_vertices.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_vertices_get_unique_id{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_vertices_t): uint32_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_vertices_make_copy    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(vertex_mode: sk_vertexmode_t; vertex_count: int32_t; const positions, textures: psk_point_t; const colors: psk_color_t; index_count: int32_t; const indices: puint16_t): sk_vertices_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_vertices_ref          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_vertices_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_vertices_unref        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_vertices_t); cdecl;
{$ENDREGION}

{$REGION 'modules/skottie/include/sk4d_skottie_types.h'}
type
  sk_skottieanimation_t = THandle;
{$ENDREGION}

{$REGION 'modules/skottie/include/sk4d_skottie.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_skottieanimation_get_duration    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_skottieanimation_t): double; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_skottieanimation_get_fps         {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_skottieanimation_t): double; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_skottieanimation_get_in_point    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_skottieanimation_t): double; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_skottieanimation_get_out_point   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_skottieanimation_t): double; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_skottieanimation_get_size        {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_skottieanimation_t; var result: sk_size_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_skottieanimation_get_version     {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_skottieanimation_t): sk_string_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_skottieanimation_make            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const data: MarshaledAString; length: size_t): sk_skottieanimation_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_skottieanimation_make_from_file  {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const file_name: MarshaledAString): sk_skottieanimation_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_skottieanimation_make_from_stream{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(stream: sk_stream_t): sk_skottieanimation_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_skottieanimation_ref             {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_skottieanimation_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_skottieanimation_render          {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_skottieanimation_t; canvas: sk_canvas_t; const dest: psk_rect_t; render_flags: uint32_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_skottieanimation_seek_frame      {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_skottieanimation_t; tick: double); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_skottieanimation_seek_frame_time {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_skottieanimation_t; tick: double); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_skottieanimation_unref           {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_skottieanimation_t); cdecl;
{$ENDREGION}

{$REGION 'modules/skshaper/include/sk4d_shaper_types.h'}
type
  sk_shaper_t                    = THandle;
  sk_shaperbidiruniterator_t     = THandle;
  sk_shaperfontruniterator_t     = THandle;
  sk_shaperlanguageruniterator_t = THandle;
  sk_shaperrunhandler_t          = THandle;
  sk_shaperrunhandlerbaseclass_t = THandle;
  sk_shaperruniterator_t         = THandle;
  sk_shaperscriptruniterator_t   = THandle;
  sk_textblobbuilderrunhandler_t = THandle;

  sk_shaperfeature_t = record
    tag   : sk_fourbytetag_t;
    value : uint32_t;
    start : size_t;
    stop  : size_t;
  end;
  psk_shaperfeature_t = ^sk_shaperfeature_t;

  sk_shaperrunhandlerbuffer_t = record
    glyphs    : psk_glyphid_t;
    positions : psk_point_t;
    offsets   : psk_point_t;
    clusters  : puint32_t;
    point     : sk_point_t;
  end;
  psk_shaperrunhandlerbuffer_t = ^sk_shaperrunhandlerbuffer_t;

  sk_shaperrunhandlerrange_t = record
    start : size_t;
    size  : size_t;
  end;
  psk_shaperrunhandlerrange_t = ^sk_shaperrunhandlerrange_t;

  sk_shaperrunhandlerinfo_t = record
    font        : sk_font_t;
    bidi_level  : uint8_t;
    advance     : sk_vector_t;
    glyph_count : size_t;
    utf8_range  : sk_shaperrunhandlerrange_t;
  end;
  psk_shaperrunhandlerinfo_t = ^sk_shaperrunhandlerinfo_t;

  sk_shaperrunhandlerbaseclass_procs_t = record
    begin_line        : procedure (context: Pointer); cdecl;
    commit_line       : procedure (context: Pointer); cdecl;
    commit_run_buffer : procedure (context: Pointer; const info: psk_shaperrunhandlerinfo_t); cdecl;
    commit_run_info   : procedure (context: Pointer); cdecl;
    run_buffer        : procedure (context: Pointer; const info: psk_shaperrunhandlerinfo_t; out result: sk_shaperrunhandlerbuffer_t); cdecl;
    run_info          : procedure (context: Pointer; const info: psk_shaperrunhandlerinfo_t); cdecl;
  end;
  psk_shaperrunhandlerbaseclass_procs_t = ^sk_shaperrunhandlerbaseclass_procs_t;
{$ENDREGION}

{$REGION 'modules/skshaper/include/sk4d_shaper.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_shaper_create                               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(): sk_shaper_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_shaper_destroy                              {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_shaper_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_shaper_make_bi_di_run_iterator              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const utf8_text: Pointer; size: size_t; bi_di_level: uint8_t): sk_shaperbidiruniterator_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_shaper_make_font_run_iterator               {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const utf8_text: Pointer; size: size_t; const font: sk_font_t; const request_name: MarshaledAString; const request_style: sk_fontstyle_t; const language: sk_shaperlanguageruniterator_t): sk_shaperfontruniterator_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_shaper_make_script_run_iterator             {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const utf8_text: Pointer; size: size_t; script: sk_fourbytetag_t): sk_shaperscriptruniterator_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_shaper_make_std_language_run_iterator       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const utf8_text: Pointer; size: size_t): sk_shaperlanguageruniterator_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_shaper_purge_caches                         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_shaper_shape                                {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_shaper_t; const utf8_text: Pointer; size: size_t; const src_font: sk_font_t; left_to_right: bool; width: float; handler: sk_shaperrunhandler_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_shaper_shape2                               {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_shaper_t; const utf8_text: Pointer; size: size_t; font: sk_shaperfontruniterator_t; bi_di: sk_shaperbidiruniterator_t; script: sk_shaperscriptruniterator_t; language: sk_shaperlanguageruniterator_t; const features: psk_shaperfeature_t; count: size_t; width: float; handler: sk_shaperrunhandler_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk_shaperbidiruniterator_get_current_level       {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_shaperbidiruniterator_t): uint8_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk_shaperfontruniterator_get_current_font        {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_shaperfontruniterator_t): sk_font_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk_shaperlanguageruniterator_get_current_language{$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_shaperlanguageruniterator_t): MarshaledAString; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_shaperrunhandler_destroy                    {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_shaperrunhandler_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_shaperrunhandlerbaseclass_create            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(context: Pointer): sk_shaperrunhandlerbaseclass_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_shaperrunhandlerbaseclass_set_procs         {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const procs: psk_shaperrunhandlerbaseclass_procs_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_shaperruniterator_consume                   {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_shaperruniterator_t): bool; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_shaperruniterator_destroy                   {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_shaperruniterator_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_shaperruniterator_get_end_of_current_run    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_shaperruniterator_t): size_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk_shaperscriptruniterator_get_current_script    {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_shaperscriptruniterator_t): sk_fourbytetag_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_textblobbuilderrunhandler_create            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const utf8_text: Pointer; const offset: psk_point_t): sk_textblobbuilderrunhandler_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_textblobbuilderrunhandler_detach            {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(self: sk_textblobbuilderrunhandler_t): sk_textblob_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_textblobbuilderrunhandler_get_end_point     {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_textblobbuilderrunhandler_t; var result: sk_point_t); cdecl;
{$ENDREGION}

{$REGION 'modules/svg/include/sk4d_svg_types.h'}
type
  sk_svgdom_t = THandle;
  sk_svgsvg_t = THandle;
{$ENDREGION}

{$REGION 'modules/svg/include/sk4d_svgdom.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_svgdom_get_root          {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(const self: sk_svgdom_t): sk_svgsvg_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}function  {$ENDIF}sk4d_svgdom_make              {$IFDEF SK_DYNAMIC_LOADING}: function  {$ENDIF}(stream: sk_stream_t): sk_svgdom_t; cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_svgdom_render            {$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_svgdom_t; canvas: sk_canvas_t); cdecl;
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk4d_svgdom_set_container_size{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(self: sk_svgdom_t; const value: psk_size_t); cdecl;
{$ENDREGION}

{$REGION 'modules/svg/include/sk4d_svgsvg.h'}
{$IFDEF SK_DYNAMIC_LOADING}
var
{$ENDIF}
{$IFNDEF SK_DYNAMIC_LOADING}procedure {$ENDIF}sk_svgsvg_get_intrinsic_size{$IFDEF SK_DYNAMIC_LOADING}: procedure {$ENDIF}(const self: sk_svgsvg_t; const view_port: psk_size_t; dpi: float; var result: sk_size_t); cdecl;
{$ENDREGION}

{$IFDEF SK_DYNAMIC_LOADING}
function SkInitialize: Boolean;
procedure SkFinalize;
{$ENDIF}

implementation

{$IFDEF SK_DYNAMIC_LOADING}
uses
  { Delphi }
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  System.SysUtils;
{$ENDIF}

const
{$IF defined(MSWINDOWS)}
  SkiaLib = 'sk4d.dll';
{$ELSEIF defined(IOS)}
  {$IFNDEF CPUARM}
    {$MESSAGE ERROR 'iOS simulator is not supported.'}
  {$ENDIF}
  SkiaLib = 'sk4d.a';
{$ELSEIF defined(MACOS)}
  SkiaLib = 'sk4d.dylib';
{$ELSE}
  SkiaLib = 'sk4d.so';
{$ENDIF}

{$IFDEF SK_DYNAMIC_LOADING}
var
  LibHandle: HMODULE;

function SkInitialize: Boolean;
begin
  LibHandle := SafeLoadLibrary(SkiaLib);
  if LibHandle = 0 then
    Exit(False);
{$ENDIF}


{$REGION 'include/c/gr4d_backendsurface.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  gr4d_backendrendertarget_create_gl               := GetProcAddress(LibHandle, 'gr4d_backendrendertarget_create_gl');
  gr4d_backendrendertarget_create_mtl              := GetProcAddress(LibHandle, 'gr4d_backendrendertarget_create_mtl');
  gr4d_backendrendertarget_destroy                 := GetProcAddress(LibHandle, 'gr4d_backendrendertarget_destroy');
  gr4d_backendrendertarget_get_backend_api         := GetProcAddress(LibHandle, 'gr4d_backendrendertarget_get_backend_api');
  gr4d_backendrendertarget_get_gl_framebuffer_info := GetProcAddress(LibHandle, 'gr4d_backendrendertarget_get_gl_framebuffer_info');
  gr4d_backendrendertarget_get_height              := GetProcAddress(LibHandle, 'gr4d_backendrendertarget_get_height');
  gr4d_backendrendertarget_get_sample_count        := GetProcAddress(LibHandle, 'gr4d_backendrendertarget_get_sample_count');
  gr4d_backendrendertarget_get_stencil_bits        := GetProcAddress(LibHandle, 'gr4d_backendrendertarget_get_stencil_bits');
  gr4d_backendrendertarget_get_width               := GetProcAddress(LibHandle, 'gr4d_backendrendertarget_get_width');
  gr4d_backendrendertarget_is_valid                := GetProcAddress(LibHandle, 'gr4d_backendrendertarget_is_valid');
  gr4d_backendtexture_create_gl                    := GetProcAddress(LibHandle, 'gr4d_backendtexture_create_gl');
  gr4d_backendtexture_create_mtl                   := GetProcAddress(LibHandle, 'gr4d_backendtexture_create_mtl');
  gr4d_backendtexture_destroy                      := GetProcAddress(LibHandle, 'gr4d_backendtexture_destroy');
  gr4d_backendtexture_get_backend_api              := GetProcAddress(LibHandle, 'gr4d_backendtexture_get_backend_api');
  gr4d_backendtexture_get_gl_texture_info          := GetProcAddress(LibHandle, 'gr4d_backendtexture_get_gl_texture_info');
  gr4d_backendtexture_get_height                   := GetProcAddress(LibHandle, 'gr4d_backendtexture_get_height');
  gr4d_backendtexture_get_width                    := GetProcAddress(LibHandle, 'gr4d_backendtexture_get_width');
  gr4d_backendtexture_has_mipmaps                  := GetProcAddress(LibHandle, 'gr4d_backendtexture_has_mipmaps');
  gr4d_backendtexture_is_valid                     := GetProcAddress(LibHandle, 'gr4d_backendtexture_is_valid');
{$ELSE}
function  gr4d_backendrendertarget_create_gl;               external SkiaLib;
function  gr4d_backendrendertarget_create_mtl;              external SkiaLib;
procedure gr4d_backendrendertarget_destroy;                 external SkiaLib;
function  gr4d_backendrendertarget_get_backend_api;         external SkiaLib;
function  gr4d_backendrendertarget_get_gl_framebuffer_info; external SkiaLib;
function  gr4d_backendrendertarget_get_height;              external SkiaLib;
function  gr4d_backendrendertarget_get_sample_count;        external SkiaLib;
function  gr4d_backendrendertarget_get_stencil_bits;        external SkiaLib;
function  gr4d_backendrendertarget_get_width;               external SkiaLib;
function  gr4d_backendrendertarget_is_valid;                external SkiaLib;
function  gr4d_backendtexture_create_gl;                    external SkiaLib;
function  gr4d_backendtexture_create_mtl;                   external SkiaLib;
procedure gr4d_backendtexture_destroy;                      external SkiaLib;
function  gr4d_backendtexture_get_backend_api;              external SkiaLib;
function  gr4d_backendtexture_get_gl_texture_info;          external SkiaLib;
function  gr4d_backendtexture_get_height;                   external SkiaLib;
function  gr4d_backendtexture_get_width;                    external SkiaLib;
function  gr4d_backendtexture_has_mipmaps;                  external SkiaLib;
function  gr4d_backendtexture_is_valid;                     external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/gr4d_directcontext.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  gr4d_directcontext_abandon_context                             := GetProcAddress(LibHandle, 'gr4d_directcontext_abandon_context');
  gr4d_directcontext_dump_memory_statistics                      := GetProcAddress(LibHandle, 'gr4d_directcontext_dump_memory_statistics');
  gr4d_directcontext_flush                                       := GetProcAddress(LibHandle, 'gr4d_directcontext_flush');
  gr4d_directcontext_free_gpu_resources                          := GetProcAddress(LibHandle, 'gr4d_directcontext_free_gpu_resources');
  gr4d_directcontext_get_max_surface_sample_count_for_color_type := GetProcAddress(LibHandle, 'gr4d_directcontext_get_max_surface_sample_count_for_color_type');
  gr4d_directcontext_get_resource_cache_limit                    := GetProcAddress(LibHandle, 'gr4d_directcontext_get_resource_cache_limit');
  gr4d_directcontext_get_resource_cache_usage                    := GetProcAddress(LibHandle, 'gr4d_directcontext_get_resource_cache_usage');
  gr4d_directcontext_make_gl                                     := GetProcAddress(LibHandle, 'gr4d_directcontext_make_gl');
  gr4d_directcontext_make_metal                                  := GetProcAddress(LibHandle, 'gr4d_directcontext_make_metal');
  gr4d_directcontext_perform_deferred_cleanup                    := GetProcAddress(LibHandle, 'gr4d_directcontext_perform_deferred_cleanup');
  gr4d_directcontext_purge_unlocked_resources                    := GetProcAddress(LibHandle, 'gr4d_directcontext_purge_unlocked_resources');
  gr4d_directcontext_purge_unlocked_resources2                   := GetProcAddress(LibHandle, 'gr4d_directcontext_purge_unlocked_resources2');
  gr4d_directcontext_release_resources_and_abandon_context       := GetProcAddress(LibHandle, 'gr4d_directcontext_release_resources_and_abandon_context');
  gr4d_directcontext_reset_context                               := GetProcAddress(LibHandle, 'gr4d_directcontext_reset_context');
  gr4d_directcontext_set_resource_cache_limit                    := GetProcAddress(LibHandle, 'gr4d_directcontext_set_resource_cache_limit');
{$ELSE}
procedure gr4d_directcontext_abandon_context;                             external SkiaLib;
procedure gr4d_directcontext_dump_memory_statistics;                      external SkiaLib;
procedure gr4d_directcontext_flush;                                       external SkiaLib;
procedure gr4d_directcontext_free_gpu_resources;                          external SkiaLib;
function  gr4d_directcontext_get_max_surface_sample_count_for_color_type; external SkiaLib;
function  gr4d_directcontext_get_resource_cache_limit;                    external SkiaLib;
procedure gr4d_directcontext_get_resource_cache_usage;                    external SkiaLib;
function  gr4d_directcontext_make_gl;                                     external SkiaLib;
function  gr4d_directcontext_make_metal;                                  external SkiaLib;
procedure gr4d_directcontext_perform_deferred_cleanup;                    external SkiaLib;
procedure gr4d_directcontext_purge_unlocked_resources;                    external SkiaLib;
procedure gr4d_directcontext_purge_unlocked_resources2;                   external SkiaLib;
procedure gr4d_directcontext_release_resources_and_abandon_context;       external SkiaLib;
procedure gr4d_directcontext_reset_context;                               external SkiaLib;
procedure gr4d_directcontext_set_resource_cache_limit;                    external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/gr4d_gl_interface.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  gr4d_gl_interface_has_extension        := GetProcAddress(LibHandle, 'gr4d_gl_interface_has_extension');
  gr4d_gl_interface_make_assembled       := GetProcAddress(LibHandle, 'gr4d_gl_interface_make_assembled');
  gr4d_gl_interface_make_assembled_gl    := GetProcAddress(LibHandle, 'gr4d_gl_interface_make_assembled_gl');
  gr4d_gl_interface_make_assembled_gles  := GetProcAddress(LibHandle, 'gr4d_gl_interface_make_assembled_gles');
  gr4d_gl_interface_make_assembled_webgl := GetProcAddress(LibHandle, 'gr4d_gl_interface_make_assembled_webgl');
  gr4d_gl_interface_validate             := GetProcAddress(LibHandle, 'gr4d_gl_interface_validate');
{$ELSE}
function  gr4d_gl_interface_has_extension;        external SkiaLib;
function  gr4d_gl_interface_make_assembled;       external SkiaLib;
function  gr4d_gl_interface_make_assembled_gl;    external SkiaLib;
function  gr4d_gl_interface_make_assembled_gles;  external SkiaLib;
function  gr4d_gl_interface_make_assembled_webgl; external SkiaLib;
function  gr4d_gl_interface_validate;             external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_canvas.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_canvas_clear                         := GetProcAddress(LibHandle, 'sk4d_canvas_clear');
  sk4d_canvas_clear2                        := GetProcAddress(LibHandle, 'sk4d_canvas_clear2');
  sk4d_canvas_destroy                       := GetProcAddress(LibHandle, 'sk4d_canvas_destroy');
  sk4d_canvas_discard                       := GetProcAddress(LibHandle, 'sk4d_canvas_discard');
  sk4d_canvas_clip_path                     := GetProcAddress(LibHandle, 'sk4d_canvas_clip_path');
  sk4d_canvas_clip_rect                     := GetProcAddress(LibHandle, 'sk4d_canvas_clip_rect');
  sk4d_canvas_clip_region                   := GetProcAddress(LibHandle, 'sk4d_canvas_clip_region');
  sk4d_canvas_clip_rrect                    := GetProcAddress(LibHandle, 'sk4d_canvas_clip_rrect');
  sk4d_canvas_clip_shader                   := GetProcAddress(LibHandle, 'sk4d_canvas_clip_shader');
  sk4d_canvas_concat                        := GetProcAddress(LibHandle, 'sk4d_canvas_concat');
  sk4d_canvas_concat2                       := GetProcAddress(LibHandle, 'sk4d_canvas_concat2');
  sk4d_canvas_draw_annotation               := GetProcAddress(LibHandle, 'sk4d_canvas_draw_annotation');
  sk4d_canvas_draw_arc                      := GetProcAddress(LibHandle, 'sk4d_canvas_draw_arc');
  sk4d_canvas_draw_atlas                    := GetProcAddress(LibHandle, 'sk4d_canvas_draw_atlas');
  sk4d_canvas_draw_circle                   := GetProcAddress(LibHandle, 'sk4d_canvas_draw_circle');
  sk4d_canvas_draw_color                    := GetProcAddress(LibHandle, 'sk4d_canvas_draw_color');
  sk4d_canvas_draw_color2                   := GetProcAddress(LibHandle, 'sk4d_canvas_draw_color2');
  sk4d_canvas_draw_glyphs                   := GetProcAddress(LibHandle, 'sk4d_canvas_draw_glyphs');
  sk4d_canvas_draw_glyphs2                  := GetProcAddress(LibHandle, 'sk4d_canvas_draw_glyphs2');
  sk4d_canvas_draw_image                    := GetProcAddress(LibHandle, 'sk4d_canvas_draw_image');
  sk4d_canvas_draw_image_lattice            := GetProcAddress(LibHandle, 'sk4d_canvas_draw_image_lattice');
  sk4d_canvas_draw_image_nine               := GetProcAddress(LibHandle, 'sk4d_canvas_draw_image_nine');
  sk4d_canvas_draw_image_rect               := GetProcAddress(LibHandle, 'sk4d_canvas_draw_image_rect');
  sk4d_canvas_draw_line                     := GetProcAddress(LibHandle, 'sk4d_canvas_draw_line');
  sk4d_canvas_draw_oval                     := GetProcAddress(LibHandle, 'sk4d_canvas_draw_oval');
  sk4d_canvas_draw_paint                    := GetProcAddress(LibHandle, 'sk4d_canvas_draw_paint');
  sk4d_canvas_draw_patch                    := GetProcAddress(LibHandle, 'sk4d_canvas_draw_patch');
  sk4d_canvas_draw_path                     := GetProcAddress(LibHandle, 'sk4d_canvas_draw_path');
  sk4d_canvas_draw_picture                  := GetProcAddress(LibHandle, 'sk4d_canvas_draw_picture');
  sk4d_canvas_draw_picture2                 := GetProcAddress(LibHandle, 'sk4d_canvas_draw_picture2');
  sk4d_canvas_draw_point                    := GetProcAddress(LibHandle, 'sk4d_canvas_draw_point');
  sk4d_canvas_draw_points                   := GetProcAddress(LibHandle, 'sk4d_canvas_draw_points');
  sk4d_canvas_draw_rect                     := GetProcAddress(LibHandle, 'sk4d_canvas_draw_rect');
  sk4d_canvas_draw_region                   := GetProcAddress(LibHandle, 'sk4d_canvas_draw_region');
  sk4d_canvas_draw_rrect                    := GetProcAddress(LibHandle, 'sk4d_canvas_draw_rrect');
  sk4d_canvas_draw_rrect2                   := GetProcAddress(LibHandle, 'sk4d_canvas_draw_rrect2');
  sk4d_canvas_draw_rrect_difference         := GetProcAddress(LibHandle, 'sk4d_canvas_draw_rrect_difference');
  sk4d_canvas_draw_simple_text              := GetProcAddress(LibHandle, 'sk4d_canvas_draw_simple_text');
  sk4d_canvas_draw_text_blob                := GetProcAddress(LibHandle, 'sk4d_canvas_draw_text_blob');
  sk4d_canvas_draw_vertices                 := GetProcAddress(LibHandle, 'sk4d_canvas_draw_vertices');
  sk4d_canvas_find_marked_ctm               := GetProcAddress(LibHandle, 'sk4d_canvas_find_marked_ctm');
  sk4d_canvas_get_device_clip_bounds        := GetProcAddress(LibHandle, 'sk4d_canvas_get_device_clip_bounds');
  sk4d_canvas_get_local_clip_bounds         := GetProcAddress(LibHandle, 'sk4d_canvas_get_local_clip_bounds');
  sk4d_canvas_get_local_to_device           := GetProcAddress(LibHandle, 'sk4d_canvas_get_local_to_device');
  sk4d_canvas_get_local_to_device_as_matrix := GetProcAddress(LibHandle, 'sk4d_canvas_get_local_to_device_as_matrix');
  sk4d_canvas_get_save_count                := GetProcAddress(LibHandle, 'sk4d_canvas_get_save_count');
  sk4d_canvas_is_clip_empty                 := GetProcAddress(LibHandle, 'sk4d_canvas_is_clip_empty');
  sk4d_canvas_is_clip_rect                  := GetProcAddress(LibHandle, 'sk4d_canvas_is_clip_rect');
  sk4d_canvas_mark_ctm                      := GetProcAddress(LibHandle, 'sk4d_canvas_mark_ctm');
  sk4d_canvas_quick_reject                  := GetProcAddress(LibHandle, 'sk4d_canvas_quick_reject');
  sk4d_canvas_quick_reject2                 := GetProcAddress(LibHandle, 'sk4d_canvas_quick_reject2');
  sk4d_canvas_reset_matrix                  := GetProcAddress(LibHandle, 'sk4d_canvas_reset_matrix');
  sk4d_canvas_restore                       := GetProcAddress(LibHandle, 'sk4d_canvas_restore');
  sk4d_canvas_restore_to_count              := GetProcAddress(LibHandle, 'sk4d_canvas_restore_to_count');
  sk4d_canvas_rotate                        := GetProcAddress(LibHandle, 'sk4d_canvas_rotate');
  sk4d_canvas_rotate2                       := GetProcAddress(LibHandle, 'sk4d_canvas_rotate2');
  sk4d_canvas_save                          := GetProcAddress(LibHandle, 'sk4d_canvas_save');
  sk4d_canvas_save_layer                    := GetProcAddress(LibHandle, 'sk4d_canvas_save_layer');
  sk4d_canvas_save_layer_alpha              := GetProcAddress(LibHandle, 'sk4d_canvas_save_layer_alpha');
  sk4d_canvas_scale                         := GetProcAddress(LibHandle, 'sk4d_canvas_scale');
  sk4d_canvas_set_matrix                    := GetProcAddress(LibHandle, 'sk4d_canvas_set_matrix');
  sk4d_canvas_set_matrix2                   := GetProcAddress(LibHandle, 'sk4d_canvas_set_matrix2');
  sk4d_canvas_skew                          := GetProcAddress(LibHandle, 'sk4d_canvas_skew');
  sk4d_canvas_translate                     := GetProcAddress(LibHandle, 'sk4d_canvas_translate');
{$ELSE}
procedure sk4d_canvas_clear;                         external SkiaLib;
procedure sk4d_canvas_clear2;                        external SkiaLib;
procedure sk4d_canvas_destroy;                       external SkiaLib;
procedure sk4d_canvas_discard;                       external SkiaLib;
procedure sk4d_canvas_clip_path;                     external SkiaLib;
procedure sk4d_canvas_clip_rect;                     external SkiaLib;
procedure sk4d_canvas_clip_region;                   external SkiaLib;
procedure sk4d_canvas_clip_rrect;                    external SkiaLib;
procedure sk4d_canvas_clip_shader;                   external SkiaLib;
procedure sk4d_canvas_concat;                        external SkiaLib;
procedure sk4d_canvas_concat2;                       external SkiaLib;
procedure sk4d_canvas_draw_annotation;               external SkiaLib;
procedure sk4d_canvas_draw_arc;                      external SkiaLib;
procedure sk4d_canvas_draw_atlas;                    external SkiaLib;
procedure sk4d_canvas_draw_circle;                   external SkiaLib;
procedure sk4d_canvas_draw_color;                    external SkiaLib;
procedure sk4d_canvas_draw_color2;                   external SkiaLib;
procedure sk4d_canvas_draw_glyphs;                   external SkiaLib;
procedure sk4d_canvas_draw_glyphs2;                  external SkiaLib;
procedure sk4d_canvas_draw_image;                    external SkiaLib;
procedure sk4d_canvas_draw_image_lattice;            external SkiaLib;
procedure sk4d_canvas_draw_image_nine;               external SkiaLib;
procedure sk4d_canvas_draw_image_rect;               external SkiaLib;
procedure sk4d_canvas_draw_line;                     external SkiaLib;
procedure sk4d_canvas_draw_oval;                     external SkiaLib;
procedure sk4d_canvas_draw_paint;                    external SkiaLib;
procedure sk4d_canvas_draw_patch;                    external SkiaLib;
procedure sk4d_canvas_draw_path;                     external SkiaLib;
procedure sk4d_canvas_draw_picture;                  external SkiaLib;
procedure sk4d_canvas_draw_picture2;                 external SkiaLib;
procedure sk4d_canvas_draw_point;                    external SkiaLib;
procedure sk4d_canvas_draw_points;                   external SkiaLib;
procedure sk4d_canvas_draw_rect;                     external SkiaLib;
procedure sk4d_canvas_draw_region;                   external SkiaLib;
procedure sk4d_canvas_draw_rrect;                    external SkiaLib;
procedure sk4d_canvas_draw_rrect2;                   external SkiaLib;
procedure sk4d_canvas_draw_rrect_difference;         external SkiaLib;
procedure sk4d_canvas_draw_simple_text;              external SkiaLib;
procedure sk4d_canvas_draw_text_blob;                external SkiaLib;
procedure sk4d_canvas_draw_vertices;                 external SkiaLib;
function  sk4d_canvas_find_marked_ctm;               external SkiaLib;
procedure sk4d_canvas_get_device_clip_bounds;        external SkiaLib;
procedure sk4d_canvas_get_local_clip_bounds;         external SkiaLib;
procedure sk4d_canvas_get_local_to_device;           external SkiaLib;
procedure sk4d_canvas_get_local_to_device_as_matrix; external SkiaLib;
function  sk4d_canvas_get_save_count;                external SkiaLib;
function  sk4d_canvas_is_clip_empty;                 external SkiaLib;
function  sk4d_canvas_is_clip_rect;                  external SkiaLib;
procedure sk4d_canvas_mark_ctm;                      external SkiaLib;
function  sk4d_canvas_quick_reject;                  external SkiaLib;
function  sk4d_canvas_quick_reject2;                 external SkiaLib;
procedure sk4d_canvas_reset_matrix;                  external SkiaLib;
procedure sk4d_canvas_restore;                       external SkiaLib;
procedure sk4d_canvas_restore_to_count;              external SkiaLib;
procedure sk4d_canvas_rotate;                        external SkiaLib;
procedure sk4d_canvas_rotate2;                       external SkiaLib;
function  sk4d_canvas_save;                          external SkiaLib;
function  sk4d_canvas_save_layer;                    external SkiaLib;
function  sk4d_canvas_save_layer_alpha;              external SkiaLib;
procedure sk4d_canvas_scale;                         external SkiaLib;
procedure sk4d_canvas_set_matrix;                    external SkiaLib;
procedure sk4d_canvas_set_matrix2;                   external SkiaLib;
procedure sk4d_canvas_skew;                          external SkiaLib;
procedure sk4d_canvas_translate;                     external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_codec.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_codec_decode   := GetProcAddress(LibHandle, 'sk4d_codec_decode');
  sk4d_codec_encode   := GetProcAddress(LibHandle, 'sk4d_codec_encode');
  sk4d_codec_get_info := GetProcAddress(LibHandle, 'sk4d_codec_get_info');
{$ELSE}
function  sk4d_codec_decode;   external SkiaLib;
function  sk4d_codec_encode;   external SkiaLib;
function  sk4d_codec_get_info; external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_colorfilter.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_colorfilter_make_blend                := GetProcAddress(LibHandle, 'sk4d_colorfilter_make_blend');
  sk4d_colorfilter_make_compose              := GetProcAddress(LibHandle, 'sk4d_colorfilter_make_compose');
  sk4d_colorfilter_make_high_contrast        := GetProcAddress(LibHandle, 'sk4d_colorfilter_make_high_contrast');
  sk4d_colorfilter_make_hsla_matrix          := GetProcAddress(LibHandle, 'sk4d_colorfilter_make_hsla_matrix');
  sk4d_colorfilter_make_lerp                 := GetProcAddress(LibHandle, 'sk4d_colorfilter_make_lerp');
  sk4d_colorfilter_make_lighting             := GetProcAddress(LibHandle, 'sk4d_colorfilter_make_lighting');
  sk4d_colorfilter_make_linear_to_srgb_gamma := GetProcAddress(LibHandle, 'sk4d_colorfilter_make_linear_to_srgb_gamma');
  sk4d_colorfilter_make_luma_color           := GetProcAddress(LibHandle, 'sk4d_colorfilter_make_luma_color');
  sk4d_colorfilter_make_matrix               := GetProcAddress(LibHandle, 'sk4d_colorfilter_make_matrix');
  sk4d_colorfilter_make_overdraw             := GetProcAddress(LibHandle, 'sk4d_colorfilter_make_overdraw');
  sk4d_colorfilter_make_srgb_to_linear_gamma := GetProcAddress(LibHandle, 'sk4d_colorfilter_make_srgb_to_linear_gamma');
  sk4d_colorfilter_make_table                := GetProcAddress(LibHandle, 'sk4d_colorfilter_make_table');
{$ELSE}
function  sk4d_colorfilter_make_blend;                external SkiaLib;
function  sk4d_colorfilter_make_compose;              external SkiaLib;
function  sk4d_colorfilter_make_high_contrast;        external SkiaLib;
function  sk4d_colorfilter_make_hsla_matrix;          external SkiaLib;
function  sk4d_colorfilter_make_lerp;                 external SkiaLib;
function  sk4d_colorfilter_make_lighting;             external SkiaLib;
function  sk4d_colorfilter_make_linear_to_srgb_gamma; external SkiaLib;
function  sk4d_colorfilter_make_luma_color;           external SkiaLib;
function  sk4d_colorfilter_make_matrix;               external SkiaLib;
function  sk4d_colorfilter_make_overdraw;             external SkiaLib;
function  sk4d_colorfilter_make_srgb_to_linear_gamma; external SkiaLib;
function  sk4d_colorfilter_make_table;                external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_colorspace.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_colorspace_get_gamma_close_to_srgb  := GetProcAddress(LibHandle, 'sk4d_colorspace_get_gamma_close_to_srgb');
  sk4d_colorspace_get_gamma_is_linear      := GetProcAddress(LibHandle, 'sk4d_colorspace_get_gamma_is_linear');
  sk4d_colorspace_get_gamut                := GetProcAddress(LibHandle, 'sk4d_colorspace_get_gamut');
  sk4d_colorspace_get_inverse_transfer_fn  := GetProcAddress(LibHandle, 'sk4d_colorspace_get_inverse_transfer_fn');
  sk4d_colorspace_get_to_xyz_d50           := GetProcAddress(LibHandle, 'sk4d_colorspace_get_to_xyz_d50');
  sk4d_colorspace_get_transfer_fn          := GetProcAddress(LibHandle, 'sk4d_colorspace_get_transfer_fn');
  sk4d_colorspace_is_equal                 := GetProcAddress(LibHandle, 'sk4d_colorspace_is_equal');
  sk4d_colorspace_is_numerical_transfer_fn := GetProcAddress(LibHandle, 'sk4d_colorspace_is_numerical_transfer_fn');
  sk4d_colorspace_is_srgb                  := GetProcAddress(LibHandle, 'sk4d_colorspace_is_srgb');
  sk4d_colorspace_make                     := GetProcAddress(LibHandle, 'sk4d_colorspace_make');
  sk4d_colorspace_make_color_spin          := GetProcAddress(LibHandle, 'sk4d_colorspace_make_color_spin');
  sk4d_colorspace_make_linear_gamma        := GetProcAddress(LibHandle, 'sk4d_colorspace_make_linear_gamma');
  sk4d_colorspace_make_rgb                 := GetProcAddress(LibHandle, 'sk4d_colorspace_make_rgb');
  sk4d_colorspace_make_srgb                := GetProcAddress(LibHandle, 'sk4d_colorspace_make_srgb');
  sk4d_colorspace_make_srgb_gamma          := GetProcAddress(LibHandle, 'sk4d_colorspace_make_srgb_gamma');
  sk4d_colorspace_make_srgb_linear         := GetProcAddress(LibHandle, 'sk4d_colorspace_make_srgb_linear');
  sk4d_colorspace_ref                      := GetProcAddress(LibHandle, 'sk4d_colorspace_ref');
  sk4d_colorspace_to_profile               := GetProcAddress(LibHandle, 'sk4d_colorspace_to_profile');
  sk4d_colorspace_unref                    := GetProcAddress(LibHandle, 'sk4d_colorspace_unref');
  sk4d_colorspacematrix33_adobe_rgb_gamut  := GetProcAddress(LibHandle, 'sk4d_colorspacematrix33_adobe_rgb_gamut');
  sk4d_colorspacematrix33_display_p3       := GetProcAddress(LibHandle, 'sk4d_colorspacematrix33_display_p3');
  sk4d_colorspacematrix33_rec_2020         := GetProcAddress(LibHandle, 'sk4d_colorspacematrix33_rec_2020');
  sk4d_colorspacematrix33_srgb_gamut       := GetProcAddress(LibHandle, 'sk4d_colorspacematrix33_srgb_gamut');
  sk4d_colorspacematrix33_xyz              := GetProcAddress(LibHandle, 'sk4d_colorspacematrix33_xyz');
  sk4d_colorspaceprimaries_get_to_xyz_d50  := GetProcAddress(LibHandle, 'sk4d_colorspaceprimaries_get_to_xyz_d50');
  sk4d_colorspacetransferfn_hlg            := GetProcAddress(LibHandle, 'sk4d_colorspacetransferfn_hlg');
  sk4d_colorspacetransferfn_linear         := GetProcAddress(LibHandle, 'sk4d_colorspacetransferfn_linear');
  sk4d_colorspacetransferfn_pq             := GetProcAddress(LibHandle, 'sk4d_colorspacetransferfn_pq');
  sk4d_colorspacetransferfn_rec2020        := GetProcAddress(LibHandle, 'sk4d_colorspacetransferfn_rec2020');
  sk4d_colorspacetransferfn_srgb           := GetProcAddress(LibHandle, 'sk4d_colorspacetransferfn_srgb');
  sk4d_colorspacetransferfn_two_dot_two    := GetProcAddress(LibHandle, 'sk4d_colorspacetransferfn_two_dot_two');
{$ELSE}
function  sk4d_colorspace_get_gamma_close_to_srgb;  external SkiaLib;
function  sk4d_colorspace_get_gamma_is_linear;      external SkiaLib;
procedure sk4d_colorspace_get_gamut;                external SkiaLib;
procedure sk4d_colorspace_get_inverse_transfer_fn;  external SkiaLib;
procedure sk4d_colorspace_get_to_xyz_d50;           external SkiaLib;
procedure sk4d_colorspace_get_transfer_fn;          external SkiaLib;
function  sk4d_colorspace_is_equal;                 external SkiaLib;
function  sk4d_colorspace_is_numerical_transfer_fn; external SkiaLib;
function  sk4d_colorspace_is_srgb;                  external SkiaLib;
function  sk4d_colorspace_make;                     external SkiaLib;
function  sk4d_colorspace_make_color_spin;          external SkiaLib;
function  sk4d_colorspace_make_linear_gamma;        external SkiaLib;
function  sk4d_colorspace_make_rgb;                 external SkiaLib;
function  sk4d_colorspace_make_srgb;                external SkiaLib;
function  sk4d_colorspace_make_srgb_gamma;          external SkiaLib;
function  sk4d_colorspace_make_srgb_linear;         external SkiaLib;
procedure sk4d_colorspace_ref;                      external SkiaLib;
procedure sk4d_colorspace_to_profile;               external SkiaLib;
procedure sk4d_colorspace_unref;                    external SkiaLib;
procedure sk4d_colorspacematrix33_adobe_rgb_gamut;  external SkiaLib;
procedure sk4d_colorspacematrix33_display_p3;       external SkiaLib;
procedure sk4d_colorspacematrix33_rec_2020;         external SkiaLib;
procedure sk4d_colorspacematrix33_srgb_gamut;       external SkiaLib;
procedure sk4d_colorspacematrix33_xyz;              external SkiaLib;
function  sk4d_colorspaceprimaries_get_to_xyz_d50;  external SkiaLib;
procedure sk4d_colorspacetransferfn_hlg;            external SkiaLib;
procedure sk4d_colorspacetransferfn_linear;         external SkiaLib;
procedure sk4d_colorspacetransferfn_pq;             external SkiaLib;
procedure sk4d_colorspacetransferfn_rec2020;        external SkiaLib;
procedure sk4d_colorspacetransferfn_srgb;           external SkiaLib;
procedure sk4d_colorspacetransferfn_two_dot_two;    external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_data.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_data_get_data           := GetProcAddress(LibHandle, 'sk4d_data_get_data');
  sk4d_data_get_size           := GetProcAddress(LibHandle, 'sk4d_data_get_size');
  sk4d_data_make               := GetProcAddress(LibHandle, 'sk4d_data_make');
  sk4d_data_make_from_file     := GetProcAddress(LibHandle, 'sk4d_data_make_from_file');
  sk4d_data_make_from_stream   := GetProcAddress(LibHandle, 'sk4d_data_make_from_stream');
  sk4d_data_make_uninitialized := GetProcAddress(LibHandle, 'sk4d_data_make_uninitialized');
  sk4d_data_make_with_copy     := GetProcAddress(LibHandle, 'sk4d_data_make_with_copy');
  sk4d_data_ref                := GetProcAddress(LibHandle, 'sk4d_data_ref');
  sk4d_data_unref              := GetProcAddress(LibHandle, 'sk4d_data_unref');
{$ELSE}
function  sk4d_data_get_data;           external SkiaLib;
function  sk4d_data_get_size;           external SkiaLib;
function  sk4d_data_make;               external SkiaLib;
function  sk4d_data_make_from_file;     external SkiaLib;
function  sk4d_data_make_from_stream;   external SkiaLib;
function  sk4d_data_make_uninitialized; external SkiaLib;
function  sk4d_data_make_with_copy;     external SkiaLib;
procedure sk4d_data_ref;                external SkiaLib;
procedure sk4d_data_unref;              external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_debugf.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_set_debug_msg_proc := GetProcAddress(LibHandle, 'sk4d_set_debug_msg_proc');
{$ELSE}
procedure sk4d_set_debug_msg_proc; external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_document.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_document_begin_page := GetProcAddress(LibHandle, 'sk4d_document_begin_page');
  sk4d_document_close      := GetProcAddress(LibHandle, 'sk4d_document_close');
  sk4d_document_end_page   := GetProcAddress(LibHandle, 'sk4d_document_end_page');
  sk4d_document_make_pdf   := GetProcAddress(LibHandle, 'sk4d_document_make_pdf');
  sk4d_document_make_pdf2  := GetProcAddress(LibHandle, 'sk4d_document_make_pdf2');
  sk4d_document_terminate  := GetProcAddress(LibHandle, 'sk4d_document_terminate');
{$ELSE}
function  sk4d_document_begin_page; external SkiaLib;
procedure sk4d_document_close;      external SkiaLib;
procedure sk4d_document_end_page;   external SkiaLib;
function  sk4d_document_make_pdf;   external SkiaLib;
function  sk4d_document_make_pdf2;  external SkiaLib;
procedure sk4d_document_terminate;  external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_font.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_font_create                  := GetProcAddress(LibHandle, 'sk4d_font_create');
  sk4d_font_create2                 := GetProcAddress(LibHandle, 'sk4d_font_create2');
  sk4d_font_destroy                 := GetProcAddress(LibHandle, 'sk4d_font_destroy');
  sk4d_font_get_baseline_snap       := GetProcAddress(LibHandle, 'sk4d_font_get_baseline_snap');
  sk4d_font_get_edging              := GetProcAddress(LibHandle, 'sk4d_font_get_edging');
  sk4d_font_get_embedded_bitmaps    := GetProcAddress(LibHandle, 'sk4d_font_get_embedded_bitmaps');
  sk4d_font_get_embolden            := GetProcAddress(LibHandle, 'sk4d_font_get_embolden');
  sk4d_font_get_force_auto_hinting  := GetProcAddress(LibHandle, 'sk4d_font_get_force_auto_hinting');
  sk4d_font_get_glyphs              := GetProcAddress(LibHandle, 'sk4d_font_get_glyphs');
  sk4d_font_get_glyphs_count        := GetProcAddress(LibHandle, 'sk4d_font_get_glyphs_count');
  sk4d_font_get_hinting             := GetProcAddress(LibHandle, 'sk4d_font_get_hinting');
  sk4d_font_get_intercepts          := GetProcAddress(LibHandle, 'sk4d_font_get_intercepts');
  sk4d_font_get_linear_metrics      := GetProcAddress(LibHandle, 'sk4d_font_get_linear_metrics');
  sk4d_font_get_metrics             := GetProcAddress(LibHandle, 'sk4d_font_get_metrics');
  sk4d_font_get_offsets             := GetProcAddress(LibHandle, 'sk4d_font_get_offsets');
  sk4d_font_get_path                := GetProcAddress(LibHandle, 'sk4d_font_get_path');
  sk4d_font_get_paths               := GetProcAddress(LibHandle, 'sk4d_font_get_paths');
  sk4d_font_get_positions           := GetProcAddress(LibHandle, 'sk4d_font_get_positions');
  sk4d_font_get_scale_x             := GetProcAddress(LibHandle, 'sk4d_font_get_scale_x');
  sk4d_font_get_size                := GetProcAddress(LibHandle, 'sk4d_font_get_size');
  sk4d_font_get_skew_x              := GetProcAddress(LibHandle, 'sk4d_font_get_skew_x');
  sk4d_font_get_subpixel            := GetProcAddress(LibHandle, 'sk4d_font_get_subpixel');
  sk4d_font_get_typeface            := GetProcAddress(LibHandle, 'sk4d_font_get_typeface');
  sk4d_font_get_typeface_or_default := GetProcAddress(LibHandle, 'sk4d_font_get_typeface_or_default');
  sk4d_font_get_widths_bounds       := GetProcAddress(LibHandle, 'sk4d_font_get_widths_bounds');
  sk4d_font_is_equal                := GetProcAddress(LibHandle, 'sk4d_font_is_equal');
  sk4d_font_measure_text            := GetProcAddress(LibHandle, 'sk4d_font_measure_text');
  sk4d_font_set_baseline_snap       := GetProcAddress(LibHandle, 'sk4d_font_set_baseline_snap');
  sk4d_font_set_edging              := GetProcAddress(LibHandle, 'sk4d_font_set_edging');
  sk4d_font_set_embedded_bitmaps    := GetProcAddress(LibHandle, 'sk4d_font_set_embedded_bitmaps');
  sk4d_font_set_embolden            := GetProcAddress(LibHandle, 'sk4d_font_set_embolden');
  sk4d_font_set_force_auto_hinting  := GetProcAddress(LibHandle, 'sk4d_font_set_force_auto_hinting');
  sk4d_font_set_hinting             := GetProcAddress(LibHandle, 'sk4d_font_set_hinting');
  sk4d_font_set_linear_metrics      := GetProcAddress(LibHandle, 'sk4d_font_set_linear_metrics');
  sk4d_font_set_scale_x             := GetProcAddress(LibHandle, 'sk4d_font_set_scale_x');
  sk4d_font_set_size                := GetProcAddress(LibHandle, 'sk4d_font_set_size');
  sk4d_font_set_skew_x              := GetProcAddress(LibHandle, 'sk4d_font_set_skew_x');
  sk4d_font_set_subpixel            := GetProcAddress(LibHandle, 'sk4d_font_set_subpixel');
  sk4d_font_set_typeface            := GetProcAddress(LibHandle, 'sk4d_font_set_typeface');
  sk4d_font_unichar_to_glyph        := GetProcAddress(LibHandle, 'sk4d_font_unichar_to_glyph');
  sk4d_font_unichars_to_glyphs      := GetProcAddress(LibHandle, 'sk4d_font_unichars_to_glyphs');
{$ELSE}
function  sk4d_font_create;                  external SkiaLib;
function  sk4d_font_create2;                 external SkiaLib;
procedure sk4d_font_destroy;                 external SkiaLib;
function  sk4d_font_get_baseline_snap;       external SkiaLib;
function  sk4d_font_get_edging;              external SkiaLib;
function  sk4d_font_get_embedded_bitmaps;    external SkiaLib;
function  sk4d_font_get_embolden;            external SkiaLib;
function  sk4d_font_get_force_auto_hinting;  external SkiaLib;
function  sk4d_font_get_glyphs;              external SkiaLib;
function  sk4d_font_get_glyphs_count;        external SkiaLib;
function  sk4d_font_get_hinting;             external SkiaLib;
function  sk4d_font_get_intercepts;          external SkiaLib;
function  sk4d_font_get_linear_metrics;      external SkiaLib;
function  sk4d_font_get_metrics;             external SkiaLib;
procedure sk4d_font_get_offsets;             external SkiaLib;
function  sk4d_font_get_path;                external SkiaLib;
procedure sk4d_font_get_paths;               external SkiaLib;
procedure sk4d_font_get_positions;           external SkiaLib;
function  sk4d_font_get_scale_x;             external SkiaLib;
function  sk4d_font_get_size;                external SkiaLib;
function  sk4d_font_get_skew_x;              external SkiaLib;
function  sk4d_font_get_subpixel;            external SkiaLib;
function  sk4d_font_get_typeface;            external SkiaLib;
function  sk4d_font_get_typeface_or_default; external SkiaLib;
procedure sk4d_font_get_widths_bounds;       external SkiaLib;
function  sk4d_font_is_equal;                external SkiaLib;
function  sk4d_font_measure_text;            external SkiaLib;
procedure sk4d_font_set_baseline_snap;       external SkiaLib;
procedure sk4d_font_set_edging;              external SkiaLib;
procedure sk4d_font_set_embedded_bitmaps;    external SkiaLib;
procedure sk4d_font_set_embolden;            external SkiaLib;
procedure sk4d_font_set_force_auto_hinting;  external SkiaLib;
procedure sk4d_font_set_hinting;             external SkiaLib;
procedure sk4d_font_set_linear_metrics;      external SkiaLib;
procedure sk4d_font_set_scale_x;             external SkiaLib;
procedure sk4d_font_set_size;                external SkiaLib;
procedure sk4d_font_set_skew_x;              external SkiaLib;
procedure sk4d_font_set_subpixel;            external SkiaLib;
procedure sk4d_font_set_typeface;            external SkiaLib;
function  sk4d_font_unichar_to_glyph;        external SkiaLib;
procedure sk4d_font_unichars_to_glyphs;      external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_fontstyle.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_fontstyle_create     := GetProcAddress(LibHandle, 'sk4d_fontstyle_create');
  sk4d_fontstyle_destroy    := GetProcAddress(LibHandle, 'sk4d_fontstyle_destroy');
  sk4d_fontstyle_get_slant  := GetProcAddress(LibHandle, 'sk4d_fontstyle_get_slant');
  sk4d_fontstyle_get_weight := GetProcAddress(LibHandle, 'sk4d_fontstyle_get_weight');
  sk4d_fontstyle_get_width  := GetProcAddress(LibHandle, 'sk4d_fontstyle_get_width');
{$ELSE}
function  sk4d_fontstyle_create;     external SkiaLib;
procedure sk4d_fontstyle_destroy;    external SkiaLib;
function  sk4d_fontstyle_get_slant;  external SkiaLib;
function  sk4d_fontstyle_get_weight; external SkiaLib;
function  sk4d_fontstyle_get_width;  external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_graphics.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_graphics_allow_jit                                       := GetProcAddress(LibHandle, 'sk4d_graphics_allow_jit');
  sk4d_graphics_dump_memory_statistics                          := GetProcAddress(LibHandle, 'sk4d_graphics_dump_memory_statistics');
  sk4d_graphics_get_font_cache_count_limit                      := GetProcAddress(LibHandle, 'sk4d_graphics_get_font_cache_count_limit');
  sk4d_graphics_get_font_cache_count_used                       := GetProcAddress(LibHandle, 'sk4d_graphics_get_font_cache_count_used');
  sk4d_graphics_get_font_cache_limit                            := GetProcAddress(LibHandle, 'sk4d_graphics_get_font_cache_limit');
  sk4d_graphics_get_font_cache_used                             := GetProcAddress(LibHandle, 'sk4d_graphics_get_font_cache_used');
  sk4d_graphics_get_resource_cache_single_allocation_byte_limit := GetProcAddress(LibHandle, 'sk4d_graphics_get_resource_cache_single_allocation_byte_limit');
  sk4d_graphics_get_resource_cache_total_byte_limit             := GetProcAddress(LibHandle, 'sk4d_graphics_get_resource_cache_total_byte_limit');
  sk4d_graphics_get_resource_cache_total_bytes_used             := GetProcAddress(LibHandle, 'sk4d_graphics_get_resource_cache_total_bytes_used');
  sk4d_graphics_init                                            := GetProcAddress(LibHandle, 'sk4d_graphics_init');
  sk4d_graphics_purge_all_caches                                := GetProcAddress(LibHandle, 'sk4d_graphics_purge_all_caches');
  sk4d_graphics_purge_font_cache                                := GetProcAddress(LibHandle, 'sk4d_graphics_purge_font_cache');
  sk4d_graphics_purge_resource_cache                            := GetProcAddress(LibHandle, 'sk4d_graphics_purge_resource_cache');
  sk4d_graphics_set_font_cache_count_limit                      := GetProcAddress(LibHandle, 'sk4d_graphics_set_font_cache_count_limit');
  sk4d_graphics_set_font_cache_limit                            := GetProcAddress(LibHandle, 'sk4d_graphics_set_font_cache_limit');
  sk4d_graphics_set_resource_cache_single_allocation_byte_limit := GetProcAddress(LibHandle, 'sk4d_graphics_set_resource_cache_single_allocation_byte_limit');
  sk4d_graphics_set_resource_cache_total_byte_limit             := GetProcAddress(LibHandle, 'sk4d_graphics_set_resource_cache_total_byte_limit');
{$ELSE}
procedure sk4d_graphics_allow_jit;                                       external SkiaLib;
procedure sk4d_graphics_dump_memory_statistics;                          external SkiaLib;
function  sk4d_graphics_get_font_cache_count_limit;                      external SkiaLib;
function  sk4d_graphics_get_font_cache_count_used;                       external SkiaLib;
function  sk4d_graphics_get_font_cache_limit;                            external SkiaLib;
function  sk4d_graphics_get_font_cache_used;                             external SkiaLib;
function  sk4d_graphics_get_resource_cache_single_allocation_byte_limit; external SkiaLib;
function  sk4d_graphics_get_resource_cache_total_byte_limit;             external SkiaLib;
function  sk4d_graphics_get_resource_cache_total_bytes_used;             external SkiaLib;
procedure sk4d_graphics_init;                                            external SkiaLib;
procedure sk4d_graphics_purge_all_caches;                                external SkiaLib;
procedure sk4d_graphics_purge_font_cache;                                external SkiaLib;
procedure sk4d_graphics_purge_resource_cache;                            external SkiaLib;
function  sk4d_graphics_set_font_cache_count_limit;                      external SkiaLib;
function  sk4d_graphics_set_font_cache_limit;                            external SkiaLib;
function  sk4d_graphics_set_resource_cache_single_allocation_byte_limit; external SkiaLib;
function  sk4d_graphics_set_resource_cache_total_byte_limit;             external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_image.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_image_encode_to_data            := GetProcAddress(LibHandle, 'sk4d_image_encode_to_data');
  sk4d_image_encode_to_data2           := GetProcAddress(LibHandle, 'sk4d_image_encode_to_data2');
  sk4d_image_get_alpha_type            := GetProcAddress(LibHandle, 'sk4d_image_get_alpha_type');
  sk4d_image_get_color_space           := GetProcAddress(LibHandle, 'sk4d_image_get_color_space');
  sk4d_image_get_color_type            := GetProcAddress(LibHandle, 'sk4d_image_get_color_type');
  sk4d_image_get_encoded_data          := GetProcAddress(LibHandle, 'sk4d_image_get_encoded_data');
  sk4d_image_get_height                := GetProcAddress(LibHandle, 'sk4d_image_get_height');
  sk4d_image_get_image_info            := GetProcAddress(LibHandle, 'sk4d_image_get_image_info');
  sk4d_image_get_unique_id             := GetProcAddress(LibHandle, 'sk4d_image_get_unique_id');
  sk4d_image_get_width                 := GetProcAddress(LibHandle, 'sk4d_image_get_width');
  sk4d_image_is_lazy_generated         := GetProcAddress(LibHandle, 'sk4d_image_is_lazy_generated');
  sk4d_image_is_texture_backed         := GetProcAddress(LibHandle, 'sk4d_image_is_texture_backed');
  sk4d_image_is_valid                  := GetProcAddress(LibHandle, 'sk4d_image_is_valid');
  sk4d_image_make_colorspace           := GetProcAddress(LibHandle, 'sk4d_image_make_colorspace');
  sk4d_image_make_from_adopted_texture := GetProcAddress(LibHandle, 'sk4d_image_make_from_adopted_texture');
  sk4d_image_make_from_encoded_data    := GetProcAddress(LibHandle, 'sk4d_image_make_from_encoded_data');
  sk4d_image_make_from_picture         := GetProcAddress(LibHandle, 'sk4d_image_make_from_picture');
  sk4d_image_make_from_picture2        := GetProcAddress(LibHandle, 'sk4d_image_make_from_picture2');
  sk4d_image_make_from_raster          := GetProcAddress(LibHandle, 'sk4d_image_make_from_raster');
  sk4d_image_make_from_texture         := GetProcAddress(LibHandle, 'sk4d_image_make_from_texture');
  sk4d_image_make_non_texture_image    := GetProcAddress(LibHandle, 'sk4d_image_make_non_texture_image');
  sk4d_image_make_raster_copy          := GetProcAddress(LibHandle, 'sk4d_image_make_raster_copy');
  sk4d_image_make_raster_image         := GetProcAddress(LibHandle, 'sk4d_image_make_raster_image');
  sk4d_image_make_shader               := GetProcAddress(LibHandle, 'sk4d_image_make_shader');
  sk4d_image_make_subset               := GetProcAddress(LibHandle, 'sk4d_image_make_subset');
  sk4d_image_make_texture_image        := GetProcAddress(LibHandle, 'sk4d_image_make_texture_image');
  sk4d_image_make_with_filter          := GetProcAddress(LibHandle, 'sk4d_image_make_with_filter');
  sk4d_image_read_pixels               := GetProcAddress(LibHandle, 'sk4d_image_read_pixels');
  sk4d_image_scale_pixels              := GetProcAddress(LibHandle, 'sk4d_image_scale_pixels');
{$ELSE}
function  sk4d_image_encode_to_data;            external SkiaLib;
function  sk4d_image_encode_to_data2;           external SkiaLib;
function  sk4d_image_get_alpha_type;            external SkiaLib;
function  sk4d_image_get_color_space;           external SkiaLib;
function  sk4d_image_get_color_type;            external SkiaLib;
function  sk4d_image_get_encoded_data;          external SkiaLib;
function  sk4d_image_get_height;                external SkiaLib;
procedure sk4d_image_get_image_info;            external SkiaLib;
function  sk4d_image_get_unique_id;             external SkiaLib;
function  sk4d_image_get_width;                 external SkiaLib;
function  sk4d_image_is_lazy_generated;         external SkiaLib;
function  sk4d_image_is_texture_backed;         external SkiaLib;
function  sk4d_image_is_valid;                  external SkiaLib;
function  sk4d_image_make_colorspace;           external SkiaLib;
function  sk4d_image_make_from_adopted_texture; external SkiaLib;
function  sk4d_image_make_from_encoded_data;    external SkiaLib;
function  sk4d_image_make_from_picture;         external SkiaLib;
function  sk4d_image_make_from_picture2;        external SkiaLib;
function  sk4d_image_make_from_raster;          external SkiaLib;
function  sk4d_image_make_from_texture;         external SkiaLib;
function  sk4d_image_make_non_texture_image;    external SkiaLib;
function  sk4d_image_make_raster_copy;          external SkiaLib;
function  sk4d_image_make_raster_image;         external SkiaLib;
function  sk4d_image_make_shader;               external SkiaLib;
function  sk4d_image_make_subset;               external SkiaLib;
function  sk4d_image_make_texture_image;        external SkiaLib;
function  sk4d_image_make_with_filter;          external SkiaLib;
function  sk4d_image_read_pixels;               external SkiaLib;
function  sk4d_image_scale_pixels;              external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_imagefilter.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_imagefilter_make_alpha_threshold        := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_alpha_threshold');
  sk4d_imagefilter_make_arithmetic             := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_arithmetic');
  sk4d_imagefilter_make_blend                  := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_blend');
  sk4d_imagefilter_make_blur                   := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_blur');
  sk4d_imagefilter_make_colorfilter            := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_colorfilter');
  sk4d_imagefilter_make_compose                := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_compose');
  sk4d_imagefilter_make_dilate                 := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_dilate');
  sk4d_imagefilter_make_displacement_map       := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_displacement_map');
  sk4d_imagefilter_make_distant_light_diffuse  := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_distant_light_diffuse');
  sk4d_imagefilter_make_distant_light_specular := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_distant_light_specular');
  sk4d_imagefilter_make_drop_shadow            := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_drop_shadow');
  sk4d_imagefilter_make_drop_shadow_only       := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_drop_shadow_only');
  sk4d_imagefilter_make_erode                  := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_erode');
  sk4d_imagefilter_make_image                  := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_image');
  sk4d_imagefilter_make_magnifier              := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_magnifier');
  sk4d_imagefilter_make_matrix_convolution     := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_matrix_convolution');
  sk4d_imagefilter_make_matrix_transform       := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_matrix_transform');
  sk4d_imagefilter_make_merge                  := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_merge');
  sk4d_imagefilter_make_offset                 := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_offset');
  sk4d_imagefilter_make_picture                := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_picture');
  sk4d_imagefilter_make_point_light_diffuse    := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_point_light_diffuse');
  sk4d_imagefilter_make_point_light_specular   := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_point_light_specular');
  sk4d_imagefilter_make_shader                 := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_shader');
  sk4d_imagefilter_make_spot_light_diffuse     := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_spot_light_diffuse');
  sk4d_imagefilter_make_spot_light_specular    := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_spot_light_specular');
  sk4d_imagefilter_make_tile                   := GetProcAddress(LibHandle, 'sk4d_imagefilter_make_tile');
{$ELSE}
function  sk4d_imagefilter_make_alpha_threshold;        external SkiaLib;
function  sk4d_imagefilter_make_arithmetic;             external SkiaLib;
function  sk4d_imagefilter_make_blend;                  external SkiaLib;
function  sk4d_imagefilter_make_blur;                   external SkiaLib;
function  sk4d_imagefilter_make_colorfilter;            external SkiaLib;
function  sk4d_imagefilter_make_compose;                external SkiaLib;
function  sk4d_imagefilter_make_dilate;                 external SkiaLib;
function  sk4d_imagefilter_make_displacement_map;       external SkiaLib;
function  sk4d_imagefilter_make_distant_light_diffuse;  external SkiaLib;
function  sk4d_imagefilter_make_distant_light_specular; external SkiaLib;
function  sk4d_imagefilter_make_drop_shadow;            external SkiaLib;
function  sk4d_imagefilter_make_drop_shadow_only;       external SkiaLib;
function  sk4d_imagefilter_make_erode;                  external SkiaLib;
function  sk4d_imagefilter_make_image;                  external SkiaLib;
function  sk4d_imagefilter_make_magnifier;              external SkiaLib;
function  sk4d_imagefilter_make_matrix_convolution;     external SkiaLib;
function  sk4d_imagefilter_make_matrix_transform;       external SkiaLib;
function  sk4d_imagefilter_make_merge;                  external SkiaLib;
function  sk4d_imagefilter_make_offset;                 external SkiaLib;
function  sk4d_imagefilter_make_picture;                external SkiaLib;
function  sk4d_imagefilter_make_point_light_diffuse;    external SkiaLib;
function  sk4d_imagefilter_make_point_light_specular;   external SkiaLib;
function  sk4d_imagefilter_make_shader;                 external SkiaLib;
function  sk4d_imagefilter_make_spot_light_diffuse;     external SkiaLib;
function  sk4d_imagefilter_make_spot_light_specular;    external SkiaLib;
function  sk4d_imagefilter_make_tile;                   external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_maskfilter.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_maskfilter_make_blur        := GetProcAddress(LibHandle, 'sk4d_maskfilter_make_blur');
  sk4d_maskfilter_make_shader      := GetProcAddress(LibHandle, 'sk4d_maskfilter_make_shader');
  sk4d_maskfilter_make_table       := GetProcAddress(LibHandle, 'sk4d_maskfilter_make_table');
  sk4d_maskfilter_make_table_clip  := GetProcAddress(LibHandle, 'sk4d_maskfilter_make_table_clip');
  sk4d_maskfilter_make_table_gamma := GetProcAddress(LibHandle, 'sk4d_maskfilter_make_table_gamma');
{$ELSE}
function  sk4d_maskfilter_make_blur;        external SkiaLib;
function  sk4d_maskfilter_make_shader;      external SkiaLib;
function  sk4d_maskfilter_make_table;       external SkiaLib;
function  sk4d_maskfilter_make_table_clip;  external SkiaLib;
function  sk4d_maskfilter_make_table_gamma; external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_paint.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_paint_as_blend_mode    := GetProcAddress(LibHandle, 'sk4d_paint_as_blend_mode');
  sk4d_paint_create           := GetProcAddress(LibHandle, 'sk4d_paint_create');
  sk4d_paint_create2          := GetProcAddress(LibHandle, 'sk4d_paint_create2');
  sk4d_paint_destroy          := GetProcAddress(LibHandle, 'sk4d_paint_destroy');
  sk4d_paint_get_alpha        := GetProcAddress(LibHandle, 'sk4d_paint_get_alpha');
  sk4d_paint_get_alphaf       := GetProcAddress(LibHandle, 'sk4d_paint_get_alphaf');
  sk4d_paint_get_anti_alias   := GetProcAddress(LibHandle, 'sk4d_paint_get_anti_alias');
  sk4d_paint_get_color        := GetProcAddress(LibHandle, 'sk4d_paint_get_color');
  sk4d_paint_get_colorf       := GetProcAddress(LibHandle, 'sk4d_paint_get_colorf');
  sk4d_paint_get_color_filter := GetProcAddress(LibHandle, 'sk4d_paint_get_color_filter');
  sk4d_paint_get_dither       := GetProcAddress(LibHandle, 'sk4d_paint_get_dither');
  sk4d_paint_get_fill_path    := GetProcAddress(LibHandle, 'sk4d_paint_get_fill_path');
  sk4d_paint_get_image_filter := GetProcAddress(LibHandle, 'sk4d_paint_get_image_filter');
  sk4d_paint_get_mask_filter  := GetProcAddress(LibHandle, 'sk4d_paint_get_mask_filter');
  sk4d_paint_get_path_effect  := GetProcAddress(LibHandle, 'sk4d_paint_get_path_effect');
  sk4d_paint_get_shader       := GetProcAddress(LibHandle, 'sk4d_paint_get_shader');
  sk4d_paint_get_stroke_cap   := GetProcAddress(LibHandle, 'sk4d_paint_get_stroke_cap');
  sk4d_paint_get_stroke_join  := GetProcAddress(LibHandle, 'sk4d_paint_get_stroke_join');
  sk4d_paint_get_stroke_miter := GetProcAddress(LibHandle, 'sk4d_paint_get_stroke_miter');
  sk4d_paint_get_stroke_width := GetProcAddress(LibHandle, 'sk4d_paint_get_stroke_width');
  sk4d_paint_get_style        := GetProcAddress(LibHandle, 'sk4d_paint_get_style');
  sk4d_paint_reset            := GetProcAddress(LibHandle, 'sk4d_paint_reset');
  sk4d_paint_set_alpha        := GetProcAddress(LibHandle, 'sk4d_paint_set_alpha');
  sk4d_paint_set_alphaf       := GetProcAddress(LibHandle, 'sk4d_paint_set_alphaf');
  sk4d_paint_set_antialias    := GetProcAddress(LibHandle, 'sk4d_paint_set_antialias');
  sk4d_paint_set_argb         := GetProcAddress(LibHandle, 'sk4d_paint_set_argb');
  sk4d_paint_set_blend_mode   := GetProcAddress(LibHandle, 'sk4d_paint_set_blend_mode');
  sk4d_paint_set_color        := GetProcAddress(LibHandle, 'sk4d_paint_set_color');
  sk4d_paint_set_colorf       := GetProcAddress(LibHandle, 'sk4d_paint_set_colorf');
  sk4d_paint_set_color_filter := GetProcAddress(LibHandle, 'sk4d_paint_set_color_filter');
  sk4d_paint_set_dither       := GetProcAddress(LibHandle, 'sk4d_paint_set_dither');
  sk4d_paint_set_image_filter := GetProcAddress(LibHandle, 'sk4d_paint_set_image_filter');
  sk4d_paint_set_mask_filter  := GetProcAddress(LibHandle, 'sk4d_paint_set_mask_filter');
  sk4d_paint_set_path_effect  := GetProcAddress(LibHandle, 'sk4d_paint_set_path_effect');
  sk4d_paint_set_shader       := GetProcAddress(LibHandle, 'sk4d_paint_set_shader');
  sk4d_paint_set_stroke_cap   := GetProcAddress(LibHandle, 'sk4d_paint_set_stroke_cap');
  sk4d_paint_set_stroke_join  := GetProcAddress(LibHandle, 'sk4d_paint_set_stroke_join');
  sk4d_paint_set_stroke_miter := GetProcAddress(LibHandle, 'sk4d_paint_set_stroke_miter');
  sk4d_paint_set_stroke_width := GetProcAddress(LibHandle, 'sk4d_paint_set_stroke_width');
  sk4d_paint_set_style        := GetProcAddress(LibHandle, 'sk4d_paint_set_style');
{$ELSE}
function sk4d_paint_as_blend_mode;     external SkiaLib;
function sk4d_paint_create;            external SkiaLib;
function  sk4d_paint_create2;          external SkiaLib;
procedure sk4d_paint_destroy;          external SkiaLib;
function  sk4d_paint_get_alpha;        external SkiaLib;
function  sk4d_paint_get_alphaf;       external SkiaLib;
function  sk4d_paint_get_anti_alias;   external SkiaLib;
function  sk4d_paint_get_color;        external SkiaLib;
procedure sk4d_paint_get_colorf;       external SkiaLib;
function  sk4d_paint_get_color_filter; external SkiaLib;
function  sk4d_paint_get_dither;       external SkiaLib;
function  sk4d_paint_get_fill_path;    external SkiaLib;
function  sk4d_paint_get_image_filter; external SkiaLib;
function  sk4d_paint_get_mask_filter;  external SkiaLib;
function  sk4d_paint_get_path_effect;  external SkiaLib;
function  sk4d_paint_get_shader;       external SkiaLib;
function  sk4d_paint_get_stroke_cap;   external SkiaLib;
function  sk4d_paint_get_stroke_join;  external SkiaLib;
function  sk4d_paint_get_stroke_miter; external SkiaLib;
function  sk4d_paint_get_stroke_width; external SkiaLib;
function  sk4d_paint_get_style;        external SkiaLib;
procedure sk4d_paint_reset;            external SkiaLib;
procedure sk4d_paint_set_alpha;        external SkiaLib;
procedure sk4d_paint_set_alphaf;       external SkiaLib;
procedure sk4d_paint_set_antialias;    external SkiaLib;
procedure sk4d_paint_set_argb;         external SkiaLib;
procedure sk4d_paint_set_blend_mode;   external SkiaLib;
procedure sk4d_paint_set_color;        external SkiaLib;
procedure sk4d_paint_set_colorf;       external SkiaLib;
procedure sk4d_paint_set_color_filter; external SkiaLib;
procedure sk4d_paint_set_dither;       external SkiaLib;
procedure sk4d_paint_set_image_filter; external SkiaLib;
procedure sk4d_paint_set_mask_filter;  external SkiaLib;
procedure sk4d_paint_set_path_effect;  external SkiaLib;
procedure sk4d_paint_set_shader;       external SkiaLib;
procedure sk4d_paint_set_stroke_cap;   external SkiaLib;
procedure sk4d_paint_set_stroke_join;  external SkiaLib;
procedure sk4d_paint_set_stroke_miter; external SkiaLib;
procedure sk4d_paint_set_stroke_width; external SkiaLib;
procedure sk4d_paint_set_style;        external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_path.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_opbuilder_add                 := GetProcAddress(LibHandle, 'sk4d_opbuilder_add');
  sk4d_opbuilder_create              := GetProcAddress(LibHandle, 'sk4d_opbuilder_create');
  sk4d_opbuilder_destroy             := GetProcAddress(LibHandle, 'sk4d_opbuilder_destroy');
  sk4d_opbuilder_detach              := GetProcAddress(LibHandle, 'sk4d_opbuilder_detach');
  sk4d_path_contains                 := GetProcAddress(LibHandle, 'sk4d_path_contains');
  sk4d_path_create                   := GetProcAddress(LibHandle, 'sk4d_path_create');
  sk4d_path_create2                  := GetProcAddress(LibHandle, 'sk4d_path_create2');
  sk4d_path_create3                  := GetProcAddress(LibHandle, 'sk4d_path_create3');
  sk4d_path_destroy                  := GetProcAddress(LibHandle, 'sk4d_path_destroy');
  sk4d_path_get_bounds               := GetProcAddress(LibHandle, 'sk4d_path_get_bounds');
  sk4d_path_get_fill_type            := GetProcAddress(LibHandle, 'sk4d_path_get_fill_type');
  sk4d_path_get_last_point           := GetProcAddress(LibHandle, 'sk4d_path_get_last_point');
  sk4d_path_get_segment_masks        := GetProcAddress(LibHandle, 'sk4d_path_get_segment_masks');
  sk4d_path_get_tight_bounds         := GetProcAddress(LibHandle, 'sk4d_path_get_tight_bounds');
  sk4d_path_interpolate              := GetProcAddress(LibHandle, 'sk4d_path_interpolate');
  sk4d_path_is_convex                := GetProcAddress(LibHandle, 'sk4d_path_is_convex');
  sk4d_path_is_empty                 := GetProcAddress(LibHandle, 'sk4d_path_is_empty');
  sk4d_path_is_equal                 := GetProcAddress(LibHandle, 'sk4d_path_is_equal');
  sk4d_path_is_finite                := GetProcAddress(LibHandle, 'sk4d_path_is_finite');
  sk4d_path_is_interpolatable        := GetProcAddress(LibHandle, 'sk4d_path_is_interpolatable');
  sk4d_path_is_last_contour_closed   := GetProcAddress(LibHandle, 'sk4d_path_is_last_contour_closed');
  sk4d_path_is_line                  := GetProcAddress(LibHandle, 'sk4d_path_is_line');
  sk4d_path_is_oval                  := GetProcAddress(LibHandle, 'sk4d_path_is_oval');
  sk4d_path_is_rect                  := GetProcAddress(LibHandle, 'sk4d_path_is_rect');
  sk4d_path_is_rrect                 := GetProcAddress(LibHandle, 'sk4d_path_is_rrect');
  sk4d_path_offset                   := GetProcAddress(LibHandle, 'sk4d_path_offset');
  sk4d_path_op                       := GetProcAddress(LibHandle, 'sk4d_path_op');
  sk4d_path_to_svg                   := GetProcAddress(LibHandle, 'sk4d_path_to_svg');
  sk4d_path_transform                := GetProcAddress(LibHandle, 'sk4d_path_transform');
  sk4d_pathiterator_create           := GetProcAddress(LibHandle, 'sk4d_pathiterator_create');
  sk4d_pathiterator_destroy          := GetProcAddress(LibHandle, 'sk4d_pathiterator_destroy');
  sk4d_pathiterator_get_conic_weight := GetProcAddress(LibHandle, 'sk4d_pathiterator_get_conic_weight');
  sk4d_pathiterator_next             := GetProcAddress(LibHandle, 'sk4d_pathiterator_next');
{$ELSE}
procedure sk4d_opbuilder_add;                 external SkiaLib;
function  sk4d_opbuilder_create;              external SkiaLib;
procedure sk4d_opbuilder_destroy;             external SkiaLib;
function  sk4d_opbuilder_detach;              external SkiaLib;
function  sk4d_path_contains;                 external SkiaLib;
function  sk4d_path_create;                   external SkiaLib;
function  sk4d_path_create2;                  external SkiaLib;
function  sk4d_path_create3;                  external SkiaLib;
procedure sk4d_path_destroy;                  external SkiaLib;
procedure sk4d_path_get_bounds;               external SkiaLib;
function  sk4d_path_get_fill_type;            external SkiaLib;
function  sk4d_path_get_last_point;           external SkiaLib;
function  sk4d_path_get_segment_masks;        external SkiaLib;
procedure sk4d_path_get_tight_bounds;         external SkiaLib;
function  sk4d_path_interpolate;              external SkiaLib;
function  sk4d_path_is_convex;                external SkiaLib;
function  sk4d_path_is_empty;                 external SkiaLib;
function  sk4d_path_is_equal;                 external SkiaLib;
function  sk4d_path_is_finite;                external SkiaLib;
function  sk4d_path_is_interpolatable;        external SkiaLib;
function  sk4d_path_is_last_contour_closed;   external SkiaLib;
function  sk4d_path_is_line;                  external SkiaLib;
function  sk4d_path_is_oval;                  external SkiaLib;
function  sk4d_path_is_rect;                  external SkiaLib;
function  sk4d_path_is_rrect;                 external SkiaLib;
procedure sk4d_path_offset;                   external SkiaLib;
function  sk4d_path_op;                       external SkiaLib;
procedure sk4d_path_to_svg;                   external SkiaLib;
procedure sk4d_path_transform;                external SkiaLib;
function  sk4d_pathiterator_create;           external SkiaLib;
procedure sk4d_pathiterator_destroy;          external SkiaLib;
function  sk4d_pathiterator_get_conic_weight; external SkiaLib;
function  sk4d_pathiterator_next;             external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_pathbuilder.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_pathbuilder_add_arc                 := GetProcAddress(LibHandle, 'sk4d_pathbuilder_add_arc');
  sk4d_pathbuilder_add_circle              := GetProcAddress(LibHandle, 'sk4d_pathbuilder_add_circle');
  sk4d_pathbuilder_add_oval                := GetProcAddress(LibHandle, 'sk4d_pathbuilder_add_oval');
  sk4d_pathbuilder_add_path                := GetProcAddress(LibHandle, 'sk4d_pathbuilder_add_path');
  sk4d_pathbuilder_add_polygon             := GetProcAddress(LibHandle, 'sk4d_pathbuilder_add_polygon');
  sk4d_pathbuilder_add_rect                := GetProcAddress(LibHandle, 'sk4d_pathbuilder_add_rect');
  sk4d_pathbuilder_add_rrect               := GetProcAddress(LibHandle, 'sk4d_pathbuilder_add_rrect');
  sk4d_pathbuilder_arc_to                  := GetProcAddress(LibHandle, 'sk4d_pathbuilder_arc_to');
  sk4d_pathbuilder_arc_to2                 := GetProcAddress(LibHandle, 'sk4d_pathbuilder_arc_to2');
  sk4d_pathbuilder_arc_to3                 := GetProcAddress(LibHandle, 'sk4d_pathbuilder_arc_to3');
  sk4d_pathbuilder_close                   := GetProcAddress(LibHandle, 'sk4d_pathbuilder_close');
  sk4d_pathbuilder_conic_to                := GetProcAddress(LibHandle, 'sk4d_pathbuilder_conic_to');
  sk4d_pathbuilder_create                  := GetProcAddress(LibHandle, 'sk4d_pathbuilder_create');
  sk4d_pathbuilder_create2                 := GetProcAddress(LibHandle, 'sk4d_pathbuilder_create2');
  sk4d_pathbuilder_cubic_to                := GetProcAddress(LibHandle, 'sk4d_pathbuilder_cubic_to');
  sk4d_pathbuilder_destroy                 := GetProcAddress(LibHandle, 'sk4d_pathbuilder_destroy');
  sk4d_pathbuilder_detach                  := GetProcAddress(LibHandle, 'sk4d_pathbuilder_detach');
  sk4d_pathbuilder_get_bounds              := GetProcAddress(LibHandle, 'sk4d_pathbuilder_get_bounds');
  sk4d_pathbuilder_get_fill_type           := GetProcAddress(LibHandle, 'sk4d_pathbuilder_get_fill_type');
  sk4d_pathbuilder_inc_reserve             := GetProcAddress(LibHandle, 'sk4d_pathbuilder_inc_reserve');
  sk4d_pathbuilder_line_to                 := GetProcAddress(LibHandle, 'sk4d_pathbuilder_line_to');
  sk4d_pathbuilder_move_to                 := GetProcAddress(LibHandle, 'sk4d_pathbuilder_move_to');
  sk4d_pathbuilder_offset                  := GetProcAddress(LibHandle, 'sk4d_pathbuilder_offset');
  sk4d_pathbuilder_polyline_to             := GetProcAddress(LibHandle, 'sk4d_pathbuilder_polyline_to');
  sk4d_pathbuilder_quad_to                 := GetProcAddress(LibHandle, 'sk4d_pathbuilder_quad_to');
  sk4d_pathbuilder_r_conic_to              := GetProcAddress(LibHandle, 'sk4d_pathbuilder_r_conic_to');
  sk4d_pathbuilder_r_cubic_to              := GetProcAddress(LibHandle, 'sk4d_pathbuilder_r_cubic_to');
  sk4d_pathbuilder_r_line_to               := GetProcAddress(LibHandle, 'sk4d_pathbuilder_r_line_to');
  sk4d_pathbuilder_r_quad_to               := GetProcAddress(LibHandle, 'sk4d_pathbuilder_r_quad_to');
  sk4d_pathbuilder_reset                   := GetProcAddress(LibHandle, 'sk4d_pathbuilder_reset');
  sk4d_pathbuilder_set_filltype            := GetProcAddress(LibHandle, 'sk4d_pathbuilder_set_filltype');
  sk4d_pathbuilder_snapshot                := GetProcAddress(LibHandle, 'sk4d_pathbuilder_snapshot');
  sk4d_pathbuilder_toggle_inverse_filltype := GetProcAddress(LibHandle, 'sk4d_pathbuilder_toggle_inverse_filltype');
{$ELSE}
procedure sk4d_pathbuilder_add_arc;                 external SkiaLib;
procedure sk4d_pathbuilder_add_circle;              external SkiaLib;
procedure sk4d_pathbuilder_add_oval;                external SkiaLib;
procedure sk4d_pathbuilder_add_path;                external SkiaLib;
procedure sk4d_pathbuilder_add_polygon;             external SkiaLib;
procedure sk4d_pathbuilder_add_rect;                external SkiaLib;
procedure sk4d_pathbuilder_add_rrect;               external SkiaLib;
procedure sk4d_pathbuilder_arc_to;                  external SkiaLib;
procedure sk4d_pathbuilder_arc_to2;                 external SkiaLib;
procedure sk4d_pathbuilder_arc_to3;                 external SkiaLib;
procedure sk4d_pathbuilder_close;                   external SkiaLib;
procedure sk4d_pathbuilder_conic_to;                external SkiaLib;
function  sk4d_pathbuilder_create;                  external SkiaLib;
function  sk4d_pathbuilder_create2;                 external SkiaLib;
procedure sk4d_pathbuilder_cubic_to;                external SkiaLib;
procedure sk4d_pathbuilder_destroy;                 external SkiaLib;
function  sk4d_pathbuilder_detach;                  external SkiaLib;
procedure sk4d_pathbuilder_get_bounds;              external SkiaLib;
function  sk4d_pathbuilder_get_fill_type;           external SkiaLib;
procedure sk4d_pathbuilder_inc_reserve;             external SkiaLib;
procedure sk4d_pathbuilder_line_to;                 external SkiaLib;
procedure sk4d_pathbuilder_move_to;                 external SkiaLib;
procedure sk4d_pathbuilder_offset;                  external SkiaLib;
procedure sk4d_pathbuilder_polyline_to;             external SkiaLib;
procedure sk4d_pathbuilder_quad_to;                 external SkiaLib;
procedure sk4d_pathbuilder_r_conic_to;              external SkiaLib;
procedure sk4d_pathbuilder_r_cubic_to;              external SkiaLib;
procedure sk4d_pathbuilder_r_line_to;               external SkiaLib;
procedure sk4d_pathbuilder_r_quad_to;               external SkiaLib;
procedure sk4d_pathbuilder_reset;                   external SkiaLib;
procedure sk4d_pathbuilder_set_filltype;            external SkiaLib;
function  sk4d_pathbuilder_snapshot;                external SkiaLib;
procedure sk4d_pathbuilder_toggle_inverse_filltype; external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_patheffect.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_patheffect_make_1dpath   := GetProcAddress(LibHandle, 'sk4d_patheffect_make_1dpath');
  sk4d_patheffect_make_2dline   := GetProcAddress(LibHandle, 'sk4d_patheffect_make_2dline');
  sk4d_patheffect_make_2dpath   := GetProcAddress(LibHandle, 'sk4d_patheffect_make_2dpath');
  sk4d_patheffect_make_compose  := GetProcAddress(LibHandle, 'sk4d_patheffect_make_compose');
  sk4d_patheffect_make_corner   := GetProcAddress(LibHandle, 'sk4d_patheffect_make_corner');
  sk4d_patheffect_make_dash     := GetProcAddress(LibHandle, 'sk4d_patheffect_make_dash');
  sk4d_patheffect_make_discrete := GetProcAddress(LibHandle, 'sk4d_patheffect_make_discrete');
  sk4d_patheffect_make_sum      := GetProcAddress(LibHandle, 'sk4d_patheffect_make_sum');
  sk4d_patheffect_make_trim     := GetProcAddress(LibHandle, 'sk4d_patheffect_make_trim');
{$ELSE}
function  sk4d_patheffect_make_1dpath;   external SkiaLib;
function  sk4d_patheffect_make_2dline;   external SkiaLib;
function  sk4d_patheffect_make_2dpath;   external SkiaLib;
function  sk4d_patheffect_make_compose;  external SkiaLib;
function  sk4d_patheffect_make_corner;   external SkiaLib;
function  sk4d_patheffect_make_dash;     external SkiaLib;
function  sk4d_patheffect_make_discrete; external SkiaLib;
function  sk4d_patheffect_make_sum;      external SkiaLib;
function  sk4d_patheffect_make_trim;     external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_pathmeasure.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_pathmeasure_create                   := GetProcAddress(LibHandle, 'sk4d_pathmeasure_create');
  sk4d_pathmeasure_destroy                  := GetProcAddress(LibHandle, 'sk4d_pathmeasure_destroy');
  sk4d_pathmeasure_get_length               := GetProcAddress(LibHandle, 'sk4d_pathmeasure_get_length');
  sk4d_pathmeasure_get_matrix               := GetProcAddress(LibHandle, 'sk4d_pathmeasure_get_matrix');
  sk4d_pathmeasure_get_position_and_tangent := GetProcAddress(LibHandle, 'sk4d_pathmeasure_get_position_and_tangent');
  sk4d_pathmeasure_get_segment              := GetProcAddress(LibHandle, 'sk4d_pathmeasure_get_segment');
  sk4d_pathmeasure_is_closed                := GetProcAddress(LibHandle, 'sk4d_pathmeasure_is_closed');
  sk4d_pathmeasure_next_contour             := GetProcAddress(LibHandle, 'sk4d_pathmeasure_next_contour');
{$ELSE}
function  sk4d_pathmeasure_create;                   external SkiaLib;
procedure sk4d_pathmeasure_destroy;                  external SkiaLib;
function  sk4d_pathmeasure_get_length;               external SkiaLib;
function  sk4d_pathmeasure_get_matrix;               external SkiaLib;
function  sk4d_pathmeasure_get_position_and_tangent; external SkiaLib;
function  sk4d_pathmeasure_get_segment;              external SkiaLib;
function  sk4d_pathmeasure_is_closed;                external SkiaLib;
function  sk4d_pathmeasure_next_contour;             external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_picture.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_picture_get_cull_rect    := GetProcAddress(LibHandle, 'sk4d_picture_get_cull_rect');
  sk4d_picture_get_unique_id    := GetProcAddress(LibHandle, 'sk4d_picture_get_unique_id');
  sk4d_picture_make_from_data   := GetProcAddress(LibHandle, 'sk4d_picture_make_from_data');
  sk4d_picture_make_from_stream := GetProcAddress(LibHandle, 'sk4d_picture_make_from_stream');
  sk4d_picture_make_shader      := GetProcAddress(LibHandle, 'sk4d_picture_make_shader');
  sk4d_picture_save_to_data     := GetProcAddress(LibHandle, 'sk4d_picture_save_to_data');
  sk4d_picture_save_to_stream   := GetProcAddress(LibHandle, 'sk4d_picture_save_to_stream');
{$ELSE}
procedure sk4d_picture_get_cull_rect;    external SkiaLib;
function  sk4d_picture_get_unique_id;    external SkiaLib;
function  sk4d_picture_make_from_data;   external SkiaLib;
function  sk4d_picture_make_from_stream; external SkiaLib;
function  sk4d_picture_make_shader;      external SkiaLib;
function  sk4d_picture_save_to_data;     external SkiaLib;
procedure sk4d_picture_save_to_stream;   external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_picturerecorder.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_picturerecorder_begin_recording   := GetProcAddress(LibHandle, 'sk4d_picturerecorder_begin_recording');
  sk4d_picturerecorder_create            := GetProcAddress(LibHandle, 'sk4d_picturerecorder_create');
  sk4d_picturerecorder_destroy           := GetProcAddress(LibHandle, 'sk4d_picturerecorder_destroy');
  sk4d_picturerecorder_finish_recording  := GetProcAddress(LibHandle, 'sk4d_picturerecorder_finish_recording');
  sk4d_picturerecorder_finish_recording2 := GetProcAddress(LibHandle, 'sk4d_picturerecorder_finish_recording2');
{$ELSE}
function  sk4d_picturerecorder_begin_recording;   external SkiaLib;
function  sk4d_picturerecorder_create;            external SkiaLib;
procedure sk4d_picturerecorder_destroy;           external SkiaLib;
function  sk4d_picturerecorder_finish_recording;  external SkiaLib;
function  sk4d_picturerecorder_finish_recording2; external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_pixmap.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_pixmap_create          := GetProcAddress(LibHandle, 'sk4d_pixmap_create');
  sk4d_pixmap_create2         := GetProcAddress(LibHandle, 'sk4d_pixmap_create2');
  sk4d_pixmap_destroy         := GetProcAddress(LibHandle, 'sk4d_pixmap_destroy');
  sk4d_pixmap_erase           := GetProcAddress(LibHandle, 'sk4d_pixmap_erase');
  sk4d_pixmap_erase2          := GetProcAddress(LibHandle, 'sk4d_pixmap_erase2');
  sk4d_pixmap_extract_subset  := GetProcAddress(LibHandle, 'sk4d_pixmap_extract_subset');
  sk4d_pixmap_get_alpha       := GetProcAddress(LibHandle, 'sk4d_pixmap_get_alpha');
  sk4d_pixmap_get_alpha_type  := GetProcAddress(LibHandle, 'sk4d_pixmap_get_alpha_type');
  sk4d_pixmap_get_color       := GetProcAddress(LibHandle, 'sk4d_pixmap_get_color');
  sk4d_pixmap_get_color_space := GetProcAddress(LibHandle, 'sk4d_pixmap_get_color_space');
  sk4d_pixmap_get_color_type  := GetProcAddress(LibHandle, 'sk4d_pixmap_get_color_type');
  sk4d_pixmap_get_height      := GetProcAddress(LibHandle, 'sk4d_pixmap_get_height');
  sk4d_pixmap_get_image_info  := GetProcAddress(LibHandle, 'sk4d_pixmap_get_image_info');
  sk4d_pixmap_get_pixel_addr  := GetProcAddress(LibHandle, 'sk4d_pixmap_get_pixel_addr');
  sk4d_pixmap_get_pixels      := GetProcAddress(LibHandle, 'sk4d_pixmap_get_pixels');
  sk4d_pixmap_get_row_bytes   := GetProcAddress(LibHandle, 'sk4d_pixmap_get_row_bytes');
  sk4d_pixmap_get_width       := GetProcAddress(LibHandle, 'sk4d_pixmap_get_width');
  sk4d_pixmap_read_pixels     := GetProcAddress(LibHandle, 'sk4d_pixmap_read_pixels');
  sk4d_pixmap_scale_pixels    := GetProcAddress(LibHandle, 'sk4d_pixmap_scale_pixels');
  sk4d_pixmap_set_colorspace  := GetProcAddress(LibHandle, 'sk4d_pixmap_set_colorspace');
{$ELSE}
function  sk4d_pixmap_create;          external SkiaLib;
function  sk4d_pixmap_create2;         external SkiaLib;
procedure sk4d_pixmap_destroy;         external SkiaLib;
function  sk4d_pixmap_erase;           external SkiaLib;
function  sk4d_pixmap_erase2;          external SkiaLib;
function  sk4d_pixmap_extract_subset;  external SkiaLib;
function  sk4d_pixmap_get_alpha;       external SkiaLib;
function  sk4d_pixmap_get_alpha_type;  external SkiaLib;
function  sk4d_pixmap_get_color;       external SkiaLib;
function  sk4d_pixmap_get_color_space; external SkiaLib;
function  sk4d_pixmap_get_color_type;  external SkiaLib;
function  sk4d_pixmap_get_height;      external SkiaLib;
procedure sk4d_pixmap_get_image_info;  external SkiaLib;
function  sk4d_pixmap_get_pixel_addr;  external SkiaLib;
function  sk4d_pixmap_get_pixels;      external SkiaLib;
function  sk4d_pixmap_get_row_bytes;   external SkiaLib;
function  sk4d_pixmap_get_width;       external SkiaLib;
function  sk4d_pixmap_read_pixels;     external SkiaLib;
function  sk4d_pixmap_scale_pixels;    external SkiaLib;
procedure sk4d_pixmap_set_colorspace;  external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_refcnt.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_refcnt_ref   := GetProcAddress(LibHandle, 'sk4d_refcnt_ref');
  sk4d_refcnt_unref := GetProcAddress(LibHandle, 'sk4d_refcnt_unref');
{$ELSE}
procedure sk4d_refcnt_ref;   external SkiaLib;
procedure sk4d_refcnt_unref; external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_region.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_region_contains           := GetProcAddress(LibHandle, 'sk4d_region_contains');
  sk4d_region_contains2          := GetProcAddress(LibHandle, 'sk4d_region_contains2');
  sk4d_region_contains3          := GetProcAddress(LibHandle, 'sk4d_region_contains3');
  sk4d_region_create             := GetProcAddress(LibHandle, 'sk4d_region_create');
  sk4d_region_create2            := GetProcAddress(LibHandle, 'sk4d_region_create2');
  sk4d_region_destroy            := GetProcAddress(LibHandle, 'sk4d_region_destroy');
  sk4d_region_get_boundary_path  := GetProcAddress(LibHandle, 'sk4d_region_get_boundary_path');
  sk4d_region_get_bounds         := GetProcAddress(LibHandle, 'sk4d_region_get_bounds');
  sk4d_region_intersects         := GetProcAddress(LibHandle, 'sk4d_region_intersects');
  sk4d_region_intersects2        := GetProcAddress(LibHandle, 'sk4d_region_intersects2');
  sk4d_region_is_complex         := GetProcAddress(LibHandle, 'sk4d_region_is_complex');
  sk4d_region_is_empty           := GetProcAddress(LibHandle, 'sk4d_region_is_empty');
  sk4d_region_is_equal           := GetProcAddress(LibHandle, 'sk4d_region_is_equal');
  sk4d_region_is_rect            := GetProcAddress(LibHandle, 'sk4d_region_is_rect');
  sk4d_region_op                 := GetProcAddress(LibHandle, 'sk4d_region_op');
  sk4d_region_op2                := GetProcAddress(LibHandle, 'sk4d_region_op2');
  sk4d_region_quick_contains     := GetProcAddress(LibHandle, 'sk4d_region_quick_contains');
  sk4d_region_quick_reject       := GetProcAddress(LibHandle, 'sk4d_region_quick_reject');
  sk4d_region_quick_reject2      := GetProcAddress(LibHandle, 'sk4d_region_quick_reject2');
  sk4d_region_set_empty          := GetProcAddress(LibHandle, 'sk4d_region_set_empty');
  sk4d_region_set_path           := GetProcAddress(LibHandle, 'sk4d_region_set_path');
  sk4d_region_set_rect           := GetProcAddress(LibHandle, 'sk4d_region_set_rect');
  sk4d_region_set_rects          := GetProcAddress(LibHandle, 'sk4d_region_set_rects');
  sk4d_region_translate          := GetProcAddress(LibHandle, 'sk4d_region_translate');
  sk4d_region_translate2         := GetProcAddress(LibHandle, 'sk4d_region_translate2');
  sk4d_regioncliperator_create   := GetProcAddress(LibHandle, 'sk4d_regioncliperator_create');
  sk4d_regioncliperator_destroy  := GetProcAddress(LibHandle, 'sk4d_regioncliperator_destroy');
  sk4d_regioncliperator_get_rect := GetProcAddress(LibHandle, 'sk4d_regioncliperator_get_rect');
  sk4d_regioncliperator_next     := GetProcAddress(LibHandle, 'sk4d_regioncliperator_next');
  sk4d_regioniterator_create     := GetProcAddress(LibHandle, 'sk4d_regioniterator_create');
  sk4d_regioniterator_destroy    := GetProcAddress(LibHandle, 'sk4d_regioniterator_destroy');
  sk4d_regioniterator_get_rect   := GetProcAddress(LibHandle, 'sk4d_regioniterator_get_rect');
  sk4d_regioniterator_next       := GetProcAddress(LibHandle, 'sk4d_regioniterator_next');
  sk4d_regioniterator_rewind     := GetProcAddress(LibHandle, 'sk4d_regioniterator_rewind');
  sk4d_regionspanerator_create   := GetProcAddress(LibHandle, 'sk4d_regionspanerator_create');
  sk4d_regionspanerator_destroy  := GetProcAddress(LibHandle, 'sk4d_regionspanerator_destroy');
  sk4d_regionspanerator_next     := GetProcAddress(LibHandle, 'sk4d_regionspanerator_next');
{$ELSE}
function  sk4d_region_contains;           external SkiaLib;
function  sk4d_region_contains2;          external SkiaLib;
function  sk4d_region_contains3;          external SkiaLib;
function  sk4d_region_create;             external SkiaLib;
function  sk4d_region_create2;            external SkiaLib;
procedure sk4d_region_destroy;            external SkiaLib;
function  sk4d_region_get_boundary_path;  external SkiaLib;
procedure sk4d_region_get_bounds;         external SkiaLib;
function  sk4d_region_intersects;         external SkiaLib;
function  sk4d_region_intersects2;        external SkiaLib;
function  sk4d_region_is_complex;         external SkiaLib;
function  sk4d_region_is_empty;           external SkiaLib;
function  sk4d_region_is_equal;           external SkiaLib;
function  sk4d_region_is_rect;            external SkiaLib;
function  sk4d_region_op;                 external SkiaLib;
function  sk4d_region_op2;                external SkiaLib;
function  sk4d_region_quick_contains;     external SkiaLib;
function  sk4d_region_quick_reject;       external SkiaLib;
function  sk4d_region_quick_reject2;      external SkiaLib;
procedure sk4d_region_set_empty;          external SkiaLib;
function  sk4d_region_set_path;           external SkiaLib;
function  sk4d_region_set_rect;           external SkiaLib;
function  sk4d_region_set_rects;          external SkiaLib;
procedure sk4d_region_translate;          external SkiaLib;
procedure sk4d_region_translate2;         external SkiaLib;
function  sk4d_regioncliperator_create;   external SkiaLib;
procedure sk4d_regioncliperator_destroy;  external SkiaLib;
procedure sk4d_regioncliperator_get_rect; external SkiaLib;
function  sk4d_regioncliperator_next;     external SkiaLib;
function  sk4d_regioniterator_create;     external SkiaLib;
procedure sk4d_regioniterator_destroy;    external SkiaLib;
procedure sk4d_regioniterator_get_rect;   external SkiaLib;
function  sk4d_regioniterator_next;       external SkiaLib;
function  sk4d_regioniterator_rewind;     external SkiaLib;
function  sk4d_regionspanerator_create;   external SkiaLib;
procedure sk4d_regionspanerator_destroy;  external SkiaLib;
function  sk4d_regionspanerator_next;     external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_rrect.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_rrect_contains         := GetProcAddress(LibHandle, 'sk4d_rrect_contains');
  sk4d_rrect_create           := GetProcAddress(LibHandle, 'sk4d_rrect_create');
  sk4d_rrect_create2          := GetProcAddress(LibHandle, 'sk4d_rrect_create2');
  sk4d_rrect_deflate          := GetProcAddress(LibHandle, 'sk4d_rrect_deflate');
  sk4d_rrect_deflate2         := GetProcAddress(LibHandle, 'sk4d_rrect_deflate2');
  sk4d_rrect_destroy          := GetProcAddress(LibHandle, 'sk4d_rrect_destroy');
  sk4d_rrect_get_height       := GetProcAddress(LibHandle, 'sk4d_rrect_get_height');
  sk4d_rrect_get_radii        := GetProcAddress(LibHandle, 'sk4d_rrect_get_radii');
  sk4d_rrect_get_rect         := GetProcAddress(LibHandle, 'sk4d_rrect_get_rect');
  sk4d_rrect_get_simple_radii := GetProcAddress(LibHandle, 'sk4d_rrect_get_simple_radii');
  sk4d_rrect_get_type         := GetProcAddress(LibHandle, 'sk4d_rrect_get_type');
  sk4d_rrect_get_width        := GetProcAddress(LibHandle, 'sk4d_rrect_get_width');
  sk4d_rrect_inflate          := GetProcAddress(LibHandle, 'sk4d_rrect_inflate');
  sk4d_rrect_inflate2         := GetProcAddress(LibHandle, 'sk4d_rrect_inflate2');
  sk4d_rrect_is_equal         := GetProcAddress(LibHandle, 'sk4d_rrect_is_equal');
  sk4d_rrect_is_valid         := GetProcAddress(LibHandle, 'sk4d_rrect_is_valid');
  sk4d_rrect_make_offset      := GetProcAddress(LibHandle, 'sk4d_rrect_make_offset');
  sk4d_rrect_offset           := GetProcAddress(LibHandle, 'sk4d_rrect_offset');
  sk4d_rrect_set_empty        := GetProcAddress(LibHandle, 'sk4d_rrect_set_empty');
  sk4d_rrect_set_nine_patch   := GetProcAddress(LibHandle, 'sk4d_rrect_set_nine_patch');
  sk4d_rrect_set_oval         := GetProcAddress(LibHandle, 'sk4d_rrect_set_oval');
  sk4d_rrect_set_rect         := GetProcAddress(LibHandle, 'sk4d_rrect_set_rect');
  sk4d_rrect_set_rect2        := GetProcAddress(LibHandle, 'sk4d_rrect_set_rect2');
  sk4d_rrect_set_rect3        := GetProcAddress(LibHandle, 'sk4d_rrect_set_rect3');
  sk4d_rrect_transform        := GetProcAddress(LibHandle, 'sk4d_rrect_transform');
{$ELSE}
function  sk4d_rrect_contains;         external SkiaLib;
function  sk4d_rrect_create;           external SkiaLib;
function  sk4d_rrect_create2;          external SkiaLib;
procedure sk4d_rrect_deflate;          external SkiaLib;
procedure sk4d_rrect_deflate2;         external SkiaLib;
procedure sk4d_rrect_destroy;          external SkiaLib;
function  sk4d_rrect_get_height;       external SkiaLib;
procedure sk4d_rrect_get_radii;        external SkiaLib;
procedure sk4d_rrect_get_rect;         external SkiaLib;
procedure sk4d_rrect_get_simple_radii; external SkiaLib;
function  sk4d_rrect_get_type;         external SkiaLib;
function  sk4d_rrect_get_width;        external SkiaLib;
procedure sk4d_rrect_inflate;          external SkiaLib;
procedure sk4d_rrect_inflate2;         external SkiaLib;
function  sk4d_rrect_is_equal;         external SkiaLib;
function  sk4d_rrect_is_valid;         external SkiaLib;
function  sk4d_rrect_make_offset;      external SkiaLib;
procedure sk4d_rrect_offset;           external SkiaLib;
procedure sk4d_rrect_set_empty;        external SkiaLib;
procedure sk4d_rrect_set_nine_patch;   external SkiaLib;
procedure sk4d_rrect_set_oval;         external SkiaLib;
procedure sk4d_rrect_set_rect;         external SkiaLib;
procedure sk4d_rrect_set_rect2;        external SkiaLib;
procedure sk4d_rrect_set_rect3;        external SkiaLib;
function  sk4d_rrect_transform;        external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_shader.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_shader_make_blend                      := GetProcAddress(LibHandle, 'sk4d_shader_make_blend');
  sk4d_shader_make_color                      := GetProcAddress(LibHandle, 'sk4d_shader_make_color');
  sk4d_shader_make_color2                     := GetProcAddress(LibHandle, 'sk4d_shader_make_color2');
  sk4d_shader_make_empty                      := GetProcAddress(LibHandle, 'sk4d_shader_make_empty');
  sk4d_shader_make_gradient_linear            := GetProcAddress(LibHandle, 'sk4d_shader_make_gradient_linear');
  sk4d_shader_make_gradient_linear2           := GetProcAddress(LibHandle, 'sk4d_shader_make_gradient_linear2');
  sk4d_shader_make_gradient_radial            := GetProcAddress(LibHandle, 'sk4d_shader_make_gradient_radial');
  sk4d_shader_make_gradient_radial2           := GetProcAddress(LibHandle, 'sk4d_shader_make_gradient_radial2');
  sk4d_shader_make_gradient_sweep             := GetProcAddress(LibHandle, 'sk4d_shader_make_gradient_sweep');
  sk4d_shader_make_gradient_sweep2            := GetProcAddress(LibHandle, 'sk4d_shader_make_gradient_sweep2');
  sk4d_shader_make_perlin_noise_fractal_noise := GetProcAddress(LibHandle, 'sk4d_shader_make_perlin_noise_fractal_noise');
  sk4d_shader_make_perlin_noise_turbulence    := GetProcAddress(LibHandle, 'sk4d_shader_make_perlin_noise_turbulence');
  sk4d_shader_make_with_color_filter          := GetProcAddress(LibHandle, 'sk4d_shader_make_with_color_filter');
  sk4d_shader_make_with_local_matrix          := GetProcAddress(LibHandle, 'sk4d_shader_make_with_local_matrix');
{$ELSE}
function  sk4d_shader_make_blend;                      external SkiaLib;
function  sk4d_shader_make_color;                      external SkiaLib;
function  sk4d_shader_make_color2;                     external SkiaLib;
function  sk4d_shader_make_empty;                      external SkiaLib;
function  sk4d_shader_make_gradient_linear;            external SkiaLib;
function  sk4d_shader_make_gradient_linear2;           external SkiaLib;
function  sk4d_shader_make_gradient_radial;            external SkiaLib;
function  sk4d_shader_make_gradient_radial2;           external SkiaLib;
function  sk4d_shader_make_gradient_sweep;             external SkiaLib;
function  sk4d_shader_make_gradient_sweep2;            external SkiaLib;
function  sk4d_shader_make_perlin_noise_fractal_noise; external SkiaLib;
function  sk4d_shader_make_perlin_noise_turbulence;    external SkiaLib;
function  sk4d_shader_make_with_color_filter;          external SkiaLib;
function  sk4d_shader_make_with_local_matrix;          external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_stream.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_dynamicmemorywstream_copy_to          := GetProcAddress(LibHandle, 'sk4d_dynamicmemorywstream_copy_to');
  sk4d_dynamicmemorywstream_create           := GetProcAddress(LibHandle, 'sk4d_dynamicmemorywstream_create');
  sk4d_dynamicmemorywstream_detach_as_data   := GetProcAddress(LibHandle, 'sk4d_dynamicmemorywstream_detach_as_data');
  sk4d_dynamicmemorywstream_detach_as_stream := GetProcAddress(LibHandle, 'sk4d_dynamicmemorywstream_detach_as_stream');
  sk4d_dynamicmemorywstream_write_to_stream  := GetProcAddress(LibHandle, 'sk4d_dynamicmemorywstream_write_to_stream');
  sk4d_filestream_create                     := GetProcAddress(LibHandle, 'sk4d_filestream_create');
  sk4d_filestream_is_valid                   := GetProcAddress(LibHandle, 'sk4d_filestream_is_valid');
  sk4d_filewstream_create                    := GetProcAddress(LibHandle, 'sk4d_filewstream_create');
  sk4d_filewstream_is_valid                  := GetProcAddress(LibHandle, 'sk4d_filewstream_is_valid');
  sk4d_managedstream_create                  := GetProcAddress(LibHandle, 'sk4d_managedstream_create');
  sk4d_managedstream_set_procs               := GetProcAddress(LibHandle, 'sk4d_managedstream_set_procs');
  sk4d_managedwstream_create                 := GetProcAddress(LibHandle, 'sk4d_managedwstream_create');
  sk4d_managedwstream_set_procs              := GetProcAddress(LibHandle, 'sk4d_managedwstream_set_procs');
  sk4d_memorystream_create                   := GetProcAddress(LibHandle, 'sk4d_memorystream_create');
  sk4d_stream_destroy                        := GetProcAddress(LibHandle, 'sk4d_stream_destroy');
  sk4d_stream_duplicate                      := GetProcAddress(LibHandle, 'sk4d_stream_duplicate');
  sk4d_stream_fork                           := GetProcAddress(LibHandle, 'sk4d_stream_fork');
  sk4d_stream_get_length                     := GetProcAddress(LibHandle, 'sk4d_stream_get_length');
  sk4d_stream_get_memory_base                := GetProcAddress(LibHandle, 'sk4d_stream_get_memory_base');
  sk4d_stream_get_position                   := GetProcAddress(LibHandle, 'sk4d_stream_get_position');
  sk4d_stream_has_length                     := GetProcAddress(LibHandle, 'sk4d_stream_has_length');
  sk4d_stream_has_position                   := GetProcAddress(LibHandle, 'sk4d_stream_has_position');
  sk4d_stream_is_at_end                      := GetProcAddress(LibHandle, 'sk4d_stream_is_at_end');
  sk4d_stream_move                           := GetProcAddress(LibHandle, 'sk4d_stream_move');
  sk4d_stream_peek                           := GetProcAddress(LibHandle, 'sk4d_stream_peek');
  sk4d_stream_read                           := GetProcAddress(LibHandle, 'sk4d_stream_read');
  sk4d_stream_rewind                         := GetProcAddress(LibHandle, 'sk4d_stream_rewind');
  sk4d_stream_seek                           := GetProcAddress(LibHandle, 'sk4d_stream_seek');
  sk4d_wstream_destroy                       := GetProcAddress(LibHandle, 'sk4d_wstream_destroy');
  sk4d_wstream_flush                         := GetProcAddress(LibHandle, 'sk4d_wstream_flush');
  sk4d_wstream_get_bytes_written             := GetProcAddress(LibHandle, 'sk4d_wstream_get_bytes_written');
  sk4d_wstream_write                         := GetProcAddress(LibHandle, 'sk4d_wstream_write');
  sk4d_wstream_write_stream                  := GetProcAddress(LibHandle, 'sk4d_wstream_write_stream');
{$ELSE}
procedure sk4d_dynamicmemorywstream_copy_to;          external SkiaLib;
function  sk4d_dynamicmemorywstream_create;           external SkiaLib;
function  sk4d_dynamicmemorywstream_detach_as_data;   external SkiaLib;
function  sk4d_dynamicmemorywstream_detach_as_stream; external SkiaLib;
function  sk4d_dynamicmemorywstream_write_to_stream;  external SkiaLib;
function  sk4d_filestream_create;                     external SkiaLib;
function  sk4d_filestream_is_valid;                   external SkiaLib;
function  sk4d_filewstream_create;                    external SkiaLib;
function  sk4d_filewstream_is_valid;                  external SkiaLib;
function  sk4d_managedstream_create;                  external SkiaLib;
procedure sk4d_managedstream_set_procs;               external SkiaLib;
function  sk4d_managedwstream_create;                 external SkiaLib;
procedure sk4d_managedwstream_set_procs;              external SkiaLib;
function  sk4d_memorystream_create;                   external SkiaLib;
procedure sk4d_stream_destroy;                        external SkiaLib;
function  sk4d_stream_duplicate;                      external SkiaLib;
function  sk4d_stream_fork;                           external SkiaLib;
function  sk4d_stream_get_length;                     external SkiaLib;
function  sk4d_stream_get_memory_base;                external SkiaLib;
function  sk4d_stream_get_position;                   external SkiaLib;
function  sk4d_stream_has_length;                     external SkiaLib;
function  sk4d_stream_has_position;                   external SkiaLib;
function  sk4d_stream_is_at_end;                      external SkiaLib;
function  sk4d_stream_move;                           external SkiaLib;
function  sk4d_stream_peek;                           external SkiaLib;
function  sk4d_stream_read;                           external SkiaLib;
function  sk4d_stream_rewind;                         external SkiaLib;
function  sk4d_stream_seek;                           external SkiaLib;
procedure sk4d_wstream_destroy;                       external SkiaLib;
procedure sk4d_wstream_flush;                         external SkiaLib;
function  sk4d_wstream_get_bytes_written;             external SkiaLib;
function  sk4d_wstream_write;                         external SkiaLib;
function  sk4d_wstream_write_stream;                  external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_string.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_string_append   := GetProcAddress(LibHandle, 'sk4d_string_append');
  sk4d_string_create   := GetProcAddress(LibHandle, 'sk4d_string_create');
  sk4d_string_create2  := GetProcAddress(LibHandle, 'sk4d_string_create2');
  sk4d_string_destroy  := GetProcAddress(LibHandle, 'sk4d_string_destroy');
  sk4d_string_get_text := GetProcAddress(LibHandle, 'sk4d_string_get_text');
  sk4d_string_is_equal := GetProcAddress(LibHandle, 'sk4d_string_is_equal');
  sk4d_string_set_text := GetProcAddress(LibHandle, 'sk4d_string_set_text');
{$ELSE}
procedure sk4d_string_append;   external SkiaLib;
function  sk4d_string_create;   external SkiaLib;
function  sk4d_string_create2;  external SkiaLib;
procedure sk4d_string_destroy;  external SkiaLib;
function  sk4d_string_get_text; external SkiaLib;
function  sk4d_string_is_equal; external SkiaLib;
procedure sk4d_string_set_text; external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_surface.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_surface_draw                     := GetProcAddress(LibHandle, 'sk4d_surface_draw');
  sk4d_surface_flush                    := GetProcAddress(LibHandle, 'sk4d_surface_flush');
  sk4d_surface_get_canvas               := GetProcAddress(LibHandle, 'sk4d_surface_get_canvas');
  sk4d_surface_get_height               := GetProcAddress(LibHandle, 'sk4d_surface_get_height');
  sk4d_surface_get_image_info           := GetProcAddress(LibHandle, 'sk4d_surface_get_image_info');
  sk4d_surface_get_props                := GetProcAddress(LibHandle, 'sk4d_surface_get_props');
  sk4d_surface_get_width                := GetProcAddress(LibHandle, 'sk4d_surface_get_width');
  sk4d_surface_make_from_ca_metal_layer := GetProcAddress(LibHandle, 'sk4d_surface_make_from_ca_metal_layer');
  sk4d_surface_make_from_rendertarget   := GetProcAddress(LibHandle, 'sk4d_surface_make_from_rendertarget');
  sk4d_surface_make_from_texture        := GetProcAddress(LibHandle, 'sk4d_surface_make_from_texture');
  sk4d_surface_make_image_snapshot      := GetProcAddress(LibHandle, 'sk4d_surface_make_image_snapshot');
  sk4d_surface_make_image_snapshot2     := GetProcAddress(LibHandle, 'sk4d_surface_make_image_snapshot2');
  sk4d_surface_make_null                := GetProcAddress(LibHandle, 'sk4d_surface_make_null');
  sk4d_surface_make_raster              := GetProcAddress(LibHandle, 'sk4d_surface_make_raster');
  sk4d_surface_make_raster_direct       := GetProcAddress(LibHandle, 'sk4d_surface_make_raster_direct');
  sk4d_surface_make_render_target       := GetProcAddress(LibHandle, 'sk4d_surface_make_render_target');
  sk4d_surface_peek_pixels              := GetProcAddress(LibHandle, 'sk4d_surface_peek_pixels');
  sk4d_surface_read_pixels              := GetProcAddress(LibHandle, 'sk4d_surface_read_pixels');
  sk4d_surface_write_pixels             := GetProcAddress(LibHandle, 'sk4d_surface_write_pixels');
  sk4d_sk_surfaceprops_create           := GetProcAddress(LibHandle, 'sk4d_sk_surfaceprops_create');
  sk4d_surfaceprops_destroy             := GetProcAddress(LibHandle, 'sk4d_surfaceprops_destroy');
  sk4d_surfaceprops_get_flags           := GetProcAddress(LibHandle, 'sk4d_surfaceprops_get_flags');
  sk4d_surfaceprops_get_pixel_geometry  := GetProcAddress(LibHandle, 'sk4d_surfaceprops_get_pixel_geometry');
  sk4d_surfaceprops_is_equal            := GetProcAddress(LibHandle, 'sk4d_surfaceprops_is_equal');
{$ELSE}
procedure sk4d_surface_draw;                     external SkiaLib;
procedure sk4d_surface_flush;                    external SkiaLib;
function  sk4d_surface_get_canvas;               external SkiaLib;
function  sk4d_surface_get_height;               external SkiaLib;
procedure sk4d_surface_get_image_info;           external SkiaLib;
function  sk4d_surface_get_props;                external SkiaLib;
function  sk4d_surface_get_width;                external SkiaLib;
function  sk4d_surface_make_from_ca_metal_layer; external SkiaLib;
function  sk4d_surface_make_from_rendertarget;   external SkiaLib;
function  sk4d_surface_make_from_texture;        external SkiaLib;
function  sk4d_surface_make_image_snapshot;      external SkiaLib;
function  sk4d_surface_make_image_snapshot2;     external SkiaLib;
function  sk4d_surface_make_null;                external SkiaLib;
function  sk4d_surface_make_raster;              external SkiaLib;
function  sk4d_surface_make_raster_direct;       external SkiaLib;
function  sk4d_surface_make_render_target;       external SkiaLib;
function  sk4d_surface_peek_pixels;              external SkiaLib;
function  sk4d_surface_read_pixels;              external SkiaLib;
procedure sk4d_surface_write_pixels;             external SkiaLib;
function  sk4d_sk_surfaceprops_create;           external SkiaLib;
procedure sk4d_surfaceprops_destroy;             external SkiaLib;
function  sk4d_surfaceprops_get_flags;           external SkiaLib;
function  sk4d_surfaceprops_get_pixel_geometry;  external SkiaLib;
function  sk4d_surfaceprops_is_equal;            external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_svgcanvas.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_svgcanvas_make := GetProcAddress(LibHandle, 'sk4d_svgcanvas_make');
{$ELSE}
function  sk4d_svgcanvas_make; external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_textblob.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_textblob_get_intercepts                  := GetProcAddress(LibHandle, 'sk4d_textblob_get_intercepts');
  sk4d_textblob_ref                             := GetProcAddress(LibHandle, 'sk4d_textblob_ref');
  sk4d_textblob_unref                           := GetProcAddress(LibHandle, 'sk4d_textblob_unref');
  sk4d_textblobbuilder_create                   := GetProcAddress(LibHandle, 'sk4d_textblobbuilder_create');
  sk4d_textblobbuilder_destroy                  := GetProcAddress(LibHandle, 'sk4d_textblobbuilder_destroy');
  sk4d_textblobbuilder_alloc_horizontal_run     := GetProcAddress(LibHandle, 'sk4d_textblobbuilder_alloc_horizontal_run');
  sk4d_textblobbuilder_alloc_positioned_run     := GetProcAddress(LibHandle, 'sk4d_textblobbuilder_alloc_positioned_run');
  sk4d_textblobbuilder_alloc_rotation_scale_run := GetProcAddress(LibHandle, 'sk4d_textblobbuilder_alloc_rotation_scale_run');
  sk4d_textblobbuilder_alloc_run                := GetProcAddress(LibHandle, 'sk4d_textblobbuilder_alloc_run');
  sk4d_textblobbuilder_detach                   := GetProcAddress(LibHandle, 'sk4d_textblobbuilder_detach');
{$ELSE}
function  sk4d_textblob_get_intercepts;                  external SkiaLib;
procedure sk4d_textblob_ref;                             external SkiaLib;
procedure sk4d_textblob_unref;                           external SkiaLib;
function  sk4d_textblobbuilder_create;                   external SkiaLib;
procedure sk4d_textblobbuilder_destroy;                  external SkiaLib;
procedure sk4d_textblobbuilder_alloc_horizontal_run;     external SkiaLib;
procedure sk4d_textblobbuilder_alloc_positioned_run;     external SkiaLib;
procedure sk4d_textblobbuilder_alloc_rotation_scale_run; external SkiaLib;
procedure sk4d_textblobbuilder_alloc_run;                external SkiaLib;
function  sk4d_textblobbuilder_detach;                   external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_time.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_datetime_to_iso8601 := GetProcAddress(LibHandle, 'sk4d_datetime_to_iso8601');
{$ELSE}
procedure sk4d_datetime_to_iso8601; external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_tracememorydump.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_tracememorydumpbaseclass_create    := GetProcAddress(LibHandle, 'sk4d_tracememorydumpbaseclass_create');
  sk4d_tracememorydumpbaseclass_destroy   := GetProcAddress(LibHandle, 'sk4d_tracememorydumpbaseclass_destroy');
  sk4d_tracememorydumpbaseclass_set_procs := GetProcAddress(LibHandle, 'sk4d_tracememorydumpbaseclass_set_procs');
{$ELSE}
function  sk4d_tracememorydumpbaseclass_create;    external SkiaLib;
procedure sk4d_tracememorydumpbaseclass_destroy;   external SkiaLib;
procedure sk4d_tracememorydumpbaseclass_set_procs; external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_typeface.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_typeface_get_family_name := GetProcAddress(LibHandle, 'sk4d_typeface_get_family_name');
  sk4d_typeface_get_slant       := GetProcAddress(LibHandle, 'sk4d_typeface_get_slant');
  sk4d_typeface_get_style       := GetProcAddress(LibHandle, 'sk4d_typeface_get_style');
  sk4d_typeface_get_unique_id   := GetProcAddress(LibHandle, 'sk4d_typeface_get_unique_id');
  sk4d_typeface_get_weight      := GetProcAddress(LibHandle, 'sk4d_typeface_get_weight');
  sk4d_typeface_get_width       := GetProcAddress(LibHandle, 'sk4d_typeface_get_width');
  sk4d_typeface_make_default    := GetProcAddress(LibHandle, 'sk4d_typeface_make_default');
  sk4d_typeface_make_from_data  := GetProcAddress(LibHandle, 'sk4d_typeface_make_from_data');
  sk4d_typeface_make_from_file  := GetProcAddress(LibHandle, 'sk4d_typeface_make_from_file');
  sk4d_typeface_make_from_name  := GetProcAddress(LibHandle, 'sk4d_typeface_make_from_name');
{$ELSE}
procedure sk4d_typeface_get_family_name; external SkiaLib;
function  sk4d_typeface_get_slant;       external SkiaLib;
function  sk4d_typeface_get_style;       external SkiaLib;
function  sk4d_typeface_get_unique_id;   external SkiaLib;
function  sk4d_typeface_get_weight;      external SkiaLib;
function  sk4d_typeface_get_width;       external SkiaLib;
function  sk4d_typeface_make_default;    external SkiaLib;
function  sk4d_typeface_make_from_data;  external SkiaLib;
function  sk4d_typeface_make_from_file;  external SkiaLib;
function  sk4d_typeface_make_from_name;  external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'include/c/sk4d_vertices.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_vertices_get_unique_id := GetProcAddress(LibHandle, 'sk4d_vertices_get_unique_id');
  sk4d_vertices_make_copy     := GetProcAddress(LibHandle, 'sk4d_vertices_make_copy');
  sk4d_vertices_ref           := GetProcAddress(LibHandle, 'sk4d_vertices_ref');
  sk4d_vertices_unref         := GetProcAddress(LibHandle, 'sk4d_vertices_unref');
{$ELSE}
function  sk4d_vertices_get_unique_id; external SkiaLib;
function  sk4d_vertices_make_copy;     external SkiaLib;
procedure sk4d_vertices_ref;           external SkiaLib;
procedure sk4d_vertices_unref;         external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'modules/skottie/include/sk4d_skottie.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_skottieanimation_get_duration     := GetProcAddress(LibHandle, 'sk4d_skottieanimation_get_duration');
  sk4d_skottieanimation_get_fps          := GetProcAddress(LibHandle, 'sk4d_skottieanimation_get_fps');
  sk4d_skottieanimation_get_in_point     := GetProcAddress(LibHandle, 'sk4d_skottieanimation_get_in_point');
  sk4d_skottieanimation_get_out_point    := GetProcAddress(LibHandle, 'sk4d_skottieanimation_get_out_point');
  sk4d_skottieanimation_get_size         := GetProcAddress(LibHandle, 'sk4d_skottieanimation_get_size');
  sk4d_skottieanimation_get_version      := GetProcAddress(LibHandle, 'sk4d_skottieanimation_get_version');
  sk4d_skottieanimation_make             := GetProcAddress(LibHandle, 'sk4d_skottieanimation_make');
  sk4d_skottieanimation_make_from_file   := GetProcAddress(LibHandle, 'sk4d_skottieanimation_make_from_file');
  sk4d_skottieanimation_make_from_stream := GetProcAddress(LibHandle, 'sk4d_skottieanimation_make_from_stream');
  sk4d_skottieanimation_ref              := GetProcAddress(LibHandle, 'sk4d_skottieanimation_ref');
  sk4d_skottieanimation_render           := GetProcAddress(LibHandle, 'sk4d_skottieanimation_render');
  sk4d_skottieanimation_seek_frame       := GetProcAddress(LibHandle, 'sk4d_skottieanimation_seek_frame');
  sk4d_skottieanimation_seek_frame_time  := GetProcAddress(LibHandle, 'sk4d_skottieanimation_seek_frame_time');
  sk4d_skottieanimation_unref            := GetProcAddress(LibHandle, 'sk4d_skottieanimation_unref');
{$ELSE}
function  sk4d_skottieanimation_get_duration;     external SkiaLib;
function  sk4d_skottieanimation_get_fps;          external SkiaLib;
function  sk4d_skottieanimation_get_in_point;     external SkiaLib;
function  sk4d_skottieanimation_get_out_point;    external SkiaLib;
procedure sk4d_skottieanimation_get_size;         external SkiaLib;
function  sk4d_skottieanimation_get_version;      external SkiaLib;
function  sk4d_skottieanimation_make;             external SkiaLib;
function  sk4d_skottieanimation_make_from_file;   external SkiaLib;
function  sk4d_skottieanimation_make_from_stream; external SkiaLib;
procedure sk4d_skottieanimation_ref;              external SkiaLib;
procedure sk4d_skottieanimation_render;           external SkiaLib;
procedure sk4d_skottieanimation_seek_frame;       external SkiaLib;
procedure sk4d_skottieanimation_seek_frame_time;  external SkiaLib;
procedure sk4d_skottieanimation_unref;            external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'modules/skshaper/include/sk4d_shaper.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_shaper_create                                := GetProcAddress(LibHandle, 'sk4d_shaper_create');
  sk4d_shaper_destroy                               := GetProcAddress(LibHandle, 'sk4d_shaper_destroy');
  sk4d_shaper_make_bi_di_run_iterator               := GetProcAddress(LibHandle, 'sk4d_shaper_make_bi_di_run_iterator');
  sk4d_shaper_make_font_run_iterator                := GetProcAddress(LibHandle, 'sk4d_shaper_make_font_run_iterator');
  sk4d_shaper_make_script_run_iterator              := GetProcAddress(LibHandle, 'sk4d_shaper_make_script_run_iterator');
  sk4d_shaper_make_std_language_run_iterator        := GetProcAddress(LibHandle, 'sk4d_shaper_make_std_language_run_iterator');
  sk4d_shaper_purge_caches                          := GetProcAddress(LibHandle, 'sk4d_shaper_purge_caches');
  sk4d_shaper_shape                                 := GetProcAddress(LibHandle, 'sk4d_shaper_shape');
  sk4d_shaper_shape2                                := GetProcAddress(LibHandle, 'sk4d_shaper_shape2');
  sk_shaperbidiruniterator_get_current_level        := GetProcAddress(LibHandle, 'sk_shaperbidiruniterator_get_current_level');
  sk_shaperfontruniterator_get_current_font         := GetProcAddress(LibHandle, 'sk_shaperfontruniterator_get_current_font');
  sk_shaperlanguageruniterator_get_current_language := GetProcAddress(LibHandle, 'sk_shaperlanguageruniterator_get_current_language');
  sk4d_shaperrunhandler_destroy                     := GetProcAddress(LibHandle, 'sk4d_shaperrunhandler_destroy');
  sk4d_shaperrunhandlerbaseclass_create             := GetProcAddress(LibHandle, 'sk4d_shaperrunhandlerbaseclass_create');
  sk4d_shaperrunhandlerbaseclass_set_procs          := GetProcAddress(LibHandle, 'sk4d_shaperrunhandlerbaseclass_set_procs');
  sk4d_shaperruniterator_consume                    := GetProcAddress(LibHandle, 'sk4d_shaperruniterator_consume');
  sk4d_shaperruniterator_destroy                    := GetProcAddress(LibHandle, 'sk4d_shaperruniterator_destroy');
  sk4d_shaperruniterator_get_end_of_current_run     := GetProcAddress(LibHandle, 'sk4d_shaperruniterator_get_end_of_current_run');
  sk_shaperscriptruniterator_get_current_script     := GetProcAddress(LibHandle, 'sk_shaperscriptruniterator_get_current_script');
  sk4d_textblobbuilderrunhandler_create             := GetProcAddress(LibHandle, 'sk4d_textblobbuilderrunhandler_create');
  sk4d_textblobbuilderrunhandler_detach             := GetProcAddress(LibHandle, 'sk4d_textblobbuilderrunhandler_detach');
  sk4d_textblobbuilderrunhandler_get_end_point      := GetProcAddress(LibHandle, 'sk4d_textblobbuilderrunhandler_get_end_point');
{$ELSE}
function  sk4d_shaper_create;                                external SkiaLib;
procedure sk4d_shaper_destroy;                               external SkiaLib;
function  sk4d_shaper_make_bi_di_run_iterator;               external SkiaLib;
function  sk4d_shaper_make_font_run_iterator;                external SkiaLib;
function  sk4d_shaper_make_script_run_iterator;              external SkiaLib;
function  sk4d_shaper_make_std_language_run_iterator;        external SkiaLib;
procedure sk4d_shaper_purge_caches;                          external SkiaLib;
procedure sk4d_shaper_shape;                                 external SkiaLib;
procedure sk4d_shaper_shape2;                                external SkiaLib;
function  sk_shaperbidiruniterator_get_current_level;        external SkiaLib;
function  sk_shaperfontruniterator_get_current_font;         external SkiaLib;
function  sk_shaperlanguageruniterator_get_current_language; external SkiaLib;
procedure sk4d_shaperrunhandler_destroy;                     external SkiaLib;
function  sk4d_shaperrunhandlerbaseclass_create;             external SkiaLib;
procedure sk4d_shaperrunhandlerbaseclass_set_procs;          external SkiaLib;
function  sk4d_shaperruniterator_consume;                    external SkiaLib;
procedure sk4d_shaperruniterator_destroy;                    external SkiaLib;
function  sk4d_shaperruniterator_get_end_of_current_run;     external SkiaLib;
function  sk_shaperscriptruniterator_get_current_script;     external SkiaLib;
function  sk4d_textblobbuilderrunhandler_create;             external SkiaLib;
function  sk4d_textblobbuilderrunhandler_detach;             external SkiaLib;
procedure sk4d_textblobbuilderrunhandler_get_end_point;      external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'modules/svg/include/sk4d_svgdom.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk4d_svgdom_get_root           := GetProcAddress(LibHandle, 'sk4d_svgdom_get_root');
  sk4d_svgdom_make               := GetProcAddress(LibHandle, 'sk4d_svgdom_make');
  sk4d_svgdom_render             := GetProcAddress(LibHandle, 'sk4d_svgdom_render');
  sk4d_svgdom_set_container_size := GetProcAddress(LibHandle, 'sk4d_svgdom_set_container_size');
{$ELSE}
function  sk4d_svgdom_get_root;           external SkiaLib;
function  sk4d_svgdom_make;               external SkiaLib;
procedure sk4d_svgdom_render;             external SkiaLib;
procedure sk4d_svgdom_set_container_size; external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$REGION 'modules/svg/include/sk4d_svgsvg.h'}
{$IFDEF SK_DYNAMIC_LOADING}
  sk_svgsvg_get_intrinsic_size := GetProcAddress(LibHandle, 'sk_svgsvg_get_intrinsic_size');
{$ELSE}
procedure sk_svgsvg_get_intrinsic_size; external SkiaLib;
{$ENDIF}
{$ENDREGION}

{$IFDEF SK_DYNAMIC_LOADING}
  Result := True;
end;

procedure SkFinalize;
begin
  FreeLibrary(LibHandle);
end;
{$ENDIF}

{$IFDEF IOS}
procedure libcompiler_rt; external '/usr/lib/clang/lib/darwin/libclang_rt.ios.a';
procedure libcpp; external '/usr/lib/libc++.dylib';
{$ENDIF}
end.
