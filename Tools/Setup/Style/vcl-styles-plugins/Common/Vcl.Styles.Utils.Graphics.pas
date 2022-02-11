//**************************************************************************************************
//
// Unit Vcl.Styles.Utils.Graphics
// unit for the VCL Styles Utils
// https://github.com/RRUZ/vcl-styles-utils/
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Vcl.Styles.Utils.Graphics.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2012-2015 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************


unit Vcl.Styles.Utils.Graphics;

interface

uses
  System.UITypes,
  System.Classes,
  System.SysUtils,
  Winapi.Windows,
  Vcl.Styles,
  Vcl.Themes,
  Vcl.StdCtrls,
  Vcl.GraphUtil,
  Vcl.Graphics;

{.$DEFINE USE_ZIP}

{$IFDEF USE_ZIP}
  {$R AwesomeFont_zip.RES}
{$ELSE}
  {$R AwesomeFont.RES}
{$ENDIF}

//http://fortawesome.github.io/Font-Awesome/cheatsheet/
//http://prettyprinter.de/index.php
const
  fa_glass = $F000;
  fa_music = $F001;
  fa_search = $F002;
  fa_envelope_o = $F003;
  fa_heart = $F004;
  fa_star = $F005;
  fa_star_o = $F006;
  fa_user = $F007;
  fa_film = $F008;
  fa_th_large = $F009;
  fa_th = $F00A;
  fa_th_list = $F00B;
  fa_check = $F00C;
  fa_remove = $F00D;
  fa_search_plus = $F00E;
  fa_search_minus = $F010;
  fa_power_off = $F011;
  fa_signal = $F012;
  fa_gear = $F013;
  fa_trash_o = $F014;
  fa_home = $F015;
  fa_file_o = $F016;
  fa_clock_o = $F017;
  fa_road = $F018;
  fa_download = $F019;
  fa_arrow_circle_o_down = $F01A;
  fa_arrow_circle_o_up = $F01B;
  fa_inbox = $F01C;
  fa_play_circle_o = $F01D;
  fa_rotate_right = $F01E;
  fa_refresh = $F021;
  fa_list_alt = $F022;
  fa_lock = $F023;
  fa_flag = $F024;
  fa_headphones = $F025;
  fa_volume_off = $F026;
  fa_volume_down = $F027;
  fa_volume_up = $F028;
  fa_qrcode = $F029;
  fa_barcode = $F02A;
  fa_tag = $F02B;
  fa_tags = $F02C;
  fa_book = $F02D;
  fa_bookmark = $F02E;
  fa_print = $F02F;
  fa_camera = $F030;
  fa_font = $F031;
  fa_bold = $F032;
  fa_italic = $F033;
  fa_text_height = $F034;
  fa_text_width = $F035;
  fa_align_left = $F036;
  fa_align_center = $F037;
  fa_align_right = $F038;
  fa_align_justify = $F039;
  fa_list = $F03A;
  fa_dedent = $F03B;
  fa_indent = $F03C;
  fa_video_camera = $F03D;
  fa_photo = $F03E;
  fa_pencil = $F040;
  fa_map_marker = $F041;
  fa_adjust = $F042;
  fa_tint = $F043;
  fa_edit = $F044;
  fa_share_square_o = $F045;
  fa_check_square_o = $F046;
  fa_arrows = $F047;
  fa_step_backward = $F048;
  fa_fast_backward = $F049;
  fa_backward = $F04A;
  fa_play = $F04B;
  fa_pause = $F04C;
  fa_stop = $F04D;
  fa_forward = $F04E;
  fa_fast_forward = $F050;
  fa_step_forward = $F051;
  fa_eject = $F052;
  fa_chevron_left = $F053;
  fa_chevron_right = $F054;
  fa_plus_circle = $F055;
  fa_minus_circle = $F056;
  fa_times_circle = $F057;
  fa_check_circle = $F058;
  fa_question_circle = $F059;
  fa_info_circle = $F05A;
  fa_crosshairs = $F05B;
  fa_times_circle_o = $F05C;
  fa_check_circle_o = $F05D;
  fa_ban = $F05E;
  fa_arrow_left = $F060;
  fa_arrow_right = $F061;
  fa_arrow_up = $F062;
  fa_arrow_down = $F063;
  fa_mail_forward = $F064;
  fa_expand = $F065;
  fa_compress = $F066;
  fa_plus = $F067;
  fa_minus = $F068;
  fa_asterisk = $F069;
  fa_exclamation_circle = $F06A;
  fa_gift = $F06B;
  fa_leaf = $F06C;
  fa_fire = $F06D;
  fa_eye = $F06E;
  fa_eye_slash = $F070;
  fa_warning = $F071;
  fa_plane = $F072;
  fa_calendar = $F073;
  fa_random = $F074;
  fa_comment = $F075;
  fa_magnet = $F076;
  fa_chevron_up = $F077;
  fa_chevron_down = $F078;
  fa_retweet = $F079;
  fa_shopping_cart = $F07A;
  fa_folder = $F07B;
  fa_folder_open = $F07C;
  fa_arrows_v = $F07D;
  fa_arrows_h = $F07E;
  fa_bar_chart_o = $F080;
  fa_twitter_square = $F081;
  fa_facebook_square = $F082;
  fa_camera_retro = $F083;
  fa_key = $F084;
  fa_gears = $F085;
  fa_comments = $F086;
  fa_thumbs_o_up = $F087;
  fa_thumbs_o_down = $F088;
  fa_star_half = $F089;
  fa_heart_o = $F08A;
  fa_sign_out = $F08B;
  fa_linkedin_square = $F08C;
  fa_thumb_tack = $F08D;
  fa_external_link = $F08E;
  fa_sign_in = $F090;
  fa_trophy = $F091;
  fa_github_square = $F092;
  fa_upload = $F093;
  fa_lemon_o = $F094;
  fa_phone = $F095;
  fa_square_o = $F096;
  fa_bookmark_o = $F097;
  fa_phone_square = $F098;
  fa_twitter = $F099;
  fa_facebook_f = $F09A;
  fa_github = $F09B;
  fa_unlock = $F09C;
  fa_credit_card = $F09D;
  fa_rss = $F09E;
  fa_hdd_o = $F0A0;
  fa_bullhorn = $F0A1;
  fa_bell = $F0F3;
  fa_certificate = $F0A3;
  fa_hand_o_right = $F0A4;
  fa_hand_o_left = $F0A5;
  fa_hand_o_up = $F0A6;
  fa_hand_o_down = $F0A7;
  fa_arrow_circle_left = $F0A8;
  fa_arrow_circle_right = $F0A9;
  fa_arrow_circle_up = $F0AA;
  fa_arrow_circle_down = $F0AB;
  fa_globe = $F0AC;
  fa_wrench = $F0AD;
  fa_tasks = $F0AE;
  fa_filter = $F0B0;
  fa_briefcase = $F0B1;
  fa_arrows_alt = $F0B2;
  fa_group = $F0C0;
  fa_chain = $F0C1;
  fa_cloud = $F0C2;
  fa_flask = $F0C3;
  fa_cut = $F0C4;
  fa_copy = $F0C5;
  fa_paperclip = $F0C6;
  fa_save = $F0C7;
  fa_square = $F0C8;
  fa_navicon = $F0C9;
  fa_list_ul = $F0CA;
  fa_list_ol = $F0CB;
  fa_strikethrough = $F0CC;
  fa_underline = $F0CD;
  fa_table = $F0CE;
  fa_magic = $F0D0;
  fa_truck = $F0D1;
  fa_pinterest = $F0D2;
  fa_pinterest_square = $F0D3;
  fa_google_plus_square = $F0D4;
  fa_google_plus = $F0D5;
  fa_money = $F0D6;
  fa_caret_down = $F0D7;
  fa_caret_up = $F0D8;
  fa_caret_left = $F0D9;
  fa_caret_right = $F0DA;
  fa_columns = $F0DB;
  fa_unsorted = $F0DC;
  fa_sort_down = $F0DD;
  fa_sort_up = $F0DE;
  fa_envelope = $F0E0;
  fa_linkedin = $F0E1;
  fa_rotate_left = $F0E2;
  fa_legal = $F0E3;
  fa_dashboard = $F0E4;
  fa_comment_o = $F0E5;
  fa_comments_o = $F0E6;
  fa_flash = $F0E7;
  fa_sitemap = $F0E8;
  fa_umbrella = $F0E9;
  fa_paste = $F0EA;
  fa_lightbulb_o = $F0EB;
  fa_exchange = $F0EC;
  fa_cloud_download = $F0ED;
  fa_cloud_upload = $F0EE;
  fa_user_md = $F0F0;
  fa_stethoscope = $F0F1;
  fa_suitcase = $F0F2;
  fa_bell_o = $F0A2;
  fa_coffee = $F0F4;
  fa_cutlery = $F0F5;
  fa_file_text_o = $F0F6;
  fa_building_o = $F0F7;
  fa_hospital_o = $F0F8;
  fa_ambulance = $F0F9;
  fa_medkit = $F0FA;
  fa_fighter_jet = $F0FB;
  fa_beer = $F0FC;
  fa_h_square = $F0FD;
  fa_plus_square = $F0FE;
  fa_angle_double_left = $F100;
  fa_angle_double_right = $F101;
  fa_angle_double_up = $F102;
  fa_angle_double_down = $F103;
  fa_angle_left = $F104;
  fa_angle_right = $F105;
  fa_angle_up = $F106;
  fa_angle_down = $F107;
  fa_desktop = $F108;
  fa_laptop = $F109;
  fa_tablet = $F10A;
  fa_mobile_phone = $F10B;
  fa_circle_o = $F10C;
  fa_quote_left = $F10D;
  fa_quote_right = $F10E;
  fa_spinner = $F110;
  fa_circle = $F111;
  fa_mail_reply = $F112;
  fa_github_alt = $F113;
  fa_folder_o = $F114;
  fa_folder_open_o = $F115;
  fa_smile_o = $F118;
  fa_frown_o = $F119;
  fa_meh_o = $F11A;
  fa_gamepad = $F11B;
  fa_keyboard_o = $F11C;
  fa_flag_o = $F11D;
  fa_flag_checkered = $F11E;
  fa_terminal = $F120;
  fa_code = $F121;
  fa_mail_reply_all = $F122;
  fa_star_half_empty = $F123;
  fa_location_arrow = $F124;
  fa_crop = $F125;
  fa_code_fork = $F126;
  fa_unlink = $F127;
  fa_question = $F128;
  fa_info = $F129;
  fa_exclamation = $F12A;
  fa_superscript = $F12B;
  fa_subscript = $F12C;
  fa_eraser = $F12D;
  fa_puzzle_piece = $F12E;
  fa_microphone = $F130;
  fa_microphone_slash = $F131;
  fa_shield = $F132;
  fa_calendar_o = $F133;
  fa_fire_extinguisher = $F134;
  fa_rocket = $F135;
  fa_maxcdn = $F136;
  fa_chevron_circle_left = $F137;
  fa_chevron_circle_right = $F138;
  fa_chevron_circle_up = $F139;
  fa_chevron_circle_down = $F13A;
  fa_html5 = $F13B;
  fa_css3 = $F13C;
  fa_anchor = $F13D;
  fa_unlock_alt = $F13E;
  fa_bullseye = $F140;
  fa_ellipsis_h = $F141;
  fa_ellipsis_v = $F142;
  fa_rss_square = $F143;
  fa_play_circle = $F144;
  fa_ticket = $F145;
  fa_minus_square = $F146;
  fa_minus_square_o = $F147;
  fa_level_up = $F148;
  fa_level_down = $F149;
  fa_check_square = $F14A;
  fa_pencil_square = $F14B;
  fa_external_link_square = $F14C;
  fa_share_square = $F14D;
  fa_compass = $F14E;
  fa_toggle_down = $F150;
  fa_toggle_up = $F151;
  fa_toggle_right = $F152;
  fa_euro = $F153;
  fa_gbp = $F154;
  fa_dollar = $F155;
  fa_rupee = $F156;
  fa_cny = $F157;
  fa_ruble = $F158;
  fa_won = $F159;
  fa_bitcoin = $F15A;
  fa_file = $F15B;
  fa_file_text = $F15C;
  fa_sort_alpha_asc = $F15D;
  fa_sort_alpha_desc = $F15E;
  fa_sort_amount_asc = $F160;
  fa_sort_amount_desc = $F161;
  fa_sort_numeric_asc = $F162;
  fa_sort_numeric_desc = $F163;
  fa_thumbs_up = $F164;
  fa_thumbs_down = $F165;
  fa_youtube_square = $F166;
  fa_youtube = $F167;
  fa_xing = $F168;
  fa_xing_square = $F169;
  fa_youtube_play = $F16A;
  fa_dropbox = $F16B;
  fa_stack_overflow = $F16C;
  fa_instagram = $F16D;
  fa_flickr = $F16E;
  fa_adn = $F170;
  fa_bitbucket = $F171;
  fa_bitbucket_square = $F172;
  fa_tumblr = $F173;
  fa_tumblr_square = $F174;
  fa_long_arrow_down = $F175;
  fa_long_arrow_up = $F176;
  fa_long_arrow_left = $F177;
  fa_long_arrow_right = $F178;
  fa_apple = $F179;
  fa_windows = $F17A;
  fa_android = $F17B;
  fa_linux = $F17C;
  fa_dribbble = $F17D;
  fa_skype = $F17E;
  fa_foursquare = $F180;
  fa_trello = $F181;
  fa_female = $F182;
  fa_male = $F183;
  fa_gittip = $F184;
  fa_sun_o = $F185;
  fa_moon_o = $F186;
  fa_archive = $F187;
  fa_bug = $F188;
  fa_vk = $F189;
  fa_weibo = $F18A;
  fa_renren = $F18B;
  fa_pagelines = $F18C;
  fa_stack_exchange = $F18D;
  fa_arrow_circle_o_right = $F18E;
  fa_arrow_circle_o_left = $F190;
  fa_toggle_left = $F191;
  fa_dot_circle_o = $F192;
  fa_wheelchair = $F193;
  fa_vimeo_square = $F194;
  fa_turkish_lira = $F195;
  fa_plus_square_o = $F196;
  fa_space_shuttle = $F197;
  fa_slack = $F198;
  fa_envelope_square = $F199;
  fa_wordpress = $F19A;
  fa_openid = $F19B;
  fa_institution = $F19C;
  fa_mortar_board = $F19D;
  fa_yahoo = $F19E;
  fa_google = $F1A0;
  fa_reddit = $F1A1;
  fa_reddit_square = $F1A2;
  fa_stumbleupon_circle = $F1A3;
  fa_stumbleupon = $F1A4;
  fa_delicious = $F1A5;
  fa_digg = $F1A6;
  fa_pied_piper = $F1A7;
  fa_pied_piper_alt = $F1A8;
  fa_drupal = $F1A9;
  fa_joomla = $F1AA;
  fa_language = $F1AB;
  fa_fax = $F1AC;
  fa_building = $F1AD;
  fa_child = $F1AE;
  fa_paw = $F1B0;
  fa_spoon = $F1B1;
  fa_cube = $F1B2;
  fa_cubes = $F1B3;
  fa_behance = $F1B4;
  fa_behance_square = $F1B5;
  fa_steam = $F1B6;
  fa_steam_square = $F1B7;
  fa_recycle = $F1B8;
  fa_automobile = $F1B9;
  fa_cab = $F1BA;
  fa_tree = $F1BB;
  fa_spotify = $F1BC;
  fa_deviantart = $F1BD;
  fa_soundcloud = $F1BE;
  fa_database = $F1C0;
  fa_file_pdf_o = $F1C1;
  fa_file_word_o = $F1C2;
  fa_file_excel_o = $F1C3;
  fa_file_powerpoint_o = $F1C4;
  fa_file_photo_o  = $F1C5;
  fa_file_zip_o = $F1C6;
  fa_file_sound_o = $F1C7;
  fa_file_movie_o = $F1C8;
  fa_file_code_o = $F1C9;
  fa_vine = $F1CA;
  fa_codepen = $F1CB;
  fa_jsfiddle = $F1CC;
  fa_life_bouy = $F1CD;
  fa_circle_o_notch = $F1CE;
  fa_ra = $F1D0;
  fa_ge = $F1D1;
  fa_git_square = $F1D2;
  fa_git = $F1D3;
  fa_hacker_news = $F1D4;
  fa_tencent_weibo = $F1D5;
  fa_qq = $F1D6;
  fa_wechat = $F1D7;
  fa_send = $F1D8;
  fa_send_o = $F1D9;
  fa_history = $F1DA;
  fa_genderless = $F1DB;
  fa_header = $F1DC;
  fa_paragraph = $F1DD;
  fa_sliders = $F1DE;
  fa_share_alt = $F1E0;
  fa_share_alt_square = $F1E1;
  fa_bomb = $F1E2;
  fa_soccer_ball_o = $F1E3;
  fa_tty = $F1E4;
  fa_binoculars = $F1E5;
  fa_plug = $F1E6;
  fa_slideshare = $F1E7;
  fa_twitch = $F1E8;
  fa_yelp = $F1E9;
  fa_newspaper_o = $F1EA;
  fa_wifi = $F1EB;
  fa_calculator = $F1EC;
  fa_paypal = $F1ED;
  fa_google_wallet = $F1EE;
  fa_cc_visa = $F1F0;
  fa_cc_mastercard = $F1F1;
  fa_cc_discover = $F1F2;
  fa_cc_amex = $F1F3;
  fa_cc_paypal = $F1F4;
  fa_cc_stripe = $F1F5;
  fa_bell_slash = $F1F6;
  fa_bell_slash_o = $F1F7;
  fa_trash = $F1F8;
  fa_copyright = $F1F9;
  fa_at = $F1FA;
  fa_eyedropper = $F1FB;
  fa_paint_brush = $F1FC;
  fa_birthday_cake = $F1FD;
  fa_area_chart = $F1FE;
  fa_pie_chart = $F200;
  fa_line_chart = $F201;
  fa_lastfm = $F202;
  fa_lastfm_square = $F203;
  fa_toggle_off = $F204;
  fa_toggle_on = $F205;
  fa_bicycle = $F206;
  fa_bus = $F207;
  fa_ioxhost = $F208;
  fa_angellist = $F209;
  fa_cc = $F20A;
  fa_shekel = $F20B;
  fa_meanpath = $F20C;
  fa_buysellads = $F20D;
  fa_connectdevelop = $F20E;
  fa_dashcube = $F210;
  fa_forumbee = $F211;
  fa_leanpub = $F212;
  fa_sellsy = $F213;
  fa_shirtsinbulk = $F214;
  fa_simplybuilt = $F215;
  fa_skyatlas = $F216;
  fa_cart_plus = $F217;
  fa_cart_arrow_down = $F218;
  fa_diamond = $F219;
  fa_ship = $F21A;
  fa_user_secret = $F21B;
  fa_motorcycle = $F21C;
  fa_street_view = $F21D;
  fa_heartbeat = $F21E;
  fa_venus = $F221;
  fa_mars = $F222;
  fa_mercury = $F223;
  fa_transgender = $F224;
  fa_transgender_alt = $F225;
  fa_venus_double = $F226;
  fa_mars_double = $F227;
  fa_venus_mars = $F228;
  fa_mars_stroke = $F229;
  fa_mars_stroke_v = $F22A;
  fa_mars_stroke_h = $F22B;
  fa_neuter = $F22C;
  fa_facebook_official = $F230;
  fa_pinterest_p = $F231;
  fa_whatsapp = $F232;
  fa_server = $F233;
  fa_user_plus = $F234;
  fa_user_times = $F235;
  fa_hotel = $F236;
  fa_viacoin = $F237;
  fa_train = $F238;
  fa_subway = $F239;
  fa_medium = $F23A;

type
  //http://fortawesome.github.io/Font-Awesome/cheatsheet/
  TAwesomeFont = class
  private
    FFontHandle : THandle;
    FDefaultQuality : DWORD;
    procedure LoadFontFromResource;
  public
    constructor Create;
    Destructor Destroy; override;
    procedure DrawChar(DC: HDC; const AChar: Char; DestRect: TRect; AColor : TColor; Orientation : Integer = 0; ImageAlignment: TImageAlignment = iaLeft); overload;
    procedure DrawChar(DC: HDC; const ACode: Word; DestRect: TRect; AColor : TColor; Orientation : Integer = 0; ImageAlignment: TImageAlignment = iaLeft); overload;
    function  GetIcon(const ACode: Word; Width, Height : Integer; AColor, ABackColor : TColor; Orientation : Integer = 0; ImageAlignment: TImageAlignment = iaLeft) : HICON; overload;
    function  GetIcon(const ACode: Word; Width, Height, CharX, CharY : Integer; AColor, ABackColor : TColor; Orientation : Integer = 0; ImageAlignment: TImageAlignment = iaLeft) : HICON; overload;
  end;


  TImageFilterCallback  = procedure (const AColor: TColor;Value: Integer; out NewColor:TColor);

const
  MaxHue = 180;
  MinHue = -180;
  DefHue = 0;

  MaxSat = 255;
  MinSat = 0;
  DefSat = 0;

  MaxLig = 255;
  MinLig = -255;
  DefLig = 0;

  procedure _ProcessBitmap32(const Dest: TBitmap;Value: Integer;_Process:TImageFilterCallback); overload;
  procedure _ProcessBitmap24(const ABitMap: TBitmap; Value: Integer; _Process:TImageFilterCallback); overload;


  procedure GetRGB(Col: TColor; var R, G, B: byte);
  function  _HSLtoRGB(HueValue, SaturationValue, LightValue: Double): TColor;
  procedure _RGBtoHSL(RGB: TColor; var HueValue, SaturationValue, LightValue: Double);

  procedure _Hue(const AColor: TColor;Value: Integer; out NewColor:TColor);
  procedure _Hue24(var ABitMap: TBitmap; Value: integer);
  procedure _Hue32(const ABitMap: TBitmap; Value: integer);

  procedure _Sepia(const AColor: TColor;Value: Integer; out NewColor:TColor);
  procedure _Sepia24(const ABitMap: TBitmap;Value : Byte=32);
  procedure _Sepia32(const ABitMap: TBitmap;Value : Byte=32);

  procedure _BlendMultiply(const AColor: TColor;Value: Integer; out NewColor:TColor);
  procedure _BlendMultiply24(const ABitMap: TBitmap;Value: Integer);
  procedure _BlendMultiply32(const ABitMap: TBitmap;Value: Integer);

  procedure _Lightness(const AColor: TColor;Value: Integer; out NewColor:TColor);
  procedure _Lightness24(var ABitMap: TBitmap; Value: integer);
  procedure _Lightness32(const ABitMap: TBitmap; Value: integer);

  procedure _Darkness(const AColor: TColor;Value: Integer; out NewColor:TColor);
  procedure _Darkness24(var ABitMap: TBitmap; Value: integer);
  procedure _Darkness32(const ABitMap: TBitmap; Value: integer);

  procedure _Saturation(const AColor: TColor;Value: Integer; out NewColor:TColor);
  procedure _Saturation24(var ABitMap: TBitmap; Value: integer);
  procedure _Saturation32(const ABitMap: TBitmap; Value: integer);

  procedure _SetRComponent(const AColor: TColor;Value: Integer; out NewColor:TColor);
  procedure _SetGComponent(const AColor: TColor;Value: Integer; out NewColor:TColor);
  procedure _SetBComponent(const AColor: TColor;Value: Integer; out NewColor:TColor);

  procedure _SetRGB24(const ABitMap: TBitmap; DR,DG,DB: Integer);
  procedure _SetRGB32(const ABitMap: TBitmap; DR,DG,DB: Integer);

  procedure _BlendBurn(const AColor: TColor;Value: Integer; out NewColor:TColor);
  procedure _BlendBurn24(const ABitMap: TBitmap;Value: Integer);
  procedure _BlendBurn32(const ABitMap: TBitmap;Value: Integer);

  procedure _BlendAdditive(const AColor: TColor;Value: Integer; out NewColor:TColor);
  procedure _BlendAdditive24(const ABitMap: TBitmap;Value: Integer);
  procedure _BlendAdditive32(const ABitMap: TBitmap;Value: Integer);

  procedure _BlendDodge(const AColor: TColor;Value: Integer; out NewColor:TColor);
  procedure _BlendDodge24(const ABitMap: TBitmap;Value: Integer);
  procedure _BlendDodge32(const ABitMap: TBitmap;Value: Integer);

  procedure _BlendOverlay(const AColor: TColor;Value: Integer; out NewColor:TColor);
  procedure _BlendOverlay24(const ABitMap: TBitmap;Value: Integer);
  procedure _BlendOverlay32(const ABitMap: TBitmap;Value: Integer);

  procedure _BlendDifference(const AColor: TColor;Value: Integer; out NewColor:TColor);
  procedure _BlendDifference24(const ABitMap: TBitmap;Value: Integer);
  procedure _BlendDifference32(const ABitMap: TBitmap;Value: Integer);

  procedure _BlendLighten(const AColor: TColor;Value: Integer; out NewColor:TColor);
  procedure _BlendLighten24(const ABitMap: TBitmap;Value: Integer);
  procedure _BlendLighten32(const ABitMap: TBitmap;Value: Integer);

  procedure _BlendDarken(const AColor: TColor;Value: Integer; out NewColor:TColor);
  procedure _BlendDarken24(const ABitMap: TBitmap;Value: Integer);
  procedure _BlendDarken32(const ABitMap: TBitmap;Value: Integer);

  procedure _BlendScreen(const AColor: TColor;Value: Integer; out NewColor:TColor);
  procedure _BlendScreen24(const ABitMap: TBitmap;Value: Integer);
  procedure _BlendScreen32(const ABitMap: TBitmap;Value: Integer);

  procedure Bitmap24_Grayscale(ABitmap: TBitmap);
  procedure Bitmap32_Grayscale(ABitmap: TBitmap);
  //Set the Alpha and Color of a 32 bit Bitmap
  procedure Bitmap32_SetAlphaAndColor(ABitmap: TBitmap; AlphaValue : Byte; AColor: TColor);
  //Set the Alpha value for a specific Color of a 32 bit Bitmap
  procedure Bitmap32_SetAlphaByColor(ABitmap: TBitmap; AlphaValue : Byte; AColor: TColor);

  //Set the Alpha value for all Colors, execept the Color Param of a 32 bit Bitmap
  procedure Bitmap32_SetAlphaExceptColor(ABitmap: TBitmap; AlphaValue : Byte; AColor: TColor);

 type
  TColorFilter=class
  private
   FColorValue : Integer;
  public
   constructor Create(AColorValue:Integer);
   property ColorValue : Integer read FColorValue Write FColorValue;
   function ProcessColor(AColor: TColor):TColor;virtual;abstract;
  end;

  TBitmapFilter=class(TColorFilter)
  private
   //FColorValue   : Integer;
   FUseBitmap: Boolean;
   FSourceBitmap : TBitmap;
  public
   constructor Create(AColorValue:Integer);
   constructor CreateBitMap(ASourceBitmap :TBitmap);
   procedure ProcessBitmap(ABitMap: TBitmap);virtual;abstract;
  end;

  TBitmap32HueFilter=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32SaturationFilter=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32LightnessFilter=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32SepiaFilter=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32RedFilter=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32GreenFilter=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlueFilter=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlendBurn=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlendMultiply=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;


  TBitmap32BlendAdditive=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlendDodge=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlendOverlay=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlendDifference=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlendLighten=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlendDarken=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlendScreen=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  procedure GradientRoundedFillCanvas(const ACanvas: TCanvas;
  const AStartColor, AEndColor: TColor; const ARect: TRect;
  const Direction: TGradientDirection; Radius : Integer);

  procedure AlphaBlendFillCanvas(const ACanvas: TCanvas;  const AColor : TColor;const ARect: TRect; SourceConstantAlpha : Byte); overload;
  procedure AlphaBlendFillCanvas(const DC: HDC;  const AColor : TColor;const ARect: TRect; SourceConstantAlpha : Byte); overload;

  procedure AlphaBlendRectangle(const ACanvas: TCanvas;  const AColor : TColor;const ARect: TRect; SourceConstantAlpha : Byte); overload;
  procedure AlphaBlendRectangle(const DC: HDC;  const AColor : TColor;const ARect: TRect; SourceConstantAlpha : Byte); overload;


  procedure DrawStyleElement(hdc : HDC; LDetails  : TThemedElementDetails; pRect : TRect; RestoreDC : Boolean = True);
  procedure DrawStyleDownArrow(hdc : HDC; LRect : TRect; AColor :TColor);
  procedure DrawStyleFillRect(hdc : HDC; LRect : TRect; AColor :TColor);
  procedure DrawStyleRectangle(hdc : HDC; LRect : TRect; AColor :TColor);


  procedure DrawStyleArrow(hdc : HDC; Direction: TScrollDirection; Location: TPoint; Size: Integer; AColor: TColor);
  procedure DrawStyleParentBackground(Handle : THandle; DC: HDC; const ARect: TRect);
  procedure DrawStyleParentBackgroundEx(Handle : THandle; DC: HDC; const ARect: TRect);


  procedure RotateBitmap(ABitMap: TBitmap; Rads: Single; AdjustSize: Boolean; BackGroundColor: TColor = clNone);
  procedure FlipBitmap24Horizontal(ABitMap : TBitmap);
  procedure FlipBitmap32Horizontal(ABitMap : TBitmap);

  function ColorIsBright(AColor : TColor) : Boolean;

var
  AwesomeFont :  TAwesomeFont;

implementation

uses
  Winapi.Messages,
  {$IFDEF USE_ZIP}
  System.Zip,
  {$ENDIF}
  System.Types,
  System.Math;


type
  PRGBArray24 = ^TRGBArray24;
  TRGBArray24 = array[0..0] of TRGBTriple;

  PRGBArray32 = ^TRGBArray32;
  TRGBArray32 = array[0..0] of TRGBQuad;


type
  TMirrorKind = (mtHorizontal, mtVertical, mtBoth );

procedure MirrorBitMap(ABitMap : TBitmap; MirrorType: TMirrorKind);
var
  LRect: TRect;
begin

    case MirrorType of

      mtHorizontal:
        begin
          LRect.Left := ABitMap.Width;
          LRect.Top := 0;
          LRect.Right := -ABitMap.Width;
          LRect.Bottom := ABitMap.Height
        end;

      mtVertical:
        begin
          LRect.Left := 0;
          LRect.Top := ABitMap.Height;
          LRect.Right := ABitMap.Width;
          LRect.Bottom := -ABitMap.Height
        end;

      mtBoth:
        begin
          LRect.Left := ABitMap.Width;
          LRect.Top := ABitMap.Height;
          LRect.Right := -ABitMap.Width;
          LRect.Bottom := -ABitMap.Height
        end;

    end;

    StretchBlt(ABitMap.Canvas.Handle, LRect.Left, LRect.Top, LRect.Right, LRect.Bottom,
               ABitMap.Canvas.Handle, 0, 0, ABitMap.Width, ABitMap.Height,
               SRCCOPY);
end;

procedure GetRGB(Col: TColor; var R, G, B: byte);
var
  Color: $0..$FFFFFFFF;
begin
  Color := ColorToRGB(Col);
  R     := ($000000FF and Color);
  G     := ($0000FF00 and Color) shr 8;
  B     := ($00FF0000 and Color) shr 16;
end;


function ColorIsBright(AColor : TColor) : Boolean;
var
 R, G, B : byte;
 Delta : Double;
begin
  GetRGB(AColor, R, G, B);
  Delta := 1 - ( (0.299 * R) + (0.587 * G) + (0.114 * B) )/255;
  Result:= (Delta < 0.5);
end;

procedure _FlipBitmap24Horizontal(ABitMap : TBitmap);
var
  LRGBArray24 : PRGBArray24;
  LRGBTriple  : TRGBTriple;
  x, y        : Integer;
begin
   for y := 0 to ABitMap.Height -1 do
   begin
     LRGBArray24 := ABitMap.ScanLine[y];
     for x := 0 to ABitMap.Width div 2 do
     begin
      {$IFOPT R+}
        {$DEFINE RANGEON}
        {$R-}
      {$ELSE}
        {$UNDEF RANGEON}
      {$ENDIF}
       LRGBTriple := LRGBArray24[x];
       LRGBArray24[x] := LRGBArray24[ABitMap.Width -x -1];
       LRGBArray24[ABitMap.Width -x -1] := LRGBTriple;
      {$IFDEF RANGEON}
        {$R+}
        {$UNDEF RANGEON}
      {$ENDIF}
     end;
   end;
end;

procedure _FlipBitmap32Horizontal(ABitMap : TBitmap);
var
  LRGBArray32 : PRGBArray32;
  LRGBQuad  : TRGBQuad;
  x, y        : Integer;
begin
   if ABitMap.PixelFormat<>pf32bit then  exit;

   for y := 0 to ABitMap.Height -1 do
   begin
     LRGBArray32 := ABitMap.ScanLine[y];
     for x := 0 to ABitMap.Width div 2 do
     begin
      {$IFOPT R+}
        {$DEFINE RANGEON}
        {$R-}
      {$ELSE}
        {$UNDEF RANGEON}
      {$ENDIF}
       LRGBQuad := LRGBArray32[x];
       LRGBArray32[x] := LRGBArray32[ABitMap.Width -x -1];
       LRGBArray32[ABitMap.Width -x -1] := LRGBQuad;
      {$IFDEF RANGEON}
        {$R+}
        {$UNDEF RANGEON}
      {$ENDIF}
     end;
   end;
end;


procedure FlipBitmap24Horizontal(ABitMap : TBitmap);
begin
   if ABitMap.PixelFormat<>pf24bit then  exit;
     MirrorBitMap(ABitMap, TMirrorKind.mtHorizontal);
end;

procedure FlipBitmap32Horizontal(ABitMap : TBitmap);
begin
   if ABitMap.PixelFormat<>pf32bit then  exit;
     MirrorBitMap(ABitMap, TMirrorKind.mtHorizontal);
end;

procedure RotateBitmap(ABitMap: TBitmap; Rads: Single; AdjustSize: Boolean; BackGroundColor: TColor = clNone);
var
  C: Single;
  S: Single;
  LXForm: TXForm;
  LBuffer: TBitmap;
begin
  C := Cos(Rads);
  S := Sin(Rads);
  LXForm.eM11 := C;
  LXForm.eM12 := S;
  LXForm.eM21 := -S;
  LXForm.eM22 := C;
  LBuffer := TBitmap.Create;
  try
    LBuffer.TransparentColor := ABitMap.TransparentColor;
    LBuffer.TransparentMode := ABitMap.TransparentMode;
    LBuffer.Transparent := ABitMap.Transparent;
    LBuffer.Canvas.Brush.Color := BackGroundColor;
    if AdjustSize then
    begin
      LBuffer.Width := Round(ABitMap.Width * Abs(C) + ABitMap.Height * Abs(S));
      LBuffer.Height := Round(ABitMap.Width * Abs(S) + ABitMap.Height * Abs(C));
      LXForm.eDx := (LBuffer.Width - ABitMap.Width * C + ABitMap.Height * S) / 2;
      LXForm.eDy := (LBuffer.Height - ABitMap.Width * S - ABitMap.Height * C) / 2;
    end
    else
    begin
      LBuffer.Width := ABitMap.Width;
      LBuffer.Height := ABitMap.Height;
      LXForm.eDx := (ABitMap.Width - ABitMap.Width * C + ABitMap.Height * S) / 2;
      LXForm.eDy := (ABitMap.Height - ABitMap.Width * S - ABitMap.Height * C) / 2;
    end;
    SetGraphicsMode(LBuffer.Canvas.Handle, GM_ADVANCED);
    SetWorldTransform(LBuffer.Canvas.Handle, LXForm);
    BitBlt(LBuffer.Canvas.Handle, 0, 0, LBuffer.Width, LBuffer.Height, ABitMap.Canvas.Handle, 0, 0, SRCCOPY);
    ABitMap.Assign(LBuffer);
  finally
    LBuffer.Free;
  end;
end;

procedure Bitmap24_Grayscale(ABitmap: TBitmap);
var
  X: Integer;
  Y: Integer;
  LGrayColor: Byte;
  LRGBTriple: PRGBTriple;
begin
  if ABitmap.PixelFormat<>pf24bit then Exit;

  for Y := 0 to ABitmap.Height - 1 do
  begin
    LRGBTriple := ABitmap.ScanLine[Y];
    for X := 0 to ABitmap.Width - 1 do
    begin
      LGrayColor := Round((0.299 * LRGBTriple.rgbtRed) + (0.587 * LRGBTriple.rgbtGreen) + (0.114 * LRGBTriple.rgbtBlue));
      LRGBTriple.rgbtRed   := LGrayColor;
      LRGBTriple.rgbtGreen := LGrayColor;
      LRGBTriple.rgbtBlue  := LGrayColor;
      Inc(LRGBTriple);
    end;
  end;
end;


procedure Bitmap32_SetAlphaAndColor(ABitmap: TBitmap; AlphaValue : Byte; AColor: TColor);
var
  X: Integer;
  Y: Integer;
  LRGBQuad: PRGBQuad;
  R, G, B : Byte;
begin
  GetRGB(AColor, R, G, B);
  if ABitmap.PixelFormat<>pf32bit then Exit;
  for Y := 0 to ABitmap.Height - 1 do
  begin
    LRGBQuad := ABitmap.ScanLine[Y];
    for X := 0 to ABitmap.Width - 1 do
    begin

      LRGBQuad.rgbRed   := R;
      LRGBQuad.rgbGreen := G;
      LRGBQuad.rgbBlue  := B;
      LRGBQuad.rgbReserved :=  AlphaValue;
      Inc(LRGBQuad);
    end;
  end;
end;

procedure Bitmap32_SetAlphaByColor(ABitmap: TBitmap; AlphaValue : Byte; AColor: TColor);
var
  X: Integer;
  Y: Integer;
  LRGBQuad: PRGBQuad;
begin
  if ABitmap.PixelFormat<>pf32bit then Exit;
  for Y := 0 to ABitmap.Height - 1 do
  begin
    LRGBQuad := ABitmap.ScanLine[Y];
    for X := 0 to ABitmap.Width - 1 do
    begin
      if Cardinal(ColorToRGB(AColor)) = RGB(LRGBQuad.rgbRed, LRGBQuad.rgbGreen, LRGBQuad.rgbBlue ) then
        LRGBQuad.rgbReserved :=  AlphaValue;
      Inc(LRGBQuad);
    end;
  end;
end;

procedure Bitmap32_SetAlphaExceptColor(ABitmap: TBitmap; AlphaValue : Byte; AColor: TColor);
var
  X: Integer;
  Y: Integer;
  LRGBQuad: PRGBQuad;
  LColorRef : COLORREF;
begin
  if ABitmap.PixelFormat<>pf32bit then Exit;

  LColorRef:=Cardinal(ColorToRGB(AColor));
  for Y := 0 to ABitmap.Height - 1 do
  begin
    LRGBQuad := ABitmap.ScanLine[Y];
    for X := 0 to ABitmap.Width - 1 do
    begin
      if  LColorRef <> RGB(LRGBQuad.rgbRed, LRGBQuad.rgbGreen, LRGBQuad.rgbBlue) then
        LRGBQuad.rgbReserved :=  AlphaValue;
      Inc(LRGBQuad);
    end;
  end;
end;

procedure Bitmap32_Grayscale(ABitmap: TBitmap);
var
  X: Integer;
  Y: Integer;
  LGrayColor: Byte;
  LRGBQuad: PRGBQuad;
begin
  if ABitmap.PixelFormat<>pf32bit then Exit;

  for Y := 0 to ABitmap.Height - 1 do
  begin
    LRGBQuad := ABitmap.ScanLine[Y];
    for X := 0 to ABitmap.Width - 1 do
    begin
      LGrayColor:= Round((0.299 * LRGBQuad.rgbRed) + (0.587 * LRGBQuad.rgbGreen) + (0.114 * LRGBQuad.rgbBlue));
      LRGBQuad.rgbRed   := LGrayColor;
      LRGBQuad.rgbGreen := LGrayColor;
      LRGBQuad.rgbBlue  := LGrayColor;
      Inc(LRGBQuad);
    end;
  end;
end;

procedure DrawStyleArrow(hdc : HDC; Direction: TScrollDirection; Location: TPoint; Size: Integer; AColor: TColor);
var
  SaveIndex : Integer;
  LCanvas   : TCanvas;
begin
  SaveIndex := SaveDC(hdc);
  LCanvas:=TCanvas.Create;
  try
    LCanvas.Handle:=hdc;
    LCanvas.Pen.Color:=AColor;
    LCanvas.Brush.Style:=bsClear;
    DrawArrow(LCanvas, Direction, Location, Size);
  finally
    LCanvas.Handle:=0;
    LCanvas.Free;
    RestoreDC(hdc, SaveIndex);
  end;
end;



procedure DrawStyleFillRect(hdc : HDC; LRect : TRect; AColor :TColor);
var
 SaveIndex : Integer;
 LCanvas : TCanvas;
begin
  LCanvas:=TCanvas.Create;
  SaveIndex := SaveDC(hdc);
  try
    LCanvas.Handle:=hdc;
    LCanvas.Brush.Color:=AColor;
    //LCanvas.Rectangle(LRect.Left, LRect.Top, LRect.Left +  LRect.Width,  LRect.Top + LRect.Height);
    LCanvas.FillRect(LRect);
  finally
    LCanvas.Handle:=0;
    LCanvas.Free;
    RestoreDC(hdc, SaveIndex);
  end;
end;

procedure DrawStyleRectangle(hdc : HDC; LRect : TRect; AColor :TColor);
var
 SaveIndex : Integer;
 LCanvas : TCanvas;
begin
  LCanvas:=TCanvas.Create;
  SaveIndex := SaveDC(hdc);
  try
    LCanvas.Handle:=hdc;
    LCanvas.Brush.Style:=bsClear;
    LCanvas.Pen.Color:=AColor;
    LCanvas.Rectangle(LRect.Left, LRect.Top, LRect.Left + LRect.Width,  LRect.Top + LRect.Height);
  finally
    LCanvas.Handle:=0;
    LCanvas.Free;
    RestoreDC(hdc, SaveIndex);
  end;
end;


procedure DrawStyleDownArrow(hdc : HDC; LRect : TRect; AColor :TColor);
var
 SaveIndex, X, Y, I : Integer;
 LColor : TColor;
 LCanvas : TCanvas;
begin
  SaveIndex := SaveDC(hdc);
  LCanvas:=TCanvas.Create;
  try
    LCanvas.Handle:=hdc;
    with LCanvas do
    begin
      LColor:=Pen.Color;
      try
        Pen.Color:= AColor;
        X := LRect.Right - 8;
        Y := LRect.Top + (LRect.Height div 2) + 1;
        for i := 3 downto 0 do
        begin
          MoveTo(X - I, Y - I);
          LineTo(X + I + 1, Y - I);
        end;
      finally
        Pen.Color:=LColor;
      end;
    end;
  finally
    LCanvas.Handle:=0;
    LCanvas.Free;
    RestoreDC(hdc, SaveIndex);
  end;
end;


procedure DrawStyleParentBackground(Handle : THandle; DC: HDC; const ARect: TRect);
var
  LBuffer: TBitmap;
  LPoint: TPoint;
  LParentHandle : THandle;
begin
  if (Handle=0) or (ARect.Width<=0) or (ARect.Height<=0) then exit;

  LPoint := Point(ARect.Left, ARect.Top);
  LBuffer := TBitmap.Create;
  try
    LParentHandle:=GetParent(Handle);
    if LParentHandle<>0 then
    begin
      LBuffer.SetSize(ARect.Width, ARect.Height);
      SendMessage(LParentHandle , WM_ERASEBKGND, LBuffer.Canvas.Handle, 0);

      //ClientToScreen(Handle, LPoint);
      //ScreenToClient(LParentHandle, LPoint);
      //BitBlt(DC, ARect.Left, ARect.Top, ARect.Width, ARect.Height, LBuffer.Canvas.Handle, LPoint.X, LPoint.Y, SRCCOPY)
    end;
  finally
    LBuffer.Free;
  end;
end;

procedure DrawStyleParentBackgroundEx(Handle : THandle; DC: HDC; const ARect: TRect);
var
  LBuffer: TBitmap;
  LPoint: TPoint;
  LParentHandle : THandle;
begin
  if (Handle=0) or (ARect.Width<=0) or (ARect.Height<=0) then exit;
  LPoint := Point(ARect.Left, ARect.Top);
  LBuffer := TBitmap.Create;
  try
    LParentHandle:=GetParent(Handle);
    if LParentHandle<>0 then
    begin
      LBuffer.SetSize(ARect.Width, ARect.Height);
      SendMessage(LParentHandle , WM_ERASEBKGND, LBuffer.Canvas.Handle, 0);
      ClientToScreen(Handle, LPoint);
      ScreenToClient(LParentHandle, LPoint);
      BitBlt(DC, ARect.Left, ARect.Top, ARect.Width, ARect.Height, LBuffer.Canvas.Handle, LPoint.X, LPoint.Y, SRCCOPY)
    end;
  finally
    LBuffer.Free;
  end;
end;


procedure DrawStyleElement(hdc : HDC; LDetails  : TThemedElementDetails; pRect : TRect; RestoreDC : Boolean = True);
var
  SaveIndex : Integer;
begin
  SaveIndex :=0;
  if RestoreDC then
   SaveIndex := SaveDC(hdc);
  try
     StyleServices.DrawElement(hdc, LDetails, pRect, nil);
  finally
    if (SaveIndex>0) and  RestoreDC then
      Winapi.Windows.RestoreDC(hdc, SaveIndex);
  end;
end;

procedure GradientRoundedFillCanvas(const ACanvas: TCanvas;
  const AStartColor, AEndColor: TColor; const ARect: TRect;
  const Direction: TGradientDirection; Radius : Integer);
var
  LBuffer : TBitmap;
  LRect : TRect;
  LRgn : THandle;
  LPoint : TPoint;
begin
  LBuffer:=TBitmap.Create;
  try
    LBuffer.Width:=1;
    LBuffer.Height:=ARect.Height;
    LRect.Create(0, 0, 1, ARect.Height);
    GradientFillCanvas(LBuffer.Canvas, AStartColor, AEndColor, LRect, Direction);

    LRgn := CreateRoundRectRgn(ARect.Left, ARect.Top, ARect.Left +  ARect.Width,  ARect.Top + ARect.Height, Radius, Radius);
    if LRgn>0 then
    try
      GetWindowOrgEx(ACanvas.Handle, LPoint);
      OffsetRgn(LRgn, -LPoint.X, -LPoint.Y);
      SelectClipRgn(ACanvas.Handle, LRgn);
      ACanvas.StretchDraw(Rect(ARect.Left,  ARect.Top, ARect.Left + ARect.Width,  ARect.Top + ARect.Height), LBuffer);
      SelectClipRgn(ACanvas.Handle, 0);
    finally
      DeleteObject(LRgn);
    end;
  finally
   LBuffer.Free;
  end;
end;


procedure AlphaBlendRectangle(const ACanvas: TCanvas;  const AColor : TColor;const ARect: TRect; SourceConstantAlpha : Byte); overload;
begin
  AlphaBlendRectangle(ACanvas.Handle, AColor, ARect, SourceConstantAlpha);
end;

procedure AlphaBlendRectangle(const DC: HDC;  const AColor : TColor;const ARect: TRect; SourceConstantAlpha : Byte); overload;
var
   SaveIndex  : Integer;
   LCanvas    : TCanvas;
   LRect      : TRect;
begin
  SaveIndex := SaveDC(DC);
  LCanvas:=TCanvas.Create;
  try
    LCanvas.Handle:=DC;
    AlphaBlendFillCanvas(LCanvas, AColor, ARect, SourceConstantAlpha);
    LCanvas.Pen.Color:=AColor;
    LCanvas.Brush.Style:=bsClear;
    LRect:=ARect;
    LCanvas.Rectangle(LRect.Left, LRect.Top, LRect.Left + LRect.Width,  LRect.Top + LRect.Height);
  finally
    LCanvas.Handle:=0;
    LCanvas.Free;
    RestoreDC(DC, SaveIndex);
  end;
end;

procedure AlphaBlendFillCanvas(const ACanvas: TCanvas;  const AColor : TColor;const ARect: TRect; SourceConstantAlpha : Byte);
begin
  AlphaBlendFillCanvas(ACanvas.Handle, AColor, ARect, SourceConstantAlpha);
end;

procedure AlphaBlendFillCanvas(const DC: HDC;  const AColor : TColor;const ARect: TRect; SourceConstantAlpha : Byte); overload;
var
 LBuffer   : TBitmap;
 LBlendFunc: TBlendFunction;
begin
  LBuffer := TBitmap.Create;
  try
    LBuffer.Width := ARect.Width;
    LBuffer.Height := ARect.Height;
    LBuffer.Canvas.Brush.Color := AColor;
    LBuffer.Canvas.FillRect(Rect(0, 0, ARect.Width, ARect.Height));
    ZeroMemory(@LBlendFunc, SizeOf(LBlendFunc));
    LBlendFunc.BlendOp := AC_SRC_OVER;
    LBlendFunc.BlendFlags := 0;
    LBlendFunc.SourceConstantAlpha := SourceConstantAlpha;
    LBlendFunc.AlphaFormat := 0;
    AlphaBlend(DC, ARect.Left, ARect.Top, LBuffer.Width, LBuffer.Height, LBuffer.Canvas.Handle, 0, 0, LBuffer.Width, LBuffer.Height, LBlendFunc);
  finally
    LBuffer.Free;
  end;
end;

function RoundIntToByte(i: integer): byte;
begin
  if i > 255 then Result := 255
  else
  if i < 0   then Result := 0
  else
    Result := i;
end;


procedure _ProcessBitmap32(const Dest: TBitmap; Value: Integer; _Process:TImageFilterCallback); overload;
var
  r, g, b, a   : byte;
  x, y:    integer;
  ARGB:    TColor;
  Line, Delta: integer;
begin
  Line  := integer(Dest.ScanLine[0]);
  Delta := integer(Dest.ScanLine[1]) - Line;
  for y := 0 to Dest.Height - 1 do
  begin
    for x := 0 to Dest.Width - 1 do
    begin
      {$IFOPT R+}
        {$DEFINE RANGEON}
        {$R-}
      {$ELSE}
        {$UNDEF RANGEON}
      {$ENDIF}
      r    := PRGBArray32(Line)[x].rgbRed;
      g    := PRGBArray32(Line)[x].rgbGreen;
      b    := PRGBArray32(Line)[x].rgbBlue;
      a    := PRGBArray32(Line)[x].rgbReserved;
      {$IFDEF RANGEON}
        {$R+}
        {$UNDEF RANGEON}
      {$ENDIF}
      _Process(RGB(r,g,b), Value, ARGB);
      GetRGB(ARGB, r, g, b);
      {$IFOPT R+}
        {$DEFINE RANGEON}
        {$R-}
      {$ELSE}
        {$UNDEF RANGEON}
      {$ENDIF}
      PRGBArray32(Line)[x].rgbRed := r;
      PRGBArray32(Line)[x].rgbGreen := g;
      PRGBArray32(Line)[x].rgbBlue := b;
      PRGBArray32(Line)[x].rgbReserved := a;
      {$IFDEF RANGEON}
        {$R+}
        {$UNDEF RANGEON}
      {$ENDIF}
    end;
    Inc(Line, Delta);
  end;
end;

procedure _ProcessBitmap32(const Source, Dest: TBitmap;_Process:TImageFilterCallback); overload;
var
  r, g, b, a   : byte;
  x, y:    integer;
  ARGB:    TColor;
  LineDest, DeltaDest: integer;
  LineSource, DeltaSource: integer;
  Value : TColor;
  SourceN : TBitmap;
begin
  SourceN:=TBitmap.Create;
  try
    SourceN.SetSize(Dest.Width, Dest.Height);
    SourceN.PixelFormat:=pf32bit;

    y := 0;
    while y < Dest.Height do
    begin
      x := 0;
      while x < Dest.Width do
      begin
        SourceN.Canvas.Draw(x, y, Source);
        x := x + Source.Width;
      end;
      y := y + Source.Height;
    end;

    LineDest  := integer(Dest.ScanLine[0]);
    DeltaDest := integer(Dest.ScanLine[1]) - LineDest;

    LineSource  := integer(SourceN.ScanLine[0]);
    DeltaSource := integer(SourceN.ScanLine[1]) - LineSource;

    for y := 0 to Dest.Height - 1 do
    begin
      for x := 0 to Dest.Width - 1 do
      begin
      {$IFOPT R+}
        {$DEFINE RANGEON}
        {$R-}
      {$ELSE}
        {$UNDEF RANGEON}
      {$ENDIF}        r    := PRGBArray32(LineDest)[x].rgbRed;
        g    := PRGBArray32(LineDest)[x].rgbGreen;
        b    := PRGBArray32(LineDest)[x].rgbBlue;
        a    := PRGBArray32(LineDest)[x].rgbReserved;
      {$IFDEF RANGEON}
        {$R+}
        {$UNDEF RANGEON}
      {$ENDIF}

        Value:=RGB(PRGBArray24(LineSource)[x].rgbtRed, PRGBArray24(LineSource)[x].rgbtGreen, PRGBArray24(LineSource)[x].rgbtBlue);


        _Process(RGB(r,g,b), Value, ARGB);
        GetRGB(ARGB, r, g, b);

      {$IFOPT R+}
        {$DEFINE RANGEON}
        {$R-}
      {$ELSE}
        {$UNDEF RANGEON}
      {$ENDIF}
        PRGBArray32(LineDest)[x].rgbRed := r;
        PRGBArray32(LineDest)[x].rgbGreen := g;
        PRGBArray32(LineDest)[x].rgbBlue := b;
        PRGBArray32(LineDest)[x].rgbReserved := a;
      {$IFDEF RANGEON}
        {$R+}
        {$UNDEF RANGEON}
      {$ENDIF}
      end;
      Inc(LineDest, DeltaDest);
      Inc(LineSource, DeltaSource);
    end;
  finally
    SourceN.Free;
  end;
end;



procedure _ProcessBitmap24(const ABitMap: TBitmap; Value: Integer; _Process:TImageFilterCallback); overload;
var
  r, g, b    : byte;
  x, y:    integer;
  ARGB:    TColor;
  Line, Delta: integer;
begin
  Line  := integer(ABitMap.ScanLine[0]);
  Delta := integer(ABitMap.ScanLine[1]) - Line;
  for y := 0 to ABitMap.Height - 1 do
  begin
    for x := 0 to ABitMap.Width - 1 do
    begin
      {$IFOPT R+}
        {$DEFINE RANGEON}
        {$R-}
      {$ELSE}
        {$UNDEF RANGEON}
      {$ENDIF}
      r    := PRGBArray24(Line)[x].rgbtRed;
      g    := PRGBArray24(Line)[x].rgbtGreen;
      b    := PRGBArray24(Line)[x].rgbtBlue;
      {$IFDEF RANGEON}
        {$R+}
        {$UNDEF RANGEON}
      {$ENDIF}

      _Process(RGB(r,g,b), Value, ARGB);
      GetRGB(ARGB, r, g, b);

      {$IFOPT R+}
        {$DEFINE RANGEON}
        {$R-}
      {$ELSE}
        {$UNDEF RANGEON}
      {$ENDIF}
      PRGBArray24(Line)[x].rgbtRed := r;
      PRGBArray24(Line)[x].rgbtGreen := g;
      PRGBArray24(Line)[x].rgbtBlue := b;
      {$IFDEF RANGEON}
        {$R+}
        {$UNDEF RANGEON}
      {$ENDIF}
    end;
    Inc(Line, Delta);
  end;
end;

procedure _ProcessBitmap24(const Source, Dest: TBitmap;_Process:TImageFilterCallback); overload;
var
  r, g, b   : byte;
  x, y:    integer;
  ARGB:    TColor;
  LineDest, DeltaDest: integer;
  LineSource, DeltaSource: integer;
  Value : TColor;
  SourceN : TBitmap;
begin
  SourceN:=TBitmap.Create;
  try
    SourceN.SetSize(Dest.Width, Dest.Height);
    SourceN.PixelFormat:=pf24bit;

    y := 0;
    while y < Dest.Height do
    begin
      x := 0;
      while x < Dest.Width do
      begin
        SourceN.Canvas.Draw(x, y, Source);
        x := x + Source.Width;
      end;
      y := y + Source.Height;
    end;

    LineDest  := integer(Dest.ScanLine[0]);
    DeltaDest := integer(Dest.ScanLine[1]) - LineDest;

    LineSource  := integer(SourceN.ScanLine[0]);
    DeltaSource := integer(SourceN.ScanLine[1]) - LineSource;

    for y := 0 to Dest.Height - 1 do
    begin
      for x := 0 to Dest.Width - 1 do
      begin
        r    := PRGBArray24(LineDest)[x].rgbtRed;
        g    := PRGBArray24(LineDest)[x].rgbtGreen;
        b    := PRGBArray24(LineDest)[x].rgbtBlue;

        Value:=RGB(PRGBArray24(LineSource)[x].rgbtRed, PRGBArray24(LineSource)[x].rgbtGreen, PRGBArray24(LineSource)[x].rgbtBlue);

        _Process(RGB(r,g,b), Value, ARGB);
        GetRGB(ARGB, r, g, b);

        PRGBArray32(LineDest)[x].rgbRed := r;
        PRGBArray32(LineDest)[x].rgbGreen := g;
        PRGBArray32(LineDest)[x].rgbBlue := b;
      end;
      Inc(LineDest, DeltaDest);
      Inc(LineSource, DeltaSource);
    end;
  finally
    SourceN.Free;
  end;
end;



procedure _Sepia(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  ARGB         : TColor;
  r, g, b      : byte;
begin
  GetRGB(AColor, r,g, b);
  ARGB:=(r+g+b) div 3;

  r:=ARGB+(Value*2);
  g:=ARGB+(Value*1);
  b:=ARGB+(Value*1);

  if r <= ((Value*2)-1) then
    r:=255;
  if g <= (Value-1) then
    g:=255;

  NewColor:= RGB(r, g, b);
end;

procedure _Sepia24(const ABitMap: TBitmap;Value : Byte);
begin
  _ProcessBitmap24(ABitMap, Value, _Sepia);
end;

procedure _Sepia32(const ABitMap: TBitmap;Value : Byte);
begin
  _ProcessBitmap32(ABitMap, Value, _Sepia);
end;

procedure _Hue(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  ARGB         : TColor;
  H, S, L      : double;
begin
  _RGBtoHSL(AColor, H, S, L);
  H    := H + Value / 360;
  ARGB := _HSLtoRGB(H, S, L);
  NewColor:= ARGB;
end;

procedure _Hue24(var ABitMap: TBitmap; Value: integer);
begin
  _ProcessBitmap24(ABitMap, Value, _Hue);
end;

procedure _Hue32(const ABitMap: TBitmap; Value: integer);
begin
  _ProcessBitmap32(ABitMap, Value, _Hue);
end;

{
if b = 0 then
  result := 0
else begin
  c := 255 - (((255-a) SHL 8) DIV b);
  if c < 0 then result := 0 else result := c;
end;
}

procedure _BlendBurn(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  ARGB         : TColor;
  r, g, b      : byte;
  br, bg, bb   : byte;
  c            : Integer;
begin
  GetRGB(AColor, r,g, b);
  ARGB := Value;
  GetRGB(ARGB, br,bg, bb);

  if br=0 then
   r:=0
  else
  begin
   c:=RoundIntToByte(255-(((255-r) SHL 8) DIV br));
   r:=c;
  end;

  if bg=0 then
   g:=0
  else
  begin
   c:=RoundIntToByte(255-(((255-g) SHL 8) DIV bg));
   g:=c;
  end;

  if bb=0 then
   b:=0
  else
  begin
   c:=RoundIntToByte(255-(((255-b) SHL 8) DIV bb));
   b:=c;
  end;

  NewColor:=RGB(r, g, b);
end;

procedure _BlendBurn24(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap24(ABitMap, Value, _BlendBurn);
end;


procedure _BlendBurn32(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap32(ABitMap, Value, _BlendBurn);
end;


{result := (a*b) SHR 8;}

procedure _BlendMultiply(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
begin
  ARGB := Value;
  GetRGB(AColor, r, g, b);

  GetRGB(ARGB, br, bg, bb);
  r:=(r*br) shr 8;
  g:=(g*bg) shr 8;
  b:=(b*bb) shr 8;

  NewColor:= RGB(r,g,b);
end;


procedure _BlendMultiply24(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap24(ABitMap, Value, _BlendMultiply);
end;

procedure _BlendMultiply32(const ABitMap: TBitmap;Value: Integer);
begin
  _ProcessBitmap32(ABitMap, Value, _BlendMultiply);
end;


{
c := a+b;
if c > 255 then result := 255 else result := c;
}
procedure _BlendAdditive(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
  c            : Integer;
begin
  ARGB := Value;
  GetRGB(AColor, r,g, b);
  GetRGB(ARGB, br,bg, bb);

  c:=RoundIntToByte(r+br);
  r:=c;
  c:=RoundIntToByte(g+bg);
  g:=c;
  c:=RoundIntToByte(b+bb);
  b:=c;

  NewColor:= RGB(r,g,b);
end;

procedure _BlendAdditive24(const ABitMap: TBitmap;Value: Integer);
begin
  _ProcessBitmap24(ABitMap, Value, _BlendAdditive);
end;


procedure _BlendAdditive32(const ABitMap: TBitmap;Value: Integer);
begin
  _ProcessBitmap32(ABitMap, Value, _BlendAdditive);
end;

{
if b = 255 then
  result := 255
else begin
  c := (a SHL 8) DIV (255-b);
  if c > 255 then result := 255 else result := c;
end;
}
procedure _BlendDodge(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
  c            : Integer;
begin
  GetRGB(AColor, r,g, b);

  ARGB := Value;
  GetRGB(ARGB, br,bg, bb);

  if br=255 then
   r:=255
  else
  begin
    c := RoundIntToByte((r SHL 8) DIV (255-br));
    r := c;
  end;

  if bg=255 then
   g:=255
  else
  begin
    c := RoundIntToByte((g SHL 8) DIV (255-bg));
    g := c;
  end;

  if bb=255 then
   b:=255
  else
  begin
    c := RoundIntToByte((b SHL 8) DIV (255-bb));
    b := c;
  end;

  NewColor:= RGB(r,g,b);
end;

procedure _BlendDodge24(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap24(ABitMap, Value, _BlendDodge);
end;


procedure _BlendDodge32(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap32(ABitMap, Value, _BlendDodge);
end;

{
if a < 128 then
  result := (a*b) SHR 7
else
  result := 255 - ((255-a) * (255-b) SHR 7);
}
procedure _BlendOverlay(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
  c            : Integer;
begin
  GetRGB(AColor, r,g, b);
  ARGB := Value;
  GetRGB(ARGB, br,bg, bb);

  if r<128 then
   r:=RoundIntToByte((r*br) shr 7)
  else
  begin
    c := RoundIntToByte(255 - ((255-r) * (255-br) SHR 7));
    r := c;
  end;

  if g<128 then
   g:=RoundIntToByte((g*bg) shr 7)
  else
  begin
    c := RoundIntToByte(255 - ((255-g) * (255-bg) SHR 7));
    g := c;
  end;

  if b<128 then
   b:=RoundIntToByte((r*bb) shr 7)
  else
  begin
    c := RoundIntToByte(255 - ((255-b) * (255-bb) SHR 7));
    b := c;
  end;

  NewColor:= RGB(r,g,b);
end;

procedure _BlendOverlay24(const ABitMap: TBitmap;Value: Integer);
begin
  _ProcessBitmap24(ABitMap, Value, _BlendOverlay);
end;


procedure _BlendOverlay32(const ABitMap: TBitmap;Value: Integer);
begin
  _ProcessBitmap32(ABitMap, Value, _BlendOverlay);
end;

{
result := abs(a-b);
}

procedure _BlendDifference(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
begin
  GetRGB(AColor, r,g, b);
  ARGB := Value;
  GetRGB(ARGB, br,bg, bb);
  r:=abs(r-br);
  g:=abs(g-bg);
  b:=abs(b-bb);
  NewColor:= RGB(r,g,b);
end;


procedure _BlendDifference24(const ABitMap: TBitmap;Value: Integer);
begin
  _ProcessBitmap24(ABitMap, Value, _BlendDifference);
end;

procedure _BlendDifference32(const ABitMap: TBitmap;Value: Integer);
begin
  _ProcessBitmap32(ABitMap, Value, _BlendDifference);
end;

{
if a > b then
  result := a
else
  result := b;
}
procedure _BlendLighten(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
begin
  GetRGB(AColor, r,g, b);

  ARGB := Value;
  GetRGB(ARGB, br,bg, bb);

  r:=IfThen(r>br, r, br);
  g:=IfThen(g>bg, g, bg);
  b:=IfThen(b>bb, b, bb);

  NewColor:= RGB(r,g,b);
end;

procedure _BlendLighten24(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap24(ABitMap, Value, _BlendLighten);
end;

procedure _BlendLighten32(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap32(ABitMap, Value, _BlendLighten);
end;

{
if a < b then
  result := a
else
  result := b;
}
procedure _BlendDarken(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
begin
  GetRGB(AColor, r,g, b);
  ARGB := Value;
  GetRGB(ARGB, br,bg, bb);
  r:=IfThen(r<br, r, br);
  g:=IfThen(g<bg, g, bg);
  b:=IfThen(b<bb, b, bb);
  NewColor:= RGB(r,g,b);
end;

procedure _BlendDarken24(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap24(ABitMap, Value, _BlendDarken);
end;

procedure _BlendDarken32(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap32(ABitMap, Value, _BlendDarken);
end;

{
result := 255 - ((255-a) * (255-b) SHR 8);
}
procedure _BlendScreen(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
  c            : Integer;
begin
  GetRGB(AColor, r,g, b);

  ARGB := Value;
  GetRGB(ARGB, br,bg, bb);

  c := RoundIntToByte(255 - ((255-r) * (255-br) SHR 8));
  r := c;

  c := RoundIntToByte(255 - ((255-g) * (255-bg) SHR 8));
  g := c;

  c := RoundIntToByte(255 - ((255-b) * (255-bb) SHR 8));
  b := c;

  NewColor:= RGB(r,g,b);
end;

procedure _BlendScreen24(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap24(ABitMap, Value, _BlendScreen);
end;


procedure _BlendScreen32(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap32(ABitMap, Value, _BlendScreen);
end;


procedure _SetRComponent(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
begin
  GetRGB(AColor, r,g, b);
  r:=RoundIntToByte(r+Value);
  NewColor:= RGB(r,g,b);
end;

procedure _SetGComponent(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
begin
  GetRGB(AColor, r,g, b);
  g:=RoundIntToByte(g+Value);
  NewColor:= RGB(r,g,b);
end;

procedure _SetBComponent(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
begin
  GetRGB(AColor, r,g, b);
  b:=RoundIntToByte(b+Value);
  NewColor:= RGB(r,g,b);
end;

procedure _SetRGB24(const ABitMap: TBitmap; DR,DG,DB: Integer);
var
  r, g, b : byte;
  x, y:    integer;
  Line, Delta: integer;
begin
  Line  := integer(ABitMap.ScanLine[0]);
  Delta := integer(ABitMap.ScanLine[1]) - Line;
  for y := 0 to ABitMap.Height - 1 do
  begin
    for x := 0 to ABitMap.Width - 1 do
    begin
      r    := PRGBArray24(Line)[x].rgbtRed;
      g    := PRGBArray24(Line)[x].rgbtGreen;
      b    := PRGBArray24(Line)[x].rgbtBlue;
      PRGBArray24(Line)[x].rgbtRed := RoundIntToByte(r+DR);
      PRGBArray24(Line)[x].rgbtGreen := RoundIntToByte(g+DG);
      PRGBArray24(Line)[x].rgbtBlue := RoundIntToByte(b+DB);
    end;
    Inc(Line, Delta);
  end;
end;


procedure _SetRGB32(const ABitMap: TBitmap; DR,DG,DB: Integer);
var
  r, g, b, a: byte;
  x, y:    integer;
  Line, Delta: integer;
begin
  Line  := integer(ABitMap.ScanLine[0]);
  Delta := integer(ABitMap.ScanLine[1]) - Line;
  for y := 0 to ABitMap.Height - 1 do
  begin
    for x := 0 to ABitMap.Width - 1 do
    begin
      r    := PRGBArray32(Line)[x].rgbRed;
      g    := PRGBArray32(Line)[x].rgbGreen;
      b    := PRGBArray32(Line)[x].rgbBlue;
      a    := PRGBArray32(Line)[x].rgbReserved;
      PRGBArray32(Line)[x].rgbRed := RoundIntToByte(r+DR);
      PRGBArray32(Line)[x].rgbGreen := RoundIntToByte(g+DG);
      PRGBArray32(Line)[x].rgbBlue := RoundIntToByte(b+DB);
      PRGBArray32(Line)[x].rgbReserved := a;
    end;
    Inc(Line, Delta);
  end;
end;


procedure _Saturation(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  Gray         : Integer;
begin
  GetRGB(AColor, r,g, b);
  Gray := (r + g + b) div 3;
  r := RoundIntToByte(Gray + (((r - Gray) * Value) div 255));
  g := RoundIntToByte(Gray + (((g - Gray) * Value) div 255));
  b := RoundIntToByte(Gray + (((b - Gray) * Value) div 255));
  NewColor:= RGB(r,g,b);
end;


procedure _Saturation24(var ABitMap: TBitmap; Value: integer);
begin
 _ProcessBitmap24(ABitMap, Value, _Saturation);
end;

procedure _Saturation32(const ABitMap: TBitmap; Value: integer);
begin
 _ProcessBitmap32(ABitMap, Value, _Saturation);
end;


procedure _Lightness(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
begin
  GetRGB(AColor, r,g, b);
  r := RoundIntToByte(r + ((255 - r) * Value) div 255);
  g := RoundIntToByte(g + ((255 - g) * Value) div 255);
  b := RoundIntToByte(b + ((255 - b) * Value) div 255);
  NewColor:= RGB(r,g,b);
end;

procedure _Lightness24(var ABitMap: TBitmap; Value: integer);
begin
 _ProcessBitmap24(ABitMap, Value, _Lightness);
end;

procedure _Lightness32(const ABitMap: TBitmap; Value: integer);
begin
 _ProcessBitmap32(ABitMap, Value, _Lightness);
end;

procedure _Darkness(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
begin
  GetRGB(AColor, r,g, b);
  r := RoundIntToByte(r - ((r) * Value) div 255);
  g := RoundIntToByte(g - ((g) * Value) div 255);
  b := RoundIntToByte(b - ((b) * Value) div 255);
  NewColor:= RGB(r,g,b);
end;


procedure _Darkness24(var ABitMap: TBitmap; Value: integer);
begin
  _ProcessBitmap24(ABitMap, Value, _Darkness);
end;

procedure _Darkness32(const ABitMap: TBitmap; Value: integer);
begin
  _ProcessBitmap32(ABitMap, Value, _Darkness);
end;


function _HSLtoRGB(HueValue, SaturationValue, LightValue: double): TColor;
var
  M1, M2: double;

  function HueToColourValue(Hue: double): byte;
  var
    V: double;
  begin
    if Hue < 0 then
      Hue := Hue + 1
    else
    if Hue > 1 then
      Hue := Hue - 1;

    if 6 * Hue < 1 then
      V := M1 + (M2 - M1) * Hue * 6
    else
    if 2 * Hue < 1 then
      V := M2
    else
    if 3 * Hue < 2 then
      V := M1 + (M2 - M1) * (2 / 3 - Hue) * 6
    else
      V := M1;
    Result := round(255 * V);
  end;

var
  R, G, B: byte;
begin
  if SaturationValue = 0 then
  begin
    R := round(255 * LightValue);
    G := R;
    B := R;
  end
  else
  begin
    if LightValue <= 0.5 then
      M2 := LightValue * (1 + SaturationValue)
    else
      M2 := LightValue + SaturationValue - LightValue * SaturationValue;
    M1 := 2 * LightValue - M2;
    R := HueToColourValue(HueValue + 1 / 3);
    G := HueToColourValue(HueValue);
    B := HueToColourValue(HueValue - 1 / 3);
  end;

  Result := RGB(R, G, B);
end;

procedure _RGBtoHSL(RGB: TColor; var HueValue, SaturationValue, LightValue: double);

  function Max(a, b: double): double;
  begin
    if a > b then
      Result := a
    else
      Result := b;
  end;

  function Min(a, b: double): double;
  begin
    if a < b then
      Result := a
    else
      Result := b;
  end;

var
  R, G, B, D, Cmax, Cmin: double;
begin
  R    := GetRValue(RGB) / 255;
  G    := GetGValue(RGB) / 255;
  B    := GetBValue(RGB) / 255;
  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Min(G, B));

  LightValue := (Cmax + Cmin) / 2;

  if Cmax = Cmin then
  begin
    HueValue := 0;
    SaturationValue := 0;
  end
  else
  begin
    D := Cmax - Cmin;

    if LightValue < 0.5 then
      SaturationValue := D / (Cmax + Cmin)
    else
      SaturationValue := D / (2 - Cmax - Cmin);

    if R = Cmax then
      HueValue := (G - B) / D
    else
    if G = Cmax then
      HueValue := 2 + (B - R) / D
    else
      HueValue := 4 + (R - G) / D;

    HueValue := HueValue / 6;
    if HueValue < 0 then
      HueValue := HueValue + 1;
  end;
end;

{ TBitmap32Filter }


{ TBitmap32HueFilter }
procedure TBitmap32HueFilter.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, ColorValue, _Hue)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, ColorValue, _Hue);
end;

function TBitmap32HueFilter.ProcessColor(AColor: TColor): TColor;
begin
  _Hue(AColor, ColorValue, Result);
end;

{ TBitmap32SaturationFilter }

procedure TBitmap32SaturationFilter.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
    _ProcessBitmap32(ABitMap, ColorValue, _Saturation)
  else
  if ABitMap.PixelFormat=pf24bit then
    _ProcessBitmap24(ABitMap, ColorValue, _Saturation);
end;

function TBitmap32SaturationFilter.ProcessColor(AColor: TColor): TColor;
begin
  _Saturation(AColor, ColorValue, Result);
end;

{ TBitmap32LightnessFilter }

procedure TBitmap32LightnessFilter.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
  begin
    if ColorValue >= 0 then
      _ProcessBitmap32(ABitMap, ColorValue, _Lightness)
    else
      _ProcessBitmap32(ABitMap, Abs(ColorValue), _Darkness);
  end
  else
  if ABitMap.PixelFormat=pf24bit then
  begin
    if ColorValue >= 0 then
      _ProcessBitmap24(ABitMap, ColorValue, _Lightness)
    else
      _ProcessBitmap24(ABitMap, Abs(ColorValue), _Darkness);
  end;
end;

function TBitmap32LightnessFilter.ProcessColor(AColor: TColor): TColor;
begin
    if ColorValue >= 0 then
      _Lightness(AColor, ColorValue, Result)
    else
      _Darkness(AColor, Abs(ColorValue), Result);
end;

{ TBitmap32SepiaFilter }

procedure TBitmap32SepiaFilter.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, ColorValue, _Sepia)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, ColorValue, _Sepia);
end;

function TBitmap32SepiaFilter.ProcessColor(AColor: TColor): TColor;
begin
   _Sepia(AColor, ColorValue, Result);
end;

{ TColorFilter }

constructor TColorFilter.Create(AColorValue: Integer);
begin
 inherited Create;
 FColorValue:=AColorValue;
end;


{ TBitmap32BlueFilter }

procedure TBitmap32BlueFilter.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
    _SetRGB32(ABitMap,0,0,ColorValue)
  else
  if ABitMap.PixelFormat=pf24bit then
   _SetRGB24(ABitMap,0,0,ColorValue);
end;

function TBitmap32BlueFilter.ProcessColor(AColor: TColor): TColor;
begin
  _SetBComponent(AColor, ColorValue, Result);
end;

{ TBitmap32RedFilter }

procedure TBitmap32RedFilter.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
    _SetRGB32(ABitMap,ColorValue,0,0)
  else
  if ABitMap.PixelFormat=pf24bit then
    _SetRGB24(ABitMap,ColorValue,0,0);
end;

function TBitmap32RedFilter.ProcessColor(AColor: TColor): TColor;
begin
  _SetRComponent(AColor, ColorValue, Result);
end;

{ TBitmap32GreenFilter }

procedure TBitmap32GreenFilter.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
    _SetRGB32(ABitMap,0,ColorValue,0)
  else
  if ABitMap.PixelFormat=pf24bit then
    _SetRGB24(ABitMap,0,ColorValue,0);
end;

function TBitmap32GreenFilter.ProcessColor(AColor: TColor): TColor;
begin
  _SetGComponent(AColor, ColorValue, Result);
end;

{ TBitmap32BlendBurn }

procedure TBitmap32BlendBurn.ProcessBitmap(ABitMap: TBitmap);
begin
  if FUseBitmap then
  begin
    if ABitMap.PixelFormat=pf32bit then
     _ProcessBitmap32(FSourceBitmap , ABitMap , _BlendBurn)
    else
    if ABitMap.PixelFormat=pf24bit then
     _ProcessBitmap24(FSourceBitmap , ABitMap , _BlendBurn)
  end
  else
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, ColorValue, _BlendBurn)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, ColorValue, _BlendBurn);
end;

function TBitmap32BlendBurn.ProcessColor(AColor: TColor): TColor;
begin
  _BlendBurn(AColor, ColorValue, Result);
end;

{ TBitmap32BlendMultiply }

procedure TBitmap32BlendMultiply.ProcessBitmap(ABitMap: TBitmap);
begin
  if FUseBitmap then
  begin
    if ABitMap.PixelFormat=pf32bit then
     _ProcessBitmap32(FSourceBitmap , ABitMap , _BlendMultiply)
    else
    if ABitMap.PixelFormat=pf24bit then
     _ProcessBitmap24(FSourceBitmap , ABitMap , _BlendMultiply)
  end
  else
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, ColorValue, _BlendMultiply)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, ColorValue, _BlendMultiply);
end;

function TBitmap32BlendMultiply.ProcessColor(AColor: TColor): TColor;
begin
  _BlendMultiply(AColor, ColorValue, Result);
end;

{ TBitmap32BlendAdditive }

procedure TBitmap32BlendAdditive.ProcessBitmap(ABitMap: TBitmap);
begin
  if FUseBitmap then
  begin
    if ABitMap.PixelFormat=pf32bit then
     _ProcessBitmap32(FSourceBitmap , ABitMap , _BlendAdditive)
    else
    if ABitMap.PixelFormat=pf24bit then
     _ProcessBitmap24(FSourceBitmap , ABitMap , _BlendAdditive)
  end
  else
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, ColorValue, _BlendAdditive)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, ColorValue, _BlendAdditive);
end;

function TBitmap32BlendAdditive.ProcessColor(AColor: TColor): TColor;
begin
  _BlendAdditive(AColor, ColorValue, Result);
end;

{ TBitmap32BlendDodge }

procedure TBitmap32BlendDodge.ProcessBitmap(ABitMap: TBitmap);
begin
  if FUseBitmap then
  begin
    if ABitMap.PixelFormat=pf32bit then
     _ProcessBitmap32(FSourceBitmap , ABitMap , _BlendDodge)
    else
    if ABitMap.PixelFormat=pf24bit then
     _ProcessBitmap24(FSourceBitmap , ABitMap , _BlendDodge)
  end
  else
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, ColorValue, _BlendDodge)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, ColorValue, _BlendDodge);
end;

function TBitmap32BlendDodge.ProcessColor(AColor: TColor): TColor;
begin
  _BlendDodge(AColor, ColorValue, Result);
end;

{ TBitmap32BlendOverlay }

procedure TBitmap32BlendOverlay.ProcessBitmap(ABitMap: TBitmap);
begin
  if FUseBitmap then
  begin
    if ABitMap.PixelFormat=pf32bit then
     _ProcessBitmap32(FSourceBitmap , ABitMap , _BlendOverlay)
    else
    if ABitMap.PixelFormat=pf24bit then
     _ProcessBitmap24(FSourceBitmap , ABitMap , _BlendOverlay)
  end
  else
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, ColorValue, _BlendOverlay)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, ColorValue, _BlendOverlay);
end;

function TBitmap32BlendOverlay.ProcessColor(AColor: TColor): TColor;
begin
  _BlendOverlay(AColor, ColorValue, Result);
end;

{ TBitmap32BlendLighten }

procedure TBitmap32BlendLighten.ProcessBitmap(ABitMap: TBitmap);
begin
  if FUseBitmap then
  begin
    if ABitMap.PixelFormat=pf32bit then
     _ProcessBitmap32(FSourceBitmap , ABitMap , _BlendLighten)
    else
    if ABitMap.PixelFormat=pf24bit then
     _ProcessBitmap24(FSourceBitmap , ABitMap , _BlendLighten)

  end
  else
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, ColorValue, _BlendLighten)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, ColorValue, _BlendLighten);
end;

function TBitmap32BlendLighten.ProcessColor(AColor: TColor): TColor;
begin
  _BlendLighten(AColor, ColorValue, Result);
end;

{ TBitmap32BlendDarken }

procedure TBitmap32BlendDarken.ProcessBitmap(ABitMap: TBitmap);
begin
  if FUseBitmap then
  begin
    if ABitMap.PixelFormat=pf32bit then
     _ProcessBitmap32(FSourceBitmap , ABitMap , _BlendDarken)
    else
    if ABitMap.PixelFormat=pf24bit then
     _ProcessBitmap24(FSourceBitmap , ABitMap , _BlendDarken)
  end
  else
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, ColorValue, _BlendDarken)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, ColorValue, _BlendDarken);
end;

function TBitmap32BlendDarken.ProcessColor(AColor: TColor): TColor;
begin
  _BlendDarken(AColor, ColorValue, Result);
end;

{ TBitmap32BlendScreen }

procedure TBitmap32BlendScreen.ProcessBitmap(ABitMap: TBitmap);
begin
  if FUseBitmap then
  begin
    if ABitMap.PixelFormat=pf32bit then
     _ProcessBitmap32(FSourceBitmap , ABitMap , _BlendScreen)
    else
    if ABitMap.PixelFormat=pf24bit then
     _ProcessBitmap24(FSourceBitmap , ABitMap , _BlendScreen)
  end
  else
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, ColorValue, _BlendScreen)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, ColorValue, _BlendScreen);
end;

function TBitmap32BlendScreen.ProcessColor(AColor: TColor): TColor;
begin
  _BlendScreen(AColor, ColorValue, Result);
end;

{ TBitmap32BlendDifference }

procedure TBitmap32BlendDifference.ProcessBitmap(ABitMap: TBitmap);
begin
  if FUseBitmap then
  begin
    if ABitMap.PixelFormat=pf32bit then
     _ProcessBitmap32(FSourceBitmap , ABitMap , _BlendDifference)
    else
    if ABitMap.PixelFormat=pf24bit then
     _ProcessBitmap24(FSourceBitmap , ABitMap , _BlendDifference)
  end
  else
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, ColorValue, _BlendDifference)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, ColorValue, _BlendDifference);
end;

function TBitmap32BlendDifference.ProcessColor(AColor: TColor): TColor;
begin
  _BlendDifference(AColor, ColorValue, Result);
end;

{ TBitmapFilter }

constructor TBitmapFilter.CreateBitMap(ASourceBitmap: TBitmap);
begin
  inherited Create (clNone);
  FSourceBitmap:=ASourceBitmap;
  FUseBitmap:=True;
end;

constructor TBitmapFilter.Create(AColorValue: Integer);
begin
  inherited Create(AColorValue);
  FUseBitmap:=False;
  FSourceBitmap:=nil;
end;

{ TFontLoader }

constructor TAwesomeFont.Create;
begin
  inherited;
  FFontHandle:=0;
  FDefaultQuality := ANTIALIASED_QUALITY;
  LoadFontFromResource;
end;

destructor TAwesomeFont.Destroy;
begin
  if FFontHandle<>0 then
    RemoveFontMemResourceEx(FFontHandle);

  inherited;
end;



procedure TAwesomeFont.LoadFontFromResource;
var
  NbFontAdded: Cardinal;
{$IFDEF USE_ZIP}
  LZipFile     : TZipFile;
  LResCompressed, LStream : TStream;
  LocalHeader  : TZipHeader;
  LMemoryStream : TMemoryStream;
{$ELSE}
  hResInfo : HRSRC;
  ResSize  : Cardinal;
  ResAddr  : HGLOBAL;
{$ENDIF}
begin
{$IFDEF USE_ZIP}
  LResCompressed:=TResourceStream.Create(HInstance, 'fontawesome_zip', RT_RCDATA);
  try
    LZipFile:=TZipFile.Create;
    try
      LResCompressed.Position:=0;
      LZipFile.Open(LResCompressed, TZipMode.zmRead);
      LZipFile.Read(0, LStream, LocalHeader);
      try
        LMemoryStream:=TMemoryStream.Create;
        try
           LMemoryStream.CopyFrom(LStream, LStream.Size);
           FFontHandle := AddFontMemResourceEx(LMemoryStream.Memory, LMemoryStream.Size, nil, @NbFontAdded);
        finally
          LMemoryStream.Free;
        end;
      finally
         LStream.Free;
      end;
    finally
      LZipFile.Free;
    end;
  finally
     LResCompressed.Free;
  end;
{$ELSE}
  hResInfo := FindResource(HInstance, 'fontawesome', RT_RCDATA);
  if hResInfo = 0 then
    RaiseLastOSError;

  ResAddr := LoadResource(HInstance, hResInfo);
  if ResAddr = 0 then
    RaiseLastOSError;

  ResSize := SizeOfResource(HInstance, hResInfo);
  if ResSize = 0 then
    RaiseLastOSError;

  FFontHandle := AddFontMemResourceEx(Pointer(ResAddr), ResSize, nil, @NbFontAdded);

  if FFontHandle = 0 then
    RaiseLastOSError;
{$ENDIF}
end;

procedure TAwesomeFont.DrawChar(DC: HDC; const ACode: Word; DestRect: TRect; AColor: TColor; Orientation : Integer = 0; ImageAlignment: TImageAlignment = iaLeft);
begin
  DrawChar(DC, Chr(ACode), DestRect, AColor, Orientation, ImageAlignment);
end;

function TAwesomeFont.GetIcon(const ACode: Word; Width, Height, CharX,
  CharY: Integer; AColor, ABackColor: TColor; Orientation: Integer;
  ImageAlignment: TImageAlignment): HICON;
var
  LIconInfo : TIconInfo;
  LBitmap, LMask : TBitmap;
  NewIcon : HICON;
begin
  LBitmap:=TBitmap.Create;
  try
    LBitmap.PixelFormat:=pf32bit;
    LBitmap.Canvas.Brush.Color:=ABackColor;
    LBitmap.SetSize(Width, Width);
    //LBitmap.Canvas.FillRect(Rect(0, 0, LBitmap.Width, LBitmap.Height));
    //Bitmap32_SetAlphaAndColor(LBitmap, 255, clFuchsia);

    //DrawChar(LBitmap.Canvas.Handle, ACode, Rect(0, 0, LBitmap.Width, LBitmap.Height), AColor, Orientation, ImageAlignment);
    DrawChar(LBitmap.Canvas.Handle, ACode, Rect(0, 0, CharX, CharY), AColor, Orientation, ImageAlignment);
    Bitmap32_SetAlphaExceptColor(LBitmap, 255, ABackColor);
    LBitmap.AlphaFormat := afDefined;

    LMask:=TBitmap.Create;
    try
      //LMask.Handle:=CreateBitmap(LBitmap.Width, LBitmap.Height, 1, 1, 0);
      LMask.PixelFormat:=pf1bit;
      LMask.SetSize(Width, Height);

      LIconInfo.fIcon:=True;
      LIconInfo.xHotspot:=Width;
      LIconInfo.yHotspot:=Height;
      LIconInfo.hbmMask:=LMask.Handle;
      LIconInfo.hbmColor:=LBitmap.Handle;

      NewIcon := CreateIconIndirect(LIconInfo);
      Result  := NewIcon;
    finally
      LMask.Free;
    end;
  finally
    LBitmap.Free;
  end;
end;


function TAwesomeFont.GetIcon(const ACode: Word; Width, Height: Integer; AColor, ABackColor: TColor; Orientation: Integer = 0; ImageAlignment: TImageAlignment = iaLeft): HICON;
begin
  Result:=GetIcon(ACode, Width, Height, Width, Height, AColor, ABackColor, Orientation, ImageAlignment);
end;
procedure TAwesomeFont.DrawChar(DC: HDC; const AChar: Char; DestRect: TRect; AColor: TColor; Orientation : Integer = 0; ImageAlignment: TImageAlignment = iaLeft);
var
  LogFont: TLogFont;
  AFont  : HFONT;
  pOldFont: HGDIOBJ;
  LColorRef: COLORREF;
  OldMode: integer;
  uFormat : Cardinal;
begin
  ZeroMemory(@LogFont, SizeOf(LogFont));
  LogFont.lfHeight := DestRect.Height;
  LogFont.lfWidth := 0;
  LogFont.lfEscapement := Orientation * 10;
  LogFont.lfOrientation := LogFont.lfEscapement;
  LogFont.lfWeight := FW_NORMAL;
  LogFont.lfItalic := 0;
  LogFont.lfUnderline := 0;
  LogFont.lfStrikeOut := 0;
  LogFont.lfCharSet := DEFAULT_CHARSET;
  LogFont.lfOutPrecision := OUT_DEFAULT_PRECIS;
  LogFont.lfClipPrecision := CLIP_DEFAULT_PRECIS;
  LogFont.lfQuality := FDefaultQuality;
  LogFont.lfPitchAndFamily := DEFAULT_PITCH;
  LogFont.lfFaceName := 'FontAwesome';

  LColorRef:= ColorToRGB(AColor);

//  OutputDebugString(PChar(Format('%s %s', [formatDateTime('hh:nn:ss.zzz', Now), '1'])));
  AFont := CreateFontIndirect(LogFont);
  if AFont <> 0 then
    try
      LColorRef := SetTextColor(DC, LColorRef);
      pOldFont := SelectObject(DC, AFont);
      try
        OldMode := SetBkMode(DC, TRANSPARENT);
        uFormat := DT_SINGLELINE;

        case ImageAlignment of
         iaLeft   : uFormat:= uFormat or DT_LEFT;
         iaRight  : uFormat:= uFormat or DT_RIGHT;
         iaCenter : uFormat:= uFormat or DT_CENTER;
         iaTop    : uFormat:= uFormat or DT_TOP;
         iaBottom : uFormat:= uFormat or DT_BOTTOM;
        end;

        Winapi.Windows.DrawText(DC, AChar, 1, DestRect, uFormat);
        SetBkMode(DC, OldMode);
        SelectObject(DC, LColorRef);
      finally
        if pOldFont <> 0 then
          SelectObject(DC, pOldFont);
      end;
    finally
      DeleteObject(AFont);
    end;
//  OutputDebugString(PChar(Format('%s %s', [formatDateTime('hh:nn:ss.zzz', Now), '2'])));
end;

initialization
   AwesomeFont := TAwesomeFont.Create;
finalization
  AwesomeFont.Free;
end.
