// **************************************************************************************************
//
// Unit Vcl.Styles.Utils.Menus
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
//
// Portions created by Mahdi Safsafi [SMP3]   e-mail SMP@LIVE.FR
// Portions created by Rodrigo Ruz V. are Copyright (C) 2013-2015 Rodrigo Ruz V.
// All Rights Reserved.
//
// **************************************************************************************************
unit Vcl.Styles.Utils.Menus;

interface

{$DEFINE UseVCLStyleUtilsMenu}
// {$IF CompilerVersion >= 27}    // uncomment these lines if you want to use the VCL Styles Menus Hooks
// {$UNDEF UseVCLStyleUtilsMenu}  // included on XE6-XE8  (Embarcadero Version)
// {$IFEND}                       //

uses
  System.Classes,
  System.Types,
  System.SysUtils,
  System.Math,
  Winapi.Windows,
  Winapi.Messages,
  Winapi.UxTheme,
  Vcl.Themes,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.ImgList,
  Vcl.GraphUtil,
  Vcl.Controls,
  Vcl.Menus,
  Vcl.Styles.Utils.SysStyleHook;

const
  { The Undocumented Messages }
  MN_SETHMENU = $01E0;
  MN_GETHMENU = $01E1;
  MN_SIZEWINDOW = $01E2;
  MN_OPENHIERARCHY = $01E3;
  MN_CLOSEHIERARCHY = $01E4;
  MN_SELECTITEM = $01E5;
  MN_CANCELMENUS = $01E6;
  MN_SELECTFIRSTVALIDITEM = $01E7;
  MN_GETPPOPUPMENU = $01EA;
  MN_FINDMENUWINDOWFROMPOINT = $01EB;
  MN_SHOWPOPUPWINDOW = $01EC;
  MN_BUTTONDOWN = $01ED;
  MN_MOUSEMOVE = $01EE;
  MN_BUTTONUP = $01EF;
  MN_SETTIMERTOOPENHIERARCHY = $01F0;
  MN_DBLCLK = $001F1;

  { MARLETT Font Char Const }
  MARLETT_RESTORE_CHAR = Char(50);
  MARLETT_MINIMIZE_CHAR = Char(48);
  MARLETT_CLOSE_CHAR = Char(114);
  MARLETT_MAXIMIZE_CHAR = Char(49);

type
  TSysPopupStyleHook = class;
  TSysPopupItemState = set of (isHot, isDisabled, isChecked, isDefault);
  TSysPopupItemStyle = (isNormal, isSep, isDropDown);

  TSysPopupStyleHook = class(TSysStyleHook)
  private type
{$REGION 'TSysPopupItem'}
    TSysPopupItem = class
    private
      FIndex: integer;
      FMenu: HMENU;
      FHandle: HWND;
      FSysParent: TSysControl;
      FSysPopupStyleHook: TSysPopupStyleHook;
      function GetItemRect: TRect;
      function IsItemDisabled: Boolean;
      function IsItemContainsSubMenu: Boolean;
      function IsItemSeparator: Boolean;
      function IsItemChecked: Boolean;
      function IsItemDefault: Boolean;
      function GetItemText: String;
      function GetVCLMenuItems: TMenuItem;
      function GetVCLMenuItemsFast: TMenuItem;
      function GetItemBitmap: HBITMAP;
      function IsItemRadioCheck: Boolean;
      // function isItemVisible: Boolean;
      function IsItemOwnerDraw: Boolean;
      function GetItemID: WORD;
      function GetVCLRealItem: TMenuItem;
    public
      constructor Create(SysPopupStyleHook: TSysPopupStyleHook; SysParent: TSysControl; const Index: integer; const Menu: HMENU); virtual;
      Destructor Destroy; override;
      property ID: WORD read GetItemID;
      property ItemRect: TRect read GetItemRect;
      property Disabled: Boolean read IsItemDisabled;
      property Separator: Boolean read IsItemSeparator;
      property HasSubMenu: Boolean read IsItemContainsSubMenu;
      property Checked: Boolean read IsItemChecked;
      // property Visible: Boolean read isItemVisible;
      property RadioCheck: Boolean read IsItemRadioCheck;
      property DefaultItem: Boolean read IsItemDefault;
      property Text: String read GetItemText;
      property OwnerDraw: Boolean read IsItemOwnerDraw;
      property VCLMenuItems: TMenuItem read GetVCLMenuItemsFast;
      property VCLItem: TMenuItem read GetVCLRealItem;
      property Bitmap: HBITMAP read GetItemBitmap;
    end;
{$ENDREGION}

  var
    FOffset : Integer;
    FItemsPainted: Boolean;
    FParentSubItemPainted: Boolean;
    FPreviousHotItemIndex: integer;
    FPaintFirstItemFromMenu: Boolean;
    FKeyIndex: integer;
    FSysPopupItem: TSysPopupItem;
    FCount: integer;
    FMenu: HMENU;
    FVCLMenuItems: TMenuItem;
    FNCRect : TRect;
    function GetMenuFromHandle(AHandle: HWND): HMENU;
    function GetItemsCount: integer;
    procedure MNSELECTITEM(var Message: TMessage); message MN_SELECTITEM;
    procedure WMPRINT(var Message: TMessage); message WM_PRINT;
    function GetSysPopupItem(Index: integer): TSysPopupItem;
    function GetRightToLeft: Boolean;
  protected
    procedure EraseItem(Canvas: TCanvas; const Index: integer; const ItemRect: TRect); virtual;
    procedure DoDrawItem(Canvas: TCanvas; const Index: integer);
    procedure DrawItem(Canvas: TCanvas; const Index: integer; const ItemRect: TRect; const ItemText: String; const State: TSysPopupItemState;
      const Style: TSysPopupItemStyle); Virtual;
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
    procedure UpdateColors; override;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
    property Menu: HMENU read FMenu;
    property Items[Index: integer]: TSysPopupItem read GetSysPopupItem;
    property Count: integer read FCount;
    property RightToLeft: Boolean read GetRightToLeft;
  end;

implementation

{
  RANGE CHECKS OFF .
  IMPLICIT_STRING_CAST_LOSS OFF .
}
{$R-,WARN IMPLICIT_STRING_CAST_LOSS OFF}

uses
  Vcl.Styles.Utils.SysControls,
  Vcl.Styles.Utils.Graphics;

type
  TControlClass = Class(TControl);

function GetBmpInfo(hBmp: HBITMAP): Bitmap;
begin
  ZeroMemory(@Result, sizeof(Bitmap));
  GetObject(hBmp, sizeof(Result), @Result);
end;

function GetBitmapHeight(hBmp: HBITMAP): integer;
begin
  Result := GetBmpInfo(hBmp).bmHeight;
end;

function GetBitmapWidth(hBmp: HBITMAP): integer;
begin
  Result := GetBmpInfo(hBmp).bmWidth;
end;

function BmpToIcon(hBmp: HBITMAP): HICON;
var
  Bmp: Bitmap;
  hbmMask: HBITMAP;
  DC: HDC;
  piconinfo: TIconInfo;
  Icon: HICON;
begin
  Icon := 0;
  FillChar(Bmp, sizeof(Bitmap), Char(0));
  if GetObject(hBmp, sizeof(Bitmap), @Bmp) > 0 then
  begin
    DC := GetDC(0);
    if DC <> 0 then
      try
        hbmMask := CreateCompatibleBitmap(DC, Bmp.bmWidth, Bmp.bmHeight);
        if hbmMask <> 0 then
          try
            ZeroMemory(@piconinfo, sizeof(piconinfo));
            piconinfo.fIcon := True;
            piconinfo.hbmColor := hBmp;
            piconinfo.hbmMask := hbmMask;
            Icon := CreateIconIndirect(piconinfo);
          finally
            DeleteObject(hbmMask);
          end;
      finally
        ReleaseDC(0, DC);
      end;
  end;
  Result := Icon;
end;

function GetMenuItemPos(Menu: HMENU; ID: integer): integer;
var
  i: integer;
  pMenuItemInfo: TMenuItemInfo;
begin
  Result := -1;
  if Menu = 0 then
    Exit;
  for i := 0 to GetMenuItemCount(Menu) do
  begin
    FillChar(pMenuItemInfo, sizeof(pMenuItemInfo), Char(0));
    pMenuItemInfo.cbSize := sizeof(pMenuItemInfo);
    pMenuItemInfo.fMask := MIIM_ID;
    if (GetMenuItemInfo(Menu, i, True, pMenuItemInfo)) then
      if pMenuItemInfo.wID = Cardinal(ID) then
        Exit(i);
  end;
end;

{ TSysPopupStyleHook }
constructor TSysPopupStyleHook.Create(AHandle: THandle);
begin
  inherited;
{$IF CompilerVersion > 23}
  StyleElements := [seFont, seClient, seBorder];
{$ELSE}
  OverridePaint := True;
  OverridePaintNC := True;
  OverrideFont := True;
{$IFEND}
  FPreviousHotItemIndex := -1;
  FKeyIndex := -1;
  FItemsPainted := False;
  FSysPopupItem := nil;
  FVCLMenuItems := nil;
  FOffset := 0;

  // Font := Screen.MenuFont;
end;

destructor TSysPopupStyleHook.Destroy;
begin
  if Assigned(FSysPopupItem) then
    FreeAndNil(FSysPopupItem);
  inherited;
end;

procedure TSysPopupStyleHook.DoDrawItem(Canvas: TCanvas; const Index: integer);
var
  LRect, LRect2, LItemRect: TRect;
  P: TPoint;
  State: TSysPopupItemState;
  Style: TSysPopupItemStyle;
  LText: String;
  SaveIndex: integer;
  Item: TSysPopupItem;
begin
  if (Index < 0) or (Index > Count - 1) then
    Exit;

  Item := Items[Index];
  LItemRect := Item.ItemRect;
  P := Point(LItemRect.Left, LItemRect.Top);
  ScreenToClient(Handle, P);

 GetMenuItemRect(0, FMenu, Index, LRect);
 //OutputDebugString(PChar(Format('Index %d  Width %d Height %d Left %d Top %d', [Index, LRect.Width, LRect.Height, LRect.Left, LRect.Top])));

 if SysControl.ClientRect.Height>LRect.Height then
 begin
   //GetWindowRect(SysControl.Handle, LRect2);
   LRect2:= SysControl.ClientRect;
   LRect2.Height:=  LRect2.Height - LRect.Height;
 end
 else
   LRect2:= SysControl.ClientRect;

  //prevent draw  not visible items on larger menus
  if not PtInRect (LRect2, P) then
   Exit;


  LItemRect := Rect(P.X, P.Y, P.X + LItemRect.Width, P.Y + LItemRect.Height);


  if LItemRect.Left < 2 then
    LItemRect.Left := 2;
  inc(LItemRect.Right, 4);
  if LItemRect.Top < 2 then
    inc(LItemRect.Top, 2);
  { Item State }
  State := [];
  if index <> FPreviousHotItemIndex then
    Include(State, isHot);
  if Item.Disabled then
    Include(State, isDisabled);
  if Item.Checked then
    Include(State, isChecked);
  if Item.DefaultItem then
    Include(State, isDefault);
  { Item Style }
  Style := isNormal;
  if Item.Separator then
    Style := isSep;
  if Item.HasSubMenu then
    Style := isDropDown;

  LText := '';
  if Style <> isSep then
    LText := Item.Text;

  SaveIndex := SaveDC(Canvas.Handle);
  try
    EraseItem(Canvas, Index, LItemRect);
  finally
    RestoreDC(Canvas.Handle, SaveIndex)
  end;

  SaveIndex := SaveDC(Canvas.Handle);
  try
    DrawItem(Canvas, Index, LItemRect, LText, State, Style);
  finally
    RestoreDC(Canvas.Handle, SaveIndex)
  end;
end;

procedure TSysPopupStyleHook.DrawItem(Canvas: TCanvas; const Index: integer; const ItemRect: TRect; const ItemText: String; const State: TSysPopupItemState;
  const Style: TSysPopupItemStyle);
var
  Detail: TThemedMenu;
  LDetails: TThemedElementDetails;
  LTextFormat: TTextFormat;
  DC: HDC;
  LSize: TSize;
  LMenuItem: TMenuItem;
  LOwnerDrawState: TOwnerDrawState;
  P, ImageIndex: integer;
  LImageRect, R: TRect;
  LImageWidth: integer;
  LTextRect: TRect;
  hBmp: HBITMAP;
  BmpHeight, BmpWidth: integer;
  Icon: HICON;
  DisplayCheckedGlyph: Boolean;
  Sign: Char;
  SysItem: TSysPopupItem;
  sShortCut: String;
  Bmp: TBitmap;
  LParentMenu: TMenu;

  procedure DrawSubMenu(const ItemRect: TRect);
  var
    Bmp: TBitmap;
    LSubMenuDetails: TThemedElementDetails;
    LSubMenuDetail: TThemedMenu;
    SubMenuSize: TSize;
    LSubMenuRect: TRect;
  begin

    LSubMenuRect := Rect(0, 0, 0, 0);
    LSubMenuDetail := tmPopupSubMenuNormal;
    if isDisabled in State then
      LSubMenuDetail := tmPopupSubMenuDisabled;
    LSubMenuDetails := StyleServices.GetElementDetails(LSubMenuDetail);
    StyleServices.GetElementSize(DC, LSubMenuDetails, esActual, SubMenuSize);
    if not RightToLeft then
      LSubMenuRect := Rect(ItemRect.Right - SubMenuSize.cx, ItemRect.Top, ItemRect.Right, ItemRect.Top + SubMenuSize.cy)
    else
      LSubMenuRect := Rect(ItemRect.Left + 4, ItemRect.Top, ItemRect.Left + 4 + SubMenuSize.Width, ItemRect.Bottom);
    Bmp := TBitmap.Create;
    try
      Bmp.SetSize(SubMenuSize.Width, SubMenuSize.Height);
      Bmp.Canvas.Brush.Color := clFuchsia;
      Bmp.Canvas.FillRect(Rect(0, 0, SubMenuSize.Width, SubMenuSize.Height));
      StyleServices.DrawElement(Bmp.Canvas.Handle, LSubMenuDetails, Rect(0, 0, SubMenuSize.Width, SubMenuSize.Height));
      if RightToLeft then
      begin
        RotateBitmap(Bmp, DegToRad(180), False, clFuchsia);
        inc(LSubMenuRect.Top, (Bmp.Height div 2) - 2);
      End
      else
        Dec(LSubMenuRect.Left, 4);

      TransparentBlt(DC, LSubMenuRect.Left, LSubMenuRect.Top, SubMenuSize.Width, SubMenuSize.Height, Bmp.Canvas.Handle, 0, 0, SubMenuSize.Width,
        SubMenuSize.Height, clFuchsia);
    finally
      Bmp.Free;
    end;
    Dec(LTextRect.Right, LSubMenuRect.Width);
  end;

  procedure DrawSpecialChar(DC: HDC; const Sign: Char; DestRect: TRect; const Bold: Boolean = False; const Disabled: Boolean = False);
  var
    LogFont: TLogFont;
    pOldFont: HGDIOBJ;
    AFont: HFONT;
    oldColor: COLORREF;
    OldMode: integer;
  begin
    LogFont.lfHeight := DestRect.Height;
    LogFont.lfWidth := 0;
    LogFont.lfEscapement := 0;
    LogFont.lfOrientation := 0;
    if Bold then
      LogFont.lfWeight := FW_BOLD
    else
      LogFont.lfWeight := FW_NORMAL;
    LogFont.lfItalic := 0;
    LogFont.lfUnderline := 0;
    LogFont.lfStrikeOut := 0;
    LogFont.lfCharSet := DEFAULT_CHARSET;
    LogFont.lfOutPrecision := OUT_DEFAULT_PRECIS;
    LogFont.lfClipPrecision := CLIP_DEFAULT_PRECIS;
    LogFont.lfQuality := DEFAULT_QUALITY;
    LogFont.lfPitchAndFamily := DEFAULT_PITCH;
    LogFont.lfFaceName := 'Marlett';

    if Disabled then
      oldColor := ColorToRGB(StyleServices.GetStyleFontColor(sfPopupMenuItemTextDisabled))
    else
    begin
      oldColor := ColorToRGB(StyleServices.GetStyleFontColor(sfPopupMenuItemTextNormal));
      if isHot in State then
        oldColor := ColorToRGB(StyleServices.GetStyleFontColor(sfPopupMenuItemTextHot));
      if isDisabled in State then
        oldColor := ColorToRGB(StyleServices.GetStyleFontColor(sfPopupMenuItemTextDisabled));
    end;

    AFont := CreateFontIndirect(LogFont);
    if AFont <> 0 then
      try
        oldColor := SetTextColor(DC, oldColor);
        pOldFont := SelectObject(DC, AFont);
        try
          OldMode := SetBkMode(DC, Transparent);
          Winapi.Windows.DrawText(DC, Sign, 1, DestRect, DT_LEFT or DT_SINGLELINE);
          SetBkMode(DC, OldMode);
          SelectObject(DC, oldColor);
        finally
          if pOldFont <> 0 then
            SelectObject(DC, pOldFont);
        end;
      finally
        DeleteObject(AFont);
      end;
  end;

begin
  DisplayCheckedGlyph := True;
  LTextRect := ItemRect;
  { Fast access . }
  SysItem := Items[Index]; // Do not destroy !!
  DC := Canvas.Handle;
  R := ItemRect;
  Detail := tmPopupItemNormal;
  if isHot in State then
    Detail := tmPopupItemHot;
  if isDisabled in State then
    Detail := tmPopupItemDisabled;
  if Style = isSep then
  begin
    Detail := tmPopupSeparator;
    inc(R.Left, 25);
  end;

  LDetails := StyleServices.GetElementDetails(Detail);

  if (Detail <> tmPopupItemNormal) and (Detail <> tmPopupItemDisabled) then
    StyleServices.DrawElement(DC, LDetails, R);

  if Style = isDropDown then
    DrawSubMenu(ItemRect);

  LImageWidth := 0;
  LMenuItem := SysItem.VCLMenuItems;
  if LMenuItem <> nil then
    LMenuItem := SysItem.VCLItem;

  LParentMenu := nil;
  if (LMenuItem <> nil) then
    LParentMenu := LMenuItem.GetParentMenu;
  if (LParentMenu <> nil) and (LParentMenu.OwnerDraw) and (@LMenuItem.OnDrawItem <> nil) then
  begin
    LMenuItem.OnDrawItem(LMenuItem, Canvas, ItemRect, (isHot in State));
    Exit;
  end;

  if (LParentMenu <> nil) and (LParentMenu.OwnerDraw) and (LMenuItem <> nil) and (@LMenuItem.OnAdvancedDrawItem <> nil) then
  begin
    LOwnerDrawState := [];

    if isHot in State then
      Include(LOwnerDrawState, odSelected);
    if isDisabled in State then
      Include(LOwnerDrawState, odDisabled);
    if isChecked in State then
      Include(LOwnerDrawState, odChecked);
    if isDefault in State then
      Include(LOwnerDrawState, odDefault);

    LMenuItem.OnAdvancedDrawItem(LMenuItem, Canvas, ItemRect, LOwnerDrawState);
    Exit;
  end;

  if LMenuItem <> nil then
  begin
    { Draw Vcl PopupMenu Bitmap }
    ImageIndex := LMenuItem.ImageIndex;
    with LMenuItem.GetParentMenu do
    begin
      if (ImageIndex < 0) and (LMenuItem.Bitmap <> nil) then
      begin
        Bmp := LMenuItem.Bitmap;
        if (Bmp.Width = 16) and (Bmp.Height = 16) then
        begin
          LImageWidth := Bmp.Width;
          LImageRect := Rect(0, 0, Bmp.Width, Bmp.Height);
          RectVCenter(LImageRect, ItemRect);

          if not RightToLeft then
            OffsetRect(LImageRect, 4, 0)
          else
          begin
            LImageRect.Left := ItemRect.Right - Bmp.Width - 4;
            LImageRect.Right := ItemRect.Right;
          end;

          Canvas.Draw(LImageRect.Left, LImageRect.Top, Bmp)
        end
        else
        if (Bmp.Width > 0) and (Bmp.Height > 0) then
        begin
          LImageWidth := 16;
          LImageRect := Rect(0, 0, 16, 16);
          RectVCenter(LImageRect, ItemRect);
          if not RightToLeft then
            OffsetRect(LImageRect, 4, 0)
          else
          begin
            LImageRect.Left := ItemRect.Right - 16 - 4;
            LImageRect.Right := ItemRect.Right;
          end;

          if (SysItem.Checked) and (not SysItem.RadioCheck)  then
          begin
           R:=LImageRect;
           InflateRect(R, 2, 2);
           Canvas.Brush.Style:=bsClear;
           Canvas.Pen.Color  :=StyleServices.GetSystemColor(clHotLight);
           Canvas.Rectangle(R);
          end;

          Canvas.StretchDraw(LImageRect, Bmp);
        end;

      end
      else if (LMenuItem.GetParentMenu.Images <> nil) and (ImageIndex > -1) then
      begin
        LImageWidth := Images.Width;
        DisplayCheckedGlyph := False;
        LImageRect := Rect(0, 0, Images.Width, Images.Height);
        RectVCenter(LImageRect, ItemRect);

        if not RightToLeft then
          OffsetRect(LImageRect, 4, 0)
        else
        begin
          LImageRect.Left := ItemRect.Right - Images.Width - 4;
          LImageRect.Right := ItemRect.Right;
        end;

        if (SysItem.Checked) and (not SysItem.RadioCheck)  then
        begin
         R:=LImageRect;
         InflateRect(R, 2, 2);
         Canvas.Brush.Style:=bsClear;
         Canvas.Pen.Color  :=StyleServices.GetSystemColor(clHotLight);
         Canvas.Rectangle(R);
        end;

        Images.Draw(Canvas, LImageRect.Left, LImageRect.Top, ImageIndex);
      end;
    end;
  end
  else if SysItem.Bitmap > 0 then
  begin
    hBmp := SysItem.Bitmap;
    if hBmp < HBMMENU_POPUP_MINIMIZE + 1 then
    begin
      { Draw System PopupMenu Bitmap }
      DisplayCheckedGlyph := False;

      case hBmp of
        HBMMENU_POPUP_RESTORE:
          Sign := MARLETT_RESTORE_CHAR;
        HBMMENU_POPUP_MINIMIZE, HBMMENU_MBAR_MINIMIZE_D:
          Sign := MARLETT_MINIMIZE_CHAR;
        HBMMENU_POPUP_MAXIMIZE:
          Sign := MARLETT_MAXIMIZE_CHAR;
        HBMMENU_POPUP_CLOSE, HBMMENU_MBAR_CLOSE_D:
          Sign := MARLETT_CLOSE_CHAR;
      else
        Sign := Char(0);
      end;
      if Sign <> #0 then
      begin
        LImageRect := Rect(0, 0, 10, 10);
        R := Rect(ItemRect.Left, ItemRect.Top, ItemRect.Left + 30, ItemRect.Bottom);
        RectCenter(LImageRect, ItemRect);
        if not RightToLeft then
          LImageRect.Left := ItemRect.Left + 10
        else
        begin
          LImageRect.Left := ItemRect.Right - 10 - 4;
          LImageRect.Right := ItemRect.Right;
        end;
        DrawSpecialChar(DC, Sign, LImageRect, False, (isDisabled in State));
      end;
    end
    else
    begin
      { Draw PopupMenu Bitmap }
      BmpWidth := GetBitmapWidth(hBmp);
      BmpHeight := GetBitmapHeight(hBmp);
      if (BmpWidth > 0) and (BmpHeight > 0) then
      begin
        DisplayCheckedGlyph := False;
        LImageRect := Rect(0, 0, BmpWidth, BmpHeight);
        RectVCenter(LImageRect, ItemRect);
        if not RightToLeft then
          OffsetRect(LImageRect, 4, 0)
        else
        begin
          LImageRect.Left := ItemRect.Right - BmpWidth - 4;
          LImageRect.Right := ItemRect.Right;
        end;

        Icon := BmpToIcon(hBmp);
        if Icon <> 0 then
        begin

          if (SysItem.Checked) and (not SysItem.RadioCheck)  then
          begin
           R:=LImageRect;
           InflateRect(R, 2, 2);
           Canvas.Brush.Style:=bsClear;
           Canvas.Pen.Color  :=StyleServices.GetSystemColor(clHotLight);
           Canvas.Rectangle(R);
          end;

          DrawIconEX(DC, LImageRect.Left, LImageRect.Top, Icon, BmpWidth, BmpHeight, 0, 0, DI_NORMAL);
          DeleteObject(Icon);
        end;
      end;
    end;
  end;

  if (SysItem.Checked)  then
  begin
    Detail := TThemedMenu(integer(tmPopupCheckNormal) + integer(SysItem.Disabled));
    if SysItem.RadioCheck then
      Detail := TThemedMenu(integer(tmPopupBulletNormal) + integer(SysItem.Disabled));
    LDetails := StyleServices.GetElementDetails(Detail);
    StyleServices.GetElementSize(DC, LDetails, esActual, LSize);
    LImageRect := Rect(0, 0, LSize.Width, LSize.Height);

    RectVCenter(LImageRect, ItemRect);
    if DisplayCheckedGlyph then
    begin
      if not RightToLeft then
        OffsetRect(LImageRect, 4, 0)
      else
      begin
        LImageRect.Left := ItemRect.Right - LSize.Width - 4;
        LImageRect.Right := ItemRect.Right;
      end;
      StyleServices.DrawElement(DC, LDetails, LImageRect);
    end;
  end;

  { Draw Text }
  LTextFormat := [tfLeft, tfVerticalCenter, tfSingleLine, tfExpandTabs, tfHidePrefix];
  if not RightToLeft then
    inc(LTextRect.Left, 30)
  else
  begin
    LTextRect.Left := ItemRect.Left;
    LTextRect.Right := ItemRect.Right - 30;
    Exclude(LTextFormat, tfLeft);
    Include(LTextFormat, tfRtlReading);
    Include(LTextFormat, tfRight);
  end;

  if LImageWidth > 0 then
  begin
    if not RightToLeft then
      LTextRect.Left := ItemRect.Left + LImageWidth + 4 + 4
    else
    begin
      LTextRect.Left := ItemRect.Left;
      LTextRect.Right := ItemRect.Right - LImageWidth - 8;
    end;
  end;

  LDetails := StyleServices.GetElementDetails(tmPopupItemNormal);
  if isHot in State then
    LDetails := StyleServices.GetElementDetails(tmPopupItemHot);
  if isDisabled in State then
    LDetails := StyleServices.GetElementDetails(tmPopupItemDisabled);

  if SysItem.DefaultItem then
    Canvas.Font.Style := [fsBold];

  if LMenuItem <> nil then
    DrawText(Canvas.Handle, LDetails, ItemText, LTextRect, LTextFormat)
  else
  begin
    sShortCut := '';
    // http://msdn.microsoft.com/en-us/library/ms647553%28v=VS.85%29.aspx#_win32_Menu_Shortcut_Keys
    P := Pos(#9, ItemText);
    if P > 1 then
    begin
      sShortCut := Copy(ItemText, P + 1, length(ItemText) - P);
      DrawText(Canvas.Handle, LDetails, Copy(ItemText, 1, P), LTextRect, LTextFormat)
    end
    else
      DrawText(Canvas.Handle, LDetails, ItemText, LTextRect, LTextFormat)
  end;

  { Draw ShortCut Text . }
  if LMenuItem <> nil then
  begin
    if LMenuItem.ShortCut <> 0 then
    begin
      sShortCut := ShortCutToText(LMenuItem.ShortCut);
      LTextRect := ItemRect;
      if RightToLeft then
      begin
        LTextRect.Left := ItemRect.Left + 14;
        LTextRect.Right := LTextRect.Left + Canvas.TextWidth(sShortCut);
      end
      else
      begin
        LTextRect.Left := ItemRect.Right - 14 - Canvas.TextWidth(sShortCut);
        LTextRect.Right := ItemRect.Right;
      end;
      DrawText(Canvas.Handle, LDetails, sShortCut, LTextRect, LTextFormat);
    end;
  end
  else if sShortCut <> '' then
  begin
    LTextRect := ItemRect;
    if RightToLeft then
    begin
      LTextRect.Left := ItemRect.Left + 14;
      LTextRect.Right := LTextRect.Left + Canvas.TextWidth(sShortCut);
    end
    else
    begin
      LTextRect.Left := ItemRect.Right - 14 - Canvas.TextWidth(sShortCut);
      LTextRect.Right := ItemRect.Right;
    end;
    DrawText(Canvas.Handle, LDetails, sShortCut, LTextRect, LTextFormat);
  end;
end;

procedure TSysPopupStyleHook.EraseItem(Canvas: TCanvas; const Index: integer; const ItemRect: TRect);
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.SetSize(SysControl.Width, SysControl.Height);
    PaintBackground(Bmp.Canvas);
    BitBlt(Canvas.Handle, ItemRect.Left, ItemRect.Top, ItemRect.Width, ItemRect.Height, Bmp.Canvas.Handle, ItemRect.Left, ItemRect.Top, SRCCOPY);
  finally
    Bmp.Free;
  end;
end;

function TSysPopupStyleHook.GetItemsCount: integer;
begin
  Result := GetMenuItemCount(FMenu);
end;

function TSysPopupStyleHook.GetMenuFromHandle(AHandle: HWND): HMENU;
begin
  Result := HMENU(SendMessage(AHandle, MN_GETHMENU, 0, 0));
end;

function TSysPopupStyleHook.GetRightToLeft: Boolean;
var
  pMenuItemInfo: TMenuItemInfo;
begin
  Result := False;
  FillChar(pMenuItemInfo, sizeof(pMenuItemInfo), Char(0));
  pMenuItemInfo.cbSize := sizeof(TMenuItemInfo);
  pMenuItemInfo.fMask := MIIM_TYPE;
  if GetMenuItemInfo(FMenu, 0, True, pMenuItemInfo) then
    Result := ((pMenuItemInfo.fType and MFT_RIGHTORDER) = MFT_RIGHTORDER) or ((pMenuItemInfo.fType and MFT_RIGHTJUSTIFY) = MFT_RIGHTJUSTIFY);
end;

function TSysPopupStyleHook.GetSysPopupItem(Index: integer): TSysPopupItem;
begin
  Result := nil;
  if (Index > -1) and (index <= Count) then
  begin
    if Assigned(FSysPopupItem) then
      FreeAndNil(FSysPopupItem);
    FSysPopupItem := TSysPopupItem.Create(Self, SysControl, Index, FMenu);
    Result := FSysPopupItem;
  end;
end;

procedure TSysPopupStyleHook.PaintBackground(Canvas: TCanvas);
begin
  StyleServices.DrawElement(Canvas.Handle, StyleServices.GetElementDetails(tmPopupBorders), SysControl.ClientRect);
end;

procedure TSysPopupStyleHook.UpdateColors;
begin
  inherited;
  Font := Screen.MenuFont;
end;

type
  TSubMenuItemInfo = record
    Menu: HMENU;
    WindowHandle: HWND;
    ItemIndex: integer;
  end;

var
  SubMenuItemInfoArray: array of TSubMenuItemInfo;

procedure TSysPopupStyleHook.MNSELECTITEM(var Message: TMessage);
var
  DC: HDC;
  Canvas: TCanvas;
  Index: integer;
  i: WORD;
  L: integer;
  ParentItem: integer;
  ParentPopup: HWND;
  LMenu: HMENU;
begin
  { The undocumented MN_SELECTITEM Message:
    This is the most importants message ,
    Windows sends this message every time when the user
    select an item (not clicking, only select) ...
    wParam=Current Item Index .
    lparam= may be it's unused (not sure).
  }
  //
  Handled := False;
  DC := 0;
  Canvas := TCanvas.Create;
  try
    ParentPopup := 0;
    ParentItem := -1;
    DC := GetDC(Handle);
    Canvas.Handle := DC;
    Index := integer(Message.WParam);

    //OutputDebugString(PChar(Format('MNSELECTITEM Index %d', [Index])));

    if Assigned(Font) then
      Canvas.Font := Font;
    { Out of index . }
    if (Index > FCount - 1) or (Index < 0) then
    begin
      { Make sure that wParam hold a valid Item Index .
        if not .. then mouse is not on the PopupMenu
        => Remove Item highlight .
      }
      SetRedraw(True);
      if (FPreviousHotItemIndex > -1) and (FPreviousHotItemIndex < FCount) then
        DoDrawItem(Canvas, FPreviousHotItemIndex);
      FPreviousHotItemIndex := -1;
      Handled := True;
      Exit;
    end;

    if not FItemsPainted then
    begin
      { Items are not painted completely . }
      FPreviousHotItemIndex := Index;
      DoDrawItem(Canvas, Index);
      if (Index = Count - 1) then
        FItemsPainted := True;
      Handled := True;
      Exit;
    end;

    L := length(SubMenuItemInfoArray);
    if L <> 0 then
    begin
      for i := 0 to L - 1 do
      begin
        { Look for SubMenu Parent }
        LMenu := SubMenuItemInfoArray[i].Menu;
        if LMenu = FMenu then
        begin
          ParentPopup := SubMenuItemInfoArray[i].WindowHandle;
          ParentItem := SubMenuItemInfoArray[i].ItemIndex;
          Break;
        end;
      end;
    end;

    if (ParentPopup = Handle) then
      SetRedraw(True) { Allow drawing the current PopupMenu }
    else if ((ParentPopup <> Handle) and (FItemsPainted) and (ParentPopup <> 0)) then
    begin
      {
        if user jump so fast from the parent PopupMenu to the
        Child PopupMenu (SubMenu) , the hot item of parent Popup menu
        will be draw as a normal item (not hot)..
        So we need to repaint the hot item that drop the child popup menu.
      }
      if (not FParentSubItemPainted) and (ParentItem > -1) then
      begin
        SendMessage(ParentPopup, MN_SELECTITEM, ParentItem, 0);
        FParentSubItemPainted := True;
      end;
      { Don't Redraw the parent of the Current PopupMenu }
      // SetRedraw(ParentPopup, False);  //issue #81
    end;

    { if Item can drop a sub Popup Menu }
    if Items[Index].HasSubMenu then
    begin
      L := length(SubMenuItemInfoArray);
      if L = 0 then
      begin
        SetLength(SubMenuItemInfoArray, 1);
        SubMenuItemInfoArray[0].Menu := 0;
        L := 1;
      end;

      LMenu := GetMenuFromHandle(Handle);
      for i := 0 to L - 1 do
        { Avoid duplication }
        if SubMenuItemInfoArray[i].Menu <> LMenu then
        begin
          inc(L);
          SetLength(SubMenuItemInfoArray, L);
          SubMenuItemInfoArray[L - 1].Menu := GetSubMenu(FMenu, Index);
          SubMenuItemInfoArray[L - 1].WindowHandle := Handle;
          SubMenuItemInfoArray[L - 1].ItemIndex := Index;
          Break;
        end;
    end;

    { If all Items are painted }
    if FItemsPainted then
    begin
      { In order to show / hide SubMenu ,we need to
        process the default message handler . }
      SetRedraw(False);
      Message.Result := CallDefaultProc(Message);
      SetRedraw(True);
    end;

    if FPreviousHotItemIndex <> Index then
    begin
      { Draw Item normal . }
      DoDrawItem(Canvas, FPreviousHotItemIndex);
      { Draw Item hot . }
      DoDrawItem(Canvas, Index);
      FPreviousHotItemIndex := Index;
    end;

  finally
    Canvas.Handle := 0;
    Canvas.Free;
    if DC <> 0 then
      ReleaseDC(Handle, DC);
  end;
  Handled := True;
end;

procedure TSysPopupStyleHook.WMPRINT(var Message: TMessage);
var
  DC: HDC;
  i: integer;
  Canvas: TCanvas;
begin
  FMenu := GetMenuFromHandle(Handle);
  FCount := GetItemsCount;

  if Message.WParam <> 0 then
    DC := HDC(Message.WParam)
  else
    DC := GetDC(Handle);

  Canvas := TCanvas.Create;
  try
    Canvas.Handle := DC;
    PaintBackground(Canvas);
  finally
    Canvas.Handle := 0;
    Canvas.Free;
    if DC <> HDC(Message.WParam) then
      ReleaseDC(Handle, DC);
  end;

  if Count > -1 then
  begin
    //exit;
    //FCount:=48;

//    for i := 0 to Count - 1 do
//    begin
//     GetMenuItemRect(0, FMenu, i, LRect);
//     OutputDebugString(PChar(Format('Index %d  Width %d Height %d Left %d Top %d', [i, LRect.Width, LRect.Height, LRect.Left, LRect.Top])));
//    end;

    for i := 0 + FOffset to Count - 1 do
      PostMessage(Handle, MN_SELECTITEM, i, 0);
  end;
  Handled := True;
end;

function IsItemSeparator(Menu: HMENU; const ItemIndex: integer): Boolean;
var
  pMenuItemInfo: TMenuItemInfo;
begin
  {
    Use this function instead of Items[Index].Separator .
    ==> Fast access in WM_KEYDOWN .
  }
  Result := False;
  FillChar(pMenuItemInfo, sizeof(pMenuItemInfo), Char(0));
  pMenuItemInfo.cbSize := sizeof(TMenuItemInfo);
  pMenuItemInfo.fMask := MIIM_FTYPE;
  if GetMenuItemInfo(Menu, ItemIndex, True, pMenuItemInfo) then
    Result := (pMenuItemInfo.fType and MFT_SEPARATOR) = MFT_SEPARATOR;
end;

procedure TSysPopupStyleHook.WndProc(var Message: TMessage);
var
  i: integer;
  TopWin: HWND;
  TopCntrl: TControl;
begin
//  AddToLog(Message);
//  Message.Result := CallDefaultProc(Message);
//  Exit;

  case Message.Msg of
//
//    MN_BUTTONDOWN:
//      begin
//        AddToLog(Message);
//        Message.Result := CallDefaultProc(Message);
//        Exit;
//      end;


    MN_SELECTITEM, WM_PRINT:
      begin
        if (not OverridePaint) or (not OverridePaintNC) then
        begin
          Message.Result := CallDefaultProc(Message);
          Exit; { Do not Dispatch . }
        end;
      end;

    WM_PAINT:
      begin
        if not OverridePaint then
        begin
          Message.Result := CallDefaultProc(Message);
          Exit;
        end;
        SetRedraw(False);
        Message.Result := CallDefaultProc(Message);
        SetRedraw(True);
        Exit; { Do not Dispatch . }
      end;

    WM_WINDOWPOSCHANGED:
      begin
        if (not OverridePaint) or (not OverridePaintNC) then
        begin
          Message.Result := CallDefaultProc(Message);
          Exit;
        end;
        SetTimer(Handle, $93, 100, nil);
      end;

    WM_TIMER:
      begin
        if (FItemsPainted) and (Message.WParam = $93) then
        begin
          { If PopupMenu is droped from MainMenu ,
            MainMenu will send WM_KEYDOWN message
            to the PopupMenu that cause the PopupMenu
            to paint the first item as a hot item instead of
            a normal item .
            I use a timer to solve this problem .
          }
          FPaintFirstItemFromMenu := True;
          KillTimer(Handle, $93);
        end;
      end;

    MN_BUTTONDOWN:
      begin
        Inc(FOffset);
        //AddToLog(Message);
        //AddToLog('Index '+IntToStr(UINT(Message.WParamLo)));
        SetRedraw(False);
        //SendMessage(Handle, WM_PRINT, 0, 0);
        Message.Result := CallDefaultProc(Message);
        SetRedraw(True);
      end;

    WM_KEYDOWN:
      begin
        if (not OverridePaint) or (not OverridePaintNC) then
        begin
          Message.Result := CallDefaultProc(Message);
          Exit;
        end;
        FMenu := GetMenuFromHandle(Handle);
        if FPreviousHotItemIndex <> -1 then
          FKeyIndex := FPreviousHotItemIndex;
        case Message.WParam of

          VK_DOWN:
            if FPaintFirstItemFromMenu then
            begin
              if FKeyIndex >= GetMenuItemCount(Menu) - 1 then
                FKeyIndex := -1;
              inc(FKeyIndex);
              { If the Current Item is Separator then
                find the next valid item .
              }
              if IsItemSeparator(Menu, FKeyIndex) then
                for i := FKeyIndex to GetMenuItemCount(Menu) - 1 do
                  if (not IsItemSeparator(Menu, i)) then
                  begin
                    FKeyIndex := i;
                    Break;
                  end;
              SendMessage(Handle, MN_SELECTITEM, FKeyIndex, 0);
              Message.Result := 0;
            end;

          VK_UP:
            begin
              if FKeyIndex <= 0 then
                FKeyIndex := GetMenuItemCount(Menu);

              Dec(FKeyIndex);
              { If the Current Item is Separator then
                find the next valid item .
              }
              if IsItemSeparator(Menu, FKeyIndex) then
                for i := FKeyIndex downto 0 do
                  if not IsItemSeparator(Menu, i) then
                  begin
                    FKeyIndex := i;
                    Break;
                  end;
              SendMessage(Handle, MN_SELECTITEM, FKeyIndex, 0);
              Message.Result := 0;
            end;

        else
          { Calling the Default Message will cause
            the WM_PAINT Message to be Sent to the PopupMenu Window }
          Message.Result := CallDefaultProc(Message);
        end;
        Exit;
      end;

    WM_ERASEBKGND:
      begin
        if (not OverridePaint) or (not OverridePaintNC) then
        begin
          Message.Result := CallDefaultProc(Message);
          Exit;
        end;
        SendMessage(Handle, WM_PRINT, Message.WParam, Message.lParam);
        Message.Result := 1;
        Exit; { Do not Dispatch . }
      end;

    WM_PRINTCLIENT:
      begin
        if (not OverridePaint) or (not OverridePaintNC) then
        begin
          Message.Result := CallDefaultProc(Message);
          Exit;
        end;
        SendMessage(Handle, WM_PRINT, Message.WParam, Message.lParam);
        Exit;
      end;

    WM_NCCALCSIZE, WM_NCPAINT:
      begin
        if Message.Msg= WM_NCCALCSIZE then
        begin
          if TWMNCCalcSize(Message).CalcValidRects then
          begin
            FNCRect := TWMNCCalcSize(Message).CalcSize_Params.rgrc[0];
            //Message.Result := CallDefaultProc(Message);
            //LRect := TWMNCCalcSize(Message).CalcSize_Params.rgrc0;
            //OutputDebugString(PChar(Format('LRect.Height %d WParam %d', [FNCRect.Height, Message.WParam])));
          end;
        end;

        if (not OverridePaint) or (not OverridePaintNC) then
        begin
          Message.Result := CallDefaultProc(Message);
          Exit;
        end;
        if not StyleServicesEnabled then
        begin
          Handled := False;
          Exit;
        end;
        Exit; { Do not Dispatch . }
      end;

    WM_DESTROY:
      begin
        TopWin := GetForegroundWindow;
        if TopWin > 0 then
        begin
          { The parent window that host menu should be repained !! }
          if IsVCLControl(TopWin) then
          begin
            TopCntrl := FindControl(TopWin);
            if Assigned(TopCntrl) then
            begin
              {
                Must use TControl.Refresh to allow invalidating
                others no TWinControl !
              }
              TopCntrl.Refresh;
            end;
          end
          else if IsControlHooked(TopWin) then
          begin
            // AddToLog(IntToStr(TopWin));
            InvalidateRect(TopWin, nil, False);
            UpdateWindow(TopWin);
          end;
        end;
        FVCLMenuItems := nil;
        SetLength(SubMenuItemInfoArray, 0);
        SubMenuItemInfoArray := nil;
        Handled := False;
      end;

    //
    // WM_NCDESTROY :
    // begin
    // end;

  end;
  inherited;
end;

{ TSysPopupItem }

constructor TSysPopupStyleHook.TSysPopupItem.Create(SysPopupStyleHook: TSysPopupStyleHook; SysParent: TSysControl; const Index: integer; const Menu: HMENU);
begin
  inherited Create;
  FSysPopupStyleHook := SysPopupStyleHook;
  FMenu := Menu;
  FHandle := SysParent.Handle;
  FSysParent := SysParent;
  FIndex := Index;
end;

destructor TSysPopupStyleHook.TSysPopupItem.Destroy;
begin
  inherited;
end;

function TSysPopupStyleHook.TSysPopupItem.GetItemBitmap: HBITMAP;
var
  pMenuItemInfo: TMenuItemInfo;
begin
  Result := 0;
  FillChar(pMenuItemInfo, sizeof(pMenuItemInfo), Char(0));
  pMenuItemInfo.cbSize := sizeof(TMenuItemInfo);
  pMenuItemInfo.fMask := MIIM_CHECKMARKS or MIIM_BITMAP;
  if GetMenuItemInfo(FMenu, FIndex, True, pMenuItemInfo) then
  begin
    Result := pMenuItemInfo.hbmpItem;
    if Result = 0 then
      Result := pMenuItemInfo.hbmpUnchecked;
  end;
end;

function TSysPopupStyleHook.TSysPopupItem.GetItemID: WORD;
begin
  Result := 0;
  if (FMenu > 0) and (FIndex > -1) then
    Result := GetMenuItemID(FMenu, FIndex);
end;

function TSysPopupStyleHook.TSysPopupItem.GetItemRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if (FMenu > 0) and (FIndex > -1) then
    GetMenuItemRect(0, FMenu, FIndex, Result);
end;

function TSysPopupStyleHook.TSysPopupItem.GetItemText: String;
var
  Buffer: PChar;
  StrSize: integer;
  pMenuItemInfo: MENUITEMINFO;
begin

  if VCLItem <> nil then
  begin
    Result := VCLItem.Caption;
    Exit;
  end;

  { Note:
    The GetMenuString function has been superseded.
    Use the GetMenuItemInfo function to retrieve the menu item text.
  }

  Result := '';

  FillChar(pMenuItemInfo, SizeOf(MENUITEMINFO), Char(0));
  pMenuItemInfo.cbSize := SizeOf(MENUITEMINFO);
  pMenuItemInfo.fMask := MIIM_STRING or MIIM_FTYPE;
  pMenuItemInfo.dwTypeData := nil;
  if GetMenuItemInfo(FMenu, FIndex, True, pMenuItemInfo) then
  begin
     //Fix for shell menus on W10
    if (VCLMenuItems=nil) or (not (pMenuItemInfo.fType and MFT_OWNERDRAW = MFT_OWNERDRAW)) then
    begin
      { The Size needed for the Buffer . }
      StrSize := pMenuItemInfo.cch * 2 + 2;
      GetMem(Buffer, StrSize);
      try
        pMenuItemInfo.dwTypeData := Buffer;
        { inc cch to get the last char . }
        inc(pMenuItemInfo.cch);
        if GetMenuItemInfo(FMenu, FIndex, True, pMenuItemInfo) then
          Result := String(Buffer);
      finally
        // OutputDebugString(PChar('StrSize '+IntToStr(StrSize)));
        FreeMem(Buffer, StrSize);
      end;
      Exit;
    end
    else
    begin
      { if the item is owner draw then we need another way to get
        the item text since , when setting an item to ownerdraw windows
        will destroy the dwTypeData that hold the text . }
      FillChar(pMenuItemInfo, sizeof(MENUITEMINFO), Char(0));
      pMenuItemInfo.cbSize := sizeof(MENUITEMINFO);
      pMenuItemInfo.fMask := MIIM_DATA;
      if GetMenuItemInfo(FMenu, FIndex, True, pMenuItemInfo) then
        Result := String(PChar(pMenuItemInfo.dwItemData));
    end;
  end;
end;

function TSysPopupStyleHook.TSysPopupItem.GetVCLRealItem: TMenuItem;
var
  i: integer;
  VisibleItems: TList;
  LVCLMenuItems: TMenuItem;
begin
  {
    Return the real menu item .
    If MenuItem has the Visible property set to false
    windows will delete this item but the VCL will not delete
    the item from Items property .And thats can cause the item to be painted !
    Do not access VCLMenuItems.Items[Index] directly
    => Instead , use this one : VCLItem .
  }
  VisibleItems := nil;
  Result := nil;
  LVCLMenuItems := VCLMenuItems;
  if LVCLMenuItems <> nil then
  begin
    VisibleItems := TList.Create;
    for i := 0 to LVCLMenuItems.Count - 1 do
    begin
      if LVCLMenuItems.Items[i].Visible then
        VisibleItems.Add(LVCLMenuItems.Items[i]);
    end;
  end;
  if Assigned(VisibleItems) then
  begin
    if (VisibleItems.Count > 0) and (FIndex < VisibleItems.Count) then
      Result := VisibleItems.Items[FIndex];
    FreeAndNil(VisibleItems);
  end;
end;

function TSysPopupStyleHook.TSysPopupItem.GetVCLMenuItems: TMenuItem;
var
  i, j: integer;
  LPopupMenu: TPopupMenu;
  LForm: TCustomForm;
  LMenuItem: TMenuItem;

  function GetChildPopup(Comp: TComponent): TMenuItem;
  var
    k: integer;
  begin
    Result := nil;
    if Assigned(Comp) then
    begin
      for k := 0 to Comp.ComponentCount - 1 do
      begin

        if Comp.Components[k] is TPopupMenu then
        begin
          LPopupMenu := TPopupMenu(Comp.Components[k]);
          if LPopupMenu.Handle = FMenu then
            Exit(LPopupMenu.Items);
        end
        else if Comp.Components[k] is TMenuItem then
        begin
          LMenuItem := TMenuItem(Comp.Components[k]);
          if LMenuItem.Handle = FMenu then
            Exit(LMenuItem);
        end;

        if Comp.Components[k].ComponentCount > 0 then
          Result := GetChildPopup(Comp.Components[k]);
        if Assigned(Result) then
          Exit;
      end;
    end;
  end;

  function GetMenuItem(AMenu: TMenuItem): TMenuItem;
  var
    LMenuItem: TMenuItem;
    i: integer;
  begin
    if (AMenu.Handle = FMenu) then
      Result := AMenu
    else
    begin
      Result := nil;
      i := 0;
      while (Result = nil) and (i < AMenu.Count) do
      begin
        LMenuItem := AMenu.Items[i];
        Result := GetMenuItem(LMenuItem);
        inc(i);
      end;
    end;
  end;

begin
  Result := nil;

  for i := 0 to PopupList.Count - 1 do
    if TPopupMenu(PopupList.Items[i]).Handle = FMenu then
      Exit(TPopupMenu(PopupList.Items[i]).Items);

  for i := 0 to Screen.FormCount - 1 do
  begin
    LForm := Screen.Forms[i];
    for j := 0 to LForm.ComponentCount - 1 do
    begin

      if LForm.Components[j] is TMenuItem then
      begin
        LMenuItem := TMenuItem(LForm.Components[j]);
        Result := GetMenuItem(LMenuItem);
        if Assigned(Result) then
          Exit;
      end
      else if LForm.Components[j] is TPopupMenu then
      begin
        LPopupMenu := TPopupMenu(LForm.Components[j]);
        if LPopupMenu.Handle = FMenu then
          Exit(LPopupMenu.Items);

        Result := GetMenuItem(LPopupMenu.Items);
        if Assigned(Result) then
          Exit;
      end
      else
      begin
        Result := GetChildPopup(LForm.Components[j]);
        if Assigned(Result) then
          Exit;
      end;
    end;
  end;
end;

function TSysPopupStyleHook.TSysPopupItem.GetVCLMenuItemsFast: TMenuItem;
begin
  if Assigned(FSysPopupStyleHook.FVCLMenuItems) then
    Result := FSysPopupStyleHook.FVCLMenuItems
  else
  begin
    Result := GetVCLMenuItems;
    FSysPopupStyleHook.FVCLMenuItems := Result;
  end;
end;

function TSysPopupStyleHook.TSysPopupItem.IsItemDisabled: Boolean;
var
  pMenuItemInfo: TMenuItemInfo;
begin
  Result := False;
  FillChar(pMenuItemInfo, sizeof(pMenuItemInfo), Char(0));
  pMenuItemInfo.cbSize := sizeof(TMenuItemInfo);
  pMenuItemInfo.fMask := MIIM_STATE;
  if GetMenuItemInfo(FMenu, FIndex, True, pMenuItemInfo) then
    Result := (pMenuItemInfo.fState and MFS_DISABLED = MFS_DISABLED) or (pMenuItemInfo.fState and MF_DISABLED = MF_DISABLED) or
      (pMenuItemInfo.fState and MF_GRAYED = MF_GRAYED);
end;

function TSysPopupStyleHook.TSysPopupItem.IsItemOwnerDraw: Boolean;
var
  pMenuItemInfo: TMenuItemInfo;
begin
  Result := False;
  FillChar(pMenuItemInfo, sizeof(MENUITEMINFO), Char(0));
  pMenuItemInfo.cbSize := sizeof(MENUITEMINFO);
  pMenuItemInfo.fMask := MIIM_FTYPE;
  pMenuItemInfo.dwTypeData := nil;
  if GetMenuItemInfo(FMenu, FIndex, True, pMenuItemInfo) then
    Result := (pMenuItemInfo.fType and MFT_OWNERDRAW = MFT_OWNERDRAW);
end;

function TSysPopupStyleHook.TSysPopupItem.IsItemRadioCheck: Boolean;
var
  pMenuItemInfo: TMenuItemInfo;
begin
  Result := False;
  FillChar(pMenuItemInfo, sizeof(pMenuItemInfo), Char(0));
  pMenuItemInfo.cbSize := sizeof(TMenuItemInfo);
  pMenuItemInfo.fMask := MIIM_FTYPE;
  if GetMenuItemInfo(FMenu, FIndex, True, pMenuItemInfo) then
    Result := (pMenuItemInfo.fType and MFT_RADIOCHECK) = MFT_RADIOCHECK;
end;

function TSysPopupStyleHook.TSysPopupItem.IsItemChecked: Boolean;
var
  pMenuItemInfo: TMenuItemInfo;
begin
  Result := False;
  FillChar(pMenuItemInfo, sizeof(pMenuItemInfo), Char(0));
  pMenuItemInfo.cbSize := sizeof(TMenuItemInfo);
  pMenuItemInfo.fMask := MIIM_STATE;
  if GetMenuItemInfo(FMenu, FIndex, True, pMenuItemInfo) then
    Result := (pMenuItemInfo.fState and MFS_CHECKED) = MFS_CHECKED;
end;

function TSysPopupStyleHook.TSysPopupItem.IsItemContainsSubMenu: Boolean;
begin
  Result := (GetSubMenu(FMenu, FIndex) > 0);
end;

function TSysPopupStyleHook.TSysPopupItem.IsItemDefault: Boolean;
var
  pMenuItemInfo: TMenuItemInfo;
begin
  Result := False;
  FillChar(pMenuItemInfo, sizeof(pMenuItemInfo), Char(0));
  pMenuItemInfo.cbSize := sizeof(TMenuItemInfo);
  pMenuItemInfo.fMask := MIIM_STATE;
  if GetMenuItemInfo(FMenu, FIndex, True, pMenuItemInfo) then
    Result := (pMenuItemInfo.fState and MFS_DEFAULT) = MFS_DEFAULT;
end;

function TSysPopupStyleHook.TSysPopupItem.IsItemSeparator: Boolean;
var
  pMenuItemInfo: TMenuItemInfo;
begin
  FillChar(pMenuItemInfo, sizeof(pMenuItemInfo), Char(0));
  pMenuItemInfo.cbSize := sizeof(TMenuItemInfo);
  pMenuItemInfo.fMask := MIIM_FTYPE;
  Result := False;
  if (FIndex > -1) and (FIndex < GetMenuItemCount(FMenu) - 1) then
  begin
    if GetMenuItemInfo(FMenu, FIndex, True, pMenuItemInfo) then
      Result := (pMenuItemInfo.fType and MFT_SEPARATOR) = MFT_SEPARATOR;
  end;
end;

initialization

SubMenuItemInfoArray := nil;

{$IFDEF UseVCLStyleUtilsMenu}
{$IF CompilerVersion >= 27} // Disable XE6-XE7 menu syshooks
TStyleManager.SystemHooks := TStyleManager.SystemHooks - [shMenus];
{$IFEND CompilerVersion}
if StyleServices.Available then
  TSysStyleManager.RegisterSysStyleHook('#32768', TSysPopupStyleHook);
{$ENDIF UseVCLStyleUtilsMenu}

finalization

{$IFDEF UseVCLStyleUtilsMenu}
  TSysStyleManager.UnRegisterSysStyleHook('#32768', TSysPopupStyleHook);
{$ENDIF UseVCLStyleUtilsMenu}

end.
