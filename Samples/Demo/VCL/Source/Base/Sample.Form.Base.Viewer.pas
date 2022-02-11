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
unit Sample.Form.Base.Viewer;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  Winapi.Windows, System.SysUtils, System.Classes, System.Types, System.UITypes,
  System.Math, System.Math.Vectors, System.TypInfo, System.Rtti, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,

  { Skia }
  Skia, Skia.Vcl,

  { Sample }
  Sample.Form.Base;

type
  TBackgroundKind = (Chess, Solid);

  { IViewerOptions }

  IViewerOptions = interface
    procedure AddBoolean(const AName: string; const ACurrentValue: Boolean);
    procedure AddEnum(const AName: string; const ATypeInfo: PTypeInfo; const ACurrentValue: Variant);
    procedure AddFloat(const AName: string; const AMinValue, AMaxValue, ACurrentValue: Double; const AFrequencyValue: Double = 0);
    procedure AddInteger(const AName: string; const AMinValue, AMaxValue, ACurrentValue: Integer);
    procedure AddStrings(const AName: string; const AStrings: TArray<string>; const ACurrentIndexValue: Integer);
    function GetControl: TControl;
    function GetValue(const AName: string): TValue;
    function GetVariant(const AName: string): Variant;
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetValue(const AName: string; const AValue: TValue);
    procedure SetVariant(const AName: string; const AValue: Variant);
    property Control: TControl read GetControl;
    property Value[const AName: string]: TValue read GetValue write SetValue;
    property Variant[const AName: string]: Variant read GetVariant write SetVariant; default;
  end;

  { TViewerOptions }

  TViewerOptions = class(TInterfacedObject, IViewerOptions)
  strict private
    type
      TUpdateSelectControlProc = reference to procedure(const AControl: TControl; const AValue: TValue);

      TItemData = record
        Name: string;
        Value: TValue;
      end;

      TItem = record
        Data: TItemData;
        LabelControl: TControl;
        SelectControl: TControl;
        UpdateSelectControl: TUpdateSelectControlProc;
        TrackBarPositionToValue: TFunc<Integer, TValue>;
      end;
  strict private
    FControl: TPanel;
    FHiddenItems: TArray<TItemData>;
    FItems: TArray<TItem>;
    FOnChange: TNotifyEvent;
    procedure AddItem(const AName: string; const ACurrentValue: TValue; const ASelectControl: TControl; const AUpdateSelectControl: TUpdateSelectControlProc; const ATrackBarPositionToValue: TFunc<Integer, TValue> = nil);
    procedure AddTrackField(const AName: string; const AMinValue, AMaxValue, ACurrentValue, AFrequency: Double);
    procedure ControlResized(ASender: TObject);
    function GetControl: TControl;
    function GetValue(const AName: string): TValue;
    function GetVariant(const AName: string): Variant;
    procedure OnComboBoxChange(ASender: TObject);
    procedure OnSwitchClick(ASender: TObject);
    procedure OnTrackBarChange(ASender: TObject);
    procedure SetValue(const AName: string; const AValue: TValue);
    procedure SetVariant(const AName: string; const AValue: Variant);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddBoolean(const AName: string; const ACurrentValue: Boolean);
    procedure AddEnum(const AName: string; const ATypeInfo: PTypeInfo; const ACurrentValue: Variant);
    procedure AddFloat(const AName: string; const AMinValue, AMaxValue, ACurrentValue: Double; const AFrequencyValue: Double = 0);
    procedure AddInteger(const AName: string; const AMinValue, AMaxValue, ACurrentValue: Integer);
    procedure AddStrings(const AName: string; const AStrings: TArray<string>; const ACurrentIndexValue: Integer);
    procedure SetOnChange(const AValue: TNotifyEvent);
    property Control: TControl read GetControl;
    property Value[const AName: string]: TValue read GetValue write SetValue;
    property Variant[const AName: string]: Variant read GetVariant write SetVariant; default;
  end;

  { TfrmBaseViewer }

  TfrmBaseViewer = class(TfrmBase)
    procedure pnlContentResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private
    class var
      FChessBitmap: TBitmap;
      FChessEffect: ISkRuntimeEffect;
      FChessPaint: ISkPaint;
    class destructor Destroy;
  private
    FBackgroundKind: TBackgroundKind;
    FOnOptionsChange: TProc;
    FOptions: IViewerOptions;
    procedure OptionsChange(ASender: TObject);
    procedure SetOptions(const AOptions: IViewerOptions);
  protected
    procedure DoSetOptions(var AOptions: IViewerOptions); virtual;
    procedure OptionsChanged; virtual;
    procedure ScrollBoxEraseBackground(ASender: TObject; const ADC: HDC); override;
    procedure Show(const ATitle, ADescription: string); reintroduce;
    property BackgroundKind: TBackgroundKind read FBackgroundKind write FBackgroundKind;
    property Options: IViewerOptions read FOptions write SetOptions;
    property OnOptionsChange: TProc read FOnOptionsChange write FOnOptionsChange;
  end;

implementation

{$IF CompilerVersion >= 30}
uses
  { Delphi }
  Vcl.WinXCtrls;
{$ENDIF}

{$R *.dfm}

type
  { TComboBoxSafe }

  TComboBoxSafe = class(TComboBox)
  strict private
    FSafeItemIndex: Integer;
    FSafeItems: TArray<string>;
    function GetSafeItemIndex: Integer;
    function GetSafeItems: TArray<string>;
    procedure SetSafeItemIndex(const AValue: Integer);
    procedure SetSafeItems(const AValue: TArray<string>);
  strict protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    property SafeItemIndex: Integer read GetSafeItemIndex write SetSafeItemIndex;
    property SafeItems: TArray<string> read GetSafeItems write SetSafeItems;
  end;

{ TViewerOptions }

procedure TViewerOptions.AddBoolean(const AName: string;
  const ACurrentValue: Boolean);
{$IF CompilerVersion >= 30}
const
  SwitchState: array[Boolean] of TToggleSwitchState = (TToggleSwitchState.tssOff, TToggleSwitchState.tssOn);
var
  LSwitch: TToggleSwitch;
begin
  LSwitch := TToggleSwitch.Create(FControl);
  LSwitch.Align := alLeft;
  LSwitch.Cursor := crHandPoint;
  LSwitch.ShowStateCaption := False;
  LSwitch.State := SwitchState[ACurrentValue];
  LSwitch.Margins.SetBounds(0, 8, 0, 8);
  LSwitch.AlignWithMargins := True;
  LSwitch.OnClick := OnSwitchClick;
  AddItem(AName, ACurrentValue, LSwitch,
    procedure(const AControl: TControl; const AValue: TValue)
    var
      LSwitch: TToggleSwitch absolute AControl;
    begin
      LSwitch.OnClick := nil;
      try
        LSwitch.State := SwitchState[AValue.AsBoolean];
      finally
        LSwitch.OnClick := OnSwitchClick;
      end;
    end);
{$ELSE}
const
  CheckBoxState: array[Boolean] of TCheckBoxState = (TCheckBoxState.cbUnchecked, TCheckBoxState.cbChecked);
var
  LCheckBox: TCheckBox;
begin
  LCheckBox := TCheckBox.Create(FControl);
  LCheckBox.Align := alLeft;
  LCheckBox.Cursor := crHandPoint;
  LCheckBox.State := CheckBoxState[ACurrentValue];
  LCheckBox.Margins.SetBounds(0, 8, 0, 8);
  LCheckBox.AlignWithMargins := True;
  LCheckBox.OnClick := OnSwitchClick;
  AddItem(AName, ACurrentValue, LCheckBox,
    procedure(const AControl: TControl; const AValue: TValue)
    var
      LCheckBox: TCheckBox absolute AControl;
    begin
      LCheckBox.OnClick := nil;
      try
        LCheckBox.State := CheckBoxState[AValue.AsBoolean];
      finally
        LCheckBox.OnClick := OnSwitchClick;
      end;
    end);
{$ENDIF}
end;

procedure TViewerOptions.AddEnum(const AName: string;
  const ATypeInfo: PTypeInfo; const ACurrentValue: Variant);
var
  LStrings: TArray<string>;
  LEnumValue: Integer;
begin
  if (ATypeInfo.Kind <> tkEnumeration) then
    raise Exception.Create('Invalid type');
  LStrings := [];
  for LEnumValue := ATypeInfo.TypeData.MinValue to ATypeInfo.TypeData.MaxValue do
    LStrings := LStrings + [GetEnumName(ATypeInfo, LEnumValue)];
  AddStrings(AName, LStrings, Integer(ACurrentValue) - ATypeInfo.TypeData.MinValue);
end;

procedure TViewerOptions.AddFloat(const AName: string; const AMinValue,
  AMaxValue, ACurrentValue, AFrequencyValue: Double);
begin
  AddTrackField(AName, AMinValue, AMaxValue, ACurrentValue, AFrequencyValue);
end;

procedure TViewerOptions.AddInteger(const AName: string; const AMinValue,
  AMaxValue, ACurrentValue: Integer);
begin
  AddTrackField(AName, AMinValue, AMaxValue, ACurrentValue, 1);
end;

procedure TViewerOptions.AddItem(const AName: string;
  const ACurrentValue: TValue; const ASelectControl: TControl;
  const AUpdateSelectControl: TUpdateSelectControlProc;
  const ATrackBarPositionToValue: TFunc<Integer, TValue>);
const
  HorizontalMargin = 20;
  ItemHeight = 44;
  LineColor = $00ECE8E6;
var
  LItem: TItem;
  LLabel: TSkLabel;
  LPanel: TPanel;
  LLine: TPanel;
begin
  LPanel := TPanel.Create(FControl);
  LPanel.Align := alTop;
  LPanel.BevelOuter := TPanelBevel.bvNone;
  LPanel.ShowCaption := False;
  LPanel.Color := TfrmBaseViewer.FormBorderColor;
  LPanel.StyleElements := LPanel.StyleElements - [seClient];
  LPanel.Height := ItemHeight;
  LPanel.Width := FControl.Width;

  LLine := TPanel.Create(LPanel);
  LLine.Align := alTop;
  LLine.BevelOuter := TPanelBevel.bvNone;
  LLine.ShowCaption := False;
  LLine.Color := LineColor;
  LLine.StyleElements := LLine.StyleElements - [seClient];
  LLine.Height := 1;
  LLine.Parent := LPanel;

  LLabel := TSkLabel.Create(LPanel);
  LLabel.TextSettings.MaxLines := 1;
  LLabel.Align := alLeft;
  LLabel.Margins.SetBounds(HorizontalMargin, 0, 0, 0);
  LLabel.AlignWithMargins := True;
  LLabel.TextSettings.Font.Size := 11;
  LLabel.TextSettings.Font.Weight := TSkFontComponent.TSkFontWeight.Medium;
  LLabel.TextSettings.FontColor := $FF9FA5A8;
  LLabel.Caption := AName;
  LLabel.Parent := LPanel;
  LLabel.OnResize := ControlResized;

  ASelectControl.Margins.Left := HorizontalMargin;
  ASelectControl.Margins.Right := HorizontalMargin;
  ASelectControl.Tag := Length(FItems);
  ASelectControl.Parent := LPanel;

  LPanel.Top := FControl.Height;
  LPanel.Parent := FControl;

  LItem.Data.Name := AName;
  LItem.Data.Value := ACurrentValue;
  LItem.LabelControl := LLabel;
  LItem.SelectControl := ASelectControl;
  LItem.UpdateSelectControl := AUpdateSelectControl;
  LItem.TrackBarPositionToValue := ATrackBarPositionToValue;
  FItems := FItems + [LItem];
  FControl.Height := FControl.Height + LPanel.Height;
end;

procedure TViewerOptions.AddStrings(const AName: string;
  const AStrings: TArray<string>; const ACurrentIndexValue: Integer);
var
  LComboBox: TComboBoxSafe;
begin
  LComboBox := TComboBoxSafe.Create(FControl);
  LComboBox.Align := alClient;
  LComboBox.Style := TComboBoxStyle.csDropDownList;
  LComboBox.Margins.SetBounds(0, 8, 0, 8);
  LComboBox.AlignWithMargins := True;
  LComboBox.Cursor := crHandPoint;
  LComboBox.SafeItems := AStrings;
  LComboBox.SafeItemIndex := ACurrentIndexValue;
  LComboBox.OnChange := OnComboBoxChange;
  AddItem(AName, ACurrentIndexValue, LComboBox,
    procedure(const AControl: TControl; const AValue: TValue)
    var
      LComboBox: TComboBoxSafe absolute AControl;
    begin
      LComboBox.OnClick := nil;
      try
        LComboBox.SafeItemIndex := AValue.AsOrdinal;
      finally
        LComboBox.OnClick := OnComboBoxChange;
      end;
    end);
end;

procedure TViewerOptions.AddTrackField(const AName: string; const AMinValue,
  AMaxValue, ACurrentValue, AFrequency: Double);
var
  LTrackBar: TTrackBar;
  LValue: Double;
begin
  LValue := Min(Max(ACurrentValue, AMinValue), AMaxValue);
  LTrackBar := TTrackBar.Create(FControl);
  LTrackBar.Align := alClient;
  LTrackBar.Margins.SetBounds(0, 10, 0, 11);
  LTrackBar.AlignWithMargins := True;
  LTrackBar.Cursor := crHandPoint;
  LTrackBar.Min := 0;
  if SameValue(AFrequency, 0, TEpsilon.Matrix) then
    LTrackBar.Max := 200
  else
    LTrackBar.Max := Round((AMaxValue - AMinValue) / AFrequency);
  LTrackBar.Position := Round(((LValue - AMinValue) / (AMaxValue - AMinValue)) * LTrackBar.Max);
  LTrackBar.Frequency := 1;
  LTrackBar.TickStyle := TTickStyle.tsNone;
  LTrackBar.OnChange := OnTrackBarChange;
  AddItem(AName, LValue, LTrackBar,
    procedure(const AControl: TControl; const AValue: TValue)
    var
      LTrackBar: TTrackBar absolute AControl;
      LValue: Double;
    begin
      LTrackBar.OnChange := nil;
      try
        LValue := Min(Max(AValue.AsOrdinal, AMinValue), AMaxValue);
        LTrackBar.Position := Round(((LValue - AMinValue) / (AMaxValue - AMinValue)) * LTrackBar.Max);
      finally
        LTrackBar.OnChange := OnTrackBarChange;
      end;
    end,
    function(APosition: Integer): TValue
    var
      LRealPosition: Double;
    begin
      LRealPosition := ((APosition / LTrackBar.Max) * (AMaxValue - AMinValue)) + AMinValue;
      if SameValue(AFrequency, 1, TEpsilon.Position) then
        Result := Round(LRealPosition)
      else
        Result := LRealPosition;
    end);
end;

procedure TViewerOptions.ControlResized(ASender: TObject);
var
  I: Integer;
  LMaxTextWidth: Single;
begin
  LMaxTextWidth := 0;
  for I := 0 to Length(FItems) - 1 do
    LMaxTextWidth := Max(FItems[I].LabelControl.Width, LMaxTextWidth);
  for I := 0 to Length(FItems) - 1 do
    FItems[I].LabelControl.Margins.Right := Round(Max(LMaxTextWidth - FItems[I].LabelControl.Width, 0));
end;

constructor TViewerOptions.Create;
begin
  inherited Create;
  FControl := TPanel.Create(nil);
  FControl.BevelOuter := TPanelBevel.bvNone;
  FControl.ShowCaption := False;
  FControl.Color := TfrmBaseViewer.FormBorderColor;
  FControl.StyleElements := FControl.StyleElements - [seClient];
  FControl.Height := 0;
  FControl.OnResize := ControlResized;
end;

destructor TViewerOptions.Destroy;
begin
  FControl.Free;
  inherited;
end;

function TViewerOptions.GetControl: TControl;
begin
  Result := FControl;
end;

function TViewerOptions.GetValue(const AName: string): TValue;
var
  I: Integer;
begin
  for I := 0 to Length(FItems) - 1 do
    if SameText(FItems[I].Data.Name, AName) then
      Exit(FItems[I].Data.Value);
  for I := 0 to Length(FHiddenItems) - 1 do
    if SameText(FHiddenItems[I].Name, AName) then
      Exit(FHiddenItems[I].Value);
  Result := TValue.Empty;
end;

function TViewerOptions.GetVariant(const AName: string): Variant;
begin
  Result := Value[AName].AsVariant;
end;

procedure TViewerOptions.OnComboBoxChange(ASender: TObject);
var
  LComboBox: TComboBoxSafe absolute ASender;
begin
  FItems[LComboBox.Tag].Data.Value := LComboBox.SafeItemIndex;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TViewerOptions.OnSwitchClick(ASender: TObject);
{$IF CompilerVersion >= 30}
var
  LSwitch: TToggleSwitch absolute ASender;
begin
  FItems[LSwitch.Tag].Data.Value := LSwitch.State = TToggleSwitchState.tssOn;
{$ELSE}
var
  LCheckBox: TCheckBox absolute ASender;
begin
  FItems[LCheckBox.Tag].Data.Value := LCheckBox.State = TCheckBoxState.cbChecked;
{$ENDIF}
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TViewerOptions.OnTrackBarChange(ASender: TObject);
var
  LTrackBar: TTrackBar absolute ASender;
begin
  FItems[LTrackBar.Tag].Data.Value := FItems[LTrackBar.Tag].TrackBarPositionToValue(LTrackBar.Position);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TViewerOptions.SetOnChange(const AValue: TNotifyEvent);
begin
  FOnChange := AValue;
end;

procedure TViewerOptions.SetValue(const AName: string; const AValue: TValue);
var
  I: Integer;
begin
  for I := 0 to Length(FItems) - 1 do
  begin
    if SameText(FItems[I].Data.Name, AName) then
    begin
      FItems[I].Data.Value := AValue;
      FItems[I].UpdateSelectControl(FItems[I].SelectControl, AValue);
      Exit;
    end;
  end;
  for I := 0 to Length(FHiddenItems) - 1 do
  begin
    if SameText(FHiddenItems[I].Name, AName) then
    begin
      FHiddenItems[I].Value := AValue;
      Exit;
    end;
  end;
  SetLength(FHiddenItems, Length(FHiddenItems) + 1);
  FHiddenItems[High(FHiddenItems)].Name := AName;
  FHiddenItems[High(FHiddenItems)].Value := AValue;
end;

procedure TViewerOptions.SetVariant(const AName: string; const AValue: Variant);
begin
  Value[AName] := TValue.FromVariant(AValue);
end;

{ TfrmBaseViewer }

class destructor TfrmBaseViewer.Destroy;
begin
  FChessBitmap.Free;
end;

procedure TfrmBaseViewer.DoSetOptions(var AOptions: IViewerOptions);
begin
  if Assigned(AOptions) then
  begin
    AOptions.Control.Align := alBottom;
    AOptions.Control.Parent := pnlContent;
    pnlTip.Top := AOptions.Control.BoundsRect.Bottom;
  end;
end;

procedure TfrmBaseViewer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if Action <> TCloseAction.caNone then
  begin
    FOnOptionsChange := nil;
    Options := nil;
  end;
end;

procedure TfrmBaseViewer.FormDestroy(Sender: TObject);
begin
  Options := nil;
  inherited;
end;

procedure TfrmBaseViewer.OptionsChange(ASender: TObject);
begin
  OptionsChanged;
end;

procedure TfrmBaseViewer.OptionsChanged;
begin
  if Assigned(FOnOptionsChange) then
    FOnOptionsChange();
end;

procedure TfrmBaseViewer.pnlContentResize(Sender: TObject);
begin
  inherited;
  sbxContent.Invalidate;
end;

// A fast way to draw the chess background
procedure TfrmBaseViewer.ScrollBoxEraseBackground(ASender: TObject; const ADC: HDC);

  procedure CreateChessPaint;
  begin
    FChessEffect := TSkRuntimeEffect.MakeForShader(
      'uniform float4 iOddSquareColor, iEvenSquareColor;' + sLineBreak +
      'uniform float2 iSquareSize;' + sLineBreak +
      'half4 main(float2 fragCoord) {' + sLineBreak +
      '  bool p = mod(fragCoord.x/iSquareSize.x, 2.0) < 1.0;' + sLineBreak +
      '  bool q = mod(fragCoord.y/iSquareSize.y, 2.0) > 1.0;' + sLineBreak +
      '  if ((p && q) || !(p || q))' + sLineBreak +
      '    return iOddSquareColor;' + sLineBreak +
      '  return iEvenSquareColor;}');
    FChessEffect.SetUniform('iOddSquareColor', TAlphaColorF.Create($FFCCCCCC));
    FChessEffect.SetUniform('iEvenSquareColor', TAlphaColorF.Create(TAlphaColors.White));
    FChessPaint := TSkPaint.Create;
    FChessPaint.Shader := FChessEffect.MakeShader(True);
  end;

  procedure UpdateChessBitmap;
  begin
    if FChessPaint = nil then
      CreateChessPaint;
    FChessEffect.SetUniform('iSquareSize', PointF(8, 8) {$IF CompilerVersion >= 33}* pnlContent.ScaleFactor{$ENDIF});
    if FChessBitmap = nil then
      FChessBitmap := TBitmap.Create;
    FChessBitmap.SetSize(Min(Screen.Width, Max(pnlContent.Width * 2, 300)), Min(Screen.Height, Max(pnlContent.Height * 2, 300)));
    FChessBitmap.SkiaDraw(
      procedure(const ACanvas: ISkCanvas)
      begin
        ACanvas.DrawPaint(FChessPaint);
      end);
  end;

var
  LScrollBox: TScrollBox absolute ASender;
  LRect: TRect;
  I: Integer;
begin
  case FBackgroundKind of
    TBackgroundKind.Chess:
      begin
        if (FChessBitmap = nil) or (FChessBitmap.Width < LScrollBox.Width) or (FChessBitmap.Height < LScrollBox.Height) then
          UpdateChessBitmap;
        if HWND(WindowFromDC(ADC)) = sbxContent.Handle then
        begin
          for I := 0 to LScrollBox.ControlCount-1 do
          begin
            LRect := LScrollBox.Controls[I].BoundsRect;
            ExcludeClipRect(ADC, LRect.Left, LRect.Top, LRect.Right, LRect.Bottom);
          end;
          BitBlt(ADC, 0, 0, LScrollBox.Width, LScrollBox.Height, FChessBitmap.Canvas.Handle, 0, 0, SrcCopy);
          SelectClipRgn(ADC, HRGN(nil));
        end
        else
          BitBlt(ADC, 0, 0, LScrollBox.Width, LScrollBox.Height, FChessBitmap.Canvas.Handle, 0, 0, SrcCopy);
      end;
    TBackgroundKind.Solid: inherited;
  end;
end;

procedure TfrmBaseViewer.SetOptions(const AOptions: IViewerOptions);
begin
  if TObject(FOptions) <> TObject(AOptions) then
  begin
    if Assigned(FOptions) then
    begin
      FOptions.SetOnChange(nil);
      FOptions.Control.Parent := nil;
      FOptions := nil;
    end;
    FOptions := AOptions;
    DoSetOptions(FOptions);
    if Assigned(FOptions) then
      FOptions.SetOnChange(OptionsChange);
  end;
end;

procedure TfrmBaseViewer.Show(const ATitle, ADescription: string);
begin
  lblTitle.Caption := ATitle;
  lblTipDescription.Caption := ADescription;
  inherited Show;
end;

{ TComboBoxSafe }

constructor TComboBoxSafe.Create(AOwner: TComponent);
begin
  inherited;
  FSafeItemIndex := -1;
end;

procedure TComboBoxSafe.CreateWnd;
var
  LOnChange: TNotifyEvent;
begin
  inherited;
  LOnChange := OnChange;
  OnChange := nil;
  try
    Items.Text := string.Join(sLineBreak, FSafeItems);
    ItemIndex := FSafeItemIndex;
  finally
    OnChange := LOnChange;
  end;
end;

procedure TComboBoxSafe.DestroyWnd;
begin
  FSafeItemIndex := ItemIndex;
  FSafeItems := Items.ToStringArray;
  inherited;
end;

function TComboBoxSafe.GetSafeItemIndex: Integer;
begin
  if WindowHandle <> 0 then
    Result := ItemIndex
  else
    Result := FSafeItemIndex;
end;

function TComboBoxSafe.GetSafeItems: TArray<string>;
begin
  if WindowHandle <> 0 then
    Result := Items.ToStringArray
  else
    Result := FSafeItems;
end;

procedure TComboBoxSafe.SetSafeItemIndex(const AValue: Integer);
begin
  if WindowHandle <> 0 then
    ItemIndex := AValue
  else
    FSafeItemIndex := AValue;
end;

procedure TComboBoxSafe.SetSafeItems(const AValue: TArray<string>);
begin
  if WindowHandle <> 0 then
    Items.Text := string.Join(sLineBreak, AValue)
  else
    FSafeItems := AValue;
end;

end.
