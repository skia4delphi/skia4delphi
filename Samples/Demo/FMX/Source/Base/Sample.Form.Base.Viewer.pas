{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2021-2024 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by the MIT license that can be     }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Sample.Form.Base.Viewer;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.TypInfo,
  System.Math, System.Math.Vectors, System.Rtti, FMX.Types, FMX.Graphics,
  FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Layouts, FMX.Objects, FMX.ListBox,
  FMX.Controls.Presentation,

  { Skia }
  System.Skia, FMX.Skia,

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
      end;
  strict private
    FControl: TLayout;
    FHiddenItems: TArray<TItemData>;
    FItems: TArray<TItem>;
    FOnChange: TNotifyEvent;
    procedure AddItem(const AName: string; const ACurrentValue: TValue; const ASelectControl: TControl; const AUpdateSelectControl: TUpdateSelectControlProc);
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
    rctBackgroundChess: TRectangle;
    pbxBackgroundChess: TSkPaintBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure pbxBackgroundChessDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
  private
    class var
      FChessImage: ISkImage;
      FChessPaint: ISkPaint;
  private
    FBackgroundKind: TBackgroundKind;
    FOnOptionsChange: TProc;
    FOptions: IViewerOptions;
    procedure OptionsChange(ASender: TObject);
    procedure SetBackgroundKind(const AValue: TBackgroundKind);
    procedure SetOptions(const AOptions: IViewerOptions);
  protected
    procedure DoSetOptions(var AOptions: IViewerOptions); virtual;
    function HasBottomContent: Boolean; override;
    procedure OptionsChanged; virtual;
    procedure Show(const ATitle, ADescription: string); reintroduce;
    property BackgroundKind: TBackgroundKind read FBackgroundKind write SetBackgroundKind;
    property Options: IViewerOptions read FOptions write SetOptions;
    property OnOptionsChange: TProc read FOnOptionsChange write FOnOptionsChange;
  end;

implementation

{$R *.fmx}

{ TViewerOptions }

procedure TViewerOptions.AddBoolean(const AName: string;
  const ACurrentValue: Boolean);
var
  LSwitch: TSwitch;
begin
  LSwitch := TSwitch.Create(FControl);
  LSwitch.Align := TAlignLayout.Left;
  LSwitch.Cursor := crHandPoint;
  LSwitch.IsChecked := ACurrentValue;
  LSwitch.Margins.Rect := RectF(0, 8, 0, 8);
  LSwitch.Width := 78;
  LSwitch.OnClick := OnSwitchClick;
  AddItem(AName, ACurrentValue, LSwitch,
    procedure(const AControl: TControl; const AValue: TValue)
    var
      LSwitch: TSwitch absolute AControl;
    begin
      LSwitch.OnClick := nil;
      try
        LSwitch.IsChecked := AValue.AsBoolean;
      finally
        LSwitch.OnClick := OnSwitchClick;
      end;
    end);
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
  const AUpdateSelectControl: TUpdateSelectControlProc);
const
  HorizontalMargin = 20;
  ItemHeight = 44;
  LabelStyleLookup = 'sklabelstyle_options';
  LineColor = $FFECE8E6;
var
  LItem: TItem;
  LLabel: TSkLabel;
  LRectangle: TRectangle;
begin
  LRectangle := TRectangle.Create(FControl);
  LRectangle.Align := TAlignLayout.Top;
  LRectangle.Fill.Color := TfrmBaseViewer.FormBorderColor;
  LRectangle.Sides := [TSide.Top];
  LRectangle.Stroke.Color := LineColor;
  LRectangle.Height := ItemHeight;
  LRectangle.Width := FControl.Width;

  LLabel := TSkLabel.Create(LRectangle);
  LLabel.TextSettings.MaxLines := 1;
  LLabel.Align := TAlignLayout.MostLeft;
  LLabel.Margins.Left := HorizontalMargin;
  LLabel.StyleLookup := LabelStyleLookup;
  LLabel.Text := AName;
  LLabel.Parent := LRectangle;
  {$IF CompilerVersion >= 32}
  LLabel.OnResized := ControlResized;
  {$ELSE}
  LLabel.OnResize := ControlResized;
  {$ENDIF}

  ASelectControl.Margins.Left := HorizontalMargin;
  ASelectControl.Margins.Right := HorizontalMargin;
  ASelectControl.Tag := Length(FItems);
  ASelectControl.Parent := LRectangle;

  LRectangle.Position.Y := FControl.Height;
  LRectangle.Parent := FControl;

  LItem.Data.Name := AName;
  LItem.Data.Value := ACurrentValue;
  LItem.LabelControl := LLabel;
  LItem.SelectControl := ASelectControl;
  LItem.UpdateSelectControl := AUpdateSelectControl;
  FItems := FItems + [LItem];
  FControl.Height := FControl.Height + LRectangle.Height;
end;

procedure TViewerOptions.AddStrings(const AName: string;
  const AStrings: TArray<string>; const ACurrentIndexValue: Integer);
var
  LComboBox: TComboBox;
begin
  LComboBox := TComboBox.Create(FControl);
  LComboBox.Align := TAlignLayout.Client;
  LComboBox.Margins.Rect := RectF(0, 8, 0, 8);
  LComboBox.Cursor := crHandPoint;
  LComboBox.Items.AddStrings(AStrings);
  LComboBox.ItemIndex := ACurrentIndexValue;
  LComboBox.OnChange := OnComboBoxChange;
  AddItem(AName, ACurrentIndexValue, LComboBox,
    procedure(const AControl: TControl; const AValue: TValue)
    var
      LComboBox: TComboBox absolute AControl;
    begin
      LComboBox.OnClick := nil;
      try
        LComboBox.ItemIndex := AValue.AsOrdinal;
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
  LValue := EnsureRange(ACurrentValue, AMinValue, AMaxValue);
  LTrackBar := TTrackBar.Create(FControl);
  LTrackBar.Align := TAlignLayout.Client;
  LTrackBar.Cursor := crHandPoint;
  LTrackBar.Min := AMinValue;
  LTrackBar.Max := AMaxValue;
  LTrackBar.Value := LValue;
  LTrackBar.Frequency := AFrequency;
  LTrackBar.OnChange := OnTrackBarChange;
  AddItem(AName, LValue, LTrackBar,
    procedure(const AControl: TControl; const AValue: TValue)
    var
      LTrackBar: TTrackBar absolute AControl;
    begin
      LTrackBar.OnChange := nil;
      try
        LTrackBar.Value := EnsureRange(AValue.AsExtended, AMinValue, AMaxValue);
      finally
        LTrackBar.OnChange := OnTrackBarChange;
      end;
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
    FItems[I].LabelControl.Margins.Right := Max(LMaxTextWidth - FItems[I].LabelControl.Width, 0);
end;

constructor TViewerOptions.Create;
begin
  inherited Create;
  FControl := TLayout.Create(nil);
  FControl.Height := 0;
  {$IF CompilerVersion >= 32}
  FControl.OnResized := ControlResized;
  {$ELSE}
  FControl.OnResize := ControlResized;
  {$ENDIF}
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
  LComboBox: TComboBox absolute ASender;
begin
  FItems[LComboBox.Tag].Data.Value := LComboBox.ItemIndex;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TViewerOptions.OnSwitchClick(ASender: TObject);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Queue(nil,
        procedure
        var
          LSwitch: TSwitch absolute ASender;
        begin
          FItems[LSwitch.Tag].Data.Value := LSwitch.IsChecked;
          if Assigned(FOnChange) then
            FOnChange(Self);
        end);
    end).Start;
end;

procedure TViewerOptions.OnTrackBarChange(ASender: TObject);
var
  LTrackBar: TTrackBar absolute ASender;
begin
  if SameValue(LTrackBar.Frequency, 1, TEpsilon.Position) then
    FItems[LTrackBar.Tag].Data.Value := Round(LTrackBar.Value)
  else
    FItems[LTrackBar.Tag].Data.Value := LTrackBar.Value;
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

procedure TfrmBaseViewer.DoSetOptions(var AOptions: IViewerOptions);
begin
  if Assigned(AOptions) then
  begin
    AOptions.Control.Align := TAlignLayout.Bottom;
    AOptions.Control.Parent := rctContent;
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

function TfrmBaseViewer.HasBottomContent: Boolean;
begin
  Result := inherited or Assigned(FOptions);
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

procedure TfrmBaseViewer.pbxBackgroundChessDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);

  function CreateChessPaint: ISkPaint;
  begin
    FChessImage := rctBackgroundChess.Fill.Bitmap.Bitmap.ToSkImage;
    Result := TSkPaint.Create;
    Result.Shader := FChessImage.MakeShader(TSkTileMode.Repeat, TSkTileMode.Repeat);
  end;

begin
  if FChessPaint = nil then
    FChessPaint := CreateChessPaint;
  ACanvas.DrawRect(ACanvas.GetLocalClipBounds, FChessPaint);
end;

procedure TfrmBaseViewer.SetBackgroundKind(const AValue: TBackgroundKind);
begin
  FBackgroundKind := AValue;
  // Just avoinding a know bug of TCanvasGpu drawing tile bitmap
  if TCanvasManager.DefaultCanvas.ClassName = 'TCanvasGpu' then
  begin
    pbxBackgroundChess.SendToBack;
    pbxBackgroundChess.Visible := FBackgroundKind = TBackgroundKind.Chess;
  end
  else
  begin
    rctBackgroundChess.SendToBack;
    rctBackgroundChess.Visible := FBackgroundKind = TBackgroundKind.Chess;
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
  lblTitle.Text := ATitle;
  lblTipDescription.Text := ADescription;
  BackgroundKind := FBackgroundKind;
  inherited Show;
end;

end.
