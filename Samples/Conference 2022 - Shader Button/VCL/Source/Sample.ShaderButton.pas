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
unit Sample.ShaderButton;

interface

uses
  { Delphi }
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Math, System.Diagnostics, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  { Skia }
  System.Skia, Vcl.Skia;

type
  { TfrmShaderButton }

  TfrmShaderButton = class(TFrame)
    apbBackground: TSkAnimatedPaintBox;
    lblText: TSkLabel;
    procedure apbBackgroundAnimationDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF;
      const AProgress: Double; const AOpacity: Single);
    procedure FrameMouseDown(ASender: TObject; AButton: TMouseButton; AShift: TShiftState; AX, AY: Integer);
    procedure FrameMouseUp(ASender: TObject; AButton: TMouseButton; AShift: TShiftState; AX, AY: Integer);
  private const
    DefaultBorderThickness = 1.33333;
    DefaultCornerRadius = 11;
    DefaultLeftColor = $FF4488FE;
    DefaultRightColor = $FFDC6BD2;
  private
    FBorderThickness: Single;
    FCornerRadius: Single;
    FLeftColor: TAlphaColor;
    FPressed: Boolean;
    FPressedTime: TStopwatch;
    FRightColor: TAlphaColor;
    FShaderBuilder: ISkRuntimeShaderBuilder;
    function GetFontColor: TAlphaColor;
    function GetCaption: string;
    procedure SetBorderThickness(const AValue: Single);
    procedure SetCaption(const AValue: string);
    procedure SetCornerRadius(const AValue: Single);
    procedure SetFontColor(const AValue: TAlphaColor);
    procedure SetLeftColor(const AValue: TAlphaColor);
    procedure SetPressed(const AValue: Boolean);
    procedure SetRightColor(const AValue: TAlphaColor);
  public
    constructor Create(AOwner: TComponent); override;
    property Pressed: Boolean read FPressed write SetPressed;
  published
    property BorderThickness: Single read FBorderThickness write SetBorderThickness;
    property Caption: string read GetCaption write SetCaption;
    property CornerRadius: Single read FCornerRadius write SetCornerRadius;
    property FontColor: TAlphaColor read GetFontColor write SetFontColor;
    property LeftColor: TAlphaColor read FLeftColor write SetLeftColor default DefaultLeftColor;
    property RightColor: TAlphaColor read FRightColor write SetRightColor default DefaultRightColor;
  end;

implementation

{$R *.dfm}

uses
  { Delphi }
  System.Math.Vectors, System.IOUtils;

type
  TSkLabelAccess = class(TSkLabel)
  public
    procedure Render(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
  end;

{ TfrmShaderButton }

procedure TfrmShaderButton.apbBackgroundAnimationDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF;
  const AProgress: Double; const AOpacity: Single);

  function GetOpacityAnimation(const APressed: Boolean; const APressedTime: TStopwatch): Single; inline;
  begin
    if not APressedTime.IsRunning then
      Result := 1
    else if APressed then
      Result := 1 - Min(APressedTime.ElapsedMilliseconds, 50) / 50
    else
      Result := Min(APressedTime.ElapsedMilliseconds, 100) / 100;
  end;

var
  LOpacityAnimation: Single;
  LPaint: ISkPaint;
begin
  if FShaderBuilder <> nil then
  begin
    LOpacityAnimation := GetOpacityAnimation(FPressed, FPressedTime);
    FShaderBuilder.SetUniform('iResolution', [ADest.Width, ADest.Height, ACanvas.GetLocalToDeviceAs3x3.ExtractScale.X]);
    FShaderBuilder.SetUniform('iTime', apbBackground.Animation.CurrentTime);
    LPaint := TSkPaint.Create;
    LPaint.Shader := FShaderBuilder.MakeShader;
    LPaint.AlphaF := AOpacity * (0.9 + 0.1 * LOpacityAnimation);
    ACanvas.DrawRect(ADest, LPaint);
    // To avoid flicker, let's draw the text inside the animation control
    TSkLabelAccess(lblText).Render(ACanvas, ADest, LPaint.AlphaF {* (lblText.Opacity / 255)} * (0.7 + 0.3 * LOpacityAnimation));
  end;
end;

constructor TfrmShaderButton.Create(AOwner: TComponent);

  function GetAssetsPath: string;
  begin
    {$IFDEF MSWINDOWS}
    Result := TPath.GetFullPath('..\..\..\..\Assets\');
    {$ELSEIF DEFINED(IOS) or DEFINED(ANDROID)}
    Result := TPath.GetDocumentsPath;
    {$ELSEIF defined(MACOS)}
    Result := TPath.GetFullPath('../Resources/');
    {$ELSE}
    Result := ExtractFilePath(ParamStr(0));
    {$ENDIF}
    if (Result <> '') and not Result.EndsWith(PathDelim) then
      Result := Result + PathDelim;
  end;

var
  LErrorCaption: string;
  LEffect: ISkRuntimeEffect;
begin
  inherited;
  FPressedTime := TStopwatch.Create;
  FBorderThickness := DefaultBorderThickness;
  FCornerRadius := DefaultCornerRadius;
  FLeftColor := DefaultLeftColor;
  FRightColor := DefaultRightColor;
  LEffect := TSkRuntimeEffect.MakeForShader(TFile.ReadAllText(GetAssetsPath + 'button.sksl'), LErrorCaption);
  if LEffect = nil then
  begin
    ShowMessage(LErrorCaption);
    Exit;
  end;
  FShaderBuilder := TSkRuntimeShaderBuilder.Create(LEffect);
  FShaderBuilder.SetUniform('iBorderThickness', FBorderThickness);
  FShaderBuilder.SetUniform('iCornerRadius', FCornerRadius);
  FShaderBuilder.SetUniform('iLeftColor', TAlphaColorF.Create(FLeftColor));
  FShaderBuilder.SetUniform('iRightColor', TAlphaColorF.Create(FRightColor));
end;

procedure TfrmShaderButton.FrameMouseDown(ASender: TObject; AButton: TMouseButton; AShift: TShiftState; AX,
  AY: Integer);
begin
  if AButton = TMouseButton.mbLeft then
    Pressed := True;
end;

procedure TfrmShaderButton.FrameMouseUp(ASender: TObject; AButton: TMouseButton; AShift: TShiftState; AX, AY: Integer);
begin
  if AButton = TMouseButton.mbLeft then
    Pressed := False;
end;

function TfrmShaderButton.GetFontColor: TAlphaColor;
begin
  Result := lblText.TextSettings.FontColor;
end;

function TfrmShaderButton.GetCaption: string;
begin
  Result := lblText.Caption;
end;

procedure TfrmShaderButton.SetBorderThickness(const AValue: Single);
begin
  if not SameValue(FBorderThickness, AValue, TEpsilon.Position) then
  begin
    FBorderThickness := AValue;
    FShaderBuilder.SetUniform('iBorderThickness', FBorderThickness);
  end;
end;

procedure TfrmShaderButton.SetCornerRadius(const AValue: Single);
begin
  if not SameValue(FCornerRadius, AValue, TEpsilon.Position) then
  begin
    FCornerRadius := AValue;
    FShaderBuilder.SetUniform('iCornerRadius', FCornerRadius);
  end;
end;

procedure TfrmShaderButton.SetFontColor(const AValue: TAlphaColor);
begin
  lblText.TextSettings.FontColor := AValue;
end;

procedure TfrmShaderButton.SetLeftColor(const AValue: TAlphaColor);
begin
  if FLeftColor <> AValue then
  begin
    FLeftColor := AValue;
    FShaderBuilder.SetUniform('iLeftColor', TAlphaColorF.Create(FLeftColor));
  end;
end;

procedure TfrmShaderButton.SetPressed(const AValue: Boolean);
begin
  if FPressed <> AValue then
  begin
    FPressed := AValue;
    FPressedTime := TStopwatch.StartNew;
  end;
end;

procedure TfrmShaderButton.SetRightColor(const AValue: TAlphaColor);
begin
  if FRightColor <> AValue then
  begin
    FRightColor := AValue;
    FShaderBuilder.SetUniform('iRightColor', TAlphaColorF.Create(FRightColor));
  end;
end;

procedure TfrmShaderButton.SetCaption(const AValue: string);
begin
  lblText.Caption := AValue;
end;

{ TSkLabelAccess }

procedure TSkLabelAccess.Render(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  inherited Draw(ACanvas, ADest, AOpacity);
end;

end.
