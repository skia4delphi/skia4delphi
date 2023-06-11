{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2021-2023 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by the MIT license that can be     }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Sample.ShaderButton;

interface

uses
  { Delphi }
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms,
  FMX.Dialogs, FMX.StdCtrls, FMX.Ani,

  { Skia }
  System.Skia, FMX.Skia;

type
  { TfrmShaderButton }

  TfrmShaderButton = class(TFrame)
    apbBackground: TSkAnimatedPaintBox;
    lblText: TSkLabel;
    fanClickAnimation: TFloatAnimation;
    procedure apbBackgroundAnimationDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF;
      const AProgress: Double; const AOpacity: Single);
    procedure fanClickAnimationProcess(Sender: TObject);
  private const
    DefaultBorderThickness = 1.33333;
    DefaultCornerRadius = 11;
    DefaultLeftColor = $FF4488FE;
    DefaultRightColor = $FFDC6BD2;
  private
    FBorderThickness: Single;
    FCornerRadius: Single;
    FLeftColor: TAlphaColor;
    FRightColor: TAlphaColor;
    FShaderBuilder: ISkRuntimeShaderBuilder;
    function GetFontColor: TAlphaColor;
    function GetText: string;
    procedure SetBorderThickness(const AValue: Single);
    procedure SetCornerRadius(const AValue: Single);
    procedure SetFontColor(const AValue: TAlphaColor);
    procedure SetLeftColor(const AValue: TAlphaColor);
    procedure SetRightColor(const AValue: TAlphaColor);
    procedure SetText(const AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure StartTriggerAnimation(const AInstance: TFmxObject; const ATrigger: string); override;
  published
    property BorderThickness: Single read FBorderThickness write SetBorderThickness;
    property CornerRadius: Single read FCornerRadius write SetCornerRadius;
    property FontColor: TAlphaColor read GetFontColor write SetFontColor;
    property LeftColor: TAlphaColor read FLeftColor write SetLeftColor default DefaultLeftColor;
    property RightColor: TAlphaColor read FRightColor write SetRightColor default DefaultRightColor;
    property Text: string read GetText write SetText;
  end;

implementation

{$R *.fmx}

uses
  { Delphi }
  System.Math, System.Math.Vectors, System.IOUtils;

{ TfrmShaderButton }

procedure TfrmShaderButton.apbBackgroundAnimationDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF;
  const AProgress: Double; const AOpacity: Single);
var
  LPaint: ISkPaint;
begin
  if FShaderBuilder <> nil then
  begin
    FShaderBuilder.SetUniform('iResolution', [ADest.Width, ADest.Height, ACanvas.GetLocalToDeviceAs3x3.ExtractScale.X]);
    FShaderBuilder.SetUniform('iTime', apbBackground.Animation.CurrentTime);
    LPaint := TSkPaint.Create;
    LPaint.Shader := FShaderBuilder.MakeShader;
    LPaint.AlphaF := AOpacity;
    ACanvas.DrawRect(ADest, LPaint);
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
  LEffect: ISkRuntimeEffect;
  LErrorText: string;
begin
  inherited;
  AutoCapture := True;
  FBorderThickness := DefaultBorderThickness;
  FCornerRadius := DefaultCornerRadius;
  FLeftColor := DefaultLeftColor;
  FRightColor := DefaultRightColor;
  LEffect := TSkRuntimeEffect.MakeForShader(TFile.ReadAllText(GetAssetsPath + 'button.sksl'), LErrorText);
  if LEffect = nil then
  begin
    ShowMessage(LErrorText);
    Exit;
  end;
  FShaderBuilder := TSkRuntimeShaderBuilder.Create(LEffect);
  FShaderBuilder.SetUniform('iBorderThickness', FBorderThickness);
  FShaderBuilder.SetUniform('iCornerRadius', FCornerRadius);
  FShaderBuilder.SetUniform('iLeftColor', TAlphaColorF.Create(FLeftColor));
  FShaderBuilder.SetUniform('iRightColor', TAlphaColorF.Create(FRightColor));
end;

procedure TfrmShaderButton.fanClickAnimationProcess(Sender: TObject);
begin
  lblText.Opacity := 0.70 + ((apbBackground.Opacity - 0.9) / 0.1) * 0.30;
end;

function TfrmShaderButton.GetFontColor: TAlphaColor;
begin
  Result := lblText.TextSettings.FontColor;
end;

function TfrmShaderButton.GetText: string;
begin
  Result := lblText.Text;
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

procedure TfrmShaderButton.SetRightColor(const AValue: TAlphaColor);
begin
  if FRightColor <> AValue then
  begin
    FRightColor := AValue;
    FShaderBuilder.SetUniform('iRightColor', TAlphaColorF.Create(FRightColor));
  end;
end;

procedure TfrmShaderButton.SetText(const AValue: string);
begin
  lblText.Text := AValue;
end;

procedure TfrmShaderButton.StartTriggerAnimation(const AInstance: TFmxObject;
  const ATrigger: string);
begin
  if (AInstance = Self) and (ATrigger = 'Pressed') then
  begin
    fanClickAnimation.StopAtCurrent;
    if Pressed then
    begin
      fanClickAnimation.StartValue := apbBackground.Opacity;
      fanClickAnimation.StopValue := 0.9;
      fanClickAnimation.Duration := 0.05;
    end
    else
    begin
      fanClickAnimation.StartValue := apbBackground.Opacity;
      fanClickAnimation.StopValue := 1;
      fanClickAnimation.Duration := 0.1;
    end;
    fanClickAnimation.Start;
  end;
  inherited;
end;

end.
