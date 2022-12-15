unit Sample.ShaderButton;

interface

uses
  { Delphi }
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  { Skia }
  Skia, Skia.Vcl;

type
  { TfrmShaderButton }

  TfrmShaderButton = class(TFrame)
    apbBackground: TSkAnimatedPaintBox;
    lblText: TSkLabel;
    procedure apbBackgroundAnimationDraw(ASender: TObject;
      const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double;
      const AOpacity: Single);
  private const
    DefaultBorderThickness = 2;
    DefaultCornerRadius = 18;
    DefaultLeftColor = $FF4488FE;
    DefaultRightColor = $FFDC6BD2;
  private
    FBorderThickness: Single;
    FCornerRadius: Single;
    FEffect: ISkRuntimeEffect;
    FLeftColor: TAlphaColor;
    FPaint: ISkPaint;
    FRightColor: TAlphaColor;
    function GetFontColor: TAlphaColor;
    function GetCaption: string;
    procedure SetBorderThickness(const AValue: Single);
    procedure SetCornerRadius(const AValue: Single);
    procedure SetFontColor(const AValue: TAlphaColor);
    procedure SetLeftColor(const AValue: TAlphaColor);
    procedure SetRightColor(const AValue: TAlphaColor);
    procedure SetCaption(const AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BorderThickness: Single read FBorderThickness write SetBorderThickness;
    property CornerRadius: Single read FCornerRadius write SetCornerRadius;
    property FontColor: TAlphaColor read GetFontColor write SetFontColor;
    property LeftColor: TAlphaColor read FLeftColor write SetLeftColor default DefaultLeftColor;
    property RightColor: TAlphaColor read FRightColor write SetRightColor default DefaultRightColor;
    property Caption: string read GetCaption write SetCaption;
  end;

implementation

{$R *.dfm}

uses
  { Delphi }
  System.Math, System.Math.Vectors, System.IOUtils;

type
  TSkLabelAccess = class(TSkLabel)
  public
    procedure Render(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
  end;

{ TfrmShaderButton }

procedure TfrmShaderButton.apbBackgroundAnimationDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double;
  const AOpacity: Single);
begin
  if Assigned(FEffect) and Assigned(FPaint) then
  begin
    FEffect.SetUniform('iResolution', [ADest.Width, ADest.Height, 0]);
    FEffect.SetUniform('iTime', apbBackground.Animation.CurrentTime);
    ACanvas.DrawRect(ADest, FPaint);
    // To avoid flicker, let's draw the text inside the animation control
    TSkLabelAccess(lblText).Render(ACanvas, ADest, AOpacity);
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
begin
  inherited;
  FBorderThickness := DefaultBorderThickness;
  FCornerRadius := DefaultCornerRadius;
  FLeftColor := DefaultLeftColor;
  FRightColor := DefaultRightColor;
  FEffect := TSkRuntimeEffect.MakeForShader(TFile.ReadAllText(GetAssetsPath + 'button.sksl'), LErrorCaption);
  if FEffect = nil then
  begin
    ShowMessage(LErrorCaption);
    Exit;
  end;
  FEffect.SetUniform('iBorderThickness', FBorderThickness);
  FEffect.SetUniform('iCornerRadius', FCornerRadius);
  FEffect.SetUniform('iLeftColor', TAlphaColorF.Create(FLeftColor));
  FEffect.SetUniform('iRightColor', TAlphaColorF.Create(FRightColor));
  FPaint := TSkPaint.Create;
  FPaint.Shader := FEffect.MakeShader;
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
    FEffect.SetUniform('iBorderThickness', FBorderThickness);
  end;
end;

procedure TfrmShaderButton.SetCornerRadius(const AValue: Single);
begin
  if not SameValue(FCornerRadius, AValue, TEpsilon.Position) then
  begin
    FCornerRadius := AValue;
    FEffect.SetUniform('iCornerRadius', FCornerRadius);
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
    FEffect.SetUniform('iLeftColor', TAlphaColorF.Create(FLeftColor));
  end;
end;

procedure TfrmShaderButton.SetRightColor(const AValue: TAlphaColor);
begin
  if FRightColor <> AValue then
  begin
    FRightColor := AValue;
    FEffect.SetUniform('iRightColor', TAlphaColorF.Create(FRightColor));
  end;
end;

procedure TfrmShaderButton.SetCaption(const AValue: string);
begin
  lblText.Caption := AValue;
end;

{ TSkLabelAccess }

procedure TSkLabelAccess.Render(const ACanvas: ISkCanvas; const ADest: TRectF;
  const AOpacity: Single);
begin
  inherited Draw(ACanvas, ADest, AOpacity);
end;

end.
