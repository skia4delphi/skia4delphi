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
unit Sample.Form.Main;

// Bricks Game ported from https://www.shadertoy.com/view/MddGzf
// Rules:
//   - Use arrow keys or tap to move the paddle.
//   - Use space or double tap to restart.
// Game code:
//   This shadertoy example is a little different as it has 2 shaders: Image tab
//   and BufferA tab. The Image code is the shader code that paints the game.
//   BufferA is usually an auxiliary code and analyzing this case specifically,
//   I realized that it is a game logic shader, which calculates the position
//   and speed of the ball, the paddle, the brick that is being destroyed, and
//   all other game information. This logic shader is processed all the time,
//   that is, every frame to update the game state, before Image renders the
//   game on the screen. Then the Image shader receives the output of BufferA,
//   that is, all the current information of the game, and then the Image shader
//   renders the game.
// About demo:
//   This demo runs the BufferA shader on the CPU. We will soon make another
//   version available, with a few more lines of code, but faster, that runs
//   both shaders only on the GPU.
// Demo video:
//   https://www.youtube.com/watch?v=2LaNI3BaOMc

interface

uses
  { Delphi }
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.IOUtils,
  System.Diagnostics, System.Math.Vectors, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.Controls.Presentation,

  { Skia }
  Skia, Skia.FMX;

type
  { TfrmMain }

  TfrmMain = class(TForm)
    SkAnimatedPaintBox1: TSkAnimatedPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure SkAnimatedPaintBox1AnimationDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double; const AOpacity: Single);
    procedure SkAnimatedPaintBox1Gesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure SkAnimatedPaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  private
    // Uniforms - These are the inputs of our shader codes
    FFrame: Integer;
    FIsVKLeftPressing: Boolean;
    FIsVKRightPressing: Boolean;
    FIsVKSpacePressing: Boolean;
    FLastFrameTime: Single;
    FMouse: TPoint3D;
    FTimer: TStopwatch;
    // Image shader - Render shader, which will draw the game based on the
    // current game state (BufferA shader).
    FImageEffect: ISkRuntimeEffect;
    FImagePaint: ISkPaint;
    // BufferA shader - Game logic shader, which will compute the current game
    // state. The resulting image is not exactly an image, with colors in its
    // pixels, but with data in its pixels, such as ball speed, positions,
    // score, game state, etc. The storage of data in image is done because GPUs
    // were made to work with images, outputing just images, and not any other
    // type of data. This doesn't stop us from storing non-color data within the
    // images, at each pixel, and that's exactly what BufferA does.
    FBufferAEffect: ISkRuntimeEffect;
    FBufferAShader: ISkShader;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

const
  MouseDownFlag: array[Boolean] of Single = (0, 1);

procedure TfrmMain.FormCreate(Sender: TObject);

  function GetAssetsPath: string;
  begin
    {$IFDEF MSWINDOWS}
    Result := TPath.GetFullPath('..\..\..\Assets\');
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

begin
  FImagePaint    := TSkPaint.Create;
  FImageEffect   := TSkRuntimeEffect.MakeForShader(TFile.ReadAllText(GetAssetsPath + 'Image.sksl'));
  FBufferAEffect := TSkRuntimeEffect.MakeForShader(TFile.ReadAllText(GetAssetsPath + 'BufferA.sksl'));
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  FIsVKLeftPressing  := FIsVKLeftPressing or (Key = vkLeft);
  FIsVKRightPressing := FIsVKRightPressing or (Key = vkRight);
  FIsVKSpacePressing := FIsVKSpacePressing or (KeyChar = ' ');
end;

procedure TfrmMain.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  FIsVKLeftPressing  := FIsVKLeftPressing and (Key <> vkLeft);
  FIsVKRightPressing := FIsVKRightPressing and (Key <> vkRight);
  FIsVKSpacePressing := FIsVKSpacePressing and (KeyChar <> ' ');
end;

procedure TfrmMain.SkAnimatedPaintBox1AnimationDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double;
  const AOpacity: Single);

  procedure RunBufferAShader(const ATime: Single);
  const
    BufferASize: TSize = (cx: 14; cy: 14);
  var
    LBufferAImage: ISkImage;
  begin
    FBufferAEffect.SetUniform('iResolution', PointF(ADest.Width, ADest.Height));
    FBufferAEffect.SetUniform('iTime', ATime);
    FBufferAEffect.SetUniform('iTimeDelta', ATime - FLastFrameTime);
    FBufferAEffect.SetUniform('iFrame', FFrame);
    FBufferAEffect.SetUniform('iMouse', [FMouse.X, FMouse.Y, FMouse.Z]);
    FBufferAEffect.SetUniform('ivkLeftPressing', Integer(FIsVKLeftPressing));
    FBufferAEffect.SetUniform('ivkRightPressing', Integer(FIsVKRightPressing));
    FBufferAEffect.SetUniform('ivkSpacePressing', Integer(FIsVKSpacePressing));
    // Set the BufferA result of last frame (the result of this shader in last
    // draw). This process is called Multipass system.
    FBufferAEffect.ChildrenShaders['iChannel0'] := FBufferAShader;

    LBufferAImage  := FBufferAEffect.MakeImage(TSkImageInfo.Create(BufferASize.Width, BufferASize.Height, TSkColorType.RGBAF16Clamped));
    FBufferAShader := LBufferAImage.MakeRawShader(TSkSamplingOptions.Create(TSkFilterMode.Nearest, TSkMipmapMode.None));
  end;

  procedure RenderImageShader(const ATime: Single);
  begin
    FImageEffect.SetUniform('iResolution', PointF(ADest.Width, ADest.Height));
    FImageEffect.SetUniform('iTime', ATime);
    FImageEffect.ChildrenShaders['iChannel0'] := FBufferAShader;
    FImagePaint.Shader := FImageEffect.MakeShader(True);
    ACanvas.DrawRect(ADest, FImagePaint);
  end;

var
  LTime: Single;
begin
  if FFrame = 0 then
    FTimer := TStopwatch.StartNew;
  LTime := FTimer.Elapsed.TotalSeconds;

  RunBufferAShader(LTime);
  RenderImageShader(LTime);

  Inc(FFrame);
  FLastFrameTime := LTime;
  FIsVKSpacePressing := False;
  FMouse := TPoint3D.Create(FMouse.X, FMouse.Y, MouseDownFlag[False]);
end;

procedure TfrmMain.SkAnimatedPaintBox1Gesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  if EventInfo.GestureID = igiDoubleTap then
    FIsVKSpacePressing := True;
end;

procedure TfrmMain.SkAnimatedPaintBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FMouse := TPoint3D.Create(X, Y, MouseDownFlag[True]);
end;

end.
