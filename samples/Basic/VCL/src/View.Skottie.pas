unit View.Skottie;

interface

uses
  { Delphi }
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Imaging.pngimage,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.StdCtrls,

  { Skia }
  Skia,

  { Sample }
  Vcl.WIC.Bitmap;

type
  { TfrmSkottie }

  TfrmSkottie = class(TForm)
    imgBackground: TImage;
    imgBackgroundPicture: TImage;
    lblHeaderTitle: TLabel;
    pnlHeader: TPanel;
    tmrRepaint: TTimer;
    pbxAnimation: TPaintBox;
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tmrRepaintTimer(Sender: TObject);
    procedure pbxAnimationPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FAnimation: ISKSkottieAnimation;
    FAnimationFrame: TWICBitmap;
    FAnimationStartTickCount: Cardinal;
  public
    { Public declarations }
    procedure Show(const AFileName: string); reintroduce;
  end;

var
  frmSkottie: TfrmSkottie;

implementation

{$R *.dfm}

uses
  { Sample }
  View.Main;

{ TfrmSkottie }

procedure TfrmSkottie.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  tmrRepaint.Enabled := False;
end;

procedure TfrmSkottie.FormCreate(Sender: TObject);
begin
  FAnimationFrame := TWICBitmap.Create;
end;

procedure TfrmSkottie.FormDestroy(Sender: TObject);
begin
  FAnimationFrame.Free;
end;

{$REGION ' - Drawing form background'}
procedure TfrmSkottie.FormResize(Sender: TObject);

  procedure TileImage(const ASource: TImage; ATarget: TCanvas; ATargetWidth,
    ATargetHeight: Integer);
  var
    X, Y: Integer;
  begin
    for Y := 0 to Ceil(ATargetHeight / ASource.Height)-1 do
      for X := 0 to Ceil(ATargetWidth / ASource.Width)-1 do
        ATarget.Draw(X * ASource.Width, Y * ASource.Height, ASource.Picture.Graphic);
  end;

begin
  imgBackground.Picture.Bitmap.SetSize(ClientWidth, ClientHeight);
  TileImage(imgBackgroundPicture, imgBackground.Picture.Bitmap.Canvas, ClientWidth, ClientHeight);
end;
{$ENDREGION}

procedure TfrmSkottie.pbxAnimationPaint(Sender: TObject);
var
  LAnimationsSeconds: Single;
  LDestRect: TRectF;
begin
  LDestRect := TRectF.Create(0, 0, Round(pbxAnimation.Width{$IF CompilerVersion >= 33} * pbxAnimation.ScaleFactor{$ENDIF}),
    Round(pbxAnimation.Height{$IF CompilerVersion >= 33} * pbxAnimation.ScaleFactor{$ENDIF}));

  // Setting the bitmap size to real rectangle size (in pixels) and clear it
  FAnimationFrame.SetSize(Round(LDestRect.Width), Round(LDestRect.Height));

  // This will define what frame we will draw at this moment, in this case we will draw the
  // frame in LAnimationsSeconds seconds
  LAnimationsSeconds := (TThread.GetTickCount - FAnimationStartTickCount) / 1000;

  // Set the frame that we want to draw. The FMod need to be used when we want a infinite loop.
  // When we want to execute the complete animation just one time, just remove this line.
  LAnimationsSeconds := (Round(LAnimationsSeconds * 1000) mod Round(FAnimation.Duration * 1000)) / 1000; // In new versions of delphi you can use System.Math.FMod(LAnimationsSeconds, FAnimation.Duration)

  FAnimation.SeekFrameTime(LAnimationsSeconds);

  DrawOnBitmap(FAnimationFrame,
    procedure (const ACanvas: ISKCanvas)
    begin
      ACanvas.Clear(TAlphaColors.Null);
      FAnimation.Render(ACanvas, LDestRect);
    end);
  pbxAnimation.Canvas.Draw(0, 0, FAnimationFrame);
end;

procedure TfrmSkottie.Show(const AFileName: string);
var
  LJsonFile: ISKStream;
begin
  LJsonFile := TSKFileStream.Create(AFileName);
  FAnimation := TSKSkottieAnimation.MakeFromStream(LJsonfile);
  // The stopwatch is to calculate what frame we will draw in every paint
  FAnimationStartTickCount := TThread.GetTickCount;
  inherited Show;

  // Start the TTimer, just to call rctAnimation.Repaint in every 16.6 ms, i.e,
  // render in 60 frames per seconds
  tmrRepaint.Enabled := True;
end;

procedure TfrmSkottie.tmrRepaintTimer(Sender: TObject);
begin
  // We are using the TTimer just to call rctAnimation.Repaint
  // in every 16.6 ms, i.e, render in 60 frames per seconds
  pbxAnimation.Repaint;
end;

end.
