unit View.Skottie;

interface

uses
  { Delphi }
  System.Types,
  System.Classes,
  System.Variants,
  System.UITypes,
  System.Math,
  FMX.Controls,
  FMX.Forms,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Objects,
  FMX.Types,
  FMX.Graphics,
  FMX.Ani,
  FMX.StdCtrls,

  { Skia }
  Skia;

type
  { TfrmSkottie }

  TfrmSkottie = class(TForm)
    rctAnimation: TRectangle;
    tmrRepaint: TTimer;
    rctHeader: TRectangle;
    btnClose: TSpeedButton;
    lblTitle: TLabel;
    imgArrow: TImage;
    procedure rctAnimationPaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tmrRepaintTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FAnimation: ISKSkottieAnimation;
    FAnimationFrame: TBitmap;
    FAnimationStartTickCount: Cardinal;
  public
    { Public declarations }
    procedure Show(const AFileName: string); reintroduce;
  end;

var
  frmSkottie: TfrmSkottie;

implementation

{$R *.fmx}

uses
  { Sample }
  View.Main;

{ TfrmSkottie }

procedure TfrmSkottie.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmSkottie.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  tmrRepaint.Enabled := False;
end;

procedure TfrmSkottie.FormCreate(Sender: TObject);
begin
  StyleBook := frmMain.StyleBook;
  FAnimationFrame := TBitmap.Create;
end;

procedure TfrmSkottie.FormDestroy(Sender: TObject);
begin
  FAnimationFrame.Free;
end;

procedure TfrmSkottie.rctAnimationPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  LAnimationsSeconds: Single;
  LDestRect: TRectF;
begin
  if Assigned(FAnimation) then
  begin
    LDestRect := TRectF.Create(0, 0, Round(rctAnimation.AbsoluteWidth * rctAnimation.Scene.GetSceneScale),
      Round(rctAnimation.AbsoluteHeight * rctAnimation.Scene.GetSceneScale));

    // Setting the bitmap size to real rectangle size (in pixels) and clear it
    FAnimationFrame.SetSize(Round(LDestRect.Width), Round(LDestRect.Height));
    FAnimationFrame.Clear(TAlphaColors.Null);

    // This will define what frame we will draw at this moment, in this case we will draw the
    // frame in LAnimationsSeconds seconds
    LAnimationsSeconds := (TThread.GetTickCount - FAnimationStartTickCount) / 1000;

    // Set the frame that we want to draw. The FMod need to be used when we want a infinite loop.
    // When we want to execute the complete animation just one time, just remove this line.
    LAnimationsSeconds := (Round(LAnimationsSeconds * 1000) mod Round(FAnimation.Duration * 1000)) / 1000;
    // In new versions of delphi you can replace the line above with this:
    // LAnimationsSeconds := FMod(LAnimationsSeconds, FAnimation.Duration);

    FAnimation.SeekFrameTime(LAnimationsSeconds);

    DrawOnBitmap(FAnimationFrame,
      procedure (const ACanvas: ISKCanvas)
      begin
        FAnimation.Render(ACanvas, LDestRect);
      end);
    Canvas.DrawBitmap(FAnimationFrame, TRectF.Create(0, 0, FAnimationFrame.Width, FAnimationFrame.Height), ARect, 1);
  end;
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
  rctAnimation.Repaint;
end;

end.
