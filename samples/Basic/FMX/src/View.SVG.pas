unit View.SVG;

interface

uses
  { Delphi }
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Math,
  FMX.Controls,
  FMX.Forms,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Objects,
  FMX.Types,
  FMX.Graphics,
  FMX.StdCtrls,

  { Skia }
  Skia;

type
  { TfrmSVG }

  TfrmSVG = class(TForm)
    rctSVG: TRectangle;
    rctHeader: TRectangle;
    btnClose: TSpeedButton;
    lblTitle: TLabel;
    imgArrow: TImage;
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure rctSVGResize(Sender: TObject);
  private
    { Private declarations }
    FDOM: ISKSVGDOM;
    procedure UpdateSVG;
  public
    { Public declarations }
    procedure Show(const AFileName: string); reintroduce;
  end;

var
  frmSVG: TfrmSVG;

implementation

{$R *.fmx}

uses
  { Sample }
  View.Main;

{ TfrmSVG }

procedure TfrmSVG.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmSVG.FormCreate(Sender: TObject);
begin
  StyleBook := frmMain.StyleBook;
end;

procedure TfrmSVG.rctSVGResize(Sender: TObject);
begin
  if Assigned(FDOM) then
    UpdateSVG;
end;

procedure TfrmSVG.Show(const AFileName: string);
var
  LSVGStream: ISKStream;
begin
  LSVGStream := TSKFileStream.Create(AFileName);
  FDOM := TSKSVGDOM.Make(LSVGStream);
  UpdateSVG;
  inherited Show;
end;

procedure TfrmSVG.UpdateSVG;
begin
  // Setting the bitmap size to real rectangle size (in pixels) and clear it
  rctSVG.Fill.Bitmap.Bitmap.SetSize(Round(rctSVG.AbsoluteWidth * rctSVG.Scene.GetSceneScale),
    Round(rctSVG.AbsoluteHeight * rctSVG.Scene.GetSceneScale));
  rctSVG.Fill.Bitmap.Bitmap.Clear(TAlphaColors.Null);

  DrawOnBitmap(rctSVG.Fill.Bitmap.Bitmap,
    procedure (const ACanvas: ISKCanvas)
    var
      LSVGRect: TRectF;
      LDest: TRectF;
    begin
      ACanvas.Save;
      try
        // Before render we need to adjust the ACanvas scale and translate to determine the position
        // and size of the draw. In the example below we will adjust it to fit into the rectangle's bitmap
        LSVGRect := TRectF.Create(TPointF.Create(0, 0), FDOM.Root.GetIntrinsicSize(TSizeF.Create(0, 0)));
        LDest := LSVGRect.FitInto(TRectF.Create(TPointF.Create(0, 0), rctSVG.Fill.Bitmap.Bitmap.Width,
          rctSVG.Fill.Bitmap.Bitmap.Height));
        if not LSVGRect.IsEmpty then
        begin
          ACanvas.Translate(LDest.Left, LDest.Top);
          ACanvas.Scale(LDest.Width / LSVGRect.Width, LDest.Height / LSVGRect.Height);
        end;

        FDOM.Render(ACanvas);
      finally
        ACanvas.Restore;
      end;
    end);
end;

end.
