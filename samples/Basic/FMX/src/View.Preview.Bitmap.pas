unit View.Preview.Bitmap;

interface

uses
  { Delphi }
  System.SysUtils,
  System.Types,
  System.Classes,
  System.Variants,
  FMX.Controls,
  FMX.Forms,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Objects,
  FMX.Types,
  FMX.Graphics,
  FMX.StdCtrls;

type
  { TfrmBitmapPreview }

  TfrmBitmapPreview = class(TForm)
    rctBitmap: TRectangle;
    btnClose: TSpeedButton;
    lblTitle: TLabel;
    sbxContent: TScrollBox;
    rctHeader: TRectangle;
    imgArrow: TImage;
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Show(ABitmap: TBitmap); reintroduce;
  end;

var
  frmBitmapPreview: TfrmBitmapPreview;

implementation

{$R *.fmx}

uses
  { Sample }
  View.Main;

{ TfrmBitmapPreview }

procedure TfrmBitmapPreview.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmBitmapPreview.FormCreate(Sender: TObject);
begin
  StyleBook := frmMain.StyleBook;
end;

procedure TfrmBitmapPreview.Show(ABitmap: TBitmap);
begin
  rctBitmap.Width := ABitmap.Width / rctBitmap.Scene.GetSceneScale;
  rctBitmap.Height := ABitmap.Height / rctBitmap.Scene.GetSceneScale;
  rctBitmap.Fill.Bitmap.Bitmap := ABitmap;
  inherited Show;
end;

end.
