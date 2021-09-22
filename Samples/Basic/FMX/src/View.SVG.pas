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
  System.IOUtils,
  FMX.Controls,
  FMX.Forms,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Objects,
  FMX.Types,
  FMX.Graphics,
  FMX.StdCtrls,

  { Skia }
  Skia.FMX;

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
  private
    { Private declarations }
    FSvg: TSkSvg;
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

procedure TfrmSVG.Show(const AFileName: string);
begin
  if not Assigned(FSvg) then
  begin
    FSvg := TSkSvg.Create(Self);
    FSvg.Align := TAlignLayout.Client;
    FSvg.Parent := rctSVG;
  end;
  FSvg.Svg.Source := TFile.ReadAllText(AFileName);
  inherited Show;
end;

end.
