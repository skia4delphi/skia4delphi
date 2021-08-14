program Skia4Delphi_Basic_XE6;

uses
  System.StartUpCopy,
  FMX.Types,
  FMX.Forms,
  sk4d in '..\..\..\sk4d.pas',
  Skia in '..\..\..\Skia.pas',
  View.Main in 'src\View.Main.pas' {frmMain},
  View.Preview.Bitmap in 'src\View.Preview.Bitmap.pas' {frmBitmapPreview},
  View.Preview.PDF in 'src\View.Preview.PDF.pas' {frmPDFPreview},
  View.Skottie in 'src\View.Skottie.pas' {frmSkottie},
  View.SVG in 'src\View.SVG.pas' {frmSVG};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmBitmapPreview, frmBitmapPreview);
  Application.CreateForm(TfrmPDFPreview, frmPDFPreview);
  Application.CreateForm(TfrmSkottie, frmSkottie);
  Application.CreateForm(TfrmSVG, frmSVG);
  Application.Run;
end.
