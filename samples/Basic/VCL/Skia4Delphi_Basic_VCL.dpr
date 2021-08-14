program Skia4Delphi_Basic_VCL;

uses
  System.SysUtils,
  Vcl.Forms,
  sk4d in '..\..\..\sk4d.pas',
  Skia in '..\..\..\Skia.pas',
  View.Main in 'src\View.Main.pas' {frmMain},
  View.Preview.Bitmap in 'src\View.Preview.Bitmap.pas' {frmBitmapPreview},
  View.Skottie in 'src\View.Skottie.pas' {frmSkottie},
  View.SVG in 'src\View.SVG.pas' {frmSVG},
  Vcl.WIC.Bitmap in 'src\Vcl.WIC.Bitmap.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmBitmapPreview, frmBitmapPreview);
  Application.CreateForm(TfrmSkottie, frmSkottie);
  Application.CreateForm(TfrmSVG, frmSVG);
  Application.Run;
end.
