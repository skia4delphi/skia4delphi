program Skia4Delphi_Basic_VCL;

uses
  System.SysUtils,
  Vcl.Forms,
  View.Main in 'src\View.Main.pas' {frmMain},
  View.Preview.Bitmap in 'src\View.Preview.Bitmap.pas' {frmBitmapPreview},
  View.Lottie in 'src\View.Lottie.pas' {frmLottie},
  View.SVG in 'src\View.SVG.pas' {frmSVG},
  View.Comparison.Image in 'src\View.Comparison.Image.pas' {frmImageComparison};

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmBitmapPreview, frmBitmapPreview);
  Application.CreateForm(TfrmLottie, frmLottie);
  Application.CreateForm(TfrmSVG, frmSVG);
  Application.CreateForm(TfrmImageComparison, frmImageComparison);
  Application.Run;
end.
