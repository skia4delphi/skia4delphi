program Skia4Delphi_Basic_XE6;

uses
  System.StartUpCopy,
  FMX.Types,
  FMX.Forms,
  View.Main in 'src\View.Main.pas' {frmMain},
  View.Preview.Bitmap in 'src\View.Preview.Bitmap.pas' {frmBitmapPreview},
  View.Preview.PDF in 'src\View.Preview.PDF.pas' {frmPDFPreview},
  View.SVG in 'src\View.SVG.pas' {frmSVG},
  View.Lottie in 'src\View.Lottie.pas' {frmLottie},
  View.Comparison.Image in 'src\View.Comparison.Image.pas' {frmImageComparison};

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmBitmapPreview, frmBitmapPreview);
  Application.CreateForm(TfrmPDFPreview, frmPDFPreview);
  Application.CreateForm(TfrmSVG, frmSVG);
  Application.CreateForm(TfrmLottie, frmLottie);
  Application.CreateForm(TfrmImageComparison, frmImageComparison);
  Application.Run;
end.
