program SK4D_Demo;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  FMX.Skia,
  Sample.Form.Main in '..\..\Source\Sample.Form.Main.pas' {frmMain},
  Sample.Form.PDF.Creation in '..\..\Source\Sample.Form.PDF.Creation.pas' {frmPDFCreation},
  Sample.Form.PDF.Viewer in '..\..\Source\Sample.Form.PDF.Viewer.pas' {frmPDFViewer},
  Sample.Form.QRCode.Render in '..\..\Source\Sample.Form.QRCode.Render.pas' {frmQRCodeRender},
  Sample.QRCode.Render in '..\..\Source\Sample.QRCode.Render.pas';

{$R *.res}

begin
  GlobalUseMetal := True;
  GlobalUseSkia := True;
  GlobalUseSkiaRasterWhenAvailable := False;
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait];
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmPDFCreation, frmPDFCreation);
  Application.CreateForm(TfrmPDFViewer, frmPDFViewer);
  Application.CreateForm(TfrmQRCodeRender, frmQRCodeRender);
  Application.Run;
end.
