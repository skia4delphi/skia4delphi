program BricksGame;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  Skia.FMX,
  Sample.Form.Main in '..\..\Source\Sample.Form.Main.pas' {frmMain};

{$R *.res}

begin
  GlobalUseSkia := True;
  GlobalUseSkiaRasterWhenAvailable := False;
  GlobalUseMetal := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.FormFactor.Orientations := [TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  Application.Run;
end.
