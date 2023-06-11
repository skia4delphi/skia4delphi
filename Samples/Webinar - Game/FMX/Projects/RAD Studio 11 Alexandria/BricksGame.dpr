program BricksGame;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  FMX.Skia,
  Sample.Form.Main in '..\..\Source\Sample.Form.Main.pas' {frmMain};

{$R *.res}

begin
  GlobalUseMetal := True;
  GlobalUseSkia := True;
  GlobalUseSkiaRasterWhenAvailable := False;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.FormFactor.Orientations := [TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  Application.Run;
end.
