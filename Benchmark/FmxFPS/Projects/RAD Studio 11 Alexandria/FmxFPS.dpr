program FmxFPS;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  {$IFDEF SKIA}
  FMX.Skia,
  {$ENDIF}
  FmxFPS.Main in '..\..\Source\FmxFPS.Main.pas' {frmMain};

{$R *.res}

begin
  GlobalUseMetal := True;
  {$IFDEF SKIA}
  GlobalUseSkiaRasterWhenAvailable := False;
  GlobalUseSkia := True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
