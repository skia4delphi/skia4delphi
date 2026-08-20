program FmxFPS;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  {$IFDEF SKIA}
  FMX.Skia,
  {$ENDIF}
  FmxFPS.Diagnostics in '..\..\Source\FmxFPS.Diagnostics.pas',
  FmxFPS.Main in '..\..\Source\FmxFPS.Main.pas' {frmMain};

{$R *.res}

begin
  GlobalUseMetal := True;
  GlobalUseVulkan := True;
  {$IFDEF SKIA}
  GlobalUseSkiaRasterWhenAvailable := False;
  GlobalUseSkia := True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
