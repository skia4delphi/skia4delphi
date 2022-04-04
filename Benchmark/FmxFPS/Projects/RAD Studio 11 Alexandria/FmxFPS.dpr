program FmxFPS;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  Skia.FMX,
  FmxFPS.Main in '..\..\Source\FmxFPS.Main.pas' {frmMain};

{$R *.res}

begin
  GlobalUseMetal := True;
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
