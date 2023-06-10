program ShaderButton;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  FMX.Skia,
  Sample.Main in '..\..\Source\Sample.Main.pas' {frmMain},
  Sample.ShaderButton in '..\..\Source\Sample.ShaderButton.pas' {frmShaderButton: TFrame};

{$R *.res}

begin
  GlobalUseMetal := True;
  GlobalUseSkia := True;
  GlobalUseSkiaRasterWhenAvailable := False;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
