program ShaderButton;

uses
  System.StartUpCopy,
  FMX.Forms,
  Skia.FMX,
  Sample.Main in '..\..\Source\Sample.Main.pas' {frmMain},
  Sample.ShaderButton in '..\..\Source\Sample.ShaderButton.pas' {frmShaderButton: TFrame};

{$R *.res}

begin
  GlobalUseSkia := True;
  GlobalUseSkiaRasterWhenAvailable := False;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
