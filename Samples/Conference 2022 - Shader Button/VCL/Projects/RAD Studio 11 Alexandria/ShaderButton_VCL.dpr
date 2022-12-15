program ShaderButton_VCL;

uses
  Vcl.Forms,
  Sample.Main in '..\..\Source\Sample.Main.pas' {frmMain},
  Sample.ShaderButton in '..\..\Source\Sample.ShaderButton.pas' {frmShaderButton: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
