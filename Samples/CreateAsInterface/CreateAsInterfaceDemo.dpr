program CreateAsInterfaceDemo;

uses
  Vcl.Forms,
  CreateAsInterfaceMain in 'CreateAsInterfaceMain.pas' {SkIntfDemoMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSkIntfDemoMain, SkIntfDemoMain);
  Application.Run;
end.
