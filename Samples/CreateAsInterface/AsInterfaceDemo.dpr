program AsInterfaceDemo;

uses
  Vcl.Forms,
  AsInterfaceMain in 'AsInterfaceMain.pas' {Form31};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm31, Form31);
  Application.Run;
end.
