program CreateAsInterfaceVclDemo;

uses
  Vcl.Forms,
  CreateAsInterfaceVclMain in 'CreateAsInterfaceVclMain.pas' {SkIntfDemoMain},
  System.Skia.API in '..\..\Source\System.Skia.API.pas',
  System.Skia in '..\..\Source\System.Skia.pas',
  Vcl.Skia in '..\..\Source\VCL\Vcl.Skia.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSkIntfDemoMain, SkIntfDemoMain);
  Application.Run;
end.
