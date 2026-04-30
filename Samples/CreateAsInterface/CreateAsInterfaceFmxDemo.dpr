program CreateAsInterfaceFmxDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  CreateAsInterfaceFmxMain in 'CreateAsInterfaceFmxMain.pas' {Form32},
  System.Skia.API in '..\..\Source\System.Skia.API.pas',
  System.Skia in '..\..\Source\System.Skia.pas',
  FMX.Skia.AnimatedCodec in '..\..\Source\FMX\FMX.Skia.AnimatedCodec.pas',
  FMX.Skia.Canvas.GL in '..\..\Source\FMX\FMX.Skia.Canvas.GL.pas',
  FMX.Skia.Canvas.Metal in '..\..\Source\FMX\FMX.Skia.Canvas.Metal.pas',
  FMX.Skia.Canvas in '..\..\Source\FMX\FMX.Skia.Canvas.pas',
  FMX.Skia.Filter in '..\..\Source\FMX\FMX.Skia.Filter.pas',
  FMX.Skia in '..\..\Source\FMX\FMX.Skia.pas',
  FMX.Skia.Printer in '..\..\Source\FMX\FMX.Skia.Printer.pas',
  FMX.Skia.Canvas.Vulkan in 'C:\Program Files (x86)\Embarcadero\Studio\23.0\source\fmx\FMX.Skia.Canvas.Vulkan.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm32, Form32);
  Application.Run;
end.
