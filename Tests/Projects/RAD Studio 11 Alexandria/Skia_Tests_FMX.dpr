program Skia_Tests_FMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  Skia.FMX,
  Skia.Tests.Codec in '..\..\Source\Skia.Tests.Codec.pas',
  Skia.Tests.ColorFilter in '..\..\Source\Skia.Tests.ColorFilter.pas',
  Skia.Tests.Documents in '..\..\Source\Skia.Tests.Documents.pas',
  Skia.Tests.Foundation.ImageHash in '..\..\Source\Skia.Tests.Foundation.ImageHash.pas',
  Skia.Tests.Foundation in '..\..\Source\Skia.Tests.Foundation.pas',
  Skia.Tests.Foundation.Runner in '..\..\Source\Skia.Tests.Foundation.Runner.pas',
  Skia.Tests.Paragraph in '..\..\Source\Skia.Tests.Paragraph.pas',
  Skia.Tests.Path in '..\..\Source\Skia.Tests.Path.pas',
  Skia.Tests.RuntimeEffect in '..\..\Source\Skia.Tests.RuntimeEffect.pas',
  Skia.Tests.Skottie in '..\..\Source\Skia.Tests.Skottie.pas',
  Skia.Tests.Svg in '..\..\Source\Skia.Tests.Svg.pas',
  Skia.Tests.Unicode in '..\..\Source\Skia.Tests.Unicode.pas',
  Skia.Tests.FMX.Runner in '..\..\Source\FMX\Skia.Tests.FMX.Runner.pas' {frmFMXRunner},
  Skia.Tests.FMX.Svg in '..\..\Source\FMX\Skia.Tests.FMX.Svg.pas',
  Skia.Tests.FMX.TextLayout in '..\..\Source\FMX\Skia.Tests.FMX.TextLayout.pas',
  Skia.Tests.FMX.TSkLabel in '..\..\Source\FMX\Skia.Tests.FMX.TSkLabel.pas';

{$R *.res}

begin
  //GlobalUseMetal := True;
  GlobalUseSkia := True;
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TfrmFMXRunner, frmFMXRunner);
  Application.Run;
end.
