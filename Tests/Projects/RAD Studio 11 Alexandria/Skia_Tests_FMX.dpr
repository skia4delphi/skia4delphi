program Skia_Tests_FMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  FMX.Skia,
  Skia.Tests.Blender in '..\..\Source\Skia.Tests.Blender.pas',
  Skia.Tests.Codec in '..\..\Source\Skia.Tests.Codec.pas',
  Skia.Tests.ColorFilter in '..\..\Source\Skia.Tests.ColorFilter.pas',
  Skia.Tests.Documents in '..\..\Source\Skia.Tests.Documents.pas',
  Skia.Tests.Foundation.ImageHash in '..\..\Source\Skia.Tests.Foundation.ImageHash.pas',
  Skia.Tests.Foundation in '..\..\Source\Skia.Tests.Foundation.pas',
  Skia.Tests.Foundation.Runner in '..\..\Source\Skia.Tests.Foundation.Runner.pas',
  Skia.Tests.Image in '..\..\Source\Skia.Tests.Image.pas',
  Skia.Tests.MaskFilter in '..\..\Source\Skia.Tests.MaskFilter.pas',
  Skia.Tests.Paragraph in '..\..\Source\Skia.Tests.Paragraph.pas',
  Skia.Tests.Path in '..\..\Source\Skia.Tests.Path.pas',
  Skia.Tests.Picture in '..\..\Source\Skia.Tests.Picture.pas',
  Skia.Tests.Region in '..\..\Source\Skia.Tests.Region.pas',
  Skia.Tests.RotationScaleMatrix in '..\..\Source\Skia.Tests.RotationScaleMatrix.pas',
  Skia.Tests.RoundRect in '..\..\Source\Skia.Tests.RoundRect.pas',
  Skia.Tests.RuntimeEffect in '..\..\Source\Skia.Tests.RuntimeEffect.pas',
  Skia.Tests.Skottie in '..\..\Source\Skia.Tests.Skottie.pas',
  Skia.Tests.Surface in '..\..\Source\Skia.Tests.Surface.pas',
  Skia.Tests.Svg in '..\..\Source\Skia.Tests.Svg.pas',
  Skia.Tests.TextBlob in '..\..\Source\Skia.Tests.TextBlob.pas',
  Skia.Tests.Unicode in '..\..\Source\Skia.Tests.Unicode.pas',
  Skia.Tests.FMX.Codec in '..\..\Source\FMX\Skia.Tests.FMX.Codec.pas',
  Skia.Tests.FMX.Runner in '..\..\Source\FMX\Skia.Tests.FMX.Runner.pas' {frmFMXRunner},
  Skia.Tests.FMX.Svg in '..\..\Source\FMX\Skia.Tests.FMX.Svg.pas',
  Skia.Tests.FMX.TextLayout in '..\..\Source\FMX\Skia.Tests.FMX.TextLayout.pas',
  Skia.Tests.FMX.TImage in '..\..\Source\FMX\Skia.Tests.FMX.TImage.pas',
  Skia.Tests.FMX.TSkLabel in '..\..\Source\FMX\Skia.Tests.FMX.TSkLabel.pas',
  Skia.Tests.Issues.ZLib in '..\..\Source\Issues\Skia.Tests.Issues.ZLib.pas';

{$R *.res}

begin
  //GlobalUseMetal := True;
  GlobalUseSkia := True;
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TfrmFMXRunner, frmFMXRunner);
  Application.Run;
end.
