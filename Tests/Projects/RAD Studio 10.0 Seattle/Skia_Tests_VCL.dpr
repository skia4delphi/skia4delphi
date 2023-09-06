program Skia_Tests_VCL;

uses
  System.SysUtils,
  Vcl.Forms,
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
  Skia.Tests.Vcl.Bitmap in '..\..\Source\VCL\Skia.Tests.Vcl.Bitmap.pas',
  Skia.Tests.Vcl.Runner in '..\..\Source\VCL\Skia.Tests.Vcl.Runner.pas' {frmVclRunner},
  Skia.Tests.Vcl.Svg in '..\..\Source\VCL\Skia.Tests.Vcl.Svg.pas',
  Skia.Tests.Vcl.TSkLabel in '..\..\Source\VCL\Skia.Tests.Vcl.TSkLabel.pas';

{$R *.res}

begin
  // Fix issue of old versions of DUnitX
  // https://github.com/VSoftTechnologies/DUnitX/issues/108
  FormatSettings.DecimalSeparator := '.';

  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmVclRunner, frmVclRunner);
  Application.Run;
end.
