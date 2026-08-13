program Skia_Tests_FMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  System.Skia.API in '..\..\..\Source\System.Skia.API.pas',
  System.Skia in '..\..\..\Source\System.Skia.pas',
  FMX.Skia.Canvas.GL in '..\..\..\Source\FMX\FMX.Skia.Canvas.GL.pas',
  FMX.Skia.Canvas.Metal in '..\..\..\Source\FMX\FMX.Skia.Canvas.Metal.pas',
  FMX.Skia.Canvas in '..\..\..\Source\FMX\FMX.Skia.Canvas.pas',
  FMX.Skia in '..\..\..\Source\FMX\FMX.Skia.pas',
  Skia.Tests.Blender in '..\..\Source\Skia.Tests.Blender.pas',
  Skia.Tests.Canvas in '..\..\Source\Skia.Tests.Canvas.pas',
  Skia.Tests.Codec in '..\..\Source\Skia.Tests.Codec.pas',
  Skia.Tests.ColorFilter in '..\..\Source\Skia.Tests.ColorFilter.pas',
  Skia.Tests.ColorSpace in '..\..\Source\Skia.Tests.ColorSpace.pas',
  Skia.Tests.Documents in '..\..\Source\Skia.Tests.Documents.pas',
  Skia.Tests.Font in '..\..\Source\Skia.Tests.Font.pas',
  Skia.Tests.FontMetrics in '..\..\Source\Skia.Tests.FontMetrics.pas',
  Skia.Tests.Foundation.ImageHash in '..\..\Source\Skia.Tests.Foundation.ImageHash.pas',
  Skia.Tests.Foundation in '..\..\Source\Skia.Tests.Foundation.pas',
  Skia.Tests.Foundation.Runner in '..\..\Source\Skia.Tests.Foundation.Runner.pas',
  Skia.Tests.Graphics in '..\..\Source\Skia.Tests.Graphics.pas',
  Skia.Tests.Image in '..\..\Source\Skia.Tests.Image.pas',
  Skia.Tests.ImageFilter in '..\..\Source\Skia.Tests.ImageFilter.pas',
  Skia.Tests.MaskFilter in '..\..\Source\Skia.Tests.MaskFilter.pas',
  Skia.Tests.OpBuilder in '..\..\Source\Skia.Tests.OpBuilder.pas',
  Skia.Tests.Paint in '..\..\Source\Skia.Tests.Paint.pas',
  Skia.Tests.Paragraph in '..\..\Source\Skia.Tests.Paragraph.pas',
  Skia.Tests.ParagraphStyle in '..\..\Source\Skia.Tests.ParagraphStyle.pas',
  Skia.Tests.Path in '..\..\Source\Skia.Tests.Path.pas',
  Skia.Tests.PathBuilder in '..\..\Source\Skia.Tests.PathBuilder.pas',
  Skia.Tests.PathEffect in '..\..\Source\Skia.Tests.PathEffect.pas',
  Skia.Tests.PathMeasure in '..\..\Source\Skia.Tests.PathMeasure.pas',
  Skia.Tests.Picture in '..\..\Source\Skia.Tests.Picture.pas',
  Skia.Tests.Pixmap in '..\..\Source\Skia.Tests.Pixmap.pas',
  Skia.Tests.Region in '..\..\Source\Skia.Tests.Region.pas',
  Skia.Tests.RotationScaleMatrix in '..\..\Source\Skia.Tests.RotationScaleMatrix.pas',
  Skia.Tests.RoundRect in '..\..\Source\Skia.Tests.RoundRect.pas',
  Skia.Tests.RuntimeEffect in '..\..\Source\Skia.Tests.RuntimeEffect.pas',
  Skia.Tests.Shader in '..\..\Source\Skia.Tests.Shader.pas',
  Skia.Tests.Skottie in '..\..\Source\Skia.Tests.Skottie.pas',
  Skia.Tests.Surface in '..\..\Source\Skia.Tests.Surface.pas',
  Skia.Tests.Svg in '..\..\Source\Skia.Tests.Svg.pas',
  Skia.Tests.TextBlob in '..\..\Source\Skia.Tests.TextBlob.pas',
  Skia.Tests.Typeface in '..\..\Source\Skia.Tests.Typeface.pas',
  Skia.Tests.Unicode in '..\..\Source\Skia.Tests.Unicode.pas',
  Skia.Tests.Vertices in '..\..\Source\Skia.Tests.Vertices.pas',
  Skia.Tests.FMX.Bitmap in '..\..\Source\FMX\Skia.Tests.FMX.Bitmap.pas',
  Skia.Tests.FMX.Canvas in '..\..\Source\FMX\Skia.Tests.FMX.Canvas.pas',
  Skia.Tests.FMX.Codec in '..\..\Source\FMX\Skia.Tests.FMX.Codec.pas',
  Skia.Tests.FMX.Effects in '..\..\Source\FMX\Skia.Tests.FMX.Effects.pas',
  Skia.Tests.FMX.Runner in '..\..\Source\FMX\Skia.Tests.FMX.Runner.pas' {frmFMXRunner},
  Skia.Tests.FMX.Svg in '..\..\Source\FMX\Skia.Tests.FMX.Svg.pas',
  Skia.Tests.FMX.TextLayout in '..\..\Source\FMX\Skia.Tests.FMX.TextLayout.pas',
  Skia.Tests.FMX.TImage in '..\..\Source\FMX\Skia.Tests.FMX.TImage.pas',
  Skia.Tests.FMX.TSkLabel in '..\..\Source\FMX\Skia.Tests.FMX.TSkLabel.pas',
  Skia.Tests.Issues.ZLib in '..\..\Source\Issues\Skia.Tests.Issues.ZLib.pas';

{$R *.res}

begin
  //GlobalUseMetal := True;
  GlobalUseVulkan := True;
  GlobalUseSkiaRasterWhenAvailable := False;
  GlobalUseSkia := True;
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TfrmFMXRunner, frmFMXRunner);
  Application.Run;
end.
