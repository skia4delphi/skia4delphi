program Skia_Tests_Console;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  Skia.Tests.Blender in '..\..\Source\Skia.Tests.Blender.pas',
  Skia.Tests.Codec in '..\..\Source\Skia.Tests.Codec.pas',
  Skia.Tests.ColorFilter in '..\..\Source\Skia.Tests.ColorFilter.pas',
  Skia.Tests.Documents in '..\..\Source\Skia.Tests.Documents.pas',
  Skia.Tests.Foundation.ImageHash in '..\..\Source\Skia.Tests.Foundation.ImageHash.pas',
  Skia.Tests.Foundation in '..\..\Source\Skia.Tests.Foundation.pas',
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
  Skia.Tests.Unicode in '..\..\Source\Skia.Tests.Unicode.pas';

var
  LRunner: ITestRunner;
  LResults: IRunResults;
  LLogger: ITestLogger;
  LNUnitLogger: ITestLogger;
begin
  ReportMemoryLeaksOnShutdown := True;
  try
    TDUnitX.CheckCommandLine;
    LRunner := TDUnitX.CreateRunner;
    LRunner.UseRTTI := True;
    LRunner.FailsOnNoAsserts := False;

    LLogger := TDUnitXConsoleLogger.Create(False);
    LRunner.AddLogger(LLogger);
    LNUnitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    LRunner.AddLogger(LNUnitLogger);

    // Run tests
    LResults := LRunner.Execute;
    if not LResults.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.
