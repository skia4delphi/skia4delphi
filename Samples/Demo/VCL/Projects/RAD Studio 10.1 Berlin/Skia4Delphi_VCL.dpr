program Skia4Delphi_VCL;

uses
  System.SysUtils,
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  Sample.Form.Base in '..\..\Source\Base\Sample.Form.Base.pas' {frmBase},
  Sample.Form.Base.Viewer in '..\..\Source\Base\Sample.Form.Base.Viewer.pas' {frmBaseViewer},
  Sample.Form.Basics in '..\..\Source\Sample.Form.Basics.pas' {frmBasics},
  Sample.Form.Controls in '..\..\Source\Sample.Form.Controls.pas' {frmControls},
  Sample.Form.Controls.TSkAnimatedImage in '..\..\Source\Sample.Form.Controls.TSkAnimatedImage.pas' {frmTSkAnimatedImage},
  Sample.Form.Controls.TSkLabel in '..\..\Source\Sample.Form.Controls.TSkLabel.pas' {frmTSkLabel},
  Sample.Form.Controls.TSkPaintBox in '..\..\Source\Sample.Form.Controls.TSkPaintBox.pas' {frmTSkPaintBox},
  Sample.Form.Controls.TSkSVG in '..\..\Source\Sample.Form.Controls.TSkSVG.pas' {frmTSkSVG},
  Sample.Form.Documents in '..\..\Source\Sample.Form.Documents.pas' {frmDocuments},
  Sample.Form.Filter in '..\..\Source\Sample.Form.Filter.pas' {frmFilter},
  Sample.Form.Image in '..\..\Source\Sample.Form.Image.pas' {frmImage},
  Sample.Form.Main in '..\..\Source\Sample.Form.Main.pas' {frmMain},
  Sample.Form.Particles in '..\..\Source\Sample.Form.Particles.pas' {frmParticles},
  Sample.Form.PathsAndEffects in '..\..\Source\Sample.Form.PathsAndEffects.pas' {frmPathsAndEffects},
  Sample.Form.RuntimeEffects in '..\..\Source\Sample.Form.RuntimeEffects.pas' {frmRuntimeEffects},
  Sample.Form.Text in '..\..\Source\Sample.Form.Text.pas' {frmText},
  Sample.Form.Transforms in '..\..\Source\Sample.Form.Transforms.pas' {frmTransforms},
  Sample.Form.Unicode in '..\..\Source\Sample.Form.Unicode.pas' {frmUnicode},
  Sample.Form.Viewer.AnimatedPaintBox in '..\..\Source\Viewer\Sample.Form.Viewer.AnimatedPaintBox.pas' {frmAnimatedPaintBoxViewer},
  Sample.Form.Viewer.Comparison.Image in '..\..\Source\Viewer\Sample.Form.Viewer.Comparison.Image.pas' {frmComparisonImageViewer},
  Sample.Form.Viewer.Control in '..\..\Source\Viewer\Sample.Form.Viewer.Control.pas' {frmControlViewer},
  Sample.Form.Viewer.PaintBox in '..\..\Source\Viewer\Sample.Form.Viewer.PaintBox.pas' {frmPaintBoxViewer},
  Sample.Form.Viewer.TImage in '..\..\Source\Viewer\Sample.Form.Viewer.TImage.pas' {frmTImageViewer},
  Sample.Form.Viewer.Unicode.BiDi in '..\..\Source\Viewer\Sample.Form.Viewer.Unicode.BiDi.pas' {frmUnicodeBiDiViewer},
  Sample.Form.Viewer.Unicode.Graphemes in '..\..\Source\Viewer\Sample.Form.Viewer.Unicode.Graphemes.pas' {frmUnicodeGraphemesViewer};

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  {$IF CompilerVersion < 34}
  if FileExists('..\..\..\Windows11.XE7.vsf') then
  begin
    TStyleManager.LoadFromFile('..\..\..\Windows11.XE7.vsf');
    TStyleManager.TrySetStyle('Windows11 Modern Light');
  end;
  {$ELSE}
  if FileExists('..\..\..\Windows11.vsf') then
  begin
    TStyleManager.LoadFromFile('..\..\..\Windows11.vsf');
    TStyleManager.TrySetStyle('Windows11 Modern Light');
  end;
  {$ENDIF}
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
