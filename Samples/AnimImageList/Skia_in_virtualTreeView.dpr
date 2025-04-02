program Skia_in_virtualTreeView;



uses
  Vcl.Forms,
  vtv_skia.mainform in 'vtv_skia.mainform.pas' {Form12},
  VCL.Skia.AnimatedImageList in '..\..\Source\VCL\VCL.Skia.AnimatedImageList.pas',
  VCL.Skia.DesignTime.Editor.Image in '..\..\Source\VCL\Designtime\VCL.Skia.DesignTime.Editor.Image.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm12, Form12);
  Application.CreateForm(TSkImageEditorForm, SkImageEditorForm);
  Application.Run;
end.
