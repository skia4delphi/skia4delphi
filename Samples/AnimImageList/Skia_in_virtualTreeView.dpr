program Skia_in_virtualTreeView;



uses
  Vcl.Forms,
  vtv_skia.mainform in 'vtv_skia.mainform.pas' {Form12},
  VCL.Skia.AnimatedImageList in '..\..\Source\VCL\VCL.Skia.AnimatedImageList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm12, Form12);
  Application.Run;
end.
