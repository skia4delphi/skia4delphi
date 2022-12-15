unit Sample.Main;

interface

uses
  { Delphi }
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,

  { Sample }
  Sample.ShaderButton;

type
  { TfrmMain }

  TfrmMain = class(TForm)
    frmShaderButton1: TfrmShaderButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

end.
