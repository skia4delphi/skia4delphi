unit Sample.Main;

interface

uses
  { Delphi }
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

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

{$R *.dfm}

end.
