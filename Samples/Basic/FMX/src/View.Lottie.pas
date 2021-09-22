unit View.Lottie;

interface

uses
  { Delphi }
  System.Types,
  System.Classes,
  System.Variants,
  System.UITypes,
  System.Math,
  FMX.Controls,
  FMX.Forms,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Objects,
  FMX.Types,
  FMX.Graphics,
  FMX.Ani,
  FMX.StdCtrls,

  { Skia }
  Skia.FMX;

type
  { TfrmLottie }

  TfrmLottie = class(TForm)
    rctAnimation: TRectangle;
    rctHeader: TRectangle;
    btnClose: TSpeedButton;
    lblTitle: TLabel;
    imgArrow: TImage;
    procedure btnCloseClick(Sender: TObject);
  private
    FLottie: TSkLottieAnimation;
  public
    procedure Show(const AFileName: string); reintroduce;
  end;

var
  frmLottie: TfrmLottie;

implementation

{$R *.fmx}

{ TfrmLottie }

procedure TfrmLottie.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmLottie.Show(const AFileName: string);
begin
  if not Assigned(FLottie) then
  begin
    FLottie := TSkLottieAnimation.Create(Self);
    FLottie.Align := TAlignLayout.Client;
    FLottie.Parent := rctAnimation;
  end;
  FLottie.LoadFromFile(AFileName);
  inherited Show;
end;

end.
