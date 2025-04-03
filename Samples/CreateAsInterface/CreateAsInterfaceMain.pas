unit CreateAsInterfaceMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Skia, Vcl.Skia,
  System.Types,
  System.UITypes;

type
  TSkIntfDemoMain = class(TForm)
    SkPaintBox1: TSkPaintBox;
    procedure SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SkIntfDemoMain: TSkIntfDemoMain;

implementation

{$R *.dfm}

procedure TSkIntfDemoMain.SkPaintBox1Draw(ASender: TObject;
	const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
	// Much better!
	var font := TSkFont.CreateAsInterface(nil, 24);
	var paint := TSkPaint.CreateAsInterface();
	paint.Color := TAlphaColors.Blueviolet;
	ACanvas.DrawSimpleText('CreateAsInterface!',2,30,font,paint)
end;

end.
