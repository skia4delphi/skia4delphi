unit CreateAsInterfaceVclMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
	Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Skia, Vcl.Skia, System.Types, System.UITypes;

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
	var family := TSkTypeface.MakeFromName('Segoe Script', TSkFontStyle.Italic);
	var font := TSkFont.Make(family, 24);
	var paint := TSkPaint.Make();
	paint.Color := TAlphaColors.Blueviolet;
	ACanvas.DrawSimpleText('.Make',2,30,font,paint);
end;

end.
