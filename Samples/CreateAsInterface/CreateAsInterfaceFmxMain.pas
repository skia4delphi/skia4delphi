unit CreateAsInterfaceFmxMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Skia, FMX.Skia, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm32 = class(TForm)
    SkPaintBox1: TSkPaintBox;
    procedure SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form32: TForm32;

implementation

{$R *.fmx}

procedure TForm32.SkPaintBox1Draw(ASender: TObject;
	const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
	var family := TSkTypeface.MakeFromName('Segoe Script', TSkFontStyle.Italic);
	var font := TSkFont.Make(family, 24);
	var paint := TSkPaint.Make();
	paint.Color := TAlphaColors.Blueviolet;
	ACanvas.DrawSimpleText('.Make',2,30,font,paint);
end;

end.
