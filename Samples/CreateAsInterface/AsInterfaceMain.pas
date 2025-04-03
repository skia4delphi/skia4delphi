unit AsInterfaceMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Skia, Vcl.Skia,
  System.Types,
  System.UITypes;

type
  TForm31 = class(TForm)
    SkPaintBox1: TSkPaintBox;
    procedure SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form31: TForm31;

implementation

{$R *.dfm}

procedure TForm31.SkPaintBox1Draw(ASender: TObject;
	const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
	var font: ISkFont := TSkFont.Create(nil, 24);
	var paint: ISkPaint := TSkPaint.Create();
	paint.Color := TAlphaColors.Blueviolet;
	ACanvas.DrawSimpleText('before',2,30,font,paint);

	var easyFont := TSkFont.CreateAsInteface(nil, 24);
	var easyPaint := TSkPaint.CreateAsInteface();
	easyPaint := TAlphaColors.Green;
	ACanvas.DrawSimpleText('Easy',32,30,font,paint);


end;

end.
