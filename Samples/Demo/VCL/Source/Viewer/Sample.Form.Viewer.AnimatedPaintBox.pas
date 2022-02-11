{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2011-2022 Google LLC.                                    }
{ Copyright (c) 2021-2022 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by a BSD-style license that can be }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Sample.Form.Viewer.AnimatedPaintBox;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.Classes, System.Types, System.Diagnostics, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,

  { Skia }
  Skia, Skia.Vcl,

  { Sample }
  Sample.Form.Base.Viewer;

type
  TAnimatedPaintBoxDrawProc = reference to procedure(const ACanvas: ISkCanvas; const ADest: TRectF; const ASeconds: Double);
  TMouseMoveProc = reference to procedure(const AX, AY: Single);

  { TfrmAnimatedPaintBoxViewer }

  TfrmAnimatedPaintBoxViewer = class(TfrmBaseViewer)
    apbDraw: TSkAnimatedPaintBox;
    procedure apbDrawAnimationDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double; const AOpacity: Single);
    procedure apbDrawMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FDrawProc: TAnimatedPaintBoxDrawProc;
    FOnMouseMove: TMouseMoveProc;
    function GetDrawSize: TSize;
    procedure SetDrawSize(const AValue: TSize);
  public
    procedure Show(const ATitle, ADescription: string; ADrawWidth, ADrawHeight: Integer; ADrawProc: TAnimatedPaintBoxDrawProc; const AOptions: IViewerOptions = nil); reintroduce; overload;
    procedure Show(const ATitle, ADescription: string; ADrawProc: TAnimatedPaintBoxDrawProc; const AOptions: IViewerOptions = nil); reintroduce; overload;
    property DrawSize: TSize read GetDrawSize write SetDrawSize;
    property OnMouseMove: TMouseMoveProc read FOnMouseMove write FOnMouseMove;
    property OnOptionsChange;
  end;

implementation

{$R *.dfm}

procedure TfrmAnimatedPaintBoxViewer.apbDrawAnimationDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double;
  const AOpacity: Single);
begin
  if Assigned(FDrawProc) then
    FDrawProc(ACanvas, ADest, AProgress * apbDraw.Duration);
end;

procedure TfrmAnimatedPaintBoxViewer.apbDrawMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(X, Y);
end;

procedure TfrmAnimatedPaintBoxViewer.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  if Action <> TCloseAction.caNone then
  begin
    apbDraw.Animate := False;
    FDrawProc := nil;
    FOnMouseMove := nil;
  end;
end;

function TfrmAnimatedPaintBoxViewer.GetDrawSize: TSize;
begin
  Result := apbDraw.BoundsRect.Size;
end;

procedure TfrmAnimatedPaintBoxViewer.SetDrawSize(const AValue: TSize);
begin
  apbDraw.SetBounds(0, 0, Round(AValue.Width * apbDraw.ScaleFactor), Round(AValue.Height * apbDraw.ScaleFactor));
end;

procedure TfrmAnimatedPaintBoxViewer.Show(const ATitle, ADescription: string;
  ADrawProc: TAnimatedPaintBoxDrawProc; const AOptions: IViewerOptions);
begin
  Options := AOptions;
  FDrawProc := ADrawProc;
  apbDraw.Align := alClient;
  apbDraw.Redraw;
  apbDraw.Animate := True;
  inherited Show(ATitle, ADescription);
end;

procedure TfrmAnimatedPaintBoxViewer.Show(const ATitle, ADescription: string;
  ADrawWidth, ADrawHeight: Integer; ADrawProc: TAnimatedPaintBoxDrawProc;
  const AOptions: IViewerOptions);
begin
  Options := AOptions;
  FDrawProc := ADrawProc;
  apbDraw.Align := alNone;
  DrawSize := TSize.Create(ADrawWidth, ADrawHeight);
  apbDraw.Redraw;
  apbDraw.Animate := True;
  inherited Show(ATitle, ADescription);
end;

end.
