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
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Types,
  FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Layouts, FMX.Objects,

  { Skia }
  Skia, Skia.FMX,

  { Sample }
  Sample.Form.Base.Viewer;

type
  TAnimatedPaintBoxDrawProc = reference to procedure(const ACanvas: ISkCanvas; const ADest: TRectF; const ASeconds: Double);
  TMouseMoveProc = reference to procedure(const AX, AY: Single);

  { TfrmAnimatedPaintBoxViewer }

  TfrmAnimatedPaintBoxViewer = class(TfrmBaseViewer)
    apbDraw: TSkAnimatedPaintBox;
    procedure apbDrawAnimationDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double; const AOpacity: Single);
    procedure apbDrawMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FDrawProc: TAnimatedPaintBoxDrawProc;
    FOnMouseMove: TMouseMoveProc;
    function GetDrawSize: TSizeF;
    procedure SetDrawSize(const AValue: TSizeF);
  public
    procedure Show(const ATitle, ADescription: string; ADrawWidth, ADrawHeight: Single; ADrawProc: TAnimatedPaintBoxDrawProc; const AOptions: IViewerOptions = nil); reintroduce; overload;
    procedure Show(const ATitle, ADescription: string; ADrawProc: TAnimatedPaintBoxDrawProc; const AOptions: IViewerOptions = nil); reintroduce; overload;
    property DrawSize: TSizeF read GetDrawSize write SetDrawSize;
    property OnMouseMove: TMouseMoveProc read FOnMouseMove write FOnMouseMove;
    property OnOptionsChange;
  end;

implementation

{$R *.fmx}

procedure TfrmAnimatedPaintBoxViewer.apbDrawAnimationDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double;
  const AOpacity: Single);
begin
  if Assigned(FDrawProc) then
    FDrawProc(ACanvas, ADest, AProgress * apbDraw.Duration);
end;

procedure TfrmAnimatedPaintBoxViewer.apbDrawMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
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

function TfrmAnimatedPaintBoxViewer.GetDrawSize: TSizeF;
begin
  Result := apbDraw.BoundsRect.Size;
end;

procedure TfrmAnimatedPaintBoxViewer.SetDrawSize(const AValue: TSizeF);
begin
  apbDraw.SetBounds(0, 0, AValue.Width, AValue.Height);
end;

procedure TfrmAnimatedPaintBoxViewer.Show(const ATitle, ADescription: string;
  ADrawProc: TAnimatedPaintBoxDrawProc; const AOptions: IViewerOptions);
begin
  Options := AOptions;
  FDrawProc := ADrawProc;
  apbDraw.Align := TAlignLayout.Client;
  apbDraw.Redraw;
  apbDraw.Animate := True;
  AllowScrollBoundsAnimation := not Assigned(FOnMouseMove);
  inherited Show(ATitle, ADescription);
end;

procedure TfrmAnimatedPaintBoxViewer.Show(const ATitle, ADescription: string;
  ADrawWidth, ADrawHeight: Single; ADrawProc: TAnimatedPaintBoxDrawProc;
  const AOptions: IViewerOptions);
begin
  Options := AOptions;
  FDrawProc := ADrawProc;
  apbDraw.Align := TAlignLayout.None;
  DrawSize := TSizeF.Create(ADrawWidth, ADrawHeight);
  apbDraw.Redraw;
  apbDraw.Animate := True;
  AllowScrollBoundsAnimation := not Assigned(FOnMouseMove);
  inherited Show(ATitle, ADescription);
end;

end.
