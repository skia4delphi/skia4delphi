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
unit Sample.Form.Viewer.PaintBox;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.Classes, System.Types, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,

  { Skia }
  Skia, Skia.Vcl,

  { Sample }
  Sample.Form.Base.Viewer;

type
  TPaintBoxDrawProc = reference to procedure(const ACanvas: ISkCanvas; const ADest: TRectF);
  TMouseDownProc = reference to procedure(const APoint: TPointF; var AShouldRedraw: Boolean);
  TMouseMoveProc = reference to procedure(const APoint: TPointF; const AIsMouseDown: Boolean; var AShouldRedraw: Boolean);
  TMouseUpProc = reference to procedure(const APoint: TPointF; var AShouldRedraw: Boolean);

  { TfrmPaintBoxViewer }

  TfrmPaintBoxViewer = class(TfrmBaseViewer)
    pbxDraw: TSkPaintBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure pbxDrawDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
    procedure pbxDrawMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbxDrawMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pbxDrawMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FDrawProc: TPaintBoxDrawProc;
    FIsMouseDown: Boolean;
    FOnMouseDown: TMouseDownProc;
    FOnMouseMove: TMouseMoveProc;
    FOnMouseUp: TMouseUpProc;
  protected
    procedure OptionsChanged; override;
  public
    procedure Show(const ATitle, ADescription: string; const ADrawProc: TPaintBoxDrawProc; ABackgroundKind: TBackgroundKind = TBackgroundKind.Chess; const AOptions: IViewerOptions = nil); reintroduce; overload;
    procedure Show(const ATitle, ADescription: string; const ADrawWidth, ADrawHeight: Integer; const ADrawProc: TPaintBoxDrawProc; ABackgroundKind: TBackgroundKind = TBackgroundKind.Chess; const AOptions: IViewerOptions = nil); reintroduce; overload;
    property OnMouseDown: TMouseDownProc read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveProc read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseUpProc read FOnMouseUp write FOnMouseUp;
  end;

implementation

{$R *.dfm}

procedure TfrmPaintBoxViewer.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FDrawProc := nil;
  FOnMouseDown := nil;
  FOnMouseMove := nil;
  FOnMouseUp := nil;
  inherited;
end;

procedure TfrmPaintBoxViewer.OptionsChanged;
begin
  inherited;
  pbxDraw.Redraw;
end;

procedure TfrmPaintBoxViewer.pbxDrawDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  if Assigned(FDrawProc) then
    FDrawProc(ACanvas, ADest);
end;

procedure TfrmPaintBoxViewer.pbxDrawMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  LShouldRedraw: Boolean;
begin
  inherited;
  FIsMouseDown := True;
  if Assigned(FOnMouseDown) then
  begin
    LShouldRedraw := False;
    FOnMouseDown(PointF(X, Y), LShouldRedraw);
    if LShouldRedraw then
      pbxDraw.Redraw;
  end;
end;

procedure TfrmPaintBoxViewer.pbxDrawMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  LShouldRedraw: Boolean;
begin
  inherited;
  if Assigned(FOnMouseMove) then
  begin
    LShouldRedraw := False;
    FOnMouseMove(PointF(X, Y), FIsMouseDown, LShouldRedraw);
    if LShouldRedraw then
      pbxDraw.Redraw;
  end;
end;

procedure TfrmPaintBoxViewer.pbxDrawMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  LShouldRedraw: Boolean;
begin
  inherited;
  if FIsMouseDown and Assigned(FOnMouseUp) then
  begin
    LShouldRedraw := False;
    FOnMouseUp(PointF(X, Y), LShouldRedraw);
    if LShouldRedraw then
      pbxDraw.Redraw;
  end;
end;

procedure TfrmPaintBoxViewer.Show(const ATitle, ADescription: string;
  const ADrawProc: TPaintBoxDrawProc; ABackgroundKind: TBackgroundKind;
  const AOptions: IViewerOptions);
begin
  BackgroundKind := ABackgroundKind;
  Options := AOptions;
  FDrawProc := ADrawProc;
  pbxDraw.Align := alClient;
  pbxDraw.Redraw;
  inherited Show(ATitle, ADescription);
end;

procedure TfrmPaintBoxViewer.Show(const ATitle, ADescription: string;
  const ADrawWidth, ADrawHeight: Integer; const ADrawProc: TPaintBoxDrawProc;
  ABackgroundKind: TBackgroundKind; const AOptions: IViewerOptions);
begin
  BackgroundKind := ABackgroundKind;
  Options := AOptions;
  FDrawProc := ADrawProc;
  pbxDraw.Align := alNone;
  pbxDraw.Width := Round(ADrawWidth * pbxDraw.ScaleFactor);
  pbxDraw.Height := Round(ADrawHeight * pbxDraw.ScaleFactor);
  pbxDraw.Redraw;
  inherited Show(ATitle, ADescription);
end;

end.
