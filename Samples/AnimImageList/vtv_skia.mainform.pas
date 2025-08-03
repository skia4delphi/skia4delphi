unit vtv_skia.mainform;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Skia, Vcl.Skia, VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL, VirtualTrees,
  VirtualTrees.Types, Vcl.BaseImageCollection,
  Vcl.ImageCollection, System.ImageList, Vcl.ImgList, Vcl.VirtualImageList,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Menus, Vcl.ToolWin, Vcl.Samples.Spin,
  VCL.Skia.AnimatedImageList, Vcl.Skia.AnimatedBitmap, Vcl.ExtCtrls;

type
  TForm12 = class(TForm)
    VST: TVirtualStringTree;
    SkAnimatedImageList1: TSkAnimatedImageList;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    TreeView1: TTreeView;
    ABWait: TSkAnimatedBitmap;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    SkAnimatedBitmap1: TSkAnimatedBitmap;
    ABSnail: TSkAnimatedBitmap;
    ABDuck: TSkAnimatedBitmap;
    ABBanana: TSkAnimatedBitmap;
    ABRocket: TSkAnimatedBitmap;
    Panel1: TPanel;
    Button1: TButton;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    SpinEdit1: TSpinEdit;
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
        Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode;
        CellRect: TRect; var ContentRect: TRect);
    procedure VSTBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor;
      var EraseAction: TItemEraseAction);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind:
        TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex:
        TImageIndex);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
        var InitialStates: TVirtualNodeInitStates);
    procedure VSTMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta:
        Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure ABWaitAnimationProcess(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }

  end;

var
  Form12: TForm12;

implementation
uses
  Vcl.Imaging.PngImage,
  VCL.Skia.DesignTime.Editor.Image,
  System.UITypes,
  System.Math;

{$R *.dfm}

procedure TForm12.ABWaitAnimationProcess(Sender: TObject);
var
  lnode : PVirtualNode;
begin
  lnode := TSkCustomAnimatedBitmap(Sender).tag_pointer;
  if not assigned(lnode) then exit;
//  Vst.InvalidateNode(lnode);
  { or invalidate whole column if single column is having animated bitmap }
  Vst.InvalidateColumn(1);
end;

procedure TForm12.Button1Click(Sender: TObject);
begin
  Button1.ImageIndex := (Button1.ImageIndex + 1) mod Button1.Images.Count;
end;

procedure TForm12.Button2Click(Sender: TObject);
var
  LS: TBytes;
begin
  Ls := [];
  SkImageEditorForm.ShowModal(LS);
end;

procedure TForm12.CheckBox1Click(Sender: TObject);
begin
  Button1.enabled := CheckBox1.Checked;
end;

procedure TForm12.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  var cpos := ScreenToClient(MousePos);
  if ControlAtPos(cpos,false,true,true) = SpinEdit1 then
  begin
    SpinEdit1.Value := EnsureRange(SpinEdit1.Value + sign(WheelDelta) ,SpinEdit1.MinValue,SpinEdit1.MaxValue);
  end;
end;

procedure TForm12.SpinEdit1Change(Sender: TObject);
begin
  SkAnimatedImageList1.SetSize(SpinEdit1.Value,SpinEdit1.Value);
end;

procedure TForm12.VSTBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas:
    TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode:
    TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  lrect: TRect;
  LBlendFunc: TBlendFunction;
  LRendered: TBitmap;
begin
  if not assigned(node) then exit;
  case Column of
    1: begin
      case node.Index of
        0: begin
          lrect := TRect.Create(0,0,CellRect.Width,CellRect.Height);
          Lrendered := ABWait.DoRender(CellRect.Width,CellRect.Height,
          procedure(Var ABg: Tbitmap)
          begin
            ABg.Canvas.CopyRect(lrect,TargetCanvas,CellRect);
          end);
          TargetCanvas.CopyRect(CellRect,Lrendered.Canvas,lrect);
        end;
        1 : begin
          lrect := TRect.Create(0,0,CellRect.Width,CellRect.Height);
          Lrendered := ABSnail.DoRender(CellRect.Width,CellRect.Height,
          procedure(Var ABg: Tbitmap)
          begin
            ABg.Canvas.CopyRect(lrect,TargetCanvas,CellRect);
          end);
          TargetCanvas.CopyRect(CellRect,Lrendered.Canvas,lrect);
        end;
        2: begin
          lrect := TRect.Create(0,0,CellRect.Width,CellRect.Height);
          Lrendered := ABDuck.DoRender(CellRect.Width,CellRect.Height,
          procedure(Var ABg: Tbitmap)
          begin
            ABg.Canvas.CopyRect(lrect,TargetCanvas,CellRect);
          end);
          TargetCanvas.CopyRect(CellRect,Lrendered.Canvas,lrect);
        end;
        3: begin
          lrect := TRect.Create(0,0,CellRect.Width,CellRect.Height);
          Lrendered := ABBanana.DoRender(CellRect.Width,CellRect.Height,
          procedure(Var ABg: Tbitmap)
          begin
            ABg.Canvas.CopyRect(lrect,TargetCanvas,CellRect);
          end);
          TargetCanvas.CopyRect(CellRect,Lrendered.Canvas,lrect);
        end;
        4: begin
          lrect := TRect.Create(0,0,CellRect.Width,CellRect.Height);
          Lrendered := ABRocket.DoRender(CellRect.Width,CellRect.Height,
          procedure(Var ABg: Tbitmap)
          begin
            ABg.Canvas.CopyRect(lrect,TargetCanvas,CellRect);
          end);
          TargetCanvas.CopyRect(CellRect,Lrendered.Canvas,lrect);
        end;
      end;
    end;
  end;
end;

procedure TForm12.VSTBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
ItemRect: TRect; var ItemColor: TColor;
  var EraseAction: TItemEraseAction);
var
  lrect: TRect;

begin
  case node.Index of
    0: begin
      EraseAction := TItemEraseAction.eaColor;
      ItemColor := TcolorRec.Greenyellow;
    end;
    1: begin
      EraseAction := TItemEraseAction.eaColor;
      ItemColor := TcolorRec.SysGradientActiveCaption;
    end;
    2: begin
      EraseAction := TItemEraseAction.eaColor;
      ItemColor := TcolorRec.Cornflowerblue;
    end;
    3: begin
      EraseAction := TItemEraseAction.eaColor;
      ItemColor := TcolorRec.Lightpink;
    end;
    else
      EraseAction := TItemEraseAction.eaDefault;
  end;
end;

procedure TForm12.VSTGetImageIndex(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted:
    Boolean; var ImageIndex: TImageIndex);
begin
  if not assigned(node) then exit;
  case Kind of
    ikNormal: ;
    ikSelected: ;
    ikState:  exit ;
    ikOverlay: exit ;
  end;
  case Column of
    2: begin
      ImageIndex := Node.Index;
    end;
  end;
end;

procedure TForm12.VSTInitNode(Sender: TBaseVirtualTree; ParentNode, Node:
    PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  case Node.Index of
    0: begin
      ABWait.tag_pointer := Node;
    end;
    1: begin
      ABSnail.tag_pointer := Node;
    end;
    2: begin
      ABDuck.tag_pointer := Node;
    end;
    3: begin
      ABBanana.tag_pointer := Node;
    end;
    4: begin
      ABRocket.tag_pointer := Node;
    end;
  end;
end;

procedure TForm12.VSTMouseWheel(Sender: TObject; Shift: TShiftState;
    WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  var lnode := VST.GetNodeAt(Vst.ScreenToClient(MousePos));
  if assigned(lnode) then
  begin
    case sign(WheelDelta) of
      1: begin
        Vst.NodeHeight[lnode] := Vst.NodeHeight[lnode] + 1;
      end;
      -1 : begin
        Vst.NodeHeight[lnode] := max(18,vst.NodeHeight[lnode] -1);
      end;
    end;
  end;
end;

end.
