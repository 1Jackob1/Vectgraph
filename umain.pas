unit UMain;
//vahob
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Buttons, EditBtn, StdCtrls, UTool, UTransform, UComparator, UDefine,
  UCreateAttributes;

type

  { TVectGraph }

  TVectGraph = class(TForm)
    DrawArea: TPaintBox;
    Scale: TEdit;
    HorizontalScroll: TScrollBar;
    VerticalScroll: TScrollBar;
    TToolZoomLoupe: TSpeedButton;
    TToolUnZoomLoupe: TSpeedButton;
    ToolsBar: TToolBar;
    AttributesBar: TToolBar;
    procedure DrawAreaMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawAreaMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DrawAreaMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawAreaPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BttnToolClck(Sender: TObject);
    procedure HorizontalScrollScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure TToolZoomLoupeClick(Sender: TObject);
    procedure TToolUnZoomLoupeClick(Sender: TObject);
    procedure ScrolCalc;
    procedure VerticalScrollScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  VectGraph: TVectGraph;
  IsDrawing: boolean;
  ToolCode: Integer;
  MaxDACoor, MinDACoor: TDoublePoint;
  HorBarPos, VertBarPos: Integer;
implementation

{$R *.lfm}

{ TVectGraph }

procedure TVectGraph.FormCreate(Sender: TObject);
var i:integer;
    Bttn: TSpeedButton;
    BttnImg: TPicture;
begin
  IsDrawing:=false;
  for i:=0 to High(ToolConst.Tools) do begin

    Bttn:=TSpeedButton.Create(ToolsBar);
    Bttn.Parent:=ToolsBar;
    Bttn.Width:=TOOL_BUTTON_SIZE;
    Bttn.Height:=TOOL_BUTTON_SIZE;
    Bttn.Align:=alLeft;
    Bttn.Tag:=i;
    Bttn.OnClick:= @BttnToolClck;
    Bttn.GroupIndex:=1;

    BttnImg:= TPicture.Create;
    BttnImg.LoadFromFile(ToolConst.Tools[i].ClassName+'.png');
    Bttn.Glyph:=BttnImg.Bitmap;

    end;

    objTransform:=TTransform.Create(DrawArea.Width, DrawArea.Height);
    EditFigure:=TEditCreate.Create;
    EditFigure.SelectAttrs(TPersistent(ToolConst.Tools[ToolCode].CreateAttributes));
    EditFigure.SetBrushColor(START_FILL_COLOR);
    EditFigure.SetPenColor(START_LINE_COLOR);

    Scale.Caption:='Масштаб '+FloatToStr(objTransform.Zoom);

end;

procedure TVectGraph.DrawAreaMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then begin
    IsDrawing:=true;
    ToolConst.Tools[ToolCode].MouseDown(Point(X,Y));
  end;
end;

procedure TVectGraph.DrawAreaMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if IsDrawing then begin
    ToolConst.Tools[ToolCode].MouseMove(Point(X,Y));
    DrawArea.Invalidate;
  end;
end;

procedure TVectGraph.DrawAreaMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if IsDrawing then begin
  IsDrawing:=false;
  ToolConst.Tools[ToolCode].MouseUp(Point(DrawArea.Width, DrawArea.Height));
  DrawArea.Invalidate;
  Scale.Caption:='Масштаб '+FloatToStr(objTransform.Zoom);
  end;
end;

procedure TVectGraph.DrawAreaPaint(Sender: TObject);
var
    i: Integer;
begin
  for i:=0 to Length(FigureItems)-1 do begin
     if i = 0 then begin
     MaxDACoor:=FigureItems[0].MaxCoor;
     MinDACoor:=FigureItems[0].MinCoor;
     end
     else begin
       MaxDACoor:=MaxPoint(FigureItems[i].MaxCoor, MaxDACoor);
       MinDACoor:=MinPoint(FigureItems[i].MinCoor, MinDACoor);
     end;
     FigureItems[i].Draw(DrawArea.Canvas);
   end;
end;

procedure TVectGraph.BttnToolClck(Sender: TObject);
begin
   ToolCode:=TSpeedButton(Sender).Tag;
   EditFigure.Destroy;
   EditFigure:=TEditCreate.Create;
   EditFigure.SelectAttrs(TPersistent(ToolConst.Tools[ToolCode].CreateAttributes));
end;

procedure TVectGraph.HorizontalScrollScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  DrawArea.Invalidate;
end;

procedure TVectGraph.TToolZoomLoupeClick(Sender: TObject);
begin
  objTransform.ZoomLoupe;
  Scale.Caption:='Масштаб '+FloatToStr(objTransform.Zoom);
  DrawArea.Invalidate;
end;

procedure TVectGraph.TToolUnZoomLoupeClick(Sender: TObject);
begin
  objTransform.UnZoomLoupe;
  Scale.Caption:='Масштаб '+FloatToStr(objTransform.Zoom);
  DrawArea.Invalidate;
end;

procedure TVectGraph.ScrolCalc;
var
 DARect: TDRect;
 DiffPoint, DATop,DABottom: TDoublePoint;
 DiffXY: Integer;
 begin
 DARect := ToDRect(MinDACoor, MaxDACoor);

  DATop := objTransform.S2W(Point(0, 0));
  if DARect.Top.X > DATop.x then DARect.top.x := DATop.x;
  if DARect.Top.y > DATop.y then DARect.Top.y := DATop.y;
  DABottom := objTransform.S2W(Point(DrawArea.Width, DrawArea.Height));
  if DARect.Bottom.x < DABottom.x then DARect.Bottom.x := DABottom.x;
  if DARect.Bottom.y < DABottom.y then DARect.Bottom.y := DABottom.y;
  DiffPoint.X := DARect.Bottom.x - DARect.Top.x;
  DiffPoint.Y := DARect.Bottom.y - DARect.Top.y;
  if DiffPoint.x * DiffPoint.y = 0 then exit;

  DiffXY := HorizontalScroll.Max - HorizontalScroll.Min;
  HorizontalScroll.PageSize := round(DrawArea.Width / (DiffPoint.X * objTransform.Zoom) * DiffXY);
  HorizontalScroll.Visible := HorizontalScroll.PageSize < DiffXY;
  if HorizontalScroll.PageSize < DiffXY then
  begin
    if (HorBarPos = HorizontalScroll.Position) then
      HorizontalScroll.Position := round(((-1) * (objTransform.Offset.X + DARect.Top.x)) / DiffPoint.X * DiffXY)
    else
      objTransform.Offset.X := (-1) * (HorizontalScroll.Position / DiffXY * DiffPoint.X + DARect.Top.x);
    HorBarPos := HorizontalScroll.Position;
  end;

  DiffXY := VerticalScroll.Max - VerticalScroll.Min;
  VerticalScroll.PageSize := round(DrawArea.Height / (DiffPoint.Y * objTransform.Zoom) * DiffXY);
  VerticalScroll.Visible := VerticalScroll.PageSize < DiffXY;
  if VerticalScroll.PageSize < DiffXY then
  begin
    if (VertBarPos = VerticalScroll.Position) then
      VerticalScroll.Position := round(((-1) * (objTransform.Offset.Y + DARect.Top.y)) / DiffPoint.Y *DiffXY)
    else
      objTransform.Offset.Y := (-1) * (VerticalScroll.Position / DiffXY * DiffPoint.Y + DARect.Top.y);
    VertBarPos := VerticalScroll.Position;
  end;
 end;

procedure TVectGraph.VerticalScrollScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  DrawArea.Invalidate;
end;

end.

