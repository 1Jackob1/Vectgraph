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
   // DrawArea.Invalidate;
end;

procedure TVectGraph.BttnToolClck(Sender: TObject);
begin
   ToolCode:=TSpeedButton(Sender).Tag;
   //if ((ToolCode <> 6) or(ToolCode<>5)) then
   EditFigure.SelectAttrs(TPersistent(ToolConst.Tools[ToolCode].CreateAttributes));
   ToolCode:=TSpeedButton(Sender).Tag;
end;



end.

