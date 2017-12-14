unit UTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, FPCanvas, UFigure, UDefine, UTransform, UComparator,
  UCreateAttributes;

type

  { TTool }

  TTool = class
  private
    objFigure: TFigure;
  public
    function CreateAttributes: TPersistent; virtual;
    procedure MouseMove(APoint: TPoint); virtual; abstract;
    procedure MouseDown(APoint: TPoint); virtual; abstract;
    procedure MouseUp(APoint: TPoint); virtual;
  end;

  TToolClass = class of TTool;
  TToolArr = array of TTool;

  { TToolReg }

  TToolReg = class
  private
    TToolArray: array of TTool;
  public
    procedure ToolReg(ATToolClass: TToolClass);
    property Tools: TToolArr read TToolArray;

  end;

  { TToolLine }

  TToolLine = class(TTool)
    function CreateAttributes: TPersistent; override;
    procedure MouseMove(APoint: TPoint); override;
    procedure MouseDown(APoint: TPoint); override;
    procedure MouseUp(APoint: TPoint); override;
  end;

  { TToolPencil}

  TToolPencil = class(TTool)
    function CreateAttributes: TPersistent; override;
    procedure MouseMove(APoint: TPoint); override;
    procedure MouseDown(APoint: TPoint); override;
    procedure MouseUp(APoint: TPoint); override;
  end;

  { TToolRectangle }

  TToolRectangle = class(TTool)
    function CreateAttributes: TPersistent; override;
    procedure MouseMove(APoint: TPoint); override;
    procedure MouseDown(APoint: TPoint); override;
    procedure MouseUp(APoint: TPoint); override;
  end;

  { TToolRoundRect }

  TToolRoundRect = class(TTool)
    function CreateAttributes: TPersistent; override;
    procedure MouseMove(APoint: TPoint); override;
    procedure MouseDown(APoint: TPoint); override;
    procedure MouseUp(APoint: TPoint); override;
  end;

  { TToolEllipse }

  TToolEllipse = class(TTool)
    function CreateAttributes: TPersistent; override;
    procedure MouseMove(APoint: TPoint); override;
    procedure MouseDown(APoint: TPoint); override;
    procedure MouseUp(APoint: TPoint); override;
  end;

  { TToolLoupe }

  TToolLoupe = class(TTool)
    procedure MouseMove(APoint: TPoint); override;
    procedure MouseDown(APoint: TPoint); override;
    procedure MouseUp(APoint: TPoint); override;
  end;

  { TToolHand }

  TToolHand = class(TTool)
  private
    DPoint: TDoublePoint;
  public
    procedure MouseMove(APoint: TPoint); override;
    procedure MouseDown(APoint: TPoint); override;
  end;

  {TToolSelection}

  TToolSelection = class(TTool)
    WasClick: Boolean;
    procedure MouseMove(APoint: TPoint); override;
    procedure MouseDown(APoint: TPoint); override;
    procedure MouseUp(APoint: TPoint); override;
  end;

  {TToolResize}

  TToolResize = class(TTool)
    procedure MouseMove(APoint: TPoint); override;
    procedure MouseDown(APoint: TPoint); override;
    procedure MouseUp(APoint: TPoint); override;
  end;


var
  ToolConst: TToolReg;
  tmpDP: TDoublePoint;
  FigureItems, History: array of TFigure;
  Delete: boolean;
  tmpSwap: TFigure;
  SelectedCount: integer;

implementation

uses UDRaw;

{ TTool }
procedure TTool.MouseUp(APoint: TPoint);
begin
end;

function TTool.CreateAttributes: TPersistent;
begin
  Result := objFigure;
end;

{ TToolLine }

function TToolLine.CreateAttributes: TPersistent;
begin
  objFigure := TPolyLine.Create;
  Result := inherited CreateAttributes;
end;

procedure TToolLine.MouseDown(APoint: TPoint);
var
  DP: TDoublePoint;
begin
  SetLength(FigureItems, Length(FigureItems) + 1);
  FigureItems[High(FigureItems)] := objFigure;
  DP := objTransform.S2W(APoint);
  FigureItems[High(FigureItems)].NextPoint(DP);
end;

procedure TToolLine.MouseMove(APoint: TPoint);
begin
  FigureItems[High(FigureItems)].MouseUp(objTransform.S2W(APoint));
end;

procedure TToolLine.MouseUp(APoint: TPoint);
begin
  inherited MouseUp(APoint);
end;


{ TToolPencil }

function TToolPencil.CreateAttributes: TPersistent;
begin
  objFigure := TPolyLine.Create;
  Result := inherited CreateAttributes;
end;

procedure TToolPencil.MouseDown(APoint: TPoint);
var
  DP: TDoublePoint;
begin
  SetLength(FigureItems, Length(FigureItems) + 1);
  FigureItems[High(FigureItems)] := objFigure;
  DP := objTransform.S2W(APoint);
  FigureItems[High(FigureItems)].NextPoint(DP);
end;

procedure TToolPencil.MouseMove(APoint: TPoint);
begin
  TPolyLine(FigureItems[High(FigureItems)]).NextLine(objTransform.S2W(APoint));
end;

procedure TToolPencil.MouseUp(APoint: TPoint);
begin
  inherited MouseUp(APoint);
end;

{ TToolRectangle }

function TToolRectangle.CreateAttributes: TPersistent;
begin
  objFigure := TRectangle.Create;
  Result := inherited CreateAttributes;
end;

procedure TToolRectangle.MouseDown(APoint: TPoint);
var
  DP: TDoublePoint;
begin
  SetLength(FigureItems, Length(FigureItems) + 1);
  FigureItems[High(FigureItems)] := objFigure;
  DP := objTransform.S2W(APoint);
  FigureItems[High(FigureItems)].NextPoint(DP);
end;

procedure TToolRectangle.MouseMove(APoint: TPoint);
begin
  TRectangle(FigureItems[High(FigureItems)]).MouseUp(objTransform.S2W(APoint));
end;

procedure TToolRectangle.MouseUp(APoint: TPoint);
begin
  inherited MouseUp(APoint);
end;

{ TToolRoundRect }

function TToolRoundRect.CreateAttributes: TPersistent;
begin
  objFigure := TRoundRect.Create;
  Result := inherited CreateAttributes;
end;

procedure TToolRoundRect.MouseDown(APoint: TPoint);
var
  DP: TDoublePoint;
begin
  SetLength(FigureItems, Length(FigureItems) + 1);
  FigureItems[High(FigureItems)] := objFigure;
  DP := objTransform.S2W(APoint);
  FigureItems[High(FigureItems)].NextPoint(DP);
end;

procedure TToolRoundRect.MouseMove(APoint: TPoint);
begin
  TRoundRect(FigureItems[High(FigureItems)]).MouseUp(objTransform.S2W(APoint));
end;

procedure TToolRoundRect.MouseUp(APoint: TPoint);
begin
  inherited MouseUp(APoint);
end;

{ TToolEllipse }
function TToolEllipse.CreateAttributes: TPersistent;
begin
  objFigure := TEllipse.Create;
  Result := inherited CreateAttributes;
end;

procedure TToolEllipse.MouseDown(APoint: TPoint);
var
  DP: TDoublePoint;
begin
  SetLength(FigureItems, Length(FigureItems) + 1);
  FigureItems[High(FigureItems)] := objFigure;
  DP := objTransform.S2W(APoint);
  FigureItems[High(FigureItems)].NextPoint(DP);
end;

procedure TToolEllipse.MouseMove(APoint: TPoint);
begin
  TEllipse(FigureItems[High(FigureItems)]).MouseUp(objTransform.S2W(APoint));
end;

procedure TToolEllipse.MouseUp(APoint: TPoint);
begin
  inherited MouseUp(APoint);
end;

{ TToolLoupe }

procedure TToolLoupe.MouseDown(APoint: TPoint);
begin
  objTransform.ALRect.TopLeft := APoint;
end;

procedure TToolLoupe.MouseMove(APoint: TPoint);
begin
  objTransform.ALRect.BottomRight := APoint;
end;

procedure TToolLoupe.MouseUp(APoint: TPoint);
begin
  objTransform.RegionLoupe(APoint.x, APoint.y, objTransform.ALRect);
end;

{ TToolHand }

procedure TToolHand.MouseDown(APoint: TPoint);
begin
  tmpDP := ToDP(APoint);
end;

procedure TToolHand.MouseMove(APoint: TPoint);
begin
  objTransform.Offset.X += (ToDP(APoint).X - tmpDP.X) / objTransform.Zoom;
  objTransform.Offset.Y += (ToDP(APoint).Y - tmpDP.Y) / objTransform.Zoom;
  tmpDP := ToDP(APoint);
end;

{ TToolSelection }

procedure TToolSelection.MouseDown(APoint: TPoint);
begin
  Delete := False;
  SetLength(FigureItems, Length(FigureItems) + 1);
  FigureItems[High(FigureItems)] := TSpecialRect.Create;
  SelectPoint := APoint;
  WasClick:=True;
end;

procedure TToolSelection.MouseMove(APoint: TPoint);
begin
  EndSelPoint := APoint;
  WasClick:=False;
end;

procedure TToolSelection.MouseUp(APoint: TPoint);
var
  i: integer;
  tmpPoint: TPoint;
begin
  if WasClick then EndSelPoint:=SelectPoint;
  SelectedCount := 0;
  WasClick:=False;;
  if(abs((SelectPoint.X - EndSelPoint.X) * (SelectPoint.Y - EndSelPoint.Y))<5) or
  (EndSelPoint.X=0) or (EndSelPoint.Y = 0) then
  begin
    EndSelPoint.X-=2;
    SelectPoint.X+=5;
    EndSelPoint.Y-=2;
    SelectPoint.Y+=5;
    WasClick:=True;
  end;
  if (SelectPoint.X < EndSelPoint.X) then
  begin
    if (SelectPoint.Y < EndSelPoint.Y) then
    begin
      tmpPoint := SelectPoint;
      SelectPoint := EndSelPoint;
      EndSelPoint := tmpPoint;
    end
    else
    begin
      tmpPoint := EndSelPoint;
      EndSelPoint.X := SelectPoint.X;
      SelectPoint.X := tmpPoint.X;
    end;
  end;


  if (SelectPoint.x > EndSelPoint.x) and (SelectPoint.y < EndSelPoint.y) then
  begin
    tmpPoint := EndSelPoint;
    EndSelPoint.Y := SelectPoint.Y;
    SelectPoint.y := tmpPoint.Y;
  end;



  for i := 0 to Length(FigureItems) - 2 do
  begin
    if (FigureItems[i] is TPolyLine) or (Length(FigureItems[i].vert) < 3) then
      FigureItems[i].selectfig(
        SelectPoint, EndSelPoint,
        objTransform.W2S(FigureItems[i].MaxCoor),
        objTransform.W2S(FigureItems[i].MinCoor))
    else
      FigureItems[i].selectfig(SelectPoint, EndSelPoint, FigureItems[i].vert);
    if FigureItems[i].IsSelected then
      SelectedCount += 1;
  end;
  EditSelectedFig;
  Delete := True;
  if WasClick then SelectPoint:=EndSelPoint;

end;

{ TToolResize }

procedure TToolResize.MouseDown(Apoint: TPoint);
begin
  tmpDP := ToDP(APoint);
end;

procedure TToolResize.MouseMove(Apoint: TPoint);
var
  i, j: integer;
begin
  for i := 0 to High(FigureItems) do
  begin
    if FigureItems[i].IsSelected then
      if Length(FigureItems[i].vert) > 0 then
      begin
        for j := 0 to High(FigureItems[i].vert) do
        begin
          FigureItems[i].vert[j].X += (ToDP(APoint).X - tmpDP.X) / objTransform.Zoom;
          FigureItems[i].vert[j].Y += (ToDP(APoint).Y - tmpDP.Y) / objTransform.Zoom;
          tmpDP := ToDP(APoint);
        end;
      end;
  end;
end;

procedure TToolResize.MouseUp(Apoint: TPoint);
begin

end;

{ TToolReg }

procedure TToolReg.ToolReg(ATToolClass: TToolClass);
begin
  SetLength(TToolArray, Length(TToolArray) + 1);
  TToolArray[High(TToolArray)] := ATToolClass.Create;
end;

initialization

  ToolConst := TToolReg.Create;
  ToolConst.ToolReg(TToolPencil);
  ToolConst.ToolReg(TToolLine);
  ToolConst.ToolReg(TToolRectangle);
  ToolConst.ToolReg(TToolRoundRect);
  ToolConst.ToolReg(TToolEllipse);
  ToolConst.ToolReg(TToolHand);
  ToolConst.ToolReg(TToolLoupe);
  ToolConst.ToolReg(TToolSelection);
  ToolConst.ToolReg(TToolResize)

end.
