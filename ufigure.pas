unit UFigure;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, FPCanvas, Types, UTransform, UDefine, UComparator;

type

  { TFigure }

  TFigure = class(TPersistent)
  private
    LineColor: TColor;
    LineType: TFPPenStyle;
    LineWidth: integer;
    RFillType: TFPBrushStyle;
    RFillColor: TColor;
    Flexure: integer;
  public
    MaxCoor, MinCoor: TDoublePoint;
    vert: array of TDoublePoint;
    IsSelected: boolean;
    Anchros:array of TPoint;
    function checkSelecetion: boolean;
    procedure changePoint(ADPoint: TDoublePoint; Code: Integer); virtual;
    procedure MouseMove(ADPoint: TDoublePoint); virtual; abstract;
    procedure NextPoint(ADPoint: TDoublePoint); virtual; abstract;
    procedure MouseUp(ADPoint: TDoublePoint); virtual;
    procedure Draw(ACanvas: TCanvas; Selected: boolean); virtual; abstract;
    procedure SetDefFigrStyles(Selected: boolean);
    procedure selectfig(FAPoint, SAPoint, FFAPoint, FSAPoint: TPoint); virtual;
    procedure selectfig(FAPoint, SAPoint: TPoint; AVert: array of TDoublePoint);
      virtual; abstract;
    procedure getFillColor(var AFillColor: TColor); virtual;
    procedure getFillType(var AFillType: TFPBrushStyle); virtual;
    property FLineWidth: integer read LineWidth write LineWidth;
    property FLineColor: TColor read LineColor write LineColor;
    property FLineType: TFPPenStyle read LineType write LineType;
    property FFillColor: TColor read RFillColor write RFillColor;
    property FFillType: TFPBrushStyle read RFillType write RFillType;
    property FFlexure: integer read Flexure write Flexure;

  end;

  { TSmlrRect }

  TSmlrRect = class(TFigure)
  private
    DPRect: TDRect;
    //Anchros: TFigure;
    //RFillType: TFPBrushStyle;
    //RFillColor: TColor;
  public
    Offs: integer;
    procedure NextPoint(ADPoint: TDoublePoint); override;
    procedure MouseUp(ADPoint: TDoublePoint); override;
    procedure Draw(ACanvas: TCanvas; Selected: boolean); override;
    procedure selectfig(FAPoint, SAPoint, FFAPoint, FSAPoint: TPoint); override;
    procedure Frame(ACanvas: TCanvas);
    procedure getFillColor(var AFillColor: TColor); override;
    procedure getFillType(var AFillType: TFPBrushStyle); override;
    procedure changePoint(ADPoint: TDoublePoint; Code: Integer); override;

    property FFillColor: TColor read RFillColor write RFillColor;
    property FFillType: TFPBrushStyle read RFillType write RFillType;
    property FLineColor: TColor read LineColor write LineColor;


  end;

  { TRectangle }

  TRectangle = class(TSmlrRect)
  public
    constructor Create;
    procedure Draw(ACanvas: TCanvas; Selected: boolean); override;
    //procedure getFillColor(var AFillColor: TColor); override;
    //procedure getFillColor(var AFillType: TFPBrushStyle); override;
  published
    property FLineType: TFPPenStyle read LineType write LineType;
    property FFillType: TFPBrushStyle read RFillType write RFillType;
    property FLineWidth: integer read LineWidth write LineWidth;
    property FLineColor: TColor read LineColor write LineColor;
    property FFillColor: TColor read RFillColor write RFillColor;


  end;

  { TRoundRect }

  TRoundRect = class(TSmlrRect)
  public
    constructor Create;
    procedure Draw(ACanvas: TCanvas; Selected: boolean); override;
    //procedure getFillColor(var AFillColor: TColor); override;
    //procedure getFillColor(var AFillType: TFPBrushStyle); override;
  published
    property FLineType: TFPPenStyle read LineType write LineType;
    property FFillType: TFPBrushStyle read RFillType write RFillType;
    property FFlexure: integer read Flexure write Flexure;
    property FLineWidth: integer read LineWidth write LineWidth;
    property FLineColor: TColor read LineColor write LineColor;
    property FFillColor: TColor read RFillColor write RFillColor;

  end;

  { TEllipse }

  TEllipse = class(TSmlrRect)
  public
    constructor Create;
    procedure Draw(ACanvas: TCanvas; Selected: boolean); override;
    procedure selectfig(FAPoint, SAPoint, FFAPoint, FSAPoint: TPoint); override;
  published
    property FLineType: TFPPenStyle read LineType write LineType;
    property FFillType: TFPBrushStyle read RFillType write RFillType;
    property FLineWidth: integer read LineWidth write LineWidth;
    property FLineColor: TColor read LineColor write LineColor;
    property FFillColor: TColor read RFillColor write RFillColor;

  end;

  { TPolyLine }

  TPolyLine = class(TFigure)
  public
    constructor Create;
    procedure NextPoint(ADPoint: TDoublePoint); override;
    procedure MouseUp(ADPoint: TDoublePoint); override;
    procedure Draw(ACanvas: TCanvas; Selected: boolean); override;
    procedure selectfig(FAPoint, SAPoint: TPoint; AVert: array of TDoublePoint);
      override;
    procedure NextLine(ADPoint: TDoublePoint);
    procedure DrawFrame(ACanvas: TCanvas; FAPoint, SAPoint: TPoint);
  published
    property FLineType: TFPPenStyle read LineType write LineType;
    property FLineWidth: integer read LineWidth write LineWidth;
    property FLineColor: TColor read LineColor write LineColor;

  end;

  { TSpecialRect }

  TSpecialRect = class(TSmlrRect)
    constructor Create;
    procedure Draw(ACanvas: TCanvas; Selected: boolean); override;
  end;

procedure Swap(var A, B: TFigure);

var
  CurrentStyles, ForAllFigrStyles: Styles;
  SelectPoint, EndSelPoint: TPoint;
  FigureItems: array of TFigure;

implementation

procedure Swap(var A, B: TFigure);
var
  t: TFigure;
begin
  t := A;
  A := B;
  B := t;
end;

procedure TFigure.MouseUp(ADPoint: TDoublePoint);
begin
end;

procedure TFigure.SetDefFigrStyles(Selected: boolean);
begin
  //MaxCoor:=ToDP(0.0,0.0);
  //MinCoor:=ToDP(99999999999.9,99999999999.9);
  LineWidth := FLineWidth;
  LineColor := FLineColor;
  LineType := FLineType;

end;

procedure TFigure.selectfig(FAPoint, SAPoint, FFAPoint, FSAPoint: TPoint);
var
  cond1, cond2, cond3, cond4, cond5: boolean;
  R1, R2: TRect;
begin
  R2 := ToRect(FSAPoint, FFAPoint);
  R1 := ToRect(SAPoint, FAPoint);
  cond1 := ((R1.Top >= R2.Top) and (R1.Top <= R2.Bottom)) or
    ((R1.Bottom >= R2.Top) and (R1.Bottom <= R2.Bottom));

  cond2 := ((R1.Left <= R2.Left) and (R1.Left >= R2.Right)) or
    ((R1.Right >= R2.Left) and (R1.Right <= R2.Right));

  cond3 := (FAPoint >= FFAPoint) and (SAPoint <= FSAPoint);

  cond4 := ((R1.Top < R2.Top) and (R1.Bottom < R2.Top)) or
    ((R1.Top > R2.Bottom) and (R1.Bottom > R2.Bottom));

  cond5 := ((R1.Left < R2.Left) and (R1.Right < R2.Left)) or
    ((R1.Left > R2.Right) and (R1.Right > R2.Right));

  if (cond1 or cond2 or cond3) and not cond4 and not cond5 then
    IsSelected := True;
end;

function TFigure.checkSelecetion: boolean;
begin
  Result := IsSelected;
  exit;
end;

procedure TFigure.getFillColor(var AFillColor: TColor);
begin

end;

procedure TFigure.getFillType(var AFillType: TFPBrushStyle);
begin

end;

procedure TFigure.changePoint(ADpoint: TDoublePoint; Code: Integer);
begin

end;

{ TSmlrRect }

procedure TSmlrRect.NextPoint(ADPoint: TDoublePoint);
begin
  DPRect := ToDRect(ADPoint, ADPoint);
end;

procedure TSmlrRect.MouseUp(ADPoint: TDoublePoint);
begin
  DPRect.Bottom := ADPoint;
end;

procedure TSmlrRect.Draw(ACanvas: TCanvas; Selected: boolean);
begin
  with ACanvas do
  begin
    Pen.Width := FLineWidth;
    Pen.Color := FLineColor;
    Pen.Style := FLineType;
    Brush.Style := FFillType;
    Brush.Color := FFillColor;
    MaxCoor := MaxPoint(DPRect.Top,DPRect.Bottom);
    MinCoor := MinPoint(DPRect.Top,DPRect.Bottom);
  end;
end;

procedure TSmlrRect.selectfig(FAPoint, SAPoint, FFAPoint, FSAPoint: TPoint);
begin
  inherited selectfig(FAPoint, SAPoint, FFAPoint, FSAPoint);
end;

procedure TSmlrRect.Frame(ACanvas: TCanvas);
var
  SelPoints: Array[0..1] of TPoint;
  i: Integer;
begin
  Offs := ACanvas.Pen.Width div 2 + 5;
  SetLength(Anchros,4);
  with ACanvas do
  begin
    Pen.Width := 2;
    Pen.Color := clBlack;
    Pen.Style := psDash;
    Brush.Style := bsClear;
  end;
  SelPoints[0]:=objTransform.W2S(DPRect.Top)- 2 - Offs;
  SelPoints[1]:=objTransform.W2S(DPRect.Bottom)+ 2 + Offs;
  ACanvas.Rectangle(
    SelPoints[0].x, SelPoints[0].y,
    SelPoints[1].x, SelPoints[1].y);
  Anchros[0]:=SelPoints[0];
  Anchros[1].X:=SelPoints[1].X;
  Anchros[1].Y:=SelPoints[0].Y;
  Anchros[2]:=SelPoints[1];
  Anchros[3].X:=SelPoints[0].X;
  Anchros[3].Y:=SelPoints[1].Y;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color:=clWhite;
  for i:=0 to 3 do begin
    ACanvas.Rectangle(Anchros[i].x-10, Anchros[i].y-10,Anchros[i].x+10, Anchros[i].y+10);
  end;

end;

procedure TSmlrRect.getFillColor(var AFillColor: TColor);
begin
  AFillColor := FFillColor;
end;

procedure TSmlrRect.getFillType(var AFillType: TFPBrushStyle);
begin
  AFillType := FFillType;
end;

procedure TSmlrRect.changePoint(ADPoint: TDoublePoint; Code: Integer);
begin
  case Code of
  0: DPRect.Top:=ADPoint;
  end;
end;

{ TRectangle }

constructor TRectangle.Create;
begin
  SetDefFigrStyles(False);
end;

procedure TRectangle.Draw(ACanvas: TCanvas; Selected: boolean);
var
a,b:TPoint;
begin
  inherited Draw(ACanvas, Selected);
  ACanvas.Rectangle(ToRect(objTransform.W2S(DPRect.Top),
    objTransform.W2S(DPRect.Bottom)));
    //a:=objTransform.W2S(MaxCoor);
    //b:=objTransform.W2S(MinCoor);
    //ACanvas.Rectangle(b.X,b.Y,a.X,a.Y);
  Offs := FLineWidth;
  if Selected then
    Frame(ACanvas);
end;

{ TRoundRect }

constructor TRoundRect.Create;
begin
  SetDefFigrStyles(False);
end;

procedure TRoundRect.Draw(ACanvas: TCanvas; Selected: boolean);
begin
  inherited Draw(ACanvas, Selected);
  ACanvas.RoundRect(ToRect(objTransform.W2S(DPRect.Top),
    objTransform.W2S(DPRect.Bottom)), Flexure, Flexure);
  if Selected then
    Frame(ACanvas);
end;

{ TEllipse }

constructor TEllipse.Create;
begin
  SetDefFigrStyles(False);
end;

procedure TEllipse.Draw(ACanvas: TCanvas; Selected: boolean);
begin
  inherited Draw(ACanvas, Selected);
  ACanvas.Ellipse(ToRect(objTransform.W2S(DPRect.Top),
    objTransform.W2S(DPRect.Bottom)));
  if Selected then
    Frame(ACanvas);
end;

procedure TEllipse.selectfig(FAPoint, SAPoint, FFAPoint, FSAPoint: TPoint);
begin
  inherited selectfig(FAPoint, SAPoint, FFAPoint, FSAPoint);
end;

{ TPolyLine }

constructor TPolyLine.Create;
begin
  LineWidth := FLineWidth;
  LineType := FLineType;
  LineColor := FLineColor;
end;

procedure TPolyLine.NextPoint(ADPoint: TDoublePoint);
begin
  SetLength(Vert, 2);
  Vert[0] := ADPoint;
  Vert[1] := Vert[0];
end;

procedure TPolyLine.Draw(ACanvas: TCanvas; Selected: boolean);
var
  i: integer;
  FPoint, SPoint, FRPoint, SRPoint: TPoint;
begin
  if Length(vert) = 0 then
    exit;
  MaxCoor := ToDP(0, 0);
  MinCoor := vert[0];
  ACanvas.Pen.Color := FLineColor;
  ACanvas.Pen.Width := FLineWidth;
  ACanvas.Pen.Style := FLineType;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Brush.Color := clWhite;
  for i := 0 to High(Vert) - 1 do
  begin
    FPoint := objTransform.W2S(Vert[i]);
    SPoint := objTransform.W2S(Vert[i + 1]);
    ACanvas.Line(FPoint, SPoint);
    MinCoor := MinPoint(Vert[i], MinCoor);
    MaxCoor := MaxPoint(Vert[i], MaxCoor);
    MinCoor := MinPoint(Vert[i + 1], MinCoor);
    MaxCoor := MaxPoint(Vert[i + 1], MaxCoor);
  end;
  if Selected then
    begin
      SRPoint := objTransform.W2S(MaxCoor);
      FRPoint := objTransform.W2S(MinCoor);
      DrawFrame(ACanvas,FRPoint,SRPoint);
    end;

end;

procedure TPolyLine.MouseUp(ADPoint: TDoublePoint);
begin
  Vert[High(Vert)] := ADPoint;
end;

procedure TPolyLine.NextLine(ADPoint: TDoublePoint);
begin
  SetLength(Vert, Length(Vert) + 1);
  Vert[High(Vert)] := ADPoint;
end;

procedure TPolyLine.selectfig(FAPoint, SAPoint: TPoint; AVert: array of TDoublePoint);
var
  i: integer;
begin
  for i := 0 to High(AVert) - 1 do
  begin
    if (objTransform.W2S(AVert[i]) <= FAPoint) and
      (objTransform.W2S(AVert[i]) >= SAPoint) and (not IsSelected) then
      IsSelected := True;
  end;
end;

procedure TPolyLine.DrawFrame(ACanvas: TCanvas; FAPoint, SAPoint: TPoint);
var
  Offs: Integer;
  i:Integer;
begin
  SetLength(Anchros,4);
  with ACanvas do
  begin
    Offs := Pen.Width div 2 + 5;
    Pen.Width := 2;
    Pen.Color := clBlack;
    Pen.Style := psDash;
    Brush.Style := bsClear;
  end;
  ACanvas.Rectangle(FAPoint.X - Offs, FAPoint.Y - Offs, SAPoint.X + Offs, SAPoint.Y + Offs);
  Anchros[0]:=FAPoint;
  Anchros[1].X:=SAPoint.X;
  Anchros[1].Y:=FAPoint.Y;
  Anchros[2]:=SAPoint;
  Anchros[3].X:=FAPoint.X;
  Anchros[3].Y:=SAPoint.Y;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color:=clWhite;
  for i:=0 to 3 do
    ACanvas.Rectangle(Anchros[i].x-10, Anchros[i].y-10,Anchros[i].x+10, Anchros[i].y+10);
end;

{TSpecialRect}
constructor TSpecialRect.Create;
begin

end;

procedure TSpecialRect.Draw(ACanvas: TCanvas; Selected: boolean);
begin
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Width := 1;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Rectangle(SelectPoint.X, SelectPoint.Y, EndSelPoint.X, EndSelPoint.Y);
end;



initialization

  RegisterClass(TPolyLine);
  RegisterClass(TRectangle);
  RegisterClass(TRoundRect);
  RegisterClass(TEllipse);

end.
