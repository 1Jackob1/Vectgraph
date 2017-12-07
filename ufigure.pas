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
    FillColor: TColor;
    LineType: TFPPenStyle;
    FillType: TFPBrushStyle;
    LineWidth: integer;
  public
    MaxCoor, MinCoor: TDoublePoint;
    vert: array of TDoublePoint;
    IsSelected: boolean;

    procedure MouseMove(ADPoint: TDoublePoint); virtual; abstract;
    procedure NextPoint(ADPoint: TDoublePoint); virtual; abstract;
    procedure MouseUp(ADPoint: TDoublePoint); virtual;
    procedure Draw(ACanvas: TCanvas); virtual; abstract;
    procedure SetDefFigrStyles;
    procedure selectfig(FAPoint, SAPoint, FFAPoint, FSAPoint: TPoint); virtual;
    procedure selectfig(FAPoint, SAPoint: TPoint; AVert: array of TDoublePoint);
      virtual; abstract;
    property FLineWidth: integer read LineWidth write LineWidth;
    property FLineColor: TColor read LineColor write LineColor;
    property FLineType: TFPPenStyle read LineType write LineType;

  end;

  { TSmlrRect }

  TSmlrRect = class(TFigure)
  private
    DPRect: TDRect;
    RFillType: TFPBrushStyle;
    RFillColor: TColor;
  public
    procedure NextPoint(ADPoint: TDoublePoint); override;
    procedure MouseUp(ADPoint: TDoublePoint); override;
    procedure Draw(ACanvas: TCanvas); override;
    procedure selectfig(FAPoint, SAPoint, FFAPoint, FSAPoint: TPoint); override;
    property FFillColor: TColor read RFillColor write RFillColor;
    property FFillType: TFPBrushStyle read RFillType write RFillType;
    property FLineColor: TColor read LineColor write LineColor;

  end;

  { TRectangle }

  TRectangle = class(TSmlrRect)
  public
    constructor Create;
    procedure Draw(ACanvas: TCanvas); override;
  published
    property FLineType: TFPPenStyle read LineType write LineType;
    property FFillType: TFPBrushStyle read RFillType write RFillType;
    property FLineWidth: integer read LineWidth write LineWidth;
    property FLineColor: TColor read LineColor write LineColor;
    property FFillColor: TColor read RFillColor write RFillColor;

  end;

  { TRoundRect }

  TRoundRect = class(TSmlrRect)
  private
    Flexure: integer;
  public
    constructor Create;
    procedure Draw(ACanvas: TCanvas); override;
  published
    property FLineType: TFPPenStyle read LineType write LineType;
    property FFillType: TFPBrushStyle read RFillType write RFillType;
    property FFlexure: integer read Flexure write Flexure;
    property FLineWidth: integer read LineWidth write LineWidth;
    property FLineColor: TColor read LineColor write LineColor;
    property FFillColor: TColor read FillColor write FillColor;

  end;

  { TEllipse }

  TEllipse = class(TSmlrRect)
  public
    constructor Create;
    procedure Draw(ACanvas: TCanvas); override;
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
    procedure Draw(ACanvas: TCanvas); override;
    procedure selectfig(FAPoint, SAPoint: TPoint; AVert: array of TDoublePoint);
      override;
    procedure NextLine(ADPoint: TDoublePoint);
  published
    property FLineType: TFPPenStyle read LineType write LineType;
    property FLineWidth: integer read LineWidth write LineWidth;
    property FLineColor: TColor read LineColor write LineColor;

  end;

  { TSpecialRect }

  TSpecialRect = class(TSmlrRect)
    constructor Create;
    procedure Draw(ACanvas: TCanvas); override;
  end;

procedure Swap(var A, B: TFigure);

var
  CurrentStyles: Styles;
  SelectPoint, EndSelPoint: TPoint;

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

procedure TFigure.SetDefFigrStyles;
begin
  LineWidth := CurrentStyles.LineWidth;
  LineColor := CurrentStyles.LineColor;
  FillColor := CurrentStyles.FillColor;
  LineType := CurrentStyles.LineStyle;
  FillType := CurrentStyles.FillStyle;
  IsSelected := False;
end;

procedure TFigure.selectfig(FAPoint, SAPoint, FFAPoint, FSAPoint: TPoint);
var
  cond1, cond2: boolean;
  R1,R2:TRect;
begin
  R2:=ToRect(FSAPoint,FFAPoint);
  R1:=ToRect(SAPoint,FAPoint);
<<<<<<< HEAD
<<<<<<< HEAD
  cond1 := ((R1.Top>=R2.Top) and (R1.Top<=R2.Bottom)) or
           ((R1.Bottom>=R2.Top) and (R1.Bottom<=R2.Bottom));
=======
  cond1 := ((R1.Top<=R2.Top) and (R1.Bottom>=R2.Bottom));
>>>>>>> d9e7cd10c18151bcd73fe60a2bc27dedf2ff70ce

  cond2 := ((R1.Left<=R2.Left) and (R1.Right>=R2.Right));

<<<<<<< HEAD
  cond3 := (FAPoint>=FFAPoint) and (SAPoint<=FSAPoint);

  cond4 := ((R1.Top<R2.Top) and (R1.Bottom<R2.Top)) or
           ((R1.Top>R2.Bottom) and (R1.Bottom>R2.Bottom));

  cond5 := ((R1.Left<R2.Left) and (R1.Right<R2.Left)) or
           ((R1.Left>R2.Right) and (R1.Right>R2.Right));
  //if (FSAPoint.X = 0) and (FSAPoint.Y = 0) then
  //  cond4:=True;
  if (cond1 or cond2 or cond3) and not cond4 and not cond5 then
=======
  if cond1 and cond2 then
>>>>>>> d9e7cd10c18151bcd73fe60a2bc27dedf2ff70ce
=======
  cond1 := ((R1.Top<=R2.Top) and (R1.Bottom>=R2.Bottom));

  cond2 := ((R1.Left<=R2.Left) and (R1.Right>=R2.Right));

  if cond1 and cond2 then
>>>>>>> d9e7cd10c18151bcd73fe60a2bc27dedf2ff70ce
    IsSelected := True;
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

procedure TSmlrRect.Draw(ACanvas: TCanvas);
begin
  SetDefFigrStyles;
  with ACanvas do
  begin
    Pen.Width := LineWidth;
    Pen.Style := LineType;
    Brush.Style := FillType;
    Pen.Color := LineColor;
    Brush.Color := FillColor;
    MaxCoor := MaxPoint(DPRect.Top, DPRect.Bottom);
    MinCoor := MinPoint(DPRect.Top, DPRect.Bottom);
  end;
end;

procedure TSmlrRect.selectfig(FAPoint, SAPoint, FFAPoint, FSAPoint: TPoint);
begin
  inherited selectfig(FAPoint, SAPoint, FFAPoint, FSAPoint);
end;

{ TRectangle }

constructor TRectangle.Create;
begin
  SetDefFigrStyles;
end;

procedure TRectangle.Draw(ACanvas: TCanvas);
var
  selectrect: TRect;
begin
  inherited Draw(ACanvas);
  ACanvas.Rectangle(ToRect(objTransform.W2S(DPRect.Top),
    objTransform.W2S(DPRect.Bottom)));
  if IsSelected then begin
    ACanvas.Brush.Style:=bsClear;
    ACanvas.Pen.Style:=psDashDotDot;
    selectrect:=ToRect(objTransform.W2S(DPRect.Top), objTransform.W2S(DPRect.Bottom));
    ACanvas.Rectangle(
      selectrect.TopLeft.x-2,selectrect.TopLeft.y-2,
      selectrect.BottomRight.x+2,selectrect.BottomRight.y+2);
  end;
end;

{ TRoundRect }

constructor TRoundRect.Create;
begin
  SetDefFigrStyles;
  Flexure := CurrentStyles.Flexure;
end;

procedure TRoundRect.Draw(ACanvas: TCanvas);
var
  selectrect: TRect;
begin
  inherited Draw(ACanvas);
  ACanvas.RoundRect(ToRect(objTransform.W2S(DPRect.Top),
    objTransform.W2S(DPRect.Bottom)), Flexure, Flexure);
  if IsSelected then begin
    ACanvas.Brush.Style:=bsClear;
    ACanvas.Pen.Style:=psDashDotDot;
    selectrect:=ToRect(objTransform.W2S(DPRect.Top),
      objTransform.W2S(DPRect.Bottom));
    ACanvas.Rectangle(selectrect.TopLeft.x-2,selectrect.TopLeft.y-2,
                      selectrect.BottomRight.x+2,selectrect.BottomRight.y+2);
  end;
end;

{ TEllipse }

constructor TEllipse.Create;
begin
  SetDefFigrStyles;
end;

procedure TEllipse.Draw(ACanvas: TCanvas);
var
  selectrect: TRect;
begin
  inherited Draw(ACanvas);
  ACanvas.Ellipse(ToRect(objTransform.W2S(DPRect.Top),
    objTransform.W2S(DPRect.Bottom)));
  if IsSelected then begin;
  ACanvas.Brush.Style:=bsClear;
  ACanvas.Pen.Style:=psDashDotDot;
  ACanvas.Pen.Width:=CurrentStyles.LineWidth+5;
  selectrect:=ToRect(objTransform.W2S(DPRect.Top),
    objTransform.W2S(DPRect.Bottom));
  ACanvas.Ellipse(selectrect.TopLeft.x-2,selectrect.TopLeft.y-2,
                    selectrect.BottomRight.x+2,selectrect.BottomRight.y+2);
  end;
end;

procedure TEllipse.selectfig(FAPoint, SAPoint, FFAPoint, FSAPoint: TPoint);
begin
  inherited selectfig(FAPoint, SAPoint, FFAPoint, FSAPoint);
end;

{ TPolyLine }

constructor TPolyLine.Create;
begin
  LineWidth := CurrentStyles.LineWidth;
  LineType := CurrentStyles.LineStyle;
  LineColor := CurrentStyles.LineColor;
end;

procedure TPolyLine.NextPoint(ADPoint: TDoublePoint);
begin
  SetLength(Vert, 2);
  Vert[0] := ADPoint;
  Vert[1] := Vert[0];
end;

procedure TPolyLine.Draw(ACanvas: TCanvas);
var
  i: integer;
  FPoint,SPoint:TPoint;
begin
  MaxCoor := ToDP(0, 0);
  MinCoor := vert[0];
  ACanvas.Pen.Color := LineColor;
  ACanvas.Pen.Width := LineWidth;
  ACanvas.Pen.Style := LineType;
  for i := 0 to High(Vert) - 1 do
  begin
    FPoint:=objTransform.W2S(Vert[i]);
    SPoint:=objTransform.W2S(Vert[i+1]);
    ACanvas.Line(FPoint, SPoint);
    if IsSelected then begin
      ACanvas.Brush.Style:=bsSolid;
      ACanvas.Brush.Color:=clBlack;
      ACanvas.Ellipse(FPoint.X-2, FPoint.Y-2,FPoint.X+5, FPoint.Y+5);
      ACanvas.Ellipse(SPoint.X-2, SPoint.Y-2,SPoint.X+5, SPoint.Y+5);
    end;
    MinCoor := MinPoint(Vert[i], MinCoor);
    MaxCoor := MaxPoint(Vert[i], MaxCoor);
    MinCoor := MinPoint(Vert[i + 1], MinCoor);
    MaxCoor := MaxPoint(Vert[i + 1], MaxCoor);
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
  tmp: TDoublePoint;
begin
    for i := 0 to Length(AVert) - 1 do
    begin
      if (objTransform.W2S(AVert[i]) <= FAPoint) and
        (objTransform.W2S(AVert[i]) >= SAPoint) and (not IsSelected) then
        IsSelected := True;
    end;

end;

{TSpecialRect}
constructor TSpecialRect.Create;
begin

end;

procedure TSpecialRect.Draw(ACanvas: TCanvas);
begin
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := clBlack;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Rectangle(SelectPoint.X, SelectPoint.Y, EndSelPoint.X, EndSelPoint.Y);
end;



initialization

  RegisterClass(TPolyLine);
  RegisterClass(TRectangle);
  RegisterClass(TRoundRect);
  RegisterClass(TEllipse);

end.
