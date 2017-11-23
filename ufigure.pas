unit UFigure;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, FPCanvas, UTransform, UDefine, UComparator;

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

  TSpecialRect = class(TFigure)
    constructor Create;
    procedure Draw(ACanvas: TCanvas); override;
  end;

var
  CurrentStyles: Styles;
  SelectPoint, EndSelPoint: TPoint;

implementation

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
  cond1, cond2, cond3, cond4, cond5: boolean;
begin
  cond1 := ((FAPoint <= FFAPoint) or (FAPoint >= FFAPoint));
  cond2 := ((SAPoint <= FSAPoint) or (SAPoint >= FSAPoint));
  cond3 := (SAPoint <= FFAPoint);
  cond4 := ((FAPoint.X >= FFAPoint.X) and (SAPoint.X <= FFAPoint.X));
  cond5 := ((FAPoint.Y >= FFAPoint.Y) and (SAPoint.Y <= FFAPoint.Y));
  if (cond1 and cond2 and cond3) or cond4 or cond5 then
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
begin
  inherited Draw(ACanvas);
  ACanvas.Rectangle(ToRect(objTransform.W2S(DPRect.Top),
    objTransform.W2S(DPRect.Bottom)));
end;

{ TRoundRect }

constructor TRoundRect.Create;
begin
  SetDefFigrStyles;
  Flexure := CurrentStyles.Flexure;
end;

procedure TRoundRect.Draw(ACanvas: TCanvas);
begin
  inherited Draw(ACanvas);
  ACanvas.RoundRect(ToRect(objTransform.W2S(DPRect.Top),
    objTransform.W2S(DPRect.Bottom)), Flexure, Flexure);
end;

{ TEllipse }

constructor TEllipse.Create;
begin
  SetDefFigrStyles;
end;

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
  inherited Draw(ACanvas);
  ACanvas.Ellipse(ToRect(objTransform.W2S(DPRect.Top),
    objTransform.W2S(DPRect.Bottom)));
end;

procedure TEllipse.selectfig(FAPoint, SAPoint, FFAPoint, FSAPoint: TPoint);
var
  A, B, FCoor, SCoor: real;
begin
  {A := (FSAPoint.X - FFAPoint.X) / 2;
  B := (FSAPoint.Y - FFAPoint.Y) / 2;
  FCoor := (FApoint.X - FFAPoint.X - A) / A;
  SCoor := (FApoint.Y - FFAPoint.Y - B) / B;
  if FCoor * FCoor + SCoor * SCoor <= 1 then
    IsSelected := True;}
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
begin
  MaxCoor := ToDP(0, 0);
  MinCoor := ToDP(0, 0);
  ACanvas.Pen.Color := LineColor;
  ACanvas.Pen.Width := LineWidth;
  ACanvas.Pen.Style := LineType;
  for i := 0 to High(Vert) - 1 do
  begin
    ACanvas.Line(objTransform.W2S(Vert[i]), objTransform.W2S(Vert[i + 1]));
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
