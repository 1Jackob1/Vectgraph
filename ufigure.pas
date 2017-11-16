unit UFigure;
//vahob
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, FPCanvas, UTransform, UDefine, UComparator;

type

  { TFigure }

  TFigure = class(TPersistent)
   private
     LineColor, FillColor: TColor;
     LineType: TFPPenStyle;
     FillType: TFPBrushStyle;
     LineWidth: Integer;
   public
     MaxCoor,MinCoor: TDoublePoint;
     procedure MouseMove(ADPoint: TDoublePoint);   virtual; abstract;
     procedure NextPoint(ADPoint: TDoublePoint);   virtual; abstract;
     procedure MouseUp(ADPoint: TDoublePoint);     virtual;
     procedure Draw(ACanvas: TCanvas);             virtual; abstract;
     procedure SetDefFigrStyles;
     property FLineWidth: Integer read LineWidth write LineWidth;
     property FLineColor: TColor read LineColor write LineColor;
     property FLineType:  TFPPenStyle read LineType write LineType;
  end;

  { TSmlrRect }

  TSmlrRect = class(TFigure)
    private
    DPRect:    TDRect;
    RFillType:  TFPBrushStyle;
    RFillColor: TColor;
    public
    //procedure MouseMove(ADPoint: TDoublePoint); override;
    procedure NextPoint(ADPoint: TDoublePoint); override;
    procedure MouseUp(ADPoint: TDoublePoint);   override;
    procedure Draw(ACanvas: TCanvas);           override;
    property FFillColor: TColor read RFillColor write RFillColor;
    property FFillType: TFPBrushStyle read RFillType write RFillType;
  end;

  { TRectangle }

  TRectangle = class(TSmlrRect)
    public
      constructor Create;
      {procedure MouseMove(ADPoint: TDoublePoint); override;
      procedure NextPoint(ADPoint: TDoublePoint); override;
      procedure MouseUp(ADPoint: TDoublePoint);   override;}
      procedure Draw(ACanvas: TCanvas);           override;
    published
      property FLineType: TFPPenStyle   read LineType write LineType;
      property FFillType: TFPBrushStyle read RFillType write RFillType;
  end;

  { TRoundRect }

  TRoundRect = class(TSmlrRect)
    private
      Flexure: Integer;
    public
      constructor Create;
      {procedure MouseMove(ADPoint: TDoublePoint); override;
      procedure NextPoint(ADPoint: TDoublePoint); override;
      procedure MouseUp(ADPoint: TDoublePoint);   override;}
      procedure Draw(ACanvas: TCanvas);           override;
    published
      property FLineType: TFPPenStyle   read LineType write LineType;
      property FFillType: TFPBrushStyle read RFillType write RFillType;
      property FFlexure:  Integer       read Flexure  write Flexure;
  end;

  { TEllipse }

  TEllipse = class(TSmlrRect)
     public
      constructor Create;
      {procedure MouseMove(ADPoint: TDoublePoint); override;
      procedure NextPoint(ADPoint: TDoublePoint); override;
      procedure MouseUp(ADPoint: TDoublePoint);   override;}
      procedure Draw(ACanvas: TCanvas);           override;
    published
      property FLineType: TFPPenStyle   read LineType write LineType;
      property FFillType: TFPBrushStyle read RFillType write RFillType;
  end;

  { TPolyLine }

  TPolyLine = class(TFigure)
   public
    vert: array of TDoublePoint;
      constructor Create;
      //procedure MouseMove(ADPoint: TDoublePoint); override;
      procedure NextPoint(ADPoint: TDoublePoint); override;
      procedure MouseUp(ADPoint: TDoublePoint);   override;
      procedure Draw(ACanvas: TCanvas);           override;
      procedure NextLine(ADPoint: TDoublePoint);
    published
      property FLineType: TFPPenStyle read LineType write LineType;
  end;

implementation
procedure TFigure.MouseUp(ADPoint: TDoublePoint);
begin end;
procedure TFigure.SetDefFigrStyles;
begin
  LineWidth := START_LINE_WIDTH;
  LineColor := START_LINE_COLOR;
  FillColor := START_FILL_COLOR;
  LineType  := START_LINE_STYLE;
  FillType  := START_FILL_STYLE;
end;

 { TSmlrRect }

procedure TSmlrRect.NextPoint(ADPoint: TDoublePoint);
  begin
    DPRect:=ToDRect(ADPoint,ADPoint);
  end;

procedure TSmlrRect.MouseUp(ADPoint: TDoublePoint);
begin
  DPRect.Bottom:=ADPoint;
end;

procedure TSmlrRect.Draw(ACanvas: TCanvas);
  begin
    with ACanvas do begin
      Pen.Width   := LineWidth;
      Pen.Style   := LineType;
      Brush.Style := RFillType;
      Pen.Color   := LineColor;
      Brush.Color := FillColor;
      MaxCoor     := MaxPoint(DPRect.Top, DPRect.Bottom);
      MinCoor     := MinPoint(DPRect.Top, DPRect.Bottom);
    end;
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
  Flexure := START_FLEXURE_VALUE;
end;

procedure TRoundRect.Draw(ACanvas: TCanvas);
begin
  inherited Draw(ACanvas);
  ACanvas.RoundRect(ToRect(objTransform.W2S(DPRect.Top),
                           objTransform.W2S(DPRect.Bottom)),Flexure,Flexure);
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


 { TPolyLine }

constructor TPolyLine.Create;
begin
  LineWidth := FLineWidth;//START_LINE_WIDTH;
  LineType  := FLineType;//START_LINE_STYLE;
end;

procedure TPolyLine.NextPoint(ADPoint: TDoublePoint);
begin
  SetLength(Vert, 2);
  Vert[0]:= ADPoint;
  Vert[1]:= Vert[0];
end;

procedure TPolyLine.Draw(ACanvas: TCanvas);
var
  i:Integer;
begin
  MaxCoor := ToDP(0,0);
  MinCoor := ToDP(0,0);
  ACanvas.Pen.Color := LineColor;
  ACanvas.Pen.Width := LineWidth;
  ACanvas.Pen.Style := LineType;
  for i:=0 to High(Vert) - 1 do begin
    ACanvas.Line(objTransform.W2S(Vert[i]),objTransform.W2S(Vert[i+1]));
    MinCoor:=MinPoint(Vert[i],MinCoor);
    MaxCoor:=MaxPoint(Vert[i],MaxCoor);
    MinCoor:=MinPoint(Vert[i+1],MinCoor);
    MaxCoor:=MaxPoint(Vert[i+1],MaxCoor);
  end;
end;

procedure TPolyLine.MouseUp(ADPoint: TDoublePoint);
begin
  Vert[High(Vert)] := ADPoint;
end;

procedure TPolyLine.NextLine(ADPoint: TDoublePoint);
begin
  SetLength(Vert,Length(Vert)+1);
  Vert[High(Vert)] := ADPoint;
end;

initialization
 RegisterClass(TPolyLine);
 RegisterClass(TRectangle);
 RegisterClass(TRoundRect);
 RegisterClass(TEllipse);
end.

