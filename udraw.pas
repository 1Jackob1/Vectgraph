unit UDraw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigure, UCreateAttributes;

procedure EditSelectedFig;

implementation

procedure EditSelectedFig;
var
  i: integer;
  EditArr: array of TPersistent;
begin
  SetLength(EditArr, 0);
  for i := 0 to High(FigureItems) do
    if FigureItems[i].IsSelected then
    begin
      CurrentStyles.LineStyleNum := integer(FigureItems[i].FLineType);
      CurrentStyles.LineColor := FigureItems[i].FLineColor;
      CurrentStyles.LineWidth := FigureItems[i].FLineWidth;
      CurrentStyles.FillColor := FigureItems[i].FFillColor;
      CurrentStyles.FillStyleNum := integer(FigureItems[i].FFillType);
      CurrentStyles.Flexure := FigureItems[i].FFlexure;
      SetLength(EditArr, Length(EditArr) + 1);
      EditArr[high(EditArr)] := FigureItems[i];
    end;
  EditFigure.SelectManyAttrs(EditArr);
end;

end.
