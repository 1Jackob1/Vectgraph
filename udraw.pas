unit UDraw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigure, UCreateAttributes, UTool;

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
      SetLength(EditArr, Length(EditArr) + 1);
      EditArr[high(EditArr)] := TPersistent(FigureItems[i]);
    end;
  EditFigure.SelectManyAttrs(EditArr);
end;
end.

