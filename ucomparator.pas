unit UComparator;

{$mode objfpc}{$H+}

interface

uses
  Classes, Math, UDefine;

type

  dfdf = record
    x, t, l: integer;
  end;

function ToDP(X, Y: real): TDoublePoint;
function ToDP(APoint: TPoint): TDoublePoint;
function ToDRect(AFDPoint, ASDPoint: TDoublePoint): TDRect;
function ToRect(AFPoint, ASPoint: TPoint): TRect;
function ToRect(ADPoint: TDRect): TRect;
function MaxPoint(FirstDPoint, SecondDPoint: TDoublePoint): TDoublePoint;
function MinPoint(FirstDPoint, SecondDPoint: TDoublePoint): TDoublePoint;
operator >= (AFPoint, ASPoint: TPoint): boolean;
operator <= (AFPoint, ASPoint: TPoint): boolean;
operator >= (AFPoint, ASPoint: TDoublePoint): boolean;
operator <= (AFPoint, ASPoint: TDoublePoint): boolean;
operator - (APoint: TPoint; AInt: Integer): TPoint;
operator + (APoint: TPoint; AInt: Integer): TPoint;
operator <> (AFPoint, ASPoint: TDoublePoint): boolean;

implementation

function ToDP(X, Y: real): TDoublePoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function ToDP(APoint: TPoint): TDoublePoint;
begin
  Result.X := APoint.X * 1.0;
  Result.Y := APoint.Y * 1.0;
end;

function ToDRect(AFDPoint, ASDPoint: TDoublePoint): TDRect;
begin
  Result.Top := AFDPoint;
  Result.Bottom := ASDPoint;
end;

function ToRect(AFPoint, ASPoint: TPoint): TRect;
begin
  Result.TopLeft := AFPoint;
  Result.BottomRight := ASPoint;
end;

function ToRect(ADPoint: TDRect): TRect;
begin
  Result.Left := Trunc(ADPoint.Top.X);
  Result.Top := Trunc(ADPoint.Top.Y);
  Result.Right := Trunc(ADPoint.Bottom.X);
  Result.Bottom := Trunc(ADPoint.Bottom.Y);
end;

function MaxPoint(FirstDPoint, SecondDPoint: TDoublePoint): TDoublePoint;
begin
  Result.X := max(FirstDPoint.X, SecondDPoint.X);
  Result.Y := max(FirstDPoint.Y, SecondDPoint.Y);
end;

function MinPoint(FirstDPoint, SecondDPoint: TDoublePoint): TDoublePoint;
begin
  Result.X := min(FirstDPoint.X, SecondDPoint.X);
  Result.Y := min(FirstDPoint.Y, SecondDPoint.Y);
end;

operator >= (AFPoint, ASPoint: TPoint): boolean;
begin
  Result := (AFPoint.X >= ASPoint.X) and (AFPoint.Y >= ASPoint.Y);
end;

operator <= (AFPoint, ASPoint: TPoint): boolean;
begin
  Result := (AFPoint.X <= ASPoint.X) and (AFPoint.Y <= ASPoint.Y);
end;

operator >= (AFPoint, ASPoint: TDoublePoint): boolean;
begin
  Result := (AFPoint.X >= ASPoint.X) and (AFPoint.Y >= ASPoint.Y);
end;

operator <= (AFPoint, ASPoint: TDoublePoint): boolean;
begin
  Result := (AFPoint.X <= ASPoint.X) and (AFPoint.Y <= ASPoint.Y);
end;

operator - (APoint: TPoint; AInt: Integer): TPoint;
begin
  Result.X:=APoint.X-AInt;
  Result.Y:=APoint.Y-AInt;
end;

operator + (APoint: TPoint; AInt: Integer): TPoint;
begin
  Result.X:=APoint.X+AInt;
  Result.Y:=APoint.Y+AInt;
end;

operator <> (AFPoint, ASPoint: TDoublePoint): boolean;
begin
  Result:=((AFPoint.X <> ASPoint.X) and (AFPoint.Y <> ASPoint.Y));
end;

end.
