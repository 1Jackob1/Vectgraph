unit UComparator;

{$mode objfpc}{$H+}

interface

uses
  Classes, Math, UDefine;

type

  dfdf = record
  x,t,l: Integer;
  end;

  function ToDP(X, Y: Real): TDoublePoint;
  function ToDP(APoint: TPoint): TDoublePoint;
  function ToDRect(AFDPoint,ASDPoint : TDoublePoint): TDRect;
  function ToRect(AFPoint, ASPoint: TPoint): TRect;
  function ToRect(ADPoint: TDRect): TRect;
  function MaxPoint(FirstDPoint, SecondDPoint: TDoublePoint): TDoublePoint;
  function MinPoint(FirstDPoint, SecondDPoint: TDoublePoint): TDoublePoint;

implementation

  function ToDP(X, Y: Real): TDoublePoint;
  begin
    Result.X := X;
    Result.Y := Y;
  end;

  function ToDP(APoint: TPoint): TDoublePoint;
  begin
    Result.X := APoint.X*1.0;
    Result.Y := APoint.Y*1.0;
  end;

  function ToDRect(AFDPoint,ASDPoint : TDoublePoint): TDRect;
  begin
    Result.Top    := AFDPoint;
    Result.Bottom := ASDPoint;
  end;

  function ToRect(AFPoint, ASPoint: TPoint): TRect;
  begin
    Result.TopLeft:=AFPoint;
    Result.BottomRight:=ASPoint;
  end;

  function ToRect(ADPoint: TDRect): TRect;
  begin
    Result.Left:= Trunc(ADPoint.Top.X);
    Result.Top:= Trunc(ADPoint.Top.Y);
    Result.Right:= Trunc(ADPoint.Bottom.X);
    Result.Bottom:= Trunc(ADPoint.Bottom.Y);
  end;

  function MaxPoint(FirstDPoint, SecondDPoint: TDoublePoint): TDoublePoint;
  begin
    Result.X:=max(FirstDPoint.X,SecondDPoint.X);
    Result.Y:=max(FirstDPoint.Y,SecondDPoint.Y);
  end;

  function MinPoint(FirstDPoint, SecondDPoint: TDoublePoint): TDoublePoint;
  begin
    Result.X:=min(FirstDPoint.X,SecondDPoint.X);
    Result.Y:=min(FirstDPoint.Y,SecondDPoint.Y);
  end;

end.

