unit UTransform;
//vahob
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Clipbrd, ExtCtrls, UDefine, UComparator;

type

  {TTransform}

  TTransform = class
    public
    Offset: TDoublePoint;
    Zoom: Real;
    ALRect: TRect;
    constructor Create(DAWidth, DAHeight:Real);
    procedure ZoomLoupe;
    procedure UnZoomLoupe;
    procedure RegionLoupe(DAWidth, DAHeight:Real; LRect: TRect);
    //function ToDP(APoint: TPoint): TDoublePoint;
    function S2W(APoint: TPoint): TDoublePoint;
    function W2S(ADoublePoint: TDoublePoint): TPoint;
  end;

  var
      objTransform: TTransform;
implementation

constructor TTransform.Create(DAWidth, DAHeight:Real);
begin
  Offset:=ToDP(Point(0,0));
  Zoom:=MIN_ZOOM*100;
end;

procedure TTransform.ZoomLoupe;
begin
  if Zoom<=100 then
     if Zoom > 1 then
        Zoom:=Zoom+1
  else
    Zoom:=Zoom*2;
end;

procedure TTransform.UnZoomLoupe;
begin
  if Zoom >= 0.125 then
    if Zoom > 1 then
      Zoom := Zoom - 1
    else
      Zoom := Zoom / 2;
end;

procedure TTransform.RegionLoupe(DAWidth, DAHeight:Real; LRect: TRect);
var tmp: Integer;
    tmpZoom: Real;
begin
   if LRect.BottomRight.x<LRect.TopLeft.x then
     begin
       tmp:=LRect.TopLeft.x;
       LRect.TopLeft.x:=LRect.BottomRight.x;
       LRect.BottomRight.x:=tmp;
     end;
   if LRect.BottomRight.y<LRect.TopLeft.y then
     begin
       tmp:=LRect.TopLeft.y;
       LRect.TopLeft.y:=LRect.BottomRight.y;
       LRect.BottomRight.y:=tmp;
     end;
   if(LRect.BottomRight.x<>LRect.TopLeft.x) and
     (LRect.BottomRight.y<>LRect.TopLeft.y) then begin
       if (DAWidth / (LRect.BottomRight.x - LRect.TopLeft.x)) <
          (DAHeight / (LRect.BottomRight.y - LRect.TopLeft.y)) then begin
        tmpZoom := DAWidth / (LRect.BottomRight.x - LRect.TopLeft.x);
    end
    else
    begin
        tmpZoom :=  DAHeight / (LRect.BottomRight.y - LRect.TopLeft.y);
    end;

    if (Zoom <= 100) then Zoom *=  tmpZoom;
    Offset.x += ((DAWidth / 2) - ((((LRect.BottomRight.x - LRect.TopLeft.x) *
            tmpZoom) / 2) + LRect.TopLeft.x * tmpZoom)) / Zoom;
    Offset.y += ((DAHeight / 2) - ((((lRect.BottomRight.y - LRect.TopLeft.y) *
            tmpZoom) / 2) + LRect.TopLeft.y * tmpZoom)) / Zoom;
     end;

end;


function TTransform.S2W(APoint: TPoint): TDoublePoint;
var
    DP: TDoublePoint;
begin
   DP:=ToDP(APoint);
   Result:= ToDP((DP.x / objTransform.Zoom) - objTransform.Offset.x,
            (DP.y / objTransform.Zoom) - objTransform.Offset.y);
end;

function TTransform.W2S(ADoublePoint: TDoublePoint): TPoint;
begin
   Result:= Point(Trunc((ADoublePoint.x + objTransform.Offset.x) * Zoom),
            Trunc((ADoublePoint.y + objTransform.Offset.y) * Zoom));
end;

end.

