unit UTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, UFigure, UDefine, UTransform, UComparator,
  UCreateAttributes;

type

  { TTool }

  TTool = class
    private
      objFigure: TFigure;
    public
      function  CreateAttributes: TPersistent; virtual;
      procedure MouseMove(APoint: TPoint);     virtual; abstract;
      procedure MouseDown(APoint: TPoint);     virtual; abstract;
      procedure MouseUp(  APoint: TPoint);     virtual;
  end;

  TToolClass = class of TTool;
  TToolArr   = array of TTool;

  { TToolReg }

  TToolReg = class
    private
      TToolArray: array of TTool;
    public
      procedure ToolReg(ATToolClass: TToolClass);
      property  Tools: TToolArr read TToolArray;

  end;

  { TToolLine }

  TToolLine = class(TTool)
    function  CreateAttributes: TPersistent; override;
    procedure MouseMove(APoint: TPoint);     override;
    procedure MouseDown(APoint: TPoint);     override;
    procedure MouseUp(  APoint: TPoint);     override;
  end;

  { TToolPencil}

  TToolPencil = class(TTool)
    function  CreateAttributes: TPersistent; override;
    procedure MouseMove(APoint: TPoint);     override;
    procedure MouseDown(APoint: TPoint);     override;
    procedure MouseUp(  APoint: TPoint);     override;
  end;

  { TToolRectangle }

  TToolRectangle = class(TTool)
    function  CreateAttributes: TPersistent; override;
    procedure MouseMove(APoint: TPoint);     override;
    procedure MouseDown(APoint: TPoint);     override;
    procedure MouseUp(  APoint: TPoint);     override;
  end;

  { TToolRoundRect }

  TToolRoundRect = class(TTool)
    function  CreateAttributes: TPersistent; override;
    procedure MouseMove(APoint: TPoint);     override;
    procedure MouseDown(APoint: TPoint);     override;
    procedure MouseUp(  APoint: TPoint);     override;
  end;

  { TToolEllipse }

  TToolEllipse = class(TTool)
    function  CreateAttributes: TPersistent; override;
    procedure MouseMove(APoint: TPoint);     override;
    procedure MouseDown(APoint: TPoint);     override;
    procedure MouseUp(  APoint: TPoint);     override;
  end;

  { TToolLoupe }

  TToolLoupe = class(TTool)
    procedure MouseMove(APoint: TPoint);     override;
    procedure MouseDown(APoint: TPoint);     override;
    procedure MouseUp(  APoint: TPoint);     override;
  end;

  { TToolHand }

  TToolHand = class(TTool)
    private
      DPoint: TDoublePoint;
    public
      procedure MouseMove(APoint: TPoint);   override;
      procedure MouseDown(APoint: TPoint);   override;
    end;
 var
    ToolConst: TToolReg;
    tmpDP: TDoublePoint;
    FigureItems, History: array of TFigure;
implementation

{ TTool }
 procedure TTool.MouseUp(APoint: TPoint);
 begin
 end;

 function TTool.CreateAttributes: TPersistent;
 begin
   Result := TPersistent(objFigure);
 end;

 { TToolLine }

 function TToolLine.CreateAttributes: TPersistent;
 begin
   objFigure := TPolyLine.Create;
   Result    := inherited CreateAttributes;
 end;

 procedure TToolLine.MouseDown(APoint: TPoint);
 var
   DP: TDoublePoint;
 begin
    objFigure := TPolyLine.Create;
    SetLength(FigureItems, Length(FigureItems)+1);
    FigureItems[High(FigureItems)] := TPolyLine.Create;
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
   Result    := inherited CreateAttributes;
 end;

 procedure TToolPencil.MouseDown(APoint: TPoint);
 var
   DP: TDoublePoint;
begin
    SetLength(FigureItems,Length(FigureItems)+1);
    FigureItems[High(FigureItems)] := TPolyLine.Create;
    DP := objTransform.S2W(APoint);
    FigureItems[High(FigureItems)].NextPoint(DP);
end;

 procedure TToolPencil.MouseMove(APoint: TPoint);
begin
    TPolyLine(FigureItems[High(FigureItems)]).NextLine(objTransform.S2W(APoint))
end;

 procedure TToolPencil.MouseUp(APoint: TPoint);
 begin
     inherited MouseUp(APoint);
 end;


 { TToolRectangle }

 function TToolRectangle.CreateAttributes: TPersistent;
 begin
   objFigure := TRectangle.Create;
   Result    := inherited CreateAttributes;
 end;

 procedure TToolRectangle.MouseDown(APoint: TPoint);
 var
   DP: TDoublePoint;
 begin
    SetLength(FigureItems,Length(FigureItems)+1);
    FigureItems[High(FigureItems)] := TRectangle.Create;
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
   Result    := inherited CreateAttributes;
 end;

 procedure TToolRoundRect.MouseDown(APoint: TPoint);
 var
   DP: TDoublePoint;
   begin
    SetLength(FigureItems,Length(FigureItems)+1);
    FigureItems[High(FigureItems)] := TRoundRect.Create;
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
   Result    := inherited CreateAttributes;
 end;

 procedure TToolEllipse.MouseDown(APoint: TPoint);
 var
   DP: TDoublePoint;
   begin
    SetLength(FigureItems,Length(FigureItems)+1);
    FigureItems[High(FigureItems)] := TEllipse.Create;
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

 { TToolReg }

procedure TToolReg.ToolReg(ATToolClass: TToolClass);
begin
  SetLength(TToolArray,Length(TToolArray)+1);
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


end.

