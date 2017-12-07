unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Buttons, EditBtn, StdCtrls, Spin, Menus, ActnList, FPCanvas, UTool,
  UTransform, UComparator, UDefine, UCreateAttributes;

type

  { TVectGraph }

  TVectGraph = class(TForm)
    ActionCls: TAction;
    ActionUnDo: TAction;
    ActionReDo: TAction;
    ActionList1: TActionList;
    DrawArea: TPaintBox;
    MainMenu: TMainMenu;
    ItemProg: TMenuItem;
    ItemProgClose: TMenuItem;
    Editions: TMenuItem;
    EditionsShowAll: TMenuItem;
    EditionsUnDo: TMenuItem;
    EditionsReDo: TMenuItem;
    EditionsCls: TMenuItem;
    EditionsReplaseUp: TMenuItem;
    EditionsReplaseDown: TMenuItem;
    EditionsDel: TMenuItem;
    ScaleSpin: TFloatSpinEdit;
    HorizontalScroll: TScrollBar;
    VerticalScroll: TScrollBar;
    TToolZoomLoupe: TSpeedButton;
    TToolUnZoomLoupe: TSpeedButton;
    ToolsBar: TToolBar;
    AttributesBar: TToolBar;
    procedure ActionClsExecute(Sender: TObject);
    procedure ActionReDoExecute(Sender: TObject);
    procedure ActionUnDoExecute(Sender: TObject);
    procedure DrawAreaMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure DrawAreaMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure DrawAreaMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure DrawAreaPaint(Sender: TObject);
    procedure EditionsClsClick(Sender: TObject);
    procedure EditionsDelClick(Sender: TObject);
    procedure EditionsReDoClick(Sender: TObject);
    procedure EditionsReplaseDownClick(Sender: TObject);
    procedure EditionsReplaseUpClick(Sender: TObject);
    procedure EditionsShowAllClick(Sender: TObject);
    procedure EditionsUnDoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BttnToolClck(Sender: TObject);
    procedure HorizontalScrollScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: integer);
    procedure ItemProgCloseClick(Sender: TObject);
    procedure ScaleSpinChange(Sender: TObject);
    procedure TToolZoomLoupeClick(Sender: TObject);
    procedure TToolUnZoomLoupeClick(Sender: TObject);
    procedure ScrolCalc;
    procedure VerticalScrollScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: integer);

  private
    { private declarations }
  public
    { public declarations }
    //procedure EditSelectedFig;
  end;

var
  VectGraph: TVectGraph;
  IsDrawing: boolean;
  ToolCode: integer;
  MaxDACoor, MinDACoor: TDoublePoint;
  HorBarPos, VertBarPos: integer;

implementation

{$R *.lfm}

{ TVectGraph }

procedure TVectGraph.FormCreate(Sender: TObject);
var
  i: integer;
  Bttn: TSpeedButton;
  BttnImg: TPicture;
begin
  IsDrawing := False;
  for i := 0 to High(ToolConst.Tools) do
  begin

    Bttn := TSpeedButton.Create(ToolsBar);
    Bttn.Parent := ToolsBar;
    Bttn.Width := TOOL_BUTTON_SIZE;
    Bttn.Height := TOOL_BUTTON_SIZE;
    Bttn.Align := alLeft;
    Bttn.Tag := i;
    Bttn.OnClick := @BttnToolClck;
    Bttn.GroupIndex := 1;

    BttnImg := TPicture.Create;
    BttnImg.LoadFromFile('./icons/'+ToolConst.Tools[i].ClassName + '.png');
    Bttn.Glyph := BttnImg.Bitmap;

  end;

  objTransform := TTransform.Create(DrawArea.Width, DrawArea.Height);
  EditFigure := TEditCreate.Create;
  EditFigure.SelectAttrs(TPersistent(ToolConst.Tools[ToolCode].CreateAttributes));
  ScaleSpin.MaxValue := MAX_ZOOM;
  ScaleSpin.MinValue := MIN_ZOOM / 100;

end;


procedure TVectGraph.ActionUnDoExecute(Sender: TObject);
begin
   EditionsUnDoClick(Sender);
end;

procedure TVectGraph.ActionReDoExecute(Sender: TObject);
begin
  EditionsReDoClick(Sender);
end;

procedure TVectGraph.ActionClsExecute(Sender: TObject);
begin
  EditionsClsClick(Sender);
end;

 { MouseDown/Move/Up }

 procedure TVectGraph.DrawAreaMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var i:integer;
begin
  if Button = mbLeft then
  begin
    for i:=0 to Length(FigureItems)-1 do
      FigureItems[i].IsSelected:=false;
    SetLength(History, 0);
    IsDrawing := True;
    ToolConst.Tools[ToolCode].MouseDown(Point(X, Y));
  end;
end;

procedure TVectGraph.DrawAreaMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if IsDrawing then
  begin
    ToolConst.Tools[ToolCode].MouseMove(Point(X, Y));
    DrawArea.Invalidate;
  end;
end;

procedure TVectGraph.DrawAreaMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if IsDrawing then
  begin
    IsDrawing := False;
    ToolConst.Tools[ToolCode].MouseUp(Point(DrawArea.Width, DrawArea.Height));
    DrawArea.Invalidate;
    ScaleSpin.Value := objTransform.Zoom;
  end;
end;
  {/////}
procedure TVectGraph.DrawAreaPaint(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to Length(FigureItems) - 1 do
  begin
    if i = 0 then
    begin
      MaxDACoor := FigureItems[0].MaxCoor;
      MinDACoor := FigureItems[0].MinCoor;
    end
    else
    begin
      MaxDACoor := MaxPoint(FigureItems[i].MaxCoor, MaxDACoor);
      MinDACoor := MinPoint(FigureItems[i].MinCoor, MinDACoor);
    end;
      FigureItems[i].Draw(DrawArea.Canvas);
  end;
  if (Delete) then begin
       FreeAndNil(FigureItems[High(FigureItems)]);
       SetLength(FigureItems, Length(FigureItems) - 1);
       Delete:=False;
       DrawArea.Invalidate;
      end;
  ScrolCalc;
end;


procedure TVectGraph.EditionsClsClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to Length(FigureItems) - 1 do
   FreeAndNil(FigureItems[i]);
  for i:=0 to Length(History) - 1 do
   FreeAndNil(History[i]);
  SetLength(FigureItems,0);
  SetLength(History,0);
  ScrolCalc;
  DrawArea.Invalidate;
end;

procedure TVectGraph.EditionsDelClick(Sender: TObject);
var
  i,j,k: Integer;
begin
  for k:=0 to SelectedCount do begin
    for j:=0 to High(FigureItems) do begin
      i:=j;
      if (FigureItems[j]<>nil) and FigureItems[j].IsSelected then begin
        FreeAndNil(FigureItems[j]);
        while i<High(FigureItems) do begin
          //tmpSwap:=FigureItems[i];
          FigureItems[i]:=FigureItems[i+1];
          //FigureItems[i+1]:=tmpSwap;
          i+=1;
        end;
        //FigureItems[i] := nil;
      end;
    end;
  end;
  SetLength(FigureItems,Length(FigureItems)-SelectedCount);
  DrawArea.Invalidate;
end;

{ Un/ReDo }

procedure TVectGraph.EditionsReDoClick(Sender: TObject);
begin
  if Length(History) > 0 then
  begin
    SetLength(FigureItems, Length(FigureItems) + 1);
    FigureItems[High(FigureItems)] := History[High(History)];
    SetLength(History, Length(History) - 1);
    DrawArea.Invalidate;
  end;
end;

procedure TVectGraph.EditionsReplaseDownClick(Sender: TObject);
var
  i,j: Integer;
begin
   for j:=Length(FigureItems)-1 downto 0 do begin
     i:=j;
       if FigureItems[j].IsSelected then
          while i>0 do begin
            tmpSwap:=FigureItems[i];
            FigureItems[i]:=FigureItems[i-1];
            FigureItems[i-1]:=tmpSwap;
            i-=1;
          end;
       DrawArea.Invalidate;
   end;
end;

procedure TVectGraph.EditionsReplaseUpClick(Sender: TObject);
var
  i,j: Integer;
begin
   for j:=0 to Length(FigureItems)-1 do begin
     i:=j;
       if FigureItems[j].IsSelected then
          while i<Length(FigureItems)-1 do begin
            tmpSwap:=FigureItems[i];
            FigureItems[i]:=FigureItems[i+1];
            FigureItems[i+1]:=tmpSwap;
            i+=1;
          end;
       DrawArea.Invalidate;
   end;
end;

procedure TVectGraph.EditionsUnDoClick(Sender: TObject);
begin
  if Length(FigureItems) > 0 then
  begin
    SetLength(History, Length(History) + 1);
    History[High(History)] := FigureItems[High(FigureItems)];
    SetLength(FigureItems, Length(FigureItems) - 1);
    DrawArea.Invalidate;
  end;
end;
{/////}

procedure TVectGraph.EditionsShowAllClick(Sender: TObject);
begin
  objTransform.RegionLoupe(DrawArea.Height, DrawArea.Width,
    ToRect(objTransform.W2S(MaxDACoor), objTransform.W2S(MinDACoor)));
  ScaleSpin.Value := objTransform.Zoom;
  DrawArea.Invalidate;
end;

procedure TVectGraph.BttnToolClck(Sender: TObject);
begin
  ToolCode := TSpeedButton(Sender).Tag;
  EditFigure.Destroy;
  EditFigure := TEditCreate.Create;
  EditFigure.SelectAttrs(TPersistent(ToolConst.Tools[ToolCode].CreateAttributes));
end;


{Hor/Vert Scrolls}
procedure TVectGraph.HorizontalScrollScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: integer);
begin
  DrawArea.Invalidate;
end;


procedure TVectGraph.VerticalScrollScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: integer);
begin
  DrawArea.Invalidate;
end;
{/////}

procedure TVectGraph.ScaleSpinChange(Sender: TObject);
begin
  objTransform.Zoom := ScaleSpin.Value;
  DrawArea.Invalidate;
end;

procedure TVectGraph.TToolZoomLoupeClick(Sender: TObject);
begin
  objTransform.ZoomLoupe;
  ScaleSpin.Value := objTransform.Zoom;
  DrawArea.Invalidate;
end;

procedure TVectGraph.TToolUnZoomLoupeClick(Sender: TObject);
begin
  objTransform.UnZoomLoupe;
  ScaleSpin.Value := objTransform.Zoom;
  DrawArea.Invalidate;
end;

procedure TVectGraph.ScrolCalc;
var
  DARect: TDRect;
  DiffPoint, DATop, DABottom: TDoublePoint;
  DiffXY: integer;
begin
  DARect := ToDRect(MinDACoor, MaxDACoor);

  DATop := objTransform.S2W(Point(0, 0));
  if DARect.Top.X > DATop.x then
    DARect.top.x := DATop.x;
  if DARect.Top.y > DATop.y then
    DARect.Top.y := DATop.y;
  DABottom := objTransform.S2W(Point(DrawArea.Width, DrawArea.Height));
  if DARect.Bottom.x < DABottom.x then
    DARect.Bottom.x := DABottom.x;
  if DARect.Bottom.y < DABottom.y then
    DARect.Bottom.y := DABottom.y;
  DiffPoint.X := DARect.Bottom.x - DARect.Top.x;
  DiffPoint.Y := DARect.Bottom.y - DARect.Top.y;
  if DiffPoint.x * DiffPoint.y = 0 then
    exit;

  DiffXY := HorizontalScroll.Max - HorizontalScroll.Min;
  HorizontalScroll.PageSize :=
    round(DrawArea.Width / (DiffPoint.X * objTransform.Zoom) * DiffXY);
  HorizontalScroll.Visible := HorizontalScroll.PageSize < DiffXY;
  if HorizontalScroll.PageSize < DiffXY then
  begin
    if (HorBarPos = HorizontalScroll.Position) then
      HorizontalScroll.Position :=
        round(((-1) * (objTransform.Offset.X + DARect.Top.x)) / DiffPoint.X * DiffXY)
    else
      objTransform.Offset.X :=
        (-1) * (HorizontalScroll.Position / DiffXY * DiffPoint.X + DARect.Top.x);
    HorBarPos := HorizontalScroll.Position;
  end;

  DiffXY := VerticalScroll.Max - VerticalScroll.Min;
  VerticalScroll.PageSize := round(DrawArea.Height /
    (DiffPoint.Y * objTransform.Zoom) * DiffXY);
  VerticalScroll.Visible := VerticalScroll.PageSize < DiffXY;
  if VerticalScroll.PageSize < DiffXY then
  begin
    if (VertBarPos = VerticalScroll.Position) then
      VerticalScroll.Position :=
        round(((-1) * (objTransform.Offset.Y + DARect.Top.y)) / DiffPoint.Y * DiffXY)
    else
      objTransform.Offset.Y :=
        (-1) * (VerticalScroll.Position / DiffXY * DiffPoint.Y + DARect.Top.y);
    VertBarPos := VerticalScroll.Position;
  end;
end;


procedure TVectGraph.ItemProgCloseClick(Sender: TObject);
begin
  VectGraph.Close;
end;

end.
