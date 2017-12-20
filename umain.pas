unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Buttons, EditBtn, StdCtrls, Spin, Menus, ActnList, FPCanvas, fpjson,
  UTool, UTransform, UComparator, UDefine, UCreateAttributes, UFigure;

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
    ClearScreen: TMenuItem;
    DelSelected: TMenuItem;
    EditionsCls: TMenuItem;
    EditionsReplaseUp: TMenuItem;
    EditionsReplaseDown: TMenuItem;
    EditionsDel: TMenuItem;
    Save: TMenuItem;
    Open: TMenuItem;
    MoveLower: TMenuItem;
    MoveUpper: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ScaleSpin: TFloatSpinEdit;
    HorizontalScroll: TScrollBar;
    VerticalScroll: TScrollBar;
    TToolZoomLoupe: TSpeedButton;
    TToolUnZoomLoupe: TSpeedButton;
    ToolsBar: TToolBar;
    AttributesBar: TToolBar;
    procedure DrawAreaMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure DrawAreaMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure DrawAreaMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure DrawAreaPaint(Sender: TObject);
    procedure DelSelectedClick(Sender: TObject);
    procedure EditionsShowAllClick(Sender: TObject);
    procedure ClearScreenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BttnToolClck(Sender: TObject);
    procedure HorizontalScrollScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: integer);
    procedure ItemProgCloseClick(Sender: TObject);
    procedure MoveLowerClick(Sender: TObject);
    procedure MoveUpperClick(Sender: TObject);
    procedure OpenClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
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
    BttnImg.LoadFromFile('./icons/' + ToolConst.Tools[i].ClassName + '.png');
    Bttn.Glyph := BttnImg.Bitmap;

  end;

  objTransform := TTransform.Create(DrawArea.Width, DrawArea.Height);
  EditFigure := TEditCreate.Create;
  EditFigure.SelectAttrs(TPersistent(ToolConst.Tools[0].CreateAttributes));
  ScaleSpin.MaxValue := MAX_ZOOM;
  ScaleSpin.MinValue := MIN_ZOOM / 100;

end;

procedure TVectGraph.DrawAreaMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  i: integer;
begin
  if Button = mbLeft then
  begin
    if ToolCode <> High(ToolConst.Tools) then
      for i := 0 to Length(FigureItems) - 1 do
        FigureItems[i].IsSelected := False;
    ForAllFigrStyles := CurrentStyles;
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
    EditFigure.SelectAttrs(TPersistent(ToolConst.Tools[ToolCode].CreateAttributes));
  end;
end;

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
    FigureItems[i].Draw(DrawArea.Canvas, FigureItems[i].IsSelected);
  end;
  if (Delete) then
  begin
    FreeAndNil(FigureItems[High(FigureItems)]);
    SetLength(FigureItems, Length(FigureItems) - 1);
    Delete := False;
    DrawArea.Invalidate;
  end;
  ScrolCalc;
end;


procedure TVectGraph.DelSelectedClick(Sender: TObject);
var
  i, j: integer;
begin
  j := 0;
  for i := 0 to High(FigureItems) do
  begin
    if (FigureItems[i].IsSelected) then
      FreeAndNil(FigureItems[i])
    else
    begin
      FigureItems[j] := FigureItems[i];
      j += 1;
    end;
  end;
  setLength(FigureItems, j);
  DrawArea.Invalidate;
end;

procedure TVectGraph.ClearScreenClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to Length(FigureItems) - 1 do
    FreeAndNil(FigureItems[i]);
  SetLength(FigureItems, 0);
  ScrolCalc;
  DrawArea.Invalidate;
end;

procedure TVectGraph.EditionsShowAllClick(Sender: TObject);
begin
  objTransform.RegionLoupe(DrawArea.Height, DrawArea.Width,
    ToRect(objTransform.W2S(MaxDACoor), objTransform.W2S(MinDACoor)));
  ScaleSpin.Value := objTransform.Zoom;
  DrawArea.Invalidate;
end;

procedure TVectGraph.BttnToolClck(Sender: TObject);
var
  i: integer;
begin
  ToolCode := TSpeedButton(Sender).Tag;
  EditFigure.Destroy;
  EditFigure := TEditCreate.Create;
  EditFigure.SelectAttrs(TPersistent(ToolConst.Tools[ToolCode].CreateAttributes));
  if ToolCode <> High(ToolConst.Tools) then
    for i := 0 to Length(FigureItems) - 1 do
      FigureItems[i].IsSelected := False;
  DrawArea.Invalidate;

end;

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

procedure TVectGraph.MoveLowerClick(Sender: TObject);
var
  i, j: integer;
begin
  for j := Length(FigureItems) - 1 downto 0 do
  begin
    i := j;
    if FigureItems[j].IsSelected then
      while i > 0 do
      begin
        Swap(FigureItems[i], FigureItems[i - 1]);
        i -= 1;
      end;
    DrawArea.Invalidate;
  end;
end;

procedure TVectGraph.MoveUpperClick(Sender: TObject);
var
  i, j: integer;
begin
  for j := 0 to Length(FigureItems) - 1 do
  begin
    i := j;
    if FigureItems[j].IsSelected then
      while i < Length(FigureItems) - 1 do
      begin
        Swap(FigureItems[i], FigureItems[i + 1]);
        i += 1;
      end;
    DrawArea.Invalidate;
  end;
end;

procedure TVectGraph.OpenClick(Sender: TObject);
var
  jData: TJSONData;
  TFile: Text;
  Point: TPoint;
  i, j: integer;
  Data: string;
begin
  if OpenDialog1.Execute then
  begin
    AssignFile(TFile, OpenDialog1.FileName);
    Reset(TFile);
    Read(TFile, Data);
    CloseFile(TFile);
    try
      jData := GetJSON(Data);
    except
      ShowMessage('Файл поврежден!#10#13 Загрузка не удалась.');
      Exit;
    end;
  end;
  SetLength(FigureItems, 0);
  for i := 0 to jData.FindPath('Count').AsInteger - 1 do
  begin
    SetLength(FigureItems, Length(FigureItems) + 1);
    Data := 'Items[' + IntToStr(i) + '].Class';
    case jData.FindPath(Data).AsString of
      'TRectangle', 'TEllipse', 'TRoundRect':
      begin
        case jData.FindPath(Data).AsString of
          'TRectangle': FigureItems[High(FigureItems)] := TRectangle.Create;
          'TEllipse': FigureItems[High(FigureItems)] := TEllipse.Create;
          'TRoundRect': FigureItems[High(FigureItems)] := TRoundRect.Create;
        end;
        with FigureItems[High(FigureItems)] do
        begin
          Data := 'Items[' + IntToStr(i) + '].LineWidth';
          FLineWidth := jData.FindPath(Data).AsInteger;
          Data := 'Items[' + IntToStr(i) + '].LineColor';
          FLineColor := jData.FindPath(Data).AsInteger;
          Data := 'Items[' + IntToStr(i) + '].LineType';
          FLineType := TFPPenStyle(jData.FindPath(Data).AsInteger);
          Data := 'Items[' + IntToStr(i) + '].FillColor';
          FFillColor := jData.FindPath(Data).AsInteger;
          Data := 'Items[' + IntToStr(i) + '].FillType';
          FFillType := TFPBrushStyle(jData.FindPath(Data).AsInteger);
          if ClassName = 'TRoundRect' then
          begin
            Data := 'Items[' + IntToStr(i) + '].Flexure';
            FFlexure := jData.FindPath(Data).AsInteger;
          end;
          Data := 'Items[' + IntToStr(i) + '].Bot[0]';
          Point.X := jData.FindPath(Data).AsInteger;
          Data := 'Items[' + IntToStr(i) + '].Bot[1]';
          Point.Y := jData.FindPath(Data).AsInteger;
          MaxCoor := objTransform.S2W(Point);
          Data := 'Items[' + IntToStr(i) + '].Top[0]';
          Point.X := jData.FindPath(Data).AsInteger;
          Data := 'Items[' + IntToStr(i) + '].Top[1]';
          Point.Y := jData.FindPath(Data).AsInteger;
          MinCoor := objTransform.S2W(Point);
          defineTopBot;
        end;
      end;
      'TPolyLine':
      begin
        FigureItems[High(FigureItems)] := TPolyLine.Create;
        with FigureItems[High(FigureItems)] do
        begin
          Data := 'Items[' + IntToStr(i) + '].LineWidth';
          FLineWidth := jData.FindPath(Data).AsInteger;
          Data := 'Items[' + IntToStr(i) + '].LineColor';
          FLineColor := jData.FindPath(Data).AsInteger;
          Data := 'Items[' + IntToStr(i) + '].LineType';
          FLineType := TFPPenStyle(jData.FindPath(Data).AsInteger);
          Data := 'Items[' + IntToStr(i) + '].VertexesCount';
          for j := 0 to jData.FindPath(Data).AsInteger-1 do
          begin
            SetLength(vert,Length(vert)+1);
            Data := 'Items[' + IntToStr(i) + '].Vertexes['+IntToStr(j*2)+']';
            Point.X:=jData.FindPath(Data).AsInteger;
            Data := 'Items[' + IntToStr(i) + '].Vertexes['+IntToStr(j*2+1)+']';
            Point.Y:=jData.FindPath(Data).AsInteger;
            vert[j]:=objTransform.S2W(Point);
          end;
        end;
      end;
    end;
  end;
  jData.Free;
  DrawArea.Invalidate;

end;

procedure TVectGraph.SaveClick(Sender: TObject);
var
  i: TFigure;
  TFile: Text;
begin
  if SaveDialog1.Execute then
  begin
    AssignFile(TFile, SaveDialog1.FileName);
    Rewrite(TFile);
    Write(TFile, '{"Count": ', Length(FigureItems), ',');
    Write(TFile, '"Items":[');
    for i in FigureItems do
    begin
      i.saveFigure(TFile, i.ClassName);
      if i <> FigureItems[High(FigureItems)] then
        Write(TFile, ',');
    end;
    Write(TFile, ']}');
    CloseFile(TFile);
  end;
end;

end.
