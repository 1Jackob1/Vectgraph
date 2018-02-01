unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Clipbrd,
  ComCtrls, Buttons, EditBtn, StdCtrls, typinfo, Spin, Menus, ActnList, FPCanvas, fpjson,
  UTool, UTransform, UComparator, UDefine, UCreateAttributes, UFigure, About;

type

  { TSmall_Editor }

  TSmall_Editor = class(TForm)
    ActionCls: TAction;
    ActionUnDo: TAction;
    ActionReDo: TAction;
    ActionList1: TActionList;
    DrawArea: TPaintBox;
    FilePath: TLabel;
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
    AboutItems: TMenuItem;
    Developer: TMenuItem;
    Functions: TMenuItem;
    Instruction: TMenuItem;
    CopyFigure: TMenuItem;
    SaveAs: TMenuItem;
    PasteFigure: TMenuItem;
    Save: TMenuItem;
    Open: TMenuItem;
    MoveLower: TMenuItem;
    MoveUpper: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    SaveAsDialog: TSaveDialog;
    ScaleSpin: TFloatSpinEdit;
    HorizontalScroll: TScrollBar;
    TToolReDo: TSpeedButton;
    TToolUnDo: TSpeedButton;
    VerticalScroll: TScrollBar;
    TToolZoomLoupe: TSpeedButton;
    TToolUnZoomLoupe: TSpeedButton;
    ToolsBar: TToolBar;
    AttributesBar: TToolBar;
    procedure CopyFigureClick(Sender: TObject);
    procedure DeveloperClick(Sender: TObject);
    procedure DrawAreaMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure DrawAreaMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure DrawAreaMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure DrawAreaPaint(Sender: TObject);
    procedure DelSelectedClick(Sender: TObject);
    procedure EditionsShowAllClick(Sender: TObject);
    procedure ClearScreenClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure BttnToolClck(Sender: TObject);
    procedure FunctionsClick(Sender: TObject);
    procedure HorizontalScrollScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: integer);
    procedure InstructionClick(Sender: TObject);
    procedure ItemProgCloseClick(Sender: TObject);
    procedure MoveLowerClick(Sender: TObject);
    procedure MoveUpperClick(Sender: TObject);
    procedure OpenClick(Sender: TObject);
    procedure PasteFigureClick(Sender: TObject);
    procedure SaveAsClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure ScaleSpinChange(Sender: TObject);
    procedure TToolReDoClick(Sender: TObject);
    procedure TToolUnDoClick(Sender: TObject);
    procedure TToolZoomLoupeClick(Sender: TObject);
    procedure TToolUnZoomLoupeClick(Sender: TObject);
    procedure ScrolCalc;
    procedure VerticalScrollScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: integer);
    //procedure SaveFile(s: String);
    procedure OpenFile(s: string; Merge: boolean);
    procedure SaveFile(var AText: Text);
    procedure PushHistory;
    procedure PopHistory;
    procedure SetHistory;
    procedure NullHist;
    procedure IsSavedState;
    procedure Saving;
    procedure PushHistAndState;
    function FSaveFile(): string;
    procedure SetFilePath;

  private

    { private declarations }
  public
    { public declarations }
  end;

var
  Small_Editor: TSmall_Editor;
  IsDrawing: boolean;
  ToolCode: integer;
  MaxDACoor, MinDACoor: TDoublePoint;
  HorBarPos, VertBarPos: integer;
  History: array of string;
  DefCondHistory, SavedFigures, CurrentSavedPath: string;

implementation

{$R *.lfm}

{ TSmall_Editor }

procedure TSmall_Editor.FormCreate(Sender: TObject);
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
  ScaleSpin.MaxValue := MAX_ZOOM;
  ScaleSpin.MinValue := MIN_ZOOM / 100;
  ToolCode := -1;
  FilePath.Caption := 'Untitled';
  DefCondHistory := FSaveFile();
  SavedFigures := FSaveFile();
  NullHist;
  DrawArea.Invalidate;
end;

procedure TSmall_Editor.DrawAreaMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  i: integer;
begin
  if (Button = mbLeft) and (ToolCode <> -1) then
  begin
    if WasUnDo then
      SetHistory;
    WasUnDo := False;
    WasUnDo := False;
    if ToolCode <> High(ToolConst.Tools) then
      for i := 0 to Length(FigureItems) - 1 do
        FigureItems[i].IsSelected := False;
    IsDrawing := True;
    ToolConst.Tools[ToolCode].MouseDown(Point(X, Y));
  end;
end;

procedure TSmall_Editor.DeveloperClick(Sender: TObject);
var
  TFile: Text;
  tmpDev, Dev: string;
begin
  AssignFile(TFile, './Documentation/Developer.txt');
  Reset(TFile);
  while (not (EOF(TFile))) do
  begin
    Readln(TFile, tmpDev);
    Dev += tmpDev + chr(10);
  end;
  CloseFile(TFile);
  Application.MessageBox(PChar(Dev), 'Small Editor', 0);
end;

procedure TSmall_Editor.CopyFigureClick(Sender: TObject);
var
  i: TFigure;
  TFile: Text;
  copyedFigures: string;
begin
  AssignFile(TFile, '34');
  Rewrite(TFile);
  Write(TFile, '{"Count": ', SelectedCount, ',');
  Write(TFile, '"Items":[');
  for i in FigureItems do
  begin
    if not i.IsSelected then
      Continue;
    i.saveFigure(TFile, 15);
    if i <> FigureItems[High(FigureItems)] then
      Write(TFile, ',');
  end;
  Write(TFile, ']}');
  CloseFile(TFile);
  AssignFile(TFile, '34');
  Reset(TFile);
  Read(TFile, copyedFigures);
  Clipboard.AsText := copyedFigures;
  CloseFile(TFile);
  AssignFile(TFile, '34');
  Erase(TFile);
end;

procedure TSmall_Editor.DrawAreaMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if IsDrawing then
  begin
    ToolConst.Tools[ToolCode].MouseMove(Point(X, Y));
    DrawArea.Invalidate;
  end;
end;

procedure TSmall_Editor.DrawAreaMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if IsDrawing then
  begin
    IsDrawing := False;
    ToolConst.Tools[ToolCode].MouseUp(Point(DrawArea.Width, DrawArea.Height));
    DrawArea.Invalidate;
    ScaleSpin.Value := objTransform.Zoom;
    EditFigure.SelectAttrs(TPersistent(ToolConst.Tools[ToolCode].CreateAttributes));
    if not (FigureItems[high(FigureItems)] is TSpecialRect) then
      PushHistAndState;
    WasChanged := False;
    IsSavedState;
  end;
end;

procedure TSmall_Editor.DrawAreaPaint(Sender: TObject);
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
  if (DeleteTopFigure) then
  begin
    FreeAndNil(FigureItems[High(FigureItems)]);
    SetLength(FigureItems, Length(FigureItems) - 1);
    DeleteTopFigure := False;
    DrawArea.Invalidate;
  end;
  ScrolCalc;
end;


procedure TSmall_Editor.DelSelectedClick(Sender: TObject);
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
  PushHistAndState;
end;

procedure TSmall_Editor.ClearScreenClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to Length(FigureItems) - 1 do
    FreeAndNil(FigureItems[i]);
  SetLength(FigureItems, 0);
  ScrolCalc;
  PushHistAndState
end;

procedure TSmall_Editor.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if not WasChanged then
  begin
    case QuestionDlg('Small Editor', 'Ваш проект не сохранён! Сохранить его?',
        mtCustom, [mrYes, 'Да', mrNo, 'Нет', mrCancel, 'Отменить'], '') of
      mrYes:
      begin
        SaveClick(Sender);
        Small_Editor.Close;
      end;
      mrNo: CanClose := True;
      mrCancel: CanClose := False;
    end;
  end;
end;

procedure TSmall_Editor.EditionsShowAllClick(Sender: TObject);
begin
  objTransform.RegionLoupe(DrawArea.Width, DrawArea.Height,
    ToRect(objTransform.W2S(MaxDACoor), objTransform.W2S(MinDACoor)));
  ScaleSpin.Value := objTransform.Zoom;
  DrawArea.Invalidate;
end;

procedure TSmall_Editor.BttnToolClck(Sender: TObject);
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

procedure TSmall_Editor.FunctionsClick(Sender: TObject);
var
  TFile: Text;
  tmpDev, Dev: string;
begin
  AssignFile(TFile, './Documentation/Functions.txt');
  Reset(TFile);
  while (not (EOF(TFile))) do
  begin
    Readln(TFile, tmpDev);
    Dev += tmpDev + chr(10);
  end;
  CloseFile(TFile);
  Application.MessageBox(PChar(Dev), 'Small Editor', 0);
end;

procedure TSmall_Editor.HorizontalScrollScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: integer);
begin
  DrawArea.Invalidate;
end;

procedure TSmall_Editor.InstructionClick(Sender: TObject);
begin
  Tools.Show;
end;


procedure TSmall_Editor.VerticalScrollScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: integer);
begin
  DrawArea.Invalidate;
end;

procedure TSmall_Editor.ScaleSpinChange(Sender: TObject);
begin
  objTransform.Zoom := ScaleSpin.Value;
  DrawArea.Invalidate;
end;

procedure TSmall_Editor.TToolReDoClick(Sender: TObject);
begin
  if HistoryIndex < 0 then
    HistoryIndex := 0;
  if HistoryIndex + 1 <= High(History) then
  begin
    HistoryIndex += 1;
    WasReDo := True;
    PopHistory;
    IsSavedState;
  end;
end;

procedure TSmall_Editor.TToolUnDoClick(Sender: TObject);
begin
  if HistoryIndex >= 0 then
    HistoryIndex -= 1;
  PopHistory;
  WasUnDo := True;
  IsSavedState;
  DrawArea.Invalidate;
end;

procedure TSmall_Editor.TToolZoomLoupeClick(Sender: TObject);
begin
  objTransform.ZoomLoupe;
  ScaleSpin.Value := objTransform.Zoom;
  ScrolCalc;
  DrawArea.Invalidate;
end;

procedure TSmall_Editor.TToolUnZoomLoupeClick(Sender: TObject);
begin
  objTransform.UnZoomLoupe;
  ScaleSpin.Value := objTransform.Zoom;
  ScrolCalc;
  DrawArea.Invalidate;
end;

procedure TSmall_Editor.ScrolCalc;
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


procedure TSmall_Editor.ItemProgCloseClick(Sender: TObject);
begin
  Small_Editor.Close;
end;

procedure TSmall_Editor.MoveLowerClick(Sender: TObject);
var
  i, j: integer;
begin
  for j := Length(FigureItems) - 1 downto 0 do
  begin
    i := j;
    if FigureItems[j].IsSelected then
      while i > 0 do
      begin
        Swap(FigureItems[j], FigureItems[i - 1]);
        i -= 1;
      end;
  end;
  PushHistAndState
end;

procedure TSmall_Editor.MoveUpperClick(Sender: TObject);
var
  i, j: integer;
begin
  for j := 0 to Length(FigureItems) - 1 do
  begin
    i := j;
    if FigureItems[j].IsSelected then
      while i < Length(FigureItems) -  1 do
      begin
        Swap(FigureItems[j], FigureItems[i + 1]);
        i += 1;
      end;
  end;
  PushHistAndState;
end;

procedure TSmall_Editor.OpenClick(Sender: TObject);
var
  TFile: Text;
  Data: string;
begin
  if not OpenDialog.Execute then
    Exit;
  FilePath.Caption := OpenDialog.FileName;
  AssignFile(TFile, OpenDialog.FileName);
  Reset(TFile);
  Read(TFile, Data);
  CloseFile(TFile);
  OpenFile(Data, False);
  SavedFigures := FSaveFile();
  NullHist;
  PushHistory;
  WasSaved:=True;
  WasOpened:=True;
  CurrentSavedPath:=OpenDialog.FileName;
end;

procedure TSmall_Editor.PasteFigureClick(Sender: TObject);
begin
  OpenFile(Clipboard.AsText, True);
end;

procedure TSmall_Editor.SaveAsClick(Sender: TObject);
var
  Sbitmap: TBitmap;
  Spng: TPortableNetworkGraphic;
  Sjpg: TJPEGImage;
begin
  if not SaveAsDialog.Execute then exit;
  CurrentSavedPath:=SaveAsDialog.FileName;
  Sbitmap:=TBitmap.Create;
  Sbitmap.LoadFromDevice(DrawArea.Canvas.Handle);
  Sbitmap.Width:=DrawArea.Width;
  Sbitmap.Height:=DrawArea.Height;
  case SaveAsDialog.FilterIndex of
    1: Saving;   //json
    2:begin      //png
      Spng:=TPortableNetworkGraphic.Create;
      Spng.Assign(Sbitmap);
      Spng.SaveToFile(SaveAsDialog.FileName);
      SetFilePath;
      end;
    3: begin     //bmp
       Sbitmap.SaveToFile(SaveAsDialog.FileName);
       SetFilePath;
    end;
    4:begin//jpg
      Sjpg:=TJPEGImage.Create;
      Sjpg.Assign(Sbitmap);
      Sjpg.SaveToFile(SaveAsDialog.FileName);
      SetFilePath;
    end;
  end;
end;

procedure TSmall_Editor.OpenFile(s: string; Merge: boolean);
var
  jData, jf: TJSONData;
  i, k: integer;
  PropList: PPropList;
begin
  try
    jData := GetJSON(s);
  except
    if not Merge then
      ShowMessage('Файл поврежден! Загрузка не удалась.');
    Exit;
  end;
  if not Merge then
    SetLength(FigureItems, 0);
  for i := 0 to jData.FindPath('Count').AsInteger - 1 do
  begin
    SetLength(FigureItems, Length(FigureItems) + 1);
    jf := jData.FindPath('Items').Items[i];

    case jf.FindPath('Class').AsString of
      'TRectangle': FigureItems[High(FigureItems)] := TRectangle.Create;
      'TEllipse': FigureItems[High(FigureItems)] := TEllipse.Create;
      'TRoundRect': FigureItems[High(FigureItems)] := TRoundRect.Create;
      'TPolyLine': FigureItems[High(FigureItems)] := TPolyLine.Create;
    end;
    if FigureItems[High(FigureItems)] = nil then
      exit;
    k := GetPropList(FigureItems[High(FigureItems)], PropList);
    FigureItems[High(FigureItems)].openFigure(jf, k, PropList);
  end;
  jData.Free;
  DrawArea.Invalidate;


end;

procedure TSmall_Editor.SaveClick(Sender: TObject);
begin
  if not WasSaved and not SaveDialog.Execute then
      Exit;
  if (not WasOpened) and (SaveDialog.FileName <> '') then
    CurrentSavedPath:=SaveDialog.FileName;
  Saving;
end;

procedure TSmall_Editor.Saving;
var
  TFile: Text;
begin
  AssignFile(TFile, CurrentSavedPath);
  Rewrite(TFile);
  SaveFile(TFile);
  CloseFile(TFile);
  FilePath.Caption := CurrentSavedPath;
  SavedFigures := FSaveFile();
  WasChanged := True;
  WasSaved:=True;
end;

procedure TSmall_Editor.PushHistAndState;
begin
  PushHistory;
  IsSavedState;
  DrawArea.Invalidate;
end;

procedure TSmall_Editor.SaveFile(var AText: Text);
var
  i: TFigure;
begin
  Write(AText, '{"Count": ', Length(FigureItems), ',');
  Write(AText, '"Items":[');
  for i in FigureItems do
  begin
    i.saveFigure(AText, 0);
    if i <> FigureItems[High(FigureItems)] then
      Write(AText, ',');
  end;
  Write(AText, ']}');
end;

function TSmall_Editor.FSaveFile(): string;
var
  AFText: Text;
  DataSave, tmpFilePath: string;
begin
  tmpFilePath := 'rfghdfuilgavlkjttlrsueihtyjiegy23523525hsdkgdgh84t8gcguiwegt83.txt';
  AssignFile(AFText, tmpFilePath);
  Rewrite(AFText);
  SaveFile(AFText);
  CloseFile(AFText);
  AssignFile(AFText, tmpFilePath);
  Reset(AFText);
  Read(AFText, DataSave);
  FSaveFile := DataSave;
  CloseFile(AFText);
  AssignFile(AFText, tmpFilePath);
  Erase(AFText);
end;

procedure TSmall_Editor.setFilePath;
begin
  FilePath.Caption := CurrentSavedPath;
  SavedFigures := FSaveFile();
  WasChanged := True;
  WasSaved:=True;
end;

procedure TSmall_Editor.PushHistory;
begin
  if Length(FigureItems) = 0 then
    exit;
  SetLength(History, Length(History) + 1);
  History[High(History)] := FSaveFile();
  HistoryIndex := High(History);
end;

procedure TSmall_Editor.PopHistory;
begin
  if (HistoryIndex <= 0) then
    HistoryIndex := 0;
  OpenFile(History[HistoryIndex], False);
end;

procedure TSmall_Editor.SetHistory;
begin
  //if not WasUnDo then exit;
  if HistoryIndex < 0 then
  begin
    SetLength(History, 0);
    NullHist;
    exit;
  end;
  SetLength(History, (HistoryIndex + 1));
  if Length(History) = 0 then
    NullHist;
end;

procedure TSmall_Editor.NullHist;
begin
  SetLength(History, 1);
  History[0] := DefCondHistory;
  HistoryIndex := High(History);
end;

procedure TSmall_Editor.IsSavedState;
var
  tmpStr: string;
  LentmpStr: integer;
begin
  tmpStr := FilePath.Caption;
  LentmpStr := Length(tmpStr);
  if History[HistoryIndex] = SavedFigures then
    WasChanged := True
  else
    WasChanged := False;
  if not WasChanged then
  begin
    if tmpStr[LentmpStr] = '*' then
      exit;
    tmpStr += '*';
    FilePath.Caption := tmpStr;
  end
  else
  begin
    if tmpStr[LentmpStr] = '*' then
      Delete(tmpStr, LentmpStr, 1);
    FilePath.Caption := tmpStr;
    WasChanged := True;
  end;
end;

end.
