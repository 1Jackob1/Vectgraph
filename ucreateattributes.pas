unit UCreateAttributes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, Graphics, ExtCtrls, StdCtrls, Controls, spin,
  LCLType, FPCanvas, Dialogs, UDefine, UFigure;

type

  { TFigureAttr }

  TFigureAttr = class
  private
    NameOfAttr: TLabel;
    Objs: array of TPersistent;
    AboutAttr: PPropInfo;
    procedure OnChange(Sender: TObject); virtual;
  public
    constructor Create(AObjs: array of TPersistent; AAboutAttr: PPropInfo;
      defProp: boolean); virtual;
    destructor Destroy; override;
    procedure ReBuild; virtual; abstract;

  end;

  { TEditCreate }

  TEditCreate = class
  private
    Objs: array of TPersistent;
    EditTools: array of TFigureAttr;
    AttrName: TLabel;
    SingleProp: boolean;
  public
    constructor Create; virtual;
    destructor Destroy;
    procedure SelectAttrs(AObj: TPersistent);
    procedure SelectManyAttrs(AObjs: array of TPersistent);
    //procedure Rebuild;
    procedure Cls;
  end;

  { TLineStyleEdit }

  TLineStyleEdit = class(TFigureAttr)
  private
    LineStyleComboBox: TComboBox;
    procedure OnChange(Sender: TObject); override;
  public
    constructor Create(AObjs: array of TPersistent; AAboutAttr: PPropInfo;
      defProp: boolean); override;
    destructor Destroy; override;
    procedure ReBuild; override;
  end;

  { TBrushStyleEdit }

  TBrushStyleEdit = class(TFigureAttr)
  private
    BrushStyleComboBox: TComboBox;
    procedure OnChange(Sender: TObject); override;
  public
    constructor Create(AObjs: array of TPersistent; AAboutAttr: PPropInfo;
      defProp: boolean); override;
    destructor Destroy; override;
    procedure ReBuild; override;
  end;

  { TEditSpin }

  TEditSpin = class(TFigureAttr)
  private
    DSpin: TSpinEdit;
    Changed: boolean;
    procedure OnChange(Sender: TObject); override;
  public
    constructor Create(AObjs: array of TPersistent; AAboutAttr: PPropInfo;
      defProp: boolean); override;
    destructor Destroy; override;
    procedure ReBuild; override;
  end;

  { TColorEdit }
  TColorEdit = class(TFigureAttr)
  private
    ColorBttn: TColorButton;
    procedure OnChange(Sender: TObject); override;
  public
    constructor Create(AObjs: array of TPersistent; AAboutAttr: PPropInfo;
      defProp: boolean); override;
    destructor Destroy; override;
    //procedure ReBuild; override;
  end;



  FigureAttrClass = class of TFigureAttr;

  RecAttr = record
    Item: FigureAttrClass;
    ItemName: ShortString;
  end;
  ArrRecAttr = array of RecAttr;

  { TRegEditTools }

  TRegEditTools = class
  private
    ArrAttr: array of RecAttr;
  public
    procedure RegTool(AItemName: ShortString; AAttr: FigureAttrClass);
    property EditTool: ArrRecAttr read ArrAttr;

  end;

var
  AttrNames: TStringList;
  AttrValues: TStringList;
  EditToolsContainer: TRegEditTools;
  EditFigure: TEditCreate;

implementation

uses Umain;


{ TFigureAttr }

constructor TFigureAttr.Create(AObjs: array of TPersistent;
  AAboutAttr: PPropInfo; defProp: boolean);
var
  i: integer;
begin
  SetLength(Objs, Length(AObjs));
  for i := 0 to High(AObjs) do
    Objs[i] := AObjs[i];
  AboutAttr := AAboutAttr;
  NameOfAttr := TLabel.Create(nil);
  NameOfAttr.Parent := VectGraph.AttributesBar;
  NameOfAttr.Caption := Attrnames.Values[AboutAttr^.Name];
  NameOfAttr.Width := Round(VectGraph.AttributesBar.Width / 2) - 5;
  NameOfAttr.Top := VectGraph.AttributesBar.Tag + 4;
  NameOfAttr.Left := 2;
  VectGraph.AttributesBar.Tag := VectGraph.AttributesBar.Tag + 20;
end;

procedure TFigureAttr.OnChange(Sender: TObject);
begin
  VectGraph.DrawArea.Invalidate;
end;

destructor TFigureAttr.Destroy;
begin
  NameOfAttr.Destroy;
end;

{ TEditCrete }

constructor TEditCreate.Create;
begin

end;

procedure TEditCreate.SelectAttrs(AObj: TPersistent);
var
  ArrOfTPer: array of TPersistent;
begin
  if AObj <> nil then
  begin
    SingleProp := True;
    SetLength(ArrOfTPer, 1);
    ArrOfTPer[0] := AObj;
    SelectManyAttrs(ArrOfTPer);
  end
  else
    exit;
end;

procedure TEditCreate.SelectManyAttrs(AObjs: array of TPersistent);

  function CheckObj(AObj: TPersistent; AProp: PPropInfo): boolean;
  var
    _i, _j: integer;
    PropList: PPropList;
  begin
    _j := GetPropList(AObj, PropList);
    Result := False;
    for _i := 0 to _j - 1 do
      if PropList^[_i] = AProp then
      begin
        Result := True;
        Exit;
      end;
  end;

var
  PropertyList: PPropList;
  i, j, k: integer;
  CrossOut: boolean;
begin
  Cls;
  SetLength(Objs, Length(AObjs));
  if Length(Objs) = 0 then
    exit
  else
  begin
    for i := 0 to High(AObjs) do
      Objs[i] := AObjs[i];
    k := GetPropList(Objs[0], PropertyList);
    for i := 0 to k - 1 do
    begin
      CrossOut := True;
      {for j := 1 to High(Objs) do
        if not (CheckObj(Objs[j], PropertyList^[i])) then
        begin
          CrossOut := False;
          Break;
        end;}
      if CrossOut then
      begin
        for j := 0 to High(EditToolsContainer.EditTool) do
        begin
          if PropertyList^[i]^.Name = EditToolsContainer.EditTool[j].ItemName then
          begin
            SetLength(EditTools, Length(EditTools) + 1);
            EditTools[High(EditTools)] :=
              EditToolsContainer.EditTool[j].Item.Create(Objs,
              PropertyList^[i], CrossOut);
            break;
          end;
        end;
      end;
    end;
  end;
  SingleProp := False;
end;

destructor TEditCreate.Destroy;
var
  i: integer;
begin
  for i := 0 to High(EditTools) do
    EditTools[i].Destroy;
  SetLength(EditTools, 0);
end;

procedure TEditCreate.Cls;
var
  i: integer;
begin
  for i := 0 to High(EditTools) do
    EditTools[i].Destroy;
  SetLength(EditTools, 0);
end;

{ TRegEditTools }

procedure TRegEditTools.RegTool(AItemName: ShortString; AAttr: FigureAttrClass);
begin
  SetLength(ArrAttr, Length(ArrAttr) + 1);
  ArrAttr[High(ArrAttr)].Item := AAttr;
  ArrAttr[High(ArrAttr)].ItemName := AItemName;
end;

{ TLineStyleEdit }

constructor TLineStyleEdit.Create(AObjs: array of TPersistent;
  AAboutAttr: PPropInfo; defProp: boolean);
var
  i: integer;
begin
  inherited Create(AObjs, AAboutAttr, defProp);
  LineStyleComboBox := TComboBox.Create(VectGraph.AttributesBar);
  LineStyleComboBox.Parent := VectGraph.AttributesBar;
  LineStyleComboBox.Left := trunc(VectGraph.AttributesBar.Width * 0.5) + 2;
  LineStyleComboBox.Width := trunc(VectGraph.AttributesBar.Width * 0.5) - 4;
  for i := 0 to Length(LineStyles) - 1 do
    LineStyleComboBox.Items.Add(LineStyles[i]._Type);
  LineStyleComboBox.OnChange := @OnChange;
  LineStyleComboBox.ReadOnly := True;
  LineStyleComboBox.ItemIndex := 1;
  LineStyleComboBox.Top := VectGraph.AttributesBar.Tag;
  if (defProp) and (AttrValues.Values[AboutAttr^.Name] <> '') then
    for i := 0 to High(AObjs) do ;
  SetInt64Prop(AObjs[i], AboutAttr, StrToInt64(AttrValues.Values[AboutAttr^.Name]));
  ReBuild;
end;


procedure TLineStyleEdit.OnChange(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to High(Objs) do
    SetInt64Prop(Objs[i], AboutAttr, TComboBox(Sender).ItemIndex);
  CurrentStyles.LineStyle := LineStyles[TComboBox(Sender).ItemIndex].LineStyle;
  AttrValues.Values[AboutAttr^.Name] := IntToStr(TComboBox(Sender).ItemIndex);
  inherited OnChange(Sender);
end;

destructor TLineStyleEdit.Destroy;
begin
  LineStyleComboBox.Destroy;
  inherited Destroy;
end;

procedure TLineStyleEdit.ReBuild;
var
  i, j: integer;
begin
  j := GetInt64Prop(objs[0], AboutAttr);
  for i := 1 to high(objs) do
    if GetInt64Prop(objs[i], AboutAttr) <> j then
    begin
      j := integer(psSolid);
      break;
    end;
  for i := 0 to LineStyleComboBox.Items.Count - 1 do
  begin
    if integer(PtrUint(LineStyleComboBox.Items.Objects[i])) = j then
    begin
      LineStyleComboBox.ItemIndex := i;
      exit;
    end;
  end;
end;

{ TBrushStyleEdit }

constructor TBrushStyleEdit.Create(AObjs: array of TPersistent;
  AAboutAttr: PPropInfo; defProp: boolean);
var
  i: integer;
begin
  inherited Create(AObjs, AAboutAttr, defProp);
  BrushStyleComboBox := TComboBox.Create(nil);
  BrushStyleComboBox.Parent := VectGraph.AttributesBar;
  BrushStyleComboBox.Left := Trunc(VectGraph.AttributesBar.Width * 0.5) + 2;
  BrushStyleComboBox.Width := Trunc(VectGraph.AttributesBar.Width * 0.5) - 4;
  for i := 0 to Length(FillStyles) - 1 do
    BrushStyleComboBox.Items.Add(FillStyles[i]._Type);
  BrushStyleComboBox.OnChange := @OnChange;
  BrushStyleComboBox.Style := csOwnerDrawFixed;
  BrushStyleComboBox.ReadOnly := True;
  BrushStyleComboBox.Top := VectGraph.AttributesBar.tag;
  if (defProp) and (AttrValues.Values[AboutAttr^.Name] <> '') then
    for i := 0 to High(AObjs) do ;
  SetInt64Prop(AObjs[i], AboutAttr, StrToInt64(AttrValues.Values[AboutAttr^.Name]));
  BrushStyleComboBox.ItemIndex := 1;
  ReBuild;
end;

procedure TBrushStyleEdit.OnChange(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to High(Objs) do
    SetInt64Prop(Objs[i], AboutAttr, TCombobox(Sender).ItemIndex);
  CurrentStyles.FillStyle := FillStyles[TCombobox(Sender).ItemIndex].BrushStyle;
  AttrValues.Values[AboutAttr^.Name] := IntToStr(TCombobox(Sender).ItemIndex);
  inherited OnChange(Sender);
end;

destructor TBrushStyleEdit.Destroy;
begin
  BrushStyleComboBox.Destroy;
  inherited Destroy;
end;

procedure TBrushStyleEdit.ReBuild;
var
  i, j: integer;
begin
  j := GetInt64Prop(objs[0], AboutAttr);
  for i := 1 to high(objs) do
    if GetInt64Prop(objs[i], AboutAttr) <> j then
    begin
      j := integer(psSolid);
      break;
    end;
  for i := 0 to BrushStyleComboBox.Items.Count - 1 do
  begin
    if integer(PtrUint(BrushStyleComboBox.Items.Objects[i])) = j then
    begin
      BrushStyleComboBox.ItemIndex := i;
      exit;
    end;
  end;
end;

{ TEditSpin }

constructor TEditSpin.Create(AObjs: array of TPersistent; AAboutAttr: PPropInfo;
  defProp: boolean);
var
  i: integer;
begin
  inherited Create(AObjs, AAboutAttr, defProp);
  DSpin := TSpinEdit.Create(nil);
  DSpin.MinValue := 1;
  DSpin.MaxValue := 100;
  DSpin.parent := VectGraph.AttributesBar;
  DSpin.Left := Trunc(VectGraph.AttributesBar.Width * 0.5) + 2;
  DSpin.Width := Trunc(VectGraph.AttributesBar.Width * 0.5) - 4;
  DSpin.OnChange := @OnChange;
  DSpin.Top := VectGraph.AttributesBar.Tag;
  if (defProp) and (AttrValues.Values[AboutAttr^.Name] <> '') then
    for i := 0 to High(AObjs) do ;
  SetInt64Prop(AObjs[i], AboutAttr, StrToInt64(AttrValues.Values[AboutAttr^.Name]));
  if AboutAttr^.Name = 'FLineWidth' then
    DSpin.Value := CurrentStyles.LineWidth
  else
    DSpin.Value := CurrentStyles.Flexure;
  ReBuild;
end;

procedure TEditSpin.OnChange(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to High(Objs) do
    SetInt64Prop(Objs[i], AboutAttr, TSpinEdit(Sender).Value);
  if AboutAttr^.Name = 'FLineWidth' then
    CurrentStyles.LineWidth := TSpinEdit(Sender).Value;
  if AboutAttr^.Name = 'FFlexure' then
    CurrentStyles.Flexure := TSpinEdit(Sender).Value;
  AttrValues.Values[AboutAttr^.Name] := IntToStr(TSpinEdit(Sender).Value);
  inherited OnChange(Sender);
end;

destructor TEditSpin.Destroy;
begin
  DSpin.Destroy;
  inherited Destroy;
end;

procedure TEditSpin.ReBuild;
var
  j: int64;
  i: integer;
begin
  j := GetInt64Prop(objs[0], AboutAttr);
  for i := 0 to high(objs) do
    if GetInt64Prop(objs[i], AboutAttr) <> j then
    begin
      j := GetInt64Prop(objs[i], AboutAttr);
      Changed := True;
      break;
    end;
  DSpin.Value := j;
end;

{ TColorEdit }

constructor TColorEdit.Create(AObjs: array of TPersistent; AAboutAttr: PPropInfo;
  defProp: boolean);
begin
  inherited Create(AObjs, AAboutAttr, defProp);
  ColorBttn := TColorButton.Create(nil);
  ColorBttn.Parent := VectGraph.AttributesBar;
  ColorBttn.Left := Trunc(VectGraph.AttributesBar.Width * 0.5) + 2;
  ColorBttn.Align := alCustom;
  ColorBttn.Width := 80;
  ColorBttn.OnColorChanged := @OnChange;
  if AboutAttr^.Name = 'FLineColor' then
    ColorBttn.ButtonColor := CurrentStyles.LineColor
  else
    ColorBttn.ButtonColor := CurrentStyles.FillColor;
end;

procedure TColorEdit.OnChange(Sender: TObject);
begin
  if AboutAttr^.Name = 'FLineColor' then
    CurrentStyles.LineColor := ColorBttn.ButtonColor
  else
    CurrentStyles.FillColor := ColorBttn.ButtonColor;
  inherited OnChange(Sender);
end;

destructor TColorEdit.Destroy;
begin
  ColorBttn.Destroy;
  inherited Destroy;
end;



initialization

  EditToolsContainer := TRegEditTools.Create;
  EditToolsContainer.RegTool('FLineType', TLineStyleEdit);
  EditToolsContainer.RegTool('FLineWidth', TEditSpin);
  EditToolsContainer.RegTool('FFillType', TBrushStyleEdit);
  EditToolsContainer.RegTool('FFlexure', TEditSpin);
  EditToolsContainer.RegTool('FLineColor', TColorEdit);
  EditToolsContainer.RegTool('FFillColor', TColorEdit);

  AttrValues := TStringList.Create;
  AttrValues.Values['FLineWidth'] := '4';
  AttrValues.Values['FLineType'] := '2';
  AttrValues.Values['FFillType'] := '0';
  AttrValues.Values['FFlexure'] := '15';

  AttrNames := TStringList.Create;
  AttrNames.Values['FLineWidth'] := 'Толщина линии';
  AttrNames.Values['FLineColor'] := 'Цвет линии';
  AttrNames.Values['FLineType'] := 'Тип линии';
  AttrNames.Values['FFillType'] := 'Тип заливки';
  AttrNames.Values['FFillColor'] := 'Цвет заливки';
  AttrNames.Values['FFlexure'] := 'Округление углов';

end.
