unit UCreateAttributes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, Graphics, ExtCtrls, StdCtrls, Controls, spin,
  LCLType, FPCanvas, UComparator, Dialogs, UDefine;

type

  { TFigureAttr }

  TFigureAttr = class
    private
      NameOfAttr: TLabel;
      Obj: TPersistent;
      AboutAttr: PPropInfo;
      procedure OnChange(Sender: TObject); virtual;
    public
      constructor Create(AObj: TPersistent; AAboutAttr: PPropInfo); virtual;
      destructor Destroy; override;

  end;

  { TEditCreate }

  TEditCreate = class
    private
      Obj: TPersistent;
      EditTools: array of TFigureAttr;
      AttrName: TLabel;
      PenColor, BrushColor: TColor;
      procedure SetColor(AString: String; AColor: TColor);
    public
      constructor Create; virtual;
      procedure SelectAttrs(AObj: TPersistent);
      procedure Cls;
      procedure SetPenColor(AColor: TColor);
      procedure SetBrushColor(AColor: TColor);
  end;

  { TLineStyleEdit }

  TLineStyleEdit = class(TFigureAttr)
    private
    LineStyleComboBox: TComboBox;
    procedure OnChange(Sender: TObject); override;
    public
      constructor Create(AObj: TPersistent; AAboutAttr: PPropInfo); override;
      destructor Destroy; override;
  end;

  { TBrushStyleEdit }

  TBrushStyleEdit = class(TFigureAttr)
    private
      BrushStyleComboBox: TComboBox;
      procedure OnChange(Sender: TObject); override;
    public
      constructor Create(AObj:TPersistent; AAboutAttr: PPropInfo); override;
      destructor Destroy; override;
  end;

  { TEditSpin }

  TEditSpin = class(TFigureAttr)
    private
      DSpin: TSpinEdit;
      procedure OnChange(Sender: TObject); override;
    public
      constructor Create(AObj: TPersistent; AAboutAttr: PPropInfo); override;
      destructor Destroy; override;
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
      property  EditTool:ArrRecAttr read ArrAttr;

  end;

  var
    AttrNames, AttrValues: TStringList;
    EditToolsContainer: TRegEditTools;
    EditFigure: TEditCreate;
    DAInvalidate: procedure of Object;
implementation
  uses Umain;
 { TFigureAttr }

constructor TFigureAttr.Create(AObj: TPersistent; AAboutAttr: PPropInfo);
begin
     AboutAttr:=AAboutAttr;
     NameOfAttr:=TLabel.Create(nil);
     NameOfAttr.Parent:=VectGraph.AttributesBar;
     NameOfAttr.Caption:=Attrnames.Values[AboutAttr^.Name];
     NameOfAttr.Width:=Round(VectGraph.AttributesBar.Width/2) - 5;
     NameOfAttr.Top:=VectGraph.AttributesBar.Tag+4;
     NameOfAttr.Left:=2;
     VectGraph.AttributesBar.Tag:=VectGraph.AttributesBar.Tag+20;
     Obj:=AObj;
end;

procedure TFigureAttr.OnChange(Sender: TObject);
begin
     DAInvalidate;
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
  PropertyList: PPropList;
  i, j, g:Integer;
begin
   obj:=AObj;
   for i:=0 to GetPropList(AObj, PropertyList) - 1 do begin
     for j:=0 to Length(EditToolsContainer.EditTool)-1 do begin
       if PropertyList^[i]^.Name = EditToolsContainer.EditTool[j].ItemName then
       begin
         SetLength(EditTools, Length(EditTools)+1);
         EditTools[High(EditTools)] := EditToolsContainer.EditTool[j].Item.Create(Aobj, PropertyList^[i]);
         break;
       end;
     end;
   end;
   SetPenColor(PenColor);
   setBrushColor(BrushColor);
end;

procedure TEditCreate.Cls;
var
  i: integer;
begin
for i:=0 to High(EditToolsContainer.ArrAttr) do
    FreeAndNil(EditToolsContainer.ArrAttr[i]);
SetLength(EditToolsContainer.ArrAttr,0);
end;

procedure TEditCreate.SetPenColor(AColor: TColor);
begin
  PenColor := AColor;
  SetColor('FLineColor', AColor);
end;

procedure TEditCreate.SetBrushColor(AColor: TColor);
begin
  BrushColor := AColor;
  SetColor('FFillColor', AColor);
end;

procedure TEditCreate.SetColor(AString: String; AColor: TColor);
var
  PropertyList: PPropList;
  i, j: integer;
begin
    j := GetPropList(obj, PropertyList);
    for i := 0 to j - 1 do begin
      if PropertyList^[i]^.Name = AString then SetInt64Prop(obj, PropertyList^[i], AColor);
  end;
  VectGraph.DrawArea.Invalidate;
end;

 { TRegEditTools }

procedure TRegEditTools.RegTool(AItemName: ShortString; AAttr: FigureAttrClass);
begin
  SetLength(ArrAttr,Length(ArrAttr)+1);
  ArrAttr[High(ArrAttr)].Item:=AAttr;
  ArrAttr[High(ArrAttr)].ItemName:= AItemName;
end;

  { TLineStyleEdit }

constructor TLineStyleEdit.Create(AObj: TPersistent; AAboutAttr: PPropInfo);
var
  tmpLineType: TFPPenStyle;
  i: Integer;
  str: String;
begin
  LineStyleComboBox:= TComboBox.Create(VectGraph.AttributesBar);
  LineStyleComboBox.Parent:=VectGraph.AttributesBar;
  LineStyleComboBox.Left := trunc(VectGraph.AttributesBar.width * 0.5) + 2;
  LineStyleComboBox.width := trunc(VectGraph.AttributesBar.Width * 0.5) - 4;
  for i:=0 to Length(LineStyles)-1 do
      LineStyleComboBox.Items.Add(LineStyles[i]._Type);
  LineStyleComboBox.OnChange := @OnChange;
  LineStyleComboBox.ReadOnly := True;
  LineStyleComboBox.Top := VectGraph.AttributesBar.Tag;
  inherited Create(AObj, AAboutAttr);
  str:=AttrValues.Values[AboutAttr^.Name];
  if AttrValues.Values[AboutAttr^.Name] <> '' then
    SetInt64Prop(obj, AboutAttr, StrToInt64(AttrValues.Values[AboutAttr^.Name]));
  i:=9;
end;


procedure TLineStyleEdit.OnChange(Sender: TObject);
var
  i: Integer;
begin
  SetInt64Prop(obj, AboutAttr, LineStyleComboBox.ItemIndex);
  AttrValues.Values[AboutAttr^.Name] := intToStr(LineStyleComboBox.ItemIndex);
  inherited OnChange(Sender);
end;

destructor TLineStyleEdit.Destroy;
begin
  LineStyleComboBox.Destroy;
  inherited Destroy;
end;

 { TBrushStyleEdit }

constructor TBrushStyleEdit.Create(AObj: TPersistent; AAboutAttr: PPropInfo);
  var
 i: Integer;
begin
  BrushStyleComboBox := TComboBox.Create(nil);
  BrushStyleComboBox.Parent := VectGraph.AttributesBar;
  BrushStyleComboBox.Left := Trunc(VectGraph.AttributesBar.Width * 0.5) + 2;
  BrushStyleComboBox.Width := Trunc(VectGraph.AttributesBar.Width * 0.5) - 4;
  for i:=0 to Length(FillStyles)-1 do
    BrushStyleComboBox.Items.Add(FillStyles[i]._Type);
  BrushStyleComboBox.OnChange := @OnChange;
  BrushStyleComboBox.Style := csOwnerDrawFixed;
  BrushStyleComboBox.ReadOnly := True;
  BrushStyleComboBox.Top := VectGraph.AttributesBar.tag;
  inherited Create(AObj, AAboutAttr);
  if (AttrValues.Values[AboutAttr^.Name] <> '') then
   SetInt64Prop(obj, AboutAttr, StrToInt64(AttrValues.Values[AboutAttr^.Name]));
  end;

procedure TBrushStyleEdit.OnChange(Sender: TObject);
begin
  SetInt64Prop(obj, AboutAttr, TCombobox(sender).ItemIndex);
  AttrValues.Values[AboutAttr^.Name] := intToStr(TCombobox(sender).ItemIndex);
  inherited OnChange(Sender);
end;

destructor TBrushStyleEdit.Destroy;
begin
  BrushStyleComboBox.Destroy;
  inherited Destroy;
end;

 { TEditSpin }

constructor TEditSpin.Create(AObj: TPersistent; AAboutAttr: PPropInfo);
var
  i: integer;
begin
  DSpin := TSpinEdit.Create(nil);
  DSpin.MinValue := 1;
  DSpin.MaxValue := 100;
  DSpin.parent := VectGraph.AttributesBar;
  DSpin.Left := Trunc(VectGraph.AttributesBar.Width * 0.5) + 2;
  DSpin.width := Trunc(VectGraph.AttributesBar.Width * 0.5) - 4;
  DSpin.OnChange := @OnChange;
  DSpin.Top := VectGraph.AttributesBar.Tag;
  inherited Create(AObj, AAboutAttr);
  if (AttrValues.Values[AboutAttr^.Name] <> '') then
   SetInt64Prop(obj, AboutAttr, StrToInt64(AttrValues.Values[AboutAttr^.Name]));
end;

procedure TEditSpin.OnChange(Sender: TObject);
begin
  SetInt64Prop(obj, AboutAttr, TSpinEdit(Sender).Value);
  AttrValues.Values[AboutAttr^.Name] := intToStr(TSpinEdit(Sender).Value);
  inherited OnChange(Sender);
end;

destructor TEditSpin.Destroy;
begin
  DSpin.Destroy;
  inherited Destroy;
end;

initialization

EditToolsContainer:= TRegEditTools.Create;
EditToolsContainer.RegTool('FLineType',TLineStyleEdit);
EditToolsContainer.RegTool('FLineWidth',TEditSpin);
EditToolsContainer.RegTool('FFillType',TBrushStyleEdit);
EditToolsContainer.RegTool('FFlexure',TEditSpin);

AttrValues:=TStringList.Create;
AttrValues.Values['FLineType']:='0';//string(START_LINE_STYLE);
AttrValues.Values['FFillType']:='0';//string(START_FILL_STYLE);
AttrValues.Values['FFlexure']:='15';//string(START_FLEXURE_VALUE);

AttrNames:=TStringList.Create;
AttrNames.Values['FLineWidth']:='Толщина линии';
AttrNames.Values['FLineColor']:='Цвет линии';
AttrNames.Values['FLineType']:='Тип линии';
AttrNames.Values['FFillType']:='Тип заливки';
AttrNames.Values['FFillColor']:='Цвет заливки';
AttrNames.Values['FFlexure']:='Округление углов';


end.

