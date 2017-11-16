unit UCreateAttributes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, Graphics, ExtCtrls, StdCtrls, Controls, spin,
  LCLType, FPCanvas, UComparator, Dialogs, UDefine, UFigure;

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
      destructor Destroy;
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
  PropertyList: PPropList;
  i, j:Integer;
begin
   if Aobj = nil then begin
   //Cls;
   Exit;
   end;
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
  CurrentStyles.LineColor:=AColor;
  SetColor('FLineColor', AColor);
end;

procedure TEditCreate.SetBrushColor(AColor: TColor);
begin
  BrushColor := clBlue;
  CurrentStyles.FillColor:=clBlue;
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

destructor TEditCreate.Destroy;
var
  i: Integer;
begin
  for i:=0 to High(EditTools) do
    EditTools[i].Destroy;
  SetLength(EditTools,0);
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
  i: Integer;
begin
  inherited Create(AObj, AAboutAttr);
  LineStyleComboBox:= TComboBox.Create(VectGraph.AttributesBar);
  LineStyleComboBox.Parent:=VectGraph.AttributesBar;
  LineStyleComboBox.Left := trunc(VectGraph.AttributesBar.width * 0.5) + 2;
  LineStyleComboBox.width := trunc(VectGraph.AttributesBar.Width * 0.5) - 4;
  for i:=0 to Length(LineStyles)-1 do
      LineStyleComboBox.Items.Add(LineStyles[i]._Type);
  LineStyleComboBox.OnChange := @OnChange;
  LineStyleComboBox.ReadOnly := True;
  LineStyleComboBox.ItemIndex:=1;
  LineStyleComboBox.Top := VectGraph.AttributesBar.Tag;
  if AttrValues.Values[AboutAttr^.Name] <> '' then
    SetInt64Prop(obj, AboutAttr, StrToInt64(AttrValues.Values[AboutAttr^.Name]));
end;


procedure TLineStyleEdit.OnChange(Sender: TObject);
var
tmp: Integer;
begin
  SetInt64Prop(obj, AboutAttr, TComboBox(Sender).ItemIndex);
  CurrentStyles.LineStyle:=LineStyles[TComboBox(Sender).ItemIndex].LineStyle;
  AttrValues.Values[AboutAttr^.Name] := intToStr(TComboBox(Sender).ItemIndex);
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
  inherited Create(AObj, AAboutAttr);
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
  //inherited Create(AObj, AAboutAttr);
  if (AttrValues.Values[AboutAttr^.Name] <> '') then
   SetInt64Prop(obj, AboutAttr, StrToInt64(AttrValues.Values[AboutAttr^.Name]));
  BrushStyleComboBox.ItemIndex:=1;
  end;

procedure TBrushStyleEdit.OnChange(Sender: TObject);
begin
  SetInt64Prop(obj, AboutAttr, TCombobox(sender).ItemIndex);
  CurrentStyles.FillStyle:=FillStyles[TCombobox(sender).ItemIndex].BrushStyle;
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
begin
  inherited Create(AObj, AAboutAttr);
  DSpin := TSpinEdit.Create(nil);
  DSpin.MinValue := 1;
  DSpin.MaxValue := 100;
  DSpin.parent := VectGraph.AttributesBar;
  DSpin.Left := Trunc(VectGraph.AttributesBar.Width * 0.5) + 2;
  DSpin.width := Trunc(VectGraph.AttributesBar.Width * 0.5) - 4;
  DSpin.OnChange := @OnChange;
  DSpin.Top := VectGraph.AttributesBar.Tag;
  //inherited Create(AObj, AAboutAttr);
  if (AttrValues.Values[AboutAttr^.Name] <> '') then
   SetInt64Prop(obj, AboutAttr, StrToInt64(AttrValues.Values[AboutAttr^.Name]));
  if AboutAttr^.Name='FLineWidth' then
     DSpin.Value:=CurrentStyles.LineWidth
  else
     DSpin.Value:=CurrentStyles.Flexure;
end;

procedure TEditSpin.OnChange(Sender: TObject);
begin
  SetInt64Prop(obj, AboutAttr, TSpinEdit(Sender).Value);
  if AboutAttr^.Name='FLineWidth' then
     CurrentStyles.LineWidth:=TSpinEdit(Sender).Value;
  if AboutAttr^.Name='FFlexure' then
     CurrentStyles.Flexure:=TSpinEdit(Sender).Value;
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
AttrValues.Values['FLineWidth']:='4';
AttrValues.Values['FLineType']:='2';//string(START_LINE_STYLE);
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

