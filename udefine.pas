unit UDefine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, FPCanvas;

type

  TDoublePoint = record
    X, Y: real;
  end;

  TDRect = record
    Top, Bottom: TDoublePoint;
  end;

  RecFillType = record
    _Type: string;
    BrushStyle: TBrushStyle;
  end;

  RecLineType = record
    _Type: string;
    LineStyle: TFPPenStyle;
  end;

  Styles = record
    LineStyleNum: integer;
    LineWidth: integer;
    LineColor: TColor;
    FillStyleNum: integer;
    FillColor: TColor;
    Flexure: integer;
  end;

const
  FillStyles: array[0..7] of RecFillType = (
    (_Type: 'Solid'; BrushStyle: bsSolid),
    (_Type: 'Empty'; BrushStyle: bsClear),
    (_Type: 'Horizontal'; BrushStyle: bsHorizontal),
    (_Type: 'Vertical'; BrushStyle: bsVertical),
    (_Type: 'LDiagonal'; BrushStyle: bsFDiagonal),
    (_Type: 'RDiagonal'; BrushStyle: bsBDiagonal),
    (_Type: 'Cross'; BrushStyle: bsCross),
    (_Type: 'DiagCross'; BrushStyle: bsDiagCross)
    );

  LineStyles: array[0..4] of RecLineType = (
    (_Type: '―'; LineStyle: psSolid),
    (_Type: '―  ―'; LineStyle: psDash),
    (_Type: '•'; LineStyle: psDot),
    (_Type: '― •'; LineStyle: psDashDot),
    (_Type: '― • •'; LineStyle: psDashDotDot)
    );

  TOOL_BUTTON_SIZE: integer = 36;
  TOOL_BUTTON_MARGIN: integer = 2;
  TOOL_BUTTON_PADDING: integer = 2;
  MIN_ZOOM: real = 100;
  MAX_ZOOM: real = 10000;

implementation

end.
