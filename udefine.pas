unit UDefine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, FPCanvas;
type

  TDoublePoint = record
     X,Y:Real;
  end;

  TDRect = record
     Top,Bottom: TDoublePoint;
  end;

  RecFillType=record
     _Type: string;
     BrushStyle: TBrushStyle;
   end;

  RecLineType=record
     _Type: string;
     LineStyle: TFPPenStyle;
     end;

  Styles = record
     LineStyle: TFPPenStyle;
     LineWidth: Integer;
     LineColor: TColor;
     FillStyle: TFPBrushStyle;
     FillColor: TColor;
     Flexure  : Integer;
  end;

    const
      FillStyles: array[0..7] of RecFillType = (
      (_Type: 'Empty';      BrushStyle: bsClear),
      (_Type: 'Solid';      BrushStyle: bsSolid),
      (_Type: 'Horizontal'; BrushStyle: bsHorizontal),
      (_Type: 'Vertical';   BrushStyle: bsVertical),
      (_Type: 'LDiagonal';  BrushStyle: bsFDiagonal),
      (_Type: 'RDiagonal';  BrushStyle: bsBDiagonal),
      (_Type: 'Cross';      BrushStyle: bsCross),
      (_Type: 'DiagCross';  BrushStyle: bsDiagCross)
      );
      LineStyles: array[0..4] of RecLineType =(
       (_Type: '―';     LineStyle: psSolid),
       (_Type: '――';    LineStyle: psDash),
       (_Type: '•';     LineStyle: psDot),
       (_Type: '― •';   LineStyle: psDashDot),
       (_Type: '― • •'; LineStyle: psDashDotDot)
        );
      START_LINE_WIDTH:    Integer       = 1;
      START_LINE_STYLE:    TFPPenStyle   = psSolid;//LineStyles[0].LineStyle;
      START_FILL_STYLE:    TFPBrushStyle = bsSolid;//FillStyles[0].BrushStyle;
      START_FILL_COLOR:    TColor        = clWhite;
      START_LINE_COLOR:    TColor        = clBlack;
      START_FLEXURE_VALUE: Integer       = 10;
      START_ZOOM:          Integer       = 1;
      TOOL_BUTTON_SIZE:    Integer       = 36;
      TOOL_BUTTON_MARGIN:  Integer       = 2;
      TOOL_BUTTON_PADDING: Integer       = 2;
      MIN_ZOOM: Real = 1;
      MAX_ZOOM: Real = 1000;

implementation

end.

