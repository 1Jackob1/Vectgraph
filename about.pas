unit About;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TTools }

  TTools = class(TForm)
    ToolPenLabel: TLabel;
    ToolLoupeMinus: TImage;
    ToolLineLabel: TLabel;
    ToolHandLabel: TLabel;
    ToolRectLabel: TLabel;
    ToolRoundRectLabel: TLabel;
    ToolEllipseLabel: TLabel;
    ToolLoupeLabel: TLabel;
    ToolPoupeMinusLabel: TLabel;
    ToolSelectionLabel: TLabel;
    ToolLoupePlusLabel: TLabel;
    ToolResizeLabel: TLabel;
    ToolSelection: TImage;
    ToolPen: TImage;
    ToolLine: TImage;
    ToolRect: TImage;
    ToolRoundRect: TImage;
    ToolEllipse: TImage;
    ToolLoupe: TImage;
    ToolLoupePlus: TImage;
    ToolResize: TImage;
    ToolHand: TImage;

  private
    { private declarations }
  public
    procedure FormCreate(Sender: TObject);
    { public declarations }
  end;

var
  Tools: TTools;

implementation

{$R *.lfm}

{ TTools }


procedure TTools.FormCreate(Sender: TObject);
begin

end;

end.

