program vectgraph1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UTool, UMain, UFigure, UDefine, UComparator, UTransform,
  UCreateAttributes, About
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TSmall_Editor, Small_Editor);
  Application.CreateForm(TTools, Tools);
  Application.Run;
end.

