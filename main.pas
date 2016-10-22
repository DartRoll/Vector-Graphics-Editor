unit main;

{$mode objfpc}{$H+}{$R+}

interface

uses
  Classes, Contnrs, SysUtils, FileUtil, ColorPalette, Forms, Controls, Graphics,
  Dialogs, Menus, ExtCtrls, StdCtrls, aboutprogram, LCLType, Spin, ComCtrls,
  Buttons, UFigures, UTools;

type

  { TVectorEditor }

  TVectorEditor = class(TForm)
    ColorPalette: TColorPalette;
    MainMenu: TMainMenu;
    FileMenuItem: TMenuItem;
    HelpMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    FileDividerMenuItem: TMenuItem;
    ClearMenuItem: TMenuItem;
    PaintBox: TPaintBox;
    InstrumentPanel: TPanel;
    PolylineBtn: TSpeedButton;
    RectangleBtn: TSpeedButton;
    EllipseBtn: TSpeedButton;
    LineBtn: TSpeedButton;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure ClearMenuItemClick(Sender: TObject);
    procedure ColorPaletteColorPick(Sender: TObject; AColor: TColor;
      Shift: TShiftState);
    procedure EllipseBtnClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure LineBtnClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PolylineBtnClick(Sender: TObject);
    procedure RectangleBtnClick(Sender: TObject);

  private
    { private declarations }

  public
    { public declarations }

  end;

var
  Figures: array of TFigure;
  CurrentTool: TTool;
  PaintingFlag: Boolean = False;
  VectorEditor: TVectorEditor;

implementation

{$R *.lfm}

{ TVectorEditor }

procedure PushFigure(Figure: TFigure);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := Figure;
end;

procedure ClearFigures;
var i:Integer;
begin
  for i := 0 to High(Figures) do
    begin
      Figures[i].Free;
    end;
  Figures := nil;
end;

procedure SetCurrentTool(Tool: TTool);
begin
  CurrentTool.Free;
  CurrentTool := Tool;
end;

procedure TVectorEditor.ExitMenuItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TVectorEditor.LineBtnClick(Sender: TObject);
begin
  SetCurrentTool(TLineTool.Create);
end;

procedure TVectorEditor.PaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PaintingFlag := True;
  CurrentTool.MouseDown(X, Y);
  CurrentTool.SetColor(PaintBox.Canvas.Pen.Color);
end;

procedure TVectorEditor.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if PaintingFlag then
  begin
    PaintBox.Repaint;//TODO: найти способ поэффективнее
    CurrentTool.MouseMove(X, Y);
    CurrentTool.GetFigure.Draw(PaintBox.Canvas);
  end;

end;

procedure TVectorEditor.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  PaintingFlag := False;
  CurrentTool.MouseUp(X, Y);
  PushFigure(CurrentTool.GetFigure);
end;

procedure TVectorEditor.PaintBoxPaint(Sender: TObject);
var i:integer;
begin
  for i := 0 to High(Figures) do
  begin
      PaintBox.Canvas.Pen.Color := Figures[i].Color;
      Figures[i].Draw(PaintBox.Canvas);
  end;
end;

procedure TVectorEditor.PolylineBtnClick(Sender: TObject);
begin
  SetCurrentTool(TPolylineTool.Create);
end;

procedure TVectorEditor.RectangleBtnClick(Sender: TObject);
begin
  SetCurrentTool(TRectangleTool.Create);
end;


procedure TVectorEditor.AboutMenuItemClick(Sender: TObject);
begin
  aboutprogram.aboutProgramForm.Show;
end;

procedure TVectorEditor.ClearMenuItemClick(Sender: TObject);
begin
  ClearFigures;
  PaintBox.Canvas.Clear;
end;

procedure TVectorEditor.ColorPaletteColorPick(Sender: TObject; AColor: TColor;
  Shift: TShiftState);
begin
  PaintBox.Canvas.Pen.Color := AColor;
end;

procedure TVectorEditor.EllipseBtnClick(Sender: TObject);
begin
  SetCurrentTool(TEllipseTool.Create);
end;

end.

