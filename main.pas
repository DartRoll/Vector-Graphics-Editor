unit main;

{$mode objfpc}{$H+}{$R+}

interface

uses
  Classes, Contnrs, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, Menus, ExtCtrls, StdCtrls, aboutprogram, LCLType, Spin, ComCtrls,
  Buttons, ActnList, UFigures, UTools;

type

  { TVectorEditor }

  TVectorEditor = class(TForm)
    BrushColorLabel: TLabel;
    LineWidthLabel: TLabel;
    PenColorLabel: TLabel;
    PenColorBtn: TColorButton;
    BrushColorBtn: TColorButton;
    MainMenu: TMainMenu;
    FileMenuItem: TMenuItem;
    HelpMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    FileDividerMenuItem: TMenuItem;
    ClearMenuItem: TMenuItem;
    PaintBox: TPaintBox;
    ToolPanel: TPanel;
    PolylineBtn: TSpeedButton;
    RectangleBtn: TSpeedButton;
    EllipseBtn: TSpeedButton;
    LineBtn: TSpeedButton;
    LineWidthSpinEdit: TSpinEdit;
    PointerBtn: TSpeedButton;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure BrushColorBtnColorChanged(Sender: TObject);
    procedure ClearMenuItemClick(Sender: TObject);
    procedure EllipseBtnClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LineBtnClick(Sender: TObject);
    procedure LineWidthSpinEditChange(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PenColorBtnColorChanged(Sender: TObject);
    procedure PolylineBtnClick(Sender: TObject);
    procedure RectangleBtnClick(Sender: TObject);

  private
    { private declarations }

  public
    { public declarations }

  end;

var
  Tools: array of TTool;
  Figures: array of TFigure;
  PenColor: TColor = clBlack;
  BrushColor: TColor = clWhite;
  LineWidth: Integer = 2;
  CurrentTool: TTool;
  isDrawing: Boolean = False;
  PolylineTool, RectangleTool, EllipseTool, LineTool: TTool;
  VectorEditor: TVectorEditor;

implementation

{$R *.lfm}

{ TVectorEditor }

procedure RegisterTool(Tool: TTool);
begin
  SetLength(Tools, Length(Tools) + 1);
  Tools[High(Tools)] := Tool;
end;

procedure SaveFigure(Figure: TFigure);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := Figure;
end;

procedure ClearFigures;
var i:Integer;
begin
  for i := 0 to High(Figures) do
    Figures[i].Destroy;
  Figures := nil;
end;

procedure SetCurrentTool(Tool: TTool);
begin
  CurrentTool := Tool;
end;

procedure TVectorEditor.ExitMenuItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TVectorEditor.FormCreate(Sender: TObject);
var i: Integer;
begin
  //PenColorBtn.ButtonColor := PenColor;
  //BrushColorBtn.ButtonColor := BrushColor;
  LineWidthSpinEdit.Value := LineWidth;

  for i := 0 to High(Tools) do
  begin

  end;
end;

procedure TVectorEditor.LineBtnClick(Sender: TObject);
begin
  SetCurrentTool(LineTool);
end;

procedure TVectorEditor.LineWidthSpinEditChange(Sender: TObject);
begin
  LineWidth := (Sender as TSpinEdit).Value;
end;

procedure TVectorEditor.PaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  isDrawing := True;
  CurrentTool.MouseDown(X, Y, PenColor, BrushColor, LineWidth);
end;

procedure TVectorEditor.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if isDrawing then
  begin
    PaintBox.Refresh;
    CurrentTool.MouseMove(X, Y);
    CurrentTool.GetFigure.Draw(PaintBox.Canvas);
  end;
end;

procedure TVectorEditor.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  isDrawing := False;
  SaveFigure(CurrentTool.GetFigure);
  CurrentTool.MouseUp(X, Y);
end;

procedure TVectorEditor.PaintBoxPaint(Sender: TObject);
var i:integer;
begin
  for i := 0 to High(Figures) do
  begin
      Figures[i].Draw(PaintBox.Canvas);
  end;
end;

procedure TVectorEditor.PenColorBtnColorChanged(Sender: TObject);
begin
  PenColor := (Sender as TColorButton).ButtonColor;
end;

procedure TVectorEditor.PolylineBtnClick(Sender: TObject);
begin
  SetCurrentTool(PolylineTool);
end;

procedure TVectorEditor.RectangleBtnClick(Sender: TObject);
begin
  SetCurrentTool(RectangleTool);
end;

procedure TVectorEditor.AboutMenuItemClick(Sender: TObject);
begin
  aboutprogram.aboutProgramForm.Show;
end;

procedure TVectorEditor.BrushColorBtnColorChanged(Sender: TObject);
begin
  BrushColor := (Sender as TColorButton).ButtonColor;
end;

procedure TVectorEditor.ClearMenuItemClick(Sender: TObject);
begin
  ClearFigures;
  PaintBox.Canvas.Clear;
  PaintBox.Repaint;
end;

procedure TVectorEditor.EllipseBtnClick(Sender: TObject);
begin
  SetCurrentTool(EllipseTool);
end;

initialization
PolylineTool := TPolylineTool.Create;
RectangleTool := TRectangleTool.Create;
EllipseTool := TEllipseTool.Create;
LineTool := TLineTool.Create;



RegisterTool(TPolylineTool.Create);
RegisterTool(TRectangleTool.Create);
RegisterTool(TEllipseTool.Create);
RegisterTool(TLineTool.Create);

end.
                                                             g
