unit main;

{$mode objfpc}{$H+}{$R+}

interface

uses
  Classes, Contnrs, SysUtils, FileUtil, Forms, Controls, Graphics, Math,
  Dialogs, Menus, ExtCtrls, StdCtrls, aboutprogram, LCLType, Spin, ComCtrls,
  Buttons, ActnList, Grids, UFigures, UTools;

type

  { TVectorEditor }

  TVectorEditor = class(TForm)
    PaletteDrawGrid: TDrawGrid;
    LineWidthLabel: TLabel;
    MainMenu: TMainMenu;
    FileMenuItem: TMenuItem;
    HelpMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    FileDividerMenuItem: TMenuItem;
    ClearMenuItem: TMenuItem;
    PaintBox: TPaintBox;
    ToolPanel: TPanel;
    LineWidthSpinEdit: TSpinEdit;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure BrushColorBtnColorChanged(Sender: TObject);
    procedure ClearMenuItemClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LineWidthSpinEditChange(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PenColorBtnColorChanged(Sender: TObject);
    procedure ToolClick(Sender: TObject);
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
var i: Integer;
begin
  for i := 0 to High(Figures) do
    Figures[i].Destroy;
  Figures := nil;
end;

procedure TVectorEditor.ToolClick(Sender: TObject);
begin
  CurrentTool := Tools[(Sender as TSpeedButton).Tag];
end;

procedure TVectorEditor.ExitMenuItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TVectorEditor.FormCreate(Sender: TObject);
var
  i: Integer;
  ToolBtn: TSpeedButton;
  ToolIcon: TBitmap;
  btnWidth, btnHeight, cols: Integer;
begin
  {РАЗДЕЛИТЬ ВСЁ ЭТО ДЕЛО}
  //PenColorBtn.ButtonColor := PenColor;
  //BrushColorBtn.ButtonColor := BrushColor;
  LineWidthSpinEdit.Value := LineWidth;

  //Кнопки
  btnWidth := 64;
  btnHeight := 64;
  cols := 2;
  for i := 0 to High(Tools) do begin
    ToolBtn := TSpeedButton.Create(VectorEditor);
    ToolIcon := TBitmap.Create;
    with TPicture.create do
    begin
      try
        LoadFromFile(Tools[i].FIcon);
        ToolIcon.Assign(Graphic);
      finally
        Free;
      end;
    end;
    ToolBtn.Transparent := True;
    ToolBtn.Flat := True;
    ToolIcon.Transparent := true;
    ToolBtn.Glyph := ToolIcon;
    ToolBtn.Width := btnWidth;
    ToolBtn.Height := btnHeight;
    ToolBtn.Top := (i div cols) * btnHeight;
    ToolBtn.Left := (i mod cols) * btnWidth;
    ToolBtn.Tag := i;
    ToolBtn.GroupIndex := 1;
    if i = 0 then ToolBtn.Down := True;
    ToolBtn.OnClick := @ToolClick;
    ToolBtn.Parent := ToolPanel;

    //Палитра
    for i := 0 to PaletteDrawGrid.Columns.Items[];
   end;

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

initialization

RegisterTool(TPolylineTool.Create);
RegisterTool(TRectangleTool.Create);
RegisterTool(TEllipseTool.Create);
RegisterTool(TLineTool.Create);

CurrentTool := Tools[0];
end.
                                                             g
