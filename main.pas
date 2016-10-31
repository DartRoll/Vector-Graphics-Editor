unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Contnrs, SysUtils, FileUtil, Forms, Controls, Graphics, Math,
  Dialogs, Menus, ExtCtrls, StdCtrls, aboutprogram, LCLType, Spin, ComCtrls,
  Buttons, ActnList, Grids, UFigures, UTools;

type

  { TVectorEditor }

  TVectorEditor = class(TForm)
    ColorDialog: TColorDialog;
    PaletteGrid: TDrawGrid;
    LineWidthLabel: TLabel;
    MainMenu: TMainMenu;
    FileMenuItem: TMenuItem;
    HelpMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    FileDividerMenuItem: TMenuItem;
    ClearMenuItem: TMenuItem;
    PaintBox: TPaintBox;
    BrushColorPanel: TPanel;
    PenColorPanel: TPanel;
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
    procedure PaletteGridDblClick(Sender: TObject);
    procedure PaletteGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure PaletteGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PenColorBtnColorChanged(Sender: TObject);
    procedure ToolClick(Sender: TObject);
    procedure CreateToolsButtons(ABtnWidth, ABtnHeight, AColsCount: Integer);
    procedure CreatePalette;
  private
    { private declarations }

  public
    { public declarations }

  end;

var
  Tools: array of TTool;
  Figures: array of TFigure;
  PaletteColors: array of array of TColor;
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

procedure TVectorEditor.CreateToolsButtons(
  ABtnWidth, ABtnHeight, AColsCount: Integer);
var
  i: Integer;
  ToolBtn: TSpeedButton;
  ToolIcon: TBitmap;
begin
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

    ToolBtn.Transparent := True;//чому не робит?
    ToolBtn.Flat := True;
    ToolIcon.Transparent := true;
    ToolBtn.Glyph := ToolIcon;
    ToolBtn.Width := ABtnWidth;
    ToolBtn.Height := ABtnHeight;
    ToolBtn.Top := (i div AColsCount) * ABtnHeight;
    ToolBtn.Left := (i mod AColsCount) * ABtnWidth;
    ToolBtn.Tag := i;
    ToolBtn.GroupIndex := 1;
    if i = 0 then ToolBtn.Down := True;
    ToolBtn.OnClick := @ToolClick;
    ToolBtn.Parent := ToolPanel;
  end;
end;

procedure TVectorEditor.CreatePalette;
var
  col, row, rate, index: Integer;
begin
  index := 0;
  rate := floor(255 / (PaletteGrid.RowCount * PaletteGrid.ColCount));
  for col := 0 to PaletteGrid.ColCount do begin
    SetLength(PaletteColors, Length(PaletteColors) + 1);
    for row := 0  to PaletteGrid.RowCount do begin
      SetLength(PaletteColors[col], Length(PaletteColors[col]) + 1);
      PaletteColors[col, row] := RGBToColor(index * rate, row * 28 , col * 42); //сделать норм цвета
      index += 1;
    end;
  end;
end;

procedure TVectorEditor.ExitMenuItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TVectorEditor.FormCreate(Sender: TObject);
var
  BtnWidth, BtnHeight, ColsCount: Integer;
begin
  //Выставляем дефолтные параметры
  PenColorPanel.Color := PenColor;
  BrushColorPanel.Color := BrushColor;
  LineWidthSpinEdit.Value := LineWidth;

  //Параметры кнопок
  BtnWidth := 64;
  BtnHeight := 64;
  ColsCount := 2;
  CreateToolsButtons(BtnWidth, BtnHeight, ColsCount);

  //Палитра
   CreatePalette;
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

procedure TVectorEditor.PaletteGridDblClick(Sender: TObject);
begin
  if ColorDialog.Execute then begin
    with Sender as TDrawGrid do begin
      PaletteColors[Col, Row] := ColorDialog.Color;
    end;
  end;
end;

procedure TVectorEditor.PaletteGridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
begin
  PaletteGrid.Canvas.Brush.Color := PaletteColors[aCol, aRow];
  PaletteGrid.Canvas.FillRect(aRect);
end;

procedure TVectorEditor.PaletteGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  aCol, aRow: Integer;
begin
  PaletteGrid.MouseToCell(X, Y, aCol, aRow);
  //Левая кнопка
  if Button = TMouseButton.mbLeft then  begin
    PenColor := PaletteColors[aCol, aRow];
    PenColorPanel.Color := PenColor;
  end;
  //Правая кнопка
  if Button = TMouseButton.mbRight then begin
    BrushColor := PaletteColors[aCol, aRow];
    BrushColorPanel.Color := BrushColor;
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

