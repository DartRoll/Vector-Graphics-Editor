unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Contnrs, SysUtils, FileUtil, Forms, Controls, Graphics, Math,
  Dialogs, Menus, ExtCtrls, StdCtrls, aboutprogram, LCLType, Spin, ComCtrls,
  Buttons, ActnList, Grids, UFigures, UTools, UScale;

type

  { TVectorEditor }

  TVectorEditor = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ColorDialog: TColorDialog;
    ScaleFloatSpinEdit: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
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
    HorizontalScrollBar: TScrollBar;
    VerticalScrollBar: TScrollBar;
    ToolPanel: TPanel;
    LineWidthSpinEdit: TSpinEdit;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
    procedure ScaleFloatSpinEditChange(Sender: TObject);
    procedure ScaleSpinEditChange(Sender: TObject);
    procedure ToolClick(Sender: TObject);
    procedure CreateToolsButtons(ABtnWidth, ABtnHeight, AColsCount: Integer);
    procedure FillPalette;
    procedure ClearCanvas;
  private
    { private declarations }

  public
    { public declarations }

  end;

var
  PenColor: TColor = clBlack;
  BrushColor: TColor = clWhite;
  LineWidth: Integer = 2;
  isDrawing: Boolean = False;
  ImageBounds: TDoubleRect;
  Figures: array of TFigure;
  PaletteColors: array of array of TColor;
  CurrentTool: TTool;
  VectorEditor: TVectorEditor;

implementation

{$R *.lfm}

{ TVectorEditor }


procedure SaveFigure(Figure: TFigure);
begin
  if Figure <> nil then begin
    SetLength(Figures, Length(Figures) + 1);
    Figures[High(Figures)] := Figure;
  end;
end;

procedure RedefineImageBounds(TDoubleRect)
begin

end;

procedure TVectorEditor.ClearCanvas;
var i: Integer;
begin
  for i := 0 to High(Figures) do
    Figures[i].free;
  Figures := nil;
  PaintBox.Canvas.Clear;
end;

procedure TVectorEditor.ToolClick(Sender: TObject);
begin
  CurrentTool := UTools.Tools[(Sender as TSpeedButton).Tag];
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
    with ToolIcon do begin
      TransparentColor := clWhite;
      Transparent := True;
      LoadFromFile(Tools[i].FIcon);
    end;
    with ToolBtn do begin
      Glyph := ToolIcon;
      Flat := True;
      Width := ABtnWidth;
      Height := ABtnHeight;
      Top := (i div AColsCount) * ABtnHeight;
      Left := (i mod AColsCount) * ABtnWidth;
      Tag := i;
      GroupIndex := 1;
      OnClick := @ToolClick;
      if i = 0 then Down := True;
      Parent := ToolPanel;
    end;
  end;
end;

procedure TVectorEditor.FillPalette;
var
  col, row, rate, index: Integer;
begin
  index := 0;
  rate := floor(255 / (PaletteGrid.RowCount * PaletteGrid.ColCount));
  for col := 0 to PaletteGrid.ColCount do begin
    SetLength(PaletteColors, Length(PaletteColors) + 1);
    for row := 0  to PaletteGrid.RowCount do begin
      SetLength(PaletteColors[col], Length(PaletteColors[col]) + 1);
      PaletteColors[col, row] := RGBToColor(index * rate, row * 28 ,
        (PaletteGrid.ColCount - col) * 42);
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
  CurrentTool := Tools[0];
  //Передаём дефолтный параметры представлению
  PenColorPanel.Color := PenColor;
  BrushColorPanel.Color := BrushColor;
  LineWidthSpinEdit.Value := LineWidth;
  //Параметры кнопок  интрументов
  BtnWidth := 48;
  BtnHeight := 48;
  ColsCount := 2;
  CreateToolsButtons(BtnWidth, BtnHeight, ColsCount);
  //Палитра
  FillPalette;
end;

procedure TVectorEditor.LineWidthSpinEditChange(Sender: TObject);
begin
  LineWidth := (Sender as TSpinEdit).Value;
end;

procedure TVectorEditor.PaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  isDrawing := True;
  CurrentTool.MouseDown(DispToWorldCoord(X, Y), PenColor, BrushColor, LineWidth);
end;

procedure TVectorEditor.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  Label1.Caption := 'x:' + FloatToStr(GetCanvasOffset.X);
  Label2.Caption := 'y:' + FloatToStr(GetCanvasOffset.Y);
  if isDrawing then begin
    CurrentTool.MouseMove(DispToWorldCoord(X, Y));
    PaintBox.Invalidate;
  end;
end;

procedure TVectorEditor.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  isDrawing := False;
  SaveFigure(CurrentTool.GetFigure);
    if CurrentTool.GetFigure <> nil then begin //сделать нормально
    CurrentTool.GetFigure.GetBounds;
  end;
end;

procedure TVectorEditor.PaintBoxPaint(Sender: TObject);
var i:integer;
begin
  for i := 0 to High(Figures) do begin
    Figures[i].Draw(PaintBox.Canvas);
  end;
  if isDrawing and (CurrentTool.GetFigure <> nil) then //сделать нормально !!!
    CurrentTool.GetFigure.Draw(PaintBox.Canvas);

end;

procedure TVectorEditor.PaletteGridDblClick(Sender: TObject);
begin
  if ColorDialog.Execute then begin
    with Sender as TDrawGrid do begin
      PaletteColors[Col, Row] := ColorDialog.Color;
    end;
    PaletteGrid.Invalidate;
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
  ACol, ARow: Integer;
begin
  PaletteGrid.MouseToCell(X, Y, ACol, ARow);
  if Button = mbLeft then  begin
    PenColor := PaletteColors[ACol, ARow];
    PenColorPanel.Color := PenColor;
  end;
  if Button = mbRight then begin
    BrushColor := PaletteColors[ACol, ARow];
    BrushColorPanel.Color := BrushColor;
  end;
end;

procedure TVectorEditor.ScaleFloatSpinEditChange(Sender: TObject);
begin
  SetScale((Sender as TFloatSpinEdit).Value);
  PaintBox.Invalidate;
end;

procedure TVectorEditor.ScaleSpinEditChange(Sender: TObject);
begin
  SetScale((Sender as TSpinEdit).Value);
  PaintBox.Invalidate;
end;

procedure TVectorEditor.AboutMenuItemClick(Sender: TObject);
begin
  aboutprogram.aboutProgramForm.Show;
end;

procedure TVectorEditor.Button1Click(Sender: TObject);
begin
  CurrentTool.MouseDown(DispToWorldCoord(10, 10), PenColor, BrushColor, LineWidth);

end;

procedure TVectorEditor.Button2Click(Sender: TObject);
begin
  CurrentTool.MouseMove(DispToWorldCoord(20, 20));
  CurrentTool.MouseMove(DispToWorldCoord(30, 30));
  PaintBox.Invalidate;
end;

procedure TVectorEditor.ClearMenuItemClick(Sender: TObject);
begin
  ClearCanvas;
  PaintBox.Invalidate;
end;

initialization

ImageBounds.Right;

end.

