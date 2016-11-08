unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Contnrs, SysUtils, FileUtil, Forms, Controls, Graphics, Math,
  Dialogs, Menus, ExtCtrls, StdCtrls, aboutprogram, LCLType, Spin, ComCtrls,
  Buttons, ActnList, Grids, UFigures, UTools, UScale, Types;

type

  { TVectorEditor }
  TVectorEditor = class(TForm)
    ColorDialog: TColorDialog;
    Label10: TLabel;
    ScaleLabel: TLabel;
    ShowEverythingMenuItem: TMenuItem;
    ScaleFloatSpinEdit: TFloatSpinEdit;
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
    procedure ClearMenuItemClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HorizontalScrollBarChange(Sender: TObject);
    procedure HorizontalScrollBarScroll(Sender: TObject;
      ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure LineWidthSpinEditChange(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure PaintBoxMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PaletteGridDblClick(Sender: TObject);
    procedure PaletteGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure PaletteGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScaleFloatSpinEditChange(Sender: TObject);
    procedure ShowEverythingMenuItemClick(Sender: TObject);
    procedure ToolClick(Sender: TObject);
    procedure CreateToolsButtons(ABtnWidth, ABtnHeight, AColsCount: Integer);
    procedure FillPalette;
    procedure ClearCanvas;
    procedure SetScrollBarsPostions;
    procedure SaveFigure(Figure: TFigure);
    procedure ToolPanelClick(Sender: TObject);
    procedure RedefineImageBounds(ADoubleRect: TDoubleRect);
    procedure UpdateScale;
    procedure VerticalScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
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
  ScrollOffset: TDoublePoint;
  CurrentTool: TTool;
  ChangeScrBar: Boolean = False;
  VectorEditor: TVectorEditor;

implementation

{$R *.lfm}

{ TVectorEditor }

procedure TVectorEditor.UpdateScale;
begin
  ScaleFloatSpinEdit.Value := GetScale * 100;
end;

procedure TVectorEditor.SaveFigure(Figure: TFigure);
begin
  if Figure <> nil then begin
    SetLength(Figures, Length(Figures) + 1);
    Figures[High(Figures)] := Figure;
    RedefineImageBounds(CurrentTool.GetFigure.GetBounds);
  end;
end;

procedure TVectorEditor.ToolPanelClick(Sender: TObject);
begin

end;

procedure TVectorEditor.RedefineImageBounds(ADoubleRect: TDoubleRect);
begin
  if Length(Figures) < 1 then begin
    ImageBounds.Left := ADoubleRect.Left;
    ImageBounds.Top := ADoubleRect.Top;
    ImageBounds.Right := ADoubleRect.Right;
    ImageBounds.Bottom := ADoubleRect.Bottom;
  end
  else begin
    ImageBounds.Left := Min(ImageBounds.left, ADoubleRect.Left);
    ImageBounds.Top := Min(ImageBounds.Top, ADoubleRect.Top);
    ImageBounds.Right := Max(ImageBounds.Right, ADoubleRect.Right);
    ImageBounds.Bottom := Max(ImageBounds.Bottom, ADoubleRect.Bottom);
  end;
end;

procedure TVectorEditor.ClearCanvas;
var i: Integer;
begin
  for i := 0 to High(Figures) do
    Figures[i].Free;
  Figures := nil;
  PaintBox.Invalidate;
end;

procedure TVectorEditor.SetScrollBarsPostions;
var
  WidthInPixels, HeightInPixels: Integer;
begin
  //WidthInPixels := ceil((ImageBounds.Right - ImageBounds.Left) * GetScale);
  //HeightInPixels := ceil((ImageBounds.Bottom - ImageBounds.Top) * GetScale);
  WidthInPixels := WorldToDispDimension(ImageBounds.Right - ImageBounds.Left);
  HeightInPixels := WorldToDispDimension(ImageBounds.Bottom - ImageBounds.Top);
  HorizontalScrollBar.Max := WidthInPixels;
  HorizontalScrollBar.PageSize := PaintBox.ClientWidth;
  ScrollOffset.X := ImageBounds.Left;
  VerticalScrollBar.Max := HeightInPixels;
  VerticalScrollBar.PageSize := PaintBox.ClientHeight;
  ScrollOffset.Y := ImageBounds.Top;
  //Бесконечный вызов
  //if HorizontalScrollBar.Position <> WorldToDispDimension(GetCanvasOffset.X) - WorldToDispDimension(ScrollOffset.X) then
  HorizontalScrollBar.Position :=  WorldToDispDimension(GetCanvasOffset.X) - WorldToDispDimension(ScrollOffset.X);
  VerticalScrollBar.Position :=  WorldToDispDimension(GetCanvasOffset.Y) - WorldToDispDimension(ScrollOffset.Y);
end;

procedure TVectorEditor.HorizontalScrollBarChange(Sender: TObject);
begin
end;

procedure TVectorEditor.VerticalScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  with Sender as TScrollBar do begin
    if Position > Max - PageSize then begin
      Position := Max - PageSize ;
      Exit;
    end;
  end;
  SetCanvasOffset(GetCanvasOffset.X,
    ScrollOffset.Y * GetScale + (Sender as TScrollBar).Position);
  PaintBox.Invalidate;
end;

procedure TVectorEditor.HorizontalScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  with Sender as TScrollBar do begin
    if Position > Max - PageSize then begin
      Position := Max - PageSize;
      Exit;
    end;
  end;
  with Sender as TScrollBar do
    SetCanvasOffset(WorldToDispDimension(ScrollOffset.X) + Position,
      GetCanvasOffset.Y);
  PaintBox.Invalidate;
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
      PaletteColors[col, row] := RGBToColor(index * rate, row * 28,
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
  ScaleFloatSpinEdit.Value := 100;
  //Параметры кнопок  интрументов
  BtnWidth := 48;
  BtnHeight := 48;
  ColsCount := 2;
  CreateToolsButtons(BtnWidth, BtnHeight, ColsCount);
  //Палитра
  FillPalette;
  //Размеры паэнтибокса
  //RedefineImageBounds(DoubleRect(DispToWorldCoord(DoubleRect(0, 0, PaintBox.ClientWidth, PaintBox.ClientHeight))));
end;

procedure TVectorEditor.LineWidthSpinEditChange(Sender: TObject);
begin
  LineWidth := (Sender as TSpinEdit).Value;
end;

procedure TVectorEditor.PaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  isDrawing := True;
  //----Насколько плохо передавать много аргументов?
  CurrentTool.MouseDown(DispToWorldCoord(X, Y), PenColor, BrushColor, LineWidth,
    Button, PaintBox.BoundsRect);
end;

procedure TVectorEditor.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if isDrawing then begin
    CurrentTool.MouseMove(DispToWorldCoord(X, Y));
    RedefineImageBounds(DispToWorldCoord(Rect(0, 0,
      PaintBox.ClientWidth, PaintBox.ClientHeight)));
    PaintBox.Invalidate;
  end;
end;

procedure TVectorEditor.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  isDrawing := False;
  CurrentTool.MouseUp;
  SaveFigure(CurrentTool.GetFigure);
  UpdateScale;
  PaintBox.Invalidate;
end;

procedure TVectorEditor.PaintBoxMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  StartingMouseCrds: TDoublePoint;
begin
  StartingMouseCrds := DispToWorldCoord(MousePos.x, MousePos.y);
  DecreaseScale;
  AddCanvasOffset(StartingMouseCrds.X - DispToWorldX(MousePos.x),
      StartingMouseCrds.Y - DispToWorldY(MousePos.Y));
  PaintBox.Invalidate;
end;

procedure TVectorEditor.PaintBoxMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  StartingMouseCrds: TDoublePoint;
begin
  StartingMouseCrds := DispToWorldCoord(MousePos.x, MousePos.y);
  IncreaseScale;
  AddCanvasOffset(StartingMouseCrds.X - DispToWorldX(MousePos.x),
      StartingMouseCrds.Y - DispToWorldY(MousePos.Y));
  PaintBox.Invalidate;
end;

procedure TVectorEditor.PaintBoxPaint(Sender: TObject);
var i:integer;
begin
  for i := 0 to High(Figures) do begin
    Figures[i].Draw(PaintBox.Canvas);
  end;
  if isDrawing and (CurrentTool.GetFigure <> nil) then begin//----и так сойдёт?
    CurrentTool.GetFigure.Draw(PaintBox.Canvas);
  end;
  SetScrollBarsPostions;
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
var
  StartingCenterCrds: TDoublePoint;
begin
  StartingCenterCrds := DispToWorldCoord(round(PaintBox.ClientWidth/2),
    round(PaintBox.ClientHeight / 2));
  SetScalePercent((Sender as TFloatSpinEdit).Value);
  AddCanvasOffset(StartingCenterCrds.X - DispToWorldX(round(PaintBox.ClientWidth / 2)),
    StartingCenterCrds.Y - DispToWorldY(round(PaintBox.ClientHeight / 2)));
  PaintBox.Invalidate;
end;

procedure TVectorEditor.ShowEverythingMenuItemClick(Sender: TObject);
const
  BorderMargin = 5;//px
var
  XScale, YScale: Double;
  ImageWorldWidth, WorldHeight: Double;
begin
  ImageWorldWidth := ImageBounds.Right - ImageBounds.Left;
  WorldHeight := ImageBounds.Bottom - ImageBounds.Top;
  //TODO: сделать постоянный отступ вне зависимости от масшатаба - СДЕЛАНО!
  //TODO: Упростить расчёты
  XScale := (Paintbox.ClientWidth - 1)  /
    (ImageWorldWidth + DispToWorldDimension(2 * BorderMargin));
  YScale := (PaintBox.ClientHeight - 1) /
    (WorldHeight + DispToWorldDimension(2 * BorderMargin));
  SetScale(Min(XScale, YScale));
  { ВНИМАНИЕ! ВПЕРЕДИ МАГИЯ! РУКАМИ НЕ ТРОГАТЬ! }
  //TODO: понять как я это сделал
  SetCanvasOffset(WorldToDispDimension(ImageBounds.Left) -
      (PaintBox.ClientWidth - WorldToDispDimension(ImageWorldWidth)) / 2,
    WorldToDispDimension(ImageBounds.Top) -
      (PaintBox.ClientHeight - WorldToDispDimension(WorldHeight)) / 2);
  UpdateScale;
  PaintBox.Invalidate;
end;

procedure TVectorEditor.AboutMenuItemClick(Sender: TObject);
begin
  aboutprogram.aboutProgramForm.Show;
end;

procedure TVectorEditor.ClearMenuItemClick(Sender: TObject);
begin
  ClearCanvas;
end;

end.
