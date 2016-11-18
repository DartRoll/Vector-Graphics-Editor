unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Contnrs, SysUtils, FileUtil, Forms, Controls, Graphics, Math,
  Dialogs, Menus, ExtCtrls, StdCtrls, aboutprogram, LCLType, Spin, ComCtrls,
  Buttons, ActnList, Grids, UFigures, UTools, UTransform, Types;

type

  { TVectorEditor }
  TVectorEditor = class(TForm)
    ColorDialog: TColorDialog;
    ImageBoundsLabel: TLabel;
    ImageBoundsX: TLabel;
    ImageBoundsY: TLabel;
    MouseWrldLabel: TLabel;
    MouseXWrldLabel: TLabel;
    MouseYWrldLabel: TLabel;
    MouseYDspLabel: TLabel;
    MouseXDspLabel: TLabel;
    MouseDspLabel: TLabel;
    OffsetLabel: TLabel;
    OffsetYLabel: TLabel;
    OffsetXLabel: TLabel;
    Label10: TLabel;
    DebugPanel: TPanel;
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
    procedure RedefineImageBounds(ADoubleRect: TDoubleRect);
    procedure UpdateScale;
    procedure VerticalScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure UpdateDimensions;
  private
    { private declarations }

  public
    { public declarations }

  end;

var
  VectorEditor: TVectorEditor;

implementation

{$R *.lfm}

var
  PenColor: TColor = clBlack;
  BrushColor: TColor = clBlue;
  LineWidth: Integer = 2;
  isDrawing: Boolean = False;
  ImageBounds: TDoubleRect;
  Figures: array of TFigure;
  PaletteColors: array of array of TColor;
  ScrollOffset: TDoublePoint;
  CurrentTool: TTool;
  ChangeBars: Boolean = True;

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
    RedefineImageBounds(Figure.GetBounds);
  end;
end;

procedure TVectorEditor.RedefineImageBounds(ADoubleRect: TDoubleRect);
begin
  if Length(Figures) = 1 then begin
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
  RedefineImageBounds(DoubleRect(0, 0, 0, 0));
  SetCanvasOffset(0, 0);
  SetScale(1);
  PaintBox.Invalidate;
end;

procedure TVectorEditor.SetScrollBarsPostions;
var
  ImgWidthInPixels, ImgHeightInPixels: Integer;
begin
  {ImgWidthInPixels := WorldToDispDimension(ImageBounds.Right - ImageBounds.Left);
  ImgHeightInPixels := WorldToDispDimension(ImageBounds.Bottom - ImageBounds.Top);

  HorizontalScrollBar.Max := ImgWidthInPixels;
  HorizontalScrollBar.PageSize := PaintBox.ClientWidth;
  ScrollOffset.X := ImageBounds.Left;

  VerticalScrollBar.Max := ImgHeightInPixels;
  VerticalScrollBar.PageSize := PaintBox.ClientHeight;
  ScrollOffset.Y := ImageBounds.Top;

  HorizontalScrollBar.Position := WorldToDispDimension(GetCanvasOffset.X - ScrollOffset.X);
  VerticalScrollBar.Position := WorldToDispDimension(GetCanvasOffset.Y - ScrollOffset.Y);}
end;

procedure TVectorEditor.VerticalScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
{  with Sender as TScrollBar do begin
    if Position > Max - PageSize then begin
      Position := Max - PageSize ;
      Exit;
    end;
  end;
  with Sender as TScrollBar do
    SetCanvasOffset(GetCanvasOffset.X,
      WorldToDispDimension(ScrollOffset.Y) + Position);
  ChangeBars := False;
  PaintBox.Invalidate;}
end;

procedure TVectorEditor.UpdateDimensions;
begin
  DispDimensions := Dimensions(
    PaintBox.ClientWidth - 1,
    PaintBox.ClientHeight - 1);
end;

procedure TVectorEditor.HorizontalScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  {with Sender as TScrollBar do begin
    if Position > Max - PageSize then begin
      Position := Max - PageSize;
      Exit;
    end;
  end;
  with Sender as TScrollBar do
    SetCanvasOffset(WorldToDispDimension(ScrollOffset.X) + Position,
      GetCanvasOffset.Y);
  ChangeBars := False;
  PaintBox.Invalidate;}
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
  UpdateDimensions;
end;

procedure TVectorEditor.LineWidthSpinEditChange(Sender: TObject);
begin
  LineWidth := (Sender as TSpinEdit).Value;
end;

procedure TVectorEditor.PaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  isDrawing := True;
  CurrentTool.MouseDown(Point(X, Y), PenColor, BrushColor, LineWidth, Button);
end;

procedure TVectorEditor.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if isDrawing then begin
    CurrentTool.MouseMove(Point(X, Y));
    PaintBox.Invalidate;
  end;
  {DEBUG}
  MouseXDspLabel.Caption := 'x: ' + FloatToStr(X);
  MouseYDspLabel.Caption := 'y: ' + FloatToStr(Y);

  MouseXWrldLabel.Caption := 'x: ' + FloatToStr(DispToWorldX(X));
  MouseYWrldLabel.Caption := 'y: ' + FloatToStr(DispToWorldY(Y));
end;

procedure TVectorEditor.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  isDrawing := False;
  CurrentTool.MouseUp(Point(X, Y));
  SaveFigure(CurrentTool.GetFigure);
  PaintBox.Invalidate;
end;

procedure TVectorEditor.PaintBoxMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  StartingMouseCrds: TDoublePoint;
begin
  StartingMouseCrds := DispToWorldCoord(MousePos);
  DecreaseScale;
  AddCanvasOffset((StartingMouseCrds - DispToWorldCoord(MousePos)) * Scale);
  PaintBox.Invalidate;
end;

procedure TVectorEditor.PaintBoxMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  StartingMouseCrds: TDoublePoint;
begin
  StartingMouseCrds := DispToWorldCoord(MousePos);
  IncreaseScale;
  AddCanvasOffset((StartingMouseCrds - DispToWorldCoord(MousePos)) * Scale);
  PaintBox.Invalidate;
end;

procedure TVectorEditor.PaintBoxPaint(Sender: TObject);
var i:integer;
begin
  for i := 0 to High(Figures) do begin
    Figures[i].Draw(PaintBox.Canvas);
  end;
  if isDrawing and (CurrentTool.GetFigure <> nil) then begin
    CurrentTool.GetFigure.Draw(PaintBox.Canvas);
  end;
  {if ChangeBars then begin
    SetScrollBarsPostions;
  end
  else ChangeBars := True;}
  { TODO : Можно объеденить в одну процедуру }
  UpdateScale;
  UpdateDimensions;
  {DEBUG}
  OffsetXLabel.Caption := 'x: ' + FloatToStr(GetCanvasOffset.X);
  OffsetYLabel.Caption := 'y: ' + FloatToStr(GetCanvasOffset.Y);
  ImageBoundsX.Caption := 'left: ' + FloatToStr(ImageBounds.Left);
  ImageBoundsY.Caption := 'top: ' + FloatToStr(ImageBounds.Top);
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
  SetScale((Sender as TFloatSpinEdit).Value / 100);
  PaintBox.Invalidate;
end;

procedure TVectorEditor.ShowEverythingMenuItemClick(Sender: TObject);
const
  BorderMargin = 5;//px
var
  XScale, YScale: Double;
  ImgWorldWidth, ImgWorldHeight: Double;
begin
  ImgWorldWidth := ImageBounds.Right - ImageBounds.Left;
  ImgWorldHeight := ImageBounds.Bottom - ImageBounds.Top;
  //TODO: Упростить расчёты
  XScale := DispDimensions.Width / (ImgWorldWidth + 2 * BorderMargin / Scale);
  YScale := DispDimensions.Height / (ImgWorldHeight + 2 * BorderMargin / Scale);
  SetScale(Min(XScale, YScale));
  { ВНИМАНИЕ! ВПЕРЕДИ МАГИЯ! РУКАМИ НЕ ТРОГАТЬ! }
  //TODO: понять как я это сделал
  SetCanvasOffset(
    WorldToDispDimension(ImageBounds.Left) -
      (DispDimensions.Width - WorldToDispDimension(ImgWorldWidth)) / 2,
    WorldToDispDimension(ImageBounds.Top) -
      (DispDimensions.Height - WorldToDispDimension(ImgWorldHeight)) / 2);
  {SetCanvasOffset(
    ImageBounds.Left * Scale,
    ImageBounds.Top * Scale);}
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
