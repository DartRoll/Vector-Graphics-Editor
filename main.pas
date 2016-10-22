unit main;

{$mode objfpc}{$H+}{$R+}

interface

uses
  Classes,Contnrs, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, aboutprogram, LCLType, Spin, ComCtrls, Buttons, UFigures,
  UTools;

type

  { TVectorEditor }

  TVectorEditor = class(TForm)
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
    SpeedButton1: TSpeedButton;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure ClearMenuItemClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
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
  CurrentInstrument: TTool;
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

procedure TVectorEditor.ExitMenuItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TVectorEditor.PaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PaintingFlag := True;
  CurrentInstrument.MouseDown(X, Y);
end;

procedure TVectorEditor.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if PaintingFlag then
  begin
    PaintBox.Repaint;//TODO: найти способ поэффективнее
    CurrentInstrument.MouseMove(X, Y);
    CurrentInstrument.GetFigure.Draw(PaintBox.Canvas);
  end;

end;

procedure TVectorEditor.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  PaintingFlag := False;
  CurrentInstrument.MouseUp(X, Y);
  PushFigure(CurrentInstrument.GetFigure);
end;

procedure TVectorEditor.PaintBoxPaint(Sender: TObject);
var i:integer;
begin
  for i := 0 to High(Figures) do
    Figures[i].Draw(PaintBox.Canvas);
end;

procedure TVectorEditor.PolylineBtnClick(Sender: TObject);
begin
  CurrentInstrument := TPolylineTool.Create;
end;

procedure TVectorEditor.RectangleBtnClick(Sender: TObject);
begin
  CurrentInstrument := TRectangleTool.Create;
end;


procedure TVectorEditor.AboutMenuItemClick(Sender: TObject);
begin
  aboutprogram.aboutProgramForm.Show;
end;

procedure TVectorEditor.ClearMenuItemClick(Sender: TObject);
begin
  ShowMessage('Чистим')
end;

end.

