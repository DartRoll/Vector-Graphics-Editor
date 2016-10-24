unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, Graphics;

type
  TTool = class
    function GetFigure: TFigure; virtual; abstract;
    procedure SetPenColor(AColor: TColor); virtual; abstract;
    procedure SetBrushColor(AColor: TColor); virtual; abstract;
    procedure SetThickness(Width: Integer); virtual; abstract;
    procedure MouseDown(X, Y: Integer); virtual; abstract;
    procedure MouseMove(X, Y: Integer); virtual; abstract;
    procedure MouseUp(X, Y: Integer); virtual; abstract;
  end;

  TPointerTool = class(TTool)
  end;

  TDrawinTool = class(TTool)//был TTool
    Figure: TFigure;
    function GetFigure: TFigure; override;
    procedure SetPenColor(AColor: TColor); override;
    procedure SetBrushColor(AColor: TColor); override;
    procedure SetThickness(Width: Integer); override;
    {procedure MouseDown(X, Y: Integer); virtual; abstract;
    procedure MouseMove(X, Y: Integer); virtual; abstract;}
    procedure MouseUp(X, Y: Integer); override;
  end;

  TRectangleTool = class(TDrawinTool)
    procedure MouseDown(X, Y: Integer); override;
    procedure MouseMove(X, Y: Integer); override;
  end;

  TPolylineTool = class(TDrawinTool)
    procedure MouseDown(X, Y: Integer); override;
    procedure MouseMove(X, Y: Integer); override;
  end;

  TLineTool = class(TDrawinTool)
    procedure MouseDown(X, Y: Integer); override;
    procedure MouseMove(X, Y: Integer); override;
  end;

  TEllipseTool = class(TDrawinTool)
    procedure MouseDown(X, Y: Integer); override;
    procedure MouseMove(X, Y: Integer); override;
  end;

implementation

function TDrawinTool.GetFigure: TFigure;
begin
  Result := Figure;
end;

procedure TDrawinTool.SetPenColor(AColor: TColor);
begin
  Figure.PenColor := AColor;
end;

procedure TDrawinTool.SetBrushColor(AColor: TColor);
begin
  Figure.BrushColor := AColor;
end;

procedure TDrawinTool.SetThickness(Width: Integer);
begin
  Figure.Thickness := Width;
end;

{ TDrawinTool }

procedure TDrawinTool.MouseUp(X, Y: Integer);
begin
end;

{ TPolylineTool }

procedure TPolylineTool.MouseDown(X, Y: Integer);
begin
  Figure := TPolyline.Create;
  (Figure as TPolyline).AddPoint(X, Y);
end;

procedure TPolylineTool.MouseMove(X, Y: Integer);
begin
  (Figure as TPolyline).AddPoint(X, Y);
end;

{ TRectangleTool }

procedure TRectangleTool.MouseDown(X, Y: Integer);
begin
  Figure := TRectangle.Create;
  (Figure as TRectangle).SetFirstPoint(X,Y);
end;

procedure TRectangleTool.MouseMove(X, Y: Integer);
begin
  (Figure as TRectangle).SetSecondPoint(X, Y);
end;

{ TEllipseTool }

procedure TEllipseTool.MouseDown(X, Y: Integer);
begin
  Figure := TEllipse.Create;
  (Figure as TEllipse).SetFirstPoint(X,Y);
end;

procedure TEllipseTool.MouseMove(X, Y: Integer);
begin
  (Figure as TEllipse).SetSecondPoint(X, Y);
end;

{ TLineTool }

procedure TLineTool.MouseDown(X, Y: Integer);
begin
  Figure := TLine.Create;
  (Figure as TLine).SetFirstPoint(X, Y);
end;

procedure TLineTool.MouseMove(X, Y: Integer);
begin
  (Figure as TLine).SetSecondPoint(X, Y);
end;

end.

