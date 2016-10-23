unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, Graphics;

type

  TTool = class
    Figure: TFigure;
    function GetFigure: TFigure;
    procedure SetPenColor(AColor: TColor);
    procedure SetBrushColor(AColor: TColor);
    procedure MouseDown(X, Y: Integer); virtual; abstract;
    procedure MouseMove(X, Y: Integer); virtual; abstract;
    procedure MouseUp(X, Y: Integer); virtual;
  end;

  TRectangleTool = class(TTool)
    procedure MouseDown(X, Y: Integer); override;
    procedure MouseMove(X, Y: Integer); override;
  end;

  TPolylineTool = class(TTool)
    procedure MouseDown(X, Y: Integer); override;
    procedure MouseMove(X, Y: Integer); override;
  end;

  TLineTool = class(TTool)
    procedure MouseDown(X, Y: Integer); override;
    procedure MouseMove(X, Y: Integer); override;
  end;

  TEllipseTool = class(TTool)
    procedure MouseDown(X, Y: Integer); override;
    procedure MouseMove(X, Y: Integer); override;
  end;

implementation

function TTool.GetFigure: TFigure;
begin
  Result := Figure;
end;

procedure TTool.SetPenColor(AColor: TColor);
begin
  Figure.PenColor := AColor;
end;

procedure TTool.SetBrushColor(AColor: TColor);
begin
  Figure.BrushColor := AColor;
end;

{ TTool }

procedure TTool.MouseUp(X, Y: Integer);
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

