unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, Graphics;

type
  TTool = class
    FFigure: TFigure;
    FIcon: String;
    function GetFigure: TFigure;
    procedure MouseDown(X, Y: Double; APenColor, ABrushColor: TColor;
      ALineWidth: Integer); virtual; abstract;
    procedure MouseMove(X, Y: Double); virtual; abstract;
  end;

  TTwoPointFigureTool = class(TTool)
    procedure MouseMove(X, Y: Integer); override;
  end;

  TPolylineTool = class(TTool)
    constructor Create;
    procedure MouseDown(X, Y: Integer; APenColor, ABrushColor: TColor;
      ALineWidth: Integer); override;
    procedure MouseMove(X, Y: Integer); override;
  end;

  TRectangleTool = class(TTwoPointFigureTool)
    constructor Create;
    procedure MouseDown(X, Y: Integer; APenColor, ABrushColor: TColor;
      ALineWidth: Integer); override;
  end;

  TLineTool = class(TTwoPointFigureTool)
    constructor Create;
    procedure MouseDown(X, Y: Integer; APenColor, ABrushColor: TColor;
      ALineWidth: Integer); override;
  end;

  TEllipseTool = class(TTwoPointFigureTool)
    constructor Create;
    procedure MouseDown(X, Y: Integer; APenColor, ABrushColor: TColor;
      ALineWidth: Integer); override;
  end;

const
  WMaxX: Double = 1.7E308;
  WMaxY: Double = 1.7E308;

var
  Tools: array of TTool;
  PolylineTool, RectangleTool, EllipseTool, LineTool: TTool;

implementation

{Misc}
procedure RegisterTool(Tool: TTool);
begin
  SetLength(Tools, Length(Tools) + 1);
  Tools[High(Tools)] := Tool;
end;

{ TTool }
function TTool.GetFigure: TFigure;
begin
  Result := FFigure;
end;

{TTwoPointFigureTool}
procedure TTwoPointFigureTool.MouseMove(X, Y: Integer);
begin
  (FFigure as TTwoPointFigure).SetSecondPoint(X, Y);
end;

{ TPolylineTool }
constructor TPolylineTool.Create;
begin
  Inherited;
  FIcon := 'img/polyline.bmp';
end;

procedure TPolylineTool.MouseDown(X, Y: Integer; APenColor, ABrushColor: TColor;
      ALineWidth: Integer);
begin
  FFigure := TPolyline.Create(X, Y, APenColor, ABrushColor, ALineWidth);
end;

procedure TPolylineTool.MouseMove(X, Y: Integer);
begin
  (FFigure as TPolyline).AddPoint(X, Y);
end;

{ TRectangleTool }
constructor TRectangleTool.Create;
begin
  Inherited;
  FIcon := 'img/rectangle.bmp';
end;

procedure TRectangleTool.MouseDown(
  X, Y: Integer; APenColor, ABrushColor: TColor; ALineWidth: Integer);
begin
  FFigure := TRectangle.Create(X, Y, APenColor, ABrushColor, ALineWidth);
end;

{ TEllipseTool }
constructor TEllipseTool.Create;
begin
  Inherited;
  FIcon := 'img/ellipse.bmp';
end;

procedure TEllipseTool.MouseDown(
  X, Y: Integer; APenColor, ABrushColor: TColor; ALineWidth: Integer);
begin
  FFigure := TEllipse.Create(X, Y, APenColor, ABrushColor, ALineWidth);
end;

{ TLineTool }
constructor TLineTool.Create;
begin
  Inherited;
  FIcon := 'img/line.bmp';
end;

procedure TLineTool.MouseDown(
  X, Y: Integer; APenColor, ABrushColor: TColor; ALineWidth: Integer);
begin
  FFigure := TLine.Create(X, Y, APenColor,ABrushColor, ALineWidth);
end;

initialization

RegisterTool(TPolylineTool.Create);
RegisterTool(TRectangleTool.Create);
RegisterTool(TEllipseTool.Create);
RegisterTool(TLineTool.Create);

end.

