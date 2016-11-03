unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, Graphics, UScale;

type
  TTool = class
    FFigure: TFigure;
    FIcon: String;
    function GetFigure: TFigure;
    procedure MouseDown(ADoublePoint: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer); virtual; abstract;
    procedure MouseMove(ADoublePoint: TDoublePoint); virtual; abstract;
  end;

  { THandTool }

  THandTool = class(TTool)
    FFirstPoint: TDoublePoint;
    constructor Create;
    procedure MouseDown(ADoublePoint: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer); override;
    procedure MouseMove(ADoublePoint: TDoublePoint); override;
  end;

  TTwoPointFigureTool = class(TTool)
    procedure MouseMove(ADoublePoint: TDoublePoint); override;
  end;

  TPolylineTool = class(TTool)
    constructor Create;
    procedure MouseDown(ADoublePoint: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer); override;
    procedure MouseMove(ADoublePoint: TDoublePoint); override;
  end;

  TRectangleTool = class(TTwoPointFigureTool)
    constructor Create;
    procedure MouseDown(ADoublePoint: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer); override;
  end;

  TLineTool = class(TTwoPointFigureTool)
    constructor Create;
    procedure MouseDown(ADoublePoint: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer); override;
  end;

  TEllipseTool = class(TTwoPointFigureTool)
    constructor Create;
    procedure MouseDown(ADoublePoint: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer); override;
  end;

var
  Tools: array of TTool;

implementation

{Misc}
procedure RegisterTool(Tool: TTool);
begin
  SetLength(Tools, Length(Tools) + 1);
  Tools[High(Tools)] := Tool;
end;

{ THandTool }
constructor THandTool.Create;
begin
  Inherited;
  FIcon := 'img/hand.bmp';
end;

procedure THandTool.MouseDown(ADoublePoint: TDoublePoint; APenColor,
  ABrushColor: TColor; ALineWidth: Integer);
begin
  FFirstPoint := ADoublePoint;
end;

procedure THandTool.MouseMove(ADoublePoint: TDoublePoint);
begin
  //добавить Scale
  {TUT OCHIBKA}
  with CanvasOffset do begin;
    X := X - (ADoublePoint.x - FFirstPoint.X);
    Y := Y - (ADoublePoint.y - FFirstPoint.Y);

  end;
  FFirstPoint := ADoublePoint;
end;

{ TTool }
function TTool.GetFigure: TFigure;
begin
  Result := FFigure;
end;

{ TTwoPointFigureTool }
procedure TTwoPointFigureTool.MouseMove(ADoublePoint: TDoublePoint);
begin
  (FFigure as TTwoPointFigure).SetSecondPoint(ADoublePoint);
end;

{ TPolylineTool }
constructor TPolylineTool.Create;
begin
  Inherited;
  FIcon := 'img/polyline.bmp';
end;

procedure TPolylineTool.MouseDown(ADoublePoint: TDoublePoint;
  APenColor, ABrushColor: TColor; ALineWidth: Integer);
begin
  FFigure := TPolyline.Create(ADoublePoint, APenColor, ABrushColor, ALineWidth);
end;

procedure TPolylineTool.MouseMove(ADoublePoint: TDoublePoint);
begin
  (FFigure as TPolyline).AddPoint(ADoublePoint);
end;

{ TRectangleTool }
constructor TRectangleTool.Create;
begin
  Inherited;
  FIcon := 'img/rectangle.bmp';
end;

procedure TRectangleTool.MouseDown(ADoublePoint: TDoublePoint;
  APenColor, ABrushColor: TColor; ALineWidth: Integer);
begin
  FFigure := TRectangle.Create(ADoublePoint, APenColor, ABrushColor, ALineWidth);
end;

{ TEllipseTool }
constructor TEllipseTool.Create;
begin
  Inherited;
  FIcon := 'img/ellipse.bmp';
end;

procedure TEllipseTool.MouseDown(ADoublePoint: TDoublePoint;
  APenColor, ABrushColor: TColor; ALineWidth: Integer);
begin
  FFigure := TEllipse.Create(ADoublePoint, APenColor, ABrushColor, ALineWidth);
end;

{ TLineTool }
constructor TLineTool.Create;
begin
  Inherited;
  FIcon := 'img/line.bmp';
end;

procedure TLineTool.MouseDown(
  ADoublePoint: TDoublePoint; APenColor, ABrushColor: TColor; ALineWidth: Integer);
begin
  FFigure := TLine.Create(ADoublePoint, APenColor,ABrushColor, ALineWidth);
end;

initialization
RegisterTool(THandTool.Create);
RegisterTool(TPolylineTool.Create);
RegisterTool(TRectangleTool.Create);
RegisterTool(TEllipseTool.Create);
RegisterTool(TLineTool.Create);
end.

