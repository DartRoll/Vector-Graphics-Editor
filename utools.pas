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
    procedure MouseDown(AX, AY: Integer; APenColor, ABrushColor: TColor;
      ALineWidth: Integer); virtual; abstract;
    procedure MouseMove(AX, AY: Integer); virtual; abstract;
    procedure MouseUp(AX, AY: Integer);
  end;

  TRectangleTool = class(TTool)
    constructor Create;
    procedure MouseDown(AX, AY: Integer; APenColor, ABrushColor: TColor;
      ALineWidth: Integer); override;
    procedure MouseMove(X, Y: Integer); override;
  end;

  TPolylineTool = class(TTool)
    constructor Create;
    procedure MouseDown(AX, AY: Integer; APenColor, ABrushColor: TColor;
      ALineWidth: Integer); override;
    procedure MouseMove(X, Y: Integer); override;
  end;

  TLineTool = class(TTool)
    constructor Create;
    procedure MouseDown(AX, AY: Integer; APenColor, ABrushColor: TColor;
      ALineWidth: Integer); override;
    procedure MouseMove(X, Y: Integer); override;
  end;

  TEllipseTool = class(TTool)
    constructor Create;
    procedure MouseDown(AX, AY: Integer; APenColor, ABrushColor: TColor;
      ALineWidth: Integer); override;
    procedure MouseMove(X, Y: Integer); override;
  end;

implementation

{TODO: может вынести mousemove?}

{ TTool }
function TTool.GetFigure: TFigure;
begin
  Result := FFigure;
end;

procedure TTool.MouseUp(AX, AY: Integer);
begin
end;

{ TPolylineTool }
constructor TPolylineTool.Create;
begin
  FIcon := 'img/polyline.png';
end;

procedure TPolylineTool.MouseDown(AX, AY: Integer; APenColor, ABrushColor: TColor;
      ALineWidth: Integer);
begin
  FFigure := TPolyline.Create(AX, AY, APenColor, ABrushColor, ALineWidth);
end;

procedure TPolylineTool.MouseMove(X, Y: Integer);
begin
  (FFigure as TPolyline).AddPoint(X, Y);
end;

{ TRectangleTool }
constructor TRectangleTool.Create;
begin
  FIcon := 'img/rectangle.png';
end;

procedure TRectangleTool.MouseDown(AX, AY: Integer; APenColor, ABrushColor: TColor;
      ALineWidth: Integer);
begin
  FFigure := TRectangle.Create(AX, AY, APenColor,ABrushColor, ALineWidth);
end;

procedure TRectangleTool.MouseMove(X, Y: Integer);
begin
  (FFigure as TRectangle).SetSecondPoint(X, Y);
end;

{ TEllipseTool }
constructor TEllipseTool.Create;
begin
  FIcon := 'img/ellipse.png';
end;

procedure TEllipseTool.MouseDown(AX, AY: Integer; APenColor, ABrushColor: TColor;
      ALineWidth: Integer);
begin
  FFigure := TEllipse.Create(AX, AY, APenColor,ABrushColor, ALineWidth);
end;

procedure TEllipseTool.MouseMove(X, Y: Integer);
begin
  (FFigure as TEllipse).SetSecondPoint(X, Y);
end;

{ TLineTool }
constructor TLineTool.Create;
begin
  FIcon := 'img/line.png';
end;

procedure TLineTool.MouseDown(AX, AY: Integer; APenColor, ABrushColor: TColor;
      ALineWidth: Integer);
begin
  FFigure := TLine.Create(AX, AY, APenColor,ABrushColor, ALineWidth);
end;

procedure TLineTool.MouseMove(X, Y: Integer);
begin
  (FFigure as TLine).SetSecondPoint(X, Y);
end;

end.

