unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  UScale, Classes, SysUtils, Graphics, math;

type

  TFigure = class
    PenColor: TColor;
    BrushColor: TColor;
    Thickness: Integer;
    constructor Create(APenColor, ABrushColor: TColor; AThickness: Integer);
    procedure Draw(Canvas: TCanvas);
    procedure DrawFigure(Canvas: TCanvas); virtual; abstract;
    function GetBounds: TDoubleRect; virtual; abstract;
  end;

  TPolyline = class(TFigure)
    Vertexes: array of TDoublePoint;
    constructor Create(ADoublePoint: TDoublePoint;
      APenColor, ABrushColor: TColor; AThickness: Integer);
    procedure AddPoint(ADoublePoint: TDoublePoint);
    procedure DrawFigure(Canvas: TCanvas); override;
    function GetBounds: TDoubleRect;
  end;

  TTwoPointFigure = class(TFigure)
    Bounds: TDoubleRect;
    constructor Create(ADoublePoint: TDoublePoint;
      APenColor, ABrushColor: TColor; AThickness: Integer);
    procedure SetSecondPoint(ADoublePoint: TDoublePoint);
    function GetBounds: TDoubleRect;
  end;

  TRectangle = class(TTwoPointFigure)
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  TLine = class(TTwoPointFigure)
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  TEllipse = class(TTwoPointFigure)
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

implementation

{ TFigure }
constructor TFigure.Create(APenColor, ABrushColor: TColor; AThickness: Integer);
begin
  PenColor := APenColor;
  BrushColor := ABrushColor;
  Thickness := AThickness;
end;

procedure TFigure.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color := PenColor;
  Canvas.Pen.Width := Thickness;
  Canvas.Brush.Color := BrushColor;
  DrawFigure(Canvas);
end;

{ TTwoPointFigure }
constructor TTwoPointFigure.Create(ADoublePoint: TDoublePoint;
  APenColor, ABrushColor: TColor; AThickness: Integer);
begin
  inherited Create(APenColor, ABrushColor, AThickness);
  Bounds := DoubleRect(ADoublePoint, ADoublePoint);
end;

procedure TTwoPointFigure.SetSecondPoint(ADoublePoint: TDoublePoint);
begin
  Bounds := DoubleRect(Bounds.TopLeft, ADoublePoint);
end;

{ TPolyline }
constructor TPolyline.Create(ADoublePoint: TDoublePoint;
  APenColor, ABrushColor: TColor; AThickness: Integer);
begin
  inherited Create(APenColor, ABrushColor, AThickness);
  AddPoint(ADoublePoint);
end;

procedure TPolyline.AddPoint(ADoublePoint: TDoublePoint);
begin
  SetLength(Vertexes, Length(Vertexes) + 1);
  Vertexes[High(Vertexes)] := ADoublePoint;
end;

procedure TPolyline.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Polyline(WorldVertexesToDispCoord(Vertexes));
end;

function TPolyline.GetBounds: TDoubleRect;
var
  i: Integer;
  LeftX, RightX, TopY, BottomY: Double;
begin
  with Vertexes[0] do begin
    LeftX := X;
    RightX := X;
    TopY := Y;
    BottomY := Y;
  end;
  for i := 1 to High(Vertexes) do begin
    with Vertexes[i] do begin
      LeftX := Min(LeftX, X);
      RightX := Max(RightX, X);
      BottomY := Min(BottomY, Y);
      TopY := Max(TopY, y);
    end;
  end;
  Result := DoubleRect(LeftX, TopY, RightX, BottomY);
end;

{ TRectangle }
procedure TRectangle.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Rectangle(WorldToDispCoord(Bounds));
end;

{ TEllipse }
procedure TEllipse.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Ellipse(WorldToDispCoord(Bounds));
end;

{ TLine }
procedure TLine.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Line(WorldToDispCoord(Bounds));
end;

end.

