unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type

  TFigure = class
    PenColor: TColor;
    BrushColor: TColor;
    Thickness: Integer;
    constructor Create(APenColor, ABrushColor: TColor; AThickness: Integer);
    procedure Draw(Canvas: TCanvas);
    procedure DrawFigure(Canvas: TCanvas); virtual; abstract;
  end;

  TPolyline = class(TFigure)
    Vertexes: array of TPoint;
    constructor Create(X, Y: Integer;
      APenColor, ABrushColor: TColor; AThickness: Integer);
    procedure AddPoint(X, Y: Integer);
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  TTwoPointFigure = class(TFigure)
    Bounds: TRect;
    constructor Create(X, Y: Integer;
      APenColor, ABrushColor: TColor; AThickness: Integer);
    procedure SetSecondPoint(X, Y: Integer);
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
constructor TTwoPointFigure.Create(X, Y: Integer;
  APenColor, ABrushColor: TColor; AThickness: Integer);
begin
  inherited Create(APenColor, ABrushColor, AThickness);
  Bounds := Rect(X, Y, X, Y);
end;

procedure TTwoPointFigure.SetSecondPoint(X, Y: Integer);
begin
  Bounds := Rect(Bounds.Left, Bounds.Top, X, Y);
end;

{ TPolyline }
constructor TPolyline.Create(X, Y: Integer;
      APenColor, ABrushColor: TColor; AThickness: Integer);
begin
  inherited Create(APenColor, ABrushColor, AThickness);
  AddPoint(X, Y);
end;

procedure TPolyline.AddPoint(X, Y: Integer);
begin
  SetLength(Vertexes, Length(Vertexes) + 1);
  Vertexes[High(Vertexes)] := Point(X, Y);
end;

procedure TPolyline.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Polyline(Vertexes);
end;

{ TRectangle }
procedure TRectangle.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Rectangle(Bounds);
end;

{ TEllipse }
procedure TEllipse.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Ellipse(Bounds);
end;

{ TLine }
procedure TLine.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Line(Bounds);
end;

end.

