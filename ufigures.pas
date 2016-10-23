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
    procedure Draw(Canvas: TCanvas);
    procedure DrawFigure(Canvas: TCanvas); virtual; abstract;
  end;

  TTwoPointFigure = class(TFigure)
    Dimensions: TRect;
    procedure SetFirstPoint(X, Y: Integer); virtual;
    procedure SetSecondPoint(X, Y: Integer); virtual;
  end;

  TRectangle = class(TTwoPointFigure)
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  TPolyline = class(TFigure)
    Vertexes: array of TPoint;
    procedure AddPoint(X, Y: Integer);
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  TLine = class(TTwoPointFigure)
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  TEllipse = class(TTwoPointFigure)
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

implementation
procedure TFigure.Draw(Canvas: TCanvas);
{var
  CurrentPenColor, CurrentBrushColor: TColor;}
begin
  {CurrentPenColor := Canvas.Pen.Color;
  CurrentBrushColor := Canvas.Brush.Color;}
  Canvas.Pen.Color := PenColor;
  Canvas.Brush.Color := BrushColor;
  DrawFigure(Canvas);
  {Canvas.Pen.Color := CurrentPenColor;
  Canvas.Brush.Color := CurrentBrushColor; }
end;

procedure TTwoPointFigure.SetFirstPoint(X, Y: Integer);
begin
  Dimensions := Rect(X, Y, X, Y);
end;

procedure TTwoPointFigure.SetSecondPoint(X, Y: Integer);
begin
  Dimensions := Rect(Dimensions.Left, Dimensions.Top, X, Y);
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

procedure TRectangle.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Rectangle(Dimensions);
end;

procedure TEllipse.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Ellipse(Dimensions);
end;

procedure TLine.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Line(Dimensions);
end;

end.

