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

  TRectangle = class(TFigure)
    ARect: TRect;
    procedure SetFirstDot(X, Y: Integer);
    procedure SetSecondDot(X, Y: Integer);
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  TPolyline = class(TFigure)
    Vertexes: array of TPoint;
    procedure AddPoint(X, Y: Integer);
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  TLine = class(TFigure)
    ALine: TRect;
    procedure SetFirstDot(X, Y: Integer);
    procedure SetSecondDot(X, Y: Integer);
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  TEllipse = class(TFigure)
    ARect: TRect;
    procedure SetFirstDot(X, Y: Integer);
    procedure SetSecondDot(X, Y: Integer);
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

implementation
procedure TFigure.Draw(Canvas: TCanvas);
//var
  //CurrentPenColor, CurrentBrushColor: TColor;
begin
  //CurrentPenColor := Canvas.Pen.Color;
  //CurrentBrushColor := Canvas.Brush.Color;
  Canvas.Pen.Color := PenColor;
  Canvas.Brush.Color := BrushColor;
  DrawFigure(Canvas);
  //Canvas.Pen.Color := CurrentPenColor;
  //Canvas.Brush.Color := CurrentBrushColor;

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

procedure TRectangle.SetFirstDot(X, Y: Integer);
begin
  ARect := Rect(X, Y, X, Y);
end;

procedure TRectangle.SetSecondDot(X, Y: Integer);
begin
  ARect := Rect(ARect.Left, ARect.Top, X, Y);
end;

procedure TRectangle.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Rectangle(ARect);
end;

procedure TEllipse.SetFirstDot(X, Y: Integer);
begin
  ARect := Rect(X, Y, X, Y);
end;

procedure TEllipse.SetSecondDot(X, Y: Integer);
begin
  ARect := Rect(ARect.Left, ARect.Top, X, Y);
end;

procedure TEllipse.DrawFigure(Canvas: TCanvas);
var
  CurrentPenColor, CurrentBrushColor: TColor;
begin
  CurrentPenColor := Canvas.Pen.Color;
  CurrentBrushColor := Canvas.Brush.Color;

  Canvas.Pen.Color := PenColor;
  Canvas.Brush.Color := BrushColor;

  Canvas.Ellipse(ARect);

  Canvas.Pen.Color := CurrentPenColor;
  Canvas.Brush.Color := CurrentBrushColor;
end;

procedure TLine.SetFirstDot(X, Y: Integer);
begin
  ALine := Rect(X, Y, X, Y);
end;

procedure TLine.SetSecondDot(X, Y: Integer);
begin
  ALine := Rect(ALine.Left, ALine.Top, X, Y);
end;

procedure TLine.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Line(ALine);
end;

end.

