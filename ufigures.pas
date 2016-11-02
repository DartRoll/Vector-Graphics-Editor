unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  UScale, Classes, SysUtils, Graphics;

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
    Vertexes: array of TDoublePoint;
    constructor Create(ADoublePoint: TDoublePoint;
      APenColor, ABrushColor: TColor; AThickness: Integer);
    procedure AddPoint(ADoublePoint: TDoublePoint);
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  TTwoPointFigure = class(TFigure)
    Bounds: TDoubleRect;
    constructor Create(ADoublePoint: TDoublePoint;
      APenColor, ABrushColor: TColor; AThickness: Integer);
    procedure SetSecondPoint(ADoublePoint: TDoublePoint);
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
  Canvas.Polyline(VertexesToDisp(Vertexes));
end;

{ TRectangle }
procedure TRectangle.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Rectangle(FigureBoundsToDisp(Bounds));
end;

{ TEllipse }
procedure TEllipse.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Ellipse(FigureBoundsToDisp(Bounds));
end;

{ TLine }
procedure TLine.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Line(FigureBoundsToDisp(Bounds));
end;

end.

