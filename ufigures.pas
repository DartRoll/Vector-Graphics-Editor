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

  TTwoPointFigure = class(TFigure)
    Bounds: TRect;
    constructor Create(AX, AY: Integer;
    APenColor, ABrushColor: TColor; AThickness: Integer);
    procedure SetSecondPoint(AX, AY: Integer);
  end;

  TRectangle = class(TTwoPointFigure)
    {constructor Create(AX, AY: Integer;
      APenColor, ABrushColor: TColor; AThickness: Integer);  }
   procedure DrawFigure(Canvas: TCanvas); override;
  end;

  TPolyline = class(TFigure)
    Vertexes: array of TPoint;
    constructor Create(AX, AY: Integer;
      APenColor, ABrushColor: TColor; AThickness: Integer);
    procedure AddPoint(X, Y: Integer);
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  TLine = class(TTwoPointFigure)
//    constructor Create(AX, AY: Integer;
//      APenColor, ABrushColor: TColor; AThickness: Integer);
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  TEllipse = class(TTwoPointFigure)
//    constructor Create(AX, AY: Integer;
//      APenColor, ABrushColor: TColor; AThickness: Integer);
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
constructor TTwoPointFigure.Create(AX, AY: Integer;
  APenColor, ABrushColor: TColor; AThickness: Integer);
begin
  inherited Create(APenColor, ABrushColor, AThickness);
  Bounds := Rect(AX, AY, AX, AY);
end;

procedure TTwoPointFigure.SetSecondPoint(AX, AY: Integer);
begin
  Bounds := Rect(Bounds.Left, Bounds.Top, AX, AY);
end;

{ TPolyline }
constructor TPolyline.Create(AX, AY: Integer;
      APenColor, ABrushColor: TColor; AThickness: Integer);
begin
  inherited Create(APenColor, ABrushColor, AThickness);
  AddPoint(AX, AY);
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
{constructor TRectangle.Create(AX, AY: Integer;
      APenColor, ABrushColor: TColor; AThickness: Integer);
begin
  inherited;
end;}

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

