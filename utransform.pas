unit UTransform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;

type

  TDoublePoint = record
    X: Double;
    Y: Double;
  end;

  TDoubleRect = record
    case Integer of
      0: (
          Left: Double;
          Top: Double;
          Right: Double;
          Bottom: Double;
        );
      1: (
          TopLeft: TDoublePoint;
          BottomRight: TDoublePoint;
        );
  end;

  TArrayOfTpoint = array of TPoint;

operator + (APoint, BPoint: TDoublePoint) RPoint: TDoublePoint;
operator - (APoint, BPoint: TDoublePoint) RPoint: TDoublePoint;

function DoublePoint(AX, AY: Double): TDoublePoint;
function DoublePoint(APoint: TPoint): TDoublePoint;
function DoubleRect(ATopLeft, ABottomRight: TDoublePoint): TDoubleRect;
function DoubleRect(ALeft, ATop, ARight, ABottom: Double): TDoubleRect;
procedure SetCanvasOffset(AX, AY: Double);
procedure AddCanvasOffset(AX, AY: Double);
procedure AddCanvasOffset(ACanvasOffset: TDoublePoint);
function GetCanvasOffset: TDoublePoint;
function DispToWorldX(AX: Integer): Double;
function DispToWorldY(AY: Integer): Double;
function DispToWorldCoord(AX, AY: Integer): TDoublePoint;
function DispToWorldCoord(ARect: TRect): TDoubleRect;
function DispToWorldCoord(APoint: TPoint): TDoublePoint;
function DispToWorldDimension(ADimension: Integer): Double;
function WorldToDispX(AX: Double): Integer;
function WorldToDispY(AY: Double): Integer;
function WorldToDispCoord(ADoubleRect: TDoubleRect): TRect;
function WorldToDispCoord(ADoublePoint: TDoublePoint): TPoint;
function WorldToDispDimension(ADimension: Double): Integer;
function WorldVertexesToDispCoord(
  AVertexes: array of TDoublePoint): TArrayOfTpoint;
procedure SetScalePercent(AScale: Double);
procedure SetScale(AScale: Double);
function IncreaseScale: Boolean;
function DecreaseScale: Boolean;
function GetScale:Double;

implementation

const
  MaxScale = 16;
  MinScale = 0.0625;

var
  Scale: Double = 1.0;
  CanvasOffset: TDoublePoint;//инициализируется X:=0, Y:=0

operator + (APoint, BPoint: TDoublePoint) RPoint: TDoublePoint;
begin
  RPoint.x := APoint.X + BPoint.X;
  RPOint.y := APoint.Y + BPoint.Y;
end;

operator - (APoint, BPoint: TDoublePoint) RPoint: TDoublePoint;
begin
  RPoint.x := APoint.X - BPoint.X;
  RPOint.y := APoint.Y - BPoint.Y;
end;

{ TDoublePoint }
function DoublePoint(AX, AY: Double): TDoublePoint;
begin
  with Result do
  begin
    X := AX;
    Y := AY;
  end;
end;

function DoublePoint(APoint: TPoint): TDoublePoint;
begin
  Result := DoublePoint(APoint.x, APoint.y);
end;

{ TDoubleRect }
function DoubleRect(ALeft, ATop, ARight, ABottom: Double): TDoubleRect;
begin
  with Result do begin
    Left := ALeft;
    Top := ATop;
    Right := ARight;
    Bottom := ABottom;
  end;
end;

function DoubleRect(ATopLeft, ABottomRight: TDoublePoint): TDoubleRect;
begin
  with Result do begin
    TopLeft := ATopLeft;
    BottomRight := ABottomRight;
  end;
end;

{ Scale }
procedure SetScalePercent(AScale: Double);
begin
  AScale := AScale / 100;
  if AScale > MaxScale then Scale := MaxScale
  else if AScale < MinScale then Scale := MinScale
  else Scale := AScale;
end;

procedure SetScale(AScale: Double);
begin
  if AScale > MaxScale then Scale := MaxScale
  else if AScale < MinScale then Scale := MinScale
  else Scale := AScale;
end;

function IncreaseScale: Boolean;
begin
  if 2 * Scale < MaxScale then begin
    Scale *= 2;
    Exit(True);
  end
  else if Scale < MaxScale then begin
    Scale := MaxScale;
  end;
  Result := False
end;

function DecreaseScale: Boolean;
begin
  if Scale / 2 > MinScale then begin
    Scale /= 2;
    Exit(True);
  end
  else if Scale > MinScale then begin
    Scale := MinScale;
  end;
  Result := False;
end;

function GetScale:Double;
begin
  Result := Scale;
end;

{ Canvas Offset }
procedure SetCanvasOffset(AX, AY: Double);
begin
  CanvasOffset.X := AX;
  CanvasOffset.Y := AY;
end;

procedure AddCanvasOffset(AX, AY: Double);
begin
  CanvasOffset.X += Scale * AX;
  CanvasOffset.Y += Scale * AY;
end;

procedure AddCanvasOffset(ACanvasOffset: TDoublePoint);
begin
  CanvasOffset += ACanvasOffset;
end;

function GetCanvasOffset: TDoublePoint;
begin
  Result := CanvasOffset;
end;

{ World -> Display }
function WorldToDispCoord(ADoublePoint: TDoublePoint): TPoint;
begin
  with Result do begin
    x := round(Scale * ADoublePoint.X - CanvasOffset.X);
    y := round(Scale * ADoublePoint.Y - CanvasOffset.Y);
  end;
end;

function WorldToDispCoord(ADoubleRect: TDoubleRect): TRect;
begin
  with Result do begin
    TopLeft := WorldToDispCoord(ADoubleRect.TopLeft);
    BottomRight := WorldToDispCoord(ADoubleRect.BottomRight);
  end;
end;

function WorldToDispX(AX: Double): Integer;
begin
  Result := round(Scale * AX - CanvasOffset.X);
end;

function WorldToDispY(AY: Double): Integer;
begin
  Result := round(Scale * AY - CanvasOffset.Y);
end;

function WorldToDispDimension(ADimension: Double): Integer;
begin
  Result := round(ADimension * Scale);
end;

function WorldVertexesToDispCoord(
  AVertexes: array of TDoublePoint): TArrayOfTpoint;
var
  i: Integer;
  PointVertexes: TArrayOfTpoint;
begin
  SetLength(PointVertexes, Length(AVertexes));
  for i := 0 to High(AVertexes) do begin
    PointVertexes[i] := WorldToDispCoord(AVertexes[i]);
  end;
  Result := PointVertexes;
end;

{ Display -> World }
function DispToWorldCoord(AX, AY: Integer): TDoublePoint;
begin
  with Result do begin
    X := (AX + CanvasOffset.X) / Scale;
    Y := (AY + CanvasOffset.Y) / Scale;
  end;
end;

function DispToWorldX(AX: Integer): Double;
begin
  Result := (AX + CanvasOffset.X) / Scale;
end;

function DispToWorldY(AY: Integer): Double;
begin
  Result := (AY + CanvasOffset.Y) / Scale;
end;

function DispToWorldCoord(APoint: TPoint): TDoublePoint;
begin
  with Result do begin
    X := (APoint.x + CanvasOffset.X) / Scale;
    Y := (APoint.y + CanvasOffset.Y) / Scale;
  end;
end;

function DispToWorldCoord(ARect: TRect): TDoubleRect;
begin
  with Result do begin
    TopLeft := DispToWorldCoord(ARect.TopLeft);
    BottomRight := DispToWorldCoord(ARect.BottomRight);
  end;
end;

function DispToWorldDimension(ADimension: Integer): Double;
begin
  Result := ADimension / Scale;
end;

initialization

CanvasOffset.X := 0;
CanvasOffset.Y := 0;

end.

