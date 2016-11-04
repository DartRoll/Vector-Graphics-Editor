unit UScale;

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

function DoublePoint(AX, AY: Double): TDoublePoint;
function DoubleRect(ATopLeft, ABottomRight: TDoublePoint): TDoubleRect;
function DoubleRect(ALeft, ATop, ARight, ABottom: Double): TDoubleRect;
procedure SetCanvasOffset(AX, AY: Double);
procedure AddCanvasOffset(AX, AY: Double);
function GetCanvasOffset: TDoublePoint;
function DispToWorldX(AX: Integer): Double;
function DispToWorldY(AY: Integer): Double;
function DispToWorldCoord(AX, AY: Integer): TDoublePoint;
function WorldToDispX(AX: Double): Integer;
function WorldToDispY(AY: Double): Integer;
function WorldToDispCoord(ADoubleRect: TDoubleRect): TRect;
function WorldToDispCoord(ADoublePoint: TDoublePoint): TPoint;
function WorldVertexesToDispCoord(
  AVertexes: array of TDoublePoint): TArrayOfTpoint;
procedure SetScalePercent(AScale: Double);
procedure SetScale(AScale: Double);
function GetScale:Double;
//const
  //999999999999999
  //WMaxX: Double = 999999999999999;
  //WMaxY: Double = 999999999999999;

implementation

var
  Scale: Double = 1.0;
  CanvasOffset: TDoublePoint;//X:=0, Y:=0

{ TDoublePoint }
function DoublePoint(AX, AY: Double): TDoublePoint;
begin
  with Result do
  begin
    X := AX;
    Y := AY;
  end;
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
  Scale := AScale / 100;
end;

procedure SetScale(AScale: Double);
begin
  Scale := AScale;
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

function GetCanvasOffset: TDoublePoint;
begin
  Result := CanvasOffset;
end;

{ World -> Display}
function WorldToDispCoord(ADoublePoint: TDoublePoint): TPoint;
begin
  with Result do begin
    //Проверить потерю точности
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

{Display -> World}

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

initialization

CanvasOffset.X := 0;
CanvasOffset.Y := 0;

end.

