unit UScale;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;

type

  TDoublePoint = packed record
    X: Double;
    Y: Double;
  end;

  TDoubleRect = packed record
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
procedure SetCanvasOffset(AWidth, AHeight: Integer);
function DispToWorld(APoint: TPoint): TDoublePoint;
function DispToWorld(AX, AY: Integer): TDoublePoint;
function WorldToDisp(ADoublePoint: TDoublePoint): TPoint;
function VertexesToDisp(AVertexes: array of TDoublePoint): TArrayOfTpoint;
function FigureBoundsToDisp(ADoubleRect: TDoubleRect): TRect;

const
  //999999999999999
  WMaxX: Double = 999999;
  WMaxY: Double = 999999;

var
  Scale: Double = 1.0;
  CanvasOffset: TDoublePoint;
implementation

function DoublePoint(AX, AY: Double): TDoublePoint;
begin
  with Result do
  begin
    X := AX;
    Y := AY;
  end;
end;

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

procedure SetCanvasOffset(AWidth, AHeight: Integer);
begin
  //floor returns integer so does round
  CanvasOffset.X := RoundTo(WMaxX / 2, 0) - round(AWidth / 2);
  CanvasOffset.Y := RoundTo(WMaxY / 2, 0) - round(AHeight / 2);
end;

function WorldToDisp(ADoublePoint: TDoublePoint): TPoint;
begin
  with Result do begin
    //Проверить потерю точности
    x := round(ADoublePoint.X - CanvasOffset.X);
    y := round(ADoublePoint.Y - CanvasOffset.Y);
  end;
end;

function DispToWorld(AX, AY: Integer): TDoublePoint;
begin
  with Result do begin
    //Потеря точности
    X := (AX + CanvasOffset.X);
    Y := (AY + CanvasOffset.Y);
  end;
end;

function DispToWorld(APoint: TPoint): TDoublePoint;
begin
  with Result do begin
    //Потеря точности?
    X := (APoint.x + CanvasOffset.X);
    Y := (APoint.y + CanvasOffset.Y);
  end;
end;

function VertexesToDisp(AVertexes: array of TDoublePoint): TArrayOfTpoint;
var
  i: Integer;
  PointVertexes: TArrayOfTpoint;
begin
  SetLength(PointVertexes, Length(AVertexes));
  for i := 0 to High(AVertexes) do begin
    PointVertexes[i] := WorldToDisp(AVertexes[i]);
  end;
  Result := PointVertexes;
end;

function FigureBoundsToDisp(ADoubleRect: TDoubleRect): TRect;
begin
  //можно перегрузить и сдлеать проще
  with Result do begin
    TopLeft := WorldToDisp(ADoubleRect.TopLeft);
    BottomRight := WorldToDisp(ADoubleRect.BottomRight);
  end;
end;

initialization
CanvasOffset.X := 0;
CanvasOffset.Y := 0;

end.

