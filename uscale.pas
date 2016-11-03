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
procedure SetCanvasOffset(AX, AY: Double);
procedure AddCanvasOffset(AX, AY: Double);
function GetCanvasOffset: TDoublePoint;
function DispToWorldCoord(AX, AY: Integer): TDoublePoint;
function WorldToDispCoord(ADoubleRect: TDoubleRect): TRect;
function WorldToDispCoord(ADoublePoint: TDoublePoint): TPoint;
function WorldVertexesToDispCoord(
  AVertexes: array of TDoublePoint): TArrayOfTpoint;
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

procedure SetScale(AScale: Double);
begin
  Scale := AScale / 100;
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

initialization

CanvasOffset.X := 0;
CanvasOffset.Y := 0;

end.

