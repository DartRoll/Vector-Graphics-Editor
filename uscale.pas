unit UScale;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TDoublePoint = packed record
    X: Double;
    Y: Double;
  end;

  TDoubleRect = packed record
    Left: Double;
    Top: Double;
    Right: Double;
    Bottom: Double;
  end;

function DoublePoint(AX, AY: Double): TDoublePoint;
function DoubleRect(ALeft, ATop, ARight, ABottom: Double): TDoubleRect;
procedure SetCanvasOffset(AWidth, AHeight: Integer);

const
  WMaxX: Double = 999999999999999;
  WMaxY: Double = 999999999999999;

var
  Scale: Double = 1;
  Offset: TDoublePoint;
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
  with Result do
  begin
    Left := ALeft;
    Top := ATop;
    Right := ARight;
    Bottom := ABottom;
  end;
end;

procedure SetCanvasOffset(AWidth, AHeight: Integer);
begin
  Offset.X := (WMaxX + 1) / 2 - round(AWidth / 2);
  Offset.Y := (WMaxY + 1) / 2 - round(AHeight / 2);
end;

function WorldToDisp(ADoublePoint: TDoublePoint): TPoint;
begin
  with Result do begin
    x := round(Scale * ADoublePoint.X - Offset.X);
    y := round(Scale * ADoublePoint.Y - Offset.Y);
  end;
end;

end.

