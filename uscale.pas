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

procedure lol;
begin
  //kek.Left := 2;
end;

end.

