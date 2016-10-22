unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, Graphics;

type

  TTool = class
    Figure: TFigure;
    function GetFigure: TFigure;
    procedure MouseDown(X, Y: Integer); virtual; abstract;
    procedure MouseMove(X, Y: Integer); virtual; abstract;
    procedure MouseUp(X, Y: Integer); virtual; //abstract;
  end;

  TRectangleTool = class(TTool)
    procedure MouseDown(X, Y: Integer); override;
    procedure MouseMove(X, Y: Integer); override;
  end;

  TPolylineTool = class(TTool)
    procedure MouseDown(X, Y: Integer); override;
    procedure MouseMove(X, Y: Integer); override;
  end;

  TLineTool = class(TTool)

  end;

  TEllipseTool = class(TTool)

  end;

implementation

function TTool.GetFigure: TFigure;
begin
  Result := Figure;
end;
{
procedure TTool.Draw(Canvas: TCanvas);
begin
end;

procedure TTool.MouseDown(X, Y: Integer);
begin
end;

procedure TTool.MouseMove(X, Y: Integer);
begin
end;
}

procedure TTool.MouseUp(X, Y: Integer);
begin
end;

procedure TPolylineTool.MouseDown(X, Y: Integer);
begin
  Figure := TPolyline.Create;
  (Figure as TPolyline).AddPoint(X, Y);
end;

procedure TPolylineTool.MouseMove(X, Y: Integer);
begin
  (Figure as TPolyline).AddPoint(X, Y);
end;

procedure TRectangleTool.MouseDown(X, Y: Integer);
begin
  Figure := TRectangle.Create;
  (Figure as TRectangle).SetFirstDot(X,Y);
end;

procedure TRectangleTool.MouseMove(X, Y: Integer);
begin
  (Figure as TRectangle).SetSecondDot(X, Y);
end;

end.

