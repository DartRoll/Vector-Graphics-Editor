unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, Graphics;

type
  TTool = class
    Figure: TFigure;
    function GetFigure: TFigure;
    procedure MouseDown(AX, AY: Integer; APenColor, ABrushColor: TColor;
      ALineWidth: Integer); virtual; abstract;
    procedure MouseMove(AX, AY: Integer); virtual; abstract;
    procedure MouseUp(AX, AY: Integer);
  end;

  TRectangleTool = class(TTool)
    procedure MouseDown(AX, AY: Integer; APenColor, ABrushColor: TColor;
      ALineWidth: Integer); override;
    procedure MouseMove(X, Y: Integer); override;
  end;

  TPolylineTool = class(TTool)
    procedure MouseDown(AX, AY: Integer; APenColor, ABrushColor: TColor;
      ALineWidth: Integer); override;
    procedure MouseMove(X, Y: Integer); override;
  end;

  TLineTool = class(TTool)
    procedure MouseDown(AX, AY: Integer; APenColor, ABrushColor: TColor;
      ALineWidth: Integer); override;
    procedure MouseMove(X, Y: Integer); override;
  end;

  TEllipseTool = class(TTool)
    procedure MouseDown(AX, AY: Integer; APenColor, ABrushColor: TColor;
      ALineWidth: Integer); override;
    procedure MouseMove(X, Y: Integer); override;
  end;

implementation

{ TTool }
function TTool.GetFigure: TFigure;
begin
  Result := Figure;
end;

procedure TTool.MouseUp(AX, AY: Integer);
begin
end;

{ TPolylineTool }

procedure TPolylineTool.MouseDown(AX, AY: Integer; APenColor, ABrushColor: TColor;
      ALineWidth: Integer);
begin
  Figure := TPolyline.Create(AX, AY, APenColor, ABrushColor, ALineWidth);
end;

procedure TPolylineTool.MouseMove(X, Y: Integer);
begin
  (Figure as TPolyline).AddPoint(X, Y);
end;

{ TRectangleTool }

procedure TRectangleTool.MouseDown(AX, AY: Integer; APenColor, ABrushColor: TColor;
      ALineWidth: Integer);
begin
  Figure := TRectangle.Create(AX, AY, APenColor,ABrushColor, ALineWidth);
end;

procedure TRectangleTool.MouseMove(X, Y: Integer);
begin
  (Figure as TRectangle).SetSecondPoint(X, Y);
end;

{ TEllipseTool }

procedure TEllipseTool.MouseDown(AX, AY: Integer; APenColor, ABrushColor: TColor;
      ALineWidth: Integer);
begin
  Figure := TEllipse.Create(AX, AY, APenColor,ABrushColor, ALineWidth);
end;

procedure TEllipseTool.MouseMove(X, Y: Integer);
begin
  (Figure as TEllipse).SetSecondPoint(X, Y);
end;

{ TLineTool }

procedure TLineTool.MouseDown(AX, AY: Integer; APenColor, ABrushColor: TColor;
      ALineWidth: Integer);
begin
  Figure := TLine.Create(AX, AY, APenColor,ABrushColor, ALineWidth);
end;

procedure TLineTool.MouseMove(X, Y: Integer);
begin
  (Figure as TLine).SetSecondPoint(X, Y);
end;

end.

