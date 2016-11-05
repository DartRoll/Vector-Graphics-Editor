unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, math, Controls, SysUtils, UFigures, Graphics, UScale;

type

  { TTool }

  TTool = class
    FFigure: TFigure;
    FIcon: String;
    function GetFigure: TFigure;
    procedure MouseDown(ADoublePoint: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer;
      Button: TMouseButton; AClientBounds: TRect); virtual; abstract;
    procedure MouseMove(ADoublePoint: TDoublePoint); virtual; abstract;
    procedure MouseUp; virtual;
  end;

  THandTool = class(TTool)
    FStartingPoint: TDoublePoint;
    constructor Create;
    procedure MouseDown(ADoublePoint: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer;
      Button: TMouseButton; AClientBounds: TRect); override;
    procedure MouseMove(ADoublePoint: TDoublePoint); override;
  end;

  TMagnifierTool = class(TTool)
    FStartingPoint: TDoublePoint;
    FIsSelectingArea: Boolean;
    FMouseButton: TMouseButton;
    FClientBounds: TRect;
    constructor Create;
    procedure MouseDown(ADoublePoint: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer;
      Button: TMouseButton; AClientBounds: TRect); override;
    procedure MouseMove(ADoublePoint: TDoublePoint); override;
    procedure MouseUp; override;
  end;

  TTwoPointFigureTool = class(TTool)
    procedure MouseMove(ADoublePoint: TDoublePoint); override;
  end;

  TPolylineTool = class(TTool)
    constructor Create;
    procedure MouseDown(ADoublePoint: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer;
      Button: TMouseButton; AClientBounds: TRect); override;
    procedure MouseMove(ADoublePoint: TDoublePoint); override;
  end;

  TRectangleTool = class(TTwoPointFigureTool)
    constructor Create;
    procedure MouseDown(ADoublePoint: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer;
      Button: TMouseButton; AClientBounds: TRect); override;
  end;

  TLineTool = class(TTwoPointFigureTool)
    constructor Create;
    procedure MouseDown(ADoublePoint: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer;
      Button: TMouseButton; AClientBounds: TRect); override;
  end;

  TEllipseTool = class(TTwoPointFigureTool)
    constructor Create;
    procedure MouseDown(ADoublePoint: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer;
      Button: TMouseButton; AClientBounds: TRect); override;
  end;

var
  Tools: array of TTool;

implementation

{ Misc }
procedure RegisterTool(Tool: TTool);
begin
  SetLength(Tools, Length(Tools) + 1);
  Tools[High(Tools)] := Tool;
end;

{ TMagnifierTool }
constructor TMagnifierTool.Create;
begin
  Inherited;
  FIcon := 'img/magnifier.bmp';
end;

procedure TMagnifierTool.MouseDown(ADoublePoint: TDoublePoint;
  APenColor, ABrushColor: TColor; ALineWidth: Integer;
  Button: TMouseButton; AClientBounds: TRect);
begin
  FClientBounds := AClientBounds;
  FIsSelectingArea := False;
  FMouseButton := Button;
  FFigure := TRectangleLine.Create(ADoublePoint, APenColor, ABrushColor, 1);
  FStartingPoint := ADoublePoint;
end;

procedure TMagnifierTool.MouseMove(ADoublePoint: TDoublePoint);
begin
  FIsSelectingArea := True;
  (FFigure as TTwoPointFigure).SetSecondPoint(ADoublePoint);
end;
// ДОБАВИТЬ ПОГРЕШНОСТЬ
//Перемещение в центр
procedure TMagnifierTool.MouseUp;
const
  Delta = 10;
var
  XScale, YScale: Double;
  ImageBounds: TDoubleRect;
  ClientWidth, ClientHeight: Double;
begin
  ImageBounds := FFigure.GetBounds;
  ClientWidth := FClientBounds.Right - FClientBounds.Left;
  ClientHeight := FClientBounds.Bottom - FClientBounds.Top;

  if FIsSelectingArea and
    ((ImageBounds.Right - ImageBounds.Left) > Delta / getScale) and
    ((ImageBounds.Bottom - ImageBounds.Top) > Delta / getScale) then begin
    XScale := (ClientWidth - 1)  /
      (ImageBounds.Right - ImageBounds.Left {+ 2 * BorderMargin});
    YScale := (ClientHeight - 1) /
      (ImageBounds.Bottom - ImageBounds.Top {+ 2 * BorderMargin});
    SetScale(Min(XScale, YScale));

    SetCanvasOffset((ImageBounds.Left * GetScale - (ClientWidth - GetScale * (ImageBounds.Right - ImageBounds.Left )) / 2),
      (ImageBounds.Top * GetScale - (ClientHeight - GetScale * (ImageBounds.Bottom - ImageBounds.Top )) / 2) );
  end
  else begin
    if FMouseButton = mbLeft then begin
      if IncreaseScale then
        SetCanvasOffset(FStartingPoint.X * GetScale - FStartingPoint.X, FStartingPoint.Y * GetScale - FStartingPoint.Y);
    end;
    if FMouseButton = mbRight then begin
      if DecreaseScale then
        SetCanvasOffset(FStartingPoint.X * GetScale - FStartingPoint.X,FStartingPoint.Y * GetScale -  FStartingPoint.Y);
    end;
  end;
  FIsSelectingArea := False;
  FFigure.Free;
  FFigure := nil; //Так можно делать?
end;

{ THandTool }
constructor THandTool.Create;
begin
  Inherited;
  FIcon := 'img/hand.bmp';
end;

procedure THandTool.MouseDown(ADoublePoint: TDoublePoint;
  APenColor, ABrushColor: TColor; ALineWidth: Integer;
  Button: TMouseButton; AClientBounds: TRect);
begin
  FStartingPoint := ADoublePoint;
end;

procedure THandTool.MouseMove(ADoublePoint: TDoublePoint);
begin
  AddCanvasOffset(FStartingPoint.X - ADoublePoint.x,
                  FStartingPoint.Y - ADoublePoint.y);
end;

{ TTool }
function TTool.GetFigure: TFigure;
begin
  Result := FFigure;
end;

procedure TTool.MouseUp;
begin
  //Ничего не делать
end;

{ TTwoPointFigureTool }
procedure TTwoPointFigureTool.MouseMove(ADoublePoint: TDoublePoint);
begin
  (FFigure as TTwoPointFigure).SetSecondPoint(ADoublePoint);
end;

{ TPolylineTool }
constructor TPolylineTool.Create;
begin
  Inherited;
  FIcon := 'img/polyline.bmp';
end;

procedure TPolylineTool.MouseDown(ADoublePoint: TDoublePoint;
  APenColor, ABrushColor: TColor; ALineWidth: Integer;
  Button: TMouseButton; AClientBounds: TRect);
begin
  FFigure := TPolyline.Create(ADoublePoint, APenColor, ABrushColor, ALineWidth);
end;

procedure TPolylineTool.MouseMove(ADoublePoint: TDoublePoint);
begin
  (FFigure as TPolyline).AddPoint(ADoublePoint);
end;

{ TRectangleTool }
constructor TRectangleTool.Create;
begin
  Inherited;
  FIcon := 'img/rectangle.bmp';
end;

procedure TRectangleTool.MouseDown(ADoublePoint: TDoublePoint;
  APenColor, ABrushColor: TColor; ALineWidth: Integer;
  Button: TMouseButton; AClientBounds: TRect);
begin
  FFigure := TRectangle.Create(ADoublePoint, APenColor, ABrushColor, ALineWidth);
end;

{ TEllipseTool }
constructor TEllipseTool.Create;
begin
  Inherited;
  FIcon := 'img/ellipse.bmp';
end;

procedure TEllipseTool.MouseDown(ADoublePoint: TDoublePoint;
  APenColor, ABrushColor: TColor; ALineWidth: Integer;
  Button: TMouseButton; AClientBounds: TRect);
begin
  FFigure := TEllipse.Create(ADoublePoint, APenColor, ABrushColor, ALineWidth);
end;

{ TLineTool }
constructor TLineTool.Create;
begin
  Inherited;
  FIcon := 'img/line.bmp';
end;

procedure TLineTool.MouseDown(ADoublePoint: TDoublePoint;
  APenColor, ABrushColor: TColor; ALineWidth: Integer;
  Button: TMouseButton; AClientBounds: TRect);
begin
  FFigure := TLine.Create(ADoublePoint, APenColor,ABrushColor, ALineWidth);
end;

initialization

RegisterTool(THandTool.Create);
RegisterTool(TMagnifierTool.Create);
RegisterTool(TPolylineTool.Create);
RegisterTool(TRectangleTool.Create);
RegisterTool(TEllipseTool.Create);
RegisterTool(TLineTool.Create);

end.


