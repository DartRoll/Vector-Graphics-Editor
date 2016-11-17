unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, math, Controls, SysUtils, UFigures, Graphics, UTransform;

type

  { TTool }

  TTool = class
    FFigure: TFigure;
    FIcon: String;
    function GetFigure: TFigure;
    procedure MouseDown(ADoublePoint: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer;
      Button: TMouseButton); virtual; abstract;
    procedure MouseMove(ADoublePoint: TDoublePoint); virtual; abstract;
    procedure MouseUp; virtual;
  end;

  { THandTool }

  THandTool = class(TTool)
    FStartingPoint: TDoublePoint;
    constructor Create;
    procedure MouseDown(AMousePoint: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer;
      Button: TMouseButton); override;
    procedure MouseMove(AMousePoint: TDoublePoint); override;
  end;

  { TMagnifierTool }

  TMagnifierTool = class(TTool)
    FStartingPoint: TDoublePoint;
    FIsSelectingArea: Boolean;
    FMouseButton: TMouseButton;
    FPaintBoxBounds: TRect;
    constructor Create;
    procedure MouseDown(ADoublePoint: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer;
      Button: TMouseButton); override;
    procedure MouseMove(ADoublePoint: TDoublePoint); override;
    procedure MouseUp; override;
  end;

  { TTwoPointFigureTool }

  TTwoPointFigureTool = class(TTool)
    procedure MouseMove(ADoublePoint: TDoublePoint); override;
  end;

  { TPolylineTool }

  TPolylineTool = class(TTool)
    constructor Create;
    procedure MouseDown(ADoublePoint: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer;
      Button: TMouseButton); override;
    procedure MouseMove(ADoublePoint: TDoublePoint); override;
  end;

  { TRectangleTool }

  TRectangleTool = class(TTwoPointFigureTool)
    constructor Create;
    procedure MouseDown(AMousePos: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer;
      Button: TMouseButton); override;
  end;

  { TLineTool }

  TLineTool = class(TTwoPointFigureTool)
    constructor Create;
    procedure MouseDown(ADoublePoint: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer;
      Button: TMouseButton); override;
  end;

  { TEllipseTool }

  TEllipseTool = class(TTwoPointFigureTool)
    constructor Create;
    procedure MouseDown(ADoublePoint: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer;
      Button: TMouseButton); override;
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
  Button: TMouseButton);
begin
  FIsSelectingArea := False;
  FMouseButton := Button;
  FFigure := TRectangleLine.Create(ADoublePoint, clBlack, ABrushColor, 1);
  FStartingPoint := ADoublePoint;
end;

procedure TMagnifierTool.MouseMove(ADoublePoint: TDoublePoint);
begin
  FIsSelectingArea := True;
  (FFigure as TRectangleLine).SetSecondPoint(ADoublePoint);
end;

procedure TMagnifierTool.MouseUp;
const
  Delta = 5;//px
var
  XScale, YScale: Double;
  SelectionBounds: TDoubleRect;
  SelectionWidth, SelectionHeight: Double;
  PaintBoxWidth, PaintBoxHeight: Double;
  StartingCenterCrds: TDoublePoint;
begin
  SelectionBounds := FFigure.GetBounds;
  PaintBoxWidth := FPaintBoxBounds.Right - FPaintBoxBounds.Left;
  PaintBoxHeight := FPaintBoxBounds.Bottom - FPaintBoxBounds.Top;
  SelectionWidth := SelectionBounds.Right - SelectionBounds.Left;
  SelectionHeight := SelectionBounds.Bottom - SelectionBounds.Top;

  if FIsSelectingArea and
    (SelectionWidth >  DispToWorldDimension(Delta)) and
    (SelectionHeight > DispToWorldDimension(Delta))
  then begin
    XScale := (PaintBoxWidth - 1) / SelectionWidth;
    YScale := (PaintBoxHeight - 1) / SelectionHeight;
    SetScale(Min(XScale, YScale));
  end
  else begin
    StartingCenterCrds := DispToWorldCoord(round(PaintBoxWidth /2),
      round(PaintBoxHeight / 2));
    if FMouseButton = mbLeft then IncreaseScale;
    if FMouseButton = mbRight then DecreaseScale;
    AddCanvasOffset(
      StartingCenterCrds.X - DispToWorldX(round(PaintBoxWidth / 2)),
      StartingCenterCrds.Y - DispToWorldY(round(PaintBoxHeight / 2)));
  end;
  FIsSelectingArea := False;
  FreeAndNil(FFigure);
end;

{ THandTool }
constructor THandTool.Create;
begin
  Inherited;
  FIcon := 'img/hand.bmp';
end;

procedure THandTool.MouseDown(AMousePoint: TDoublePoint;
  APenColor, ABrushColor: TColor; ALineWidth: Integer;
  Button: TMouseButton);
begin
  FStartingPoint := AMousePoint;
end;

procedure THandTool.MouseMove(AMousePoint: TDoublePoint);
begin
   { TODO : Выяснить почему это работает}
  {AddCanvasOffset(
    FStartingPoint.X - AMousePoint.x,
    FStartingPoint.Y - AMousePoint.y);}
  AddCanvasOffset(FStartingPoint - AMousePoint);
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
  Button: TMouseButton);
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

procedure TRectangleTool.MouseDown(AMousePos: TDoublePoint;
  APenColor, ABrushColor: TColor; ALineWidth: Integer;
  Button: TMouseButton);
begin
  FFigure := TRectangle.Create(AMousePos, APenColor, ABrushColor, ALineWidth);
end;

{ TEllipseTool }
constructor TEllipseTool.Create;
begin
  Inherited;
  FIcon := 'img/ellipse.bmp';
end;

procedure TEllipseTool.MouseDown(ADoublePoint: TDoublePoint;
  APenColor, ABrushColor: TColor; ALineWidth: Integer;
  Button: TMouseButton);
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
  Button: TMouseButton);
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


