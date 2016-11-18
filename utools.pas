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
    procedure MouseDown(AMousePos: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer;
      AButton: TMouseButton); virtual; abstract;
    procedure MouseMove(AMousePos: TDoublePoint); virtual; abstract;
    procedure MouseUp; virtual;
  end;

  { THandTool }

  THandTool = class(TTool)
    FStartingPoint: TDoublePoint;
    constructor Create;
    procedure MouseDown(AMousePoint: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer;
      AButton: TMouseButton); override;
    procedure MouseMove(AMousePoint: TDoublePoint); override;
  end;

  { TMagnifierTool }

  TMagnifierTool = class(TTool)
    FStartingPoint: TDoublePoint;
    FIsSelectingArea: Boolean;
    FMouseButton: TMouseButton;
    constructor Create;
    procedure MouseDown(AMousePos: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer;
      AButton: TMouseButton); override;
    procedure MouseMove(AMousePos: TDoublePoint); override;
    procedure MouseUp; override;
  end;

  { TTwoPointFigureTool }

  TTwoPointFigureTool = class(TTool)
    procedure MouseMove(AMousePos: TDoublePoint); override;
  end;

  { TPolylineTool }

  TPolylineTool = class(TTool)
    constructor Create;
    procedure MouseDown(AMousePos: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer;
      AButton: TMouseButton); override;
    procedure MouseMove(AMousePos: TDoublePoint); override;
  end;

  { TRectangleTool }

  TRectangleTool = class(TTwoPointFigureTool)
    constructor Create;
    procedure MouseDown(AMousePos: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer;
      AButton: TMouseButton); override;
  end;

  { TLineTool }

  TLineTool = class(TTwoPointFigureTool)
    constructor Create;
    procedure MouseDown(AMousePos: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer;
      AButton: TMouseButton); override;
  end;

  { TEllipseTool }

  TEllipseTool = class(TTwoPointFigureTool)
    constructor Create;
    procedure MouseDown(AMousePos: TDoublePoint;
      APenColor, ABrushColor: TColor; ALineWidth: Integer;
      AButton: TMouseButton); override;
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

procedure TMagnifierTool.MouseDown(AMousePos: TDoublePoint;
  APenColor, ABrushColor: TColor; ALineWidth: Integer;
  AButton: TMouseButton);
begin
  FIsSelectingArea := False;
  FMouseButton := AButton;
  FFigure := TRectangleLine.Create(AMousePos, clBlack, ABrushColor, 1);
  FStartingPoint := AMousePos;
end;

procedure TMagnifierTool.MouseMove(AMousePos: TDoublePoint);
begin
  FIsSelectingArea := True;
  (FFigure as TRectangleLine).SetSecondPoint(AMousePos);
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
  SelectionWidth := SelectionBounds.Right - SelectionBounds.Left;
  SelectionHeight := SelectionBounds.Bottom - SelectionBounds.Top;

  if FIsSelectingArea and
    (SelectionWidth > Delta / Scale) and
    (SelectionHeight > Delta / Scale)
  then begin
    XScale := (DispDimensions.Width) / SelectionWidth;
    YScale := (DispDimensions.Height) / SelectionHeight;
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
  AButton: TMouseButton);
begin
  FStartingPoint := AMousePoint;
end;

procedure THandTool.MouseMove(AMousePoint: TDoublePoint);
begin
  AddCanvasOffset((FStartingPoint - AMousePoint) * Scale);
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
procedure TTwoPointFigureTool.MouseMove(AMousePos: TDoublePoint);
begin
  (FFigure as TTwoPointFigure).SetSecondPoint(AMousePos);
end;

{ TPolylineTool }
constructor TPolylineTool.Create;
begin
  Inherited;
  FIcon := 'img/polyline.bmp';
end;

procedure TPolylineTool.MouseDown(AMousePos: TDoublePoint;
  APenColor, ABrushColor: TColor; ALineWidth: Integer;
  AButton: TMouseButton);
begin
  FFigure := TPolyline.Create(AMousePos, APenColor, ABrushColor, ALineWidth);
end;

procedure TPolylineTool.MouseMove(AMousePos: TDoublePoint);
begin
  (FFigure as TPolyline).AddPoint(AMousePos);
end;

{ TRectangleTool }
constructor TRectangleTool.Create;
begin
  Inherited;
  FIcon := 'img/rectangle.bmp';
end;

procedure TRectangleTool.MouseDown(AMousePos: TDoublePoint;
  APenColor, ABrushColor: TColor; ALineWidth: Integer;
  AButton: TMouseButton);
begin
  FFigure := TRectangle.Create(AMousePos, APenColor, ABrushColor, ALineWidth);
end;

{ TEllipseTool }
constructor TEllipseTool.Create;
begin
  Inherited;
  FIcon := 'img/ellipse.bmp';
end;

procedure TEllipseTool.MouseDown(AMousePos: TDoublePoint;
  APenColor, ABrushColor: TColor; ALineWidth: Integer;
  AButton: TMouseButton);
begin
  FFigure := TEllipse.Create(AMousePos, APenColor, ABrushColor, ALineWidth);
end;

{ TLineTool }
constructor TLineTool.Create;
begin
  Inherited;
  FIcon := 'img/line.bmp';
end;

procedure TLineTool.MouseDown(AMousePos: TDoublePoint;
  APenColor, ABrushColor: TColor; ALineWidth: Integer;
  AButton: TMouseButton);
begin
  FFigure := TLine.Create(AMousePos, APenColor,ABrushColor, ALineWidth);
end;

initialization

RegisterTool(THandTool.Create);
RegisterTool(TMagnifierTool.Create);
RegisterTool(TPolylineTool.Create);
RegisterTool(TRectangleTool.Create);
RegisterTool(TEllipseTool.Create);
RegisterTool(TLineTool.Create);

end.


