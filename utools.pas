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
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      ALineWidth: Integer; AButton: TMouseButton); virtual; abstract;
    procedure MouseMove(AMousePos: TPoint); virtual; abstract;
    procedure MouseUp(AMousePos: TPoint); virtual;
  end;

  { THandTool }

  THandTool = class(TTool)
    FStartingPoint: TDoublePoint;
    constructor Create;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      ALineWidth: Integer; AButton: TMouseButton); override;
    procedure MouseMove(AMousePos: TPoint); override;
  end;

  { TMagnifierTool }

  TMagnifierTool = class(TTool)
    FStartingPoint: TDoublePoint;
    FIsSelectingArea: Boolean;
    FMouseButton: TMouseButton;
    constructor Create;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      ALineWidth: Integer; AButton: TMouseButton); override;
    procedure MouseMove(AMousePos: TPoint); override;
    procedure MouseUp(AMousePos: TPoint); override;
  end;

  { TTwoPointFigureTool }

  TTwoPointFigureTool = class(TTool)
    procedure MouseMove(AMousePos: TPoint); override;
  end;

  { TPolylineTool }

  TPolylineTool = class(TTool)
    constructor Create;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      ALineWidth: Integer; AButton: TMouseButton); override;
    procedure MouseMove(AMousePos: TPoint); override;
  end;

  { TRectangleTool }

  TRectangleTool = class(TTwoPointFigureTool)
    constructor Create;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      ALineWidth: Integer; AButton: TMouseButton); override;
  end;

  { TLineTool }

  TLineTool = class(TTwoPointFigureTool)
    constructor Create;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      ALineWidth: Integer; AButton: TMouseButton); override;
  end;

  { TEllipseTool }

  TEllipseTool = class(TTwoPointFigureTool)
    constructor Create;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      ALineWidth: Integer; AButton: TMouseButton); override;
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

procedure TMagnifierTool.MouseDown(AMousePos: TPoint; APenColor,
  ABrushColor: TColor; ALineWidth: Integer; AButton: TMouseButton);
begin
  FIsSelectingArea := False;
  FMouseButton := AButton;
  FFigure := TRectangleLine.Create(
    DispToWorldCoord(AMousePos), clBlack, ABrushColor, 1);
  FStartingPoint := DispToWorldCoord(AMousePos);
end;

procedure TMagnifierTool.MouseMove(AMousePos: TPoint);
begin
  FIsSelectingArea := True;
  (FFigure as TRectangleLine).SetSecondPoint(DispToWorldCoord(AMousePos));
end;

procedure TMagnifierTool.MouseUp(AMousePos: TPoint);
const
  Delta = 5;//px
var
  XScale, YScale: Double;
  SelectionBounds: TDoubleRect;
  SelectionWidth, SelectionHeight: Double;
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
    Scale := Min(XScale, YScale);
    //Размещение по центру
    SetCanvasOffset(
      SelectionBounds.Left * Scale - (DispDimensions.Width - (SelectionWidth ) * Scale) / 2,
      SelectionBounds.Top * Scale - (DispDimensions.Height - (SelectionHeight) * Scale) / 2);
  end
  else begin
    if FMouseButton = mbLeft then IncreaseScale;
    if FMouseButton = mbRight then DecreaseScale;
    AddCanvasOffset((FStartingPoint - DispToWorldCoord(AMousePos)) * Scale);
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

procedure THandTool.MouseDown(AMousePos: TPoint; APenColor,
  ABrushColor: TColor; ALineWidth: Integer; AButton: TMouseButton);
begin
  FStartingPoint := DispToWorldCoord(AMousePos);
end;

procedure THandTool.MouseMove(AMousePos: TPoint);
begin
  AddCanvasOffset((FStartingPoint - DispToWorldCoord(AMousePos)) * Scale);
end;

{ TTool }
function TTool.GetFigure: TFigure;
begin
  Result := FFigure;
end;

procedure TTool.MouseUp(AMousePos: TPoint);
begin
  //Ничего не делать
end;

{ TTwoPointFigureTool }
procedure TTwoPointFigureTool.MouseMove(AMousePos: TPoint);
begin
  (FFigure as TTwoPointFigure).SetSecondPoint(DispToWorldCoord(AMousePos));
end;

{ TPolylineTool }
constructor TPolylineTool.Create;
begin
  Inherited;
  FIcon := 'img/polyline.bmp';
end;

procedure TPolylineTool.MouseDown(AMousePos: TPoint; APenColor,
  ABrushColor: TColor; ALineWidth: Integer; AButton: TMouseButton);
begin
  FFigure := TPolyline.Create(
    DispToWorldCoord(AMousePos), APenColor, ABrushColor, ALineWidth);
end;

procedure TPolylineTool.MouseMove(AMousePos: TPoint);
begin
  (FFigure as TPolyline).AddPoint(DispToWorldCoord(AMousePos));
end;

{ TRectangleTool }
constructor TRectangleTool.Create;
begin
  Inherited;
  FIcon := 'img/rectangle.bmp';
end;

procedure TRectangleTool.MouseDown(AMousePos: TPoint; APenColor,
  ABrushColor: TColor; ALineWidth: Integer; AButton: TMouseButton);
begin
  FFigure := TRectangle.Create(
    DispToWorldCoord(AMousePos), APenColor, ABrushColor, ALineWidth);
end;

{ TEllipseTool }
constructor TEllipseTool.Create;
begin
  Inherited;
  FIcon := 'img/ellipse.bmp';
end;

procedure TEllipseTool.MouseDown(AMousePos: TPoint; APenColor,
  ABrushColor: TColor; ALineWidth: Integer; AButton: TMouseButton);
begin
  FFigure := TEllipse.Create(
   DispToWorldCoord(AMousePos), APenColor, ABrushColor, ALineWidth);
end;

{ TLineTool }
constructor TLineTool.Create;
begin
  Inherited;
  FIcon := 'img/line.bmp';
end;

procedure TLineTool.MouseDown(AMousePos: TPoint; APenColor,
  ABrushColor: TColor; ALineWidth: Integer; AButton: TMouseButton);
begin
  FFigure := TLine.Create(
   DispToWorldCoord(AMousePos), APenColor,ABrushColor, ALineWidth);
end;

initialization

RegisterTool(THandTool.Create);
RegisterTool(TMagnifierTool.Create);
RegisterTool(TPolylineTool.Create);
RegisterTool(TRectangleTool.Create);
RegisterTool(TEllipseTool.Create);
RegisterTool(TLineTool.Create);

end.


