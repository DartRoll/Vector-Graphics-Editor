unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, math, Controls, SysUtils, UFigures, Graphics, UTransform, StdCtrls,
  ExtCtrls, Spin, UParameters, FPCanvas;

type

  { TTool }

  TTool = class
    FFigure: TFigure;
    FPanel: TPanel;
    FIcon: String;
    procedure InitParams; virtual; abstract;
    procedure AddParam(AParam: TParameter);
    procedure ShowParams;
    procedure Init(APanel: TPanel);
    function GetFigure: TFigure;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton); virtual; abstract;
    procedure MouseMove(AMousePos: TPoint); virtual; abstract;
    procedure MouseUp(AMousePos: TPoint); virtual;
  end;

  { THandTool }

  THandTool = class(TTool)
    FStartingPoint: TDoublePoint;
    constructor Create;
    procedure InitParams; override;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton); override;
    procedure MouseMove(AMousePos: TPoint); override;
  end;

  { TMagnifierTool }

  TMagnifierTool = class(TTool)
    FStartingPoint: TDoublePoint;
    FIsSelectingArea: Boolean;
    FMouseButton: TMouseButton;
    constructor Create;
    procedure InitParams; override;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton); override;
    procedure MouseMove(AMousePos: TPoint); override;
    procedure MouseUp(AMousePos: TPoint); override;
  end;

  { TFigureTool }

  TFigureTool = class(TTool)
    FLineWidth: Integer;
    FLineStyle: TFPPenStyle;
    procedure InitParams; override;
    procedure ChangeLineWidth(Sender: TObject);
    procedure ChangeLineStyle(Sender: TObject);
  end;

  { TTwoPointFigureTool }

  TTwoPointFigureTool = class(TFigureTool)
    procedure MouseMove(AMousePos: TPoint); override;
  end;

  { TFilledFigureTool }

  TFilledFigureTool = class(TTwoPointFigureTool)
    FBrushStyle: TFPBrushStyle;
    procedure ChangeBrushStyle(Sender: TObject);
    procedure InitParams; override;
  end;

  { TPolylineTool }

  TPolylineTool = class(TFigureTool)
    procedure InitParams; override;
    constructor Create;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton); override;
    procedure MouseMove(AMousePos: TPoint); override;
  end;

  { TRectangleTool }

  TRectangleTool = class(TFilledFigureTool)
    constructor Create;
    procedure InitParams; override;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton); override;
  end;

  { TLineTool }

  TLineTool = class(TTwoPointFigureTool)
    constructor Create;
    procedure InitParams; override;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton); override;
  end;

  { TEllipseTool }

  TEllipseTool = class(TFilledFigureTool)
    constructor Create;
    procedure InitParams; override;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton); override;
  end;

  { TRegularPolygonTool }

  TRegularPolygonTool = class(TFilledFigureTool)
    constructor Create;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton); override;
  end;

var
  Tools: array of TTool;
implementation
var
  Params: array of TParameter;

{ Misc }
procedure RegisterTool(Tool: TTool);
begin
  SetLength(Tools, Length(Tools) + 1);
  Tools[High(Tools)] := Tool;
end;

{ TRegularPolygonTool }

constructor TRegularPolygonTool.Create;
begin
  Inherited;
  FIcon := 'img/hand.bmp';
end;

procedure TRegularPolygonTool.MouseDown(AMousePos: TPoint; APenColor,
  ABrushColor: TColor; AButton: TMouseButton);
begin
  FFigure := TRegularPolygon.Create(
    DispToWorldCoord(AMousePos), APenColor, ABrushColor, FLineStyle, FLineWidth, FBrushStyle, 6);
end;

{ TFilledFigureTool }

procedure TFilledFigureTool.ChangeBrushStyle(Sender: TObject);
begin
  with Sender as TComboBox do begin
    case ItemIndex of
      0: FBrushStyle := bsSolid;
      1: FBrushStyle := bsClear;
      2: FBrushStyle := bsHorizontal;
      3: FBrushStyle := bsVertical;
      4: FBrushStyle := bsFDiagonal;
      5: FBrushStyle := bsBDiagonal;
      6: FBrushStyle := bsCross;
      7: FBrushStyle := bsDiagCross;
    end;
  end;
end;

procedure TFilledFigureTool.InitParams;
begin
  Inherited;
  AddParam(TFillStyleParameter.Create(@ChangeBrushStyle));
end;

{ TFigureTool }
procedure TFigureTool.InitParams;
begin
  AddParam(TBorderWidthParameter.Create(@ChangeLineWidth));
  AddParam(TBorderStyleParameter.Create(@ChangeLineStyle));
  FLineWidth := 3;
end;

procedure TFigureTool.ChangeLineWidth(Sender: TObject);
begin
  FLineWidth := (Sender as TSpinEdit).Value;
end;

procedure TFigureTool.ChangeLineStyle(Sender: TObject);
begin
  with Sender as TComboBox do begin
    case ItemIndex of
      0: FLineStyle := psSolid;
      1: FLineStyle := psDash;
      2: FLineStyle := psDot;
      3: FLineStyle := psDashDot;
      4: FLineStyle := psDashDotDot;
    end;
  end;
end;

{ TMagnifierTool }
constructor TMagnifierTool.Create;
begin
  Inherited;
  FIcon := 'img/magnifier.bmp';
end;

procedure TMagnifierTool.InitParams;
begin
  //пока ничего
end;

procedure TMagnifierTool.MouseDown(AMousePos: TPoint; APenColor,
  ABrushColor: TColor; AButton: TMouseButton);
begin
  FIsSelectingArea := False;
  FMouseButton := AButton;
  FFigure := TRectangleLine.Create(
    DispToWorldCoord(AMousePos), clBlack, psSolid, 1);
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

procedure THandTool.InitParams;
begin
  //нет параметров
end;

procedure THandTool.MouseDown(AMousePos: TPoint; APenColor,
  ABrushColor: TColor; AButton: TMouseButton);
begin
  FStartingPoint := DispToWorldCoord(AMousePos);
end;

procedure THandTool.MouseMove(AMousePos: TPoint);
begin
  AddCanvasOffset((FStartingPoint - DispToWorldCoord(AMousePos)) * Scale);
end;

{ TTool }

procedure TTool.AddParam(AParam: TParameter);
begin
  SetLength(Params, Length(Params) + 1);
  Params[High(Params)] := AParam;
end;

procedure TTool.ShowParams;
var i: Integer;
begin
  for i := 0 to High(Params) do begin
      with Params[i] do begin
        FLabel.Top := i * 50;
        FLabel.Parent := FPanel;
        FComponent.Top := i * 50 + FLabel.ClientHeight + 5;
        FComponent.Left := FPanel.ClientWidth - FComponent.ClientWidth;
        FComponent.Parent := FPanel;
      end;
  end;
end;

procedure TTool.Init(APanel: TPanel);
var i: Integer;
begin
  for i := Low(Params) to High(Params) do begin
    Params[i].Free;
  end;
  Params := Nil;
  FPanel := APanel;
  InitParams;
  ShowParams;
  APanel.Invalidate;
end;

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

procedure TPolylineTool.InitParams;
begin
  Inherited;
end;

procedure TPolylineTool.MouseDown(AMousePos: TPoint; APenColor,
  ABrushColor: TColor; AButton: TMouseButton);
begin
  FFigure := TPolyline.Create(
    DispToWorldCoord(AMousePos), APenColor, FLineStyle, FLineWidth);
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

procedure TRectangleTool.InitParams;
begin
  Inherited;
end;

procedure TRectangleTool.MouseDown(AMousePos: TPoint; APenColor,
  ABrushColor: TColor; AButton: TMouseButton);
begin
  FFigure := TRectangle.Create(
    DispToWorldCoord(AMousePos), APenColor, ABrushColor, FLineStyle, FLineWidth, FBrushStyle);
end;

{ TEllipseTool }
constructor TEllipseTool.Create;
begin
  Inherited;
  FIcon := 'img/ellipse.bmp';
end;

procedure TEllipseTool.InitParams;
begin
  inherited;
end;

procedure TEllipseTool.MouseDown(AMousePos: TPoint; APenColor,
  ABrushColor: TColor; AButton: TMouseButton);
begin
  FFigure := TEllipse.Create(
   DispToWorldCoord(AMousePos), APenColor, ABrushColor, FLineStyle, FLineWidth, FBrushStyle);
end;

{ TLineTool }
constructor TLineTool.Create;
begin
  Inherited;
  FIcon := 'img/line.bmp';
end;

procedure TLineTool.InitParams;
begin
  inherited;
end;

procedure TLineTool.MouseDown(AMousePos: TPoint; APenColor,
  ABrushColor: TColor; AButton: TMouseButton);
begin
  FFigure := TLine.Create(
   DispToWorldCoord(AMousePos), APenColor, FLineStyle, FLineWidth);
end;

initialization

RegisterTool(THandTool.Create);
RegisterTool(TMagnifierTool.Create);
RegisterTool(TPolylineTool.Create);
RegisterTool(TRectangleTool.Create);
RegisterTool(TEllipseTool.Create);
RegisterTool(TLineTool.Create);
RegisterTool(TRegularPolygonTool.Create);

end.

