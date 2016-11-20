unit UParameters;

{$mode objfpc}{$H+}

interface

uses
  Classes, math, Controls, SysUtils, UFigures, Graphics, UTransform, StdCtrls,
  ExtCtrls, LCLClasses, Spin;

type

  TParamChange = procedure(Sender: TObject) of Object;

  { TParameter }

  TParameter = class
    FLabel: TLabel;
    FComponent: TControl;
    constructor Create;
    destructor Destroy; override;
  end;

  { TBorderWidthParameter }

  TBorderWidthParameter = class(TParameter)
    constructor Create(AonChange: TParamChange);
  end;

  { TBorderStyleParameter }

  TBorderStyleParameter = class(TParameter)
    constructor Create(AonChange: TParamChange);
  end;

  { TFillStyleParameter }

  TFillStyleParameter= class(TParameter)
    constructor Create(AonChange: TParamChange);
  end;


implementation

{ TFillStyleParameter }

constructor TFillStyleParameter.Create(AonChange: TParamChange);
begin
  Inherited Create;
  FLabel.Caption := 'Стиль заливки';

  FComponent := TComboBox.Create(nil);
  with FComponent as TComboBox do begin
    Items.Add('Сплошная');
    Items.Add('Без заливки');
    Items.Add('bsHorizontal');
    Items.Add('bsVertical');
    Items.Add('bsFDiagonal');
    Items.Add('bsBDiagonal');
    Items.Add('bsCross');
    Items.Add('bsDiagCross');
    Font.Size := 10;
    Width := 130;
    ItemIndex := 0;
    OnChange := AonChange;
  end;
end;

{ TBorderStyleParameter }

constructor TBorderStyleParameter.Create(AonChange: TParamChange);
begin
  Inherited Create;
  FLabel.Caption := 'Стиль линии';

  FComponent := TComboBox.Create(nil);
  with FComponent as TComboBox do begin
    Items.Add('─────');
    Items.Add('─ ─ ─ ─ ─');
    Items.Add('• • • • • • • • •');
    Items.Add('─ • ─ • ─ •');
    Items.Add('─ • • ─ • •');
    AutoComplete := False;

    Font.Bold := True;
    Font.Size := 10;
    Width := 130;
    ItemIndex := 0;
    OnChange := AonChange;
  end;
end;

{ TParameter }

constructor TParameter.Create;
begin
  Inherited;
  FLabel := TLabel.Create(nil);
  FLabel.Font.Size := 11;
end;

destructor TParameter.Destroy;
begin
  FLabel.Free;
  FComponent.Free;
  inherited Destroy;
end;

{ TBorderWidthParameter }

constructor TBorderWidthParameter.Create(AonChange: TParamChange);
begin
  Inherited Create;

  FLabel.Caption := 'Толщина линии';
  FComponent := TSpinEdit.Create(nil);
  with FComponent as TSpinEdit do begin
      MaxValue := 500;
      MinValue := 1;
      Value := 3;
      Font.Size := 11;
      Width := 64;
      OnChange := AonChange;
  end;
end;

end.

