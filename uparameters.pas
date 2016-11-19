unit UParameters;

{$mode objfpc}{$H+}

interface

uses
  Classes, math, Controls, SysUtils, UFigures, Graphics, UTransform, StdCtrls,
  ExtCtrls, LCLClasses, Spin;

type

  TParamChange = procedure(Sender: TObject) of Object;

    { TParameter }//может вынести в отдельный модуль?
  TParameter = class
    FLabel: TLabel;
    FComponent: TControl;
    destructor Destroy; override; //этот деструктор будет выполнтся для потомков?
  end;

    { TWidthParameter }

  TWidthParameter = class(TParameter)
    constructor Create(AonChange: TParamChange);
  end;
implementation

{ TParameter }

destructor TParameter.Destroy;
begin
  FLabel.Free;
  FComponent.Free;
  inherited Destroy;
end;

{ TWidthParameter }

constructor TWidthParameter.Create(AonChange: TParamChange);
begin
  FLabel := TLabel.Create(nil);
  FLabel.Caption := 'Ширина';

  FComponent := TSpinEdit.Create(nil);
  with FComponent as TSpinEdit do begin
      MaxValue := 500;
      MinValue := 1;
      Value := 2;
      OnChange := AonChange;
  end;
end;


end.

