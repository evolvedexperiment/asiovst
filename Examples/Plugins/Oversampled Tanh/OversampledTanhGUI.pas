unit OversampledTanhGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, Mask,
  DAV_Common, DAV_VSTModule, ComCtrls;

type

  TFmOversampledTanh = class(TForm)
    LbNbrOfCoeff: TLabel;
    CBCoeffs: TComboBox;
    TBTransition: TTrackBar;
    LbTransition: TLabel;
    procedure CBCoeffsChange(Sender: TObject);
    procedure TBTransitionChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
    procedure UpdateCoeffs;
    procedure UpdateTransition;
  end;

implementation

{$R *.DFM}

uses
  OversampledTanhModule;

procedure TFmOversampledTanh.FormShow(Sender: TObject);
begin
 UpdateCoeffs;
 UpdateTransition;
end;

procedure TFmOversampledTanh.CBCoeffsChange(Sender: TObject);
begin
 with TOversampledTanhModule(Owner) do
  begin
   Parameter[0] := StrtoInt(CBCoeffs.Text);
  end;
end;

procedure TFmOversampledTanh.TBTransitionChange(Sender: TObject);
begin
 with TOversampledTanhModule(Owner) do
  begin
   Parameter[1] := 0.01 * TBTransition.Position;
  end;
end;

procedure TFmOversampledTanh.UpdateCoeffs;
begin
 with TOversampledTanhModule(Owner) do
  begin
   CBCoeffs.Text := IntToStr(round(Parameter[0]));
  end;
end;

procedure TFmOversampledTanh.UpdateTransition;
begin
 with TOversampledTanhModule(Owner) do
  begin
   TBTransition.Position := round(100 * Parameter[1]);
  end;
end;

end.
