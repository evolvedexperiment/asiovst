unit TransientDM;

interface

uses
  Windows, Messages, SysUtils, Classes, DAVDCommon, DVSTModule;

type
  TTransientDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAttackChangeHold(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChangeHold(Sender: TObject; const Index: Integer; var Value: Single);
  private
    fDry   : Single;
    fBuf   : Array [0..1] of Single;
    fEnv   : Array [0..3] of Single;
    fAtt12 : Single;
    fAtt34 : Single;
    fRel12 : Single;
    fRel34 : Single;
    fAtt1  : Single;
    fAtt2  : Single;
    fRel3  : Single;
    fRel4  : Single;
    fFili  : Single;
    fFilo  : Single;
    fFilx  : Single;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TTransientDataModule.ParameterOutputChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fDry := dB_to_Amp(Value);
end;

procedure TTransientDataModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Value > 0.5 then
  begin
   fRel3 := 1 - Power(10, -4.5);
   fRel4 := 1 - Power(10, -5.85 + 2.7 * Value);
  end
 else
  begin
   fRel3 := 1 - Power(10, -3.15 - 2.7 * Value);
   fRel4 := 1 - Power(10, -4.5);
  end;
end;

procedure TTransientDataModule.ParameterAttackChangeHold(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fRel12 := 1 - Power(10, -2 - 4 * Value);
end;

procedure TTransientDataModule.ParameterReleaseChangeHold(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fAtt34 := Power(10, - 4 * Value);
end;

procedure TTransientDataModule.ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Value > 0.5 then
  begin
   fAtt1 := Power(10, -1.5);
   fAtt2 := Power(10, 1 - 5 * Value);
  end
 else
  begin
   fAtt1 := Power(10, -4 + 5 * Value);
   fAtt2 := Power(10, -1.5);
  end;
end;

procedure TTransientDataModule.ParameterFilterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Value > 0.5 then
  begin
   fFili := 0.8 - 1.6 * Value;
   fFilo := 1 + fFili;
   fFilx := 1;
  end
 else
  begin
   fFili := 0.1 + 1.8 * Value;
   fFilo := 1 - fFili;
   fFilx := 0;
  end;
end;

procedure TTransientDataModule.VSTModuleCreate(Sender: TObject);
begin
 Parameter[0] := 0.50; // Attack
 Parameter[1] := 0.50; // Release
 Parameter[2] := 0.50; // Output
 Parameter[3] := 0.49; // Filter
 Parameter[4] := 0.35; // Att-rel
 Parameter[5] := 0.35; // Rel-att
end;

procedure TTransientDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample: Integer;
  a, b, e, f, g, i         : Single;
  e1, e2, e3, e4, y        : Single;
  a1, a2, r12, a34, r3, r4 : Single;
  fi, fo, fx, fb1, fb2     : Single;
begin
 e1  := fEnv[0];
 e2  := fEnv[1];
 e3  := fEnv[2];
 e4  := fEnv[3];
 y   := fDry;
 a1  := fAtt1;
 a2  := fAtt2;
 r12 := fRel12;
 a34 := fAtt34;
 r3  := fRel3;
 r4  := fRel4;
 fi  := fFili;
 fo  := fFilo;
 fx  := fFilx;
 fb1 := fBuf[0];
 fb2 := fBuf[1];

 for Sample := 0 to SampleFrames - 1 do
  begin
   fb1 := fo * fb1 + fi * Inputs[0, Sample];
   fb2 := fo * fb2 + fi * Inputs[1, Sample];
   e   := fb1 + fx * Inputs[0, Sample];
   f   := fb2 + fx * Inputs[1, Sample];
   i   := abs(Inputs[0, Sample] + Inputs[1, Sample]);

   if i > e1 then e1 := e1 + a1  * (i-e1) else e1 := e1 * r12;
   if i > e2 then e2 := e2 + a2  * (i-e2) else e2 := e2 * r12;
   if i > e3 then e3 := e3 + a34 * (i-e3) else e3 := e3 * r3;
   if i > e4 then e4 := e4 + a34 * (i-e4) else e4 := e4 * r4;
   g := (e1 - e2 + e3 - e4);

   Outputs[0, Sample] := y * (Inputs[0, Sample] + e * g);
   Outputs[1, Sample] := y * (Inputs[1, Sample] + f * g);
  end;
 if (e1 < 1E-10) then
  begin
   fEnv[0] := 0;
   fEnv[1] := 0;
   fEnv[2] := 0;
   fEnv[3] := 0;
   fBuf[0] := 0;
   fBuf[1] := 0;
  end
 else
  begin
   fEnv[0] := e1;
   fEnv[1] := e2;
   fEnv[2] := e3;
   fEnv[3] := e4;
   fBuf[0] := fb1;
   fBuf[1] := fb2;
  end;
end;

end.
