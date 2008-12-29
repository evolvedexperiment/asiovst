unit TransientDM;

interface

uses
  Windows, Messages, SysUtils, Classes, DAV_Common, DAV_VSTModule;

type
  TTransientDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAttackChangeHold(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChangeHold(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleOpen(Sender: TObject);
  private
    FDry         : Single;
    FBuf         : Array [0..1] of Single;
    FEnv         : Array [0..3] of Single;
    FAtt12       : Single;
    FAtt34       : Single;
    FRel12       : Single;
    FRel34       : Single;
    FAttack      : Array [0..1] of Single;
    FRelease     : Array [2..3] of Single;
    FFilterIn    : Single;
    FFilterOut   : Single;
    FFilterState : Single;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TTransientDataModule.VSTModuleOpen(Sender: TObject);
begin
 // Initial Parameters
 Parameter[0] := 50;   // Attack [%]
 Parameter[1] := 50;   // Release [%]
 Parameter[2] := 0;    // Output [dB]
 Parameter[3] := 0.49; // Filter
 Parameter[4] := 35;   // Att-rel [%]
 Parameter[5] := 35;   // Rel-att [%]
end;

procedure TTransientDataModule.ParameterOutputChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FDry := dB_to_Amp(Value);
end;

procedure TTransientDataModule.ParameterReleaseChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if Value > 50 then
  begin
   FRelease[2] := 1 - Power(10, -4.5);
   FRelease[3] := 1 - Power(10, -5.85 + 0.027 * Value);
  end
 else
  begin
   FRelease[2] := 1 - Power(10, -3.15 - 0.027 * Value);
   FRelease[3] := 1 - Power(10, -4.5);
  end;
end;

procedure TTransientDataModule.ParameterAttackChangeHold(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FRel12 := 1 - Power(10, -2 - 0.04 * Value);
end;

procedure TTransientDataModule.ParameterReleaseChangeHold(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FAtt34 := Power(10, - 0.04 * Value);
end;

procedure TTransientDataModule.ParameterAttackChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if Value > 50 then
  begin
   FAttack[0] := Power(10, -1.5);
   FAttack[1] := Power(10, 1 - 0.05 * Value);
  end
 else
  begin
   FAttack[0] := Power(10, -4 + 0.05 * Value);
   FAttack[1] := Power(10, -1.5);
  end;
end;

procedure TTransientDataModule.ParameterFilterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if Value > 0.5 then
  begin
   FFilterIn := 0.8 - 1.6 * Value;
   FFilterOut := 1 + FFilterIn;
   FFilterState := 1;
  end
 else
  begin
   FFilterIn := 0.1 + 1.8 * Value;
   FFilterOut := 1 - FFilterIn;
   FFilterState := 0;
  end;
end;

procedure TTransientDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample: Integer;
  a, b, e, f, g, i         : Single;
  e1, e2, e3, e4, y        : Single;
  a1, a2, r12, a34, r3, r4 : Single;
  fi, fo, fx, fb1, fb2     : Single;
begin
 e1  := FEnv[0];
 e2  := FEnv[1];
 e3  := FEnv[2];
 e4  := FEnv[3];
 y   := FDry;
 a1  := FAttack[0];
 a2  := FAttack[1];
 a34 := FAtt34;
 r12 := FRel12;
 r3  := FRelease[2];
 r4  := FRelease[3];
 fi  := FFilterIn;
 fo  := FFilterOut;
 fx  := FFilterState;
 fb1 := FBuf[0];
 fb2 := FBuf[1];

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
   FEnv[0] := 0;
   FEnv[1] := 0;
   FEnv[2] := 0;
   FEnv[3] := 0;
   FBuf[0] := 0;
   FBuf[1] := 0;
  end
 else
  begin
   FEnv[0] := e1;
   FEnv[1] := e2;
   FEnv[2] := e3;
   FEnv[3] := e4;
   FBuf[0] := fb1;
   FBuf[1] := fb2;
  end;
end;

end.
