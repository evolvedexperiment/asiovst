unit TransientDM;

interface

uses
  Windows, Messages, Classes, DAV_Common, DAV_VSTModule, DAV_DspTransient;

type
  TTransientDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAttackChangeHold(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChangeHold(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FTransientProcessor : TStereoTransientProcessor;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, SysUtils;

procedure TTransientDataModule.VSTModuleOpen(Sender: TObject);
begin
 FTransientProcessor := TStereoTransientProcessor.Create;
 FTransientProcessor.SampleRate := SampleRate;

 // Initial Parameters
 Parameter[0] := 0;   // Attack [%]
 Parameter[1] := 0;   // Release [%]
 Parameter[2] := 0;   // Output Gain [dB]
 Parameter[3] := -1;  // Filter [%]
 Parameter[4] := 35;  // Att-rel [%]
 Parameter[5] := 35;  // Rel-att [%]
end;

procedure TTransientDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FTransientProcessor);
end;

procedure TTransientDataModule.ParameterOutputChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if assigned(FTransientProcessor)
  then FTransientProcessor.Output := Value;
end;

procedure TTransientDataModule.ParameterAttackChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if assigned(FTransientProcessor)
  then FTransientProcessor.Attack := Value;
end;

procedure TTransientDataModule.ParameterReleaseChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if assigned(FTransientProcessor)
  then FTransientProcessor.Release := Value;
end;

procedure TTransientDataModule.ParameterAttackChangeHold(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if assigned(FTransientProcessor)
  then FTransientProcessor.AttackHold := Value;
end;

procedure TTransientDataModule.ParameterReleaseChangeHold(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if assigned(FTransientProcessor)
  then FTransientProcessor.ReleaseHold := Value;
end;

procedure TTransientDataModule.ParameterFilterDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 0.5 then PreDefined := 'Lowpass: ' + PreDefined else
 if Parameter[Index] > 0.5 then PreDefined := 'Highpass: ' + PreDefined;
end;

procedure TTransientDataModule.ParameterFilterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if assigned(FTransientProcessor)
  then FTransientProcessor.Filter := Value;
end;

procedure TTransientDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample      : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   FTransientProcessor.Process(Inputs[0, Sample], Inputs[1, Sample],
     Outputs[0, Sample], Outputs[1, Sample]);
  end;

(*
 // catch denormals
 if (FEnv[0] < 1E-10) then
  begin
   FEnv[0] := 0;
   FEnv[1] := 0;
   FEnv[2] := 0;
   FEnv[3] := 0;
   FState[0] := 0;
   FState[1] := 0;
  end;
*)
end;

end.
