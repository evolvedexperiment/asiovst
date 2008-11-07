unit OverdriveDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule;

type
  TOverdriveDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterDriveChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMuffleChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    fDrive  : Single;
    fGain   : Single;
    fFilter : Single;
    fState  : Array [0..1] of Double;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TOverdriveDataModule.ParameterDriveChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fDrive := 0.01 * Value;
end;

procedure TOverdriveDataModule.ParameterMuffleChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
  fFilter := Power(10, -1.6 * 0.01 * Value);
end;

procedure TOverdriveDataModule.ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fGain := dB_to_Amp(Value);
end;

procedure TOverdriveDataModule.VSTModuleCreate(Sender: TObject);
begin
 Parameter[0] := 0;  // [%]
 Parameter[1] := 0;  // [%]
 Parameter[2] := 0;  // [dB]
end;

procedure TOverdriveDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  State  : Array [0..1] of Double;
  Inp    : Array [0..1] of Double;
  Outp   : Array [0..1] of Double;
begin
 State[0] := fState[0];
 State[1] := fState[1];

 for Sample := 0 to SampleFrames - 1 do
  begin
   Inp[0] := Inputs[0, Sample];
   if (Inp[0] > 0)
    then Outp[0] :=  sqrt( Inp[0])
    else Outp[0] := -sqrt(-Inp[0]); //overdrive

   Inp[1] := Inputs[1, Sample];
   if (Inputs[1, Sample] > 0)
    then Outp[1] :=  sqrt( Inp[1])
    else Outp[1] := -sqrt(-Inp[1]); //overdrive

   State[0] := State[0] + fFilter * (fDrive * (Outp[0] - Inp[0]) + Inp[0] - State[0]); //filter
   State[1] := State[1] + fFilter * (fDrive * (Outp[1] - Inp[1]) + Inp[1] - State[1]);

   Outputs[0, Sample] := fGain * State[0];
   Outputs[1, Sample] := fGain * State[1];
  end;

  if abs(State[0]) > 1E-10
   then fState[0] := State[0]
   else fState[0] := 0;        //catch denormals
  if abs(State[1]) > 1E-10
   then fState[1] := State[1]
   else fState[1] := 0;
end;

end.
