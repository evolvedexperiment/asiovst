unit StereoDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms,
  DAVDCommon, DVSTModule;

type
  TStereoDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterWidthLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterWidthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDelayChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRateChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterModChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    fPhi, fDel : Single;
    fDeltaPhi  : Single;
    fMode      : Single;
    fSize      : Integer;
    fBufferPos : Integer;
    fDelay     : Integer;
    fRi, fRd   : Single;
    fLi, fLd   : Single;
    fBuffer    : PAVDSingleFixedArray;
    procedure DeltaPhiChanged;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TStereoDataModule.VSTModuleCreate(Sender: TObject);
begin
(*
 //inits here!
 Parameter[0] = 0.78; // Haas/Comb width
 Parameter[1] = 0.43; // Delay
 Parameter[2] = 0.50; // Balance
 Parameter[3] = 0.00; // Mod
 Parameter[4] = 0.50; // Rate
*)

 fSize      := 4800;
 fBufferPos := 0;
 GetMem(fBuffer, fSize * SizeOf(Single));
end;

procedure TStereoDataModule.VSTModuleDestroy(Sender: TObject);
begin
 if assigned(fBuffer) then Dispose(fBuffer);
end;

procedure TStereoDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if (Parameter[2] > 0.5) then
  begin
   fLi := fLi * 2 * (1 - Parameter[2]);
   fLd := fLd * 2 * (1 - Parameter[2]);
  end
 else
  begin
   fRi := fRi * (2 * Parameter[2]);
   fRd := fRd * (2 * Parameter[2]);
  end;

 fRi := fRi * abs(Parameter[0]);
 fRd := fRd * abs(Parameter[0]);
 fLi := fLi * abs(Parameter[0]);
 fLd := fLd * abs(Parameter[0]);
end;

function fmod(Arg1, Arg2: Single): Single;
var
  Norm : Single;
begin
 Norm := Arg1 / Arg2;
 result := (Norm - round(Norm - 0.5)) * Arg2
end;

procedure TStereoDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample  : Integer;
  a       : Double;
  li, ld,
  ri, rd,
  del, ph : Double;
  dph, md : Double;
  tmp, bp : Integer;
begin
 ph  := fPhi;
 dph := fDeltaPhi;
 bp  := fBufferPos;

 li  := fLi;
 ld  := fLd;
 ri  := fRi;
 rd  := fRd;
 del := fDel;

 if (fMode > 0) then //modulated delay
  for Sample := 0 to SampleFrames - 1 do
   begin
    a := Inputs[0, Sample] + Inputs[1, Sample]; //sum to mono

    fBuffer[bp] := a; //write
//    tmp := bp[round(del + abs(mo * sin(ph)))] mod 4410;
//    b   := fBuffer[tmp];

    Outputs[0, Sample] := (Inputs[0, Sample] * li) - (Inputs[1, Sample] * ld); // output
    Outputs[1, Sample] := (Inputs[0, Sample] * ri) - (Inputs[1, Sample] * rd);

    dec(bp);
    if (bp < 0)
     then bp := 4410; //fBuffer position

    ph := ph + dph;
   end
 else
  for Sample := 0 to SampleFrames - 1 do
   begin
    a := Inputs[0, Sample] + Inputs[1, Sample]; //sum to mono

    fBuffer[bp] := a; //write
//    tmp := bp[round(del)] mod 4410;
//    b   := fBuffer[tmp];

    Outputs[0, Sample] := (Inputs[0, Sample] * li) - (Inputs[1, Sample] * ld); // output
    Outputs[1, Sample] := (Inputs[0, Sample] * ri) - (Inputs[1, Sample] * rd);

    dec(bp);
    if (bp < 0)
     then bp := 4410; //fBuffer position

    ph := ph + dph;
   end;
 fBufferPos := bp;
 fPhi       := fmod(ph, 2 * Pi);
end;

procedure TStereoDataModule.VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
begin
 DeltaPhiChanged;
end;

procedure TStereoDataModule.VSTModuleSuspend(Sender: TObject);
begin
 FillChar(fBuffer, fSize * SizeOf(Single), 0);
end;

procedure TStereoDataModule.ParameterWidthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if (Parameter[0] < 0.5) then
  begin
   fLi := (0.25 + (1.5 * Parameter[0]));
   fLd := 0;
   fRi := (2 * Parameter[0]);
   fRd := (1 - fRi);
  end
 else
  begin
   fLi := (1.5 - Parameter[0]);
   fLd := (Parameter[0] - 0.5);
   fRi := fLi;
   fRd := -fLd;
  end;
end;

procedure TStereoDataModule.ParameterWidthLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 0.5
  then Predefined := 'Haas'
  else Predefined := 'Comb';
end;

procedure TStereoDataModule.ParameterModChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fMode := (2100 * Power(Value, 2));
end;

procedure TStereoDataModule.ParameterRateChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 DeltaPhiChanged;
end;

procedure TStereoDataModule.DeltaPhiChanged;
begin
 fDeltaPhi := (Pi * Power(10, -2 + 3 * Parameter[3]) / SampleRate);
end;

procedure TStereoDataModule.ParameterDelayChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fDelay := round(20 + 2080 * Power(Value, 2));
end;

end.

(*
void mdaStereo::getParameterDisplay(VstInt32 index, char *text)
begin
  switch(index)
  begin
    case 0: long2string((long)(200.0 * abs(Parameter[0] - 0.5)), text);break;
    case 1: float2strng((1000.0 * fdel / SampleRate), text); break;
    case 2: long2string((long)(200.0 * (Parameter[2] - 0.5)), text); break;
    case 3: if(mod > 0) float2strng((1000.0 * mod / SampleRate), text);
            else strcpy(text, "OFF"); break;
    case 4: float2strng(Power(10.0,2.0 - 3.0 * Parameter[4]), text); break;
  end;
end;
*)
