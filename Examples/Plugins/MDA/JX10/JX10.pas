unit JX10;

interface

uses
  Windows, Messages, SysUtils, Classes, DAVDCommon, DVSTEffect,
  DVSTCustomModule, DVSTModule;

type
  TJX10DataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    function VSTModuleOutputProperties(Sender: TObject; var vLabel, shortLabel: string; var SpeakerArrangement: TVstSpeakerArrangementType; var Flags: TChannelPropertyFlags): Integer;
  private
    fMode     : Single;
    fNoiseMix : Single;
    fVolTrim  : Single;
    fSemi     : Single;
    fCent     : Single;
    fOscMix   : Single;
    fDetune   : Single;
    fTune     : Single;
    fVibrato  : Single;
    fPWDepth  : Single;
    fLFOHz    : Single;
    fDeltaLFO : Single;
    procedure Update;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TJX10DataModule.Update;  // Parameter Change
var
  ifs : Double;
begin
 ifs := 1 / SampleRate;
 fMode     := round(7.9 * Parameter[3]);
 fNoiseMix := sqr(Parameter[21]);
 fVolTrim  := (3.2 - Parameter[0] - 1.5 * fNoiseMix) * (1.5 - 0.5 * Parameter[7]);
 fNoiseMix := fNoiseMix * 0.06;
 fOscMix   := Parameter[0];

 fSemi     := trunc(48 * Parameter[1]) - 24;
 fCent     := 15.876 * Parameter[2] - 7.938;
 fCent     := 0.1 * trunc(sqr(fCent) * fCent);
 fDetune   := Power(1.059463094359, - fSemi - 0.01 * fCent);
 fTune     := -23.376 - 2 * Parameter[23] - 12 * trunc(Parameter[22] * 4.9);
 fTune     := SampleRate * Power(1.059463094359, fTune);

 fVibrato  := 0.2 * (Parameter[20] - 0.5) * (Parameter[20] - 0.5);
 fPWDepth    := fVibrato;
 if Parameter[20] < 0.5 then fVibrato := 0;

(*
 fLFOHz    := exp(7.0 * Parameter[19] - 4.0);
 fDeltaLFO := fLFOHz * (ifs * TWOPI * KMAX);

 fFilterFreq := 8 * Parameter[6] - 1.5;
 fFilterQ    := sqr(1 - Parameter[7]);        ////// + 0.02;
 fFilterLFO  := 2.5 * sqr(Parameter[9]);
 fFilterEnv  := 12 * Parameter[8] - 6;
 fFilterVel  := 0.1 * Parameter[10] - 0.05;
 if (Parameter[10] < 0.05) then
   begin
    fVelOff := 1;
    fFilterVel := 0;
   end
  else fVelOff := 0;

 fAttack  := 1 - exp(-ifs * exp(5.5 - 7.5 * Parameter[15]));
 fDecay   := 1 - exp(-ifs * exp(5.5 - 7.5 * Parameter[16]));
 fSustain := Parameter[17];
 fRelease := 1 - exp(-ifs * exp(5.5 - 7.5 * Parameter[18]));
 if (Parameter[18] < 0.01
  then fRelease := 0.1; //extra fast release

 ifs := ifs * KMAX; //lower update rate...

 fAtt := 1 - exp(-ifs * exp(5.5 - 7.5 * Parameter[11]));
 fDec := 1 - exp(-ifs * exp(5.5 - 7.5 * Parameter[12]));
 fSus := sqr(Parameter[13]);
 fRel := 1 - exp(-ifs * exp(5.5 - 7.5 * Parameter[14]));

 if(Parameter[4]<0.02) fGlide = 1.0; else
 fGlide     := 1 - exp(-ifs * exp(6.0 - 7.0 * Parameter[4]));
 fGlidedisp := (6.604 * Parameter[5] - 3.302);
 fGlidedisp := fGlidedisp * sqr(fGlidedisp);
*)
end;


procedure TJX10DataModule.VSTModuleCreate(Sender: TObject);
var
  i : Integer;
begin
 i := 0;
(*
  if(programs)
  begin
    fillpatch(i++, "5th Sweep Pad", 1.0, 0.37, 0.25, 0.3, 0.32, 0.5, 0.9, 0.6, 0.12, 0.0, 0.5, 0.9, 0.89, 0.9, 0.73, 0.0, 0.5, 1.0, 0.71, 0.81, 0.65, 0.0, 0.5, 0.5);
    fillpatch(i++, "Echo Pad [SA]", 0.88, 0.51, 0.5, 0.0, 0.49, 0.5, 0.46, 0.76, 0.69, 0.1, 0.69, 1.0, 0.86, 0.76, 0.57, 0.3, 0.8, 0.68, 0.66, 0.79, 0.13, 0.25, 0.45, 0.5);
    fillpatch(i++, "Space Chimes [SA]", 0.88, 0.51, 0.5, 0.16, 0.49, 0.5, 0.49, 0.82, 0.66, 0.08, 0.89, 0.85, 0.69, 0.76, 0.47, 0.12, 0.22, 0.55, 0.66, 0.89, 0.34, 0.0, 1.0, 0.5);
    fillpatch(i++, "Solid Backing", 1.0, 0.26, 0.14, 0.0, 0.35, 0.5, 0.3, 0.25, 0.7, 0.0, 0.63, 0.0, 0.35, 0.0, 0.25, 0.0, 0.5, 1.0, 0.3, 0.81, 0.5, 0.5, 0.5, 0.5);
    fillpatch(i++, "Velocity Backing [SA]", 0.41, 0.5, 0.79, 0.0, 0.08, 0.32, 0.49, 0.01, 0.34, 0.0, 0.93, 0.61, 0.87, 1.0, 0.93, 0.11, 0.48, 0.98, 0.32, 0.81, 0.5, 0.0, 0.5, 0.5);
    fillpatch(i++, "Rubber Backing [ZF]", 0.29, 0.76, 0.26, 0.0, 0.18, 0.76, 0.35, 0.15, 0.77, 0.14, 0.54, 0.0, 0.42, 0.13, 0.21, 0.0, 0.56, 0.0, 0.32, 0.2, 0.58, 0.22, 0.53, 0.5);
    fillpatch(i++, "808 State Lead", 1.0, 0.65, 0.24, 0.4, 0.34, 0.85, 0.65, 0.63, 0.75, 0.16, 0.5, 0.0, 0.3, 0.0, 0.25, 0.17, 0.5, 1.0, 0.03, 0.81, 0.5, 0.0, 0.68, 0.5);
    fillpatch(i++, "Mono Glide", 0.0, 0.25, 0.5, 1.0, 0.46, 0.5, 0.51, 0.0, 0.5, 0.0, 0.0, 0.0, 0.3, 0.0, 0.25, 0.37, 0.5, 1.0, 0.38, 0.81, 0.62, 0.0, 0.5, 0.5);
    fillpatch(i++, "Detuned Techno Lead", 0.84, 0.51, 0.15, 0.45, 0.41, 0.42, 0.54, 0.01, 0.58, 0.21, 0.67, 0.0, 0.09, 1.0, 0.25, 0.2, 0.85, 1.0, 0.3, 0.83, 0.09, 0.4, 0.49, 0.5);
    fillpatch(i++, "Hard Lead [SA]", 0.71, 0.75, 0.53, 0.18, 0.24, 1.0, 0.56, 0.52, 0.69, 0.19, 0.7, 1.0, 0.14, 0.65, 0.95, 0.07, 0.91, 1.0, 0.15, 0.84, 0.33, 0.0, 0.49, 0.5);
    fillpatch(i++, "Bubble", 0.0, 0.25, 0.43, 0.0, 0.71, 0.48, 0.23, 0.77, 0.8, 0.32, 0.63, 0.4, 0.18, 0.66, 0.14, 0.0, 0.38, 0.65, 0.16, 0.48, 0.5, 0.0, 0.67, 0.5);
    fillpatch(i++, "Monosynth", 0.62, 0.26, 0.51, 0.79, 0.35, 0.54, 0.64, 0.39, 0.51, 0.65, 0.0, 0.07, 0.52, 0.24, 0.84, 0.13, 0.3, 0.76, 0.21, 0.58, 0.3, 0.0, 0.36, 0.5);
    fillpatch(i++, "Moogcury Lite", 0.81, 1.0, 0.21, 0.78, 0.15, 0.35, 0.39, 0.17, 0.69, 0.4, 0.62, 0.0, 0.47, 0.19, 0.37, 0.0, 0.5, 0.2, 0.33, 0.38, 0.53, 0.0, 0.12, 0.5);
    fillpatch(i++, "Gangsta Whine", 0.0, 0.51, 0.52, 0.96, 0.44, 0.5, 0.41, 0.46, 0.5, 0.0, 0.0, 0.0, 0.0, 1.0, 0.25, 0.15, 0.5, 1.0, 0.32, 0.81, 0.49, 0.0, 0.83, 0.5);
    fillpatch(i++, "Higher Synth [ZF]", 0.48, 0.51, 0.22, 0.0, 0.0, 0.5, 0.5, 0.47, 0.73, 0.3, 0.8, 0.0, 0.1, 0.0, 0.07, 0.0, 0.42, 0.0, 0.22, 0.21, 0.59, 0.16, 0.98, 0.5);
    fillpatch(i++, "303 Saw Bass", 0.0, 0.51, 0.5, 0.83, 0.49, 0.5, 0.55, 0.75, 0.69, 0.35, 0.5, 0.0, 0.56, 0.0, 0.56, 0.0, 0.8, 1.0, 0.24, 0.26, 0.49, 0.0, 0.07, 0.5);
    fillpatch(i++, "303 Square Bass", 0.75, 0.51, 0.5, 0.83, 0.49, 0.5, 0.55, 0.75, 0.69, 0.35, 0.5, 0.14, 0.49, 0.0, 0.39, 0.0, 0.8, 1.0, 0.24, 0.26, 0.49, 0.0, 0.07, 0.5);
    fillpatch(i++, "Analog Bass", 1.0, 0.25, 0.2, 0.81, 0.19, 0.5, 0.3, 0.51, 0.85, 0.09, 0.0, 0.0, 0.88, 0.0, 0.21, 0.0, 0.5, 1.0, 0.46, 0.81, 0.5, 0.0, 0.27, 0.5);
    fillpatch(i++, "Analog Bass 2", 1.0, 0.25, 0.2, 0.72, 0.19, 0.86, 0.48, 0.43, 0.94, 0.0, 0.8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.61, 1.0, 0.32, 0.81, 0.5, 0.0, 0.27, 0.5);
    fillpatch(i++, "Low Pulses", 0.97, 0.26, 0.3, 0.0, 0.35, 0.5, 0.8, 0.4, 0.52, 0.0, 0.5, 0.0, 0.77, 0.0, 0.25, 0.0, 0.5, 1.0, 0.3, 0.81, 0.16, 0.0, 0.0, 0.5);
    fillpatch(i++, "Sine Infra-Bass", 0.0, 0.25, 0.5, 0.65, 0.35, 0.5, 0.33, 0.76, 0.53, 0.0, 0.5, 0.0, 0.3, 0.0, 0.25, 0.0, 0.55, 0.25, 0.3, 0.81, 0.52, 0.0, 0.14, 0.5);
    fillpatch(i++, "Wobble Bass [SA]", 1.0, 0.26, 0.22, 0.64, 0.82, 0.59, 0.72, 0.47, 0.34, 0.34, 0.82, 0.2, 0.69, 1.0, 0.15, 0.09, 0.5, 1.0, 0.07, 0.81, 0.46, 0.0, 0.24, 0.5);
    fillpatch(i++, "Squelch Bass", 1.0, 0.26, 0.22, 0.71, 0.35, 0.5, 0.67, 0.7, 0.26, 0.0, 0.5, 0.48, 0.69, 1.0, 0.15, 0.0, 0.5, 1.0, 0.07, 0.81, 0.46, 0.0, 0.24, 0.5);
    fillpatch(i++, "Rubber Bass [ZF]", 0.49, 0.25, 0.66, 0.81, 0.35, 0.5, 0.36, 0.15, 0.75, 0.2, 0.5, 0.0, 0.38, 0.0, 0.25, 0.0, 0.6, 1.0, 0.22, 0.19, 0.5, 0.0, 0.17, 0.5);
    fillpatch(i++, "Soft Pick Bass", 0.37, 0.51, 0.77, 0.71, 0.22, 0.5, 0.33, 0.47, 0.71, 0.16, 0.59, 0.0, 0.0, 0.0, 0.25, 0.04, 0.58, 0.0, 0.22, 0.15, 0.44, 0.33, 0.15, 0.5);
    fillpatch(i++, "Fretless Bass", 0.5, 0.51, 0.17, 0.8, 0.34, 0.5, 0.51, 0.0, 0.58, 0.0, 0.67, 0.0, 0.09, 0.0, 0.25, 0.2, 0.85, 0.0, 0.3, 0.81, 0.7, 0.0, 0.0, 0.5);
    fillpatch(i++, "Whistler", 0.23, 0.51, 0.38, 0.0, 0.35, 0.5, 0.33, 1.0, 0.5, 0.0, 0.5, 0.0, 0.29, 0.0, 0.25, 0.68, 0.39, 0.58, 0.36, 0.81, 0.64, 0.38, 0.92, 0.5);
    fillpatch(i++, "Very Soft Pad", 0.39, 0.51, 0.27, 0.38, 0.12, 0.5, 0.35, 0.78, 0.5, 0.0, 0.5, 0.0, 0.3, 0.0, 0.25, 0.35, 0.5, 0.8, 0.7, 0.81, 0.5, 0.0, 0.5, 0.5);
    fillpatch(i++, "Pizzicato", 0.0, 0.25, 0.5, 0.0, 0.35, 0.5, 0.23, 0.2, 0.75, 0.0, 0.5, 0.0, 0.22, 0.0, 0.25, 0.0, 0.47, 0.0, 0.3, 0.81, 0.5, 0.8, 0.5, 0.5);
    fillpatch(i++, "Synth Strings", 1.0, 0.51, 0.24, 0.0, 0.0, 0.35, 0.42, 0.26, 0.75, 0.14, 0.69, 0.0, 0.67, 0.55, 0.97, 0.82, 0.7, 1.0, 0.42, 0.84, 0.67, 0.3, 0.47, 0.5);
    fillpatch(i++, "Synth Strings 2", 0.75, 0.51, 0.29, 0.0, 0.49, 0.5, 0.55, 0.16, 0.69, 0.08, 0.2, 0.76, 0.29, 0.76, 1.0, 0.46, 0.8, 1.0, 0.39, 0.79, 0.27, 0.0, 0.68, 0.5);
    fillpatch(i++, "Leslie Organ", 0.0, 0.5, 0.53, 0.0, 0.13, 0.39, 0.38, 0.74, 0.54, 0.2, 0.0, 0.0, 0.55, 0.52, 0.31, 0.0, 0.17, 0.73, 0.28, 0.87, 0.24, 0.0, 0.29, 0.5);
    fillpatch(i++, "Click Organ", 0.5, 0.77, 0.52, 0.0, 0.35, 0.5, 0.44, 0.5, 0.65, 0.16, 0.0, 0.0, 0.0, 0.18, 0.0, 0.0, 0.75, 0.8, 0.0, 0.81, 0.49, 0.0, 0.44, 0.5);
    fillpatch(i++, "Hard Organ", 0.89, 0.91, 0.37, 0.0, 0.35, 0.5, 0.51, 0.62, 0.54, 0.0, 0.0, 0.0, 0.37, 0.0, 1.0, 0.04, 0.08, 0.72, 0.04, 0.77, 0.49, 0.0, 0.58, 0.5);
    fillpatch(i++, "Bass Clarinet", 1.0, 0.51, 0.51, 0.37, 0.0, 0.5, 0.51, 0.1, 0.5, 0.11, 0.5, 0.0, 0.0, 0.0, 0.25, 0.35, 0.65, 0.65, 0.32, 0.79, 0.49, 0.2, 0.35, 0.5);
    fillpatch(i++, "Trumpet", 0.0, 0.51, 0.51, 0.82, 0.06, 0.5, 0.57, 0.0, 0.32, 0.15, 0.5, 0.21, 0.15, 0.0, 0.25, 0.24, 0.6, 0.8, 0.1, 0.75, 0.55, 0.25, 0.69, 0.5);
    fillpatch(i++, "Soft Horn", 0.12, 0.9, 0.67, 0.0, 0.35, 0.5, 0.5, 0.21, 0.29, 0.12, 0.6, 0.0, 0.35, 0.36, 0.25, 0.08, 0.5, 1.0, 0.27, 0.83, 0.51, 0.1, 0.25, 0.5);
    fillpatch(i++, "Brass Section", 0.43, 0.76, 0.23, 0.0, 0.28, 0.36, 0.5, 0.0, 0.59, 0.0, 0.5, 0.24, 0.16, 0.91, 0.08, 0.17, 0.5, 0.8, 0.45, 0.81, 0.5, 0.0, 0.58, 0.5);
    fillpatch(i++, "Synth Brass", 0.4, 0.51, 0.25, 0.0, 0.3, 0.28, 0.39, 0.15, 0.75, 0.0, 0.5, 0.39, 0.3, 0.82, 0.25, 0.33, 0.74, 0.76, 0.41, 0.81, 0.47, 0.23, 0.5, 0.5);
    fillpatch(i++, "Detuned Syn Brass [ZF]", 0.68, 0.5, 0.93, 0.0, 0.31, 0.62, 0.26, 0.07, 0.85, 0.0, 0.66, 0.0, 0.83, 0.0, 0.05, 0.0, 0.75, 0.54, 0.32, 0.76, 0.37, 0.29, 0.56, 0.5);
    fillpatch(i++, "Power PWM", 1.0, 0.27, 0.22, 0.0, 0.35, 0.5, 0.82, 0.13, 0.75, 0.0, 0.0, 0.24, 0.3, 0.88, 0.34, 0.0, 0.5, 1.0, 0.48, 0.71, 0.37, 0.0, 0.35, 0.5);
    fillpatch(i++, "Water Velocity [SA]", 0.76, 0.51, 0.35, 0.0, 0.49, 0.5, 0.87, 0.67, 1.0, 0.32, 0.09, 0.95, 0.56, 0.72, 1.0, 0.04, 0.76, 0.11, 0.46, 0.88, 0.72, 0.0, 0.38, 0.5);
    fillpatch(i++, "Ghost [SA]", 0.75, 0.51, 0.24, 0.45, 0.16, 0.48, 0.38, 0.58, 0.75, 0.16, 0.81, 0.0, 0.3, 0.4, 0.31, 0.37, 0.5, 1.0, 0.54, 0.85, 0.83, 0.43, 0.46, 0.5);
    fillpatch(i++, "Soft E.Piano", 0.31, 0.51, 0.43, 0.0, 0.35, 0.5, 0.34, 0.26, 0.53, 0.0, 0.63, 0.0, 0.22, 0.0, 0.39, 0.0, 0.8, 0.0, 0.44, 0.81, 0.51, 0.0, 0.5, 0.5);
    fillpatch(i++, "Thumb Piano", 0.72, 0.82, 1.0, 0.0, 0.35, 0.5, 0.37, 0.47, 0.54, 0.0, 0.5, 0.0, 0.45, 0.0, 0.39, 0.0, 0.39, 0.0, 0.48, 0.81, 0.6, 0.0, 0.71, 0.5);
    fillpatch(i++, "Steel Drums [ZF]", 0.81, 0.76, 0.19, 0.0, 0.18, 0.7, 0.4, 0.3, 0.54, 0.17, 0.4, 0.0, 0.42, 0.23, 0.47, 0.12, 0.48, 0.0, 0.49, 0.53, 0.36, 0.34, 0.56, 0.5);

    fillpatch(58,  "Car Horn", 0.57, 0.49, 0.31, 0.0, 0.35, 0.5, 0.46, 0.0, 0.68, 0.0, 0.5, 0.46, 0.3, 1.0, 0.23, 0.3, 0.5, 1.0, 0.31, 1.0, 0.38, 0.0, 0.5, 0.5);
    fillpatch(59,  "Helicopter", 0.0, 0.25, 0.5, 0.0, 0.35, 0.5, 0.08, 0.36, 0.69, 1.0, 0.5, 1.0, 1.0, 0.0, 1.0, 0.96, 0.5, 1.0, 0.92, 0.97, 0.5, 1.0, 0.0, 0.5);
    fillpatch(60,  "Arctic Wind", 0.0, 0.25, 0.5, 0.0, 0.35, 0.5, 0.16, 0.85, 0.5, 0.28, 0.5, 0.37, 0.3, 0.0, 0.25, 0.89, 0.5, 1.0, 0.89, 0.24, 0.5, 1.0, 1.0, 0.5);
    fillpatch(61,  "Thip", 1.0, 0.37, 0.51, 0.0, 0.35, 0.5, 0.0, 1.0, 0.97, 0.0, 0.5, 0.02, 0.2, 0.0, 0.2, 0.0, 0.46, 0.0, 0.3, 0.81, 0.5, 0.78, 0.48, 0.5);
    fillpatch(62,  "Synth Tom", 0.0, 0.25, 0.5, 0.0, 0.76, 0.94, 0.3, 0.33, 0.76, 0.0, 0.68, 0.0, 0.59, 0.0, 0.59, 0.1, 0.5, 0.0, 0.5, 0.81, 0.5, 0.7, 0.0, 0.5);
    fillpatch(63,  "Squelchy Frog", 0.5, 0.41, 0.23, 0.45, 0.77, 0.0, 0.4, 0.65, 0.95, 0.0, 0.5, 0.33, 0.5, 0.0, 0.25, 0.0, 0.7, 0.65, 0.18, 0.32, 1.0, 0.0, 0.06, 0.5);

    //for testing...
    //fillpatch(0, "Monosynth", 0.62, 0.26, 0.51, 0.79, 0.35, 0.54, 0.64, 0.39, 0.51, 0.65, 0.0, 0.07, 0.52, 0.24, 0.84, 0.13, 0.3, 0.76, 0.21, 0.58, 0.3, 0.0, 0.36, 0.5);

    setProgram(0);
  end;

  if(audioMaster)
  begin
    setNumInputs(0);
    setNumOutputs(NOUTS);
    canProcessReplacing();
    isSynth();
    setUniqueID('MDAj');  ///
  end;

  //initialise...
  for(long v=0; v<NVOICES; v++)
  begin
    voice[v].dp   = voice[v].dp2   = 1.0;
    voice[v].saw  = voice[v].p     = voice[v].p2    = 0.0;
    voice[v].env  = voice[v].envd  = voice[v].envl  = 0.0;
    voice[v].fenv = voice[v].fenvd = voice[v].fenvl = 0.0;
    voice[v].f0   = voice[v].f1    = voice[v].f2    = 0.0;
    voice[v].note = 0;
  end;
  notes[0] = EVENTS_DONE;
  lfo = modwhl = filtwhl = press = fzip = 0.0;
  rezwhl = pbend = ipbend = 1.0;
  volume = 0.0005;
  K = fMode = lastnote = sustain = activevoices = 0;
  noise = 22222;

  update();
  suspend();
*)
end;

function TJX10DataModule.VSTModuleOutputProperties(Sender: TObject; var vLabel,
  shortLabel: string; var SpeakerArrangement: TVstSpeakerArrangementType;
  var Flags: TChannelPropertyFlags): Integer;
begin
(*
 if (index < NOUTS) then
  begin
    sprintf(properties->label, "JX10", index + 1);
    properties->flags = kVstPinIsActive;
    if(index<2) properties->flags |= kVstPinIsStereo; //make channel 1+2 stereo
    return true;
  end;
 result := False;
*)
end;

procedure TJX10DataModule.VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  event, frame, frames, v     : Integer;
  o, e, vib, pwm, pb, ipb, gl : Single;
  x, y, hpf, min, w, ww       : Single;
  ff, fe, fq, fx, fz          : Single;
  k                           : Integer;
  r                           : Cardinal;
begin
(*
  float* out1 = outputs[0];
  float* out2 = outputs[1];
  long event=0, frame=0, frames, v;
  pb   := pbend;
  ipb  := ipbend;
  gl   := fGlide;
  hpf  := 0.997;
  min  := 1;
  w    := 0;
  ww   := fNoiseMix;
  fe   := fFilterEnv;
  fq   := fFilterQ * rezwhl;
  fx   := 1.97 - 0.85 * fq;
  fz   := fzip;
  k    := K;

  vib := sin(lfo);
  ff  := fFilterFreq + filtwhl + (fFilterLFO + press) * vib; //have to do again here as way that
  pwm := 1 + vib * (modwhl + fPWDepth);           //below triggers on k was too cheap!
  vib := 1 + vib * (modwhl + fVibrato);

  if ((fActiveVoices > 0) or (notes[event] < sampleFrames))
   begin
    while (frame < sampleFrames) do
     begin
      frames := notes[event++];
      if (frames > sampleFrames)
       then frames := sampleFrames;
      frames := frames - frame;
      frame  := frame + frames;

      while (--frames >= 0) do
       begin
        VOICE *V = voice;
        o = 0.0;
        
        noise = (noise * 196314165) + 907633515;
        r = (noise & 0x7FFFFF) + 0x40000000; //generate noise + fast convert to float
        w = *(float * )&r;
        w = ww * (w - 3.0);

        if(--k<0) then
         begin
          lfo += fDeltaLFO;
          if(lfo>PI) lfo -= TWOPI;
          vib = (float)sin(lfo);
          ff = fFilterFreq + filtwhl + (fFilterLFO + press) * vib;
          pwm = 1.0 + vib * (modwhl + fPWDepth);
          vib = 1.0 + vib * (modwhl + fVibrato);
          k = KMAX;
         end;

        for(v=0; v<NVOICES; v++)  //for each voice
        begin 
          e = V->env;
          if(e > SILENCE)
          begin //Sinc-Loop Oscillator
            x = V->p + V->dp;
            if(x > min) 
            begin
              if(x > V->pmax) 
              begin 
                x = V->pmax + V->pmax - x;  
                V->dp = -V->dp; 
              end;
              V->p = x;
              x = V->sin0 * V->sinx - V->sin1; //sine osc
              V->sin1 = V->sin0;
              V->sin0 = x;
              x = x / V->p;
            end;
            else
            begin 
              V->p = x = - x;  
              V->dp = V->period * vib * pb; //set period for next cycle
              V->pmax = (float)trunc(0.5 + V->dp) - 0.5;
              V->dc = -0.5 * V->lev / V->pmax;
              V->pmax *= PI;
              V->dp = V->pmax / V->dp;
              V->sin0 = V->lev * (float)sin(x);
              V->sin1 = V->lev * (float)sin(x - V->dp);
              V->sinx = 2.0 * (float)cos(V->dp);
              if(x*x > .1) x = V->sin0 / x; else x = V->lev; //was 0.01;
            end;
            
            y = V->p2 + V->dp2; //osc2
            if(y > min) 
            begin 
              if(y > V->pmax2) 
              begin 
                y = V->pmax2 + V->pmax2 - y;
                V->dp2 = -V->dp2; 
              end;
              V->p2 = y;
              y = V->sin02 * V->sinx2 - V->sin12;
              V->sin12 = V->sin02;
              V->sin02 = y;
              y = y / V->p2;
            end;
            else
            begin
              V->p2 = y = - y;  
              V->dp2 = V->period * V->fDetune * pwm * pb;
              V->pmax2 = (float)trunc(0.5 + V->dp2) - 0.5;
              V->dc2 = -0.5 * V->lev2 / V->pmax2;
              V->pmax2 *= PI;
              V->dp2 = V->pmax2 / V->dp2;
              V->sin02 = V->lev2 * (float)sin(y);
              V->sin12 = V->lev2 * (float)sin(y - V->dp2);
              V->sinx2 = 2.0 * (float)cos(V->dp2);
              if(y*y > .1) y = V->sin02 / y; else y = V->lev2;
            end;
            V->saw = V->saw * hpf + V->dc + x - V->dc2 - y;  //integrated sinc = saw
            x = V->saw + w;
            V->env += V->envd * (V->envl - V->env);

            if(k==KMAX) //filter freq update at LFO rate
            begin
              if((V->env+V->envl)>3.0) begin V->envd=fDecay; V->envl=fSustain; end; //envelopes
              V->fenv += V->fenvd * (V->fenvl - V->fenv);
              if((V->fenv+V->fenvl)>3.0) begin V->fenvd=fdec; V->fenvl=fsus; end;

              fz += 0.005 * (ff - fz); //filter zipper noise filter
              y = V->fc * (float)exp(fz + fe * V->fenv) * ipb; //filter cutoff
              if(y<0.005) y=0.005;
              V->ff = y;
 
              V->period += gl * (V->target - V->period); //fGlide
              if(V->target < V->period) V->period += gl * (V->target - V->period);
            end;

            if(V->ff > fx) V->ff = fx; //stability limit
            
            V->f0 += V->ff * V->f1; //state-variable filter
            V->f1 -= V->ff * (V->f0 + fq * V->f1 - x - V->f2);
            V->f1 -= 0.2 * V->f1 * V->f1 * V->f1; //soft limit

            V->f2 = x;
            
            o += V->env * V->f0;
          end;
          V++;
        end;

        *out1++ = o;
        *out2++ = o;
      end;

      if (frame < sampleFrames)
      begin
        long note = notes[event++];
        long vel  = notes[event++];
        noteOn(note, vel);
      end;
    end;
  
    activevoices = NVOICES;
    for(v=0; v<NVOICES; v++)
    begin
      if(voice[v].env<SILENCE)  //choke voices
      begin
        voice[v].env = voice[v].envl = 0.0;
        voice[v].f0 = voice[v].f1 = voice[v].f2 = 0.0;
        activevoices--;
      end;
    end;
  end;
  else //empty block
  begin
    while(--sampleFrames >= 0)
    begin
      *out1++ = 0.0;
      *out2++ = 0.0;
    end;
  end;
  notes[0] = EVENTS_DONE;  //mark events buffer as done
  fzip = fz;
  K = k;
*)
end;

procedure TJX10DataModule.VSTModuleResume(Sender: TObject);
begin
 // DECLARE_VST_DEPRECATED (wantEvents) ();
end;

procedure TJX10DataModule.VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
begin
// fDeltaLFO = fLFOHz * (TWOPI * KMAX) / SampleRate;
end;

end.

(*
mdaJX10Program::mdaJX10Program()
begin
  Parameter[0] := 0.00; //OSC Mix
  Parameter[1] := 0.25; //OSC fTune
  Parameter[2] := 0.50; //OSC Fine

  Parameter[3] := 0.00; //OSC fMode
  Parameter[4] := 0.35; //OSC Rate
  Parameter[5] := 0.50; //OSC Bend

  Parameter[6] := 1.00; //VCF Freq
  Parameter[7] := 0.15; //VCF Reso
  Parameter[8] := 0.75; //VCF <Env

  Parameter[9] := 0.00; //VCF <LFO
  Parameter[10] = 0.50; //VCF <Vel
  Parameter[11] = 0.00; //VCF fAttack

  Parameter[12] = 0.30; //VCF fDecay
  Parameter[13] = 0.00; //VCF fSustain
  Parameter[14] = 0.25; //VCF fRelease

  Parameter[15] = 0.00; //ENV fAttack
  Parameter[16] = 0.50; //ENV fDecay
  Parameter[17] = 1.00; //ENV fSustain
  
  Parameter[18] = 0.30; //ENV fRelease
  Parameter[19] = 0.81; //LFO Rate
  Parameter[20] = 0.50; //fVibrato

  Parameter[21] = 0.00; //Noise   - not present in original patches
  Parameter[22] = 0.50; //Octave
  Parameter[23] = 0.50; //Tuning
  strcpy (name, "Empty Patch");
end;

void mdaJX10::suspend() //Used by Logic (have note off code in 3 places now...)
begin
  for(long v=0; v<NVOICES; v++)
  begin
    voice[v].envl = voice[v].env = 0.0; 
    voice[v].envd = 0.99;
    voice[v].note = 0;
    voice[v].f0 = voice[v].f1 = voice[v].f2 = 0.0;
  end;
end;

void mdaJX10::setProgram(VstInt32 program)
begin
  long i;

  mdaJX10Program *p = &programs[program];
  curProgram = program;
  for(i=0; i<NPARAMS; i++) Parameter[i] = p->Parameter[i];
  update();
end; //may want all notes off here - but this stops use of patches as snapshots!


void mdaJX10::setParameter(VstInt32 index, float value)
begin
  mdaJX10Program *p = &programs[curProgram];
  Parameter[index] = p->Parameter[index] = value;
  update();

  ///if(editor) editor->postUpdate();
end;


void mdaJX10::fillpatch(long p, char *name,
                      float p0,  float p1,  float p2,  float p3,  float p4,  float p5, 
                      float p6,  float p7,  float p8,  float p9,  float p10, float p11,
                      float p12, float p13, float p14, float p15, float p16, float p17, 
                      float p18, float p19, float p20, float p21, float p22, float p23)
begin
  strcpy(programs[p].name, name);
  programs[p].Parameter[0] := p0;   programs[p].Parameter[1] := p1;
  programs[p].Parameter[2] := p2;   programs[p].Parameter[3] := p3;
  programs[p].Parameter[4] := p4;   programs[p].Parameter[5] := p5;
  programs[p].Parameter[6] := p6;   programs[p].Parameter[7] := p7;
  programs[p].Parameter[8] := p8;   programs[p].Parameter[9] := p9;
  programs[p].Parameter[10] = p10;  programs[p].Parameter[11] = p11;
  programs[p].Parameter[12] = p12;  programs[p].Parameter[13] = p13;
  programs[p].Parameter[14] = p14;  programs[p].Parameter[15] = p15;
  programs[p].Parameter[16] = p16;  programs[p].Parameter[17] = p17;
  programs[p].Parameter[18] = p18;  programs[p].Parameter[19] = p19;
  programs[p].Parameter[20] = p20;  programs[p].Parameter[21] = p21;
  programs[p].Parameter[22] = p22;  programs[p].Parameter[23] = p23;  
end;


float mdaJX10::getParameter(VstInt32 index)     begin return Parameter[index]; end;
void  mdaJX10::setProgramName(char *name)   begin strcpy(programs[curProgram].name, name); end;
void  mdaJX10::getProgramName(char *name)   begin strcpy(name, programs[curProgram].name); end;
void  mdaJX10::setBlockSize(VstInt32 blockSize) begin  AudioEffectX::setBlockSize(blockSize); end;

bool mdaJX10::getProgramNameIndexed(VstInt32 category, VstInt32 index, char* text)
begin
  if(index<NPROGS)
  begin
    strcpy(text, programs[index].name);
    return true;
  end;
  return false;
end;


bool mdaJX10::copyProgram(VstInt32 destination)
begin
  if(destination<NPROGS)
  begin
    programs[destination] = programs[curProgram];
    return true;
  end;
  return false;
end;


void mdaJX10::getParameterDisplay(VstInt32 index, char *text)
begin
  char string[16];
  
  switch(index)
  begin
    case  0: sprintf(string, "%4.0f:%2.0f", 100.0-50.0*Parameter[index], 50.0*Parameter[index]); break;
    case  1: sprintf(string, "%.0f", fSemi); break;
    case  2: sprintf(string, "%.1f", fCent); break; 
    case  3: switch(fMode)
             begin case  0:
               case  1: strcpy(string, "POLY    "); break;
               case  2: strcpy(string, "P-LEGATO"); break;
               case  3: strcpy(string, "P-GLIDE "); break;
               case  4:
               case  5: strcpy(string, "MONO    "); break;
               case  6: strcpy(string, "M-LEGATO"); break;
               default: strcpy(string, "M-GLIDE "); break; end; break;
    case  5: sprintf(string, "%.2f", fGlidedisp); break;
    case  6: sprintf(string, "%.1f", 100.0 * Parameter[index]); break;
    case  8:
    case 23: sprintf(string, "%.1f", 200.0 * Parameter[index] - 100.0); break;
    case 10: if(Parameter[index]<0.05) strcpy(string, "   OFF  ");
               else sprintf(string, "%.0f", 200.0 * Parameter[index] - 100.0); break;
    case 19: sprintf(string, "%.3f", fLFOHz); break;
    case 20: if(Parameter[index]<0.5) sprintf(string, "PWM %3.0f", 100.0 - 200.0 * Parameter[index]);
               else sprintf(string, "%7.0f", 200.0 * Parameter[index] - 100.0); break;
    case 22: sprintf(string, "%d", (long)(Parameter[index] * 4.9) - 2); break;
    default: sprintf(string, "%.0f", 100.0 * Parameter[index]);
  end;
  string[8] = 0;
  strcpy(text, (char * )string);
end;


void mdaJX10::getParameterLabel(VstInt32 index, char *label)
begin
  switch(index)
  begin
    case  1: 
    case  5: strcpy(label, "   semi "); break;
    case  2:
    case 23: strcpy(label, "   cent "); break;
    case  3: 
    case 22: strcpy(label, "        "); break;
    case 19: strcpy(label, "     Hz "); break;
    default: strcpy(label, "      % ");
  end;
end;

void mdaJX10::noteOn(long note, long velocity)
begin
  float p, l=100.0; //louder than any envelope!
  long  v=0, tmp, held=0;
  
  if(velocity>0) //note on
  begin
    if(fVelOff) velocity = 80;
    
    if(fMode & 4) //monophonic
    begin
      if(voice[0].note > 0) //legato pitch change
      begin
        for(tmp=(NVOICES-1); tmp>0; tmp--)  //queue any held notes
        begin
          voice[tmp].note = voice[tmp - 1].note;
        end;
        p = fTune * (float)exp(-0.05776226505 * ((double)note + ANALOG * (double)v));
        while(p<3.0 || (p * fDetune)<3.0) p += p;
        voice[v].target = p;
        if((fMode & 2)==0) voice[v].period = p;
        voice[v].fc = (float)exp(fFilterVel * (float)(velocity - 64)) / p;
        voice[v].env += SILENCE + SILENCE; ///was missed out below if returned?
        voice[v].note = note;
        return;
      end;
    end;
    else //polyphonic 
    begin
      for(tmp=0; tmp<NVOICES; tmp++)  //replace quietest voice not in attack
      begin
        if(voice[tmp].note > 0) held++;
        if(voice[tmp].env<l && voice[tmp].envl<2.0) begin l=voice[tmp].env;  v=tmp; end;
      end;
    end;  
    p = fTune * (float)exp(-0.05776226505 * ((double)note + ANALOG * (double)v));
    while(p<3.0 || (p * fDetune)<3.0) p += p;
    voice[v].target = p;
    voice[v].fDetune = fDetune;
  
    tmp = 0;
    if(fMode & 2)
    begin
      if((fMode & 1) || held) tmp = note - lastnote; //fGlide
    end;
    voice[v].period = p * (float)pow(1.059463094359, (double)tmp - fGlidedisp);
    if(voice[v].period<3.0) voice[v].period = 3.0; //limit min period

    voice[v].note = lastnote = note;

    voice[v].fc = (float)exp(fFilterVel * (float)(velocity - 64)) / p; //filter tracking

    voice[v].lev = fVolTrim * volume * (0.004 * (float)((velocity + 64) * (velocity + 64)) - 8.0);
    voice[v].lev2 = voice[v].lev * fOscMix;

    if(Parameter[20]<0.5) //force 180 deg phase difference for PWM
    begin
      if(voice[v].dp>0.0)
      begin
        p = voice[v].pmax + voice[v].pmax - voice[v].p;
        voice[v].dp2 = -voice[v].dp;
      end;
      else
      begin
        p = voice[v].p;
        voice[v].dp2 = voice[v].dp;
      end;
      voice[v].p2 = voice[v].pmax2 = p + PI * voice[v].period;

      voice[v].dc2 = 0.0;
      voice[v].sin02 = voice[v].sin12 = voice[v].sinx2 = 0.0;
    end;

    if(fMode & 4) //monophonic retriggering
    begin
      voice[v].env += SILENCE + SILENCE;
    end;
    else
    begin
      //if(Parameter[15] < 0.28) 
      //begin
      //  voice[v].f0 = voice[v].f1 = voice[v].f2 = 0.0; //reset filter
      //  voice[v].env = SILENCE + SILENCE;
      //  voice[v].fenv = 0.0;
      //end;
      //else 
        voice[v].env += SILENCE + SILENCE; //anti-glitching trick
    end;
    voice[v].envl  := 2;
    voice[v].envd  := fAttack;
    voice[v].fenvl := 2;
    voice[v].fenvd := fatt;
  end;
  else //note off
  begin
    if((fMode & 4) && (voice[0].note==note)) //monophonic (and current note)
    begin
      for(v=(NVOICES-1); v>0; v--)
      begin
        if(voice[v].note>0) held = v; //any other notes queued?
      end;
      if(held>0)
      begin
        voice[v].note = voice[held].note;
        voice[held].note = 0;
        
        p = fTune * (float)exp(-0.05776226505 * ((double)voice[v].note + ANALOG * (double)v));
        while(p<3.0 || (p * fDetune)<3.0) p += p;
        voice[v].target = p;
        if((fMode & 2)==0) voice[v].period = p;
        voice[v].fc = 1.0 / p;
      end;
      else
      begin
        voice[v].envl  = 0.0;
        voice[v].envd  = fRelease;
        voice[v].fenvl = 0.0;
        voice[v].fenvd = frel;
        voice[v].note  = 0;
      end;
    end;
    else //polyphonic
    begin
      for(v=0; v<NVOICES; v++) if(voice[v].note==note) //any voices playing that note?
      begin
        if(sustain==0)
        begin
          voice[v].envl  = 0.0;
          voice[v].envd  = fRelease;
          voice[v].fenvl = 0.0;
          voice[v].fenvd = frel;
          voice[v].note  = 0;
        end;
        else voice[v].note = SUSTAIN;
      end;
    end;
  end;
end;


VstInt32 mdaJX10::processEvents(VstEvents* ev)
begin
  long npos=0;

  for (long i=0; i<ev->numEvents; i++)
  begin
    if((ev->events[i])->type != kVstMidiType) continue;
    VstMidiEvent* event = (VstMidiEvent* )ev->events[i];
    char* midiData = event->midiData;
    
    switch(midiData[0] & 0xf0) //status byte (all channels)
    begin
      case 0x80: //note off
        notes[npos++] = event->deltaFrames; //delta
        notes[npos++] = midiData[1] & 0x7F; //note
        notes[npos++] = 0;                  //vel
        break;

      case 0x90: //note on
        notes[npos++] = event->deltaFrames; //delta
        notes[npos++] = midiData[1] & 0x7F; //note
        notes[npos++] = midiData[2] & 0x7F; //vel
        break;

      case 0xB0: //controller
        switch(midiData[1])
        begin
          case 0x01:  //mod wheel
            modwhl = 0.000005 * (float)(midiData[2] * midiData[2]);
            break;
          case 0x02:  //filter +
          case 0x4A:
            filtwhl = 0.02 * (float)(midiData[2]);
            break;
          case 0x03:  //filter -
            filtwhl = -0.03 * (float)(midiData[2]);
            break;

          case 0x07:  //volume
            volume = 0.00000005 * (float)(midiData[2] * midiData[2]);
            break;

          case 0x10:  //resonance
          case 0x47:
            rezwhl = 0.0065 * (float)(154 - midiData[2]);
            break;

          case 0x40:  //sustain
            sustain = midiData[2] & 0x40;
            if(sustain==0)
            begin
              notes[npos++] = event->deltaFrames;
              notes[npos++] = SUSTAIN; //end all sustained notes
              notes[npos++] = 0;
            end;
            break;

          default:  //all notes off
            if(midiData[1]>0x7A) 
            begin  
              for(long v=0; v<NVOICES; v++) 
              begin
                voice[v].envl = voice[v].env = 0.0; 
                voice[v].envd = 0.99;
                voice[v].note = 0;
                //could probably reset some more stuff here for safety!
              end;
              sustain = 0;
            end;
            break;
        end;
        break;

      case 0xC0: //program change
        if(midiData[1]<NPROGS) setProgram(midiData[1]);
        break;

      case 0xD0: //channel aftertouch
        press = 0.00001 * (float)(midiData[1] * midiData[1]);
        break;
      
      case 0xE0: //pitch bend
        ipbend = (float)exp(0.000014102 * (double)(midiData[1] + 128 * midiData[2] - 8192));
        pbend = 1.0 / ipbend;
        break;
      
      default: break;
    end;

    if(npos>EVENTBUFFER) npos -= 3; //discard events if buffer full!!
    event++;
  end;
  notes[npos] = EVENTS_DONE;
  return 1;
end;
*)
