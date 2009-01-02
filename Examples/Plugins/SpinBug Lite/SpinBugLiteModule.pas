unit SpinBugLiteModule;

{$I DAV_Compiler.INC}

interface

uses
  {$IFDEF FPC} LCLIntf, LMessages, {$ELSE} FastMove, Windows, Messages, {$ENDIF}
  SysUtils, Classes, Forms, DAV_Common, DAV_DspPolyphaseHilbert, DAV_VSTModule,
  DAV_DSPLFO;

type
  TSpinBugLiteModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender : TObject);
    procedure VSTEditOpen(Sender : TObject; var GUI : TForm; ParentWindow : Cardinal);
    procedure VSTModuleProcessStereoA(const Inputs, Outputs : TDAVArrayOfSingleDynArray; const SampleFrames : Integer);
    procedure VSTModuleProcessStereoB(const Inputs, Outputs : TDAVArrayOfSingleDynArray; const SampleFrames : Integer);
    procedure VSTModuleProcessStereoC(const Inputs, Outputs : TDAVArrayOfSingleDynArray; const SampleFrames : Integer);
    procedure VSTModuleProcessStereoD(const Inputs, Outputs : TDAVArrayOfSingleDynArray; const SampleFrames : Integer);
    procedure VSTModuleProcessMono(const Inputs, Outputs : TDAVArrayOfSingleDynArray; const SampleFrames : Integer);
    procedure VSTModuleProcessMonoL(const Inputs, Outputs : TDAVArrayOfSingleDynArray; const SampleFrames : Integer);
    procedure VSTModuleProcessMonoR(const Inputs, Outputs : TDAVArrayOfSingleDynArray; const SampleFrames : Integer);
    procedure VSTModuleProcessMS(const Inputs, Outputs : TDAVArrayOfSingleDynArray; const SampleFrames : Integer);
    procedure VSTModuleProcessSpecial(const Inputs, Outputs : TDAVArrayOfSingleDynArray; const SampleFrames : Integer);
    procedure VSTModuleProcessOldOne(const Inputs, Outputs : TDAVArrayOfSingleDynArray; const SampleFrames : Integer);
    procedure VSTModuleSampleRateChange(Sender : TObject; const SampleRate : Single);
    procedure SBMCoefficientsDisplay(Sender : TObject; const Index : Integer; var PreDefined : String);
    procedure SBMCoefficientsChange(Sender : TObject; const Index : Integer; var Value : Single);
    procedure SBMProcessTypeDisplay(Sender : TObject; const Index : Integer; var PreDefined : String);
    procedure SBMProcessTypeChange(Sender : TObject; const Index : Integer; var Value : Single);
    procedure SBMLFOSpeedChange(Sender : TObject; const Index : Integer; var Value : Single);
    procedure SBMTBWChange(Sender : TObject; const Index : Integer; var Value : Single);
    procedure VSTModuleClose(Sender: TObject);
  private
    FHilbert   : array[0..1] of TPhaseHalfPi32;
    FSineLFO   : array[0..1] of TLFOSine;
    FTBW       : Single;
  public
    property BasicSineLFO: TLFOSine read FSineLFO[0];
    property AdditionalSineLFO: TLFOSine read FSineLFO[1];
  end;

implementation

{$R *.dfm}

uses
  SpinBugLiteGUI, Math;

procedure TSpinBugLiteModule.VSTModuleOpen(Sender : TObject);
begin
 FTBW := 0.01;

 FHilbert[0] := TPhaseHalfPi32.Create;
 FHilbert[1] := TPhaseHalfPi32.Create;
 FSineLFO[0] := TLFOSine.Create;
 FSineLFO[1] := TLFOSine.Create;

 Parameter[0] := 8;
 OnProcess := VSTModuleProcessMono;
 OnProcessReplacing := VSTModuleProcessMono;

 if assigned(Programs) then
  try
   // default preset
   with programs[0] do
    begin
     Parameter[0] := 8;
     Parameter[1] := 5;
     Parameter[2] := 0.5;
     Parameter[3] := 0.01;
    end;

   // preset 1
   with programs[1] do
    begin
     Parameter[0] := 32;
     Parameter[1] := 3;
     Parameter[2] := 0.5;
     Parameter[3] := 0.01;
    end;

   // preset 2
   with programs[2] do
    begin
     Parameter[0] := 7;
     Parameter[1] := 2;
     Parameter[2] := 1.3;
     Parameter[3] := 0.01;
    end;

   // preset 3
   with programs[3] do
    begin
     Parameter[0] := 12;
     Parameter[1] := 4;
     Parameter[2] := 0.8;
     Parameter[3] := 0.01;
    end;

   // preset 4
   with programs[4] do
    begin
     Parameter[0] := 16;
     Parameter[1] := 9;
     Parameter[2] := 1;
     Parameter[3] := 0.01;
    end;

   // preset 5
   with programs[5] do
    begin
     Parameter[0] := 8;
     Parameter[1] := 10;
     Parameter[2] := 1.7;
     Parameter[3] := 0.01;
    end;
  except
  end;

 // default parameters
 Parameter[0] := 8;
 Parameter[1] := 5;
 Parameter[2] := 0.6;
 Parameter[3] := 0.01;
end;

procedure TSpinBugLiteModule.VSTModuleClose(Sender: TObject);
begin
 if Assigned(FHilbert[0]) then FreeAndNil(FHilbert[0]);
 if Assigned(FHilbert[1]) then FreeAndNil(FHilbert[1]);
 if Assigned(FSineLFO[0]) then FreeAndNil(FSineLFO[0]);
 if Assigned(FSineLFO[1]) then FreeAndNil(FSineLFO[1]);
end;

procedure TSpinBugLiteModule.VSTEditOpen(Sender : TObject;
  var GUI : TForm; ParentWindow : Cardinal);
begin
 GUI := TFmSpinBugLite.Create(Self);
end;

procedure TSpinBugLiteModule.VSTModuleProcessStereoA(
  const Inputs, Outputs : TDAVArrayOfSingleDynArray; const SampleFrames : Integer);
var
  i      : Integer;
  a1, b1 : Single;
  a2, b2 : Single;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   FHilbert[0].ProcessHilbertSample(Inputs[0, i], a1, b1);
   FHilbert[1].ProcessHilbertSample(Inputs[1, i], a2, b2);
   a1 := a1 * BasicSineLFO.Cosine;
   b1 := b1 * BasicSineLFO.Sine;
   a2 := a2 * BasicSineLFO.Cosine;
   b2 := b2 * BasicSineLFO.Sine;
   BasicSineLFO.CalculateNextSample;
   Outputs[0, i] := a1 + b1 + Inputs[0, i];
   Outputs[1, i] := a1 - b1 + Inputs[0, i];
   Outputs[0, i] := 0.25 * (Outputs[0, i] + a2 - b2 + Inputs[1, i]);
   Outputs[1, i] := 0.25 * (Outputs[1, i] + a2 + b2 + Inputs[1, i]);
  end;
end;

procedure TSpinBugLiteModule.VSTModuleProcessStereoB(
  const Inputs, Outputs : TDAVArrayOfSingleDynArray; const SampleFrames : Integer);
var
  i      : Integer;
  a1, b1 : Single;
  a2, b2 : Single;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   FHilbert[0].ProcessHilbertSample(Inputs[0, i], a1, b1);
   FHilbert[1].ProcessHilbertSample(Inputs[1, i], a2, b2);
   a1 := a1 * BasicSineLFO.Cosine;
   b1 := b1 * BasicSineLFO.Sine;
   a2 := a2 * BasicSineLFO.Cosine;
   b2 := b2 * BasicSineLFO.Sine;
   BasicSineLFO.CalculateNextSample;
   Outputs[0, i] := a1 + b1 + Inputs[0, i];
   Outputs[1, i] := a1 - b1 + Inputs[0, i];
   Outputs[0, i] := 0.25 * (Outputs[0, i] + a2 + b2 + Inputs[1, i]);
   Outputs[1, i] := 0.25 * (Outputs[1, i] + a2 - b2 + Inputs[1, i]);
  end;
end;

procedure TSpinBugLiteModule.VSTModuleProcessStereoC(
  const Inputs, Outputs : TDAVArrayOfSingleDynArray; const SampleFrames : Integer);
var
  i      : Integer;
  a1, b1 : Single;
  a2, b2 : Single;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   FHilbert[0].ProcessHilbertSample(Inputs[0, i], a1, b1);
   FHilbert[1].ProcessHilbertSample(Inputs[1, i], a2, b2);
   a1 := a1 * BasicSineLFO.Cosine;
   b1 := b1 * BasicSineLFO.Sine;
   a2 := a2 * BasicSineLFO.Cosine;
   b2 := b2 * BasicSineLFO.Sine;
   BasicSineLFO.CalculateNextSample;
   Outputs[0, i] := 0.5 * (a1 + b1 + Inputs[0, i]);
   Outputs[1, i] := 0.5 * (a2 - b2 + Inputs[1, i]);
  end;
end;

procedure TSpinBugLiteModule.VSTModuleProcessStereoD(
  const Inputs, Outputs : TDAVArrayOfSingleDynArray; const SampleFrames : Integer);
var
  i      : Integer;
  s, c   : Double;
  a1, b1 : Single;
  a2, b2 : Single;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   s := BasicSineLFO.Sine;
   c := 1 - 2 * s * s;
   BasicSineLFO.CalculateNextSample;
   FHilbert[0].ProcessHilbertSample(Inputs[0, i], a1, b1);
   FHilbert[1].ProcessHilbertSample(Inputs[1, i], a2, b2);
   a1 := a1 * c;
   b1 := b1 * s;
   a2 := a2 * c;
   b2 := b2 * s;
   Outputs[0, i] := a1 + b1 + Inputs[0, i];
   Outputs[1, i] := a1 - b1 + Inputs[0, i];
   Outputs[0, i] := 0.25 * (Outputs[0, i] + a2 + b2 + Inputs[1, i]);
   Outputs[1, i] := 0.25 * (Outputs[1, i] + a2 - b2 + Inputs[1, i]);
  end;
end;

procedure TSpinBugLiteModule.VSTModuleProcessMono(
  const Inputs, Outputs : TDAVArrayOfSingleDynArray; const SampleFrames : Integer);
var
  i    : Integer;
  a, b : Single;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   FHilbert[0].ProcessHilbertSample(0.5 * (Inputs[0, i] + Inputs[1, i]), a, b);
   a := a * BasicSineLFO.Sine;
   b := b * BasicSineLFO.Cosine;
   BasicSineLFO.CalculateNextSample;
   Outputs[0, i] := 0.5 * (a + b + Inputs[0, i]);
   Outputs[1, i] := 0.5 * (a - b + Inputs[1, i]);
  end;
end;

procedure TSpinBugLiteModule.VSTModuleProcessMonoL(
  const Inputs, Outputs : TDAVArrayOfSingleDynArray; const SampleFrames : Integer);
var
  i    : Integer;
  a, b : Single;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   FHilbert[0].ProcessHilbertSample(Inputs[0, i], a, b);
   a := a * BasicSineLFO.Sine;
   b := b * BasicSineLFO.Cosine;
   BasicSineLFO.CalculateNextSample;
   Outputs[0, i] := 0.5 * (a + b + Inputs[0, i]);
   Outputs[1, i] := 0.5 * (a - b + Inputs[0, i]);
  end;
end;

procedure TSpinBugLiteModule.VSTModuleProcessMonoR(
  const Inputs, Outputs : TDAVArrayOfSingleDynArray; const SampleFrames : Integer);
var
  i    : Integer;
  a, b : Single;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   FHilbert[0].ProcessHilbertSample(Inputs[1, i], a, b);
   a := a * BasicSineLFO.Sine;
   b := b * BasicSineLFO.Cosine;
   BasicSineLFO.CalculateNextSample;
   Outputs[0, i] := 0.5 * (a + b + Inputs[0, i]);
   Outputs[1, i] := 0.5 * (a - b + Inputs[0, i]);
  end;
end;

procedure TSpinBugLiteModule.VSTModuleProcessMS(
  const Inputs, Outputs : TDAVArrayOfSingleDynArray; const SampleFrames : Integer);
var
  i      : Integer;
  a1, b1 : Single;
  a2, b2 : Single;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   FHilbert[0].ProcessHilbertSample(0.5 * (Inputs[0, i] + Inputs[1, i]), a1, b1);
   FHilbert[1].ProcessHilbertSample(0.5 * (Inputs[0, i] - Inputs[1, i]), a2, b2);
   a1 := a1 * BasicSineLFO.Sine;
   b1 := b1 * BasicSineLFO.Cosine;
   a2 := a2 * BasicSineLFO.Sine;
   b2 := b2 * BasicSineLFO.Cosine;
   BasicSineLFO.CalculateNextSample;
   Outputs[0, i] := a1 + b1 + Inputs[0, i];
   Outputs[1, i] := a1 - b1 + Inputs[0, i];
   Outputs[0, i] := 0.25 * (Outputs[0, i] + a2 + b2 + Inputs[1, i]);
   Outputs[1, i] := 0.25 * (Outputs[1, i] + a2 - b2 + Inputs[1, i]);
  end;
end;

procedure TSpinBugLiteModule.VSTModuleProcessSpecial(
  const Inputs, Outputs : TDAVArrayOfSingleDynArray; const SampleFrames : Integer);
var
  i      : Integer;
  a1, b1 : Single;
  a2, b2 : Single;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   FHilbert[0].ProcessHilbertSample(Inputs[0, i], a1, b1);
   FHilbert[0].ProcessHilbertSample(-Inputs[1, i], a2, b2);
   a1 := a1 * BasicSineLFO.Sine;
   b1 := b1 * BasicSineLFO.Cosine;
   a2 := a2 * AdditionalSineLFO.Sine;
   b2 := b2 * AdditionalSineLFO.Cosine;
   BasicSineLFO.CalculateNextSample;
   AdditionalSineLFO.CalculateNextSample;
   Outputs[0, i] := a1 + b1 + Inputs[0, i];
   Outputs[1, i] := a1 - b1 + Inputs[0, i];
   Outputs[0, i] := 0.25 * (Outputs[0, i] + a2 - b2 + Inputs[1, i]);
   Outputs[1, i] := 0.25 * (Outputs[1, i] + a2 + b2 + Inputs[1, i]);
  end;
end;

procedure TSpinBugLiteModule.VSTModuleProcessOldOne(
  const Inputs, Outputs : TDAVArrayOfSingleDynArray; const SampleFrames : Integer);
var
  i      : Integer;
  a1, b1 : Single;
  a2, b2 : Single;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   FHilbert[0].ProcessHilbertSample(tanh2b(2.1 * Inputs[0, i]), a1, b1);
   FHilbert[1].ProcessHilbertSample(tanh2c(2.2 * Inputs[1, i]), a2, b2);
   a1 := a1 * BasicSineLFO.Sine;
   b1 := b1 * BasicSineLFO.Cosine;
   a2 := a2 * BasicSineLFO.Sine;
   b2 := b2 * BasicSineLFO.Cosine;
   BasicSineLFO.CalculateNextSample;
   Outputs[0, i] := a1 + b1 + Inputs[0, i];
   Outputs[1, i] := a1 - b1 + Inputs[1, i];
   Outputs[0, i] := 0.25 * (Outputs[0, i] + a2 + b2 + Inputs[1, i]);
   Outputs[1, i] := 0.25 * (Outputs[1, i] + a2 - b2 + Inputs[0, i]);
  end;
end;

procedure TSpinBugLiteModule.VSTModuleSampleRateChange(Sender : TObject;
  const SampleRate : Single);
begin
 FSineLFO[0].SampleRate := SampleRate;
 FSineLFO[1].SampleRate := SampleRate;
end;

procedure TSpinBugLiteModule.SBMCoefficientsDisplay(Sender : TObject;
  const Index : Integer; var PreDefined : String);
begin
 PreDefined := IntToStr(round(parameter[0]));
end;

procedure TSpinBugLiteModule.SBMCoefficientsChange(Sender : TObject;
  const Index : Integer; var Value : Single);
var
  ival : Integer;
begin
 ival := round(Value);
 if (ival < 1) or (ival > 32) then Exit;
 if (ival = 17) or (ival = 18) or (ival = 19) or (ival = 20) or (ival = 21) or
    (ival = 22) or (ival = 23) or (ival = 25) or (ival = 26) or (ival = 27) or
    (ival = 28) or (ival = 29) or (ival = 30) or (ival = 31) then Exit;
 if assigned(FHilbert[0]) then FHilbert[0].NumberOfCoefficients := ival;
 if assigned(FHilbert[1]) then FHilbert[1].NumberOfCoefficients := ival;
 if Assigned(EditorForm) then
  with (EditorForm as TFmSpinBugLite)
   do UpdateColour;
end;

procedure TSpinBugLiteModule.SBMProcessTypeDisplay(Sender : TObject;
  const Index : Integer; var PreDefined : String);
begin
 PreDefined := GetProcessTypeCaption(round(Parameter[1]));
end;

procedure TSpinBugLiteModule.SBMProcessTypeChange(Sender : TObject;
  const Index : Integer; var Value : Single);
begin
 Value := round(Value);
 if (Value < 1)
  then Value := 10 else
 if (Value > 10) then Value := 1;
 case Round(Value) of
   1 :
    begin
     OnProcess := VSTModuleProcessStereoA;
     OnProcessReplacing := VSTModuleProcessStereoA;
    end;
   2 :
    begin
     OnProcess := VSTModuleProcessStereoB;
     OnProcessReplacing := VSTModuleProcessStereoB;
    end;
   3 :
    begin
     OnProcess := VSTModuleProcessStereoC;
     OnProcessReplacing := VSTModuleProcessStereoC;
    end;
   4 :
    begin
     OnProcess := VSTModuleProcessStereoD;
     OnProcessReplacing := VSTModuleProcessStereoD;
    end;
   5 :
    begin
     OnProcess := VSTModuleProcessMono;
     OnProcessReplacing := VSTModuleProcessMono;
    end;
   6 :
    begin
     OnProcess := VSTModuleProcessMonoL;
     OnProcessReplacing := VSTModuleProcessMonoL;
    end;
   7 :
    begin
     OnProcess := VSTModuleProcessMonoR;
     OnProcessReplacing := VSTModuleProcessMonoR;
    end;
   8 :
    begin
     OnProcess := VSTModuleProcessMS;
     OnProcessReplacing := VSTModuleProcessMS;
    end;
   9 :
    begin
     OnProcess := VSTModuleProcessSpecial;
     OnProcessReplacing := VSTModuleProcessSpecial;
    end;
   10 :
    begin
     OnProcess := VSTModuleProcessOldOne;
     OnProcessReplacing := VSTModuleProcessOldOne;
    end;
  end;
 if Assigned(EditorForm) then
   with (EditorForm as TFmSpinBugLite) do
     UpdateType;
end;

procedure TSpinBugLiteModule.SBMLFOSpeedChange(Sender : TObject;
  const Index : Integer; var Value : Single);
begin
 FSineLFO[0].Frequency := Value;
 FSineLFO[1].Frequency := 1.01 * Value;

 if assigned(EditorForm) then
  with (EditorForm as TFmSpinBugLite)
   do UpdateLFO;
end;

procedure TSpinBugLiteModule.SBMTBWChange(Sender : TObject;
  const Index : Integer; var Value : Single);
begin
 if (Value <= 0) or (Value >= 0.5) then Exit;
 if Value <> FTBW then
  begin
   FTBW := Value;
   if assigned(FHilbert[0]) then FHilbert[0].Transition := FTBW;
   if assigned(FHilbert[1]) then FHilbert[1].Transition := FTBW;
  end;
end;

end.
