unit RePsychoDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule;

type
  TRePsychoDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterQualityDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDecayChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHoldChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTuneChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFineChanged(Sender: TObject; const Index: Integer; var Value: Single);
  private
    fWet       : Single;
    fDry       : Single;
    fEnvelope  : Single;
    fSize      : Integer;
    fBuffer    : Array [0..1] of PAVDSingleFixedArray;
    fState     : Array [0..1] of Single;
    fDelayTime : Integer;
    fGain      : Single;
    fThreshold : Single;
    fTune      : Single;
    fTim       : Integer;
    fDTim      : Integer;
    procedure TuneChanged;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, DVSTCustomModule;

procedure TRePsychoDataModule.ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fWet := (0.5 * sqrt(0.01 * Value));
 fDry := sqrt(1 - 0.01 * Value);
end;

procedure TRePsychoDataModule.ParameterDecayChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Value > 0
  then fEnvelope := 1 + 0.003 * Power(Value / 50, 5)
  else fEnvelope := 1 + 0.025 * Power(Value / 50, 5);

// if Value > 0.5
//  then fEnvelope := (1.0 + 0.01 * (Value / 50))
//  else fEnvelope := (1.0 + 0.01 * (Value / 50));

end;

procedure TRePsychoDataModule.ParameterHoldChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fDelayTime := 441 + round(0.5 * fSize * Value);
end;

procedure TRePsychoDataModule.ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fThreshold := dB_to_Amp(Value);
end;

procedure TRePsychoDataModule.ParameterTuneChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 TuneChanged;
end;

procedure TRePsychoDataModule.ParameterFineChanged(Sender: TObject; const Index: Integer; var Value: Single);
begin
 TuneChanged;
end;

procedure TRePsychoDataModule.ParameterQualityDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] > 0.5
  then PreDefined := 'HIGH'
  else PreDefined := 'LOW';
end;

procedure TRePsychoDataModule.TuneChanged;
begin
 fTune := ((round(Parameter[0] * 24) - 24) + (Parameter[1] - 1)) / 24;
 fTune := Power(10, 0.60206 * fTune);
end;

procedure TRePsychoDataModule.VSTModuleCreate(Sender: TObject);
begin
 fSize := 22050;
 GetMem(fBuffer[0], fSize * SizeOf(Single));
 GetMem(fBuffer[1], fSize * SizeOf(Single));

 VSTModuleSuspend(Sender);

 Parameter[5] := 100; // Mix

(*
 //inits here!
 Parameter[0] := 1.0; //tune
 Parameter[1] := 1.0; //fine tune
 Parameter[2] := 0.5; //env
 Parameter[3] := 0.6; //thresh
 Parameter[4] := 0.45; //minimum chunk length
 Parameter[6] := 0.4; //quality

 //calcs here!
 fBuf[0]      := 0.0;
 fBuf[1]      := 0.0;
 fTim         := fSize + 1;
 fDTim        := 441 + int(0.5 * fSize * Parameter[4]);
 fFil         := 0.0;
 fThreshold   := Power(10.0,(1.5 * Parameter[3]) - 1.5);

 if (Parameter[2] > 0.5)
  then fEnv := (1.0 + 0.003 * Power(Parameter[2] - 0.5,5.0))
  else fEnv := (1.0 + 0.025 * Power(Parameter[2] - 0.5,5.0));

 fTune := (((int(Parameter[0] * 24.0) - 24.0) + (Parameter[1] - 1.0)) / 24.0);
 fTune := Power(10, 0.60206 * fTune);
 fWet  := (0.5 * sqrt(Parameter[5]));
 fDry  := sqrt(1.0 - Parameter[5]);
*)
end;

procedure TRePsychoDataModule.VSTModuleDestroy(Sender: TObject);
begin
 if assigned(fBuffer[0]) then Dispose(fBuffer[0]);
 if assigned(fBuffer[1]) then Dispose(fBuffer[1]);
end;

procedure TRePsychoDataModule.VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample            : Integer;
  a, b, c, d        : Single;
  we, dr, tu, en    : Single;
  ga, x, x2, xx     : Single;
  xx2, it1, it2     : Single;
  ti, dti, of1, of2 : Integer;
begin
 we  := fWet;
 dr  := fDry;
 tu  := fTune;
 en  := fEnvelope;
 ga  := fGain;
 x   := 0;
 x2  := 0;
 xx  := fState[0];
 xx2 := fState[1];
 ti  := fTim;
 dti := fDTim;

 if (Parameter[6] > 0.5) then //high quality
  begin
   we := 2 * we;
   for Sample := 0 to SampleFrames - 1 do
    begin
     a := Inputs[0, Sample];
     b := Inputs[1, Sample]; //process from here...

     if ((a + b > fThreshold) and (ti > dti)) then //trigger
      begin
       ga := 1;
       ti := 0;
      end;

      if (ti < 22050) then //play out
       begin
        if (ti < 80) then //fade in
         begin
          if (ti = 0) then
           begin
            xx  := x;
            xx2 := x2;
           end;

          fBuffer[0, ti] := a;
          fBuffer[1, ti] := b;
          x  := fBuffer[0, round(ti * tu)];
          x2 := fBuffer[1, round(ti * tu)];

          x  := (xx  * (1 - (0.0125 * ti)) + (x  * 0.0125 * ti));
          x2 := (xx2 * (1 - (0.0125 * ti)) + (x2 * 0.0125 * ti));
         end
        else
         begin
          // update to/from fBuffer[0]
          fBuffer[0, ti] := a;
          fBuffer[1, ti] := b;

          it1 := (ti * tu); //interpolation
          of1 := round(it1);
          of2 := of1 + 1;
          it1 := it1 - of1;
          it2 := (1 - it1);

          x  := (it2* fBuffer[0, of1]) + (it1* fBuffer[0, of2]);
          x2 := (it2* fBuffer[1, of1]) + (it1* fBuffer[1, of2]);
         end;

        inc(ti);
        ga := ga * en;
       end
      else ga := 0;   //mute

      Outputs[0, Sample] := (a * dr) + (x * ga * we); // output
      Outputs[1, Sample] := (b * dr) + (x2 * ga * we);
    end;
  end
 else
  begin

   for Sample := 0 to SampleFrames - 1 do
    begin
     a := Inputs[0, Sample];
     b := Inputs[1, Sample];    // process from here...

     if ((a + b > fThreshold) and (ti > dti)) then //trigger
      begin
       ga := 1;
       ti := 0;
      end;

     if ti < 22050 then //play out
      begin
       if ti < 80 then //fade in
        begin
         if ti = 0 then xx := x;

         fBuffer[0, ti] := (a + b);
         x := fBuffer[0, round(ti * tu)];

         x := (xx * (1 - (0.0125 * ti)) + (x * 0.0125 * ti));
        end
       else
        begin
         //update to/from fBuffer[0]
         fBuffer[0, ti] := (a + b);
         x := fBuffer[0, round(ti * tu)];
        end;

       inc(ti);
       ga := ga * en;
      end
     else ga := 0; //mute

     Outputs[0, Sample] := (Inputs[0, Sample] * dr) + (x * ga * we); // output
     Outputs[1, Sample] := (Inputs[1, Sample] * dr) + (x * ga * we);
    end;
  end;
 fTim      := ti;
 fGain     := ga;
 fState[0] := xx;
 fState[1] := xx2;
end;

procedure TRePsychoDataModule.VSTModuleSuspend(Sender: TObject);
begin
  FillChar(fBuffer[0], fSize * SizeOf(Single), 0);
  FillChar(fBuffer[1], fSize * SizeOf(Single), 0);
end;

end.

(*
void mdaRePsycho::getParameterDisplay(VstInt32 index, char *text)
begin
  switch(index)
  begin
    case 0: long2string(round(int(24.0 * Parameter[0]) - 24.0), text); break;
    case 1: long2string(round(int(99.0 * Parameter[1]) - 99.0), text); break;
    case 4: long2string(round(1000.0 * fDTim / getSampleRate()), text); break;
  end;
end;
*)