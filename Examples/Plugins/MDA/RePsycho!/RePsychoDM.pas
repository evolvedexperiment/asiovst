unit RePsychoDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule;

type
  TRePsychoDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterQualityDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDecayChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHoldChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTuneChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFineChanged(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTuneDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterFineDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterHoldDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FWet       : Single;
    FDry       : Single;
    FEnvelope  : Single;
    FSize      : Integer;
    FBuffer    : Array [0..1] of PDAVSingleFixedArray;
    FState     : Array [0..1] of Single;
    FDelayTime : Integer;
    FGain      : Single;
    FThreshold : Single;
    FTune      : Single;
    FTim       : Integer;
    FDTim      : Integer;
    procedure TuneChanged;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_VSTCustomModule;

procedure TRePsychoDataModule.VSTModuleOpen(Sender: TObject);
begin
 FSize := 22050;
 GetMem(FBuffer[0], FSize * SizeOf(Single));
 GetMem(FBuffer[1], FSize * SizeOf(Single));

 VSTModuleSuspend(Sender);

 Parameter[5] := 100; // Mix

(*
 // Initial Parameters
 Parameter[0] := 1.0; //tune
 Parameter[1] := 1.0; //fine tune
 Parameter[2] := 0.5; //env
 Parameter[3] := 0.6; //thresh
 Parameter[4] := 0.45; //minimum chunk length
 Parameter[6] := 0.4; //quality

 //calcs here!
 fBuf[0]      := 0.0;
 fBuf[1]      := 0.0;
 FTim         := FSize + 1;
 FDTim        := 441 + int(0.5 * FSize * Parameter[4]);
 fFil         := 0.0;
 FThreshold   := Power(10.0,(1.5 * Parameter[3]) - 1.5);

 if (Parameter[2] > 0.5)
  then fEnv := (1.0 + 0.003 * Power(Parameter[2] - 0.5,5.0))
  else fEnv := (1.0 + 0.025 * Power(Parameter[2] - 0.5,5.0));

 FTune := (((int(Parameter[0] * 24.0) - 24.0) + (Parameter[1] - 1.0)) / 24.0);
 FTune := Power(10, 0.60206 * FTune);
 FWet  := (0.5 * sqrt(Parameter[5]));
 FDry  := sqrt(1.0 - Parameter[5]);
*)
end;

procedure TRePsychoDataModule.VSTModuleClose(Sender: TObject);
begin
 if assigned(FBuffer[0]) then Dispose(FBuffer[0]);
 if assigned(FBuffer[1]) then Dispose(FBuffer[1]);
end;

procedure TRePsychoDataModule.ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FWet := (0.5 * sqrt(0.01 * Value));
 FDry := sqrt(1 - 0.01 * Value);
end;

procedure TRePsychoDataModule.ParameterDecayChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Value > 0
  then FEnvelope := 1 + 0.003 * Power(Value / 50, 5)
  else FEnvelope := 1 + 0.025 * Power(Value / 50, 5);

// if Value > 0.5
//  then FEnvelope := (1.0 + 0.01 * (Value / 50))
//  else FEnvelope := (1.0 + 0.01 * (Value / 50));

end;

procedure TRePsychoDataModule.ParameterHoldChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FDelayTime := 441 + round(0.5 * FSize * Value);
end;

procedure TRePsychoDataModule.ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FThreshold := dB_to_Amp(Value);
end;

procedure TRePsychoDataModule.ParameterTuneChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 TuneChanged;
end;

procedure TRePsychoDataModule.ParameterTuneDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(round(24 * Parameter[0]) - 24);
end;

procedure TRePsychoDataModule.ParameterFineDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(round(99.0 * Parameter[1]) - 99);
end;

procedure TRePsychoDataModule.ParameterHoldDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(round(1000.0 * FDTim / SampleRate));
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
 FTune := ((round(Parameter[0] * 24) - 24) + (Parameter[1] - 1)) / 24;
 FTune := Power(10, 0.60206 * FTune);
end;

procedure TRePsychoDataModule.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample            : Integer;
  a, b              : Single;
  we, dr, tu, en    : Single;
  ga, x, x2, xx     : Single;
  xx2, it1, it2     : Single;
  ti, dti, of1, of2 : Integer;
begin
 we  := FWet;
 dr  := FDry;
 tu  := FTune;
 en  := FEnvelope;
 ga  := FGain;
 x   := 0;
 x2  := 0;
 xx  := FState[0];
 xx2 := FState[1];
 ti  := FTim;
 dti := FDTim;

 if (Parameter[6] > 0.5) then //high quality
  begin
   we := 2 * we;
   for Sample := 0 to SampleFrames - 1 do
    begin
     a := Inputs[0, Sample];
     b := Inputs[1, Sample]; //process from here...

     if ((a + b > FThreshold) and (ti > dti)) then //trigger
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

          FBuffer[0, ti] := a;
          FBuffer[1, ti] := b;
          x  := FBuffer[0, round(ti * tu)];
          x2 := FBuffer[1, round(ti * tu)];

          x  := (xx  * (1 - (0.0125 * ti)) + (x  * 0.0125 * ti));
          x2 := (xx2 * (1 - (0.0125 * ti)) + (x2 * 0.0125 * ti));
         end
        else
         begin
          // update to/from FBuffer[0]
          FBuffer[0, ti] := a;
          FBuffer[1, ti] := b;

          it1 := (ti * tu); //interpolation
          of1 := round(it1);
          of2 := of1 + 1;
          it1 := it1 - of1;
          it2 := (1 - it1);

          x  := (it2* FBuffer[0, of1]) + (it1* FBuffer[0, of2]);
          x2 := (it2* FBuffer[1, of1]) + (it1* FBuffer[1, of2]);
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

     if ((a + b > FThreshold) and (ti > dti)) then //trigger
      begin
       ga := 1;
       ti := 0;
      end;

     if ti < 22050 then //play out
      begin
       if ti < 80 then //fade in
        begin
         if ti = 0 then xx := x;

         FBuffer[0, ti] := (a + b);
         x := FBuffer[0, round(ti * tu)];

         x := (xx * (1 - (0.0125 * ti)) + (x * 0.0125 * ti));
        end
       else
        begin
         //update to/from FBuffer[0]
         FBuffer[0, ti] := (a + b);
         x := FBuffer[0, round(ti * tu)];
        end;

       inc(ti);
       ga := ga * en;
      end
     else ga := 0; //mute

     Outputs[0, Sample] := (Inputs[0, Sample] * dr) + (x * ga * we); // output
     Outputs[1, Sample] := (Inputs[1, Sample] * dr) + (x * ga * we);
    end;
  end;
 FTim      := ti;
 FGain     := ga;
 FState[0] := xx;
 FState[1] := xx2;
end;

procedure TRePsychoDataModule.VSTModuleSuspend(Sender: TObject);
begin
  FillChar(FBuffer[0]^, FSize * SizeOf(Single), 0);
  FillChar(FBuffer[1]^, FSize * SizeOf(Single), 0);
end;

end.
