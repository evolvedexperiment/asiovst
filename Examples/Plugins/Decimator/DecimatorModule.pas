unit DecimatorModule;

interface

uses Windows, DAVDCommon, Forms, DVSTEffect, DVSTModule;

type
  TVSTDecimator = class(TVSTModule)
    procedure VSTModuleInitialize(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessMidi(Sender: TObject; MidiEvent: TVstMidiEvent);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
  private
    fCutoffFreqNorm  : Double;
    fResonance       : Double;
    fFC              : Double;
    fSHrate          : Double;
    fYL,fYR          : Double;
    fOutVol, fMixWet : Double;
    fOld             : array [0..1, 0..1] of Double;
    fSHCounter       : Double;
    fBitMul,fBitDiv  : Double;
  public
  end;

implementation

{$R *.DFM}

uses Math, DecimatorGUI;

const kDenorm = 1E-24;

////////////////////////////////////////////////////////////////////////////////
// Initialize
////////////////////////////////////////////////////////////////////////////////

procedure TVSTDecimator.VSTModuleInitialize(Sender: TObject);
begin
 Parameter[0] := 44100;
 Parameter[1] := 24;
 Parameter[2] := 1000;
 Parameter[3] := 1;
 Parameter[4] := 1;
 Parameter[5] := 100;
 Parameter[6] := 0;
 with Programs[0] do
  begin
   Parameter[0] := 44100;
   Parameter[1] := 24;
   Parameter[2] := 1000;
   Parameter[3] := 1;
   Parameter[4] := 1;
   Parameter[5] := 100;
   Parameter[6] := 0;
  end;
 with Programs[1] do
  begin
   Parameter[0] := 20000;
   Parameter[1] := 16;
   Parameter[2] := 2000;
   Parameter[3] := 1;
   Parameter[4] := 1;
   Parameter[5] := 80;
   Parameter[6] := 0;
  end;
 with Programs[2] do
  begin
   Parameter[0] := 44100;
   Parameter[1] := 24;
   Parameter[2] := 400;
   Parameter[3] := 4;
   Parameter[4] := 1;
   Parameter[5] := 100;
   Parameter[6] := 0;
  end;
end;

procedure TVSTDecimator.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TVSTGUI.Create(Self);
end;

////////////////////////////////////////////////////////////////////////////////
// Parameter Changed
////////////////////////////////////////////////////////////////////////////////

procedure TVSTDecimator.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
const
  cMul = 200/441;
  cOne24th = 1/24;
var
  i : Integer;
begin
 case index of
  0 : begin
       fSHrate := FreqLogToLinear(Value * cMul);
       if Assigned(EditorForm) then
        with TVSTGUI(EditorForm) do
         if ShSHRate.Top<>Round((1 - fSHRate) * ShSHRateBg.Height + ShSHRateBg.Top) then
          begin
           ShSHRate.Top := Round((1 - fSHRate) * ShSHRateBg.Height + ShSHRateBg.Top);
           ShSHRate.Height := ShSHRateBg.Height - ShSHRate.Top + ShSHRateBg.Top;
          end;
      end;
  1 : begin
       fBitMul := Power(2, Value + 1) - 1;
       fBitDiv := 1 / fBitMul;
       if Assigned(EditorForm) then
        with TVSTGUI(EditorForm) do
         begin
          i := Round((1 - Value * COne24th) * ShBitsBg.Height + ShBitsBg.Top);
          if ShBits.Top <> i then
           try
            ShBits.Top := i;
            ShBits.Height := ShBitsBg.Height - ShBits.Top + ShBitsBg.Top;
           except
           end;
         end;
      end;
  2 : begin
       fCutoffFreqNorm := f_Limit(0.5 * FreqLogToLinear(Value), 0.01, 1);
       fFC := fResonance * (1 + 1 / (1 - fCutoffFreqNorm));
       if Assigned(EditorForm) then
        with TVSTGUI(EditorForm) do
         if ShCut.Top<>Round((1 - 2 * fCutoffFreqNorm) * ShCutBg.Height + ShCutBg.Top) then
          begin
           ShCut.Top := Round((1 - 2 * fCutoffFreqNorm) * ShCutBg.Height + ShCutBg.Top);
           ShCut.Height := ShCutBg.Height - ShCut.Top + ShCutBg.Top;
          end;
      end;
  3 : begin
       fResonance := Value * 0.1;
       if fResonance > 0.8 then fResonance := 0.8;
       fFC := fResonance * (1 + 1 / (1 - fCutoffFreqNorm));
       if Assigned(EditorForm) then
        with TVSTGUI(EditorForm) do
         if ShRes.Top <> Round((1 - 1.25 * fResonance) * ShResBg.Height + ShResBg.Top) then
          begin
           ShRes.Top := Round((1 - 1.25 * fResonance) * ShResBg.Height + ShResBg.Top);
           ShRes.Height := ShResBg.Height - ShRes.Top + ShResBg.Top;
          end;
      end;
  5 : begin
       fMixWet := Value * 0.01;
       if Assigned(EditorForm) then
        with TVSTGUI(EditorForm) do
         if ShMix.Top <> Round((1 - fMixWet) * ShMixBg.Height + ShMixBg.Top) then
          begin
           ShMix.Top := Round((1 - fMixWet) * ShMixBg.Height + ShMixBg.Top);
           ShMix.Height := ShMixBg.Height - ShMix.Top + ShMixBg.Top;
          end;
      end;
  6 : begin
       fOutVol := dB_to_Amp(Value);
       if Assigned(EditorForm) then
        with TVSTGUI(EditorForm) do
         if ShVol.Top<>Round(ShVolBg.Top - (Value-6) / 30 * ShVolBg.Height) then
          begin
           ShVol.Top := Round(ShVolBg.Top - (Value - 6) / 30 * ShVolBg.Height);
           ShVol.Height := ShVolBg.Height-(ShVol.Top-ShVolBg.Top);
          end;
      end;
 end;
end;



////////////////////////////////////////////////////////////////////////////////
// 32 Bit Processing
////////////////////////////////////////////////////////////////////////////////

procedure TVSTDecimator.VSTModuleProcess(const inputs, outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i :Integer;
begin
 if Parameter[4] < 0.5 then
  begin
   for i := 0 to SampleFrames - 1 do
    begin
     fSHCounter := fSHCounter + fSHrate;
     if (fSHCounter>1) then
      begin
       fSHCounter := fSHCounter - 1;
       fYL := round(fBitMul * inputs[0, i]) * fBitDiv;
       fYR := round(fBitMul * inputs[1, i]) * fBitDiv;
      end;

     fOld[0, 0] := fOld[0, 0] + fCutoffFreqNorm * (fYL - fOld[0, 0] + fFC * (fOld[0, 0] - fOld[0, 1])) + kDenorm;
     fOld[0, 1] := fOld[0, 1] + fCutoffFreqNorm * (fOld[0, 0] - fOld[0, 1]) + kDenorm;
     fOld[1, 0] := fOld[1, 0] + fCutoffFreqNorm * (fYR - fOld[1, 0] + fFC * (fOld[1, 0] - fOld[1, 1])) + kDenorm;
     fOld[1, 1] := fOld[1, 1] + fCutoffFreqNorm * (fOld[1, 0] - fOld[1, 1]) + kDenorm;

     //limit coeffcients - not very elegant, but else filter
     //is particularly unstable with high resonance and low sample&hold rates
     if fOld[0, 0]>1 then fOld[0, 0] := 1 else if fOld[0, 0] < -1 then fOld[0, 0] := -1;
     if fOld[0, 1]>1 then fOld[0, 1] := 1 else if fOld[0, 1] < -1 then fOld[0, 1] := -1;
     if fOld[1, 0]>1 then fOld[1, 0] := 1 else if fOld[1, 0] < -1 then fOld[1, 0] := -1;
     if fOld[1, 1]>1 then fOld[1, 1] := 1 else if fOld[1, 1] < -1 then fOld[1, 1] := -1;

     outputs[0,i] := f_Limit((fOutVol * ((1 - fMixWet) * inputs[0, i] + fMixWet * fOld[0, 1])));
     outputs[1,i] := f_Limit((fOutVol * ((1 - fMixWet) * inputs[1, i] + fMixWet * fOld[1, 1])));
    end
  end
 else
  begin
   for i := 0 to SampleFrames - 1 do
    begin
     fSHCounter := fSHCounter + fSHrate;
     if (fSHCounter > 1) then
      begin
       fSHCounter := fSHCounter - 1;
       fYL := round(fBitMul * inputs[0, i]) * fBitDiv;
       fYR := round(fBitMul * inputs[1, i]) * fBitDiv;
      end;

     fOld[0, 0] := fOld[0, 0] + fCutoffFreqNorm * (fYL - fOld[0, 0] + fFC * (fOld[0, 0] - fOld[0, 1])) + kDenorm;
     fOld[0, 1] := fOld[0, 1] + fCutoffFreqNorm * (fOld[0, 0] - fOld[0, 1]) + kDenorm;
     fOld[1, 0] := fOld[1, 0] + fCutoffFreqNorm * (fYR - fOld[1, 0] + fFC * (fOld[1, 0] - fOld[1, 1])) + kDenorm;
     fOld[1, 1] := fOld[1, 1] + fCutoffFreqNorm * (fOld[1, 0] - fOld[1, 1]) + kDenorm;

     //limit coeffcients - not very elegant, but else filter
     //is particularly unstable with high resonance and low sample&hold rates
     if fOld[0, 0] > 1 then fOld[0, 0] := 1 else if fOld[0, 0] < -1 then fOld[0, 0] := -1;
     if fOld[0, 1] > 1 then fOld[0, 1] := 1 else if fOld[0, 1] < -1 then fOld[0, 1] := -1;
     if fOld[1, 0] > 1 then fOld[1, 0] := 1 else if fOld[1, 0] < -1 then fOld[1, 0] := -1;
     if fOld[1, 1] > 1 then fOld[1, 1] := 1 else if fOld[1, 1] < -1 then fOld[1, 1] := -1;

     outputs[0, i] := f_Limit((fOutVol * ((1 - fMixWet) * inputs[0, i] + fMixWet * (fYL - fOld[0, 1]))));
     outputs[1, i] := f_Limit((fOutVol * ((1 - fMixWet) * inputs[1, i] + fMixWet * (fYR - fOld[0, 1]))));
    end;
  end;
end;



////////////////////////////////////////////////////////////////////////////////
// 64 Bit Processing
////////////////////////////////////////////////////////////////////////////////

procedure TVSTDecimator.VSTModuleProcessDoubleReplacing(const inputs,
  outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 if Parameter[4] < 0.5 then
  begin
   for i := 0 to SampleFrames - 1 do
    begin
     fSHCounter := fSHCounter + fSHrate;
     if (fSHCounter > 1) then
      begin
       fSHCounter := fSHCounter - 1;
       fYL := round(fBitMul * inputs[0, i]) * fBitDiv;
       fYR := round(fBitMul * inputs[1, i]) * fBitDiv;
      end;

     fOld[0,0] := fOld[0,0]+fCutoffFreqNorm*(fYL-fOld[0,0]+fFC*(fOld[0,0]-fOld[0,1]))+kDenorm;
     fOld[0,1] := fOld[0,1]+fCutoffFreqNorm*(fOld[0,0]-fOld[0,1])+kDenorm;
     fOld[1,0] := fOld[1,0]+fCutoffFreqNorm*(fYR-fOld[1,0]+fFC*(fOld[1,0]-fOld[1,1]))+kDenorm;
     fOld[1,1] := fOld[1,1]+fCutoffFreqNorm*(fOld[1,0]-fOld[1,1])+kDenorm;

     //limit coeffcients - not very elegant, but else filter
     //is particularly unstable with high resonance and low sample&hold rates
     if fOld[0,0]>1 then fOld[0,0] := 1 else if fOld[0,0]<-1 then fOld[0,0] := -1;
     if fOld[0,1]>1 then fOld[0,1] := 1 else if fOld[0,1]<-1 then fOld[0,1] := -1;
     if fOld[1,0]>1 then fOld[1,0] := 1 else if fOld[1,0]<-1 then fOld[1,0] := -1;
     if fOld[1,1]>1 then fOld[1,1] := 1 else if fOld[1,1]<-1 then fOld[1,1] := -1;

     outputs[0,i] := f_Limit((fOutVol*((1-fMixWet)*inputs[0,i]+fMixWet*fOld[0,1])));
     outputs[1,i] := f_Limit((fOutVol*((1-fMixWet)*inputs[1,i]+fMixWet*fOld[1,1])));
    end
  end
 else
  begin
   for i := 0 to SampleFrames - 1 do
    begin
     fSHCounter := fSHCounter + fSHrate;
     if (fSHCounter > 1) then
      begin
       fSHCounter := fSHCounter-1;
       fYL := round(fBitMul * inputs[0,i]) * fBitDiv;
       fYR := round(fBitMul * inputs[1,i]) * fBitDiv;
      end;

     fOld[0, 0] := fOld[0, 0] + fCutoffFreqNorm * (fYL - fOld[0, 0] + fFC * (fOld[0, 0] - fOld[0, 1])) + kDenorm;
     fOld[0, 1] := fOld[0, 1] + fCutoffFreqNorm * (fOld[0, 0] - fOld[0, 1]) + kDenorm;
     fOld[1, 0] := fOld[1, 0] + fCutoffFreqNorm * (fYR - fOld[1, 0] + fFC*(fOld[1, 0] - fOld[1, 1])) + kDenorm;
     fOld[1, 1] := fOld[1, 1] + fCutoffFreqNorm * (fOld[1, 0] - fOld[1, 1]) + kDenorm;

     //limit coeffcients - not very elegant, but else filter
     //is particularly unstable with high resonance and low sample&hold rates
     if fOld[0, 0] > 1 then fOld[0, 0] := 1 else if fOld[0, 0] < -1 then fOld[0, 0] := -1;
     if fOld[0, 1] > 1 then fOld[0, 1] := 1 else if fOld[0, 1] < -1 then fOld[0, 1] := -1;
     if fOld[1, 0] > 1 then fOld[1, 0] := 1 else if fOld[1, 0] < -1 then fOld[1, 0] := -1;
     if fOld[1, 1] > 1 then fOld[1, 1] := 1 else if fOld[1, 1] < -1 then fOld[1, 1] := -1;

     outputs[0, i] := f_Limit((fOutVol * ((1 - fMixWet) * inputs[0, i] + fMixWet*(fYL - fOld[0, 1]))));
     outputs[1, i] := f_Limit((fOutVol * ((1 - fMixWet) * inputs[1, i] + fMixWet*(fYR - fOld[0, 1]))));
    end;
  end;
end;

procedure TVSTDecimator.VSTModuleProcessMidi(Sender: TObject;
  MidiEvent: TVstMidiEvent);
var status : Integer;
    CCData : Single;
begin
 status := MidiEvent.midiData[0] and $F0; // channel information is removed

 if (status=$B0) then // midi CC ?
  begin
   CCData := MidiEvent.midiData[2]/127; // CC data
   case MidiEvent.midiData[1] of // midi CC#
    70: Parameter[0] := FreqLinearToLog(CCData)*2.205;
    71: Parameter[1] := 24*CCData;
    72: Parameter[2] := FreqLinearToLog(CCData);
    73: Parameter[3] := 10*CCData;
    74: Parameter[5] := 100*CCData;
    75: Parameter[6] := 6-30*CCData;
   end;
  end;
end;

end.
