unit DecimatorModule;

interface

uses
  Windows, DAV_Common, Forms, DAV_VSTEffect, DAV_VSTModule;

type
  TVSTDecimator = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessMidi(Sender: TObject; MidiEvent: TVstMidiEvent);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleOpen(Sender: TObject);
  private
    FCutoffFreqNorm  : Double;
    FResonance       : Double;
    FFC              : Double;
    FSHrate          : Double;
    FYL, FYR         : Double;
    FOutVol, FMixWet : Double;
    FOld             : array [0..1, 0..1] of Double;
    FSHCounter       : Double;
    FBitMul, FBitDiv : Double;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, DecimatorGUI;

////////////////////////////////////////////////////////////////////////////////
// Open / Close
////////////////////////////////////////////////////////////////////////////////

procedure TVSTDecimator.VSTModuleOpen(Sender: TObject);
begin
 // default parameters
 Parameter[0] := 44100;
 Parameter[1] := 24;
 Parameter[2] := 1000;
 Parameter[3] := 1;
 Parameter[4] := 1;
 Parameter[5] := 100;
 Parameter[6] := 0;

 // default preset
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

 // preset 1
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

 // preset 2
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
  CMul = 200/441;
  COne24th = 1/24;
var
  i : Integer;
begin
 case index of
  0 : begin
       FSHrate := FreqLogToLinear(Value * cMul);
       if Assigned(EditorForm) then
        with TVSTGUI(EditorForm) do
         if ShSHRate.Top<>Round((1 - FSHrate) * ShSHRateBg.Height + ShSHRateBg.Top) then
          begin
           ShSHRate.Top := Round((1 - FSHrate) * ShSHRateBg.Height + ShSHRateBg.Top);
           ShSHRate.Height := ShSHRateBg.Height - ShSHRate.Top + ShSHRateBg.Top;
          end;
      end;
  1 : begin
       FBitMul := Power(2, Value + 1) - 1;
       FBitDiv := 1 / FBitMul;
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
       FCutoffFreqNorm := f_Limit(0.5 * FreqLogToLinear(Value), 0.01, 1);
       FFC := FResonance * (1 + 1 / (1 - FCutoffFreqNorm));
       if Assigned(EditorForm) then
        with TVSTGUI(EditorForm) do
         if ShCut.Top<>Round((1 - 2 * FCutoffFreqNorm) * ShCutBg.Height + ShCutBg.Top) then
          begin
           ShCut.Top := Round((1 - 2 * FCutoffFreqNorm) * ShCutBg.Height + ShCutBg.Top);
           ShCut.Height := ShCutBg.Height - ShCut.Top + ShCutBg.Top;
          end;
      end;
  3 : begin
       FResonance := Value * 0.1;
       if FResonance > 0.8 then FResonance := 0.8;
       FFC := FResonance * (1 + 1 / (1 - FCutoffFreqNorm));
       if Assigned(EditorForm) then
        with TVSTGUI(EditorForm) do
         if ShRes.Top <> Round((1 - 1.25 * FResonance) * ShResBg.Height + ShResBg.Top) then
          begin
           ShRes.Top := Round((1 - 1.25 * FResonance) * ShResBg.Height + ShResBg.Top);
           ShRes.Height := ShResBg.Height - ShRes.Top + ShResBg.Top;
          end;
      end;
  5 : begin
       FMixWet := Value * 0.01;
       if Assigned(EditorForm) then
        with TVSTGUI(EditorForm) do
         if ShMix.Top <> Round((1 - FMixWet) * ShMixBg.Height + ShMixBg.Top) then
          begin
           ShMix.Top := Round((1 - FMixWet) * ShMixBg.Height + ShMixBg.Top);
           ShMix.Height := ShMixBg.Height - ShMix.Top + ShMixBg.Top;
          end;
      end;
  6 : begin
       FOutVol := dB_to_Amp(Value);
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

procedure TVSTDecimator.VSTModuleProcess(const inputs, outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i :Integer;
begin
 if Parameter[4] < 0.5 then
  begin
   for i := 0 to SampleFrames - 1 do
    begin
     FSHCounter := FSHCounter + FSHrate;
     if (FSHCounter>1) then
      begin
       FSHCounter := FSHCounter - 1;
       FYL := round(FBitMul * inputs[0, i]) * FBitDiv;
       FYR := round(FBitMul * inputs[1, i]) * FBitDiv;
      end;

     FOld[0, 0] := FOld[0, 0] + FCutoffFreqNorm * (FYL - FOld[0, 0] + FFC * (FOld[0, 0] - FOld[0, 1])) + CDenorm32;
     FOld[0, 1] := FOld[0, 1] + FCutoffFreqNorm * (FOld[0, 0] - FOld[0, 1]) + CDenorm32;
     FOld[1, 0] := FOld[1, 0] + FCutoffFreqNorm * (FYR - FOld[1, 0] + FFC * (FOld[1, 0] - FOld[1, 1])) + CDenorm32;
     FOld[1, 1] := FOld[1, 1] + FCutoffFreqNorm * (FOld[1, 0] - FOld[1, 1]) + CDenorm32;

     //limit coeffcients - not very elegant, but else filter
     //is particularly unstable with high resonance and low sample&hold rates
     if FOld[0, 0]>1 then FOld[0, 0] := 1 else if FOld[0, 0] < -1 then FOld[0, 0] := -1;
     if FOld[0, 1]>1 then FOld[0, 1] := 1 else if FOld[0, 1] < -1 then FOld[0, 1] := -1;
     if FOld[1, 0]>1 then FOld[1, 0] := 1 else if FOld[1, 0] < -1 then FOld[1, 0] := -1;
     if FOld[1, 1]>1 then FOld[1, 1] := 1 else if FOld[1, 1] < -1 then FOld[1, 1] := -1;

     outputs[0,i] := f_Limit((FOutVol * ((1 - FMixWet) * inputs[0, i] + FMixWet * FOld[0, 1])));
     outputs[1,i] := f_Limit((FOutVol * ((1 - FMixWet) * inputs[1, i] + FMixWet * FOld[1, 1])));
    end
  end
 else
  begin
   for i := 0 to SampleFrames - 1 do
    begin
     FSHCounter := FSHCounter + FSHrate;
     if (FSHCounter > 1) then
      begin
       FSHCounter := FSHCounter - 1;
       FYL := round(FBitMul * inputs[0, i]) * FBitDiv;
       FYR := round(FBitMul * inputs[1, i]) * FBitDiv;
      end;

     FOld[0, 0] := FOld[0, 0] + FCutoffFreqNorm * (FYL - FOld[0, 0] + FFC * (FOld[0, 0] - FOld[0, 1])) + CDenorm32;
     FOld[0, 1] := FOld[0, 1] + FCutoffFreqNorm * (FOld[0, 0] - FOld[0, 1]) + CDenorm32;
     FOld[1, 0] := FOld[1, 0] + FCutoffFreqNorm * (FYR - FOld[1, 0] + FFC * (FOld[1, 0] - FOld[1, 1])) + CDenorm32;
     FOld[1, 1] := FOld[1, 1] + FCutoffFreqNorm * (FOld[1, 0] - FOld[1, 1]) + CDenorm32;

     //limit coeffcients - not very elegant, but else filter
     //is particularly unstable with high resonance and low sample&hold rates
     if FOld[0, 0] > 1 then FOld[0, 0] := 1 else if FOld[0, 0] < -1 then FOld[0, 0] := -1;
     if FOld[0, 1] > 1 then FOld[0, 1] := 1 else if FOld[0, 1] < -1 then FOld[0, 1] := -1;
     if FOld[1, 0] > 1 then FOld[1, 0] := 1 else if FOld[1, 0] < -1 then FOld[1, 0] := -1;
     if FOld[1, 1] > 1 then FOld[1, 1] := 1 else if FOld[1, 1] < -1 then FOld[1, 1] := -1;

     outputs[0, i] := f_Limit((FOutVol * ((1 - FMixWet) * inputs[0, i] + FMixWet * (FYL - FOld[0, 1]))));
     outputs[1, i] := f_Limit((FOutVol * ((1 - FMixWet) * inputs[1, i] + FMixWet * (FYR - FOld[0, 1]))));
    end;
  end;
end;



////////////////////////////////////////////////////////////////////////////////
// 64 Bit Processing
////////////////////////////////////////////////////////////////////////////////

procedure TVSTDecimator.VSTModuleProcessDoubleReplacing(const inputs,
  outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 if Parameter[4] < 0.5 then
  begin
   for i := 0 to SampleFrames - 1 do
    begin
     FSHCounter := FSHCounter + FSHrate;
     if (FSHCounter > 1) then
      begin
       FSHCounter := FSHCounter - 1;
       FYL := round(FBitMul * inputs[0, i]) * FBitDiv;
       FYR := round(FBitMul * inputs[1, i]) * FBitDiv;
      end;

     FOld[0,0] := FOld[0,0]+FCutoffFreqNorm*(FYL-FOld[0,0]+FFC*(FOld[0,0]-FOld[0,1]))+CDenorm32;
     FOld[0,1] := FOld[0,1]+FCutoffFreqNorm*(FOld[0,0]-FOld[0,1])+CDenorm32;
     FOld[1,0] := FOld[1,0]+FCutoffFreqNorm*(FYR-FOld[1,0]+FFC*(FOld[1,0]-FOld[1,1]))+CDenorm32;
     FOld[1,1] := FOld[1,1]+FCutoffFreqNorm*(FOld[1,0]-FOld[1,1])+CDenorm32;

     //limit coeffcients - not very elegant, but else filter
     //is particularly unstable with high resonance and low sample&hold rates
     if FOld[0,0]>1 then FOld[0,0] := 1 else if FOld[0,0]<-1 then FOld[0,0] := -1;
     if FOld[0,1]>1 then FOld[0,1] := 1 else if FOld[0,1]<-1 then FOld[0,1] := -1;
     if FOld[1,0]>1 then FOld[1,0] := 1 else if FOld[1,0]<-1 then FOld[1,0] := -1;
     if FOld[1,1]>1 then FOld[1,1] := 1 else if FOld[1,1]<-1 then FOld[1,1] := -1;

     outputs[0,i] := f_Limit((FOutVol*((1-FMixWet)*inputs[0,i]+FMixWet*FOld[0,1])));
     outputs[1,i] := f_Limit((FOutVol*((1-FMixWet)*inputs[1,i]+FMixWet*FOld[1,1])));
    end
  end
 else
  begin
   for i := 0 to SampleFrames - 1 do
    begin
     FSHCounter := FSHCounter + FSHrate;
     if (FSHCounter > 1) then
      begin
       FSHCounter := FSHCounter-1;
       FYL := round(FBitMul * inputs[0,i]) * FBitDiv;
       FYR := round(FBitMul * inputs[1,i]) * FBitDiv;
      end;

     FOld[0, 0] := FOld[0, 0] + FCutoffFreqNorm * (FYL - FOld[0, 0] + FFC * (FOld[0, 0] - FOld[0, 1])) + CDenorm32;
     FOld[0, 1] := FOld[0, 1] + FCutoffFreqNorm * (FOld[0, 0] - FOld[0, 1]) + CDenorm32;
     FOld[1, 0] := FOld[1, 0] + FCutoffFreqNorm * (FYR - FOld[1, 0] + FFC*(FOld[1, 0] - FOld[1, 1])) + CDenorm32;
     FOld[1, 1] := FOld[1, 1] + FCutoffFreqNorm * (FOld[1, 0] - FOld[1, 1]) + CDenorm32;

     //limit coeffcients - not very elegant, but else filter
     //is particularly unstable with high resonance and low sample&hold rates
     if FOld[0, 0] > 1 then FOld[0, 0] := 1 else if FOld[0, 0] < -1 then FOld[0, 0] := -1;
     if FOld[0, 1] > 1 then FOld[0, 1] := 1 else if FOld[0, 1] < -1 then FOld[0, 1] := -1;
     if FOld[1, 0] > 1 then FOld[1, 0] := 1 else if FOld[1, 0] < -1 then FOld[1, 0] := -1;
     if FOld[1, 1] > 1 then FOld[1, 1] := 1 else if FOld[1, 1] < -1 then FOld[1, 1] := -1;

     outputs[0, i] := f_Limit((FOutVol * ((1 - FMixWet) * inputs[0, i] + FMixWet*(FYL - FOld[0, 1]))));
     outputs[1, i] := f_Limit((FOutVol * ((1 - FMixWet) * inputs[1, i] + FMixWet*(FYR - FOld[0, 1]))));
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
    70: Parameter[0] := FreqLinearToLog(CCData) * 2.205;
    71: Parameter[1] := 24 * CCData;
    72: Parameter[2] := FreqLinearToLog(CCData);
    73: Parameter[3] := 10 * CCData;
    74: Parameter[5] := 100 * CCData;
    75: Parameter[6] := 6 - 30 * CCData;
   end;
  end;
end;

end.
