unit AmpSimDM;

interface

{$DEFINE UseGUI}

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DSPFilter;

type
  TModelType = (mtDI, mtSpeakerSim, mtRadio, mtMesaBoogie1, mtMesaBoogie8,
    mtMarshall4x12, mtScoopedOutMetal);
  TComboDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamBiasChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamDriveChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHPFFreqChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHPFResonanceChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamModelChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamModelDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamNoiseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOutputChanged(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamProcessChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamProcessDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    fBufferSize     : Integer;
    fBufferPosition : Integer;
    fBuffer         : array [0..1] of PDAVSingleFixedArray;
    fMix            : array [0..1] of Double;
    fDelay          : array [0..1] of Integer;
    fHighPass       : array [0..1] of TSimpleHighpassFilter;
    fFilterState    : array [0..1, 0..4] of Double;
    fRndAmt         : Double;
    fLPF, fHPF      : Double;
    fTrim           : Double;
    fDrive, fClip   : Double;
    fBias           : Double;
    fStereo         : Boolean;
    fIsSoftClipping : Boolean;
    function FilterFreq(Frequency: Double): Double;
    function GetModelType: TModelType;
    procedure SetModelType(const Value: TModelType);
  protected
    procedure DriveChanged;
    procedure BiasChanged;
    procedure TrimChanged;
  public
    property ModelType: TModelType read GetModelType write SetModelType;
    property Stereo: Boolean read fStereo;
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_VSTEffect, AmpSimGUI, Controls;

procedure TComboDataModule.VSTModuleCreate(Sender: TObject);
begin
 fBufferSize := 1024;
 fBufferPosition := 0;
 GetMem(fBuffer[0], fBufferSize * SizeOf(Single));
 GetMem(fBuffer[1], fBufferSize * SizeOf(Single));
 fHighPass[0] := TSimpleHighpassFilter.Create;
 fHighPass[1] := TSimpleHighpassFilter.Create;

 {$IFDEF UseGUI}
 Flags := Flags + [effFlagsHasEditor];
 {$ENDIF}
end;

procedure TComboDataModule.VSTModuleDestroy(Sender: TObject);
begin
 Dispose(fBuffer[0]);
 Dispose(fBuffer[1]);
 FreeAndNil(fHighPass[0]);
 FreeAndNil(fHighPass[1]);
end;

procedure TComboDataModule.ParamProcessChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fStereo := Value > 0.5;
 {$IFDEF UseGUI}
 if Assigned(EditorForm) then
  with TFmCombo(EditorForm)
   do UpdateProcess;
 {$ENDIF}
end;

procedure TComboDataModule.ParamDriveChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 TrimChanged;
 DriveChanged;
 BiasChanged;
 {$IFDEF UseGUI}
 if Assigned(EditorForm) then
  with TFmCombo(EditorForm)
   do UpdateDrive;
 {$ENDIF}
end;

procedure TComboDataModule.ParamHPFResonanceChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fHighPass[0].Bandwidth := 1.1 - 0.01 * Parameter[6];
 fHighPass[1].Bandwidth := fHighPass[0].Bandwidth;
 {$IFDEF UseGUI}
 if Assigned(EditorForm) then
  with TFmCombo(EditorForm)
   do UpdateReso;
 {$ENDIF}
end;

procedure TComboDataModule.ParamBiasChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 BiasChanged;
 {$IFDEF UseGUI}
 if Assigned(EditorForm) then
  with TFmCombo(EditorForm)
   do UpdateBias;
 {$ENDIF}
end;

function TComboDataModule.FilterFreq(Frequency: Double): Double;
var
  j, k, r : Double;
begin
 r := 0.999;
 j := sqr(r) - 1;
 k := 2 - 2 * sqr(r) * cos(0.647 * Frequency / SampleRate);
 result := (sqrt(sqr(k) - 4 * sqr(j)) - k) / (2 * j);
end;

function TComboDataModule.GetModelType: TModelType;
begin
 result := TModelType(round(ParameterByName['Model']));
end;

procedure TComboDataModule.SetModelType(const Value: TModelType);
begin
 case Value of
  mtDI:
    begin
     fLPF      := 0;
     fMix[0]   := 0;
     fMix[1]   := 0;
     fDelay[0] := 0;
     fDelay[1] := 0;
     fHPF      := filterFreq(25);
    end;

  mtSpeakerSim:
    begin
     fLPF      := filterFreq(2700);
     fMix[0]   := 0;
     fMix[1]   := 0;
     fDelay[0] := 0;
     fDelay[1] := 0;
     fHPF      := filterFreq(382);
    end;

  mtRadio:
    begin
     fLPF      := filterFreq(1685);
     fMix[0]   := -1.7;
     fMix[1]   := 0.82;
     fDelay[0] := round(SampleRate * 1.5276504735716468072105102352582E-4);
     fDelay[1] := round(SampleRate * 2.3174971031286210892236384704519E-4);
     fHPF      := filterFreq(25);
    end;

  mtMesaBoogie1:
    begin
     fLPF      := filterFreq(1385);
     fMix[0]   := -0.53;
     fMix[1]   := 0.21;
     fDelay[0] := round(SampleRate * 1.361470388019060585432266848196E-4);
     fDelay[1] := round(SampleRate * 8.382229673093042749371332774518E-4);
     fHPF      := filterFreq(25);
    end;

  mtMesaBoogie8:
    begin
     fLPF      := filterFreq(1685);
     fMix[0]   := -0.85;
     fMix[1]   := 0.41;
     fDelay[0] := round(SampleRate * 1.5276504735716468072105102352582E-4);
     fDelay[1] := round(SampleRate * 3.0165912518853695324283559577677E-4);
     fHPF      := filterFreq(25);
    end;

  mtMarshall4x12:
    begin
     fLPF      := filterFreq(2795);
     fMix[0]   := -0.29;
     fMix[1]   := 0.38;
     fDelay[0] := round(SampleRate * 1.0183299389002036659877800407332E-3);
     fDelay[1] := round(SampleRate * 4.1631973355537052456286427976686E-4);
     fHPF      := filterFreq(459);
    end;

  mtScoopedOutMetal:
    begin
     fLPF      := filterFreq(1744);
     fMix[0]   := -0.96;
     fMix[1]   := 1.6;
     fDelay[0] := round(SampleRate * 2.8089887640449438202247191011236E-3);
     fDelay[1] := round(SampleRate * 7.9176563737133808392715756136184E-4);
     fHPF      := filterFreq(382);
    end;
 end;
end;

procedure TComboDataModule.ParamModelChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 SetModelType(TModelType(round(Value)));
 TrimChanged;
 {$IFDEF UseGUI}
 if Assigned(EditorForm) then
  with TFmCombo(EditorForm)
   do UpdateModel;
 {$ENDIF}
end;

procedure TComboDataModule.ParamHPFFreqChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fHighPass[0].Frequency := Value;
 fHighPass[1].Frequency := fHighPass[0].Frequency;
 DriveChanged;
 {$IFDEF UseGUI}
 if Assigned(EditorForm) then
  with TFmCombo(EditorForm)
   do UpdateFreq;
 {$ENDIF}
end;

procedure TComboDataModule.ParamNoiseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fRndAmt := dB_to_Amp(Value);
end;

procedure TComboDataModule.DriveChanged;
begin
 fIsSoftClipping := Parameter[1] < 0;

 if fIsSoftClipping
  then fDrive := Power(10, -(1 + 0.03 * Parameter[1]))  // soft clipping
  else                                                  // hard clipping
   begin
    fDrive := 1;
    fClip  := 3.7 - 0.08 * Parameter[1];
    if Parameter[1] > 40 then
     begin
      fDrive := Power(10, 0.035 * Parameter[1] - 1.4);
      fClip  := 0.5;
     end;
   end;

 if (Parameter[5] > 0.05)
  then fDrive := fDrive * (1 + 0.1 * fDrive);
end;

procedure TComboDataModule.BiasChanged;
begin
 fBias := 6 * Parameter[2] / (1000 + abs(1.5 * Parameter[1]));
end;

procedure TComboDataModule.TrimChanged;
begin
 case round(Parameter[0]) of
  0: fTrim := 0.50;   // DI
  1: fTrim := 0.53;   // speaker sim
  2: fTrim := 1.10;   // radio
  3: fTrim := 0.98;   // mesa boogie 1"
  4: fTrim := 0.96;   // mesa boogie 8"
  5: fTrim := 0.59;   // Marshall 4x12" celestion
  6: fTrim := 0.30;   // scooped-out metal
 end;

 if fIsSoftClipping
  then fTrim := fTrim * (0.55 + 150 * IntPower((0.5 + 0.005 * Parameter[1]), 4));

 fTrim := fTrim * Power(10, Parameter[3] * 0.05);
 if fStereo then fTrim := fTrim * 2;
end;

procedure TComboDataModule.ParamModelDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0 : PreDefined := 'D.I.';
  1 : PreDefined := 'Spkr Sim';
  2 : PreDefined := 'Radio';
  3 : PreDefined := 'MB 1"';
  4 : PreDefined := 'MB 8"';
  5 : PreDefined := '4x12 ^';
  6 : PreDefined := '4x12 >';
 end;
end;

procedure TComboDataModule.ParamOutputChanged(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 TrimChanged;
 {$IFDEF UseGUI}
 if Assigned(EditorForm) then
  with TFmCombo(EditorForm)
   do UpdateOutput;
 {$ENDIF}
end;

procedure TComboDataModule.ParamProcessDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] > 0.5
  then PreDefined := 'STEREO'
  else PreDefined := 'MONO';
end;

procedure TComboDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TFmCombo.Create(Self);
end;

procedure TComboDataModule.VSTModuleOpen(Sender: TObject);
begin
 Parameter[0] := 0;
 Parameter[1] := 0;
 Parameter[2] := 0;
 Parameter[3] := 0;
 Parameter[4] := 0;
 Parameter[5] := 100;
 Parameter[6] := 0;
 Parameter[7] := -75;
 with Programs[0] do
  begin
   Parameter[0] := 0;
   Parameter[1] := 0;
   Parameter[2] := 0;
   Parameter[3] := 0;
   Parameter[4] := 0;
   Parameter[5] := 100;
   Parameter[6] := 0;
   Parameter[7] := -75;
  end;
 with Programs[1] do
  begin
   Parameter[0] := 3;
   Parameter[1] := 43.5;
   Parameter[2] := 41;
   Parameter[3] := 3.9;
   Parameter[4] := 0;
   Parameter[5] := 565;
   Parameter[6] := 9;
   Parameter[7] := -75;
  end;
end;

procedure TComboDataModule.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  InP, OutP    : Array [0..1] of Double;
  trm, clp     : Single;
  LPF, bi      : Single;
  HPF, drv     : Single;
  FilterState  : Array [0..1, 0..4] of Double;
  d            : Array [0..1] of Integer;
  m            : Array [0..1] of Single;
  bp, Sample   : Integer;
begin
 m[0] := fMix[0];
 m[1] := fMix[1];
 clp  := fClip;
 LPF  := fLPF;
 HPF  := fHPF;
 bi   := fBias - 0.0001;
 drv  := fDrive;
 FilterState[0, 0] := fFilterState[0, 0];
 FilterState[0, 1] := fFilterState[0, 1];
 FilterState[0, 2] := fFilterState[0, 2];
 FilterState[0, 3] := fFilterState[0, 3];
 FilterState[0, 4] := fFilterState[0, 4];
 FilterState[1, 0] := fFilterState[1, 0];
 FilterState[1, 1] := fFilterState[1, 1];
 FilterState[1, 2] := fFilterState[1, 2];
 FilterState[1, 3] := fFilterState[1, 3];
 FilterState[1, 4] := fFilterState[1, 4];
 d[0] := fDelay[0];
 d[1] := fDelay[1];
 bp   := fBufferPosition;
 trm  := fTrim * sqr(sqr(1 - LPF));

 if fStereo then //stereo
  begin
   for  Sample := 0 to SampleFrames - 1 do
    begin
     InP[0] := fHighPass[0].ProcessSample(drv * (fRndAmt * random + Inputs[0, Sample] + bi));
     InP[1] := fHighPass[1].ProcessSample(drv * (fRndAmt * random + Inputs[1, Sample] + bi));

     if fIsSoftClipping then
      begin
       OutP[0] := InP[0] / (1 + abs(InP[0]));
       OutP[1] := InP[1] / (1 + abs(InP[1]));
      end
     else
      begin
       if InP[0] > clp        then OutP[0] := clp
        else if InP[0] < -clp then OutP[0] := -clp
        else OutP[0] := InP[0];
       if InP[1] > clp        then OutP[1] := clp
        else if InP[1] < -clp then OutP[1] := -clp
        else OutP[1] := InP[1];
      end;

     fBuffer[0]^[bp] := OutP[0];
     fBuffer[1]^[bp] := OutP[1];
     OutP[0] := OutP[0] + (m[0] * fBuffer[0]^[(bp + d[0]) mod 1000]) +
                          (m[1] * fBuffer[0]^[(bp + d[1]) mod 1000]);
     OutP[1] := OutP[1] + (m[0] * fBuffer[1]^[(bp + d[0]) mod 1000]) +
                          (m[1] * fBuffer[1]^[(bp + d[1]) mod 1000]);

     FilterState[0, 0] := LPF * FilterState[0, 0] + trm * OutP[0];
     FilterState[1, 0] := LPF * FilterState[1, 0] + trm * OutP[1];
     FilterState[0, 1] := LPF * FilterState[0, 1] + FilterState[0, 0];
     FilterState[1, 1] := LPF * FilterState[1, 1] + FilterState[1, 0];
     FilterState[0, 2] := LPF * FilterState[0, 2] + FilterState[0, 1];
     FilterState[1, 2] := LPF * FilterState[1, 2] + FilterState[1, 1];
     FilterState[0, 3] := LPF * FilterState[0, 3] + FilterState[0, 2];
     FilterState[1, 3] := LPF * FilterState[1, 3] + FilterState[1, 2];  //-24dB/oct filter

     FilterState[0, 4] := HPF * (FilterState[0, 4] - FilterState[0, 3]) + FilterState[0, 3];
     FilterState[1, 4] := HPF * (FilterState[1, 4] - FilterState[1, 3]) + FilterState[1, 3];  //high pass

     OutP[0] := FilterState[0, 3] - FilterState[0, 4];
     OutP[1] := FilterState[1, 3] - FilterState[1, 4];

     if (bp = 0)
      then bp := 999
      else bp := bp - 1;

     Outputs[0, Sample] := Outp[0];
     Outputs[1, Sample] := Outp[1];
    end;
  end
 else //mono
  begin
   if fIsSoftClipping then //soft clip
    begin
     for Sample := 0 to SampleFrames - 1 do
      begin
       InP[0] := fHighPass[0].ProcessSample(drv * (fRndAmt * random + Inputs[0, Sample] + Inputs[1, Sample] + bi));

       OutP[0] := InP[0] / (1 + abs(InP[0]));

       fBuffer[0]^[bp] := OutP[0];
       OutP[0] := OutP[0] + (m[0] * fBuffer[0]^[(bp + d[0]) mod 1000]) +
                            (m[1] * fBuffer[0]^[(bp + d[1]) mod 1000]);

       FilterState[0, 0] := LPF * FilterState[0, 0] + trm * OutP[0];
       FilterState[0, 1] := LPF * FilterState[0, 1] + FilterState[0, 0];
       FilterState[0, 2] := LPF * FilterState[0, 2] + FilterState[0, 1];
       FilterState[0, 3] := LPF * FilterState[0, 3] + FilterState[0, 2]; //-24dB/oct filter

       FilterState[0, 4] := HPF * (FilterState[0, 4] - FilterState[0, 3]) + FilterState[0, 3]; //high pass
       OutP[0] := FilterState[0, 3] - FilterState[0, 4];

       if (bp = 0)
        then bp := 999
        else bp := bp - 1; //buffer position

       Outputs[0, Sample] := Outp[0];
       Outputs[1, Sample] := Outp[0];
      end;
    end
   else //hard clip
    begin
     for Sample := 0 to SampleFrames - 1 do
      begin
       InP[0] := fHighPass[0].ProcessSample(drv * (fRndAmt * random + Inputs[0, Sample] + Inputs[1, Sample] + bi));

       if InP[0] > clp        then OutP[0] :=  clp
        else if InP[0] < -clp then OutP[0] := -clp
        else OutP[0] := InP[0];

       fBuffer[0]^[bp] := OutP[0];
       OutP[0] := OutP[0] + (m[0] * fBuffer[0]^[(bp + d[0]) mod 1000]) +
                            (m[1] * fBuffer[0]^[(bp + d[1]) mod 1000]);

       FilterState[0, 0] := LPF * FilterState[0, 0] + trm * OutP[0];
       FilterState[0, 1] := LPF * FilterState[0, 1] + FilterState[0, 0];
       FilterState[0, 2] := LPF * FilterState[0, 2] + FilterState[0, 1];
       FilterState[0, 3] := LPF * FilterState[0, 3] + FilterState[0, 2]; //-24dB/oct filter

       FilterState[0, 4] := HPF * (FilterState[0, 4] - FilterState[0, 3]) + FilterState[0, 3]; //high pass //also want smile curve here...
       OutP[0] := FilterState[0, 3] - FilterState[0, 4];

       if (bp = 0)
        then bp := 999
        else bp := bp - 1; //buffer position

       Outputs[0, Sample] := Outp[0];
       Outputs[1, Sample] := Outp[0];
      end;
    end;
  end;
 fBufferPosition := bp;
 if (abs(FilterState[0, 0]) < 1E-10)
  then FillChar(fFilterState[0, 0], 5 * SizeOf(Double), 0)
  else Move(FilterState[0, 0], fFilterState[0, 0], 5 * SizeOf(Double));
 if (abs(FilterState[1, 0]) < 1E-10) or (not fStereo)
  then FillChar(fFilterState[1, 0], 5 * SizeOf(Double), 0)
  else Move(FilterState[1, 0], fFilterState[1, 0], 5 * SizeOf(Double));

end;

procedure TComboDataModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  InP, OutP    : Array [0..1] of Double;
  trm, clp     : Double;
  LPF, bi      : Double;
  HPF, drv     : Double;
  FilterState  : Array [0..1, 0..4] of Double;
  d            : Array [0..1] of Integer;
  m            : Array [0..1] of Single;
  bp, Sample   : Integer;
begin
 m[0] := fMix[0];
 m[1] := fMix[1];
 clp  := fClip;
 LPF  := fLPF;
 HPF  := fHPF;
 bi   := fBias;
 drv  := fDrive;
 FilterState[0, 0] := fFilterState[0, 0];
 FilterState[0, 1] := fFilterState[0, 1];
 FilterState[0, 2] := fFilterState[0, 2];
 FilterState[0, 3] := fFilterState[0, 3];
 FilterState[0, 4] := fFilterState[0, 4];
 FilterState[1, 0] := fFilterState[1, 0];
 FilterState[1, 1] := fFilterState[1, 1];
 FilterState[1, 2] := fFilterState[1, 2];
 FilterState[1, 3] := fFilterState[1, 3];
 FilterState[1, 4] := fFilterState[1, 4];

 d[0] := fDelay[0];
 d[1] := fDelay[1];
 bp   := fBufferPosition;
 trm  := fTrim * sqr(sqr(1 - LPF));

 if fStereo then //stereo
  begin
   for  Sample := 0 to SampleFrames - 1 do
    begin
     InP[0] := drv * (Inputs[0, Sample] + bi);
     InP[1] := drv * (Inputs[1, Sample] + bi);

      if fIsSoftClipping then
       begin
        OutP[0] := InP[0] / (1 + abs(InP[0]));
        OutP[1] := InP[1] / (1 + abs(InP[1]));
       end
      else
       begin
        if InP[0] > clp        then OutP[0] := clp
         else if InP[0] < -clp then OutP[0] := -clp
         else OutP[0] := InP[0];
        if InP[1] > clp        then OutP[1] := clp
         else if InP[1] < -clp then OutP[1] := -clp
         else OutP[1] := InP[1];
       end;

      fBuffer[0]^[bp] := OutP[0];
      fBuffer[1]^[bp] := OutP[1];
      OutP[0] := OutP[0] + (m[0] * fBuffer[0]^[(bp + d[0]) mod 1000]) +
                           (m[1] * fBuffer[0]^[(bp + d[1]) mod 1000]);
      OutP[1] := OutP[1] + (m[0] * fBuffer[1]^[(bp + d[0]) mod 1000]) +
                           (m[1] * fBuffer[1]^[(bp + d[1]) mod 1000]);

      FilterState[0, 0] := LPF * FilterState[0, 0] + trm * OutP[0];
      FilterState[1, 0] := LPF * FilterState[1, 0] + trm * OutP[1];
      FilterState[0, 1] := LPF * FilterState[0, 1] + FilterState[0, 0];
      FilterState[1, 1] := LPF * FilterState[1, 1] + FilterState[1, 0];
      FilterState[0, 2] := LPF * FilterState[0, 2] + FilterState[0, 1];
      FilterState[1, 2] := LPF * FilterState[1, 2] + FilterState[1, 1];
      FilterState[0, 3] := LPF * FilterState[0, 3] + FilterState[0, 2];
      FilterState[1, 3] := LPF * FilterState[1, 3] + FilterState[1, 2];  //-24dB/oct filter

      FilterState[0, 4] := HPF * (FilterState[0, 4] - FilterState[0, 3]) + FilterState[0, 3];
      FilterState[1, 4] := HPF * (FilterState[1, 4] - FilterState[1, 3]) + FilterState[1, 3];  //high pass

      OutP[0] := FilterState[0, 3] - FilterState[0, 4];
      OutP[1] := FilterState[1, 3] - FilterState[0, 4];

      if (bp = 0)
       then bp := 999
       else bp := bp - 1;

      Outputs[0, Sample] := Outp[0];
      Outputs[1, Sample] := Outp[1];
    end;
  end
 else //mono
  begin
   if fIsSoftClipping then //soft clip
    begin
     for Sample := 0 to SampleFrames - 1 do
      begin
       InP[0] := fHighPass[0].ProcessSample(drv * (Inputs[0, Sample] + Inputs[1, Sample] + bi));

       OutP[0] := InP[0] / (1 + abs(InP[0]));

       fBuffer[0]^[bp] := OutP[0];
       OutP[0] := OutP[0] + (m[0] * fBuffer[0]^[(bp + d[0]) mod 1000]) +
                            (m[1] * fBuffer[0]^[(bp + d[1]) mod 1000]);

       FilterState[0, 0] := LPF * FilterState[0, 0] + trm * OutP[0];
       FilterState[0, 1] := LPF * FilterState[0, 1] + FilterState[0, 0];
       FilterState[0, 2] := LPF * FilterState[0, 2] + FilterState[0, 1];
       FilterState[0, 3] := LPF * FilterState[0, 3] + FilterState[0, 2]; //-24dB/oct filter

       FilterState[0, 4] := HPF * (FilterState[0, 4] - FilterState[0, 3]) + FilterState[0, 3]; //high pass
       OutP[0] := FilterState[0, 3] - FilterState[0, 4];

       if (bp = 0)
        then bp := 999
        else bp := bp - 1; //buffer position

       Outputs[0, Sample] := Outp[0];
       Outputs[1, Sample] := Outp[1];
      end;
    end
   else //hard clip
    begin
     for Sample := 0 to SampleFrames - 1 do
      begin
       InP[0] := fHighPass[0].ProcessSample(drv * (Inputs[0, Sample] + Inputs[1, Sample] + bi));

       if InP[0] > clp        then OutP[0] :=  clp
        else if InP[0] < -clp then OutP[0] := -clp
        else OutP[0] := InP[0];

       fBuffer[0]^[bp] := OutP[0];
       OutP[0] := OutP[0] + (m[0] * fBuffer[0]^[(bp + d[0]) mod 1000]) +
                            (m[1] * fBuffer[0]^[(bp + d[1]) mod 1000]);

       FilterState[0, 0] := LPF * FilterState[0, 0] + trm * OutP[0];
       FilterState[0, 1] := LPF * FilterState[0, 1] + FilterState[0, 0];
       FilterState[0, 2] := LPF * FilterState[0, 2] + FilterState[0, 1];
       FilterState[0, 3] := LPF * FilterState[0, 3] + FilterState[0, 2]; //-24dB/oct filter

       FilterState[0, 4] := HPF * (FilterState[0, 4] - FilterState[0, 3]) + FilterState[0, 3]; //high pass //also want smile curve here...
       OutP[0] := FilterState[0, 3] - FilterState[0, 4];

       if (bp = 0)
        then bp := 999
        else bp := bp - 1; //buffer position

       Outputs[0, Sample] := Outp[0];
       Outputs[1, Sample] := Outp[1];
      end;
    end;
  end;
 fBufferPosition := bp;
 if (abs(FilterState[0, 0]) < 1E-10)
  then FillChar(fFilterState[0, 0], 5 * SizeOf(Double), 0)
  else Move(FilterState[0, 0], fFilterState[0, 0], 5 * SizeOf(Double));
 if (abs(FilterState[1, 0]) < 1E-10) or (not fStereo)
  then FillChar(fFilterState[1, 0], 5 * SizeOf(Double), 0)
  else Move(FilterState[1, 0], fFilterState[1, 0], 5 * SizeOf(Double));
end;

procedure TComboDataModule.VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
begin
 fHighPass[0].SampleRate := SampleRate;
 fHighPass[1].SampleRate := SampleRate;
 ModelType := ModelType;
end;

procedure TComboDataModule.VSTModuleSuspend(Sender: TObject);
begin
 FillChar(fBuffer[0]^[0], fBufferSize * SizeOf(Single), 0);
 FillChar(fBuffer[1]^[0], fBufferSize * SizeOf(Single), 0);
 FillChar(fFilterState[0, 0], 10 * SizeOf(Double), 0);
end;

end.
