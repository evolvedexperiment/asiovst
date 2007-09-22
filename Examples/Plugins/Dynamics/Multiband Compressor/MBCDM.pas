unit MBCDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, 
  DDSPBase, DVSTModule, DFilter, DButterworthFilter, DDynamics;

type
  TMultiband = record
    Lowpass      : TButterworthLP;
    LowComp      : TSimpleCompressor;
    MidHighpass  : TButterworthHP;
    MidComp      : TSimpleCompressor;
    MidLowpass   : TButterworthLP;
    Highpass     : TButterworthHP;
    HighComp     : TSimpleCompressor;
  end;

  TMBCDataModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TArrayOfSingleDynArray; sampleframes: Integer);
    procedure MBCDMLowFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure MBCDMHighFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TArrayOfDoubleDynArray; sampleframes: Integer);
    procedure MBCDCLowOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure MBCDCHighOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure MBCDMLowGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure MBCDMMidGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure MBCDMHighGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure MBCDMLowThresholdChange(Sender: TObject;
      const Index: Integer; var Value: Single);
    procedure MBCDMMidThresholdChange(Sender: TObject;
      const Index: Integer; var Value: Single);
    procedure MBCDMHighThresholdChange(Sender: TObject;
      const Index: Integer; var Value: Single);
    procedure MBCDMMidRatioChange(Sender: TObject;
      const Index: Integer; var Value: Single);
    procedure MBCDMLowRatioChange(Sender: TObject;
      const Index: Integer; var Value: Single);
    procedure MBCDMHighRatioChange(Sender: TObject;
      const Index: Integer; var Value: Single);
    procedure MBCDMLowAttackChange(Sender: TObject;
      const Index: Integer; var Value: Single);
    procedure MBCDMMidAttackChange(Sender: TObject;
      const Index: Integer; var Value: Single);
    procedure MBCDMHighAttackChange(Sender: TObject;
      const Index: Integer; var Value: Single);
    procedure MBCDMLowReleaseChange(Sender: TObject;
      const Index: Integer; var Value: Single);
    procedure MBCDMMidReleaseChange(Sender: TObject;
      const Index: Integer; var Value: Single);
    procedure MBCDMHighReleaseChange(Sender: TObject;
      const Index: Integer; var Value: Single);
  private
    fMultiband : Array [0..1] of TMultiband;
  public
  end;

implementation

{$R *.DFM}

uses
  MBCGUI;

procedure TMBCDataModule.VSTModuleCreate(Sender: TObject);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   begin
    Lowpass      := TButterworthLP.Create;
    LowComp      := TSimpleCompressor.Create;
    MidHighpass  := TButterworthHP.Create;
    MidComp      := TSimpleCompressor.Create;
    MidLowpass   := TButterworthLP.Create;
    Highpass     := TButterworthHP.Create;
    HighComp     := TSimpleCompressor.Create;
   end;
 Parameter[ 0] := 0;
 Parameter[ 1] := 200;
 Parameter[ 2] := 2;
 Parameter[ 3] := -6;
 Parameter[ 4] := 4;
 Parameter[ 5] := 0.01;
 Parameter[ 6] := 0.1;
 Parameter[ 7] := 0;
 Parameter[ 8] := -6;
 Parameter[ 9] := 4;
 Parameter[10] := 0.01;
 Parameter[11] := 0.1;
 Parameter[12] := 0;
 Parameter[13] := 6000;
 Parameter[14] := 2;
 Parameter[15] := -6;
 Parameter[16] := 4;
 Parameter[17] := 0.01;
 Parameter[18] := 0.1;
end;

procedure TMBCDataModule.VSTModuleDestroy(Sender: TObject);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   begin
    Lowpass.Free;
    LowComp.Free;
    MidHighpass.Free;
    MidComp.Free;
    MidLowpass.Free;
    Highpass.Free;
    HighComp.Free;
   end;
end;

procedure TMBCDataModule.MBCDMLowFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   begin
    Lowpass.Frequency := Value;
    MidHighpass.Frequency := Value;
   end;
end;

procedure TMBCDataModule.MBCDCLowOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   begin
    Lowpass.Order := round(Value);
    MidHighpass.Order := round(Value);
   end;
end;

procedure TMBCDataModule.MBCDMLowGainChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i]
   do Lowpass.Gain := Value;
end;

procedure TMBCDataModule.MBCDMMidGainChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i]
   do MidHighpass.Gain := Value;
end;

procedure TMBCDataModule.MBCDMHighGainChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i]
   do Highpass.Gain := Value;
end;

procedure TMBCDataModule.MBCDMLowThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   LowComp.Threshold := Value;
end;

procedure TMBCDataModule.MBCDMMidThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   MidComp.Threshold := Value;
end;

procedure TMBCDataModule.MBCDMHighThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   HighComp.Threshold := Value;
end;

procedure TMBCDataModule.MBCDMLowRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   LowComp.Ratio := 1 / Value;
end;

procedure TMBCDataModule.MBCDMMidRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   MidComp.Ratio := 1 / Value;
end;

procedure TMBCDataModule.MBCDMHighRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   HighComp.Ratio := 1 / Value;
end;

procedure TMBCDataModule.MBCDMLowAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   LowComp.Attack := Value;
end;

procedure TMBCDataModule.MBCDMMidAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   MidComp.Attack := Value;
end;

procedure TMBCDataModule.MBCDMHighAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   HighComp.Attack := Value;
end;

procedure TMBCDataModule.MBCDMLowReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   LowComp.Decay := Value;
end;

procedure TMBCDataModule.MBCDMMidReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   MidComp.Decay := Value;
end;

procedure TMBCDataModule.MBCDMHighReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   HighComp.Decay := Value;
end;

procedure TMBCDataModule.MBCDCHighOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   begin
    MidLowpass.Order := round(Value);
    Highpass.Order := round(Value);
   end;
end;

procedure TMBCDataModule.MBCDMHighFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   begin
    MidLowpass.Frequency := Value;
    Highpass.Frequency := Value;
   end;
end;

procedure TMBCDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm);
begin
  GUI := TFmMBC.Create(nil);
  (GUI As TFmMBC).MBCDataModule := Self;
end;

procedure TMBCDataModule.VSTModuleProcess(const Inputs,
  Outputs: TArrayOfSingleDynArray; sampleframes: Integer);
var i : Integer;
begin
 for i := 0 to sampleframes - 1 do
  begin
(*
   with fMultiband[0] do
    Outputs[0, i] := Lowpass.ProcessSample(Inputs[0, i]) +
                     MidHighpass.ProcessSample(Inputs[0, i]);
   with fMultiband[1] do
    Outputs[1, i] := Lowpass.ProcessSample(Inputs[1, i]) +
                     MidHighpass.ProcessSample(Inputs[1, i]);
*)
   with fMultiband[0] do
    Outputs[0, i] := LowComp.ProcessSample(Lowpass.ProcessSample(Inputs[0, i])) +
                     MidComp.ProcessSample(MidHighpass.ProcessSample(MidLowpass.ProcessSample(Inputs[0, i]))) -
                     HighComp.ProcessSample(Highpass.ProcessSample(Inputs[0, i]));
   with fMultiband[1] do
    Outputs[1, i] := LowComp.ProcessSample(Lowpass.ProcessSample(Inputs[1, i])) +
                     MidComp.ProcessSample(MidHighpass.ProcessSample(MidLowpass.ProcessSample(Inputs[0, i]))) -
                     HighComp.ProcessSample(Highpass.ProcessSample(Inputs[1, i]));
  end;
end;

procedure TMBCDataModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TArrayOfDoubleDynArray; sampleframes: Integer);
var i : Integer;
begin
 for i := 0 to sampleframes - 1 do
  begin
   with fMultiband[0] do
   Outputs[0, i] := Lowpass.ProcessSample(Inputs[0, i]) +
                    MidHighpass.ProcessSample(MidLowpass.ProcessSample(Inputs[0, i])) +
                    Highpass.ProcessSample(Inputs[0, i]);
   with fMultiband[1] do
   Outputs[1, i] := Lowpass.ProcessSample(Inputs[1, i]) +
                    MidHighpass.ProcessSample(MidLowpass.ProcessSample(Inputs[0, i])) +
                    Highpass.ProcessSample(Inputs[1, i]);
  end;
end;

end.