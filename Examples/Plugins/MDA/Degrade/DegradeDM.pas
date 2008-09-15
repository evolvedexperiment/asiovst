unit DegradeDM;

interface

uses
  Windows, Messages, SysUtils, Classes, DAV_Common, DAV_VSTModule;

type
  TDegradeDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterNonLinearChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterPostFilterChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    fBuffer : array [0..9] of Single;
    fLin    : Single;
    fLin2   : Single;
    fClip   : Single;
    fFreq   : Single;
    fMode   : Integer;
    fTCount : Integer;
    fGain   : array [0..2] of Single;
    fo2     : Single;
    fi2     : Single;
    tn      : Integer;
    function filterFreq(Hz: Single): Single;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TDegradeDataModule.VSTModuleCreate(Sender: TObject);
begin
 //inits here!
 Parameter[0] := 0.8;  // Clip
 Parameter[1] := 0.50; // Bits
 Parameter[2] := 0.65; // Rate
 Parameter[3] := 0.9;  // Postfilt
 Parameter[4] := 0.58; // Non-lin
 Parameter[5] := 0.5;  // Level

 FillChar(fBuffer[0], SizeOf(fBuffer), 0);
end;

procedure TDegradeDataModule.ParameterNonLinearChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if (Value > 0.5) then
  begin
   fLin  := Power(10, 0.3 * (0.5 - Value));
   fLin2 := fLin;
  end
 else
  begin
   fLin  := Power(10, 0.3 * (Value - 0.5));
   fLin2 := 1;
  end;
end;

procedure TDegradeDataModule.ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fGain[2] := Power(10, 2 * Value - 1.0);
end;

procedure TDegradeDataModule.ParameterPostFilterChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fo2 := filterFreq(Power(10, 2.30104 + 2 * Value));
 fi2 := sqr(sqr(1 - fo2));
end;

function TDegradeDataModule.filterFreq(Hz: Single): Single;
var
  j, k, r : Single;
begin
 r := 0.999;
 j := sqr(r) - 1;
 k := (2 - 2 * sqr(r) * cos(0.647 * Hz / SampleRate));
 result := (sqrt(k * k - 4 * j * j) - k) / (2 * j);
end;

procedure TDegradeDataModule.VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  g1 : Single;
begin
 //calcs here
 if (Parameter[2] > 0.5) then
  begin
   fFreq := Parameter[2] - 0.5;
   fMode := 1;
  end
 else
  begin
   fFreq := 0.5 - Parameter[2];
   fMode := 0;
  end;

 tn := round(exp(18 * fFreq));

 fTCount := 1;
 fClip   := Power(10, (-1.5 + 1.5 * Parameter[0]));

 g1      := Power(2, 2 + round(Parameter[1] * 12));
 fGain[1] := 1 / (2 * g1);
 if (Parameter[2] > 0.5)
  then fGain[0] := -g1 / tn
  else fGain[0] := -g1;
end;

procedure TDegradeDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample     : Integer;
  l, l2      : Single;
  cl, i2, o2 : Single;
  b          : array [0..9] of Single;
  gi, go     : Single;
  ga, m      : Single;
  n, t       : Integer;
begin
 b[0] := fBuffer[0];
 b[1] := fBuffer[1];
 b[2] := fBuffer[2];
 b[3] := fBuffer[3];
 b[4] := fBuffer[4];
 b[5] := fBuffer[5];
 b[6] := fBuffer[6];
 b[7] := fBuffer[7];
 b[8] := fBuffer[8];
 b[9] := fBuffer[9];
 l    := fLin;
 l2   := fLin2;
 cl   := fClip;
 m    := fMode;
 i2   := fi2;
 o2   := fo2;
 gi   := fGain[0];
 go   := fGain[1];
 ga   := fGain[2];
 n    := tn;

 t    := fTCount;
 for Sample := 0 to SampleFrames - 1 do
  begin
   b[0] := (Inputs[0, Sample] + Inputs[1, Sample]) + m * b[0];

   if (t = n) then
    begin
     t  := 0;
     b[5] := (go * int(b[0] * gi));
     if (b[5] > 0)
      then begin b[5] :=  Power( b[5], l2); if (b[5] > cl) then b[5] :=  cl; end
      else begin b[5] := -Power(-b[5],  l); if (b[5] <-cl) then b[5] := -cl; end;
     b[0] := 0;
    end;
   t := t + 1;

   b[1] := i2 * (b[5] * ga) + o2 * b[1];
   b[2] :=      b[1] + o2 * b[2];
   b[3] :=      b[2] + o2 * b[3];
   b[4] :=      b[3] + o2 * b[4];
   b[6] := i2 * b[4] + o2 * b[6];
   b[7] :=      b[6] + o2 * b[7];
   b[8] :=      b[7] + o2 * b[8];
   b[9] :=      b[8] + o2 * b[9];

   Outputs[0, Sample] := b[9];
   Outputs[1, Sample] := b[9];
  end;

 if (abs(b[1]) < 1E-10) then 
  begin
   fBuffer[0] := 0;
   fBuffer[1] := 0;
   fBuffer[2] := 0;
   fBuffer[3] := 0;
   fBuffer[4] := 0;
   fBuffer[5] := 0;
   fBuffer[6] := 0;
   fBuffer[7] := 0;
   fBuffer[8] := 0;
   fBuffer[9] := 0;
  end
 else
  begin
   fBuffer[0] := b[0];
   fBuffer[1] := b[1];
   fBuffer[2] := b[2];
   fBuffer[3] := b[3];
   fBuffer[4] := b[4];
   fBuffer[5] := b[5];
   fBuffer[6] := b[6];
   fBuffer[7] := b[7];
   fBuffer[8] := b[8];
   fBuffer[9] := b[9];
   fTCount := t;
  end;
end;

end.
