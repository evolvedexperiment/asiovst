unit mdaAmbienceDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms,
  DAVDCommon, DVSTModule;

const
  FeedBack = 0.8;

type
  TmdaAmbienceDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure ParamSizeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHFDampChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    fBuffers      : Array [0..3] of PAVDSingleFixedArray;
    fPos          : Integer;
    fFil, fDamp   : Single;
    fOutputFactor : Single;
    fDry, fWet    : Single;
    fRoomsize     : Double;
    fReady, fDen  : Boolean;
    procedure CalculateDryWet;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TmdaAmbienceDataModule.ParamHFDampChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Parameter[Index] <> Value then
  begin
   fDamp := 0.05 + 0.9 * 0.01 * Value;
  end;
end;

procedure TmdaAmbienceDataModule.ParamMixChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if Parameter[Index] <> Value
  then CalculateDryWet;
end;

procedure TmdaAmbienceDataModule.ParamOutputChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if Parameter[Index] <> Value then
  begin
   fOutputFactor := Power(10.0, Parameter[3] * 0.05);
   CalculateDryWet;
  end;
end;

procedure TmdaAmbienceDataModule.CalculateDryWet;
begin
 fDry := fOutputFactor - sqr(0.01 * Parameter[2]) * fOutputFactor;
 fWet := 0.01 * 0.8 * Parameter[2] * fOutputFactor;
end;

procedure TmdaAmbienceDataModule.ParamSizeChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  temp : Double;
begin
 if Parameter[Index] <> Value then
  begin
   temp := 0.025 + 0.2665 * Value;
   if (fRoomSize <> temp)
    then fReady := False;  //need to flush buffer
   fRoomSize := temp;
  end;
end;

procedure TmdaAmbienceDataModule.VSTModuleCreate(Sender: TObject);
begin
 GetMem(fBuffers[0], 1024);
 GetMem(fBuffers[1], 1024);
 GetMem(fBuffers[2], 1024);
 GetMem(fBuffers[3], 1024);

 fFil := 0.0;
 fDen := fPos = 0;

 VSTModuleSuspend(Sender);  // flush buffer

 //inits here!
 Parameter[0] := 0.7; //size
 Parameter[1] := 0.7; //hf
 Parameter[2] := 0.9; //mix
 Parameter[3] := 0.5; //output
end;

procedure TmdaAmbienceDataModule.VSTModuleDestroy(Sender: TObject);
begin
 if assigned(fBuffers[0]) then Dispose(fBuffers[0]);
 if assigned(fBuffers[1]) then Dispose(fBuffers[1]);
 if assigned(fBuffers[2]) then Dispose(fBuffers[2]);
 if assigned(fBuffers[3]) then Dispose(fBuffers[3]);
end;

procedure TmdaAmbienceDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  r    : Double;
  t, f,
  dmp,
  y, w : Double;
  i, p : Integer;
  d    : Array [0..3] of Integer;
begin
 f   := fFil;
 dmp := fDamp;
 y   := fDry;
 w   := fWet;
 p   := fPos;

 if (fReady = False)
  then VSTModuleSuspend(nil);

 d[0] := p + round(107 * fRoomSize) and 1023;
 d[1] := p + round(142 * fRoomSize) and 1023;
 d[2] := p + round(277 * fRoomSize) and 1023;
 d[3] := p + round(379 * fRoomSize) and 1023;

 for i := 0 to SampleFrames - 1 do
  begin
   f := f + dmp * (w * (inputs[0, i] + inputs[1, i]) - f);  //HF damping
   r := f;

   t := fBuffers[0]^[p];
   r := r - FeedBack * t;
   fBuffers[0]^[d[0]] := r; //allpass
   r := r + t;

   t := fBuffers[1]^[p];
   r := r - FeedBack * t;
   fBuffers[0]^[d[1]] := r; //allpass
   r := r + t;

   t := fBuffers[2]^[p];
   r := r - FeedBack * t;
   fBuffers[2]^[d[2]] := r; //allpass
   r := r + t;
   Outputs[0, i] := y * Inputs[0, i] + r - f; //left output

   t := fBuffers[3]^[p];
   r := r - FeedBack * t;
   fBuffers[3]^[d[3]] := r; //allpass
   r := r + t;
   Outputs[1, i] := y * Inputs[1, i] + r - f; //right output

   p    := (p    + 1) and 1023;
   d[0] := (d[0] + 1) and 1023;
   d[1] := (d[1] + 1) and 1023;
   d[2] := (d[2] + 1) and 1023;
   d[3] := (d[3] + 1) and 1023;
  end;

 fPos := p;
 if (abs(f) > 1E-10) then
  begin   //catch denormals
   fFil := f;
   fDen := False;
  end
 else
  begin
   fFil := 0;
   if fDen = False then
    begin
     fDen := True;
     VSTModuleSuspend(nil);
    end;
  end;
end;

procedure TmdaAmbienceDataModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  r    : Double;
  t, f,
  dmp,
  y, w : Double;
  i, p : Integer;
  d    : Array [0..3] of Integer;
begin
 f   := fFil;
 dmp := fDamp;
 y   := fDry;
 w   := fWet;
 p   := fPos;

 if (fReady = False)
  then VSTModuleSuspend(nil);

 d[0] := p + round(107 * fRoomSize) and 1023;
 d[1] := p + round(142 * fRoomSize) and 1023;
 d[2] := p + round(277 * fRoomSize) and 1023;
 d[3] := p + round(379 * fRoomSize) and 1023;

 for i := 0 to SampleFrames - 1 do
  begin
   f := f + dmp * (w * (inputs[0, i] + inputs[1, i]) - f);  //HF damping
   r := f;

   t := fBuffers[0]^[p];
   r := r - FeedBack * t;
   fBuffers[0]^[d[0]] := r; //allpass
   r := r + t;

   t := fBuffers[1]^[p];
   r := r - FeedBack * t;
   fBuffers[0]^[d[1]] := r; //allpass
   r := r + t;

   t := fBuffers[2]^[p];
   r := r - FeedBack * t;
   fBuffers[2]^[d[2]] := r; //allpass
   r := r + t;
   Outputs[0, i] := y * Inputs[0, i] + r - f; //left output

   t := fBuffers[3]^[p];
   r := r - FeedBack * t;
   fBuffers[3]^[d[3]] := r; //allpass
   r := r + t;
   Outputs[1, i] := y * Inputs[1, i] + r - f; //right output

   p    := (p    + 1) and 1023;
   d[0] := (d[0] + 1) and 1023;
   d[1] := (d[1] + 1) and 1023;
   d[2] := (d[2] + 1) and 1023;
   d[3] := (d[3] + 1) and 1023;
  end;

 fPos := p;
 if (abs(f) > 1E-10) then
  begin   //catch denormals
   fFil := f;
   fDen := False;
  end
 else
  begin
   fFil := 0;
   if fDen = False then
    begin
     fDen := True;
     VSTModuleSuspend(nil);
    end;
  end;
end;

procedure TmdaAmbienceDataModule.VSTModuleSuspend(Sender: TObject);
begin
 FillChar(fBuffers[0], 1024, 0);
 FillChar(fBuffers[1], 1024, 0);
 FillChar(fBuffers[2], 1024, 0);
 FillChar(fBuffers[3], 1024, 0);
 fReady := True;
end;

end.