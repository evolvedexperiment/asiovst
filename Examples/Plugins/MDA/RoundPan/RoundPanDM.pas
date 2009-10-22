unit RoundPanDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule;

type
  TRoundPanDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterAutoChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRateChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FBuffer  : Array [0..1] of PDAVSingleFixedArray;
    FSize    : Integer;
    FPhi     : Single;
    FDPhi    : Single;
  public
  end;

implementation

{$R *.DFM}

procedure TRoundPanDataModule.ParameterAutoChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
  if (Parameter[1] > 0.55)
   then FDPhi := 20 * (Parameter[1] - 0.55) / SampleRate else
  if (Parameter[1] < 0.45)
   then FDPhi := -20 * (0.45 - Parameter[1]) / SampleRate
   else FDPhi := 0;
end;

procedure TRoundPanDataModule.ParameterRateChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FPhi := (6.2831853 * (Parameter[0] - 0.5));
end;

procedure TRoundPanDataModule.VSTModuleOpen(Sender: TObject);
begin
(*
  //inits here!
  Parameter[0] = 0.5; //pan
  Parameter[1] = 0.8; //auto
*)

{
  FSize   := 1500;
  fBufpos := 0;
  GetMem(FBuffer[0], FSize * SizeOf(Single));
  GetMem(FBuffer[1], FSize * SizeOf(Single));
}

 VSTModuleSuspend(Sender);

 //calcs here!
 FPhi  := 0;
 FDPhi := (5 / SampleRate);
end;

procedure TRoundPanDataModule.VSTModuleClose(Sender: TObject);
begin
 // if assigned(FBuffer[0]) then Dispose(FBuffer[0]);
 // if assigned(FBuffer[1]) then Dispose(FBuffer[1]);
end;

procedure TRoundPanDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample     : Integer;
  a, ph, dph : Double;
const
  cHalf : Single = 0.5;
  cRoot : Single = 0.7854;
begin
 ph  := FPhi;
 dph := FDPhi;
 for Sample := 0 to SampleFrames - 1 do
  begin
   a := cHalf * (Inputs[0, Sample] + Inputs[1, Sample]); //process from here...

   Outputs[0, Sample] := (a * -sin((cHalf * ph) - cRoot)); // output
   Outputs[1, Sample] := (a *  sin((cHalf * ph) + cRoot));

   ph := ph + dph;
  end;
 if (ph < 0.0)
  then ph := ph + 4 * Pi else
 if (ph > 4 * Pi) then ph := ph - 4 * Pi;
 FPhi := ph;
end;

procedure TRoundPanDataModule.VSTModuleSuspend(Sender: TObject);
begin
 // FillChar(FBuffer[0], FSize * SizeOf(Single), 0);
 // FillChar(FBuffer[1], FSize * SizeOf(Single), 0);
end;

end.

(*

C Source:

void mdaRoundPan::getParameterDisplay(VstInt32 index, char *text)
{
  switch(index)
  {
    case 0: long2string((long)(360.0 * (Parameter[0] - 0.5)), text); break;
    case 1: long2string((long)(57.296 * dphi * SampleRate()), text); break;
  }
}

*)
