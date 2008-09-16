unit RoundPanDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule;

type
  TRoundPanDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure ParameterAutoChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRateChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleDestroy(Sender: TObject);
  private
    fBuffer  : Array [0..1] of PDAVSingleFixedArray;
    fSize    : Integer;
    fPhi     : Single;
    fDPhi    : Single;
  public
  end;

implementation

{$R *.DFM}

procedure TRoundPanDataModule.ParameterAutoChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
  if (Parameter[1] > 0.55)
   then fDPhi := 20 * (Parameter[1] - 0.55) / SampleRate else
  if (Parameter[1] < 0.45)
   then fDPhi := -20 * (0.45 - Parameter[1]) / SampleRate
   else fDPhi := 0;
end;

procedure TRoundPanDataModule.ParameterRateChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fPhi := (6.2831853 * (Parameter[0] - 0.5));
end;

procedure TRoundPanDataModule.VSTModuleCreate(Sender: TObject);
begin
(*
  //inits here!
  Parameter[0] = 0.5; //pan
  Parameter[1] = 0.8; //auto
*)

{
  fSize   := 1500;
  fBufpos := 0;
  GetMem(fBuffer[0], fSize * SizeOf(Single));
  GetMem(fBuffer[1], fSize * SizeOf(Single));
}

 VSTModuleSuspend(Sender);

 //calcs here!
 fPhi  := 0;
 fDPhi := (5 / SampleRate);
end;

procedure TRoundPanDataModule.VSTModuleDestroy(Sender: TObject);
begin
 // if assigned(fBuffer[0]) then Dispose(fBuffer[0]);
 // if assigned(fBuffer[1]) then Dispose(fBuffer[1]);
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
 ph  := fPhi;
 dph := fDPhi;
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
 fPhi := ph;
end;

procedure TRoundPanDataModule.VSTModuleSuspend(Sender: TObject);
begin
 // FillChar(fBuffer[0], fSize * SizeOf(Single), 0);
 // FillChar(fBuffer[1], fSize * SizeOf(Single), 0);
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
