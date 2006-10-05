unit DASIOGenerator;
{$I JEDI.INC}

interface

uses
  Windows, Messages, Classes, SysUtils, DDSPBase;

{-$I DSPTYPES.INC}
type
  TNoiseColor = (ncWhite, ncPink, ncBrown);
  TToneFlavor = (tfSine, tfSaw, tfSquare, tfTriangle, tfPulse);
  TASIOGenerator = class(TComponent)
  private
    fSampleRate  : Double;
    fBlockSize   : Cardinal;
    procedure SetSampleRate(const Value: Double);
    procedure SetBlockSize(const Value: Cardinal);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ProcessBlock(SingleBlock: TArrayOfSingleArray; isOutput: boolean = true); virtual;
  published
    property SampleRate: Double read fSampleRate write SetSampleRate;
    property BlockSize: Cardinal read fBlockSize write SetBlockSize;
  end;

  TASIOGeneratorNoise = class(TASIOGenerator)
  private
   NoiZe : Array[0..7] of Single;
   fNoiseColor: TNoiseColor;
  public
   procedure ProcessBlock(SingleBlock:TArrayOfSingleArray; isOutput: boolean = true); override;
  published
   property NoiseColor: TNoiseColor read fNoiseColor write fNoiseColor;
  end;

  TASIOGeneratorTone = class(TASIOGenerator)
  private
   fFrequency  : Single;
   fToneFlavor : TToneFlavor;
   fPhase      : Double;
   procedure SetFrequency(value: Single);
  public
   constructor Create(AOwner: TComponent); override;
   procedure ProcessBlock(SingleBlock: TArrayOfSingleArray; isOutput: boolean = true); override;
  published
   property Frequency: Single read fFrequency write SetFrequency;
   property ToneFlavor:TToneFlavor read fToneFlavor write fToneFlavor;
  end;

procedure Register;

implementation

uses Math, DASIOHost;

constructor TASIOGenerator.Create(AOwner: TComponent);
begin
 fBlocksize := 2048;
 fSampleRate := 44100;
 inherited;
end;

destructor TASIOGenerator.Destroy;
begin
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TASIOGenerator.ProcessBlock(SingleBlock: TArrayOfSingleArray; isOutput: boolean = true);
begin
 FillChar(SingleBlock[0], BlockSize * SizeOf(Single), 0);
end;

procedure TASIOGenerator.SetSampleRate(const Value: Double);
begin
 fSampleRate := Value;
end;

procedure TASIOGenerator.SetBlockSize(const Value: Cardinal);
begin
 fBlockSize := Value;
end;

procedure TASIOGeneratorNoise.ProcessBlock(SingleBlock: TArrayOfSingleArray;
 isOutput: boolean = true);
var i, j : integer;
begin
 if NoiseColor = ncWhite then
  for i := 0 to Length(SingleBlock) - 1 do
   for j := 0 to Blocksize - 1 do
    SingleBlock[i, j] := (2 * Random - 1)
 else if NoiseColor = ncPink then
  for i := 0 to Length(SingleBlock) - 1 do
   for j := 0 to Blocksize - 1 do
    begin
     NoiZe[0]:=(2 * Random - 1);
     NoiZe[1]:= 0.99886 * NoiZe[1] + NoiZe[0] * 0.0555179;
     NoiZe[2]:= 0.99332 * NoiZe[2] + NoiZe[0] * 0.0750759;
     NoiZe[3]:= 0.96900 * NoiZe[3] + NoiZe[0] * 0.1538520;
     NoiZe[4]:= 0.86650 * NoiZe[4] + NoiZe[0] * 0.3104856;
     NoiZe[5]:= 0.55000 * NoiZe[5] + NoiZe[0] * 0.5329522;
     NoiZe[6]:= -0.7616 * NoiZe[6] - NoiZe[0] * 0.0168980;
     SingleBlock[i,j] := 0.1 * (NoiZe[0] + NoiZe[1] + NoiZe[2] + NoiZe[3] + NoiZe[4] + NoiZe[5] + NoiZe[6] + NoiZe[7] * 0.5362);   NoiZe[7]:= NoiZe[0] * 0.115926;
    end
 else if NoiseColor = ncBrown then
  for i := 0 to Length(SingleBlock) - 1 do
   for j := 0 to Blocksize - 1 do
    begin
     SingleBlock[i, j] := NoiZe[i] + 0.0002 * (2 * Random - 1);
     NoiZe[i] := 0.999 * SingleBlock[i, j];
     SingleBlock[i, j] := 150 * SingleBlock[i, j];
   end;
end;

procedure TASIOGeneratorTone.SetFrequency(value:Single);
begin
 fFrequency := value;
end;

constructor TASIOGeneratorTone.Create(AOwner: TComponent);
begin
 inherited;
 fFrequency := 1000;
end;

procedure TASIOGeneratorTone.ProcessBlock(SingleBlock: TArrayOfSingleArray;
 isOutput: boolean = true);
var i, j : Integer;
    fPh: Double;
begin
 fPh := Frequency / SampleRate;
 i := 0;
 for j := 0 to Blocksize - 1 do
 begin
  case ToneFlavor of
  tfSine: SingleBlock[i, j] := sin(fPhase * 2 * PI);
  tfSaw: if fPhase <= 0.5 then SingleBlock[i, j] := 2 * fPhase
         else SingleBlock[i, j] := 2 * fPhase - 2;
  tfSquare: if fPhase <= 0.5 then SingleBlock[i, j] := 1 else
   SingleBlock[i, j] := -1;
  tfTriangle: if (fPhase >= 0.25) and (fPhase < 0.75) then
   SingleBlock[i, j] := -4 * (fPhase + 0.25) + 3
   else if (fphase < 0.25) then SingleBlock[i, j] := 4 * (fPhase + 0.25) - 1
   else SingleBlock[i, j] := 4 * (fphase + 0.25) - 5;
  tfPulse: if j = 0 then SingleBlock[i, j] := 0 else
   SingleBlock[i, j] := 1;
  else
  end;
  fPhase := fPhase + fPh;
  if fPhase >= 1 then fPhase := fPhase - 1;
 end;
 for i := 1 to High(SingleBlock) do
  for j := 0 to Blocksize - 1 do
   SingleBlock[i, j] := SingleBlock[0, j];
end;

procedure Register;
begin
  RegisterComponents('Audio', [TASIOGeneratorNoise]);
  RegisterComponents('Audio', [TASIOGeneratorTone]);
end;

initialization

end.
