unit DAV_ASIOGenerator;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common;

type
  TNoiseColor = (ncWhite, ncPink, ncBrown);
  TToneFlavor = (tfSine, tfSaw, tfSquare, tfTriangle, tfPulse);
  TASIOGenerator = class(TComponent)
  private
    FSampleRate  : Double;
    FBlockSize   : Cardinal;
    procedure SetSampleRate(const Value: Double);
    procedure SetBlockSize(const Value: Cardinal);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ProcessBlock(SingleBlock: TDAVArrayOfSingleDynArray; isOutput: Boolean = True); virtual;
  published
    property SampleRate: Double read FSampleRate write SetSampleRate;
    property BlockSize: Cardinal read FBlockSize write SetBlockSize;
  end;

  TASIOGeneratorNoise = class(TASIOGenerator)
  private
   FNoiZe      : Array[0..7] of Single;
   FNoiseColor : TNoiseColor;
  public
   procedure ProcessBlock(SingleBlock: TDAVArrayOfSingleDynArray; isOutput: Boolean = True); override;
  published
   property NoiseColor: TNoiseColor read FNoiseColor write FNoiseColor;
  end;

  TASIOGeneratorTone = class(TASIOGenerator)
  private
   FFrequency  : Single;
   FToneFlavor : TToneFlavor;
   FPhase      : Double;
   procedure SetFrequency(value: Single);
  public
   constructor Create(AOwner: TComponent); override;
   procedure ProcessBlock(SingleBlock: TDAVArrayOfSingleDynArray; isOutput: Boolean = True); override;
  published
   property Frequency: Single read FFrequency write SetFrequency;
   property ToneFlavor:TToneFlavor read FToneFlavor write FToneFlavor;
  end;

implementation

uses
  Math, DAV_ASIOHost;

constructor TASIOGenerator.Create(AOwner: TComponent);
begin
 FBlockSize := 2048;
 FSampleRate := 44100;
 inherited;
end;

destructor TASIOGenerator.Destroy;
begin
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TASIOGenerator.ProcessBlock(SingleBlock: TDAVArrayOfSingleDynArray; isOutput: Boolean = True);
begin
 FillChar(SingleBlock[0], BlockSize * SizeOf(Single), 0);
end;

procedure TASIOGenerator.SetSampleRate(const Value: Double);
begin
 FSampleRate := Value;
end;

procedure TASIOGenerator.SetBlockSize(const Value: Cardinal);
begin
 FBlockSize := Value;
end;

procedure TASIOGeneratorNoise.ProcessBlock(SingleBlock: TDAVArrayOfSingleDynArray;
 isOutput: Boolean = True);
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
     FNoiZe[0]:=(2 * Random - 1);
     FNoiZe[1]:= 0.99886 * FNoiZe[1] + FNoiZe[0] * 0.0555179;
     FNoiZe[2]:= 0.99332 * FNoiZe[2] + FNoiZe[0] * 0.0750759;
     FNoiZe[3]:= 0.96900 * FNoiZe[3] + FNoiZe[0] * 0.1538520;
     FNoiZe[4]:= 0.86650 * FNoiZe[4] + FNoiZe[0] * 0.3104856;
     FNoiZe[5]:= 0.55000 * FNoiZe[5] + FNoiZe[0] * 0.5329522;
     FNoiZe[6]:= -0.7616 * FNoiZe[6] - FNoiZe[0] * 0.0168980;
     SingleBlock[i,j] := 0.1 * (FNoiZe[0] + FNoiZe[1] + FNoiZe[2] + FNoiZe[3] + FNoiZe[4] + FNoiZe[5] + FNoiZe[6] + FNoiZe[7] * 0.5362);   FNoiZe[7]:= FNoiZe[0] * 0.115926;
    end
 else if NoiseColor = ncBrown then
  for i := 0 to Length(SingleBlock) - 1 do
   for j := 0 to Blocksize - 1 do
    begin
     SingleBlock[i, j] := FNoiZe[i] + 0.0002 * (2 * Random - 1);
     FNoiZe[i] := 0.999 * SingleBlock[i, j];
     SingleBlock[i, j] := 150 * SingleBlock[i, j];
   end;
end;

procedure TASIOGeneratorTone.SetFrequency(value:Single);
begin
 FFrequency := value;
end;

constructor TASIOGeneratorTone.Create(AOwner: TComponent);
begin
 inherited;
 FFrequency := 1000;
end;

procedure TASIOGeneratorTone.ProcessBlock(SingleBlock: TDAVArrayOfSingleDynArray;
 isOutput: Boolean = True);
var i, j : Integer;
    fPh: Double;
begin
 fPh := Frequency / SampleRate;
 i := 0;
 for j := 0 to Blocksize - 1 do
 begin
  case ToneFlavor of
  tfSine: SingleBlock[i, j] := sin(FPhase * 2 * PI);
  tfSaw: if FPhase <= 0.5 then SingleBlock[i, j] := 2 * FPhase
         else SingleBlock[i, j] := 2 * FPhase - 2;
  tfSquare: if FPhase <= 0.5 then SingleBlock[i, j] := 1 else
   SingleBlock[i, j] := -1;
  tfTriangle: if (FPhase >= 0.25) and (FPhase < 0.75) then
   SingleBlock[i, j] := -4 * (FPhase + 0.25) + 3
   else if (FPhase < 0.25) then SingleBlock[i, j] := 4 * (FPhase + 0.25) - 1
   else SingleBlock[i, j] := 4 * (FPhase + 0.25) - 5;
  tfPulse: if j = 0 then SingleBlock[i, j] := 0 else
   SingleBlock[i, j] := 1;
  else
  end;
  FPhase := FPhase + fPh;
  if FPhase >= 1 then FPhase := FPhase - 1;
 end;
 for i := 1 to High(SingleBlock) do
  for j := 0 to Blocksize - 1 do
   SingleBlock[i, j] := SingleBlock[0, j];
end;

initialization

end.
