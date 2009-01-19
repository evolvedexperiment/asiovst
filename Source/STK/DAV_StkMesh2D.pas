unit DAV_StkMesh2D;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ Two-dimensional rectilinear waveguide mesh class.

  This class implements a rectilinear, two-dimensional digital waveguide mesh
  structure.  For details, see Van Duyne and Smith, "Physical Modeling with the
  2-D Digital Waveguide Mesh", Proceedings of the 1993 International Computer
  Music Conference.

  This is a digital waveguide model, making its use possibly subject to patents
  held by Stanford University, Yamaha, and others.

  Control Change Numbers:
    - X Dimension = 2
    - Y Dimension = 4
    - Mesh Decay = 11
    - X-Y Input Position = 1
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkInstrument, DAV_StkOnePole;

const
  CNxMax = 12;
  CNyMax = 12;

type
  TStkMesh2D = class(TStkInstrument)
  protected
    FNX, FNY            : Integer;
    FXInput, FYInput    : Integer;
    FFilterX            : array[0..CNxMax - 1] of TOnePole;
    FFilterY            : array[0..CNyMax - 1] of TOnePole;
    FJunctionVelocities : array[0..CNxMax - 2, 0..CNyMax - 2] of Single; // junction velocities
    FVxp1, FVxm1        : array[0..CNxMax - 1, 0..CNyMax - 1] of Single;
    FVyp1, FVym1        : array[0..CNxMax - 1, 0..CNyMax - 1] of Single;
    FVxm, FVyp          : array[0..CNxMax - 1, 0..CNyMax - 1] of Single;
    FVym, FVxp          : array[0..CNxMax - 1, 0..CNyMax - 1] of Single;
    FCounter            : Integer; // time in samples
    procedure ClearMesh;
    function Tick0: Single;
    function Tick1: Single;
  public
    // Class constructor, taking the x and y dimensions in samples.
    constructor Create(SampleRate: Single; FNX, FNY: Integer);

    // Class destructor.
    destructor Destroy;

    // Reset and clear all internal state.
    procedure Clear;

    // Set the x dimension size in samples.
    procedure SetNX(lenX: Integer);

    // Set the y dimension size in samples.
    procedure SetNY(lenY: Integer);

    // Set the x, y Input position on a 0.0 - 1.0 scale.
    procedure SetInputPosition(xFactor, yFactor: Single);

    // Set the loss filters gains (0.0 - 1.0).
    procedure SetDecay(DecayFactor: Single);

    // Impulse the mesh with the given Amplitude (frequency ignored).
    procedure NoteOn(frequency, Amplitude: Single);

    // Stop a note with the given Amplitude (speed of decay) ... currently ignored.
    procedure NoteOff(Amplitude: Single);

    // Calculate and return the signal Energy stored in the mesh.
    function Energy: Single;

    // Compute one output sample, without adding Energy to the mesh.
    function Tick: Single; overload;

    // Input a sample to the mesh and compute one output sample.
    function Tick(Input: Single): Single; overload;

    // Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure ControlChange(number: Integer; Value: Single);

  end;

implementation

constructor TStkMesh2D.Create;
var
  Pole: Single;
  i: Integer;
begin
  inherited Create(SampleRate);
  SetNX(FNX);
  SetNY(FNY);
  Pole := 0.05;
  for i := 0 to CNyMax - 1 do
   begin
    FFilterY[i] := TOnePole.Create(SampleRate, Pole);
    FFilterY[i].SetGain(0.99);
   end;
  for i := 0 to CNxMax - 1 do
   begin
    FFilterX[i] := TOnePole.Create(SampleRate, Pole);
    FFilterX[i].SetGain(0.99);
   end;
  ClearMesh;
  FCounter := 0;
  FXInput := 0;
  FYInput := 0;
end;

destructor TStkMesh2D.Destroy;
var
  i: Integer;
begin
  inherited Destroy;
  for i := 0 to CNyMax - 1 do
    FFilterY[i].Free;
  for i := 0 to CNxMax - 1 do
    FFilterX[i].Free;
end;

procedure TStkMesh2D.Clear;
var
  i: Integer;
begin
  ClearMesh;
  for i := 0 to FNY - 1 do
    FFilterY[i].Clear;
  for i := 0 to FNX - 1 do
    FFilterX[i].Clear;
  FCounter := 0;
end;

procedure TStkMesh2D.ClearMesh;
var
  x, y: Integer;
begin
  for x := 0 to CNxMax - 2 do
    for y := 0 to CNyMax - 2 do
      FJunctionVelocities[x][y] := 0;
  for x := 0 to CNxMax - 1 do
    for y := 0 to CNyMax - 1 do
     begin
      FVxp[x][y] := 0;
      FVxm[x][y] := 0;
      FVyp[x][y] := 0;
      FVym[x][y] := 0;
      FVxp1[x][y] := 0;
      FVxm1[x][y] := 0;
      FVyp1[x][y] := 0;
      FVym1[x][y] := 0;
     end;
end;

function TStkMesh2D.Energy: Single;
var
  x, y: Integer;
  e, t: Single;
begin
  // Return total Energy contained in wave variables Note that some
  // Energy is also contained in any filter delay elements.
  e := 0;
  if (FCounter and 1 > 0) then
    for x := 0 to FNX - 1 do
      for y := 0 to FNY - 1 do
       begin
        t := FVxp1[x][y];
        e := e + t * t;
        t := FVxm1[x][y];
        e := e + t * t;
        t := FVyp1[x][y];
        e := e + t * t;
        t := FVym1[x][y];
        e := e + t * t;
       end// Ready for TStkMesh2D::Tick1() to be called.

  else
    for x := 0 to FNX - 1 do
      for y := 0 to FNY - 1 do
       begin
        t := FVxp[x][y];
        e := e + t * t;
        t := FVxm[x][y];
        e := e + t * t;
        t := FVyp[x][y];
        e := e + t * t;
        t := FVym[x][y];
        e := e + t * t;
       end// Ready for TStkMesh2D::Tick0() to be called.
  ;

  Result := e;
end;

procedure TStkMesh2D.SetNX;
begin
  FNX := lenX;
  if (lenX < 2) then
    FNX := 2
  else if (lenX > CNxMax) then
    FNX := CNxMax;
end;

procedure TStkMesh2D.SetNY;
begin
  FNY := lenY;
  if (lenY < 2) then
    FNY := 2
  else if (lenY > CNyMax) then
    FNY := CNyMax;
end;

procedure TStkMesh2D.SetDecay;
var
  Gain: Single;
  i: Integer;
begin
  Gain := DecayFactor;
  if (DecayFactor < 0.0) then
    Gain := 0.0
  else if (DecayFactor > 1.0) then
    Gain := 1.0;
  for i := 0 to CNyMax - 1 do
    FFilterY[i].SetGain(Gain);
  for i := 0 to CNxMax - 1 do
    FFilterX[i].SetGain(Gain);
end;

procedure TStkMesh2D.SetInputPosition;
begin
  if (xFactor < 0.0) then
    FXInput := 0
  else if (xFactor > 1.0) then
    FXInput := FNX - 1
  else
    FXInput := round(xFactor * (FNX - 1));

  if (yFactor < 0.0) then
    FYInput := 0
  else if (yFactor > 1.0) then
    FYInput := FNY - 1
  else
    FYInput := round(yFactor * (FNY - 1));
end;

procedure TStkMesh2D.NoteOn;
begin
  // Input at corner.
  if (FCounter and 1 > 0) then
   begin
    FVxp1[FXInput][FYInput] := FVxp1[FXInput][FYInput] + Amplitude;
    FVyp1[FXInput][FYInput] := FVyp1[FXInput][FYInput] + Amplitude;
   end
  else
   begin
    FVxp[FXInput][FYInput] := FVxp[FXInput][FYInput] + Amplitude;
    FVyp[FXInput][FYInput] := FVyp[FXInput][FYInput] + Amplitude;
   end;
end;

procedure TStkMesh2D.NoteOff;
begin
end;

function TStkMesh2D.Tick(Input: Single): Single;
begin
  if (FCounter and 1 > 0) then
   begin
    FVxp1[FXInput][FYInput] := FVxp1[FXInput][FYInput] + Input;
    FVyp1[FXInput][FYInput] := FVyp1[FXInput][FYInput] + Input;
    LastOutput := Tick1;
   end
  else
   begin
    FVxp[FXInput][FYInput] := FVxp[FXInput][FYInput] + Input;
    FVyp[FXInput][FYInput] := FVyp[FXInput][FYInput] + Input;
    LastOutput := Tick0;
   end;

  FCounter := FCounter + 1;
  Result := LastOutput;
end;

function TStkMesh2D.Tick: Single;
begin
  if (FCounter and 1 > 0) then
    LastOutput := Tick1
  else
    LastOutput := Tick0;
  FCounter := FCounter + 1;
  Result := LastOutput;
end;

const
  VSCALE = 0.5;

function TStkMesh2D.Tick0: Single;
var
  x, y: Integer;
  Vxy, Outsamp: Single;
begin
  // Update junction velocities.
  for x := 0 to FNX - 2 do
    for y := 0 to FNY - 2 do
      FJunctionVelocities[x][y] := (FVxp[x][y] + FVxm[x + 1][y] + FVyp[x][y] +
        FVym[x][y + 1]) * VSCALE;

  // Update junction outgoing waves, using alternate wave-variable buffers.
  for x := 0 to FNX - 2 do
    for y := 0 to FNY - 2 do
     begin
      Vxy := FJunctionVelocities[x][y];
      // Update positive-going waves.
      FVxp1[x + 1][y] := Vxy - FVxm[x + 1][y];
      FVyp1[x][y + 1] := Vxy - FVym[x][y + 1];
      // Update minus-going waves.
      FVxm1[x][y] := Vxy - FVxp[x][y];
      FVym1[x][y] := Vxy - FVyp[x][y];
     end;

  // Loop over velocity-junction boundary faces, update edge
  // reflections, with filtering.  We're only filtering on one x and y
  // edge here and even this could be made much sparser.
  for y := 0 to FNY - 2 do
   begin
    FVxp1[0][y] := FFilterY[y].Tick(FVxm[0][y]);
    FVxm1[FNX - 1][y] := FVxp[FNX - 1][y];
   end;
  for x := 0 to FNX - 2 do
   begin
    FVyp1[x][0] := FFilterX[x].Tick(FVym[x][0]);
    FVym1[x][FNY - 1] := FVyp[x][FNY - 1];
   end;

  // Output := sum of outgoing waves at far corner.  Note that the last
  // index in each coordinate direction is used only with the other
  // coordinate indices at their next-to-last values.  This is because
  // the "unit strings" attached to each velocity node to terminate
  // the mesh are not themselves connected together.
  Outsamp := FVxp[FNX - 1][FNY - 2] + FVyp[FNX - 2][FNY - 1];

  Result := Outsamp;
end;

function TStkMesh2D.Tick1: Single;
var
  x, y: Integer;
  Vxy, Outsamp: Single;
begin
  // Update junction velocities.
  for x := 0 to FNX - 2 do
    for y := 0 to FNY - 2 do
      FJunctionVelocities[x][y] := (FVxp1[x][y] + FVxm1[x + 1][y] + FVyp1[x][y] +
        FVym1[x][y + 1]) * VSCALE;

  // Update junction outgoing waves, 
  // using alternate wave-variable buffers.
  for x := 0 to FNX - 2 do
    for y := 0 to FNY - 2 do
     begin
      Vxy := FJunctionVelocities[x][y];

      // Update positive-going waves.
      FVxp[x + 1][y] := Vxy - FVxm1[x + 1][y];
      FVyp[x][y + 1] := Vxy - FVym1[x][y + 1];

      // Update minus-going waves.
      FVxm[x][y] := Vxy - FVxp1[x][y];
      FVym[x][y] := Vxy - FVyp1[x][y];
     end;

  // Loop over velocity-junction boundary faces, update edge
  // reflections, with filtering.  We're only filtering on one x and y
  // edge here and even this could be made much sparser.
  for y := 0 to FNY - 2 do
   begin
    FVxp[0][y] := FFilterY[y].Tick(FVxm1[0][y]);
    FVxm[FNX - 1][y] := FVxp1[FNX - 1][y];
   end;
  for x := 0 to FNX - 2 do
   begin
    FVyp[x][0] := FFilterX[x].Tick(FVym1[x][0]);
    FVym[x][FNY - 1] := FVyp1[x][FNY - 1];
   end;

  // Output := sum of outgoing waves at far corner.
  Outsamp := FVxp1[FNX - 1][FNY - 2] + FVyp1[FNX - 2][FNY - 1];

  Result := Outsamp;
end;

procedure TStkMesh2D.ControlChange;
var
  Norm: Single;
begin
  Norm := Value;// * ONE_OVER_128;
  if (Norm < 0) then
    Norm := 0.0
  else if (Norm > 1.0) then
    Norm := 1.0;

  if (number = 2) then // 2
    SetNX(round(Norm * (CNxMax - 2) + 2))
  else if (number = 4) then // 4
    SetNY(round(Norm * (CNyMax - 2) + 2))
  else if (number = 11) then // 11
    SetDecay(0.9 + (Norm * 0.1))
  else if (number = __SK_ModWheel_) then // 1
    SetInputPosition(Norm, Norm);
end;

end.
