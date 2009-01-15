unit DAV_StkMesh2D;

{
/***************************************************/
/*! \class TMesh2D
    \brief Two-dimensional rectilinear waveguide mesh class.

    This class implements a rectilinear,
    two-dimensional digital waveguide mesh
    structure.  For details, see Van Duyne and
    Smith, "Physical Modeling with the 2-D Digital
    Waveguide Mesh", Proceedings of the 1993
    International Computer Music Conference.

    This is a digital waveguide model, making its
    use possibly subject to patents held by Stanford
    University, Yamaha, and others.

    Control Change Numbers: 
       - X Dimension := 2
       - Y Dimension := 4
       - Mesh Decay := 11
       - X-Y Input Position := 1

    by Julius Smith, 2000 - 2002.
    Revised by Gary Scavone for STK, 2002.
*/
/***************************************************/
}
interface

uses stk, instrmnt, onepole;

const
  nxmax = 12;
  nymax = 12;

type
  TMesh2D = class(TInstrmnt)
  public
  //! Class constructor, taking the x and y dimensions in samples.
    constructor Create(sr: my_float; nX, nY: integer);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Set the x dimension size in samples.
    procedure setNX(lenX: integer);

  //! Set the y dimension size in samples.
    procedure setNY(lenY: integer);

  //! Set the x, y input position on a 0.0 - 1.0 scale.
    procedure setInputPosition(xFactor, yFactor: my_float);

  //! Set the loss filters gains (0.0 - 1.0).
    procedure setDecay(decayFactor: my_float);

  //! Impulse the mesh with the given amplitude (frequency ignored).
    procedure noteOn(frequency, amplitude: my_float);

  //! Stop a note with the given amplitude (speed of decay) ... currently ignored.
    procedure noteOff(amplitude: my_float);

  //! Calculate and return the signal energy stored in the mesh.
    function energy: my_float;

  //! Compute one output sample, without adding energy to the mesh.
    function tick: my_float; overload;

  //! Input a sample to the mesh and compute one output sample.
    function tick(input: my_float): my_float; overload;

  //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: integer; Value: my_float);

  protected
    NX, NY, xInput, yInput: integer;
    filterX: array[0..NXMAX - 1] of TOnePole;
    filterY: array[0..NYMAX - 1] of TOnePole;
    v: array[0..NXMAX - 2, 0..NYMAX - 2] of my_float; // junction velocities
    vxp1, vxm1, vyp1, vym1, vxm, vyp, vym, vxp: array[0..NXMAX - 1, 0..NYMAX - 1] of my_float;
    counter: longint; // time in samples
    procedure clearMesh;
    function tick0: my_float;
    function tick1: my_float;
  end;

implementation

constructor TMesh2D.Create;
var
  pole: my_float;
  i: integer;
begin
  inherited Create(sr);
  setNX(nX);
  setNY(nY);
  pole := 0.05;
  for i := 0 to NYMAX - 1 do
   begin
    filterY[i] := TOnePole.Create(srate, pole);
    filterY[i].setGain(0.99);
   end;
  for i := 0 to NXMAX - 1 do
   begin
    filterX[i] := TOnePole.Create(srate, pole);
    filterX[i].setGain(0.99);
   end;
  clearMesh;
  counter := 0;
  xInput := 0;
  yInput := 0;
end;

destructor TMesh2D.Destroy;
var
  i: integer;
begin
  inherited Destroy;
  for i := 0 to NYMAX - 1 do
    filterY[i].Free;
  for i := 0 to NXMAX - 1 do
    filterX[i].Free;
end;

procedure TMesh2D.Clear;
var
  i: integer;
begin
  clearMesh;
  for i := 0 to NY - 1 do
    filterY[i].Clear;
  for i := 0 to NX - 1 do
    filterX[i].Clear;
  counter := 0;
end;

procedure TMesh2D.clearMesh;
var
  x, y: integer;
begin
  for x := 0 to NXMAX - 2 do
    for y := 0 to NYMAX - 2 do
      v[x][y] := 0;
  for x := 0 to NXMAX - 1 do
    for y := 0 to NYMAX - 1 do
     begin
      vxp[x][y] := 0;
      vxm[x][y] := 0;
      vyp[x][y] := 0;
      vym[x][y] := 0;
      vxp1[x][y] := 0;
      vxm1[x][y] := 0;
      vyp1[x][y] := 0;
      vym1[x][y] := 0;
     end;
end;

function TMesh2D.energy: my_float;
var
  x, y: integer;
  e, t: my_float;
begin
  // Return total energy contained in wave variables Note that some
  // energy is also contained in any filter delay elements.
  e := 0;
  if (counter and 1 > 0) then
    for x := 0 to NX - 1 do
      for y := 0 to NY - 1 do
       begin
        t := vxp1[x][y];
        e := e + t * t;
        t := vxm1[x][y];
        e := e + t * t;
        t := vyp1[x][y];
        e := e + t * t;
        t := vym1[x][y];
        e := e + t * t;
       end// Ready for TMesh2D::tick1() to be called.

  else
    for x := 0 to NX - 1 do
      for y := 0 to NY - 1 do
       begin
        t := vxp[x][y];
        e := e + t * t;
        t := vxm[x][y];
        e := e + t * t;
        t := vyp[x][y];
        e := e + t * t;
        t := vym[x][y];
        e := e + t * t;
       end// Ready for TMesh2D::tick0() to be called.
  ;

  Result := e;
end;

procedure TMesh2D.setNX;
begin
  NX := lenX;
  if (lenX < 2) then
    NX := 2
  else if (lenX > NXMAX) then
    NX := NXMAX;
end;

procedure TMesh2D.setNY;
begin
  NY := lenY;
  if (lenY < 2) then
    NY := 2
  else if (lenY > NYMAX) then
    NY := NYMAX;
end;

procedure TMesh2D.setDecay;
var
  gain: my_float;
  i: integer;
begin
  gain := decayFactor;
  if (decayFactor < 0.0) then
    gain := 0.0
  else if (decayFactor > 1.0) then
    gain := 1.0;
  for i := 0 to NYMAX - 1 do
    filterY[i].setGain(gain);
  for i := 0 to NXMAX - 1 do
    filterX[i].setGain(gain);
end;

procedure TMesh2D.setInputPosition;
begin
  if (xFactor < 0.0) then
    xInput := 0
  else if (xFactor > 1.0) then
    xInput := NX - 1
  else
    xInput := round(xFactor * (NX - 1));

  if (yFactor < 0.0) then
    yInput := 0
  else if (yFactor > 1.0) then
    yInput := NY - 1
  else
    yInput := round(yFactor * (NY - 1));
end;

procedure TMesh2D.noteOn;
begin
  // Input at corner.
  if (counter and 1 > 0) then
   begin
    vxp1[xInput][yInput] := vxp1[xInput][yInput] + amplitude;
    vyp1[xInput][yInput] := vyp1[xInput][yInput] + amplitude;
   end
  else
   begin
    vxp[xInput][yInput] := vxp[xInput][yInput] + amplitude;
    vyp[xInput][yInput] := vyp[xInput][yInput] + amplitude;
   end;
end;

procedure TMesh2D.noteOff;
begin
end;

function TMesh2D.tick(input: my_float): my_float;
begin
  if (counter and 1 > 0) then
   begin
    vxp1[xInput][yInput] := vxp1[xInput][yInput] + input;
    vyp1[xInput][yInput] := vyp1[xInput][yInput] + input;
    lastOutput := tick1;
   end
  else
   begin
    vxp[xInput][yInput] := vxp[xInput][yInput] + input;
    vyp[xInput][yInput] := vyp[xInput][yInput] + input;
    lastOutput := tick0;
   end;

  counter := counter + 1;
  Result := lastOutput;
end;

function TMesh2D.tick: my_float;
begin
  if (counter and 1 > 0) then
    lastOutput := tick1
  else
    lastOutput := tick0;
  counter := counter + 1;
  Result := lastOutput;
end;

const
  VSCALE = 0.5;

function TMesh2D.tick0: my_float;
var
  x, y: integer;
  vxy, outsamp: my_float;
begin
  // Update junction velocities.
  for x := 0 to NX - 2 do
    for y := 0 to NY - 2 do
      v[x][y] := (vxp[x][y] + vxm[x + 1][y] + vyp[x][y] +
        vym[x][y + 1]) * VSCALE;

  // Update junction outgoing waves, using alternate wave-variable buffers.
  for x := 0 to NX - 2 do
    for y := 0 to NY - 2 do
     begin
      vxy := v[x][y];
      // Update positive-going waves.
      vxp1[x + 1][y] := vxy - vxm[x + 1][y];
      vyp1[x][y + 1] := vxy - vym[x][y + 1];
      // Update minus-going waves.
      vxm1[x][y] := vxy - vxp[x][y];
      vym1[x][y] := vxy - vyp[x][y];
     end;

  // Loop over velocity-junction boundary faces, update edge
  // reflections, with filtering.  We're only filtering on one x and y
  // edge here and even this could be made much sparser.
  for y := 0 to NY - 2 do
   begin
    vxp1[0][y] := filterY[y].tick(vxm[0][y]);
    vxm1[NX - 1][y] := vxp[NX - 1][y];
   end;
  for x := 0 to NX - 2 do
   begin
    vyp1[x][0] := filterX[x].tick(vym[x][0]);
    vym1[x][NY - 1] := vyp[x][NY - 1];
   end;

  // Output := sum of outgoing waves at far corner.  Note that the last
  // index in each coordinate direction is used only with the other
  // coordinate indices at their next-to-last values.  This is because
  // the "unit strings" attached to each velocity node to terminate
  // the mesh are not themselves connected together.
  outsamp := vxp[NX - 1][NY - 2] + vyp[NX - 2][NY - 1];

  Result := outsamp;
end;

function TMesh2D.tick1: my_float;
var
  x, y: integer;
  vxy, outsamp: my_float;
begin
  // Update junction velocities.
  for x := 0 to NX - 2 do
    for y := 0 to NY - 2 do
      v[x][y] := (vxp1[x][y] + vxm1[x + 1][y] + vyp1[x][y] +
        vym1[x][y + 1]) * VSCALE;

  // Update junction outgoing waves, 
  // using alternate wave-variable buffers.
  for x := 0 to NX - 2 do
    for y := 0 to NY - 2 do
     begin
      vxy := v[x][y];

      // Update positive-going waves.
      vxp[x + 1][y] := vxy - vxm1[x + 1][y];
      vyp[x][y + 1] := vxy - vym1[x][y + 1];

      // Update minus-going waves.
      vxm[x][y] := vxy - vxp1[x][y];
      vym[x][y] := vxy - vyp1[x][y];
     end;

  // Loop over velocity-junction boundary faces, update edge
  // reflections, with filtering.  We're only filtering on one x and y
  // edge here and even this could be made much sparser.
  for y := 0 to NY - 2 do
   begin
    vxp[0][y] := filterY[y].tick(vxm1[0][y]);
    vxm[NX - 1][y] := vxp1[NX - 1][y];
   end;
  for x := 0 to NX - 2 do
   begin
    vyp[x][0] := filterX[x].tick(vym1[x][0]);
    vym[x][NY - 1] := vyp1[x][NY - 1];
   end;

  // Output := sum of outgoing waves at far corner.
  outsamp := vxp1[NX - 1][NY - 2] + vyp1[NX - 2][NY - 1];

  Result := outsamp;
end;

procedure TMesh2D.controlChange;
var
  norm: my_float;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = 2) then // 2
    setNX(round(norm * (NXMAX - 2) + 2))
  else if (number = 4) then // 4
    setNY(round(norm * (NYMAX - 2) + 2))
  else if (number = 11) then // 11
    setDecay(0.9 + (norm * 0.1))
  else if (number = __SK_ModWheel_) then // 1
    setInputPosition(norm, norm);
end;

end.
