unit DDSPRemez;

interface

uses Classes, DAVDCommon;

type
  TFilterKind = (fkBandPass, fkDifferentiator, fkHilbert);

  TRemezFilterDesigner = class(TComponent)
  private
    fFilterTyp: TFilterKind;
    fSampleRate: Double;
  protected
    procedure SetSampleRate(const Value: Double); virtual;
    procedure CalculateFilterKernel(var Data : TAVDSingleDynArray); overload; virtual;
    procedure CalculateFilterKernel(var Data : TAVDDoubleDynArray); overload; virtual; abstract;
    property FilterTyp : TFilterKind read fFilterTyp write fFilterTyp;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property SampleRate : Double read fSampleRate write SetSampleRate;
  end;

  TRemezLowpassFilterDesigner = class(TRemezFilterDesigner)
  private
    fCutoffFrequency: Double;
    fRippleRatio: Double;
    procedure SetCutoffFrequency(const Value: Double);
  protected
  public
    procedure CalculateFilterKernel(var Data : TAVDDoubleDynArray); override;
    constructor Create(AOwner: TComponent); override;
  published
    property CutoffFrequency : Double read fCutoffFrequency write SetCutoffFrequency;
    property RippleRatio : Double read fRippleRatio write fRippleRatio;
  end;

  TRemezHighpassFilterDesigner = class(TRemezFilterDesigner)
  private
    fCutoffFrequency: Double;
    fRippleRatio: Double;
    procedure SetCutoffFrequency(const Value: Double);
  protected
  public
    procedure CalculateFilterKernel(var Data : TAVDDoubleDynArray); override;
    constructor Create(AOwner: TComponent); override;
  published
    property CutoffFrequency : Double read fCutoffFrequency write SetCutoffFrequency;
    property RippleRatio : Double read fRippleRatio write fRippleRatio;
  end;

  TRemezBandpassFilterDesigner = class(TRemezFilterDesigner)
  private
    fLowFrequency: Double;
    fHighFrequency: Double;
    fRippleRatio: Double;
    procedure SetLowFrequency(const Value: Double);
    procedure SetHighFrequency(const Value: Double);
  protected
  public
    procedure CalculateFilterKernel(var Data : TAVDDoubleDynArray); override;
    constructor Create(AOwner: TComponent); override;
  published
    property LowFrequency : Double read fLowFrequency write SetLowFrequency;
    property HighFrequency : Double read fHighFrequency write SetHighFrequency;
    property RippleRatio : Double read fRippleRatio write fRippleRatio;
  end;

  TRemezBandstopFilterDesigner = class(TRemezFilterDesigner)
  private
    fLowFrequency: Double;
    fHighFrequency: Double;
    fRippleRatio: Double;
    procedure SetLowFrequency(const Value: Double);
    procedure SetHighFrequency(const Value: Double);
  protected
  public
    procedure CalculateFilterKernel(var Data : TAVDDoubleDynArray); override;
    constructor Create(AOwner: TComponent); override;
  published
    property LowFrequency : Double read fLowFrequency write SetLowFrequency;
    property HighFrequency : Double read fHighFrequency write SetHighFrequency;
    property RippleRatio : Double read fRippleRatio write fRippleRatio;
  end;

procedure Remez(var h : TAVDDoubleDynArray; const bands, des, weight: TAVDDoubleDynArray; FilterTyp:TFilterKind);

implementation

uses Math;

const
  Pi2 = 2 * Pi;
  GRIDDENSITY = 16;
  MAXITERATIONS = 40;

type
  TSymmetryKind = (skOdd, skEven);
  TIntegerArray = array of Integer;

{
 CreateDenseGrid
 =================
 Creates the dense grid of frequencies from the specified bands.
 Also creates the Desired Frequency Response function (D[]) and
 the Weight function (W[]) on that dense grid

 INPUT:
 ------
 Integer        r         - 1 / 2 the number of filter coefficients
 Integer        numtaps   - Number of taps in the resulting filter
 Integer        numband   - Number of bands in user specification
 Double         bands[]   - User - specified band edges [2 * numband]
 Double         des[]     - Desired response per band [numband]
 Double         weight[]  - Weight per band [numband]
 TSymmetryKind  symmetry  - Symmetry of filter - used for grid check

 OUTPUT:
 -------
 Integer    gridsize      - Number of elements in the dense frequency grid
 Double     Grid[]        - Frequencies (0 to 0.5) on the dense grid [gridsize]
 Double     D[]           - Desired response on the dense grid [gridsize]
 Double     W[]           - Weight function on the dense grid [gridsize]
}

procedure CreateDenseGrid(r, numtaps, numband:Integer;
                          bands, des, weight: TAVDDoubleDynArray;
                          gridsize : Integer;
                          Grid, D, W : TAVDDoubleDynArray; symmetry:TSymmetryKind);
var i,j,k,band:Integer;
    delf,lowf,Highf:Double;
begin
 delf := 0.5 / (GRIDDENSITY * r);

 // For differentiator, hilbert, symmetry is odd and Grid[0] = max(delf, band[0])
 if (symmetry=skOdd) and (delf>bands[0])
  then bands[0] := delf;

 j := 0;
 for band := 0 to numband - 1 do
  begin
   Grid[j] := bands[2 * band];
   lowf    := bands[2 * band];
   highf   := bands[2 * band + 1];
   k := round((highf - lowf) / delf { +  0.5}); { .5 for rounding }  //eigentlich int
   for i := 0 to k - 1 do
    begin
     D[j] := des[band];
     W[j] := weight[band];
     Grid[j] := lowf;
     lowf := lowf + delf;
     Inc(j);
    end;
   Grid[j - 1] := highf;
  end;

 // Similar to above, if odd symmetry, last grid point can't be .5
 // - but, if there are even taps, leave the last grid point at .5
{ if ((symmetry = skOdd) and (Grid[gridsize - 1] > (0.5 - delf)) and ((numtaps  mod  2)=1)) //=1???
  then} Grid[gridsize - 1] := 0.5 - delf;
end;


{
 InitialGuess
 ==============
 Places Extremal Frequencies evenly throughout the dense grid.


 INPUT:
 ------
 Integer r          - 1 / 2 the number of filter coefficients
 Integer gridsize   - Number of elements in the dense frequency grid

 OUTPUT:
 -------
 int Ext[]          - Extremal indexes to dense frequency grid [r + 1]
}

procedure  InitialGuess(r:Integer; Ext:TIntegerArray; gridsize:Integer);
var i : Integer;
begin
 for i := 0 to r
  do Ext[i] := i * (gridsize - 1) div r;
end;


{
 CalcParms
 ===========

 INPUT:
 ------
 Integer    r       - 1 / 2 the number of filter coefficients
 Integer    Ext[]   - Extremal indexes to dense frequency grid [r + 1]
 Double     Grid[]  - Frequencies (0 to 0.5) on the dense grid [gridsize]
 Double     D[]     - Desired response on the dense grid [gridsize]
 Double     W[]     - Weight function on the dense grid [gridsize]

 OUTPUT:
 ------- 
 Double ad[]    - 'b' in Oppenheim & Schafer [r + 1]
 Double x[]     - [r + 1]
 Double y[]     - 'C' in Oppenheim & Schafer [r + 1]
}

procedure CalcParms(r:Integer; Ext:TIntegerArray; Grid, D, W, ad, x, y : TAVDDoubleDynArray);
var i,j,k,ld:Integer;
    sign,xi,delta,denom,numer : Double;
begin
// Find x[]
 for i := 0 to r
  do x[i] := cos(Pi2 * Grid[Ext[i]]);

 // Calculate ad[]  - Oppenheim & Schafer eq 7.132
 ld := ((r - 1) div 15) + 1;   { Skips around to avoid round errors }

 for i := 0 to r do
  begin
   denom := 1.0;
   xi := x[i];
   for j := 0 to ld - 1 do
    begin
     k := j;
     while (k<=r) do
      begin
       if (k<>i)
        then denom := denom * 2.0 * (xi - x[k]);
       k := k + ld
      end;
    end;
   if abs(denom) < 0.00001
    then denom := 0.00001;
   ad[i] := 1.0 / denom;
  end;

 // Calculate delta  - Oppenheim & Schafer eq 7.131
 numer := 0;
 denom := 0;
 sign := 1;
 for i := 0 to r do
  begin
   numer := numer + ad[i] * D[Ext[i]];
   denom := denom + ad[i] / W[Ext[i]] * sign;
   sign :=  - sign;
  end;
 if denom > 0 then delta := numer / denom else delta := 1e6; {kludge!!}
 sign := 1;

 // Calculate y[]  - Oppenheim & Schafer eq 7.133b
 for i := 0 to r do
  begin
   y[i] := D[Ext[i]] - sign * delta / W[Ext[i]];
   sign :=  - sign;
  end;
end;

{
 ComputeA
 ==========
 Using values calculated in CalcParms, ComputeA calculates the
 actual filter response at a given frequency (freq).  Uses
 eq 7.133a from Oppenheim & Schafer.

 INPUT:
 ------ 
 Double      freq  - Frequency (0 to 0.5) at which to calculate A
 Integer     r     - 1 / 2 the number of filter coefficients
 Double      ad[]  - 'b' in Oppenheim & Schafer [r + 1]
 Double      x[]   - [r + 1]
 Double      y[]   - 'C' in Oppenheim & Schafer [r + 1]

 OUTPUT:
 ------- 
 Returns Double value of A[freq]
}

function ComputeA(freq: Double; r: Integer; ad, x, y: TAVDDoubleDynArray) : Double;
var i:Integer;
    xc,c,denom,numer:Double;
begin
 denom := 0;
 numer := 0;
 xc    := cos(Pi2 * freq);
 for i := 0 to r do
  begin
   c := xc - x[i];
   if abs(c) < 1.0E-7 then
    begin
     numer := y[i];
     denom := 1;
     break;
    end;
   c := ad[i] / c;
   denom := denom + c;
   numer := numer + c * y[i];
  end;
 ComputeA := numer / denom;
end;

{                       
 CalcError
 ===========
 Calculates the Error function from the desired frequency response
 on the dense grid (D[]), the weight function on the dense grid (W[]),
 and the present response calculation (A[])

 INPUT:
 ------ 
 Integer  r           - 1 / 2 the number of filter coefficients
 Double     ad[]      - [r + 1]
 Double     x[]       - [r + 1]
 Double     y[]       - [r + 1]
 Integer gridsize     - Number of elements in the dense frequency grid
 Double     Grid[]    - Frequencies on the dense grid [gridsize]
 Double     D[]       - Desired response on the dense grid [gridsize]
 Double     W[]       - Weight function on the desnse grid [gridsize]

 OUTPUT:
 ------- 
 Double     E[]       - Error function on dense grid [gridsize]
}

procedure CalcError(r:Integer; ad,x,y:TAVDDoubleDynArray; gridsize:Integer;
                    Grid,D,W,E:TAVDDoubleDynArray);
var i:Integer;
    A:Double;
begin
 for i := 0 to gridsize - 1 do
  begin
   A := ComputeA(Grid[i], r, ad, x, y);
   E[i] := W[i] * (D[i] - A);
  end;
end;

{                       
 Search
 ========
 Searches for the maxima / minima of the error curve.  If more than
 r + 1 extrema are found, it uses the following heuristic (thanks
 Chris Hanson):
 1) Adjacent non - alternating extrema deleted first.
 2) If there are more than one excess extrema, delete the
  one with the smallest error.  This will create a non - alternation
  condition that is fixed by 1).
 3) If there is exactly one excess extremum, delete the smaller
  of the first / last extremum


 INPUT:
 ------ 
 Integer     r        - 1 / 2 the number of filter coefficients
 Integer     Ext[]    - Indexes to Grid[] of extremal frequencies [r + 1]
 Integer     Gridsize - Number of elements in the dense frequency grid
 Double      E[]      - Array of error values.  [gridsize]
 OUTPUT:
 ------- 
 Integer     Ext[]    - New indexes to extremal frequencies [r + 1]
}

procedure  Search(r:Integer; Ext: TIntegerArray; gridsize:Integer; E:TAVDDoubleDynArray);
var i,j,k,l,extra:Integer;   { Counters }
    alt,up: Boolean;
    foundExt:TIntegerArray; { Array of found extremals }
begin

 // Allocate enough space for found extremals.
 SetLength(foundExt,2 * r);
 k := 0;

 // Check for extremum at 0.
 if ((E[0] > 0.0) and (E[0] > E[1])) or ((E[0] < 0.0) and (E[0] < E[1])) then
  begin
   foundExt[k] := 0;
   inc(k)
  end;

 // Check for extrema inside dense grid
 for i := 1 to gridsize - 2 do       {<<bugfix: was gridsize - 1}
  if ((E[i] >= E[i - 1]) and (E[i] > E[i + 1]) and (E[i] > 0.0)) or ((E[i] <= E[i - 1]) and (E[i]<E[i + 1]) and (E[i] < 0.0)) then
   begin
    foundExt[k] := i;
    inc(k)
   end;

 // Check for extremum at 0.5
 j := gridsize - 1;
 if ((E[j] > 0.0) and (E[j] > E[j - 1])) or ((E[j] < 0.0) and (E[j] < E[j - 1])) then
  begin
   foundExt[k] := j;
   inc(k)
  end;

 // Remove extra extremals
 extra := k - (r + 1);

 while (extra > 0) do
  begin
   if E[foundExt[0]] > 0.0
    then up := True    // first one is a maxima
    else up := False;  // first one is a minima

   l := 0;
   alt := True;
   for j := 1 to k - 1 do
    begin
     if abs(E[foundExt[j]]) < abs(E[foundExt[l]])
      then l := j; // new smallest error.
     if up and (E[foundExt[j]] < 0.0)
      then up := false // switch to a minima
      else if (not up) and (E[foundExt[j]] > 0.0)
            then up := True // switch to a maxima
            else
             begin
//              k := j;
              alt := False;
              break; // Ooops, found two non - alternating extrema. Delete smallest of them.
             end;
    end; // if the loop finishes, all extrema are alternating

  // If there's only one extremal and all are alternating,
  // delete the smallest of the first / last extremals.
   if alt and (extra = 1) then
    if abs(E[foundExt[k - 1]]) < abs(E[foundExt[0]])
     then l := foundExt[k - 1]  // Delete last extremal
     else l := foundExt[0];     // Delete first extremal

   for j := l to k - 1          // Loop that does the deletion
    do foundExt[j] := foundExt[j + 1];
   Dec(k);
   Dec(extra);
  end;

 for i := 0 to r
  do Ext[i] := foundExt[i];   { Copy found extremals to Ext[] }

 SetLength(foundExt, 0);
end;

{                    
 FreqSample
 ============
 Simple frequency sampling algorithm to determine the impulse
 response h[] from A's found in ComputeA

 INPUT:
 ------ 
 Integer       N        - Number of filter coefficients
 Double        A[]      - Sample points of desired response [N / 2]
 TSymmetrykind Symmetry - Symmetry of desired filter

 OUTPUT:
 -------
 Double h[]         - Impulse Response of final filter [N]
}

procedure FreqSample(N:Integer; A,h:TAVDDoubleDynArray; symm:TSymmetryKind);
var i,k: Integer;
    x,val,M: Double;
begin
 M := (N - 1) * 0.5;
 if (symm = skEven) then
  begin
   if (N mod 2) <> 0 then
    begin
     for i := 0 to N - 1 do
      begin
       val := A[0];
       x := Pi2 * (i - M) / N;
       for k := 1 to trunc(M)
        do val := val + 2.0 * A[k] * cos(x * k);
       h[i] := val / N;
      end;
    end
   else
    begin
     for i := 0 to N - 1 do
      begin
       val := A[0];
       x := Pi2 * (i - M) / N;
       for k := 1 to ((N div 2) - 1) // for (k=1; k<=(N / 2 - 1); k +  + )
        do val := val + 2.0 * A[k] * cos(x * k);
       h[i] := val / N;
    end;
   end;
  end
 else
  begin
   if (N mod 2) <> 0 then
    begin
     for i := 0 to N - 1 do
      begin
       val := 0;
       x := Pi2 * (i - M) / N;
       for k := 1 to trunc(M)
        do val := val + 2.0 * A[k] * sin(x * k);
       h[i] := val / N;
      end;
    end
   else
    begin
     for i := 0 to N - 1 do
      begin
       val := A[N div 2] * sin(Pi * (i - M));
       x := Pi2 * (i - M) / N;
       for k := 1 to ((N div 2) - 1)
        do val := val + 2.0 * A[k] * sin(x * k);
       h[i] := val / N;
      end;
    end;
  end;
end;

{                  
 isDone
 ========
 Checks to see if the error function is small enough to consider
 the result to have converged.

 INPUT:
 ------ 
 Integer    r       - 1 / 2 the number of filter coeffiecients
 Integer    Ext[]   - Indexes to extremal frequencies [r + 1]
 Double     E[]     - Error function on the dense grid [gridsize]

 OUTPUT:
 ------- 
 Returns 1 if the result converged
 Returns 0 if the result has not converged
}

function IsDone(r:Integer; Ext:TIntegerArray; E:TAVDDoubleDynArray):Boolean;
var i: Integer;
    min,max,current:Double;
begin
 min := abs(E[Ext[0]]);
 max := min;
 for i := 1 to r do
  begin
   current := abs(E[Ext[i]]);
   if current < min then min := current;
   if current > max then max := current;
  end;
 IsDone := ((max - min) / max) < 0.0001;
end;

{                   
 Remez
 =======
 Calculates the optimal (in the Chebyshev / minimax sense)
 FIR filter impulse response given a set of band edges,
 the desired reponse on those bands, and the weight given to
 the error in those bands.

 INPUT:
 ------ 
 Integer     numtaps   - Number of filter coefficients
 Integer     numband   - Number of bands in filter specification
 Double      bands[]   - User - specified band edges [2 numband]
 Double      des[]     - User - specified band responses [numband]
 Double      weight[]  - User - specified error weights [numband]
 Integer     type      - Type of filter

 OUTPUT:
 -------
 Double h[]        - Impulse response of final filter [numtaps]
}

procedure remez(var h:TAVDDoubleDynArray; const bands,des,weight:TAVDDoubleDynArray; FilterTyp:TFilterKind);
var i, iter, gridsize, r: Integer;
    symmetry: TSymmetryKind;
    Grid,W,D,E,taps: TAVDDoubleDynArray;
    x, y, ad: TAVDDoubleDynArray;
    c: Double;
    Ext: TIntegerArray;
    numtaps, numband: Integer;
begin
 numtaps := Length(h);
 numband := Length(des);
 if (FilterTyp = fkBandPass)
  then symmetry := skEven // ==POSITIVE
  else symmetry := skOdd;

 r := numtaps div 2;      { number of extrema }
 if ((numtaps mod 2) <> 0) and (symmetry = skEven)
  then Inc(r);

 gridsize := 0;
 for i := 0 to numband - 1 do
  gridsize := gridsize + round(2 * r * GRIDDENSITY * (bands[2 * i + 1] - bands[2 * i])); //ceil???

 if symmetry = skOdd then Dec(gridsize);

 // Dynamically allocate memory for arrays with proper sizes
 SetLength(Grid, gridsize);
 SetLength(D, gridsize);
 SetLength(W, gridsize);
 SetLength(E, gridsize);
 SetLength(Ext, r + 1);
 SetLength(taps ,r + 1);
 SetLength(x, r + 1);
 SetLength(y, r + 1);
 SetLength(ad, r + 1);

 // Create dense frequency grid
 CreateDenseGrid(r,numtaps,numband,bands,des,weight,gridsize,Grid,D,W,symmetry);
 InitialGuess(r,Ext,gridsize);

 // For Differentiator: (fix grid)
 if (FilterTyp=fkDifferentiator) then
  for i := 0 to gridsize - 1 do
   begin
    D[i] := D[i] * Grid[i];
    if D[i]>0.0001 then W[i] := W[i] / Grid[i];
   end;

 // For odd or Negative symmetry filters, alter the D[] and W[] according to Parks McClellan
 if (symmetry=skEven) then
  begin
   if ((numtaps mod 2) = 0) then
    for i := 0 to gridsize - 1 do
     begin
      c := cos(Pi * Grid[i]);
      D[i] := D[i] / c;
      W[i] := W[i] * c;
     end;
  end
 else
  if ((numtaps mod 2) <> 0) then
   for i := 0 to gridsize - 1 do
    begin
     c := sin(Pi2 * Grid[i]);
     D[i] := D[i] / c;
     W[i] := W[i] * c;
    end
  else
   for i := 0 to gridsize - 1 do
    begin
     c := sin(Pi * Grid[i]);
     D[i] := D[i] / c;
     W[i] := W[i] * c;
    end;

 // Perform the Remez Exchange algorithm
 for iter := 0 to MAXITERATIONS - 1 do
  begin
   CalcParms(r, Ext, Grid, D, W, ad, x, y);
   CalcError(r, ad, x, y, gridsize, Grid, D, W, E);
   Search(r, Ext, gridsize, E);
   if (isDone(r, Ext, E)) then break;
  end;
 if iter = 39 then ;
 CalcParms(r, Ext, Grid, D, W, ad, x, y); {not needed?}

 // Find the 'taps' of the filter for use with Frequency Sampling.  If odd
 // or Negative symmetry, fix the taps according to Parks McClellan.
 for i := 0 to (numtaps div 2) - 1 do
 begin
  if (symmetry = skEven) then
  begin
   if (numtaps mod 2) <> 0
    then c := 1
     else c := cos(Pi * i / numtaps);
  end
  else
  begin
   if (numtaps mod 2) <> 0
    then c := sin(Pi2 * i / numtaps)
     else c := sin(Pi * i / numtaps);
  end;
  taps[i] := ComputeA(i / numtaps, r, ad, x, y) * c;
 end;

 // Frequency sampling design with calculated taps
 FreqSample(numtaps, taps, h, symmetry);

 // Delete allocated memory
 SetLength(Grid, 0);
 SetLength(taps, 0);
 SetLength(W, 0);
 SetLength(D, 0);
 SetLength(E, 0);
 SetLength(Ext, 0);
 SetLength(x, 0);
 SetLength(y, 0);
 SetLength(ad, 0);
end;

{ TRemezLowpassFilterDesigner }

{ TRemezFilterDesigner }

procedure TRemezFilterDesigner.CalculateFilterKernel(var Data: TAVDSingleDynArray);
var
  i        : Integer;
  DblArray : TAVDDoubleDynArray;
begin
 SetLength(DblArray, Length(Data));
 CalculateFilterKernel(DblArray);
 for i := 0 to Length(Data) - 1
  do Data[i] := DblArray[i];
end;

constructor TRemezFilterDesigner.Create(AOwner: TComponent);
begin
 inherited;
 fSampleRate := 44100;
end;

procedure TRemezFilterDesigner.SetSampleRate(const Value: Double);
begin
  fSampleRate := Value;
end;

procedure TRemezLowpassFilterDesigner.CalculateFilterKernel(var Data: TAVDDoubleDynArray);
var
  bands, weights, desired : TAVDDoubleDynArray;
begin
 inherited;
 SetLength(bands, 4);
 SetLength(weights, 2);
 SetLength(desired, 2);
 desired[0] := 1.0;
 desired[1] := 0.0;
 bands[0]   := 0.0;
 bands[1]   := 0.9 * fCutoffFrequency / fSampleRate;
 bands[2]   := 1.1 * fCutoffFrequency / fSampleRate;
 bands[3]   := 0.5;
 weights[0] := 1.0;
 weights[1] := fRippleRatio;
 Remez(Data, bands, desired, weights, fkBandPass);
end;

constructor TRemezLowpassFilterDesigner.Create(AOwner: TComponent);
begin
 inherited;
 fCutoffFrequency := 1000;
 fRippleRatio := 1;
end;

procedure TRemezLowpassFilterDesigner.SetCutoffFrequency(const Value: Double);
begin
 if fCutoffFrequency <> Value then
  begin
   fCutoffFrequency := Value;
  end;
end;

{ TRemezHighpassFilterDesigner }

procedure TRemezHighpassFilterDesigner.CalculateFilterKernel(var Data: TAVDDoubleDynArray);
var
  bands, weights, desired : TAVDDoubleDynArray;
begin
 inherited;
 SetLength(bands, 4);
 SetLength(weights, 2);
 SetLength(desired, 2);
 desired[0] := 0.0;
 desired[1] := 1.0;
 bands[0]   := 0.0;
 bands[1]   := 0.9 * fCutoffFrequency / fSampleRate;
 bands[2]   := 1.1 * fCutoffFrequency / fSampleRate;
 bands[3]   := 0.5;
 weights[0] := 1.0;
 weights[1] := fRippleRatio;
 Remez(Data, bands, desired, weights, fkBandPass);
end;

constructor TRemezHighpassFilterDesigner.Create(AOwner: TComponent);
begin
 inherited;
 fCutoffFrequency := 1000;
 fRippleRatio := 1;
end;

procedure TRemezHighpassFilterDesigner.SetCutoffFrequency(const Value: Double);
begin
 if fCutoffFrequency <> Value then
  begin
   fCutoffFrequency := Value;
  end;
end;

{ TRemezBandpassFilterDesigner }

procedure TRemezBandpassFilterDesigner.CalculateFilterKernel(var Data: TAVDDoubleDynArray);
var
  bands, weights, desired : TAVDDoubleDynArray;
begin
 inherited;
 SetLength(bands, 6);
 SetLength(weights, 3);
 SetLength(desired, 3);
 desired[0] := 0.0;
 desired[1] := 1.0;
 desired[2] := 0.0;
 bands[0]   := 0.0;
 bands[1]   := 0.9 * fLowFrequency;
 bands[2]   := fLowFrequency;
 bands[3]   := fHighFrequency;
 bands[4]   := 1.1 * fHighFrequency;
 bands[5]   := 0.5;
 weights[0] := rippleRatio;
 weights[1] := 1.0;
 weights[2] := rippleRatio;
 Remez(Data, bands, desired, weights, fkBandPass);
end;

constructor TRemezBandpassFilterDesigner.Create(AOwner: TComponent);
begin
 inherited;
 fLowFrequency := 1000;
 fHighFrequency := 8000;
 fRippleRatio := 1;
end;

procedure TRemezBandpassFilterDesigner.SetLowFrequency(const Value: Double);
begin
 if fLowFrequency <> Value then
  begin
   fLowFrequency := Value;
  end;
end;

procedure TRemezBandpassFilterDesigner.SetHighFrequency(const Value: Double);
begin
 if fHighFrequency <> Value then
  begin
   fHighFrequency := Value;
  end;
end;

{ TRemezBandstopFilterDesigner }

procedure TRemezBandstopFilterDesigner.CalculateFilterKernel(var Data: TAVDDoubleDynArray);
var
  bands, weights, desired : TAVDDoubleDynArray;
begin
 inherited;
 SetLength(bands, 6);
 SetLength(weights, 3);
 SetLength(desired, 3);
 desired[0] := 0.0;
 desired[1] := 1.0;
 desired[2] := 0.0;
 bands[0]   := 0.0;
 bands[1]   := 0.9 * fLowFrequency;
 bands[2]   := fLowFrequency;
 bands[3]   := fHighFrequency;
 bands[4]   := 1.1 * fHighFrequency;
 bands[5]   := 0.5;
 weights[0] := rippleRatio;
 weights[1] := 1.0;
 weights[2] := rippleRatio;
 Remez(Data, bands, desired, weights, fkBandPass);
end;

constructor TRemezBandstopFilterDesigner.Create(AOwner: TComponent);
begin
 inherited;
 fLowFrequency := 1000;
 fHighFrequency := 8000;
 fRippleRatio := 1;
end;

procedure TRemezBandstopFilterDesigner.SetLowFrequency(const Value: Double);
begin
 if fLowFrequency <> Value then
  begin
   fLowFrequency := Value;
  end;
end;

procedure TRemezBandstopFilterDesigner.SetHighFrequency(const Value: Double);
begin
 if fHighFrequency <> Value then
  begin
   fHighFrequency := Value;
  end;
end;

end.
