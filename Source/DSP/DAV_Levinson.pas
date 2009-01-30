unit DAV_Levinson;

interface

uses
  DAV_Common;

implementation

//find the P-order autocorrelation array, R, for the sequence x of length L and warping of lambda
procedure Autocorrelate(x, R: TSingleArray; P: Integer;
  lambda: Single; l: Integer = -1);
var
  dl, Rt: TDoubleArray;
  r1, r2, r1t: Double;
  k, i: Integer;
begin
 // Initialization
  if l = -1 then l := Length(x);
  SetLength(dl, l);
  SetLength(Rt, l);
  R[0] := 0;
  Rt[0] := 0;
  r1 := 0;
  r2 := 0;
  r1t := 0;

  for k := 0 to l - 1 do
   begin
    Rt[0] := Rt[0] + x[k] * x[k];
    dl[k] := r1 - lambda * (x[k] - r2);
    r1 := x[k];
    r2 := dl[k];
   end;

  for i := 1 to P do
   begin
    Rt[i] := 0;
    r1 := 0;
    r2 := 0;
    for k := 0 to L - 1 do
     begin
      Rt[i] := Rt[i] + dl[k] * x[k];
      r1t := dl[k];
      dl[k] := r1 - lambda * (r1t - r2);
      r1 := r1t;
      r2 := dl[k];
     end;
   end;

  for i := 1 to P do
    R[i] := Rt[i];
  setlength(Rt, 0);
  setlength(dl, 0);
end;

// Calculate the Levinson-Durbin recursion for the autocorrelation sequence
// R of length P+1 and return the autocorrelation coefficients a and reflection coefficients K
procedure LevinsonRecursion(P: Integer; R, A, K: TSingleArray);
var
  Am1: TDoubleArray;
  i, j, s, m: Integer;
  km, Em1, Em: Double;
  err: Double;
begin
  SetLength(Am1, 62);
  if (R[0] = 0.0) then
    for i := 1 to P do
     begin
      K[i] := 0.0;
      A[i] := 0.0;
     end
  else
   begin
    for j := 0 to P do
     begin
      A[0] := 0;
      Am1[0] := 0;
     end;
    A[0] := 1;
    Am1[0] := 1;
    km := 0;
    Em1 := R[0];
    for m := 1 to P do
     begin
      err := 0.0;
      for j := 1 to m - 1 do
        err := err + Am1[j] * R[m - j];
      km := (R[m] - err) / Em1;
      K[m - 1] := -km;
      A[m] := km;
      for j := 1 to m - 1 do
        A[j] := Am1[j] - km * Am1[m - j];
      Em := (1 - km * km) * Em1;
      for s := 0 to P do
        Am1[s] := A[s];
      Em1 := Em;
     end;
   end;
end;

end.
