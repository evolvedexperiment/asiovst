unit DAV_DspLevinson;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The code is based on the mda VST plug-ins by Paul Kellett, which is       //
//  located at http://sourceforge.net/projects/mda-vst/                       //
//  It was reviewed and rewritten from scratch by Christian-W. Budde          //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2006             //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common;

procedure AutoCorrelate(x, R: TDAVSingleDynArray; P: Integer; lambda: Single; l: Integer = -1);
procedure LevinsonRecursion(P: Integer; R, A, K: TDAVSingleDynArray);
  
implementation

//find the P-order autocorrelation array, R, for the sequence x of length L and warping of lambda
procedure AutoCorrelate(x, R: TDAVSingleDynArray; P: Integer; lambda: Single; l: Integer = -1);
var
  dl, Rt      : TDAVDoubleDynArray;
  r1, r2, r1t : Double;
  k, i        : Integer;
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

  for i := 1 to P do R[i] := Rt[i];
  SetLength(Rt, 0);
  SetLength(dl, 0);
end;

// Calculate the Levinson-Durbin recursion for the autocorrelation sequence
// R of length P+1 and return the autocorrelation coefficients a and reflection coefficients K
procedure LevinsonRecursion(P: Integer; R, A, K: TDAVSingleDynArray);
var
  Am1         : TDAVDoubleDynArray;
  i, j, s, m  : Integer;
  km, Em1, Em : Double;
  err         : Double;
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
     for j := 1 to m - 1 do err := err + Am1[j] * R[m - j];
     km := (R[m] - err) / Em1;
     K[m - 1] := -km;
     A[m] := km;
     for j := 1 to m - 1 do A[j] := Am1[j] - km * Am1[m - j];
     Em := (1 - km * km) * Em1;
     for s := 0 to P do Am1[s] := A[s];
     Em1 := Em;
    end;
  end;
end;

end.
