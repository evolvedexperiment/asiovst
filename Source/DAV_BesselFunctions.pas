unit DAV_BesselFunctions;
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
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2009             //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  DAV_Types;

function ModifiedBesselFirstKindOrderZero(Value: Double): Double;

implementation

uses
  DAV_Math;

// Modified Bessel function of the first kind of order zero
// minimax rational approximations on intervals, see
// Blair and Edwards, Chalk River Report AECL-4928, 1974

const
  P1 : array [0..14] of Double = (-2.2335582639474375249E+15,
      -5.5050369673018427753e+14, -3.2940087627407749166e+13,
      -8.4925101247114157499e+11, -1.1912746104985237192e+10,
      -1.0313066708737980747e+08, -5.9545626019847898221e+05,
      -2.4125195876041896775e+03, -7.0935347449210549190e+00,
      -1.5453977791786851041e-02, -2.5172644670688975051e-05,
      -3.0517226450451067446e-08, -2.6843448573468483278e-11,
      -1.5982226675653184646e-14, -5.2487866627945699800e-18);

  Q1 : array [0..5] of Double =  (-2.2335582639474375245e+15,
       7.8858692566751002988e+12, -1.2207067397808979846e+10,
       1.0377081058062166144e+07, -4.8527560179962773045e+03, 1.0);

  P2 : array [0..6] of Double = (
        -2.2210262233306573296e-04, 1.3067392038106924055e-02,
        -4.4700805721174453923e-01, 5.5674518371240761397,
        -2.3517945679239481621e+01, 3.1611322818701131207e+01,
        -9.609002196865618);

  Q2 : array [0..7] of Double =  (-5.5194330231005480228e-04,
    3.2547697594819615062e-02, -1.1151759188741312645,
    1.3982595353892851542e+01, -6.0228002066743340583e+01,
    8.5539563258012929600e+01, -3.1446690275135491500e+01, 1);

function ModifiedBesselFirstKindOrderZero(Value: Double): Double;
var
  Temp : Double;
begin
 // even function
 Value := Abs(Value);

 if Value = 0 then Result := 1 else
 if Value <= 15
  then Result := EvaluatePolynomial(P1, Sqr(Value)) / EvaluatePolynomial(Q1, Sqr(Value))
  else
  begin
    Temp := 1 / Value - 1 / 15;
    Result := exp(Value) / sqrt(Value) *
      EvaluatePolynomial(P2, Temp) / EvaluatePolynomial(Q2, Temp);
  end;
end;

end.
