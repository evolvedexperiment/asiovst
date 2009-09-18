unit DAV_DspPinkNoiseGenerator;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_Complex, DAV_DspCommon;

type
  TPinkNoiseGenerator = class(TDspPersistent)
  protected
    FContribution : Array [0..4] of Double;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    function ProcessSample: Double; virtual;
  end;

implementation

{ TPinkNoiseGenerator }

constructor TPinkNoiseGenerator.Create;
begin
 inherited;
 FillChar(FContribution[0], Length(FContribution) * SizeOf(Double), 0);
end;

procedure TPinkNoiseGenerator.AssignTo(Dest: TPersistent);
begin
 if Dest is TPinkNoiseGenerator then
  with TPinkNoiseGenerator(Dest) do
   begin
    FContribution := Self.FContribution;
   end
 else inherited;
end;

function TPinkNoiseGenerator.ProcessSample: Double;
var
  ur1      : Double;
const
  pA   : Array [0..4] of Double = (0.23980, 0.18727, 0.1638, 0.194685, 0.214463);
  pSUM : Array [0..4] of Double = (0.00198, 0.01478, 0.06378, 0.23378, 0.91578);
begin
 ur1 := random;
 if (ur1 <= pSUM[0]) then FContribution[0] := (2 * random - 1) * pA[0] else
 if (ur1 <= pSUM[1]) then FContribution[1] := (2 * random - 1) * pA[1] else
 if (ur1 <= pSUM[2]) then FContribution[2] := (2 * random - 1) * pA[2] else
 if (ur1 <= pSUM[3]) then FContribution[3] := (2 * random - 1) * pA[3] else
 if (ur1 <= pSUM[4]) then FContribution[4] := (2 * random - 1) * pA[4];
 result := (FContribution[0] + FContribution[1] +
            FContribution[2] + FContribution[3] +
            FContribution[4]);
end;

end.
