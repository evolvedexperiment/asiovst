unit ASIOVSTGUI;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, DAV_Common,
  DAV_VSTModule, DAV_ASIOHost;

type
  TFmASIOVST = class(TForm)
    Lb_ASIOOutput: TLabel;
    CB_ASIO: TComboBox;
    Memo: TMemo;
    CBShortCircuit: TCheckBox;
    procedure CB_ASIOChange(Sender: TObject);
    procedure Lb_ASIOOutputClick(Sender: TObject);
    procedure CBShortCircuitClick(Sender: TObject);
  public
    procedure DisplayASIOInformation;
  end;

implementation

{$R *.DFM}

uses
  ASIOVSTModule;

procedure TFmASIOVST.CBShortCircuitClick(Sender: TObject);
begin
 with TASIOVSTModule(Owner) do
  begin
   if not CBShortCircuit.Checked
    then AsioHost.OnBufferSwitch32 := AHBufferSwitch
    else AsioHost.OnBufferSwitch32 := AHShortCircuit;
  end;
end;

procedure TFmASIOVST.CB_ASIOChange(Sender: TObject);
begin
 with TASIOVSTModule(Owner) do
  begin
   Parameter[0] := CB_ASIO.ItemIndex;
   DisplayASIOInformation;
  end;
end;

procedure TFmASIOVST.DisplayASIOInformation;
begin
 with TASIOVSTModule(Owner), AsioHost, Memo do
  begin
   Clear;
   Lines.Add('Driver: ' + DriverName);
   Lines.Add('Buffersize: ' + IntToStr(BufferSize));
   Lines.Add('Latency: ' + IntToStr(InputLatency + OutputLatency + Integer(BufferSize)));
   Lines.Add('Channel 1: ' + OutputChannelInfos[0].name);
   Lines.Add('Channel 2: ' + OutputChannelInfos[1].name);
   Lines.Add('Format In 1: ' + ChannelTypeToString(OutputChannelInfos[0].SampleType));
   Lines.Add('Format In 2: ' + ChannelTypeToString(OutputChannelInfos[1].SampleType));
   Lines.Add('Format Out 1: ' + ChannelTypeToString(OutputChannelInfos[0].SampleType));
   Lines.Add('Format Out 2: ' + ChannelTypeToString(OutputChannelInfos[1].SampleType));
  end;
end;

procedure TFmASIOVST.Lb_ASIOOutputClick(Sender: TObject);
begin
 TASIOVSTModule(Owner).AsioHost.ControlPanel;
end;

end.
