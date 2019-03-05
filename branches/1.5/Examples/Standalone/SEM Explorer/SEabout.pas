{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2013          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit SEabout;

interface

{$I ..\DAV_Compiler.inc}

uses
{$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, DAV_GuiLabel,
  DAV_GuiGraphicControl;

type
  TFormAbout = class(TForm)
    ImageSynthEdit: TImage;
    LabelSem: TGuiLabel;
    LabelAbout: TLabel;
    LabelDelphiASIOVST: TLabel;
    LabelPainting: TLabel;
    LabelExplorer: TGuiLabel;
    procedure FormClick(Sender: TObject);
    procedure ImageSynthEditClick(Sender: TObject);
    procedure LabelDelphiASIOVSTClick(Sender: TObject);
    procedure LabelAboutClick(Sender: TObject);
  end;

var
  FormAbout: TFormAbout;

implementation

uses
  ShellAPI;

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

procedure TFormAbout.FormClick(Sender: TObject);
begin
  Close;
end;

procedure TFormAbout.ImageSynthEditClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open',
    'http://jacky.theprize.googlepages.com/paintings', nil, nil, SW_SHOW);
end;

procedure TFormAbout.LabelAboutClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'http://www.savioursofsoul.de/Christian', nil,
    nil, SW_SHOW);
end;

procedure TFormAbout.LabelDelphiASIOVSTClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'http://delphiasiovst.sourceforge.net', nil,
    nil, SW_SHOW);
end;

end.
