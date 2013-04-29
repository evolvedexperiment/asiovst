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

unit DAV_AsioDriverBasicModule;

// This unit implements the basic AsioDriver-Plugin <--> Host communications

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, SysUtils, DAV_Common, DAV_Asio;

type
  TBasicAsioDriverModuleClass = class of TBasicAsioDriverModule;

  TBasicAsioDriverModule = class
    ({$IFDEF UseDelphi}TDataModule{$ELSE}TComponent{$ENDIF})
  private
{$IFNDEF UseDelphi}
    FOnDestroy: TNotifyEvent;
    FOnCreate: TNotifyEvent;
{$ENDIF}
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
{$IFNDEF UseDelphi}
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
{$ENDIF}
  end;

  EAsioDriverError = class(Exception);

implementation

{ TBasicAsioDriverModule }

constructor TBasicAsioDriverModule.Create(AOwner: TComponent);
begin
  inherited;
  // not implemented
end;

destructor TBasicAsioDriverModule.Destroy;
begin
  // not implemented
  inherited;
end;

end.
