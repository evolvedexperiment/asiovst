unit OpAmpGUI;

{$IFDEF FPC}
 {$MODE DELPHI}
 {$HINTS OFF}
 {$OVERFLOWCHECKS OFF}
 {$RANGECHECKS OFF}
 {$IFDEF CPUI386}
  {$DEFINE CPU386}
  {$ASMMODE INTEL}
 {$ENDIF}
 {$IFDEF FPC_LITTLE_ENDIAN}
  {$DEFINE LITTLE_ENDIAN}
 {$ELSE}
  {$IFDEF FPC_BIG_ENDIAN}
   {$DEFINE BIG_ENDIAN}
  {$ENDIF}
 {$ENDIF}
{$ELSE}
 {$DEFINE LITTLE_ENDIAN}
 {$IFNDEF CPU64}
  {$DEFINE CPU32}
 {$ENDIF}
 {$OPTIMIZATION ON}
{$ENDIF}

interface

uses {$IFDEF FPC}LCLIntf, LResources, {$ENDIF} Messages, SysUtils, Classes,
     Forms, DAVDCommon, DVSTModule, Controls, StdCtrls;

type
  TVSTGUI = class(TForm)
    SBGain: TScrollBar;
    LbGain: TLabel;
    procedure SBGainChange(Sender: TObject);
  private
  public
    theModule: TVSTModule;
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses OpAmpModule;

procedure TVSTGUI.SBGainChange(Sender: TObject);
begin
 TVSTOpAmp(Owner).Parameter[0] := SBGain.Position * 0.1;
end;

{$IFDEF FPC}
initialization
  {$i OpAmpGUI.lrs}
{$ENDIF}

end.