unit HVSTAbout;

{$IFDEF FPC}
 {$MODE DELPHI}
 {$WARNINGS OFF}
 {$HINTS OFF}
 {$OVERFLOWCHECKS OFF}
 {$RANGECHECKS ON}
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

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Classes,
  SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls, StdCtrls,
  GR32_Image;

type

  { TFmAbout }

  TFmAbout = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    procedure Label1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FmAbout: TFmAbout;

implementation

{ TFmAbout }

procedure TFmAbout.Label1Click(Sender: TObject);
begin
 Close;
end;

initialization
{$IFDEF FPC}
  {$I HVSTAbout.lrs}
{$ENDIF}

end.

