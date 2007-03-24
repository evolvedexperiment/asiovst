unit LunchBoxAbout;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}LCLIntf, LMessages, LResources,
  {$ELSE} Windows, Messages,{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, StdCtrls;

type
  TFmAbout = class(TForm)
    Label1: TLabel;
    Lb: TLabel;
    Label2: TLabel;
    procedure FormClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FmAbout: TFmAbout;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

procedure TFmAbout.FormClick(Sender: TObject);
begin
 Close;
end;

{$IFDEF FPC}
initialization
  {$i LunchBoxAbout.lrs}
{$ENDIF}

end.
