unit LazPlugin;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, LResources, DVSTModule; 

type
  { TVSTModule1 }
  TVSTModule1 = class(TVSTModule)
    procedure VSTModule1Create(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  VSTModule1: TVSTModule1; 

implementation

{ TVSTModule1 }

procedure TVSTModule1.VSTModule1Create(Sender: TObject);
begin
 UniqueID:='ACDC';
end;

initialization
{$I LazPlugin.lrs}
  RegisterInitComponentHandler(TVSTModule1,@InitResourceComponent);

end.

