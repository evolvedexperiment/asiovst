unit PhaserDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, 
  DDSPBase, DVSTModule, DPhaser;

type
  TPhaserModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm);
  private

  public
  end;

implementation

{$R *.DFM}

uses
  PhaserFrm;

procedure TPhaserModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm);
begin
  GUI := TPhaserForm.Create(nil);
  (GUI As TPhaserForm).PhaserModule := Self;
end;

end.