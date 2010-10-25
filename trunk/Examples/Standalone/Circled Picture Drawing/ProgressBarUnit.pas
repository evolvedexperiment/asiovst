unit ProgressBarUnit;

{$I DAV_Compiler.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls;

type
  TFmProgressBar = class(TForm)
    ProgressBar: TProgressBar;
  end;

implementation

{$R *.lfm}

end.

