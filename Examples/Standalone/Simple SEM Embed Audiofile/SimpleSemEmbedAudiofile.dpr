program SimpleSemEmbedAudioFile;

{$R 'DAV_AudioFileOscillator.res' 'DAV_AudioFileOscillator.rc'}

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  Dialogs, StdCtrls, DAV_DLLResources;

{$R *.res}

var
  SeModule : TPEResourceModule;
  RS       : TResourceStream;
  RD       : TResourceDetails;
  DLLName  : string;

function RemoveFileExt(Filename: TFileName): TFileName;
var
  DotPos : Integer;
begin
 result := Filename;
 DotPos := Pos('.', result);
 if DotPos > 0
  then result := Copy(result, 1, DotPos - 1);
end;

begin
 with TOpenDialog.Create(nil) do
  try
   DefaultExt := '.WAV';
   Filter := 'WAV File (*.wav)|*.wav|AIFF File (*.aiff)|*.aif*|AU File (*.au)|*.au';
   Options := [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing];
   if Execute then
    begin
     DLLName := FileName;
     with TSaveDialog.Create(nil) do
      try
       DefaultExt := '.SEM';
       Filter := 'SE Module (*.SEM)|*.SEM';
       if Execute then
        begin
         SeModule := TPEResourceModule.Create;

         RS := TResourceStream.Create(HInstance, 'AudioFileOscillator', 'SEM');
         try
          SeModule.LoadFromStream(RS);
         finally
          FreeAndNil(RS);
         end;

         try
          with TMemoryStream.Create do
           try
            LoadFromFile(DLLName);
            RD := TResourceDetails.CreateResourceDetails(SeModule, 0,
              RemoveFileExt(ExtractFilename(DLLName)), 'WaveTable', Size, Memory);
            SeModule.AddResource(RD);
            SeModule.Dirty := True;

            SeModule.SortResources;
            SeModule.SaveToFile(FileName);

           finally
            Free;
           end;

         finally
          FreeAndNil(SeModule);
         end;
        end;
     finally
      Free;
     end;
    end;
  finally
   Free;
  end;
end.
