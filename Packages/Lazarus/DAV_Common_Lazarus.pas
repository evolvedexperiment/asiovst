{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet 
  werden!
  Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit DAV_Common_Lazarus; 

interface

uses
    DAV_Common, DAV_CommonRegister, DAV_Complex, DAV_ComplexData, 
  DAV_MidiFile, DAV_MidiIO, DAV_AudioData, DAV_Approximations, 
  DAV_MpegAudioLayer3, DAV_AudioFile, DAV_AudioFileAIFF, DAV_AudioFileAU, 
  DAV_AudioFileDataCache, DAV_AudioFileWAV, DAV_ChannelDataCoder, 
  DAV_ChunkAiffBasic, DAV_ChunkClasses, DAV_ChunkWaveBasic, 
  DAV_ChunkWaveCustom, DAV_MpegAudio, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('DAV_CommonRegister', @DAV_CommonRegister.Register); 
end; 

initialization
  RegisterPackage('DAV_Common_Lazarus', @Register); 
end.
