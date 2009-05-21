program SemEmbedConvolutionIR;

{$R 'DAV_Convolution.res' 'DAV_Convolution.rc'}

uses
  Forms,
  ECImain in 'ECImain.pas' {FmSemEmbedConvolutionIR};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmSemEmbedConvolutionIR, FmSemEmbedConvolutionIR);
  Application.Run;
end.
