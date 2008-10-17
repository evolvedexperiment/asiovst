unit SECommon;

interface

const
  SDK_VERSION = 2230;

  // the 'magic number' that identifies a SynthEdit module (spells SEPL)
  SepMagic  = $5345504C;
  SepMagic2 = SepMagic + 1;

type
  // plug datatype
  TSEPlugDataType = (dtNone = -1, dtEnum, dtText, dtMidi2, dtDouble, dtBoolean,
       dtFSample, dtSingle, dtVstParam, dtInteger, dtBlob);

  // plug direction
  TSEDirection = (drIn, drOut, drContainer_IO, DR_Parameter, drFeature = drIn,
    drCntrl = drOut);

implementation

end.
