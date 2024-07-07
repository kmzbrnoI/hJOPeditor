unit ReliefCommon;

interface

uses System.SysUtils;

type
  EFileLoad = class(Exception);

  TReliefFileState = (
    fsClosed = 0,
    fsUnsaved = 1,
    fsSaved = 2
  );

implementation

end.
