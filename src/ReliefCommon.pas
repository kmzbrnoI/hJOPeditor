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

  TGOpStep = ( // graphics operation step
    gosNone = 0,
    gosActive = 1,
    gosSelecting = 2,
    gosMoving = 3
  );

implementation

end.
