unit OblastRizeni;

// delkarace struktur Oblasti rizeni

interface

uses Types, Generics.Collections;

const
  _MAX_OR = 8;
  _MAX_OSV = 8;

  _OR_DK_SIZE: TPoint = (X: 5; Y: 3);
  _OR_QUEUE_SIZE: TPoint = (X: 14; Y: 1);
  _OR_TIME_SIZE: TPoint = (X: 16; Y: 1);

type
  // 1 osvetleni
  TORLight = record
    Board: Cardinal;
    Port: Cardinal;
    Name: string; // max 5 characters
  end;

  // prava
  TORRights = record
    ModCasStart: Boolean;
    ModCasStop: Boolean;
    ModCasSet: Boolean;
  end;

  TDKOrientation = (dkoDown = 0, dkoUp = 1);

  // pozice symbolu OR
  TPoss = record
    DK: TPoint;
    DKOr: TDKOrientation;
    Queue: TPoint;
    Time: TPoint;
  end;

  TOROddDirection = (ordLeftToRight = 0, ordRightToLeft = 1);

  // 1 OR
  TOR = class
  public
    Name: string;
    ShortName: string;
    Id: string;
    OddDirection: TOROddDirection;
    Rights: TORRights;
    Lights: TList<TORLight>;
    Poss: TPoss;

    constructor Create();
    destructor Destroy(); override;
  end;

  TORGraphSymbol = (orsDK = 0, orsQueue = 1, orsTime = 2);

  // pouzivao pri presunech OR apod.
  TORGraf = record
    areai: Integer; // index or a moving or (-1 = no moving)
    objType: TORGraphSymbol;
  end;

implementation

constructor TOR.Create();
begin
  inherited;
  Self.Lights := TList<TORLight>.Create();
end;

destructor TOR.Destroy();
begin
  Self.Lights.Free();
  inherited;
end;

end.// unit
