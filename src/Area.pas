unit Area;

// Dopravna

interface

uses Types, Generics.Collections;

const
  _OR_DK_SIZE: TPoint = (X: 5; Y: 3);
  _OR_QUEUE_SIZE: TPoint = (X: 14; Y: 1);
  _OR_TIME_SIZE: TPoint = (X: 16; Y: 1);

type
  // 1 osvetleni dopravny
  TAreaLight = record
    Board: Cardinal;
    Port: Cardinal;
    Name: string; // max 5 characters
  end;

  // prava dopravny
  TAreaRights = record
    ModCasStart: Boolean;
    ModCasStop: Boolean;
    ModCasSet: Boolean;
  end;

  TDKOrientation = (dkoDown = 0, dkoUp = 1);

  // pozice symbolu domecku dopravny
  TPoss = record
    DK: TPoint;
    DKOr: TDKOrientation;
    Queue: TPoint;
    Time: TPoint;
  end;

  TOROddDirection = (ordLeftToRight = 0, ordRightToLeft = 1);

  // Dopravna
  TArea = class
  public
    Name: string;
    ShortName: string;
    Id: string;
    OddDirection: TOROddDirection;
    Rights: TAreaRights;
    Lights: TList<TAreaLight>;
    Poss: TPoss;

    constructor Create();
    destructor Destroy(); override;

    function IsDK(pos: TPoint): Boolean;
  end;

  TORGraphSymbol = (orsDK = 0, orsQueue = 1, orsTime = 2);

  // pouzivao pri presunech dopraven apod.
  TORGraf = record
    areai: Integer; // index of a moving area (-1 = no moving)
    objType: TORGraphSymbol;
  end;

implementation

constructor TArea.Create();
begin
  inherited;
  Self.Lights := TList<TAreaLight>.Create();
end;

destructor TArea.Destroy();
begin
  Self.Lights.Free();
  inherited;
end;

function TArea.IsDK(pos: TPoint): Boolean;
begin
  Result := (pos.X >= Self.Poss.DK.X) and (pos.X <= (Self.Poss.DK.X+_OR_DK_SIZE.X)) and
    (pos.Y >= Self.Poss.DK.Y) and (pos.Y <= (Self.Poss.DK.Y+_OR_DK_SIZE.Y))
end;

end.// unit
