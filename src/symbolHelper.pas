unit symbolHelper;

{
  Tato unita implementuje pomocne konstanty a funkce pro symboly reliefu.
}

interface

uses Types, SysUtils, Graphics, Vcl.Controls, Classes, Generics.Collections;

type
  TNavDir = (ndPositive = 0, ndNegative = 1, ndThird = 2);

  TReliefSym = record
    Position: TPoint;
    SymbolID: Integer;
  end;

  SymbolColor = (
    scPurple = 0,
    scGray = 1,
    scRed = 2,
    scLime = 3,
    scWhite = 4,
    scAqua = 5,
    scBlue = 6,
    scYellow = 7,
    scFuchsia = 8,
    scBlack = 9
  );

  TSymbolConnection = record
    a: TPoint;
    b: TPoint;
    c: TPoint;
     function dir(dir: TNavDir): TPoint;
  end;

const
  _SYMBOL_WIDTH = 8;
  _SYMBOL_HEIGHT = 12;

  _SYMBOL_COLORS: array [0..9] of TColor = (
    $FF00FF, // purple
    $A0A0A0, // gray
    clRed,
    clLime,
    clWhite,
    clAqua,
    clBlue,
    clYellow, // yellow
    $800080, // fuchsia
    clBlack
  );

  _AREA_COLORS: array [0..4] of SymbolColor = (
    scBlue,
    scYellow,
    scLime,
    scWhite,
    scGray
  );

  // same for detected & undetected
  _TRACK_CONNECTIONS: array [0..9] of TSymbolConnection = (
    (a: (X: -1; Y: 0); b: (X: 1; Y: 0); c: (X: 0; Y: 0)),
    (a: (X: 0; Y: -1); b: (X: 0; Y: 1); c: (X: 0; Y: 0)),
    (a: (X: 0; Y: 1); b: (X: 1; Y: 0); c: (X: 0; Y: 0)),
    (a: (X: -1; Y: 0); b: (X: 0; Y: -1); c: (X: 0; Y: 0)),
    (a: (X: -1; Y: 0); b: (X: 0; Y: 1); c: (X: 0; Y: 0)),
    (a: (X: 0; Y: -1); b: (X: 1; Y: 0); c: (X: 0; Y: 0)),
    (a: (X: -1; Y: 0); b: (X: 1; Y: 0); c: (X: 0; Y: 1)), // dks start
    (a: (X: -1; Y: 0); b: (X: 1; Y: 0); c: (X: 0; Y: -1)),
    (a: (X: 0; Y: -1); b: (X: 0; Y: 1); c: (X: -1; Y: 0)),
    (a: (X: 0; Y: -1); b: (X: 0; Y: 1); c: (X: 1; Y: 0)) // last dks
  );

  // Turnout connections must folow strictly this rule for branches computation to work
  // A = turnout +, B = turnout -, C = turnout source

  _TURNOUT_CONNECTIONS: array [0..3] of TSymbolConnection = (
    (a: (X: 1; Y: 0); b: (X: 0; Y: -1); c: (X: -1; Y: 0)),
    (a: (X: 1; Y: 0); b: (X: 0; Y: 1); c: (X: -1; Y: 0)),
    (a: (X: -1; Y: 0); b: (X: 0; Y: -1); c: (X: 1; Y: 0)),
    (a: (X: -1; Y: 0); b: (X: 0; Y: 1); c: (X: 1; Y: 0))
  );

  _S_TURNOUT_B = 0;
  _S_TURNOUT_E = 3;

  _S_DERAIL_B = 12;
  _S_DERAIL_E = 23;

  _S_TRACK_DET_B = 24;
  _S_TRACK_DET_BR = 26;
  _S_TRACK_DET_LU = 27;
  _S_TRACK_DET_LD = 28;
  _S_TRACK_DET_UR = 29;
  _S_DKS_DET_TOP = 30;
  _S_DKS_DET_BOT = 31;
  _S_DKS_DET_R = 32;
  _S_DKS_DET_L = 33;
  _S_TRACK_DET_E = 33;

  _S_TRACK_NODET_B = 34;
  _S_TRACK_NODET_BR = 36;
  _S_TRACK_NODET_LU = 37;
  _S_TRACK_NODET_LD = 38;
  _S_TRACK_NODET_UR = 39;
  _S_DKS_NODET_TOP = 40;
  _S_DKS_NODET_BOT = 41;
  _S_DKS_NODET_R = 42;
  _S_DKS_NODET_L = 43;
  _S_TRACK_NODET_E = 43;

  _S_BUMPER_R = 44;
  _S_BUMPER_L = 45;

  _S_SIGNAL_B = 46;
  _S_SIGNAL_E = 51;

  _S_CROSSING = 52;
  _S_LINKER_B = 53;
  _S_LINKER_TRAIN = 55;
  _S_LOCK = 56;
  _S_DISC_TRACK = 57;
  _S_DISC_ALONE = 58;
  _S_PLATFORM_B = 59;
  _S_PLATFORM_E = 61;

  _S_PST_TOP = 62;
  _S_PST_BOT = 63;

  _S_SEPAR_VERT = 71;
  _S_SEPAR_HOR = 72;

  _S_KC = 64;
  _S_FULL = 65;
  _S_CIRCLE = 68;

function LoadIL(ResourceName: string; PartWidth, PartHeight: Byte; MaskColor: TColor = clPurple): TImageList;

function GetTrackContinue(symbol: Integer; dir: TNavDir): TPoint;
function SymbolDrawColor(symbol: Integer): SymbolColor;
function ColorToSymbolColor(color: TColor): SymbolColor;
function SymbolColorToColor(color: SymbolColor): TColor;
function SymbolIndex(symbol: Integer; color: SymbolColor): Integer;
procedure SymbolDraw(IL: TImageList; Canvas: TCanvas; X, Y: Integer; symbol: Integer; color: SymbolColor); overload;
procedure SymbolDraw(IL: TImageList; Canvas: TCanvas; pos: TPoint; symbol: Integer; color: SymbolColor); overload;
function TranscodeSymbolFromBpnlV3(symbol: Integer): Integer;

function CompareReliefSymPos(const left: TReliefSym; const right: TReliefSym): Integer;
function ArePointListsEq(a: TList<TPoint>; b: TList<TPoint>): Boolean;

function AreaColor(areai: Integer): SymbolColor;

implementation

/// /////////////////////////////////////////////////////////////////////////////

function TSymbolConnection.dir(dir: TNavDir): TPoint;
begin
  case (dir) of
    TNavDir.ndPositive: Result := a;
    TNavDir.ndNegative: Result := b;
    TNavDir.ndThird: Result := c;
  end;
end;

function GetTrackContinue(symbol: Integer; dir: TNavDir): TPoint;
begin
  if ((dir = ndThird) and ((symbol < _S_DKS_DET_TOP) or (symbol > _S_DKS_DET_L)) and
      ((symbol < _S_DKS_NODET_TOP) or (symbol > _S_DKS_NODET_L))) then
    raise Exception.Create('Unsupported direction!');

  if ((symbol >= _S_TRACK_DET_B) and (symbol <= _S_TRACK_DET_E)) then
    Result := _TRACK_CONNECTIONS[symbol-_S_TRACK_DET_B].dir(dir)
  else if ((symbol >= _S_TRACK_NODET_B) and (symbol <= _S_TRACK_NODET_E)) then
    Result := _TRACK_CONNECTIONS[symbol-_S_TRACK_NODET_B].dir(dir)
  else if ((symbol = _S_BUMPER_R) and (dir = ndNegative)) then
    Result := Point(1, 0)
  else if ((symbol = _S_BUMPER_L) and (dir = ndPositive)) then
    Result := Point(-1, 0)
  else
    Result := Point(0, 0);
end;

function SymbolDrawColor(symbol: Integer): SymbolColor;
begin
  if ((symbol >= _S_PLATFORM_B) and (symbol <= _S_PLATFORM_E)) then
    Result := scBlue
  else if (symbol = _S_LINKER_TRAIN) then
    Result := scYellow
  else
    Result := scGray;
end;

function ColorToSymbolColor(color: TColor): SymbolColor;
begin
  for var i: Integer := 0 to Length(_SYMBOL_COLORS)-1 do
    if (_SYMBOL_COLORS[i] = color) then
      Exit(SymbolColor(i));
  Result := scPurple;
end;

function SymbolColorToColor(color: SymbolColor): TColor;
begin
  Result := _SYMBOL_COLORS[Integer(color)];
end;

procedure SymbolDraw(IL: TImageList; Canvas: TCanvas; X, Y: Integer; symbol: Integer; color: SymbolColor); overload;
begin
  IL.Draw(Canvas, X*_SYMBOL_WIDTH, Y*_SYMBOL_HEIGHT, (symbol*Length(_SYMBOL_COLORS)) + Integer(color));
end;

procedure SymbolDraw(IL: TImageList; Canvas: TCanvas; pos: TPoint; symbol: Integer; color: SymbolColor); overload;
begin
  SymbolDraw(IL, Canvas, pos.X, pos.Y, symbol, color);
end;

function SymbolIndex(symbol: Integer; color: SymbolColor): Integer;
begin
  Result := symbol*Length(_SYMBOL_COLORS) + Integer(color);
end;

////////////////////////////////////////////////////////////////////////////////

function LoadIL(ResourceName: string; PartWidth, PartHeight: Byte;
  MaskColor: TColor = clPurple): TImageList;
var AllImages, ColouredImages: TBitmap;
  IL: TImageList;
const
  _Symbols_DefColor: TColor = clBlack;
begin
  IL := TImageList.Create(nil);
  AllImages := TBitmap.Create;
  ColouredImages := TBitmap.Create;

  try
    try
      AllImages.LoadFromResourceName(HInstance, ResourceName);

      IL.SetSize(PartWidth, PartHeight);
      ColouredImages.SetSize(PartWidth * Length(_SYMBOL_COLORS), PartHeight);

      for var symbol: Integer := 0 to (AllImages.Width div PartWidth) - 1 do
      begin
        for var i: Integer := 0 to Length(_SYMBOL_COLORS) - 1 do
        begin
          ColouredImages.Canvas.CopyRect(Rect(i * PartWidth, 0, (i * PartWidth) + PartWidth, PartHeight), AllImages.Canvas,
            Rect(symbol * PartWidth, 0, (symbol * PartWidth) + PartWidth, PartHeight));

          for var j: Integer := 0 to PartWidth - 1 do
            for var k: Integer := 0 to PartHeight - 1 do
              if (ColouredImages.Canvas.Pixels[j + (i * PartWidth), k] = _Symbols_DefColor) then
                ColouredImages.Canvas.Pixels[j + (i * PartWidth), k] := _Symbol_Colors[i];
        end; // for i

        IL.AddMasked(ColouredImages, MaskColor);
      end; // for symbol
    except
      IL.Free();
      raise;
    end;
  finally
    ColouredImages.Free();
    AllImages.Free();
  end;

  Result := IL;
end;

////////////////////////////////////////////////////////////////////////////////

function TranscodeSymbolFromBpnlV3(symbol: Integer): Integer;
begin
 if ((symbol >= 12) and (symbol <= 17)) then
   Result := symbol-12 + _S_TRACK_DET_B
 else if ((symbol >= 18) and (symbol <= 23)) then
   Result := symbol-18 + _S_TRACK_NODET_B
 else if ((symbol >= 24) and (symbol <= 29)) then
   Result := symbol-24 + _S_SIGNAL_B
 else if (symbol = 30) then
   Result := _S_BUMPER_R
 else if (symbol = 31) then
   Result := _S_BUMPER_L
 else if ((symbol >= 32) and (symbol <= 34)) then
   Result := symbol-32 + _S_PLATFORM_B
 else if (symbol = 40) then
   Result := _S_CROSSING
 else if (symbol = 42) then
   Result := _S_CIRCLE
 else if ((symbol >= 43) and (symbol <= 45)) then
   Result := symbol-43 + _S_LINKER_B
 else if ((symbol >= 49) and (symbol <= 54)) then
   Result := symbol-49 + _S_DERAIL_B
 else if ((symbol >= 55) and (symbol <= 56)) then
   Result := symbol-55 + _S_DISC_TRACK
 else if ((symbol >= 58) and (symbol <= 59)) then
   Result := symbol-58 + _S_DKS_DET_TOP
 else
   Result := symbol;
end;

////////////////////////////////////////////////////////////////////////////////

function CompareReliefSymPos(const left: TReliefSym; const right: TReliefSym): Integer;
begin
  Result := left.Position.Y - right.Position.Y;
  if (Result = 0) then
    Result := left.Position.X - right.Position.X;
end;

////////////////////////////////////////////////////////////////////////////////

function ArePointListsEq(a: TList<TPoint>; b: TList<TPoint>): Boolean;
begin
  if (a.Count <> b.Count) then
    Exit(False);
  for var i: Integer := 0 to a.Count-1 do
    if (a[i] <> b[i]) then
      Exit(False);
  Result := True;
end;

////////////////////////////////////////////////////////////////////////////////

function AreaColor(areai: Integer): SymbolColor;
begin
  if (areai < 0) then
    Exit(scAqua);
  Result := _AREA_COLORS[areai mod Length(_AREA_COLORS)];
end;

////////////////////////////////////////////////////////////////////////////////

end.
