unit symbolHelper;

{
  Tato unita implementuje pomocne konstanty a funkce pro symboly reliefu.
}

interface

uses Types, SysUtils, Graphics, Vcl.Controls;

const
  _Symbol_Sirka = 8;
  _Symbol_Vyska = 12;

  _SYMBOL_COLORS: array [0..9] of TColor = (
    $FF00FF, // purple
    $A0A0A0, // gray
    $0000FF, // red
    $00FF00, // lime
    $FFFFFF, // white
    $FFFF00, // aqua
    $FF0000, // blue
    $00FFFF, // yellow
    $800080, // fuchsia
    $000000  // black
  );

  _Usek_Navaznost: array [0 .. 47] of ShortInt = (-1, 0, 1, 0, 0, -1, 0, 1, 0, 1, 1, 0, -1, 0, 0, -1, -1, 0, 0, 1, 0,
    -1, 1, 0, -1, 0, 1, 0, 0, -1, 0, 1, 0, 1, 1, 0, -1, 0, 0, -1, -1, 0, 0, 1, 0, -1, 1, 0);
  _Krizeni_Navaznost: array [0 .. 11] of ShortInt = (-1, 0, 1, 0, 0, 1, -1, 0, 1, 0, 0, -1);
  _Vyh_Navaznost: array [0 .. 23] of ShortInt = (-1, 0, 0, -1, 1, 0, -1, 0, 0, 1, 1, 0, -1, 0, 0, -1, 1, 0, -1, 0,
    0, 1, 1, 0);

  _Vyhybka_Start = 0;
  _Vyhybka_End = 3;

  _Vykol_Start = 12;
  _Vykol_Top_Start = 12;
  _Vykol_Bot_Start = 18;
  _Vykol_End = 23;

  _Usek_Detek_Start = 24;
  _DKS_Detek_Top = 30;
  _DKS_Detek_Bot = 31;
  _Usek_Detek_End = 31;
  _Usek_Nedetek_Start = 32;
  _Usek_Nedetek_End = 39;

  _Zarazedlo_r = 40;
  _Zarazedlo_l = 41;

  _Navestidlo_Start = 42;
  _Navestidlo_End = 47;

  _Prejezd = 48;
  _Uvazka_Start = 49;
  _Uvazka_Spr = 51;
  _Zamek = 52;
  _Rozp_Kolej = 53;
  _Rozp_NoKolej = 54;
  _Peron_Start = 55;
  _Peron_End = 57;

  _Separ_Vert_Index = 582;
  _Separ_Hor_Index = 582;

  _KC = 60;
  _Full = 61;
  _Kolecko = 64;


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

function LoadIL(ResourceName: string; PartWidth, PartHeight: Byte; MaskColor: TColor = clPurple): TImageList;

function GetUsekNavaznost(symbol: Integer; dir: TNavDir): TPoint;
function SymbolDrawColor(symbol: Integer): SymbolColor;
function ColorToSymbolColor(color: TColor): SymbolColor;
function SymbolIndex(symbol: Integer; color: SymbolColor): Integer;
procedure SymbolDraw(IL: TImageList; Canvas: TCanvas; X, Y: Integer; symbol: Integer; color: SymbolColor); overload;
procedure SymbolDraw(IL: TImageList; Canvas: TCanvas; pos: TPoint; symbol: Integer; color: SymbolColor); overload;

implementation

/// /////////////////////////////////////////////////////////////////////////////

function GetUsekNavaznost(symbol: Integer; dir: TNavDir): TPoint;
begin
  if (((symbol < _DKS_Detek_Top) or (symbol > _DKS_Detek_Bot)) and (dir = ndThird)) then
    raise Exception.Create('Unsupported direction!');

  if ((symbol >= _Usek_Detek_Start) and (symbol <= _Usek_Detek_End)) then
    Result := Point(_Usek_Navaznost[(symbol - _Usek_Detek_Start) * 4 + (2 * Integer(dir))],
      _Usek_Navaznost[(symbol - _Usek_Detek_Start) * 4 + (2 * Integer(dir)) + 1])
  else if ((symbol >= _Usek_Detek_Start) and (symbol <= _Usek_Detek_End)) then
    Result := Point(_Krizeni_Navaznost[(symbol - _Usek_Detek_Start) * 6 + (2 * Integer(dir))],
      _Krizeni_Navaznost[(symbol - _Usek_Detek_Start) * 6 + (2 * Integer(dir)) + 1])
  else if ((symbol = _Zarazedlo_r) and (dir = ndNegative)) then
    Result := Point(1, 0)
  else if ((symbol = _Zarazedlo_l) and (dir = ndPositive)) then
    Result := Point(-1, 0)
  else
    Result := Point(0, 0);
end;

function SymbolDrawColor(symbol: Integer): SymbolColor;
begin
  if ((symbol >= _Peron_Start) and (symbol <= _Peron_End)) then
    Result := scBlue
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

procedure SymbolDraw(IL: TImageList; Canvas: TCanvas; X, Y: Integer; symbol: Integer; color: SymbolColor); overload;
begin
  IL.Draw(Canvas, X*_Symbol_Sirka, Y*_Symbol_Vyska, (symbol*10) + Integer(color));
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
  i, j, k, symbol: Byte;
  IL: TImageList;
const
  _Symbols_DefColor: TColor = clBlack;
begin
  IL := TImageList.Create(nil);
  AllImages := TBitmap.Create;
  ColouredImages := TBitmap.Create;

  try
    AllImages.LoadFromResourceName(HInstance, ResourceName);

    IL.SetSize(PartWidth, PartHeight);
    ColouredImages.SetSize(PartWidth * Length(_SYMBOL_COLORS), PartHeight);

    for symbol := 0 to (AllImages.Width div PartWidth) - 1 do
    begin
      for i := 0 to Length(_SYMBOL_COLORS) - 1 do
      begin
        ColouredImages.Canvas.CopyRect(Rect(i * PartWidth, 0, (i * PartWidth) + PartWidth, PartHeight), AllImages.Canvas,
          Rect(symbol * PartWidth, 0, (symbol * PartWidth) + PartWidth, PartHeight));

        for j := 0 to PartWidth - 1 do
          for k := 0 to PartHeight - 1 do
            if (ColouredImages.Canvas.Pixels[j + (i * PartWidth), k] = _Symbols_DefColor) then
              ColouredImages.Canvas.Pixels[j + (i * PartWidth), k] := _Symbol_Colors[i];
      end; // for i

      IL.AddMasked(ColouredImages, MaskColor);
    end; // for symbol

    ColouredImages.Free();
    AllImages.Free();
  except
    IL.Free();
    raise;
  end;

  ColouredImages.Free();
  AllImages.Free();
  Result := IL;
end;

////////////////////////////////////////////////////////////////////////////////

end.
