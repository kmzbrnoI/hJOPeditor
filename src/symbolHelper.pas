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
    clRed,
    clLime,
    clWhite,
    clAqua,
    clBlue,
    clYellow, // yellow
    $800080, // fuchsia
    clBlack
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
  _DKS_Detek_R = 32;
  _DKS_Detek_L = 33;
  _Usek_Detek_End = 33;

  _Usek_Nedetek_Start = 34;
  _DKS_Nedetek_Top = 40;
  _DKS_Nedetek_Bot = 41;
  _DKS_Nedetek_R = 42;
  _DKS_Nedetek_L = 43;
  _Usek_Nedetek_End = 43;

  _Zarazedlo_r = 44;
  _Zarazedlo_l = 45;

  _Navestidlo_Start = 46;
  _Navestidlo_End = 51;

  _Prejezd = 52;
  _Uvazka_Start = 53;
  _Uvazka_Spr = 55;
  _Zamek = 56;
  _Rozp_Kolej = 57;
  _Rozp_NoKolej = 58;
  _Peron_Start = 59;
  _Peron_End = 61;

  _Pst_Top = 62;
  _Pst_Bot = 63;

  _Separ_Vert = 71;
  _Separ_Hor = 72;

  _KC = 64;
  _Full = 65;
  _Kolecko = 68;


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
function TranscodeSymbolFromBpnlV3(symbol: Integer): Integer;

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
  else if (symbol = _Uvazka_Spr) then
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

procedure SymbolDraw(IL: TImageList; Canvas: TCanvas; X, Y: Integer; symbol: Integer; color: SymbolColor); overload;
begin
  IL.Draw(Canvas, X*_Symbol_Sirka, Y*_Symbol_Vyska, (symbol*Length(_SYMBOL_COLORS)) + Integer(color));
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
   Result := symbol-12 + _Usek_Detek_Start
 else if ((symbol >= 18) and (symbol <= 23)) then
   Result := symbol-18 + _Usek_Nedetek_Start
 else if ((symbol >= 24) and (symbol <= 29)) then
   Result := symbol-24 + _Navestidlo_Start
 else if (symbol = 30) then
   Result := _Zarazedlo_r
 else if (symbol = 31) then
   Result := _Zarazedlo_l
 else if ((symbol >= 32) and (symbol <= 34)) then
   Result := symbol-32 + _Peron_Start
 else if (symbol = 40) then
   Result := _Prejezd
 else if (symbol = 42) then
   Result := _Kolecko
 else if ((symbol >= 43) and (symbol <= 45)) then
   Result := symbol-43 + _Uvazka_Start
 else if ((symbol >= 49) and (symbol <= 54)) then
   Result := symbol-49 + _Vykol_Start
 else if ((symbol >= 55) and (symbol <= 56)) then
   Result := symbol-55 + _Rozp_Kolej
 else if ((symbol >= 58) and (symbol <= 59)) then
   Result := symbol-58 + _DKS_Detek_Top
 else
   Result := symbol;
end;

////////////////////////////////////////////////////////////////////////////////

end.
