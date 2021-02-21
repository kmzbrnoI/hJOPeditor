unit symbolHelper;

{
  Tato unita implementuje pomocne konstanty a funkce pro symboly reliefu.
}

interface

uses Types, SysUtils;

const
  _Symbol_Sirka = 8;
  _Symbol_Vyska = 12;

  _Usek_Navaznost: array [0 .. 47] of ShortInt = (-1, 0, 1, 0, 0, -1, 0, 1, 0, 1, 1, 0, -1, 0, 0, -1, -1, 0, 0, 1, 0,
    -1, 1, 0, -1, 0, 1, 0, 0, -1, 0, 1, 0, 1, 1, 0, -1, 0, 0, -1, -1, 0, 0, 1, 0, -1, 1, 0);
  _Krizeni_Navaznost: array [0 .. 11] of ShortInt = (-1, 0, 1, 0, 0, 1, -1, 0, 1, 0, 0, -1);
  _Vyh_Navaznost: array [0 .. 23] of ShortInt = (-1, 0, 0, -1, 1, 0, -1, 0, 0, 1, 1, 0, -1, 0, 0, -1, 1, 0, -1, 0,
    0, 1, 1, 0);

  _Vyhybka_Start = 0;
  _Vyhybka_End = 3;
  _Usek_Start = 12;
  _Usek_End = 23;
  _Nedetek_Start = 18;
  _Nedetek_End = 23;
  _SCom_Start = 24;
  _SCom_End = 29;
  _Zarazedlo_r = 30;
  _Zarazedlo_l = 31;
  _Peron_Start = 32;
  _Prj = 40;
  _Osvetleni = 41;
  _Kolecko = 42;
  _Uvazka = 43;
  _Uvazka_Spr = 45;
  _Zamek = 48;
  _Uvazka_Start = 43;
  _Uvazka_Spr_Index = 45;
  _Vykol_Start = 49;
  _Vykol_End = 54;
  _Rozp_Start = 55;
  _Krizeni_Start = 58;
  _Krizeni_End = 59;
  _DKS_Top = 58;
  _DKS_Bot = 59;

  _KPopisek_Index = 377;
  _JCPopisek_Index = 360;
  _Soupravy_Index = 376;
  _Prj_Index = 400;
  _Root_Index = 42;
  _Root_Color = 3;

  _Separ_Vert_Index = 352;
  _Separ_Hor_Index = 572;

  // zde je definovano, jaky symbol se ma vykreslovat jakou barvou (mimo separatoru)
  _Bitmap_DrawColors: array [0 .. 60] of Byte = (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 6, 6, 6, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 7, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);

type
  TNavDir = (ndPositive = 0, ndNegative = 1, ndThird = 2);

  TReliefSym = record
    Position: TPoint;
    SymbolID: Integer;
  end;

function GetUsekNavaznost(symbol: Integer; dir: TNavDir): TPoint;

implementation

/// /////////////////////////////////////////////////////////////////////////////

function GetUsekNavaznost(symbol: Integer; dir: TNavDir): TPoint;
begin
  if (((symbol < _Krizeni_Start) or (symbol > _Krizeni_End)) and (dir = ndThird)) then
    raise Exception.Create('Unsupported direction!');

  if ((symbol >= _Usek_Start) and (symbol <= _Usek_End)) then
    Result := Point(_Usek_Navaznost[(symbol - _Usek_Start) * 4 + (2 * Integer(dir))],
      _Usek_Navaznost[(symbol - _Usek_Start) * 4 + (2 * Integer(dir)) + 1])
  else if ((symbol >= _Krizeni_Start) and (symbol <= _Krizeni_End)) then
    Result := Point(_Krizeni_Navaznost[(symbol - _Krizeni_Start) * 6 + (2 * Integer(dir))],
      _Krizeni_Navaznost[(symbol - _Krizeni_Start) * 6 + (2 * Integer(dir)) + 1])
  else if ((symbol = _Zarazedlo_r) and (dir = ndNegative)) then
    Result := Point(1, 0)
  else if ((symbol = _Zarazedlo_l) and (dir = ndPositive)) then
    Result := Point(-1, 0)
  else
    Result := Point(0, 0);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
