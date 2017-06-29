unit symbolHelper;

{
  Tato unita implementuje pomocne konstanty a funkce pro symboly reliefu.
}

interface

uses Types;

const
  _Usek_Navaznost: array [0..47] of ShortInt =
    (-1,0,1,0,0,-1,0,1,0,1,1,0,-1,0,0,-1,-1,0,0,1,0,-1,1,0,-1,0,1,0,0,-1,0,1,
    0,1,1,0,-1,0,0,-1,-1,0,0,1,0,-1,1,0);
  _Krizeni_Navaznost: array [0..7] of ShortInt =
    (-1,0,1,0,-1,0,1,0);
  _Vyh_Navaznost : array [0..23] of ShortInt =
    (-1,0,0,-1,1,0,-1,0,0,1,1,0,-1,0,0,-1,1,0,-1,0,0,1,1,0);

  _Vyhybka_Start    = 0;
  _Vyhybka_End      = 3;
  _Usek_Start       = 12;
  _Usek_End         = 23;
  _SCom_Start       = 24;
  _SCom_End         = 29;
  _Prj              = 40;
  _Uvazka           = 43;
  _Uvazka_Spr       = 45;
  _Zamek            = 48;
  _Uvazka_Start     = 43;
  _Uvazka_Spr_Index = 45;
  _Vykol_Start      = 49;
  _Vykol_End        = 54;
  _Rozp_Start       = 55;
  _Krizeni_Start    = 58;
  _Krizeni_End      = 59;
  _DKS_Top          = 58;
  _DKS_Bot          = 59;

 _KPopisek_Index    = 377;
 _JCPopisek_Index   = 360;
 _Prj_Index         = 400;
 _Root_Index        = 42;
 _Root_Color        = 3;

 _Separ_Vert_Index  = 352;
 _Separ_Hor_Index   = 572;

type
  TNavDir = (ndPositive = 0, ndNegative = 1);

  TReliefSym=record
   Position:TPoint;
   SymbolID:Integer;
  end;

function GetUsekNavaznost(symbol:Integer; dir:TNavDir):TPoint;

implementation

////////////////////////////////////////////////////////////////////////////////

function GetUsekNavaznost(symbol:Integer; dir:TNavDir):TPoint;
begin
 if ((symbol >= _Usek_Start) and (symbol <= _Usek_End)) then
   Result := Point( _Usek_Navaznost[(symbol - _Usek_Start) * 4 + (2*Integer(dir))],
                    _Usek_Navaznost[(symbol - _Usek_Start) * 4 + (2*Integer(dir)) + 1] )
 else if ((symbol >= _Krizeni_Start) and (symbol <= _Krizeni_End)) then
   Result := Point( _Krizeni_Navaznost[(symbol - _Krizeni_Start) * 4 + (2*Integer(dir))],
                    _Krizeni_Navaznost[(symbol - _Krizeni_Start) * 4 + (2*Integer(dir)) + 1] )
 else Result := Point(0, 0);
end;

////////////////////////////////////////////////////////////////////////////////


end.
