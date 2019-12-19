unit ReliefZT;
//mod reliefu Zaverova tabulka
//tento mod umoznuje editaci jizdnich cest
//pri vstupu do nej dochazi k vyhodoceni useku podle jejich korene

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, IniFiles,
  StrUtils, symbolHelper, Menus, Forms, DXDraws, Global, ownStrUtils;

const
 _MAX_USEK          = 256;       //maximalni pocet useku
 _MAX_VYH           = 256;       //maxiamlni pocet vyhybek
 _MAX_VETVE         = 16;        //maxiamlni pocet vetvi jednoho useku
 _MAX_JCCLICK       = 16;
 _MAX_KPOPISEK      = 16;
 _MAX_POPISKY       = 64;
 _MAX_JC            = 256;

 _INI_ZT_SECT       = 'ZT';

 _color_Normal   = 1;
 _color_VC       = 3;
 _color_PC       = 4;

 _ZT_FileVersion = '1.0';

type

//usek je zde ulozen rozdeleny na vetve

//vetev useku
TVetev=record             //vetev useku
 vyh:Integer;               //reference na vyhybku, ktera ukoncuje tuto vetev
                            //referuje na index v poli vyhybek (nikoliv na technologicke ID vyhybky!)
                            //pokud usek nema vyhybky -> vyh = -1 (nastava u useku bez vyhybky a u koncovych vetvi)
                            //vychazi z predpokladu, ze kazda vetev obsahuje na konci maximalne jednou vyhybku!
 ref_plus,ref_minus:Integer;//reference  na vetev, kterou se pokracuje, pokud je vyh v poloze + resp. poloze -
                            //posledni vetev resp. usek bez vyhybky ma obe reference = -1
 Symbols:array of TReliefSym;
                            //s timto dynamicky alokovanym polem je potreba zachaze opradu opatrne
                            //realokace trva strasne dlouho !
                            //pole se bude plnit jen jednou !
end;

//usek
TUsek=record              //usek
 Blok:Integer;              //ID bloku technologie
 OblRizeni:Integer;         //ID oblasti rizeni

 Vetve:record               //vetve useku
   //vetev 0 je vzdy koren
   //zde je ulozen binarni strom v pseudo-forme
     //na 0. indexu je koren, kazdy dalsi vrchol pak obsahuje referenci na jeho predka
     //neukladame klasicky, protoze strom muze byt velmi nevyvazeny!
   Data:array [0.._MAX_VETVE] of TVetev;
                              //staticke pole se zde podle me vyplati, narocnost struktury uvnitr je kvuli dynamickemu poli vevnitr velmi mala
   Count:Byte;
  end;//Symbols
 JCClick:record             //databeze poli koncu JC
   Data:array [0.._MAX_JCCLICK] of TPoint;
   Count:Byte;
  end;//JCClick
 KPopisek:record            //databaze pozic popisku koleje
   Nazev:string;
   Data:array [0.._MAX_KPOPISEK] of TPoint;
   Count:Byte;
  end;//JCClick
end;

//vyhybka
TVyhybka=record           //vyhybka
 Blok:Integer;              //ID bloku technologie
 PolohaPlus:Byte;           //jaka poloha je skutecne +
 Position:TPoint;           //souradnice pozice
 SymbolID:Integer;          //ID symbolu
 obj:integer;               //index useku, na kterem vyhybka je
 OblRizeni:Integer;         //ID oblasti rizeni
end;//Navestidlo

//popisek
TPopisek=record           //popisek - text v layoutu
 Text:string;               //obsah textu
 Position:TPoint;           //pozice
 Color:Integer;             //barva
end;//Text

//navestidlo
TNavestidlo=record        //navestidlo
 Blok:Integer;              //ID technologieho bloku
 Position:TPoint;           //pozice
 SymbolID:Integer;          //ID symbolu
 OblRizeni:Integer;         //oblast rizeni
end;//Navestidlo

//pomocny objekt
TPomocnyObj=record        //pomocny objekt, napr. peron
 Pos:array of TPoint;       //souradnice objektu, tady se pdle me dynamicke pole vyplati
 Symbol:Integer;            //ID symbolu
end;//PomocnyObj

////////////////////////////////////////////////////////////////////////////////

TJCType = (VC = 0, PC = 1);

TVyhZaver=record          //zaver vyhybky
 blok:Smallint;             //technologicke ID
 poloha:Shortint;           //poloha: 0 = + ; 1 = -
end;

TJC = record              //jizdni cesta
 nazev:string;              //nazev JC
 nav:Smallint;              //navestidlo - technologicke ID
 typ:TJCType;               //typ jizdni cesty
 poznamka:string;           //poznamka
 dalsiNav:Smallint;         //navaznost na dalsi navestidlo
                            //-2 = trat
                            //-1 = nic
                            //0 .. n-1 = index v poli navestidel
 vyhybky:array of TVyhZaver;
 odvraty:array of TVyhZaver;
 Useky:array of Cardinal;   //pole referenci na useky - technologicke ID
 trat:record                //navaznost na trat
  index:Integer;              //-1 = zadna trat, 0.. n-1 = index trati
  smer:Byte;                  //0..1
 end;
 prejezdy:string;           //navaznost na prejezdy, zde jsou technologicke indexy oddelene strednikem
 rychlost:byte;             //rychlost v JC 1..12 = 10 - 120 km/h
end;

////////////////////////////////////////////////////////////////////////////////

TUseky      = array of TUsek;
TNavestidla = array of TNavestidlo;
TPopisky    = array of TPopisek;
TPomocneObj = array of TPomocnyObj;
TVyhybky    = array of TVyhybka;
TJCs        = array [0.._MAX_JC-1] of TJC;

////////////////////////////////////////////////////////////////////////////////

TPanelZT = class
  private
   //symboly na reliefu
   Useky      : TUseky;
   Vyhybky    : TVyhybky;
   Navestidla : TNavestidla;
   Popisky    : TPopisky;
   PomocneObj : TPomocneObj;
   JCs        : record           //tato data se budou intenzivne menit - proto jsou staticky alokovana
      data:TJCs;
      count:Cardinal;
   end;


   FSoubor:string;
   FStav:ShortInt;
            //FileSystemStav:
               //0 - soubor zavren
               //1 - soubor neulozen
               //2 - soubor ulozen


   DrawObject:record
    Canvas:TCanvas;
    SymbolIL,TextIL:TImageList;
    Width,Height:Integer;
   end;

    //nacitani a ukladani souboru *.zt (= pouze zaverove tabulky)
    function ZTLoad(mem:TMemIniFile):Byte; overload;
    procedure ZTSave(mem:TMemIniFile); overload;


  public
    constructor Create(SymbolIL,TextIL:TImageList;DrawCanvas:TCanvas;Width,Height:Integer;Parent:TDXDraw);
    destructor Destroy; override;

    //nacitani a ukaldani souboru *.zpnl
    function FLoad(aFile:string;var ORs:string):Byte;
    procedure FSave(aFile:string;const ORs:string);

    //nacitani a ukladani souboru *.zt (= pouze zaverove tabulky)
    function ZTLoad(aFile:string):Byte; overload;
    procedure ZTSave(aFIle:string); overload;

    //import dat z TPanelObjects
    procedure Import(PanelObjects: TObject);

    procedure Paint();
    function PaintCursor(CursorPos:TPoint):TCursorDraw;

    procedure ResetPanel();                    //maze vsechna data - uvadi tridu do vychoziho stavu
    procedure Escape();

    function MouseUp(Position:TPoint;Button:TMouseButton):Byte;
    procedure MouseMove(Position:TPoint);
    function DblClick(Position:TPoint):Byte;

    function CheckValid(var error_cnt:Byte):TStrings;     //overi validitu naeditovanych dat a vrati chybove hlasky

    property PanelWidth:Integer read DrawObject.Width;
    property PanelHeight:Integer read DrawObject.Height;
    property Soubor:string read FSoubor;
    property FileStav:ShortInt read FStav;
end;//TPanelObjects

////////////////////////////////////////////////////////////////////////////////

//Format zaveroveho souboru reliefu: *.zpnl
//ini soubor
//'G'-globalni vlastnost
//    ver: verze souboru

//'P'-Panel: zakladni udaje
//    W= width (sirka reliefu)
//    H= height (vyska reliefu)
//    U= pocet useku
//    N= pocet navestidel
//    T= pocet textu (popisku)
//    P= pocet pomocnych objektu
//    V= pocet vyhybek

//'U1'..'Un' - sekce useku
//  B= [blok technologie, jemuz je symbol prirazen]
//  VC= vetve count = [pocet vetvi]
//  V0...V(N-1)= [vetve] ulozeno textove
//    pevne delky: 0-2..vyhybka - index v poli vyhybek
//                 3-4..index nasledovnika polohy +
//                 5-6..index nasledovnika polohy -
//
//    [-           7-9..souradnice X
//               10-12..souradnice Y
//               13-15..symbol  -]  <- tato cast se opakuje
//  C= [JCClick] ulozeno textove
//    pevne delky: 0-2..souradnice X
//                 3-5..souradnice Y
//  P= [KPopisek] ulozeno textove
//    pevne delky: 0-2..souradnice X
//                 3-5..souradnice Y
//  N= [nazev koleje] ulozeno textove
//  OR= [oblast rizeni] integer 0-n
//  R= [root, koren] X;Y

//'N1'..'Nn' - sekce navestidel
//  B= [asociovany blok technologie]
//  X= [pozice X]
//  Y= [pozice Y]
//  S= [symbol]
//     0..3 (4 typy navestidel)
//  OR= [oblast rizeni] integer 0-n

//'P0'..'Pn' - sekce pomocnych bloku
//  1 blok vzdy v 1 objektu
//  P= [pozice] - 3*X;3*Y;... (bez stredniku - pevne delky)
//  S= [symbol]

//'T1'..'Tn' - sekce textu
//  T= [text]
//  X= [pozice X]
//  Y= [pozice Y]
//  C= [barva]

//'V0'..'Vn' - sekce vyhybek
//  B= [asociovany blok technologie (ID)]
//  S= [symbol]
//  P= [poloha plus]
//  X= [pozice X]
//  Y= [pozice Y]
//  O= [objekt, kteremu vyhybka patri]
//  OR= [oblast rizeni] integer 0-n

////////////////////////////////////////////////////////////////////////////////

//Format zaverove tabulky (soubor *.zt, nebo sekce [ZT] v souboru *.zpnl)
//ini soubor
//'ZT' - sekce zaverova tabulky
//  '0'..'n-1' - jednotlive zaznamy

//format zaznamu:
//  textovy retezec, kde separatory jsou (dle priority): |;~
//  tj. text se nedriv rozdeli podle |, tyto casti pak (pokud je potreba) podle ; atp.

//format ulozeni dat v radku:
//  nazev_jc   - string
//  typ_jc     - cislo
//  poznamka   - string
//  navestidlo - techmologicke ID bloku
//  prejezd    - string
//      format: cisla oddelena ;, znaci technologicke indexy jednotlivych prejezdu
//  trat_index - cislo
//  trat_smer  - cislo 0,1
//  dalsi_nav  - technologicke ID bloky, popr. -2 = trat, -1 = nic
//  rychlost   - cislo
//  useky      - string
//      format: cisla oddelena ;, znaci technologicka ID jednotlivych useku v poradi od nav po koncovy blok
//  vyhybky    - string
//      format: id,poloha;id,poloha; ...    kde ID je technololgicke ID vyhybky a poloha je cislo 0,1 (+,-)
//  odvraty    - string
//      format: stejny, jako vyhybky

////////////////////////////////////////////////////////////////////////////////

implementation

////////////////////////////////////////////////////////////////////////////////

constructor TPanelZT.Create(SymbolIL,TextIL:TImageList;DrawCanvas:TCanvas;Width,Height:Integer;Parent:TDXDraw);
begin

end;//ctor

destructor TPanelZT.Destroy();
begin

end;//dtor

////////////////////////////////////////////////////////////////////////////////

function TPanelZT.FLoad(aFile:string;var ORs:string):Byte;
var i,j,k:Integer;
    inifile:TMemIniFile;
    BlkNazvy:TStrings;
    Obj:string;
    ver:string;
    sect_str:TStrings;
begin
 Self.FStav := 2;
 Self.FSoubor := aFile;

 Self.ResetPanel();

 //kontrola existence
 if (not FileExists(aFile)) then
  begin
   Result := 1;
   Exit;
  end;

 //samotne nacitani dat
 inifile := TMemIniFile.Create(aFile, TEncoding.UTF8);

 //kontrola verze
 ver := inifile.ReadString('G','ver',_ZT_FileVersion);
 if (_ZT_FileVersion <> ver) then
  begin
   if (Application.MessageBox(PChar('Načítáte soubor s verzí '+ver+#13#10+'Aplikace momentálně podporuje verzi '+_ZT_FileVersion+#13#10+'Chcete pokračovat?'), 'Varování', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
    begin
     Result := 2;
     Exit;
    end;
  end;

 Self.DrawObject.Height := inifile.ReadInteger('P','H',0);
 Self.DrawObject.Width  := inifile.ReadInteger('P','W',0);

 BlkNazvy := TStringList.Create;

 SetLength(Self.Useky      , inifile.ReadInteger('P','U',0));
 SetLength(Self.Vyhybky    , inifile.ReadInteger('P','V',0));
 SetLength(Self.Navestidla , inifile.ReadInteger('P','N',0));
 SetLength(Self.PomocneObj , inifile.ReadInteger('P','P',0));
 SetLength(Self.Popisky    , inifile.ReadInteger('P','T',0));
 BlkNazvy.Free;

 //oblati rizeni
 //tato silena metoda vytvoreni binarniho souboru opravdu zjednodusuje cely program
 sect_str := TStringList.Create();
 inifile.ReadSection('OR',sect_str);
 ORs := '';
 for i := 0 to sect_str.Count-1 do
    ORs := ORs + inifile.ReadString('OR',sect_str[i],'')+#13;
 ORs := ORs+#13;
 sect_str.Free;

 //useky
 for i := 0 to Length(Self.Useky)-1 do
  begin
   Self.Useky[i].Blok := inifile.ReadInteger('U'+IntToStr(i),'B',-1);

   //OR
   Self.Useky[i].OblRizeni := inifile.ReadInteger('U'+IntToStr(i),'OR',-1);

   //nacitani vetvi:
   Self.Useky[i].Vetve.Count := inifile.ReadInteger('U'+IntToStr(i), 'VC', 0);
   for j := 0 to Self.Useky[i].Vetve.Count-1 do
    begin
     obj := inifile.ReadString('U'+IntToStr(i), 'V'+IntToStr(j), '');

     Self.Useky[i].Vetve.Data[j].vyh       := StrToIntDef(copy(obj, 0, 3), 0);
     Self.Useky[i].Vetve.Data[j].ref_plus  := StrToIntDef(copy(obj, 3, 2), 0);
     Self.Useky[i].Vetve.Data[j].ref_minus := StrToIntDef(copy(obj, 5, 2), 0);

     obj := RightStr(obj, Length(obj)-7);

     SetLength(Self.Useky[i].Vetve.Data[j].Symbols, Length(obj) div 9);

     for k := 0 to Length(Self.Useky[i].Vetve.Data[j].Symbols)-1 do
      begin
       Self.Useky[i].Vetve.Data[j].Symbols[k].Position.X := StrToIntDef(copy(obj, 9*k, 3), 0);
       Self.Useky[i].Vetve.Data[j].Symbols[k].Position.Y := StrToIntDef(copy(obj, (9*k + 3), 3), 0);
       Self.Useky[i].Vetve.Data[j].Symbols[k].SymbolID   := StrToIntDef(copy(obj, (9*k + 6), 3), 0);
      end;
    end;//for j

   //JCClick
   obj := inifile.ReadString('U'+IntToStr(i),'C','');
   Self.Useky[i].JCClick.Count := (Length(obj) div 6);
   for j := 0 to Self.Useky[i].JCClick.Count - 1 do
    begin
     Self.Useky[i].JCClick.Data[j].X := StrToIntDef(copy(obj,j*6+1,3),0);
     Self.Useky[i].JCClick.Data[j].Y := StrToIntDef(copy(obj,j*6+4,3),0);
    end;//for j

   //KPopisek
   obj := inifile.ReadString('U'+IntToStr(i),'P','');
   Self.Useky[i].KPopisek.Count := (Length(obj) div 6);
   for j := 0 to Self.Useky[i].KPopisek.Count - 1 do
    begin
     Self.Useky[i].KPopisek.Data[j].X := StrToIntDef(copy(obj,j*6+1,3),0);
     Self.Useky[i].KPopisek.Data[j].Y := StrToIntDef(copy(obj,j*6+4,3),0);
    end;//for j

   //Nazev
   Self.Useky[i].KPopisek.Nazev := inifile.ReadString('U'+IntToStr(i),'N','');
  end;//for i

 //navestidla
 for i := 0 to Length(Self.Navestidla)-1 do
  begin
   Self.Navestidla[i].Blok       := inifile.ReadInteger('N'+IntToStr(i), 'B' , -1);
   Self.Navestidla[i].Position.X := inifile.ReadInteger('N'+IntToStr(i), 'X' ,  0);
   Self.Navestidla[i].Position.Y := inifile.ReadInteger('N'+IntToStr(i), 'Y' ,  0);
   Self.Navestidla[i].SymbolID   := inifile.ReadInteger('N'+IntToStr(i), 'S' ,  0);
   Self.Navestidla[i].OblRizeni  := inifile.ReadInteger('N'+IntToStr(i), 'OR', -1);
  end;//for i

 //pomocne symboly
 for i := 0 to Length(Self.PomocneObj)-1 do
  begin
   Self.PomocneObj[i].Symbol := inifile.ReadInteger('P'+IntToStr(i),'S',0);

   obj := inifile.ReadString('P'+IntToStr(i),'P','');
   SetLength(Self.PomocneObj[i].Pos, (Length(obj) div 6));
   for j := 0 to Length(Self.PomocneObj[i].Pos)-1 do
    begin
     Self.PomocneObj[i].Pos[j].X := StrToIntDef(copy(obj,j*6+1,3),0);
     Self.PomocneObj[i].Pos[j].Y := StrToIntDef(copy(obj,j*6+4,3),0);
    end;//for j
  end;//for i

 //popisky
 for i := 0 to Length(Self.Popisky)-1 do
  begin
   Self.Popisky[i].Text       := inifile.ReadString('T'+IntToStr(i) , 'T', '0');
   Self.Popisky[i].Position.X := inifile.ReadInteger('T'+IntToStr(i), 'X', 0);
   Self.Popisky[i].Position.Y := inifile.ReadInteger('T'+IntToStr(i), 'Y', 0);
   Self.Popisky[i].Color      := inifile.ReadInteger('T'+IntToStr(i), 'C', 0);
  end;//for i

 //Vyhybky
 for i := 0 to Length(Self.Vyhybky)-1 do
  begin
   Self.Vyhybky[i].Blok        := inifile.ReadInteger('V'+IntToStr(i), 'B' , -1);
   Self.Vyhybky[i].SymbolID    := inifile.ReadInteger('V'+IntToStr(i), 'S' ,  0);
   Self.Vyhybky[i].PolohaPlus  := inifile.ReadInteger('V'+IntToStr(i), 'P' ,  0);
   Self.Vyhybky[i].Position.X  := inifile.ReadInteger('V'+IntToStr(i), 'X' ,  0);
   Self.Vyhybky[i].Position.Y  := inifile.ReadInteger('V'+IntToStr(i), 'Y' ,  0);
   Self.Vyhybky[i].obj         := inifile.ReadInteger('V'+IntToStr(i), 'O' , -1);
   Self.Vyhybky[i].OblRizeni   := inifile.ReadInteger('V'+IntToStr(i), 'OR', -1);
  end;

 inifile.Free;
 Result := 0;

 Self.Escape;
end;

procedure TPanelZT.FSave(aFile:string;const ORs:string);
var i,j,k:Integer;
    inifile:TMemIniFile;
    Obj:string;
    str_list:TStrings;
begin
 Self.FStav := 2;

 DeleteFile(aFile);

 inifile := TMemIniFile.Create(aFile, TEncoding.UTF8);

 inifile.WriteString('G', 'ver', _ZT_FileVersion);

 inifile.WriteInteger('P', 'H', Self.DrawObject.Height);
 inifile.WriteInteger('P', 'W', Self.DrawObject.Width);

 inifile.WriteInteger('P', 'U', Length(Self.Useky));
 inifile.WriteInteger('P', 'N', Length(Self.Navestidla));
 inifile.WriteInteger('P', 'T', Length(Self.Popisky));
 inifile.WriteInteger('P', 'P', Length(Self.PomocneObj));
 inifile.WriteInteger('P', 'V', Length(Self.Vyhybky));

 //Oblasti rizeni
 str_list := TStringList.Create();
 ExtractStringsEx([#13], [], RightStr(LeftStr(ORs,Length(ORs)-1),Length(ORs)-3), str_list);
 for i := 0 to str_list.Count-1 do
   inifile.WriteString('OR',IntToStr(i),str_list[i]);
 str_list.Free;

 //useky
 for i := 0 to Length(Self.Useky)-1 do
  begin
   inifile.WriteInteger('U'+IntToStr(i),'B' , Self.Useky[i].Blok);
   inifile.WriteInteger('U'+IntToStr(i),'OR', Self.Useky[i].OblRizeni);

   //ukladani vetvi
   inifile.WriteInteger('U'+IntToStr(i), 'VC', Self.Useky[i].Vetve.Count);

   for j := 0 to Self.Useky[i].Vetve.Count-1 do
    begin
     Obj := Format('%.3d%.2d%.2d',[Self.Useky[i].Vetve.Data[j].vyh, Self.Useky[i].Vetve.Data[j].ref_plus, Self.Useky[i].Vetve.Data[j].ref_minus]);
     for k := 0 to Length(Self.Useky[i].Vetve.Data[j].Symbols)-1 do
       Obj := Obj + Format('%.3d%.3d%.3d',[Self.Useky[i].Vetve.Data[j].Symbols[k].Position.X, Self.Useky[i].Vetve.Data[j].Symbols[k].Position.Y, Self.Useky[i].Vetve.Data[j].Symbols[k].SymbolID]);
     inifile.WriteString('U'+IntToStr(i), 'V'+IntToStr(j), Obj);
    end;//for j

   //JCClick
   obj := '';
   for j := 0 to Self.Useky[i].JCClick.Count-1 do obj := obj + Format('%.3d%.3d',[Self.Useky[i].JCClick.Data[j].X, Self.Useky[i].JCClick.Data[j].Y]);
   inifile.WriteString('U'+IntToStr(i),'C',obj);

   //KPopisek
   obj := '';
   for j := 0 to Self.Useky[i].KPopisek.Count-1 do obj := obj + Format('%.3d%.3d',[Self.Useky[i].KPopisek.Data[j].X, Self.Useky[i].KPopisek.Data[j].Y]);
   inifile.WriteString('U'+IntToStr(i),'P',obj);

   //Nazev
   inifile.WriteString('U'+IntToStr(i),'N',Self.Useky[i].KPopisek.Nazev);
  end;//for i

 //navestidla
 for i := 0 to Length(Self.Navestidla)-1 do
  begin
   inifile.WriteInteger('N'+IntToStr(i), 'X' , Self.Navestidla[i].Position.X);
   inifile.WriteInteger('N'+IntToStr(i), 'Y' , Self.Navestidla[i].Position.Y);
   inifile.WriteInteger('N'+IntToStr(i), 'S' , Self.Navestidla[i].SymbolID);
   inifile.WriteInteger('N'+IntToStr(i), 'B' , Self.Navestidla[i].Blok);
   inifile.WriteInteger('N'+IntToStr(i), 'OR', Self.Navestidla[i].OblRizeni);
  end;//for i

 //pomocne symboly
 for i := 0 to Length(Self.PomocneObj)-1 do
  begin
   inifile.WriteInteger('P'+IntToStr(i),'S',Self.PomocneObj[i].Symbol);

   obj := '';
   for j := 0 to Length(Self.PomocneObj[i].Pos)-1 do obj := obj + Format('%.3d%.3d',[Self.PomocneObj[i].Pos[j].X, Self.PomocneObj[i].Pos[j].Y]);
   inifile.WriteString('P'+IntToStr(i),'P',obj);
  end;//for i

 //popisky
 for i := 0 to Length(Self.Popisky)-1 do
  begin
   inifile.WriteString('T'+IntToStr(i) , 'T', Self.Popisky[i].Text);
   inifile.WriteInteger('T'+IntToStr(i), 'X', Self.Popisky[i].Position.X);
   inifile.WriteInteger('T'+IntToStr(i), 'Y', Self.Popisky[i].Position.Y);
   inifile.WriteInteger('T'+IntToStr(i), 'C', Self.Popisky[i].Color);
  end;//for i

 //Vyhybky
 for i := 0 to Length(Self.Vyhybky)-1 do
  begin
   inifile.WriteInteger('V'+IntToStr(i), 'B' , Self.Vyhybky[i].Blok);
   inifile.WriteInteger('V'+IntToStr(i), 'S' , Self.Vyhybky[i].SymbolID);
   inifile.WriteInteger('V'+IntToStr(i), 'P' , Self.Vyhybky[i].PolohaPlus);
   inifile.WriteInteger('V'+IntToStr(i), 'X' , Self.Vyhybky[i].Position.X);
   inifile.WriteInteger('V'+IntToStr(i), 'Y' , Self.Vyhybky[i].Position.Y);
   inifile.WriteInteger('V'+IntToStr(i), 'O' , Self.Vyhybky[i].obj);
   inifile.WriteInteger('V'+IntToStr(i), 'OR', Self.Vyhybky[i].OblRizeni);
  end;

 //ulozit zaverovou tabulku
 Self.ZTSave(inifile);

 inifile.UpdateFile;
 inifile.Free;
end;

////////////////////////////////////////////////////////////////////////////////

//nacist ZT ze souboru
function TPanelZT.ZTLoad(aFile:string):Byte;
var mem:TMemIniFile;
begin
 if (not FileExists(aFile)) then
  begin
   Result := 1;
   Exit;
  end;

 mem := TMemIniFile.Create(aFile, TEncoding.UTF8);
 Result := Self.ZTLoad(mem);
 mem.Free();
end;

//ulozit ZT do souboru
procedure TPanelZT.ZTSave(aFIle:string);
var mem:TMemIniFile;
begin
 mem := TMemIniFile.Create(aFile, TEncoding.UTF8);
 Self.ZTSave(mem);
 mem.UpdateFile();
 mem.Free();
end;

////////////////////////////////////////////////////////////////////////////////

//nacist ZT ze souboru
//pred volanim teto fce musi byt jednoznacne nacten zbytek dat
function TPanelZT.ZTLoad(mem:TMemIniFile):Byte;
var i,j:Integer;
    data:array [0..3] of TStrings;
begin
 for i := 0 to 3 do
   data[i] := TStringList.Create();

 mem.ReadSectionValues(_INI_ZT_SECT, data[0]);

 Self.JCs.count := data[0].Count;

 for i := 0 to data[0].Count-1 do
  begin
   //nacitani jednotlivych sekci
   data[1].Clear;
   ExtractStringsEx(['|'], [], data[0][0], data[1]);

   if (data[1].Count < 12) then
    begin
     Result := 2;
     Exit;
    end;

   Self.JCs.data[i].nazev       := data[1][0];
   Self.JCs.data[i].typ         := TJCType(StrToInt(data[1][1]));
   Self.JCs.data[i].poznamka    := data[1][2];
   Self.JCs.data[i].nav         := StrToInt(data[1][3]);
   Self.JCs.data[i].prejezdy    := data[1][4];
   Self.JCs.data[i].trat.index  := StrToInt(data[1][5]);
   Self.JCs.data[i].trat.smer   := StrToInt(data[1][6]);
   Self.JCs.data[i].dalsiNav    := StrToInt(data[1][7]);
   Self.JCs.data[i].rychlost    := StrToInt(data[1][8]);

   //useky:
   data[2].Clear;
   ExtractStringsEx([';'], [], data[1][9], data[2]);
   SetLength(Self.JCs.data[i].Useky, data[2].Count);
   for j := 0 to data[2].Count-1 do
     Self.JCs.data[i].Useky[j] := StrToInt(data[2][j]);

   //vyhybky
   data[2].Clear;
   ExtractStringsEx([';'], [], data[1][10], data[2]);
   SetLength(Self.JCs.data[i].vyhybky, data[2].Count);
   for j := 0 to data[2].Count-1 do
    begin
     data[3].Clear;
     ExtractStringsEx([','], [], data[2][j], data[3]);

     if (data[3].Count < 2) then
      begin
       Result := 3;
       Exit;
      end;

     Self.JCs.data[i].vyhybky[j].blok   := StrToInt(data[3][0]);
     Self.JCs.data[i].vyhybky[j].poloha := StrToInt(data[3][1]);
    end;

   //odvraty
   data[2].Clear;
   ExtractStringsEx([';'], [], data[1][11], data[2]);
   SetLength(Self.JCs.data[i].odvraty, data[2].Count);
   for j := 0 to data[2].Count-1 do
    begin
     data[3].Clear;
     ExtractStringsEx([','], [], data[2][j], data[3]);

     if (data[3].Count < 2) then
      begin
       Result := 3;
       Exit;
      end;

     Self.JCs.data[i].odvraty[j].blok   := StrToInt(data[3][0]);
     Self.JCs.data[i].odvraty[j].poloha := StrToInt(data[3][1]);
    end;


  end;//for i

 for i := 0 to 3 do
   data[i].Free();

 Result := 0;
end;

//ulozit ZT do souboru
procedure TPanelZT.ZTSave(mem:TMemIniFile);
var i,j:Integer;
    line:string;
begin
 for i := 0 to Self.JCs.count-1 do
  begin
   line := Self.JCs.data[i].nazev + '|' +
      IntToStr(Integer(Self.JCs.data[i].typ)) + '|' +
      Self.JCs.data[i].poznamka + '|' +
      IntToStr(Self.JCs.data[i].nav) + '|' +
      Self.JCs.data[i].prejezdy + '|' +
      IntToStr(Self.JCs.data[i].trat.index) + '|' +
      IntToStr(Self.JCs.data[i].trat.smer) + '|' +
      IntToStr(Self.JCs.data[i].dalsiNav) + '|' +
      IntToStr(Self.JCs.data[i].rychlost) + '|';

   //useky
   for j := 0 to Length(Self.JCs.data[i].Useky)-1 do
     line := line + IntToStr(Self.JCs.data[i].Useky[j]) + ';';
   line[Length(line)] := '|';

   //vyhybky
   for j := 0 to Length(Self.JCs.data[i].vyhybky)-1 do
     line := line + IntToStr(Self.JCs.data[i].vyhybky[j].blok) + ',' + IntToStr(Self.JCs.data[i].vyhybky[j].poloha) + ';';
   line[Length(line)] := '|';

   //odvraty
   for j := 0 to Length(Self.JCs.data[i].odvraty)-1 do
     line := line + IntToStr(Self.JCs.data[i].odvraty[j].blok) + ',' + IntToStr(Self.JCs.data[i].odvraty[j].poloha) + ';';
   line[Length(line)] := '|';

   mem.WriteString(_INI_ZT_SECT, IntToStr(i), line);
  end;//for i
end;

////////////////////////////////////////////////////////////////////////////////

//import dat z TPanelObjects
procedure TPanelZT.Import(PanelObjects: TObject);
begin

end;

////////////////////////////////////////////////////////////////////////////////

procedure TPanelZT.Paint();
begin

end;

function TPanelZT.PaintCursor(CursorPos:TPoint):TCursorDraw;
begin

end;

////////////////////////////////////////////////////////////////////////////////

//maze vsechna data - uvadi tridu do vychoziho stavu
procedure TPanelZT.ResetPanel();
begin

end;

procedure TPanelZT.Escape();
begin

end;


////////////////////////////////////////////////////////////////////////////////

function TPanelZT.MouseUp(Position:TPoint;Button:TMouseButton):Byte;
begin
 Result := 0;
end;

procedure TPanelZT.MouseMove(Position:TPoint);
begin
end;

function TPanelZT.DblClick(Position:TPoint):Byte;
begin
 Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////

//overi validitu naeditovanych dat a vrati chybove hlasky
function TPanelZT.CheckValid(var error_cnt:Byte):TStrings;
var err:TStrings;
begin
 err := TStringList.Create;
 err.Clear;
 Result := err;
end;

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

end.//unit
