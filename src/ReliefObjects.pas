unit ReliefObjects;
// tato unita pracuje s objektovymi daty reliefu

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, IniFiles,
  StrUtils, Global, Menus, Forms, DXDraws, OblastRizeni, PGraphics, RPConst,
  Generics.Collections;

const


 _Def_Color_Selected      = 2;
 _Def_Color_Normal        = 1;
 _Def_Color_Alert         = 5;
 _Def_Color_IntUnassigned = 4;

 _Usek_Start    = 0;
 _Usek_End      = 23;
 _Vyhybka_End   = 3;
 _SCom_Start    = 24;
 _SCom_End      = 29;

 _KPopisek_Index   = 377;
 _JCPopisek_Index  = 360;
 _Prj_Index        = 400;
 _Root_Index       = 42;
 _Root_Color       = 3;
 _Uvazka_Start     = 43;
 _Uvazka_Spr_Index = 45;
 _Zamek            = 48;
 _Vykol_Start      = 49;
 _Vykol_End        = 54;
 _Rozp_Start       = 55;

 //zde je definovano, jaky symbol se ma vykreslovat jakou barvou (mimo separatoru)
 _Bitmap_DrawColors:array [0..35] of Byte = (1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,6,6,6,1);

 _FileVersion = '1.1';

type

TBlkType = (usek, navestidlo, vyhybka, prejezd, popisek, pomocny_obj, uvazka, uvazka_spr, zamek, vykol, rozp, TU);

// abstraktni trida, ze ktere dedi konkretni graficke bloky
TGraphBlok = class
  typ:TBlkType;

  Blok:Integer;
  OblRizeni:Integer;
  index:Integer;            // index bloku daneho typu v poli vsech bloku (tzn. kazdy typ ma svou indexaci - pocinaje 0)
end;

// vyhybka
TVyhybka = class(TGraphBlok)
 PolohaPlus:Byte;
 Position:TPoint;
 SymbolID:Integer;
 obj:integer;               //index useku, na kterem vyhybka je
end;//Vyhybka

// po rozdeleni useku je na vetve je usek reprezentovan takto:

// ukoncovaci element vetve = vyhybka
TVetevEnd = record
  vyh:Integer;                     // pokud usek nema vyhybky -> vyh1 = -1, vyh2 = -1 (nastava u useku bez vyhybky a u koncovych vetvi)
                                   // referuje na index v poli vyhybek (nikoliv na technologicke ID vyhybky!)
                                   // kazda vetev je ukoncena maximalne 2-ma vyhybkama - koren muze byt ukoncen 2-ma vyhybkama, pak jen jedna
  ref_plus,ref_minus:Integer;      // reference  na vetev, kterou se pokracuje, pokud je vyh v poloze + resp. poloze -
                                   // posledni vetev resp. usek bez vyhybky ma obe reference = -1
end;

//vetev useku
TVetev=record             //vetev useku

 node1:TVetevEnd;           // reference na 1. vyhybku, ktera ukoncuje tuto vetev
 node2:TVetevEnd;           // reference na 2. vyhybku, ktera ukoncuje tuto vetev



 Symbols:array of TReliefSym;
                            // s timto dynamicky alokovanym polem je potreba zachazet opradu opatrne
                            // realokace trva strasne dlouho !
                            // presto si myslim, ze se jedna o vyhodne reseni: pole se bude plnit jen jednou
                            // pokud je v useku vykolejka, jednoduse sem ulozime symbol vykolejky
end;

// usek
TUsek = class(TGraphBlok)
 Root:TPoint;
 IsVyhybka:boolean;         //pomocny flag pro vykreslovani v modu korenu
 Symbols:TList<TReliefSym>; // pokud je v useku vykolejka, je zde ulozena jako klasicky symbol
 JCClick:TList<TPoint>;
 KPopisek:TList<TPoint>;
 KpopisekStr:string;

 Vetve:TList<TVetev>;               // vetve useku
   //vetev 0 je vzdy koren
   //zde je ulozen binarni strom v pseudo-forme
     //na 0. indexu je koren, kazdy vrchol pak obsahuje referenci na jeho deti


 // program si duplikuje ulozena data - po rozdeleni useku na vetve uklada usek jak nerozdeleny tak rozdeleny
end;

// popisek
TPopisek = class(TGraphBlok)
 Text:string;
 Position:TPoint;
 Color:Integer;
end;//Text

// navestidlo
TNavestidlo = class(TGraphBlok)
 Position:TPoint;
 SymbolID:Integer;
end;//Navestidlo

// pomocny objekt
TPomocnyObj = class(TGraphBlok)
 Positions:TList<TPoint>;
 Symbol:Integer;
end;//PomocnyObj

TBlikPoint = record
 Pos:TPoint;
 TechUsek:Integer;     // jaky technologicky usek ma tato cast prejezdu
end;

// prejezd
TPrejezd = class(TGraphBlok)
 StaticPositions : TList<TPoint>;
 BlikPositions : TList<TBlikPoint>;
end;//Navestidlo

// uvazka
TUvazka = class(TGraphBlok)
  Pos:TPoint;
  defalt_dir:Integer;
end;

TUvazkaSprVertDir = (top = 0, bottom = 1);

//uvazka spr
TUvazkaSpr = class(TGraphBlok)
  Pos:TPoint;
  vertical_dir:TUvazkaSprVertDir;
  spr_cnt:Integer;
end;

// zamek
TZamek = class(TGraphBlok)
 Pos:TPoint;
end;

// rozpojovac
TRozp = class(TGraphBlok)
  Pos:TPoint;
end;

// vykolejka
TVykol = class(TGraphBlok)
  Pos:TPoint;
  symbol:Integer;
  obj:integer;               // index useku, na kterem je vykolejka
  vetev:integer;             // cislo vetve, ve kterem je vykolejka
end;

TBlokAskEvent     = procedure(Sender:TObject; Blok:TGraphBlok) of object;
TMsgEvent         = procedure(Sender:TObject; msg:string) of object;

TVetveData = array of array of Integer;

TPanelObjects=class
  private

   PM_Properties:TPopUpMenu;

   FSoubor:string;
   FStav:ShortInt;
   FMode:TMode;
   Graphics:TPanelGraphics;

   PopUpPos:TPoint;

   DrawObject:record
    Canvas:TCanvas;
    SymbolIL,TextIL:TImageList;
    Width,Height:Integer;
   end;
   Colors:record
    Selected,Normal,Alert,IntUnassigned:Byte;
   end;

   Selected:TGraphBlok;

  FOnBlokEdit     : TBlokAskEvent;
  FOnShow         : TNEvent;
  FOnMsg          : TMsgEvent;
  FFormBlkClose   : TGlobalEvent;

    procedure CreatePM(var PM:TPopUpMenu;Parent:TDXDraw);
    procedure PMPropertiesClick(Sender: TObject);

    procedure SetMode(mode:TMode);

    procedure ComputeVyhybkaFlag();                                   //volano pri prechodu z Bloky do Koreny
    procedure ComputeVetve();
    procedure ComputeBlokVetve(data:TVetveData; start:TPoint; Vetve:TList<TVetev>);

  public
   Bloky:TList<TGraphBlok>;

    constructor Create(SymbolIL,TextIL:TImageList;DrawCanvas:TCanvas;Width,Height:Integer;Parent:TDXDraw; Graphics:TPanelGraphics);
    destructor Destroy; override;

    function Import(Data:TObject):Byte;     // import dat zatim z PanelBitmap

    function FLoad(aFile:string;var ORs:string):Byte;
    function FSave(aFile:string;const ORs:string):Byte;

    procedure Paint();            //obecny paint
    procedure PaintBloky();       //paint modu bloky
    function PaintCursor(CursorPos:TPoint):TCursorDraw;

    procedure ResetPanel;
    procedure Escape;

    function MouseUp(Position:TPoint;Button:TMouseButton):Byte;
    procedure MouseMove(Position:TPoint);
    function BlokyMouseUp(Position:TPoint;Button:TMouseButton):Byte;
    function KorenyMouseUp(Position:TPoint;Button:TMouseButton):Byte;
    function DblClick(Position:TPoint):Byte;

    function GetObject(Pos:TPoint):Integer;

    function SetOR(OblR:Integer):Byte;  //nastavi oblast rizeni vybranemu bloku

    function CheckValid(var error_cnt:Byte):TStrings;     //overi validitu naeditovanych dat a vrati chybove hlasky

    procedure ComputePrjTechUsek();

    property PanelWidth:Integer read DrawObject.Width;
    property PanelHeight:Integer read DrawObject.Height;
    property Soubor:string read FSoubor;
    property FileStav:ShortInt read FStav;
    property Mode:TMode read FMode write SetMode;

    property ColorSelected:Byte read Colors.Selected write Colors.Selected;
    property ColorNormal:Byte read Colors.Normal write Colors.Normal;
    property ColorAlert:Byte read Colors.Alert write Colors.Alert;
    property ColorIntUnassigned:Byte read Colors.IntUnassigned write Colors.IntUnassigned;
    property selected_obj:TGraphBlok read Selected;

    property OnBlokEdit: TBlokAskEvent read FOnBlokEdit write FOnBlokEdit;

    property OnShow: TNEvent read FOnShow write FOnShow;
    property OnMsg: TMsgEvent read FOnMsg write FOnMsg;
    property OnFormBlkClose: TGlobalEvent read FFormBlkClose write FFormBlkClose;
end;//TPanelObjects

//FileSystemStav:
 //0 - soubor zavren
 //1 - soubor neulozen
 //2 - soubor ulozen

implementation

uses ReliefBitmap, BItmapToObj;   // kvuli importu dat

//vytvoreni objektu
constructor TPanelObjects.Create(SymbolIL,TextIL:TImageList;DrawCanvas:TCanvas;Width,Height:Integer;Parent:TDXDraw; Graphics:TPanelGraphics);
begin
 Self.DrawObject.Canvas := DrawCanvas;

 Self.DrawObject.SymbolIL := SymbolIL;
 Self.DrawObject.TextIL   := TextIL;

 Self.DrawObject.Width  := Width;
 Self.DrawObject.Height := Height;

 Self.Graphics := Graphics;

 //vychozi barvy
 Self.Colors.Selected      := _Def_Color_Selected;
 Self.Colors.Normal        := _Def_Color_Normal;
 Self.Colors.Alert         := _Def_Color_Alert;
 Self.Colors.IntUnassigned := _Def_Color_IntUnassigned;

 Self.Selected := nil;

 Self.FMode := dmBloky;

 Self.Bloky := TList<TGraphBlok>.Create();

 Self.CreatePM(Self.PM_Properties,Parent);
end;//constructor

destructor TPanelObjects.Destroy;
begin
 Self.ResetPanel();
 Self.Bloky.Free();

 if (Assigned(Self.PM_Properties) = true) then
  begin
   Self.PM_Properties.Free;
   Self.PM_Properties := nil;
  end;
end;//destructor

//reset dat
procedure TPanelObjects.ResetPanel;
var i:Integer;
begin
 //vymazani dat
 for i := 0 to Self.Bloky.Count-1 do
  begin
   if ((Self.Bloky[i].typ = TBlkType.usek) or (Self.Bloky[i].typ = TBlkType.TU)) then
    begin
     (Self.Bloky[i] as TUsek).Symbols.Free();
     (Self.Bloky[i] as TUsek).JCClick.Free();
     (Self.Bloky[i] as TUsek).KPopisek.Free();
     (Self.Bloky[i] as TUsek).Vetve.Free();
    end;
   if (Self.Bloky[i].typ = TBlkType.pomocny_obj) then
     (Self.Bloky[i] as TPomocnyObj).Positions.Free();
   if (Self.Bloky[i].typ = TBlkType.prejezd) then
    begin
     (Self.Bloky[i] as TPrejezd).StaticPositions.Free();
     (Self.Bloky[i] as TPrejezd).BlikPositions.Free();
    end;

   Self.Bloky[i].Free();
  end;
 Self.Bloky.Clear();
 Self.Selected := nil;
end;//procedure NewFile

//nacitani souboru
function TPanelObjects.FLoad(aFile:string;var ORs:string):Byte;
var i,j,k:Integer;
    inifile:TMemIniFile;
    Obj:string;
    ver:string;
    sect_str:TStrings;

    symbol:TReliefSym;
    blik_pos:TBlikPoint;
    pos:TPoint;
    blok:TGraphBlok;
    count,count2:Integer;
    Vetev:TVetev;
    indexes:array [0..7] of Integer;
begin
 Self.FStav := 2;
 Self.FSoubor := aFile;

 Self.ResetPanel;

 //kontrola existence
 if (not FileExists(aFile)) then
  begin
   Result := 1;
   Exit;
  end;

 //samotne nacitani dat
 try
   inifile := TMemIniFile.Create(aFile, TEncoding.UTF8);
 except
   Result := 3;
   Exit;
 end;

 //kontrola verze
 ver := inifile.ReadString('G','ver',_FileVersion);
 if (_FileVersion <> ver) then
  begin
   if (Application.MessageBox(PChar('Naèítáte soubor s verzí '+ver+#13#10+'Aplikace momentálnì podporuje verzi '+_FileVersion+#13#10+'Chcete pokraèovat?'), 'Varování', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
    begin
     Result := 2;
     Exit;
    end;
  end;

 Self.DrawObject.Height := inifile.ReadInteger('P','H',0);
 Self.DrawObject.Width  := inifile.ReadInteger('P','W',0);

 // oblati rizeni
 // tato silena metoda vytvoreni binarniho souboru opravdu zjednodusuje cely program
 sect_str := TStringList.Create();
 inifile.ReadSection('OR',sect_str);
 ORs := '';
 for i := 0 to sect_str.Count-1 do
    ORs := ORs + inifile.ReadString('OR',sect_str[i],'')+#13;
 ORs := ORs+#13;
 sect_str.Free;

 // useky
 count := inifile.ReadInteger('P', 'U', 0);
 for i := 0 to count-1 do
  begin
   try
     blok           := TUsek.Create();
     blok.index     := i;
     blok.typ       := TBlkType.usek;
     blok.Blok      := inifile.ReadInteger('U'+IntToStr(i),'B',-1);
     blok.OblRizeni := inifile.ReadInteger('U'+IntToStr(i),'OR',-1);

     //root
     (blok as TUsek).Root := GetPos(inifile.ReadString('U'+IntToStr(i), 'R', '-1;-1'));

     //Symbols
     (blok as TUsek).Symbols := TList<TReliefSym>.Create();
     obj := inifile.ReadString('U'+IntToStr(i),'S', '');
     for j := 0 to (Length(obj) div 8)-1 do
      begin
       try
         symbol.Position.X := StrToInt(copy(obj,j*8+1,3));
         symbol.Position.Y := StrToInt(copy(obj,j*8+4,3));
         symbol.SymbolID   := StrToInt(copy(obj,j*8+7,2));
       except
         continue;
       end;
       (blok as TUsek).Symbols.Add(symbol);
      end;//for j

     //JCClick
     (blok as TUsek).JCClick := TList<TPoint>.Create();
     obj := inifile.ReadString('U'+IntToStr(i),'C','');
     for j := 0 to (Length(obj) div 6)-1 do
      begin
       try
         pos.X := StrToInt(copy(obj,j*6+1,3));
         pos.Y := StrToInt(copy(obj,j*6+4,3));
       except
        continue;
       end;
       (blok as TUsek).JCClick.Add(pos);
      end;//for j

     //KPopisek
     obj := inifile.ReadString('U'+IntToStr(i),'P','');
     (blok as TUsek).KPopisek := TList<TPoint>.Create();
     for j := 0 to (Length(obj) div 6)-1 do
      begin
       try
         pos.X := StrToIntDef(copy(obj,j*6+1,3),0);
         pos.Y := StrToIntDef(copy(obj,j*6+4,3),0);
       except
         continue;
       end;
       (blok as TUsek).KPopisek.Add(pos);
      end;//for j

     //Nazev
     (blok as TUsek).KpopisekStr := inifile.ReadString('U'+IntToStr(i),'N','');
     (blok as TUsek).Vetve := TList<TVetev>.Create();

     // vetve
     count2 := inifile.ReadInteger('U'+IntToStr(i), 'VC', 0);
     for j := 0 to count2-1 do
      begin
       obj := inifile.ReadString('U'+IntToStr(i), 'V'+IntToStr(j), '');

       vetev.node1.vyh        := StrToIntDef(copy(obj, 0, 3), 0);
       vetev.node1.ref_plus   := StrToIntDef(copy(obj, 4, 2), 0);
       vetev.node1.ref_minus  := StrToIntDef(copy(obj, 6, 2), 0);

       vetev.node2.vyh        := StrToIntDef(copy(obj, 8, 3), 0);
       vetev.node2.ref_plus   := StrToIntDef(copy(obj, 11, 2), 0);
       vetev.node2.ref_minus  := StrToIntDef(copy(obj, 13, 2), 0);

       obj := RightStr(obj, Length(obj)-14);

       SetLength(vetev.Symbols, Length(obj) div 9);

       for k := 0 to Length(vetev.Symbols)-1 do
        begin
         vetev.Symbols[k].Position.X := StrToIntDef(copy(obj, 9*k + 1, 3), 0);
         vetev.Symbols[k].Position.Y := StrToIntDef(copy(obj, (9*k + 4), 3), 0);
         vetev.Symbols[k].SymbolID   := StrToIntDef(copy(obj, (9*k + 7), 3), 0);
        end;

       (blok as TUsek).Vetve.Add(vetev);
      end;//for j

     Self.Bloky.Add(blok);
   except

   end;

  end;//for i

 // navestidla
 count := inifile.ReadInteger('P','N',0);
 for i := 0 to count-1 do
  begin
   blok           := TNavestidlo.Create();
   blok.index     := i;
   blok.typ       := TBlkType.navestidlo;
   blok.Blok      := inifile.ReadInteger('N'+IntToStr(i),'B',-1);
   blok.OblRizeni := inifile.ReadInteger('N'+IntToStr(i),'OR',-1);

   (blok as TNavestidlo).Position.X := inifile.ReadInteger('N'+IntToStr(i),'X',0);
   (blok as TNavestidlo).Position.Y := inifile.ReadInteger('N'+IntToStr(i),'Y',0);
   (blok as TNavestidlo).SymbolID   := inifile.ReadInteger('N'+IntToStr(i),'S',0);
   Self.Bloky.Add(blok);
  end;//for i

 // pomocne symboly
 count := inifile.ReadInteger('P','P',0);
 for i := 0 to count-1 do
  begin
   blok           := TPomocnyObj.Create();
   blok.index     := i;
   blok.typ       := TBlkType.pomocny_obj;
   blok.Blok      := -1;
   blok.OblRizeni := -1;

   (blok as TPomocnyObj).Symbol :=  inifile.ReadInteger('P'+IntToStr(i),'S',0);

   obj := inifile.ReadString('P'+IntToStr(i),'P','');
   (blok as TPomocnyObj).Positions := TList<TPoint>.Create();
   for j := 0 to (Length(obj) div 6)-1 do
    begin
     pos.X := StrToIntDef(copy(obj,j*6+1,3),0);
     pos.Y := StrToIntDef(copy(obj,j*6+4,3),0);
     (blok as TPomocnyObj).Positions.Add(pos);
    end;//for j

   Self.Bloky.Add(blok);
  end;//for i

 // popisky
 count := inifile.ReadInteger('P','T',0);
 for i := 0 to count-1 do
  begin
   blok           := TPopisek.Create();
   blok.index     := i;
   blok.typ       := TBlkType.popisek;
   blok.Blok      := inifile.ReadInteger('T'+IntToStr(i),'B', -1);
   blok.OblRizeni := inifile.ReadInteger('T'+IntToStr(i),'OR', -1);

   (blok as TPopisek).Text       := inifile.ReadString('T'+IntToStr(i),'T', 'text');
   (blok as TPopisek).Position.X := inifile.ReadInteger('T'+IntToStr(i),'X', 0);
   (blok as TPopisek).Position.Y := inifile.ReadInteger('T'+IntToStr(i),'Y', 0);
   (blok as TPopisek).Color      := inifile.ReadInteger('T'+IntToStr(i),'C', 0);

   Self.Bloky.Add(blok);
  end;//for i

 // vyhybky
 count := inifile.ReadInteger('P','V',0);
 for i := 0 to count-1 do
  begin
   blok           := TVyhybka.Create();
   blok.index     := i;
   blok.typ       := TBlkType.vyhybka;
   blok.Blok      := inifile.ReadInteger('V'+IntToStr(i),'B', -1);
   blok.OblRizeni := inifile.ReadInteger('V'+IntToStr(i),'OR', -1);

   (blok as TVyhybka).SymbolID    := inifile.ReadInteger('V'+IntToStr(i),'S',0);
   (blok as TVyhybka).PolohaPlus  := inifile.ReadInteger('V'+IntToStr(i),'P',0);
   (blok as TVyhybka).Position.X  := inifile.ReadInteger('V'+IntToStr(i),'X',0);
   (blok as TVyhybka).Position.Y  := inifile.ReadInteger('V'+IntToStr(i),'Y',0);
   (blok as TVyhybka).obj         := inifile.ReadInteger('V'+IntToStr(i),'O',-1);

   Self.Bloky.Add(blok);
  end;

 // prejezdy
 count := inifile.ReadInteger('P','PRJ',0);
 for i := 0 to count-1 do
  begin
   blok           := TPrejezd.Create();
   blok.index     := i;
   blok.typ       := TBlkType.prejezd;
   blok.Blok      := inifile.ReadInteger('PRJ'+IntToStr(i),'B', -1);
   blok.OblRizeni := inifile.ReadInteger('PRJ'+IntToStr(i),'OR', -1);

   obj := inifile.ReadString('PRJ'+IntToStr(i), 'BP', '');
   (blok as TPrejezd).BlikPositions := TList<TBlikPoint>.Create();
   for j := 0 to (Length(obj) div 9)-1 do
    begin
     try
       blik_pos.Pos.X    := StrToInt(copy(obj, j*9+1, 3));
       blik_pos.Pos.Y    := StrToInt(copy(obj, j*9+4, 3));
       blik_pos.TechUsek := StrToInt(copy(obj, j*9+7, 3));
     except
       continue;
     end;

     (blok as TPrejezd).BlikPositions.Add(blik_pos);
    end;//for j

   obj := inifile.ReadString('PRJ'+IntToStr(i), 'SP', '');
   (blok as TPrejezd).StaticPositions := TList<TPoint>.Create();
   for j := 0 to (Length(obj) div 6)-1 do
    begin
     try
       pos.X := StrToInt(copy(obj, j*6+1, 3));
       pos.Y := StrToInt(copy(obj, j*6+4, 3));
     except
       continue;
     end;
     (blok as TPrejezd).StaticPositions.Add(pos);
    end;//for j

   Self.Bloky.Add(blok);
  end;

 // uvazky
 count := inifile.ReadInteger('P','Uv',0);
 for i := 0 to count-1 do
  begin
   blok           := TUvazka.Create();
   blok.index     := i;
   blok.typ       := TBlkType.uvazka;
   blok.Blok      := inifile.ReadInteger('Uv'+IntToStr(i),'B', -1);
   blok.OblRizeni := inifile.ReadInteger('Uv'+IntToStr(i),'OR', -1);

   (blok as TUvazka).Pos.X       := inifile.ReadInteger('Uv'+IntToStr(i), 'X', 0);
   (blok as TUvazka).Pos.Y       := inifile.ReadInteger('Uv'+IntToStr(i), 'Y', 0);
   (blok as TUvazka).defalt_dir  := inifile.ReadInteger('Uv'+IntToStr(i), 'D', 0);

   Self.Bloky.Add(blok);
  end;//for i

 // uvazky soupravy
 count := inifile.ReadInteger('P','UvS',0);
 for i := 0 to count-1 do
  begin
   blok           := TUvazkaSpr.Create();
   blok.index     := i;
   blok.typ       := TBlkType.uvazka_spr;
   blok.Blok      := inifile.ReadInteger('UvS'+IntToStr(i),'B', -1);
   blok.OblRizeni := inifile.ReadInteger('UvS'+IntToStr(i),'OR', -1);

   (blok as TUvazkaSpr).Pos.X        := inifile.ReadInteger('UvS'+IntToStr(i), 'X', 0);
   (blok as TUvazkaSpr).Pos.Y        := inifile.ReadInteger('UvS'+IntToStr(i), 'Y', 0);
   (blok as TUvazkaSpr).vertical_dir := TUvazkaSprVertDir(inifile.ReadInteger('UvS'+IntToStr(i), 'VD', 0));
   (blok as TUvazkaSpr).spr_cnt      := inifile.ReadInteger('UvS'+IntToStr(i), 'C', 1);

   Self.Bloky.Add(blok);
  end;//for i

 // zamky
 count := inifile.ReadInteger('P','Z',0);
 for i := 0 to count-1 do
  begin
   blok           := TZamek.Create();
   blok.index     := i;
   blok.typ       := TBlkType.zamek;
   blok.Blok      := inifile.ReadInteger('Z'+IntToStr(i),'B', -1);
   blok.OblRizeni := inifile.ReadInteger('Z'+IntToStr(i),'OR', -1);

   (blok as TZamek).Pos.X := inifile.ReadInteger('Z'+IntToStr(i), 'X', 0);
   (blok as TZamek).Pos.Y := inifile.ReadInteger('Z'+IntToStr(i), 'Y', 0);

   Self.Bloky.Add(blok);
  end;

 // vykolejky
 count := inifile.ReadInteger('P', 'Vyk', 0);
 for i := 0 to count-1 do
  begin
   blok           := TVykol.Create();
   blok.index     := i;
   blok.typ       := TBlkType.vykol;
   blok.Blok      := inifile.ReadInteger('Vyk'+IntToStr(i),'B', -1);
   blok.OblRizeni := inifile.ReadInteger('Vyk'+IntToStr(i),'OR', -1);

   (blok as TVykol).Pos.X := inifile.ReadInteger('Vyk'+IntToStr(i), 'X', 0);
   (blok as TVykol).Pos.Y := inifile.ReadInteger('Vyk'+IntToStr(i), 'Y', 0);
   (blok as TVykol).symbol:= inifile.ReadInteger('Vyk'+IntToStr(i), 'T', 0);
   (blok as TVykol).obj   := inifile.ReadInteger('Vyk'+IntToStr(i), 'O', 0);

   Self.Bloky.Add(blok);
  end;

 // rozpojovace
 count := inifile.ReadInteger('P', 'R', 0);
 for i := 0 to count-1 do
  begin
   blok           := TRozp.Create();
   blok.index     := i;
   blok.typ       := TBlkType.rozp;
   blok.Blok      := inifile.ReadInteger('R'+IntToStr(i),'B', -1);
   blok.OblRizeni := inifile.ReadInteger('R'+IntToStr(i),'OR', -1);

   (blok as TRozp).Pos.X := inifile.ReadInteger('R'+IntToStr(i), 'X', 0);
   (blok as TRozp).Pos.Y := inifile.ReadInteger('R'+IntToStr(i), 'Y', 0);

   Self.Bloky.Add(blok);
  end;

 inifile.Free;
 Result := 0;

 Self.ComputeVyhybkaFlag();
 Self.Escape;
end;//procedure LoadFile

function TPanelObjects.FSave(aFile:string;const ORs:string):Byte;
var i,j,k:Integer;
    inifile:TMemIniFile;
    Obj:string;
    str_list:TStrings;
    counts:array [0..10] of Cardinal;
      // pocty bloku v tomto poradi: useky, navestidla, vyhybky, prejezdy, popisky, pomocne_objekty, uvazky, uvazky_spr, zamky, vykolejky, rozpojovace
begin
 Self.FStav := 2;

 Self.ComputeVyhybkaFlag();
 Self.ComputeVetve();

 DeleteFile(aFile);

 try
   inifile := TMemIniFile.Create(aFile, TEncoding.UTF8);
 except
   Result := 1;
   Exit;
 end;

 for i := 0 to 7 do
   counts[i] := 0;

 inifile.WriteString('G', 'ver', _FileVersion);

 inifile.WriteInteger('P', 'H', Self.DrawObject.Height);
 inifile.WriteInteger('P', 'W', Self.DrawObject.Width);

 Self.ComputePrjTechUsek();

 // oblasti rizeni
 str_list := TStringList.Create();
 ExtractStrings([#13],[],PChar(LeftStr(ORs,Length(ORs)-1)),str_list);
 for i := 0 to str_list.Count-1 do inifile.WriteString('OR',IntToStr(i),str_list[i]);
 str_list.Free;

 // useky
 for i := 0 to Self.Bloky.Count-1 do
  begin
   case (Self.Bloky[i].typ) of
    TBlkType.usek:begin
     inifile.WriteInteger('U'+IntToStr(Self.Bloky[i].Index),'B',  Self.Bloky[i].Blok);
     inifile.WriteInteger('U'+IntToStr(Self.Bloky[i].Index),'OR', Self.Bloky[i].OblRizeni);

     //root
     if ((Self.Bloky[i] as TUsek).IsVyhybka) then
       inifile.WriteString('U'+IntToStr(Self.Bloky[i].Index), 'R', GetPos((Self.Bloky[i] as TUsek).Root));

     //objekty
     obj := '';
     for j := 0 to (Self.Bloky[i] as TUsek).Symbols.Count-1 do obj := obj + Format('%.3d%.3d%.2d',[(Self.Bloky[i] as TUsek).Symbols[j].Position.X, (Self.Bloky[i] as TUsek).Symbols[j].Position.Y, (Self.Bloky[i] as TUsek).Symbols[j].SymbolID]);
     if (obj <> '') then
       inifile.WriteString('U'+IntToStr(Self.Bloky[i].Index),'S',obj);

     //JCClick
     obj := '';
     for j := 0 to (Self.Bloky[i] as TUsek).JCClick.Count-1 do obj := obj + Format('%.3d%.3d',[(Self.Bloky[i] as TUsek).JCClick[j].X, (Self.Bloky[i] as TUsek).JCClick[j].Y]);
     if (obj <> '') then
       inifile.WriteString('U'+IntToStr(Self.Bloky[i].Index),'C',obj);

     //KPopisek
     obj := '';
     for j := 0 to (Self.Bloky[i] as TUsek).KPopisek.Count-1 do obj := obj + Format('%.3d%.3d',[(Self.Bloky[i] as TUsek).KPopisek[j].X, (Self.Bloky[i] as TUsek).KPopisek[j].Y]);
     if (obj <> '') then
       inifile.WriteString('U'+IntToStr(Self.Bloky[i].Index),'P',obj);

     //Nazev
     if ((Self.Bloky[i] as TUsek).KpopisekStr <> '') then
       inifile.WriteString('U'+IntToStr(Self.Bloky[i].Index),'N',(Self.Bloky[i] as TUsek).KpopisekStr);

     //vetve
     if ((Self.Bloky[i] as TUsek).Vetve.Count > 0) then
       inifile.WriteInteger('U'+IntToStr(i), 'VC', (Self.Bloky[i] as TUsek).Vetve.Count);

     for j := 0 to (Self.Bloky[i] as TUsek).Vetve.Count-1 do
      begin
       if ((Self.Bloky[i] as TUsek).Vetve[j].node1.vyh < 0) then
         Obj := Format('%.2d',[(Self.Bloky[i] as TUsek).Vetve[j].node1.vyh])
        else
         Obj := Format('%.3d',[(Self.Bloky[i] as TUsek).Vetve[j].node1.vyh]);

       if ((Self.Bloky[i] as TUsek).Vetve[j].node1.ref_plus < 0) then
         Obj := Obj + Format('%.1d',[(Self.Bloky[i] as TUsek).Vetve[j].node1.ref_plus])
        else
         Obj := Obj + Format('%.2d',[(Self.Bloky[i] as TUsek).Vetve[j].node1.ref_plus]);

       if ((Self.Bloky[i] as TUsek).Vetve[j].node1.ref_minus < 0) then
         Obj := Obj + Format('%.1d',[(Self.Bloky[i] as TUsek).Vetve[j].node1.ref_minus])
        else
         Obj := Obj + Format('%.2d',[(Self.Bloky[i] as TUsek).Vetve[j].node1.ref_minus]);

       ////////

       if ((Self.Bloky[i] as TUsek).Vetve[j].node2.vyh < 0) then
         Obj := Obj + Format('%.2d',[(Self.Bloky[i] as TUsek).Vetve[j].node2.vyh])
        else
         Obj := Obj + Format('%.3d',[(Self.Bloky[i] as TUsek).Vetve[j].node2.vyh]);

       if ((Self.Bloky[i] as TUsek).Vetve[j].node2.ref_plus < 0) then
         Obj := Obj + Format('%.1d',[(Self.Bloky[i] as TUsek).Vetve[j].node2.ref_plus])
        else
         Obj := Obj + Format('%.2d',[(Self.Bloky[i] as TUsek).Vetve[j].node2.ref_plus]);

       if ((Self.Bloky[i] as TUsek).Vetve[j].node2.ref_minus < 0) then
         Obj := Obj + Format('%.1d',[(Self.Bloky[i] as TUsek).Vetve[j].node2.ref_minus])
        else
         Obj := Obj + Format('%.2d',[(Self.Bloky[i] as TUsek).Vetve[j].node2.ref_minus]);

       ////////

       for k := 0 to Length((Self.Bloky[i] as TUsek).Vetve[j].Symbols)-1 do
         Obj := Obj + Format('%.3d%.3d%.3d',[(Self.Bloky[i] as TUsek).Vetve[j].Symbols[k].Position.X, (Self.Bloky[i] as TUsek).Vetve[j].Symbols[k].Position.Y, (Self.Bloky[i] as TUsek).Vetve[j].Symbols[k].SymbolID]);
       inifile.WriteString('U'+IntToStr(i), 'V'+IntToStr(j), Obj);
      end;//for j

    end;// case usek

    TBlkType.navestidlo:begin
     inifile.WriteInteger('N'+IntToStr(Self.Bloky[i].Index), 'X', (Self.Bloky[i] as TNavestidlo).Position.X);
     inifile.WriteInteger('N'+IntToStr(Self.Bloky[i].Index), 'Y', (Self.Bloky[i] as TNavestidlo).Position.Y);
     inifile.WriteInteger('N'+IntToStr(Self.Bloky[i].Index), 'S', (Self.Bloky[i] as TNavestidlo).SymbolID);

     inifile.WriteInteger('N'+IntToStr(Self.Bloky[i].Index), 'B', Self.Bloky[i].Blok);
     inifile.WriteInteger('N'+IntToStr(Self.Bloky[i].Index), 'OR',Self.Bloky[i].OblRizeni);
    end;//case navestidlo

    TBlkType.vyhybka:begin
     inifile.WriteInteger('V'+IntToStr(Self.Bloky[i].Index),'B', Self.Bloky[i].Blok);
     inifile.WriteInteger('V'+IntToStr(Self.Bloky[i].Index),'OR',Self.Bloky[i].OblRizeni);

     inifile.WriteInteger('V'+IntToStr(Self.Bloky[i].Index),'S', (Self.Bloky[i] as TVyhybka).SymbolID);
     inifile.WriteInteger('V'+IntToStr(Self.Bloky[i].Index),'P', (Self.Bloky[i] as TVyhybka).PolohaPlus);
     inifile.WriteInteger('V'+IntToStr(Self.Bloky[i].Index),'X', (Self.Bloky[i] as TVyhybka).Position.X);
     inifile.WriteInteger('V'+IntToStr(Self.Bloky[i].Index),'Y', (Self.Bloky[i] as TVyhybka).Position.Y);
     inifile.WriteInteger('V'+IntToStr(Self.Bloky[i].Index),'O', (Self.Bloky[i] as TVyhybka).obj);
    end;

    TBlkType.prejezd:begin
     inifile.WriteInteger('PRJ'+IntToStr(Self.Bloky[i].Index), 'B',  Self.Bloky[i].Blok);
     inifile.WriteInteger('PRJ'+IntToStr(Self.Bloky[i].Index), 'OR', Self.Bloky[i].OblRizeni);

     obj := '';
     for j := 0 to (Self.Bloky[i] as TPrejezd).BlikPositions.Count-1 do
      begin
       if ((Self.Bloky[i] as TPrejezd).BlikPositions[j].TechUsek >= 0) then
         obj := obj + Format('%.3d%.3d%.3d',[(Self.Bloky[i] as TPrejezd).BlikPositions[j].Pos.X, (Self.Bloky[i] as TPrejezd).BlikPositions[j].Pos.Y, (Self.Bloky[i] as TPrejezd).BlikPositions[j].TechUsek])
       else
         obj := obj + Format('%.3d%.3d-01',[(Self.Bloky[i] as TPrejezd).BlikPositions[j].Pos.X, (Self.Bloky[i] as TPrejezd).BlikPositions[j].Pos.Y]);
      end;
     inifile.WriteString('PRJ'+IntToStr(Self.Bloky[i].Index), 'BP', obj);

     obj := '';
     for j := 0 to (Self.Bloky[i] as TPrejezd).StaticPositions.Count-1 do obj := obj + Format('%.3d%.3d',[(Self.Bloky[i] as TPrejezd).StaticPositions[j].X, (Self.Bloky[i] as TPrejezd).StaticPositions[j].Y]);
     inifile.WriteString('PRJ'+IntToStr(Self.Bloky[i].Index), 'SP', obj);
    end;

    TBlkType.popisek:begin
     inifile.WriteString('T'+IntToStr(Self.Bloky[i].Index),  'T', (Self.Bloky[i] as TPopisek).Text);
     inifile.WriteInteger('T'+IntToStr(Self.Bloky[i].Index), 'X', (Self.Bloky[i] as TPopisek).Position.X);
     inifile.WriteInteger('T'+IntToStr(Self.Bloky[i].Index), 'Y', (Self.Bloky[i] as TPopisek).Position.Y);
     inifile.WriteInteger('T'+IntToStr(Self.Bloky[i].Index), 'C', (Self.Bloky[i] as TPopisek).Color);

     if (Length((Self.Bloky[i] as TPopisek).Text) = 1) then
      begin
       inifile.WriteInteger('T'+IntToStr(Self.Bloky[i].Index), 'B', Self.Bloky[i].Blok);
       inifile.WriteInteger('T'+IntToStr(Self.Bloky[i].Index), 'OR', Self.Bloky[i].OblRizeni);
      end;
    end;

    TBlkType.pomocny_obj:begin
     inifile.WriteInteger('P'+IntToStr(Self.Bloky[i].Index),'S', (Self.Bloky[i] as TPomocnyObj).Symbol);
     obj := '';
     for j := 0 to (Self.Bloky[i] as TPomocnyObj).Positions.Count-1 do obj := obj + Format('%.3d%.3d',[(Self.Bloky[i] as TPomocnyObj).Positions[j].X, (Self.Bloky[i] as TPomocnyObj).Positions[j].Y]);
     inifile.WriteString('P'+IntToStr(Self.Bloky[i].Index),'P', obj);
    end;

    TBlkType.uvazka:begin
     inifile.WriteInteger('Uv'+IntToStr(Self.Bloky[i].Index), 'B',  Self.Bloky[i].Blok);
     inifile.WriteInteger('Uv'+IntToStr(Self.Bloky[i].Index), 'OR', Self.Bloky[i].OblRizeni);

     inifile.WriteInteger('Uv'+IntToStr(Self.Bloky[i].Index), 'X', (Self.Bloky[i] as TUvazka).Pos.X);
     inifile.WriteInteger('Uv'+IntToStr(Self.Bloky[i].Index), 'Y', (Self.Bloky[i] as TUvazka).Pos.Y);
     inifile.WriteInteger('Uv'+IntToStr(Self.Bloky[i].Index), 'D', (Self.Bloky[i] as TUvazka).defalt_dir);
    end;

    TBlkType.uvazka_spr:begin
     inifile.WriteInteger('UvS'+IntToStr(Self.Bloky[i].Index), 'B', Self.Bloky[i].Blok);
     inifile.WriteInteger('UvS'+IntToStr(Self.Bloky[i].Index), 'OR', Self.Bloky[i].OblRizeni);

     inifile.WriteInteger('UvS'+IntToStr(Self.Bloky[i].Index), 'X', (Self.Bloky[i] as TUvazkaSpr).Pos.X);
     inifile.WriteInteger('UvS'+IntToStr(Self.Bloky[i].Index), 'Y', (Self.Bloky[i] as TUvazkaSpr).Pos.Y);
     inifile.WriteInteger('UvS'+IntToStr(Self.Bloky[i].Index), 'VD', Integer((Self.Bloky[i] as TUvazkaSpr).vertical_dir));
     inifile.WriteInteger('UvS'+IntToStr(Self.Bloky[i].Index), 'C', (Self.Bloky[i] as TUvazkaSpr).spr_cnt);
    end;

    TBlkType.zamek:begin
     inifile.WriteInteger('Z'+IntToStr(Self.Bloky[i].Index), 'B',  Self.Bloky[i].Blok);
     inifile.WriteInteger('Z'+IntToStr(Self.Bloky[i].Index), 'OR', Self.Bloky[i].OblRizeni);

     inifile.WriteInteger('Z'+IntToStr(Self.Bloky[i].Index), 'X', (Self.Bloky[i] as TZamek).Pos.X);
     inifile.WriteInteger('Z'+IntToStr(Self.Bloky[i].Index), 'Y', (Self.Bloky[i] as TZamek).Pos.Y);
    end;

    TBlkType.vykol:begin
     inifile.WriteInteger('Vyk'+IntToStr(Self.Bloky[i].Index), 'B',  Self.Bloky[i].Blok);
     inifile.WriteInteger('Vyk'+IntToStr(Self.Bloky[i].Index), 'OR', Self.Bloky[i].OblRizeni);

     inifile.WriteInteger('Vyk'+IntToStr(Self.Bloky[i].Index), 'X', (Self.Bloky[i] as TVykol).Pos.X);
     inifile.WriteInteger('Vyk'+IntToStr(Self.Bloky[i].Index), 'Y', (Self.Bloky[i] as TVykol).Pos.Y);
     inifile.WriteInteger('Vyk'+IntToStr(Self.Bloky[i].Index), 'T', (Self.Bloky[i] as TVykol).symbol);
     inifile.WriteInteger('Vyk'+IntToStr(Self.Bloky[i].Index), 'O', (Self.Bloky[i] as TVykol).obj);
    end;

    TBlkType.rozp:begin
     inifile.WriteInteger('R'+IntToStr(Self.Bloky[i].Index), 'B',  Self.Bloky[i].Blok);
     inifile.WriteInteger('R'+IntToStr(Self.Bloky[i].Index), 'OR', Self.Bloky[i].OblRizeni);

     inifile.WriteInteger('R'+IntToStr(Self.Bloky[i].Index), 'X', (Self.Bloky[i] as TRozp).Pos.X);
     inifile.WriteInteger('R'+IntToStr(Self.Bloky[i].Index), 'Y', (Self.Bloky[i] as TRozp).Pos.Y);
    end;

   end;//case

   Inc(counts[Integer(Self.Bloky[i].typ)]);
  end;//for i

 // pocty bloku v tomto poradi: useky, navestidla, vyhybky, prejezdy, popisky, pomocne_objekty, uvazky, uvazky_spr
 inifile.WriteInteger('P', 'U',   counts[0]);
 inifile.WriteInteger('P', 'N',   counts[1]);
 inifile.WriteInteger('P', 'V',   counts[2]);
 inifile.WriteInteger('P', 'PRJ', counts[3]);
 inifile.WriteInteger('P', 'T',   counts[4]);
 inifile.WriteInteger('P', 'P',   counts[5]);
 inifile.WriteInteger('P', 'Uv',  counts[6]);
 inifile.WriteInteger('P', 'UvS', counts[7]);
 inifile.WriteInteger('P', 'Z'  , counts[8]);
 inifile.WriteInteger('P', 'Vyk', counts[9]);
 inifile.WriteInteger('P', 'R'  , counts[10]);

 inifile.UpdateFile;
 inifile.Free;

 Result := 0;
end;//procedure SaveFile

////////////////////////////////////////////////////////////////////////////////

procedure TPanelObjects.Paint();
begin
 case (Self.FMode) of
   dmBloky, dmRoots : Self.PaintBloky();
 end;
end;//procedure

procedure TPanelObjects.PaintBloky();
var i,j, color:Integer;
begin
 // pruhlednost
 Self.DrawObject.Canvas.Pen.Mode := pmMerge;

 for i := 0 to Self.Bloky.Count-1 do
  begin
   case (Self.Bloky[i].typ) of
    TBlkType.usek:begin
       if (Self.Selected = Self.Bloky[i]) then
        begin
         for j := 0 to (Self.Bloky[i] as TUsek).JCClick.Count-1 do Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (Self.Bloky[i] as TUsek).JCClick[j].X*_Symbol_Sirka, (Self.Bloky[i] as TUsek).JCClick[j].Y*_Symbol_Vyska, _KPopisek_Index);
         for j := 0 to (Self.Bloky[i] as TUsek).KPopisek.Count-1 do Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (Self.Bloky[i] as TUsek).KPopisek[j].X*_Symbol_Sirka, (Self.Bloky[i] as TUsek).KPopisek[j].Y*_Symbol_Vyska, _JCPopisek_Index);
         for j := 0 to (Self.Bloky[i] as TUsek).Symbols.Count-1 do Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (Self.Bloky[i] as TUsek).Symbols[j].Position.X*_Symbol_Sirka, (Self.Bloky[i] as TUsek).Symbols[j].Position.Y*_Symbol_Vyska,((Self.Bloky[i] as TUsek).Symbols[j].SymbolID*10)+Self.Colors.Selected);
         Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (Self.Bloky[i] as TUsek).Root.X*_Symbol_Sirka, (Self.Bloky[i] as TUsek).Root.Y*_Symbol_Vyska, (_Root_Index*10) + Self.Colors.Selected);
        end else begin
         for j := 0 to (Self.Bloky[i] as TUsek).JCClick.Count-1 do Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (Self.Bloky[i] as TUsek).JCClick[j].X*_Symbol_Sirka, (Self.Bloky[i] as TUsek).JCClick[j].Y*_Symbol_Vyska, _JCPopisek_Index);
         for j := 0 to (Self.Bloky[i] as TUsek).KPopisek.Count-1 do Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (Self.Bloky[i] as TUsek).KPopisek[j].X*_Symbol_Sirka, (Self.Bloky[i] as TUsek).KPopisek[j].Y*_Symbol_Vyska, _KPopisek_Index);

         if (Self.Mode = TMode.dmBloky) then
          begin
           case (Self.Bloky[i].Blok) of
            -1: color := Self.Colors.Alert;
            -2: color := Self.Colors.IntUnassigned;
           else
             color := Self.Colors.Normal;
           end;
          end else begin
           if (((Self.Bloky[i] as TUsek).IsVyhybka) and ((Self.Bloky[i] as TUsek).Root.X = -1)) then
             color := Self.Colors.Alert
           else
             color := Self.Colors.Normal;
          end;

         for j := 0 to (Self.Bloky[i] as TUsek).Symbols.Count-1 do Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (Self.Bloky[i] as TUsek).Symbols[j].Position.X*_Symbol_Sirka, (Self.Bloky[i] as TUsek).Symbols[j].Position.Y*_Symbol_Vyska,((Self.Bloky[i] as TUsek).Symbols[j].SymbolID*10)+color);
         Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (Self.Bloky[i] as TUsek).Root.X*_Symbol_Sirka, (Self.Bloky[i] as TUsek).Root.Y*_Symbol_Vyska, (_Root_Index*10) + _Root_Color);
        end;//else Selected = i
    end;

    /////////////////////////////////////////////////

    TBlkType.navestidlo:begin
       if (Self.Selected = Self.Bloky[i]) then
        begin
         Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (Self.Bloky[i] as TNavestidlo).Position.X*_Symbol_Sirka, (Self.Bloky[i] as TNavestidlo).Position.Y*_Symbol_Vyska, ((_SCom_Start+(Self.Bloky[i] as TNavestidlo).SymbolID)*10)+Self.Colors.Selected);
        end else begin
         case (Self.Bloky[i].Blok) of
           -1: Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (Self.Bloky[i] as TNavestidlo).Position.X*_Symbol_Sirka, (Self.Bloky[i] as TNavestidlo).Position.Y*_Symbol_Vyska,((_SCom_Start+(Self.Bloky[i] as TNavestidlo).SymbolID)*10)+Self.Colors.Alert);
           -2: Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (Self.Bloky[i] as TNavestidlo).Position.X*_Symbol_Sirka, (Self.Bloky[i] as TNavestidlo).Position.Y*_Symbol_Vyska,((_SCom_Start+(Self.Bloky[i] as TNavestidlo).SymbolID)*10)+Self.Colors.IntUnassigned);
         else
           Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (Self.Bloky[i] as TNavestidlo).Position.X*_Symbol_Sirka, (Self.Bloky[i] as TNavestidlo).Position.Y*_Symbol_Vyska,((_SCom_Start+(Self.Bloky[i] as TNavestidlo).SymbolID)*10)+Self.Colors.Normal);
         end;
        end;//else (Self.Selected > 255)
    end;

    /////////////////////////////////////////////////

    TBlkType.vyhybka:begin
       if (Self.Selected = Self.Bloky[i]) then
        begin
         Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (Self.Bloky[i] as TVyhybka).Position.X*_Symbol_Sirka, (Self.Bloky[i] as TVyhybka).Position.Y*_Symbol_Vyska, (((Self.Bloky[i] as TVyhybka).SymbolID)*10)+Self.Colors.Selected);
        end else begin
         case (Self.Bloky[i].Blok) of
           -1: Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (Self.Bloky[i] as TVyhybka).Position.X*_Symbol_Sirka, (Self.Bloky[i] as TVyhybka).Position.Y*_Symbol_Vyska, (((Self.Bloky[i] as TVyhybka).SymbolID)*10)+Self.Colors.Alert);
           -2: Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (Self.Bloky[i] as TVyhybka).Position.X*_Symbol_Sirka, (Self.Bloky[i] as TVyhybka).Position.Y*_Symbol_Vyska, (((Self.Bloky[i] as TVyhybka).SymbolID)*10)+Self.Colors.IntUnassigned);
         else
           Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (Self.Bloky[i] as TVyhybka).Position.X*_Symbol_Sirka, (Self.Bloky[i] as TVyhybka).Position.Y*_Symbol_Vyska, (((Self.Bloky[i] as TVyhybka).SymbolID)*10)+Self.Colors.Normal);
         end;
        end;//else (Self.Selected > 255)
    end;

    /////////////////////////////////////////////////

    TBlkType.prejezd:begin
       if (Self.Selected = Self.Bloky[i]) then
        begin
         color := Self.Colors.Selected;
        end else begin
         case (Self.Bloky[i].Blok) of
          -1: color := Self.Colors.Alert;
          -2: color := Self.Colors.IntUnassigned;
         else
           color := Self.Colors.Normal;
         end;
        end;//else (Self.Selected > 255)

       for j := 0 to (Self.Bloky[i] as TPrejezd).StaticPositions.Count-1 do
           Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (Self.Bloky[i] as TPrejezd).StaticPositions[j].X*_Symbol_Sirka, (Self.Bloky[i] as TPrejezd).StaticPositions[j].Y*_Symbol_Vyska, _Prj_Index+color);

       for j := 0 to (Self.Bloky[i] as TPrejezd).BlikPositions.Count-1 do
           Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (Self.Bloky[i] as TPrejezd).BlikPositions[j].Pos.X*_Symbol_Sirka, (Self.Bloky[i] as TPrejezd).BlikPositions[j].Pos.Y*_Symbol_Vyska, _Prj_Index+color);
    end;

    /////////////////////////////////////////////////

    TBlkType.popisek:begin
       if (Length((Self.Bloky[i] as TPopisek).Text) = 1) then
        begin
         if (Self.Selected = Self.Bloky[i]) then
          begin
           color := Self.Colors.Selected;
          end else begin
           case (Self.Bloky[i].Blok) of
            -1: color := Self.Colors.Alert;
            -2: color := Self.Colors.IntUnassigned;
           else
             color := Self.Colors.Normal;
           end;
          end;//else (Self.Selected > 255)
        end else begin
          color := (Self.Bloky[i] as TPopisek).Color;
        end;

       Self.Graphics.TextOutputI((Self.Bloky[i] as TPopisek).Position, (Self.Bloky[i] as TPopisek).Text, color, clBlack);
    end;

    /////////////////////////////////////////////////

    TBlkType.pomocny_obj:begin
       for j := 0 to (Self.Bloky[i] as TPomocnyObj).Positions.Count-1 do
         Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas,
                                       (Self.Bloky[i] as TPomocnyObj).Positions[j].X*_Symbol_Sirka,
                                       (Self.Bloky[i] as TPomocnyObj).Positions[j].Y*_Symbol_Vyska,
                                       ((Self.Bloky[i] as TPomocnyObj).Symbol*10)+_Bitmap_DrawColors[(Self.Bloky[i] as TPomocnyObj).Symbol]);
    end;

    /////////////////////////////////////////////////

    TBlkType.uvazka:begin
     if (Self.Selected = Self.Bloky[i]) then
      begin
       color := Self.Colors.Selected;
      end else begin
       case (Self.Bloky[i].Blok) of
        -1: color := Self.Colors.Alert;
        -2: color := Self.Colors.IntUnassigned;
       else
         color := Self.Colors.Normal;
       end;
      end;//else (Self.Selected > 255)

     Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (Self.Bloky[i] as TUvazka).Pos.X*_Symbol_Sirka, (Self.Bloky[i] as TUvazka).Pos.Y*_Symbol_Vyska, (_Uvazka_Start*10)+color);
     Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, ((Self.Bloky[i] as TUvazka).Pos.X+1)*_Symbol_Sirka, (Self.Bloky[i] as TUvazka).Pos.Y*_Symbol_Vyska, ((_Uvazka_Start+1)*10)+color);
    end;

    /////////////////////////////////////////////////

    TBlkType.uvazka_spr:begin
       if (Self.Selected = Self.Bloky[i]) then
        begin
         Self.DrawObject.Canvas.Pen.Color := clRed;
         color := Self.Colors.Selected;
        end else begin
         case (Self.Bloky[i].Blok) of
          -1: begin
            color := Self.Colors.Alert;
            Self.DrawObject.Canvas.Pen.Color := clAqua;
          end;
          -2: begin
            color := Self.Colors.IntUnassigned;
            Self.DrawObject.Canvas.Pen.Color := clWhite;
          end
         else
           color := 7;
           Self.DrawObject.Canvas.Pen.Color := clYellow;
         end;
        end;//else (Self.Selected > 255)

       Self.DrawObject.Canvas.Brush.Color := clBlack;

       case ((Self.Bloky[i] as TUvazkaSpr).vertical_dir) of
        TUvazkaSprVertDir.top:begin
         Self.DrawObject.Canvas.Rectangle(
            (Self.Bloky[i] as TUvazkaSpr).Pos.X*_Symbol_Sirka,
            (Self.Bloky[i] as TUvazkaSpr).Pos.Y*_Symbol_Vyska + _Symbol_Vyska - 1,
            ((Self.Bloky[i] as TUvazkaSpr).Pos.X + _Uvazka_Spr_Sirka)*_Symbol_Sirka - 1,
            ((Self.Bloky[i] as TUvazkaSpr).Pos.Y - (Self.Bloky[i] as TUvazkaSpr).spr_cnt + 1)*_Symbol_Vyska);
        end;

        TUvazkaSprVertDir.bottom:begin
         Self.DrawObject.Canvas.Rectangle(
            (Self.Bloky[i] as TUvazkaSpr).Pos.X*_Symbol_Sirka,
            (Self.Bloky[i] as TUvazkaSpr).Pos.Y*_Symbol_Vyska,
            ((Self.Bloky[i] as TUvazkaSpr).Pos.X + _Uvazka_Spr_Sirka)*_Symbol_Sirka - 1,
            ((Self.Bloky[i] as TUvazkaSpr).Pos.Y + (Self.Bloky[i] as TUvazkaSpr).spr_cnt)*_Symbol_Vyska - 1);
        end;
       end;//case

       Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (Self.Bloky[i] as TUvazkaSpr).Pos.X*_Symbol_Sirka, (Self.Bloky[i] as TUvazkaSpr).Pos.Y*_Symbol_Vyska, (_Uvazka_Spr_Index*10)+color);
    end;

    /////////////////////////////////////////////////

    TBlkType.zamek:begin
     if (Self.Selected = Self.Bloky[i]) then
      begin
       color := Self.Colors.Selected;
      end else begin
       case (Self.Bloky[i].Blok) of
        -1: color := Self.Colors.Alert;
        -2: color := Self.Colors.IntUnassigned;
       else
         color := Self.Colors.Normal;
       end;
      end;//else (Self.Selected > 255)

     Self.DrawObject.Canvas.Brush.Color := clBlack;
     Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (Self.Bloky[i] as TZamek).Pos.X*_Symbol_Sirka, (Self.Bloky[i] as TZamek).Pos.Y*_Symbol_Vyska, (_Zamek*10)+color);
    end;

    /////////////////////////////////////////////////

    TBlkType.vykol:begin
     if (Self.Selected = Self.Bloky[i]) then
      begin
       color := Self.Colors.Selected;
      end else begin
       case (Self.Bloky[i].Blok) of
        -1: color := Self.Colors.Alert;
        -2: color := Self.Colors.IntUnassigned;
       else
         color := Self.Colors.Normal;
       end;
      end;//else (Self.Selected > 255)

     Self.DrawObject.Canvas.Brush.Color := clBlack;
     Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (Self.Bloky[i] as TVykol).Pos.X*_Symbol_Sirka, (Self.Bloky[i] as TVykol).Pos.Y*_Symbol_Vyska, ((_Vykol_Start+(Self.Bloky[i] as TVykol).symbol)*10)+color);
    end;

    /////////////////////////////////////////////////

    TBlkType.rozp:begin
     if (Self.Selected = Self.Bloky[i]) then
      begin
       color := Self.Colors.Selected;
      end else begin
       case (Self.Bloky[i].Blok) of
        -1: color := Self.Colors.Alert;
        -2: color := Self.Colors.IntUnassigned;
       else
         color := Self.Colors.Normal;
       end;
      end;//else (Self.Selected > 255)

     Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (Self.Bloky[i] as TRozp).Pos.X*_Symbol_Sirka, (Self.Bloky[i] as TRozp).Pos.Y*_Symbol_Vyska, ((_Rozp_Start+1)*10)+color);
    end;

    /////////////////////////////////////////////////

   end;//case typ
  end;//for i
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//Escape
procedure TPanelObjects.Escape();
begin
 Self.Selected := nil;
end;//procedure

//vykresleni kurzoru - vraci data v PIXELECH!
function TPanelObjects.PaintCursor(CursorPos:TPoint):TCursorDraw;
begin
 //vykreslit kurzor
 Result.Color := 0;
 Result.Pos1.X := CursorPos.X*_Symbol_Sirka;
 Result.Pos1.Y := CursorPos.Y*_Symbol_Vyska;
 Result.Pos2.X := CursorPos.X*_Symbol_Sirka;
 Result.Pos2.Y := CursorPos.Y*_Symbol_Vyska;

 //vykreslit koren pod kurzorem
 if (Self.FMode = dmRoots) then
   Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, CursorPos.X*_Symbol_Sirka, CursorPos.Y*_Symbol_Vyska, (_Root_Index*10)+_Root_Color);
end;//function

////////////////////////////////////////////////////////////////////////////////

function TPanelObjects.GetObject(Pos:TPoint):Integer;
var i,j, tmp:Integer;
begin
 tmp := -1;
 for i := 0 to Self.Bloky.Count-1 do
  begin
   case (Self.Bloky[i].typ) of
    TBlkType.usek:begin
     for j := 0 to (Self.Bloky[i] as TUsek).Symbols.Count-1 do
       if ((Pos.X = (Self.Bloky[i] as TUsek).Symbols[j].Position.X) and (Pos.Y = (Self.Bloky[i] as TUsek).Symbols[j].Position.Y)) then
         tmp := i;      // usek nema prioritu (napriklad nad rozpojovacem)
    end;

    TBlkType.navestidlo : if ((Pos.X = (Self.Bloky[i] as TNavestidlo).Position.X) and (Pos.Y = (Self.Bloky[i] as TNavestidlo).Position.Y)) then Exit(i);

    TBlkType.vyhybka : if ((Pos.X = (Self.Bloky[i] as TVyhybka).Position.X) and (Pos.Y = (Self.Bloky[i] as TVyhybka).Position.Y)) then Exit(i);

    TBlkType.prejezd:begin
     for j := 0 to (Self.Bloky[i] as TPrejezd).StaticPositions.Count-1 do
       if ((Self.Bloky[i] as TPrejezd).StaticPositions[j].X = Pos.X) and ((Self.Bloky[i] as TPrejezd).StaticPositions[j].Y = Pos.Y) then
         Exit(i);
     for j := 0 to (Self.Bloky[i] as TPrejezd).BlikPositions.Count-1 do
       if ((Self.Bloky[i] as TPrejezd).BlikPositions[j].Pos.X = Pos.X) and ((Self.Bloky[i] as TPrejezd).BlikPositions[j].Pos.Y = Pos.Y) then
         Exit(i);
    end;

    TBlkType.popisek:begin
     if ((Pos.Y = (Self.Bloky[i] as TPopisek).Position.Y) and (Pos.X >= (Self.Bloky[i] as TPopisek).Position.X)
      and (Pos.X < (Self.Bloky[i] as TPopisek).Position.X+Length((Self.Bloky[i] as TPopisek).Text)) and (Length((Self.Bloky[i] as TPopisek).Text) = 1)) then
       Exit(i);
    end;

    TBlkType.pomocny_obj:begin
      // klik na pomocny objekt nas nezajima
    end;

    TBlkType.uvazka : if (((Self.Bloky[i] as TUvazka).Pos.Y = Pos.Y) and (Pos.X >= (Self.Bloky[i] as TUvazka).Pos.X) and (Pos.X <= (Self.Bloky[i] as TUvazka).Pos.X+1)) then Exit(i);

    TBlkType.uvazka_spr : if (((Self.Bloky[i] as TUvazkaSpr).Pos.Y = Pos.Y) and (Pos.X = (Self.Bloky[i] as TUvazkaSpr).Pos.X)) then Exit(i);

    TBlkType.zamek : if (((Self.Bloky[i] as TZamek).Pos.Y = Pos.Y) and (Pos.X = (Self.Bloky[i] as TZamek).Pos.X)) then Exit(i);

    TBlkType.vykol : if (((Self.Bloky[i] as TVykol).Pos.Y = Pos.Y) and (Pos.X = (Self.Bloky[i] as TVykol).Pos.X)) then Exit(i);

    TBlkType.rozp : if (((Self.Bloky[i] as TRozp).Pos.Y = Pos.Y) and (Pos.X = (Self.Bloky[i] as TRozp).Pos.X)) then Exit(i);

   end;//case
  end;

 Result := tmp;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TPanelObjects.MouseMove(Position:TPoint);
var tmp:Integer;
begin
 if (Self.FMode = dmRoots) then
  begin
   tmp := Self.GetObject(Position);
   if ((tmp = -1) or (Self.bloky[tmp].typ <> TBLkType.usek)) then
    begin
     Self.Selected := nil;
     Exit;
    end;
   if ((Self.bloky[tmp] as TUsek).IsVyhybka) then
     Self.Selected := Self.bloky[tmp];
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TPanelObjects.MouseUp(Position:TPoint;Button:TMouseButton):Byte;
begin
 case (Self.FMode) of
  dmBloky : Result := Self.BlokyMouseUp(Position, Button);
  dmRoots : Result := Self.KorenyMouseUp(Position, Button);
 else
  Result := 0;
 end;
end;//function

function TPanelObjects.BlokyMouseUp(Position:TPoint;Button:TMouseButton):Byte;
var blk:Integer;
begin
 Result := 0;

 blk := Self.GetObject(Position);
 if (blk < 0) then
  begin
   Self.Selected := nil;
   Exit();
  end else
   Self.Selected := Self.Bloky[blk];

 if (Assigned(Self.FOnMsg)) then
  begin
   case (Self.Selected.typ) of
    TBlkType.usek         : Self.FOnMsg(Self, 'Blok '+IntToStr(blk)+ ' (úsek '+IntToStr(Self.Selected.index)+')');
    TBlkType.navestidlo   : Self.FOnMsg(Self, 'Blok '+IntToStr(blk)+ ' (návìstidlo '+IntToStr(Self.Selected.index)+')');
    TBlkType.vyhybka      : Self.FOnMsg(Self, 'Blok '+IntToStr(blk)+ ' (výhybka '+IntToStr(Self.Selected.index)+'), pøiøazen úseku '+IntToStr((Self.Selected as TVyhybka).obj));
    TBlkType.prejezd      : Self.FOnMsg(Self, 'Blok '+IntToStr(blk)+ ' (pøejezd '+IntToStr(Self.Selected.index)+')');
    TBlkType.popisek      : Self.FOnMsg(Self, 'Blok '+IntToStr(blk)+ ' (popisek '+IntToStr(Self.Selected.index)+')');
    TBlkType.pomocny_obj  : Self.FOnMsg(Self, 'Blok '+IntToStr(blk)+ ' (pomocný objekt '+IntToStr(Self.Selected.index)+')');
    TBlkType.uvazka       : Self.FOnMsg(Self, 'Blok '+IntToStr(blk)+ ' (úvazka '+IntToStr(Self.Selected.index)+')');
    TBlkType.uvazka_spr   : Self.FOnMsg(Self, 'Blok '+IntToStr(blk)+ ' (úvazka spr. '+IntToStr(Self.Selected.index)+')');
    TBlkType.zamek        : Self.FOnMsg(Self, 'Blok '+IntToStr(blk)+ ' (zámek '+IntToStr(Self.Selected.index)+')');
    TBlkType.vykol        : Self.FOnMsg(Self, 'Blok '+IntToStr(blk)+ ' (výkolejka '+IntToStr(Self.Selected.index)+'), pøiøazena úseku '+IntToStr((Self.Selected as TVykol).obj));
    TBlkType.rozp         : Self.FOnMsg(Self, 'Blok '+IntToStr(blk)+ ' (rozpojovaè '+IntToStr(Self.Selected.index)+')');
   end;//case
  end;//if Assigned(FOnMsg)

 //prave tlacitko mysi
 if (Button = mbRight) then
  begin
   if (Self.Selected.typ <> TBlkType.pomocny_obj) then
    begin
     Self.PopUpPos := Position;
     Self.PM_Properties.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    end;
  end;//mbRight

 ////////////////////////////////////////

 //leve tlacitko mysi
 if (Button = mbLeft) then
   if (Self.Selected.typ <> TBlkType.pomocny_obj) then
     Self.PMPropertiesClick(self);
end;//function

function TPanelObjects.KorenyMouseUp(Position:TPoint;Button:TMouseButton):Byte;
var blk:Integer;
begin
 Result := 0;

 //leve tlacitko mysi
 if (Button <> mbLeft) then Exit;

 blk := Self.GetObject(Position);
 if ((blk < 0) or (Self.Bloky[blk].typ <> TBlkType.usek)) then
  Self.Selected := nil
 else
  Self.Selected := Self.Bloky[blk];

 if (Assigned(Self.FOnMsg)) then Self.FOnMsg(Self, 'Blok '+IntToStr(blk)+ ' (úsek)');
 if ((not Assigned(Self.Selected)) or (not (Self.Selected as TUsek).IsVyhybka)) then Exit;
 (Self.Selected as TUsek).Root := Position;
end;//function

////////////////////////////////////////////////////////////////////////////////

function TPanelObjects.DblClick(Position:TPoint):Byte;
begin
 Self.PopUpPos := Position;
 Self.PMPropertiesClick(self);
 Result := 0;
end;//function

procedure TPanelObjects.CreatePM(var PM:TPopUpMenu;Parent:TDXDraw);
var MI:TMenuItem;
begin
 PM := TPopupMenu.Create(Parent);

 MI := TMenuItem.Create(PM);
 MI.Caption := 'Vlastnosti';
 MI.OnClick := Self.PMPropertiesClick;

 PM.Items.Add(MI);
end;//procedure

procedure TPanelObjects.PMPropertiesClick(Sender: TObject);
begin
 if (Self.Selected <> nil) then
   if (Assigned(Self.FOnBlokEdit)) then Self.FOnBlokEdit(Self, Self.Selected);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// tato funkce prirazuje aktivnimu bloku obalst rizeni v paramtru
//  to je uzitecne k tomu, ze pri vybrani bloku a nesdlednemu kliku na baracek dojde k prirazeni teto oblasti rizeni
function TPanelObjects.SetOR(OblR:Integer):Byte;
begin
 if (Self.Selected = nil) then Exit(1);
 Self.Selected.OblRizeni := OblR;
 Result := 0;
end;//function

//checking data valid
function TPanelObjects.CheckValid(var error_cnt:Byte):TStrings;
var str:string;
    i:Integer;
    vetev:TVetev;
begin
 Result := TStringList.Create();

 DateTimeToString(str,'yyyy-mm-dd hh:nn:ss',Now);
 Result.Add('Validace objektu: '+str);

 Result.Add('Overuji navaznost useku na technologicke bloky, navaznost na oblasti rizeni...');
 Result.Add('Overuji navaznost vyhybek na useky...');
 Result.Add('Overuji pojmenovani koleji...');
 Result.Add('Overuji koreny...');

 for i := 0 to Self.Bloky.Count-1 do
  begin
   if ((Self.Bloky[i].Blok = -1) and (Self.Bloky[i].typ <> TBlkType.pomocny_obj) and ((Self.Bloky[i].typ <> TBlkType.popisek) or (Length((Self.Bloky[i] as TPopisek).Text) = 1))) then
    begin
     Result.Add('ERR: blok '+IntToStr(i)+': neni navaznost na technologicky blok');
     error_cnt := error_cnt + 1;
    end;
   if ((Self.Bloky[i].OblRizeni < 0) and
    ((Self.Bloky[i].typ <> TBlkType.pomocny_obj) xor ((Self.Bloky[i].typ = TBlkType.popisek) and (Length((Self.Bloky[i] as TPopisek).Text) > 1)))) then
    begin
     Result.Add('ERR: blok '+IntToStr(i)+': neni navaznost na oblast rizeni');
     error_cnt := error_cnt + 1;
    end;


   case (Self.Bloky[i].typ) of
    TBlkType.usek:begin
     if (((Self.Bloky[i] as TUsek).KPopisek.Count > 0) and ((Self.Bloky[i] as TUsek).KpopisekStr = '')) then
      begin
       Result.Add('ERR: blok '+IntToStr(i)+' (usek): kolej neni pojmenovana');
       error_cnt := error_cnt + 1;
      end;

     if (((Self.Bloky[i] as TUsek).IsVyhybka) and ((Self.Bloky[i] as TUsek).Root.X < 0)) then
      begin
       Result.Add('ERR: blok '+IntToStr(i)+' (usek) : obsahuje vyhybky a presto nema koren');
       error_cnt := error_cnt + 1;
      end;

     for vetev in (Self.Bloky[i] as TUsek).Vetve do
      begin
       if (((vetev.node1.vyh >= 0) and ((vetev.node1.ref_plus = -1) or (vetev.node1.ref_minus = -1))) or
          ((vetev.node2.vyh >= 0) and ((vetev.node2.ref_plus = -1) or (vetev.node2.ref_minus = -1)))) then
         Result.Add('WARN: blok '+IntToStr(i)+' (usek) : vetev ma navaznost na vyhybku, presto za vyhybkou nikam nepokracuje');
      end;
    end;

    //////////////////////////////////

    TBlkType.vyhybka:begin
     if ((Self.Bloky[i] as TVyhybka).obj < 0) then
      begin
       Result.Add('ERR: blok '+IntToStr(i)+' (vyhybka) : neni navaznost na usek');
       error_cnt := error_cnt + 1;
      end;
    end;

    //////////////////////////////////

   end;//case
  end;//for i

 Result.Add('--- Validace hotova. ---');
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TPanelObjects.SetMode(mode:TMode);
begin
 Self.FMode := mode;
 Self.Selected := nil;
 if (mode = TMode.dmRoots) then
   Self.ComputeVyhybkaFlag();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//prirazuje blokum flag, ktery rika, jestli jsou an nem vyhybky
procedure TPanelObjects.ComputeVyhybkaFlag();
var i, j:Integer;
begin
 //reset flagu
 for i := 0 to Self.Bloky.Count-1 do
   if (Self.Bloky[i].typ = TBlkType.usek) then
     (Self.Bloky[i] as TUsek).IsVyhybka := false;

 //zjistime, jestli na danych blocich jsou vyhybky
 for i := 0 to Self.Bloky.Count-1 do
   if (Self.Bloky[i].typ = TBlkType.vyhybka) then
    begin
     // .obj referuje na index v seznamu useku -> musime vypocitat index v poli vsech bloku
     for j := 0 to Self.Bloky.Count-1 do
       if ((Self.Bloky[j].typ = TBlkType.usek) and (Self.Bloky[j].index = (Self.Bloky[i] as TVyhybka).obj)) then
        begin
         (Self.Bloky[j] as TUsek).IsVyhybka := true;
         break;
        end;
    end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// import dat z jineho modu
// zatim podporuje jen mod bitmap
function TPanelObjects.Import(Data:TObject):Byte;
var BitmapToObj:TBitmapToObj;
begin
 Result := 0;

 if (Data.ClassType = TPanelBitmap) then
  begin
   BitmapToObj := TBitmapToObj.Create;
   Result := BitmapToObj.BitmapToObjects((Data as TPanelBitmap), Self);
  end;//if (Data.ClassType = TPanelBitmap)

end;//function

////////////////////////////////////////////////////////////////////////////////

// pri ukladani dat je vypocteno, jakemu techologickemu useku patri prislusny prejezd
//  resp. jaky usek prejezd obsluhuje
//  jedna se o prostredni usek prejezdu
// tento usek se pripradi na zaklade leveho useku prejezdu
procedure TPanelObjects.ComputePrjTechUsek();
var i, j, usek:Integer;
    blik_point:TBlikPoint;
begin
 for i := 0 to Self.Bloky.count-1 do
  begin
   if (Self.Bloky[i].typ <> TBlkType.prejezd) then continue;
   
   for j := 0 to (Self.Bloky[i] as TPrejezd).BlikPositions.Count-1 do
    begin
     usek := Self.GetObject(Point((Self.Bloky[i] as TPrejezd).BlikPositions[j].Pos.X - 1, (Self.Bloky[i] as TPrejezd).BlikPositions[j].Pos.Y));
     blik_point := (Self.Bloky[i] as TPrejezd).BlikPositions[j];
     if (usek = -1) then
       blik_point.TechUsek := -1
      else
       blik_point.TechUsek := (Self.Bloky[usek] as TUsek).Blok;
     (Self.Bloky[i] as TPrejezd).BlikPositions[j] := blik_point;
    end;//for j
  end;//for i
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// tato metoda pocita rozvetveni useku na jednotlive vetve
procedure TPanelObjects.ComputeVetve();
var i, row, col, j:Integer;
    data:TVetveData;     // = Array of Array of Integer; toto pole je pro algoritmus nize zdrojem vsech dat
                         // jedna s o dvourozmerne pole reprezentujici cely panel, na kazdem poli je cislo bitmapoveho
                         // metoda pro zpracovani jednoho bloku dostane na vstup vzdy toto pole s tim, ze jsou v nem jen body, ktere patri prislusnemu bloku
                         // vsude jinde je (-1)
begin
 // inicializace pole data
 SetLength(data, Self.PanelWidth, Self.PanelHeight);
 for col := 0 to Self.PanelWidth-1 do
   for row := 0 to Self.PanelHeight-1 do
     data[col, row] := -1;

 for i := 0 to Self.Bloky.Count-1 do
  begin
   if (Self.Bloky[i].typ <> TBlkType.usek) then continue;

   (Self.Bloky[i] as TUsek).Vetve.Clear();
   if (not (Self.Bloky[i] as TUsek).IsVyhybka) then continue;

   if ((Self.Bloky[i] as TUsek).Root.X < 0) then
     continue;

   // inicializujeme pole symboly bloku
   for j := 0 to (Self.Bloky[i] as TUsek).Symbols.Count-1 do
    data[(Self.Bloky[i] as TUsek).Symbols[j].Position.X, (Self.Bloky[i] as TUsek).Symbols[j].Position.Y] := (Self.Bloky[i] as TUsek).Symbols[j].SymbolID;

   // do tohoto pole take musime ulozit vyhybky
   for j := 0 to Self.Bloky.Count-1 do
    begin
     if ((Self.Bloky[j].typ <> TBlkType.vyhybka) or ((Self.Bloky[j] as TVyhybka).obj <> Self.Bloky[i].index)) then continue;
     data[(Self.Bloky[j] as TVyhybka).Position.X, (Self.Bloky[j] as TVyhybka).Position.Y] := (Self.Bloky[j] as TVyhybka).SymbolID;
    end;

   // provedeme algoritmus
   Self.ComputeBlokVetve(data, (Self.Bloky[i] as TUsek).Root, (Self.Bloky[i] as TUsek).Vetve);

   // nastavime pole data opet na -1 (algortimus by to mel udelat sam, ale pro jistotu)
   for j := 0 to (Self.Bloky[i] as TUsek).Symbols.Count-1 do
    data[(Self.Bloky[i] as TUsek).Symbols[j].Position.X, (Self.Bloky[i] as TUsek).Symbols[j].Position.Y] := -1;

   // odstranit vyhybky
   for j := 0 to Self.Bloky.Count-1 do
    begin
     if ((Self.Bloky[i].typ <> TBlkType.vyhybka) or ((Self.Bloky[i] as TVyhybka).obj <> i)) then continue;
     data[(Self.Bloky[i] as TVyhybka).Position.X, (Self.Bloky[i] as TVyhybka).Position.Y] := -1;
    end;
  end;//for i
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// tato funkce pocita vetve jednoho bloku
//   provadi prohledavani do sirky
//   ve fronte je vzdy ulozena startovni pozice, jedna iterace ji expanduje az do vyhybky (nebo prazdneho pole)
//   pokud algortimus narazi na vyhybky, do fronty prida 2 deti reprezentujici 2 cesty, kam jde kolej
//   v tomto momente lze ziskat index deti v poli vetve jako Vetve.Count + Queue.Count (2. vetev +1) z predpokladu zachovani poradi:
//      1) kolej do rovna
//      2) kolej do odbocky
//    pozor: deti pridavame jen v pripade, ze existuji (muze se taky stat, ze za vyhybkou nic neni) !!!
procedure TPanelObjects.ComputeBlokVetve(data:TVetveData; start:TPoint; Vetve:TList<TVetev>);
const
    _Usek_Navaznost: array [0..47] of ShortInt = (-1,0,1,0,0,-1,0,1,0,1,1,0,-1,0,0,-1,-1,0,0,1,0,-1,1,0,-1,0,1,0,0,-1,0,1,0,1,1,0,-1,0,0,-1,-1,0,0,1,0,-1,1,0);
    _Vyh_Navaznost : array [0..15]  of ShortInt = (1,0,0,-1, 1,0,0,1, -1,0,0,-1, -1,0,0,1);        // vedjesi symboly vyhybek ulozeny ve formatu: rovna_x,rovna_y,odbocka_x,odbocka,y, ...

    _UsekS_Start   = 12;
    _UsekS_End     = 23;

    _VyhybkaS_Start = 0;
    _VyhybkaS_End   = 3;

var queue:TQueue<TPoint>;
    first, temp, new:TPoint;
    i, j:Integer;
    vetev:TVetev;

    symbols:TList<TReliefSym>;
    symbol:TReliefSym;
    node:^TVetevEnd;
begin
 queue := TQueue<TPoint>.Create();
 queue.Enqueue(start);

 symbols := TList<TReliefSym>.Create();

 while (queue.Count > 0) do
  begin
   // expanduji jeden vrchol: pozor:  musim expandovat doleva i doprava

   // resetuji vetev:
   vetev.node1.vyh        := -1;
   vetev.node1.ref_plus   := 0;
   vetev.node1.ref_minus  := 0;

   vetev.node2.vyh        := -1;
   vetev.node2.ref_plus   := 0;
   vetev.node2.ref_minus  := 0;

   // vezmu aktualni vrchol z fronty
   first := queue.Dequeue();

   // dodelat kontrolu toho, jestli nahodou nejsem na vyhybce

   // do pole symbols si ulzoim useku aktualni vetve a pak je pouze predelam do dynamickeho pole vetev.symbols
   symbols.Clear();

   // expanduji oba smery z prvniho symbolu, dalsi symboly maji vzdy jen jeden smer k expanzi
   for j := 0 to 1 do
    begin
     new := first;
     // projizdim usek jednim smerem pro jedno 'j'
     while (((data[new.X, new.Y] >= _UsekS_Start) and (data[new.X, new.Y] <=_UsekS_End)) or
          ((data[new.X, new.Y] >= _Vykol_Start) and (data[new.X, new.Y] <=_Vykol_End))) do
      begin
       // pridam symbol do seznamu symbolu
       if (((new.X <> first.X) or (new.Y <> first.Y)) or (j = 1)) then
        begin
         symbol.Position := new;
         symbol.SymbolID := data[new.X, new.Y];
         symbols.Add(symbol);
        end;

       temp := new;

       // vypocitam prvni vedlejsi pole
       if ((data[new.X, new.Y] >= _Vykol_Start) and (data[new.X, new.Y] <=_Vykol_End)) then
        begin
         (Self.Bloky[Self.GetObject(new)] as TVykol).vetev := queue.Count;
         data[new.X, new.Y] := _UsekS_Start;
        end;

       // podivame se na prvni ze dvou vedlejsich policek
       temp.X := new.X + _Usek_Navaznost[((data[new.X, new.Y]-_UsekS_Start)*4) + 0];
       temp.Y := new.Y + _Usek_Navaznost[((data[new.X, new.Y]-_UsekS_Start)*4) + 1];

       // je na vedlejsim poli symbol?
       if (data[temp.X, temp.Y] > -1) and ((temp.X <> first.X) or (temp.Y <> first.Y)) then
        begin
         // ano, na prvnim vedlejsim poli je symbol -> resetuji aktualni pole a do noveho pole priradim pole vedlejsi
        end else begin
         // ne, na vedlejsim poli neni symbol -> toto policko jsme asi uz prosli, nebo tam proste nic neni
         // zkusime druhe vedlejsi policko
         temp.X := new.X + _Usek_Navaznost[((data[new.X, new.Y]-_UsekS_Start)*4) + 2];
         temp.Y := new.Y + _Usek_Navaznost[((data[new.X, new.Y]-_UsekS_Start)*4) + 3];
        end;

       // pokud je na druhem vedlejsim poli symbol, expanduji ho; pokud tam neni, while cyklus prochazeni utne
       if (((new.X <> first.X) or (new.Y <> first.Y)) or (j = 1)) then
         data[new.X, new.Y] := -1;
       new := temp;
      end;//while

     // skoncilo prochazeni jednoho smeru aktualni vetve
     if (data[new.X, new.Y] >= _VyhybkaS_Start) and (data[new.X, new.Y] <= _VyhybkaS_End) then
      begin
       // na aktualnim poli je vyhybka -> nova vetev
       if (vetev.node1.vyh > -1) then
        begin
         // 1. vyhybka uz existuje -> vytvorim 2. vyhybku
         node := @vetev.node2;
        end else begin
         // 1. vyhybka neexstuje -> vytvorim 1. vyhybku
         node := @vetev.node1;
        end;

       // zjistim index vyhybky a pridam dalsi vetve, pokud za vyhybkou existuji useky
       node^.vyh := Self.Bloky[Self.GetObject(new)].index;
       node^.ref_plus := -1;
       node^.ref_minus := -1;

       // projdu 2 smery, do kterych muze vyhybka (je jasne, ze barvici cesta musela prijit z te strany, kde se koleje spojuji)
       for i := 0 to 1 do
        begin
         temp.X := new.X + _Vyh_Navaznost[((data[new.X, new.Y]-_VyhybkaS_Start)*4) + (i*2)];
         temp.Y := new.Y + _Vyh_Navaznost[((data[new.X, new.Y]-_VyhybkaS_Start)*4) + (i*2)+1];

         if (data[temp.X, temp.Y] > -1) then
          begin
           // na dalsi pozici za vyhybkou je symbol -> novy zaznam do fronty
           queue.Enqueue(temp);

           case (i) of
            0 : node^.ref_plus  := Vetve.Count + queue.Count;
            1 : node^.ref_minus := Vetve.Count + queue.Count;
           end;//case
          end;
        end;//for 2_smery_do_kterych_muze_expandovat_vyhybka
       data[new.X, new.Y] := -1;      // vyhybku jsem zpracoval
      end;//if vetev_skoncila_vyhybkou
    end;//for j

   // prochazeni vetve je kompletni vcetne pripadnych koncovych vyhybek -> priradim do vetve symboly a vytvorim vetev
   SetLength(vetev.Symbols, symbols.Count);
   for i := 0 to symbols.Count-1 do
    vetev.Symbols[i] := symbols[i]; 
   Vetve.Add(vetev);
  end;//while

 symbols.Free();
 queue.Free();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

end.//unit
