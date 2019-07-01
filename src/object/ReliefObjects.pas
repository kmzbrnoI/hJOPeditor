unit ReliefObjects;
// tato unita pracuje s objektovymi daty reliefu

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, IniFiles,
  StrUtils, Global, Menus, Forms, DXDraws, OblastRizeni, PGraphics, RPConst,
  Generics.Collections, symbolHelper, vetev, ObjBlok;

const
 _Def_Color_Selected      = 2;
 _Def_Color_Normal        = 1;
 _Def_Color_Alert         = 5;
 _Def_Color_IntUnassigned = 4;

 //zde je definovano, jaky symbol se ma vykreslovat jakou barvou (mimo separatoru)
  _Bitmap_DrawColors:array [0..60] of Byte = (1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,6,6,6,1,1,1,1,1,1,1,1,1,1,7,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1);

 _FileVersion = '1.1';

type

EFileLoad = class(Exception);

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

  public
   Bloky:TList<TGraphBlok>;

    constructor Create(SymbolIL,TextIL:TImageList;DrawCanvas:TCanvas;Width,Height:Integer;Parent:TDXDraw; Graphics:TPanelGraphics);
    destructor Destroy; override;

    function Import(Data:TObject):Byte;     // import dat zatim z PanelBitmap

    procedure FLoad(aFile:string;var ORs:string);
    procedure FSave(aFile:string;const ORs:string);

    procedure Paint();            //obecny paint
    procedure PaintBloky();       //paint modu bloky
    function PaintCursor(CursorPos:TPoint):TCursorDraw;

    procedure ResetPanel;
    procedure Escape;

    procedure MouseUp(Position:TPoint;Button:TMouseButton);
    procedure MouseMove(Position:TPoint);
    procedure BlokyMouseUp(Position:TPoint;Button:TMouseButton);
    procedure KorenyMouseUp(Position:TPoint;Button:TMouseButton);
    procedure DblClick(Position:TPoint);

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

uses ReliefBitmap, BitmapToObj, VetveComputer, ObjBlokUsek, ObjBlokVyhybka,
     ObjBlokPomocny, ObjBlokNavestidlo, ObjBlokText, ObjBlokPrejezd, ObjBlokUvazka,
     ObjBlokUvazkaSpr, ObjBlokZamek, ObjBlokVykol, ObjBlokRozp;

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
procedure TPanelObjects.ResetPanel();
var blok:TGraphBlok;
begin
 for blok in Self.Bloky do
   blok.Free();
 Self.Bloky.Clear();
 Self.Selected := nil;
end;

//nacitani souboru
procedure TPanelObjects.FLoad(aFile:string;var ORs:string);
var i:Integer;
    inifile:TMemIniFile;
    ver:string;
    sect_str:TStrings;
    blok:TGraphBlok;
    count:Integer;
begin
 Self.FStav := 2;
 Self.FSoubor := aFile;

 Self.ResetPanel;

 //samotne nacitani dat
 inifile := TMemIniFile.Create(aFile, TEncoding.UTF8);
 try
   //kontrola verze
   ver := inifile.ReadString('G', 'ver', _FileVersion);
   if (_FileVersion <> ver) then
    begin
     if (Application.MessageBox(PChar('Na��t�te soubor s verz� '+ver+#13#10+'Aplikace moment�ln� podporuje verzi '+_FileVersion+#13#10+'Chcete pokra�ovat?'),
         'Varov�n�', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
       raise EFileLoad.Create('U�ivatel zru�il na��t�n� souboru!');
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
       blok := ObjBlokUsek.TUsek.Create(i);
       blok.Load(inifile, 'U'+IntToStr(i));
       Self.Bloky.Add(blok);
     except

     end;
    end;

   // navestidla
   count := inifile.ReadInteger('P', 'N', 0);
   for i := 0 to count-1 do
    begin
     try
       blok := TNavestidlo.Create(i);
       blok.Load(inifile, 'N'+IntToStr(i));
       Self.Bloky.Add(blok);
     except

     end;
    end;

   // pomocne symboly
   count := inifile.ReadInteger('P', 'P', 0);
   for i := 0 to count-1 do
    begin
     try
       blok := TPomocnyObj.Create(i);
       blok.Load(inifile, 'P'+IntToStr(i));
       Self.Bloky.Add(blok);
     except

     end;
    end;

   // popisky
   count := inifile.ReadInteger('P','T',0);
   for i := 0 to count-1 do
    begin
     try
       blok := TText.Create(i);
       blok.Load(inifile, 'T'+IntToStr(i));
       Self.Bloky.Add(blok);
     except

     end;
    end;

   // vyhybky
   count := inifile.ReadInteger('P','V',0);
   for i := 0 to count-1 do
    begin
     try
       blok := TVyhybka.Create(i);
       blok.Load(inifile, 'V'+IntToStr(i));
       Self.Bloky.Add(blok);
     except

     end;
    end;

   // prejezdy
   count := inifile.ReadInteger('P','PRJ',0);
   for i := 0 to count-1 do
    begin
     try
       blok := TPrejezd.Create(i);
       blok.Load(inifile, 'PRJ'+IntToStr(i));
       Self.Bloky.Add(blok);
     except

     end;
    end;

   // uvazky
   count := inifile.ReadInteger('P','Uv',0);
   for i := 0 to count-1 do
    begin
     try
       blok := TUvazka.Create(i);
       blok.Load(inifile, 'Uv'+IntToStr(i));
       Self.Bloky.Add(blok);
     except

     end;
    end;//for i

   // uvazky soupravy
   count := inifile.ReadInteger('P','UvS',0);
   for i := 0 to count-1 do
    begin
     try
       blok := TUvazkaSpr.Create(i);
       blok.Load(inifile, 'UvS'+IntToStr(i));
       Self.Bloky.Add(blok);
     except

     end;
    end;//for i

   // zamky
   count := inifile.ReadInteger('P','Z',0);
   for i := 0 to count-1 do
    begin
     try
       blok := TZamek.Create(i);
       blok.Load(inifile, 'Z'+IntToStr(i));
       Self.Bloky.Add(blok);
     except

     end;
    end;

   // vykolejky
   count := inifile.ReadInteger('P', 'Vyk', 0);
   for i := 0 to count-1 do
    begin
     try
       blok := TVykol.Create(i);
       blok.Load(inifile, 'Vyk'+IntToStr(i));
       Self.Bloky.Add(blok);
     except

     end;
    end;

   // rozpojovace
   count := inifile.ReadInteger('P', 'R', 0);
   for i := 0 to count-1 do
    begin
     try
       blok := TRozp.Create(i);
       blok.Load(inifile, 'R'+IntToStr(i));
       Self.Bloky.Add(blok);
     except

     end;
    end;

   Self.ComputeVyhybkaFlag();
   Self.Escape();
 finally
   inifile.Free;
 end;
end;

procedure TPanelObjects.FSave(aFile:string;const ORs:string);
var i:Integer;
    inifile:TMemIniFile;
    str_list:TStrings;
    blok:TGraphBlok;
    counts:TDictionary<TBlkType, Integer>;
    blkTyp:TBlkType;
      // pocty bloku v tomto poradi: useky, navestidla, vyhybky, prejezdy, popisky, pomocne_objekty, uvazky, uvazky_spr, zamky, vykolejky, rozpojovace
begin
 Self.FStav := 2;

 Self.ComputeVyhybkaFlag();
 ComputeVetve(Self);

 DeleteFile(aFile);

 inifile := TMemIniFile.Create(aFile, TEncoding.UTF8);
 counts := TDictionary<TBlkType, Integer>.Create();
 try
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
   for blok in Self.Bloky do
    begin
     blok.Save(inifile, TGraphBlok.TypeToFileStr(blok.typ)+IntToStr(blok.index ));
     if (not counts.ContainsKey(blok.typ)) then
       counts.Add(blok.typ, 0);
     counts[blok.typ] := counts[blok.typ] + 1;
    end;//for i

   // pocty bloku v tomto poradi: useky, navestidla, vyhybky, prejezdy, popisky, pomocne_objekty, uvazky, uvazky_spr
   for blkTyp in counts.Keys do
     inifile.WriteInteger('P', TGraphBlok.TypeToFileStr(blkTyp), counts[blkTyp]);

 finally
   inifile.UpdateFile();
   inifile.Free();
   counts.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPanelObjects.Paint();
begin
 case (Self.FMode) of
   dmBloky, dmRoots : Self.PaintBloky();
 end;
end;

procedure TPanelObjects.PaintBloky();
var i,j, color:Integer;
    blok:TGraphBlok;
begin
 // pruhlednost
 Self.DrawObject.Canvas.Pen.Mode := pmMerge;

 for blok in Self.Bloky do
  begin
   case (blok.typ) of
    TBlkType.usek:begin
       if (Self.Selected = blok) then
        begin
         for j := 0 to (blok as TUsek).JCClick.Count-1 do
           Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TUsek).JCClick[j].X*_Symbol_Sirka,
                                         (blok as TUsek).JCClick[j].Y*_Symbol_Vyska, _KPopisek_Index);

         for j := 0 to (blok as TUsek).KPopisek.Count-1 do
           Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TUsek).KPopisek[j].X*_Symbol_Sirka,
                                         (blok as TUsek).KPopisek[j].Y*_Symbol_Vyska, _JCPopisek_Index);

         for j := 0 to (blok as TUsek).Soupravy.Count-1 do
           Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TUsek).Soupravy[j].X*_Symbol_Sirka,
                                         (blok as TUsek).Soupravy[j].Y*_Symbol_Vyska, _Soupravy_Index-5);

         for j := 0 to (blok as TUsek).Symbols.Count-1 do
           Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TUsek).Symbols[j].Position.X*_Symbol_Sirka,
                                         (blok as TUsek).Symbols[j].Position.Y*_Symbol_Vyska,((blok as TUsek).Symbols[j].SymbolID*10)+Self.Colors.Selected);

         Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TUsek).Root.X*_Symbol_Sirka, (blok as TUsek).Root.Y*_Symbol_Vyska, (_Root_Index*10) + Self.Colors.Selected);
        end else begin
         for j := 0 to (blok as TUsek).JCClick.Count-1 do
           Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TUsek).JCClick[j].X*_Symbol_Sirka,
                                         (blok as TUsek).JCClick[j].Y*_Symbol_Vyska, _JCPopisek_Index);

         for j := 0 to (blok as TUsek).KPopisek.Count-1 do
           Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TUsek).KPopisek[j].X*_Symbol_Sirka,
                                         (blok as TUsek).KPopisek[j].Y*_Symbol_Vyska, _KPopisek_Index);

         for j := 0 to (blok as TUsek).Soupravy.Count-1 do
           Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TUsek).Soupravy[j].X*_Symbol_Sirka,
                                         (blok as TUsek).Soupravy[j].Y*_Symbol_Vyska, _Soupravy_Index);

         if (Self.Mode = TMode.dmBloky) then
          begin
           case (blok.Blok) of
            -1: color := Self.Colors.Alert;
            -2: color := Self.Colors.IntUnassigned;
           else
             color := Self.Colors.Normal;
           end;
          end else begin
           if (((blok as TUsek).IsVyhybka) and ((blok as TUsek).Root.X = -1)) then
             color := Self.Colors.Alert
           else
             color := Self.Colors.Normal;
          end;

         for j := 0 to (blok as TUsek).Symbols.Count-1 do
           Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TUsek).Symbols[j].Position.X*_Symbol_Sirka, (blok as TUsek).Symbols[j].Position.Y*_Symbol_Vyska,((blok as TUsek).Symbols[j].SymbolID*10)+color);
         Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TUsek).Root.X*_Symbol_Sirka,
                                       (blok as TUsek).Root.Y*_Symbol_Vyska, (_Root_Index*10) + _Root_Color);
        end;//else Selected = i
    end;

    /////////////////////////////////////////////////

    TBlkType.navestidlo:begin
       if (Self.Selected = blok) then
        begin
         Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TNavestidlo).Position.X*_Symbol_Sirka, (blok as TNavestidlo).Position.Y*_Symbol_Vyska, ((_SCom_Start+(blok as TNavestidlo).SymbolID)*10)+Self.Colors.Selected);
        end else begin
         case (blok.Blok) of
           -1: Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TNavestidlo).Position.X*_Symbol_Sirka, (blok as TNavestidlo).Position.Y*_Symbol_Vyska,((_SCom_Start+(blok as TNavestidlo).SymbolID)*10)+Self.Colors.Alert);
           -2: Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TNavestidlo).Position.X*_Symbol_Sirka, (blok as TNavestidlo).Position.Y*_Symbol_Vyska,((_SCom_Start+(blok as TNavestidlo).SymbolID)*10)+Self.Colors.IntUnassigned);
         else
           Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TNavestidlo).Position.X*_Symbol_Sirka, (blok as TNavestidlo).Position.Y*_Symbol_Vyska,((_SCom_Start+(blok as TNavestidlo).SymbolID)*10)+Self.Colors.Normal);
         end;
        end;//else (Self.Selected > 255)
    end;

    /////////////////////////////////////////////////

    TBlkType.vyhybka:begin
       if (Self.Selected = blok) then
        begin
         Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TVyhybka).Position.X*_Symbol_Sirka, (blok as TVyhybka).Position.Y*_Symbol_Vyska, (((blok as TVyhybka).SymbolID)*10)+Self.Colors.Selected);
        end else begin
         case (blok.Blok) of
           -1: Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TVyhybka).Position.X*_Symbol_Sirka, (blok as TVyhybka).Position.Y*_Symbol_Vyska, (((blok as TVyhybka).SymbolID)*10)+Self.Colors.Alert);
           -2: Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TVyhybka).Position.X*_Symbol_Sirka, (blok as TVyhybka).Position.Y*_Symbol_Vyska, (((blok as TVyhybka).SymbolID)*10)+Self.Colors.IntUnassigned);
         else
           Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TVyhybka).Position.X*_Symbol_Sirka, (blok as TVyhybka).Position.Y*_Symbol_Vyska, (((blok as TVyhybka).SymbolID)*10)+Self.Colors.Normal);
         end;
        end;//else (Self.Selected > 255)
    end;

    /////////////////////////////////////////////////

    TBlkType.prejezd:begin
       if (Self.Selected = blok) then
        begin
         color := Self.Colors.Selected;
        end else begin
         case (blok.Blok) of
          -1: color := Self.Colors.Alert;
          -2: color := Self.Colors.IntUnassigned;
         else
           color := Self.Colors.Normal;
         end;
        end;//else (Self.Selected > 255)

       for j := 0 to (blok as TPrejezd).StaticPositions.Count-1 do
           Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TPrejezd).StaticPositions[j].X*_Symbol_Sirka, (blok as TPrejezd).StaticPositions[j].Y*_Symbol_Vyska, _Prj_Index+color);

       for j := 0 to (blok as TPrejezd).BlikPositions.Count-1 do
           Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TPrejezd).BlikPositions[j].Pos.X*_Symbol_Sirka, (blok as TPrejezd).BlikPositions[j].Pos.Y*_Symbol_Vyska, _Prj_Index+color);
    end;

    /////////////////////////////////////////////////

    TBlkType.text:begin
       if (Length((blok as TText).Text) = 1) then
        begin
         if (Self.Selected = blok) then
          begin
           color := Self.Colors.Selected;
          end else begin
           case (blok.Blok) of
            -1: color := Self.Colors.Alert;
            -2: color := Self.Colors.IntUnassigned;
           else
             color := Self.Colors.Normal;
           end;
          end;//else (Self.Selected > 255)
        end else begin
          color := (blok as TText).Color;
        end;

       Self.Graphics.TextOutputI((blok as TText).Position, (blok as TText).Text, color, clBlack);
    end;

    /////////////////////////////////////////////////

    TBlkType.pomocny_obj:begin
       for j := 0 to (blok as TPomocnyObj).Positions.Count-1 do
         Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas,
                                       (blok as TPomocnyObj).Positions[j].X*_Symbol_Sirka,
                                       (blok as TPomocnyObj).Positions[j].Y*_Symbol_Vyska,
                                       ((blok as TPomocnyObj).Symbol*10)+_Bitmap_DrawColors[(blok as TPomocnyObj).Symbol]);
    end;

    /////////////////////////////////////////////////

    TBlkType.uvazka:begin
     if (Self.Selected = blok) then
      begin
       color := Self.Colors.Selected;
      end else begin
       case (blok.Blok) of
        -1: color := Self.Colors.Alert;
        -2: color := Self.Colors.IntUnassigned;
       else
         color := Self.Colors.Normal;
       end;
      end;//else (Self.Selected > 255)

     Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TUvazka).Pos.X*_Symbol_Sirka, (blok as TUvazka).Pos.Y*_Symbol_Vyska, (_Uvazka_Start*10)+color);
     Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, ((blok as TUvazka).Pos.X+1)*_Symbol_Sirka, (blok as TUvazka).Pos.Y*_Symbol_Vyska, ((_Uvazka_Start+1)*10)+color);
    end;

    /////////////////////////////////////////////////

    TBlkType.uvazka_spr:begin
       if (Self.Selected = blok) then
        begin
         Self.DrawObject.Canvas.Pen.Color := clRed;
         color := Self.Colors.Selected;
        end else begin
         case (blok.Blok) of
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

       case ((blok as TUvazkaSpr).vertical_dir) of
        TUvazkaSprVertDir.top:begin
         Self.DrawObject.Canvas.Rectangle(
            (blok as TUvazkaSpr).Pos.X*_Symbol_Sirka,
            (blok as TUvazkaSpr).Pos.Y*_Symbol_Vyska + _Symbol_Vyska - 1,
            ((blok as TUvazkaSpr).Pos.X + _Uvazka_Spr_Sirka)*_Symbol_Sirka - 1,
            ((blok as TUvazkaSpr).Pos.Y - (blok as TUvazkaSpr).spr_cnt + 1)*_Symbol_Vyska);
        end;

        TUvazkaSprVertDir.bottom:begin
         Self.DrawObject.Canvas.Rectangle(
            (blok as TUvazkaSpr).Pos.X*_Symbol_Sirka,
            (blok as TUvazkaSpr).Pos.Y*_Symbol_Vyska,
            ((blok as TUvazkaSpr).Pos.X + _Uvazka_Spr_Sirka)*_Symbol_Sirka - 1,
            ((blok as TUvazkaSpr).Pos.Y + (blok as TUvazkaSpr).spr_cnt)*_Symbol_Vyska - 1);
        end;
       end;//case

       Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TUvazkaSpr).Pos.X*_Symbol_Sirka, (blok as TUvazkaSpr).Pos.Y*_Symbol_Vyska, (_Uvazka_Spr_Index*10)+color);
    end;

    /////////////////////////////////////////////////

    TBlkType.zamek:begin
     if (Self.Selected = blok) then
      begin
       color := Self.Colors.Selected;
      end else begin
       case (blok.Blok) of
        -1: color := Self.Colors.Alert;
        -2: color := Self.Colors.IntUnassigned;
       else
         color := Self.Colors.Normal;
       end;
      end;//else (Self.Selected > 255)

     Self.DrawObject.Canvas.Brush.Color := clBlack;
     Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TZamek).Pos.X*_Symbol_Sirka, (blok as TZamek).Pos.Y*_Symbol_Vyska, (_Zamek*10)+color);
    end;

    /////////////////////////////////////////////////

    TBlkType.vykol:begin
     if (Self.Selected = blok) then
      begin
       color := Self.Colors.Selected;
      end else begin
       case (blok.Blok) of
        -1: color := Self.Colors.Alert;
        -2: color := Self.Colors.IntUnassigned;
       else
         color := Self.Colors.Normal;
       end;
      end;//else (Self.Selected > 255)

     Self.DrawObject.Canvas.Brush.Color := clBlack;
     Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TVykol).Pos.X*_Symbol_Sirka, (blok as TVykol).Pos.Y*_Symbol_Vyska, ((_Vykol_Start+(blok as TVykol).symbol)*10)+color);
    end;

    /////////////////////////////////////////////////

    TBlkType.rozp:begin
     if (Self.Selected = blok) then
      begin
       color := Self.Colors.Selected;
      end else begin
       case (blok.Blok) of
        -1: color := Self.Colors.Alert;
        -2: color := Self.Colors.IntUnassigned;
       else
         color := Self.Colors.Normal;
       end;
      end;//else (Self.Selected > 255)

     Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas, (blok as TRozp).Pos.X*_Symbol_Sirka, (blok as TRozp).Pos.Y*_Symbol_Vyska, ((_Rozp_Start+1)*10)+color);
    end;

    /////////////////////////////////////////////////

   end;//case typ
  end;//for i
end;

////////////////////////////////////////////////////////////////////////////////

//Escape
procedure TPanelObjects.Escape();
begin
 Self.Selected := nil;
end;

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
end;

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

    TBlkType.text:begin
     if ((Pos.Y = (Self.Bloky[i] as TText).Position.Y) and (Pos.X >= (Self.Bloky[i] as TText).Position.X)
      and (Pos.X < (Self.Bloky[i] as TText).Position.X+Length((Self.Bloky[i] as TText).Text)) and (Length((Self.Bloky[i] as TText).Text) = 1)) then
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
end;

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

procedure TPanelObjects.MouseUp(Position:TPoint;Button:TMouseButton);
begin
 case (Self.FMode) of
  dmBloky : Self.BlokyMouseUp(Position, Button);
  dmRoots : Self.KorenyMouseUp(Position, Button);
 end;
end;

procedure TPanelObjects.BlokyMouseUp(Position:TPoint;Button:TMouseButton);
var blk:Integer;
begin
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
    TBlkType.usek         : Self.FOnMsg(Self, 'Blok '+IntToStr(blk)+ ' (�sek '+IntToStr(Self.Selected.index)+')');
    TBlkType.navestidlo   : Self.FOnMsg(Self, 'Blok '+IntToStr(blk)+ ' (n�v�stidlo '+IntToStr(Self.Selected.index)+')');
    TBlkType.vyhybka      : Self.FOnMsg(Self, 'Blok '+IntToStr(blk)+ ' (v�hybka '+IntToStr(Self.Selected.index)+'), p�i�azen �seku '+IntToStr((Self.Selected as TVyhybka).obj));
    TBlkType.prejezd      : Self.FOnMsg(Self, 'Blok '+IntToStr(blk)+ ' (p�ejezd '+IntToStr(Self.Selected.index)+')');
    TBlkType.text         : Self.FOnMsg(Self, 'Blok '+IntToStr(blk)+ ' (popisek '+IntToStr(Self.Selected.index)+')');
    TBlkType.pomocny_obj  : Self.FOnMsg(Self, 'Blok '+IntToStr(blk)+ ' (pomocn� objekt '+IntToStr(Self.Selected.index)+')');
    TBlkType.uvazka       : Self.FOnMsg(Self, 'Blok '+IntToStr(blk)+ ' (�vazka '+IntToStr(Self.Selected.index)+')');
    TBlkType.uvazka_spr   : Self.FOnMsg(Self, 'Blok '+IntToStr(blk)+ ' (�vazka spr. '+IntToStr(Self.Selected.index)+')');
    TBlkType.zamek        : Self.FOnMsg(Self, 'Blok '+IntToStr(blk)+ ' (z�mek '+IntToStr(Self.Selected.index)+')');
    TBlkType.vykol        : Self.FOnMsg(Self, 'Blok '+IntToStr(blk)+ ' (v�kolejka '+IntToStr(Self.Selected.index)+'), p�i�azena �seku '+IntToStr((Self.Selected as TVykol).obj));
    TBlkType.rozp         : Self.FOnMsg(Self, 'Blok '+IntToStr(blk)+ ' (rozpojova� '+IntToStr(Self.Selected.index)+')');
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
end;

procedure TPanelObjects.KorenyMouseUp(Position:TPoint;Button:TMouseButton);
var blk, i:Integer;
begin
 //leve tlacitko mysi
 if (Button <> mbLeft) then Exit;

 blk := Self.GetObject(Position);
 if (blk < 0) then
  Self.Selected := nil
 else if (Self.Bloky[blk].typ = TBlkType.usek) then
  Self.Selected := Self.Bloky[blk]
 else if (Self.Bloky[blk].typ = TBlkType.vyhybka) then begin
  for i := 0 to Self.Bloky.Count-1 do
    if ((Self.Bloky[i].typ = TBlkType.usek) and (Self.Bloky[i].index = (Self.Bloky[blk] as TVyhybka).obj)) then
     begin
      Self.Selected := Self.Bloky[i];
      break;
     end;
 end else
  Self.Selected := nil;

 if (Assigned(Self.FOnMsg)) then Self.FOnMsg(Self, 'Blok '+IntToStr(blk)+ ' (�sek)');
 if ((not Assigned(Self.Selected)) or (not (Self.Selected as TUsek).IsVyhybka)) then Exit;
 (Self.Selected as TUsek).Root := Position;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPanelObjects.DblClick(Position:TPoint);
begin
 Self.PopUpPos := Position;
 Self.PMPropertiesClick(self);
end;

procedure TPanelObjects.CreatePM(var PM:TPopUpMenu;Parent:TDXDraw);
var MI:TMenuItem;
begin
 PM := TPopupMenu.Create(Parent);

 MI := TMenuItem.Create(PM);
 MI.Caption := 'Vlastnosti';
 MI.OnClick := Self.PMPropertiesClick;

 PM.Items.Add(MI);
end;

procedure TPanelObjects.PMPropertiesClick(Sender: TObject);
begin
 if (Self.Selected <> nil) then
   if (Assigned(Self.FOnBlokEdit)) then Self.FOnBlokEdit(Self, Self.Selected);
end;

////////////////////////////////////////////////////////////////////////////////

// tato funkce prirazuje aktivnimu bloku obalst rizeni v paramtru
//  to je uzitecne k tomu, ze pri vybrani bloku a nesdlednemu kliku na baracek dojde k prirazeni teto oblasti rizeni
function TPanelObjects.SetOR(OblR:Integer):Byte;
begin
 if (Self.Selected = nil) then Exit(1);
 Self.Selected.OblRizeni := OblR;
 Result := 0;
end;

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
   if ((Self.Bloky[i].Blok = -1) and (Self.Bloky[i].typ <> TBlkType.pomocny_obj) and
       ((Self.Bloky[i].typ <> TBlkType.text) or (Length((Self.Bloky[i] as TText).Text) = 1))) then
    begin
     Result.Add('ERR: blok '+IntToStr(i)+': neni navaznost na technologicky blok');
     error_cnt := error_cnt + 1;
    end;
   if ((Self.Bloky[i].OblRizeni < 0) and
       ((Self.Bloky[i].typ <> TBlkType.pomocny_obj) xor ((Self.Bloky[i].typ = TBlkType.text) and
       (Length((Self.Bloky[i] as TText).Text) > 1)))) then
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
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPanelObjects.SetMode(mode:TMode);
begin
 Self.FMode := mode;
 Self.Selected := nil;
 if (mode = TMode.dmRoots) then
   Self.ComputeVyhybkaFlag();
end;

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
end;

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

end;

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
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
