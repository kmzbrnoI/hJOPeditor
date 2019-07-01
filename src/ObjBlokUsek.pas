unit ObjBlokUsek;

interface

uses ObjBLok, Generics.Collections, Types, symbolHelper, vetev, IniFiles;

type

TDKSType = (dksNone = 0, dksTop = 1, dksBottom = 2);

TUsek = class(TGraphBlok)
 Root:TPoint;
 IsVyhybka:boolean;         //pomocny flag pro vykreslovani v modu korenu
 Symbols:TList<TReliefSym>; // pokud je v useku vykolejka, je zde ulozena jako klasicky symbol
 JCClick:TList<TPoint>;
 KPopisek:TList<TPoint>;
 Soupravy:TList<TPoint>;
 KpopisekStr:string;
 DKStype:TDKSType;

 Vetve:TList<TVetev>;               // vetve useku
   //vetev 0 je vzdy koren
   //zde je ulozen binarni strom v pseudo-forme
     //na 0. indexu je koren, kazdy vrchol pak obsahuje referenci na jeho deti


 // program si duplikuje ulozena data - po rozdeleni useku na vetve uklada usek jak nerozdeleny tak rozdeleny

 procedure Save(ini:TMemIniFile; key:string);
end;

implementation

procedure TUsek.Save(ini: TMemIniFile; key: string);
begin
 inifile.WriteInteger('U'+IntToStr(blok.Index), 'B',  blok.Blok);
 inifile.WriteInteger('U'+IntToStr(blok.Index), 'OR', blok.OblRizeni);

 //root
 if ((blok as TUsek).IsVyhybka) then
   inifile.WriteString('U'+IntToStr(blok.Index), 'R', GetPos((blok as TUsek).Root));

 if ((blok as TUsek).DKStype <> dksNone) then
   inifile.WriteInteger('U'+IntToStr(blok.Index), 'DKS', Integer((blok as TUsek).DKStype));

 //objekty
 obj := '';
 for j := 0 to (blok as TUsek).Symbols.Count-1 do
   obj := obj + Format('%.3d%.3d%.2d',[(blok as TUsek).Symbols[j].Position.X, (blok as TUsek).Symbols[j].Position.Y, (blok as TUsek).Symbols[j].SymbolID]);
 if (obj <> '') then
   inifile.WriteString('U'+IntToStr(blok.Index),'S',obj);

 //JCClick
 obj := '';
 for j := 0 to (blok as TUsek).JCClick.Count-1 do
   obj := obj + Format('%.3d%.3d',[(blok as TUsek).JCClick[j].X, (blok as TUsek).JCClick[j].Y]);
 if (obj <> '') then
   inifile.WriteString('U'+IntToStr(blok.Index),'C',obj);

 //KPopisek
 obj := '';
 for j := 0 to (blok as TUsek).KPopisek.Count-1 do
   obj := obj + Format('%.3d%.3d',[(blok as TUsek).KPopisek[j].X, (blok as TUsek).KPopisek[j].Y]);
 if (obj <> '') then
   inifile.WriteString('U'+IntToStr(blok.Index),'P',obj);

 //soupravy
 obj := '';
 for j := 0 to (blok as TUsek).Soupravy.Count-1 do
   obj := obj + Format('%.3d%.3d',[(blok as TUsek).Soupravy[j].X, (blok as TUsek).Soupravy[j].Y]);
 if (obj <> '') then
   inifile.WriteString('U'+IntToStr(blok.Index),'Spr',obj);

 //Nazev
 if ((blok as TUsek).KpopisekStr <> '') then
   inifile.WriteString('U'+IntToStr(blok.Index),'N',(blok as TUsek).KpopisekStr);

 //vetve
 if ((blok as TUsek).Vetve.Count > 0) then
   inifile.WriteInteger('U'+IntToStr(blok.Index), 'VC', (blok as TUsek).Vetve.Count);

 for j := 0 to (blok as TUsek).Vetve.Count-1 do
  begin
   if ((blok as TUsek).Vetve[j].node1.vyh < 0) then
     Obj := Format('%.2d',[(blok as TUsek).Vetve[j].node1.vyh])
    else
     Obj := Format('%.3d',[(blok as TUsek).Vetve[j].node1.vyh]);

   if ((blok as TUsek).Vetve[j].node1.ref_plus < 0) then
     Obj := Obj + Format('%.1d',[(blok as TUsek).Vetve[j].node1.ref_plus])
    else
     Obj := Obj + Format('%.2d',[(blok as TUsek).Vetve[j].node1.ref_plus]);

   if ((blok as TUsek).Vetve[j].node1.ref_minus < 0) then
     Obj := Obj + Format('%.1d',[(blok as TUsek).Vetve[j].node1.ref_minus])
    else
     Obj := Obj + Format('%.2d',[(blok as TUsek).Vetve[j].node1.ref_minus]);

   ////////

   if ((blok as TUsek).Vetve[j].node2.vyh < 0) then
     Obj := Obj + Format('%.2d',[(blok as TUsek).Vetve[j].node2.vyh])
    else
     Obj := Obj + Format('%.3d',[(blok as TUsek).Vetve[j].node2.vyh]);

   if ((blok as TUsek).Vetve[j].node2.ref_plus < 0) then
     Obj := Obj + Format('%.1d',[(blok as TUsek).Vetve[j].node2.ref_plus])
    else
     Obj := Obj + Format('%.2d',[(blok as TUsek).Vetve[j].node2.ref_plus]);

   if ((blok as TUsek).Vetve[j].node2.ref_minus < 0) then
     Obj := Obj + Format('%.1d',[(blok as TUsek).Vetve[j].node2.ref_minus])
    else
     Obj := Obj + Format('%.2d',[(blok as TUsek).Vetve[j].node2.ref_minus]);

   ////////

   for k := 0 to Length((blok as TUsek).Vetve[j].Symbols)-1 do
     Obj := Obj + Format('%.3d%.3d%.3d',[(blok as TUsek).Vetve[j].Symbols[k].Position.X, (blok as TUsek).Vetve[j].Symbols[k].Position.Y, (blok as TUsek).Vetve[j].Symbols[k].SymbolID]);
   inifile.WriteString('U'+IntToStr(blok.Index), 'V'+IntToStr(j), Obj);
  end;//for j
end;

end.
