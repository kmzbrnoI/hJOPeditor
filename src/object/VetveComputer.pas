unit VetveComputer;

{
  Tato unit implementuje algoritmus pro vypocet vetvi useku.
}

interface

uses symbolHelper, Types, Generics.Collections, ReliefObjects, vetev, SysUtils;

const
  // vedjesi symboly vyhybek ulozeny ve formatu: rovna_x,rovna_y,odbocka_x,odbocka_y,zpet_x,zpet_y ...
  _Vyh_Navaznost: array [0 .. 23] of ShortInt = (1, 0, 0, -1, -1, 0, 1, 0, 0, 1, -1, 0, -1, 0, 0, -1, 1, 0, -1, 0,
    0, 1, 1, 0);
  // _Vyh_Navaznost : array [0..15] of ShortInt = (1,0,0,-1, 1,0,0,1, -1,0,0,-1, -1,0,0,1);

type
  TVetevReturner = record
    symbols: TList<TReliefSym>;
    next: TPoint;
  end;

  TVetveData = array of array of Integer;

procedure ComputeVetve(po: TPanelObjects);
procedure ComputeNormalBlokVetve(po: TPanelObjects; var data: TVetveData; start: TPoint; Vetve: TList<TVetev>;
  future_offset: Integer = 0);
procedure GetUsekTillVyhybka(var data: TVetveData; start: TPoint; initDir: TNavDir; var res: TVetevReturner);
procedure ComputeDKSBlokVetve(po: TPanelObjects; var data: TVetveData; start: TPoint; Vetve: TList<TVetev>);
function IsSecondCross(var data: TVetveData; start: TPoint): Boolean;
function SecondCrossPos(var data: TVetveData; start: TPoint): TPoint;
procedure OffsetVetve(Vetve: TList<TVetev>; offset: Integer);

implementation

uses ObjBlok, ObjBlokUsek, ObjBlokVyhybka, ObjBlokVykol;

/// /////////////////////////////////////////////////////////////////////////////

{
  Pocitani vetvi vsech useku.
  Projde vsechy useky a pro kazdy spusti prislusou funkci, ktera spocita vetve.
}
procedure ComputeVetve(po: TPanelObjects);
var i, row, col, j: Integer;
  data: TVetveData; // = Array of Array of Integer; toto pole je pro algoritmus nize zdrojem vsech dat
  // jedna s o dvourozmerne pole reprezentujici cely panel, na kazdem poli je cislo bitmapoveho
  // metoda pro zpracovani jednoho bloku dostane na vstup vzdy toto pole s tim, ze jsou v nem jen body, ktere patri prislusnemu bloku
  // vsude jinde je (-1)
begin
  // inicializace pole data
  SetLength(data, po.PanelWidth, po.PanelHeight);
  for col := 0 to po.PanelWidth - 1 do
    for row := 0 to po.PanelHeight - 1 do
      data[col, row] := -1;

  for i := 0 to po.Bloky.Count - 1 do
  begin
    if (po.Bloky[i].typ <> TBlkType.usek) then
      continue;

    (po.Bloky[i] as TUsek).Vetve.Clear();
    if (not(po.Bloky[i] as TUsek).IsVyhybka) then
      continue;

    if ((po.Bloky[i] as TUsek).Root.X < 0) then
      continue;

    // inicializujeme pole symboly bloku
    for j := 0 to (po.Bloky[i] as TUsek).symbols.Count - 1 do
      data[(po.Bloky[i] as TUsek).symbols[j].Position.X, (po.Bloky[i] as TUsek).symbols[j].Position.Y] :=
        (po.Bloky[i] as TUsek).symbols[j].SymbolID;

    // do tohoto pole take musime ulozit vyhybky a vykolejky
    for j := 0 to po.Bloky.Count - 1 do
    begin
      if ((po.Bloky[j].typ = TBlkType.vyhybka) and ((po.Bloky[j] as TVyhybka).obj = po.Bloky[i].index)) then
        data[(po.Bloky[j] as TVyhybka).Position.X, (po.Bloky[j] as TVyhybka).Position.Y] :=
          (po.Bloky[j] as TVyhybka).SymbolID;
      if ((po.Bloky[j].typ = TBlkType.vykol) and ((po.Bloky[j] as TVykol).obj = po.Bloky[i].index)) then
        data[(po.Bloky[j] as TVykol).Pos.X, (po.Bloky[j] as TVykol).Pos.Y] := (po.Bloky[j] as TVykol).symbol +
          _Vykol_Start;
    end;

    if (data[TUsek(po.Bloky[i]).Root.X, TUsek(po.Bloky[i]).Root.Y] = _DKS_Top) then
      (po.Bloky[i] as TUsek).DKStype := dksTop
    else if (data[TUsek(po.Bloky[i]).Root.X, TUsek(po.Bloky[i]).Root.Y] = _DKS_Bot) then
      (po.Bloky[i] as TUsek).DKStype := dksBottom
    else
      (po.Bloky[i] as TUsek).DKStype := dksNone;

    // provedeme algoritmus
    if ((po.Bloky[i] as TUsek).DKStype <> dksNone) then
    begin
      ComputeDKSBlokVetve(po, data, (po.Bloky[i] as TUsek).Root, (po.Bloky[i] as TUsek).Vetve);
      if (TUsek(po.Bloky[i]).Vetve.Count < 3) then
      begin
        (po.Bloky[i] as TUsek).DKStype := dksNone;
        ComputeNormalBlokVetve(po, data, (po.Bloky[i] as TUsek).Root, (po.Bloky[i] as TUsek).Vetve);
      end;
    end
    else
      ComputeNormalBlokVetve(po, data, (po.Bloky[i] as TUsek).Root, (po.Bloky[i] as TUsek).Vetve);

    // nastavime pole data opet na -1 (algortimus by to mel udelat sam, ale pro jistotu)
    for j := 0 to (po.Bloky[i] as TUsek).symbols.Count - 1 do
      data[(po.Bloky[i] as TUsek).symbols[j].Position.X, (po.Bloky[i] as TUsek).symbols[j].Position.Y] := -1;

    // odstranit vyhybky
    for j := 0 to po.Bloky.Count - 1 do
    begin
      if ((po.Bloky[i].typ <> TBlkType.vyhybka) or ((po.Bloky[i] as TVyhybka).obj <> i)) then
        continue;
      data[(po.Bloky[i] as TVyhybka).Position.X, (po.Bloky[i] as TVyhybka).Position.Y] := -1;
    end;
  end; // for i
end;

/// /////////////////////////////////////////////////////////////////////////////

{
  Pocitani vetvi jednoho bloku.
  Tato funkce provadi prohledavani do sirky.
  Ve fronte je vzdy ulozena startovni pozice, jedna iterace ji expanduje az do
  vyhybky (nebo prazdneho pole).
  Pokud algortimus narazi na vyhybky, do fronty prida 2 deti reprezentujici 2
  cesty, kam jde kolej. V tomto momente lze ziskat index deti v poli vetve jako
  Vetve.Count + Queue.Count (2. vetev +1) z predpokladu zachovani poradi:
  1) kolej do rovna
  2) kolej do odbocky
  Pozor: deti pridavame jen v pripade, ze existuji (muze se taky stat, ze za
  vyhybkou nic neni)!
}
procedure ComputeNormalBlokVetve(po: TPanelObjects; var data: TVetveData; start: TPoint; Vetve: TList<TVetev>;
  future_offset: Integer);
var queue: TQueue<TPoint>;
  first, temp, new: TPoint;
  i, j, up: Integer;
  vetev: TVetev;
  symbols: TList<TReliefSym>;
  symbol: TReliefSym;
  node: ^TVetevEnd;
  startOnVyh: Boolean;
begin
  queue := TQueue<TPoint>.Create();
  queue.Enqueue(start);

  startOnVyh := ((data[start.X, start.Y] >= _Vyhybka_Start) and (data[start.X, start.Y] <= _Vyhybka_End));
  symbols := TList<TReliefSym>.Create();

  try
    while (queue.Count > 0) do
    begin
      // expanduji jeden vrchol: pozor:  musim expandovat doleva i doprava

      // resetuji vetev:
      vetev := DefaultVetev();

      // vezmu aktualni vrchol z fronty
      first := queue.Dequeue();

      // dodelat kontrolu toho, jestli nahodou nejsem na vyhybce

      // do pole symbols si ulozim useku aktualni vetve a pak je pouze predelam do dynamickeho pole vetev.symbols
      symbols.Clear();

      // expanduji oba smery z prvniho symbolu, dalsi symboly maji vzdy jen jeden smer k expanzi
      for j := 0 to 1 do
      begin
        new := first;
        // projizdim usek jednim smerem pro jedno 'j'
        while (((data[new.X, new.Y] >= _Usek_Start) and (data[new.X, new.Y] <= _Usek_End)) or
          ((data[new.X, new.Y] >= _Vykol_Start) and (data[new.X, new.Y] <= _Vykol_End)) or
          ((data[new.X, new.Y] >= _Krizeni_Start) and (data[new.X, new.Y] <= _Krizeni_End))) do
        begin
          // pridam symbol do seznamu symbolu
          if ((((new.X <> first.X) or (new.Y <> first.Y)) or (j = 1)) and ((data[new.X, new.Y] < _Vykol_Start) or
            (data[new.X, new.Y] > _Vykol_End))) then
          begin
            symbol.Position := new;
            symbol.SymbolID := data[new.X, new.Y];
            symbols.Add(symbol);
          end;

          temp := new;

          // vypocitam prvni vedlejsi pole
          if ((data[new.X, new.Y] >= _Vykol_Start) and (data[new.X, new.Y] <= _Vykol_End)) then
          begin
            (po.Bloky[po.GetObject(new)] as TVykol).vetev := Vetve.Count + future_offset;
            data[new.X, new.Y] := _Usek_Start;
          end;

          // podivame se na prvni ze dvou vedlejsich policek
          temp.X := new.X + GetUsekNavaznost(data[new.X, new.Y], ndPositive).X;
          temp.Y := new.Y + GetUsekNavaznost(data[new.X, new.Y], ndPositive).Y;

          // je na vedlejsim poli symbol?
          if (data[temp.X, temp.Y] > -1) and ((temp.X <> first.X) or (temp.Y <> first.Y)) then
          begin
            // ano, na prvnim vedlejsim poli je symbol -> resetuji aktualni pole a do noveho pole priradim pole vedlejsi
          end else begin
            // ne, na vedlejsim poli neni symbol -> toto policko jsme asi uz prosli, nebo tam proste nic neni
            // zkusime druhe vedlejsi policko

            temp.X := new.X + GetUsekNavaznost(data[new.X, new.Y], ndNegative).X;
            temp.Y := new.Y + GetUsekNavaznost(data[new.X, new.Y], ndNegative).Y;
          end;

          if ((data[temp.X, temp.Y] = _Zarazedlo_l) or (data[temp.X, temp.Y] = _Zarazedlo_r)) then
          begin
            symbol.Position := temp;
            symbol.SymbolID := data[temp.X, temp.Y];
            symbols.Add(symbol);
          end;

          // pokud je na druhem vedlejsim poli symbol, expanduji ho; pokud tam neni, while cyklus prochazeni utne
          if (((new.X <> first.X) or (new.Y <> first.Y)) or (j = 1)) then
            data[new.X, new.Y] := -1;

          new := temp;
        end; // while

        // skoncilo prochazeni jednoho smeru aktualni vetve
        if (data[new.X, new.Y] >= _Vyhybka_Start) and (data[new.X, new.Y] <= _Vyhybka_End) then
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
          node^.vyh := po.Bloky[po.GetObject(new)].index;
          node^.ref_plus := -1;
          node^.ref_minus := -1;

          // projdu 2 smery, do kterych muze vyhybka (je jasne, ze barvici cesta
          // musela prijit z te strany, kde se koleje spojuji)/
          // Pokud se zacinalo na vyhybce, projde se i 3. smer.
          if (startOnVyh) then
          begin
            startOnVyh := false;
            up := 2;
          end
          else
            up := 1;

          for i := 0 to up do
          begin
            temp.X := new.X + _Vyh_Navaznost[((data[new.X, new.Y] - _Vyhybka_Start) * 6) + (i * 2)];
            temp.Y := new.Y + _Vyh_Navaznost[((data[new.X, new.Y] - _Vyhybka_Start) * 6) + (i * 2) + 1];

            if (data[temp.X, temp.Y] > -1) then
            begin
              // na dalsi pozici za vyhybkou je symbol -> novy zaznam do fronty
              if (i <> 2) then
                queue.Enqueue(temp);

              case (i) of
                0:
                  node^.ref_plus := Vetve.Count + queue.Count;
                1:
                  node^.ref_minus := Vetve.Count + queue.Count;
                2:
                  first := temp;
              end; // case
            end;
          end; // for 2_smery_do_kterych_muze_expandovat_vyhybka
          data[new.X, new.Y] := -1; // vyhybku jsem zpracoval
        end; // if vetev_skoncila_vyhybkou
      end; // for j

      // prochazeni vetve je kompletni vcetne pripadnych koncovych vyhybek -> priradim do vetve symboly a vytvorim vetev
      SetLength(vetev.symbols, symbols.Count);
      for i := 0 to symbols.Count - 1 do
        vetev.symbols[i] := symbols[i];
      Vetve.Add(vetev);
    end; // while
  finally
    symbols.Free();
    queue.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

{
  Vyrabi vetve poloviny DKS.
  Predpoklada se, ze root je nastaven na kriz DKS.
}
procedure ComputeDKSBlokVetve(po: TPanelObjects; var data: TVetveData; start: TPoint; Vetve: TList<TVetev>);
var vetev: TVetev;
  r: TVetevReturner;
  i: Integer;
  vlPos, vrPos, Pos: TPoint;
  symbol, addI: Integer;
  tempVetve: TList<TVetev>;
begin
  r.symbols := TList<TReliefSym>.Create();
  tempVetve := TList<TVetev>.Create();
  vlPos := Point(-1, -1);
  vrPos := Point(-1, -1);

  try
    // 1) nejprve hledame usek k nejblizsi leve vyhybce
    GetUsekTillVyhybka(data, Point(start.X - 1, start.Y), ndPositive, r);
    if (r.next.X < 0) then
      Exit();

    vetev := DefaultVetev();
    SetLength(vetev.symbols, r.symbols.Count);
    for i := 0 to r.symbols.Count - 1 do
      vetev.symbols[i] := r.symbols[i];

    if ((data[r.next.X, r.next.Y] >= _Vyhybka_Start) and (data[r.next.X, r.next.Y] <= _Vyhybka_End)) then
    begin
      vetev.node1.vyh := po.Bloky[po.GetObject(r.next)].index;
      vlPos := r.next;
    end;

    Vetve.Add(vetev);

    // 2) hledame usek k nejblizsi prave vyhybce
    r.symbols.Clear();
    GetUsekTillVyhybka(data, Point(start.X + 1, start.Y), ndNegative, r);
    if (r.next.X < 0) then
    begin
      Vetve.Clear();
      Exit();
    end;

    vetev := DefaultVetev();
    SetLength(vetev.symbols, r.symbols.Count);
    for i := 0 to r.symbols.Count - 1 do
      vetev.symbols[i] := r.symbols[i];

    if ((data[r.next.X, r.next.Y] >= _Vyhybka_Start) and (data[r.next.X, r.next.Y] <= _Vyhybka_End)) then
    begin
      vetev.node1.vyh := po.Bloky[po.GetObject(r.next)].index;
      vrPos := r.next;
    end;

    Vetve.Add(vetev);

    // dal nema smysl pokracovat, pokud nejsou obe vyhybky nalezeny
    if ((vlPos.X < 0) or (vrPos.X < 0)) then
    begin
      Vetve.Clear(); // neni DKS -> vykresli se vse pomoci symbolu
      Exit();
    end;

    // 3) hledame spojovaci usek obou vyhybek
    // musime expandovat v obbou smerech
    r.symbols.Clear();
    symbol := data[vlPos.X + 1, vlPos.Y];
    GetUsekTillVyhybka(data, Point(vlPos.X + 1, vlPos.Y), ndPositive, r);

    data[vlPos.X + 1, vlPos.Y] := symbol;
    GetUsekTillVyhybka(data, Point(vlPos.X + 1, vlPos.Y), ndNegative, r);

    // symbol na pozici [vlPos.X+1, vlPos.Y] dvakrat -> vymazat
    r.symbols.Delete(0);

    vetev := DefaultVetev();
    SetLength(vetev.symbols, r.symbols.Count);
    for i := 0 to r.symbols.Count - 1 do
      vetev.symbols[i] := r.symbols[i];
    Vetve.Add(vetev);

    // 4) odstranit vyhybky
    data[vlPos.X, vlPos.Y] := -1;
    data[vrPos.X, vrPos.Y] := -1;

    // 4) hledame navazujici levou vetev
    tempVetve.Clear();
    ComputeNormalBlokVetve(po, data, Point(vlPos.X - 1, vlPos.Y), tempVetve, 4);
    OffsetVetve(tempVetve, 4);

    Vetve.Add(tempVetve[0]);
    Vetve.Add(tempVetve[0]); // docasne i prava vetev
    tempVetve.Delete(0);
    Vetve.AddRange(tempVetve);

    // 5) hledame navazujici pravou vetev
    tempVetve.Clear();
    ComputeNormalBlokVetve(po, data, Point(vrPos.X + 1, vrPos.Y), tempVetve, Vetve.Count - 1);
    OffsetVetve(tempVetve, Vetve.Count - 1);
    Vetve[4] := tempVetve[0];
    tempVetve.Delete(0);
    Vetve.AddRange(tempVetve);

    // 6) kontrolujeme krizeni "obe casti DKS v jednom bloku"
    if (IsSecondCross(data, start)) then
    begin
      Pos := SecondCrossPos(data, start);
      data[Pos.X, Pos.Y] := -1;

      addI := Vetve.Count;
      ComputeNormalBlokVetve(po, data, Point(Pos.X - 1, Pos.Y), Vetve);
      if (addI <> Vetve.Count) then
      begin
        vetev := Vetve[1];
        vetev.node1.ref_minus := addI;
        Vetve[1] := vetev;
      end;

      addI := Vetve.Count;
      ComputeNormalBlokVetve(po, data, Point(Pos.X + 1, Pos.Y), Vetve);
      if (addI <> Vetve.Count) then
      begin
        vetev := Vetve[0];
        vetev.node1.ref_minus := addI;
        Vetve[0] := vetev;
      end;
    end;

  finally
    r.symbols.Free();
    tempVetve.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

{
  Vraci serializovanou jednu cast koleje po nejblizsi neussekovy blok
  Pozice bloku, na kterem se zastavila expanze, je take vracena v Result.net
  (na tomto bloku uz neni symbol useku).
  Pozor: tato funkce neni pouzitelna obecne (nepocita treba s vykolejkami).
  Je uzitecna predevsim pro potreby DKS.
}
procedure GetUsekTillVyhybka(var data: TVetveData; start: TPoint; initDir: TNavDir; var res: TVetevReturner);
var temp, new: TPoint;
  symbol: TReliefSym;
  first: Boolean;
begin
  new := start;
  first := true;
  res.next := Point(-1, -1);

  while ((data[new.X, new.Y] >= _Usek_Start) and (data[new.X, new.Y] <= _Usek_End)) do
  begin
    // pridam symbol do seznamu symbolu
    symbol.Position := new;
    symbol.SymbolID := data[new.X, new.Y];
    res.symbols.Add(symbol);

    if (first) then
    begin
      // podivame se na vedlejsi pole ve spravnem smeru
      temp.X := new.X + GetUsekNavaznost(data[new.X, new.Y], initDir).X;
      temp.Y := new.Y + GetUsekNavaznost(data[new.X, new.Y], initDir).Y;
    end else begin
      // podivame se na vedlejsi pole
      temp.X := new.X + GetUsekNavaznost(data[new.X, new.Y], ndPositive).X;
      temp.Y := new.Y + GetUsekNavaznost(data[new.X, new.Y], ndPositive).Y;

      // je na vedlejsim poli symbol?
      if (data[temp.X, temp.Y] = -1) then
      begin
        // ne, na vedlejsim poli neni symbol -> zkusime poliko v druhem smeru
        temp.X := new.X + GetUsekNavaznost(data[new.X, new.Y], ndNegative).X;
        temp.Y := new.Y + GetUsekNavaznost(data[new.X, new.Y], ndNegative).Y;
      end;
    end;

    // Pokud je na druhem vedlejsim poli symbol, expanduji ho; pokud tam neni,
    // while cyklus prochazeni utne.
    data[new.X, new.Y] := -1;
    new := temp;
    first := false;
    res.next := new;
  end; // while
end;

/// /////////////////////////////////////////////////////////////////////////////

function IsSecondCross(var data: TVetveData; start: TPoint): Boolean;
begin
  Result := false;

  if ((data[start.X, start.Y] = _Krizeni_Start) and (data[start.X, start.Y + 1] = _Krizeni_End)) then
    Result := true;
  if ((data[start.X, start.Y] = _Krizeni_End) and (data[start.X, start.Y - 1] = _Krizeni_Start)) then
    Result := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

function SecondCrossPos(var data: TVetveData; start: TPoint): TPoint;
begin
  if ((data[start.X, start.Y] = _Krizeni_Start) and (data[start.X, start.Y + 1] = _Krizeni_End)) then
    Result := Point(start.X, start.Y + 1)
  else if ((data[start.X, start.Y] = _Krizeni_End) and (data[start.X, start.Y - 1] = _Krizeni_Start)) then
    Result := Point(start.X, start.Y - 1)
  else
    raise Exception.Create('No second cross!');
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure OffsetVetve(Vetve: TList<TVetev>; offset: Integer);
var vetev: TVetev;
  i: Integer;
begin
  for i := 0 to Vetve.Count - 1 do
  begin
    vetev := Vetve[i];

    if (vetev.node1.ref_plus > -1) then
      vetev.node1.ref_plus := vetev.node1.ref_plus + offset;
    if (vetev.node1.ref_minus > -1) then
      vetev.node1.ref_minus := vetev.node1.ref_minus + offset;

    if (vetev.node2.ref_plus > -1) then
      vetev.node2.ref_plus := vetev.node2.ref_plus + offset;
    if (vetev.node2.ref_minus > -1) then
      vetev.node2.ref_minus := vetev.node2.ref_minus + offset;

    Vetve[i] := vetev;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
