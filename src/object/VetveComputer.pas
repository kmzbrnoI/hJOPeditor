unit VetveComputer;

{
  Tato unit implementuje algoritmus pro vypocet vetvi useku.
}

interface

uses symbolHelper, Types, Generics.Collections, ReliefObjects, vetev, SysUtils;

type
  TBranchReturner = record
    symbols: TList<TReliefSym>;
    next: TPoint;
  end;

  TBranchData = array of array of Integer;

procedure ComputeBranches(po: TPanelObjects);
procedure ComputeNormalBlockBranches(po: TPanelObjects; var data: TBranchData; start: TPoint; branches: TList<TTrackBranch>;
  future_offset: Integer = 0);
procedure GetTrackTillCrossing(var data: TBranchData; start: TPoint; initDir: TNavDir; var res: TBranchReturner);
procedure ComputeDKSBlockBranches(po: TPanelObjects; var data: TBranchData; start: TPoint; branches: TList<TTrackBranch>);
function IsSecondCross(var data: TBranchData; start: TPoint): Boolean;
function SecondCrossPos(var data: TBranchData; start: TPoint): TPoint;
procedure OffsetBranches(branches: TList<TTrackBranch>; offset: Integer);

implementation

uses ObjBlok, ObjBlokUsek, ObjBlokVyhybka, ObjBlokVykol;

/// /////////////////////////////////////////////////////////////////////////////

{
  Pocitani vetvi vsech useku.
  Projde vsechy useky a pro kazdy spusti prislusou funkci, ktera spocita vetve.
}
procedure ComputeBranches(po: TPanelObjects);
var data: TBranchData; // = Array of Array of Integer; toto pole je pro algoritmus nize zdrojem vsech dat
  // jedna s o dvourozmerne pole reprezentujici cely panel, na kazdem poli je cislo bitmapoveho
  // metoda pro zpracovani jednoho bloku dostane na vstup vzdy toto pole s tim, ze jsou v nem jen body, ktere patri prislusnemu bloku
  // vsude jinde je (-1)
begin
  // inicializace pole data
  SetLength(data, po.PanelWidth, po.PanelHeight);
  for var col := 0 to po.PanelWidth - 1 do
    for var row := 0 to po.PanelHeight - 1 do
      data[col, row] := -1;

  for var i := 0 to po.blocks.Count - 1 do
  begin
    if (po.blocks[i].typ <> TBlkType.track) then
      continue;

    (po.blocks[i] as TTrack).branches.Clear();
    if (not(po.blocks[i] as TTrack).IsTurnout) then
      continue;

    if ((po.blocks[i] as TTrack).Root.X < 0) then
      continue;

    // inicializujeme pole symboly bloku
    for var j := 0 to (po.blocks[i] as TTrack).symbols.Count - 1 do
      data[(po.blocks[i] as TTrack).symbols[j].Position.X, (po.blocks[i] as TTrack).symbols[j].Position.Y] :=
        (po.blocks[i] as TTrack).symbols[j].SymbolID;

    // do tohoto pole take musime ulozit vyhybky a vykolejky
    for var j := 0 to po.blocks.Count - 1 do
    begin
      if ((po.blocks[j].typ = TBlkType.turnout) and ((po.blocks[j] as TTurnout).obj = po.blocks[i].index)) then
        data[(po.blocks[j] as TTurnout).Position.X, (po.blocks[j] as TTurnout).Position.Y] :=
          (po.blocks[j] as TTurnout).SymbolID;
      if ((po.blocks[j].typ = TBlkType.derail) and ((po.blocks[j] as TDerail).obj = po.blocks[i].index)) then
        data[(po.blocks[j] as TDerail).Pos.X, (po.blocks[j] as TDerail).Pos.Y] := (po.blocks[j] as TDerail).symbol +
          _S_DERAIL_B;
    end;

    var symbol := data[TTrack(po.blocks[i]).Root.X, TTrack(po.blocks[i]).Root.Y];
    if ((symbol = _S_DKS_DET_TOP) or (symbol = _S_DKS_NODET_TOP)) then
      (po.blocks[i] as TTrack).DKStype := dksTop
    else if ((symbol = _S_DKS_DET_BOT) or (symbol = _S_DKS_NODET_BOT)) then
      (po.blocks[i] as TTrack).DKStype := dksBottom
    else
      (po.blocks[i] as TTrack).DKStype := dksNone;

    // provedeme algoritmus
    if ((po.blocks[i] as TTrack).DKStype <> dksNone) then
    begin
      ComputeDKSBlockBranches(po, data, (po.blocks[i] as TTrack).Root, (po.blocks[i] as TTrack).branches);
      if (TTrack(po.blocks[i]).branches.Count < 3) then
      begin
        (po.blocks[i] as TTrack).DKStype := dksNone;
        ComputeNormalBlockBranches(po, data, (po.blocks[i] as TTrack).Root, (po.blocks[i] as TTrack).branches);
      end;
    end
    else
      ComputeNormalBlockBranches(po, data, (po.blocks[i] as TTrack).Root, (po.blocks[i] as TTrack).branches);

    // nastavime pole data opet na -1 (algortimus by to mel udelat sam, ale pro jistotu)
    for var j := 0 to (po.blocks[i] as TTrack).symbols.Count - 1 do
      data[(po.blocks[i] as TTrack).symbols[j].Position.X, (po.blocks[i] as TTrack).symbols[j].Position.Y] := -1;

    // odstranit vyhybky
    for var j := 0 to po.blocks.Count - 1 do
    begin
      if ((po.blocks[i].typ <> TBlkType.turnout) or ((po.blocks[i] as TTurnout).obj <> i)) then
        continue;
      data[(po.blocks[i] as TTurnout).Position.X, (po.blocks[i] as TTurnout).Position.Y] := -1;
    end;
  end;
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
procedure ComputeNormalBlockBranches(po: TPanelObjects; var data: TBranchData; start: TPoint; branches: TList<TTrackBranch>;
  future_offset: Integer);
var queue: TQueue<TPoint>;
  symbols: TList<TReliefSym>;
  node: ^TVetevEnd;
  startOnVyh: Boolean;
begin
  queue := TQueue<TPoint>.Create();
  queue.Enqueue(start);

  startOnVyh := ((data[start.X, start.Y] >= _S_TURNOUT_B) and (data[start.X, start.Y] <= _S_TURNOUT_E));
  symbols := TList<TReliefSym>.Create();

  try
    while (queue.Count > 0) do
    begin
      // expanduji jeden vrchol: pozor:  musim expandovat doleva i doprava

      // resetuji vetev:
      var branch := DefaultVetev();

      // vezmu aktualni vrchol z fronty
      var first := queue.Dequeue();

      // dodelat kontrolu toho, jestli nahodou nejsem na vyhybce

      // do pole symbols si ulozim useku aktualni vetve a pak je pouze predelam do dynamickeho pole vetev.symbols
      symbols.Clear();

      // expanduji oba smery z prvniho symbolu, dalsi symboly maji vzdy jen jeden smer k expanzi
      for var j := 0 to 1 do
      begin
        var new := first;
        // projizdim usek jednim smerem pro jedno 'j'
        while (((data[new.X, new.Y] >= _S_TRACK_DET_B) and (data[new.X, new.Y] <= _S_TRACK_NODET_E)) or
          ((data[new.X, new.Y] >= _S_DERAIL_B) and (data[new.X, new.Y] <= _S_DERAIL_E))) do
        begin
          // pridam symbol do seznamu symbolu
          if ((((new.X <> first.X) or (new.Y <> first.Y)) or (j = 1)) and ((data[new.X, new.Y] < _S_DERAIL_B) or
            (data[new.X, new.Y] > _S_DERAIL_E))) then
          begin
            var symbol: TReliefSym;
            symbol.Position := new;
            symbol.SymbolID := data[new.X, new.Y];
            symbols.Add(symbol);
          end;

          var temp := new;

          // vypocitam prvni vedlejsi pole
          if ((data[new.X, new.Y] >= _S_DERAIL_B) and (data[new.X, new.Y] <= _S_DERAIL_E)) then
          begin
            (po.blocks[po.GetObject(new)] as TDerail).branch := branches.Count + future_offset;
            data[new.X, new.Y] := _S_TRACK_DET_B;
          end;

          // podivame se na prvni ze dvou vedlejsich policek
          temp.X := new.X + GetTrackContinue(data[new.X, new.Y], ndPositive).X;
          temp.Y := new.Y + GetTrackContinue(data[new.X, new.Y], ndPositive).Y;

          // je na vedlejsim poli symbol?
          if (data[temp.X, temp.Y] > -1) and ((temp.X <> first.X) or (temp.Y <> first.Y)) then
          begin
            // ano, na prvnim vedlejsim poli je symbol -> resetuji aktualni pole a do noveho pole priradim pole vedlejsi
          end else begin
            // ne, na vedlejsim poli neni symbol -> toto policko jsme asi uz prosli, nebo tam proste nic neni
            // zkusime druhe vedlejsi policko

            temp.X := new.X + GetTrackContinue(data[new.X, new.Y], ndNegative).X;
            temp.Y := new.Y + GetTrackContinue(data[new.X, new.Y], ndNegative).Y;
          end;

          if ((data[temp.X, temp.Y] = _S_BUMPER_L) or (data[temp.X, temp.Y] = _S_BUMPER_R)) then
          begin
            var symbol: TReliefSym;
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
        if (data[new.X, new.Y] >= _S_TURNOUT_B) and (data[new.X, new.Y] <= _S_TURNOUT_E) then
        begin
          // na aktualnim poli je vyhybka -> nova vetev
          if (branch.node1.vyh > -1) then
          begin
            // 1. vyhybka uz existuje -> vytvorim 2. vyhybku
            node := @branch.node2;
          end else begin
            // 1. vyhybka neexstuje -> vytvorim 1. vyhybku
            node := @branch.node1;
          end;

          // zjistim index vyhybky a pridam dalsi vetve, pokud za vyhybkou existuji useky
          node^.vyh := po.blocks[po.GetObject(new)].index;
          node^.ref_plus := -1;
          node^.ref_minus := -1;

          // projdu 2 smery, do kterych muze vyhybka (je jasne, ze barvici cesta
          // musela prijit z te strany, kde se koleje spojuji)/
          // Pokud se zacinalo na vyhybce, projde se i 3. smer.
          var up: Integer;
          if (startOnVyh) then
          begin
            startOnVyh := false;
            up := 2;
          end
          else
            up := 1;

          for var i := 0 to up do
          begin
            var temp: TPoint;
            temp.X := new.X + _TURNOUT_CONNECTIONS[data[new.X, new.Y] - _S_TURNOUT_B].dir(TNavDir(i)).X;
            temp.Y := new.Y + _TURNOUT_CONNECTIONS[data[new.X, new.Y] - _S_TURNOUT_B].dir(TNavDir(i)).Y;

            if (data[temp.X, temp.Y] > -1) then
            begin
              // na dalsi pozici za vyhybkou je symbol -> novy zaznam do fronty
              if (i <> 2) then
                queue.Enqueue(temp);

              case (i) of
                0:
                  node^.ref_plus := branches.Count + queue.Count;
                1:
                  node^.ref_minus := branches.Count + queue.Count;
                2:
                  first := temp;
              end; // case
            end;
          end; // for 2_smery_do_kterych_muze_expandovat_vyhybka
          data[new.X, new.Y] := -1; // vyhybku jsem zpracoval
        end; // if branch_skoncila_vyhybkou
      end; // for j

      // prochazeni vetve je kompletni vcetne pripadnych koncovych vyhybek -> priradim do vetve symboly a vytvorim branch
      SetLength(branch.symbols, symbols.Count);
      for var i := 0 to symbols.Count - 1 do
        branch.symbols[i] := symbols[i];
      branches.Add(branch);
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
procedure ComputeDKSBlockBranches(po: TPanelObjects; var data: TBranchData; start: TPoint; branches: TList<TTrackBranch>);
var
  r: TBranchReturner;
  vlPos, vrPos, Pos: TPoint;
  tempBranches: TList<TTrackBranch>;
begin
  r.symbols := TList<TReliefSym>.Create();
  tempBranches := TList<TTrackBranch>.Create();
  vlPos := Point(-1, -1);
  vrPos := Point(-1, -1);

  try
    // 1) nejprve hledame usek k nejblizsi leve vyhybce
    GetTrackTillCrossing(data, Point(start.X - 1, start.Y), ndPositive, r);
    if (r.next.X < 0) then
      Exit();

    var branch := DefaultVetev();
    SetLength(branch.symbols, r.symbols.Count);
    for var i := 0 to r.symbols.Count - 1 do
      branch.symbols[i] := r.symbols[i];

    if ((data[r.next.X, r.next.Y] >= _S_TURNOUT_B) and (data[r.next.X, r.next.Y] <= _S_TURNOUT_E)) then
    begin
      branch.node1.vyh := po.blocks[po.GetObject(r.next)].index;
      vlPos := r.next;
    end;

    branches.Add(branch);

    // 2) hledame usek k nejblizsi prave vyhybce
    r.symbols.Clear();
    GetTrackTillCrossing(data, Point(start.X + 1, start.Y), ndNegative, r);
    if (r.next.X < 0) then
    begin
      branches.Clear();
      Exit();
    end;

    branch := DefaultVetev();
    SetLength(branch.symbols, r.symbols.Count);
    for var i := 0 to r.symbols.Count - 1 do
      branch.symbols[i] := r.symbols[i];

    if ((data[r.next.X, r.next.Y] >= _S_TURNOUT_B) and (data[r.next.X, r.next.Y] <= _S_TURNOUT_E)) then
    begin
      branch.node1.vyh := po.blocks[po.GetObject(r.next)].index;
      vrPos := r.next;
    end;

    branches.Add(branch);

    // dal nema smysl pokracovat, pokud nejsou obe vyhybky nalezeny
    if ((vlPos.X < 0) or (vrPos.X < 0)) then
    begin
      branches.Clear(); // neni DKS -> vykresli se vse pomoci symbolu
      Exit();
    end;

    // 3) hledame spojovaci usek obou vyhybek
    // musime expandovat v obbou smerech
    r.symbols.Clear();
    var symbol := data[vlPos.X + 1, vlPos.Y];
    GetTrackTillCrossing(data, Point(vlPos.X + 1, vlPos.Y), ndPositive, r);

    data[vlPos.X + 1, vlPos.Y] := symbol;
    GetTrackTillCrossing(data, Point(vlPos.X + 1, vlPos.Y), ndNegative, r);

    // symbol na pozici [vlPos.X+1, vlPos.Y] dvakrat -> vymazat
    r.symbols.Delete(0);

    branch := DefaultVetev();
    SetLength(branch.symbols, r.symbols.Count);
    for var i := 0 to r.symbols.Count - 1 do
      branch.symbols[i] := r.symbols[i];
    branches.Add(branch);

    // 4) odstranit vyhybky
    data[vlPos.X, vlPos.Y] := -1;
    data[vrPos.X, vrPos.Y] := -1;

    // 4) hledame navazujici levou branch
    tempBranches.Clear();
    ComputeNormalBlockBranches(po, data, Point(vlPos.X - 1, vlPos.Y), tempBranches, 4);
    OffsetBranches(tempBranches, 4);

    branches.Add(tempBranches[0]);
    branches.Add(tempBranches[0]); // docasne i prava branch
    tempBranches.Delete(0);
    branches.AddRange(tempBranches);

    // 5) hledame navazujici pravou branch
    tempBranches.Clear();
    ComputeNormalBlockBranches(po, data, Point(vrPos.X + 1, vrPos.Y), tempBranches, branches.Count - 1);
    OffsetBranches(tempBranches, branches.Count - 1);
    branches[4] := tempBranches[0];
    tempBranches.Delete(0);
    branches.AddRange(tempBranches);

    // 6) kontrolujeme krizeni "obe casti DKS v jednom bloku"
    if (IsSecondCross(data, start)) then
    begin
      Pos := SecondCrossPos(data, start);
      data[Pos.X, Pos.Y] := -1;

      var addI := branches.Count;
      ComputeNormalBlockBranches(po, data, Point(Pos.X - 1, Pos.Y), branches);
      if (addI <> branches.Count) then
      begin
        branch := branches[1];
        branch.node1.ref_minus := addI;
        branches[1] := branch;
      end;

      addI := branches.Count;
      ComputeNormalBlockBranches(po, data, Point(Pos.X + 1, Pos.Y), branches);
      if (addI <> branches.Count) then
      begin
        branch := branches[0];
        branch.node1.ref_minus := addI;
        branches[0] := branch;
      end;
    end;

  finally
    r.symbols.Free();
    tempBranches.Free();
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
procedure GetTrackTillCrossing(var data: TBranchData; start: TPoint; initDir: TNavDir; var res: TBranchReturner);
var new: TPoint;
  first: Boolean;
begin
  new := start;
  first := true;
  res.next := Point(-1, -1);

  while (((data[new.X, new.Y] >= _S_TRACK_DET_B) and (data[new.X, new.Y] < _S_DKS_DET_TOP)) or
         ((data[new.X, new.Y] >= _S_TRACK_NODET_B) and (data[new.X, new.Y] < _S_DKS_NODET_TOP))) do
  begin
    // pridam symbol do seznamu symbolu
    var symbol: TReliefSym;
    symbol.Position := new;
    symbol.SymbolID := data[new.X, new.Y];
    res.symbols.Add(symbol);

    var temp: TPoint;
    if (first) then
    begin
      // podivame se na vedlejsi pole ve spravnem smeru
      temp.X := new.X + GetTrackContinue(data[new.X, new.Y], initDir).X;
      temp.Y := new.Y + GetTrackContinue(data[new.X, new.Y], initDir).Y;
    end else begin
      // podivame se na vedlejsi pole
      temp.X := new.X + GetTrackContinue(data[new.X, new.Y], ndPositive).X;
      temp.Y := new.Y + GetTrackContinue(data[new.X, new.Y], ndPositive).Y;

      // je na vedlejsim poli symbol?
      if (data[temp.X, temp.Y] = -1) then
      begin
        // ne, na vedlejsim poli neni symbol -> zkusime poliko v druhem smeru
        temp.X := new.X + GetTrackContinue(data[new.X, new.Y], ndNegative).X;
        temp.Y := new.Y + GetTrackContinue(data[new.X, new.Y], ndNegative).Y;
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

function IsSecondCross(var data: TBranchData; start: TPoint): Boolean;
begin
  if ((data[start.X, start.Y] = _S_DKS_DET_TOP) and (data[start.X, start.Y + 1] = _S_DKS_DET_BOT)) then
    Exit(true);
  if ((data[start.X, start.Y] = _S_DKS_DET_BOT) and (data[start.X, start.Y - 1] = _S_DKS_DET_TOP)) then
    Exit(true);
  if ((data[start.X, start.Y] = _S_DKS_NODET_TOP) and (data[start.X, start.Y + 1] = _S_DKS_NODET_BOT)) then
    Exit(true);
  if ((data[start.X, start.Y] = _S_DKS_NODET_BOT) and (data[start.X, start.Y - 1] = _S_DKS_NODET_TOP)) then
    Exit(true);

  Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

function SecondCrossPos(var data: TBranchData; start: TPoint): TPoint;
begin
  if ((data[start.X, start.Y] = _S_DKS_DET_TOP) and (data[start.X, start.Y + 1] = _S_DKS_DET_BOT)) then
    Result := Point(start.X, start.Y + 1)
  else if ((data[start.X, start.Y] = _S_DKS_DET_BOT) and (data[start.X, start.Y - 1] = _S_DKS_DET_TOP)) then
    Result := Point(start.X, start.Y - 1)
  else if ((data[start.X, start.Y] = _S_DKS_NODET_TOP) and (data[start.X, start.Y + 1] = _S_DKS_NODET_BOT)) then
    Result := Point(start.X, start.Y + 1)
  else if ((data[start.X, start.Y] = _S_DKS_NODET_BOT) and (data[start.X, start.Y - 1] = _S_DKS_NODET_TOP)) then
    Result := Point(start.X, start.Y - 1)
  else
    raise Exception.Create('No second cross!');
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure OffsetBranches(branches: TList<TTrackBranch>; offset: Integer);
begin
  for var i := 0 to branches.Count - 1 do
  begin
    var branch := branches[i];

    if (branch.node1.ref_plus > -1) then
      branch.node1.ref_plus := branch.node1.ref_plus + offset;
    if (branch.node1.ref_minus > -1) then
      branch.node1.ref_minus := branch.node1.ref_minus + offset;

    if (branch.node2.ref_plus > -1) then
      branch.node2.ref_plus := branch.node2.ref_plus + offset;
    if (branch.node2.ref_minus > -1) then
      branch.node2.ref_minus := branch.node2.ref_minus + offset;

    branches[i] := branch;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
