unit BitmapToObj;

interface

uses DXDraws, ImgList, Controls, Windows, SysUtils, Graphics, Classes, Types,
  ReliefObjects, Forms, StdCtrls, ExtCtrls, ReliefBitmap, Menus, ReliefText,
  symbolHelper, Generics.Collections, vetev, Global;

type
  TBitmapToObj = class
  private
    bitmap: TPanelBitmap;
    objects: TPanelObjects;
    processed: array [0 .. _MAX_WIDTH, 0 .. _MAX_HEIGHT] of Boolean;

    procedure Reset();
    procedure ExploreTrack(VychoziPos: TPoint; var index: Integer; var vyh_index: Integer; var vykol_index: Integer);
    procedure AddCrossing(Pos: TPoint; index: Integer);
    function IsSeparator(from, dir: TPoint): Boolean;

  public
    procedure BitmapToObjects(BitmapData: TPanelBitmap; ObjectData: TPanelObjects);

  end;

implementation

uses ObjBlok, ObjBlokRozp, ObjBlokUsek, ObjBlokNavestidlo, ObjBlokText,
  ObjBlokUvazka, ObjBlokUvazkaSpr, ObjBlokZamek, ObjBlokPomocny, ObjBlokVyhybka,
  ObjBlokVykol, ObjBlokPrejezd, ObjBlokPst;

procedure TBitmapToObj.BitmapToObjects(BitmapData: TPanelBitmap; ObjectData: TPanelObjects);
begin
  Self.bitmap := BitmapData;
  Self.objects := ObjectData;

  Self.Reset();

  // nejdriv si najdeme vsechny rozpojovace a nahradime je beznymi rovnymi kolejemi
  begin
    var index: Integer := 0;
    for var x: Integer := 0 to Self.bitmap.PanelWidth - 1 do
    begin
      for var y: Integer := 0 to Self.bitmap.PanelHeight - 1 do
      begin
        var Symbol := Self.bitmap.Symbols.GetSymbol(Point(x, y));
        if (Symbol = _S_DISC_TRACK) then
        begin
          var blk := TDisconnector.Create(index);
          blk.Pos := Point(x, y);
          Self.objects.Bloky.Add(blk);

          // na misto [i, j] dame rovnou kolej (bud detekovanou nebo nedetekovanou)
          if (((x > 0) and (Self.bitmap.Symbols.Bitmap[x - 1, y] >= _S_TRACK_NODET_B) and (Self.bitmap.Symbols.Bitmap[x - 1,
            y] <= _S_TRACK_NODET_E)) or ((x < Self.bitmap.PanelWidth - 1) and (Self.bitmap.Symbols.Bitmap[x + 1,
            y] >= _S_TRACK_NODET_B) and (Self.bitmap.Symbols.Bitmap[x + 1, y] <= _S_TRACK_NODET_E))) then
            Self.bitmap.Symbols.Bitmap[x, y] := _S_TRACK_NODET_B
          else
            Self.bitmap.Symbols.Bitmap[x, y] := _S_TRACK_DET_B;

          Inc(Index);
        end; // if
      end;
    end;
  end;

  // projedeme cely dvourozmerny bitmapovy obrazek a spojime useky do bloku
  begin
    var index: Integer := 0;
    var vyh_index: Integer := 0;
    var vykol_index: Integer := 0;
    for var x: Integer := 0 to Self.bitmap.PanelWidth - 1 do
    begin
      for var y: Integer := 0 to Self.bitmap.PanelHeight - 1 do
      begin
        var Symbol := Self.bitmap.Symbols.GetSymbol(Point(x, y));
        if (Symbol <> -1) then
        begin
          if ((not Self.processed[x, y]) and (((Symbol >= _S_TRACK_DET_B) and (Symbol <= _S_TRACK_NODET_E)) or
            ((Symbol >= _S_TURNOUT_B) and (Symbol <= _S_TURNOUT_E)) or (Symbol = _S_BUMPER_R) or (Symbol = _S_BUMPER_L))) then
            Self.ExploreTrack(Point(x, y), index, vyh_index, vykol_index);
        end;
      end;
    end;
  end;

  // KPopisek, JCClick, soupravy
  for var x: Integer := 0 to Self.objects.Bloky.Count - 1 do
  begin
    if (Self.objects.Bloky[x].typ <> TBlkType.track) then
      continue;
    for var y: Integer := 0 to (Self.objects.Bloky[x] as TTrack).Symbols.Count - 1 do
    begin
      if (Self.bitmap.trackNames.IsObject((Self.objects.Bloky[x] as TTrack).Symbols[y].Position)) then
        (Self.objects.Bloky[x] as TTrack).labels.Add((Self.objects.Bloky[x] as TTrack).Symbols[y].Position);

      if (Self.bitmap.JCClick.IsObject((Self.objects.Bloky[x] as TTrack).Symbols[y].Position)) then
        (Self.objects.Bloky[x] as TTrack).JCClick.Add((Self.objects.Bloky[x] as TTrack).Symbols[y].Position);

      if (Self.bitmap.trainPoss.IsObject((Self.objects.Bloky[x] as TTrack).Symbols[y].Position)) then
        (Self.objects.Bloky[x] as TTrack).trains.Add((Self.objects.Bloky[x] as TTrack).Symbols[y].Position);
    end;
  end;

  // navestidla
  begin
    var index: Integer := 0;
    for var x: Integer := 0 to Self.bitmap.PanelWidth - 1 do
    begin
      for var y: Integer := 0 to Self.bitmap.PanelHeight - 1 do
      begin
        var Symbol := Self.bitmap.Symbols.GetSymbol(Point(x, y));
        if ((Symbol >= _S_SIGNAL_B) and (Symbol <= _S_SIGNAL_E)) then
        begin
          var blk := TSignal.Create(index);
          blk.Position := Point(x, y);
          blk.SymbolID := Symbol - _S_SIGNAL_B;
          Self.objects.Bloky.Add(blk);
          Self.processed[x, y] := true;
          Inc(Index);
        end; // ((Symbol >= _SCom_Start) and (Symbol <= _SCom_End))
      end;
    end;
  end;

  // text
  begin
    var index: Integer := 0;
    var popisek_index: Integer := 0;
    for var x: Integer := 0 to Self.bitmap.texts.Count - 1 do
    begin
      var PopData := Self.bitmap.texts.GetPopisekData(x);

      var blk: TText;
      if (PopData.BlokPopisek) then
        blk := TText.Create(popisek_index)
      else
        blk := TText.Create(index);
      blk.Text := PopData.Text;
      blk.Position := PopData.Position;
      blk.Color := PopData.Color;

      if (PopData.BlokPopisek) then
      begin
        blk.typ := TBlkType.description;
        blk.block := -2;
        Inc(popisek_index);
      end
      else
        Inc(index);

      Self.objects.Bloky.Add(blk);
    end;
  end;

  // prejezdy
  begin
    var index: Integer := 0;
    for var x: Integer := 0 to Self.bitmap.PanelWidth - 1 do
    begin
      for var y: Integer := 0 to Self.bitmap.PanelHeight - 1 do
      begin
        var Symbol := Self.bitmap.Symbols.GetSymbol(Point(x, y));
        if (not Self.processed[x, y]) then
          if (Symbol = _S_CROSSING) then
          begin
            Self.AddCrossing(Point(x, y), index);
            Self.processed[x, y] := true;
            Inc(index);
          end;
      end;
    end;
  end;

  // uvazky
  begin
    var index: Integer := 0;
    for var x: Integer := 0 to Self.bitmap.PanelWidth - 1 do
    begin
      for var y: Integer := 0 to Self.bitmap.PanelHeight - 1 do
      begin
        var Symbol := Self.bitmap.Symbols.GetSymbol(Point(x, y));
        if (not Self.processed[x, y]) then
          if (Symbol = _S_LINKER_B) then
          begin
            var blk := TLinker.Create(index);
            blk.Pos := Point(x, y);
            Self.objects.Bloky.Add(blk);
            Self.processed[x, y] := true;
            Inc(index);
          end;
      end;
    end;
  end;

  // uvazky soupravy
  begin
    var index: Integer := 0;
    for var x: Integer := 0 to Self.bitmap.PanelWidth - 1 do
    begin
      for var y: Integer := 0 to Self.bitmap.PanelHeight - 1 do
      begin
        var Symbol := Self.bitmap.Symbols.GetSymbol(Point(x, y));
        if (not Self.processed[x, y]) then
          if (Symbol = _S_LINKER_TRAIN) then
          begin
            var blk := TLinkerTrain.Create(index);
            blk.Pos := Point(x, y);
            blk.spr_cnt := 1;
            Self.objects.Bloky.Add(blk);
            Self.processed[x, y] := true;
            Inc(index);
          end;
      end;
    end;
  end;

  // zamky
  begin
    var index: Integer := 0;
    for var x: Integer := 0 to Self.bitmap.PanelWidth - 1 do
    begin
      for var y: Integer := 0 to Self.bitmap.PanelHeight - 1 do
      begin
        var Symbol := Self.bitmap.Symbols.GetSymbol(Point(x, y));
        if (not Self.processed[x, y]) then
          if (Symbol = _S_LOCK) then
          begin
            var blk := TLock.Create(index);
            blk.Pos := Point(x, y);
            Self.objects.Bloky.Add(blk);
            Self.processed[x, y] := true;
            Inc(index);
          end;
      end;
    end;
  end;

  // Pomocna stavedla
  begin
    var index: Integer := 0;
    for var x: Integer := 0 to Self.bitmap.PanelWidth - 1 do
    begin
      for var y: Integer := 0 to Self.bitmap.PanelHeight - 1 do
      begin
        var Symbol := Self.bitmap.Symbols.GetSymbol(Point(x, y));
        if (not Self.processed[x, y]) then
          if (Symbol = _S_PST_TOP) then
          begin
            var blk := TPst.Create(index);
            blk.Pos := Point(x, y);
            Self.objects.Bloky.Add(blk);
            Self.processed[x, y] := true;
            Inc(index);
          end;
      end;
    end;
  end;

  // vsechny ostatni symboly = pomocne symboly
  // vsechny symboly jednoho typu davame do jednoho bloku
  // je jedno, jestli jsou vedle sebe nebo na opacne strane reliefu
  begin
    var index: Integer := 0;
    var indexMap: TDictionary<Integer, TGraphBlok> := TDictionary<Integer, TGraphBlok>.Create(); // tady je namapovan index symbolu na bloky

    try
      for var x: Integer := 0 to Self.bitmap.PanelWidth - 1 do
      begin
        for var y: Integer := 0 to Self.bitmap.PanelHeight - 1 do
        begin
          if (Self.processed[x, y]) then
            continue;

          var Symbol := Self.bitmap.Symbols.GetSymbol(Point(x, y));
          if (Symbol = -1) then
            continue;

          var blk: TGraphBlok;
          if ((indexMap.TryGetValue(Symbol, blk)) and (not(Symbol in ObjBlokPomocny.BLK_ASSIGN_SYMBOLS))) then
          begin
            // pridat do existujiciho bloku
            (blk as TObjOther).Positions.Add(Point(x, y));
          end else begin
            // vytvorit novy blok
            blk := TObjOther.Create(index);
            (blk as TObjOther).Symbol := Symbol;
            (blk as TObjOther).Positions.Add(Point(x, y));

            indexMap.AddOrSetValue(Symbol, blk);
            Self.objects.Bloky.Add(blk);
            Inc(index);
          end;

          Self.processed[x, y] := true;
        end;
      end;
    finally
      indexMap.Free();
    end;
  end;

  Self.objects.ComputePrjPanelUsek();
end;

// resetuje data potrebna pro operaci BitmapToObject - vola se pri startu operace
procedure TBitmapToObj.Reset();
begin
  for var i: Integer := 0 to _MAX_WIDTH - 1 do
    for var j: Integer := 0 to _MAX_HEIGHT - 1 do
      Self.processed[i, j] := false;
end;

// Tato metoda dostane na vstup libovolny symbol libovolne koleje a expanduje
// celou kolej, vcetne vyhybek.
// Tj. tato metoda provadi fakticke spojeni bitmapovych symbolu do bloku.
// Metoda vyuziva techniku prohledavani do hloubky.
procedure TBitmapToObj.ExploreTrack(VychoziPos: TPoint; var index: Integer; var vyh_index: Integer;
  var vykol_index: Integer);
var track: TTrack;
  symbol: Integer;
begin
  // vytvoreni objektu a vlozeni do nej 1. symbolu
  track := TTrack.Create(index);
  Inc(index);
  track.Root := Point(-1, -1);

  symbol := Self.bitmap.Symbols.GetSymbol(VychoziPos);
  if ((symbol >= _S_TURNOUT_B) and (symbol <= _S_TURNOUT_E)) then
  begin
    var vyhybka: TTurnout := TTurnout.Create(vyh_index);
    Inc(vyh_index);
    vyhybka.Position := VychoziPos;
    vyhybka.SymbolID := symbol;
    vyhybka.obj := index - 1;
    Self.objects.Bloky.Add(vyhybka);
  end else begin
    var sym: TReliefSym;
    sym.SymbolID := symbol;
    sym.Position := VychoziPos;
    track.Symbols.Add(sym);
  end;

  Self.objects.Bloky.Add(track);
  var s: TStack<TPoint> := TStack<TPoint>.Create();

  try
    s.Push(VychoziPos);
    Self.processed[VychoziPos.X, VychoziPos.Y] := true;

    while (s.Count <> 0) do
    begin
      var cur: TPoint := s.Pop();
      symbol := Self.bitmap.Symbols.GetSymbol(cur);

      if (((symbol >= _S_TRACK_DET_B) and (symbol <= _S_TRACK_NODET_E)) or
        ((symbol >= _S_DERAIL_B) and (symbol <= _S_DERAIL_E)) or
        ((symbol >= _S_TURNOUT_B) and (symbol <= _S_TURNOUT_E)) or (symbol = _S_BUMPER_R) or (symbol = _S_BUMPER_L) or
         (symbol = _S_CROSSING))
      then
      begin
        // cyklus kvuli smerum navaznosti (vyhybka ma az 3 smery navaznosti)
        for var j: Integer := 0 to 2 do
        begin
          // vykolejka je pro nase potreby rovna kolej
          if ((symbol >= _S_DERAIL_B) and (symbol <= _S_DERAIL_E)) then
            Self.bitmap.Symbols.Bitmap[cur.X, cur.Y] := _S_TRACK_DET_B;

          var dir: TPoint;
          if ((symbol >= _S_TURNOUT_B) and (symbol <= _S_TURNOUT_E)) then
          begin
            dir := _TURNOUT_CONNECTIONS[symbol-_S_TURNOUT_B].dir(TNavDir(j));
          end else if (((symbol >= _S_DKS_DET_TOP) and (symbol <= _S_DKS_DET_L)) or
                       ((symbol >= _S_DKS_NODET_TOP) and (symbol <= _S_DKS_NODET_L))) then
          begin
            dir := GetTrackContinue(Self.bitmap.Symbols.GetSymbol(cur), TNavDir(j));
          end else if (symbol = _S_CROSSING) then
          begin
            if (j = 2) then
              break;
            dir := GetTrackContinue(_S_TRACK_DET_B, TNavDir(j));
          end else begin // usek
            if (j = 2) then
              break; // usek ma jen 2 smery navaznosti
            dir := GetTrackContinue(Self.bitmap.Symbols.GetSymbol(cur), TNavDir(j));
          end;

          // TempPos = pozice teoreticky navaznych objektu

          if (Self.IsSeparator(cur, dir)) then
            continue;

          var tempPos: TPoint := Point(cur.X + dir.X, cur.Y + dir.Y);
          var tempSym: Integer := Self.bitmap.Symbols.GetSymbol(tempPos);

          if (Self.processed[tempPos.X, tempPos.Y]) then
            continue;

          // vedlejsi symbol je vykolejka
          if ((tempSym >= _S_DERAIL_B) and (tempSym <= _S_DERAIL_E)) then
          begin
            var derail: TDerail := TDerail.Create(vykol_index);
            Inc(vykol_index);
            derail.symbol := tempSym - _S_DERAIL_B;
            derail.Pos := tempPos;
            derail.obj := index - 1;
            derail.branch := -1;
            Self.objects.Bloky.Add(derail);

            processed[tempPos.X, tempPos.Y] := true;
            s.Push(tempPos);
          end;

          if (tempSym = _S_CROSSING) then
          begin
            var sym: TReliefSym;
            if ((Self.bitmap.Symbols.Bitmap[cur.X, cur.Y] >= _S_TRACK_NODET_B) and (Self.bitmap.Symbols.Bitmap[cur.X, cur.Y] <= _S_TRACK_NODET_E)) then
              sym.SymbolID := _S_TRACK_NODET_B
            else
              sym.SymbolID := _S_TRACK_DET_B;
            sym.Position := tempPos;
            track.Symbols.Add(sym);
            processed[tempPos.X, tempPos.Y] := true;
            s.Push(tempPos);
          end;

          // vedlejsi symbol je usek, krizeni nebo zarazedlo
          if (((tempSym >= _S_TRACK_DET_B) and (tempSym <= _S_TRACK_NODET_E)) or (tempSym = _S_BUMPER_R) or
            (tempSym = _S_BUMPER_L)) then
          begin
            // ted vime, ze na NavaznostPos je usek

            if (((tempPos.X + GetTrackContinue(tempSym, ndPositive).X = cur.X) and
              (tempPos.Y + GetTrackContinue(tempSym, ndPositive).Y = cur.Y)) or
              ((tempPos.X + GetTrackContinue(tempSym, ndNegative).X = cur.X) and (tempPos.Y + GetTrackContinue(tempSym,
              ndNegative).Y = cur.Y)) or ((((tempSym >= _S_DKS_DET_TOP) and (tempSym <= _S_DKS_DET_L)) or ((tempSym >= _S_DKS_NODET_TOP) and (tempSym <= _S_DKS_NODET_L))) and
              ((tempPos.X + GetTrackContinue(tempSym, ndThird).X = cur.X) and (tempPos.Y + GetTrackContinue(tempSym,
              ndThird).Y = cur.Y)))) then
            begin
              // ted vime, ze i navaznost z tempPos vede na cur.Pos - muzeme pridat Symbol2 do bloku

              // pri pohybu vlevo a nahoru je zapotrebi overovat separator uz tady, protoze jenom tady vime, ze se pohybujeme doleva nebo nahoru
              // pohyb doprava a dolu resi podminka vyse

              var vertSep: Boolean := Self.bitmap.SeparatorsVert.IsObject(tempPos);
              var horSep: Boolean := Self.bitmap.SeparatorsHor.IsObject(tempPos);

              if (((not vertSep) and (not horSep)) or ((not horSep) and ((tempPos.X > cur.X) or (tempPos.Y <> cur.Y))
                ) or ((not vertSep) and ((tempPos.Y > cur.Y) or (tempPos.X <> cur.X)))) then
              begin
                var sym: TReliefSym;
                sym.SymbolID := tempSym;
                sym.Position := tempPos;
                track.Symbols.Add(sym);
                processed[tempPos.X, tempPos.Y] := true;
                s.Push(tempPos);
              end;
            end;
          end; // if ((not Self.processed[tempPos[j].X,tempPos[j].Y])...

          // vedlejsi symbol je vyhybka
          if ((tempSym >= _S_TURNOUT_B) and (tempSym <= _S_TURNOUT_E)) then
          begin
            // ted vime, ze na NavaznostPos je vyhybka
            for var k: Integer := 0 to 2 do
            begin
              if ((tempPos.X + _TURNOUT_CONNECTIONS[tempSym-_S_TURNOUT_B].dir(TNavDir(k)).X = cur.X) and
                (tempPos.Y + _TURNOUT_CONNECTIONS[tempSym-_S_TURNOUT_B].dir(TNavDir(k)).Y = cur.Y)) then
              begin
                // ted vime, ze i navaznost z tempPos vede na cur.Pos - muzeme pridat Symbol2 do bloku
                // pridavame vyhybku
                var vyhybka: TTurnout := TTurnout.Create(vyh_index);
                Inc(vyh_index);
                vyhybka.Position := tempPos;
                vyhybka.SymbolID := tempSym;
                vyhybka.obj := index - 1;
                Self.objects.Bloky.Add(vyhybka);

                processed[tempPos.X, tempPos.Y] := true;
                s.Push(tempPos);
              end; // if tempPos.x + _Vyh_Navaznost[...
            end; // for k
          end; // if ((Symbol2 >= _S_TURNOUT_B) and (Symbol2 <= _S_TURNOUT_E))
        end; // for j
      end; // if ((Symbol >= _Usek_Start) and (Symbol <= _Usek_End)) ...
    end; // while

  finally
    s.Free();
  end;
end;

// tato funkce predpoklada, ze jedeme odzhora dolu a narazime na prvni vyskyt symbolu uplne nahore
procedure TBitmapToObj.AddCrossing(Pos: TPoint; index: Integer);
begin
  var crossing: TCrossing := TCrossing.Create(index);

  var height := 0;
  while (Self.bitmap.Symbols.GetSymbol(Point(Pos.X, Pos.Y + height)) = _S_CROSSING) do
    Inc(height);

  // edges are always static points
  crossing.StaticPositions.Add(Point(Pos.X, Pos.Y));
  crossing.StaticPositions.Add(Point(Pos.X, Pos.Y + height - 1));
  Self.processed[Pos.X, Pos.Y] := true;
  Self.processed[Pos.X, Pos.Y + height - 1] := true;

  if (height = 3) then
  begin
    // special case
    var blik_point: TBlikPoint;
    blik_point.Pos := Point(Pos.X, Pos.Y + 1);
    blik_point.PanelUsek := -1;
    crossing.BlikPositions.Add(blik_point);
    Self.processed[Pos.X, Pos.Y + 1] := true;
  end else begin
    for var Y := 1 to height - 2 do
    begin
      var lefts := Self.bitmap.Symbols.GetSymbol(Point(Pos.X - 1, Pos.Y + Y));
      if ((lefts >= _S_TRACK_DET_B) and (lefts <= _S_TRACK_NODET_E) and
          (lefts <> _S_TRACK_DET_LU) and (lefts <> _S_TRACK_DET_LD) and (lefts <> _S_TRACK_NODET_LU) and (lefts <> _S_TRACK_NODET_LD)) then
      begin
        var blik_point: TBlikPoint;
        blik_point.Pos := Point(Pos.X, Pos.Y + Y);
        blik_point.PanelUsek := -1;
        crossing.BlikPositions.Add(blik_point);
      end else
        crossing.StaticPositions.Add(Point(Pos.X, Pos.Y + Y));
      Self.processed[Pos.X, Pos.Y + Y] := true;
    end;
  end;

  Self.objects.Bloky.Add(crossing);
end;

function TBitmapToObj.IsSeparator(from, dir: TPoint): Boolean;
begin
  if (dir.X = 1) then
    Result := (Self.bitmap.SeparatorsVert.IsObject(from))
  else if (dir.X = -1) then
    Result := (Self.bitmap.SeparatorsVert.IsObject(Point(from.X - 1, from.Y)))
  else if (dir.Y = 1) then
    Result := (Self.bitmap.SeparatorsHor.IsObject(from))
  else if (dir.Y = -1) then
    Result := (Self.bitmap.SeparatorsHor.IsObject(Point(from.X, from.Y - 1)))
  else
    Result := false;
end;

end.// unit
