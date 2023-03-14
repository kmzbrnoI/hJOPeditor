unit BitmapToObj;

interface

uses DXDraws, ImgList, Controls, Windows, SysUtils, Graphics, Classes, Types,
  ReliefObjects, Forms, StdCtrls, ExtCtrls, ReliefBitmap, Menus, ReliefText,
  symbolHelper, Generics.Collections, vetev, Global;

type
  TBitmapToObj = class
  private
    Bitmap: TPanelBitmap;
    Objects: TPanelObjects;

    processed: array [0 .. _MAX_WIDTH, 0 .. _MAX_HEIGHT] of Boolean;

    procedure ResetData;
    procedure ZpracujObject(VychoziPos: TPoint; var index: Integer; var vyh_index: Integer; var vykol_index: Integer);
    procedure AddPrj(Pos: TPoint; index: Integer);
    function IsSeparator(from, dir: TPoint): Boolean;

  public
    procedure BitmapToObjects(BitmapData: TPanelBitmap; ObjectData: TPanelObjects);
  end;

implementation

uses ObjBlok, ObjBlokRozp, ObjBlokUsek, ObjBlokNavestidlo, ObjBlokText,
  ObjBlokUvazka, ObjBlokUvazkaSpr, ObjBlokZamek, ObjBlokPomocny, ObjBlokVyhybka,
  ObjBlokVykol, ObjBlokPrejezd, ObjBlokPst;

// tato funkce se vola zvnejsku
procedure TBitmapToObj.BitmapToObjects(BitmapData: TPanelBitmap; ObjectData: TPanelObjects);
begin
  Self.Bitmap := BitmapData;
  Self.Objects := ObjectData;

  Self.ResetData();

  // nejdriv si najdeme vsechny rozpojovace a nahradime je beznymi rovnymi kolejemi
  begin
    var index: Integer := 0;
    for var x: Integer := 0 to Self.Bitmap.PanelWidth - 1 do
    begin
      for var y: Integer := 0 to Self.Bitmap.PanelHeight - 1 do
      begin
        var Symbol := Self.Bitmap.Symbols.GetSymbol(Point(x, y));
        if (Symbol = _S_DISC_TRACK) then
        begin
          var blk := TDisconnector.Create(index);
          blk.Pos := Point(x, y);
          Self.Objects.Bloky.Add(blk);

          // na misto [i, j] dame rovnou kolej (bud detekovanou nebo nedetekovanou)
          if (((x > 0) and (Self.Bitmap.Symbols.Bitmap[x - 1, y] >= _S_TRACK_NODET_B) and (Self.Bitmap.Symbols.Bitmap[x - 1,
            y] <= _S_TRACK_NODET_E)) or ((x < Self.Bitmap.PanelWidth - 1) and (Self.Bitmap.Symbols.Bitmap[x + 1,
            y] >= _S_TRACK_NODET_B) and (Self.Bitmap.Symbols.Bitmap[x + 1, y] <= _S_TRACK_NODET_E))) then
            Self.Bitmap.Symbols.Bitmap[x, y] := _S_TRACK_NODET_B
          else
            Self.Bitmap.Symbols.Bitmap[x, y] := _S_TRACK_DET_B;

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
    for var x: Integer := 0 to Self.Bitmap.PanelWidth - 1 do
    begin
      for var y: Integer := 0 to Self.Bitmap.PanelHeight - 1 do
      begin
        var Symbol := Self.Bitmap.Symbols.GetSymbol(Point(x, y));
        if (Symbol <> -1) then
        begin
          if ((not Self.processed[x, y]) and (((Symbol >= _S_TRACK_DET_B) and (Symbol <= _S_TRACK_NODET_E)) or
            ((Symbol >= _S_TURNOUT_B) and (Symbol <= _S_TURNOUT_E)) or (Symbol = _S_BUMPER_R) or (Symbol = _S_BUMPER_L))) then
            Self.ZpracujObject(Point(x, y), index, vyh_index, vykol_index);
        end; // if (Self.BitmapData.Symbols.GetSymbol(Point(i,j)) <> -1)
      end;
    end;
  end;

  // KPopisek, JCClick, soupravy
  for var x: Integer := 0 to Self.Objects.Bloky.Count - 1 do
  begin
    if (Self.Objects.Bloky[x].typ <> TBlkType.track) then
      continue;
    for var y: Integer := 0 to (Self.Objects.Bloky[x] as TTrack).Symbols.Count - 1 do
    begin
      if (Self.Bitmap.KPopisky.GetObject((Self.Objects.Bloky[x] as TTrack).Symbols[y].Position) <> -1) then
        (Self.Objects.Bloky[x] as TTrack).labels.Add((Self.Objects.Bloky[x] as TTrack).Symbols[y].Position);

      if (Self.Bitmap.JCClick.GetObject((Self.Objects.Bloky[x] as TTrack).Symbols[y].Position) <> -1) then
        (Self.Objects.Bloky[x] as TTrack).JCClick.Add((Self.Objects.Bloky[x] as TTrack).Symbols[y].Position);

      if (Self.Bitmap.Soupravy.GetObject((Self.Objects.Bloky[x] as TTrack).Symbols[y].Position) <> -1) then
        (Self.Objects.Bloky[x] as TTrack).trains.Add((Self.Objects.Bloky[x] as TTrack).Symbols[y].Position);
    end;
  end;

  // navestidla
  begin
    var index: Integer := 0;
    for var x: Integer := 0 to Self.Bitmap.PanelWidth - 1 do
    begin
      for var y: Integer := 0 to Self.Bitmap.PanelHeight - 1 do
      begin
        var Symbol := Self.Bitmap.Symbols.GetSymbol(Point(x, y));
        if ((Symbol >= _S_SIGNAL_B) and (Symbol <= _S_SIGNAL_E)) then
        begin
          var blk := TSignal.Create(index);
          blk.Position := Point(x, y);
          blk.SymbolID := Symbol - _S_SIGNAL_B;
          Self.Objects.Bloky.Add(blk);
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
    for var x: Integer := 0 to Self.Bitmap.Text.Count - 1 do
    begin
      var PopData := Self.Bitmap.Text.GetPopisekData(x);

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

      Self.Objects.Bloky.Add(blk);
    end;
  end;

  // prejezdy
  begin
    var index: Integer := 0;
    for var x: Integer := 0 to Self.Bitmap.PanelWidth - 1 do
    begin
      for var y: Integer := 0 to Self.Bitmap.PanelHeight - 1 do
      begin
        var Symbol := Self.Bitmap.Symbols.GetSymbol(Point(x, y));
        if (not Self.processed[x, y]) then
          if (Symbol = _S_CROSSING) then
          begin
            Self.AddPrj(Point(x, y), index);
            Self.processed[x, y] := true;
            Inc(index);
          end;
      end;
    end;
  end;

  // uvazky
  begin
    var index: Integer := 0;
    for var x: Integer := 0 to Self.Bitmap.PanelWidth - 1 do
    begin
      for var y: Integer := 0 to Self.Bitmap.PanelHeight - 1 do
      begin
        var Symbol := Self.Bitmap.Symbols.GetSymbol(Point(x, y));
        if (not Self.processed[x, y]) then
          if (Symbol = _S_LINKER_B) then
          begin
            var blk := TLinker.Create(index);
            blk.Pos := Point(x, y);
            Self.Objects.Bloky.Add(blk);
            Self.processed[x, y] := true;
            Inc(index);
          end;
      end;
    end;
  end;

  // uvazky soupravy
  begin
    var index: Integer := 0;
    for var x: Integer := 0 to Self.Bitmap.PanelWidth - 1 do
    begin
      for var y: Integer := 0 to Self.Bitmap.PanelHeight - 1 do
      begin
        var Symbol := Self.Bitmap.Symbols.GetSymbol(Point(x, y));
        if (not Self.processed[x, y]) then
          if (Symbol = _S_LINKER_TRAIN) then
          begin
            var blk := TLinkerTrain.Create(index);
            blk.Pos := Point(x, y);
            blk.spr_cnt := 1;
            Self.Objects.Bloky.Add(blk);
            Self.processed[x, y] := true;
            Inc(index);
          end;
      end;
    end;
  end;

  // zamky
  begin
    var index: Integer := 0;
    for var x: Integer := 0 to Self.Bitmap.PanelWidth - 1 do
    begin
      for var y: Integer := 0 to Self.Bitmap.PanelHeight - 1 do
      begin
        var Symbol := Self.Bitmap.Symbols.GetSymbol(Point(x, y));
        if (not Self.processed[x, y]) then
          if (Symbol = _S_LOCK) then
          begin
            var blk := TLock.Create(index);
            blk.Pos := Point(x, y);
            Self.Objects.Bloky.Add(blk);
            Self.processed[x, y] := true;
            Inc(index);
          end;
      end;
    end;
  end;

  // Pomocna stavedla
  begin
    var index: Integer := 0;
    for var x: Integer := 0 to Self.Bitmap.PanelWidth - 1 do
    begin
      for var y: Integer := 0 to Self.Bitmap.PanelHeight - 1 do
      begin
        var Symbol := Self.Bitmap.Symbols.GetSymbol(Point(x, y));
        if (not Self.processed[x, y]) then
          if (Symbol = _S_PST_TOP) then
          begin
            var blk := TPst.Create(index);
            blk.Pos := Point(x, y);
            Self.Objects.Bloky.Add(blk);
            Self.processed[x, y] := true;
            Inc(index);
          end;
      end;
    end;
  end;

  // vsechny ostatni symboly = pomocne symboly
  // vsehcny symboly jednoho typu davame do jednoho bloku
  // to, ze jsou vedle sebe, nebo na opacne strane reliefu, nas nezajima

  begin
    var index: Integer := 0;
    var indexMap: TDictionary<Integer, TGraphBlok> := TDictionary<Integer, TGraphBlok>.Create(); // tady je namapovan index symbolu na bloky

    try
      for var x: Integer := 0 to Self.Bitmap.PanelWidth - 1 do
      begin
        for var y: Integer := 0 to Self.Bitmap.PanelHeight - 1 do
        begin
          if (Self.processed[x, y]) then
            continue;

          var Symbol := Self.Bitmap.Symbols.GetSymbol(Point(x, y));
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
            Self.Objects.Bloky.Add(blk);
            Inc(index);
          end;

          Self.processed[x, y] := true;
        end;
      end;
    finally
      indexMap.Free();
    end;
  end;

  Self.Objects.ComputePrjPanelUsek();
end;

// resetuje data potrebna pro operaci BitmapToObject - vola se pri startu operace
procedure TBitmapToObj.ResetData;
var i, j: Integer;
begin
  for i := 0 to _MAX_WIDTH - 1 do
  begin
    for j := 0 to _MAX_HEIGHT - 1 do
      Self.processed[i, j] := false;
  end; // for i
end;

// Tato metoda dostane na vstup libovolny symbol libovolne koleje a expanduje
// celou kolej, vcetne vyhybek.
// Tj. tato metoda provadi fakticke spojeni bitmapovych symbolu do bloku.
// Metoda vyuziva techniku prohledavani do hloubky.
procedure TBitmapToObj.ZpracujObject(VychoziPos: TPoint; var index: Integer; var vyh_index: Integer;
  var vykol_index: Integer);
var usek: TTrack;
  Symbol: Integer;
begin
  // vytvoreni objektu a vlozeni do nej 1. symbolu
  usek := TTrack.Create(index);
  Inc(index);
  usek.Root := Point(-1, -1);

  Symbol := Self.Bitmap.Symbols.GetSymbol(VychoziPos);
  if ((Symbol >= _S_TURNOUT_B) and (Symbol <= _S_TURNOUT_E)) then
  begin
    var vyhybka: TTurnout := TTurnout.Create(vyh_index);
    Inc(vyh_index);
    vyhybka.Position := VychoziPos;
    vyhybka.SymbolID := Symbol;
    vyhybka.obj := index - 1;
    Self.Objects.Bloky.Add(vyhybka);
  end else begin
    var sym: TReliefSym;
    sym.SymbolID := Symbol;
    sym.Position := VychoziPos;
    usek.Symbols.Add(sym);
  end;

  Self.Objects.Bloky.Add(usek);
  var s: TStack<TPoint> := TStack<TPoint>.Create();

  try
    s.Push(VychoziPos);
    Self.processed[VychoziPos.X, VychoziPos.Y] := true;

    while (s.Count <> 0) do
    begin
      var cur: TPoint := s.Pop();
      Symbol := Self.Bitmap.Symbols.GetSymbol(cur);

      if (((Symbol >= _S_TRACK_DET_B) and (Symbol <= _S_TRACK_NODET_E)) or
        ((Symbol >= _S_DERAIL_B) and (Symbol <= _S_DERAIL_E)) or
        ((Symbol >= _S_TURNOUT_B) and (Symbol <= _S_TURNOUT_E)) or (Symbol = _S_BUMPER_R) or (Symbol = _S_BUMPER_L))
      then
      begin
        // cyklus kvuli smerum navaznosti (vyhybka ma az 3 smery navaznosti)
        for var j: Integer := 0 to 2 do
        begin
          // vykolejka je pro nase potreby rovna kolej
          if ((Symbol >= _S_DERAIL_B) and (Symbol <= _S_DERAIL_E)) then
            Self.Bitmap.Symbols.Bitmap[cur.X, cur.Y] := _S_TRACK_DET_B;

          var dir: TPoint;
          if ((Symbol >= _S_TURNOUT_B) and (Symbol <= _S_TURNOUT_E)) then
          begin
            dir := _TURNOUT_CONNECTIONS[Symbol-_S_TURNOUT_B].dir(TNavDir(j));
          end else if (((Symbol >= _S_DKS_DET_TOP) and (Symbol <= _S_DKS_DET_L)) or
                       ((Symbol >= _S_DKS_NODET_TOP) and (Symbol <= _S_DKS_NODET_L))) then
          begin
            dir := GetTrackContinue(Self.Bitmap.Symbols.GetSymbol(cur), TNavDir(j));
          end else begin // usek
            if (j = 2) then
              break; // usek ma jen 2 smery navaznosti
            dir := GetTrackContinue(Self.Bitmap.Symbols.GetSymbol(cur), TNavDir(j));
          end;

          // TempPos = pozice teoreticky navaznych objektu

          if (Self.IsSeparator(cur, dir)) then
            continue;

          var TempPos: TPoint := Point(cur.X + dir.X, cur.Y + dir.Y);
          var tempSym: Integer := Self.Bitmap.Symbols.GetSymbol(TempPos);

          if (Self.processed[TempPos.X, TempPos.Y]) then
            continue;

          // vedlejsi symbol je vykolejka
          if ((tempSym >= _S_DERAIL_B) and (tempSym <= _S_DERAIL_E)) then
          begin
            var derail: TDerail := TDerail.Create(vykol_index);
            Inc(vykol_index);
            derail.Symbol := tempSym - _S_DERAIL_B;
            derail.Pos := TempPos;
            derail.obj := index - 1;
            derail.branch := -1;
            Self.Objects.Bloky.Add(derail);

            processed[TempPos.X, TempPos.Y] := true;
            s.Push(TempPos);
          end;

          // vedlejsi symbol je usek, krizeni nebo zarazedlo
          if (((tempSym >= _S_TRACK_DET_B) and (tempSym <= _S_TRACK_NODET_E)) or (tempSym = _S_BUMPER_R) or
            (tempSym = _S_BUMPER_L)) then
          begin
            // ted vime, ze na NavaznostPos je usek

            if (((TempPos.X + GetTrackContinue(tempSym, ndPositive).X = cur.X) and
              (TempPos.Y + GetTrackContinue(tempSym, ndPositive).Y = cur.Y)) or
              ((TempPos.X + GetTrackContinue(tempSym, ndNegative).X = cur.X) and (TempPos.Y + GetTrackContinue(tempSym,
              ndNegative).Y = cur.Y)) or ((((tempSym >= _S_DKS_DET_TOP) and (tempSym <= _S_DKS_DET_L)) or ((tempSym >= _S_DKS_NODET_TOP) and (tempSym <= _S_DKS_NODET_L))) and
              ((TempPos.X + GetTrackContinue(tempSym, ndThird).X = cur.X) and (TempPos.Y + GetTrackContinue(tempSym,
              ndThird).Y = cur.Y)))) then
            begin
              // ted vime, ze i navaznost z TempPos vede na cur.Pos - muzeme pridat Symbol2 do bloku

              // pri pohybu vlevo a nahoru je zapotrebi overovat separator uz tady, protoze jenom tady vime, ze se pohybujeme doleva nebo nahoru
              // pohyb doprava a dolu resi podminka vyse

              var vertSep: Integer := Self.Bitmap.SeparatorsVert.GetObject(TempPos);
              var horSep: Integer := Self.Bitmap.SeparatorsHor.GetObject(TempPos);

              if (((vertSep = -1) and (horSep = -1)) or ((horSep = -1) and ((TempPos.X > cur.X) or (TempPos.Y <> cur.Y))
                ) or ((vertSep = -1) and ((TempPos.Y > cur.Y) or (TempPos.X <> cur.X)))) then
              begin
                var sym: TReliefSym;
                sym.SymbolID := tempSym;
                sym.Position := TempPos;
                usek.Symbols.Add(sym);
                processed[TempPos.X, TempPos.Y] := true;
                s.Push(TempPos);
              end;
            end;
          end; // if ((not Self.processed[TempPos[j].X,TempPos[j].Y])...

          // vedlejsi symbol je vyhybka
          if ((tempSym >= _S_TURNOUT_B) and (tempSym <= _S_TURNOUT_E)) then
          begin
            // ted vime, ze na NavaznostPos je vyhybka
            for var k: Integer := 0 to 2 do
            begin
              if ((TempPos.X + _TURNOUT_CONNECTIONS[tempSym-_S_TURNOUT_B].dir(TNavDir(k)).X = cur.X) and
                (TempPos.Y + _TURNOUT_CONNECTIONS[tempSym-_S_TURNOUT_B].dir(TNavDir(k)).Y = cur.Y)) then
              begin
                // ted vime, ze i navaznost z TempPos vede na cur.Pos - muzeme pridat Symbol2 do bloku
                // pridavame vyhybku
                var vyhybka: TTurnout := TTurnout.Create(vyh_index);
                Inc(vyh_index);
                vyhybka.Position := TempPos;
                vyhybka.SymbolID := tempSym;
                vyhybka.obj := index - 1;
                Self.Objects.Bloky.Add(vyhybka);

                processed[TempPos.X, TempPos.Y] := true;
                s.Push(TempPos);
              end; // if TempPos.x + _Vyh_Navaznost[...
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
// tato funkce prida kazdy druhy symbol do blikajicich
procedure TBitmapToObj.AddPrj(Pos: TPoint; index: Integer);
var blk: TGraphBlok;
  blik_point: TBlikPoint;
  height: Integer;
  Y: Integer;
  lefts: Integer;
begin
  blk := TCrossing.Create(index);

  height := 0;
  while (Self.Bitmap.Symbols.GetSymbol(Point(Pos.X, Pos.Y + height)) = _S_CROSSING) do
    Inc(height);

  // edges are always static
  (blk as TCrossing).StaticPositions.Add(Point(Pos.X, Pos.Y));
  (blk as TCrossing).StaticPositions.Add(Point(Pos.X, Pos.Y + height - 1));
  Self.processed[Pos.X, Pos.Y] := true;
  Self.processed[Pos.X, Pos.Y + height - 1] := true;

  if (height = 3) then
  begin
    // special case
    blik_point.Pos := Point(Pos.X, Pos.Y + 1);
    blik_point.PanelUsek := -1;
    (blk as TCrossing).BlikPositions.Add(blik_point);
    Self.processed[Pos.X, Pos.Y + 1] := true;
  end else begin
    for Y := 1 to height - 2 do
    begin
      lefts := Self.Bitmap.Symbols.GetSymbol(Point(Pos.X - 1, Pos.Y + Y));
      if ((lefts >= _S_TRACK_DET_B) and (lefts <= _S_TRACK_NODET_E) and (lefts <> 15) and (lefts <> 16) and (lefts <> 21) and
        (lefts <> 22)) then
      begin
        blik_point.Pos := Point(Pos.X, Pos.Y + Y);
        blik_point.PanelUsek := -1;
        (blk as TCrossing).BlikPositions.Add(blik_point)
      end
      else
        (blk as TCrossing).StaticPositions.Add(Point(Pos.X, Pos.Y + Y));
      Self.processed[Pos.X, Pos.Y + Y] := true;
    end;
  end;

  Self.Objects.Bloky.Add(blk);
end;

function TBitmapToObj.IsSeparator(from, dir: TPoint): Boolean;
begin
  if (dir.X = 1) then
    Result := (Self.Bitmap.SeparatorsVert.GetObject(from) <> -1)
  else if (dir.X = -1) then
    Result := (Self.Bitmap.SeparatorsVert.GetObject(Point(from.X - 1, from.Y)) <> -1)
  else if (dir.Y = 1) then
    Result := (Self.Bitmap.SeparatorsHor.GetObject(from) <> -1)
  else if (dir.Y = -1) then
    Result := (Self.Bitmap.SeparatorsHor.GetObject(Point(from.X, from.Y - 1)) <> -1)
  else
    Result := false;
end;

end.// unit
