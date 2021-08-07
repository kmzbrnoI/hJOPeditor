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
        if (Symbol = _Rozp_Kolej) then
        begin
          var blk := TRozp.Create(index);
          blk.Pos := Point(x, y);
          Self.Objects.Bloky.Add(blk);

          // na misto [i, j] dame rovnou kolej (bud detekovanou nebo nedetekovanou)
          if (((x > 0) and (Self.Bitmap.Symbols.Bitmap[x - 1, y] >= _Usek_Nedetek_Start) and (Self.Bitmap.Symbols.Bitmap[x - 1,
            y] <= _Usek_Nedetek_End)) or ((x < Self.Bitmap.PanelWidth - 1) and (Self.Bitmap.Symbols.Bitmap[x + 1,
            y] >= _Usek_Nedetek_Start) and (Self.Bitmap.Symbols.Bitmap[x + 1, y] <= _Usek_Nedetek_End))) then
            Self.Bitmap.Symbols.Bitmap[x, y] := _Usek_Nedetek_Start
          else
            Self.Bitmap.Symbols.Bitmap[x, y] := _Usek_Detek_Start;

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
          if ((not Self.processed[x, y]) and (((Symbol >= _Usek_Detek_Start) and (Symbol <= _Usek_Nedetek_End)) or
            ((Symbol >= _Vyhybka_Start) and (Symbol <= _Vyhybka_End)) or (Symbol = _Zarazedlo_r) or (Symbol = _Zarazedlo_l))) then
            Self.ZpracujObject(Point(x, y), index, vyh_index, vykol_index);
        end; // if (Self.BitmapData.Symbols.GetSymbol(Point(i,j)) <> -1)
      end;
    end;
  end;

  // KPopisek, JCClick, soupravy
  for var x: Integer := 0 to Self.Objects.Bloky.Count - 1 do
  begin
    if (Self.Objects.Bloky[x].typ <> TBlkType.usek) then
      continue;
    for var y: Integer := 0 to (Self.Objects.Bloky[x] as TUsek).Symbols.Count - 1 do
    begin
      if (Self.Bitmap.KPopisky.GetObject((Self.Objects.Bloky[x] as TUsek).Symbols[y].Position) <> -1) then
        (Self.Objects.Bloky[x] as TUsek).KPopisek.Add((Self.Objects.Bloky[x] as TUsek).Symbols[y].Position);

      if (Self.Bitmap.JCClick.GetObject((Self.Objects.Bloky[x] as TUsek).Symbols[y].Position) <> -1) then
        (Self.Objects.Bloky[x] as TUsek).JCClick.Add((Self.Objects.Bloky[x] as TUsek).Symbols[y].Position);

      if (Self.Bitmap.Soupravy.GetObject((Self.Objects.Bloky[x] as TUsek).Symbols[y].Position) <> -1) then
        (Self.Objects.Bloky[x] as TUsek).Soupravy.Add((Self.Objects.Bloky[x] as TUsek).Symbols[y].Position);
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
        if ((Symbol >= _Navestidlo_Start) and (Symbol <= _Navestidlo_End)) then
        begin
          var blk := TNavestidlo.Create(index);
          blk.Position := Point(x, y);
          blk.SymbolID := Symbol - _Navestidlo_Start;
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
        blk.typ := TBlkType.blok_popisek;
        blk.Blok := -2;
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
          if (Symbol = _Prejezd) then
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
          if (Symbol = _Uvazka_Start) then
          begin
            var blk := TUvazka.Create(index);
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
          if (Symbol = _Uvazka_Spr) then
          begin
            var blk := TUvazkaSpr.Create(index);
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
          if (Symbol = _Zamek) then
          begin
            var blk := TZamek.Create(index);
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
          if (Symbol = _Pst_Top) then
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
            (blk as TPomocnyObj).Positions.Add(Point(x, y));
          end else begin
            // vytvorit novy blok
            blk := TPomocnyObj.Create(index);
            (blk as TPomocnyObj).Symbol := Symbol;
            (blk as TPomocnyObj).Positions.Add(Point(x, y));

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
var usek: TUsek;
  Symbol: Integer;
begin
  // vytvoreni objektu a vlozeni do nej 1. symbolu
  usek := TUsek.Create(index);
  Inc(index);
  usek.Root := Point(-1, -1);

  Symbol := Self.Bitmap.Symbols.GetSymbol(VychoziPos);
  if ((Symbol >= _Vyhybka_Start) and (Symbol <= _Vyhybka_End)) then
  begin
    var vyhybka: TVyhybka := TVyhybka.Create(vyh_index);
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

      if (((Symbol >= _Usek_Detek_Start) and (Symbol <= _Usek_Nedetek_End)) or
        ((Symbol >= _Vykol_Start) and (Symbol <= _Vykol_End)) or
        ((Symbol >= _Vyhybka_Start) and (Symbol <= _Vyhybka_End)) or (Symbol = _Zarazedlo_r) or (Symbol = _Zarazedlo_l))
      then
      begin
        // cyklus kvuli smerum navaznosti (vyhybka ma az 3 smery navaznosti)
        for var j: Integer := 0 to 2 do
        begin
          // vykolejka je pro nase potreby rovna kolej
          if ((Symbol >= _Vykol_Start) and (Symbol <= _Vykol_End)) then
            Self.Bitmap.Symbols.Bitmap[cur.X, cur.Y] := _Usek_Detek_Start;

          var dir: TPoint;
          if ((Symbol >= _Vyhybka_Start) and (Symbol <= _Vyhybka_End)) then
          begin
            dir := _TURNOUT_CONNECTIONS[Symbol-_Vyhybka_Start].dir(TNavDir(j));
          end else if (((Symbol >= _DKS_Detek_Top) and (Symbol <= _DKS_Detek_L)) or
                       ((Symbol >= _DKS_Nedetek_Top) and (Symbol <= _DKS_Nedetek_L))) then
          begin
            dir := GetUsekNavaznost(Self.Bitmap.Symbols.GetSymbol(cur), TNavDir(j));
          end else begin // usek
            if (j = 2) then
              break; // usek ma jen 2 smery navaznosti
            dir := GetUsekNavaznost(Self.Bitmap.Symbols.GetSymbol(cur), TNavDir(j));
          end;

          // TempPos = pozice teoreticky navaznych objektu

          if (Self.IsSeparator(cur, dir)) then
            continue;

          var TempPos: TPoint := Point(cur.X + dir.X, cur.Y + dir.Y);
          var tempSym: Integer := Self.Bitmap.Symbols.GetSymbol(TempPos);

          if (Self.processed[TempPos.X, TempPos.Y]) then
            continue;

          // vedlejsi symbol je vykolejka
          if ((tempSym >= _Vykol_Start) and (tempSym <= _Vykol_End)) then
          begin
            var vykol: TVykol := TVykol.Create(vykol_index);
            Inc(vykol_index);
            vykol.Symbol := tempSym - _Vykol_Start;
            vykol.Pos := TempPos;
            vykol.obj := index - 1;
            vykol.vetev := -1;
            Self.Objects.Bloky.Add(vykol);

            processed[TempPos.X, TempPos.Y] := true;
            s.Push(TempPos);
          end;

          // vedlejsi symbol je usek, krizeni nebo zarazedlo
          if (((tempSym >= _Usek_Detek_Start) and (tempSym <= _Usek_Nedetek_End)) or (tempSym = _Zarazedlo_r) or
            (tempSym = _Zarazedlo_l)) then
          begin
            // ted vime, ze na NavaznostPos je usek

            if (((TempPos.X + GetUsekNavaznost(tempSym, ndPositive).X = cur.X) and
              (TempPos.Y + GetUsekNavaznost(tempSym, ndPositive).Y = cur.Y)) or
              ((TempPos.X + GetUsekNavaznost(tempSym, ndNegative).X = cur.X) and (TempPos.Y + GetUsekNavaznost(tempSym,
              ndNegative).Y = cur.Y)) or ((((tempSym >= _DKS_Detek_L) and (tempSym <= _DKS_Detek_Bot)) or ((tempSym >= _DKS_Nedetek_Top) and (tempSym <= _DKS_Nedetek_L))) and
              ((TempPos.X + GetUsekNavaznost(tempSym, ndThird).X = cur.X) and (TempPos.Y + GetUsekNavaznost(tempSym,
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
          if ((tempSym >= _Vyhybka_Start) and (tempSym <= _Vyhybka_End)) then
          begin
            // ted vime, ze na NavaznostPos je vyhybka
            for var k: Integer := 0 to 2 do
            begin
              if ((TempPos.X + _TURNOUT_CONNECTIONS[tempSym-_Vyhybka_Start].dir(TNavDir(k)).X = cur.X) and
                (TempPos.Y + _TURNOUT_CONNECTIONS[tempSym-_Vyhybka_Start].dir(TNavDir(k)).Y = cur.Y)) then
              begin
                // ted vime, ze i navaznost z TempPos vede na cur.Pos - muzeme pridat Symbol2 do bloku
                // pridavame vyhybku
                var vyhybka: TVyhybka := TVyhybka.Create(vyh_index);
                Inc(vyh_index);
                vyhybka.Position := TempPos;
                vyhybka.SymbolID := tempSym;
                vyhybka.obj := index - 1;
                Self.Objects.Bloky.Add(vyhybka);

                processed[TempPos.X, TempPos.Y] := true;
                s.Push(TempPos);
              end; // if TempPos.x + _Vyh_Navaznost[...
            end; // for k
          end; // if ((Symbol2 >= _Vyhybka_Start) and (Symbol2 <= _Vyhybka_End))
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
  blk := TPrejezd.Create(index);

  height := 0;
  while (Self.Bitmap.Symbols.GetSymbol(Point(Pos.X, Pos.Y + height)) = _Prejezd) do
    Inc(height);

  // edges are always static
  (blk as TPrejezd).StaticPositions.Add(Point(Pos.X, Pos.Y));
  (blk as TPrejezd).StaticPositions.Add(Point(Pos.X, Pos.Y + height - 1));
  Self.processed[Pos.X, Pos.Y] := true;
  Self.processed[Pos.X, Pos.Y + height - 1] := true;

  if (height = 3) then
  begin
    // special case
    blik_point.Pos := Point(Pos.X, Pos.Y + 1);
    blik_point.PanelUsek := -1;
    (blk as TPrejezd).BlikPositions.Add(blik_point);
    Self.processed[Pos.X, Pos.Y + 1] := true;
  end else begin
    for Y := 1 to height - 2 do
    begin
      lefts := Self.Bitmap.Symbols.GetSymbol(Point(Pos.X - 1, Pos.Y + Y));
      if ((lefts >= _Usek_Detek_Start) and (lefts <= _Usek_Nedetek_End) and (lefts <> 15) and (lefts <> 16) and (lefts <> 21) and
        (lefts <> 22)) then
      begin
        blik_point.Pos := Point(Pos.X, Pos.Y + Y);
        blik_point.PanelUsek := -1;
        (blk as TPrejezd).BlikPositions.Add(blik_point)
      end
      else
        (blk as TPrejezd).StaticPositions.Add(Point(Pos.X, Pos.Y + Y));
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
