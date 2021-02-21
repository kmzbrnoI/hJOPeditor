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

    Zahrnuto: array [0 .. _MAX_WIDTH, 0 .. _MAX_HEIGHT] of Boolean;

    procedure ResetData;
    procedure ZpracujObject(VychoziPos: TPoint; var index: Integer; var vyh_index: Integer; var vykol_index: Integer);
    procedure AddPrj(Pos: TPoint; index: Integer);
    function IsSeparator(from, dir: TPoint): Boolean;

  public
    procedure BitmapToObjects(BitmapData: TPanelBitmap; ObjectData: TPanelObjects);
  end; // TConvert

implementation

uses ObjBlok, ObjBlokRozp, ObjBlokUsek, ObjBlokNavestidlo, ObjBlokText,
  ObjBlokUvazka, ObjBlokUvazkaSpr, ObjBlokZamek, ObjBlokPomocny, ObjBlokVyhybka,
  ObjBlokVykol, ObjBlokPrejezd;

// tato funkce se vola zvnejsku
procedure TBitmapToObj.BitmapToObjects(BitmapData: TPanelBitmap; ObjectData: TPanelObjects);
var i, j: Integer;
  Symbol: ShortInt;
  blk: TGraphBlok;
  PopData: TPopisek;
  pomocne: TDictionary<Integer, TGraphBlok>;
  index, popisek_index, vyh_index, vykol_index: Integer;
begin
  Self.Bitmap := BitmapData;
  Self.Objects := ObjectData;

  Self.ResetData;

  // nejdriv si najdeme vsechny rozpojovace a nahradime je beznymi rovnymi kolejemi
  index := 0;
  for i := 0 to Self.Bitmap.PanelWidth - 1 do
  begin
    for j := 0 to Self.Bitmap.PanelHeight - 1 do
    begin
      Symbol := Self.Bitmap.Symbols.GetSymbol(Point(i, j));
      if (Symbol = _Rozp_Start) then
      begin
        blk := TRozp.Create(index);
        (blk as TRozp).Pos := Point(i, j);
        Self.Objects.Bloky.Add(blk);

        // na misto [i, j] dame rovnou kolej (bud detekovanou nebo nedetekovanou)
        if (((i > 0) and (Self.Bitmap.Symbols.Bitmap[i - 1, j] >= _Nedetek_Start) and (Self.Bitmap.Symbols.Bitmap[i - 1,
          j] <= _Nedetek_End)) or ((i < Self.Bitmap.PanelWidth - 1) and (Self.Bitmap.Symbols.Bitmap[i + 1,
          j] >= _Nedetek_Start) and (Self.Bitmap.Symbols.Bitmap[i + 1, j] <= _Nedetek_End))) then
          Self.Bitmap.Symbols.Bitmap[i, j] := _Nedetek_Start
        else
          Self.Bitmap.Symbols.Bitmap[i, j] := _Usek_Start;

        Inc(Index);
      end; // if
    end; // for j
  end; // for i

  // projedeme cely dvourozmerny bitmapovy obrazek a spojime useky do bloku
  index := 0;
  vyh_index := 0;
  vykol_index := 0;
  for i := 0 to Self.Bitmap.PanelWidth - 1 do
  begin
    for j := 0 to Self.Bitmap.PanelHeight - 1 do
    begin
      Symbol := Self.Bitmap.Symbols.GetSymbol(Point(i, j));
      if (Symbol <> -1) then
      begin
        if ((not Self.Zahrnuto[i, j]) and (((Symbol >= _Usek_Start) and (Symbol <= _Usek_End)) or
          ((Symbol >= _Krizeni_Start) and (Symbol <= _Krizeni_End)) or ((Symbol >= _Vyhybka_Start) and
          (Symbol <= _Vyhybka_End)) or (Symbol = _Zarazedlo_r) or (Symbol = _Zarazedlo_l))) then
          Self.ZpracujObject(Point(i, j), index, vyh_index, vykol_index);
      end; // if (Self.BitmapData.Symbols.GetSymbol(Point(i,j)) <> -1)
    end; // for j
  end; // for i

  // pridavani KPopisek, JCClick a souprav
  for i := 0 to Self.Objects.Bloky.Count - 1 do
  begin
    if (Self.Objects.Bloky[i].typ <> TBlkType.usek) then
      continue;
    for j := 0 to (Self.Objects.Bloky[i] as TUsek).Symbols.Count - 1 do
    begin
      if (Self.Bitmap.KPopisky.GetObject((Self.Objects.Bloky[i] as TUsek).Symbols[j].Position) <> -1) then
        (Self.Objects.Bloky[i] as TUsek).KPopisek.Add((Self.Objects.Bloky[i] as TUsek).Symbols[j].Position);

      if (Self.Bitmap.JCClick.GetObject((Self.Objects.Bloky[i] as TUsek).Symbols[j].Position) <> -1) then
        (Self.Objects.Bloky[i] as TUsek).JCClick.Add((Self.Objects.Bloky[i] as TUsek).Symbols[j].Position);

      if (Self.Bitmap.Soupravy.GetObject((Self.Objects.Bloky[i] as TUsek).Symbols[j].Position) <> -1) then
        (Self.Objects.Bloky[i] as TUsek).Soupravy.Add((Self.Objects.Bloky[i] as TUsek).Symbols[j].Position);
    end; // for j
  end; // for i

  // prevedeni navestidel
  index := 0;
  for i := 0 to Self.Bitmap.PanelWidth - 1 do
  begin
    for j := 0 to Self.Bitmap.PanelHeight - 1 do
    begin
      Symbol := Self.Bitmap.Symbols.GetSymbol(Point(i, j));
      if ((Symbol >= _SCom_Start) and (Symbol <= _SCom_End)) then
      begin
        blk := TNavestidlo.Create(index);
        (blk as TNavestidlo).Position := Point(i, j);
        (blk as TNavestidlo).SymbolID := Symbol - _SCom_Start;
        Self.Objects.Bloky.Add(blk);
        Self.Zahrnuto[i, j] := true;
        Inc(Index);
      end; // ((Symbol >= _SCom_Start) and (Symbol <= _SCom_End))
    end; // for j
  end; // for i

  // prevadeni textu
  index := 0;
  popisek_index := 0;
  for i := 0 to Self.Bitmap.Text.Count - 1 do
  begin
    PopData := Self.Bitmap.Text.GetPopisekData(i);

    if (PopData.BlokPopisek) then
      blk := TText.Create(popisek_index)
    else
      blk := TText.Create(index);
    (blk as TText).Text := PopData.Text;
    (blk as TText).Position := PopData.Position;
    (blk as TText).Color := PopData.Color;

    if (PopData.BlokPopisek) then
    begin
      blk.typ := TBlkType.blok_popisek;
      blk.Blok := -2;
      Inc(popisek_index);
    end
    else
      Inc(index);

    Self.Objects.Bloky.Add(blk);
  end; // for i

  // prevedeni prejezdu
  index := 0;
  for i := 0 to Self.Bitmap.PanelWidth - 1 do
  begin
    for j := 0 to Self.Bitmap.PanelHeight - 1 do
    begin
      Symbol := Self.Bitmap.Symbols.GetSymbol(Point(i, j));
      if (not Self.Zahrnuto[i, j]) then
        if (Symbol = _Prj) then
        begin
          Self.AddPrj(Point(i, j), index);
          Self.Zahrnuto[i, j] := true;
          Inc(index);
        end;
    end; // for j
  end; // for i

  // prevedeni uvazek
  index := 0;
  for i := 0 to Self.Bitmap.PanelWidth - 1 do
  begin
    for j := 0 to Self.Bitmap.PanelHeight - 1 do
    begin
      Symbol := Self.Bitmap.Symbols.GetSymbol(Point(i, j));
      if (not Self.Zahrnuto[i, j]) then
        if (Symbol = _Uvazka) then
        begin
          blk := TUvazka.Create(index);
          (blk as TUvazka).Pos := Point(i, j);
          Self.Objects.Bloky.Add(blk);
          Self.Zahrnuto[i, j] := true;
          Inc(index);
        end;
    end; // for j
  end; // for i

  // prevedeni uvazek spr
  index := 0;
  for i := 0 to Self.Bitmap.PanelWidth - 1 do
  begin
    for j := 0 to Self.Bitmap.PanelHeight - 1 do
    begin
      Symbol := Self.Bitmap.Symbols.GetSymbol(Point(i, j));
      if (not Self.Zahrnuto[i, j]) then
        if (Symbol = _Uvazka_Spr) then
        begin
          blk := TUvazkaSpr.Create(index);
          (blk as TUvazkaSpr).Pos := Point(i, j);
          (blk as TUvazkaSpr).spr_cnt := 1;
          Self.Objects.Bloky.Add(blk);
          Self.Zahrnuto[i, j] := true;
          Inc(index);
        end;
    end; // for j
  end; // for i

  // prevedeni zamku
  index := 0;
  for i := 0 to Self.Bitmap.PanelWidth - 1 do
  begin
    for j := 0 to Self.Bitmap.PanelHeight - 1 do
    begin
      Symbol := Self.Bitmap.Symbols.GetSymbol(Point(i, j));
      if (not Self.Zahrnuto[i, j]) then
        if (Symbol = _Zamek) then
        begin
          blk := TZamek.Create(index);
          (blk as TZamek).Pos := Point(i, j);
          Self.Objects.Bloky.Add(blk);
          Self.Zahrnuto[i, j] := true;
          Inc(index);
        end;
    end; // for j
  end; // for i

  // vsechny ostatni symboly = pomocne symboly
  // vsehcny symboly jednoho typu davame do jednoho bloku
  // to, ze jsou vedle sebe, nebo na opacne strane reliefu, nas nezajima

  // tady je namapovan index symbolu na bloky
  pomocne := TDictionary<Integer, TGraphBlok>.Create();

  index := 0;
  for i := 0 to Self.Bitmap.PanelWidth - 1 do
  begin
    for j := 0 to Self.Bitmap.PanelHeight - 1 do
    begin
      if (Self.Zahrnuto[i, j]) then
        continue;

      Symbol := Self.Bitmap.Symbols.GetSymbol(Point(i, j));
      if (Symbol = -1) then
        continue;

      if ((pomocne.TryGetValue(Symbol, blk)) and (not(Symbol in ObjBlokPomocny.BLK_ASSIGN_SYMBOLS))) then
      begin
        // pridat do existujiciho bloku
        (blk as TPomocnyObj).Positions.Add(Point(i, j));
      end else begin
        // vytvorit novy blok
        blk := TPomocnyObj.Create(index);
        (blk as TPomocnyObj).Symbol := Symbol;
        (blk as TPomocnyObj).Positions.Add(Point(i, j));

        pomocne.AddOrSetValue(Symbol, blk);
        Self.Objects.Bloky.Add(blk);
        Inc(index);
      end;

      Self.Zahrnuto[i, j] := true;
    end; // for j
  end; // for i

  pomocne.Free();

  Self.Objects.ComputePrjPanelUsek();
end;

// resetuje data potrebna pro operaci BitmapToObject - vola se pri startu operace
procedure TBitmapToObj.ResetData;
var i, j: Integer;
begin
  for i := 0 to _MAX_WIDTH - 1 do
  begin
    for j := 0 to _MAX_HEIGHT - 1 do
      Self.Zahrnuto[i, j] := false;
  end; // for i
end;

// Tato metoda dostane na vstup libovolny symbol libovolne koleje a expanduje
// celou kolej, vcetne vyhybek.
// Tj. tato metoda provadi fakticke spojeni bitmapovych symbolu do bloku.
// Metoda vyuziva techniku prohledavani do hloubky.
procedure TBitmapToObj.ZpracujObject(VychoziPos: TPoint; var index: Integer; var vyh_index: Integer;
  var vykol_index: Integer);
var usek: TUsek;
  sym: TReliefSym;
  j, k: Integer;
  Symbol, Symbol2: Integer;
  TempPos: TPoint;
  vyhybka: TVyhybka;
  vykol: TVykol;
  vertSep, horSep: SmallInt;
  dir: TPoint;
  s: TStack<TPoint>;
  cur: TPoint;
begin
  // vytvoreni objektu a vlozeni do nej 1. symbolu
  usek := TUsek.Create(index);
  Inc(index);
  usek.Root := Point(-1, -1);

  Symbol := Self.Bitmap.Symbols.GetSymbol(VychoziPos);
  if ((Symbol >= _Vyhybka_Start) and (Symbol <= _Vyhybka_End)) then
  begin
    vyhybka := TVyhybka.Create(vyh_index);
    Inc(vyh_index);
    vyhybka.Position := VychoziPos;
    vyhybka.SymbolID := Symbol;
    vyhybka.obj := index - 1;
    Self.Objects.Bloky.Add(vyhybka);
  end else begin
    sym.SymbolID := Symbol;
    sym.Position := VychoziPos;
    usek.Symbols.Add(sym);
  end;

  Self.Objects.Bloky.Add(usek);
  s := TStack<TPoint>.Create();

  try
    s.Push(VychoziPos);
    Self.Zahrnuto[VychoziPos.X, VychoziPos.Y] := true;

    while (s.Count <> 0) do
    begin
      cur := s.Pop();
      Symbol := Self.Bitmap.Symbols.GetSymbol(cur);

      if (((Symbol >= _Usek_Start) and (Symbol <= _Usek_End)) or
        ((Symbol >= _Krizeni_Start) and (Symbol <= _Krizeni_End)) or
        ((Symbol >= _Vykol_Start) and (Symbol <= _Vykol_End)) or
        ((Symbol >= _Vyhybka_Start) and (Symbol <= _Vyhybka_End)) or (Symbol = _Zarazedlo_r) or (Symbol = _Zarazedlo_l))
      then
      begin
        // cyklus kvuli smerum navaznosti (vyhybka ma az 3 smery navaznosti)
        for j := 0 to 2 do
        begin
          // vykolejka je pro nase potreby rovna kolej
          if ((Symbol >= _Vykol_Start) and (Symbol <= _Vykol_End)) then
            Self.Bitmap.Symbols.Bitmap[cur.X, cur.Y] := _Usek_Start;

          if ((Symbol >= _Vyhybka_Start) and (Symbol <= _Vyhybka_End)) then
          begin
            dir.X := _Vyh_Navaznost[((Symbol - _Vyhybka_Start) * 6) + (j * 2)];
            dir.Y := _Vyh_Navaznost[((Symbol - _Vyhybka_Start) * 6) + (j * 2) + 1];
          end else if ((Symbol >= _Krizeni_Start) and (Symbol <= _Krizeni_End)) then
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

          TempPos := Point(cur.X + dir.X, cur.Y + dir.Y);
          Symbol2 := Self.Bitmap.Symbols.GetSymbol(TempPos);

          if (Self.Zahrnuto[TempPos.X, TempPos.Y]) then
            continue;

          // vedlejsi symbol je vykolejka
          if ((Symbol2 >= _Vykol_Start) and (Symbol2 <= _Vykol_End)) then
          begin
            vykol := TVykol.Create(vykol_index);
            Inc(vykol_index);
            vykol.Symbol := Symbol2 - _Vykol_Start;
            vykol.Pos := TempPos;
            vykol.obj := index - 1;
            vykol.vetev := -1;
            Self.Objects.Bloky.Add(vykol);

            Zahrnuto[TempPos.X, TempPos.Y] := true;
            s.Push(TempPos);
          end;

          // vedlejsi symbol je usek, krizeni nebo zarazedlo
          if (((Symbol2 >= _Usek_Start) and (Symbol2 <= _Usek_End)) or
            ((Symbol2 >= _Krizeni_Start) and (Symbol2 <= _Krizeni_End)) or (Symbol2 = _Zarazedlo_r) or
            (Symbol2 = _Zarazedlo_l)) then
          begin
            // ted vime, ze na NavaznostPos je usek

            if (((TempPos.X + GetUsekNavaznost(Symbol2, ndPositive).X = cur.X) and
              (TempPos.Y + GetUsekNavaznost(Symbol2, ndPositive).Y = cur.Y)) or
              ((TempPos.X + GetUsekNavaznost(Symbol2, ndNegative).X = cur.X) and (TempPos.Y + GetUsekNavaznost(Symbol2,
              ndNegative).Y = cur.Y)) or ((Symbol2 >= _Krizeni_Start) and (Symbol2 <= _Krizeni_End) and
              ((TempPos.X + GetUsekNavaznost(Symbol2, ndThird).X = cur.X) and (TempPos.Y + GetUsekNavaznost(Symbol2,
              ndThird).Y = cur.Y)))) then
            begin
              // ted vime, ze i navaznost z TempPos vede na cur.Pos - muzeme pridat Symbol2 do bloku

              // pri pohybu vlevo a nahoru je zapotrebi overovat separator uz tady, protoze jenom tady vime, ze se pohybujeme doleva nebo nahoru
              // pohyb doprava a dolu resi podminka vyse

              vertSep := Self.Bitmap.SeparatorsVert.GetObject(TempPos);
              horSep := Self.Bitmap.SeparatorsHor.GetObject(TempPos);

              if (((vertSep = -1) and (horSep = -1)) or ((horSep = -1) and ((TempPos.X > cur.X) or (TempPos.Y <> cur.Y))
                ) or ((vertSep = -1) and ((TempPos.Y > cur.Y) or (TempPos.X <> cur.X)))) then
              begin
                sym.SymbolID := Symbol2;
                sym.Position := TempPos;
                usek.Symbols.Add(sym);
                Zahrnuto[TempPos.X, TempPos.Y] := true;
                s.Push(TempPos);
              end;
            end;
          end; // if ((not Self.Zahrnuto[TempPos[j].X,TempPos[j].Y])...

          // vedlejsi symbol je vyhybka
          if ((Symbol2 >= _Vyhybka_Start) and (Symbol2 <= _Vyhybka_End)) then
          begin
            // ted vime, ze na NavaznostPos je vyhybka
            for k := 0 to 2 do
            begin
              if ((TempPos.X + _Vyh_Navaznost[(Symbol2 - _Vyhybka_Start) * 6 + (k * 2)] = cur.X) and
                (TempPos.Y + _Vyh_Navaznost[((Symbol2 - _Vyhybka_Start) * 6) + (k * 2) + 1] = cur.Y)) then
              begin
                // ted vime, ze i navaznost z TempPos vede na cur.Pos - muzeme pridat Symbol2 do bloku
                // pridavame vyhybku
                vyhybka := TVyhybka.Create(vyh_index);
                Inc(vyh_index);
                vyhybka.Position := TempPos;
                vyhybka.SymbolID := Symbol2;
                vyhybka.obj := index - 1;
                Self.Objects.Bloky.Add(vyhybka);

                Zahrnuto[TempPos.X, TempPos.Y] := true;
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
  while (Self.Bitmap.Symbols.GetSymbol(Point(Pos.X, Pos.Y + height)) = _Prj) do
    Inc(height);

  // edges are always static
  (blk as TPrejezd).StaticPositions.Add(Point(Pos.X, Pos.Y));
  (blk as TPrejezd).StaticPositions.Add(Point(Pos.X, Pos.Y + height - 1));
  Self.Zahrnuto[Pos.X, Pos.Y] := true;
  Self.Zahrnuto[Pos.X, Pos.Y + height - 1] := true;

  if (height = 3) then
  begin
    // special case
    blik_point.Pos := Point(Pos.X, Pos.Y + 1);
    blik_point.PanelUsek := -1;
    (blk as TPrejezd).BlikPositions.Add(blik_point);
    Self.Zahrnuto[Pos.X, Pos.Y + 1] := true;
  end else begin
    for Y := 1 to height - 2 do
    begin
      lefts := Self.Bitmap.Symbols.GetSymbol(Point(Pos.X - 1, Pos.Y + Y));
      if ((lefts >= _Usek_Start) and (lefts <= _Usek_End) and (lefts <> 15) and (lefts <> 16) and (lefts <> 21) and
        (lefts <> 22)) then
      begin
        blik_point.Pos := Point(Pos.X, Pos.Y + Y);
        blik_point.PanelUsek := -1;
        (blk as TPrejezd).BlikPositions.Add(blik_point)
      end
      else
        (blk as TPrejezd).StaticPositions.Add(Point(Pos.X, Pos.Y + Y));
      Self.Zahrnuto[Pos.X, Pos.Y + Y] := true;
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
