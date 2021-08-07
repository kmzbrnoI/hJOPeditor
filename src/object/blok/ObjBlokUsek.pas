unit ObjBlokUsek;

interface

uses ObjBlok, Generics.Collections, Types, symbolHelper, vetev, IniFiles,
  SysUtils, StrUtils, Global, Graphics, PGraphics;

type

  TDKSType = (dksNone = 0, dksTop = 1, dksBottom = 2);

  TUsek = class(TGraphBlok)
    Root: TPoint;
    IsVyhybka: boolean; // pomocny flag pro vykreslovani v modu korenu
    Symbols: TList<TReliefSym>; // pokud je v useku vykolejka, je zde ulozena jako klasicky symbol
    JCClick: TList<TPoint>;
    KPopisek: TList<TPoint>;
    Soupravy: TList<TPoint>;
    KpopisekStr: string;
    DKStype: TDKSType;

    Vetve: TList<TVetev>; // vetve useku
    // vetev 0 je vzdy koren
    // zde je ulozen binarni strom v pseudo-forme
    // na 0. indexu je koren, kazdy vrchol pak obsahuje referenci na jeho deti


    // program si duplikuje ulozena data - po rozdeleni useku na vetve uklada usek jak nerozdeleny tak rozdeleny

    constructor Create(index: Integer);
    destructor Destroy(); override;
    procedure Load(ini: TMemIniFile; key: string; version: Word); override;
    procedure Save(ini: TMemIniFile; key: string); override;
    procedure Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
      mode: TMode); override;
  end;

implementation

uses ReliefObjects;

/// /////////////////////////////////////////////////////////////////////////////

constructor TUsek.Create(index: Integer);
begin
  inherited;
  Self.typ := TBlkType.usek;
  Self.Symbols := TList<TReliefSym>.Create();
  Self.JCClick := TList<TPoint>.Create();
  Self.KPopisek := TList<TPoint>.Create();
  Self.Soupravy := TList<TPoint>.Create();
  Self.Vetve := TList<TVetev>.Create();
end;

destructor TUsek.Destroy();
begin
  Self.Symbols.Free();
  Self.JCClick.Free();
  Self.KPopisek.Free();
  Self.Soupravy.Free();
  Self.Vetve.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TUsek.Load(ini: TMemIniFile; key: string; version: Word);
var obj: string;
  j, k: Integer;
  symbol: TReliefSym;
  pos: TPoint;
  vetevCount: Integer;
  vetev: TVetev;
begin
  inherited;

  Self.Root := GetPos(ini.ReadString(key, 'R', '-1;-1'));
  Self.DKStype := TDKSType(ini.ReadInteger(key, 'DKS', Integer(dksNone)));

  // Symbols
  Self.Symbols.Clear();
  obj := ini.ReadString(key, 'S', '');
  for j := 0 to (Length(obj) div 8) - 1 do
  begin
    try
      symbol.Position.X := StrToInt(copy(obj, j * 8 + 1, 3));
      symbol.Position.Y := StrToInt(copy(obj, j * 8 + 4, 3));
      symbol.SymbolID := StrToInt(copy(obj, j * 8 + 7, 2));
      if (version < _FILEVERSION_20) then
        symbol.SymbolID := TranscodeSymbolFromBpnlV3(symbol.SymbolID);
    except
      continue;
    end;
    Self.Symbols.Add(symbol);
  end; // for j

  // JCClick
  Self.JCClick.Clear();
  obj := ini.ReadString(key, 'C', '');
  for j := 0 to (Length(obj) div 6) - 1 do
  begin
    try
      pos.X := StrToInt(copy(obj, j * 6 + 1, 3));
      pos.Y := StrToInt(copy(obj, j * 6 + 4, 3));
    except
      continue;
    end;
    Self.JCClick.Add(pos);
  end; // for j

  // KPopisek
  obj := ini.ReadString(key, 'P', '');
  Self.KPopisek.Clear();
  for j := 0 to (Length(obj) div 6) - 1 do
  begin
    try
      pos.X := StrToIntDef(copy(obj, j * 6 + 1, 3), 0);
      pos.Y := StrToIntDef(copy(obj, j * 6 + 4, 3), 0);
    except
      continue;
    end;
    Self.KPopisek.Add(pos);
  end; // for j

  // soupravy
  obj := ini.ReadString(key, 'Spr', '');
  Self.Soupravy.Clear();
  for j := 0 to (Length(obj) div 6) - 1 do
  begin
    try
      pos.X := StrToIntDef(copy(obj, j * 6 + 1, 3), 0);
      pos.Y := StrToIntDef(copy(obj, j * 6 + 4, 3), 0);
    except
      continue;
    end;
    Self.Soupravy.Add(pos);
  end; // for j

  // Nazev
  Self.KpopisekStr := ini.ReadString(key, 'N', '');
  Self.Vetve.Clear();

  // vetve
  vetevCount := ini.ReadInteger(key, 'VC', 0);
  for j := 0 to vetevCount - 1 do
  begin
    obj := ini.ReadString(key, 'V' + IntToStr(j), '');

    vetev.node1.vyh := StrToIntDef(copy(obj, 0, 3), 0);
    vetev.node1.ref_plus := StrToIntDef(copy(obj, 4, 2), 0);
    vetev.node1.ref_minus := StrToIntDef(copy(obj, 6, 2), 0);

    vetev.node2.vyh := StrToIntDef(copy(obj, 8, 3), 0);
    vetev.node2.ref_plus := StrToIntDef(copy(obj, 11, 2), 0);
    vetev.node2.ref_minus := StrToIntDef(copy(obj, 13, 2), 0);

    obj := RightStr(obj, Length(obj) - 14);

    SetLength(vetev.Symbols, Length(obj) div 9);

    for k := 0 to Length(vetev.Symbols) - 1 do
    begin
      vetev.Symbols[k].Position.X := StrToIntDef(copy(obj, 9 * k + 1, 3), 0);
      vetev.Symbols[k].Position.Y := StrToIntDef(copy(obj, (9 * k + 4), 3), 0);
      vetev.Symbols[k].SymbolID := StrToIntDef(copy(obj, (9 * k + 7), 3), 0);
      if (version < _FILEVERSION_20) then
        vetev.Symbols[k].SymbolID := TranscodeSymbolFromBpnlV3(vetev.Symbols[k].SymbolID);
    end;

    Self.Vetve.Add(vetev);
  end; // for j
end;

procedure TUsek.Save(ini: TMemIniFile; key: string);
var obj: string;
  sym: TReliefSym;
  point: TPoint;
  vetev: TVetev;
  i: Integer;
begin
  inherited;

  // root
  if (Self.IsVyhybka) then
    ini.WriteString(key, 'R', GetPos(Self.Root));

  if (Self.DKStype <> dksNone) then
    ini.WriteInteger(key, 'DKS', Integer(Self.DKStype));

  // objekty
  obj := '';
  for sym in Self.Symbols do
    obj := obj + Format('%.3d%.3d%.2d', [sym.Position.X, sym.Position.Y, sym.SymbolID]);
  if (obj <> '') then
    ini.WriteString(key, 'S', obj);

  // JCClick
  obj := '';
  for point in Self.JCClick do
    obj := obj + Format('%.3d%.3d', [point.X, point.Y]);
  if (obj <> '') then
    ini.WriteString(key, 'C', obj);

  // KPopisek
  obj := '';
  for point in Self.KPopisek do
    obj := obj + Format('%.3d%.3d', [point.X, point.Y]);
  if (obj <> '') then
    ini.WriteString(key, 'P', obj);

  // soupravy
  obj := '';
  for point in Self.Soupravy do
    obj := obj + Format('%.3d%.3d', [point.X, point.Y]);
  if (obj <> '') then
    ini.WriteString(key, 'Spr', obj);

  // Nazev
  if (Self.KpopisekStr <> '') then
    ini.WriteString(key, 'N', Self.KpopisekStr);

  // vetve
  if (Self.Vetve.Count > 0) then
    ini.WriteInteger(key, 'VC', Self.Vetve.Count);

  for i := 0 to Self.Vetve.Count - 1 do
  begin
    vetev := Self.Vetve[i];
    if (vetev.node1.vyh < 0) then
      obj := Format('%.2d', [vetev.node1.vyh])
    else
      obj := Format('%.3d', [vetev.node1.vyh]);

    if (vetev.node1.ref_plus < 0) then
      obj := obj + Format('%.1d', [vetev.node1.ref_plus])
    else
      obj := obj + Format('%.2d', [vetev.node1.ref_plus]);

    if (vetev.node1.ref_minus < 0) then
      obj := obj + Format('%.1d', [vetev.node1.ref_minus])
    else
      obj := obj + Format('%.2d', [vetev.node1.ref_minus]);

    /// /////

    if (vetev.node2.vyh < 0) then
      obj := obj + Format('%.2d', [vetev.node2.vyh])
    else
      obj := obj + Format('%.3d', [vetev.node2.vyh]);

    if (vetev.node2.ref_plus < 0) then
      obj := obj + Format('%.1d', [vetev.node2.ref_plus])
    else
      obj := obj + Format('%.2d', [vetev.node2.ref_plus]);

    if (vetev.node2.ref_minus < 0) then
      obj := obj + Format('%.1d', [vetev.node2.ref_minus])
    else
      obj := obj + Format('%.2d', [vetev.node2.ref_minus]);

    /// /////

    for sym in vetev.Symbols do
      obj := obj + Format('%.3d%.3d%.3d', [sym.Position.X, sym.Position.Y, sym.SymbolID]);
    ini.WriteString(key, 'V' + IntToStr(i), obj);
  end; // for j
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TUsek.Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
  mode: TMode);
var pos: TPoint;
  sym: TReliefSym;
  color: SymbolColor;
begin
  if (selected) then
  begin
    for pos in Self.JCClick do
      SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, pos, _S_FULL, scYellow);

    for pos in Self.KPopisek do
      SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, pos, _S_KC, scLime);

    for pos in Self.Soupravy do
      SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, pos, _S_FULL, scBlue);

    for sym in Self.Symbols do
      SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, sym.Position, sym.SymbolID, colors.Selected);

    SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Root, _S_CIRCLE, colors.Selected);
  end else begin
    for pos in Self.JCClick do
      SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, pos, _S_KC, scLime);

    for pos in Self.KPopisek do
      SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, pos, _S_FULL, scYellow);

    for pos in Self.Soupravy do
      SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, pos, _S_FULL, scBlue);

    if (mode = TMode.dmBloky) then
    begin
      case (Self.Blok) of
        -1: color := colors.Alert;
        -2: color := colors.IntUnassigned;
      else
        color := colors.Normal;
      end;
    end else begin
      if ((Self.IsVyhybka) and (Self.Root.X = -1)) then
        color := colors.Alert
      else
        color := colors.Normal;
    end;

    for sym in Self.Symbols do
      SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, sym.Position, sym.SymbolID, color);

    SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Root, _S_CIRCLE, scAqua);
  end; // else Selected = i
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
