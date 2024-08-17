unit ObjBlokUsek;

interface

uses ObjBlok, Generics.Collections, Types, symbolHelper, vetev, IniFiles,
  SysUtils, StrUtils, Global, Graphics, PGraphics, System.Generics.Defaults;

type

  TDKSType = (dksNone = 0, dksTop = 1, dksBottom = 2);

  TTrack = class(TGraphBlok)
    Root: TPoint;
    IsTurnout: boolean; // pomocny flag pro vykreslovani v modu korenu
    Symbols: TList<TReliefSym>; // pokud je v useku vykolejka, je zde ulozena jako klasicky symbol
    SymbolsSorted: TList<TReliefSym>;
    JCClick: TList<TPoint>;
    labels: TList<TPoint>;
    trains: TList<TPoint>;
    caption: string;
    DKStype: TDKSType;

    branches: TList<TTrackBranch>; // vetve useku
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
    procedure FillSymbolsSorted();

    function IsSymbolsSortedEq(symbolsSorted: TList<TReliefSym>): Boolean;
    function GetEqTrack(blocks: TList<TGraphBlok>): TTrack;
  end;

implementation

uses ReliefObjects;

/// /////////////////////////////////////////////////////////////////////////////

constructor TTrack.Create(index: Integer);
begin
  inherited;
  Self.typ := TBlkType.track;
  Self.Symbols := TList<TReliefSym>.Create();
  Self.SymbolsSorted := TList<TReliefSym>.Create(TComparer<TReliefSym>.Construct(CompareReliefSymPos));
  Self.JCClick := TList<TPoint>.Create();
  Self.labels := TList<TPoint>.Create();
  Self.trains := TList<TPoint>.Create();
  Self.branches := TList<TTrackBranch>.Create();
end;

destructor TTrack.Destroy();
begin
  Self.Symbols.Free();
  Self.SymbolsSorted.Free();
  Self.JCClick.Free();
  Self.labels.Free();
  Self.trains.Free();
  Self.branches.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTrack.Load(ini: TMemIniFile; key: string; version: Word);
var obj: string;
begin
  inherited;

  Self.Root := GetPos(ini.ReadString(key, 'R', '-1;-1'));
  Self.DKStype := TDKSType(ini.ReadInteger(key, 'DKS', Integer(dksNone)));

  // Symbols
  Self.Symbols.Clear();
  obj := ini.ReadString(key, 'S', '');
  for var j: Integer := 0 to (Length(obj) div 8) - 1 do
  begin
    var symbol: TReliefSym;
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
  for var j: Integer := 0 to (Length(obj) div 6) - 1 do
  begin
    var pos: TPoint;
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
  Self.labels.Clear();
  for var j: Integer := 0 to (Length(obj) div 6) - 1 do
  begin
    var pos: TPoint;
    try
      pos.X := StrToIntDef(copy(obj, j * 6 + 1, 3), 0);
      pos.Y := StrToIntDef(copy(obj, j * 6 + 4, 3), 0);
    except
      continue;
    end;
    Self.labels.Add(pos);
  end; // for j

  // soupravy
  obj := ini.ReadString(key, 'Spr', '');
  Self.trains.Clear();
  for var j: Integer := 0 to (Length(obj) div 6) - 1 do
  begin
    var pos: TPoint;
    try
      pos.X := StrToIntDef(copy(obj, j * 6 + 1, 3), 0);
      pos.Y := StrToIntDef(copy(obj, j * 6 + 4, 3), 0);
    except
      continue;
    end;
    Self.trains.Add(pos);
  end; // for j

  // Nazev
  Self.caption := ini.ReadString(key, 'N', '');
  Self.branches.Clear();

  // vetve
  var vetevCount: Integer := ini.ReadInteger(key, 'VC', 0);
  for var j: Integer := 0 to vetevCount - 1 do
  begin
    obj := ini.ReadString(key, 'V' + IntToStr(j), '');

    var vetev: TTrackBranch;
    vetev.node1.vyh := StrToIntDef(copy(obj, 0, 3), 0);
    vetev.node1.ref_plus := StrToIntDef(copy(obj, 4, 2), 0);
    vetev.node1.ref_minus := StrToIntDef(copy(obj, 6, 2), 0);

    vetev.node2.vyh := StrToIntDef(copy(obj, 8, 3), 0);
    vetev.node2.ref_plus := StrToIntDef(copy(obj, 11, 2), 0);
    vetev.node2.ref_minus := StrToIntDef(copy(obj, 13, 2), 0);

    obj := RightStr(obj, Length(obj) - 14);

    SetLength(vetev.Symbols, Length(obj) div 9);

    for var k: Integer := 0 to Length(vetev.Symbols) - 1 do
    begin
      vetev.Symbols[k].Position.X := StrToIntDef(copy(obj, 9 * k + 1, 3), 0);
      vetev.Symbols[k].Position.Y := StrToIntDef(copy(obj, (9 * k + 4), 3), 0);
      vetev.Symbols[k].SymbolID := StrToIntDef(copy(obj, (9 * k + 7), 3), 0);
      if (version < _FILEVERSION_20) then
        vetev.Symbols[k].SymbolID := TranscodeSymbolFromBpnlV3(vetev.Symbols[k].SymbolID);
    end;

    Self.branches.Add(vetev);
  end; // for j
end;

procedure TTrack.Save(ini: TMemIniFile; key: string);
begin
  inherited;

  // root
  if (Self.IsTurnout) then
    ini.WriteString(key, 'R', GetPos(Self.Root));

  if (Self.DKStype <> dksNone) then
    ini.WriteInteger(key, 'DKS', Integer(Self.DKStype));

  // objekty
  begin
    var obj: string := '';
    for var sym in Self.Symbols do
      obj := obj + Format('%.3d%.3d%.2d', [sym.Position.X, sym.Position.Y, sym.SymbolID]);
    if (obj <> '') then
      ini.WriteString(key, 'S', obj);
  end;

  // JCClick
  begin
    var obj: string := '';
    for var point: TPoint in Self.JCClick do
      obj := obj + Format('%.3d%.3d', [point.X, point.Y]);
    if (obj <> '') then
      ini.WriteString(key, 'C', obj);
  end;

  // KPopisek
  begin
    var obj: string := '';
    for var point: TPoint in Self.labels do
      obj := obj + Format('%.3d%.3d', [point.X, point.Y]);
    if (obj <> '') then
      ini.WriteString(key, 'P', obj);
  end;

  // soupravy
  begin
    var obj: string := '';
    for var point: TPoint in Self.trains do
      obj := obj + Format('%.3d%.3d', [point.X, point.Y]);
    if (obj <> '') then
      ini.WriteString(key, 'Spr', obj);
  end;

  // Nazev
  if (Self.caption <> '') then
    ini.WriteString(key, 'N', Self.caption);

  // vetve
  if (Self.branches.Count > 0) then
    ini.WriteInteger(key, 'VC', Self.branches.Count);

  for var i: Integer := 0 to Self.branches.Count - 1 do
  begin
    var obj: string := '';
    var vetev := Self.branches[i];
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

    for var sym in vetev.Symbols do
      obj := obj + Format('%.3d%.3d%.3d', [sym.Position.X, sym.Position.Y, sym.SymbolID]);
    ini.WriteString(key, 'V' + IntToStr(i), obj);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTrack.Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
  mode: TMode);
begin
  if (selected) then
  begin
    for var pos: TPoint in Self.JCClick do
      SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, pos, _S_FULL, scYellow);

    for var pos: TPoint in Self.labels do
      SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, pos, _S_KC, scLime);

    for var pos: TPoint in Self.trains do
      SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, pos, _S_FULL, scBlue);

    for var sym: TReliefSym in Self.Symbols do
      SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, sym.Position, sym.SymbolID, colors.Selected);

    SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Root, _S_CIRCLE, colors.Selected);
  end else begin
    for var pos: TPoint in Self.JCClick do
      SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, pos, _S_KC, scLime);

    for var pos: TPoint in Self.labels do
      SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, pos, _S_FULL, scYellow);

    for var pos: TPoint in Self.trains do
      SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, pos, _S_FULL, scBlue);

    var color: SymbolColor;
    if (mode = TMode.dmBloky) then
    begin
      case (Self.block) of
        -1: color := colors.Alert;
        -2: color := colors.IntUnassigned;
      else
        color := colors.Normal;
      end;
    end else begin
      if ((Self.IsTurnout) and (Self.Root.X = -1)) then
        color := colors.Alert
      else
        color := colors.Normal;
    end;

    for var sym: TReliefSym in Self.Symbols do
      SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, sym.Position, sym.SymbolID, color);

    SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Root, _S_CIRCLE, scAqua);
  end; // else Selected = i
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTrack.FillSymbolsSorted();
begin
  Self.SymbolsSorted.Clear();
  Self.SymbolsSorted.AddRange(Self.Symbols);
  Self.SymbolsSorted.Sort();
end;

function TTrack.IsSymbolsSortedEq(symbolsSorted: TList<TReliefSym>): Boolean;
begin
  if (Self.SymbolsSorted.Count <> symbolsSorted.Count) then
    Exit(False);
  for var i := 0 to Self.SymbolsSorted.Count-1 do
    if (Self.SymbolsSorted[i].Position <> symbolsSorted[i].Position) then
      Exit(False);
  Result := True;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TTrack.GetEqTrack(blocks: TList<TGraphBlok>): TTrack;
begin
  for var block: TGraphBlok in blocks do
    if ((block.typ = TBlkType.track) and (TTrack(block).IsSymbolsSortedEq(Self.SymbolsSorted))) then
      Exit(TTrack(block));
  Result := nil;
end;

end.
