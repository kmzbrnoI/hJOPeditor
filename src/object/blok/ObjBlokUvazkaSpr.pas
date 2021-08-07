unit ObjBlokUvazkaSpr;

interface

uses ObjBlok, Types, IniFiles, Global, PGraphics, Graphics, symbolHelper;

type

  TUvazkaSprVertDir = (top = 0, bottom = 1);

  TUvazkaSpr = class(TGraphBlok)
    Pos: TPoint;
    vertical_dir: TUvazkaSprVertDir;
    spr_cnt: Integer;

    constructor Create(index: Integer);
    procedure Load(ini: TMemIniFile; key: string; version: Word); override;
    procedure Save(ini: TMemIniFile; key: string); override;
    procedure Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
      mode: TMode); override;
  end;

implementation

/// /////////////////////////////////////////////////////////////////////////////

constructor TUvazkaSpr.Create(index: Integer);
begin
  inherited;
  Self.typ := TBlkType.uvazka_spr;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TUvazkaSpr.Load(ini: TMemIniFile; key: string; version: Word);
begin
  inherited;

  Self.Pos.X := ini.ReadInteger(key, 'X', 0);
  Self.Pos.Y := ini.ReadInteger(key, 'Y', 0);
  Self.vertical_dir := TUvazkaSprVertDir(ini.ReadInteger(key, 'VD', 0));
  Self.spr_cnt := ini.ReadInteger(key, 'C', 1);
end;

procedure TUvazkaSpr.Save(ini: TMemIniFile; key: string);
begin
  inherited;

  ini.WriteInteger(key, 'X', Self.Pos.X);
  ini.WriteInteger(key, 'Y', Self.Pos.Y);
  ini.WriteInteger(key, 'VD', Integer(Self.vertical_dir));
  ini.WriteInteger(key, 'C', Self.spr_cnt);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TUvazkaSpr.Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors;
  selected: boolean; mode: TMode);
var color: SymbolColor;
begin
  if (selected) then
  begin
    DrawObject.Canvas.Pen.color := clRed;
    color := colors.Selected;
  end else begin
    case (Self.blok) of
      -1:
        begin
          color := colors.Alert;
          DrawObject.Canvas.Pen.color := clAqua;
        end;
      -2:
        begin
          color := colors.IntUnassigned;
          DrawObject.Canvas.Pen.color := clWhite;
        end
    else
      color := scYellow;
      DrawObject.Canvas.Pen.color := clYellow;
    end;
  end;

  DrawObject.Canvas.Brush.color := clBlack;

  case (Self.vertical_dir) of
    TUvazkaSprVertDir.top:
      begin
        DrawObject.Canvas.Rectangle(Self.Pos.X * _Symbol_Sirka, Self.Pos.Y * _Symbol_Vyska + _Symbol_Vyska - 1,
          (Self.Pos.X + _Uvazka_Spr_Sirka) * _Symbol_Sirka - 1, (Self.Pos.Y - Self.spr_cnt + 1) * _Symbol_Vyska);
      end;

    TUvazkaSprVertDir.bottom:
      begin
        DrawObject.Canvas.Rectangle(Self.Pos.X * _Symbol_Sirka, Self.Pos.Y * _Symbol_Vyska,
          (Self.Pos.X + _Uvazka_Spr_Sirka) * _Symbol_Sirka - 1, (Self.Pos.Y + Self.spr_cnt) * _Symbol_Vyska - 1);
      end;
  end;

  SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Pos, _S_LINKER_TRAIN, color);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
