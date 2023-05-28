unit ReliefSettings;

interface

uses SysUtils, Graphics, IniFiles, Panel;

type
  TReliefOptions = class

  private const
    _Def_Color_Pozadi = clBlack;
    _Def_Color_Mrizka = clGray;
    _Def_Color_Kurzor = clYellow;
    _Def_Color_Kurzor_OnObject = clRed;
    _Def_Color_Kurzor_Operation = clFuchsia;
    _Def_Color_Objects = 1;
    _Def_Mrizka = true;

  private
    Colors: record
      Mrizka, Pozadi, Kurzor, KurzorOnObject, KurzorOperation: TColor;
    end; // Colors

    FBlockFile: string;
    aMrizka: boolean;
  public
    procedure LoadData(FileName: string);
    procedure SaveData(FileName: string);
    procedure UseData(Relief: TRelief);

    property MrizkaColor: TColor read Colors.Mrizka write Colors.Mrizka;
    property PozadiColor: TColor read Colors.Pozadi write Colors.Pozadi;
    property KurzorColor: TColor read Colors.Kurzor write Colors.Kurzor;
    property KurzorOnObjectColor: TColor read Colors.KurzorOnObject write Colors.KurzorOnObject;
    property KurzorOperation: TColor read Colors.KurzorOperation write Colors.KurzorOperation;
    property Mrizka: boolean read aMrizka write aMrizka;
    property BlockFile: string read FBlockFile write FBlockFile;
  end; // TReliefOptions

var
  ReliefOptions: TReliefOptions;

implementation

procedure TReliefOptions.LoadData(FileName: string);
var ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(FileName, TEncoding.UTF8);

  try
    Self.Colors.Mrizka := StringToColor(ini.ReadString('Colors', 'Mrizka',
      ColorToString(_Def_Color_Mrizka)));
    Self.Colors.Pozadi := StringToColor(ini.ReadString('Colors', 'Pozadi',
      ColorToString(_Def_Color_Pozadi)));
    Self.Colors.Kurzor := StringToColor(ini.ReadString('Colors', 'Kurzor',
      ColorToString(_Def_Color_Kurzor)));
    Self.Colors.KurzorOnObject := StringToColor(ini.ReadString('Colors', 'KurzorOnObject',
      ColorToString(_Def_Color_Kurzor_OnObject)));
    Self.Colors.KurzorOperation := StringToColor(ini.ReadString('Colors', 'KurzorOperation',
      ColorToString(_Def_Color_Kurzor_Operation)));
    Self.Mrizka := ini.ReadBool('Obecne', 'Mrizka', _Def_Mrizka);
    Self.FBlockFile := ini.ReadString('Obecne', 'BlockFile', '');
  finally
    ini.Free();
  end;
end;

procedure TReliefOptions.SaveData(FileName: string);
var ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(FileName, TEncoding.UTF8);

  try
    ini.WriteString('Colors', 'Mrizka', ColorToString(Self.Colors.Mrizka));
    ini.WriteString('Colors', 'Pozadi', ColorToString(Self.Colors.Pozadi));
    ini.WriteString('Colors', 'Kurzor', ColorToString(Self.Colors.Kurzor));
    ini.WriteString('Colors', 'KurzorOnObject', ColorToString(Self.Colors.KurzorOnObject));
    ini.WriteString('Colors', 'KurzorOperation', ColorToString(Self.Colors.KurzorOperation));
    ini.WriteBool('Obecne', 'Mrizka', Self.Mrizka);
    ini.WriteString('Obecne', 'BlockFile', Self.FBlockFile);

    ini.UpdateFile();
  finally
    ini.Free();
  end;
end;

procedure TReliefOptions.UseData(Relief: TRelief);
begin
  Relief.MrizkaColor := Self.MrizkaColor;
  Relief.PozadiColor := Self.PozadiColor;
  Relief.KurzorColor := Self.KurzorColor;
  Relief.KurzorOnObjectColor := Self.KurzorOnObjectColor;
  Relief.KurzorOperation := Self.KurzorOperation;
  Relief.Mrizka := Self.Mrizka;
end;

initialization
  ReliefOptions := TReliefOptions.Create();
finalization
  FreeAndNil(ReliefOptions);

end.// unit
