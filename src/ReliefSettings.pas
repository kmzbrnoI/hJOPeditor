unit ReliefSettings;

interface

uses SysUtils, Graphics, IniFiles, Panel;

type
 TReliefOptions=class

  private const
   _Def_Color_Pozadi = clBlack;
   _Def_Color_Mrizka = clGray;
   _Def_Color_Kurzor = clYellow;
   _Def_Color_Kurzor_OnObject = clRed;
   _Def_Color_Kurzor_Operation = clFuchsia;
   _Def_Color_Objects = 1;
   _Def_Mrizka = true;


  private
   Colors:record
    Mrizka,Pozadi,Kurzor,KurzorOnObject,KurzorOperation:TColor;
   end;//Colors

   FBlockFile:string;

   aMrizka:boolean;
   Files:record
    IniFile:TMemIniFile;
   end;//Files
  public
    procedure LoadData(FileName:string);
    procedure SaveData(FileName:string);
    procedure UseData(Relief:TRelief);

    property MrizkaColor:TColor read Colors.Mrizka write Colors.Mrizka;
    property PozadiColor:TColor read Colors.Pozadi write Colors.Pozadi;
    property KurzorColor:TColor read Colors.Kurzor write Colors.Kurzor;
    property KurzorOnObjectColor:TColor read Colors.KurzorOnObject write Colors.KurzorOnObject;
    property KurzorOperation:TColor read Colors.KurzorOperation write Colors.KurzorOperation;
    property Mrizka:boolean read aMrizka write aMrizka;
    property BlockFile:string read FBlockFile write FBlockFile;
 end;//TReliefOptions

var
 ReliefOptions:TReliefOptions;

implementation

procedure TReliefOptions.LoadData(FileName:string);
begin
 Self.Files.IniFile := TMemIniFile.Create(FileName);

 Self.Colors.Mrizka           := StringToColor(Self.Files.IniFile.ReadString('Colors','Mrizka',ColorToString(_Def_Color_Mrizka)));
 Self.Colors.Pozadi           := StringToColor(Self.Files.IniFile.ReadString('Colors','Pozadi',ColorToString(_Def_Color_Pozadi)));
 Self.Colors.Kurzor           := StringToColor(Self.Files.IniFile.ReadString('Colors','Kurzor',ColorToString(_Def_Color_Kurzor)));
 Self.Colors.KurzorOnObject   := StringToColor(Self.Files.IniFile.ReadString('Colors','KurzorOnObject',ColorToString(_Def_Color_Kurzor_OnObject)));
 Self.Colors.KurzorOperation  := StringToColor(Self.Files.IniFile.ReadString('Colors','KurzorOperation',ColorToString(_Def_Color_Kurzor_Operation)));
 Self.Mrizka                  := Self.Files.IniFile.ReadBool('Obecne','Mrizka',_Def_Mrizka);
 Self.FBlockFile              := Self.Files.IniFile.ReadString('Obecne','BlockFile','');

 Self.Files.IniFile.Free;
end;//function

procedure TReliefOptions.SaveData(FileName:string);
begin
 Self.Files.IniFile := TMemIniFile.Create(FileName);

 Self.Files.IniFile.WriteString('Colors','Mrizka',ColorToString(Self.Colors.Mrizka));
 Self.Files.IniFile.WriteString('Colors','Pozadi',ColorToString(Self.Colors.Pozadi));
 Self.Files.IniFile.WriteString('Colors','Kurzor',ColorToString(Self.Colors.Kurzor));
 Self.Files.IniFile.WriteString('Colors','KurzorOnObject',ColorToString(Self.Colors.KurzorOnObject));
 Self.Files.IniFile.WriteString('Colors','KurzorOperation',ColorToString(Self.Colors.KurzorOperation));
 Self.Files.IniFile.WriteBool('Obecne','Mrizka',Self.Mrizka);
 Self.Files.IniFile.WriteString('Obecne','BlockFile', Self.FBlockFile);

 Self.Files.IniFile.UpdateFile;
 Self.Files.IniFile.Free;
end;//procedure

procedure TReliefOptions.UseData(Relief:TRelief);
begin
 Relief.MrizkaColor         := Self.MrizkaColor;
 Relief.PozadiColor         := Self.PozadiColor;
 Relief.KurzorColor         := Self.KurzorColor;
 Relief.KurzorOnObjectColor := Self.KurzorOnObjectColor;
 Relief.KurzorOperation     := Self.KurzorOperation;
 Relief.Mrizka              := Self.Mrizka;
end;//procedure

end.//unit
