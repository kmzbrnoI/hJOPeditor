// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
program hJOPeditor;

uses
  Forms,
  fMain in 'forms\fMain.pas' {F_Hlavni},
  ReliefObjects in 'object\ReliefObjects.pas',
  Panel in 'Panel.pas',
  fNewRelief in 'forms\fNewRelief.pas' {F_NewRelief},
  ReliefBitmap in 'bitmap\ReliefBitmap.pas',
  fChangeRelief in 'forms\fChangeRelief.pas' {F_ReliefProperties},
  Global in 'Global.pas',
  ReliefSettings in 'ReliefSettings.pas',
  fReliefSettings in 'forms\fReliefSettings.pas' {F_ReliefOptions},
  ReliefText in 'bitmap\ReliefText.pas',
  fPopiskek in 'forms\fPopiskek.pas' {F_Popisek},
  VektorBasedObject in 'bitmap\VektorBasedObject.pas',
  ReliefBitmapSymbols in 'bitmap\ReliefBitmapSymbols.pas',
  BitmapToObj in 'BitmapToObj.pas',
  fBlockEdit in 'forms\fBlockEdit.pas' {F_BlockEdit},
  OblastRizeni in 'OblastRizeni.pas',
  fOREdit in 'forms\fOREdit.pas' {F_OREdit},
  fDataCheck in 'forms\fDataCheck.pas' {F_DataCheck},
  ReliefZT in 'zt\ReliefZT.pas',
  ObjToZT in 'ObjToZT.pas',
  PGraphics in 'PGraphics.pas',
  VetveComputer in 'object\VetveComputer.pas',
  symbolHelper in 'symbolHelper.pas',
  vetev in 'object\vetev.pas',
  ObjBlok in 'object\blok\ObjBlok.pas',
  ObjBlokVyhybka in 'object\blok\ObjBlokVyhybka.pas',
  ObjBlokUsek in 'object\blok\ObjBlokUsek.pas',
  ObjBlokText in 'object\blok\ObjBlokText.pas',
  ObjBlokNavestidlo in 'object\blok\ObjBlokNavestidlo.pas',
  ObjBlokPomocny in 'object\blok\ObjBlokPomocny.pas',
  ObjBlokPrejezd in 'object\blok\ObjBlokPrejezd.pas',
  ObjBlokUvazkaSpr in 'object\blok\ObjBlokUvazkaSpr.pas',
  ObjBlokUvazka in 'object\blok\ObjBlokUvazka.pas',
  ObjBlokPst in 'object\blok\ObjBlokPst.pas',
  ObjBlokRozp in 'object\blok\ObjBlokRozp.pas',
  ObjBlokVykol in 'object\blok\ObjBlokVykol.pas',
  ownStrUtils in 'helpers\ownStrUtils.pas',
  fImportLog in 'forms\fImportLog.pas' {F_ImportLog},
  ObjBlokZamek in 'object\blok\ObjBlokZamek.pas',
  ReliefCommon in 'ReliefCommon.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'hJOPeditor';
  Application.CreateForm(TF_Hlavni, F_Hlavni);
  Application.CreateForm(TF_NewRelief, F_NewRelief);
  Application.CreateForm(TF_ReliefProperties, F_ReliefProperties);
  Application.CreateForm(TF_ReliefOptions, F_ReliefOptions);
  Application.CreateForm(TF_Popisek, F_Popisek);
  Application.CreateForm(TF_BlockEdit, F_BlockEdit);
  Application.CreateForm(TF_OREdit, F_OREdit);
  Application.CreateForm(TF_DataCheck, F_DataCheck);
  Application.CreateForm(TF_ImportLog, F_ImportLog);
  if (ParamCount = 1) then
   begin
    //open the file
    F_Hlavni.OpenFile(ParamStr(1));
   end;

  Application.Run;
end.
