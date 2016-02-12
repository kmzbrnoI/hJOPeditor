// JCL_DEBUG_EXPERT_INSERTJDBG OFF
program hJOPeditor;

uses
  Forms,
  fMain in 'fMain.pas' {F_Hlavni},
  ReliefObjects in 'ReliefObjects.pas',
  Panel in 'Panel.pas',
  fNewRelief in 'fNewRelief.pas' {F_NewRelief},
  ReliefBitmap in 'ReliefBitmap.pas',
  fChangeRelief in 'fChangeRelief.pas' {F_ReliefProperties},
  Global in 'Global.pas',
  ReliefSettings in 'ReliefSettings.pas',
  fReliefSettings in 'fReliefSettings.pas' {F_ReliefOptions},
  ReliefText in 'ReliefText.pas',
  fPopiskek in 'fPopiskek.pas' {F_Popisek},
  VektorBasedObject in 'VektorBasedObject.pas',
  ReliefBitmapSymbols in 'ReliefBitmapSymbols.pas',
  BitmapToObj in 'BitmapToObj.pas',
  fBlockEdit in 'fBlockEdit.pas' {F_BlockEdit},
  OblastRizeni in 'OblastRizeni.pas',
  fOREdit in 'fOREdit.pas' {F_OREdit},
  fDataCheck in 'fDataCheck.pas' {F_DataCheck},
  ReliefZT in 'ReliefZT.pas',
  ObjToZT in 'ObjToZT.pas',
  PGraphics in 'PGraphics.pas',
  RPConst in 'RPConst.pas';

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
  if (ParamCount = 1) then
   begin
    //open the file
    F_Hlavni.OpenFile(ParamStr(1));
   end;

  Application.Run;
end.
