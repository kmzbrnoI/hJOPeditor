unit fReliefSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ReliefSettings, StdCtrls, ExtCtrls;

type
  TF_ReliefOptions = class(TForm)
    GB_Colors: TGroupBox;
    CB_Mrizka: TColorBox;
    CB_Kurzor: TColorBox;
    CB_KurzorOperation: TColorBox;
    CB_Pozadi: TColorBox;
    CB_KurzorOnObject: TColorBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    B_Apply: TButton;
    B_Storno: TButton;
    Label6: TLabel;
    E_BlocksFileName: TEdit;
    B_Prochazet: TButton;
    OD_Open: TOpenDialog;
    CHB_RelativePath: TCheckBox;
    procedure B_ApplyClick(Sender: TObject);
    procedure B_StornoClick(Sender: TObject);
    procedure B_ProchazetClick(Sender: TObject);
    procedure CHB_RelativePathClick(Sender: TObject);
  private
    { Private declarations }
   const
    _Config_File = 'Config.ini';
  public
    procedure OpenForm;
  end;

var
  F_ReliefOptions: TF_ReliefOptions;

implementation

uses fMain, fBlockEdit;

{$R *.dfm}

procedure TF_ReliefOptions.B_ApplyClick(Sender: TObject);
begin
 ReliefOptions.MrizkaColor          := Self.CB_Mrizka.Selected;
 ReliefOptions.PozadiColor          := Self.CB_Pozadi.Selected;
 ReliefOptions.KurzorColor          := Self.CB_Kurzor.Selected;
 ReliefOptions.KurzorOnObjectColor  := Self.CB_KurzorOnObject.Selected;
 ReliefOptions.KurzorOperation      := Self.CB_KurzorOperation.Selected;
 ReliefOptions.BlockFile            := Self.E_BlocksFileName.Text;

 ReliefOptions.SaveData(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+_Config_File);

 if (Assigned(F_Hlavni.Relief)) then
   ReliefOptions.UseData(F_Hlavni.Relief);

 try
   F_BlockEdit.Bloky.LoadData(ReliefOptions.BlockFile);
 except
   on E:Exception do
     Application.MessageBox(PChar('Chyba pøi naèítáni souboru s bloky technologie:'+#13#10+E.Message),
                            'Chyba', MB_OK OR MB_ICONWARNING);
 end;

 Self.Close;
end;//procedure

procedure TF_ReliefOptions.B_ProchazetClick(Sender: TObject);
begin
 Self.OD_Open.InitialDir := ExtractFilePath(Self.E_BlocksFileName.Text);
 if (Self.OD_Open.Execute(Self.Handle)) then
  begin
   if (Self.CHB_RelativePath.Checked) then
    Self.E_BlocksFileName.Text := ExtractRelativePath(ExtractFilePath(Application.ExeName), Self.OD_Open.FileName)
   else
    Self.E_BlocksFileName.Text := Self.OD_Open.FileName;
  end;//(Self.OD_Open.Execute(Self.Handle))
end;//procedure

procedure TF_ReliefOptions.B_StornoClick(Sender: TObject);
begin
 Self.Close;
end;

procedure TF_ReliefOptions.CHB_RelativePathClick(Sender: TObject);
begin
 if (Self.CHB_RelativePath.Checked) then
  Self.E_BlocksFileName.Text := ExtractRelativePath(ExtractFilePath(Application.ExeName), Self.E_BlocksFileName.Text)
 else
  Self.E_BlocksFileName.Text := ExpandFileName(Self.E_BlocksFileName.Text);
end;

//procedure

procedure TF_ReliefOptions.OpenForm;
begin
 Self.CB_Mrizka.Selected          := ReliefOptions.MrizkaColor;
 Self.CB_Pozadi.Selected          := ReliefOptions.PozadiColor;
 Self.CB_Kurzor.Selected          := ReliefOptions.KurzorColor;
 Self.CB_KurzorOnObject.Selected  := ReliefOptions.KurzorOnObjectColor;
 Self.CB_KurzorOperation.Selected := ReliefOptions.KurzorOperation;
 Self.E_BlocksFileName.Text       := ReliefOptions.BlockFile;

 Self.ShowModal;
end;//procedure

end.//unit
