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
    procedure B_ApplyClick(Sender: TObject);
    procedure B_StornoClick(Sender: TObject);
    procedure B_ProchazetClick(Sender: TObject);
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

uses fMain;

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
 ReliefOptions.UseData(F_Hlavni.Relief);

 Self.Close;
end;//procedure

procedure TF_ReliefOptions.B_ProchazetClick(Sender: TObject);
begin
 if (Self.OD_Open.Execute(Self.Handle)) then
  begin
   Self.E_BlocksFileName.Text := ExtractRelativePath(ExtractFilePath(Application.ExeName), Self.OD_Open.FileName);
  end;//(Self.OD_Open.Execute(Self.Handle))
end;//procedure

procedure TF_ReliefOptions.B_StornoClick(Sender: TObject);
begin
 Self.Close;
end;//procedure

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
