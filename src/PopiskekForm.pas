unit PopiskekForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Main, ReliefText;

type
  TF_Popisek = class(TForm)
    B_Apply: TButton;
    B_Storno: TButton;
    Label1: TLabel;
    E_Text: TEdit;
    GB_Color: TGroupBox;
    RB_Col0: TRadioButton;
    RB_Col1: TRadioButton;
    RB_Col2: TRadioButton;
    RB_Col3: TRadioButton;
    RB_Col4: TRadioButton;
    RB_Col5: TRadioButton;
    RB_Col6: TRadioButton;
    RB_Col7: TRadioButton;
    S_Col0: TShape;
    S_Col1: TShape;
    S_Col2: TShape;
    S_Col3: TShape;
    S_Col4: TShape;
    S_Col5: TShape;
    S_Col6: TShape;
    S_Col7: TShape;
    procedure B_StornoClick(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
    procedure RB_Col0Click(Sender: TObject);
    procedure E_TextKeyPress(Sender: TObject; var Key: Char);
  private
   //private
  public
   PopisekText:string;
   PopisekColor:ShortInt;

    procedure NewPopisek;
    procedure OpenPopisek(aText:string;aColor:Byte);
  end;

var
  F_Popisek: TF_Popisek;

implementation

{$R *.dfm}

procedure TF_Popisek.B_ApplyClick(Sender: TObject);
begin
 if (Length(Self.E_Text.Text) < 1) then
  begin
   Application.MessageBox('Zadejte text!','Chyba',MB_OK OR MB_ICONWARNING);
   Exit;
  end;
 if (Self.PopisekColor = -1) then
  begin
   Application.MessageBox('Vyberte barvu!','Chyba',MB_OK OR MB_ICONWARNING);
   Exit;
  end;

 Self.PopisekText := Self.E_Text.Text;

 Self.Close;
end;//procedure

procedure TF_Popisek.B_StornoClick(Sender: TObject);
begin
 Self.PopisekColor := -1;

 Self.Close;
end;//procedure

procedure TF_Popisek.E_TextKeyPress(Sender: TObject; var Key: Char);
begin
 case Key of
  #33..#90:;     // cislice, velka pismena, specialni znaky
  #97..#126:;    // mala pismena, specialni znaky
  #8,#32:;       // backspace, space

  'š',
  '',
  'ž',
  'á',
  'è',
  'é',
  'ì',
  'í',
  'ï',
  'ò',
  'ó',
  'ø',
  'ù',
  'ú',
  'ý',

  'Š',
  '',
  'Ž',
  'Á',
  'È',
  'É',
  'Ì',
  'Í',
  'Ï',
  'Ò',
  'Ó',
  'Ø',
  'Ù',
  'Ú',
  'Ý':;

  #13:B_ApplyClick(Self);
 else//case
  Key := #0;
 end;
end;//procedure

procedure TF_Popisek.NewPopisek;
begin
 Self.E_Text.Text     := '';
 Self.RB_Col1.Checked := true;

 Self.PopisekText  := '';
 Self.PopisekColor := 1;

 Self.Caption := 'Nový popisek';
 Self.ShowModal;
end;//procedure

procedure TF_Popisek.OpenPopisek(aText:string;aColor:Byte);
begin
 Self.PopisekText  := aText;
 Self.PopisekColor := aColor;

 Self.E_Text.Text := aText;
 case (aColor) of
  0:Self.RB_Col0.Checked := true;
  1:Self.RB_Col1.Checked := true;
  2:Self.RB_Col2.Checked := true;
  3:Self.RB_Col3.Checked := true;
  4:Self.RB_Col4.Checked := true;
  5:Self.RB_Col5.Checked := true;
  6:Self.RB_Col6.Checked := true;
  7:Self.RB_Col7.Checked := true;
 end;//case

 Self.Caption := 'Editovat popisek';
 Self.ShowModal;
end;//procedure

procedure TF_Popisek.RB_Col0Click(Sender: TObject);
begin
 Self.PopisekColor := (Sender as TRadioButton).Tag;
end;//procedure

end.//unit
