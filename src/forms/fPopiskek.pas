unit fPopiskek;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, fMain, ReliefText;

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
    CHB_Blok_Popisek: TCheckBox;
    StaticText1: TStaticText;
    procedure B_StornoClick(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
    procedure RB_Col0Click(Sender: TObject);
    procedure E_TextKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
  private
    // private
  public
    PopisekText: string;
    PopisekColor: Integer;
    PopisekBlok: Boolean;

    procedure NewPopisek;
    procedure OpenPopisek(aText: string; popisek: TPanelLabel);
  end;

var
  F_Popisek: TF_Popisek;

implementation

uses symbolHelper;

{$R *.dfm}

procedure TF_Popisek.B_ApplyClick(Sender: TObject);
begin
  if (Length(Self.E_Text.Text) < 1) then
  begin
    Application.MessageBox('Zadejte text!', 'Chyba', MB_OK OR MB_ICONWARNING);
    Exit;
  end;
  if (Self.PopisekColor = -1) then
  begin
    Application.MessageBox('Vyberte barvu!', 'Chyba', MB_OK OR MB_ICONWARNING);
    Exit;
  end;

  Self.PopisekText := Self.E_Text.Text;
  Self.PopisekBlok := Self.CHB_Blok_Popisek.Checked;

  Self.Close();
end;

procedure TF_Popisek.B_StornoClick(Sender: TObject);
begin
  Self.PopisekColor := -1;
  Self.Close();
end;

procedure TF_Popisek.E_TextKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    #33 .. #90:
      ; // cislice, velka pismena, specialni znaky
    #97 .. #126:
      ; // mala pismena, specialni znaky
    #8, #32:
      ; // backspace, space

    'š', 'ť', 'ž', 'á', 'č', 'é', 'ě', 'í', 'ď', 'ň', 'ó', 'ř', 'ů', 'ú', 'ý',
      'Š', 'Ť', 'Ž', 'Á', 'Č', 'É', 'Ě', 'Í', 'Ď', 'Ň', 'Ó', 'Ř', 'Ů', 'Ú', 'Ý':
      ;

    #13:
      B_ApplyClick(Self);
  else // case
    Key := #0;
  end;
end;

procedure TF_Popisek.FormCreate(Sender: TObject);
begin
  Self.PopisekColor := 1;
end;

procedure TF_Popisek.NewPopisek();
begin
  Self.E_Text.Text := '';
  Self.PopisekText := '';

  Self.Caption := 'Nový popisek';
  Self.ActiveControl := Self.E_Text;
  Self.ShowModal();
end;

procedure TF_Popisek.OpenPopisek(aText: string; popisek: TPanelLabel);
begin
  Self.PopisekText := popisek.Text;
  Self.PopisekColor := Integer(popisek.Color);
  Self.PopisekBlok := popisek.BlokPopisek;

  Self.E_Text.Text := popisek.Text;
  case (popisek.Color) of
    scPurple: Self.RB_Col0.Checked := true;
    scGray: Self.RB_Col1.Checked := true;
    scRed: Self.RB_Col2.Checked := true;
    scLime: Self.RB_Col3.Checked := true;
    scWhite: Self.RB_Col4.Checked := true;
    scAqua: Self.RB_Col5.Checked := true;
    scBlue: Self.RB_Col6.Checked := true;
    scYellow: Self.RB_Col7.Checked := true;
  end;
  Self.CHB_Blok_Popisek.Checked := popisek.BlokPopisek;

  Self.Caption := 'Editovat popisek';
  Self.ActiveControl := Self.E_Text;
  Self.ShowModal();
end;

procedure TF_Popisek.RB_Col0Click(Sender: TObject);
begin
  Self.PopisekColor := (Sender as TRadioButton).Tag;
end;

end.// unit
