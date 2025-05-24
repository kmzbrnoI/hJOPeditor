unit fChangeRelief;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, fMain, Panel, ReliefBitmap, Spin, Global;

type
  TF_ReliefProperties = class(TForm)
    Label2: TLabel;
    Label1: TLabel;
    B_Storno: TButton;
    B_Apply: TButton;
    SE_Width: TSpinEdit;
    SE_Height: TSpinEdit;
    procedure B_StornoClick(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure OpenForm();
  end;

var
  F_ReliefProperties: TF_ReliefProperties;

implementation

{$R *.dfm}

procedure TF_ReliefProperties.OpenForm();
begin
  Self.SE_Width.Value := F_Main.Relief.PanelWidth;
  Self.SE_Height.Value := F_Main.Relief.PanelHeight;

  Self.ShowModal();
end;

procedure TF_ReliefProperties.B_ApplyClick(Sender: TObject);
begin
  if (Self.SE_Width.Value > _MAX_WIDTH) then
  begin
    Application.MessageBox(PChar('Maximální šíøka panelu je '+IntToStr(_MAX_WIDTH)+' symbolù'), 'Nelze pokraèovat', MB_OK OR MB_ICONERROR);
    Exit();
  end;
  if (Self.SE_Height.Value > _MAX_HEIGHT) then
  begin
    Application.MessageBox(PChar('Maximální výška panelu je '+IntToStr(_MAX_HEIGHT)+' symbolù'), 'Nelze pokraèovat', MB_OK OR MB_ICONERROR);
    Exit();
  end;

  try
    F_Main.Relief.SetSize(SE_Width.Value, SE_Height.Value);
    Self.Close();
  except
    on E: Exception do
      Application.MessageBox(PChar('Chyba: ' + E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
  end;
end;

procedure TF_ReliefProperties.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_ReliefProperties.FormCreate(Sender: TObject);
begin
  Self.SE_Width.MaxValue := _MAX_WIDTH;
  Self.SE_Height.MaxValue := _MAX_HEIGHT;
end;

end.// unit
