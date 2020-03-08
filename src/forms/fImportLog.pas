unit fImportLog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TF_ImportLog = class(TForm)
    M_Log: TMemo;
    Label1: TLabel;
    B_OK: TButton;
    procedure B_OKClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Open(log:string);
  end;

var
  F_ImportLog: TF_ImportLog;

implementation

{$R *.dfm}

procedure TF_ImportLog.B_OKClick(Sender: TObject);
begin
 Self.Close();
end;

procedure TF_ImportLog.Open(log:string);
begin
 Self.M_Log.Text := log;
 Self.Show();
 Self.B_OK.SetFocus();
end;

end.
