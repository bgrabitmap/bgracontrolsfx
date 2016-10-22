unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, FXPanel;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    FXPanel1: TFXPanel;
    FXPanel2: TFXPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    ContextAssigned: boolean;
  public
    procedure FXPanel2Click(Sender: TObject);

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormShow(Sender: TObject);
begin
  { ToDo: Move this to the FXPanel if possible }
  if not ContextAssigned then
  begin
    FXPanel1.AssignContext;
    ContextAssigned := True;
  end;
end;

procedure TfrmMain.FXPanel2Click(Sender: TObject);
begin
  ShowMessage('Hello World!');
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FXPanel2.Visible := False;
  FXPanel2.OnClick := @FXPanel2Click;

  ContextAssigned := False;
end;

end.

