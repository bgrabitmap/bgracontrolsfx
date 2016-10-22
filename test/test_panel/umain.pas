unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, FXPanel, FXButton;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    FXButton1: TFXButton;
    FXPanel1: TFXPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    ContextAssigned: boolean;
  public
    procedure HelloWorld(Sender: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormShow(Sender: TObject);
begin
  { ToDo: Move this to the FXPanel if possible }
  {if not ContextAssigned then
  begin
    FXPanel1.AssignContext;
    ContextAssigned := True;
  end;}
end;

procedure TfrmMain.HelloWorld(Sender: TObject);
begin
  ShowMessage('Hello World!');
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FXButton1.OnClick:=@HelloWorld;

  //ContextAssigned := False;
end;

end.

