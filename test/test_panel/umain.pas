unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, FXContainer, FXButton;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    FXButton1: TFXButton;
    FXButton2: TFXButton;
    FXButton3: TFXButton;
    FXPanel1: TFXContainer;
    procedure FormCreate(Sender: TObject);
  public
    procedure HelloWorld(Sender: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.HelloWorld(Sender: TObject);
begin
  ShowMessage('Hello World!');
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FXButton1.OnClick := @HelloWorld;
  FXButton2.Enabled := False;
end;

end.
