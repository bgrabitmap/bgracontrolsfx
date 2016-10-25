unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, FXContainer, FXButton, FXMaterialButton, BGRABitmap,
  BGRABitmapTypes, BGRAOpenGL, FXMaterialColors, Math;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    ComboBox1: TComboBox;
    FXButton1: TFXButton;
    FXButton2: TFXButton;
    FXButton3: TFXButton;
    FXContainer1: TFXContainer;
    FXMaterialButton2: TFXMaterialButton;
    FXMaterialButton3: TFXMaterialButton;
    Panel1: TPanel;
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FXButton1Click(Sender: TObject);
    procedure FXButton2Click(Sender: TObject);
    procedure FXContainer1Paint(Sender: TObject);
  private
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }
procedure TfrmMain.FXButton1Click(Sender: TObject);
begin
  FXButton1.Caption := 'Hi.';
end;

procedure TfrmMain.FXButton2Click(Sender: TObject);
begin
  FXButton2.Caption := 'You did it.';
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Randomize;
  MaterialColorsListStr(ComboBox1.Items);
  ComboBox1.ItemIndex := RandomRange(0, ComboBox1.Items.Count+1);
  ComboBox1Change(Self);
end;

procedure TfrmMain.ComboBox1Change(Sender: TObject);
var
  i: integer;
begin
  for i:=0 to FXContainer1.ControlCount-1 do
  begin
    if FXContainer1.Controls[i] is TFXButton then
      TFXButton(FXContainer1.Controls[i]).ColorKind := StrToMaterialColor(ComboBox1.Caption);
    if FXContainer1.Controls[i] is TFXMaterialButton then
      TFXMaterialButton(FXContainer1.Controls[i]).ColorKind := StrToMaterialColor(ComboBox1.Caption);
  end;
end;

procedure TfrmMain.FXContainer1Paint(Sender: TObject);
begin
  Panel1.Color := MaterialColorsList.KeyData[ComboBox1.Caption].M900;
  FXContainer1.Color := MaterialColorsList.KeyData[ComboBox1.Caption].M800;
end;

end.
