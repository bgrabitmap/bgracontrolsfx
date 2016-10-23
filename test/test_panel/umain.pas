unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  FXContainer, FXButton, FXMaterialDesignButton, BGRABitmap, BGRABitmapTypes,
  BGRAOpenGL;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    FXButton1: TFXButton;
    FXButton2: TFXButton;
    FXButton3: TFXButton;
    FXContainer1: TFXContainer;
    FXMaterialDesignButton1: TFXMaterialDesignButton;
    procedure FormDestroy(Sender: TObject);
    procedure FXButton1Click(Sender: TObject);
    procedure FXButton2Click(Sender: TObject);
    procedure FXContainer1Paint(Sender: TObject);
  private
    bg: TBGLBitmap;
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

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(bg);
end;

procedure TfrmMain.FXContainer1Paint(Sender: TObject);
begin
  BGLCanvas.FillRectLinearColor(0, 0, FXContainer1.Width, FXContainer1.Height,
    BGRABlack, BGRABlack, BGRAWhite, BGRAWhite);
end;

end.
