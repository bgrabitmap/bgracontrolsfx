unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  FXContainer, FXButton, BGRABitmap, BGRABitmapTypes, BGRAOpenGL,
  BGLVirtualScreen;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    FXButton1: TFXButton;
    FXButton2: TFXButton;
    FXButton3: TFXButton;
    FXContainer1: TFXContainer;
    procedure FormDestroy(Sender: TObject);
    procedure FXButton1Click(Sender: TObject);
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
  ShowMessage('Hello World!');
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(bg);
end;

procedure TfrmMain.FXContainer1Paint(Sender: TObject);
begin
  if bg = nil then
    bg := TBGLBitmap.Create(Width, Height);

  if (bg.Width <> FXContainer1.Width) or (bg.Height <> FXContainer1.Height) then
    bg.SetSize(FXContainer1.Width, FXContainer1.Height);

  bg.GradientFill(0, 0, FXContainer1.Width, FXContainer1.Height, BGRABlack, BGRAWhite, gtLinear, PointF(0, 0), PointF(0, FXContainer1.Height), dmSet, False);
  BGLCanvas.PutImage(0, 0, bg.Texture);
end;

end.
