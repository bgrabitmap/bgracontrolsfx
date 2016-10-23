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
    procedure FXMaterialDesignButton1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
  if bg = nil then
    bg := TBGLBitmap.Create;

  if (bg.Width <> FXContainer1.Width) or (bg.Height <> FXContainer1.Height) then
  begin
    bg.SetSize(FXContainer1.Width, FXContainer1.Height);
    bg.GradientFill(0, 0, FXContainer1.Width, FXContainer1.Height,
      BGRABlack, BGRAWhite, gtLinear, PointF(0, 0), PointF(0, FXContainer1.Height),
      dmSet, False);
  end;

  BGLCanvas.PutImage(0, 0, bg.Texture);
end;

procedure TfrmMain.FXMaterialDesignButton1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  e: extended;
begin
  e := random;
  FXMaterialDesignButton1.Caption := FloatToStr(Random);
  FXMaterialDesignButton1.TextSize := round(random * 30);
end;

end.
