unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  FXContainer, FXMaterialButton, FXButton;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    FXContainer1: TFXContainer;
    FXMaterialButton1: TFXMaterialButton;
    FXMaterialButton2: TFXMaterialButton;
    FXMaterialButton3: TFXMaterialButton;
    FXMaterialButton4: TFXMaterialButton;
    FXMaterialButton5: TFXMaterialButton;
    FXMaterialButton6: TFXMaterialButton;
    FXMaterialButton7: TFXMaterialButton;
    FXMaterialButton8: TFXMaterialButton;
    Timer1: TTimer;
    procedure Timer1StartTimer(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  { Will be invalidated only by this timer at the interval specified }
  FXContainer1.Invalidate;
end;

procedure TfrmMain.Timer1StartTimer(Sender: TObject);
begin
  { Tell other controls that only this can call DoOnPaint }
  FXContainer1.ReceivePaintFrom := Timer1;
  { Lock changing the ReceivePaintFrom control }
  FXContainer1.LockReceivePaint := true;
end;

end.

