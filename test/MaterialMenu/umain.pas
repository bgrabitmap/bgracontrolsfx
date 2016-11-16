unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, FXMaterialButton;

type

  { TForm1 }

  TForm1 = class(TForm)
    FXMaterialButton1: TFXMaterialButton;
    FXMaterialButton2: TFXMaterialButton;
    FXMaterialButton3: TFXMaterialButton;
    FXMaterialButton4: TFXMaterialButton;
    FXMaterialButton5: TFXMaterialButton;
    FXMaterialButton6: TFXMaterialButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1StartTimer(Sender: TObject);
    procedure Timer1StopTimer(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FShow: boolean;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if FShow then
  begin
    Panel2.Left := Panel2.Left + (Panel2.Width div 8);
    if Panel2.Left > 0 then
    begin
      Panel2.Left := 0;
      Timer1.Enabled := False;
    end;
  end
  else
  begin
    Panel2.Left := Panel2.Left - (Panel2.Width div 8);
    if Panel2.Left < -Panel2.Width then
    begin
      Panel2.Left := -Panel2.Width;
      Timer1.Enabled := False;
    end;
  end;
end;

procedure TForm1.Timer1StartTimer(Sender: TObject);
begin
  if Panel2.Left = 0 then
    FShow := False
  else
    FShow := True;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Timer1.Enabled := True;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Panel2.Left := -Panel2.Width;
end;

procedure TForm1.Timer1StopTimer(Sender: TObject);
begin

end;

end.

