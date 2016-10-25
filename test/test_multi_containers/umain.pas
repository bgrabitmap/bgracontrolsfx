unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  FXContainer, FXMaterialButton;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    FXContainer1: TFXContainer;
    FXContainer2: TFXContainer;
    FXContainer3: TFXContainer;
    FXMaterialButton1: TFXMaterialButton;
    FXMaterialButton2: TFXMaterialButton;
    FXMaterialButton3: TFXMaterialButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

end.

