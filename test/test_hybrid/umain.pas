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
    FXMaterialDesignButton1: TFXMaterialButton;
    FXMaterialDesignButton2: TFXMaterialButton;
    Splitter1: TSplitter;
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

end.

