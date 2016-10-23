unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  FXContainer, FXMaterialDesignButton;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    FXContainer1: TFXContainer;
    FXMaterialDesignButton1: TFXMaterialDesignButton;
    FXMaterialDesignButton2: TFXMaterialDesignButton;
    Splitter1: TSplitter;
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

end.

