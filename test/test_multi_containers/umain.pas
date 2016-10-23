unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  FXContainer, FXMaterialDesignButton;

type

  { TForm1 }

  TForm1 = class(TForm)
    FXContainer1: TFXContainer;
    FXContainer2: TFXContainer;
    FXContainer3: TFXContainer;
    FXMaterialDesignButton1: TFXMaterialDesignButton;
    FXMaterialDesignButton2: TFXMaterialDesignButton;
    FXMaterialDesignButton3: TFXMaterialDesignButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

