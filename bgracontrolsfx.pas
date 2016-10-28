{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit BGRAControlsFX;

{$warn 5023 off : no warning about unused units}
interface

uses
  FXButton, FXContainer, fxmaterialbutton, FXProgressBar, FXRadialProgressBar, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('FXButton', @FXButton.Register);
  RegisterUnit('FXContainer', @FXContainer.Register);
  RegisterUnit('fxmaterialbutton', @fxmaterialbutton.Register);
  RegisterUnit('FXProgressBar', @FXProgressBar.Register);
  RegisterUnit('FXRadialProgressBar', @FXRadialProgressBar.Register);
end;

initialization
  RegisterPackage('BGRAControlsFX', @Register);
end.
