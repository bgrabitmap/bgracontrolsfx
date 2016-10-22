{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit BGRAControlsFX;

{$warn 5023 off : no warning about unused units}
interface

uses
  fxcontainer, FXButton, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('fxcontainer', @fxcontainer.Register);
  RegisterUnit('FXButton', @FXButton.Register);
end;

initialization
  RegisterPackage('BGRAControlsFX', @Register);
end.
