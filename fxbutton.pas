unit FXButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  fxcontainer, BGRABitmap, BGRABitmapTypes, BGRAOpenGL;

type

  TFXButtonState = (fxbHovered, fxbActive);
  TFXButtonStates = set of TFXButtonState;

  { TFXButton }

  TFXButton = class(TGraphicControl, IFXDrawable)
  private
    fx: TBGLBitmap;
    FState: TFXButtonStates;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
  protected
    procedure FXInvalidateParent;
    procedure FXDraw;
    procedure Draw;
  public
    procedure Paint; override;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls FX', [TFXButton]);
end;

{ TFXButton }

procedure TFXButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  FState := FState + [fxbActive];
  FXInvalidateParent;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TFXButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  FState := FState - [fxbActive];
  FXInvalidateParent;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TFXButton.MouseEnter;
begin
  FState := FState + [fxbHovered];
  FXInvalidateParent;
  inherited MouseEnter;
end;

procedure TFXButton.MouseLeave;
begin
  FState := FState - [fxbHovered];
  FXInvalidateParent;
  inherited MouseLeave;
end;

procedure TFXButton.FXInvalidateParent;
begin
  if Parent is TFXContainer then
    TFXContainer(Parent).DoOnPaint;
end;

procedure TFXButton.Paint;
begin
  inherited Paint;
  if (csDesigning in ComponentState) then
  begin
    Canvas.Rectangle(0, 0, Width, Height);
  end;
end;

procedure TFXButton.FXDraw;
begin
  Draw;
  BGLCanvas.PutImage(Left, Top, fx.Texture);
end;

procedure TFXButton.Draw;
begin
  if (Width <> fx.Width) and (Height <> fx.Height) then
    fx.SetSize(Width, Height);

  fx.FillTransparent;

  if Enabled then
  begin
    { Button Down }
    if fxbActive in FState then
    begin
      fx.Fill(BGRA(50, 50, 50, 255));
    end
    else
    begin
      { Button Hovered }
      if fxbHovered in FState then
      begin
        fx.Fill(BGRA(200, 200, 200, 255));
      end
      { Button Normal }
      else
      begin
        fx.Fill(BGRA(125, 125, 125, 255));
      end;
    end;
  end
  { Button Disabled }
  else
  begin
    fx.Fill(BGRA(25, 25, 25, 255));
  end;
end;

constructor TFXButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fx := TBGLBitmap.Create;
end;

destructor TFXButton.Destroy;
begin
  FreeAndNil(fx);
  inherited Destroy;
end;

end.
