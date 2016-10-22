unit FXContainer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  OpenGLContext, LMessages, LCLType, BGRABitmap, BGRABitmapTypes, BGRAOpenGL;

const
  SFXDrawable = '{46c3b16f-a846-4b27-a3d3-2313cc9be63e}';

type

  IFXDrawable = interface
    [SFXDrawable]
    procedure FXDraw;
    procedure FXInvalidateParent;
  end;

  { TFXContainer }

  TFXContainer = class(TCustomOpenGLControl)
  private
    fx: TBGLBitmap;
  protected
    procedure DrawChilds;
    procedure Draw;
  public
    procedure DoOnPaint; override;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls FX', [TFXContainer]);
end;

{ TFXContainer }

procedure TFXContainer.DrawChilds;
var
  i: integer;
  IFX: IFXDrawable;
begin
  for i := 0 to ControlCount - 1 do
    if Controls[i].GetInterface(SFXDrawable, IFX) then
      IFX.FXDraw;
end;

procedure TFXContainer.Draw;
begin
  if (Width <> fx.Width) and (Height <> fx.Height) then
  begin
    fx.SetSize(Width, Height);
    fx.FillTransparent;
    fx.RoundRect(0, 0, Width, Height, 20, 20, BGRAWhite, BGRA(255, 255, 255, 100));
  end;
end;

procedure TFXContainer.DoOnPaint;
begin
  BGLViewPort(Width, Height, BGRABlack);
  Draw;
  BGLCanvas.PutImage(0, 0, fx.Texture);
  DrawChilds;
  inherited DoOnPaint;
  SwapBuffers;
end;

constructor TFXContainer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCompStyle := csPanel;
  ControlStyle := ControlStyle + [csAcceptsControls, csCaptureMouse,
    csClickEvents, csSetCaption, csDoubleClicks, csReplicatable,
    csNoFocus, csAutoSize0x0] - [csOpaque];
  fx := TBGLBitmap.Create;
end;

destructor TFXContainer.Destroy;
begin
  FreeAndNil(fx);
  inherited Destroy;
end;

end.
