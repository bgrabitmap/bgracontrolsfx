unit FXPanel;

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
  end;

  { TFXPanel }

  TFXPanel = class(TCustomOpenGLControl, IFXDrawable)
  private
    fx: TBGLBitmap;
  public
    procedure AssignContext;
    procedure DrawChilds;
    procedure FXDraw;
    procedure Draw;
    procedure DoOnPaint; override;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls FX', [TFXPanel]);
end;

{ TFXPanel }

procedure TFXPanel.AssignContext;
var
  i: integer;
begin
  for i := 0 to Self.ControlCount - 1 do
  begin
    if Controls[i] is TCustomOpenGLControl then
      TCustomOpenGLControl(Controls[i]).SharedControl := Self;
  end;
end;

procedure TFXPanel.DrawChilds;
var
  i: integer;
  IFX: IFXDrawable;
begin
  for i := 0 to ControlCount - 1 do
  begin
    if Controls[i].GetInterface(SFXDrawable, IFX) then
      IFX.FXDraw;
  end;
end;

procedure TFXPanel.FXDraw;
begin
  Draw;
  BGLCanvas.PutImage(Left, Top, fx.Texture);
end;

procedure TFXPanel.Draw;
begin
  if (Width <> fx.Width) and (Height <> fx.Height) then
  begin
    fx.SetSize(Width, Height);
    fx.FillTransparent;
    fx.RoundRect(0, 0, Width, Height, 20, 20, BGRAWhite, BGRA(255, 255, 255, 100));
  end;
end;

procedure TFXPanel.DoOnPaint;
begin
  //inherited DoOnPaint;

  { Must be nil always for the main container }
  if SharedControl = nil then
  begin
    BGLViewPort(Width, Height, BGRABlack);

    if (Width <> fx.Width) and (Height <> fx.Height) then
    begin
      fx.SetSize(Width, Height);
      fx.FillTransparent;
      fx.RoundRect(0, 0, Width, Height, 20, 20, BGRAWhite, BGRA(255, 255, 255, 100));
    end;
    BGLCanvas.PutImage(0, 0, fx.Texture);

    DrawChilds;
    SwapBuffers;
  end;
end;

constructor TFXPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCompStyle := csPanel;
  ControlStyle := ControlStyle + [csAcceptsControls, csCaptureMouse,
    csClickEvents, csSetCaption, csDoubleClicks, csReplicatable, csNoFocus];
  //, csAutoSize0x0]
  //- [csOpaque];
  fx := TBGLBitmap.Create;
end;

destructor TFXPanel.Destroy;
begin
  FreeAndNil(fx);
  inherited Destroy;
end;

end.
