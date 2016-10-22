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

  { TCustomFXContainer }

  TCustomFXContainer = class(TCustomOpenGLControl)
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

  TFXContainer = class(TCustomFXContainer)
  published
    property Align;
    property Anchors;
    property AutoResizeViewport;
    property BorderSpacing;
    property Enabled;
    {$IFDEF HasRGBBits}
    property RedBits;
    property GreenBits;
    property BlueBits;
    {$ENDIF}
    property OpenGLMajorVersion;
    property OpenGLMinorVersion;
    property MultiSampling;
    property AlphaBits;
    property DepthBits;
    property StencilBits;
    property AUXBuffers;
    property OnChangeBounds;
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMakeCurrent;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
    property OnShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls FX', [TFXContainer]);
end;

{ TCustomFXContainer }

procedure TCustomFXContainer.DrawChilds;
var
  i: integer;
  IFX: IFXDrawable;
begin
  for i := 0 to ControlCount - 1 do
    if Controls[i].GetInterface(SFXDrawable, IFX) then
      IFX.FXDraw;
end;

procedure TCustomFXContainer.Draw;
begin
  if (Width <> fx.Width) and (Height <> fx.Height) then
  begin
    fx.SetSize(Width, Height);
    fx.FillTransparent;
    fx.RoundRect(0, 0, Width, Height, 20, 20, BGRAWhite, BGRA(255, 255, 255, 100));
  end;
end;

procedure TCustomFXContainer.DoOnPaint;
begin
  BGLViewPort(Width, Height, BGRABlack);
  Draw;
  BGLCanvas.PutImage(0, 0, fx.Texture);
  inherited DoOnPaint;
  DrawChilds;
  SwapBuffers;
end;

constructor TCustomFXContainer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCompStyle := csPanel;
  ControlStyle := ControlStyle + [csAcceptsControls, csCaptureMouse,
    csClickEvents, csSetCaption, csDoubleClicks, csReplicatable,
    csNoFocus, csAutoSize0x0] - [csOpaque];
  AutoResizeViewport := true;
  fx := TBGLBitmap.Create;
end;

destructor TCustomFXContainer.Destroy;
begin
  FreeAndNil(fx);
  inherited Destroy;
end;

end.
