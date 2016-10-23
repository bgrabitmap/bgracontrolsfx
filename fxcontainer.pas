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
    procedure FXPreview(var aCanvas: TCanvas);
    procedure FXInvalidate;
  end;

  { TCustomFXContainer }

  TCustomFXContainer = class(TCustomOpenGLControl)
  private
    FCanvas: TCanvas;
  protected
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    procedure DrawChilds;
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

procedure TCustomFXContainer.WMPaint(var Message: TLMPaint);
var
  i: integer;
  IFX: IFXDrawable;
begin
  Include(FControlState, csCustomPaint);
  inherited WMPaint(Message);
  if (csDesigning in ComponentState) and (FCanvas<>nil) then begin
      if Message.DC <> 0 then
        FCanvas.Handle := Message.DC;

      FCanvas.Brush.Color := clWhite;
      FCanvas.FillRect(0, 0, Width, Height);

      for i := 0 to ControlCount - 1 do
        if Controls[i].GetInterface(SFXDrawable, IFX) then
          IFX.FXPreview(FCanvas);

      if Message.DC <> 0 then
        FCanvas.Handle := 0;
  end else begin
    Paint;
  end;
  Exclude(FControlState, csCustomPaint);
end;

procedure TCustomFXContainer.DrawChilds;
var
  i: integer;
  IFX: IFXDrawable;
begin
  for i := 0 to ControlCount - 1 do
    if Controls[i].GetInterface(SFXDrawable, IFX) then
      IFX.FXDraw;
end;

procedure TCustomFXContainer.DoOnPaint;
begin
  if (csDesigning in ComponentState) then
    exit;

  BGLViewPort(Width, Height, BGRAWhite);
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

  AutoResizeViewport := True;

  if (csDesigning in ComponentState) then begin
    FCanvas := TControlCanvas.Create;
    TControlCanvas(FCanvas).Control := Self;
  end else
    FCompStyle:=csNonLCL;
end;

destructor TCustomFXContainer.Destroy;
begin
  inherited Destroy;
end;

end.
