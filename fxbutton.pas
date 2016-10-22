unit FXButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  FXContainer, LMessages, LCLType, BGRABitmap, BGRABitmapTypes, BGRAOpenGL;

type

  TFXButtonState = (fxbHovered, fxbActive);
  TFXButtonStates = set of TFXButtonState;

  { TCustomFXButton }

  TCustomFXButton = class(TGraphicControl, IFXDrawable)
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
    procedure Paint; override;
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TFXButton = class(TCustomFXButton)
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Caption;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBidiMode;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls FX', [TFXButton]);
end;

{ TCustomFXButton }

procedure TCustomFXButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  FState := FState + [fxbActive];
  FXInvalidateParent;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomFXButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  FState := FState - [fxbActive];
  FXInvalidateParent;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomFXButton.MouseEnter;
begin
  FState := FState + [fxbHovered];
  FXInvalidateParent;
  inherited MouseEnter;
end;

procedure TCustomFXButton.MouseLeave;
begin
  FState := FState - [fxbHovered];
  FXInvalidateParent;
  inherited MouseLeave;
end;

procedure TCustomFXButton.FXInvalidateParent;
begin
  if Parent is TFXContainer then
    TFXContainer(Parent).DoOnPaint;
end;

procedure TCustomFXButton.FXDraw;
begin
  Draw;
  BGLCanvas.PutImage(Left, Top, fx.Texture);
end;

procedure TCustomFXButton.Draw;
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

procedure TCustomFXButton.Paint;
begin
  inherited Paint;
  if (csDesigning in ComponentState) then
  begin
    Canvas.Pen.Color := clBlack;
    Canvas.Rectangle(0, 0, Width, Height);
  end;
end;

procedure TCustomFXButton.WMPaint(var Message: TLMPaint);
begin
  if (csDesigning in ComponentState) then
  begin
    Canvas.Pen.Color := clBlack;
    Canvas.Rectangle(0, 0, Width, Height);
  end;
end;

constructor TCustomFXButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fx := TBGLBitmap.Create;
end;

destructor TCustomFXButton.Destroy;
begin
  FreeAndNil(fx);
  inherited Destroy;
end;

end.
