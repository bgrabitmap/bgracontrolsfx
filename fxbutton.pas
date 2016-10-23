unit FXButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Types,
  FXContainer, BGRABitmap, BGRABitmapTypes, BGRAOpenGL;

type

  TFXButtonState = (fxbHovered, fxbActive);
  TFXButtonStates = set of TFXButtonState;

  { TCustomFXButton }

  TCustomFXButton = class(TGraphicControl, IFXDrawable)
  private
    FBGRA: TBGRABitmap;
    FTexture: IBGLTexture;
    FState: TFXButtonStates;
    FNeedDraw: boolean;
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
    {%H-}WithThemeSpace: boolean); override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure TextChanged; override;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
  protected
    procedure FXInvalidate;
    procedure FXDraw;
    procedure FXPreview(var aCanvas: TCanvas);
    procedure Draw;
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

procedure TCustomFXButton.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
begin
  PreferredWidth := 150;
  PreferredHeight := 50;
end;

class function TCustomFXButton.GetControlClassDefaultSize: TSize;
begin
  Result := inherited GetControlClassDefaultSize;
end;

procedure TCustomFXButton.TextChanged;
begin
  InvalidatePreferredSize;
  if Assigned(Parent) and Parent.AutoSize then
    Parent.AdjustSize;
  AdjustSize;
  FNeedDraw := True;
  if not (csLoading in ComponentState) then
    FXInvalidate;
  inherited TextChanged;
end;

procedure TCustomFXButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  FState := FState + [fxbActive];
  FNeedDraw := True;
  FXInvalidate;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomFXButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  FState := FState - [fxbActive];
  FNeedDraw := True;
  FXInvalidate;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomFXButton.MouseEnter;
begin
  FState := FState + [fxbHovered];
  FNeedDraw := True;
  FXInvalidate;
  inherited MouseEnter;
end;

procedure TCustomFXButton.MouseLeave;
begin
  FState := FState - [fxbHovered];
  FNeedDraw := True;
  FXInvalidate;
  inherited MouseLeave;
end;

procedure TCustomFXButton.FXInvalidate;
begin
  if (csDesigning in ComponentState) then
    Invalidate;

  if Parent is TFXContainer then
  begin
    if TFXContainer(Parent).ReceivePaintFrom = nil then
      Parent.Invalidate;
  end;
end;

procedure TCustomFXButton.FXDraw;
begin
  if (csDesigning in ComponentState) then
    exit;

  Draw;
  if (FTexture = nil) then
    FTexture := BGLTexture(FBGRA);
  BGLCanvas.PutImage(Left, Top, FTexture);
end;

procedure TCustomFXButton.FXPreview(var aCanvas: TCanvas);
begin
  Draw;
  FBGRA.Draw(aCanvas, Left, Top, False);
end;

procedure TCustomFXButton.Draw;
var
  style: TTextStyle;
begin
  if (Width <> FBGRA.Width) and (Height <> FBGRA.Height) then
  begin
    FNeedDraw := True;
    FBGRA.SetSize(Width, Height);
  end;

  if FNeedDraw then
  begin
    FBGRA.FillTransparent;

    if Enabled then
    begin
      { Button Down }
      if fxbActive in FState then
      begin
        FBGRA.Fill(BGRA(50, 50, 50, 255));
      end
      else
      begin
        { Button Hovered }
        if fxbHovered in FState then
        begin
          FBGRA.Fill(BGRA(200, 200, 200, 255));
        end
        { Button Normal }
        else
        begin
          FBGRA.Fill(BGRA(125, 125, 125, 255));
        end;
      end;
    end
    { Button Disabled }
    else
    begin
      FBGRA.Fill(BGRA(25, 25, 25, 255));
    end;

    style.Alignment := taCenter;
    style.Layout := tlCenter;

    FBGRA.TextRect(Rect(0, 0, Width, Height), 0, 0, Caption, style, Font.Color);

    FNeedDraw := False;
    FTexture := nil;
  end;
end;

constructor TCustomFXButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FBGRA := TBGRABitmap.Create;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

destructor TCustomFXButton.Destroy;
begin
  FreeAndNil(FBGRA);
  inherited Destroy;
end;

end.
