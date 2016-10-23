unit FXButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Types,
  FXContainer, BGRABitmap, BGRABitmapTypes, BGRAOpenGL, FXMaterialColors;

type

  TFXButtonState = (fxbHovered, fxbActive);
  TFXButtonStates = set of TFXButtonState;

  { TCustomFXButton }

  TCustomFXButton = class(TGraphicControl, IFXDrawable)
  private
    FBGRA: TBGRABitmap;
    FColorActive: TColor;
    FColorDisabled: TColor;
    FColorHover: TColor;
    FColorKind: TMaterialColor;
    FColorNormal: TColor;
    FFontColorAutomatic: boolean;
    FTexture: IBGLTexture;
    FState: TFXButtonStates;
    FNeedDraw: boolean;
    procedure SetFColorActive(AValue: TColor);
    procedure SetFColorDisabled(AValue: TColor);
    procedure SetFColorHover(AValue: TColor);
    procedure SetFColorKind(AValue: TMaterialColor);
    procedure SetFColorNormal(AValue: TColor);
    procedure SetFFontColorAutomatic(AValue: boolean);
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
    procedure Paint; override;
    function GetFillColor: TColor;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property FontColorAutomatic: boolean read FFontColorAutomatic
      write SetFFontColorAutomatic;
    property ColorKind: TMaterialColor read FColorKind write SetFColorKind;
    property ColorNormal: TColor read FColorNormal write SetFColorNormal;
    property ColorHover: TColor read FColorHover write SetFColorHover;
    property ColorActive: TColor read FColorActive write SetFColorActive;
    property ColorDisabled: TColor read FColorDisabled write SetFColorDisabled;
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

procedure TCustomFXButton.SetFColorActive(AValue: TColor);
begin
  if FColorActive = AValue then
    Exit;
  FColorActive := AValue;
  FNeedDraw := True;
  if not (csLoading in ComponentState) then
    FXInvalidate;
end;

procedure TCustomFXButton.SetFColorDisabled(AValue: TColor);
begin
  if FColorDisabled = AValue then
    Exit;
  FColorDisabled := AValue;
  FNeedDraw := True;
  if not (csLoading in ComponentState) then
    FXInvalidate;
end;

procedure TCustomFXButton.SetFColorHover(AValue: TColor);
begin
  if FColorHover = AValue then
    Exit;
  FColorHover := AValue;
  FNeedDraw := True;
  if not (csLoading in ComponentState) then
    FXInvalidate;
end;

procedure TCustomFXButton.SetFColorKind(AValue: TMaterialColor);
begin
  if FColorKind = AValue then
    Exit;
  FColorKind := AValue;
  FNeedDraw := True;
  if not (csLoading in ComponentState) then
    FXInvalidate;
end;

procedure TCustomFXButton.SetFColorNormal(AValue: TColor);
begin
  if FColorNormal = AValue then
    Exit;
  FColorNormal := AValue;
  FNeedDraw := True;
  if not (csLoading in ComponentState) then
    FXInvalidate;
end;

procedure TCustomFXButton.SetFFontColorAutomatic(AValue: boolean);
begin
  if FFontColorAutomatic = AValue then
    Exit;
  FFontColorAutomatic := AValue;
  FNeedDraw := True;
  if not (csLoading in ComponentState) then
    FXInvalidate;
end;

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
  end
  else
    Invalidate;
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
  fill_color: TColor;
begin
  if (Width <> FBGRA.Width) and (Height <> FBGRA.Height) then
  begin
    FNeedDraw := True;
    FBGRA.SetSize(Width, Height);
  end;

  if FNeedDraw then
  begin
    FBGRA.FillTransparent;

    style.Alignment := taCenter;
    style.Layout := tlCenter;
    FBGRA.FontHeight := Font.GetTextHeight(Caption);
    FBGRA.FontAntialias := True;

    fill_color := GetFillColor;
    FBGRA.Fill(fill_color);

    if FontColorAutomatic then
      FBGRA.TextRect(Rect(0, 0, Width, Height), 0, 0, Caption, style,
        GetContrastColor(fill_color))
    else
      FBGRA.TextRect(Rect(0, 0, Width, Height), 0, 0, Caption, style, Font.Color);

    FNeedDraw := False;
    FTexture := nil;
  end;
end;

procedure TCustomFXButton.Paint;
begin
  if (Parent is TFXContainer) then
    exit;
  Draw;
  FBGRA.Draw(Canvas, 0, 0, False);
end;

function TCustomFXButton.GetFillColor: TColor;
begin
  if ColorKind = mcDefault then
  begin
    if Enabled then
    begin
      { Button Down }
      if fxbActive in FState then
        Result := ColorActive
      else
      begin
        { Button Hovered }
        if fxbHovered in FState then
          Result := ColorHover
        { Button Normal }
        else
          Result := ColorNormal;
      end;
    end
    { Button Disabled }
    else
      Result := ColorDisabled;
  end
  else
  begin
    if Enabled then
    begin
      { Button Down }
      if fxbActive in FState then
        Result := MaterialColorsList.KeyData[MaterialColorStr[ColorKind]].M100
      else
      begin
        { Button Hovered }
        if fxbHovered in FState then
          Result := MaterialColorsList.KeyData[MaterialColorStr[ColorKind]].M300
        { Button Normal }
        else
          Result := MaterialColorsList.KeyData[MaterialColorStr[ColorKind]].M500;
      end;
    end
    { Button Disabled }
    else
      Result := MaterialColorsList.KeyData[MaterialColorStr[ColorKind]].M900;
  end;
end;

constructor TCustomFXButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FBGRA := TBGRABitmap.Create;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  FColorNormal := clWhite;
  FColorHover := clSilver;
  FColorActive := clMedGray;
  FColorDisabled := clGray;
  FFontColorAutomatic := True;
end;

destructor TCustomFXButton.Destroy;
begin
  FreeAndNil(FBGRA);
  inherited Destroy;
end;

end.
