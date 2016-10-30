unit FXButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Types,
  BGRABitmap, BGRABitmapTypes, BGRAOpenGL, FXMaterialColors,
  FXGraphicControl, Themes, LCLType;

type

  TFXButtonState = (fxbHovered, fxbActive);
  TFXButtonStates = set of TFXButtonState;

  { TCustomFXButton }

  { TFXBaseButton }

  TFXBaseButton = class(TFXGraphicControl)
  private
    FState: TFXButtonStates;
    FNeedDraw: boolean;
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
    {%H-}WithThemeSpace: boolean); override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure FontChanged(Sender: TObject); override;
    procedure SetEnabled(Value: boolean); override;
    procedure TextChanged; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
  end;

  TCustomFXButton = class(TFXBaseButton)
  private
    FColorActive: TColor;
    FColorDisabled: TColor;
    FColorHover: TColor;
    FColorKind: TMaterialColor;
    FColorNormal: TColor;
    FFontColorAutomatic: boolean;
    procedure SetFColorActive(AValue: TColor);
    procedure SetFColorDisabled(AValue: TColor);
    procedure SetFColorHover(AValue: TColor);
    procedure SetFColorKind(AValue: TMaterialColor);
    procedure SetFColorNormal(AValue: TColor);
    procedure SetFFontColorAutomatic(AValue: boolean);
  protected
    procedure Draw; override;
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
    property ColorKind;
    property ColorNormal;
    property ColorHover;
    property ColorActive;
    property ColorDisabled;
    property FontColorAutomatic;
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

  { TFXCustomNativeButton }

  TFXCustomNativeButton = class(TCustomFXButton)
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
    {%H-}WithThemeSpace: boolean); override;
    procedure Draw; override;
  end;

  TFXNativeButton = class(TFXCustomNativeButton)
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Caption;
    property ColorKind;
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
  RegisterComponents('BGRA Controls FX', [TFXNativeButton]);
end;

function SaveAlphaRect(ABitmap: TBGRABitmap; ARect: TRect): Pointer;
var
  Width, Height, Count, y: integer;
  pAlphaData: PByte;
  pSrc: PBGRAPixel;
begin
  IntersectRect(ARect, ARect, Classes.Rect(0, 0, ABitmap.Width, ABitmap.Height));
  Width := ARect.Right - ARect.Left;
  Height := ARect.Bottom - ARect.Top;
  if (Width <= 0) or (Height <= 0) then
    Result := nil;
  getmem(Result, sizeof(longint) * 2 + sizeof(byte) * Width * Height);
  PLongint(Result)^ := Width;
  (PLongint(Result) +1)^ := Height;
  pAlphaData := pbyte(plongint(Result) + 2);
  for y := ARect.Top to ARect.Bottom - 1 do
  begin
    pSrc := ABitmap.ScanLine[y] + ARect.Left;
    Count := Width;
    while Count > 0 do
    begin
      pAlphaData^ := pSrc^.alpha;
      Inc(pAlphaData);
      Inc(pSrc);
      Dec(Count);
    end;
  end;
end;

procedure RestoreAlphaRectAndFree(ABitmap: TBGRABitmap; AX, AY: integer;
  ASavedAlphaRect: Pointer);
var
  Width, Height, Count, y: integer;
  pAlphaData: PByte;
  pSrc: PBGRAPixel;
begin
  if ASavedAlphaRect = nil then
    exit;
  if AX < 0 then
    AX := 0;
  if AY < 0 then
    AY := 0;
  Width := PLongint(ASavedAlphaRect)^;
  Height := (PLongint(ASavedAlphaRect) + 1)^;
  pAlphaData := pbyte(plongint(ASavedAlphaRect) + 2);
  for y := AY to AY + Height - 1 do
  begin
    pSrc := ABitmap.ScanLine[y] + AX;
    Count := Width;
    while Count > 0 do
    begin
      pSrc^.alpha := pAlphaData^;
      Inc(pAlphaData);
      Inc(pSrc);
      Dec(Count);
    end;
  end;
  freemem(ASavedAlphaRect);
end;

{ TFXBaseButton }

procedure TFXBaseButton.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
var
  ts: TSize;
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);

  if Caption <> '' then
  begin
    FXLayers[0].BGRA.FontStyle := Font.Style;
    FXLayers[0].BGRA.FontQuality := fqFineAntialiasing;
    FXLayers[0].BGRA.FontName := Font.Name;
    if Font.Height = 0 then
      FXLayers[0].BGRA.FontHeight := Font.GetTextHeight(Caption)
    else
      FXLayers[0].BGRA.FontHeight := Font.Height;
    FXLayers[0].BGRA.FontAntialias := True;

    ts := FXLayers[0].BGRA.TextSize(Caption);
    Inc(PreferredWidth, ts.cx + 26);
    Inc(PreferredHeight, ts.cy + 10);
  end;
end;

class function TFXBaseButton.GetControlClassDefaultSize: TSize;
begin
  Result := inherited GetControlClassDefaultSize;
end;

procedure TFXBaseButton.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  FNeedDraw := True;
  Invalidate;
end;

procedure TFXBaseButton.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);
  FNeedDraw := True;
  Invalidate;
end;

procedure TFXBaseButton.TextChanged;
begin
  InvalidatePreferredSize;
  if Assigned(Parent) and Parent.AutoSize then
    Parent.AdjustSize;
  AdjustSize;
  FNeedDraw := True;
  Invalidate;
  inherited TextChanged;
end;

procedure TFXBaseButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  if Button = mbLeft then
  begin
    FState := FState + [fxbActive];
    FNeedDraw := True;
    Invalidate;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TFXBaseButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  if Button = mbLeft then
  begin
    FState := FState - [fxbActive];
    FNeedDraw := True;
    Invalidate;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TFXBaseButton.MouseEnter;
begin
  FState := FState + [fxbHovered];
  FNeedDraw := True;
  Invalidate;
  inherited MouseEnter;
end;

procedure TFXBaseButton.MouseLeave;
begin
  FState := FState - [fxbHovered];
  FNeedDraw := True;
  Invalidate;
  inherited MouseLeave;
end;

{ TFXCustomNativeButton }

procedure TFXCustomNativeButton.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
var
  ts: TSize;
begin
  PreferredWidth := 0;
  PreferredHeight := 0;

  if Caption <> '' then
  begin
    FXLayers[0].BGRA.Canvas.Font := Font;

    ts.cx := FXLayers[0].BGRA.Canvas.Font.GetTextWidth(Caption);
    ts.cy := FXLayers[0].BGRA.Canvas.Font.GetTextHeight(Caption);
    Inc(PreferredWidth, ts.cx + 26);
    Inc(PreferredHeight, ts.cy + 10);
  end;
end;

procedure TFXCustomNativeButton.Draw;
var
  Details: TThemedElementDetails;
  PaintRect: TRect;
  AlphaRect: Pointer;
begin
  if (FXLayers[0].BGRA.Width <> ClientWidth) or
    (FXLayers[0].BGRA.Height <> ClientHeight) then
  begin
    FNeedDraw := True;
    FXLayers[0].BGRA.SetSize(ClientWidth, ClientHeight);
  end;

  if FNeedDraw then
  begin
    {$IFDEF LINUX}
    FXLayers[0].BGRA.Fill(ColorToBGRA(ColorToRGB(Parent.Color)));
    {$ELSE}
    FXLayers[0].BGRA.FillTransparent;
    {$ENDIF}
    PaintRect := Rect(0, 0, FXLayers[0].BGRA.Width, FXLayers[0].BGRA.Height);

    if Enabled then
    begin
      { Button Down }
      if fxbActive in FState then
        Details := ThemeServices.GetElementDetails(tbPushButtonPressed)
      else
      begin
        { Button Hovered }
        if fxbHovered in FState then
          Details := ThemeServices.GetElementDetails(tbPushButtonHot)
        { Button Normal }
        else
          Details := ThemeServices.GetElementDetails(tbPushButtonNormal);
      end;
    end
    { Button Disabled }
    else
      Details := ThemeServices.GetElementDetails(tbPushButtonDisabled);

    FXLayers[0].BGRA.Canvas.Font := Font;

    ThemeServices.DrawElement(FXLayers[0].BGRA.Canvas.Handle, Details, PaintRect, nil);
    PaintRect := ThemeServices.ContentRect(FXLayers[0].BGRA.Canvas.Handle,
      Details, PaintRect);
    AlphaRect := SaveAlphaRect(FXLayers[0].BGRA, PaintRect);
    ThemeServices.DrawText(FXLayers[0].BGRA.Canvas, Details, Caption, PaintRect,
      DT_CENTER or DT_VCENTER or DT_SINGLELINE, 0);
    RestoreAlphaRectAndFree(FXLayers[0].BGRA, PaintRect.Left,
      PaintRect.Top, AlphaRect);

    if ColorKind = mcDefault then
      FXLayers[0].Color := BGRAWhite
    else
      FXLayers[0].Color := GetFillColor;

    FNeedDraw := False;
    FXLayers[0].Texture := nil;
  end;
end;

{ TCustomFXButton }

procedure TCustomFXButton.SetFColorActive(AValue: TColor);
begin
  if FColorActive = AValue then
    Exit;
  FColorActive := AValue;
  FNeedDraw := True;
  Invalidate;
end;

procedure TCustomFXButton.SetFColorDisabled(AValue: TColor);
begin
  if FColorDisabled = AValue then
    Exit;
  FColorDisabled := AValue;
  FNeedDraw := True;
  Invalidate;
end;

procedure TCustomFXButton.SetFColorHover(AValue: TColor);
begin
  if FColorHover = AValue then
    Exit;
  FColorHover := AValue;
  FNeedDraw := True;
  Invalidate;
end;

procedure TCustomFXButton.SetFColorKind(AValue: TMaterialColor);
begin
  if FColorKind = AValue then
    Exit;
  FColorKind := AValue;
  FNeedDraw := True;
  Invalidate;
end;

procedure TCustomFXButton.SetFColorNormal(AValue: TColor);
begin
  if FColorNormal = AValue then
    Exit;
  FColorNormal := AValue;
  FNeedDraw := True;
  Invalidate;
end;

procedure TCustomFXButton.SetFFontColorAutomatic(AValue: boolean);
begin
  if FFontColorAutomatic = AValue then
    Exit;
  FFontColorAutomatic := AValue;
  FNeedDraw := True;
  Invalidate;
end;

procedure TCustomFXButton.Draw;
var
  style: TTextStyle;
  fill_color: TColor;
begin
  if (FXLayers[0].BGRA.Width <> ClientWidth) or
    (FXLayers[0].BGRA.Height <> ClientHeight) then
  begin
    FNeedDraw := True;
    FXLayers[0].BGRA.SetSize(ClientWidth, ClientHeight);
  end;

  if FNeedDraw then
  begin
    FXLayers[0].BGRA.FillTransparent;

    style.Alignment := taCenter;
    style.Layout := tlCenter;

    FXLayers[0].BGRA.FontStyle := Font.Style;
    FXLayers[0].BGRA.FontQuality := fqFineAntialiasing;
    FXLayers[0].BGRA.FontName := Font.Name;
    if Font.Height = 0 then
      FXLayers[0].BGRA.FontHeight := Font.GetTextHeight(Caption)
    else
      FXLayers[0].BGRA.FontHeight := Font.Height;
    FXLayers[0].BGRA.FontAntialias := True;

    fill_color := GetFillColor;
    FXLayers[0].BGRA.Fill(fill_color);

    if FontColorAutomatic then
      FXLayers[0].BGRA.TextRect(Rect(0, 0, FXLayers[0].BGRA.Width,
        FXLayers[0].BGRA.Height), 0, 0, Caption, style,
        GetContrastColor(fill_color))
    else
      FXLayers[0].BGRA.TextRect(Rect(0, 0, FXLayers[0].BGRA.Width,
        FXLayers[0].BGRA.Height), 0, 0, Caption, style, Font.Color);

    FNeedDraw := False;
    FXLayers[0].Texture := nil;
  end;
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
  inherited Destroy;
end;

end.
