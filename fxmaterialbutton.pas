unit FXMaterialButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes, BGRAOpenGL, ExtCtrls, FXMaterialColors,
  FXGraphicControl;

type

  { TFXMaterialButton }

  TFXMaterialButton = class(TFXGraphicControl)
  private
    FColorKind: TMaterialColor;
    FFontColorAutomatic: boolean;
    FNormalColor: TColor;
    FNormalColorEffect: TColor;
    FRoundBorders: single;
    FShadow: boolean;
    FShadowColor: TColor;
    FShadowSize: integer;
    FTextColor: TColor;
    FTextFont: string;
    FTextQuality: TBGRAFontQuality;
    FTextShadow: boolean;
    FTextShadowColor: TColor;
    FTextShadowOffsetX: integer;
    FTextShadowOffsetY: integer;
    FTextShadowSize: integer;
    FTextSize: integer;
    FTextStyle: TFontStyles;
    FTimer: TTimer;
    FMousePos: TPoint;
    FCircleSize: single;
    FCircleAlpha: byte;
    FCircular: boolean;
    FNeedDraw: boolean;
    procedure SetFCircular(AValue: boolean);
    procedure SetFColorKind(AValue: TMaterialColor);
    procedure SetFFontColorAutomatic(AValue: boolean);
    procedure SetFNormalColor(AValue: TColor);
    procedure SetFNormalColorEffect(AValue: TColor);
    procedure SetFRoundBorders(AValue: single);
    procedure SetFShadow(AValue: boolean);
    procedure SetFShadowColor(AValue: TColor);
    procedure SetFShadowSize(AValue: integer);
    procedure SetFTextColor(AValue: TColor);
    procedure SetFTextFont(AValue: string);
    procedure SetFTextQuality(AValue: TBGRAFontQuality);
    procedure SetFTextShadow(AValue: boolean);
    procedure SetFTextShadowColor(AValue: TColor);
    procedure SetFTextShadowOffsetX(AValue: integer);
    procedure SetFTextShadowOffsetY(AValue: integer);
    procedure SetFTextShadowSize(AValue: integer);
    procedure SetFTextSize(AValue: integer);
    procedure SetFTextStyle(AValue: TFontStyles);
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
    {%H-}WithThemeSpace: boolean); override;
    procedure OnStartTimer({%H-}Sender: TObject);
    procedure OnTimer({%H-}Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure TextChanged; override;
    procedure UpdateShadow;
    procedure DrawTextShadow(AHeight: integer; ATextColor: TColor);
  protected
    procedure Draw; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Circular: boolean read FCircular write SetFCircular default false;
    property RoundBorders: single read FRoundBorders write SetFRoundBorders default 5;
    property ColorKind: TMaterialColor read FColorKind write SetFColorKind;
    property FontColorAutomatic: boolean read FFontColorAutomatic
      write SetFFontColorAutomatic;
    property NormalColor: TColor read FNormalColor write SetFNormalColor default clWhite;
    property NormalColorEffect: TColor read FNormalColorEffect
      write SetFNormalColorEffect default clSilver;
    property Shadow: boolean read FShadow write SetFShadow default True;
    property ShadowColor: TColor read FShadowColor write SetFShadowColor default clGray;
    property ShadowSize: integer read FShadowSize write SetFShadowSize default 5;
    property TextColor: TColor read FTextColor write SetFTextColor default clBlack;
    property TextSize: integer read FTextSize write SetFTextSize default 16;
    property TextShadow: boolean read FTextShadow write SetFTextShadow default True;
    property TextShadowColor: TColor read FTextShadowColor
      write SetFTextShadowColor default clBlack;
    property TextShadowSize: integer read FTextShadowSize
      write SetFTextShadowSize default 2;
    property TextShadowOffsetX: integer read FTextShadowOffsetX
      write SetFTextShadowOffsetX default 0;
    property TextShadowOffsetY: integer read FTextShadowOffsetY
      write SetFTextShadowOffsetY default 0;
    property TextStyle: TFontStyles read FTextStyle write SetFTextStyle default [];
    property TextFont: string read FTextFont write SetFTextFont;
    property TextQuality: TBGRAFontQuality
      read FTextQuality write SetFTextQuality default fqFineAntialiasing;
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
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property ParentBidiMode;
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
  {$I icons\fxmaterialbutton_icon.lrs}
  RegisterComponents('BGRA Controls FX', [TFXMaterialButton]);
end;

{ TFXMaterialButton }

procedure TFXMaterialButton.SetFRoundBorders(AValue: single);
begin
  if FRoundBorders = AValue then
    Exit;
  FRoundBorders := AValue;
  UpdateShadow;
  Invalidate;
end;

procedure TFXMaterialButton.SetFShadow(AValue: boolean);
begin
  if FShadow = AValue then
    Exit;
  FShadow := AValue;
  InvalidatePreferredSize;
  AdjustSize;
  UpdateShadow;
  Invalidate;
end;

procedure TFXMaterialButton.SetFShadowColor(AValue: TColor);
begin
  if FShadowColor = AValue then
    Exit;
  FShadowColor := AValue;
  UpdateShadow;
  Invalidate;
end;

procedure TFXMaterialButton.SetFShadowSize(AValue: integer);
begin
  if FShadowSize = AValue then
    Exit;
  FShadowSize := AValue;
  InvalidatePreferredSize;
  AdjustSize;
  UpdateShadow;
  Invalidate;
end;

procedure TFXMaterialButton.SetFTextColor(AValue: TColor);
begin
  if FTextColor = AValue then
    Exit;
  FTextColor := AValue;
  FNeedDraw := True;
  Invalidate;
end;

procedure TFXMaterialButton.SetFTextFont(AValue: string);
begin
  if FTextFont = AValue then
    Exit;
  FTextFont := AValue;
  InvalidatePreferredSize;
  AdjustSize;
  FNeedDraw := True;
  Invalidate;
end;

procedure TFXMaterialButton.SetFTextQuality(AValue: TBGRAFontQuality);
begin
  if FTextQuality = AValue then
    Exit;
  FTextQuality := AValue;
  InvalidatePreferredSize;
  AdjustSize;
  FNeedDraw := True;
  Invalidate;
end;

procedure TFXMaterialButton.SetFTextShadow(AValue: boolean);
begin
  if FTextShadow = AValue then
    Exit;
  FTextShadow := AValue;
  InvalidatePreferredSize;
  AdjustSize;
  FNeedDraw := True;
  Invalidate;
end;

procedure TFXMaterialButton.SetFTextShadowColor(AValue: TColor);
begin
  if FTextShadowColor = AValue then
    Exit;
  FTextShadowColor := AValue;
  UpdateShadow;
  Invalidate;
end;

procedure TFXMaterialButton.SetFTextShadowOffsetX(AValue: integer);
begin
  if FTextShadowOffsetX = AValue then
    Exit;
  FTextShadowOffsetX := AValue;
  InvalidatePreferredSize;
  AdjustSize;
  FNeedDraw := True;
  Invalidate;
end;

procedure TFXMaterialButton.SetFTextShadowOffsetY(AValue: integer);
begin
  if FTextShadowOffsetY = AValue then
    Exit;
  FTextShadowOffsetY := AValue;
  InvalidatePreferredSize;
  AdjustSize;
  FNeedDraw := True;
  Invalidate;
end;

procedure TFXMaterialButton.SetFTextShadowSize(AValue: integer);
begin
  if FTextShadowSize = AValue then
    Exit;
  FTextShadowSize := AValue;
  InvalidatePreferredSize;
  AdjustSize;
  FNeedDraw := True;
  Invalidate;
end;

procedure TFXMaterialButton.SetFTextSize(AValue: integer);
begin
  if FTextSize = AValue then
    Exit;
  FTextSize := AValue;
  InvalidatePreferredSize;
  AdjustSize;
  FNeedDraw := True;
  Invalidate;
end;

procedure TFXMaterialButton.SetFTextStyle(AValue: TFontStyles);
begin
  if FTextStyle = AValue then
    Exit;
  FTextStyle := AValue;
  InvalidatePreferredSize;
  AdjustSize;
  FNeedDraw := True;
  Invalidate;
end;

procedure TFXMaterialButton.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
var
  ts: TSize;
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);

  if Caption <> '' then
  begin
    FXLayers[1].BGRA.FontQuality := FTextQuality;
    FXLayers[1].BGRA.FontName := FTextFont;
    FXLayers[1].BGRA.FontStyle := FTextStyle;
    FXLayers[1].BGRA.FontHeight := FTextSize;
    FXLayers[1].BGRA.FontAntialias := True;

    ts := FXLayers[1].BGRA.TextSize(Caption);
    Inc(PreferredWidth, ts.cx + 26);
    Inc(PreferredHeight, ts.cy + 10);
  end;

  if FShadow then
  begin
    Inc(PreferredWidth, FShadowSize * 2);
    Inc(PreferredHeight, FShadowSize * 2);
  end;

  if FCircular then
  begin
    PreferredHeight := PreferredWidth;
  end;
end;

procedure TFXMaterialButton.SetFNormalColor(AValue: TColor);
begin
  if FNormalColor = AValue then
    Exit;
  FNormalColor := AValue;
  FNeedDraw := True;
  Invalidate;
end;

procedure TFXMaterialButton.SetFColorKind(AValue: TMaterialColor);
begin
  if FColorKind = AValue then
    Exit;
  FColorKind := AValue;
  FNeedDraw := True;
  Invalidate;
end;

procedure TFXMaterialButton.SetFCircular(AValue: boolean);
begin
  if FCircular=AValue then Exit;
  FCircular:=AValue;
  InvalidatePreferredSize;
  AdjustSize;
  UpdateShadow;
  Invalidate;
end;

procedure TFXMaterialButton.SetFFontColorAutomatic(AValue: boolean);
begin
  if FFontColorAutomatic = AValue then
    Exit;
  FFontColorAutomatic := AValue;
  FNeedDraw := True;
  Invalidate;
end;

procedure TFXMaterialButton.SetFNormalColorEffect(AValue: TColor);
begin
  if FNormalColorEffect = AValue then
    Exit;
  FNormalColorEffect := AValue;
  FNeedDraw := True;
  Invalidate;
end;

procedure TFXMaterialButton.OnStartTimer(Sender: TObject);
begin
  FCircleAlpha := 255;
  FCircleSize := 0;
end;

procedure TFXMaterialButton.OnTimer(Sender: TObject);
begin
  FNeedDraw := True;

  if Width > Height then
    FCircleSize := FCircleSize + (Width div 15)
  else
    FCircleSize := FCircleSize + (Height div 15);

  if FCircleAlpha - 10 > 0 then
    FCircleAlpha := FCircleAlpha - 10
  else
    FCircleAlpha := 0;
  if FCircleAlpha <= 0 then
    FTimer.Enabled := False;
  Invalidate;
end;

procedure TFXMaterialButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
  begin
    FTimer.Enabled := False;
    FMousePos := Point(X, Y);
    FTimer.Enabled := True;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

class function TFXMaterialButton.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 123;
  Result.CY := 33;
end;

procedure TFXMaterialButton.TextChanged;
begin
  InvalidatePreferredSize;
  AdjustSize;
  FNeedDraw := True;
  Invalidate;
end;

procedure TFXMaterialButton.UpdateShadow;
var
  temp: TBGRABitmap;
begin
  FXLayers[0].BGRA.FillTransparent;
  if FShadow then
  begin
    if not FCircular then
      FXLayers[0].BGRA.RoundRectAntialias(FShadowSize, FShadowSize, Width - FShadowSize,
        Height - FShadowSize, FRoundBorders, FRoundBorders,
        FShadowColor, 1, FShadowColor, [rrDefault])
    else
      FXLayers[0].BGRA.FillEllipseAntialias(Width div 2, Height div 2, (Width div 2) - FShadowSize, (Width div 2) - FShadowSize, FShadowColor);
    temp := FXLayers[0].BGRA.FilterBlurRadial(FShadowSize /
      sqrt(2), FShadowSize / sqrt(2), rbBox) as TBGRABitmap;
    FXLayers[0].BGRA.Assign(temp);
    temp.Free;
  end;
  FNeedDraw := True;
  FXLayers[0].Texture := nil;
end;

procedure TFXMaterialButton.DrawTextShadow(AHeight: integer; ATextColor: TColor);
var
  bmpSdw: TBGRABitmap;
  OutTxtSize: TSize;
  OutX, OutY: integer;
begin
  FXLayers[1].BGRA.FontAntialias := True;
  FXLayers[1].BGRA.FontHeight := TextSize;
  FXLayers[1].BGRA.FontStyle := TextStyle;
  FXLayers[1].BGRA.FontName := TextFont;
  FXLayers[1].BGRA.FontQuality := TextQuality;

  OutTxtSize := FXLayers[1].BGRA.TextSize(Caption);
  OutX := Round(FXLayers[1].BGRA.Width / 2) - Round(OutTxtSize.cx / 2);
  OutY := Round(AHeight / 2) - Round(OutTxtSize.cy / 2);

  if FTextShadow then
  begin
    bmpSdw := TBGRABitmap.Create(OutTxtSize.cx + 2 * FTextShadowSize,
      OutTxtSize.cy + 2 * FTextShadowSize);
    bmpSdw.FontAntialias := True;
    bmpSdw.FontHeight := TextSize;
    bmpSdw.FontStyle := TextStyle;
    bmpSdw.FontName := TextFont;
    bmpSdw.FontQuality := TextQuality;

    bmpSdw.TextOut(FTextShadowSize, FTextShadowSize, Caption, FTextShadowColor);
    BGRAReplace(bmpSdw, bmpSdw.FilterBlurRadial(FTextShadowSize /
      sqrt(2), FTextShadowSize / sqrt(2), rbBox));
    FXLayers[1].BGRA.PutImage(OutX + FTextShadowOffsetX - FTextShadowSize, OutY +
      FTextShadowOffSetY - FTextShadowSize, bmpSdw,
      dmDrawWithTransparency);
    bmpSdw.Free;
  end;

  FXLayers[1].BGRA.TextOut(OutX, OutY, Caption, ATextColor);
end;

procedure TFXMaterialButton.Draw;
var
  temp: TBGRABitmap;
  round_rect_left: integer;
  round_rect_width: integer;
  round_rect_height: integer;
  text_height: integer;
  color_normal: TColor;
  color_effect: TColor;
begin
  if (ClientWidth - 1 > FRoundBorders * 2) and (ClientHeight - 1 > FRoundBorders * 2) then
  begin
    if (FXLayers[1].BGRA.Width <> ClientWidth) or (FXLayers[1].BGRA.Height <> ClientHeight) then
    begin
      FXLayers[1].BGRA.SetSize(ClientWidth, ClientHeight);
      FXLayers[0].BGRA.SetSize(ClientWidth, ClientHeight);
      UpdateShadow;
    end;

    if FNeedDraw then
    begin
      FXLayers[1].BGRA.FillTransparent;

      if ColorKind = mcDefault then
      begin
        color_normal := FNormalColor;
        color_effect := FNormalColorEffect;
      end
      else
      begin
        color_normal := MaterialColorsList.KeyData[MaterialColorStr[ColorKind]].M500;
        color_effect := MaterialColorsList.KeyData[MaterialColorStr[ColorKind]].M50;
      end;

      temp := TBGRABitmap.Create(ClientWidth, ClientHeight, color_normal);
      temp.EllipseAntialias(FMousePos.X, FMousePos.Y, FCircleSize, FCircleSize,
        ColorToBGRA(color_effect, FCircleAlpha), 1,
        ColorToBGRA(color_effect, FCircleAlpha));

      if FShadow then
      begin
        round_rect_left := FShadowSize;
        round_rect_width := ClientWidth - FShadowSize;
        round_rect_height := ClientHeight - FShadowSize;
        if FCircular then
        begin
          round_rect_width := (Width div 2) - FShadowSize;
          round_rect_height := (Height - FShadowSize) div 2;
        end;
      end
      else
      begin
        round_rect_left := 0;
        round_rect_width := ClientWidth;
        round_rect_height := ClientHeight;
        if FCircular then
        begin
          round_rect_width := Width div 2;
          round_rect_height := Height div 2;
        end;
      end;

      if not FCircular then
      FXLayers[1].BGRA.FillRoundRectAntialias(round_rect_left, 0, round_rect_width,
        round_rect_height,
        FRoundBorders, FRoundBorders, temp, [rrDefault], False)
      else
        FXLayers[1].BGRA.FillEllipseAntialias(Width div 2, round_rect_height, round_rect_width, round_rect_width, temp);
      temp.Free;

      if Caption <> '' then
      begin
        if FShadow then
          text_height := ClientHeight - FShadowSize
        else
          text_height := ClientHeight;
        if FontColorAutomatic then
          DrawTextShadow(text_height, GetContrastColor(color_normal))
        else
          DrawTextShadow(text_height, FTextColor);
      end;

      FNeedDraw := False;
      FXLayers[1].Texture := nil;
    end;
  end
  else
  begin
    FXLayers[0].BGRA.FillTransparent;
    FXLayers[0].Texture := nil;
    FXLayers[1].BGRA.FillTransparent;
    FXLayers[1].Texture := nil;
  end;
end;

constructor TFXMaterialButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 15;
  FTimer.Enabled := False;
  FTimer.OnStartTimer := @OnStartTimer;
  FTimer.OnTimer := @OnTimer;
  FXLayers.Insert(0, TFXLayer.Create);
  FRoundBorders := 5;
  FNormalColor := clWhite;
  FNormalColorEffect := clSilver;
  FShadow := True;
  FShadowColor := clGray;
  FShadowSize := 5;
  FTextColor := clBlack;
  FTextSize := 16;
  FTextShadow := True;
  FTextShadowColor := clBlack;
  FTextShadowSize := 2;
  FTextShadowOffsetX := 0;
  FTextShadowOffsetY := 0;
  FTextStyle := [];
  FTextFont := 'default';
  FTextQuality := fqFineAntialiasing;
  FFontColorAutomatic := True;
end;

destructor TFXMaterialButton.Destroy;
begin
  FTimer.Enabled := False;
  FTimer.OnStartTimer := nil;
  FTimer.OnStopTimer := nil;
  FTimer.OnTimer := nil;
  inherited Destroy;
end;

end.
