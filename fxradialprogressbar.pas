unit FXRadialProgressBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, FXGraphicControl,
  BGRABitmap, BGRABitmapTypes, BGRATextFX;

type

  { TFXRadialProgressBar }

  TFXRadialProgressBar = class(TFXGraphicControl)
  private
    { Private declarations }
    FMaxValue: integer;
    FMinValue: integer;
    FValue: integer;
    FLineColor: TColor;
    FLineBkgColor: TColor;
    FFontShadowColor: TColor;
    FFontShadowOffsetX: integer;
    FFontShadowOffsetY: integer;
    FFontShadowRadius: integer;
    procedure SetFFontShadowColor(AValue: TColor);
    procedure SetFFontShadowOffsetX(AValue: integer);
    procedure SetFFontShadowOffsetY(AValue: integer);
    procedure SetFFontShadowRadius(AValue: integer);
    procedure SetFLineBkgColor(AValue: TColor);
    procedure SetFLineColor(AValue: TColor);
    procedure SetMaxValue(AValue: integer);
    procedure SetMinValue(AValue: integer);
    procedure SetValue(AValue: integer);
  protected
    { Protected declarations }
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      {%H-}WithThemeSpace: boolean); override;
    procedure Draw; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property MinValue: integer read FMinValue write SetMinValue default 0;
    property MaxValue: integer read FMaxValue write SetMaxValue default 100;
    property Value: integer read FValue write SetValue default 0;
    property OnClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property Color default clWhite;
    property LineColor: TColor read FLineColor write SetFLineColor default clBlack;
    property LineBkgColor: TColor read FLineBkgColor write SetFLineBkgColor default
      clSilver;
    property FontShadowColor: TColor read FFontShadowColor
      write SetFFontShadowColor default clBlack;
    property FontShadowOffsetX: integer read FFontShadowOffsetX
      write SetFFontShadowOffsetX default 2;
    property FontShadowOffsetY: integer read FFontShadowOffsetY
      write SetFFontShadowOffsetY default 2;
    property FontShadowRadius: integer read FFontSHadowRadius
      write SetFFontShadowRadius default 4;
    property Font;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I icons\fxradialprogressbar_icon.lrs}
  RegisterComponents('BGRA Controls FX', [TFXRadialProgressBar]);
end;

{ TFXRadialProgressBar }

procedure TFXRadialProgressBar.SetMaxValue(AValue: integer);
begin
  if FMaxValue = AValue then
    exit;
  FMaxValue := AValue;
  if FValue > FMaxValue then
    FValue := FMaxValue;
  if FMinValue > FMaxValue then
    FMinValue := FMaxValue;
  Invalidate;
end;

procedure TFXRadialProgressBar.SetFLineBkgColor(AValue: TColor);
begin
  if FLineBkgColor = AValue then
    Exit;
  FLineBkgColor := AValue;
  Invalidate;
end;

procedure TFXRadialProgressBar.SetFFontShadowColor(AValue: TColor);
begin
  if FFontShadowColor = AValue then
    Exit;
  FFontShadowColor := AValue;
  Invalidate;
end;

procedure TFXRadialProgressBar.SetFFontShadowOffsetX(AValue: integer);
begin
  if FFontShadowOffsetX = AValue then
    Exit;
  FFontShadowOffsetX := AValue;
  Invalidate;
end;

procedure TFXRadialProgressBar.SetFFontShadowOffsetY(AValue: integer);
begin
  if FFontShadowOffsetY = AValue then
    Exit;
  FFontShadowOffsetY := AValue;
  Invalidate;
end;

procedure TFXRadialProgressBar.SetFFontShadowRadius(AValue: integer);
begin
  if FFontSHadowRadius = AValue then
    Exit;
  FFontSHadowRadius := AValue;
  Invalidate;
end;

procedure TFXRadialProgressBar.SetFLineColor(AValue: TColor);
begin
  if FLineColor = AValue then
    Exit;
  FLineColor := AValue;
  Invalidate;
end;

procedure TFXRadialProgressBar.SetMinValue(AValue: integer);
begin
  if FMinValue = AValue then
    exit;
  FMinValue := AValue;
  if FValue < FMinValue then
    FValue := FMinValue;
  if FMaxValue < FMinValue then
    FMaxValue := FMinValue;
  Invalidate;
end;

procedure TFXRadialProgressBar.SetValue(AValue: integer);
begin
  if FValue = AValue then
    exit;
  FValue := AValue;
  if FValue < FMinValue then
    FValue := FMinValue;
  if FValue > FMaxValue then
    FValue := FMaxValue;
  Invalidate;
end;

procedure TFXRadialProgressBar.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
begin
  PreferredWidth := 200;
  PreferredHeight := 200;
end;

procedure TFXRadialProgressBar.Draw;
var
  textBmp: TBGRABitmap;
  textStr: string;
begin
  if (FXLayers[0].BGRA.Width <> Width) or (FXLayers[0].BGRA.Height <> Height) then
    FXLayers[0].BGRA.SetSize(Width, Height);

  FXLayers[0].BGRA.FillTransparent;

  FXLayers[0].BGRA.Canvas2D.beginPath;
  FXLayers[0].BGRA.Canvas2D.arc(Width / 2, Height / 2, Height / 2.5, 0, pi * 2, False);
  FXLayers[0].BGRA.Canvas2D.fillStyle(Color);
  FXLayers[0].BGRA.Canvas2D.fill;

  FXLayers[0].BGRA.Canvas2D.lineWidth := Height / 50;
  FXLayers[0].BGRA.Canvas2D.strokeStyle(LineBkgColor);
  FXLayers[0].BGRA.Canvas2D.stroke;

  FXLayers[0].BGRA.Canvas2D.beginPath;
  if Value <> MinValue then
    FXLayers[0].BGRA.Canvas2D.arc(Width / 2, Height / 2, Height / 2.5, pi * 1.5,
      (pi * 1.5) + ((pi * 2) * Value / MaxValue), False);
  FXLayers[0].BGRA.Canvas2D.fillStyle(BGRAPixelTransparent);
  FXLayers[0].BGRA.Canvas2D.fill;

  FXLayers[0].BGRA.Canvas2D.lineWidth := Height / 50;
  FXLayers[0].BGRA.Canvas2D.strokeStyle(LineColor);
  FXLayers[0].BGRA.Canvas2D.stroke;

  textStr := FloatToStr((Value / MaxValue) * 100) + '%';

  textBmp := TextShadow(Width, Height, textStr, Font.Height,
    Font.Color, FontShadowColor, FontShadowOFfsetX,
    FontShadowOffsetY, FontSHadowRadius, Font.Style, Font.Name) as TBGRABitmap;
  FXLayers[0].BGRA.PutImage(0, 0, textBmp, dmDrawWithTransparency);
  textBmp.Free;

  FXLayers[0].Texture := nil;
end;

constructor TFXRadialProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, 200, 200);
  FMaxValue := 100;
  FMinValue := 0;
  FValue := 0;
  FLineColor := clBlack;
  FLineBkgColor := clSilver;
  FFontShadowColor := clBlack;
  FFontShadowOffsetX := 2;
  FFontShadowOffsetY := 2;
  FFontShadowRadius := 4;
  Font.Color := clBlack;
  Font.Height := 20;
  Color := clWhite;
end;

end.

