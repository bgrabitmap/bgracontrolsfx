unit FXProgressBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LMessages, Forms, Controls, Graphics,
  Dialogs, BGRABitmap, FXGraphicControl;

type

  TFXProgressBarRedrawEvent = procedure(Sender: TObject; Bitmap: TBGRABitmap;
    xpos: integer) of object;

  { TFXProgressBar }

  TFXProgressBar = class(TFXGraphicControl)
  private
    FMaxValue: integer;
    FMinValue: integer;
    FValue: integer;
    FRandSeed: integer;
    FOnRedraw: TFXProgressBarRedrawEvent;
    procedure SetMaxValue(const AValue: integer);
    procedure SetMinValue(const AValue: integer);
    procedure SetValue(const AValue: integer);
    { Private declarations }
  protected
    { Protected declarations }
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: boolean); override;
    procedure Draw; override;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    { Streaming }
    procedure SaveToFile(AFileName: string);
    procedure LoadFromFile(AFileName: string);
    procedure OnFindClass({%H-}Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);
  published
    { Published declarations }
    property Align;
    property Anchors;
    property MinValue: integer read FMinValue write SetMinValue;
    property MaxValue: integer read FMaxValue write SetMaxValue;
    property Value: integer read FValue write SetValue;
    property OnClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property OnRedraw: TFXProgressBarRedrawEvent read FOnredraw write FOnRedraw;
    property Color;
  end;

procedure Register;

implementation

uses BGRABitmapTypes, BGRAGradients, Types;

procedure Register;
begin
  {$I icons\fxprogressbar_icon.lrs}
  RegisterComponents('BGRA Controls FX', [TFXProgressBar]);
end;

{ TFXProgressBar }

procedure TFXProgressBar.SetMinValue(const AValue: integer);
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

procedure TFXProgressBar.SetValue(const AValue: integer);
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

{$hints off}
procedure TFXProgressBar.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
begin
  PreferredWidth := 379;
  PreferredHeight := 33;
end;

{$hints on}

procedure TFXProgressBar.Draw;
var
  content: TRect;
  xpos, y, tx, ty: integer;
  grayValue: integer;

  function ApplyLightness(c: TBGRAPixel; lightness: word): TBGRAPixel;
  begin
    Result := GammaCompression(SetLightness(GammaExpansion(c), lightness));
  end;

  procedure DrawBar(bounds: TRect);
  var
    lCol: TBGRAPixel;
  begin
    lCol := Color;

    DoubleGradientAlphaFill(FXLayers[0].BGRA, bounds,
      ApplyLightness(lCol, 37000), ApplyLightness(lCol, 29000),
      ApplyLightness(lCol, 26000), ApplyLightness(lCol, 18000),
      gdVertical, gdVertical, gdVertical, 0.53);

    InflateRect(bounds, -1, -1);

    DoubleGradientAlphaFill(FXLayers[0].BGRA, bounds,
      ApplyLightness(lCol, 28000), ApplyLightness(lCol, 22000),
      ApplyLightness(lCol, 19000), ApplyLightness(lCol, 11000),
      gdVertical, gdVertical, gdVertical, 0.53);
  end;

begin
  tx := ClientWidth;
  ty := ClientHeight;
  if Assigned(FXLayers[0].BGRA) and ((FXLayers[0].BGRA.Width <> tx) or
    (FXLayers[0].BGRA.Height <> ty)) then
      FXLayers[0].BGRA.SetSize(tx, ty);

  FXLayers[0].BGRA.FillTransparent;

  FXLayers[0].BGRA.Rectangle(0, 0, tx, ty, BGRA(255, 255, 255, 6), dmSet);
  if (tx > 2) and (ty > 2) then
    FXLayers[0].BGRA.Rectangle(1, 1, tx - 1, ty - 1, BGRA(29, 29, 29), dmSet);

  if (tx > 4) and (ty > 4) then
  begin
    content := Rect(2, 2, tx - 2, ty - 2);
    randseed := FRandSeed;
    for y := content.Top to content.Bottom - 1 do
    begin
      if y = content.Top then
        grayValue := 33
      else
      if y = content.Top + 1 then
        grayValue := 43
      else
        grayValue := 47 + random(50 - 47 + 1);
      FXLayers[0].BGRA.SetHorizLine(content.Left, y, content.Right -
        1, BGRA(grayValue, grayValue, grayValue));
    end;
    if tx >= 6 then
      FXLayers[0].BGRA.DrawVertLine(content.Right - 1, content.Top, content.Bottom - 1,
        BGRA(0, 0, 0, 32));
    if FMaxValue > FMinValue then
    begin
      xpos := round((FValue - FMinValue) / (FMaxValue - FMinValue) *
        (content.right - content.left)) + content.left;
      if xpos > content.left then
      begin
        DrawBar(rect(content.left, content.top, xpos, content.bottom));
        if xpos < content.right then
        begin
          FXLayers[0].BGRA.SetPixel(xpos, content.top, BGRA(62, 62, 62));
          FXLayers[0].BGRA.SetVertLine(xpos, content.top + 1,
            content.bottom - 1, BGRA(40, 40, 40));
        end;
      end;
    end;
  end;
  if Assigned(OnRedraw) then
    OnRedraw(Self, FXLayers[0].BGRA, {%H-}xpos);
  FXLayers[0].Texture := nil;
end;

{$hints off}
procedure TFXProgressBar.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  //do nothing
end;

{$hints on}

constructor TFXProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, 33);
  FMinValue := 0;
  FMaxValue := 100;
  FValue := 30;
  randomize;
  FRandSeed := randseed;
  Color := BGRA(102, 163, 226);
end;

destructor TFXProgressBar.Destroy;
begin
  inherited Destroy;
end;

procedure TFXProgressBar.SaveToFile(AFileName: string);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    WriteComponentAsTextToStream(AStream, Self);
    AStream.SaveToFile(AFileName);
  finally
    AStream.Free;
  end;
end;

procedure TFXProgressBar.LoadFromFile(AFileName: string);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    AStream.LoadFromFile(AFileName);
    ReadComponentFromTextStream(AStream, TComponent(Self), @OnFindClass);
  finally
    AStream.Free;
  end;
end;

procedure TFXProgressBar.OnFindClass(Reader: TReader; const AClassName: string;
  var ComponentClass: TComponentClass);
begin
  if CompareText(AClassName, 'TFXProgressBar') = 0 then
    ComponentClass := TFXProgressBar;
end;

procedure TFXProgressBar.SetMaxValue(const AValue: integer);
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

end.
