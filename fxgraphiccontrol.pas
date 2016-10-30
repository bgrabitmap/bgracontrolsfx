unit FXGraphicControl;

{$mode objfpc}{$H+}{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes, BGRAOpenGL, FXContainer, FGL;

type

  { TFXLayer }

  TFXLayer = class
  private
    FTexture: IBGLTexture;
    FBGRA: TBGRABitmap;
    FColor: TBGRAPixel;
    procedure SetFBGRA(AValue: TBGRABitmap);
    procedure SetFColor(AValue: TBGRAPixel);
    procedure SetFTexture(AValue: IBGLTexture);
  public
    constructor Create;
    destructor Destroy; override;
    property Texture: IBGLTexture read FTexture write SetFTexture;
    property BGRA: TBGRABitmap read FBGRA write SetFBGRA;
    property Color: TBGRAPixel read FColor write SetFColor;
  end;

  TFXLayers = specialize TFPGObjectList<TFXLayer>;

  { TFXGraphicControl }

  TFXGraphicControl = class(TGraphicControl, IFXDrawable)
  protected
    FXLayers: TFXLayers;
  protected
    procedure FXDraw; virtual;
    procedure FXPreview(aCanvas: TCanvas);
    procedure Draw; virtual;
    procedure Paint; override;
    procedure DrawWithColor(Source: TBGRABitmap; c: TBGRAPixel;
      Dest: TCanvas; x, y: integer; Opaque: boolean = True);
    procedure Colorize(Source, Dest: TBGRABitmap; c: TBGRAPixel);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TFXLayer }

procedure TFXLayer.SetFTexture(AValue: IBGLTexture);
begin
  if FTexture = AValue then
    Exit;
  FTexture := AValue;
end;

constructor TFXLayer.Create;
begin
  inherited Create;
  FBGRA := TBGRABitmap.Create;
  FColor := BGRAWhite;
end;

destructor TFXLayer.Destroy;
begin
  if Assigned(FBGRA) then
    FBGRA.Free;
  FTexture := nil;
  inherited Destroy;
end;

procedure TFXLayer.SetFBGRA(AValue: TBGRABitmap);
begin
  if FBGRA = AValue then
    Exit;
  FBGRA := AValue;
end;

procedure TFXLayer.SetFColor(AValue: TBGRAPixel);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;
end;

{ TFXGraphicControl }

procedure TFXGraphicControl.FXDraw;
var
  i: integer;
begin
  Draw;
  for i := 0 to FXLayers.Count - 1 do
  begin
    if (FXLayers[i].Texture = nil) then
      FXLayers[i].Texture := BGLTexture(FXLayers[i].BGRA);
    BGLCanvas.PutImage(Left, Top, FXLayers[i].Texture, FXLayers[i].Color);
  end;
end;

procedure TFXGraphicControl.FXPreview(aCanvas: TCanvas);
var
  i: integer;
begin
  Draw;
  for i := 0 to FXLayers.Count - 1 do
  begin
    if FXLayers[i].Color <> BGRAWhite then
      DrawWithColor(FXLayers[i].BGRA, FXLayers[i].Color, aCanvas, Left, Top, False)
    else
      FXLayers[i].BGRA.Draw(aCanvas, Left, Top, False);
  end;
end;

procedure TFXGraphicControl.Draw;
begin
  if (FXLayers[0].BGRA.Width <> Width) or (FXLayers[0].BGRA.Height <> Height) then
    FXLayers[0].BGRA.SetSize(Width, Height);

  FXLayers[0].BGRA.FillTransparent;
  FXLayers[0].Texture := nil;
end;

procedure TFXGraphicControl.Paint;
var
  i: integer;
begin
  if csLoading in ComponentState then
    exit;

  if Parent is TFXContainer then
      Parent.Invalidate
  else
  begin
    Draw;
    for i := 0 to FXLayers.Count - 1 do
    begin
      if FXLayers[i].Color <> BGRAWhite then
        DrawWithColor(FXLayers[i].BGRA, FXLayers[i].Color, Canvas, 0, 0, False)
      else
        FXLayers[i].BGRA.Draw(Canvas, 0, 0, False);
    end;
  end;
end;

procedure TFXGraphicControl.DrawWithColor(Source: TBGRABitmap;
  c: TBGRAPixel; Dest: TCanvas; x, y: integer; Opaque: boolean = True);
var
  temp: TBGRABitmap;
begin
  temp := TBGRABitmap.Create(Source.Width, Source.Height);
  Colorize(Source, temp, c);
  temp.Draw(Dest, x, y, Opaque);
  temp.Free;
end;

procedure TFXGraphicControl.Colorize(Source, Dest: TBGRABitmap; c: TBGRAPixel);
var
  psource: PBGRAPixel;
  pdest: PBGRAPixel;
  ec: TExpandedPixel;
  n: integer;
begin
  psource := Source.Data;
  pdest := Dest.Data;
  ec := GammaExpansion(c);
  for n := Source.NbPixels - 1 downto 0 do
  begin
    pdest^.red := GammaCompressionTab[
      ((GammaExpansionTab[psource^.red] * ec.red + 65535) shr 16)];
    pdest^.green := GammaCompressionTab[
      ((GammaExpansionTab[psource^.green] * ec.green + 65535) shr 16)];
    pdest^.blue := GammaCompressionTab[
      ((GammaExpansionTab[psource^.blue] * ec.blue + 65535) shr 16)];
    pdest^.alpha := (psource^.alpha * ec.alpha + 255) shr 16;
    Inc(pdest);
    Inc(psource);
  end;
end;

constructor TFXGraphicControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FXLayers := TFXLayers.Create;
  FXLayers.Add(TFXLayer.Create);
end;

destructor TFXGraphicControl.Destroy;
begin
  FreeAndNil(FXLayers);
  inherited Destroy;
end;

end.
