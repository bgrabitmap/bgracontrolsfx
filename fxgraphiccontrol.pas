unit FXGraphicControl;

{$mode objfpc}{$H+}{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes, BGRAOpenGL, FXContainer, FGL;

type

  { TFXLayer }

  TFXLayer = record
  private
    FTexture: IBGLTexture;
    FBGRA: TBGRABitmap;
    FColor: TBGRAPixel;
    procedure SetFBGRA(AValue: TBGRABitmap);
    procedure SetFColor(AValue: TBGRAPixel);
    procedure SetFTexture(AValue: IBGLTexture);
  public
    class function CreateNew: TFXLayer; static;
    procedure Create;
    procedure Free;
    class operator =(Source1, Source2: TFXLayer): boolean;
    property Texture: IBGLTexture read FTexture write SetFTexture;
    property BGRA: TBGRABitmap read FBGRA write SetFBGRA;
    property Color: TBGRAPixel read FColor write SetFColor;
  end;

  TFXLayers = specialize TFPGList<TFXLayer>;

  { TFXGraphicControl }

  TFXGraphicControl = class(TGraphicControl, IFXDrawable)
  protected
    FXLayers: TFXLayers;
  protected
    procedure FXInvalidate;
    procedure FXDraw; virtual;
    procedure FXPreview(var aCanvas: TCanvas);
    procedure Draw; virtual;
    procedure Paint; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TFXLayer }

procedure TFXLayer.SetFTexture(AValue: IBGLTexture);
begin
  if FTexture=AValue then Exit;
  FTexture:=AValue;
end;

procedure TFXLayer.SetFBGRA(AValue: TBGRABitmap);
begin
  if FBGRA=AValue then Exit;
  FBGRA:=AValue;
end;

procedure TFXLayer.SetFColor(AValue: TBGRAPixel);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
end;

class function TFXLayer.CreateNew: TFXLayer;
begin
  Result.Create;
end;

procedure TFXLayer.Create;
begin
  BGRA := TBGRABitmap.Create;
  Color := BGRAPixelTransparent;
end;

procedure TFXLayer.Free;
begin
  if BGRA <> nil then
    BGRA.Free;
  Texture := nil;
end;

class operator TFXLayer.=(Source1, Source2: TFXLayer): boolean;
begin
  if (Source1.Color = Source2.Color) and (Source1.BGRA = Source2.BGRA) then
    Result := True
  else
    Result := False;
end;

{ TFXGraphicControl }

procedure TFXGraphicControl.FXInvalidate;
begin
  if (csDesigning in ComponentState) then
    Invalidate
  else
  begin
    if Parent is TFXContainer then
    begin
      if TFXContainer(Parent).ReceivePaintFrom = nil then
        Parent.Invalidate;
    end
    else
      Invalidate;
  end;
end;

procedure TFXGraphicControl.FXDraw;
var
  i: integer;
begin
  if (csDesigning in ComponentState) then
    exit;

  Draw;
  for i:=0 to FXLayers.Count-1 do
  begin
    if (FXLayers[i].Texture = nil) then
      FXLayers[i].Texture := BGLTexture(FXLayers[i].BGRA);
    BGLCanvas.PutImage(Left, Top, FXLayers[i].Texture, FXLayers[i].Color);
  end;
end;

procedure TFXGraphicControl.FXPreview(var aCanvas: TCanvas);
var
  i: integer;
begin
  Draw;
  for i:=0 to FXLayers.Count-1 do
    FXLayers[i].BGRA.Draw(aCanvas, Left, Top, False);
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
  if (Parent is TFXContainer) then
    exit;
  Draw;
  for i:=0 to FXLayers.Count-1 do
    FXLayers[i].BGRA.Draw(Canvas, 0, 0, False);
end;

constructor TFXGraphicControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FXLayers := TFXLayers.Create;
  FXLayers.Add(TFXLayer.CreateNew);
end;

destructor TFXGraphicControl.Destroy;
var
  i: integer;
begin
  for i:=0 to FXLayers.Count-1 do
    FXLayers[i].Free;
  FreeAndNil(FXLayers);
  inherited Destroy;
end;

end.
