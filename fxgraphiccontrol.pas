unit FXGraphicControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes, BGRAOpenGL, FXContainer;

type

  { TFXGraphicControl }

  TFXGraphicControl = class(TGraphicControl, IFXDrawable)
  protected
    FBGRA: TBGRABitmap;
    FTexture: IBGLTexture;
  protected
    procedure FXInvalidate;
    procedure FXDraw;
    procedure FXPreview(var aCanvas: TCanvas);
    procedure Draw; virtual;
    procedure Paint; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TFXGraphicControl }

procedure TFXGraphicControl.FXInvalidate;
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

procedure TFXGraphicControl.FXDraw;
begin
  if (csDesigning in ComponentState) then
    exit;

  Draw;
  if (FTexture = nil) then
    FTexture := BGLTexture(FBGRA);
  BGLCanvas.PutImage(Left, Top, FTexture);
end;

procedure TFXGraphicControl.FXPreview(var aCanvas: TCanvas);
begin
  Draw;
  FBGRA.Draw(aCanvas, Left, Top, False);
end;

procedure TFXGraphicControl.Draw;
begin
  if (FBGRA.Width <> Width) or (FBGRA.Height <> Height) then
    FBGRA.SetSize(Width, Height);

  FBGRA.FillTransparent;
  FTexture := nil;
end;

procedure TFXGraphicControl.Paint;
begin
  if (Parent is TFXContainer) then
    exit;
  Draw;
  FBGRA.Draw(Canvas, 0, 0, False);
end;

constructor TFXGraphicControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FBGRA := TBGRABitmap.Create;
end;

destructor TFXGraphicControl.Destroy;
begin
  FreeAndNil(FBGRA);
  inherited Destroy;
end;

end.
