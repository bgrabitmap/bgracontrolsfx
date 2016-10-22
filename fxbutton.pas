unit FXButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  FXPanel, BGRABitmap, BGRABitmapTypes, BGRAOpenGL;

type

  { TFXButton }

  TFXButton = class(TGraphicControl, IFXDrawable)
  private
    fx: TBGLBitmap;
  protected

  public
    procedure Paint; override;
    procedure FXDraw;
    procedure Draw;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls FX',[TFXButton]);
end;

{ TFXButton }

procedure TFXButton.Paint;
begin
  inherited Paint;
  if (csDesigning in ComponentState) then
  begin
    //Draw;
    //fx.Draw(Canvas, 0, 0, False);
  end;
end;

procedure TFXButton.FXDraw;
begin
  Draw;
  BGLCanvas.PutImage(Left, Top, fx.Texture);
end;

procedure TFXButton.Draw;
begin
  if (Width <> fx.Width) and (Height <> fx.Height) then
  begin
    fx.SetSize(Width, Height);
    fx.FillTransparent;
    fx.RoundRect(0, 0, Width, Height, 20, 20, BGRAWhite, BGRA(255, 255, 255, 100));
  end;
end;

constructor TFXButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fx := TBGLBitmap.Create;
end;

destructor TFXButton.Destroy;
begin
  FreeAndNil(fx);
  inherited Destroy;
end;

end.
