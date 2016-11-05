unit fxstyle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, Graphics;

type

  { TFXStyle }

  TFXStyle = class(TPersistent)
  private
    FBorderColor: TColor;
    FBorderWidth: single;
    FColor: TColor;
    FEllipse: boolean;
    FHeight: integer;
    FLeft: integer;
    FOnChange: TNotifyEvent;
    FRoundOptions: TRoundRectangleOptions;
    FRoundX: single;
    FRoundY: single;
    FTop: integer;
    FWidth: integer;
    procedure SetFBorderColor(AValue: TColor);
    procedure SetFBorderWidth(AValue: single);
    procedure SetFColor(AValue: TColor);
    procedure SetFEllipse(AValue: boolean);
    procedure SetFHeight(AValue: integer);
    procedure SetFLeft(AValue: integer);
    procedure SetFRoundOptions(AValue: TRoundRectangleOptions);
    procedure SetFRoundX(AValue: single);
    procedure SetFRoundY(AValue: single);
    procedure SetFTop(AValue: integer);
    procedure SetFWidth(AValue: integer);
  protected
    procedure Change;
  public
    procedure Draw(ABitmap: TBGRABitmap);
    constructor Create;
  published
    property Ellipse: boolean read FEllipse write SetFEllipse;
    property BorderWidth: single read FBorderWidth write SetFBorderWidth;
    property BorderColor: TColor read FBorderColor write SetFBorderColor;
    property Color: TColor read FColor write SetFColor;
    property Left: integer read FLeft write SetFLeft;
    property Top: integer read FTop write SetFTop;
    property Width: integer read FWidth write SetFWidth;
    property Height: integer read FHeight write SetFHeight;
    property RoundX: single read FRoundX write SetFRoundX;
    property RoundY: single read FRoundY write SetFRoundY;
    property RoundOptions: TRoundRectangleOptions
      read FRoundOptions write SetFRoundOptions;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TFXStyle }

procedure TFXStyle.SetFLeft(AValue: integer);
begin
  if FLeft = AValue then
    Exit;
  FLeft := AValue;
  Change;
end;

procedure TFXStyle.SetFRoundOptions(AValue: TRoundRectangleOptions);
begin
  if FRoundOptions = AValue then
    Exit;
  FRoundOptions := AValue;
  Change;
end;

procedure TFXStyle.SetFHeight(AValue: integer);
begin
  if FHeight = AValue then
    Exit;
  FHeight := AValue;
  Change;
end;

procedure TFXStyle.SetFColor(AValue: TColor);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;
  Change;
end;

procedure TFXStyle.SetFEllipse(AValue: boolean);
begin
  if FEllipse = AValue then
    Exit;
  FEllipse := AValue;
  Change;
end;

procedure TFXStyle.SetFBorderColor(AValue: TColor);
begin
  if FBorderColor = AValue then
    Exit;
  FBorderColor := AValue;
  Change;
end;

procedure TFXStyle.SetFBorderWidth(AValue: single);
begin
  if FBorderWidth = AValue then
    Exit;
  FBorderWidth := AValue;
  Change;
end;

procedure TFXStyle.SetFRoundX(AValue: single);
begin
  if FRoundX = AValue then
    Exit;
  FRoundX := AValue;
  Change;
end;

procedure TFXStyle.SetFRoundY(AValue: single);
begin
  if FRoundY = AValue then
    Exit;
  FRoundY := AValue;
  Change;
end;

procedure TFXStyle.SetFTop(AValue: integer);
begin
  if FTop = AValue then
    Exit;
  FTop := AValue;
  Change;
end;

procedure TFXStyle.SetFWidth(AValue: integer);
begin
  if FWidth = AValue then
    Exit;
  FWidth := AValue;
  Change;
end;

procedure TFXStyle.Change;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TFXStyle.Draw(ABitmap: TBGRABitmap);
begin
  // If there's nothing to draw exit
  if (Width < Left) or (Height < Top) then
     exit;

  // If must draw an ellipse
  if Ellipse then
  begin
    // If there's no border make it thin and the same color of filling
    if BorderWidth = 0 then
      ABitmap.EllipseAntialias(Left + (Width-Left) div 2, Top + (Height-Top) div 2, (Width-Left) div 2, (Height-Top) div 2, Color, 0.1, Color)
    // Else draw an antialiased border and ellipse
    else
      ABitmap.EllipseAntialias(Left + (Width-Left) div 2, Top + (Height-Top) div 2, (Width-Left) div 2, (Height-Top) div 2, BorderColor, BorderWidth, Color);
  end
  else
  begin
    // If there's no rounding
    if (RoundX = 0) and (RoundY = 0) then
    begin
      // If there's no border, draw perfect pixel square
      if (BorderWidth = 0) then
        ABitmap.Rectangle(Left, Top, Width, Height, Color, Color, dmDrawWithTransparency)
      // If there's 1 px border, draw perfect pixel square and border
      else if (BorderWidth = 1) then
        ABitmap.Rectangle(Left, Top, Width, Height, BorderColor, Color,
          dmDrawWithTransparency)
      // Else draw antialiased border and square
      else
        ABitmap.RectangleAntialias(Left, Top, Width, Height, BorderColor,
          BorderWidth, Color);
    end
    else
    begin
      // If there's not enough space to draw
      if (RoundX * 2 > Width - Left) or (RoundY * 2 > Height - Top) then
        exit;
      // Else draw antialiased border and square
      ABitmap.RoundRectAntialias(Left, Top, Width, Height, RoundX,
        RoundY, BorderColor, BorderWidth, Color, RoundOptions);
    end;
  end;
end;

constructor TFXStyle.Create;
begin
  inherited Create;
  Left := 0;
  Top := 0;
  Width := 100;
  Height := 100;
  RoundX := 0;
  RoundY := 0;
  BorderColor := clBlack;
  Color := clWhite;
  BorderWidth := 0;
  RoundOptions := [];
end;

end.
