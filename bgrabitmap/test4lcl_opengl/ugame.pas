unit UGame;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, BGRAGraphics, BGRABitmap, BGRABitmapTypes, BGRAOpenGL,
  Controls;

const
  FrameDurationMs = 15;
  ResourceDir = '..'+PathDelim;

type
  TTux = class;

  { TGameContext }

  TGameContext = class
    DataLoaded: boolean;
    LeftKey,RightKey,UpKey: boolean;
    texWalking,texGround: IBGLTexture;
    tux,smallTux: TTux;
    elapsedMs: single;
    sun: IBGLTexture;
    constructor Create;
    destructor Destroy; override;
    procedure Render(AWidth,AHeight: integer);
    procedure LoadData;
    procedure AddGround(x,y,w: single);
    procedure Elapse(ms: single);
    function FindGround(x: single; var y: single): boolean;
    procedure MouseDown({%H-}Button: TMouseButton; {%H-}X,{%H-}Y: integer);
    procedure MouseMove({%H-}X,{%H-}Y: integer);
    procedure MouseUp({%H-}Button: TMouseButton);
  end;

  { TTux }

  TTux = class(TBGLSprite)
    Speed: TPointF;
    LookingLeft: boolean;
    Context: TGameContext;
    Autoplay: boolean;
    Parent: TTux;
    OnTheGround: boolean;
    Bubble,BubbleTooFar: IBGLTexture;
    BubbleTime: integer;
    ShowBubbleTooFar: boolean;
    goRight,goLeft,goUp: boolean;
    waiting: integer;
    JumpStrength: single;
    procedure OnInit; override;
    procedure OnDraw; override;
    procedure OnTimer; override;
    constructor Create(ATexture: IBGLTexture; AContext: TGameContext);
  end;

  { TSmallTux }

  TSmallTux = class(TTux)
    procedure OnInit; override;
    procedure OnTimer; override;
  end;

  { TGround }

  TGround = class(TBGLSprite)
    constructor Create(ATexture: IBGLTexture; AX,AY: Single; AFrame: integer);
    procedure OnInit; override;
  end;

implementation

function CreateTextBubble(AText: string): IBGLTexture;
const horizMargin = 6; vertMargin = 4;
var bmp: TBGLBitmap;
begin
  bmp := TBGLBitmap.Create;
  bmp.FontHeight := 14;
  bmp.FontStyle := [fsBold];
  bmp.SetSize(((bmp.TextSize(AText).cx+2*horizMargin+2)+1) and not 1, bmp.FontFullHeight+vertMargin*2+2);
  bmp.FillTransparent;
  bmp.RoundRectAntialias(1,1,bmp.Width-2,bmp.Height-2,12,12, BGRABlack, 1, BGRA(255,255,250));
  bmp.TextOut(horizMargin+1,vertMargin+1,AText, BGRABlack);
  result := bmp.MakeTextureAndFree;
end;

{ TSmallTux }

procedure TSmallTux.OnInit;
begin
  inherited OnInit;
  Autoplay:= true;
  H := H*0.75;
  W := W*0.75;
  LookingLeft := true;
  Bubble := CreateTextBubble('Where are my parents?');
  BubbleTooFar := CreateTextBubble('Please stay close to me!');
  //JumpStrength := 4;
end;

procedure TSmallTux.OnTimer;
begin
  inherited OnTimer;
  if Parent = nil then
  begin
    if sqr(X-Context.tux.X)+sqr(Y-Context.tux.Y) < sqr(50) then
    begin
      Parent := Context.tux;
      Bubble := CreateTextBubble('Hey my parent!');
      BubbleTime := 400;
    end;
  end;
end;

{ TGameContext }

constructor TGameContext.Create;
begin
  LeftKey := false;
  RightKey := false;
  UpKey := false;
  DataLoaded:= false;
end;

destructor TGameContext.Destroy;
begin
  BGLSpriteEngine.Clear;
  inherited Destroy;
end;

procedure TGameContext.Render(AWidth, AHeight: integer);
begin
  LoadData;
  BGLViewPort(AWidth, AHeight);

  BGLCanvas.FillQuadLinearColor(PointF(0,0),PointF(AWidth,0),PointF(AWidth,AHeight),PointF(0,AHeight),
        CSSSkyBlue,CSSSkyBlue,MergeBGRA(CSSSkyBlue,CSSBlue),MergeBGRA(CSSSkyBlue,CSSBlue));

  BGLSpriteEngine.OnDraw;
end;

procedure TGameContext.LoadData;
var sunBmp: TBGLBitmap;
begin
  if DataLoaded then exit;
  texWalking := BGLTexture(ResourceDir+'tux_walking.png');
  texWalking.SetFrameSize(64,64);

  texGround := BGLTexture(ResourceDir+'ground.png');
  texGround.SetFrameSize(32,32);

  AddGround(32,128,200);
  AddGround(400,128,200);
  AddGround(200,250,200);
  AddGround(-32,250,150);
  AddGround(400,380,200);
  AddGround(300,500,150);
  AddGround(-32,600-32,800+64);

  tux := TTux.Create(texWalking,self);
  tux.Location := PointF(128,128);

  smallTux := TSmallTux.Create(texWalking,self);
  smallTux.Location := PointF(450,128);

  sunBmp := TBGLBitmap.Create(100,100);
  sunBmp.FillEllipseLinearColorAntialias(50,50,49,49,CSSYellow,CSSOrange);
  sun := sunBmp.MakeTextureAndFree;
  TBGLSprite.Create(sun,-2).Location := PointF(0,0);

  DataLoaded:= true;
end;

procedure TGameContext.AddGround(x, y, w: single);
begin
  TGround.Create(texGround,x,y,1);
  x += 32;
  w -= 32;
  while w > 32 do
  begin
    TGround.Create(texGround,x,y,2);
    x += 32;
    w -= 32;
  end;
  TGround.Create(texGround,x,y,3);
end;

procedure TGameContext.Elapse(ms: single);
begin
  elapsedMs += ms;
  while elapsedMs > FrameDurationMs do
  begin
    BGLSpriteEngine.OnTimer;
    elapsedMs -= FrameDurationMs;
  end;
end;

function TGameContext.FindGround(x: single; var y: single): boolean;
var
  i: Integer;
  s: TBGLSprite;
  g: TGround;
begin
  for i := 0 to BGLSpriteEngine.Count-1 do
  begin
    s := BGLSpriteEngine.Sprite[i] as TBGLSprite;
    if s is TGround then
    begin
      g := TGround(s);
      if (x >= g.X-g.W/2) and (x <= g.X+g.W/2) then
      begin
        if (y >= g.Y-4) and (Y <= g.Y-4+16) then
        begin
          result := true;
          y := g.Y-4;
          exit;
        end;
      end;
    end;
  end;
  result := false;
end;

procedure TGameContext.MouseDown(Button: TMouseButton; X, Y: integer);
begin

end;

procedure TGameContext.MouseMove(X, Y: integer);
begin

end;

procedure TGameContext.MouseUp(Button: TMouseButton);
begin

end;

{ TGround }

constructor TGround.Create(ATexture: IBGLTexture; AX, AY: Single;
  AFrame: integer);
begin
  inherited Create(ATexture, -1);
  Location := PointF(AX,AY+4);
  Frame := AFrame;
end;

procedure TGround.OnInit;
begin
  HorizontalAlign := taCenter;
  VerticalAlign := tlCenter;
end;

{ TTux }

procedure TTux.OnInit;
begin
  HorizontalAlign := taCenter;
  VerticalAlign := tlBottom;
  Frame := 1;
  Speed := PointF(0,0);
  FrameLoopStart := 1;
  FrameLoopEnd := 10;
  LookingLeft:= false;
  goRight:= false;
  goLeft:= false;
  goUp := false;
  waiting := 0;
  JumpStrength:= 5;
  Parent := nil;
  Autoplay:= false;
end;

procedure TTux.OnDraw;
begin
  if lookingLeft then Texture.ToggleFlipX;
  inherited OnDraw;
  if lookingLeft then Texture.ToggleFlipX;
  if Bubble <> nil then
  begin
    if BubbleTime mod 500 > 400 then
    begin
      if ShowBubbleTooFar then
        BubbleTooFar.Draw(x,y-H,taCenter,tlBottom)
      else
        Bubble.Draw(x,y-H,taCenter,tlBottom);
    end;
  end;
end;

procedure TTux.OnTimer;
var curY: single;
  nearParent: boolean;
begin
  if not OnTheGround then
  begin
    //jumping
  end else
  begin
    nearParent:= false;
    if Autoplay then
    begin
      if waiting > 0 then
        dec(waiting)
      else
      begin
        goRight:= false;
        goLeft:= false;
        goUp := false;
        if Parent<> nil then
          nearParent := (abs(X-Parent.X) < 100) and (abs(Y-Parent.Y) < 150);

        ShowBubbleTooFar := (Parent <> nil) and not nearParent;
        if (Parent<> nil) and (abs(X-Parent.X) < 200) and (abs(Y-Parent.Y) < 200) then
        begin
          if BubbleTime > 500 then
          begin
            if not nearParent then
              BubbleTime := 0
            else
              BubbleTime := 500;
          end;
          if (abs(X-Parent.X) > 40) then
          begin
            waiting := random(150);
            if X > Parent.X then goLeft := true
            else goRight := true;
          end else
          if (Parent.Y < Y-40) and (Parent.Y > Y-200) then
          begin
            waiting := random(150);
            goUp := true;
            waiting := 0;
          end;
        end else
        begin
          waiting := random(300);
          case random(10) of
            0..2: goLeft:= true;
            3..5: goRight:= true;
            9: begin goUp:= true; waiting := 0; end;
          end;
        end;
      end;
      curY := Y+5;
      if goLeft and (X < 1) then
      begin
        goLeft := false;
        waiting:= 0;
      end;
      if goRight and (X > 799) then
      begin
        goRight := false;
        waiting:= 0;
      end;
      if not nearParent then
      begin
        if goLeft and not Context.FindGround(X-20,curY) then
        begin
          goLeft:= false;
          waiting:= 0;
        end;
        if goRight and not Context.FindGround(X+20,curY) then
        begin
          goRight:= false;
          waiting := 0;
        end;
      end else
      if (Parent <> nil) and (abs(X-Parent.X) < 5) then
      begin
        goLeft:= false;
        goRight:= false;
      end;
    end else
    begin
      goRight:= Context.RightKey;
      goLeft:= Context.LeftKey;
      goUp := Context.UpKey;
    end;
    if not goRight and not goLeft and not goUp then
    begin
      speed.X := speed.X*0.9;
      if abs(speed.X)<0.1 then speed.X := 0;
    end else
    begin
      //on the ground and can move
      if goRight then Speed.X += 0.1;
      if Speed.X > 1.3 then Speed.X := 1.3;
      if goLeft then Speed.X -= 0.1;
      if Speed.X < -1.3 then Speed.X := -1.3;
      if goUp then Speed.Y := -JumpStrength;

      if (Speed.X < 0) and not LookingLeft and (round(Frame) = 1) then
        lookingLeft:= true;
      if (Speed.X > 0) and LookingLeft and (round(Frame) = 1) then
        lookingLeft := false;
    end;
    if LookingLeft xor (Speed.X < 0) then
      Frame := Frame-Speed.X*0.5
    else
      Frame := Frame+Speed.X*0.5;
  end;

  Speed.Y += 0.1;
  Location := Location+Speed;
  curY := Y;
  OnTheGround:= Context.FindGround(X,curY);
  if OnTheGround then Speed.Y := 0;
  Y := curY;
  if X < 0 then X := 0;
  if X > 800 then X := 800;
  inc(BubbleTime);
end;

constructor TTux.Create(ATexture: IBGLTexture; AContext: TGameContext);
begin
  inherited Create(ATexture, 0);
  Context := AContext;
end;

end.

