program demo08;

{$I zglCustomConfig.cfg}

{$IFDEF WINDOWS}
  {$R *.res}
{$ENDIF}

uses
  BGRAZenGL in '..\BGRAZenGL.pas',
  BGRABitmapTypes,
  {$IFDEF USE_ZENGL_STATIC}
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_keyboard,
  zgl_render_2d,
  zgl_primitives_2d,
  zgl_utils
  {$ELSE}
  zglHeader
  {$ENDIF}
  ;

type
  { TMikuSprite }

  TMikuSprite = class(TBGLSprite)
    Speed: TPointF;
    procedure OnInit; override;
    procedure OnTimer; override;
    procedure OnDraw; override;
  end;

var
  dirRes    : UTF8String {$IFNDEF MACOSX} = '../data/' {$ENDIF};
  fntMain   : IBGLFont;
  texLogo   : IBGLTexture;
  texMiku   : IBGLTexture;
  time      : Integer;

{ TMikuSprite }

procedure TMikuSprite.OnInit;
var ratio: single;
begin
  X := 800 + random( 800 ) + W/2;
  Y := random( 600 - round(H) ) + H/2;
  HorizontalAlign:= taCenter;
  VerticalAlign:= tlCenter;

  // RU: Задаем скорость движения.
  // EN: Set the moving speed.
  Speed.X := -random( 10 ) / 5 - 0.5;
  Speed.Y := ( random( 10 ) - 5 ) / 5;

  ratio := (Layer+3)/(9+3);
  W := W*ratio;
  H := H*ratio;

  Color := BGRA(round(255*ratio),round(255*ratio),round(255*ratio));
end;

procedure TMikuSprite.OnTimer;
begin
  Location := Location + Speed;
  Frame := Frame + ( abs( speed.X ) + abs( speed.Y ) ) / 25;
  if Frame >= 8.49 Then
    Frame := 0.51;

  // RU: Если спрайт выходит за пределы по X, сразу же удаляем его.
  // EN: Delete the sprite if it goes beyond X.
  if X < -W/2 Then QueryDestroy;

  // RU: Если спрайт выходит за пределы по Y, ставим его в очередь на удаление.
  // EN: Add sprite to queue for delete if it goes beyond Y.
  if Y < -H/2 Then QueryDestroy;
  if Y > 600+H/2  Then QueryDestroy;
end;

procedure TMikuSprite.OnDraw;
begin
  if Layer = 9 then
    Texture.BlendMode := obmAdd
  else
    Texture.BlendMode := obmNormal;
  inherited OnDraw;
end;

// RU: Добавить 100 спрайтов.
// EN: Add 100 sprites.
procedure AddMiku;
  var
    i : Integer;
begin
  for i := 1 to 100 do
    TMikuSprite.Create( texMiku, random( 91 ) div 10 );
end;

// RU: Удалить 100 спрайтов.
// EN: Delete 100 sprites.
procedure DelMiku;
  var
    i : Integer;
begin
  // RU: Удалим 100 спрайтов со случайным ID.
  // EN: Delete 100 sprites with random ID.
  for i := 1 to 100 do
    BGLSpriteEngine.Delete( random( BGLSpriteEngine.Count ));
end;

procedure Init;
  var
    i : Integer;
begin
  texLogo := BGLTexture( dirRes + 'zengl.png' );

  texMiku := BGLTexture( dirRes + 'miku.png' );
  texMiku.SetFrameSize( 128, 128 );

  // RU: Создадим 200 спрайтов Miku-chan :)
  // EN: Create 200 sprites of Miku-chan :)
  for i := 1 to 2 do
    AddMiku();

  fntMain := BGLZenFont( dirRes + 'font.zfi' );
end;

procedure Draw;
begin
  batch2d_Begin();
  // RU: Рисуем все спрайты находящиеся в текущем спрайтовом менеджере.
  // EN: Render all sprites contained in current sprite engine.
  if time > 255 Then
  begin
    BGLSpriteEngine.OnDraw;
    BGLCanvas.BlendMode := obmAdd;
    BGLCanvas.FillEllipse(400,300,100,100, BGRA(255,0,0,128),False);
    BGLCanvas.BlendMode := obmNormal;
  end;

  if time <= 255 Then
    texLogo.Draw( 400, 300, taCenter, tlCenter, time )
  else
    if time < 510 Then
      begin
        BGLCanvas.FillRect(RectF(0, 0, 800, 600), BGRA(0,0,0,510 - time));
        texLogo.Draw( 400, 300, taCenter, tlCenter, 510 - time)
      end;

  if time > 255 Then
    begin
      BGLCanvas.FillRect(RectF(0, 0, fntMain.TextWidth('Up/Down - Add/Delete Miku :)')+4, 64), BGRA(0,0,0,200));

      fntMain.TextOut( 0, 0, 'FPS: ' + u_IntToStr( zgl_Get( RENDER_FPS ) ) );
      fntMain.TextOut( 0, 20, 'Sprites: ' + u_IntToStr( BGLSpriteEngine.Count ) );
      fntMain.TextOut( 0, 40, 'Up/Down - Add/Delete Miku :)' );
    end;
  batch2d_End();
end;

procedure Timer;
begin
  INC( time, 2 );

  // RU: Выполняем обработку всех спрайтов в текущем спрайтовом менеджере.
  // EN: Process all sprites contained in current sprite engine.
  BGLSpriteEngine.OnTimer;

  // RU: По нажатию пробела очистить все спрайты.
  // EN: Delete all sprites if space was pressed.
  if key_Press( K_SPACE ) Then BGLSpriteEngine.Clear;
  if key_Press( K_UP ) Then AddMiku();
  if key_Press( K_DOWN ) Then DelMiku();

  if key_Press( K_ESCAPE ) Then zgl_Exit();

  key_ClearState();
end;

procedure Quit;
begin
  // RU: Очищаем память от созданных спрайтов.
  // EN: Free allocated memory for sprites.
  BGLSpriteEngine.Clear;
end;

Begin
  {$IFNDEF USE_ZENGL_STATIC}
  if not zglLoad( libZenGL ) Then exit;
  {$ENDIF}

  randomize;

  timer_Add( @Timer, 16 );
  timer_Add( @AddMiku, 1000 );

  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Draw );
  zgl_Reg( SYS_EXIT, @Quit );

  wnd_SetCaption( '08 - Sprite Engine' );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init();
End.
