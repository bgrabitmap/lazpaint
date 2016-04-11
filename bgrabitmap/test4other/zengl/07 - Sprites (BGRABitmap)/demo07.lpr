program demo07;

{$I zglCustomConfig.cfg}

{$IFDEF WINDOWS}
  {$R *.res}
{$ENDIF}

uses
  Classes,sysutils,
  BGRABitmapTypes,BGRAGraphics,
  BGRAZenGL in '..\BGRAZenGl.pas',
  {$IFDEF USE_ZENGL_STATIC}
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_keyboard,
  zgl_camera_2d,
  zgl_render_2d
  {$ELSE}
  zglHeader
  {$ENDIF}
  ;

type
  TTux = record
    Texture : IBGLTexture;
    Frame   : Integer;
    Pos     : TPointF;
  end;

const
  tuxSpeech : array[0..10] of string =
    ('Hello!', '', 'This is an example with BGRABitmap and ZenGL.', 'Text bubbles are rendered with BGRABitmap and LazFreeType.',
    'Then they are used as textures in OpenGL', '', 'In order to keep a high frame rate,',
    'textures are rather prepared in advance.', 'However, some textures can be generated on the fly',
    'as long as they are small enough.', '');

var
  dirRes      : UTF8String {$IFNDEF MACOSX} = '../data/' {$ENDIF};
  texBack     : IBGLTexture;
  texLogo     : IBGLTexture;
  texGround   : IBGLTexture;
  texTuxWalk  : IBGLTexture;
  texTuxStand : IBGLTexture;
  texBublRed  : IBGLTexture;
  texBublBig  : IBGLTexture;
  texBublGhost: IBGLTexture;
  texSpeech   : IBGLTexture;
  bmpTime     : TBGLBitmap;
  fntMain     : IBGLFont;
  tux         : array[ 0..20 ] of TTux;
  speechIndex : integer;
  camMain     : zglTCamera2D;
  time        : Integer;
  tuxHorizMargin: single;

function CreateTextBubble(AText: string): IBGLTexture;
const horizMargin = 6; vertMargin = 4;
var bmp: TBGLBitmap;
begin
  bmp := TBGLBitmap.Create;
  bmp.FontHeight := 14;
  bmp.FontStyle := [fsBold];
  bmp.SetSize(((bmp.TextSize(AText).cx+2*horizMargin+2)+1) and not 1, bmp.FontPixelMetric.DescentLine+vertMargin*2+2);
  bmp.FillTransparent;
  bmp.RoundRectAntialias(1,1,bmp.Width-2,bmp.Height-2,12,12, BGRABlack, 1, BGRA(255,255,250));
  bmp.TextOut(horizMargin+1,vertMargin+1,AText, BGRABlack);
  result := bmp.MakeTextureAndFree;
end;

procedure Init;
const rectBorder=1;
  var
    i : Integer;
begin
  TBGLBitmap.AddFreeTypeFontFolder(StringReplace(dirRes, '/', PathDelim, [rfReplaceAll]), true);

  // RU: Т.к. по умолчанию вся структура камеры заполняется нулями, следует инициализировать её стандартными значениями.
  // EN: Camera must be initialized, because camera structure is zero-filled by default.
  cam2d_Init( camMain );

  texLogo := BGLTexture(dirRes + 'zengl.png');
  texBack := BGLTexture(dirRes + 'back01.jpg');

  texGround := BGLTexture(dirRes + 'ground.png');
  // RU: Указываем размер кадра в текстуре.
  // EN: Set the size of single frame for texture.
  texGround.SetFrameSize(32,32);

  texTuxWalk := BGLTexture( dirRes + 'tux_walking.png' );
  texTuxWalk.SetFrameSize( 64, 64 );

  texBublRed := CreateTextBubble('I''m so red...');
  texBublBig := CreateTextBubble('I''m so big...');
  texBublGhost := CreateTextBubble('???');

  texTuxStand := BGLTexture( dirRes + 'tux_stand.png' );
  texTuxStand.SetFrameSize( 64, 64 );

  speechIndex := -1;
  texSpeech := nil;

  tuxHorizMargin:= 10*96 - 800;
  for i := 0 to 9 do
    begin
      tux[ i ].Texture := texTuxWalk;
      tux[ i ].Frame   := random( 19 ) + 2;
      tux[ i ].Pos.X   := i * 96 - tuxHorizMargin/2;
      tux[ i ].Pos.Y   := 32 + 64;
    end;
  for i := 10 to 19 do
    begin
      tux[ i ].Texture := texTuxWalk;
      tux[ i ].Frame   := random( 19 ) + 2;
      tux[ i ].Pos.X   := ( i - 9 ) * 96 - tuxHorizMargin/2;
      tux[ i ].Pos.Y   := 600 - 32;
    end;
  tux[ 20 ].Texture := texTuxStand;
  tux[ 20 ].Frame   := random( 19 ) + 2;
  tux[ 20 ].Pos.X   := 400;
  tux[ 20 ].Pos.Y   := 300 - 4;

  fntMain := BGLFont('Arial', 20, BGRAWhite,BGRABlack);
end;

procedure ExitProg;
begin
  bmpTime.Free;
end;

procedure UpdateBmpTime;
var
  x : Single;
  timeMask: TBGLBitmap;
begin
  //prepare texture for rotating text
  if bmpTime = nil then
    bmpTime := TBGLBitmap.Create(256,32);
  //draw gradient background
  bmpTime.GradientFill(0,0,bmpTime.Width,bmpTime.Height, BGRABlack,BGRAPixelTransparent, gtReflected,
                       PointF(0,bmpTime.Height/2),PointF(0,0),dmSet);
  bmpTime.FontHeight := bmpTime.Height;
  x := time - trunc(time/(2*bmpTime.width))*bmpTime.width*2;
  if x > bmpTime.Width then x := 2*bmpTime.width-x;
  bmpTime.TextOut(x, bmpTime.Height/2 - bmpTime.FontFullHeight/2, TimeToStr(Now), BGRAWhite, taCenter);
  //apply alpha gradient
  timeMask := TBGLBitmap.Create(bmpTime.width,bmpTime.Height);
  timeMask.GradientFill(0,0,timeMask.width,timeMask.Height, BGRAWhite,BGRABlack, gtLinear, PointF(timeMask.Width-64,0),PointF(timeMask.Width,0),dmSet,false);
  bmpTime.ApplyMask(timeMask,Rect(0,0,bmpTime.Width,bmpTime.Height),Point(0,0));
  timeMask.Free;
end;

procedure Draw;
  var
    i : Integer;
    a,t : Single;
    pt : TPointF;
    newSpeechIndex: Integer;
begin
  batch2d_Begin();
  if time > 255 Then
    begin
      // RU: Для увеличения быстродействия можно отключить очистку буфера цвета, учитывая что экран полностью заполнен.
      // EN: Rendering perfomance can be increased by disabling clearing the color buffer. This is a good idea because screen is full of objects.
      zgl_Disable( COLOR_BUFFER_CLEAR );


      // RU: Рисуем задний фон с размерами 800х600 используя текстуру "texBack".
      // EN: Render the background with size 800x600 and using texture "texBack".
      texBack.StretchDraw(0,0,800,600);

      UpdateBmpTime;

      //draw in four directions
      if time > 512 then
        a := (time-512)/3
      else
        a := 0;
      pt := PointF(0,bmpTime.Height*0.5);
      bmpTime.Texture.DrawAngle(400,300, 0 + a, pt, False, CSSOrange);
      bmpTime.Texture.Mask.DrawAngle(400,300, 90 + a, pt, False, CSSSkyBlue);
      bmpTime.Texture.DrawAngle(400,300, 180 + a, pt, False, CSSYellowGreen);
      bmpTime.Texture.Mask.DrawAngle(400,300, 270 + a, pt, False);

      // RU: Установить текущую камеру.
      // EN: Set the current camera.
      cam2d_Set( @camMain );

      // RU: Рисуем землю.
      // EN: Render the ground.
      for i := -2 to 800 div texGround.FrameWidth + 1 do
        texGround.Frame[2].Draw(i * texGround.FrameWidth, 96 - 12);
      for i := -2 to 800 div texGround.FrameWidth + 1 do
        texGround.Frame[2].Draw(i * texGround.FrameWidth, 600 - texGround.FrameHeight - 12);

      // RU: Рисуем шагающих пингвинов.
      // EN: Render penguins
      for i := 0 to 9 do
        if i = 2 Then
          begin
            // RU: Рисуем надпись в "рамочке" над пингвином.
            // EN: Render the text in frame over penguins.
            texBublRed.StretchDrawAngle(tux[ i ].Pos.X, tux[ i ].Pos.Y - tux[i].Texture.FrameHeight, texBublRed.Width*1.5,texBublRed.Height*1.5, sin(time*Pi/500)*30, taCenter,tlBottom);
            // RU: Рисуем красного пингвина
            // EN: Render red penguin
            tux[i].Texture.SetGradientColors(CSSWhite,CSSWhite,CSSRed,CSSRed);
            tux[i].Texture.Frame[tux[ i ].Frame div 2].Draw(tux[ i ].Pos.X, tux[ i ].Pos.Y, taCenter,tlBottom);
            tux[i].Texture.GradientColors := false;
          end else
            if i = 7 Then
              begin
                texBublGhost.Draw(tux[ i ].Pos.X, tux[ i ].Pos.Y - tux[i].Texture.FrameHeight,taCenter,tlBottom,128);
                // RU: Рисуем пингвина приведение
                // EN: Render penguin ghost
                tux[i].Texture.Frame[tux[ i ].Frame div 2].Mask.Draw(tux[ i ].Pos.X, tux[ i ].Pos.Y, taCenter,tlBottom, 155);
              end else
                tux[i].Texture.Frame[tux[ i ].Frame div 2].Draw(tux[ i ].Pos.X, tux[ i ].Pos.Y, taCenter,tlBottom);

      // RU: Рисуем пингвинов шагающих в обратную сторону
      // EN: Render penguins, that go another way
      for i := 10 to 19 do
        if i = 13 Then
          begin
            texBublBig.Draw(tux[ i ].Pos.X, tux[ i ].Pos.Y-80, taCenter,tlBottom);
            // RU: Рисуем "большего" пингвина
            // EN: Render "big" penguin
            tux[i].Texture.Frame[tux[ i ].Frame div 2].FlipX.StretchDraw(tux[ i ].Pos.X, tux[ i ].Pos.Y, 80, 80, taCenter, tlBottom);
          end else
            if i = 17 Then
              begin
                // RU: Рисуем "высокого" пингвина
                // EN: Render "tall" penguin
                tux[i].Texture.Frame[tux[ i ].Frame div 2].FlipX.StretchDraw(tux[ i ].Pos.X, tux[ i ].Pos.Y, 64, 80, taCenter, tlBottom);
              end else
                tux[i].Texture.Frame[tux[ i ].Frame div 2].FlipX.Draw(tux[ i ].Pos.X, tux[ i ].Pos.Y, taCenter, tlBottom);

      // RU: Сбросить камеру.
      // EN: Reset the camera.
      cam2d_Set( nil );

      // RU: Рисуем участок земли по центру экрана.
      // EN: Render piece of ground in the center of screen.
      texGround.Frame[1].Draw(11 * texGround.FrameWidth, 300 - 16);
      texGround.Frame[2].Draw(12 * texGround.FrameWidth, 300 - 16);
      texGround.Frame[3].Draw(13 * texGround.FrameWidth, 300 - 16);

      newSpeechIndex := trunc(time/500) mod length(tuxSpeech);
      if newSpeechIndex <> speechIndex then
      begin
        if texSpeech<> nil then
        begin
          texSpeech.FreeMemory;
          texSpeech := nil;
        end;
        speechIndex:= newSpeechIndex;
        if tuxSpeech[speechIndex]<>'' then
          texSpeech := CreateTextBubble(tuxSpeech[speechIndex]);
      end;
      if texSpeech <> nil then texSpeech.Draw(tux[ 20 ].Pos.X, tux[ 20 ].Pos.Y - tux[ 20 ].Texture.FrameHeight, taCenter, tlBottom);

      tux[ 20 ].Texture.Frame[tux[ 20 ].Frame div 2].Draw(tux[ 20 ].Pos.X, tux[ 20 ].Pos.Y, taCenter, tlBottom);
    end;

  if time <= 255 Then
    texLogo.Draw(400,300, taCenter,tlCenter, time)
  else
    if time < 510 Then
      begin
        BGLCanvas.FillRect(RectF( 0, 0, 800, 600), BGRA(0,0,0,510 - time));
        texLogo.Draw(400,300, taCenter,tlCenter, 510-time);
      end;

  if time > 255 Then
    fntMain.TextOut(0, 0, 'FPS: ' + IntToStr( zgl_Get( RENDER_FPS ) ));
  batch2d_End();
end;

procedure Timer;
  var
    i : Integer;
begin
  INC( time, 2 );

  if time > 1000*Pi then
    camMain.Angle := camMain.Angle + cos( time / 1000 ) / 10;

  for i := 0 to 20 do
    if (i <> 20) or (texSpeech <> nil) then
    begin
      INC( tux[ i ].Frame );
      if tux[ i ].Frame > 20 Then
        tux[ i ].Frame := 2;
    end;
  for i := 0 to 9 do
    begin
      tux[ i ].Pos.X := tux[ i ].Pos.X + 1.5;
      if tux[ i ].Pos.X >= 800 + tuxHorizMargin/2 Then
        tux[ i ].Pos.X := -tuxHorizMargin/2;
    end;
  for i := 10 to 19 do
    begin
      tux[ i ].Pos.X := tux[ i ].Pos.X - 1.5;
      if tux[ i ].Pos.X <= -tuxHorizMargin/2 Then
        tux[ i ].Pos.X := 800+tuxHorizMargin/2;
    end;

  if key_Press( K_ESCAPE ) Then zgl_Exit();
  key_ClearState();
end;

Begin
  {$IFNDEF USE_ZENGL_STATIC}
  if not zglLoad( libZenGL ) Then exit;
  {$ENDIF}

  if not FileExists(dirRes+'arial.ttf') then
     raise exception.Create('Please copy "arial.ttf" in directory "'+dirRes+'"');

  randomize();

  timer_Add( @Timer, 16 );

  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Draw );
  zgl_Reg( SYS_EXIT, @ExitProg );

  wnd_SetCaption( '07 - Sprites' );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init();
End.
