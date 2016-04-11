program demo06;

{$I zglCustomConfig.cfg}

{$IFDEF WINDOWS}
  {$R *.res}
{$ENDIF}

uses
  Classes,BGRAGraphics,
  BGRABitmapTypes,
  BGRAZenGL in '..\BGRAZenGL.pas',
  {$IFDEF USE_ZENGL_STATIC}
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_keyboard,
  zgl_render_2d,
  zgl_fx,
  zgl_primitives_2d,
  zgl_math_2d,
  zgl_utils
  {$ELSE}
  zglHeader
  {$ENDIF}
  ;

var
  dirRes  : UTF8String {$IFNDEF MACOSX} = '../data/' {$ENDIF};
  fntMain : IBGLFont;

procedure Init;
  //var
  //  i : Integer;
begin
  // RU: Загружаем данные о шрифте.
  // EN: Load the font.
  fntMain := BGLZenFont( dirRes + 'font.zfi' );
  //fntMain.Clipped := true;
  // RU: Если же текстуры именуются без использования маски вида "$(имя_шрифта)FontName-page$(номер).$(расширение)", то загрузку можно произвести следующим образом(для png):
  // EN: If textures were named without special mask - "$(font_name)-page$(number).$(extension)", then use this method to load them(for png):
  //for i := 0 to fntMain.Count.Pages - 1 do
  //  fntMain.Pages[ i ] := tex_LoadFromFile( dirRes + 'font-page' + u_IntToStr( i ) + '.png' );
end;

procedure Draw;
  var
    r : zglTRect;
    s : UTF8String;
begin
  batch2d_Begin();

  // RU: ZenGL работает исключительно с кодировкой UTF-8, поэтому весь текст должен быть в UTF-8.
  // EN: ZenGL works only with UTF-8 encoding, so all text should be encoded with UTF-8.

  fntMain.TextOut(400, 25, 'String with center alignment', taCenter);

  fntMain.Scale := 2;
  fntMain.TextOut(400, 65, 'Scaling', taCenter);
  fntMain.Scale := 1;

  fntMain.SetGradientColors(CSSRed,CSSLime,CSSBlue,CSSWhite);
  fntMain.TextOut( 400, 125, 'Gradient color for every symbol', taCenter);
  fntMain.GradientColors := false;

  r.X := 0;
  r.Y := 300 - 128;
  r.W := 192;
  r.H := 256;
  with r do fntMain.TextRect(X,Y,W,H, 'Simple text rendering in rectangle' );
  pr2d_Rect( r.X, r.Y, r.W, r.H, $FF0000 );

  r.X := 800 - 192;
  r.Y := 300 - 128;
  r.W := 192;
  r.H := 256;
  with r do fntMain.TextRect(X,Y,W,H, 'Text rendering using horizontal right alignment and vertical bottom alignment', taRightJustify, tlBottom);
  pr2d_Rect( r.X, r.Y, r.W, r.H, $FF0000 );

  r.X := 400 - 192;
  r.Y := 300 - 128;
  r.W := 384-200;
  r.H := 256-190;
  fntMain.Clipped:= true;
  fntMain.Justify:= true;
  with r do fntMain.TextRect(X,Y,W,H, 'This text uses justify alignment and centered vertically. Text which doesn''t fit inside the rectangle will be cropped.',
                    tlCenter);
  fntMain.Justify:= false;
  fntMain.Clipped:= false;
  pr2d_Rect( r.X, r.Y, r.W, r.H, $FF0000 );

  r.X := 400 - 320;
  r.Y := 300 + 160;
  r.W := 640;
  r.H := 128;
  with r do fntMain.TextRect(X,Y,W,H, 'For starting new line LF symbol can be used' + #10 + 'code of which is equal to 10 and named in Unicode as "Line Feed"',
                    taCenter, tlCenter);
  pr2d_Rect( r.X, r.Y, r.W, r.H, $FF0000 );

  // RU: Выводим количество FPS в правом углу
  // EN: Render FPS in the top right corner
  s := 'FPS: ' + u_IntToStr( zgl_Get( RENDER_FPS ) );
  fntMain.TextOut(800,0, s, taRightJustify);

  batch2d_End();
end;

procedure Timer;
begin
  if key_Press( K_ESCAPE ) Then zgl_Exit();

  key_ClearState();
end;

Begin
  {$IFNDEF USE_ZENGL_STATIC}
  if not zglLoad( libZenGL ) Then exit;
  {$ENDIF}

  randomize();

  timer_Add( @Timer, 16 );

  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Draw );

  wnd_SetCaption( '06 - Text' );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init();
End.
