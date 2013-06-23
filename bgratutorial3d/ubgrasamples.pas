unit ubgrasamples;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics,
  BGRABitmapTypes, BGRABitmap, BGRAGradients, BGRAButton;

type
  TBGRASampleStyle = (ssFlashPlayer, ssWin7ToolBar, ssWin7, ssOffice2010,
                      ssMacOSXLion);

{ Drawings }
procedure DrawFlashPlayerBody(ABitmap: TBGRABitmap);
procedure DrawFlashPlayerButtonPanel(ABitmap: TBGRABitmap);
procedure DrawWin7ToolBar(ABitmap: TBGRABitmap; ADir: TAlign);

{ StyleButtons }
procedure StyleButtons(AControl: TControl; AButton: TBGRAButton);
procedure StyleButtonsSample(AControl: TControl; AStyle: TBGRASampleStyle);

{ Buttons }
procedure FlashPlayerButton(AButton: TBGRAButton);
procedure Win7ToolBarButton(AButton: TBGRAButton);
procedure Win7Button(AButton: TBGRAButton);
procedure Office2010Button(AButton: TBGRAButton);
procedure MacOSXLionButton(AButton: TBGRAButton);

implementation

{ Drawings }

procedure DrawFlashPlayerBody(ABitmap: TBGRABitmap);
begin
  with ABitmap do begin
    GradientFill(0,0,Width,Height,BGRA(203,19,23,255),BGRA(110,3,20,255),
    gtLinear,PointF(0,0),PointF(0,Height),dmSet);
    Rectangle(0,0,Width,Height+1,BGRA(0,0,0,215),dmDrawWithTransparency);
  end;
end;

procedure DrawFlashPlayerButtonPanel(ABitmap: TBGRABitmap);
begin
  with ABitmap do begin
    DrawHorizLine(0,0,Width,BGRA(30,30,30,255));
    DrawHorizLine(0,Height-1,Width,BGRA(62,62,62,255));
    Rectangle(0,1,Width,Height-1,BGRA(91,91,91,255),BGRA(76,76,76,255),dmSet);
  end;
end;

procedure DrawWin7ToolBar(ABitmap: TBGRABitmap; ADir: TAlign);
var
  tempBmp: TBGRABitmap;
begin
  tempBmp := DoubleGradientAlphaFill(Rect(0,0,ABitmap.Width,ABitmap.Height),
  BGRA(245,250,255,255),BGRA(230,240,250,255),
  BGRA(220,230,244,255),BGRA(221,233,247,255),
  gdVertical,gdVertical,gdVertical,0.50);
  ABitmap.PutImage(0,0,tempBmp,dmSet);
  tempBmp.Free;
  case ADir of
    alLeft :  with ABitmap do begin
      Rectangle(0,0,Width-2,Height,BGRA(255,255,255,100),dmDrawWithTransparency);
      SetVertLine(Width-1,0,Height-1,BGRA(160,175,195,255));
      SetVertLine(Width-2,0,Height-1,BGRA(205,218,234,255));
    end;
    alTop : with ABitmap do begin
      Rectangle(0,0,Width,Height-2,BGRA(255,255,255,100),dmDrawWithTransparency);
      SetHorizLine(0,Height-1,Width-1,BGRA(160,175,195,255));
      SetHorizLine(0,Height-2,Width-1,BGRA(205,218,234,255));
    end;
    alRight : with ABitmap do begin
      Rectangle(2,0,Width,Height,BGRA(255,255,255,100),dmDrawWithTransparency);
      SetVertLine(0,0,Height,BGRA(160,175,195,255));
      SetVertLine(1,0,Height,BGRA(205,218,234,255));
    end;
    alBottom : with ABitmap do begin
      Rectangle(0,2,Width,Height,BGRA(255,255,255,100),dmDrawWithTransparency);
      SetHorizLine(0,0,Width-1,BGRA(160,175,195,255));
      SetHorizLine(0,1,Width-1,BGRA(205,218,234,255));
    end;
    alClient, alCustom, alNone : with ABitmap do begin
      Rectangle(0,0,Width,Height,BGRA(255,255,255,100),dmDrawWithTransparency);
    end;
  end;
end;

{ StyleButtons }

procedure StyleButtons(AControl: TControl; AButton: TBGRAButton);
var
  i: Integer;
  WinControl: TWinControl;
begin
  if AControl is TBGRAButton then AControl.Assign(AButton);
  if AControl is TWinControl then begin
    WinControl:=TWinControl(AControl);
    if WinControl.ControlCount = 0 then exit;
    for i:=0 to WinControl.ControlCount-1 do
      StyleButtons(WinControl.Controls[i],AButton);
  end;
end;

procedure StyleButtonsSample(AControl: TControl; AStyle: TBGRASampleStyle);
var
  tempBGRAButton: TBGRAButton;
begin
  tempBGRAButton:=TBGRAButton.Create(nil);
  case AStyle of
    ssFlashPlayer : FlashPlayerButton(tempBGRAButton);
    ssWin7Toolbar : Win7ToolBarButton(tempBGRAButton);
    ssWin7        : Win7Button(tempBGRAButton);
    ssOffice2010  : Office2010Button(tempBGRAButton);
    ssMacOSXLion  : MacOSXLionButton(tempBGRAButton);
  end;
  StyleButtons(AControl,tempBGRAButton);
  tempBGRAButton.Free;
end;

{ Buttons }

procedure FlashPlayerButton(AButton: TBGRAButton);
begin
  with AButton do begin
    TextShadowOffSetX:=1;
    TextShadowOffSetY:=1;
    TextShadowRadius:=0;
    with BodyNormal do begin
      BorderColor:=RGBToColor(24,24,24);
      Font.Color:=clWhite;
      Font.Size:=7;
      LightOpacity:=20;
      LightWidth:=1;
      Gradient1.StartColor:=RGBToColor(104,104,104);
      Gradient1.EndColor:=RGBToColor(104,104,104);
      Gradient2.StartColor:=RGBToColor(103,103,103);
      Gradient2.EndColor:=RGBToColor(71,71,71);
    end;
    with BodyHover do begin
      BorderColor:=RGBToColor(24,24,24);
      Font.Color:=clWhite;
      Font.Size:=7;
      LightOpacity:=20;
      LightWidth:=1;
      Gradient1.StartColor:=RGBToColor(118,118,118);
      Gradient1.EndColor:=RGBToColor(118,118,118);
      Gradient2.StartColor:=RGBToColor(117,117,117);
      Gradient2.EndColor:=RGBToColor(81,81,81);
    end;
    with BodyClicked do begin
      BorderColor:=RGBToColor(24,24,24);
      Font.Color:=clWhite;
      Font.Size:=7;
      LightOpacity:=20;
      LightWidth:=1;
      Gradient1.StartColor:=RGBToColor(92,92,92);
      Gradient1.EndColor:=RGBToColor(92,92,92);
      Gradient2.StartColor:=RGBToColor(91,91,91);
      Gradient2.EndColor:=RGBToColor(62,62,62);
    end;
  end;
end;

procedure Win7ToolBarButton(AButton: TBGRAButton);
begin
  with AButton do begin
    TextShadow:=False;
    RoundX:=2;
    RoundY:=2;
    with BodyNormal do begin
      Font.Color:=clBlack;
      BorderStyle:=bboNone;
      Style:=bbsClear;
      LightWidth:=1;
      LightOpacity:=100;
      Font.Size:=7;
    end;
    with BodyHover do begin
      Font.Color:=clBlack;
      Gradient1EndPercent:=50;
      BorderColor:=RGBToColor(187,202,219);
      Gradient1.StartColor:=RGBToColor(248,251,254);
      Gradient1.EndColor:=RGBToColor(237,242,250);
      Gradient2.StartColor:=RGBToColor(215,228,244);
      Gradient2.EndColor:=RGBToColor(193,210,232);
      LightWidth:=1;
      LightOpacity:=100;
      Font.Size:=7;
    end;
    with BodyClicked do begin
      Font.Color:=clBlack;
      Gradient1EndPercent:=55;
      BorderColor:=RGBToColor(187,202,219);
      Gradient1.StartColor:=RGBToColor(226,236,245);
      Gradient1.EndColor:=RGBToColor(216,228,241);
      Gradient2.StartColor:=RGBToColor(207,219,236);
      Gradient2.EndColor:=RGBToColor(207,220,237);
      LightWidth:=1;
      LightOpacity:=100;
      Font.Size:=7;
    end;
  end;
end;

procedure Win7Button(AButton: TBGRAButton);
begin
  with AButton do begin
    TextShadow:=False;
    RoundX:=3;
    RoundY:=3;
    with BodyNormal do begin
      Font.Color:=clBlack;
      Gradient1EndPercent:=50;
      BorderColor:=RGBToColor(112,112,112);
      Gradient1.StartColor:=RGBToColor(242,242,242);
      Gradient1.EndColor:=RGBToColor(235,235,235);
      Gradient2.StartColor:=RGBToColor(221,221,221);
      Gradient2.EndColor:=RGBToColor(207,207,207);
      Font.Size:=7;
      LightOpacity:=175;
      LightWidth:=1;
    end;
    with BodyHover do begin
      Font.Color:=clBlack;
      Gradient1EndPercent:=50;
      BorderColor:=RGBToColor(60,127,177);
      Gradient1.StartColor:=RGBToColor(234,246,253);
      Gradient1.EndColor:=RGBToColor(217,240,252);
      Gradient2.StartColor:=RGBToColor(190,230,253);
      Gradient2.EndColor:=RGBToColor(167,217,245);
      Font.Size:=7;
      LightOpacity:=175;
      LightWidth:=1;
    end;
    with BodyClicked do begin
      Font.Color:=clBlack;
      Gradient1EndPercent:=55;
      BorderColor:=RGBToColor(44,98,139);
      Gradient1.StartColor:=RGBToColor(229,244,252);
      Gradient1.EndColor:=RGBToColor(196,229,246);
      Gradient2.StartColor:=RGBToColor(152,209,239);
      Gradient2.EndColor:=RGBToColor(104,179,219);
      Font.Size:=7;
      LightWidth:=0;
    end;
  end;
end;

procedure Office2010Button(AButton: TBGRAButton);
begin
  with AButton do begin
    RoundX:=3;
    RoundY:=3;
    TextShadow:=False;
    with BodyNormal do begin
      BorderColor:=RGBToColor(207,208,210);
      Font.Color:=clBlack;
      Gradient1.StartColor:=RGBToColor(255,255,255);
      Gradient1.EndColor:=RGBToColor(237,239,241);
      Gradient1EndPercent:=100;
      Font.Size:=7;
      LightOpacity:=175;
      LightWidth:=1;
    end;
    with BodyHover do begin
      BorderColor:=RGBToColor(244,210,81);
      Font.Color:=clBlack;
      Gradient1.StartColor:=RGBToColor(254,241,189);
      Gradient1.EndColor:=RGBToColor(254,228,134);
      Gradient1EndPercent:=50;
      Gradient2.StartColor:=RGBToColor(254,228,134);
      Gradient2.EndColor:=RGBToColor(254,248,196);
      Font.Size:=7;
      LightOpacity:=175;
      LightWidth:=1;
    end;
    with BodyClicked do begin
      BorderColor:=RGBToColor(194,161,63);
      Font.Color:=clBlack;
      Gradient1.StartColor:=RGBToColor(255,229,117);
      Gradient1.EndColor:=RGBToColor(255,216,107);
      Gradient1EndPercent:=50;
      Gradient2.StartColor:=RGBToColor(255,216,107);
      Gradient2.EndColor:=RGBToColor(255,239,129);
      Font.Size:=7;
      LightWidth:=0;
    end;
  end;
end;

procedure MacOSXLionButton(AButton: TBGRAButton);
begin
  with AButton do begin
    TextShadow:=False;
    RoundX:=3;
    RoundY:=3;
    with BodyNormal do begin
      Font.Color:=clBlack;
      Gradient1EndPercent:=50;
      BorderColor:=RGBToColor(154,154,154);
      Gradient1.StartColor:=RGBToColor(255,255,255);
      Gradient1.EndColor:=RGBToColor(243,243,243);
      Gradient2.StartColor:=RGBToColor(236,236,236);
      Gradient2.EndColor:=RGBToColor(235,235,235);
      Font.Size:=7;
      LightOpacity:=175;
      LightWidth:=1;
    end;
    with BodyHover do begin
      Font.Color:=clBlack;
      Gradient1EndPercent:=50;
      BorderColor:=RGBToColor(86,87,143);
      Gradient1.StartColor:=RGBToColor(204,229,252);
      Gradient1.EndColor:=RGBToColor(161,209,249);
      Gradient2.StartColor:=RGBToColor(143,202,251);
      Gradient2.EndColor:=RGBToColor(207,245,253);
      Font.Size:=7;
      LightOpacity:=175;
      LightWidth:=1;
    end;
    with BodyClicked do begin
      Font.Color:=clBlack;
      Gradient1EndPercent:=55;
      BorderColor:=RGBToColor(86,87,143);
      Gradient1.StartColor:=RGBToColor(144,195,241);
      Gradient1.EndColor:=RGBToColor(113,180,239);
      Gradient2.StartColor:=RGBToColor(97,173,240);
      Gradient2.EndColor:=RGBToColor(147,206,241);
      Font.Size:=7;
      LightWidth:=0;
    end;
  end;
end;

end.

