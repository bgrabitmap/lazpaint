unit ubgrasamples;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics,
  BGRABitmapTypes, BGRABitmap, BGRAGradients;

{ Drawings }
procedure DrawFlashPlayerBody(ABitmap: TBGRABitmap);
procedure DrawFlashPlayerButtonPanel(ABitmap: TBGRABitmap);
procedure DrawWin7ToolBar(ABitmap: TBGRABitmap; ADir: TAlign);

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

end.

