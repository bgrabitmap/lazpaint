unit UUnitTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes;

implementation

procedure Test(AExpression: boolean; ADescription: string);
begin
  if not AExpression then
    raise EAssertionFailed.Create('Assertion failed: '+ADescription);
end;

var error: boolean;

initialization

 Test(StrToBGRA('red ')=CSSRed,'ignore spaces');
 Test(StrToBGRA('red@')=BGRAPixelTransparent,'error fallback to transparent');
 Test(StrToBGRA('red@',CSSYellow)=CSSYellow,'error fallback to transparent');
 Test(StrToBGRA('rgb(255,0,0)')=CSSRed,'rgb format');
 Test(StrToBGRA('rgb(255,0,0,0.502)')=BGRA(255,0,0,128),'rgba format');
 Test(StrToBGRA('rgb(255,0,?)')=BGRAPixelTransparent,'missing as an error');
 Test(StrToBGRA('rgb(255,0,?)',CSSYellow)=CSSYellow,'missing as an error');
 Test(PartialStrToBGRA('rgb(255,?,?,?)',BGRA(128,128,128,128),error)=BGRA(255,128,128,128),'missing values replacement');
 Test(not error, 'missing is not an error');
 Test(PartialStrToBGRA('rgb(255,?,?)',BGRA(128,128,128,128),error)=BGRA(255,128,128,255),'implicit rgb alpha');
 Test(not error, 'missing is not an error');
 Test(PartialStrToBGRA('rgb(255,abc,0)',BGRA(128,128,128,128),error)=BGRA(255,0,0,255),'error replaced by 0 for rgb');
 Test(error, 'non numeric error');
 Test(PartialStrToBGRA('#ff????',BGRA(128,128,128,128),error)=BGRA(255,128,128,255),'missing values replacement');
 Test(not error, 'missing is not an error');
 Test(PartialStrToBGRA('#f??',BGRA(128,128,128,128),error)=BGRA(255,128,128,255),'missing values replacement');
 Test(not error, 'missing is not an error');
 Test(PartialStrToBGRA('#12??3456',BGRA(128,128,128,128),error)=BGRA($12,128,$34,$56),'html color with missing values');
 Test(not error, 'missing is not an error');
 Test(BGRAToStr(VGARed)='FF0000FF','Default color format');
 Test(BGRAToStr(BGRA(255,0,0), VGAColors)='Red','VGA color names');
 Test(BGRAToStr(BGRA(255,255,0), CSSColors)='Yellow','CSS color names');
 Test(BGRAToStr(BGRA(250,128,114), CSSColors)='Salmon','CSS color names');
end.

