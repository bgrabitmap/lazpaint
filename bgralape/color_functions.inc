procedure MyGammaExpansionValue{$i lape.func}
begin
  Word(Result^) := GammaExpansionTab[PByte(Params^[0])^];
end;

procedure MyGammaExpansionPixel{$i lape.func}
begin
  TExpandedPixel(Result^) := GammaExpansion(PBGRAPixel(Params^[0])^);
end;

procedure MyGammaCompressionValue{$i lape.func}
begin
  Byte(Result^) := GammaCompressionTab[PWord(Params^[0])^];
end;

procedure MyGammaCompressionPixel{$i lape.func}
type
  PExpandedPixel = ^TExpandedPixel;
begin
  TBGRAPixel(Result^) := GammaCompression(PExpandedPixel(Params^[0])^);
end;

procedure MyGetIntensityExpanded{$i lape.func}
type
  PExpandedPixel = ^TExpandedPixel;
begin
  Word(Result^) := GetIntensity(PExpandedPixel(Params^[0])^);
end;

procedure MySetIntensityExpanded{$i lape.func}
type
  PExpandedPixel = ^TExpandedPixel;
var v: Int32;
begin
  v := PInt32(Params^[1])^;
  if v < 0 then v := 0;
  if v > 65535 then v := 65535;
  TExpandedPixel(Result^) := SetIntensity(PExpandedPixel(Params^[0])^, v);
end;

procedure MyGetIntensityBGRA{$i lape.func}
begin
  Word(Result^) := GetIntensity(GammaExpansion(PBGRAPixel(Params^[0])^));
end;

procedure MySetIntensityBGRA{$i lape.func}
var v: Int32;
begin
  v := PInt32(Params^[1])^;
  if v < 0 then v := 0;
  if v > 65535 then v := 65535;
  TBGRAPixel(Result^) := GammaCompression(SetIntensity(GammaExpansion(PBGRAPixel(Params^[0])^), v));
end;



procedure MyGetLightnessExpanded{$i lape.func}
type
  PExpandedPixel = ^TExpandedPixel;
begin
  Word(Result^) := GetLightness(PExpandedPixel(Params^[0])^);
end;

procedure MySetLightnessExpanded{$i lape.func}
type
  PExpandedPixel = ^TExpandedPixel;
var v: Int32;
begin
  v := PInt32(Params^[1])^;
  if v < 0 then v := 0;
  if v > 65535 then v := 65535;
  TExpandedPixel(Result^) := SetLightness(PExpandedPixel(Params^[0])^, v);
end;

procedure MyGetLightnessBGRA{$i lape.func}
begin
  Word(Result^) := GetLightness(GammaExpansion(PBGRAPixel(Params^[0])^));
end;

procedure MySetLightnessBGRA{$i lape.func}
var v: Int32;
begin
  v := PInt32(Params^[1])^;
  if v < 0 then v := 0;
  if v > 65535 then v := 65535;
  TBGRAPixel(Result^) := GammaCompression(SetLightness(GammaExpansion(PBGRAPixel(Params^[0])^), v));
end;


procedure MyBGRAToHSLA{$i lape.func}
begin
  THSLAPixel(Result^) := BGRAToHSLA(PBGRAPixel(Params^[0])^);
end;

procedure MyHSLAToBGRA{$i lape.func}
type PHSLAPixel = ^THSLAPixel;
begin
  TBGRAPixel(Result^) := HSLAToBGRA(PHSLAPixel(Params^[0])^);
end;

procedure MyExpandedToHSLA{$i lape.func}
type
  PExpandedPixel = ^TExpandedPixel;
begin
  THSLAPixel(Result^) := ExpandedToHSLA(PExpandedPixel(Params^[0])^);
end;

procedure MyHSLAToExpanded{$i lape.func}
type PHSLAPixel = ^THSLAPixel;
begin
  TExpandedPixel(Result^) := HSLAToExpanded(PHSLAPixel(Params^[0])^);
end;


procedure MyGrayscaleToBGRA{$i lape.func}
var v: Int32;
begin
  v := PInt32(Params^[0])^;
  if v < 0 then v := 0;
  if v > 65535 then v := 65535;
  TBGRAPixel(Result^) := GrayscaleToBGRA(v);
end;

procedure MyGrayscaleToExpanded{$i lape.func}
var v: Int32;
begin
  v := PInt32(Params^[0])^;
  if v < 0 then v := 0;
  if v > 65535 then v := 65535;
  with TExpandedPixel(Result^) do
  begin
    red := v;
    green := v;
    blue := v;
    alpha := 65535;
  end;
end;


procedure MyBGRAToGSBA{$i lape.func}
begin
  TGSBAPixel(Result^) := BGRAToGSBA(PBGRAPixel(Params^[0])^);
end;

procedure MyGSBAToBGRA{$i lape.func}
type PGSBAPixel = ^TGSBAPixel;
begin
  TBGRAPixel(Result^) := GSBAToBGRA(PGSBAPixel(Params^[0])^);
end;

procedure MyExpandedToGSBA{$i lape.func}
type
  PExpandedPixel = ^TExpandedPixel;
begin
  TGSBAPixel(Result^) := ExpandedToGSBA(PExpandedPixel(Params^[0])^);
end;

procedure MyGSBAToExpanded{$i lape.func}
type PGSBAPixel = ^TGSBAPixel;
begin
  TExpandedPixel(Result^) := GSBAToExpanded(PGSBAPixel(Params^[0])^);
end;



procedure MyMergeBGRAArray{$i lape.func}
type
  ArrayOfTBGRAPixel = array of TBGRAPixel;
  PArrayOfTBGRAPixel = ^ArrayOfTBGRAPixel;
var
  p: PArrayOfTBGRAPixel;
begin
  p := PArrayOfTBGRAPixel(Params^[0]);
  TBGRAPixel(Result^) := MergeBGRA(p^);
end;

procedure MyMergeBGRAWithWeight{$i lape.func}
begin
  TBGRAPixel(Result^) := MergeBGRA(PBGRAPixel(Params^[0])^,PInt32(Params^[1])^,PBGRAPixel(Params^[2])^,PInt32(Params^[3])^);
end;


procedure MyBGRAToStr{$i lape.func}
begin
  lpString(Result^) := BGRAToStr(PBGRAPixel(Params^[0])^,CSSColors);
end;

procedure MyStrToBGRA{$i lape.func}
begin
  TBGRAPixel(Result^) := StrToBGRA(PlpString(Params^[0])^);
end;


procedure RegisterColorFunctions(Compiler: TLapeCompiler);
begin
  Compiler.addGlobalFunc('function GammaExpansion(AValue: Byte): Word;', @MyGammaExpansionValue);
  Compiler.addGlobalFunc('function GammaExpansion(APixel: TBGRAPixel): TExpandedPixel; overload;', @MyGammaExpansionPixel);
  Compiler.addGlobalFunc('function GammaCompression(AValue: Word): Byte;', @MyGammaCompressionValue);
  Compiler.addGlobalFunc('function GammaCompression(const APixel: TExpandedPixel): TBGRAPixel; overload;', @MyGammaCompressionPixel);
  Compiler.addGlobalFunc('function GetIntensity(ec: TExpandedPixel): Word;', @MyGetIntensityExpanded);
  Compiler.addGlobalFunc('function SetIntensity(ec: TExpandedPixel; AIntensity: Int32): TExpandedPixel;', @MySetIntensityExpanded);
  Compiler.addGlobalFunc('function GetIntensity(c: TBGRAPixel): Word; overload;', @MyGetIntensityBGRA);
  Compiler.addGlobalFunc('function SetIntensity(c: TBGRAPixel; AIntensity: Int32): TBGRAPixel; overload;', @MySetIntensityBGRA);
  Compiler.addGlobalFunc('function GetLightness(ec: TExpandedPixel): Word;', @MyGetLightnessExpanded);
  Compiler.addGlobalFunc('function SetLightness(ec: TExpandedPixel; ALightness: Int32): TExpandedPixel;', @MySetLightnessExpanded);
  Compiler.addGlobalFunc('function GetLightness(c: TBGRAPixel): Word; overload;', @MyGetLightnessBGRA);
  Compiler.addGlobalFunc('function SetLightness(c: TBGRAPixel; ALightness: Int32): TBGRAPixel; overload;', @MySetLightnessBGRA);
  Compiler.addGlobalFunc('function BGRAToHSLA(c: TBGRAPixel): THSLAPixel;', @MyBGRAToHSLA);
  Compiler.addGlobalFunc('function ExpandedToHSLA(c: TExpandedPixel): THSLAPixel;', @MyExpandedToHSLA);
  Compiler.addGlobalFunc('function HSLAToBGRA(c: THSLAPixel): TBGRAPixel;', @MyHSLAToBGRA);
  Compiler.addGlobalFunc('function HSLAToExpanded(c: THSLAPixel): TExpandedPixel;', @MyHSLAToExpanded);
  Compiler.addGlobalFunc('function GrayscaleToBGRA(ALightness: Int32): TBGRAPixel;', @MyGrayscaleToBGRA);
  Compiler.addGlobalFunc('function GrayscaleToExpanded(ALightness: Int32): TExpandedPixel;', @MyGrayscaleToExpanded);
  Compiler.addGlobalFunc('function BGRAToGSBA(c: TBGRAPixel): TGSBAPixel;', @MyBGRAToGSBA);
  Compiler.addGlobalFunc('function ExpandedToGSBA(c: TExpandedPixel): TGSBAPixel;', @MyExpandedToGSBA);
  Compiler.addGlobalFunc('function GSBAToBGRA(c: TGSBAPixel): TBGRAPixel;', @MyGSBAToBGRA);
  Compiler.addGlobalFunc('function GSBAToExpanded(c: TGSBAPixel): TExpandedPixel;', @MyGSBAToExpanded);
  Compiler.addGlobalFunc('function MergeBGRA(const colors: array of TBGRAPixel): TBGRAPixel;', @MyMergeBGRAArray);
  Compiler.addGlobalFunc('function MergeBGRA(c1: TBGRAPixel; w1: Int32; c2: TBGRAPixel; w2: Int32): TBGRAPixel; overload;', @MyMergeBGRAWithWeight);
  Compiler.addGlobalFunc('function BGRAToStr(c: TBGRAPixel): string;', @MyBGRAToStr);
  Compiler.addGlobalFunc('function StrToBGRA(s: string): TBGRAPixel;', @MyStrToBGRA);
end;
