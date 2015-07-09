{=== Color definitions ===}

{$IFDEF INCLUDE_INTERFACE}
{$UNDEF INCLUDE_INTERFACE}
var
  {* This is the value used for transparent pixels. In theory, any
     color with alpha = 0 is transparent, however it is recommended to
     use all other channels to zero as well. }
  BGRAPixelTransparent: TBGRAPixel;

  {* [#FFFFFF] White opaque }
  BGRAWhite: TBGRAPixel;
  {* [#000000] Black opaque }
  BGRABlack: TBGRAPixel;

const
  {* This color [#000001] looks just like black. It is needed for drawing black
     shapes using the ''Canvas'' property of ''TBGRABitmap''. This is a standard
     ''TCanvas'' and when drawing with pure black (''clBlack''), there is no way to know if
     something has been drawn or if it is transparent }
  clBlackOpaque = TColor($010000);

var
  //VGA colors
  VGABlack,VGAGray,VGASilver,VGAWhite,
  VGAMaroon,VGARed,VGAPurple,VGAFuchsia,
  VGAGreen,VGALime,VGAOlive,VGAYellow,
  VGANavy,VGABlue,VGATeal,VGAAqua: TBGRAPixel;

  //Red colors
  CSSIndianRed,CSSLightCoral,CSSSalmon,CSSDarkSalmon,
  CSSRed,CSSCrimson,CSSFireBrick,CSSDarkRed: TBGRAPixel;
  //Pink colors
  CSSPink,CSSLightPink,CSSHotPink,CSSDeepPink,
  CSSMediumVioletRed,CSSPaleVioletRed: TBGRAPixel;
  //Orange colors
  CSSLightSalmon,CSSCoral,CSSTomato,CSSOrangeRed,
  CSSDarkOrange,CSSOrange: TBGRAPixel;
  //Yellow colors
  CSSGold,CSSYellow,CSSLightYellow,CSSLemonChiffon,
  CSSLightGoldenrodYellow,CSSPapayaWhip,CSSMoccasin,CSSPeachPuff,
  CSSPaleGoldenrod,CSSKhaki,CSSDarkKhaki: TBGRAPixel;
  //Purple colors
  CSSLavender,CSSThistle,CSSPlum,CSSViolet,
  CSSOrchid,CSSFuchsia,CSSMagenta,CSSMediumOrchid,
  CSSMediumPurple,CSSBlueViolet,CSSDarkViolet,CSSDarkOrchid,
  CSSDarkMagenta,CSSPurple,CSSIndigo,CSSDarkSlateBlue,
  CSSSlateBlue,CSSMediumSlateBlue: TBGRAPixel;
  //Green colors
  CSSGreenYellow,CSSChartreuse,CSSLawnGreen,CSSLime,
  CSSLimeGreen,CSSPaleGreen,CSSLightGreen,CSSMediumSpringGreen,
  CSSSpringGreen,CSSMediumSeaGreen,CSSSeaGreen,CSSForestGreen,
  CSSGreen,CSSDarkGreen,CSSYellowGreen,CSSOliveDrab,
  CSSOlive,CSSDarkOliveGreen,CSSMediumAquamarine,CSSDarkSeaGreen,
  CSSLightSeaGreen,CSSDarkCyan,CSSTeal: TBGRAPixel;
  //Blue/Cyan colors
  CSSAqua,CSSCyan,CSSLightCyan,CSSPaleTurquoise,
  CSSAquamarine,CSSTurquoise,CSSMediumTurquoise,CSSDarkTurquoise,
  CSSCadetBlue,CSSSteelBlue,CSSLightSteelBlue,CSSPowderBlue,
  CSSLightBlue,CSSSkyBlue,CSSLightSkyBlue,CSSDeepSkyBlue,
  CSSDodgerBlue,CSSCornflowerBlue,CSSRoyalBlue,CSSBlue,
  CSSMediumBlue,CSSDarkBlue,CSSNavy,CSSMidnightBlue: TBGRAPixel;
  //Brown colors
  CSSCornsilk, CSSBlanchedAlmond, CSSBisque, CSSNavajoWhite,
  CSSWheat, CSSBurlyWood, CSSTan, CSSRosyBrown,
  CSSSandyBrown, CSSGoldenrod, CSSDarkGoldenrod, CSSPeru,
  CSSChocolate, CSSSaddleBrown, CSSSienna, CSSBrown,
  CSSMaroon: TBGRAPixel;
  //White colors
  CSSWhite, CSSSnow, CSSHoneydew, CSSMintCream,
  CSSAzure, CSSAliceBlue, CSSGhostWhite, CSSWhiteSmoke,
  CSSSeashell, CSSBeige, CSSOldLace, CSSFloralWhite,
  CSSIvory, CSSAntiqueWhite, CSSLinen, CSSLavenderBlush,
  CSSMistyRose: TBGRAPixel;
  //Gray colors
  CSSGainsboro, CSSLightGray, CSSSilver, CSSDarkGray,
  CSSGray, CSSDimGray, CSSLightSlateGray, CSSSlateGray,
  CSSDarkSlateGray, CSSBlack: TBGRAPixel;

type
  TBGRAColorDefinition = record
    Name: string;
    Color: TBGRAPixel;
  end;

  { TBGRAColorList }
  {* Contains a fixed list of colors }
  TBGRAColorList = class
  protected
    FFinished: boolean;
    FNbColors: integer;
    FColors: array of TBGRAColorDefinition;
    function GetByIndex(Index: integer): TBGRAPixel;
    function GetByName(Name: string): TBGRAPixel;
    function GetName(Index: integer): string;
    procedure Add(Name: string; out Color: TBGRAPixel; red,green,blue: byte); overload;
  public
    {** Creates an empty color list }
    constructor Create;
    {** Add a color to the list }
    procedure Add(Name: string; const Color: TBGRAPixel);
    {** Ends the color list and prevents further modifications }
    procedure Finished;
    {** Returns the index of a color with a given name }
    function IndexOf(Name: string): integer;
    {** Returns the index of a color. Colors are considered to match if
        the difference is less than or equal to ''AMaxDiff'' }
    function IndexOfColor(const AColor: TBGRAPixel; AMaxDiff: Word = 0): integer;

    {** Gets the color associated with a color name }
    property ByName[Name: string]: TBGRAPixel read GetByName;
    {** Gets the color at the specified index }
    property ByIndex[Index: integer]: TBGRAPixel read GetByIndex; default;
    {** Gets the name of the color at the specified index }
    property Name[Index: integer]: string read GetName;
    {** Gets the number of colors }
    property Count: integer read FNbColors;
  end;

var
  {* List of VGA colors:
   * [#000000] Black, [#808080] Gray, [#C0C0C0] Silver, [#FFFFFF] White,
   * [#800000] Maroon, [#FF0000] Red,  [#800080] Purple, [#FF00FF] Fuchsia,
   * [#008000] Green,  [#00FF00] Lime, [#808000] Olive,  [#FFFF00] Yellow,
   * [#000080] Navy,   [#0000FF] Blue, [#008080] Teal,   [#00FFFF] Aqua.
   * Shortcut constants are provided: [#000000] ''VGABlack'', [#808080] ''VGAGray''... }
  VGAColors: TBGRAColorList;
  {* List of [http://www.w3schools.com/cssref/css_colornames.asp web colors].
  Shortcut constants are provided: [#000000] ''CSSBlack'', [#FF0000] ''CSSRed''... }
  CSSColors: TBGRAColorList;

{------------------- string conversion ------------------------}

{* Converts a ''TBGRAPixel'' value into a string, using color names provided in ''AColorList'', and
   considering that a color matches in the color list if its difference is within ''AMaxDiff'' }
function BGRAToStr(c: TBGRAPixel; AColorList: TBGRAColorList = nil; AMaxDiff: Word= 0): string;
{* Converts a fully defined string into a ''TBGRAPixel'' value. Color names from ''VGAColors'' and ''CSSColors''
   are used if there is an exact match }
function StrToBGRA(str: string): TBGRAPixel;
{* Converts a string into a ''TBGRAPixel'' value. If the value is not fully defined or that
   there is an error, ''DefaultColor'' is returned.
   Color names from ''VGAColors'' and ''CSSColors'' are used if there is an exact match. }
function StrToBGRA(str: string; const DefaultColor: TBGRAPixel): TBGRAPixel;
{* Converts a string into a ''TBGRAPixel'' value. If the value is not fully defined, missing channels (expressed with '?')
   are filled with fallbackValues. You can check if there was an error with the provided boolean.
   Color names from ''VGAColors'' and ''CSSColors'' are used if there is an exact match. }
function PartialStrToBGRA(str: string; const fallbackValues: TBGRAPixel; out error: boolean): TBGRAPixel;
{* Converts a string into a ''TBGRAPixel'' value into ''parsedValue''. ''parsedValue'' is not changed if
   some channels are missing (expressed with '?'). You can check if there was an error with the provided boolean.
   Color names from ''VGAColors'' and ''CSSColors'' are used if there is an exact match. }
procedure TryStrToBGRA(str: string; var parsedValue: TBGRAPixel; out missingValues: boolean; out error: boolean);
{$ENDIF}

{$IFDEF INCLUDE_IMPLEMENTATION}
{$UNDEF INCLUDE_IMPLEMENTATION}
{ TBGRAColorList }

function TBGRAColorList.GetByIndex(Index: integer): TBGRAPixel;
begin
  if (Index < 0) or (Index >= FNbColors) then
    result := BGRAPixelTransparent
  else
    result := FColors[Index].Color;
end;

function TBGRAColorList.GetByName(Name: string): TBGRAPixel;
var i: integer;
begin
  i := IndexOf(Name);
  if i = -1 then
    result := BGRAPixelTransparent
  else
    result := FColors[i].Color;
end;

function TBGRAColorList.GetName(Index: integer): string;
begin
  if (Index < 0) or (Index >= FNbColors) then
    result := ''
  else
    result := FColors[Index].Name;
end;

procedure TBGRAColorList.Add(Name: string; out Color: TBGRAPixel; red, green,
  blue: byte);
begin
  Color := BGRA(red,green,blue);
  Add(Name,Color);
end;

constructor TBGRAColorList.Create;
begin
  FNbColors:= 0;
  FColors := nil;
  FFinished:= false;
end;

procedure TBGRAColorList.Add(Name: string; const Color: TBGRAPixel);
begin
  if FFinished then
    raise Exception.Create('This list is already finished');
  if length(FColors) = FNbColors then
    SetLength(FColors, FNbColors*2+1);
  FColors[FNbColors].Name := Name;
  FColors[FNbColors].Color := Color;
  inc(FNbColors);
end;

procedure TBGRAColorList.Finished;
begin
  if FFinished then exit;
  FFinished := true;
  SetLength(FColors, FNbColors);
end;

function TBGRAColorList.IndexOf(Name: string): integer;
var i: integer;
begin
  for i := 0 to FNbColors-1 do
    if CompareText(Name, FColors[i].Name) = 0 then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

function TBGRAColorList.IndexOfColor(const AColor: TBGRAPixel; AMaxDiff: Word = 0): integer;
var i: integer;
  MinDiff,CurDiff: Word;
begin
  if AMaxDiff = 0 then
  begin
    for i := 0 to FNbColors-1 do
      if AColor = FColors[i].Color then
      begin
        result := i;
        exit;
      end;
    result := -1;
  end else
  begin
    MinDiff := AMaxDiff;
    result := -1;
    for i := 0 to FNbColors-1 do
    begin
      CurDiff := BGRAWordDiff(AColor,FColors[i].Color);
      if CurDiff <= MinDiff then
      begin
        result := i;
        MinDiff := CurDiff;
        if MinDiff = 0 then exit;
      end;
    end;
  end;
end;

{------------------- string conversion ---------------------------------}

{ Write a color in hexadecimal format RRGGBBAA or using the name in a color list }
function BGRAToStr(c: TBGRAPixel; AColorList: TBGRAColorList = nil; AMaxDiff: Word= 0): string;
var idx: integer;
begin
  if Assigned(AColorList) then
  begin
    idx := AColorList.IndexOfColor(c, AMaxDiff);
    if idx<> -1 then
    begin
      result := AColorList.Name[idx];
      exit;
    end;
  end;
  result := IntToHex(c.red,2)+IntToHex(c.green,2)+IntToHex(c.Blue,2)+IntToHex(c.Alpha,2);
end;

type
    arrayOfString = array of string;

function SimpleParseFuncParam(str: string; var flagError: boolean): arrayOfString;
var idxOpen,start,cur: integer;
begin
    result := nil;
    idxOpen := pos('(',str);
    if idxOpen = 0 then
    begin
      start := 1;
      //find first space
      while (start <= length(str)) and (str[start]<>' ') do inc(start);
    end else
      start := idxOpen+1;
    cur := start;
    while cur <= length(str) do
    begin
       if str[cur] in[',',')'] then
       begin
         setlength(result,length(result)+1);
         result[high(result)] := trim(copy(str,start,cur-start));
         start := cur+1;
         if str[cur] = ')' then exit;
       end;
       inc(cur);
    end;
    if idxOpen <> 0 then flagError := true; //should exit on ')'
    if start <= length(str) then
    begin
      setlength(result,length(result)+1);
      result[high(result)] := copy(str,start,length(str)-start+1);
    end;
end;

function ParseColorValue(str: string; var flagError: boolean): byte;
var pourcent,unclipped,{%H-}errPos: integer;
begin
  if str = '' then result := 0 else
  begin
    if str[length(str)]='%' then
    begin
      val(copy(str,1,length(str)-1),pourcent,errPos);
      if errPos <> 0 then flagError := true;
      if pourcent < 0 then result := 0 else
      if pourcent > 100 then result := 255 else
        result := pourcent*255 div 100;
    end else
    begin
      val(str,unclipped,errPos);
      if errPos <> 0 then flagError := true;
      if unclipped < 0 then result := 0 else
      if unclipped > 255 then result := 255 else
        result := unclipped;
    end;
  end;
end;

//this function returns the parsed value only if it contains no error nor missing values, otherwise
//it returns BGRAPixelTransparent
function StrToBGRA(str: string): TBGRAPixel;
var missingValues, error: boolean;
begin
  result := BGRABlack;
  TryStrToBGRA(str, result, missingValues, error);
  if missingValues or error then result := BGRAPixelTransparent;
end;

//this function changes the content of parsedValue depending on available and parsable information.
//set parsedValue to the fallback values before calling this function.
//missing values are expressed by empty string or by '?', for example 'rgb(255,?,?,?)' will change only the red value.
//note that if alpha is not expressed by the string format, it will be opaque. So 'rgb(255,?,?)' will change the red value and the alpha value.
//the last parameter of rgba() is a floating point number where 1 is opaque and 0 is transparent.
procedure TryStrToBGRA(str: string; var parsedValue: TBGRAPixel; out missingValues: boolean; out error: boolean);
var errPos: integer;
    values: array of string;
    alphaF: single;
    idx: integer;
begin
  str := Trim(str);
  error := false;
  if (str = '') or (str = '?') then
  begin
    missingValues := true;
    exit;
  end else
    missingValues := false;
  str := StringReplace(lowerCase(str),'grey','gray',[]);

  //VGA color names
  idx := VGAColors.IndexOf(str);
  if idx <> -1 then
  begin
    parsedValue := VGAColors[idx];
    exit;
  end;
  if str='transparent' then parsedValue := BGRAPixelTransparent else
  begin
    //check CSS color
    idx := CSSColors.IndexOf(str);
    if idx <> -1 then
    begin
      parsedValue := CSSColors[idx];
      exit;
    end;

    //CSS RGB notation
    if (copy(str,1,4)='rgb(') or (copy(str,1,5)='rgba(') or
      (copy(str,1,4)='rgb ') or (copy(str,1,5)='rgba ') then
    begin
      values := SimpleParseFuncParam(str,error);
      if (length(values)=3) or (length(values)=4) then
      begin
        if (values[0] <> '') and (values[0] <> '?') then
           parsedValue.red := ParseColorValue(values[0], error)
        else
           missingValues := true;
        if (values[1] <> '') and (values[1] <> '?') then
           parsedValue.green := ParseColorValue(values[1], error)
        else
           missingValues := true;
        if (values[2] <> '') and (values[2] <> '?') then
           parsedValue.blue := ParseColorValue(values[2], error)
        else
           missingValues := true;
        if length(values)=4 then
        begin
          if (values[3] <> '') and (values[3] <> '?') then
          begin
            val(values[3],alphaF,errPos);
            if errPos <> 0 then
            begin
               parsedValue.alpha := 255;
               error := true;
            end
            else
            begin
              if alphaF < 0 then
                parsedValue.alpha := 0 else
              if alphaF > 1 then
                parsedValue.alpha := 255
              else
                parsedValue.alpha := round(alphaF*255);
            end;
          end else
            missingValues := true;
        end else
          parsedValue.alpha := 255;
      end else
        error := true;
      exit;
    end;

    //remove HTML notation header
    if str[1]='#' then delete(str,1,1);

    //add alpha if missing (if you want an undefined alpha use '??' or '?')
    if length(str)=6 then str += 'FF';
    if length(str)=3 then str += 'F';

    //hex notation
    if length(str)=8 then
    begin
      if copy(str,1,2) <> '??' then
      begin
        val('$'+copy(str,1,2),parsedValue.red,errPos);
        if errPos <> 0 then error := true;
      end else missingValues := true;
      if copy(str,3,2) <> '??' then
      begin
        val('$'+copy(str,3,2),parsedValue.green,errPos);
        if errPos <> 0 then error := true;
      end else missingValues := true;
      if copy(str,5,2) <> '??' then
      begin
        val('$'+copy(str,5,2),parsedValue.blue,errPos);
        if errPos <> 0 then error := true;
      end else missingValues := true;
      if copy(str,7,2) <> '??' then
      begin
        val('$'+copy(str,7,2),parsedValue.alpha,errPos);
        if errPos <> 0 then
        begin
          error := true;
          parsedValue.alpha := 255;
        end;
      end else missingValues := true;
    end else
    if length(str)=4 then
    begin
      if str[1] <> '?' then
      begin
        val('$'+str[1],parsedValue.red,errPos);
        if errPos <> 0 then error := true;
        parsedValue.red *= $11;
      end else missingValues := true;
      if str[2] <> '?' then
      begin
        val('$'+str[2],parsedValue.green,errPos);
        if errPos <> 0 then error := true;
        parsedValue.green *= $11;
      end else missingValues := true;
      if str[3] <> '?' then
      begin
        val('$'+str[3],parsedValue.blue,errPos);
        if errPos <> 0 then error := true;
        parsedValue.blue *= $11;
      end else missingValues := true;
      if str[4] <> '?' then
      begin
        val('$'+str[4],parsedValue.alpha,errPos);
        if errPos <> 0 then
        begin
          error := true;
          parsedValue.alpha := 255;
        end else
          parsedValue.alpha *= $11;
      end else missingValues := true;
    end else
      error := true; //string format not recognised
  end;

end;

//this function returns the values that can be read from the string, otherwise
//it fills the gaps with the fallback values. The error boolean is True only
//if there was invalid values, it is not set to True if there was missing values.
function PartialStrToBGRA(str: string; const fallbackValues: TBGRAPixel; out
  error: boolean): TBGRAPixel;
var missingValues: boolean;
begin
  result := fallbackValues;
  TryStrToBGRA(str, result, missingValues, error);
end;

{ Read a color, for example in hexadecimal format RRGGBB(AA) or RGB(A). Partial colors are not accepted by this function. }
function StrToBGRA(str: string; const DefaultColor: TBGRAPixel): TBGRAPixel;
var missingValues, error: boolean;
begin
  result := BGRABlack;
  TryStrToBGRA(str, result, missingValues, error);
  if missingValues or error then result := DefaultColor;
end;

function BlueGreenRedToBGRA(blue,green,red: byte): TBGRAPixel;
begin
  result := BGRA(red,green,blue);
end;

{$ENDIF}

{$IFDEF INCLUDE_INIT}
{$UNDEF INCLUDE_INIT}
  BGRAPixelTransparent := BGRA(0,0,0,0);
  BGRAWhite := BGRA(255,255,255);
  BGRABlack := BGRA(0,0,0);

  VGAColors := TBGRAColorList.Create;
  VGAColors.Add('Black',VGABlack,0,0,0);
  VGAColors.Add('Gray',VGAGray,128,128,128);
  VGAColors.Add('Silver',VGASilver,192,192,192);
  VGAColors.Add('White',VGAWhite,255,255,255);
  VGAColors.Add('Maroon',VGAMaroon,128,0,0);
  VGAColors.Add('Red',VGARed,255,0,0);
  VGAColors.Add('Purple',VGAPurple,128,0,128);
  VGAColors.Add('Fuchsia',VGAFuchsia,255,0,255);
  VGAColors.Add('Green',VGAGreen,0,128,0);
  VGAColors.Add('Lime',VGALime,0,255,0);
  VGAColors.Add('Olive',VGAOlive,128,128,0);
  VGAColors.Add('Yellow',VGAYellow,255,255,0);
  VGAColors.Add('Navy',VGANavy,0,0,128);
  VGAColors.Add('Blue',VGABlue,0,0,255);
  VGAColors.Add('Teal',VGATeal,0,128,128);
  VGAColors.Add('Aqua',VGAAqua,0,255,255);
  VGAColors.Finished;

  //Red colors
  CSSIndianRed:= BlueGreenRedToBGRA(92, 92, 205);
  CSSLightCoral:= BlueGreenRedToBGRA(128, 128, 240);
  CSSSalmon:= BlueGreenRedToBGRA(114, 128, 250);
  CSSDarkSalmon:= BlueGreenRedToBGRA(122, 150, 233);
  CSSRed:= BlueGreenRedToBGRA(0, 0, 255);
  CSSCrimson:= BlueGreenRedToBGRA(60, 20, 220);
  CSSFireBrick:= BlueGreenRedToBGRA(34, 34, 178);
  CSSDarkRed:= BlueGreenRedToBGRA(0, 0, 139);

  //Pink colors
  CSSPink:= BlueGreenRedToBGRA(203, 192, 255);
  CSSLightPink:= BlueGreenRedToBGRA(193, 182, 255);
  CSSHotPink:= BlueGreenRedToBGRA(180, 105, 255);
  CSSDeepPink:= BlueGreenRedToBGRA(147, 20, 255);
  CSSMediumVioletRed:= BlueGreenRedToBGRA(133, 21, 199);
  CSSPaleVioletRed:= BlueGreenRedToBGRA(147, 112, 219);

  //Orange colors
  CSSLightSalmon:= BlueGreenRedToBGRA(122, 160, 255);
  CSSCoral:= BlueGreenRedToBGRA(80, 127, 255);
  CSSTomato:= BlueGreenRedToBGRA(71, 99, 255);
  CSSOrangeRed:= BlueGreenRedToBGRA(0, 69, 255);
  CSSDarkOrange:= BlueGreenRedToBGRA(0, 140, 255);
  CSSOrange:= BlueGreenRedToBGRA(0, 165, 255);

  //Yellow colors
  CSSGold:= BlueGreenRedToBGRA(0, 215, 255);
  CSSYellow:= BlueGreenRedToBGRA(0, 255, 255);
  CSSLightYellow:= BlueGreenRedToBGRA(224, 255, 255);
  CSSLemonChiffon:= BlueGreenRedToBGRA(205, 250, 255);
  CSSLightGoldenrodYellow:= BlueGreenRedToBGRA(210, 250, 250);
  CSSPapayaWhip:= BlueGreenRedToBGRA(213, 239, 255);
  CSSMoccasin:= BlueGreenRedToBGRA(181, 228, 255);
  CSSPeachPuff:= BlueGreenRedToBGRA(185, 218, 255);
  CSSPaleGoldenrod:= BlueGreenRedToBGRA(170, 232, 238);
  CSSKhaki:= BlueGreenRedToBGRA(140, 230, 240);
  CSSDarkKhaki:= BlueGreenRedToBGRA(107, 183, 189);

  //Purple colors
  CSSLavender:= BlueGreenRedToBGRA(250, 230, 230);
  CSSThistle:= BlueGreenRedToBGRA(216, 191, 216);
  CSSPlum:= BlueGreenRedToBGRA(221, 160, 221);
  CSSViolet:= BlueGreenRedToBGRA(238, 130, 238);
  CSSOrchid:= BlueGreenRedToBGRA(214, 112, 218);
  CSSFuchsia:= BlueGreenRedToBGRA(255, 0, 255);
  CSSMagenta:= BlueGreenRedToBGRA(255, 0, 255);
  CSSMediumOrchid:= BlueGreenRedToBGRA(211, 85, 186);
  CSSMediumPurple:= BlueGreenRedToBGRA(219, 112, 147);
  CSSBlueViolet:= BlueGreenRedToBGRA(226, 43, 138);
  CSSDarkViolet:= BlueGreenRedToBGRA(211, 0, 148);
  CSSDarkOrchid:= BlueGreenRedToBGRA(204, 50, 153);
  CSSDarkMagenta:= BlueGreenRedToBGRA(139, 0, 139);
  CSSPurple:= BlueGreenRedToBGRA(128, 0, 128);
  CSSIndigo:= BlueGreenRedToBGRA(130, 0, 75);
  CSSDarkSlateBlue:= BlueGreenRedToBGRA(139, 61, 72);
  CSSSlateBlue:= BlueGreenRedToBGRA(205, 90, 106);
  CSSMediumSlateBlue:= BlueGreenRedToBGRA(238, 104, 123);

  //Green colors
  CSSGreenYellow:= BlueGreenRedToBGRA(47, 255, 173);
  CSSChartreuse:= BlueGreenRedToBGRA(0, 255, 127);
  CSSLawnGreen:= BlueGreenRedToBGRA(0, 252, 124);
  CSSLime:= BlueGreenRedToBGRA(0, 255, 0);
  CSSLimeGreen:= BlueGreenRedToBGRA(50, 205, 50);
  CSSPaleGreen:= BlueGreenRedToBGRA(152, 251, 152);
  CSSLightGreen:= BlueGreenRedToBGRA(144, 238, 144);
  CSSMediumSpringGreen:= BlueGreenRedToBGRA(154, 250, 0);
  CSSSpringGreen:= BlueGreenRedToBGRA(127, 255, 0);
  CSSMediumSeaGreen:= BlueGreenRedToBGRA(113, 179, 60);
  CSSSeaGreen:= BlueGreenRedToBGRA(87, 139, 46);
  CSSForestGreen:= BlueGreenRedToBGRA(34, 139, 34);
  CSSGreen:= BlueGreenRedToBGRA(0, 128, 0);
  CSSDarkGreen:= BlueGreenRedToBGRA(0, 100, 0);
  CSSYellowGreen:= BlueGreenRedToBGRA(50, 205, 154);
  CSSOliveDrab:= BlueGreenRedToBGRA(35, 142, 107);
  CSSOlive:= BlueGreenRedToBGRA(0, 128, 128);
  CSSDarkOliveGreen:= BlueGreenRedToBGRA(47, 107, 85);
  CSSMediumAquamarine:= BlueGreenRedToBGRA(170, 205, 102);
  CSSDarkSeaGreen:= BlueGreenRedToBGRA(143, 188, 143);
  CSSLightSeaGreen:= BlueGreenRedToBGRA(170, 178, 32);
  CSSDarkCyan:= BlueGreenRedToBGRA(139, 139, 0);
  CSSTeal:= BlueGreenRedToBGRA(128, 128, 0);

  //Blue/Cyan colors
  CSSAqua:= BlueGreenRedToBGRA(255, 255, 0);
  CSSCyan:= BlueGreenRedToBGRA(255, 255, 0);
  CSSLightCyan:= BlueGreenRedToBGRA(255, 255, 224);
  CSSPaleTurquoise:= BlueGreenRedToBGRA(238, 238, 175);
  CSSAquamarine:= BlueGreenRedToBGRA(212, 255, 127);
  CSSTurquoise:= BlueGreenRedToBGRA(208, 224, 64);
  CSSMediumTurquoise:= BlueGreenRedToBGRA(204, 209, 72);
  CSSDarkTurquoise:= BlueGreenRedToBGRA(209, 206, 0);
  CSSCadetBlue:= BlueGreenRedToBGRA(160, 158, 95);
  CSSSteelBlue:= BlueGreenRedToBGRA(180, 130, 70);
  CSSLightSteelBlue:= BlueGreenRedToBGRA(222, 196, 176);
  CSSPowderBlue:= BlueGreenRedToBGRA(230, 224, 176);
  CSSLightBlue:= BlueGreenRedToBGRA(230, 216, 173);
  CSSSkyBlue:= BlueGreenRedToBGRA(235, 206, 135);
  CSSLightSkyBlue:= BlueGreenRedToBGRA(250, 206, 135);
  CSSDeepSkyBlue:= BlueGreenRedToBGRA(255, 191, 0);
  CSSDodgerBlue:= BlueGreenRedToBGRA(255, 144, 30);
  CSSCornflowerBlue:= BlueGreenRedToBGRA(237, 149, 100);
  CSSRoyalBlue:= BlueGreenRedToBGRA(255, 105, 65);
  CSSBlue:= BlueGreenRedToBGRA(255, 0, 0);
  CSSMediumBlue:= BlueGreenRedToBGRA(205, 0, 0);
  CSSDarkBlue:= BlueGreenRedToBGRA(139, 0, 0);
  CSSNavy:= BlueGreenRedToBGRA(128, 0, 0);
  CSSMidnightBlue:= BlueGreenRedToBGRA(112, 25, 25);

  //Brown colors
  CSSCornsilk:= BlueGreenRedToBGRA(220, 248, 255);
  CSSBlanchedAlmond:= BlueGreenRedToBGRA(205, 235, 255);
  CSSBisque:= BlueGreenRedToBGRA(196, 228, 255);
  CSSNavajoWhite:= BlueGreenRedToBGRA(173, 222, 255);
  CSSWheat:= BlueGreenRedToBGRA(179, 222, 245);
  CSSBurlyWood:= BlueGreenRedToBGRA(135, 184, 222);
  CSSTan:= BlueGreenRedToBGRA(140, 180, 210);
  CSSRosyBrown:= BlueGreenRedToBGRA(143, 143, 188);
  CSSSandyBrown:= BlueGreenRedToBGRA(96, 164, 244);
  CSSGoldenrod:= BlueGreenRedToBGRA(32, 165, 218);
  CSSDarkGoldenrod:= BlueGreenRedToBGRA(11, 134, 184);
  CSSPeru:= BlueGreenRedToBGRA(63, 133, 205);
  CSSChocolate:= BlueGreenRedToBGRA(30, 105, 210);
  CSSSaddleBrown:= BlueGreenRedToBGRA(19, 69, 139);
  CSSSienna:= BlueGreenRedToBGRA(45, 82, 160);
  CSSBrown:= BlueGreenRedToBGRA(42, 42, 165);
  CSSMaroon:= BlueGreenRedToBGRA(0, 0, 128);

  //White colors
  CSSWhite:= BlueGreenRedToBGRA(255, 255, 255);
  CSSSnow:= BlueGreenRedToBGRA(250, 250, 255);
  CSSHoneydew:= BlueGreenRedToBGRA(240, 255, 250);
  CSSMintCream:= BlueGreenRedToBGRA(250, 255, 245);
  CSSAzure:= BlueGreenRedToBGRA(255, 255, 240);
  CSSAliceBlue:= BlueGreenRedToBGRA(255, 248, 240);
  CSSGhostWhite:= BlueGreenRedToBGRA(255, 248, 248);
  CSSWhiteSmoke:= BlueGreenRedToBGRA(245, 245, 245);
  CSSSeashell:= BlueGreenRedToBGRA(255, 245, 238);
  CSSBeige:= BlueGreenRedToBGRA(220, 245, 245);
  CSSOldLace:= BlueGreenRedToBGRA(230, 245, 253);
  CSSFloralWhite:= BlueGreenRedToBGRA(240, 250, 255);
  CSSIvory:= BlueGreenRedToBGRA(240, 255, 255);
  CSSAntiqueWhite:= BlueGreenRedToBGRA(215, 235, 250);
  CSSLinen:= BlueGreenRedToBGRA(230, 240, 250);
  CSSLavenderBlush:= BlueGreenRedToBGRA(245, 240, 255);
  CSSMistyRose:= BlueGreenRedToBGRA(255, 228, 255);

  //Gray colors
  CSSGainsboro:= BlueGreenRedToBGRA(220, 220, 220);
  CSSLightGray:= BlueGreenRedToBGRA(211, 211, 211);
  CSSSilver:= BlueGreenRedToBGRA(192, 192, 192);
  CSSDarkGray:= BlueGreenRedToBGRA(169, 169, 169);
  CSSGray:= BlueGreenRedToBGRA(128, 128, 128);
  CSSDimGray:= BlueGreenRedToBGRA(105, 105, 105);
  CSSLightSlateGray:= BlueGreenRedToBGRA(153, 136, 119);
  CSSSlateGray:= BlueGreenRedToBGRA(144, 128, 112);
  CSSDarkSlateGray:= BlueGreenRedToBGRA(79, 79, 47);
  CSSBlack:= BlueGreenRedToBGRA(0, 0, 0);

  CSSColors := TBGRAColorList.Create;
  CSSColors.Add('AliceBlue',CSSAliceBlue);
  CSSColors.Add('AntiqueWhite',CSSAntiqueWhite);
  CSSColors.Add('Aqua',CSSAqua);
  CSSColors.Add('Aquamarine',CSSAquamarine);
  CSSColors.Add('Azure',CSSAzure);
  CSSColors.Add('Beige',CSSBeige);
  CSSColors.Add('Bisque',CSSBisque);
  CSSColors.Add('Black',CSSBlack);
  CSSColors.Add('BlanchedAlmond',CSSBlanchedAlmond);
  CSSColors.Add('Blue',CSSBlue);
  CSSColors.Add('BlueViolet',CSSBlueViolet);
  CSSColors.Add('Brown',CSSBrown);
  CSSColors.Add('BurlyWood',CSSBurlyWood);
  CSSColors.Add('CadetBlue',CSSCadetBlue);
  CSSColors.Add('Chartreuse',CSSChartreuse);
  CSSColors.Add('Chocolate',CSSChocolate);
  CSSColors.Add('Coral',CSSCoral);
  CSSColors.Add('CornflowerBlue',CSSCornflowerBlue);
  CSSColors.Add('Cornsilk',CSSCornsilk);
  CSSColors.Add('Crimson',CSSCrimson);
  CSSColors.Add('Cyan',CSSCyan);
  CSSColors.Add('DarkBlue',CSSDarkBlue);
  CSSColors.Add('DarkCyan',CSSDarkCyan);
  CSSColors.Add('DarkGoldenrod',CSSDarkGoldenrod);
  CSSColors.Add('DarkGray',CSSDarkGray);
  CSSColors.Add('DarkGreen',CSSDarkGreen);
  CSSColors.Add('DarkKhaki',CSSDarkKhaki);
  CSSColors.Add('DarkMagenta',CSSDarkMagenta);
  CSSColors.Add('DarkOliveGreen',CSSDarkOliveGreen);
  CSSColors.Add('DarkOrange',CSSDarkOrange);
  CSSColors.Add('DarkOrchid',CSSDarkOrchid);
  CSSColors.Add('DarkRed',CSSDarkRed);
  CSSColors.Add('DarkSalmon',CSSDarkSalmon);
  CSSColors.Add('DarkSeaGreen',CSSDarkSeaGreen);
  CSSColors.Add('DarkSlateBlue',CSSDarkSlateBlue);
  CSSColors.Add('DarkSlateGray',CSSDarkSlateGray);
  CSSColors.Add('DarkTurquoise',CSSDarkTurquoise);
  CSSColors.Add('DarkViolet',CSSDarkViolet);
  CSSColors.Add('DeepPink',CSSDeepPink);
  CSSColors.Add('DeepSkyBlue',CSSDeepSkyBlue);
  CSSColors.Add('DimGray',CSSDimGray);
  CSSColors.Add('DodgerBlue',CSSDodgerBlue);
  CSSColors.Add('FireBrick',CSSFireBrick);
  CSSColors.Add('FloralWhite',CSSFloralWhite);
  CSSColors.Add('ForestGreen',CSSForestGreen);
  CSSColors.Add('Fuchsia',CSSFuchsia);
  CSSColors.Add('Gainsboro',CSSGainsboro);
  CSSColors.Add('GhostWhite',CSSGhostWhite);
  CSSColors.Add('Gold',CSSGold);
  CSSColors.Add('Goldenrod',CSSGoldenrod);
  CSSColors.Add('Gray',CSSGray);
  CSSColors.Add('Green',CSSGreen);
  CSSColors.Add('GreenYellow',CSSGreenYellow);
  CSSColors.Add('Honeydew',CSSHoneydew);
  CSSColors.Add('HotPink',CSSHotPink);
  CSSColors.Add('IndianRed',CSSIndianRed);
  CSSColors.Add('Indigo',CSSIndigo);
  CSSColors.Add('Ivory',CSSIvory);
  CSSColors.Add('Khaki',CSSKhaki);
  CSSColors.Add('Lavender',CSSLavender);
  CSSColors.Add('LavenderBlush',CSSLavenderBlush);
  CSSColors.Add('LawnGreen',CSSLawnGreen);
  CSSColors.Add('LemonChiffon',CSSLemonChiffon);
  CSSColors.Add('LightBlue',CSSLightBlue);
  CSSColors.Add('LightCoral',CSSLightCoral);
  CSSColors.Add('LightCyan',CSSLightCyan);
  CSSColors.Add('LightGoldenrodYellow',CSSLightGoldenrodYellow);
  CSSColors.Add('LightGray',CSSLightGray);
  CSSColors.Add('LightGreen',CSSLightGreen);
  CSSColors.Add('LightPink',CSSLightPink);
  CSSColors.Add('LightSalmon',CSSLightSalmon);
  CSSColors.Add('LightSeaGreen',CSSLightSeaGreen);
  CSSColors.Add('LightSkyBlue',CSSLightSkyBlue);
  CSSColors.Add('LightSlateGray',CSSLightSlateGray);
  CSSColors.Add('LightSteelBlue',CSSLightSteelBlue);
  CSSColors.Add('LightYellow',CSSLightYellow);
  CSSColors.Add('Lime',CSSLime);
  CSSColors.Add('LimeGreen',CSSLimeGreen);
  CSSColors.Add('Linen',CSSLinen);
  CSSColors.Add('Magenta',CSSMagenta);
  CSSColors.Add('Maroon',CSSMaroon);
  CSSColors.Add('MediumAquamarine',CSSMediumAquamarine);
  CSSColors.Add('MediumBlue',CSSMediumBlue);
  CSSColors.Add('MediumOrchid',CSSMediumOrchid);
  CSSColors.Add('MediumPurple',CSSMediumPurple);
  CSSColors.Add('MediumSeaGreen',CSSMediumSeaGreen);
  CSSColors.Add('MediumSlateBlue',CSSMediumSlateBlue);
  CSSColors.Add('MediumSpringGreen',CSSMediumSpringGreen);
  CSSColors.Add('MediumTurquoise',CSSMediumTurquoise);
  CSSColors.Add('MediumVioletRed',CSSMediumVioletRed);
  CSSColors.Add('MidnightBlue',CSSMidnightBlue);
  CSSColors.Add('MintCream',CSSMintCream);
  CSSColors.Add('MistyRose',CSSMistyRose);
  CSSColors.Add('Moccasin',CSSMoccasin);
  CSSColors.Add('NavajoWhite',CSSNavajoWhite);
  CSSColors.Add('Navy',CSSNavy);
  CSSColors.Add('OldLace',CSSOldLace);
  CSSColors.Add('Olive',CSSOlive);
  CSSColors.Add('OliveDrab',CSSOliveDrab);
  CSSColors.Add('Orange',CSSOrange);
  CSSColors.Add('OrangeRed',CSSOrangeRed);
  CSSColors.Add('Orchid',CSSOrchid);
  CSSColors.Add('PaleGoldenrod',CSSPaleGoldenrod);
  CSSColors.Add('PaleGreen',CSSPaleGreen);
  CSSColors.Add('PaleTurquoise',CSSPaleTurquoise);
  CSSColors.Add('PaleVioletRed',CSSPaleVioletRed);
  CSSColors.Add('PapayaWhip',CSSPapayaWhip);
  CSSColors.Add('PeachPuff',CSSPeachPuff);
  CSSColors.Add('Peru',CSSPeru);
  CSSColors.Add('Pink',CSSPink);
  CSSColors.Add('Plum',CSSPlum);
  CSSColors.Add('PowderBlue',CSSPowderBlue);
  CSSColors.Add('Purple',CSSPurple);
  CSSColors.Add('Red',CSSRed);
  CSSColors.Add('RosyBrown',CSSRosyBrown);
  CSSColors.Add('RoyalBlue',CSSRoyalBlue);
  CSSColors.Add('SaddleBrown',CSSSaddleBrown);
  CSSColors.Add('Salmon',CSSSalmon);
  CSSColors.Add('SandyBrown',CSSSandyBrown);
  CSSColors.Add('SeaGreen',CSSSeaGreen);
  CSSColors.Add('Seashell',CSSSeashell);
  CSSColors.Add('Sienna',CSSSienna);
  CSSColors.Add('Silver',CSSSilver);
  CSSColors.Add('SkyBlue',CSSSkyBlue);
  CSSColors.Add('SlateBlue',CSSSlateBlue);
  CSSColors.Add('SlateGray',CSSSlateGray);
  CSSColors.Add('Snow',CSSSnow);
  CSSColors.Add('SpringGreen',CSSSpringGreen);
  CSSColors.Add('SteelBlue',CSSSteelBlue);
  CSSColors.Add('Tan',CSSTan);
  CSSColors.Add('Teal',CSSTeal);
  CSSColors.Add('Thistle',CSSThistle);
  CSSColors.Add('Tomato',CSSTomato);
  CSSColors.Add('Turquoise',CSSTurquoise);
  CSSColors.Add('Violet',CSSViolet);
  CSSColors.Add('Wheat',CSSWheat);
  CSSColors.Add('White',CSSWhite);
  CSSColors.Add('WhiteSmoke',CSSWhiteSmoke);
  CSSColors.Add('Yellow',CSSYellow);
  CSSColors.Add('YellowGreen',CSSYellowGreen);
  CSSColors.Finished;
{$ENDIF}

{$IFDEF INCLUDE_FINAL}
{$UNDEF INCLUDE_FINAL}
  CSSColors.Free;
  VGAColors.Free;
{$ENDIF}
