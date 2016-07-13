unit BGRASVGType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRATransform, BGRABitmapTypes, BGRAUnits,
  laz2_DOM, BGRACanvas2D;

type
  TSVGElement = class;
  TSVGFactory = class of TSVGElement;

  { TSVGElement }

  TSVGElement = class
    private
      function GetAttributeOrStyle(AName: string): string;
      function GetFill: string;
      function GetFillColor: TBGRAPixel;
      function GetFillOpacity: single;
      function GetHorizAttributeOrStyleWithUnit(AName: string
        ): TFloatWithCSSUnit;
      function GetIsFillNone: boolean;
      function GetIsStrokeNone: boolean;
      function GetMatrix(AUnit: TCSSUnit): TAffineMatrix;
      function GetOpacity: single;
      function GetOrthoAttributeOrStyleWithUnit(AName: string
        ): TFloatWithCSSUnit;
      function GetStroke: string;
      function GetStrokeColor: TBGRAPixel;
      function GetStrokeLineCap: string;
      function GetStrokeLineJoin: string;
      function GetStrokeMiterLimit: single;
      function GetStrokeOpacity: single;
      function GetStrokeWidth: TFloatWithCSSUnit;
      function GetStyle(const AName: string): string;
      function GetTransform: string;
      function GetUnits: TCSSUnitConverter;
      function GetAttribute(AName: string): string;
      function GetVerticalAttributeOrStyleWithUnit(AName: string
        ): TFloatWithCSSUnit;
      procedure SetAttribute(AName: string; AValue: string);
      function GetAttributeWithUnit(AName: string): TFloatWithCSSUnit;
      function GetAttributeOrStyleWithUnit(AName: string): TFloatWithCSSUnit;
      function GetOrthoAttributeWithUnit(AName: string): TFloatWithCSSUnit;
      function GetHorizAttributeWithUnit(AName: string): TFloatWithCSSUnit;
      function GetVerticalAttributeWithUnit(AName: string): TFloatWithCSSUnit;
      procedure SetAttributeWithUnit(AName: string; AValue: TFloatWithCSSUnit);
      procedure SetFill(AValue: string);
      procedure SetFillColor(AValue: TBGRAPixel);
      procedure SetFillOpacity(AValue: single);
      procedure SetHorizAttributeWithUnit(AName: string; AValue: TFloatWithCSSUnit);
      procedure SetMatrix(AUnit: TCSSUnit; const AValue: TAffineMatrix);
      procedure SetOpacity(AValue: single);
      procedure SetStroke(AValue: string);
      procedure SetStrokeColor(AValue: TBGRAPixel);
      procedure SetStrokeLineCap(AValue: string);
      procedure SetStrokeLineJoin(AValue: string);
      procedure SetStrokeMiterLimit(AValue: single);
      procedure SetStrokeOpacity(AValue: single);
      procedure SetStrokeWidth(AValue: TFloatWithCSSUnit);
      procedure SetStyle(AName: string; AValue: string);
      procedure SetTransform(AValue: string);
      procedure SetVerticalAttributeWithUnit(AName: string; AValue: TFloatWithCSSUnit);
      procedure SetOrthoAttributeWithUnit(AName: string; AValue: TFloatWithCSSUnit);
    protected
      FDomElem: TDOMElement;
      FUnits: TCSSUnitConverter;
      function GetDOMElement: TDOMElement; virtual;
      procedure Init(ADocument: TXMLDocument; ATag: string; AUnits: TCSSUnitConverter); overload;
      procedure Init({%H-}ADocument: TXMLDocument; AElement: TDOMElement; AUnits: TCSSUnitConverter); overload;
      procedure InternalDraw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit); virtual;
      procedure LocateStyleDeclaration(AText: string; AProperty: string; out AStartPos,AColonPos,AValueLength: integer);
      procedure ApplyStrokeStyle(ACanvas2D: TBGRACanvas2D; AUnit: TCSSUnit);
    public
      constructor Create({%H-}ADocument: TXMLDocument; AElement: TDOMElement; AUnits: TCSSUnitConverter); virtual;
      constructor Create({%H-}ADocument: TXMLDocument; {%H-}AUnits: TCSSUnitConverter); virtual;
      procedure Draw({%H-}ACanvas2d: TBGRACanvas2D; {%H-}AUnit: TCSSUnit);
      procedure fillNone;
      procedure strokeNone;
      procedure transformNone;
      procedure RemoveStyle(const AName: string);
      property Attribute[AName: string]: string read GetAttribute write SetAttribute;
      property AttributeOrStyle[AName: string]: string read GetAttributeOrStyle;
      property Style[AName: string]: string read GetStyle write SetStyle;
      property OrthoAttributeWithUnit[AName: string]: TFloatWithCSSUnit read GetOrthoAttributeWithUnit write SetOrthoAttributeWithUnit;
      property HorizAttributeWithUnit[AName: string]: TFloatWithCSSUnit read GetHorizAttributeWithUnit write SetHorizAttributeWithUnit;
      property VerticalAttributeWithUnit[AName: string]: TFloatWithCSSUnit read GetVerticalAttributeWithUnit write SetVerticalAttributeWithUnit;
      property OrthoAttributeOrStyleWithUnit[AName: string]: TFloatWithCSSUnit read GetOrthoAttributeOrStyleWithUnit;
      property HorizAttributeOrStyleWithUnit[AName: string]: TFloatWithCSSUnit read GetHorizAttributeOrStyleWithUnit;
      property VerticalAttributeOrStyleWithUnit[AName: string]: TFloatWithCSSUnit read GetVerticalAttributeOrStyleWithUnit;
      property DOMElement: TDOMElement read GetDOMElement;
      property Units: TCSSUnitConverter read GetUnits;
      property transform: string read GetTransform write SetTransform;
      property matrix[AUnit: TCSSUnit]: TAffineMatrix read GetMatrix write SetMatrix;
      property isFillNone: boolean read GetIsFillNone;
      property isStrokeNone: boolean read GetIsStrokeNone;
      property stroke: string read GetStroke write SetStroke;
      property strokeWidth: TFloatWithCSSUnit read GetStrokeWidth write SetStrokeWidth;
      property strokeColor: TBGRAPixel read GetStrokeColor write SetStrokeColor;
      property strokeOpacity: single read GetStrokeOpacity write SetStrokeOpacity;
      property strokeMiterLimit: single read GetStrokeMiterLimit write SetStrokeMiterLimit;
      property strokeLineJoin: string read GetStrokeLineJoin write SetStrokeLineJoin;
      property strokeLineCap: string read GetStrokeLineCap write SetStrokeLineCap;
      property fill: string read GetFill write SetFill;
      property fillColor: TBGRAPixel read GetFillColor write SetFillColor;
      property fillOpacity: single read GetFillOpacity write SetFillOpacity;
      property opacity: single read GetOpacity write SetOpacity;
  end;

  { TSVGParser }

  TSVGParser = class
  private
    function GetDone: boolean;
  protected
    FPos: integer;
    FNumberError: boolean;
    FText: string;
  public
    constructor Create(AText: string);
    function ParseFloat: single;
    function ParseId: string;
    function ParseSymbol: char;
    procedure SkipSymbol(ASymbol: char);
    procedure SkipUpToSymbol(ASymbol:char);
    procedure ClearError;
    property Position: integer read FPos write FPos;
    property NumberError: boolean read FNumberError;
    property Text: string read FText;
    property Done: boolean read GetDone;
  end;

implementation

{ TSVGParser }

function TSVGParser.GetDone: boolean;
begin
  result := FPos>length(FText)
end;

constructor TSVGParser.Create(AText: string);
begin
  FNumberError:= false;
  FPos := 1;
  FText := AText;
end;

function TSVGParser.ParseFloat: single;
var numberStart: integer;
    errPos: integer;
begin
  while (FPos <= length(FText)) and (FText[FPos] in[#0..#32,',']) do inc(FPos);
  numberStart:= FPos;
  if (FPos <= length(FText)) and (FText[FPos] in['+','-']) then inc(FPos);
  while (FPos <= length(FText)) and (FText[FPos] in['0'..'9','.']) do inc(FPos);
  if (FPos <= length(FText)) and (FText[FPos] in['e','E']) then inc(FPos);
  if (FPos <= length(FText)) and (FText[FPos] in['+','-']) then inc(FPos);
  while (FPos <= length(FText)) and (FText[FPos] in['0'..'9','.']) do inc(FPos);
  if FPos = numberStart then
  begin
    FNumberError := true;
    result := 0;
  end
  else
  begin
    val(copy(FText,numberStart,FPos-numberStart),result,errPos);
    if errPos <> 0 then FNumberError := true;
  end;
end;

function TSVGParser.ParseId: string;
var idStart: integer;
begin
  while (FPos <= length(FText)) and (FText[FPos] in[#0..#32,',']) do inc(FPos);
  idStart:= FPos;
  if (FPos <= length(FText)) and (FText[FPos] in['A'..'Z','a'..'z']) then inc(FPos);
  while (FPos <= length(FText)) and (FText[FPos] in['0'..'9','A'..'Z','a'..'z','_']) do inc(FPos);
  result := copy(FText,idStart,FPos-idStart);
end;

function TSVGParser.ParseSymbol: char;
begin
  while (FPos <= length(FText)) and (FText[FPos] in[#0..#32,',']) do inc(FPos);
  if (FPos <= length(FText)) and not (FText[FPos] in['A'..'Z','a'..'z','0'..'9']) then
  begin
    result := FText[FPos];
    inc(FPos);
  end else
    result := #0;
end;

procedure TSVGParser.SkipSymbol(ASymbol: char);
begin
  while (FPos <= length(FText)) and (FText[FPos] in[#0..#32,',']) do inc(FPos);
  if (FPos <= length(FText)) and (FText[FPos] = ASymbol) then inc(FPos);
end;

procedure TSVGParser.SkipUpToSymbol(ASymbol: char);
begin
  while (FPos <= length(FText)) and (FText[FPos]<>ASymbol) do inc(FPos);
  if (FPos <= length(FText)) and (FText[FPos]=ASymbol) then inc(FPos);
end;

procedure TSVGParser.ClearError;
begin
  FNumberError:= false;
end;

{ TSVGElement }

function TSVGElement.GetAttribute(AName: string): string;
begin
  result := FDomElem.GetAttribute(AName);
end;

function TSVGElement.GetVerticalAttributeOrStyleWithUnit(AName: string
  ): TFloatWithCSSUnit;
begin
  result := GetAttributeOrStyleWithUnit(AName);
  if result.CSSUnit <> cuCustom then
    if units.DpiScaleY = 0 then
      result.value := 0
    else
      result.value /= Units.DpiScaleY;
end;

function TSVGElement.GetAttributeWithUnit(AName: string): TFloatWithCSSUnit;
begin
  result := TCSSUnitConverter.parseValue(Attribute[AName],FloatWithCSSUnit(0,cuCustom));
end;

function TSVGElement.GetAttributeOrStyleWithUnit(AName: string
  ): TFloatWithCSSUnit;
var valueText: string;
begin
  valueText := Style[AName];
  if valueText = '' then valueText := Attribute[AName];
  result := TCSSUnitConverter.parseValue(valueText,FloatWithCSSUnit(0,cuCustom));
end;

function TSVGElement.GetOrthoAttributeWithUnit(AName: string
  ): TFloatWithCSSUnit;
begin
  result := GetHorizAttributeWithUnit(AName);
  //value will be inconsistent if scaling is inconsistent
end;

function TSVGElement.GetHorizAttributeWithUnit(AName: string
  ): TFloatWithCSSUnit;
begin
  result := GetAttributeWithUnit(AName);
  if result.CSSUnit <> cuCustom then
    if units.DpiScaleX = 0 then
      result.value := 0
    else
      result.value /= Units.DpiScaleX;
end;

function TSVGElement.GetAttributeOrStyle(AName: string): string;
begin
  result := GetStyle(AName);
  if result = '' then result := GetAttribute(AName);
end;

function TSVGElement.GetFill: string;
begin
  result := AttributeOrStyle['fill'];
end;

function TSVGElement.GetFillColor: TBGRAPixel;
begin
  result := StrToBGRA(fill,BGRABlack);
  result.alpha := round(result.alpha*fillOpacity*opacity);
  if result.alpha = 0 then result := BGRAPixelTransparent;
end;

function TSVGElement.GetFillOpacity: single;
var errPos: integer;
begin
  val(AttributeOrStyle['fill-opacity'], result, errPos);
  if errPos <> 0 then result := 1 else
    if result < 0 then result := 0 else
      if result > 1 then result := 1;
end;

function TSVGElement.GetHorizAttributeOrStyleWithUnit(AName: string
  ): TFloatWithCSSUnit;
begin
  result := GetAttributeOrStyleWithUnit(AName);
  if result.CSSUnit <> cuCustom then
    if units.DpiScaleX = 0 then
      result.value := 0
    else
      result.value /= Units.DpiScaleX;
end;

function TSVGElement.GetIsFillNone: boolean;
begin
  result := compareText(trim(fill),'none')=0;
end;

function TSVGElement.GetIsStrokeNone: boolean;
var strokeStr: string;
begin
  strokeStr := stroke;
  result := (trim(strokeStr)='') or (compareText(trim(strokeStr),'none')=0);
end;

function TSVGElement.GetMatrix(AUnit: TCSSUnit): TAffineMatrix;
var parser: TSVGParser;
    s,kind: string;
    m : TAffineMatrix;
    angle,tx,ty: single;
begin
  result := AffineMatrixIdentity;
  s := transform;
  if s='' then exit;
  parser := TSVGParser.Create(s);
  while not parser.Done do
  begin
    kind := parser.ParseId;
    if kind = '' then break;
    if parser.ParseSymbol <> '(' then break;
    if compareText(kind,'matrix')=0 then
    begin
      m[1,1] := parser.ParseFloat;
      parser.SkipSymbol(',');
      m[2,1] := parser.ParseFloat;
      parser.SkipSymbol(',');
      m[1,2] := parser.ParseFloat;
      parser.SkipSymbol(',');
      m[2,2] := parser.ParseFloat;
      parser.SkipSymbol(',');
      m[1,3] := parser.ParseFloat;
      parser.SkipSymbol(',');
      m[2,3] := parser.ParseFloat;
      result *= m;
    end else
    if compareText(kind,'translate')=0 then
    begin
      tx := parser.ParseFloat;
      parser.SkipSymbol(',');
      ty := parser.ParseFloat;
      result *= AffineMatrixTranslation(tx,ty);
    end else
    if compareText(kind,'scale')=0 then
    begin
      tx := parser.ParseFloat;
      parser.SkipSymbol(',');
      parser.ClearError;
      ty := parser.ParseFloat;
      if parser.NumberError then ty := tx;
      result *= AffineMatrixScale(tx,ty);
    end else
    if compareText(kind,'rotate')=0 then
    begin
      angle := parser.ParseFloat;
      parser.SkipSymbol(',');
      tx := parser.ParseFloat;
      parser.SkipSymbol(',');
      ty := parser.ParseFloat;
      result *= AffineMatrixTranslation(tx,ty)*AffineMatrixRotationDeg(angle)*
                AffineMatrixTranslation(-tx,-ty);
    end else
    if compareText(kind,'skewx')=0 then
    begin
      angle := parser.ParseFloat;
      result *= AffineMatrixSkewXDeg(angle);
    end else
    if compareText(kind,'skewy')=0 then
    begin
      angle := parser.ParseFloat;
      result *= AffineMatrixSkewYDeg(angle);
    end;
    parser.SkipUpToSymbol(')');
  end;
  parser.free;
  result[1,3] := Units.ConvertWidth(result[1,3],cuCustom,AUnit);
  result[2,3] := Units.ConvertHeight(result[2,3],cuCustom,AUnit);
end;

function TSVGElement.GetOpacity: single;
var errPos: integer;
begin
  val(AttributeOrStyle['opacity'], result, errPos);
  if errPos <> 0 then result := 1 else
    if result < 0 then result := 0 else
      if result > 1 then result := 1;
end;

function TSVGElement.GetOrthoAttributeOrStyleWithUnit(AName: string
  ): TFloatWithCSSUnit;
begin
  result := GetHorizAttributeOrStyleWithUnit(AName);
  //value will be inconsistent if scaling is inconsistent
end;

function TSVGElement.GetStroke: string;
begin
  result := AttributeOrStyle['stroke'];
end;

function TSVGElement.GetStrokeColor: TBGRAPixel;
begin
  result := StrToBGRA(stroke);
  result.alpha := round(result.alpha*strokeOpacity*opacity);
  if result.alpha = 0 then result := BGRAPixelTransparent;
end;

function TSVGElement.GetStrokeLineCap: string;
begin
  result := AttributeOrStyle['stroke-linecap'];
  if result = '' then result := 'butt';
end;

function TSVGElement.GetStrokeLineJoin: string;
begin
  result := AttributeOrStyle['stroke-linejoin'];
  if result = '' then result := 'miter';
end;

function TSVGElement.GetStrokeMiterLimit: single;
var errPos: integer;
begin
  val(AttributeOrStyle['stroke-miterlimit'], result, errPos);
  if errPos <> 0 then result := 4 else
    if result < 1 then result := 1;
end;

function TSVGElement.GetStrokeOpacity: single;
var errPos: integer;
begin
  val(AttributeOrStyle['stroke-opacity'], result, errPos);
  if errPos <> 0 then result := 1 else
    if result < 0 then result := 0 else
      if result > 1 then result := 1;
end;

function TSVGElement.GetStrokeWidth: TFloatWithCSSUnit;
begin
  result := OrthoAttributeOrStyleWithUnit['stroke-width'];
end;

function TSVGElement.GetStyle(const AName: string): string;
var
    startPos, colonPos, valueLength: integer;
    ruleset: string;
begin
  ruleset := Attribute['style'];
  LocateStyleDeclaration(ruleset, AName, startPos,colonPos, valueLength);
  if valueLength <> -1 then
  begin
    result := trim(copy(ruleset, colonPos+1, valueLength));
  end else
    result := '';
end;

function TSVGElement.GetTransform: string;
begin
  result := Attribute['transform'];
end;

function TSVGElement.GetUnits: TCSSUnitConverter;
begin
  result := FUnits;
end;

function TSVGElement.GetVerticalAttributeWithUnit(AName: string): TFloatWithCSSUnit;
begin
  result := GetAttributeWithUnit(AName);
  if result.CSSUnit <> cuCustom then
    if units.DpiScaleY = 0 then
      result.value := 0
    else
      result.value /= Units.DpiScaleY;
end;

function TSVGElement.GetDOMElement: TDOMElement;
begin
  result := FDomElem;
end;

procedure TSVGElement.SetAttribute(AName: string; AValue: string);
begin
  FDomElem.SetAttribute(AName,AValue);
end;

procedure TSVGElement.SetAttributeWithUnit(AName: string;
  AValue: TFloatWithCSSUnit);
begin
  Attribute[AName] := TCSSUnitConverter.formatValue(AValue);
end;

procedure TSVGElement.SetFill(AValue: string);
begin
  Attribute['fill'] := AValue;
  RemoveStyle('fill');
end;

procedure TSVGElement.SetFillColor(AValue: TBGRAPixel);
begin
  fillOpacity:= AValue.alpha/255;
  AValue.alpha:= 255;
  fill := BGRAToStr(AValue, CSSColors);
end;

procedure TSVGElement.SetFillOpacity(AValue: single);
begin
  Attribute['fill-opacity'] := Units.formatValue(AValue);
  RemoveStyle('fill-opacity');
end;

procedure TSVGElement.SetHorizAttributeWithUnit(AName: string;
  AValue: TFloatWithCSSUnit);
begin
  if Units.DpiScaled then
    SetAttribute(AName, TCSSUnitConverter.formatValue(Units.ConvertWidth(AValue,cuCustom)))
  else
  if AValue.CSSUnit <> cuCustom then
    SetAttributeWithUnit(AName, FloatWithCSSUnit(AValue.value*Units.DpiScaleX,AValue.CSSUnit))
  else
    SetAttributeWithUnit(AName, AValue);
end;

procedure TSVGElement.SetMatrix(AUnit: TCSSUnit; const AValue: TAffineMatrix);
var m: TAffineMatrix;
    s: string;
    translateStr: string;
begin
  translateStr := 'translate('+Units.formatValue(Units.ConvertWidth(AValue[1,3],AUnit,cuCustom))+' '+
      Units.formatValue(Units.ConvertHeight(AValue[2,3],AUnit,cuCustom))+')';
  if IsAffineMatrixTranslation(AValue) then
  begin
    if IsAffineMatrixIdentity(AValue) then
    begin
      transformNone;
      exit;
    end;
    transform := translateStr;
  end else
  begin
    m := AValue;
    if (m[1,3] <> 0) or (m[2,3] <> 0) then
    begin
      s := translateStr;
      m[1,3] := 0;
      m[2,3] := 0;
    end else
      s := '';
    if IsAffineMatrixScale(AValue) then
    begin
      transform := trim(s+' scale('+Units.formatValue(m[1,1])+' '+Units.formatValue(m[2,2])+')');
      exit;
    end;
    transform := trim(s+' matrix('+Units.formatValue(m[1,1])+' '+Units.formatValue(m[2,1])+' '+
                     Units.formatValue(m[1,2])+' '+Units.formatValue(m[2,2])+' ' +
                     Units.formatValue(m[1,3])+' '+Units.formatValue(m[2,3]));
  end;
end;

procedure TSVGElement.SetOpacity(AValue: single);
begin
  Attribute['opacity'] := Units.formatValue(AValue);
  RemoveStyle('opacity');
end;

procedure TSVGElement.SetStroke(AValue: string);
begin
  Attribute['stroke'] := AValue;
  RemoveStyle('stroke');
end;

procedure TSVGElement.SetStrokeColor(AValue: TBGRAPixel);
begin
  strokeOpacity:= AValue.alpha/255;
  AValue.alpha:= 255;
  stroke := BGRAToStr(AValue, CSSColors);
end;

procedure TSVGElement.SetStrokeLineCap(AValue: string);
begin
  Attribute['stroke-linecap'] := AValue;
  RemoveStyle('stroke-linecap');
end;

procedure TSVGElement.SetStrokeLineJoin(AValue: string);
begin
  Attribute['stroke-linejoin'] := AValue;
  RemoveStyle('stroke-linejoin');
end;

procedure TSVGElement.SetStrokeMiterLimit(AValue: single);
begin
  if AValue < 1 then AValue := 1;
  Attribute['stroke-miterlimit'] := Units.formatValue(AValue);
  RemoveStyle('stroke-miterlimit');
end;

procedure TSVGElement.SetStrokeOpacity(AValue: single);
begin
  Attribute['stroke-opacity'] := Units.formatValue(AValue);
  RemoveStyle('stroke-opacity');
end;

procedure TSVGElement.SetStrokeWidth(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['stroke-width'] := AValue;
  RemoveStyle('stroke-width');
end;

procedure TSVGElement.SetStyle(AName: string; AValue: string);
var
    startPos, colonPos, valueLength: integer;
    ruleset: string;
begin
  if pos(';',AValue)<>0 then
    raise exception.Create('Invalid character in value');
  if pos(':',AName)<>0 then
    raise exception.Create('Invalid character in name');
  ruleset := Attribute['style'];
  LocateStyleDeclaration(ruleset, AName, startPos,colonPos, valueLength);
  if valueLength <> -1 then
  begin
    delete(ruleset, colonPos+1, valueLength);
    insert(' '+Trim(AValue), ruleset, colonPos+1);
  end else
  begin
    while (length(ruleset) > 0) and (ruleset[length(ruleset)] in[' ',#9,#10,#12,#13]) do
      delete(ruleset, length(ruleset), 1);
    if length(ruleset)>0 then
    begin
      if ruleset[length(ruleset)] <> ';' then ruleset += '; ';
    end;
    ruleset += AName+': '+AValue;
  end;
  Attribute['style'] := ruleset;
end;

procedure TSVGElement.SetTransform(AValue: string);
begin
  Attribute['transform'] := AValue;
end;

procedure TSVGElement.SetVerticalAttributeWithUnit(AName: string;
  AValue: TFloatWithCSSUnit);
begin
  if Units.DpiScaled then
    SetAttribute(AName, TCSSUnitConverter.formatValue(Units.ConvertHeight(AValue,cuCustom)))
  else
  if AValue.CSSUnit <> cuCustom then
    SetAttributeWithUnit(AName, FloatWithCSSUnit(AValue.value*Units.DpiScaleY,AValue.CSSUnit))
  else
    SetAttributeWithUnit(AName, AValue);
end;

procedure TSVGElement.SetOrthoAttributeWithUnit(AName: string;
  AValue: TFloatWithCSSUnit);
begin
  if (AValue.CSSUnit <> cuCustom) and (Units.DpiScaleX<>Units.DpiScaleY) then
    raise exception.Create('Impossible to set value with inconsistent scaling');
  if Units.DpiScaled then
    SetAttribute(AName, TCSSUnitConverter.formatValue(Units.ConvertWidth(AValue,cuCustom)))
  else
    SetHorizAttributeWithUnit(AName,AValue);
end;

procedure TSVGElement.Init(ADocument: TXMLDocument; ATag: string;
  AUnits: TCSSUnitConverter);
begin
  FDomElem := ADocument.CreateElement(ATag);
  FUnits := AUnits;
end;

procedure TSVGElement.Init(ADocument: TXMLDocument; AElement: TDOMElement;
  AUnits: TCSSUnitConverter);
begin
  FDomElem := AElement;
  FUnits := AUnits;
end;

procedure TSVGElement.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
begin
  //nothing
end;

procedure TSVGElement.LocateStyleDeclaration(AText: string; AProperty: string; out AStartPos,
  AColonPos, AValueLength: integer);
var i: integer;
    curStart,curColon,curValueLength: integer;

    function CheckShouldReturnResult: boolean;
    begin
      if Trim(Copy(AText,curStart,curColon-curStart)) = AProperty then
      begin
        AStartPos:= curStart;
        AColonPos:= curColon;
        AValueLength:= curValueLength;
        result := true
      end
      else
        result := false
    end;

begin
  AProperty := Trim(AProperty);
  AStartPos := -1;
  AColonPos := -1;
  AValueLength:= -1;
  curStart := -1;
  curColon := -1;
  curValueLength := -1;
  for i := 1 to length(AText) do
  begin
    if curStart = -1 then
    begin
      if AText[i] in['-','_','a'..'z','A'..'Z','\'] then
      begin
        curStart := i;
        curColon := -1;
      end;
    end else
    if curColon = -1 then
    begin
      if AText[i] = ':' then
      begin
        curColon := i;
        curValueLength:= -1;
      end;
    end else
    if AText[i] = ';' then
    begin
      curValueLength := i-(curColon+1);
      if CheckShouldReturnResult then exit;
      curStart := -1;
      curColon := -1;
      curValueLength:= -1;
    end;
  end;
  if curColon <> -1 then
  begin
    curValueLength:= length(AText)-(curColon+1)+1;
    if CheckShouldReturnResult then exit;
  end;
end;

procedure TSVGElement.ApplyStrokeStyle(ACanvas2D: TBGRACanvas2D; AUnit: TCSSUnit);
begin
  ACanvas2d.strokeStyle(strokeColor);
  ACanvas2d.lineWidth := Units.ConvertWidth(strokeWidth,AUnit).value;
  ACanvas2d.lineCap := strokeLineCap;
  ACanvas2d.lineJoin := strokeLineJoin;
  ACanvas2d.miterLimit := strokeMiterLimit;
end;

constructor TSVGElement.Create(ADocument: TXMLDocument; AElement: TDOMElement;
  AUnits: TCSSUnitConverter);
begin
  Init(ADocument,AElement,AUnits);
end;

constructor TSVGElement.Create(ADocument: TXMLDocument;
  AUnits: TCSSUnitConverter);
begin
  raise exception.Create('Cannot create a generic element');
end;

procedure TSVGElement.Draw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
var prevMatrix: TAffineMatrix;
begin
  prevMatrix := ACanvas2d.matrix;
  ACanvas2d.transform(matrix[AUnit]);
  InternalDraw(ACanvas2d,AUnit);
  ACanvas2d.matrix := prevMatrix;
end;

procedure TSVGElement.fillNone;
begin
  fill := 'none';
end;

procedure TSVGElement.strokeNone;
begin
  stroke := 'none';
end;

procedure TSVGElement.transformNone;
begin
  FDomElem.RemoveAttribute('transform');
end;

procedure TSVGElement.RemoveStyle(const AName: string);
var
    startPos, colonPos, valueLength: integer;
    ruleset: string;
begin
  ruleset := Attribute['style'];
  LocateStyleDeclaration(ruleset, AName, startPos,colonPos, valueLength);
  if valueLength <> -1 then
  begin
    delete(ruleset, startPos, colonPos+valueLength-startPos);
    while (length(ruleset)>=startPos) and (ruleset[startPos] in[' ',#9,#10,#12,#13]) do delete(ruleset,startPos,1);
    if (length(ruleset)>=startPos) and (ruleset[startPos] = ';') then delete(ruleset,startPos,1);
    Attribute['style'] := ruleset;
  end;
end;

end.

