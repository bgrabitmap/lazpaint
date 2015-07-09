unit BGRADNetDeserial;

{$mode objfpc}{$H+}

interface

{ This unit allow to read .Net serialized classes with BinaryFormatter of
  namespace System.Runtime.Serialization.Formatters.Binary.

  Serialization is a process by which objects in memory are saved according
  to their structure.

  This unit is used by BGRAPaintNet to read Paint.NET images. }

uses
  Classes, SysUtils;

type
  arrayOfLongword = array of longword;

  TTypeCategory = (ftPrimitiveType = 0, ftString = 1, ftObjectType =
    2, ftRuntimeType = 3,
    ftGenericType = 4, ftArrayOfObject = 5, ftArrayOfString = 6,
    ftArrayOfPrimitiveType = 7);

  TPrimitiveType = (ptNone = 0, ptBoolean = 1, ptByte = 2, ptChar = 3, ptDecimal = 5,
    ptDouble = 6, ptInt16 = 7, ptInt32 = 8, ptInt64 = 9, ptSByte = 10, ptSingle = 11,
    ptDateTime = 13, ptUInt16 = 14, ptUInt32 = 15, ptUInt64 = 16, ptString = 18);

  TGenericArrayType = (gatSingleDimension, gatJagged, gatMultidimensional);

  TDotNetDeserialization = class;

  ArrayOfNameValue = array of record
    Name: string;
    Value, valueType: string;
  end;

  TFieldType = record
    category: TTypeCategory;
    primitiveType: TPrimitiveType;
    refAssembly: longword;
    Name: string;
  end;

  TSerializedType = record
    ClassName:   string;
    nbFields:    integer;
    fieldNames:  array of string;
    fieldTypes:  array of TFieldType;
    refAssembly: longword;
  end;

  TAssemblyReference = record
    idAssembly: longword;
    Name: string;
  end;

  { TCustomSerializedObject }

  TCustomSerializedObject = class
  protected
    FContainer: TDotNetDeserialization;
    function GetTypeAsString: string; virtual; abstract;
    function GetFieldAsString(Index: longword): string; virtual; abstract;
    function GetFieldAsString(Name: string): string;
    function GetFieldCount: longword; virtual; abstract;
    function GetFieldName(Index: longword): string; virtual; abstract;
    function GetFieldTypeAsString(Index: longword): string; virtual; abstract;
    function IsReferenceType(index: longword): boolean; virtual; abstract;
  public
    idObject:   longword;
    refCount:   integer;
    inToString: boolean;
    constructor Create(container: TDotNetDeserialization); virtual;
    property FieldCount: longword read GetFieldCount;
    property FieldName[Index: longword]:string read GetFieldName;
    property FieldAsString[Index: longword]: string read GetFieldAsString;
    property FieldByNameAsString[Name: string]: string read GetFieldAsString;
    property FieldTypeAsString[Index: longword]: string read GetFieldTypeAsString;
    property TypeAsString: string read GetTypeAsString;
    function GetFieldIndex(Name: string): integer;
  end;

  { TSerializedClass }

  TSerializedClass = class(TCustomSerializedObject)
  protected
    function GetFieldAsString(Index: longword): string; override;
    function GetFieldCount: longword; override;
    function GetFieldName(Index: longword): string; override;
    function GetFieldTypeAsString(Index: longword): string; override;
    function IsReferenceType(index: longword): boolean; override;
    function GetTypeAsString: string; override;
  public
    numType: integer;
    fields:  ArrayOfNameValue;
  end;

  { TSerializedArray }

  TSerializedArray = class(TCustomSerializedObject)
  private
    data:       pointer;
    FItemSize:   longword;
    function GetItemPtr(Index: longword): pointer;
    procedure InitData;
  protected
    FArrayType: TGenericArrayType;
    function GetFieldAsString(Index: longword): string; override;
    function GetFieldCount: longword; override;
    function GetFieldName(Index: longword): string; override;
    function GetFieldTypeAsString(Index: longword): string; override;
    function IsReferenceType(index: longword): boolean; override;
    function GetTypeAsString: string; override;
  public
    dimensions: array of longword;
    itemType:   TFieldType;
    nbItems:    longword;
    constructor Create(AContainer: TDotNetDeserialization; AItemType: TFieldType; ALength: longword); overload;
    constructor Create(AContainer: TDotNetDeserialization; AArrayType: TGenericArrayType; AItemType: TFieldType; ADimensions: arrayOfLongword); overload;
    destructor Destroy; override;
    property ItemPtr[Index:longword]: pointer read GetItemPtr;
    property ItemSize: longword read FItemSize;
  end;

  { TSerializedValue }

  TSerializedValue = class(TSerializedArray)
  protected
    function GetIsReferenceType: boolean;
    function GetValueAsString: string;
    function GetTypeAsString: string; override;
  public
    constructor Create(AContainer: TDotNetDeserialization; AItemType: TFieldType); overload;
    property ValueAsString: string read GetValueAsString;
    property IsReferenceType: boolean read GetIsReferenceType;
  end;

  { TDotNetDeserialization }
  TDotNetDeserialization = class
    objectTypes: array of TSerializedType;
    assemblies:  array of TAssemblyReference;
    objects:     array of TCustomSerializedObject;

    function FindClass(typeName: string): TSerializedClass;
    function FindObject(typeName: string): TCustomSerializedObject;
    function GetSimpleField(obj: TCustomSerializedObject; Name: string): string;
    function GetObjectField(obj: TCustomSerializedObject; Name: string): TCustomSerializedObject;
    function GetObjectField(obj: TCustomSerializedObject; index: integer): TCustomSerializedObject;
    function GetObject(id: string): TCustomSerializedObject;
    function GetObject(id: longword): TCustomSerializedObject;
    function IsBoxedValue(obj: TCustomSerializedObject; index: integer): boolean;
    function GetBoxedValue(obj: TCustomSerializedObject; index: integer): string;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(filename: string);
    procedure LoadFromFileUTF8(filenameUTF8: string);
    function ToString: string; override;
    constructor Create;
    destructor Destroy; override;
    function GetTypeOfClassObject(idObject: longword): integer;
  private
    EndOfStream:      boolean;
    ArrayFillerCount: Longword;
    currentAutoObjectValue: longword;
    function nextAutoObjectId: longword;
    function LoadNextFromStream(Stream: TStream): longword;
    function LoadStringFromStream(Stream: TStream): string;
    function LoadDotNetCharFromStream(Stream: TStream): string;
    function LoadTypeFromStream(Stream: TStream; IsRuntimeType: boolean): integer;
    function LoadValuesFromStream(Stream: TStream; numType: integer): ArrayOfNameValue;
    function LoadValueFromStream(Stream: TStream; const fieldType: TFieldType): string;
    function LoadFieldType(Stream: TStream; category: TTypeCategory): TFieldType;
  end;

function WinReadByte(stream: TStream): byte;
function WinReadWord(Stream: TStream): word;
function WinReadSmallInt(Stream: TStream): smallint;
function WinReadLongint(Stream: TStream): longint;
function WinReadLongword(Stream: TStream): longword;
function WinReadInt64(Stream: TStream): int64;
function WinReadQWord(Stream: TStream): QWord;

implementation

uses BGRAUTF8;

const
  //block types
  btRefTypeObject = 1;
  btRuntimeObject = 4;
  btExternalObject = 5;
  btString      = 6;
  btGenericArray = 7;
  btBoxedPrimitiveTypeValue = 8;
  btObjectReference = 9;
  btNullValue   = 10;
  btEndOfStream = 11;
  btAssembly    = 12;
  btArrayFiller8b = 13;
  btArrayFiller32b = 14;
  btArrayOfPrimitiveType = 15;
  btArrayOfObject = 16;
  btArrayOfString = 17;
  btMethodCall  = 21;
  btMethodResponse = 22;

  idArrayFiller = $80000000;

{$hints off}

function WinReadByte(stream: TStream): byte;
begin
  stream.Read(Result, sizeof(Result));
end;

function WinReadWord(Stream: TStream): word;
begin
  stream.Read(Result, sizeof(Result));
  Result := LEtoN(Result);
end;

function WinReadSmallInt(Stream: TStream): smallint;
begin
  stream.Read(Result, sizeof(Result));
  Result := LEtoN(Result);
end;

function WinReadLongint(Stream: TStream): longint;
begin
  stream.Read(Result, sizeof(Result));
  Result := LEtoN(Result);
end;

function WinReadLongword(Stream: TStream): longword;
begin
  stream.Read(Result, sizeof(Result));
  Result := LEtoN(Result);
end;

function WinReadInt64(Stream: TStream): int64;
begin
  stream.Read(Result, sizeof(Result));
  Result := LEtoN(Result);
end;

function WinReadQWord(Stream: TStream): QWord;
begin
  stream.Read(Result, sizeof(Result));
  Result := LEtoN(Result);
end;

{$hints on}

function GetFieldTypeSize(const fieldType: TFieldType): longword;
begin
  case fieldType.category of
    ftPrimitiveType:
      case fieldType.primitiveType of
        ptBoolean, ptByte,ptSByte: result := 1;
        ptChar,ptString, ptDecimal: Result := sizeof(string);
        ptSingle: result := sizeof(single);
        ptDouble: result := sizeof(double);
        ptInt16,ptUInt16: result := 2;
        ptInt32,ptUInt32: result := 4;
        ptInt64,ptUInt64,ptDateTime: result := 8;
      else
        raise Exception.Create('Unknown primitive type (' + IntToStr(
          byte(fieldType.primitiveType)) + ')');
      end;
    ftString, ftObjectType, ftRuntimeType, ftGenericType, ftArrayOfObject,
    ftArrayOfString, ftArrayOfPrimitiveType: result := 4;
  else
    raise Exception.Create('Unknown field type (' + IntToStr(
      byte(fieldType.category)) + ')');
  end;
end;

function IsDotNetTypeStoredAsString(const fieldType: TFieldType): boolean;
begin
  result := (fieldType.category = ftPrimitiveType) and
    (fieldType.primitiveType in [ptChar,ptString,ptDecimal]);
end;

function DotNetValueToString(var value; const fieldType: TFieldType): string;
var
  tempByte:     byte;
  value2bytes: record
    case byte of
    2: (tempWord: word);
    3: (tempInt16: smallint);
  end;
  value4bytes: record
    case byte of
    1: (tempSingle:   single);
    2: (tempLongWord: longword);
    3: (tempLongInt: longint);
  end;
  value8bytes: record
    case byte of
    1: (tempDouble:   double);
    2: (tempInt64:    Int64);
    2: (tempUInt64:   QWord);
  end;
  tempIdObject: longword;

begin
  if IsDotNetTypeStoredAsString(fieldType) then
  begin
    Result := pstring(@value)^;
    exit;
  end;
  case fieldType.category of
    ftPrimitiveType: case fieldType.primitiveType of
        ptBoolean:
        begin
          {$hints off}
          move(value,tempByte,sizeof(tempByte));
          {$hints on}
          if tempByte = 0 then
            Result := 'False'
          else
          if tempByte = 1 then
            Result := 'True'
          else
            raise Exception.Create('Invalid boolean value (' +
              IntToStr(tempByte) + ')');
        end;
        ptByte: Result := inttostr(pbyte(@value)^);
        ptSByte: Result := inttostr(pshortint(@value)^);
        ptInt16,ptUInt16:
        begin
          {$hints off}
          move(value, value2bytes.tempWord,sizeof(word));
          {$hints on}
          value2bytes.tempWord := LEtoN(value2bytes.tempWord);
          if fieldType.primitiveType = ptInt16 then
            Result := IntToStr(value2bytes.tempInt16)
          else
            Result := IntToStr(value2bytes.tempWord);
        end;
        ptInt32,ptUInt32,ptSingle:
        begin
          {$hints off}
          move(value, value4bytes.tempLongWord,sizeof(longword));
          {$hints on}
          value4bytes.tempLongWord := LEtoN(value4bytes.tempLongWord);
          if fieldType.primitiveType = ptInt32 then
            Result := IntToStr(value4bytes.tempLongInt)
          else if fieldType.primitiveType = ptUInt32 then
            Result := IntToStr(value4bytes.tempLongWord)
          else
            result := FloatToStr(value4bytes.tempSingle);
        end;

        ptInt64,ptUInt64,ptDouble,ptDateTime:
        begin
          {$hints off}
          move(value, value8bytes.tempUInt64,8);
          {$hints on}
          value8bytes.tempUInt64 := LEtoN(value8bytes.tempUInt64);
          if fieldType.primitiveType = ptInt64 then
            Result := IntToStr(value8bytes.tempInt64)
          else if fieldType.primitiveType = ptUInt64 then
            Result := IntToStr(value8bytes.tempUInt64)
          else if fieldType.primitiveType = ptDouble then
            result := FloatToStr(value8bytes.tempDouble)
          else
            Result := DateTimeToStr(
            (value8bytes.tempUInt64 and $7FFFFFFFFFFFFFFF - 599264352000000000) / 864000000000);
        end;
        else
          raise Exception.Create('Unknown primitive type (' + IntToStr(
            byte(fieldType.primitiveType)) + ')');
      end;
    ftString, ftObjectType, ftRuntimeType, ftGenericType, ftArrayOfObject,
    ftArrayOfString, ftArrayOfPrimitiveType:
    begin
      {$hints off}
      move(value,tempIdObject,sizeof(tempIdObject));
      {$hints on}
      result := '#' + IntToStr(tempIdObject);
    end;
    else
      raise Exception.Create('Unknown field type (' + IntToStr(
        byte(fieldType.category)) + ')');
  end;
end;

function PrimitiveTypeName(pt: TPrimitiveType): string;
begin
  case pt of
    ptBoolean: Result  := 'Boolean';
    ptByte: Result     := 'Byte';
    ptChar: Result     := 'Char';
    ptDecimal: Result  := 'Decimal';
    ptDouble: Result   := 'Double';
    ptInt16: Result    := 'Int16';
    ptInt32: Result    := 'Int32';
    ptInt64: Result    := 'Int64';
    ptSByte: Result    := 'SByte';
    ptSingle: Result   := 'Single';
    ptDateTime: Result := 'DateTime';
    ptUInt16: Result   := 'UInt16';
    ptUInt32: Result   := 'UInt32';
    ptUInt64: Result   := 'UInt64';
    ptString: Result   := 'String';
    else
      raise Exception.Create('Unknown primitive type (' + IntToStr(integer(pt)) + ')');
  end;
end;

Function DotNetTypeToString(ft: TFieldType): string;
begin
  if ft.category = ftPrimitiveType then
    result := PrimitiveTypeName(ft.primitiveType)
  else
    case ft.category of
      ftString: result := 'String';
      ftObjectType: result := 'Object';
      ftRuntimeType: result := 'RuntimeType';
      ftGenericType: result := 'GenericType';
      ftArrayOfObject: result := 'Object[]';
      ftArrayOfString: result := 'String[]';
      ftArrayOfPrimitiveType: result := 'PrimitiveType[]';
    else
      raise Exception.Create('Unknown field type (' + IntToStr(
        byte(ft.category)) + ')');
    end;
end;

{ TCustomSerializedObject }

function TCustomSerializedObject.GetFieldAsString(Name: string): string;
begin
  result := GetFieldAsString(GetFieldIndex(Name));
end;

constructor TCustomSerializedObject.Create(container: TDotNetDeserialization);
begin
  FContainer := container;
  refCount := 0;
end;

function TCustomSerializedObject.GetFieldIndex(Name: string): integer;
var
  i: integer;
  fn: string;
begin
  if FieldCount = 0 then
  begin
    result := -1;
    exit;
  end;
  //case sensitive
  for i := 0 to FieldCount-1 do
    if FieldName[i] = Name then
    begin
      Result := i;
      exit;
    end;
  //case insensitive
  for i := 0 to FieldCount-1 do
    if compareText(FieldName[i], Name) = 0 then
    begin
      Result := i;
      exit;
    end;
  //case sensitive inner member
  for i := 0 to FieldCount-1 do
  begin
    fn := FieldName[i];
    if (length(Name) < length(fn)) and
      (copy(fn, length(fn) - length(Name),
      length(Name) + 1) = '+' + Name) then
    begin
      Result := i;
      exit;
    end;
  end;
  //case insensitive inner member
  for i := 0 to FieldCount-1 do
  begin
    fn := FieldName[i];
    if (length(Name) < length(fn)) and
      (compareText(copy(fn, length(fn) -
      length(Name), length(Name) + 1), '+' + Name) = 0) then
    begin
      Result := i;
      exit;
    end;
  end;
  Result := -1;
end;

{ TSerializedClass }

function TSerializedClass.GetFieldAsString(Index: longword): string;
begin
  result := fields[Index].Value;
end;

function TSerializedClass.GetFieldCount: longword;
begin
  Result:= length(fields);
end;

function TSerializedClass.GetFieldName(Index: longword): string;
begin
  result := fields[Index].Name;
end;

function TSerializedClass.GetFieldTypeAsString(Index: longword): string;
begin
  result := fields[Index].valueType;
end;

function TSerializedClass.IsReferenceType(index: longword): boolean;
begin
  Result:= FContainer.objectTypes[numType].fieldTypes[index].category <> ftPrimitiveType;
end;

function TSerializedClass.GetTypeAsString: string;
begin
  Result:= FContainer.objectTypes[numType].ClassName;
end;

{ TSerializedArray }

procedure TSerializedArray.InitData;
begin
  FItemSize := GetFieldTypeSize(itemType);
  getmem(data, itemSize*nbItems);
  fillchar(data^, itemSize*nbItems, 0);
end;

function TSerializedArray.GetItemPtr(Index: longword): pointer;
begin
  if index >= nbItems then
    raise exception.Create('Index out of bounds');
  result := pointer(pbyte(data)+Index*itemsize);
end;

function TSerializedArray.GetFieldAsString(Index: longword): string;
begin
  if data = nil then
    result := ''
  else
    result := DotNetValueToString(ItemPtr[index]^, itemType);
end;

function TSerializedArray.GetFieldCount: longword;
begin
  Result:= nbItems;
end;

function TSerializedArray.GetFieldName(Index: longword): string;
var
  r: longword;
begin
  result := '[';
  for r := 1 to length(dimensions) do
  begin
    if r <> 1 then result+=',';
    result += inttostr(index mod dimensions[r-1]);
    index := index div dimensions[r-1];
  end;
  result += ']';
end;

{$hints off}
function TSerializedArray.GetFieldTypeAsString(Index: longword): string;
begin
  Result:= DotNetTypeToString(itemType);
end;
{$hints on}

{$hints off}
function TSerializedArray.IsReferenceType(index: longword): boolean;
begin
  Result:= itemType.category <> ftPrimitiveType;
end;
{$hints on}

function TSerializedArray.GetTypeAsString: string;
var
  i: Integer;
begin
  Result:= DotNetTypeToString(itemType)+'[';
  for i := 2 to length(dimensions) do
    result += ',';
  result += ']';
end;

constructor TSerializedArray.Create(AContainer: TDotNetDeserialization; AItemType: TFieldType; ALength: longword);
begin
  inherited Create(AContainer);
  setlength(dimensions,1);
  dimensions[0] := ALength;
  nbItems := ALength;
  FArrayType := gatSingleDimension;
  itemType := AItemType;
  InitData;
end;

constructor TSerializedArray.Create(AContainer: TDotNetDeserialization; AArrayType: TGenericArrayType; AItemType: TFieldType;
  ADimensions: arrayOfLongword);
var n: longword;
begin
  inherited Create(AContainer);
  setlength(dimensions, length(ADimensions));
  nbItems := 1;
  if length(ADimensions) <> 0 then
    for n := 0 to length(ADimensions)-1 do
    begin
      dimensions[n] := ADimensions[n];
      nbItems *= ADimensions[n];
    end;
  FArrayType := AArrayType;
  itemType := AItemType;
  InitData;
end;

destructor TSerializedArray.Destroy;
var ps: PString;
  n: longword;
begin
  if IsDotNetTypeStoredAsString(itemType) and (nbItems <> 0) then
  begin
    ps := PString(data);
    for n := 1 to nbItems do
    begin
      ps^ := '';
      inc(ps);
    end;
  end;
  freemem(data);
  inherited Destroy;
end;

{ TSerializedValue }

function TSerializedValue.GetIsReferenceType: boolean;
begin
  result := inherited IsReferenceType(0);
end;

function TSerializedValue.GetValueAsString: string;
begin
  result := GetFieldAsString(0);
end;

function TSerializedValue.GetTypeAsString: string;
begin
  Result:= GetFieldTypeAsString(0);
end;

constructor TSerializedValue.Create(AContainer: TDotNetDeserialization;
  AItemType: TFieldType);
begin
  inherited Create(AContainer,AItemType,1);
end;

{ TDotNetDeserialization }

function TDotNetDeserialization.FindClass(typeName: string): TSerializedClass;
var obj: TCustomSerializedObject;
begin
  obj := FindObject(typeName);
  if obj is TSerializedClass then
    result := obj as TSerializedClass
  else
    raise exception.Create('FindClass: found object is not a class');
end;

function TDotNetDeserialization.FindObject(typeName: string): TCustomSerializedObject;
var
  i:   integer;
  comparedType: string;
begin
  for i := 0 to high(objects) do
  begin
    comparedType := objects[i].TypeAsString;
    if (comparedType = typeName) or
      ( (length(typeName) < length(comparedType) ) and
        (copy(comparedType, length(comparedType) - length(typeName),
        length(typeName) + 1) = '.' + typeName) ) then
    begin
      Result := objects[i];
      exit;
    end;
  end;
  Result := nil;
end;

function TDotNetDeserialization.GetSimpleField(obj: TCustomSerializedObject;
  Name: string): string;
var
  i,idxSlash: integer;
  tempSub: TCustomSerializedObject;
begin
  i := obj.GetFieldIndex(Name);
  if i = -1 then
  begin
    idxSlash := pos('\',name);
    if idxSlash <> 0 then
    begin
      tempSub := GetObjectField(obj,copy(name,1,idxSlash-1));
      if tempSub <> nil then
      begin
        result := GetSimpleField(tempSub,copy(name,idxSlash+1,length(name)-idxSlash));
        exit;
      end;
    end;
    Result := ''
  end
  else
  begin
    if IsBoxedValue(obj, i) then
      Result := GetBoxedValue(obj, i)
    else
      Result := obj.FieldAsString[i];
  end;
end;

function TDotNetDeserialization.GetObjectField(obj: TCustomSerializedObject;
  Name: string): TCustomSerializedObject;
var
  i: integer;
  idxSlash: LongInt;
  tempSub: TCustomSerializedObject;
begin
  i := obj.GetFieldIndex(Name);
  if i = -1 then
  begin
    idxSlash := pos('\',name);
    if idxSlash <> 0 then
    begin
      tempSub := GetObjectField(obj,copy(name,1,idxSlash-1));
      if tempSub <> nil then
      begin
        result := GetObjectField(tempSub,copy(name,idxSlash+1,length(name)-idxSlash));
        exit;
      end;
    end;
    Result := nil
  end
  else
  begin
    if not obj.IsReferenceType(i) then
      raise Exception.Create('GetObjectField: Not a reference type');
    Result := GetObject(obj.FieldAsString[i]);
  end;
end;

function TDotNetDeserialization.GetObjectField(obj: TCustomSerializedObject;
  index: integer): TCustomSerializedObject;
begin
  if not obj.IsReferenceType(index) then
    raise Exception.Create('GetObjectField: Not a reference type');
  Result := GetObject(obj.FieldAsString[index]);
end;

function TDotNetDeserialization.GetObject(id: string): TCustomSerializedObject;
var
  idObj: longword;
begin
  if copy(id, 1, 1) = '#' then
    Delete(id, 1, 1);
  idObj  := StrToInt64(id);
  Result := GetObject(idObj);
end;

function TDotNetDeserialization.GetObject(id: longword): TCustomSerializedObject;
var
  i: integer;
begin
  for i := 0 to high(objects) do
    if objects[i].idObject = id then
    begin
      Result := objects[i];
      exit;
    end;
  Result := nil;
end;

function TDotNetDeserialization.IsBoxedValue(obj: TCustomSerializedObject;
  index: integer): boolean;
var
  subObj: TCustomSerializedObject;
begin
  if not obj.IsReferenceType(index) then
  begin
    Result := False;
    exit;
  end;
  subObj := GetObject(obj.FieldAsString[index]);
  if subObj = nil then //suppose Nothing is a boxed value
  begin
    Result := True;
    exit;
  end;
  Result := subObj is TSerializedValue;
end;

function TDotNetDeserialization.GetBoxedValue(obj: TCustomSerializedObject;
  index: integer): string;
var
  subObj: TCustomSerializedObject;
begin
  if not obj.IsReferenceType(index) then
    raise Exception.Create('GetBoxedValue: Not a reference type');
  subObj := GetObject(obj.FieldAsString[index]);
  if subObj = nil then
  begin
    Result := ''; //empty value
    exit;
  end;
  if (subObj is TSerializedValue) and not (subObj as TSerializedValue).IsReferenceType then
    Result := (subObj as TSerializedValue).ValueAsString
  else
    raise Exception.Create('GetBoxedValue: Not a primitive type');
end;

procedure TDotNetDeserialization.LoadFromStream(Stream: TStream);
var
  header: packed record
    blockId: byte;
    value1, value2, value3, value4: longint;
  end;
  curStreamPosition, prevStreamPosition: int64;
begin
  {$hints off}
  if Stream.Read(header, sizeof(header)) <> sizeof(header) then
    raise Exception.Create('Invalid header size');
  if (header.blockId <> 0) or (header.value1 <> 1) or (header.value2 <> -1) or
    (header.value3 <> 1) or (header.value4 <> 0) then
    raise Exception.Create('Invalid header format');
  {$hints on}

  EndOfStream := False;
  curStreamPosition := Stream.Position;
  try
    while (Stream.Position < Stream.Size) and not EndOfStream do
    begin
      prevStreamPosition := curStreamPosition;
      curStreamPosition  := Stream.Position;
      LoadNextFromStream(Stream);
    end;
  except
    on ex: Exception do
      raise Exception.Create('Error while loading serialized data at position ' +
        IntToStr(stream.Position) + ' (block starting at ' +
        IntToStr(curStreamPosition) + ', previous block at ' +
        IntToStr(prevStreamPosition) + '). ' + ex.message);
  end;
end;

procedure TDotNetDeserialization.LoadFromFile(filename: string);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(filename, fmOpenRead);
  try
    LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TDotNetDeserialization.LoadFromFileUTF8(filenameUTF8: string);
var
  stream: TFileStreamUTF8;
begin
  stream := TFileStreamUTF8.Create(filenameUTF8, fmOpenRead);
  try
    LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

function TDotNetDeserialization.ToString: string;

  function ObjectToString(num: integer; expectedType: string;
    tab: string; main: boolean): string;
  var
    j, k:   integer;
    subId:  longword;
    subNum: integer;
    objType, subExpectedType: string;
    fieldTypeStr: string;
  begin
    Result := '';
    if (num < 0) or (num > high(objects)) then
      raise Exception.Create('Index out of bounds');
    with objects[num] do  //here array is not changed so it won't move
    begin
      if inToString then
      begin
        if main then
          Result := ''
        else
          Result := '#' + IntToStr(idObject) + LineEnding;
        exit;
      end;
      inToString := True;
      objType := TypeAsString;
      if main then
      begin
        Result += tab + 'Object';
        Result += ' #' + IntToStr(idObject);
        if (objType = '') or (objType = expectedType) then
          Result += ' = '
        else
          Result += ' As ' + objType + ' = ';
      end
      else
      begin
        if (objType = '') or (objType = expectedType) then
          Result := ''
        else
          Result := '(' + objType + ') ';
        if (idObject < idArrayFiller) and (refCount > 0) then
          Result += '#' + IntToStr(idObject) + ' = ';
      end;
      if (length(objType) > 2) and (copy(objType, length(objType) - 1, 2) = '[]') then
        subExpectedType := copy(objType, 1, length(objType) - 2)
      else
        subExpectedType := '';

      if not main and (objects[num] is TSerializedValue) then
      begin
        Result += (objects[num] as TSerializedValue).ValueAsString + LineEnding;
      end
      else
      if (FieldCount = 0) then
      begin
        Result += '{}' + LineEnding;
      end
      else
      begin
        Result += '{' + LineEnding;
        for j := 0 to FieldCount-1 do
        begin
          Result += tab + '  ' + FieldName[j];
          fieldTypeStr := FieldTypeAsString[j];
          if (fieldTypeStr <> '') and (fieldTypeStr <> subExpectedType) and
            not ((subExpectedType = '') and ((fieldTypeStr = 'Int32') or
            (fieldTypeStr = 'Boolean') or (fieldTypeStr = 'Double'))) then
            Result += ' As ' + fieldTypeStr;
          Result   += ' = ';
          if not IsReferenceType(j) then
            Result += FieldAsString[j] + lineending
          else
          begin
            try
              subId  := StrToInt64(copy(fieldAsString[j], 2, length(fieldAsString[j]) - 1));
              if subId = 0 then result += 'null'+LineEnding else
              begin
                begin
                  subNum := -1;
                  for k := 0 to high(objects) do
                  if (objects[k].idObject = subId) then
                  begin
                    subNum := k;
                    break;
                  end;
                end;
                if subNum = -1 then
                  Result += '(Not found) #' + IntToStr(subId)+LineEnding
                else
                  Result += objectToString(subNum, fieldTypeStr, tab + '  ', False);
              end;
            except
              result += '!' + fieldAsString[j]+'!' +LineEnding
            end;
          end;
        end;
        Result += tab + '}' + LineEnding;
        if main then
          Result += LineEnding;
      end;
    end;
  end;

var
  i: integer;
begin
  Result := '';
  for i := 0 to high(assemblies) do
    Result += 'Imports ' + assemblies[i].Name + LineEnding;
  Result   += lineEnding;
  for i := 0 to high(objects) do
    objects[i].inToString := False;
  for i := 0 to high(objects) do
    Result += ObjectToString(i, 'Object', '', True);
end;

constructor TDotNetDeserialization.Create;
begin
  currentAutoObjectValue := idArrayFiller + 1;
end;

destructor TDotNetDeserialization.Destroy;
var
  i: Integer;
begin
  for i := 0 to high(objects) do
    objects[i].Free;
  inherited Destroy;
end;

function TDotNetDeserialization.GetTypeOfClassObject(idObject: longword
  ): integer;
var
  i: Integer;
begin
  for i := 0 to high(objects) do
    if objects[i].idObject = idObject then
    begin
      if objects[i] is TSerializedClass then
      begin
        result := (objects[i] as TSerializedClass).numType;
        exit;
      end
      else
        raise exception.Create('GetTypeOfClassObject: Specified object is not of class type');
    end;
  raise exception.Create('GetTypeOfClassObject: Object not found');
end;

function TDotNetDeserialization.nextAutoObjectId: longword;
begin
  Inc(currentAutoObjectValue);
  Result := currentAutoObjectValue;
end;

function TDotNetDeserialization.LoadNextFromStream(Stream: TStream): longword;
var
  blockType:    byte;
  idRefObject, tempIdObject: longword;
  tempType:     TFieldType;
  arrayCount, arrayIndex,FillZeroCount : longword;
  tempAnyObj: TCustomSerializedObject;
  newClassObj: TSerializedClass;
  newValueObj: TSerializedValue;
  newArrayObj: TSerializedArray;
  genericArrayType: TGenericArrayType;
  genericArrayRank: longword;
  genericArrayDims: array of longword;
  genericArrayItemType: TFieldType;

  function GetArrayCellNumber(index: longword): string;
  var r: longword;
  begin
    result := '';
    for r := 1 to genericArrayRank do
    begin
      if r <> 1 then result+=',';
      result += inttostr(index mod genericArrayDims[r-1]);
      index := index div genericArrayDims[r-1];
    end;
  end;

begin
  Result := 0; //idObject or zero
  blockType := WinReadByte(Stream);
  case blockType of

    btAssembly:
    begin
      setlength(assemblies, length(assemblies) + 1);
      with assemblies[high(assemblies)] do
      begin
        idAssembly := WinReadLongword(Stream);
        Name := LoadStringFromStream(Stream);
      end;
    end;

    btRuntimeObject, btExternalObject:
    begin
      newClassObj := TSerializedClass.Create(self);
      setlength(objects, length(objects) + 1);
      objects[high(objects)] := newClassObj;
      with newClassObj do
      begin
        idObject := WinReadLongword(Stream);
        Result   := idObject;
        numType  := LoadTypeFromStream(Stream, blockType = btRuntimeObject);
        fields   := LoadValuesFromStream(Stream, numType);
      end;
    end;

    btRefTypeObject:
    begin
      newClassObj := TSerializedClass.Create(self);
      setlength(objects, length(objects) + 1);
      objects[high(objects)] := newClassObj;
      with newClassObj do
      begin
        idObject    := WinReadLongword(Stream);
        Result      := idObject;
        idRefObject := WinReadLongword(Stream);
        numType     := GetTypeOfClassObject(idRefObject);
        fields      := LoadValuesFromStream(Stream, numType);
      end;
    end;

    btString:
    begin
      tempType.primitiveType := ptString;
      tempType.category := ftPrimitiveType;
      tempType.Name := PrimitiveTypeName(ptString);
      tempType.refAssembly := 0;

      newValueObj := TSerializedValue.Create(self,tempType);
      setlength(objects, length(objects) + 1);
      objects[high(objects)] := newValueObj;
      with newValueObj do
      begin
        idObject := WinReadLongword(Stream);
        Result  := idObject;
        pstring(data)^ := LoadStringFromStream(Stream);
      end;
    end;

    btBoxedPrimitiveTypeValue:
    begin
      try
        tempType.category    := ftPrimitiveType;
        tempType.refAssembly := 0;
        tempType.primitiveType := TPrimitiveType(WinReadByte(stream));
        tempType.Name := PrimitiveTypeName(tempType.primitiveType);

        newValueObj := TSerializedValue.Create(self,tempType);
        setlength(objects, length(objects) + 1);
        objects[high(objects)] := newValueObj;

        with newValueObj do
        begin
          idObject := nextAutoObjectId;
          Result   := idObject;

          if IsDotNetTypeStoredAsString(tempType) then
            pstring(data)^ := LoadValueFromStream(Stream, tempType)
          else
            Stream.Read(data^, itemSize);
        end;
      except
        on ex: Exception do
          raise Exception.Create('Error while reading boxed primitive values. ' +
            ex.Message);
      end;
    end;

    btObjectReference:
    begin
      result := WinReadLongword(Stream);
      tempAnyObj := GetObject(Result);
      if tempAnyObj <> nil then
        Inc(tempAnyObj.refCount);
    end;

    btNullValue: Result := 0;

    btArrayOfPrimitiveType:
    begin
      try
        result := WinReadLongword(Stream);
        arrayCount := WinReadLongword(Stream);

        tempType.category    := ftPrimitiveType;
        tempType.refAssembly := 0;
        tempType.primitiveType := TPrimitiveType(WinReadByte(stream));
        tempType.Name := PrimitiveTypeName(tempType.primitiveType);

        newArrayObj := TSerializedArray.Create(self,tempType,arrayCount);
        setlength(objects, length(objects) + 1);
        objects[high(objects)] := newArrayObj;
        with newArrayObj do
        begin
          idObject := result;

          if arrayCount <> 0 then
          begin
            if IsDotNetTypeStoredAsString(tempType) then
            begin
              for arrayIndex := 0 to arrayCount - 1 do
                pstring(ItemPtr[arrayIndex])^ := LoadValueFromStream(Stream, tempType);
            end else
            begin
              for arrayIndex := 0 to arrayCount - 1 do
                stream.Read(ItemPtr[arrayIndex]^, itemSize);
            end;
          end;
        end;
      except
        on ex: Exception do
          raise Exception.Create('Error while reading array of primitive values. ' +
            ex.Message);
      end;
    end;

    btArrayOfObject,btArrayOfString:
    begin
      try
        result := WinReadLongword(Stream);
        arrayCount := WinReadLongword(Stream);

        if blockType = btArrayOfObject then
          tempType.category := ftObjectType
        else
          tempType.category := ftString;

        tempType.refAssembly := 0;
        tempType.primitiveType := ptNone;
        tempType.Name := DotNetTypeToString(tempType);

        newArrayObj := TSerializedArray.Create(self,tempType,arrayCount);
        setlength(objects, length(objects) + 1);
        objects[high(objects)] := newArrayObj;

        with newArrayObj do
        begin
          idObject:= result;
          FillZeroCount := 0;
          if arrayCount <> 0 then
            for arrayIndex := 0 to arrayCount - 1 do
            begin
              if FillZeroCount > 0 then
                Dec(FillZeroCount)
              else
              begin
                tempIdObject := LoadNextFromStream(Stream);
                if tempIdObject = idArrayFiller then
                begin
                  tempIdObject     := 0;
                  FillZeroCount    := ArrayFillerCount;
                  ArrayFillerCount := 0;
                end;
                if FillZeroCount > 0 then
                  Dec(FillZeroCount)
                else
                  plongword(ItemPtr[arrayIndex])^ := tempIdObject;
              end;
            end;
        end;
      except
        on ex: Exception do
          raise Exception.Create('Error while reading array of object. ' + ex.Message);
      end;
    end;

    btArrayFiller8b, btArrayFiller32b:
    begin
      Result     := idArrayFiller;
      arrayCount := 0;
      if blockType = btArrayFiller8b then
        arrayCount := WinReadByte(Stream)
      else
        arrayCount := WinReadLongWord(Stream);
      ArrayFillerCount := arraycount;
    end;

    btGenericArray:
    begin
        try
          result := WinReadLongword(Stream);
          genericArrayType := TGenericArrayType( WinReadByte(Stream) );
          genericArrayRank := WinReadLongword(Stream);
          setlength(genericArrayDims,genericArrayRank);
          arrayCount := 0;
          if genericArrayRank <> 0 then
            for arrayIndex := 0 to genericArrayRank-1 do
            begin
              genericArrayDims[arrayIndex] := WinReadLongword(Stream);
              if arrayIndex=0 then
                arrayCount := genericArrayDims[arrayIndex]
              else
                arrayCount *= genericArrayDims[arrayIndex];
            end;
          genericArrayItemType.category := TTypeCategory(WinReadByte(Stream));
          genericArrayItemType := LoadFieldType(stream,genericArrayItemType.category);

          newArrayObj := TSerializedArray.Create(self,genericArrayType,genericArrayItemType,genericArrayDims);
          setlength(objects, length(objects) + 1);
          objects[high(objects)] := newArrayObj;
          newArrayObj.idObject := result;

          FillZeroCount := 0;
          if arrayCount <> 0 then
            for arrayIndex := 0 to arrayCount - 1 do
            begin
              if IsDotNetTypeStoredAsString(genericArrayItemType) then
                PString(newArrayObj.ItemPtr[arrayIndex])^ := LoadValueFromStream(Stream,genericArrayItemType)
              else
              if genericArrayItemType.category = ftPrimitiveType then
                Stream.Read(newArrayObj.ItemPtr[arrayIndex]^, newArrayObj.ItemSize)
              else
              begin
                if FillZeroCount > 0 then
                  Dec(FillZeroCount)
                else
                begin
                  tempIdObject := LoadNextFromStream(Stream);
                  if tempIdObject = idArrayFiller then
                  begin
                    tempIdObject     := 0;
                    FillZeroCount    := ArrayFillerCount;
                    ArrayFillerCount := 0;
                  end;
                  if FillZeroCount > 0 then
                    Dec(FillZeroCount)
                  else
                    plongword(newArrayObj.ItemPtr[arrayIndex])^ := tempIdObject;
                end;
              end;
            end;
        except
          on ex: Exception do
            raise Exception.Create('Error while reading array of object. ' + ex.Message);
        end;
      end;

    btMethodCall, btMethodResponse:
      raise Exception.Create('Method or method response not supported');

    btEndOfStream: EndOfStream := True;

    else
      raise Exception.Create('Unknown block type (' + IntToStr(blockType) + ')');
  end;
end;

function TDotNetDeserialization.LoadStringFromStream(Stream: TStream): string;
var
  byteLength, shift: byte;
  fullLength: integer;
  utf8value:  string;
begin
  fullLength := 0;
  shift      := 0;
     {$hints off}
  repeat
    Stream.Read(byteLength, 1);
    Inc(fullLength, (byteLength and 127) shl shift);
    shift := shift + 7;
  until (byteLength < 128) or (shift > 24);
     {$hints on}
  setlength(utf8value, fullLength);
  if Stream.Read(utf8value[1], fullLength) <> fullLength then
    raise Exception.Create('String length error');
  Result := utf8value;
end;

function TDotNetDeserialization.LoadDotNetCharFromStream(Stream: TStream
  ): string;
var
  tempByte: byte;
  dataLen: Byte;
  utf8value: string;
begin
  tempByte:= WinReadByte(Stream);

  if tempByte and $80 = 0 then
    dataLen := 1
  else
  if tempByte and $E0 = $C0 then
    dataLen := 2
  else
  if tempByte and $F0 = $E0 then
    dataLen := 3
  else
  if tempByte and $F8 = $F0 then
    dataLen := 4
  else
    raise Exception.Create('Invalid UTF8 char');

  setlength(utf8value, dataLen);
  utf8value[1] := char(tempByte);
  Stream.Read(utf8value[2], dataLen - 1);
  Result := utf8value;
end;

function TDotNetDeserialization.LoadTypeFromStream(Stream: TStream;
  IsRuntimeType: boolean): integer;
var
  i: integer;
begin
  try
    setlength(objectTypes, length(objectTypes) + 1);
    Result := high(objectTypes);
    with objectTypes[Result] do
    begin
      ClassName := LoadStringFromStream(Stream);
      nbFields := WinReadLongword(Stream);
      setlength(fieldNames, nbFields);
      setlength(fieldTypes, nbFields);
      for i := 0 to nbFields - 1 do
        fieldNames[i] := LoadStringFromStream(Stream);
      for i := 0 to nbFields - 1 do
        fieldTypes[i].category := TTypeCategory(WinReadByte(Stream));
      for i := 0 to nbFields - 1 do
        fieldTypes[i] := LoadFieldType(Stream,fieldTypes[i].category);
      if isRuntimeType then
        refAssembly := 0
      else
        refAssembly := WinReadLongword(Stream);
    end;
  except
    on ex: Exception do
      raise Exception.Create('Error while reading object type definition. ' +
        ex.Message);
  end;
end;

function TDotNetDeserialization.LoadValuesFromStream(Stream: TStream;
  numType: integer): ArrayOfNameValue;
var
  i:  integer;
  ot: TSerializedType;
begin
  if (numType < 0) or (numType > high(objectTypes)) then
    raise Exception.Create('Type number out of bounds (' + IntToStr(numType) + ')');
  ot := objectTypes[numType]; //use temp because array address may change
  try
    with ot do
    begin
      setlength(Result, nbFields);
      for i := 0 to nbFields - 1 do
      begin
        Result[i].Name      := fieldNames[i];
        Result[i].valueType := fieldTypes[i].Name;
        Result[i].Value     := LoadValueFromStream(Stream, fieldTypes[i]);
      end;
    end;
  except
    on ex: Exception do
      raise Exception.Create('Error while reading values of object of type ' +
        ot.ClassName + '. ' + ex.Message);
  end;
end;

function TDotNetDeserialization.LoadValueFromStream(Stream: TStream;
  const fieldType: TFieldType): string;
var
  data : record
    case byte of
    1: (ptr: pointer);
    2: (bytes: array[0..7] of byte);
    end;
  dataLen: longword;
  tempIdObject: longword;
begin
  try
    if fieldType.Category = ftPrimitiveType then
    begin
      case fieldType.primitiveType of
        ptChar: Result := LoadDotNetCharFromStream(Stream);
        ptString, ptDecimal: Result := LoadStringFromStream(Stream);
      else
        begin
          dataLen := GetFieldTypeSize(fieldType);
          {$hints off}
          stream.read(data,dataLen);
          {$hints on}
          result := DotNetValueToString(data,fieldType);
        end;
      end;
    end else
    if fieldType.Category in [ftString, ftObjectType, ftRuntimeType, ftGenericType, ftArrayOfObject,
        ftArrayOfString, ftArrayOfPrimitiveType] then
    begin
      tempIdObject := LoadNextFromStream(stream);
      Result := '#' + IntToStr(tempIdObject);
    end else
      raise Exception.Create('Unknown field type (' + IntToStr(
        byte(fieldType.category)) + ')');
  except
    on ex: Exception do
      raise Exception.Create('Error while reading object value. ' + ex.Message);
  end;
end;

function TDotNetDeserialization.LoadFieldType(Stream: TStream; category: TTypeCategory
  ): TFieldType;
begin
  result.category := category;
  result.Name := '';
  result.refAssembly := 0;
  result.primitiveType := ptNone;
  case category of
    ftPrimitiveType, ftArrayOfPrimitiveType:
    begin
      result.primitiveType := TPrimitiveType(WinReadByte(stream));
      result.Name := PrimitiveTypeName(result.primitiveType);
      if result.category = ftArrayOfPrimitiveType then
        result.Name += '[]';
    end;
    ftString: result.Name      := 'String';
    ftObjectType: result.Name  := 'Object';
    ftRuntimeType: result.Name := LoadStringFromStream(Stream);
    ftGenericType:
    begin
      result.Name := LoadStringFromStream(Stream);
      result.refAssembly := WinReadLongword(Stream);
    end;
    ftArrayOfObject: result.Name := 'Object[]';
    ftArrayOfString: result.Name := 'String[]';
    else
      raise Exception.Create('Unknown field type tag (' + IntToStr(
        byte(result.category)) + ')');
  end;
end;

initialization


end.

