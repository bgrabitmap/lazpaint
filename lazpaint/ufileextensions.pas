// SPDX-License-Identifier: GPL-3.0-only
unit UFileExtensions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, BGRABitmapTypes;

Type
  TExtensionOptions = set of (eoReadable, eoWritable);

var
  PictureFileExtensions: array of record
    name, extensionsWithoutDot: string;
    fileFormat: TBGRAImageFormat;
    filterForAllCases: string;
    options: TExtensionOptions;
  end;

//returns the list of extensions for a specified filter index.
//- if none is found, nil is returned.
function GetSelectedFilterExtensions(const Filter: string; FilterIndex: integer; ARemoveLeadingDot: boolean): TStringList;

//apply the extension of the selected filter index
//- if the extension is already among the extensions for this filter index, the extension is not changed
//- otherwise, the first extension is used
//- it is not case sensitive
function ApplySelectedFilterExtension(const FileName: string; const Filter: string; FilterIndex: integer): string;

function GetExtensionFilter(AOption: TExtensionOptions; ADisplayPrefix: string = '*.'): string;
function GetExtensionFilterIndex(AOption: TExtensionOptions; AExtensions: string): integer;
function GetExtensionFilterByIndex(AOption: TExtensionOptions; AIndex: integer): string;

procedure RegisterPicExt(AName: string; AExtensionsWithoutDot: string; AOptions: TExtensionOptions);

function IsExtensionReadable(AFilename: string): boolean;
function IsExtensionWritable(AFilename: string): boolean;

function GetImageFormatName(AFormat: TBGRAImageFormat): string;

implementation

uses Masks, LazUTF8, UResourceStrings, BGRASVG,
     BGRALayerOriginal, BGRASVGOriginal, BGRAGradientOriginal,
     LCVectorOriginal, LCVectorShapes, URaw;

function GetSelectedFilterExtensions(const Filter: string;
  FilterIndex: integer; ARemoveLeadingDot: boolean): TStringList;
var
  ParsedFilter: TParseStringList;
  i: integer;
  ext: string;
begin
  Result := nil;
  ParsedFilter := TParseStringList.Create(Filter, '|');
  try
    if (FilterIndex > 0) and (FilterIndex * 2 <= ParsedFilter.Count) then
    begin
      Result := TParseStringList.Create(ParsedFilter[FilterIndex * 2 - 1],';');
      //go backward because we may delete some entries
      for i := result.Count-1 downto 0 do
      begin
        ext := ExtractFileExt(result[i]);
        if ARemoveLeadingDot and (length(ext)>0) and (ext[1]='.') then Delete(ext,1,1);
        //ignore extensions with wildcards and empty extensions
        if (pos('*',ext)<>0) or (pos('?',ext)<>0) or (ext = '') then
          result.Delete(i)
        else
          result[i] := ext;
      end;
    end;
  finally
    ParsedFilter.Free;
  end;
  if (result <> nil) and (result.count = 0) then FreeAndNil(result);
end;

function ApplySelectedFilterExtension(const FileName: string;
  const Filter: string; FilterIndex: integer): string;
var exts: TStringList;
  currentExt: string;
  i: integer;
  found: boolean;
begin
  exts := GetSelectedFilterExtensions(Filter,FilterIndex,False);
  if exts = nil then
  begin
    result := FileName;
    exit;
  end;
  currentExt := ExtractFileExt(FileName);
  found := false;
  for i := 0 to exts.Count-1 do
  begin
    if UTF8CompareText(exts[i],currentExt) = 0 then
    begin
      found := true;
      break;
    end;
  end;
  if found or (exts.Count = 0) then
    result := FileName
  else
    result := ChangeFileExt(FileName, exts[0]);
  exts.Free;
end;

function GetExtensionFilter(AOption: TExtensionOptions; ADisplayPrefix: string = '*.'): string;
var i: integer;
  extDescription, allExtWithoutDot, allExtFilter: string;
begin
  result := '';
  allExtWithoutDot := '';
  allExtFilter := '';
  for i := 0 to high(PictureFileExtensions) do
    if (PictureFileExtensions[i].options * AOption = AOption) and
     (PictureFileExtensions[i].filterForAllCases <> '') then
    begin
      if result <> '' then result += '|';
      extDescription := ADisplayPrefix + StringReplace(PictureFileExtensions[i].extensionsWithoutDot,';',', ' +ADisplayPrefix,[rfReplaceAll]);
      result += PictureFileExtensions[i].name+' ('+extDescription+')|'+PictureFileExtensions[i].filterForAllCases;
      //do not repeat extensions in all-file-types
      if pos(', '+extDescription+', ', ', '+allExtWithoutDot+', ') = 0 then
      begin
        if allExtWithoutDot <> '' then allExtWithoutDot += ', ';
        allExtWithoutDot += extDescription;
        if allExtFilter <> '' then allExtFilter += ';';
        allExtFilter += PictureFileExtensions[i].filterForAllCases;
      end;
    end;
  if allExtWithoutDot = '' then
  begin
    allExtWithoutDot := ADisplayPrefix + '*';
    allExtFilter:= '*.*';
  end;
  if result <> '' then result := '|' + result;
  if length(allExtWithoutDot)>12 then
    result := rsAllSupportedFiletypes + ' (' + ADisplayPrefix+ '*)|' + allExtFilter + result
  else
    result := rsAllSupportedFiletypes + ' (' + allExtWithoutDot + ')|' + allExtFilter + result;
end;

function GetExtensionFilterIndex(AOption: TExtensionOptions; AExtensions: string): integer;
var
  i: Integer;
begin
  result := 2;
  for i := 0 to high(PictureFileExtensions) do
    if (PictureFileExtensions[i].options * AOption = AOption) and
     (PictureFileExtensions[i].filterForAllCases <> '') then
    begin
      if PictureFileExtensions[i].filterForAllCases = AExtensions then exit;
      inc(result);
    end;
  result := 1;
end;

function GetExtensionFilterByIndex(AOption: TExtensionOptions; AIndex: integer): string;
var curIndex, i: integer;
begin
  curIndex := 2;
  for i := 0 to high(PictureFileExtensions) do
    if (PictureFileExtensions[i].options * AOption = AOption) and
     (PictureFileExtensions[i].filterForAllCases <> '') then
    begin
      if curIndex = AIndex then exit(PictureFileExtensions[i].filterForAllCases);
      inc(curIndex);
    end;
  result := '*.*';
end;

function GetBit(Value: QWord; Index: Byte): Boolean;
begin
  Result := ((Value shr Index) and 1) = 1;
end;

{(en) Converts AStrUtf8 to uppercase if AUppercase = true otherwise to lowercase}
function ULCaseUtf8(AStrUtf8: string; AUppercase: Boolean): string;
begin
  if AUppercase then Result:=UTF8UpperCase(AStrUtf8) else Result:= UTF8LowerCase(AStrUtf8);
end;

{(en) Generates various cases that may be encountered}
function SingleExtAllCases (ASingleExtension: string; Delimiter: String=';'; Prefix: string=''; Suffix: String=''):string;
var
  otherCase: String;
begin
  Result := Prefix + ASingleExtension + Suffix;

  otherCase := UTF8LowerCase(ASingleExtension);
  if otherCase <> ASingleExtension then
    Result += Delimiter + Prefix + otherCase + Suffix;

  otherCase := UTF8UpperCase(ASingleExtension);
  if otherCase <> ASingleExtension then
    Result += Delimiter + Prefix + otherCase + Suffix;

  otherCase := UTF8UpperCase(UTF8Copy(ASingleExtension, 1, 1)) +
               UTF8LowerCase(UTF8Copy(ASingleExtension, 2, UTF8Length(ASingleExtension) - 1));
  if otherCase <> ASingleExtension then
    Result += Delimiter + Prefix + otherCase + Suffix;
end;

{(en) Generates various cases of file extensions}
function ExtensionsAllCases (AllExtensions: String; ADelimiter: string = ';'; APrefix:string = '*.'): String;
var
  ExtList: TStringList;
  i: integer;
  item: string;
begin
  Result := '';
  ExtList:= TParseStringList.Create(AllExtensions,ADelimiter);
  for i:=0 to ExtList.Count -1 do
  begin
    item := SingleExtAllCases(ExtList[i],ADelimiter,APrefix,'');
    if item <> '' then
    begin
       if Result <> '' then result += ADelimiter;
       Result += item;
    end;
  end;
  ExtList.Free;
end;
//end All case extension subs

procedure RegisterPicExt(AName: string; AExtensionsWithoutDot: string;
  AOptions: TExtensionOptions);
var extList: TStringList;
  i: integer;
  suggested: TBGRAImageFormat;
begin
  setlength(PictureFileExtensions, length(PictureFileExtensions)+1);
  with PictureFileExtensions[high(PictureFileExtensions)] do
  begin
    name := AName;
    extensionsWithoutDot := AExtensionsWithoutDot;
    filterForAllCases:= ExtensionsAllCases(extensionsWithoutDot, ';', '*.');
    fileFormat := ifUnknown;
    extList := TParseStringList.Create(extensionsWithoutDot,';');
    for i := 0 to extList.Count-1 do
    begin
      suggested := SuggestImageFormat(extList[i]);
      if suggested <> ifUnknown then
      begin
        if fileFormat = ifUnknown then
          fileFormat:= suggested
        else if fileFormat <> suggested then //contradiction
        begin
          fileFormat:= ifUnknown;
          break;
        end;
      end;
    end;
    extList.free;

    options := AOptions;
  end;
end;

function HasExtensionOptions(AFilename: string; AOptions: TExtensionOptions): boolean;
var
  ext: string;
  i : integer;
begin
  ext := UTF8LowerCase(ExtractFileExt(AFilename));
  if (ext<>'') and (ext[1]='.') then delete(ext,1,1);
  for i := 0 to high(PictureFileExtensions) do
  begin
    if pos(';'+ext+';', UTF8LowerCase(';'+PictureFileExtensions[i].extensionsWithoutDot+';'))<> 0 then
    begin
      if PictureFileExtensions[i].options * AOptions = AOptions then
      begin
        result := true;
        exit;
      end;
    end;
  end;
  result := false;
end;

function IsExtensionReadable(AFilename: string): boolean;
begin
  result := HasExtensionOptions(AFilename,[eoReadable]);
end;

function IsExtensionWritable(AFilename: string): boolean;
begin
  result := HasExtensionOptions(AFilename,[eoWritable]);
end;

function GetImageFormatName(AFormat: TBGRAImageFormat): string;
var i: integer;
begin
  if AFormat = ifUnknown then
  begin
    result := 'Unknown';
    exit;
  end;
  for i := 0 to high(PictureFileExtensions) do
    if PictureFileExtensions[i].fileFormat = AFormat then
    begin
      result := PictureFileExtensions[i].name;
      exit;
    end;
  result := 'Error';
end;

initialization

  RegisterPicExt(rsLayeredImage,'lzp;ora;pdn;oXo', [eoReadable]);
  RegisterPicExt(rsLayeredImage,'lzp;ora;oXo', [eoWritable]);
  RegisterPicExt('AVIF','avif', [eoReadable,eoWritable]);
  RegisterPicExt(rsBitmap,'bmp', [eoReadable,eoWritable]);
  RegisterPicExt(rsAnimatedGIF,'gif', [eoReadable,eoWritable]);
  RegisterPicExt(rsIconOrCursor,'ico;cur', [eoReadable,eoWritable]);
  RegisterPicExt('JPEG','jpg;jpeg', [eoReadable,eoWritable]);
  RegisterPicExt(rsLazPaint,'lzp', [eoReadable,eoWritable]);
  RegisterPicExt(rsOpenRaster,'ora', [eoReadable,eoWritable]);
  RegisterPicExt('PC eXchange','pcx', [eoReadable,eoWritable]);
  RegisterPicExt('Paint.NET','pdn', [eoReadable]);
  RegisterPicExt('PhoXo','oXo', [eoReadable,eoWritable]);
  RegisterPicExt('Portable Network Graphic','png', [eoReadable,eoWritable]);
  RegisterPicExt(rsPhotoshop,'psd', [eoReadable]);
  BGRASVG.RegisterSvgFormat;
  RegisterPicExt('Scalable Vector Graphic','svg', [eoReadable, eoWritable]);
  RegisterPicExt('Targa','tga', [eoReadable,eoWritable]);
  RegisterPicExt('Tiff','tif;tiff', [eoReadable,eoWritable]);
  RegisterPicExt('WebP','webp', [eoReadable,eoWritable]);
  RegisterPicExt('X PixMap','xpm', [eoReadable,eoWritable]);
  RegisterPicExt('Portable Any Map', 'pbm;pgm;ppm', [eoReadable]);
  RegisterPicExt('X Window','xwd', [eoReadable]);
  RegisterPicExt('Raw',AllRawExtensions, [eoReadable]);

end.

