// SPDX-License-Identifier: GPL-3.0-only
unit UBrushType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UScripting, BGRABitmap, BGRABitmapTypes;

type

  { TLazPaintBrush }

  TLazPaintBrush = class
  private
    function GetAsString: string;
    procedure SetAsString(AValue: string);
  protected
    FVariable: TVariableSet;
    FSize: Single;
    FBrushImage: TBGRABitmap;
    FSourceImage: TBGRABitmap;
    FSourceImageFetched: boolean;
    procedure SetIsGradient(AValue: boolean);
    procedure SetSize(AValue: Single);
    procedure InvalidateBrush;
    procedure InvalidateSource;
    function GetFileName: string;
    function GetIsGradient: boolean;
    function GetSideCount: integer;
    function GetSourceImage: TBGRABitmap;
    function MakeBrushBitmap: TBGRABitmap;
    procedure SetFileName(AValue: string);
    procedure SetSideCount(AValue: integer);
    function GetBrushImage: TBGRABitmap;
    function GetStream64: string;
    procedure SetStream64(AValue: string);
    procedure Init;
  public
    constructor Create;
    constructor Create(ASideCount: integer; AIsGradient: boolean);
    constructor CreateFromStream64(AStream64: string);
    destructor Destroy; override;
    procedure AssignBrushImage(ABitmap: TBGRABitmap);
    function MakeColoredBrushImage(AColor: TBGRAPixel): TBGRABitmap;
    procedure Put(ADest: TBGRABitmap; x,y: integer; AColor: TBGRAPixel);
    property Size: Single read FSize write SetSize;
    property IsGradient: boolean read GetIsGradient write SetIsGradient;
    property SideCount: integer read GetSideCount write SetSideCount;
    property FileName: string read GetFileName write SetFileName;
    property Stream64: string read GetStream64 write SetStream64;
    property SourceImage: TBGRABitmap read GetSourceImage;
    property BrushImage: TBGRABitmap read GetBrushImage;
    property AsString: string read GetAsString write SetAsString;
  end;

implementation

uses ULoadImage, Math, base64, Dialogs;

{ TLazPaintBrush }

function TLazPaintBrush.MakeBrushBitmap: TBGRABitmap;
var source: TBGRABitmap;
  sourceSize, resultSize: integer;
  brushSides: integer;
  brushGrad: boolean;
  pts: ArrayOfTPointF;
  i: Integer;
  tmp: TBGRABitmap;
  ASize: single;
  orig: TPointF;
begin
  ASize := Size;
  resultSize := ceil(ASize);
  if resultSize < 0 then resultSize:= 0;
  if not odd(resultSize) then resultSize += 1;
  result:= TBGRABitmap.Create(resultSize,resultSize,BGRAWhite);
  source := SourceImage;
  brushGrad := IsGradient;
  brushSides := SideCount;

  if source <> nil then
  begin
    if source.Width > source.Height then sourceSize := source.Width else sourceSize := source.Height;
  end
    else sourceSize := 0;

  if sourceSize <> 0 then
  begin
    if sourceSize > 2*ASize then
    begin
      tmp := source.Resample(round(source.Width/sourceSize*ASize),round(source.Height/sourceSize*ASize)) as TBGRABitmap;
      orig := PointF((result.Width-tmp.Width)/2,(result.Height-tmp.Height)/2);
      result.PutImageAffine(orig, orig+PointF(tmp.Width,0),orig+PointF(0,tmp.Height), tmp);
      tmp.Free;
    end else
    begin
      orig := PointF((result.Width-source.Width/sourceSize*ASize)/2,(result.Height-source.Height/sourceSize*ASize)/2);
      result.PutImageAffine(orig, orig+PointF(source.Width/sourceSize*ASize,0),orig+PointF(0,source.Height/sourceSize*ASize), source);
    end;
  end else
  begin
    if brushGrad then
    begin
      if brushSides <= 2 then
      begin
        result.GradientFill(0,0,result.width,result.height, BGRABlack,BGRAPixelTransparent, gtRadial, PointF((result.Width-1)/2,(result.Height-1)/2),
          PointF((result.Width-1)/2+(ASize+0.4)/2,(result.Height-1)/2),dmDrawWithTransparency);
      end else
      begin
        tmp := TBGRABitmap.Create(result.width,result.height);
        result.Fill(BGRABlack);
        for i := 0 to brushSides-1 do
        begin
          tmp.GradientFill(0,0,result.width,result.height, BGRABlack,BGRAWhite, gtLinear, PointF((result.Width-1)/2,(result.Height-1)/2),
            PointF((result.Width-1)/2+(ASize+0.4)/2*(sin(i*2*Pi/brushSides)+sin((i+1)*2*Pi/brushSides))/2,
            (result.Height-1)/2-(ASize+0.4)/2*(cos(i*2*Pi/brushSides)+cos((i+1)*2*Pi/brushSides))/2),dmDrawWithTransparency);
          result.BlendImage(0,0,tmp,boLighten);
        end;
        tmp.Free;
      end;

    end else
    begin
      if brushSides <= 2 then
      begin
        result.FillEllipseAntialias((result.Width-1)/2,(result.Height-1)/2,ASize/2,ASize/2,BGRABlack);
      end else
      begin
        pts := nil;
        setlength(pts, brushSides);
        for i := 0 to high(pts) do
          pts[i] := PointF((result.Width-1)/2+sin(i*2*Pi/brushSides)*ASize/2,
            (result.Height-1)/2-cos(i*2*Pi/brushSides)*ASize/2);
        result.FillPolyAntialias(pts,BGRABlack);
      end;
    end;
  end;

  result.ConvertToLinearRGB;
  result.LinearNegative;
end;

function TLazPaintBrush.GetFileName: string;
begin
  result := FVariable.Strings['FileName'];
end;

function TLazPaintBrush.GetIsGradient: boolean;
begin
  result := FVariable.Booleans['IsGradient'];
end;

function TLazPaintBrush.GetSideCount: integer;
begin
  result := FVariable.Integers['SideCount'];
end;

function TLazPaintBrush.GetSourceImage: TBGRABitmap;
var
  string64: TStringStream;
  decode64: TBase64DecodingStream;
  temp: TMemoryStream;
begin
  if not FSourceImageFetched then
  begin
    FSourceImage := nil;
    try
      if FileName <> '' then
        FSourceImage := LoadFlatImageUTF8(FileName, 0).bmp
      else
      if Stream64<> '' then
      begin
        string64 := TStringStream.Create(Stream64);
        temp := TMemoryStream.Create;
        decode64 := TBase64DecodingStream.Create(string64);
        try
          temp.CopyFrom(decode64, decode64.Size);
          temp.Position := 0;
          FSourceImage := TBGRABitmap.Create(temp);
        finally
          decode64.Free;
          temp.Free;
          string64.Free;
        end;
      end;
    except
      on ex:exception do
        ShowMessage(ex.Message);
    end;
    FSourceImageFetched := true;
  end;
  result := FSourceImage;
end;

procedure TLazPaintBrush.SetFileName(AValue: string);
begin
  if AValue = FileName then exit;
  if AValue <> '' then Stream64 := '';
  FVariable.Strings['FileName'] := AValue;
  InvalidateSource;
end;

procedure TLazPaintBrush.SetSideCount(AValue: integer);
begin
  if AValue <= 2 then AValue := 0;
  if AValue = SideCount then exit;
  FVariable.Integers['SideCount'] := AValue;
  InvalidateBrush;
end;

procedure TLazPaintBrush.Init;
begin
  FVariable := TVariableSet.Create('');
  IsGradient:= false;
  SideCount:= 0;
  Size := 10;
  FileName := '';
  Stream64 := '';
  FSourceImage := nil;
  FSourceImageFetched := false;
end;

function TLazPaintBrush.GetBrushImage: TBGRABitmap;
begin
  if FBrushImage = nil then
    FBrushImage := MakeBrushBitmap;
  result := FBrushImage;
end;

function TLazPaintBrush.GetStream64: string;
begin
  result := FVariable.Strings['Stream64'];
end;

procedure TLazPaintBrush.SetStream64(AValue: string);
begin
  if AValue = Stream64 then exit;
  if AValue <> '' then FileName := '';
  FVariable.Strings['Stream64'] := AValue;
  InvalidateSource;
end;

function TLazPaintBrush.GetAsString: string;
begin
  result := FVariable.VariablesAsString;
end;

procedure TLazPaintBrush.SetAsString(AValue: string);
var temp: TVariableSet;
begin
  temp := TVariableSet.Create('',AValue);
  temp.CopyValuesTo(FVariable);
  temp.Free;
end;

procedure TLazPaintBrush.SetIsGradient(AValue: boolean);
begin
  if IsGradient=AValue then Exit;
  FVariable.Booleans['IsGradient'] := AValue;
  InvalidateBrush;
end;

procedure TLazPaintBrush.SetSize(AValue: Single);
begin
  if AValue < 1 then AValue := 1;
  if FSize=AValue then Exit;
  FSize:=AValue;
  InvalidateBrush;
end;

procedure TLazPaintBrush.InvalidateBrush;
begin
  FreeAndNil(FBrushImage);
end;

procedure TLazPaintBrush.InvalidateSource;
begin
  FreeAndNil(FSourceImage);
  FSourceImageFetched := false;
  InvalidateBrush;
end;

constructor TLazPaintBrush.Create;
begin
  Init;
end;

constructor TLazPaintBrush.Create(ASideCount: integer; AIsGradient: boolean);
begin
  Init;
  SideCount:= ASideCount;
  IsGradient := AIsGradient;
end;

constructor TLazPaintBrush.CreateFromStream64(AStream64: string);
begin
  Init;
  Stream64 := AStream64;
end;

destructor TLazPaintBrush.Destroy;
begin
  FreeAndNil(FBrushImage);
  FreeAndNil(FSourceImage);
  FreeAndNil(FVariable);
  inherited Destroy;
end;

procedure TLazPaintBrush.AssignBrushImage(ABitmap: TBGRABitmap);
var
  temp: TMemoryStream;
  encode64: TBase64EncodingStream;
  str: TStringStream;
  reduced,filtered,opaque: TBGRABitmap;
begin
  if (ABitmap.Width > 999) or (ABitmap.Height > 999) then
  begin
    reduced := ABitmap.Resample(Min(ABitmap.Width,999),Min(ABitmap.Height,999)) as TBGRABitmap;
    try
      AssignBrushImage(reduced);
    finally
      reduced.free;
    end;
    exit;
  end;
  str := TStringStream.Create('');
  filtered := ABitmap.FilterGrayscale as TBGRABitmap;
  if filtered.HasTransparentPixels then
  begin
    opaque := TBGRABitmap.Create(filtered.Width,filtered.Height,BGRAWhite);
    opaque.PutImage(0,0,filtered,dmDrawWithTransparency);
    filtered.Free;
  end else
    opaque := filtered;
  try
    encode64 := TBase64EncodingStream.Create(str);
    temp := TMemoryStream.Create;
    try
      opaque.SaveToStreamAs(temp,ifLazPaint);
      temp.Position := 0;
      encode64.CopyFrom(temp,temp.Size);
      Stream64:= str.DataString;
    finally
      temp.Free;
      encode64.Free;
    end;
  finally
    str.Free;
    opaque.free;
  end;
end;

function TLazPaintBrush.MakeColoredBrushImage(AColor: TBGRAPixel): TBGRABitmap;
begin
  result := TBGRABitmap.Create(BrushImage.Width,BrushImage.Height);
  result.FillMask(0,0,BrushImage,AColor);
end;

procedure TLazPaintBrush.Put(ADest: TBGRABitmap; x, y: integer;
  AColor: TBGRAPixel);
var
  img: TBGRABitmap;
begin
  img := MakeColoredBrushImage(AColor);
  ADest.PutImage(x-img.width div 2,y-img.height div 2,img,dmDrawWithTransparency);
  img.Free;
end;

end.

