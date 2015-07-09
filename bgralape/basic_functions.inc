procedure MyGetScanLine{$i lape.func}
begin
  PBGRAPixel(Result^) := target.ScanLine[PInt32(Params^[0])^];
  WillInvalidateBitmap(targetIndex);
end;

procedure MyGetBitmapWidth{$i lape.func}
begin
  Int32(Result^) := target.Width;
end;

procedure MyGetBitmapHeight{$i lape.func}
begin
  Int32(Result^) := target.Height;
end;

procedure MyPtInClipRect{$i lape.func}
begin
  LongBool(Result^) := target.PtInClipRect(PInt32(Params^[0])^,PInt32(Params^[1])^);
end;

procedure MyPtInRectPointFirst{$i lape.func}
type PPoint = ^TPoint;
  PRect = ^TRect;
begin
  LongBool(Result^) := PtInRect(PPoint(Params^[0])^,PRect(Params^[1])^);
end;

procedure MyPtInRectPointLast{$i lape.func}
type PPoint = ^TPoint;
  PRect = ^TRect;
begin
  LongBool(Result^) := PtInRect(PPoint(Params^[1])^,PRect(Params^[0])^);
end;

procedure MyGetClipRect{$i lape.func}
begin
  TRect(Result^) := target.ClipRect;
end;

procedure MySetClipRect{$i lape.proc}
type PRect = ^TRect;
begin
  target.ClipRect := PRect(Params^[0])^;
end;

procedure MySetNoClip{$i lape.proc}
begin
  target.NoClip;
end;

procedure MyBGRA3{$i lape.func}
begin
  TBGRAPixel(Result^) := BGRA(PByte(Params^[0])^,PByte(Params^[1])^,PByte(Params^[2])^);
end;

procedure MyBGRA4{$i lape.func}
begin
  TBGRAPixel(Result^) := BGRA(PByte(Params^[0])^,PByte(Params^[1])^,PByte(Params^[2])^,PByte(Params^[3])^);
end;

procedure MySetPixel{$i lape.proc}
begin
  target.SetPixel(PInt32(Params^[0])^,PInt32(Params^[1])^,PBGRAPixel(Params^[2])^);
end;

procedure MyNormalPixel{$i lape.proc}
begin
  target.DrawPixel(PInt32(Params^[0])^,PInt32(Params^[1])^,PBGRAPixel(Params^[2])^);
end;

procedure MyLinearPixel{$i lape.proc}
begin
  target.FastBlendPixel(PInt32(Params^[0])^,PInt32(Params^[1])^,PBGRAPixel(Params^[2])^);
end;

procedure MyXorPixel{$i lape.proc}
begin
  target.XorPixel(PInt32(Params^[0])^,PInt32(Params^[1])^,PBGRAPixel(Params^[2])^);
end;

procedure MyErasePixel{$i lape.proc}
begin
  target.ErasePixel(PInt32(Params^[0])^,PInt32(Params^[1])^,PByte(Params^[2])^);
end;

procedure MyDrawPixel{$i lape.proc}
begin
  case TDrawMode(PInt32(Params^[3])^) of
  dmSet: target.SetPixel(PInt32(Params^[0])^,PInt32(Params^[1])^,PBGRAPixel(Params^[2])^);
  dmSetExceptTransparent: if PBGRAPixel(Params^[2])^.alpha = 255 then target.SetPixel(PInt32(Params^[0])^,PInt32(Params^[1])^,PBGRAPixel(Params^[2])^);
  dmLinearBlend: target.FastBlendPixel(PInt32(Params^[0])^,PInt32(Params^[1])^,PBGRAPixel(Params^[2])^);
  dmDrawWithTransparency: target.DrawPixel(PInt32(Params^[0])^,PInt32(Params^[1])^,PBGRAPixel(Params^[2])^);
  dmXor: target.XorPixel(PInt32(Params^[0])^,PInt32(Params^[1])^,PBGRAPixel(Params^[2])^);
  end;
end;

procedure MyAlphaPixel{$i lape.proc}
begin
  target.AlphaPixel(PInt32(Params^[0])^,PInt32(Params^[1])^,PByte(Params^[2])^);
end;

procedure MyGetPixel{$i lape.func}
begin
  TBGRAPixel(Result^) := target.GetPixel(PInt32(Params^[0])^,PInt32(Params^[1])^);
end;

procedure MyGetPixelSingle{$i lape.func}
begin
  TBGRAPixel(Result^) := target.GetPixel(PSingle(Params^[0])^,PSingle(Params^[1])^);
end;

procedure MyGetPixelSingleCycleX{$i lape.func}
begin
  TBGRAPixel(Result^) := target.GetPixelCycle(PSingle(Params^[0])^,PSingle(Params^[1])^,rfLinear,true,false);
end;

procedure MyGetPixelSingleCycleY{$i lape.func}
begin
  TBGRAPixel(Result^) := target.GetPixelCycle(PSingle(Params^[0])^,PSingle(Params^[1])^,rfLinear,false,true);
end;

procedure MyGetPixelSingleCycleXY{$i lape.func}
begin
  TBGRAPixel(Result^) := target.GetPixelCycle(PSingle(Params^[0])^,PSingle(Params^[1])^,rfLinear,true,true);
end;

procedure MyFill{$i lape.proc}
begin
  target.Fill(PBGRAPixel(Params^[0])^);
end;

procedure MyAlphaFill{$i lape.proc}
begin
  target.AlphaFill(PByte(Params^[0])^);
end;

procedure MyPutImage{$i lape.proc}
begin
  target.PutImage(PInt32(Params^[0])^,PInt32(Params^[1])^, GetBitmap(PInt32(Params^[2])^), TDrawMode(PInt32(Params^[3])^), PByte(Params^[4])^);
end;

procedure MyNewBitmap{$i lape.func}
var idx: integer;
  bmp: TBGRABitmap;
begin
  bmp := TBGRABitmap.Create(PInt32(Params^[0])^,PInt32(Params^[1])^);
  idx := NewBitmapEntry;
  Int32(result^) := idx;
  bitmaps[idx].Bitmap := bmp;
  bitmaps[idx].Registered := false;
end;

procedure MyNewBitmapFromColor{$i lape.func}
var idx: integer;
  bmp: TBGRABitmap;
begin
  bmp := TBGRABitmap.Create(PInt32(Params^[0])^,PInt32(Params^[1])^,PBGRAPixel(Params^[2])^);
  idx := NewBitmapEntry;
  Int32(result^) := idx;
  bitmaps[idx].Bitmap := bmp;
  bitmaps[idx].Registered := false;
end;

procedure MyNewBitmapFromFile{$i lape.func}
var idx: integer;
  bmp: TBGRABitmap;
begin
  bmp := TBGRABitmap.Create(PlpString(Params^[0])^,true);
  idx := NewBitmapEntry;
  Int32(result^) := idx;
  bitmaps[idx].Bitmap := bmp;
  bitmaps[idx].Registered := false;
end;

procedure MySelectedBitmap{$i lape.func}
begin
  Int32(result^) := targetIndex;
end;

procedure MyFreeBitmap{$i lape.proc}
var idx: integer;
begin
  idx := PInt32(Params^[0])^;
  if idx = targetIndex then
    raise exception.Create('You cannot free the active bitmap');
  FreeBitmap(idx);
end;

procedure MyLockBitmap{$i lape.proc}
var idx: integer;
begin
  idx := PInt32(Params^[0])^;
  if (idx >= 0) and (idx < length(bitmaps)) then
    inc(bitmaps[idx].LockedCount);
end;

procedure MyUnlockBitmap{$i lape.proc}
var idx: integer;
begin
  idx := PInt32(Params^[0])^;
  if (idx >= 0) and (idx < length(bitmaps)) then
  begin
    if bitmaps[idx].LockedCount <= 0 then
      raise exception.Create('Bitmap is not locked');
    dec(bitmaps[idx].LockedCount);
  end;
end;

procedure MySelectBitmap{$i lape.proc}
var idx: integer;
begin
  idx := PInt32(Params^[0])^;
  SetTargetBitmap(idx);
end;

procedure MySetBitmapSize{$i lape.proc}
begin
  if (targetIndex >= 0) and (targetIndex < length(bitmaps)) then
  begin
    if (bitmaps[targetIndex].LockedCount <> 0) then
      raise exception.Create('Bitmap is locked');
    target.SetSize(PInt32(Params^[0])^,PInt32(Params^[1])^);
  end;
end;

procedure MyAssignBitmap{$i lape.proc}
begin
  target.Assign(GetBitmap(PInt32(Params^[0])^));
end;

procedure MyDuplicateBitmap{$i lape.func}
var copy: TBGRABitmap;
  srcIdx,idx: integer;
begin
  srcIdx := PInt32(Params^[0])^;
  copy := GetBitmap(srcIdx).Duplicate as TBGRABitmap;
  idx := NewBitmapEntry;
  bitmaps[idx].Bitmap := copy;
  bitmaps[idx].Invalidated := false;
  bitmaps[idx].Registered := false;
  Int32(result^) := idx;
end;

procedure RegisterBasicFunctions(Compiler: TLapeCompiler);
begin
  Compiler.addGlobalFunc('function GetScanLine(y: Int32) : PBGRAPixel;', @MyGetScanline);
  Compiler.addGlobalFunc('function BitmapWidth : Int32;', @MyGetBitmapWidth);
  Compiler.addGlobalFunc('function BitmapHeight : Int32;', @MyGetBitmapHeight);
  Compiler.addGlobalFunc('procedure FillBitmap(c: TBGRAPixel);', @MyFill);
  Compiler.addGlobalFunc('procedure FillBitmapAlpha(alpha: byte);', @MyAlphaFill);
  Compiler.addGlobalFunc('function BGRA(red,green,blue,alpha: byte): TBGRAPixel;', @MyBGRA4);
  Compiler.addGlobalFunc('function BGRA(red,green,blue: byte): TBGRAPixel; overload;', @MyBGRA3);
  Compiler.addGlobalFunc('function PtInClipRect(x, y: Int32): LongBool;', @MyPtInClipRect);
  Compiler.addGlobalFunc('function GetClipRect: TRect;', @MyGetClipRect);
  Compiler.addGlobalFunc('procedure SetClipRect(ARect: TRect);', @MySetClipRect);
  Compiler.addGlobalFunc('function PtInRect(const APoint: TPoint; const ARect: TRect): LongBool;', @MyPtInRectPointFirst);
  Compiler.addGlobalFunc('function PtInRect(const ARect: TRect; const APoint: TPoint): LongBool; overload;', @MyPtInRectPointLast);
  Compiler.addGlobalFunc('procedure NoClip;', @MySetNoClip);
  Compiler.addGlobalFunc('procedure SetPixel(x,y: Int32; c: TBGRAPixel);', @MySetPixel);
  Compiler.addGlobalFunc('procedure NormalPixel(x,y: Int32; c: TBGRAPixel);', @MyNormalPixel);
  Compiler.addGlobalFunc('procedure LinearPixel(x,y: Int32; c: TBGRAPixel);', @MyLinearPixel);
  Compiler.addGlobalFunc('procedure XorPixel(x,y: Int32; c: TBGRAPixel);', @MyXorPixel);
  Compiler.addGlobalFunc('procedure ErasePixel(x,y: Int32; alpha: byte);', @MyErasePixel);
  Compiler.addGlobalFunc('procedure AlphaPixel(x,y: Int32; alpha: byte);', @MyAlphaPixel);
  Compiler.addGlobalFunc('procedure _DrawPixel(x,y: Int32; c: TBGRAPixel; ADrawMode: Int32);', @MyDrawPixel);
  Compiler.addGlobalFunc('function GetPixel(x,y: Int32): TBGRAPixel;', @MyGetPixel);
  Compiler.addGlobalFunc('function GetPixel(x,y: single): TBGRAPixel; overload;', @MyGetPixelSingle);
  Compiler.addGlobalFunc('function GetPixelCycle(x,y: single): TBGRAPixel; overload;', @MyGetPixelSingleCycleXY);
  Compiler.addGlobalFunc('function GetPixelCycleX(x,y: single): TBGRAPixel; overload;', @MyGetPixelSingleCycleX);
  Compiler.addGlobalFunc('function GetPixelCycleY(x,y: single): TBGRAPixel; overload;', @MyGetPixelSingleCycleY);
  Compiler.addGlobalFunc('function CreateBitmap(width,height: Int32): TBGRABitmap;', @MyNewBitmap);
  Compiler.addGlobalFunc('function CreateBitmap(width,height: Int32; c: TBGRAPixel): TBGRABitmap; overload;', @MyNewBitmapFromColor);
  Compiler.addGlobalFunc('function CreateBitmap(filename: string): TBGRABitmap; overload;', @MyNewBitmapFromFile);
  Compiler.addGlobalFunc('function SelectedBitmap: TBGRABitmap;', @MySelectedBitmap);
  Compiler.addGlobalFunc('procedure TBGRABitmap.Free;', @MyFreeBitmap);
  Compiler.addGlobalFunc('procedure TBGRABitmap.Select;', @MySelectBitmap);
  Compiler.addGlobalFunc('procedure _PutImage(x,y: Int32; bmp: TBGRABitmap; ADrawMode: Int32; alpha: byte);', @MyPutImage);
  Compiler.addGlobalFunc('procedure TBGRABitmap._Lock;', @MyLockBitmap);
  Compiler.addGlobalFunc('procedure TBGRABitmap._Unlock;', @MyUnlockBitmap);
  Compiler.addGlobalFunc('procedure AssignBitmap(bmp: TBGRABitmap);', @MyAssignBitmap);
  Compiler.addGlobalFunc('procedure SetBitmapSize(width,height: integer);', @MySetBitmapSize);
  Compiler.addGlobalFunc('function TBGRABitmap.Duplicate: TBGRABitmap;', @MyDuplicateBitmap);
end;
