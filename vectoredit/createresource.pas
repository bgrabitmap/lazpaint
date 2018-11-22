program createresource;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  BGRABitmap, BGRALazPaint, BGRABitmapTypes,
  BGRALazResource, BGRAMultiFileType,
  LazFileUtils, LazUTF8Classes, SysUtils,
  LCVectorOriginal, LCVectorShapes, LCVectorPolyShapes;

{$R *.res}

const
  imgWidth = 24;
  imgHeight = 24;

var
  path: String;
  search: TSearchRec;
  res: TMultiFileContainer;
  fs: TFileStreamUTF8;
  lzp: TBGRALazPaintImage;
  bigImg: TBGRABitmap;
  i, idxEntry: Integer;
  mem: TMemoryStreamUTF8;
  combineList: TStringListUTF8;
begin
  path := StringReplace('../lazpaint/buttons/vector/','/',PathDelim,[rfReplaceAll]);
  if FindFirstUTF8(path+'*.lzp', faAnyFile, search)=0 then
  begin
    res := TLazResourceContainer.Create;
    writeln('Adding files to resource...');
    repeat
      fs := TFileStreamUTF8.Create(path+search.name, fmOpenRead);
      res.Add(EntryFilename(search.name), fs, false, false);
      fs.Free;
      writeln(search.Name);
    until FindNextUTF8(search)<>0;
    FindClose(search);

    combineList:= TStringListUTF8.Create;
    if FileExists('vectorimages.lst') then
      combineList.LoadFromFile('vectorimages.lst')
    else
    begin
      for i := 0 to res.Count-1 do
        combineList.Add(res.Entry[i].Name+'.'+res.Entry[i].Extension);
      combineList.SaveToFile('vectorimages.lst');
    end;
    for i := combineList.Count-1 downto 0 do
      if combineList[i]='' then combineList.Delete(i);
    res.RawStringByFilename['vectorimages.lst'] := combineList.CommaText;

    res.SaveToFile('vectorimages.lrs');
    writeln('Done Resource');
    lzp := TBGRALazPaintImage.Create;
    bigImg := TBGRABitmap.Create(imgWidth, imgHeight*combineList.Count);
    for i := 0 to combineList.Count-1 do
    begin
      idxEntry := res.IndexOf(EntryFilename(combineList[i]));
      if idxEntry = -1 then
      begin
        writeln('Cannot find "'+combineList[i]+'"');
        continue;
      end;
      mem := TMemoryStreamUTF8.Create;
      res.Entry[idxEntry].CopyTo(mem);
      mem.Position:= 0;
      lzp.LoadFromStream(mem);
      mem.Free;

      lzp.Resample(imgWidth,imgHeight,rmFineResample);
      lzp.Draw(bigImg,0,i*imgHeight);
    end;
    combineList.Free;
    res.Free;

    bigImg.SaveToFileUTF8('vectorimages'+inttostr(imgHeight)+'.png');
    writeln('Done PNG');
    bigImg.Free;
  end;
end.

