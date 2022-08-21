// SPDX-License-Identifier: GPL-3.0-only
unit UCommandline;

{$mode objfpc}{$H+}

interface

uses classes, LazpaintType, uresourcestrings, LCLStrConsts;

{$IFDEF WINDOWS}
  {$DEFINE SHOW_MANUAL_IN_WINDOW}
{$ENDIF}

const Manual: array[0..79] of string = (
'NAME',
'       LazPaint - Image editor',
'',
'SYNOPSIS',
'       lazpaint [INPUT FILE] [OUTPUT FILE]',
'       lazpaint [INPUT FILE] [ACTION]... [OUTPUT FILE]',
'',
'DESCRIPTION',
'       Graphics viewer and editor.',
'',
'       Can  read  layered  files (lzp, ora, pdn, oXo), multi-images (gif, ico,',
'       tiff), flat files (bmp, jpeg,  pcx,  png,  tga,  xpm,  xwd),  vectorial',
'       (svg),  3D  (obj). Has drawing tools, phong shading, curve adjustments,',
'       filters and render some textures.',
'',
'OPTIONS',
'       If supplied, the INPUT FILE is loaded. If the OUTPUT FILE is  supplied,',
'       the image is saved and the program ends. Otherwise, the GUI of the pro‐',
'       gram is displayed.',
'',
'       -scriptbasedir DIRECTORY',
'              set the directory where Python scripts for LazPaint are located.',
'',
'       -script FILENAME',
'              runs the specified Python script. It must have  a  ".py"  exten‐',
'              sion.',
'',
'       -editor default|CONFIGFILE|OPTION1,OPTION2...',
'              shows the image editor with validate and cancel buttons.  If the',
'              validate button is used,  the rest of the commands are executed.',
'              Otherwise the program stops.',
'',
'              Examples:',
'              -editor default',
'              -editor /Users/me/lazpaintCustom.cfg',
'              -editor [Window]ColorWindowVisible=0,LayerWindowVisible=0',
'',
'       -quit',
'              quits the program even if no output file was  provided.  Can  be',
'              useful when only running scripts.',
'',
'       -new WIDTH,HEIGHT',
'              creates an empty image of size WIDTH x HEIGHT.',
'',
'       -screenshot SCREEN|X1,Y1,X2,Y2',
'              takes a screenshot of the screen of index SCREEN  (0 for primary',
'              monitor) or of specified coordinates.',
'',
'       -resample WIDTH,HEIGHT',
'              resamples the image to the size WIDTH x HEIGHT.',
'',
'       -opacity ALPHA',
'              applies the opacity to the image. ALPHA is between 0 and 255.',
'',
'       -gradient R1,G1,B1,A1,R2,G2,B2,A2,TYPE,X1,Y1,X2,Y2',
'              renders  a gradient from point X1,Y1 to point X2,Y2. TYPE can be',
'              linear, reflected, diamond,  radial  or  angular.  The  starting',
'              color is (R1,G1,B1,A1) and final color is (R2,G2,B2,A2).',
'',
'       -horizontalflip',
'              flips selection or image horizontally.',
'',
'       -verticalflip',
'              flips selection or image vertically.',
'',
'       -swapredblue',
'              swap red and blue channels.',
'',
'       -smartzoom3',
'              resample  the  image 3 times bigger with smart detection of bor‐',
'              ders.',
'',
'       -rotatecw',
'              rotates the image clockwise.',
'',
'       -rotateccw',
'              rotates the image counter-clockwise.',
'',
'       -rotate180',
'              rotates the image 180 degrees.');

procedure ProcessCommands(instance: TLazPaintCustomInstance; commandsUTF8: TStringList; out errorEncountered, fileSaved, quitQuery: boolean);
function ParamStrUTF8(AIndex: integer): string;

implementation

uses
  SysUtils, BGRAUTF8, LazFileUtils, BGRABitmap, BGRABitmapTypes, BGRALayers, Dialogs, uparse,
  UImage, UImageAction, ULayerAction, UScripting, UPython, Forms, Controls,
  UFileSystem, BGRAIconCursor, UGraph
  {$IFDEF SHOW_MANUAL_IN_WINDOW},StdCtrls{$ENDIF};

function ParamStrUTF8(AIndex: integer): string;
begin
  result := SysToUTF8(ParamStr(AIndex)); //not perfect
end;

procedure InternalProcessCommands(instance: TLazPaintCustomInstance; commandsUTF8: TStringList;
  out errorEncountered, fileSaved, quitQuery: boolean; AImageActions: TImageActions);
var
  commandPrefix: set of char;
  InputFilename:string;

  i,iStart: integer;
  errPos: integer; //number conversion

  //functions
  CommandStr,LowerCmd:string;
  funcParams: ArrayOfString;
  Filter: TPictureFilter;

  //resample
  w,h: integer;

  //opacity
  opacity: byte;

  //gradient
  c1,c2: TBGRAPixel;
  gt: TGradientType;
  o1,o2: TPointF;
  layerAction: TLayerAction;
  enableScript: Boolean;

  function DoGradient: boolean;
  begin
    //c1, c2: TBGRAPixel; gtype: TGradientType; o1, o2: TPointF;
    funcParams := SimpleParseFuncParam(CommandStr);
    if length(funcParams)<>13 then
    begin
      instance.ShowError('Gradient','"Gradient" '+StringReplace(rsExpectNParameters,'N','13',[])+'red1,green1,blue1,alpha1,red2,green2,blue2,alpha2,type,x1,y1,x2,y2');
      errorEncountered := true;
      exit(false);
    end;
    val(funcParams[0],c1.red,errPos);
    val(funcParams[1],c1.green,errPos);
    val(funcParams[2],c1.blue,errPos);
    val(funcParams[3],c1.alpha,errPos);
    val(funcParams[4],c2.red,errPos);
    val(funcParams[5],c2.green,errPos);
    val(funcParams[6],c2.blue,errPos);
    val(funcParams[7],c2.alpha,errPos);
    gt := StrToGradientType(funcParams[8]);
    val(funcParams[9],o1.x,errPos);
    val(funcParams[10],o1.y,errPos);
    val(funcParams[11],o2.x,errPos);
    val(funcParams[12],o2.y,errPos);
    layerAction := instance.Image.CreateAction(true);
    layerAction.DrawingLayer.GradientFill(0,0,
      instance.Image.Width,instance.Image.Height,
      c1,c2,gt,o1,o2,dmDrawWithTransparency,True,False);
    layerAction.Validate;
    FreeAndNil(layerAction);
    result := true;
  end;

  function DoOpacity: boolean;
  begin
    funcParams := SimpleParseFuncParam(CommandStr);
    if length(funcParams)<>1 then
    begin
      instance.ShowError('Opacity','"Opacity" ' + rsExpect1Parameter+CommandStr);
      errorEncountered := true;
      exit(false);
    end;
    val(funcParams[0],opacity,errPos);
    if (errPos <> 0) then
    begin
      instance.ShowError('Opacity',rsInvalidOpacity+CommandStr);
      errorEncountered := true;
      exit(false);
    end;
    layerAction := instance.Image.CreateAction(true);
    layerAction.DrawingLayer.ApplyGlobalOpacity(opacity);
    layerAction.Validate;
    FreeAndNil(layerAction);
    result := true;
  end;

  function DoResample: boolean;
  begin
    funcParams := SimpleParseFuncParam(CommandStr);
    if length(funcParams)<>2 then
    begin
      instance.ShowError('Resample','"Resample" ' + rsExpect2Parameters+CommandStr);
      errorEncountered := true;
      exit(false);
    end;
    val(funcParams[0],w,errPos);
    val(funcParams[1],h,errPos);
    if (errPos <> 0) or (w <= 0) or (h <= 0) then
    begin
      instance.ShowError('Resample',rsInvalidResampleSize+CommandStr);
      errorEncountered := true;
      exit(false);
    end;
    instance.Image.Resample(w,h,rfHalfCosine);
    result := true;
  end;

  function DoNew: boolean;
  begin
    funcParams := SimpleParseFuncParam(CommandStr);
    if length(funcParams)<>2 then
    begin
      instance.ShowError('New','"New" ' + rsExpect2Parameters+CommandStr);
      errorEncountered := true;
      exit(false);
    end;
    val(funcParams[0],w,errPos);
    val(funcParams[1],h,errPos);
    if (errPos <> 0) or (w <= 0) or (h <= 0) then
    begin
      instance.ShowError('New',rsInvalidSizeForNew+CommandStr);
      errorEncountered := true;
      exit(false);
    end;
    instance.Image.Assign(instance.MakeNewBitmapReplacement(w,h,BGRAPixelTransparent),True,False);
    result := true;
  end;

  function DoScreenshot: boolean;
  var r: TRect;
    screenIndex,errPos: integer;
    invalid: boolean;
  begin
    funcParams := SimpleParseFuncParam(CommandStr);
    if length(funcParams)=1 then
    begin
      val(funcParams[0],screenIndex,errPos);
      if errPos <> 0 then
      begin
        instance.ShowError('Screenshot', '"Screenshot" ' + rsExpect1Parameter+CommandStr);
        errorEncountered := true;
        exit(false);
      end;
      if (screenIndex < 0) or (screenIndex >= Screen.MonitorCount) then
      begin
        instance.ShowError('Screenshot', '"Screenshot" ' +
                                         lclstrconsts.rsListIndexExceedsBounds.Replace('%d', inttostr(screenIndex)));
        errorEncountered := true;
        exit(false);
      end;
      r := Screen.Monitors[screenIndex].BoundsRect;
      r := rect(r.Left*CanvasScale, r.Top*CanvasScale,
                r.Right*CanvasScale, r.Bottom*CanvasScale);
    end else
    if length(funcParams)=4 then
    begin
      r := rect(0,0,0,0);
      invalid := false;
      val(funcParams[0],r.Left,errPos);
      if errPos <> 0 then invalid := true;
      val(funcParams[1],r.Top,errPos);
      if errPos <> 0 then invalid := true;
      val(funcParams[2],r.Right,errPos);
      if errPos <> 0 then invalid := true;
      val(funcParams[3],r.Bottom,errPos);
      if errPos <> 0 then invalid := true;
      if invalid then
      begin
        instance.ShowError('Screenshot', '"Screenshot" ' + StringReplace(rsExpectNParameters,'N','4',[])+CommandStr);
        errorEncountered := true;
        exit(false);
      end;
      if (errPos <> 0) or (r.Width <= 0) or (r.Height <= 0) then
      begin
        instance.ShowError('New',rsInvalidSizeForNew+IntToStr(r.Width)+'x'+IntToStr(r.Height));
        errorEncountered := true;
        exit(false);
      end;
    end else
    begin
      instance.ShowError('Screenshot', '"Screenshot" ' + StringReplace(rsExpectNParameters,'N','4',[])+CommandStr);
      errorEncountered := true;
      exit(false);
    end;
    AImageActions.TakeScreenshot(r);
    result := true;
  end;

  function MakeConfigFromFuncParam: string;
  var
    cfg: TStringList;
    curSection, newSection, p, param: string;
  begin
    cfg := TStringList.Create;
    curSection := '[General]';
    cfg.Add(curSection);
    try
      for p in funcParams do
      begin
        param := p;
        if param.StartsWith('[') then
        begin
          if param.IndexOf(']') <> -1 then
          begin
            newSection := param.Substring(0, param.IndexOf(']')+1);
            param := param.Substring(length(newSection));
          end else
          begin
            newSection := param;
            param := '';
          end;
          if newSection <> curSection then
          begin
            curSection := newSection;
            cfg.Add(curSection);
          end;
        end;
        if param<>'' then
        begin
          if param.IndexOf('=') = -1 then
            raise Exception.Create(SParExpected.Replace('%s', '"="'));
          cfg.Add(param);
        end;
      end;
    finally
      result := cfg.Text;
      cfg.Free;
    end;
  end;

  function DoEditor: boolean;
  var
    iniStream: TStream;
    bmp: TBGRALayeredBitmap;
  begin
    result := false;
    funcParams := SimpleParseFuncParam(CommandStr);
    if (length(funcParams) = 1) and
       ((ExtractFileExt(funcParams[0])='.ini') or (ExtractFileExt(funcParams[0])='.cfg')) then
      iniStream := FileManager.CreateFileStream(funcParams[0], fmOpenRead)
    else if (length(funcParams) = 1) and (funcParams[0] = 'default') then
      iniStream := TMemoryStream.Create
    else
      iniStream := TStringStream.Create(MakeConfigFromFuncParam);

    bmp := instance.Image.CurrentState.GetLayeredBitmapCopy;
    try
      if instance.EditBitmap(bmp, iniStream) then
      begin
        instance.Image.CurrentState.Assign(bmp, true);
        result := true;
      end
      else
      begin
        bmp.Free;
        quitQuery := true;
      end;
    finally
      FileManager.CancelStreamAndFree(iniStream);
    end;
  end;

  function NextAsFuncParam: boolean;
  begin
    inc(i);
    CommandStr := commandsUTF8[i];
    if (length(CommandStr) >= 1) and (CommandStr[1] in commandPrefix) then
    begin
      instance.ShowError('Command line','Expecting parameters but command found');
      exit(false);
    end;
    result := true;
  end;

  procedure DisplayHelp;
  var
    j: Integer;
    {$IFDEF SHOW_MANUAL_IN_WINDOW}
    f: TForm;
    memo: TMemo;
    {$ENDIF}
  begin
    {$IFDEF SHOW_MANUAL_IN_WINDOW}
    f := TForm.Create(nil);
    try
      f.Caption := rsLazPaint;
      f.Position:= poDesktopCenter;
      f.Width := Screen.Width*3 div 4;
      f.Height := Screen.Height*3 div 4;
      memo := TMemo.Create(f);
      memo.Align:= alClient;
      memo.Parent := f;
      memo.Font.Name:= 'monospace';
      memo.ScrollBars := ssVertical;
      memo.Lines.Clear;
      for j := low(manual) to high(manual) do
        memo.Lines.Add(manual[j]);
      f.ShowModal;
    finally
      f.Free;
    end;
    {$ELSE}
    for j := low(manual) to high(manual) do
      writeln(manual[j]);
    {$ENDIF}
  end;

  procedure DoSaveFile(outputFilename: string);
  var
    icoCur: TBGRAIconCursor;
    stream: TStream;
    ext: String;
  begin
    instance.StartSavingImage(outputFilename);
    try
      ext := ExtractFileExt(outputFilename);
      // normally ICO and CUR cannot be saved directly but make an exception
      if (CompareText(ext, '.ico')=0) or (CompareText(ext, '.cur')=0) then
      begin
        icoCur := TBGRAIconCursor.Create;
        try
          if CompareText(ext, '.cur') = 0 then
            icoCur.FileType := ifCur
            else icoCur.FileType := ifIco;
          icoCur.Add(instance.Image.RenderedImage, BGRABitDepthIconCursor(instance.Image.RenderedImage));
          stream := FileManager.CreateFileStream(outputFilename, fmCreate);
          try
            icoCur.SaveToStream(stream);
          finally
            stream.Free;
          end;
        finally
          icoCur.Free;
        end;
      end else
        instance.Image.SaveToFileUTF8(outputFilename)
    except
      on ex: Exception do
      begin
        instance.ShowError(rsSave, rsUnableToSaveFile+outputFilename);
      end;
    end;
    instance.EndSavingImage;
  end;

begin
  fileSaved := True;
  quitQuery:= false;
  errorEncountered := false;
  enableScript:= false;
  if commandsUTF8.count = 0 then exit;

  commandPrefix := ['-'];
  {$WARNINGS OFF}
  if PathDelim<>'/' then commandPrefix += ['/'];
  {$WARNINGS ON}
  InputFilename:= commandsUTF8[0];
  iStart := 0;
  if InputFilename <> '' then
  begin
    if not (InputFilename[1] in commandPrefix) then
    begin
      iStart := 1;
      if not FileManager.FileExists(ExpandFileNameUTF8(InputFilename)) then
      begin
        instance.ShowError(rsOpen, rsFileNotFound);
        errorEncountered := true;
        exit;
      end else
      if not instance.TryOpenFileUTF8(ExpandFileNameUTF8(InputFilename), true) then
      begin
        instance.ShowError(rsOpen, rsUnableToLoadFile+InputFilename);
        errorEncountered := true;
        exit;
      end;
    end;
  end;

  fileSaved := false;
  i := iStart-1;
  while i < commandsUTF8.count-1 do
  begin
    inc(i);

    CommandStr := commandsUTF8[i];
    if (length(CommandStr) >= 1) and (CommandStr[1] in commandPrefix) then
    begin
      if (commandStr[1] = '-') and (length(commandStr)>=2) and (commandStr[2] = '-') then
        delete(commandStr,1,2)
        else Delete(CommandStr,1,1);
      Filter := StrToPictureFilter(CommandStr);
      if Filter <> pfNone then
      begin
        if instance.ExecuteFilter(Filter,True) <> srOk then
        begin
          instance.ShowError(CommandStr, rsUnableToApplyFilter+CommandStr);
          errorEncountered := true;
          exit;
        end;
      end else
      begin
        LowerCmd := UTF8LowerCase(CommandStr);
        if (LowerCmd='help') or (LowerCmd = 'h') or (LowerCmd = '?') then
        begin DisplayHelp; quitQuery := true; exit; end else
        if LowerCmd='horizontalflip' then AImageActions.HorizontalFlip(foAuto) else
        if LowerCmd='verticalflip' then AImageActions.VerticalFlip(foAuto) else
        if LowerCmd='swapredblue' then instance.Image.SwapRedBlue else
        if LowerCmd='smartzoom3' then AImageActions.SmartZoom3 else
        if LowerCmd='rotatecw' then AImageActions.RotateCW else
        if LowerCmd='rotateccw' then AImageActions.RotateCCW else
        if LowerCmd='rotate180' then AImageActions.Rotate180 else
        if copy(lowerCmd,1,9)='gradient(' then begin if not DoGradient then exit end else
        if lowerCmd = 'gradient' then begin if not NextAsFuncParam or not DoGradient then exit end else
        if copy(lowerCmd,1,8)='opacity(' then begin if not DoOpacity then exit end else
        if lowerCmd = 'opacity' then begin if not NextAsFuncParam or not DoOpacity then exit end else
        if copy(lowerCmd,1,9)='resample(' then begin if not DoResample then exit end else
        if lowerCmd = 'resample' then begin if not NextAsFuncParam or not DoResample then exit end else
        if copy(lowerCmd,1,4)='new(' then begin if not DoNew then exit end else
        if lowerCmd = 'new' then begin if not NextAsFuncParam or not DoNew then exit end else
        if lowerCmd.StartsWith('screenshot(') then begin if not DoScreenShot then exit end else
        if lowerCmd = 'screenshot' then begin if not NextAsFuncParam or not DoScreenShot then exit end else
        if lowerCmd.StartsWith('editor(') then begin if not DoEditor then exit end else
        if lowerCmd = 'editor' then begin if not NextAsFuncParam or not DoEditor then exit end else
        if lowerCmd = 'script' then
        begin
          enableScript := true;
        end else
        if lowerCmd = 'scriptbasedir' then
        begin
          if not NextAsFuncParam then exit;
          CustomScriptDirectory:= ChompPathDelim(ExpandFileNameUTF8(commandStr));
        end else
        if lowerCmd = 'quit' then
        begin
          quitQuery:= true;
          exit;
        end else
        if Copy(CommandStr,1,4) <> 'psn_' then //ignore mac parameter
        begin
          instance.ShowError('Command line', rsUnknownCommand+CommandStr);
          errorEncountered := true;
          exit;
        end;
      end;
    end else
    if enableScript then
    begin
      ForcePathDelims(commandStr);
      if (CompareText(ExtractFileExt(CommandStr), '.py') <> 0) then
      begin
        instance.ShowError('Command line', rsFileExtensionNotSupported + ' ('+ExtractFileExt(commandStr)+')');
        errorEncountered:= true;
        exit;
      end;
      if not FileExistsUTF8(commandStr) and
        (pos(PathDelim, commandStr) = 0) then
        commandStr := TPythonScript.DefaultScriptDirectory + PathDelim + commandStr;
      if not instance.RunScript(commandStr) then exit;
      enableScript := false;
    end else
    begin
      ForcePathDelims(CommandStr);
      DoSaveFile(commandStr);
      fileSaved:= true;
      exit;
    end;
  end;

end;

procedure ProcessCommands(instance: TLazPaintCustomInstance; commandsUTF8: TStringList;
  out errorEncountered, fileSaved, quitQuery: boolean);
begin
  InternalProcessCommands(instance, commandsUTF8, errorEncountered, fileSaved, quitQuery, TImageActions(instance.ImageAction));
end;

end.

