unit UCommandline;

{$mode objfpc}{$H+}

interface

uses classes, LazpaintType, uresourcestrings;

procedure ProcessCommands(instance: TLazPaintCustomInstance; commandsUTF8: TStringList; out errorEncountered, fileSaved: boolean);
function ParamStrUTF8(AIndex: integer): string;

implementation

uses
  SysUtils, FileUtil, LCLProc, BGRABitmap, BGRABitmapTypes, Dialogs, uparse,
  UImage, UImageAction, ULayerAction;

function ParamStrUTF8(AIndex: integer): string;
begin
  result := SysToUTF8(ParamStr(AIndex)); //not perfect
end;

procedure InternalProcessCommands(instance: TLazPaintCustomInstance; commandsUTF8: TStringList; out errorEncountered, fileSaved: boolean; AImageActions: TImageActions);
var
  commandPrefix: set of char;
  InputFilename:string;
  OutputFilename:string;

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

begin
  fileSaved := True;
  errorEncountered := false;
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
      if not instance.TryOpenFileUTF8(ExpandFileNameUTF8(InputFilename)) then
      begin
        instance.ShowError(rsOpen, rsUnableToLoadFile+InputFilename);
        errorEncountered := true;
        exit;
      end;
    end;
  end;

  fileSaved := false;
  for i := iStart to commandsUTF8.count-1 do
  begin
    CommandStr := commandsUTF8[i];
    if (length(CommandStr) >= 1) and (CommandStr[1] in commandPrefix) then
    begin
      Delete(CommandStr,1,1);
      Filter := StrToPictureFilter(CommandStr);
      if Filter <> pfNone then
      begin
        if not instance.ExecuteFilter(Filter,True) then
        begin
          instance.ShowError(CommandStr, rsUnableToApplyFilter+CommandStr);
          errorEncountered := true;
          exit;
        end;
      end else
      begin
        LowerCmd := UTF8LowerCase(CommandStr);
        if LowerCmd='horizontalflip' then AImageActions.HorizontalFlip(foAuto) else
        if LowerCmd='verticalflip' then AImageActions.VerticalFlip(foAuto) else
        if LowerCmd='swapredblue' then instance.Image.SwapRedBlue else
        if LowerCmd='smartzoom3' then AImageActions.SmartZoom3 else
        if LowerCmd='rotatecw' then AImageActions.RotateCW else
        if LowerCmd='rotateccw' then AImageActions.RotateCCW else
        if copy(lowerCmd,1,9)='gradient(' then
        begin
          //c1, c2: TBGRAPixel; gtype: TGradientType; o1, o2: TPointF;
          funcParams := SimpleParseFuncParam(CommandStr);
          if length(funcParams)<>13 then
          begin
            instance.ShowError('Gradient','"Gradient" '+StringReplace(rsExpectNParameters,'N','13',[])+'red1,green1,blue1,alpha1,red2,green2,blue2,alpha2,type,x1,y1,x2,y2');
            errorEncountered := true;
            exit;
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
          layerAction := TLayerAction.Create(instance.Image);
          layerAction.DrawingLayer.GradientFill(0,0,
            instance.Image.Width,instance.Image.Height,
            c1,c2,gt,o1,o2,dmDrawWithTransparency,True,False);
          layerAction.Validate;
          FreeAndNil(layerAction);
        end else
        if copy(lowerCmd,1,8)='opacity(' then
        begin
          funcParams := SimpleParseFuncParam(CommandStr);
          if length(funcParams)<>1 then
          begin
            instance.ShowError('Opacity','"Opacity" ' + rsExpect1Parameter+CommandStr);
            errorEncountered := true;
            exit;
          end;
          val(funcParams[0],opacity,errPos);
          if (errPos <> 0) then
          begin
            instance.ShowError('Opacity',rsInvalidOpacity+CommandStr);
            errorEncountered := true;
            exit;
          end;
          layerAction := TLayerAction.Create(instance.Image);
          layerAction.DrawingLayer.ApplyGlobalOpacity(opacity);
          layerAction.Validate;
          FreeAndNil(layerAction);
        end else
        if copy(lowerCmd,1,9)='resample(' then
        begin
          funcParams := SimpleParseFuncParam(CommandStr);
          if length(funcParams)<>2 then
          begin
            instance.ShowError('Resample','"Resample" ' + rsExpect2Parameters+CommandStr);
            errorEncountered := true;
            exit;
          end;
          val(funcParams[0],w,errPos);
          val(funcParams[1],h,errPos);
          if (errPos <> 0) or (w <= 0) or (h <= 0) then
          begin
            instance.ShowError('Resample',rsInvalidResampleSize+CommandStr);
            errorEncountered := true;
            exit;
          end;
          instance.Image.Resample(w,h,rfHalfCosine);
        end else
        if copy(lowerCmd,1,4)='new(' then
        begin
          funcParams := SimpleParseFuncParam(CommandStr);
          if length(funcParams)<>2 then
          begin
            instance.ShowError('New','"New" ' + rsExpect2Parameters+CommandStr);
            errorEncountered := true;
            exit;
          end;
          val(funcParams[0],w,errPos);
          val(funcParams[1],h,errPos);
          if (errPos <> 0) or (w <= 0) or (h <= 0) then
          begin
            instance.ShowError('New',rsInvalidSizeForNew+CommandStr);
            errorEncountered := true;
            exit;
          end;
          instance.Image.Assign(instance.MakeNewBitmapReplacement(w,h),True,False);
        end else
        if Copy(CommandStr,1,4) <> 'psn_' then //ignore mac parameter
        begin
          instance.ShowError('Command line', rsUnknownCommand+CommandStr);
          errorEncountered := true;
          exit;
        end;
      end;
    end else
    begin
      OutputFilename := CommandStr;
      try
         instance.Image.SaveToFileUTF8(OutputFilename)
      except
        on ex: Exception do
        begin
          instance.ShowError(rsSave, rsUnableToSaveFile+OutputFilename);
        end;
      end;
      fileSaved:= true;
      exit;
    end;
  end;

end;

procedure ProcessCommands(instance: TLazPaintCustomInstance; commandsUTF8: TStringList; out errorEncountered, fileSaved: boolean);
var imageActions: TImageActions;
begin
  imageActions := TImageActions.Create(instance);
  InternalProcessCommands(instance,commandsUTF8,errorEncountered,fileSaved,imageActions);
  imageActions.Free;
end;

end.

