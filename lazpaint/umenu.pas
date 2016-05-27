unit UMenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ActnList, Forms, Menus, UTool, LCLType, ExtCtrls, UConfig;

type

  { TMainFormMenu }

  TMainFormMenu = class
  private
    FActionList: TActionList;
    FMainMenus: array of TMenuItem;
    FToolsShortcuts: array[TPaintToolType] of TUTF8Char;
    FToolbars: array of TPanel;
    FToolbarsHeight : integer;
    FToolbarBackground: TPanel;
  protected
    procedure AddMenus(AMenu: TMenuItem; AActionList: TActionList; AActionsCommaText: string; AIndex: integer = -1); overload;
    procedure AddMenus(AMenuName: string; AActionsCommaText: string); overload;
    procedure ApplyShortcuts;
    procedure ActionShortcut(AName: string; AShortcut: TUTF8Char);
  public
    constructor Create(AActionList: TActionList);
    procedure PredefinedMainMenus(const AMainMenus: array of TMenuItem);
    procedure Toolbars(const AToolbars: array of TPanel; AToolbarBackground: TPanel);
    procedure CycleTool(var ATool: TPaintToolType; var AShortCut: TUTF8Char);
    procedure Apply;
    procedure ArrangeToolbars(ClientWidth: integer);
    procedure RepaintToolbar;
    property ToolbarsHeight: integer read FToolbarsHeight;
  end;

implementation

uses UResourceStrings, LCLProc, LazPaintType, UScaleDPI, ComCtrls, Graphics,
  Spin, StdCtrls, BGRAText, Controls;

{ TMainFormMenu }

procedure TMainFormMenu.AddMenus(AMenu: TMenuItem; AActionList: TActionList;
  AActionsCommaText: string; AIndex: integer);
var actions: TStringList;
  foundAction: TBasicAction;
  item: TMenuItem;
  i,j: NativeInt;
begin
  actions := TStringList.Create;
  actions.CommaText := AActionsCommaText;
  for i := 0 to actions.Count-1 do
    if (actions[i]='*') and (AIndex = -1) then
      AIndex := 0;
  for i := 0 to actions.Count-1 do
  begin
    if actions[i]='*' then
    begin
      AIndex := -1;
      Continue;
    end;
    item := TMenuItem.Create(nil);
    if trim(actions[i]) = '-' then
      item.Caption := cLineCaption
    else
    begin
      foundAction := AActionList.ActionByName(actions[i]);
      if foundAction <> nil then
        item.Action := foundAction
      else
      begin
        for j := 0 to AMenu.Count-1 do
          if UTF8CompareText(AMenu.Items[j].Name,actions[i])=0 then
          begin
            FreeAndNil(item);
            AMenu.Items[j].Visible := true;
            if (AIndex <> -1) and (AIndex < j) then
            begin
              item := AMenu.Items[j];
              AMenu.Remove(item);
              AMenu.Insert(AIndex,item);
              item := nil;
              inc(AIndex);
            end else
            if AIndex = -1 then
            begin
              item := AMenu.Items[j];
              AMenu.Remove(item);
              AMenu.Add(item);
              item := nil;
            end;
            break;
          end;
        if Assigned(item) then item.Caption := trim(actions[i])+'?';
      end;
    end;
    if Assigned(item) then
    begin
      if AIndex = -1 then
        AMenu.Add(item)
      else
      begin
        AMenu.Insert(AIndex,item);
        inc(AIndex);
      end;
    end;
  end;
  actions.Free;
end;

procedure TMainFormMenu.AddMenus(AMenuName: string; AActionsCommaText: string);
var i: NativeInt;
begin
  for i := 0 to MenuDefinitionKeys.count-1 do
    if UTF8CompareText(MenuDefinitionKeys[i],AMenuName)=0 then
    begin
      AActionsCommaText:= MenuDefinitionValues[i];
      if AActionsCommaText = '' then exit;
      break;
    end;
  for i := 0 to high(FMainMenus) do
    if FMainMenus[i].Name = AMenuName then
    begin
      AddMenus(FMainMenus[i], FActionList, AActionsCommaText);
      FMainMenus[i].Visible := true;
    end;
end;

procedure TMainFormMenu.ActionShortcut(AName: string; AShortcut: TUTF8Char);
var foundAction: TBasicAction;
  ShortcutStr: string;
begin
  foundAction := FActionList.ActionByName(AName);
  if foundAction <> nil then
  begin
    ShortcutStr := AShortcut;
    if (length(AName) >= 5) and (copy(AName,1,4) = 'Tool') and
        (AName[5] = upcase(AName[5])) then
      FToolsShortcuts[StrToPaintToolType(copy(AName,5,length(AName)-4))] := AShortcut;
    AppendShortcut(foundAction as TAction, ShortcutStr);
  end;
end;

constructor TMainFormMenu.Create(AActionList: TActionList);
begin
  FActionList := AActionList;
  FToolbarsHeight := 0;
end;

procedure TMainFormMenu.PredefinedMainMenus(const AMainMenus: array of TMenuItem);
var i: NativeInt;
begin
  setlength(FMainMenus, length(AMainMenus));
  for i := 0 to high(AMainMenus) do
    FMainMenus[i] := AMainMenus[i];
end;

procedure TMainFormMenu.Toolbars(const AToolbars: array of TPanel; AToolbarBackground: TPanel);
var i,j: NativeInt;
begin
  setlength(FToolbars, length(AToolbars));
  for i := 0 to high(FToolbars) do
  begin
    FToolbars[i] := AToolbars[i];
    FToolbars[i].Cursor := crArrow;
    for j := 0 to FToolbars[i].ControlCount-1 do
    begin
      FToolbars[i].Controls[j].Cursor := crArrow;
      if (FToolbars[i].Controls[j] is TLabel) then
      begin
        FToolbars[i].Controls[j].Font.Height := FToolbars[i].Controls[j].Height*FontEmHeightSign*11 div 20;
      end;
    end;
  end;
  FToolbarBackground := AToolbarBackground;
end;

procedure TMainFormMenu.CycleTool(var ATool: TPaintToolType;
  var AShortCut: TUTF8Char);
var
  curTool: TPaintToolType;
begin
  AShortCut := UTF8UpperCase(AShortCut);
  curTool := ATool;
  repeat
    if curTool = high(TPaintToolType) then
      curTool := low(TPaintToolType)
    else
      curTool := succ(curTool);

    if FToolsShortcuts[curTool] = AShortCut then
    begin
      ATool := curTool;
      AShortCut:= '';
      exit;
    end;
  until curTool = ATool;
end;

procedure TMainFormMenu.Apply;
const ImageBrowser = {$IFNDEF DARWIN}'FileUseImageBrowser,'{$ENDIF};
var i,j,tbHeight,tbHeightOrig: NativeInt;
begin
  for i := 0 to FActionList.ActionCount-1 do
  with FActionList.Actions[i] as TAction do
    if (Caption = '') and (Hint <> '') then Caption := Hint;

  AddMenus('MenuFile',   'FileNew,FileOpen,LayerFromFile,MenuRecentFiles,FileReload,-,FileSave,FileSaveAsInSameFolder,FileSaveAs,-,FileImport3D,-,FilePrint,-,'+ImageBrowser+'MenuLanguage,MenuIconSize,*');
  AddMenus('MenuEdit',   'EditUndo,EditRedo,-,EditCut,EditCopy,EditPaste,EditPasteAsNew,EditPasteAsNewLayer,EditDeleteSelection,-,EditSelectAll,EditInvertSelection,EditSelectionFit,EditDeselect');
  AddMenus('MenuSelect', 'EditSelection,FileLoadSelection,FileSaveSelectionAs,-,EditSelectAll,EditInvertSelection,EditSelectionFit,EditDeselect,-,ToolSelectRect,ToolSelectEllipse,ToolSelectPoly,ToolSelectSpline,-,ToolMoveSelection,ToolRotateSelection,-,ToolSelectPen,ToolMagicWand');
  AddMenus('MenuView',   'ViewZoomOriginal,ViewZoomIn,ViewZoomOut,ViewZoomFit,-,*');
  AddMenus('MenuImage',  'ImageCrop,ImageCropLayer,ImageFlatten,-,ImageHorizontalFlip,MenuHorizFlipSub,ImageVerticalFlip,MenuVertFlipSub,-,MenuRemoveTransparency,ImageChangeCanvasSize,ImageRepeat,-,ImageResample,ImageSmartZoom3,ImageRotateCW,ImageRotateCCW');
  AddMenus('MenuRemoveTransparency', 'ImageClearAlpha,ImageFillBackground');
  AddMenus('MenuFilter', 'MenuRadialBlur,FilterBlurMotion,FilterBlurCustom,FilterPixelate,-,FilterSharpen,FilterSmooth,FilterNoise,FilterMedian,FilterClearType,FilterClearTypeInverse,FilterFunction,-,FilterContour,FilterEmboss,FilterPhong,-,FilterSphere,FilterTwirl,FilterCylinder');
  AddMenus('MenuRadialBlur',  'FilterBlurBox,FilterBlurFast,FilterBlurRadial,FilterBlurCorona,FilterBlurDisk');
  AddMenus('MenuColors', 'ColorCurves,ColorPosterize,ColorColorize,ColorShiftColors,FilterComplementaryColor,ColorIntensity,-,ColorLightness,FilterNegative,FilterLinearNegative,FilterNormalize,FilterGrayscale');
  AddMenus('MenuTool',   'ToolHand,ToolColorPicker,-,ToolPen,ToolBrush,ToolEraser,-,ToolRect,ToolEllipse,ToolPolygon,ToolSpline,-,ToolFloodFill,ToolGradient,ToolPhong,-,ToolText,ToolDeformation,ToolTextureMapping,ToolClone');
  AddMenus('MenuRender', 'RenderPerlinNoise,RenderCyclicPerlinNoise,-,RenderWater,RenderCustomWater,RenderSnowPrint,RenderWood,RenderWoodVertical,RenderMetalFloor,RenderPlastik,RenderStone,RenderRoundStone,RenderMarble,RenderCamouflage,-,RenderClouds,FilterRain');
  AddMenus('MenuHelp',   'HelpIndex,-,HelpAbout');
  for i := 0 to high(FMainMenus) do
    if FMainMenus[i].Count = 0 then FMainMenus[i].visible := false;

  ApplyShortcuts;

  tbHeightOrig := DoScaleY(26,OriginalDPI);
  tbHeight := tbHeightOrig;
  for i := 0 to high(FToolbars) do
  with FToolbars[i] do
  begin
    Top := 0;
    Left := -Width;
    Color := clBtnFace;
    for j := 0 to ControlCount-1 do
    begin
      if Controls[j] is TToolBar then
        Controls[j].Color := clBtnFace;
      if Controls[j] is TSpinEdit then
      begin
        if Controls[j].Top + Controls[j].Height+4 > tbHeight then
          tbHeight := Controls[j].Top + Controls[j].Height+4;
      end;
    end;
  end;
  for i := 0 to high(FToolbars) do
  with FToolbars[i] do
  begin
    Height := tbHeight;
    for j := 0 to ControlCount-1 do
    begin
      if not (Controls[j] is TSpinEdit) then
        Controls[j].Top := Controls[j].Top + (tbHeight-tbHeightOrig) div 2;
    end;
  end;

end;

procedure TMainFormMenu.ArrangeToolbars(ClientWidth: integer);
var i,curx,cury,maxh: integer; tb: TPanel;
begin
   curx := 0;
   cury := 0;
   maxh := 0;
   for i := 0 to high(FToolbars) do
   begin
     tb := FToolbars[i];
     if tb.Visible then
     begin
       if curx+tb.Width > ClientWidth then
       begin
         curx := 0;
         cury += maxh;
         maxh := 0;
       end;
       tb.Left := curx;
       tb.Top := cury;
       inc(curx, tb.Width);
       if tb.Height > maxh then maxh := tb.Height;
     end else
     begin
       //hide fix for Gtk
       tb.Top := -tb.Height;
     end;
   end;
   if curx <> 0 then FToolbarsHeight := cury+maxh else FToolbarsHeight := cury;
   if FToolbarsHeight = 0 then
   begin
     FToolbarBackground.Visible := false;
   end else
   begin
     FToolbarBackground.Top := 0;
     FToolbarBackground.Left := 0;
     FToolbarBackground.width := ClientWidth;
     FToolbarBackground.Height := FToolbarsHeight;
     FToolbarBackground.Visible := true;
   end;
end;

procedure TMainFormMenu.RepaintToolbar;
var i: NativeInt;
begin
  FToolbarBackground.Invalidate;
  for i := 0 to high(FToolbars) do FToolbars[i].Invalidate;
  FToolbarBackground.Update;
  for i := 0 to high(FToolbars) do FToolbars[i].Update;
end;

procedure TMainFormMenu.ApplyShortcuts;
begin
  ActionShortcut('ToolHand','H');
  ActionShortcut('ToolPen','P');
  ActionShortcut('ToolBrush','B');
  ActionShortcut('ToolColorPicker','I');
  ActionShortcut('ToolEraser','E');
  ActionShortcut('ToolRect','U');
  ActionShortcut('ToolEllipse','U');
  ActionShortcut('ToolPolygon','D');
  ActionShortcut('ToolSpline','D');
  ActionShortcut('ToolFloodfill','G');
  ActionShortcut('ToolGradient','G');
  ActionShortcut('ToolPhong','G');
  ActionShortcut('ToolText','T');
  ActionShortcut('ToolSelectRect','M');
  ActionShortcut('ToolSelectEllipse','M');
  ActionShortcut('ToolSelectPoly','A');
  ActionShortcut('ToolSelectSpline','A');
  ActionShortcut('ToolMoveSelection','V');
  ActionShortcut('ToolRotateSelection','V');
  ActionShortcut('ToolSelectPen','P');
  ActionShortcut('ToolMagicWand','W');
  ActionShortcut('ViewZoomIn','+');
  ActionShortcut('ViewZoomOut','-');
end;

end.

