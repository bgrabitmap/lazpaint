// SPDX-License-Identifier: GPL-3.0-only
unit UMenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ActnList, Forms, Menus, UTool, LCLType, ExtCtrls, UConfig,
  Controls, LazPaintType;

type

  { TMainFormMenu }

  TMainFormMenu = class
  private
    FActionList: TActionList;
    FDarkTheme: boolean;
    FMainMenus: array of TMenuItem;
    FToolsShortcuts: array[TPaintToolType] of TUTF8Char;
    FToolbars: array of record
                 tb: TPanel;
                 fixed: boolean;
               end;
    FToolbarsHeight : integer;
    FToolbarBackground: TPanel;
    FImageList: TImageList;
    procedure IconSizeItemClick(Sender: TObject);
    procedure IconSizeMenuClick(Sender: TObject);
    procedure Script_Click(Sender: TObject);
    procedure SetDarkTheme(AValue: boolean);
  protected
    FInstance: TLazPaintCustomInstance;
    FInstalledScripts: TStringList;
    FTargetDPI: integer;
    procedure AddMenus(AMenu: TMenuItem; AActionList: TActionList; AActionsCommaText: string; AIndex: integer = -1); overload;
    procedure AddMenus(AMenuName: string; AActionsCommaText: string); overload;
    procedure AddInstalledScripts(AMenu: TMenuItem; AIndex: integer = -1);
    procedure ApplyShortcuts;
    procedure ActionShortcut(AName: string; AShortcut: TUTF8Char);
    procedure ApplyTheme;
    function GetIndividualToolbarHeight: integer;
  public
    constructor Create(AInstance: TLazPaintCustomInstance; AActionList: TActionList);
    destructor Destroy; override;
    procedure PredefinedMainMenus(const AMainMenus: array of TMenuItem);
    procedure Toolbars(const AToolbars: array of TPanel; AToolbarBackground: TPanel);
    procedure ScaleToolbars(ATargetDPI: integer);
    procedure CycleTool(var ATool: TPaintToolType; var AShortCut: TUTF8Char);
    procedure Apply;
    procedure ArrangeToolbars(ClientWidth: integer);
    procedure RepaintToolbar;
    property ToolbarsHeight: integer read FToolbarsHeight;
    property ImageList: TImageList read FImageList write FImageList;
    property DarkTheme: boolean read FDarkTheme write SetDarkTheme;
  end;

implementation

uses UResourceStrings, BGRAUTF8, LCScaleDPI, ComCtrls, Graphics,
  StdCtrls, BGRAText, math, udarktheme,
  ugraph, BGRABitmapTypes, LCVectorialFillControl,
  UPython, UTranslation;

{ TMainFormMenu }

procedure TMainFormMenu.IconSizeMenuClick(Sender: TObject);
var
  menu: TMenuItem;
  i, iconSize: Integer;
begin
  menu := Sender as TMenuItem;
  iconSize := FInstance.Config.DefaultIconSize(0);
  for i := 0 to menu.Count-1 do
    menu.Items[i].Checked := (menu.Items[i].Tag = iconSize);
end;

procedure TMainFormMenu.Script_Click(Sender: TObject);
var
  item: TMenuItem;
  scriptIndex: integer;
begin
  if Assigned(FInstalledScripts) then
  begin
    item := Sender as TMenuItem;
    scriptIndex := item.Tag;
    FInstance.RunScript(FInstalledScripts[scriptIndex], item.Caption);
  end;
end;

procedure TMainFormMenu.SetDarkTheme(AValue: boolean);
begin
  if FDarkTheme=AValue then Exit;
  FDarkTheme:=AValue;
  ApplyTheme;
end;

procedure TMainFormMenu.IconSizeItemClick(Sender: TObject);
var
  item: TMenuItem;
begin
  item:= Sender as TMenuItem;
  FInstance.ChangeIconSize(item.Tag);
end;

procedure TMainFormMenu.AddMenus(AMenu: TMenuItem; AActionList: TActionList;
  AActionsCommaText: string; AIndex: integer);
var actions: TStringList;
  foundAction: TBasicAction;
  item: TMenuItem;
  i,j: NativeInt;

  procedure AddSubItem(ACaption: string; AOnClick: TNotifyEvent; ATag: integer);
  var
    subItem: TMenuItem;
  begin
    subItem := TMenuItem.Create(item);
    subItem.Caption := ACaption;
    subItem.Tag := ATag;
    subItem.OnClick := AOnClick;
    item.Add(subItem);
  end;

  procedure AddSubItem(AAction: TBasicAction; ATag: integer = 0);
  var
    subItem: TMenuItem;
  begin
    subItem := TMenuItem.Create(item);
    subItem.Action := AAction;
    subItem.Tag := ATag;
    item.Add(subItem);
  end;

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
    if actions[i]='InstalledScripts' then
    begin
      AddInstalledScripts(AMenu, AIndex);
      continue;
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
        if Assigned(item) and (actions[i] = 'MenuIconSize') then
        begin
          item.Caption := rsIconSize;
          item.OnClick:=@IconSizeMenuClick;
          AddSubItem('16px', @IconSizeItemClick, 16);
          AddSubItem('20px', @IconSizeItemClick, 20);
          AddSubItem('24px', @IconSizeItemClick, 24);
          AddSubItem('32px', @IconSizeItemClick, 32);
          AddSubItem('40px', @IconSizeItemClick, 40);
          AddSubItem('48px', @IconSizeItemClick, 48);
          AddSubItem(rsAutodetect, @IconSizeItemClick, 0);
          AMenu.Add(item);
          item := nil;
        end else
        if Assigned(item) and (actions[i] = 'EditShapeAlign') then
        begin
          item.Caption := rsAlignShape;
          AddSubItem(AActionList.ActionByName('EditShapeAlignLeft'));
          AddSubItem(AActionList.ActionByName('EditShapeCenterHorizontally'));
          AddSubItem(AActionList.ActionByName('EditShapeAlignRight'));
          AddSubItem('-',nil,0);
          AddSubItem(AActionList.ActionByName('EditShapeAlignTop'));
          AddSubItem(AActionList.ActionByName('EditShapeCenterVertically'));
          AddSubItem(AActionList.ActionByName('EditShapeAlignBottom'));
          AMenu.Add(item);
          item := nil;
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

procedure TMainFormMenu.AddInstalledScripts(AMenu: TMenuItem; AIndex: integer);

  procedure AddScriptRec(AMenu: TMenuItem; var AIndex: integer; AItem: TMenuItem);
  var
    posSub, j, subIndex: integer;
    sectionName: String;
    sectionItem: TMenuItem;
  begin
    posSub := pos('>', AItem.Caption);
    if posSub > 0 then
    begin
      sectionName := copy(AItem.Caption, 1, posSub-1);
      AItem.Caption := copy(AItem.Caption, posSub+1, length(AItem.Caption) - posSub);
      subIndex := -1;
      for j := 0 to AMenu.Count-1 do
        if AMenu.Items[j].Caption = sectionName then
        begin
          AddScriptRec(AMenu.Items[j], subIndex, AItem);
          exit;
        end;
      sectionItem := TMenuItem.Create(AMenu);
      sectionItem.Caption := sectionName;
      if AIndex = -1 then
        AMenu.Add(sectionItem)
      else
      begin
        AMenu.Insert(AIndex, sectionItem);
        inc(AIndex);
      end;
      AddScriptRec(sectionItem, subIndex, AItem);
      exit;
    end;

    if AIndex = -1 then
      AMenu.Add(AItem)
    else
    begin
      AMenu.Insert(AIndex, AItem);
      inc(AIndex);
    end;
  end;

var
  path, fullname, title: String;
  searchRec: TSearchRec;
  t: textFile;
  item: TMenuItem;
  items: TStringList;
  i: Integer;

begin
  if FInstalledScripts = nil then FInstalledScripts := TStringList.Create;
  path := TPythonScript.DefaultScriptDirectory;
  if FindFirstUTF8(path+PathDelim+'*.py', faAnyFile, searchRec)=0 then
  begin
    items := TStringList.Create;
    items.Sorted := true;
    try
      repeat
        fullname := path+PathDelim+searchRec.Name;
        if FileExistsUTF8(fullname) then
        begin
          title := GetScriptTitle(fullname);
          if title <> '' then
          begin
            item := TMenuItem.Create(AMenu);
            item.Caption := title;
            item.Tag := FInstalledScripts.Add(fullname);
            item.OnClick:=@Script_Click;
            items.AddObject(title, item);
          end;
        end;
      until FindNextUTF8(searchRec)<>0;
    finally
      FindCloseUTF8(searchRec);
      for i := 0 to items.Count-1 do
        AddScriptRec(AMenu, AIndex, TMenuItem(items.Objects[i]));
      items.Free;
    end;
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

procedure TMainFormMenu.ApplyTheme;
var
  i, j: Integer;
begin
  for i := 0 to high(FToolbars) do
  begin
    with FToolbars[i].tb do
    begin
      DarkThemeInstance.Apply(FToolbars[i].tb, DarkTheme);
      for j := 0 to ControlCount-1 do
        if Controls[j] is TLabel then
        begin
          if (Controls[j].Name = 'Label_CurrentZoom') then
          begin
            Controls[j].Color := DarkThemeInstance.GetColorEditableFace(FDarkTheme);
            Controls[j].Font.Color := DarkThemeInstance.GetColorEditableText(FDarkTheme);
          end;
        end;
    end;
  end;
  if Assigned(FToolbarBackground) then
    FToolbarBackground.Color := DarkThemeInstance.GetColorButtonFace(FDarkTheme);
end;

function TMainFormMenu.GetIndividualToolbarHeight: integer;
begin
  result := DoScaleY(24,OriginalDPI,FTargetDPI);
end;

constructor TMainFormMenu.Create(AInstance: TLazPaintCustomInstance; AActionList: TActionList);
begin
  FInstance := AInstance;
  FActionList := AActionList;
  FToolbarsHeight := 0;
  FTargetDPI := OriginalDPI;
end;

destructor TMainFormMenu.Destroy;
begin
  FInstalledScripts.Free;
  inherited Destroy;
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
    FToolbars[i].tb := AToolbars[i];
    FToolbars[i].tb.Cursor := crArrow;
    with FToolbars[i].tb do
    for j := 0 to ControlCount-1 do
    begin
      Controls[j].Cursor := crArrow;
      if Controls[j] is TLabel then
      begin
        if (Controls[j].Name = 'Label_Coordinates') or
           (Controls[j].Name = 'Label_CurrentZoom') or
           (Controls[j].Name = 'Label_CurrentDiff') then
          Controls[j].Font.Height := -DoScaleY(12, OriginalDPI, FTargetDPI);
      end;
    end;
  end;
  FToolbarBackground := AToolbarBackground;
end;

procedure TMainFormMenu.ScaleToolbars(ATargetDPI: integer);
var
  i: Integer;
begin
  FTargetDPI := ATargetDPI;
  for i := 0 to high(FToolbars) do
    ScaleControl(FToolbars[i].tb, OriginalDPI, ATargetDPI, ATargetDPI, true);
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

    if (FToolsShortcuts[curTool] = AShortCut) and not
       ((curTool = ptHotSpot) and not FInstance.Image.IsCursor) then
    begin
      ATool := curTool;
      AShortCut:= '';
      exit;
    end;
  until curTool = ATool;
end;

procedure TMainFormMenu.Apply;
const ImageBrowser = 'FileUseImageBrowser,';
var i,j,tbHeight,tbHeightOrig: NativeInt;
begin
  for i := 0 to FActionList.ActionCount-1 do
  with FActionList.Actions[i] as TAction do
    if (Caption = '') and (Hint <> '') then Caption := Hint;

  AddMenus('MenuFile',   'FileNew,FileOpen,LayerFromFile,FileChooseEntry,FileReload,MenuRecentFiles,-,FileSave,FileSaveAsInSameFolder,FileSaveAs,FileExport,-,FileImport3D,-,FilePrint,-,'+ImageBrowser+'FileRememberSaveFormat,ForgetDialogAnswers,MenuLanguage,*');
  AddMenus('MenuEdit',   'EditUndo,EditRedo,-,EditCut,EditCopy,EditPaste,EditPasteAsNew,EditPasteAsNewLayer,EditDeleteSelection,-,EditMoveUp,EditMoveToFront,EditMoveDown,EditMoveToBack,EditShapeAlign,EditShapeToCurve');
  AddMenus('MenuSelect', 'EditSelection,FileLoadSelection,FileSaveSelectionAs,-,EditSelectAll,EditInvertSelection,EditSelectionFit,EditDeselect,-,ToolSelectRect,ToolSelectEllipse,ToolSelectPoly,ToolSelectSpline,-,ToolMoveSelection,ToolRotateSelection,SelectionHorizontalFlip,SelectionVerticalFlip,-,ToolSelectPen,ToolMagicWand');
  AddMenus('MenuView',   'ViewGrid,ViewZoomOriginal,ViewZoomIn,ViewZoomOut,ViewZoomFit,-,ViewToolBox,ViewColors,ViewPalette,ViewLayerStack,ViewImageList,ViewStatusBar,-,*,-,ViewDarkTheme,ViewWorkspaceColor,MenuIconSize');
  AddMenus('MenuImage',  'ImageCrop,ImageCropLayer,ImageFlatten,MenuRemoveTransparency,-,ImageNegative,ImageLinearNegative,ImageSwapRedBlue,-,ImageChangeCanvasSize,ImageRepeat,-,ImageResample,ImageSmartZoom3,-,ImageRotateCW,ImageRotateCCW,ImageRotate180,ImageHorizontalFlip,ImageVerticalFlip');
  AddMenus('MenuRemoveTransparency', 'ImageClearAlpha,ImageFillBackground');
  AddMenus('MenuFilter', 'MenuRadialBlur,FilterBlurMotion,FilterBlurCustom,FilterPixelate,-,FilterSharpen,FilterSmooth,FilterNoise,FilterMedian,FilterClearType,FilterClearTypeInverse,FilterFunction,-,FilterContour,FilterEmboss,FilterPhong,-,FilterSphere,FilterTwirl,FilterWaveDisplacement,FilterCylinder');
  AddMenus('MenuRadialBlur',  'FilterBlurBox,FilterBlurFast,FilterBlurRadial,FilterBlurCorona,FilterBlurDisk');
  AddMenus('MenuColors', 'ColorCurves,ColorPosterize,ColorColorize,ColorShiftColors,FilterComplementaryColor,ColorIntensity,-,ColorLightness,FilterNegative,FilterLinearNegative,FilterNormalize,FilterGrayscale');
  AddMenus('MenuTool',   'ToolHand,ToolHotSpot,ToolColorPicker,-,ToolPen,ToolBrush,ToolEraser,ToolFloodFill,ToolClone,-,ToolEditShape,ToolRect,ToolEllipse,ToolPolyline,ToolOpenedCurve,ToolPolygon,ToolSpline,ToolGradient,ToolPhong,ToolText,-,ToolDeformation,ToolTextureMapping');
  AddMenus('MenuRender', 'RenderPerlinNoise,RenderCyclicPerlinNoise,-,RenderWater,RenderCustomWater,RenderSnowPrint,RenderWood,RenderWoodVertical,RenderMetalFloor,RenderPlastik,RenderStone,RenderRoundStone,RenderMarble,RenderCamouflage,-,RenderClouds,FilterRain');
  AddMenus('MenuScript', 'FileRunScript,-,InstalledScripts');
  AddMenus('MenuHelp',   'HelpIndex,-,HelpAbout');
  for i := 0 to high(FMainMenus) do
    if FMainMenus[i].Count = 0 then FMainMenus[i].visible := false;

  ApplyShortcuts;

  if Assigned(FImageList) then
    FActionList.Images := FImageList;

  tbHeightOrig := GetIndividualToolbarHeight;
  tbHeight := tbHeightOrig;
  for i := 0 to high(FToolbars) do
  with FToolbars[i].tb do
  begin
    Top := 0;
    Left := -Width;
    Color := clBtnFace;
    for j := 0 to ControlCount-1 do
    begin
      if Controls[j] is TToolBar then
      begin
        if assigned(FImageList) then TToolbar(Controls[j]).Images := FImageList;
        TToolbar(Controls[j]).ButtonWidth := TToolbar(Controls[j]).Images.Width+ScaleX(6, 96);
        TToolbar(Controls[j]).ButtonHeight := TToolbar(Controls[j]).Images.Height+ScaleY(5, 96);
      end else
      if Controls[j] is TLCVectorialFillControl then
      begin
        if assigned(FImageList) then
          TLCVectorialFillControl(Controls[j]).ToolIconSize:= FImageList.Height;
      end;
    end;
  end;
  for i := 0 to high(FToolbars) do
  with FToolbars[i].tb do
  begin
    Height := tbHeight;
    for j := 0 to ControlCount-1 do
      Controls[j].Top := Controls[j].Top + (tbHeight-tbHeightOrig) div 2;
  end;

  ApplyTheme;
end;

procedure TMainFormMenu.ArrangeToolbars(ClientWidth: integer);
var i,j,k,curx,cury,maxh, w, minNextX, delta,
  tbNormalHeight: integer;
  tb: TPanel;
  vfc: TLCVectorialFillControl;
begin
   tbNormalHeight := GetIndividualToolbarHeight;
   curx := 0;
   cury := 0;
   maxh := 0;
   for i := 0 to high(FToolbars) do
   begin
     tb := FToolbars[i].tb;

     if not FToolbars[i].fixed then
     begin
       for j := 0 to tb.ControlCount-1 do
       begin
         tb.Controls[j].Top := DoScaleY(1, OriginalDPI, FTargetDPI);
         if tb.Controls[j] is TLCVectorialFillControl then
         begin
           vfc := TLCVectorialFillControl(tb.Controls[j]);
           if tb.Height < vfc.PreferredSize.cy then
             vfc.Height := min(vfc.ToolIconSize + vfc.VerticalPadding,
                               tb.Height - tb.Controls[j].Top - 1)
           else
             vfc.Height := vfc.PreferredSize.cy;
         end else
         if tb.Controls[j] is TLabel then
           tb.Controls[j].Height := tbNormalHeight - DoScaleY(3, OriginalDPI, FTargetDPI)
         else
           tb.Controls[j].Height := tbNormalHeight - DoScaleY(2, OriginalDPI, FTargetDPI);
         if tb.Controls[j] is TToolBar then
         begin
           minNextX := MaxLongInt;
           for k := 0 to tb.ControlCount-1 do
             if tb.Controls[k].Left > tb.Controls[j].Left then
               minNextX := min(minNextX, tb.Controls[k].Left);
           delta := tb.Controls[j].Left+tb.Controls[j].Width+2-minNextX;
           for k := 0 to tb.ControlCount-1 do
             if tb.Controls[k].Left > tb.Controls[j].Left then
               tb.Controls[k].Left := tb.Controls[k].Left+delta;
         end;
       end;
     end;

     w := DoScaleX(4, OriginalDPI);
     for j := 0 to tb.ControlCount-1 do
       if tb.Controls[j].Visible then
         w := max(w, tb.Controls[j].Left + tb.Controls[j].Width);
     w += DoScaleX(4, OriginalDPI);
     tb.Width := w;

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
       maxh := min(maxh, tbNormalHeight);
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
     FToolbarBackground.Anchors:= [akLeft,akTop,akRight];
     FToolbarBackground.Visible := true;
   end;
end;

procedure TMainFormMenu.RepaintToolbar;
var i: NativeInt;
begin
  FToolbarBackground.Invalidate;
  for i := 0 to high(FToolbars) do FToolbars[i].tb.Invalidate;
  FToolbarBackground.Update;
  for i := 0 to high(FToolbars) do FToolbars[i].tb.Update;
end;

procedure TMainFormMenu.ApplyShortcuts;
begin
  ActionShortcut('ToolHand','H');
  ActionShortcut('ToolHotSpot','H');
  ActionShortcut('ToolPen','P');
  ActionShortcut('ToolBrush','B');
  ActionShortcut('ToolColorPicker','C');
  ActionShortcut('ToolEraser','E');
  ActionShortcut('ToolEditShape','J');
  ActionShortcut('ToolRect','U');
  ActionShortcut('ToolEllipse','U');
  ActionShortcut('ToolPolyline','L');
  ActionShortcut('ToolOpenedCurve','N');
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
  ActionShortcut('ToolSelectPen','O');
  ActionShortcut('ToolMagicWand','W');
  ActionShortcut('ViewZoomIn','+');
  ActionShortcut('ViewZoomOut','-');
end;

end.

