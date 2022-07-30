// SPDX-License-Identifier: GPL-3.0-only
unit LCVectorialFillInterface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types,
  Controls, ComCtrls, Menus, Dialogs, ExtDlgs, ExtCtrls,
  BGRAImageList, BCTrackbarUpdown,
  BGRABitmap, BGRABitmapTypes, LCVectorialFill, LCVectorOriginal,
  BGRAGradientScanner, Graphics, BGRAGraphics;

function GradRepetitionToStr(AValue: TBGRAGradientRepetition): string;
function ColorInterpToStr(AValue: TBGRAColorInterpolation): string;
function TextureRepetitionToStr(AValue: TTextureRepetition): string;

type
  TLCFillTarget = (ftPen, ftBack, ftOutline);
  TChooseColorEvent = procedure(ASender: TObject; AButton: TMouseButton; AColorIndex: integer;
    var AColorValue: TBGRAPixel; out AHandled: boolean) of object;

  { TVectorialFillInterface }

  TVectorialFillInterface = class(TComponent)
  private
    FCanEditGradTexPoints: boolean;
    FIsTarget: boolean;
    FOnMouseDown: TMouseEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    procedure EditGradTextPointsClick(Sender: TObject);
    function GetEditingGradTexPoints: boolean;
    procedure Preview_MouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, {%H-}Y: Integer);
    procedure SetCanEditGradTexPoints(AValue: boolean);
    procedure SetEditingGradTexPoints(AValue: boolean);
    procedure SetIsTarget(AValue: boolean);
    procedure SetVerticalPadding(AValue: integer);
    procedure ToolbarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ToolbarMouseEnter(Sender: TObject);
    procedure ToolbarMouseLeave(Sender: TObject);
    procedure ToolbarMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ToolbarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure AnyButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure AnyButtonMouseEnter(Sender: TObject);
    procedure AnyButtonMouseLeave(Sender: TObject);
    procedure AnyButtonMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure AnyButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  protected
    FFillType: TVectorialFillType;
    FAllowedFillTypes: TVectorialFillTypes;
    FSolidColor: TBGRAPixel;
    FOnChooseColor: TChooseColorEvent;

    FGradStartColor, FGradEndColor: TBGRAPixel;
    FGradType: TGradientType;
    FGradRepetition: TBGRAGradientRepetition;
    FGradInterp: TBGRAColorInterpolation;

    FTexRepetition: TTextureRepetition;
    FTexture: TBGRABitmap;
    FTexOpacity: byte;
    FTextureAverageColor: TBGRAPixel;
    FTextureAverageColorComputed: boolean;

    //interface
    FContainer: TWinControl;
    FVerticalPadding: integer;

    FPreview: TImage;
    FButtonFillNone, FButtonFillSolid,
    FButtonFillGradient, FButtonFillTexture: TToolButton;
    FOnFillChange, FOnFillTypeChange, FOnOpacityChange: TNotifyEvent;
    FButtonEditGradTexPoints, FButtonAdjustToShape: TToolButton;
    FOnEditGradTexPoints, FOnAdjustToShape: TNotifyEvent;

    FSolidColorInterfaceCreated: boolean;
    FShapeSolidColor: TShape;
    FUpDownSolidAlpha: TBCTrackbarUpdown;
    FSolidColorChange: TNotifyEvent;

    FTextureInterfaceCreated: boolean;
    FCanAdjustToShape: boolean;
    FButtonTexRepeat, FButtonLoadTexture: TToolButton;
    FUpDownTexAlpha: TBCTrackbarUpdown;
    FOnTextureClick: TNotifyEvent;
    FOnTextureChange: TNotifyEvent;

    FGradientInterfaceCreated: boolean;
    //FShapeStartColor, FShapeEndColor: TShape;
    FUpDownStartAlpha, FUpDownEndAlpha: TBCTrackbarUpdown;
    FButtonSwapColor, FButtonGradRepetition, FButtonGradInterp: TToolButton;
    FGradTypeMenu, FGradRepetitionMenu, FGradInterpMenu: TPopupMenu;

    FColorDlg: TColorDialog;
    FOpenPictureDlg: TOpenPictureDialog;
    FTexRepetitionMenu: TPopupMenu;

    FToolbar: TToolBar;
    FImageList: TBGRAImageList;
    FImageListLoaded: boolean;
    FImageListSize: TSize;

    procedure AdjustToShapeClick(Sender: TObject);
    procedure ButtonFillChange(Sender: TObject);
    procedure ButtonFillGradClick(Sender: TObject);
    procedure ButtonFillTexClick(Sender: TObject);
    procedure ButtonGradInterpClick(Sender: TObject);
    procedure ButtonGradRepetitionClick(Sender: TObject);
    procedure ButtonLoadTextureClick(Sender: TObject);
    procedure ButtonSwapColorClick(Sender: TObject);
    procedure ButtonTexRepeatClick(Sender: TObject);
    procedure Changed(AUpdatePreview: boolean = True);
    procedure OnClickBackGradType(ASender: TObject);
    procedure OnClickBackTexRepeat(ASender: TObject);
    procedure OnClickGradInterp(ASender: TObject);
    procedure OnClickGradRepeat(ASender: TObject);
    function GetPreferredSize: TSize;
    function GetAverageColor: TBGRAPixel;
    procedure SetCanAdjustToShape(AValue: boolean);
    procedure SetContainer(AValue: TWinControl);
    procedure SetFillType(AValue: TVectorialFillType);
    procedure SetAllowedFillTypes(AValue: TVectorialFillTypes);
    procedure SetSolidColor(AValue: TBGRAPixel);
    procedure SetGradientType(AValue: TGradientType);
    procedure SetGradEndColor(AValue: TBGRAPixel);
    procedure SetGradStartColor(AValue: TBGRAPixel);
    procedure SetGradRepetition(AValue: TBGRAGradientRepetition);
    procedure SetGradInterpolation(AValue: TBGRAColorInterpolation);
    procedure SetImageListSize(AValue: TSize);
    procedure SetTexture(AValue: TBGRABitmap);
    procedure SetTextureRepetition(AValue: TTextureRepetition);
    procedure SetTextureOpacity(AValue: byte);
    procedure SetOnTextureClick(AValue: TNotifyEvent);
//    procedure ShapeEndColorMouseUp({%H-}Sender: TObject; {%H-}Button: TMouseButton;
//      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure ShapeSolidColorMouseUp({%H-}Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
//    procedure ShapeStartColorMouseUp({%H-}Sender: TObject; {%H-}Button: TMouseButton;
//      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure UpdateAccordingToFillType;
    procedure UpdateTopToolbar;
    procedure UpdatePreview;
    procedure UpdateShapeSolidColor;
    procedure UpdateTextureParams;
    procedure UpdateGradientParams;
    procedure UpdateButtonAdjustToShape;
    procedure UpDownEndAlphaChange(Sender: TObject; AByUser: boolean);
    procedure UpDownSolidAlphaChange(Sender: TObject; AByUser: boolean);
    procedure UpDownStartAlphaChange(Sender: TObject; AByUser: boolean);
    procedure UpDownTexAlphaChange(Sender: TObject; AByUser: boolean);
    procedure ChooseColor(AColorIndex: integer; AButton: TMouseButton);
    procedure CreateSolidColorInterface;
    procedure CreateGradientInterface;
    procedure CreateTextureInterface;
    procedure HideSolidColorInterface;
    procedure HideGradientInterface;
    procedure HideTextureInterface;
    procedure Init(AImageListWidth,AImageListHeight: Integer);
    procedure AttachMouseEvent(AControl: TToolBar); overload;
    procedure AttachMouseEvent(AControl: TToolButton); overload;
    procedure AttachMouseEvent(AControl: TBCTrackbarUpdown); overload;
    procedure AttachMouseEvent(AControl: TImage); overload;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; AImageListWidth,AImageListHeight: Integer);
    destructor Destroy; override;
    procedure LoadTexture;
    procedure LoadImageList;
    procedure ContainerSizeChanged;
    function GetTextureThumbnail(AWidth, AHeight: integer; ABackColor: TColor): TBitmap;
    procedure AssignFill(AFill: TVectorialFill);
    procedure UpdateFillExceptGeometry(ATargetFill: TVectorialFill);
    function CreateShapeFill(AShape: TVectorShape): TVectorialFill;
    procedure UpdateShapeFill(AShape: TVectorShape; ATarget: TLCFillTarget);
    property FillType: TVectorialFillType read FFillType write SetFillType;
    property IsTarget: boolean read FIsTarget write SetIsTarget;
    property SolidColor: TBGRAPixel read FSolidColor write SetSolidColor;
    property AverageColor: TBGRAPixel read GetAverageColor;
    property GradientType: TGradientType read FGradType write SetGradientType;
    property GradStartColor: TBGRAPixel read FGradStartColor write SetGradStartColor;
    property GradEndColor: TBGRAPixel read FGradEndColor write SetGradEndColor;
    property GradRepetition: TBGRAGradientRepetition read FGradRepetition write SetGradRepetition;
    property GradInterpolation: TBGRAColorInterpolation read FGradInterp write SetGradInterpolation;
    property Texture: TBGRABitmap read FTexture write SetTexture;
    property TextureRepetition: TTextureRepetition read FTexRepetition write SetTextureRepetition;
    property TextureOpacity: byte read FTexOpacity write SetTextureOpacity;
    property CanAdjustToShape: boolean read FCanAdjustToShape write SetCanAdjustToShape;
    property CanEditGradTexPoints: boolean read FCanEditGradTexPoints write SetCanEditGradTexPoints;
    property EditingGradTexPoints: boolean read GetEditingGradTexPoints write SetEditingGradTexPoints;
    property OnFillChange: TNotifyEvent read FOnFillChange write FOnFillChange;
    property OnTextureChange: TNotifyEvent read FOnTextureChange write FOnTextureChange;
    property OnTextureClick: TNotifyEvent read FOnTextureClick write SetOnTextureClick;
    property OnAdjustToShape: TNotifyEvent read FOnAdjustToShape write FOnAdjustToShape;
    property OnEditGradTexPoints: TNotifyEvent read FOnEditGradTexPoints write FOnEditGradTexPoints;
    property OnFillTypeChange: TNotifyEvent read FOnFillTypeChange write FOnFillTypeChange;
    property OnOpacityChange: TNotifyEvent read FOnOpacityChange write FOnOpacityChange;
    property OnChooseColor: TChooseColorEvent read FOnChooseColor write FOnChooseColor;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property Container: TWinControl read FContainer write SetContainer;
    property ImageListSize: TSize read FImageListSize write SetImageListSize;
    property VerticalPadding: integer read FVerticalPadding write SetVerticalPadding;
    property PreferredSize: TSize read GetPreferredSize;
    property AllowedFillTypes: TVectorialFillTypes read FAllowedFillTypes write SetAllowedFillTypes;
  end;

implementation

uses LCToolbars, BGRAThumbnail, LResources,
  LCVectorShapes, BGRAGradientOriginal, BGRATransform, math,
  LCResourceString;

function GradRepetitionToStr(AValue: TBGRAGradientRepetition): string;
begin
  case AValue of
    grPad: result := rsGrPad;
    grRepeat: result := rsGrRepeat;
    grReflect: result := rsGrReflect;
    grSine: result := rsGrSine;
    else result := '';
  end;
end;

function ColorInterpToStr(AValue: TBGRAColorInterpolation): string;
begin
  case AValue of
    ciStdRGB: result := rsCiStdRGB;
    ciLinearRGB: result := rsCiLinearRGB;
    ciLinearHSLPositive: result := rsCiLinearHSLPositive;
    ciLinearHSLNegative: result := rsCiLinearHSLNegative;
    ciGSBPositive: result := rsCiGSBPositive;
    ciGSBNegative: result := rsCiGSBNegative;
    else result := '';
  end;
end;

function TextureRepetitionToStr(AValue: TTextureRepetition): string;
begin
  case AValue of
    trNone: result := rsTrNone;
    trRepeatX: result := rsTrRepeatX;
    trRepeatY: result := rsTrRepeatY;
    trRepeatBoth: result := rsTrRepeatBoth;
    else result := '';
  end;
end;

{ TVectorialFillInterface }

procedure TVectorialFillInterface.LoadImageList;
var
  i: Integer;
  lst: TStringList;
begin
  if FImageList = nil then FImageList := TBGRAImageList.Create(self);
  if FImageListLoaded and (FImageList.Width=FImageListSize.cx) and (FImageList.Height=FImageListSize.cy) then exit;
  FImageList.Clear;
  FImageList.Width := FImageListSize.cx;
  FImageList.Height := FImageListSize.cy;
  {$IFDEF DARWIN}
  FImageList.Scaled := true;
  FImageList.RegisterResolutions([FImageListSize.cx, FImageListSize.cx*2]);
  {$ENDIF}

  lst := TStringList.Create;
  lst.CommaText := GetResourceString('fillimages.lst');
  for i := 0 to lst.Count-1 do
    LoadToolbarImage(FImageList, i, lst[i]);
  lst.Free;

  FImageListLoaded := true;
  if Assigned(FToolbar) then
  begin
    SetToolbarImages(FToolbar, FImageList, 5, VerticalPadding);
    for i := 0 to FToolbar.ControlCount-1 do
      if FToolbar.Controls[i] is TBCTrackbarUpdown then
        FToolbar.Controls[i].Width := FToolbar.ButtonWidth*2
      else if FToolbar.Controls[i] is TShape then
        FToolbar.Controls[i].Width := FToolbar.ButtonWidth;
  end;

  UpdatePreview;
end;

procedure TVectorialFillInterface.Changed(AUpdatePreview: boolean);
begin
  if AUpdatePreview then UpdatePreview;
  if Assigned(FOnFillChange) then
    FOnFillChange(self);
end;

procedure TVectorialFillInterface.OnClickBackGradType(ASender: TObject);
begin
  GradientType:= TGradientType((ASender as TMenuItem).Tag);
  FillType := vftGradient;
end;

procedure TVectorialFillInterface.OnClickBackTexRepeat(ASender: TObject);
begin
  TextureRepetition := TTextureRepetition((ASender as TMenuItem).Tag);
end;

procedure TVectorialFillInterface.OnClickGradInterp(ASender: TObject);
begin
  GradInterpolation:= TBGRAColorInterpolation((ASender as TMenuItem).Tag);
end;

procedure TVectorialFillInterface.OnClickGradRepeat(ASender: TObject);
begin
  GradRepetition:= TBGRAGradientRepetition((ASender as TMenuItem).Tag);
end;

procedure TVectorialFillInterface.SetTexture(AValue: TBGRABitmap);
begin
  if FTexture=AValue then Exit;

  if Assigned(FTexture) then
  begin
    FTexture.FreeReference;
    FTexture := nil;
  end;
  if Assigned(AValue) then
    FTexture := AValue.NewReference as TBGRABitmap;

  FTextureAverageColorComputed := false;
  if Assigned(FOnTextureChange) then FOnTextureChange(self);
  if FFillType = vftTexture then Changed;
end;

procedure TVectorialFillInterface.LoadTexture;
var
  newTex: TBGRABitmap;
begin
  if FOpenPictureDlg.Execute then
  begin
    try
      newTex := TBGRABitmap.Create(FOpenPictureDlg.FileName, true);
      Texture := newTex;
      newTex.FreeReference;
      FillType:= vftTexture;
    except
      on ex: exception do
        ShowMessage(ex.Message);
    end;
  end;
end;

procedure TVectorialFillInterface.ContainerSizeChanged;
begin
  FToolbar.Align:= alTop;
  FToolbar.Height := FContainer.Height;
end;

procedure TVectorialFillInterface.SetFillType(AValue: TVectorialFillType);
begin
  if FFillType=AValue then Exit;
  FFillType:=AValue;
  UpdateAccordingToFillType;
  UpdatePreview;
  if Assigned(FOnFillTypeChange) then FOnFillTypeChange(self);
  Changed(False);
end;

procedure TVectorialFillInterface.ShapeSolidColorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ChooseColor(-1, Button);
end;

{procedure TVectorialFillInterface.ShapeStartColorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ChooseColor(0, Button);
end;}

procedure TVectorialFillInterface.UpdateAccordingToFillType;
begin
  FButtonFillNone.Down := FillType = vftNone;
  FButtonFillSolid.Down := FillType = vftSolid;
  FButtonFillGradient.Down := FillType = vftGradient;
  FButtonFillTexture.Down := FillType = vftTexture;
  UpdateButtonAdjustToShape;

  if FillType <> vftSolid then HideSolidColorInterface;
  if FillType <> vftGradient then HideGradientInterface;
  if FillType <> vftTexture then HideTextureInterface;

  case FillType of
    vftSolid: begin
      CreateSolidColorInterface;
      UpdateShapeSolidColor;
      ShowAppendToolButtons([FShapeSolidColor,FUpDownSolidAlpha]);
    end;
    vftGradient: begin
      CreateGradientInterface;
      UpdateGradientParams;
      ShowAppendToolButtons([FButtonGradRepetition,FButtonGradInterp,
                           {FShapeStartColor,}FUpDownStartAlpha,FButtonSwapColor,
                           {FShapeEndColor,}FUpDownEndAlpha]);
    end;
    vftTexture: begin
      CreateTextureInterface;
      UpdateTextureParams;
      ShowAppendToolButtons([FButtonTexRepeat,FUpDownTexAlpha,FButtonLoadTexture]);
    end;
  end;
end;

procedure TVectorialFillInterface.UpdateTopToolbar;
var
  x: Integer;
begin
  FToolbar.BeginUpdate;
  x := FToolbar.Indent;
  FButtonFillNone.Left := x;
  //FButtonFillNone.Wrap := [vftSolid,vftGradient,vftTexture]*FAllowedFillTypes = [];
  FButtonFillNone.Visible:= vftNone in FAllowedFillTypes;
  if vftNone in FAllowedFillTypes then inc(x, FButtonFillNone.Width);
  FButtonFillSolid.Left := x;
  //FButtonFillSolid.Wrap := [vftGradient,vftTexture]*FAllowedFillTypes = [];
  FButtonFillSolid.Visible:= vftSolid in FAllowedFillTypes;
  if vftSolid in FAllowedFillTypes then inc(x, FButtonFillSolid.Width);
  FButtonFillGradient.Left := x;
  //FButtonFillGradient.Wrap := [vftTexture]*FAllowedFillTypes = [];
  FButtonFillGradient.Visible:= vftGradient in FAllowedFillTypes;
  if vftGradient in FAllowedFillTypes then inc(x, FButtonFillGradient.Width);
  FButtonFillTexture.Left := x;
  FButtonFillTexture.Visible:= vftTexture in FAllowedFillTypes;
  if vftTexture in FAllowedFillTypes then inc(x, FButtonFillTexture.Width);

  FPreview.Left := x;
  inc(x, FPreview.Width);

  FButtonEditGradTexPoints.Left := x;
  inc(x, FButtonEditGradTexPoints.Width);

  FButtonAdjustToShape.Left := x;
  FToolbar.EndUpdate;
end;

procedure TVectorialFillInterface.UpdatePreview;
var
  bmp, thumb: TBGRABitmap;
  grad: TBGRALayerGradientOriginal;
  bmpCopy: TBitmap;
  ratio: single;
  previewWidth: Integer;
begin
  if FillType = vftGradient then
    previewWidth := round(FToolbar.ButtonWidth*1.5)
  else previewWidth := FToolbar.ButtonWidth;
  FPreview.Width:= previewWidth + round(FToolbar.ButtonWidth*0.2);
  FPreview.Height:= FToolbar.ButtonHeight;

  if not FImageListLoaded then exit;
  bmp := TBGRABitmap.Create(previewWidth, FPreview.Height - VerticalPadding);
  bmp.DrawCheckers(bmp.ClipRect, CSSWhite, CSSSilver);
  case FillType of
    vftSolid: bmp.Fill(SolidColor, dmDrawWithTransparency);
    vftTexture:
        if Assigned(FTexture) and (FTexture.Width > 0) and (FTexture.Height > 0) then
        begin
          ratio := min(bmp.Width/FTexture.Width, bmp.Height/FTexture.Height);
          if ratio > 1 then ratio := 1;
          thumb := TBGRABitmap.Create(max(round(FTexture.Width*ratio),1),
                                      max(round(FTexture.Height*ratio),1));
          thumb.StretchPutImage(thumb.ClipRect, FTexture, dmSet);
          bmp.Fill(thumb, dmDrawWithTransparency, TextureOpacity*$0101);
          thumb.Free;
        end;
    vftGradient:
      begin
        grad := TBGRALayerGradientOriginal.Create;
        grad.StartColor := GradStartColor;
        grad.EndColor := GradEndColor;
        grad.Origin := PointF(0,0);
        grad.XAxis := PointF(bmp.Width, 0);
        grad.ColorInterpolation:= GradInterpolation;
        grad.Render(bmp, AffineMatrixIdentity, false, dmDrawWithTransparency);
        grad.Free;
      end;
  end;
  if IsTarget then
  begin
    if bmp.GetPixel(bmp.Width/2,bmp.Height/2).Lightness > 20000 then
      bmp.Rectangle(bmp.ClipRect, BGRABlack, dmDrawWithTransparency)
      else bmp.Rectangle(bmp.ClipRect, CSSSilver, dmDrawWithTransparency);
  end
    else bmp.Rectangle(bmp.ClipRect, BGRA(0,0,0,128), dmDrawWithTransparency);
  bmpCopy := bmp.MakeBitmapCopy(clBtnFace);
  bmp.Free;
  FPreview.Picture.Assign(bmpCopy);
  bmpCopy.Free;

  if (FillType = vftTexture) and Assigned(Texture) and Assigned(FOnTextureClick) then
    FPreview.Cursor := crHandPoint
  else
    FPreview.Cursor := crDefault;
end;

procedure TVectorialFillInterface.UpdateShapeSolidColor;
var
  c: TBGRAPixel;
begin
  c := SolidColor;
  c.alpha := 255;
  if Assigned(FShapeSolidColor) then FShapeSolidColor.Brush.Color := c;
  if Assigned(FUpDownSolidAlpha) then FUpDownSolidAlpha.Value := SolidColor.alpha;
end;

procedure TVectorialFillInterface.UpdateTextureParams;
begin
  if Assigned(FButtonTexRepeat) then FButtonTexRepeat.ImageIndex := 17 + ord(TextureRepetition);
  if Assigned(FUpDownTexAlpha) then FUpDownTexAlpha.Value := TextureOpacity;
end;

procedure TVectorialFillInterface.UpdateGradientParams;
{var
  c: TBGRAPixel;}
begin
{  c := GradStartColor;
  c.alpha := 255;
  if Assigned(FShapeStartColor) then FShapeStartColor.Brush.Color := c;}
  if Assigned(FUpDownStartAlpha) then FUpDownStartAlpha.Value := GradStartColor.alpha;
{  c := GradEndColor;
  c.alpha := 255;
  if Assigned(FShapeEndColor) then FShapeEndColor.Brush.Color := c;}
  if Assigned(FUpDownEndAlpha) then FUpDownEndAlpha.Value := GradEndColor.alpha;

  if Assigned(FButtonGradRepetition) then FButtonGradRepetition.ImageIndex := 7+ord(FGradRepetition);
  if Assigned(FButtonGradInterp) then FButtonGradInterp.ImageIndex := 11+ord(FGradInterp);
end;

procedure TVectorialFillInterface.UpdateButtonAdjustToShape;
begin
  if Assigned(FButtonAdjustToShape) then
  begin
    FButtonAdjustToShape.Enabled := FCanAdjustToShape and (FillType in[vftGradient,vftTexture]);
    if FillType in[vftGradient,vftTexture] then
      FButtonAdjustToShape.Style := tbsButton
    else
      FButtonAdjustToShape.Style := tbsDivider;
  end;
  if Assigned(FButtonEditGradTexPoints) then
  begin
    FButtonEditGradTexPoints.Enabled := FCanEditGradTexPoints and (FillType in [vftGradient,vftTexture]);
    if FillType in [vftGradient,vftTexture] then
      FButtonEditGradTexPoints.Style := tbsCheck
    else
      FButtonEditGradTexPoints.Style := tbsDivider;
  end;
end;

procedure TVectorialFillInterface.UpDownEndAlphaChange(Sender: TObject;
  AByUser: boolean);
var
  c: TBGRAPixel;
begin
  if AByUser then
  begin
    c := GradEndColor;
    c.alpha := FUpDownEndAlpha.Value;
    GradEndColor:= c;
    if assigned(FOnOpacityChange) then FOnOpacityChange(self);
  end;
end;

procedure TVectorialFillInterface.UpDownSolidAlphaChange(Sender: TObject;
  AByUser: boolean);
begin
  if AByUser then
  begin
    SolidColor:= ColorToBGRA(FShapeSolidColor.Brush.Color, FUpDownSolidAlpha.Value);
    if assigned(FOnOpacityChange) then FOnOpacityChange(self);
  end;
end;

procedure TVectorialFillInterface.UpDownStartAlphaChange(Sender: TObject;
  AByUser: boolean);
var
  c: TBGRAPixel;
begin
  if AByUser then
  begin
    c := GradStartColor;
    c.alpha := FUpDownStartAlpha.Value;
    GradStartColor:= c;
    if assigned(FOnOpacityChange) then FOnOpacityChange(self);
  end;
end;

procedure TVectorialFillInterface.UpDownTexAlphaChange(Sender: TObject;
  AByUser: boolean);
begin
  if AByUser then
  begin
    FTexOpacity:= FUpDownTexAlpha.Value;
    if FillType = vftTexture then Changed;
  end;
end;

procedure TVectorialFillInterface.ChooseColor(AColorIndex: integer; AButton: TMouseButton);

  procedure AssignNewColor(AColor: TBGRAPixel);
  begin
    case AColorIndex of
      -1: SolidColor := AColor;
      0: GradStartColor := AColor;
      1: GradEndColor := AColor;
    end;
  end;

var
  curColorBGRA: TBGRAPixel;
  curColor: TColor;
  handled: boolean;
begin
  case AColorIndex of
    -1: curColorBGRA := SolidColor;
    0: curColorBGRA := GradStartColor;
    1: curColorBGRA := GradEndColor;
  else exit;
  end;
  if Assigned(FOnChooseColor) then
  begin
    FOnChooseColor(self, AButton, AColorIndex, curColorBGRA, handled);
    if handled then
    begin
      AssignNewColor( curColorBGRA );
      exit;
    end;
  end;
  curColor := RGBToColor(curColorBGRA.red, curColorBGRA.green, curColorBGRA.blue);
  FColorDlg.Color := curColor;
  if FColorDlg.Execute then
  begin
    if curColorBGRA.alpha = 0 then
      AssignNewColor( ColorToBGRA(FColorDlg.Color) )
    else
      AssignNewColor( ColorToBGRA(FColorDlg.Color, curColorBGRA.alpha) );
  end;
end;

procedure TVectorialFillInterface.CreateSolidColorInterface;
begin
  if FSolidColorInterfaceCreated then exit;
  FSolidColorInterfaceCreated := true;

  //solid color interface
  FShapeSolidColor := TShape.Create(FToolbar);
  FShapeSolidColor.Width := FToolbar.ButtonWidth;
  FShapeSolidColor.Height := FToolbar.ButtonHeight;
  FShapeSolidColor.OnMouseUp:= @ShapeSolidColorMouseUp;
  FShapeSolidColor.Hint := rsColor;
  AddToolbarControl(FToolbar, FShapeSolidColor);
  FUpDownSolidAlpha := TBCTrackbarUpdown.Create(FToolbar);
  FUpDownSolidAlpha.Width := FToolbar.ButtonWidth*2;
  FUpDownSolidAlpha.Height := FToolbar.ButtonHeight;
  FUpDownSolidAlpha.MinValue := 0;
  FUpDownSolidAlpha.MaxValue := 255;
  FUpDownSolidAlpha.Increment:= 15;
  FUpDownSolidAlpha.OnChange:=@UpDownSolidAlphaChange;
  FUpDownSolidAlpha.Hint := rsOpacity;
  AddToolbarControl(FToolbar, FUpDownSolidAlpha);
  AttachMouseEvent(FUpDownSolidAlpha);
end;

procedure TVectorialFillInterface.CreateGradientInterface;
var
  gr: TBGRAGradientRepetition;
  ci: TBGRAColorInterpolation;
  item: TMenuItem;
begin
  if FGradientInterfaceCreated then exit;
  FGradientInterfaceCreated := true;

  FButtonGradRepetition := AddToolbarButton(FToolbar, rsGradientRepetition+'...', 7+ord(FGradRepetition), @ButtonGradRepetitionClick);
  AttachMouseEvent(FButtonGradRepetition);
  FButtonGradInterp := AddToolbarButton(FToolbar, rsColorInterpolation+'...', 11+ord(FGradInterp), @ButtonGradInterpClick);
  AttachMouseEvent(FButtonGradInterp);

{  FShapeStartColor := TShape.Create(FToolbar);
  FShapeStartColor.Width := FToolbar.ButtonWidth*3 div 4;
  FShapeStartColor.Height := FToolbar.ButtonHeight;
  FShapeStartColor.OnMouseUp:=@ShapeStartColorMouseUp;
  FShapeStartColor.Hint := 'Start color';
  AddToolbarControl(FToolbar, FShapeStartColor);}
  FUpDownStartAlpha := TBCTrackbarUpdown.Create(FToolbar);
  FUpDownStartAlpha.Width := FToolbar.ButtonWidth*2;
  FUpDownStartAlpha.Height := FToolbar.ButtonHeight;
  FUpDownStartAlpha.MinValue := 0;
  FUpDownStartAlpha.MaxValue := 255;
  FUpDownStartAlpha.Increment:= 15;
  FUpDownStartAlpha.OnChange:=@UpDownStartAlphaChange;
  FUpDownStartAlpha.Hint := rsStartOpacity;
  AddToolbarControl(FToolbar, FUpDownStartAlpha);
  AttachMouseEvent(FUpDownStartAlpha);
  FButtonSwapColor := AddToolbarButton(FToolbar, rsSwapColors, 23, @ButtonSwapColorClick);
  AttachMouseEvent(FButtonSwapColor);
{  FShapeEndColor := TShape.Create(FToolbar);
  FShapeEndColor.Width := FToolbar.ButtonWidth*3 div 4;
  FShapeEndColor.Height := FToolbar.ButtonHeight;
  FShapeEndColor.OnMouseUp:=@ShapeEndColorMouseUp;
  FShapeEndColor.Hint := 'End color';
  AddToolbarControl(FToolbar, FShapeEndColor);}
  FUpDownEndAlpha := TBCTrackbarUpdown.Create(FToolbar);
  FUpDownEndAlpha.Width := FToolbar.ButtonWidth*2;
  FUpDownEndAlpha.Height := FToolbar.ButtonHeight;
  FUpDownEndAlpha.MinValue := 0;
  FUpDownEndAlpha.MaxValue := 255;
  FUpDownEndAlpha.Increment:= 15;
  FUpDownEndAlpha.OnChange:=@UpDownEndAlphaChange;
  FUpDownEndAlpha.Hint := rsEndOpacity;
  AddToolbarControl(FToolbar, FUpDownEndAlpha);
  AttachMouseEvent(FUpDownEndAlpha);

  FGradRepetitionMenu := TPopupMenu.Create(self);
  FGradRepetitionMenu.Images := FImageList;
  for gr := low(TBGRAGradientRepetition) to high(TBGRAGradientRepetition) do
  begin
    item := TMenuItem.Create(FGradRepetitionMenu);  item.Caption := GradRepetitionToStr(gr);
    item.OnClick:=@OnClickGradRepeat;               item.Tag := ord(gr);
    item.ImageIndex:= 7+ord(gr);
    FGradRepetitionMenu.Items.Add(item);
  end;

  FGradInterpMenu := TPopupMenu.Create(self);
  FGradInterpMenu.Images := FImageList;
  for ci := low(TBGRAColorInterpolation) to high(TBGRAColorInterpolation) do
  begin
    item := TMenuItem.Create(FGradInterpMenu);  item.Caption := ColorInterpToStr(ci);
    item.OnClick:=@OnClickGradInterp;           item.Tag := ord(ci);
    item.ImageIndex:= 11+ord(ci);
    FGradInterpMenu.Items.Add(item);
  end;
end;

procedure TVectorialFillInterface.CreateTextureInterface;
var
  tr: TTextureRepetition;
  item: TMenuItem;
begin
  if FTextureInterfaceCreated then exit;
  FTextureInterfaceCreated := true;

  FButtonTexRepeat := AddToolbarButton(FToolbar, rsTextureRepetition+'...', -1, @ButtonTexRepeatClick);
  AttachMouseEvent(FButtonTexRepeat);
  FUpDownTexAlpha := TBCTrackbarUpdown.Create(FToolbar);
  FUpDownTexAlpha.Width := FToolbar.ButtonWidth*2;
  FUpDownTexAlpha.Height := FToolbar.ButtonHeight;
  FUpDownTexAlpha.MinValue := 0;
  FUpDownTexAlpha.MaxValue := 255;
  FUpDownTexAlpha.Increment:= 15;
  FUpDownTexAlpha.OnChange:=@UpDownTexAlphaChange;
  FUpDownTexAlpha.Hint := rsOpacity;
  AddToolbarControl(FToolbar, FUpDownTexAlpha);
  AttachMouseEvent(FUpDownTexAlpha);
  FButtonLoadTexture := AddToolbarButton(FToolbar, rsLoadTexture+'...', 22, @ButtonLoadTextureClick);
  AttachMouseEvent(FButtonLoadTexture);
  FTextureAverageColorComputed := false;

  FTexRepetitionMenu := TPopupMenu.Create(self);
  FTexRepetitionMenu.Images := FImageList;
  for tr := low(TTextureRepetition) to high(TTextureRepetition) do
  begin
    item := TMenuItem.Create(FTexRepetitionMenu);  item.Caption := TextureRepetitionToStr(tr);
    item.OnClick:=@OnClickBackTexRepeat;           item.Tag := ord(tr);
    item.ImageIndex:= 17+ord(tr);
    FTexRepetitionMenu.Items.Add(item);
  end;
end;

procedure TVectorialFillInterface.HideSolidColorInterface;
begin
  if not FSolidColorInterfaceCreated then exit;
  FShapeSolidColor.Visible := false;
  FUpDownSolidAlpha.Visible := false;
end;

procedure TVectorialFillInterface.HideGradientInterface;
begin
  if not FGradientInterfaceCreated then exit;
  FButtonGradRepetition.Visible := false;
  FButtonGradInterp.Visible := false;
  //FShapeStartColor.Visible := false;
  FUpDownStartAlpha.Visible := false;
  FButtonSwapColor.Visible := false;
  //FShapeEndColor.Visible := false;
  FUpDownEndAlpha.Visible := false;
end;

procedure TVectorialFillInterface.HideTextureInterface;
begin
  if not FTextureInterfaceCreated then exit;
  FButtonTexRepeat.Visible := false;
  FUpDownTexAlpha.Visible := false;
  FButtonLoadTexture.Visible := false;
end;

procedure TVectorialFillInterface.Init(AImageListWidth,
  AImageListHeight: Integer);
var
  gt: TGradientType;
  item: TMenuItem;
begin
  FContainer := nil;

  FAllowedFillTypes := [vftNone, vftSolid, vftGradient, vftTexture];
  FFillType:= vftSolid;
  FSolidColor:= BGRAWhite;
  FGradStartColor:= CSSRed;
  FGradEndColor:= CSSYellow;
  FGradType:= gtLinear;
  FGradRepetition:= grPad;
  FGradInterp:= ciLinearRGB;
  FTexture:= nil;
  FTexRepetition:= trRepeatBoth;
  FTexOpacity:= 255;
  FCanAdjustToShape:= true;

  FVerticalPadding:= 4;
  FImageList := TBGRAImageList.Create(self);
  FImageListLoaded:= false;
  FImageListSize := Size(AImageListWidth,AImageListHeight);

  FOpenPictureDlg := TOpenPictureDialog.Create(self);
  FColorDlg:= TColorDialog.Create(self);

  FOnFillChange:= nil;
  FOnTextureChange:= nil;

  FToolbar := CreateToolBar(FImageList);
  FToolbar.Wrapable := false;
  AttachMouseEvent(FToolbar);
  FButtonFillNone := AddToolbarCheckButton(FToolbar, rsNoFill, 0, @ButtonFillChange, False, False);
  AttachMouseEvent(FButtonFillNone);
  FButtonFillSolid := AddToolbarCheckButton(FToolbar, rsSolidColor, 1, @ButtonFillChange, False, False);
  AttachMouseEvent(FButtonFillSolid);
  FButtonFillGradient := AddToolbarButton(FToolbar, rsGradientFill, 2+ord(FGradType), @ButtonFillGradClick);
  AttachMouseEvent(FButtonFillGradient);
  FButtonFillTexture := AddToolbarButton(FToolbar, rsTextureFill, 24, @ButtonFillTexClick);
  AttachMouseEvent(FButtonFillTexture);

  FPreview := TImage.Create(FToolbar);
  FPreview.Center:= true;
  FPreview.OnMouseUp:=@Preview_MouseUp;
  FPreview.Hint := rsPreview;
  UpdatePreview;
  AddToolbarControl(FToolbar, FPreview);
  AttachMouseEvent(FPreview);

  FButtonEditGradTexPoints := AddToolbarCheckButton(FToolbar, rsEditGradTexPoints, 25, @EditGradTextPointsClick, false, false);
  AttachMouseEvent(FButtonEditGradTexPoints);
  FButtonAdjustToShape := AddToolbarButton(FToolbar, rsAdjustToShape, 21, @AdjustToShapeClick);
  AttachMouseEvent(FButtonAdjustToShape);
  FButtonAdjustToShape.Wrap := true;
  UpdateButtonAdjustToShape;

  //menu to access gradient interface
  FGradTypeMenu := TPopupMenu.Create(self);
  FGradTypeMenu.Images := FImageList;
  for gt := low(TGradientType) to high(TGradientType) do
  begin
    item := TMenuItem.Create(FGradTypeMenu);  item.Caption := GradientTypeToTranslatedStr(gt);
    item.OnClick:=@OnClickBackGradType;       item.Tag := ord(gt);
    item.ImageIndex:= 2+ord(gt);
    FGradTypeMenu.Items.Add(item);
  end;

  FSolidColorInterfaceCreated := false;
  FGradientInterfaceCreated:= false;
  FTextureInterfaceCreated:= false;

  UpdateAccordingToFillType;
end;

procedure TVectorialFillInterface.AttachMouseEvent(AControl: TToolBar);
begin
  AControl.OnMouseMove:=@ToolbarMouseMove;
  AControl.OnMouseDown:=@ToolbarMouseDown;
  AControl.OnMouseUp:=@ToolbarMouseUp;
  AControl.OnMouseEnter:=@ToolbarMouseEnter;
  AControl.OnMouseLeave:=@ToolbarMouseLeave;
end;

procedure TVectorialFillInterface.AttachMouseEvent(AControl: TToolButton);
begin
  AControl.OnMouseMove:=@AnyButtonMouseMove;
  AControl.OnMouseDown:=@AnyButtonMouseDown;
  AControl.OnMouseUp:=@AnyButtonMouseUp;
  AControl.OnMouseEnter:=@AnyButtonMouseEnter;
  AControl.OnMouseLeave:=@AnyButtonMouseLeave;
end;

procedure TVectorialFillInterface.AttachMouseEvent(AControl: TBCTrackbarUpdown);
begin
  AControl.OnMouseMove:=@AnyButtonMouseMove;
  AControl.OnMouseDown:=@AnyButtonMouseDown;
  AControl.OnMouseUp:=@AnyButtonMouseUp;
  AControl.OnMouseEnter:=@AnyButtonMouseEnter;
  AControl.OnMouseLeave:=@AnyButtonMouseLeave;
end;

procedure TVectorialFillInterface.AttachMouseEvent(AControl: TImage);
begin
  AControl.OnMouseMove:=@AnyButtonMouseMove;
  AControl.OnMouseEnter:=@AnyButtonMouseEnter;
  AControl.OnMouseLeave:=@AnyButtonMouseLeave;
end;

procedure TVectorialFillInterface.SetSolidColor(AValue: TBGRAPixel);
begin
  if FSolidColor.EqualsExactly(AValue) then Exit;
  FSolidColor:=AValue;
  UpdateShapeSolidColor;
  If FillType = vftSolid then Changed;
end;

procedure TVectorialFillInterface.ButtonFillChange(Sender: TObject);
begin
  if Sender = FButtonFillNone then
  begin
    FillType:= vftNone;
    FButtonFillNone.Down := true;
  end
  else if Sender = FButtonFillSolid then
  begin
    FillType:= vftSolid;
    FButtonFillSolid.Down := true;
  end;
end;

procedure TVectorialFillInterface.SetTextureRepetition(
  AValue: TTextureRepetition);
begin
  if FTexRepetition=AValue then Exit;
  FTexRepetition:=AValue;
  UpdateTextureParams;
  If FillType = vftTexture then Changed;
end;

procedure TVectorialFillInterface.SetTextureOpacity(AValue: byte);
begin
  if FTexOpacity=AValue then Exit;
  FTexOpacity:=AValue;
  FUpDownTexAlpha.Value := AValue;
  If FillType = vftTexture then Changed;
end;

{procedure TVectorialFillInterface.ShapeEndColorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ChooseColor(1, Button);
end;}

procedure TVectorialFillInterface.SetGradientType(AValue: TGradientType);
begin
  if FGradType=AValue then Exit;
  FGradType:=AValue;
  FButtonFillGradient.ImageIndex := 2+ord(GradientType);
  if FillType = vftGradient then Changed;
end;

procedure TVectorialFillInterface.SetGradEndColor(AValue: TBGRAPixel);
begin
  if FGradEndColor.EqualsExactly(AValue) then Exit;
  FGradEndColor:=AValue;
  UpdateGradientParams;
  if FillType = vftGradient then Changed;
end;

procedure TVectorialFillInterface.SetGradStartColor(AValue: TBGRAPixel);
begin
  if FGradStartColor.EqualsExactly(AValue) then Exit;
  FGradStartColor:=AValue;
  UpdateGradientParams;
  if FillType = vftGradient then Changed;
end;

procedure TVectorialFillInterface.SetGradRepetition(AValue: TBGRAGradientRepetition);
begin
  if FGradRepetition=AValue then Exit;
  FGradRepetition:=AValue;
  UpdateGradientParams;
  if FillType = vftGradient then Changed;
end;

procedure TVectorialFillInterface.SetGradInterpolation(
  AValue: TBGRAColorInterpolation);
begin
  if FGradInterp=AValue then Exit;
  FGradInterp:=AValue;
  UpdateGradientParams;
  if FillType = vftGradient then Changed;
end;

procedure TVectorialFillInterface.SetContainer(AValue: TWinControl);
begin
  if FContainer=AValue then Exit;
  if Assigned(FContainer) then FContainer.RemoveControl(FToolbar);
  FContainer:=AValue;
  if Assigned(FContainer) then
  begin
    FContainer.InsertControl(FToolBar);
    ContainerSizeChanged;
  end;
end;

function TVectorialFillInterface.GetPreferredSize: TSize;
begin
  result := GetToolbarSize(FToolbar,0);
end;

procedure TVectorialFillInterface.SetCanAdjustToShape(AValue: boolean);
begin
  if FCanAdjustToShape=AValue then Exit;
  FCanAdjustToShape:=AValue;
  UpdateButtonAdjustToShape;
end;

procedure TVectorialFillInterface.SetImageListSize(AValue: TSize);
begin
  if (FImageListSize.cx=AValue.cx) and (FImageListSize.cy=AValue.cy) then Exit;
  FImageListSize:=AValue;
  if FImageListLoaded then LoadImageList;
end;

procedure TVectorialFillInterface.SetAllowedFillTypes(
  AValue: TVectorialFillTypes);
begin
  Include(AValue, FFillType); //cannot exclude current type
  if FAllowedFillTypes=AValue then Exit;
  FAllowedFillTypes:=AValue;
  UpdateTopToolbar;
end;

procedure TVectorialFillInterface.SetOnTextureClick(AValue: TNotifyEvent);
begin
  if FOnTextureClick=AValue then Exit;
  FOnTextureClick:=AValue;
  UpdatePreview;
end;

function TVectorialFillInterface.GetAverageColor: TBGRAPixel;
begin
  case FillType of
  vftNone: result := BGRAPixelTransparent;
  vftGradient: result := MergeBGRAWithGammaCorrection(GradStartColor, 1, GradEndColor, 1);
  vftTexture: begin
      if not FTextureAverageColorComputed then
      begin
        if Assigned(FTexture) then
          FTextureAverageColor := FTexture.AverageColor
        else
          FTextureAverageColor := BGRAPixelTransparent;
        FTextureAverageColorComputed := true;
      end;
      result := FTextureAverageColor;
    end
  else {vftSolid} result := SolidColor;
  end;
end;

procedure TVectorialFillInterface.ToolbarMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then FOnMouseMove(self, Shift, X+FToolbar.Left,Y+FToolbar.Top);
end;

procedure TVectorialFillInterface.ToolbarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then FOnMouseUp(self, Button, Shift, X+FToolbar.Left,Y+FToolbar.Top);
end;

procedure TVectorialFillInterface.AnyButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then FOnMouseDown(self, Button, Shift,
      X+FToolbar.Left+TControl(Sender).Left,Y+FToolbar.Top+TControl(Sender).Top);
end;

procedure TVectorialFillInterface.AnyButtonMouseEnter(Sender: TObject);
begin
  If Assigned(FOnMouseEnter) then FOnMouseEnter(self);
end;

procedure TVectorialFillInterface.AnyButtonMouseLeave(Sender: TObject);
begin
  If Assigned(FOnMouseLeave) then FOnMouseLeave(self);
end;

procedure TVectorialFillInterface.AnyButtonMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then FOnMouseMove(self, Shift,
      X+FToolbar.Left+TControl(Sender).Left,Y+FToolbar.Top+TControl(Sender).Top);
end;

procedure TVectorialFillInterface.AnyButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then FOnMouseUp(self, Button, Shift,
      X+FToolbar.Left+TControl(Sender).Left,Y+FToolbar.Top+TControl(Sender).Top);
end;

procedure TVectorialFillInterface.ToolbarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then FOnMouseDown(self, Button, Shift, X+FToolbar.Left,Y+FToolbar.Top);
end;

procedure TVectorialFillInterface.SetVerticalPadding(AValue: integer);
begin
  if FVerticalPadding=AValue then Exit;
  FVerticalPadding:=AValue;
  if Assigned(FToolbar) and Assigned(FImageList) then
  begin
    FToolbar.ButtonHeight:= FImageList.Height+AValue;
    UpdatePreview;
  end;
end;

procedure TVectorialFillInterface.Preview_MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case FillType of
  vftSolid: ChooseColor(-1, Button);
  vftGradient: if X < FPreview.Width div 2 then ChooseColor(0, Button) else ChooseColor(1, Button);
  vftTexture: if Assigned(Texture) and Assigned(FOnTextureClick) then
                FOnTextureClick(self);
  end;
end;

procedure TVectorialFillInterface.EditGradTextPointsClick(Sender: TObject);
begin
  if Assigned(FOnEditGradTexPoints) then FOnEditGradTexPoints(self);
end;

function TVectorialFillInterface.GetEditingGradTexPoints: boolean;
begin
  if Assigned(FButtonEditGradTexPoints) then
    result := FButtonEditGradTexPoints.Down
  else result := false;
end;

procedure TVectorialFillInterface.SetCanEditGradTexPoints(AValue: boolean);
begin
  if FCanEditGradTexPoints=AValue then Exit;
  FCanEditGradTexPoints:=AValue;
  UpdateButtonAdjustToShape;
end;

procedure TVectorialFillInterface.SetEditingGradTexPoints(AValue: boolean);
begin
  if Assigned(FButtonEditGradTexPoints) then
    FButtonEditGradTexPoints.Down := AValue;
end;

procedure TVectorialFillInterface.SetIsTarget(AValue: boolean);
begin
  if FIsTarget=AValue then Exit;
  FIsTarget:=AValue;
  UpdatePreview;
end;

procedure TVectorialFillInterface.ToolbarMouseEnter(Sender: TObject);
begin
  If Assigned(FOnMouseEnter) then FOnMouseEnter(self);
end;

procedure TVectorialFillInterface.ToolbarMouseLeave(Sender: TObject);
begin
  If Assigned(FOnMouseLeave) then FOnMouseLeave(self);
end;

procedure TVectorialFillInterface.AdjustToShapeClick(Sender: TObject);
begin
  if Assigned(FOnAdjustToShape) then FOnAdjustToShape(self);
end;

procedure TVectorialFillInterface.ButtonFillGradClick(Sender: TObject);
begin
  if Assigned(FGradTypeMenu) then
    with FButtonFillGradient.ClientToScreen(Point(0,FButtonFillGradient.Height)) do
      FGradTypeMenu.PopUp(X,Y);
  FButtonFillGradient.Down := (FillType = vftGradient);
end;

procedure TVectorialFillInterface.ButtonFillTexClick(Sender: TObject);
begin
  if FFillType = vftTexture then
  begin
    FButtonFillTexture.Down := true;
    exit;
  end;

  if Assigned(FTexture) then FillType := vftTexture
  else LoadTexture;
end;

procedure TVectorialFillInterface.ButtonGradInterpClick(Sender: TObject);
begin
  if Assigned(FGradInterpMenu) then
    with FButtonGradInterp.ClientToScreen(Point(0,FButtonGradInterp.Height)) do
      FGradInterpMenu.PopUp(X,Y);
end;

procedure TVectorialFillInterface.ButtonGradRepetitionClick(Sender: TObject);
begin
  if Assigned(FGradRepetitionMenu) then
    with FButtonGradRepetition.ClientToScreen(Point(0,FButtonGradRepetition.Height)) do
      FGradRepetitionMenu.PopUp(X,Y);
end;

procedure TVectorialFillInterface.ButtonLoadTextureClick(Sender: TObject);
begin
  LoadTexture;
end;

procedure TVectorialFillInterface.ButtonSwapColorClick(Sender: TObject);
var
  temp: TBGRAPixel;
begin
  temp := GradStartColor;
  FGradStartColor := GradEndColor;
  FGradEndColor := temp;
  UpdateGradientParams;
  if FillType = vftGradient then Changed;
end;

procedure TVectorialFillInterface.ButtonTexRepeatClick(Sender: TObject);
begin
  if Assigned(FTexRepetitionMenu) then
    with FButtonTexRepeat.ClientToScreen(Point(0,FButtonTexRepeat.Height)) do
      FTexRepetitionMenu.PopUp(X,Y);
end;

constructor TVectorialFillInterface.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Init(16,16);
end;

constructor TVectorialFillInterface.Create(AOwner: TComponent; AImageListWidth,
  AImageListHeight: Integer);
begin
  inherited Create(AOwner);
  Init(AImageListWidth,AImageListHeight);
end;

destructor TVectorialFillInterface.Destroy;
begin
  FTexture.FreeReference;
  if Assigned(FContainer) then
  begin
    FContainer.RemoveControl(FToolbar);
    FContainer := nil;
  end;
  FToolbar.Free;
  inherited Destroy;
end;

function TVectorialFillInterface.GetTextureThumbnail(AWidth, AHeight: integer; ABackColor: TColor): TBitmap;
var
  thumb: TBGRABitmap;
begin
  if FTexture = nil then exit(nil);
  thumb := GetBitmapThumbnail(FTexture, AWidth,AHeight,BGRAPixelTransparent,true);
  try
    result := thumb.MakeBitmapCopy(ABackColor);
  finally
    thumb.Free;
  end;
end;

procedure TVectorialFillInterface.AssignFill(AFill: TVectorialFill);
begin
  FillType := AFill.FillType;
  case FillType of
    vftTexture:
      begin
        Texture := AFill.Texture;
        TextureOpacity:= AFill.TextureOpacity;
        TextureRepetition:= AFill.TextureRepetition;
      end;
    vftSolid: SolidColor := AFill.SolidColor;
    vftGradient:
      begin
        GradStartColor := AFill.Gradient.StartColor;
        GradEndColor := AFill.Gradient.EndColor;
        GradientType:= AFill.Gradient.GradientType;
        GradRepetition:= AFill.Gradient.Repetition;
        GradInterpolation := AFill.Gradient.ColorInterpolation;
      end;
  end;
end;

procedure TVectorialFillInterface.UpdateFillExceptGeometry(ATargetFill: TVectorialFill);
var
  f: TVectorialFill;
begin
  f := CreateShapeFill(nil);
  if Assigned(ATargetFill) then
    ATargetFill.AssignExceptGeometry(f);
  f.Free;
end;

function TVectorialFillInterface.CreateShapeFill(AShape: TVectorShape): TVectorialFill;
var
  grad: TBGRALayerGradientOriginal;
begin
  if FillType = vftSolid then
    exit(TVectorialFill.CreateAsSolid(SolidColor))
  else if (FillType = vftTexture) and Assigned(Texture) then
    result := TVectorialFill.CreateAsTexture(Texture, AffineMatrixIdentity,
         TextureOpacity, TextureRepetition)
  else if FillType = vftGradient then
  begin
    grad := TBGRALayerGradientOriginal.Create;
    grad.StartColor := GradStartColor;
    grad.EndColor := GradEndColor;
    grad.GradientType:= GradientType;
    grad.Repetition := GradRepetition;
    grad.ColorInterpolation:= GradInterpolation;
    result := TVectorialFill.CreateAsGradient(grad, true);
  end
  else exit(nil); //none

  if Assigned(AShape) then
    result.FitGeometry(AShape.SuggestGradientBox(AffineMatrixIdentity));
end;

procedure TVectorialFillInterface.UpdateShapeFill(AShape: TVectorShape;
  ATarget: TLCFillTarget);
var
  vectorFill: TVectorialFill;
  curFill: TVectorialFill;
begin
  case ATarget of
    ftPen: curFill:= AShape.PenFill;
    ftBack: curFill := AShape.BackFill;
    ftOutline: curFill := AShape.OutlineFill;
    else exit;
  end;

  if (FillType = vftTexture) and (TextureOpacity = 0) then
    vectorFill := nil else
  if (FillType = vftTexture) and (curFill.FillType = vftTexture) then
  begin
    vectorFill := TVectorialFill.CreateAsTexture(Texture, curFill.TextureMatrix,
                                                 TextureOpacity, TextureRepetition);
  end
  else if (FillType = vftGradient) and (curFill.FillType = vftGradient) then
  begin
    vectorFill := curFill.Duplicate;
    vectorFill.Gradient.StartColor := GradStartColor;
    vectorFill.Gradient.EndColor := GradEndColor;
    vectorFill.Gradient.GradientType := GradientType;
    vectorFill.Gradient.Repetition := GradRepetition;
    vectorFill.Gradient.ColorInterpolation:= GradInterpolation;
  end else
    vectorFill := CreateShapeFill(AShape);

  case ATarget of
    ftPen: AShape.PenFill:= vectorFill;
    ftBack: AShape.BackFill:= vectorFill;
    ftOutline: AShape.OutlineFill:= vectorFill;
  end;
  vectorFill.Free;
end;

begin
  {$i fillimages.lrs}
end.

