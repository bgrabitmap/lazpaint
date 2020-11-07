// SPDX-License-Identifier: GPL-3.0-only
unit UBlendOp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, types, BGRABitmap, BGRABitmapTypes, LazPaintType;

type

  { TFBlendOp }

  TFBlendOp = class(TForm)
    Bevel1: TBevel;
    Button_Cancel: TButton;
    Button_OK: TButton;
    Label_PreviewWith: TLabel;
    Label_BlendOpCategory: TLabel;
    Label_BlendOpValue: TLabel;
    Label_SelectedBlendOp: TLabel;
    Label_SvgOver: TLabel;
    Label_KritaOver: TLabel;
    Label_OtherOver: TLabel;
    Label_PatternUnder: TLabel;
    Label_PatternOver: TLabel;
    ListBox_BlendOther: TListBox;
    ListBox_BlendSvg: TListBox;
    ListBox_BlendKrita: TListBox;
    ListBox_PatternUnder: TListBox;
    ListBox_PatternOver: TListBox;
    ScrollBar1: TScrollBar;
    TimerResize: TTimer;
    procedure Button_OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox_BlendDblClick(Sender: TObject);
    procedure ListBox_BlendSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure ListBox_DrawBlendItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ListBox_DrawPatternItem(Control: TWinControl;
      Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure ListBox_PatternSelectionChange(Sender: TObject; {%H-}User: boolean
      );
    procedure ListBox_MeasureItem(Control: TWinControl;
      {%H-}Index: Integer; var AHeight: Integer);
    procedure TimerResizeTimer(Sender: TObject);
  private
    FPatterns: array of record
      name:string;
      bmp: TBGRABitmap;
      width,height: integer;
    end;
    FListBoxInternalMargin: integer;
    FFirstColumnLeft: integer;
    FLastColumnRightMargin: integer;
    FComputedWidth,FComputedHeight: integer;
    procedure DrawPattern(ACanvas: TCanvas; ARect: TRect; APattern: string;
      State: TOwnerDrawState);
    function GetPattern(AWidth, AHeight: integer; APattern: string;
      ACheckers: boolean): TBGRABitmap;
    { private declarations }
    procedure UpdateBlendOpLabel;
    procedure DiscardPatterns;
  public
    { public declarations }
    SelectedBlendOp: TBlendOperation;
    PatternUnder,PatternOver: TBGRABitmap;
  end;

function ShowBlendOpDialog(AInstance: TLazPaintCustomInstance; var BlendOp: TBlendOperation; APatternUnder, APatternOver: TBGRABitmap): boolean;

implementation

uses LCLType,LCScaleDPI,umac,uresourcestrings,ugraph,BGRAThumbnail,Math, BGRATextFX;

function TFBlendOp.GetPattern(AWidth,AHeight: integer; APattern: string; ACheckers: boolean): TBGRABitmap;
var lColor: TBGRAPixel;
  idx: integer;
  fullPatternName, attr: string;
  i: integer;
begin
  fullPatternName:= APattern;
  for i := 0 to high(FPatterns) do
  begin
    if (FPatterns[i].name = fullPatternName) and (FPatterns[i].width = AWidth) and (FPatterns[i].height = AHeight) then
    begin
      result := FPatterns[i].bmp;
      exit;
    end;
  end;
  BGRAThumbnail.CheckersScale:= GetCanvasScaleFactor;
  if APattern = 'Under' then
  begin
    result := GetBitmapThumbnail(PatternUnder,AWidth,AHeight,BGRAPixelTransparent,ACheckers) as TBGRABitmap;
  end else
  if APattern = 'Over' then
  begin
    result := GetBitmapThumbnail(PatternOver,AWidth,AHeight,BGRAPixelTransparent,ACheckers) as TBGRABitmap;
  end else
  begin
    result := TBGRABitmap.Create(AWidth,AHeight, BGRABlack);
    lColor := BGRAWhite;
    idx := pos('.',APattern);
    if idx <> 0 then
    begin
      attr := copy(APattern,idx+1,length(APattern)-idx);
      delete(APattern,idx,length(APattern)-idx+1);
      lColor := StrToBGRA(attr,BGRAWhite);
    end;
    if APattern = 'LeftToRight' then
      result.GradientFill(0,0,result.Width,result.Height,BGRABlack,lColor,gtLinear,PointF(0,0),PointF(result.Width-1,0),dmSet,False) else
    if APattern = 'TopToBottom' then
      result.GradientFill(0,0,result.Width,result.Height,BGRABlack,lColor,gtLinear,PointF(0,0),PointF(0,result.Height-1),dmSet,False) else
    if APattern = 'Ellipse' then
      result.GradientFill(0,0,result.Width,result.Height,lColor,BGRABlack,gtRadial,PointF((result.Width-1)/2,(result.Height-1)/2),PointF(0,(result.Height-1)/2),dmSet,False);
    BGRAReplace(result,GetBitmapThumbnail(result,AWidth,AHeight,BGRAPixelTransparent,false));
  end;
  BGRAThumbnail.CheckersScale:= 1;
  setlength(FPatterns,length(FPatterns)+1);
  FPatterns[high(FPatterns)].name := fullPatternName;
  FPatterns[high(FPatterns)].bmp := result;
  FPatterns[high(FPatterns)].width:= AWidth;
  FPatterns[high(FPatterns)].height:= AHeight;
end;

procedure DrawPatternHighlight(ABmp: TBGRABitmap);
begin
  ABmp.FillPoly([PointF(0,0),PointF(ABmp.Width,0),PointF(ABmp.Width,ABmp.Height),PointF(0,ABmp.Height),EmptyPointF,
                 PointF(ABmp.Width div 8,ABmp.Height*7 div 8),PointF(ABmp.Width*7 div 8,ABmp.Height*7 div 8),
                 PointF(ABmp.Width*7 div 8,ABmp.Height div 8),PointF(ABmp.Width div 8,ABmp.Height div 8)],
                 ColorToBGRA(ColorToRGB(clHighlight),128),dmDrawWithTransparency);
end;

procedure AddCheckersIfNeeded(var ABmp: TBGRABitmap);
var temp: TBGRABitmap;
begin
  if ABmp.HasTransparentPixels then
  begin
    temp := TBGRABitmap.Create(ABmp.Width,ABmp.Height);
    DrawCheckers(temp, rect(0,0,temp.Width,temp.Height));
    temp.PutImage(0,0,ABmp,dmDrawWithTransparency);
    ABmp.Free;
    ABmp := temp;
  end;
end;

procedure TFBlendOp.DrawPattern(ACanvas: TCanvas; ARect: TRect; APattern: string; State: TOwnerDrawState);
var bmp: TBGRABitmap;
  scaling: Double;
begin
  if (ARect.Right <= ARect.Left) or (ARect.Bottom <= ARect.Top) then exit;
  scaling := GetCanvasScaleFactor;
  bmp := TBGRABitmap.Create(round(ARect.Width*scaling),
    round(ARect.Height*scaling), ColorToRGB(clBtnFace));
  bmp.PutImage(0,0, GetPattern(bmp.width,bmp.height,APattern,True), dmDrawWithTransparency);
  if odSelected in State then DrawPatternHighlight(bmp);
  bmp.Draw(ACanvas,ARect,false);
  bmp.Free;
end;

function ShowBlendOpDialog(AInstance: TLazPaintCustomInstance; var BlendOp: TBlendOperation; APatternUnder,
  APatternOver: TBGRABitmap): boolean;
var f: TFBlendOp;
begin
  result := false;
  f:= TFBlendOp.Create(nil);
  f.PatternOver := APatternOver;
  f.PatternUnder := APatternUnder;
  try
    if f.ShowModal = mrOK then
    begin
      result := true;
      BlendOp := f.SelectedBlendOp;
    end;
  except on ex:Exception do
    AInstance.ShowError('ShowBlendOpDialog',ex.Message);
  end;
  f.Free;
end;

function BlendThumbNailSize: integer;
begin
  result := ScaleY(80,OriginalDPI);
end;

{ TFBlendOp }

procedure TFBlendOp.ListBox_DrawPatternItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  {$IFDEF LINUX}
  ARect.Right := ARect.Left+Control.Width-FListBoxInternalMargin;
  {$ENDIF}
  if Index <> -1 then
    DrawPattern((Control as TListBox).Canvas,ARect,(Control as TListBox).Items[Index],State);
end;

procedure TFBlendOp.ListBox_PatternSelectionChange(Sender: TObject;
  User: boolean);
begin
  ListBox_BlendSvg.Invalidate;
  ListBox_BlendKrita.Invalidate;
  ListBox_BlendOther.Invalidate;
end;

procedure TFBlendOp.ListBox_MeasureItem(Control: TWinControl;
  Index: Integer; var AHeight: Integer);
begin
  AHeight := (Control as TListBox).ItemHeight;
end;

procedure TFBlendOp.TimerResizeTimer(Sender: TObject);
var leftPos: integer;
  columnWidth, rowHeight: integer;
begin
  DiscardPatterns;
  leftPos := FFirstColumnLeft;
  columnWidth := (ClientWidth - FLastColumnRightMargin - leftPos) div 3;
  if columnWidth < 4 then columnWidth:= 4;
  rowHeight := columnWidth*600 div 800;
  Label_SvgOver.Left := leftPos;
  Label_SvgOver.Width := columnWidth-2;
  ListBox_BlendSvg.Left := leftPos;
  ListBox_BlendSvg.Width := columnWidth-2;
  ListBox_BlendSvg.ItemHeight := rowHeight;
  leftPos += columnWidth;
  Label_KritaOver.Left := leftPos;
  Label_KritaOver.Width := columnWidth-2;
  ListBox_BlendKrita.Left := leftPos;
  ListBox_BlendKrita.Width := columnWidth-2;
  ListBox_BlendKrita.ItemHeight := rowHeight;
  leftPos += columnWidth;
  Label_OtherOver.Left := leftPos;
  Label_OtherOver.Width := columnWidth-2;
  ListBox_BlendOther.Left := leftPos;
  ListBox_BlendOther.Width := columnWidth-2;
  ListBox_BlendOther.ItemHeight := rowHeight;
  TimerResize.Enabled := false;
end;

procedure TFBlendOp.UpdateBlendOpLabel;
var str: string;
  compatible: TStringList;
begin
  if SelectedBlendOp = boTransparent then
    str := rsNormalBlendOp
  else
  begin
    str := BlendOperationStr[SelectedBlendOp];
    compatible := TStringList.Create;
    if SelectedBlendOp in[boColorBurn,boColorDodge,boDarken,boHardLight,boLighten,
      boMultiply,boOverlay,boScreen,boSoftLight,boLinearDifference] then compatible.Add(rsAllApplications);
    if SelectedBlendOp in[boLinearAdd,boXor,boGlow,boReflect,boLinearNegation] then compatible.Add('Paint.NET');
    if SelectedBlendOp in[boDivide,boLinearAdd,boLinearExclusion,boLinearSubtract,boLinearSubtractInverse] then compatible.Add('Krita');
    if compatible.Count = 0 then str += ' ('+rsLazPaintOnly+')' else
      str += ' (' + compatible.CommaText+')';
    compatible.Free;
  end;
  Label_BlendOpValue.Left := Label_SelectedBlendOp.Left + Label_SelectedBlendOp.Width + ScaleX(8,OriginalDPI);
  Label_BlendOpValue.Caption := str;
end;

procedure TFBlendOp.DiscardPatterns;
var i: integer;
begin
  for i := 0 to high(FPatterns) do
    FPatterns[i].bmp.free;
  FPatterns := nil;
end;

procedure TFBlendOp.FormCreate(Sender: TObject);
begin
  ScaleControl(self,OriginalDPI);
  FListBoxInternalMargin:= ListBox_PatternUnder.Width - ListBox_PatternUnder.ClientWidth + ScrollBar1.Height;
  {$IFDEF LINUX}
  ListBox_PatternUnder.Style := lbOwnerDrawVariable;
  ListBox_PatternUnder.ScrollWidth := 0;
  ListBox_PatternOver.Style := lbOwnerDrawVariable;
  ListBox_PatternOver.ScrollWidth := 0;
  ListBox_BlendSvg.Style := lbOwnerDrawVariable;
  ListBox_BlendSvg.ScrollWidth := 0;
  ListBox_BlendKrita.Style := lbOwnerDrawVariable;
  ListBox_BlendKrita.ScrollWidth := 0;
  ListBox_BlendOther.Style := lbOwnerDrawVariable;
  ListBox_BlendOther.ScrollWidth := 0;
  {$ENDIF}
  ListBox_PatternUnder.ItemHeight := BlendThumbNailSize;
  ListBox_PatternOver.ItemHeight := BlendThumbNailSize;
  ListBox_BlendSvg.ItemHeight := BlendThumbNailSize;
  ListBox_BlendKrita.ItemHeight := BlendThumbNailSize;
  ListBox_BlendOther.ItemHeight := BlendThumbNailSize;
  ListBox_PatternUnder.ItemIndex := 0;
  ListBox_PatternOver.ItemIndex := 0;
  CheckOKCancelBtns(Button_OK,Button_Cancel);
  FFirstColumnLeft := ListBox_BlendSvg.Left;
  FLastColumnRightMargin:= ClientWidth-(ListBox_BlendOther.Left+ListBox_BlendOther.Width);
  TimerResizeTimer(nil);
end;

procedure TFBlendOp.FormHide(Sender: TObject);
begin
  DiscardPatterns;
end;

procedure TFBlendOp.FormResize(Sender: TObject);
begin
  TimerResize.Enabled := false;
  TimerResize.Enabled := true;
end;

procedure TFBlendOp.Button_OKClick(Sender: TObject);
begin
  ModalResult:= mrOk;
end;

procedure TFBlendOp.FormShow(Sender: TObject);
begin
  SelectedBlendOp := boTransparent;
  FComputedWidth := Max(PatternOver.Width,PatternUnder.Width);
  FComputedHeight := Max(PatternOver.Height,PatternUnder.Height);
  UpdateBlendOpLabel;
end;

procedure TFBlendOp.ListBox_BlendDblClick(Sender: TObject);
begin
  if not Visible then exit;
  with Sender as TListBox do
  begin
    if ItemIndex <> -1 then
    begin
      SelectedBlendOp := StrToBlendOperation(Items[ItemIndex]);
      UpdateBlendOpLabel;
      ModalResult := mrOk;
    end;
  end;
end;

procedure TFBlendOp.ListBox_BlendSelectionChange(Sender: TObject;
  User: boolean);
begin
  if not Visible then exit;
  with Sender as TListBox do
  begin
    if ItemIndex <> -1 then
    begin
      SelectedBlendOp := StrToBlendOperation(Items[ItemIndex]);
      UpdateBlendOpLabel;
      if not (Sender = ListBox_BlendSvg) then ListBox_BlendSvg.ItemIndex := -1;
      if not (Sender = ListBox_BlendKrita) then ListBox_BlendKrita.ItemIndex := -1;
      if not (Sender = ListBox_BlendOther) then ListBox_BlendOther.ItemIndex := -1;
    end;
  end;
end;

procedure TFBlendOp.ListBox_DrawBlendItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  background,preview,over: TBGRABitmap;
  w,h, checkerSize, shadowOfs: integer;
  BlendStr: string;
  fx: TBGRATextEffect;
  scaling: Double;
begin
  {$IFDEF LINUX}
  ARect.Right := ARect.Left+Control.Width-FListBoxInternalMargin;
  {$ENDIF}
  if (ListBox_PatternUnder.ItemIndex <> -1) and
    (ListBox_PatternOver.ItemIndex <> -1) and
    (Index <> -1) then
  begin
    if (ARect.Right <= ARect.Left) or (ARect.Bottom <= ARect.Top) then exit;
    BlendStr := (Control as TListBox).Items[Index];
    scaling := GetCanvasScaleFactor;
    checkerSize := DoScaleX(round(8*scaling), OriginalDPI);
    w := round(ARect.Width*scaling);
    h := round(ARect.Height*scaling);
    background := TBGRABitmap.Create(w,h,ColorToBGRA(ColorToRGB(clBtnFace)));
    background.DrawCheckers(background.ClipRect, ImageCheckersColor1, ImageCheckersColor2, checkerSize, checkerSize);
    preview := GetPattern(w,h,ListBox_PatternUnder.Items[ListBox_PatternUnder.ItemIndex],False).Duplicate as TBGRABitmap;
    over := GetPattern(w,h,ListBox_PatternOver.Items[ListBox_PatternOver.ItemIndex],False);
    preview.BlendImageOver(0,0,over,StrToBlendOperation(BlendStr));
    background.PutImage(0,0,preview,dmDrawWithTransparency);
    preview.Free;
    if odSelected in State then DrawPatternHighlight(background);
    fx := TBGRATextEffect.Create(BlendStr,'Arial',Max(DoScaleY(round(12*scaling),OriginalDPI),h div 10),true);
    shadowOfs := round(DoScaleX(round(10*scaling), OriginalDPI)/10);
    fx.DrawShadow(background,1+shadowOfs,1+shadowOfs,DoScaleX(round(2*scaling), OriginalDPI),BGRABlack);
    fx.DrawOutline(background,1,1,BGRABlack);
    fx.Draw(background,1,1,BGRAWhite);
    fx.Free;
    background.FontName := 'Arial';
    background.Draw((Control as TListBox).Canvas,ARect,True);
    background.Free;
  end;
end;

{$R *.lfm}

end.

