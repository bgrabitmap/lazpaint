unit ublendop;

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
    procedure Button_OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox_BlendDblClick(Sender: TObject);
    procedure ListBox_BlendSelectionChange(Sender: TObject; {%H-} User: boolean);
    procedure ListBox_DrawBlendItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ListBox_DrawPatternItem(Control: TWinControl;
      Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure ListBox_PatternSelectionChange(Sender: TObject; {%H-}User: boolean
      );
  private
    procedure DrawPattern(ACanvas: TCanvas; ARect: TRect; APattern: string;
      State: TOwnerDrawState);
    function GetPattern(AWidth, AHeight: integer; APattern: string
      ): TBGRABitmap;
    { private declarations }
    procedure UpdateBlendOpLabel;
  public
    { public declarations }
    SelectedBlendOp: TBlendOperation;
    PatternUnder,PatternOver: TBGRABitmap;
  end;

function ShowBlendOpDialog(var BlendOp: TBlendOperation; APatternUnder, APatternOver: TBGRABitmap): boolean;
function BlendThumbNailSize: integer;

implementation

uses LCLType,uscaledpi,umac,uresourcestrings,ugraph;

function TFBlendOp.GetPattern(AWidth,AHeight: integer; APattern: string): TBGRABitmap;
var lColor: TBGRAPixel;
  idx: integer;
  attr: string;
begin
  if APattern = 'Under' then
  begin
    result := PatternUnder.Resample(AWidth,AHeight,rmSimpleStretch) as TBGRABitmap;
    exit;
  end else
  if APattern = 'Over' then
  begin
    result := PatternOver.Resample(AWidth,AHeight,rmSimpleStretch) as TBGRABitmap;
    exit;
  end;
  result := TBGRABitmap.Create(AWidth,AHeight,BGRABlack);
  lColor := BGRAWhite;
  idx := pos('.',APattern);
  if idx <> 0 then
  begin
    attr := copy(APattern,idx+1,length(APattern)-idx);
    delete(APattern,idx,length(APattern)-idx+1);
    lColor := StrToBGRA(attr,BGRAWhite);
  end;
  if APattern = 'LeftToRight' then
    result.GradientFill(0,0,AWidth,AHeight,BGRABlack,lColor,gtLinear,PointF(0,0),PointF(AWidth-1,0),dmSet,False) else
  if APattern = 'TopToBottom' then
    result.GradientFill(0,0,AWidth,AHeight,BGRABlack,lColor,gtLinear,PointF(0,0),PointF(0,AHeight-1),dmSet,False) else
  if APattern = 'Ellipse' then
    result.GradientFill(0,0,AWidth,AHeight,lColor,BGRABlack,gtRadial,PointF((AWidth-1)/2,(AHeight-1)/2),PointF(0,(AHeight-1)/2),dmSet,False);
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
    DrawCheckers(temp);
    temp.PutImage(0,0,ABmp,dmDrawWithTransparency);
    ABmp.Free;
    ABmp := temp;
  end;
end;

procedure TFBlendOp.DrawPattern(ACanvas: TCanvas; ARect: TRect; APattern: string; State: TOwnerDrawState);
var bmp: TBGRABitmap;
begin
  if (ARect.Right <= ARect.Left) or (ARect.Bottom <= ARect.Top) then exit;
  bmp := GetPattern(ARect.Right-ARect.Left,ARect.Bottom-ARect.Top,APattern);
  if odSelected in State then DrawPatternHighlight(bmp);
  AddCheckersIfNeeded(bmp);
  bmp.Draw(ACanvas,ARect.Left,ARect.Top,true);
  bmp.Free;
end;

function ShowBlendOpDialog(var BlendOp: TBlendOperation; APatternUnder,
  APatternOver: TBGRABitmap): boolean;
var f: TFBlendOp;
  resampledOver,resampledUnder: TBGRABitmap;
begin
  result := false;
  f:= TFBlendOp.Create(nil);
  resampledOver := APatternOver.Resample(BlendThumbNailSize,BlendThumbNailSize,rmSimpleStretch) as TBGRABitmap;
  resampledUnder := APatternUnder.Resample(BlendThumbNailSize,BlendThumbNailSize,rmSimpleStretch) as TBGRABitmap;
  f.PatternOver := resampledOver;
  f.PatternUnder := resampledUnder;
  try
    if f.ShowModal = mrOK then
    begin
      result := true;
      BlendOp := f.SelectedBlendOp;
    end;
  except on ex:Exception do
    MessageDlg(ex.Message,mtError,[mbOk],0);
  end;
  resampledOver.Free;
  resampledUnder.Free;
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
    if SelectedBlendOp in[boLinearAdd,boXor] then compatible.Add('Paint.NET');
    if SelectedBlendOp in[boDivide,boLinearAdd,boLinearExclusion,boLinearSubtract,boLinearSubtractInverse] then compatible.Add('Krita');
    if compatible.Count = 0 then str += ' ('+rsLazPaintOnly+')' else
      str += ' (' + compatible.CommaText+')';
    compatible.Free;
  end;
  Label_BlendOpValue.Caption := str;
end;

procedure TFBlendOp.FormCreate(Sender: TObject);
begin
  ScaleDPI(self,OriginalDPI);
  ListBox_PatternUnder.ItemHeight := BlendThumbNailSize;
  ListBox_PatternOver.ItemHeight := BlendThumbNailSize;
  ListBox_BlendSvg.ItemHeight := BlendThumbNailSize;
  ListBox_BlendKrita.ItemHeight := BlendThumbNailSize;
  ListBox_BlendOther.ItemHeight := BlendThumbNailSize;
  ListBox_PatternUnder.ItemIndex := 1;
  ListBox_PatternOver.ItemIndex := 2;
  CheckOKCancelBtns(Button_OK,Button_Cancel);
end;

procedure TFBlendOp.Button_OKClick(Sender: TObject);
begin
  ModalResult:= mrOk;
end;

procedure TFBlendOp.FormShow(Sender: TObject);
begin
  SelectedBlendOp := boTransparent;
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
    end;
  end;
end;

procedure TFBlendOp.ListBox_DrawBlendItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  under,over: TBGRABitmap;
  w,h: integer;
  BlendStr: string;
begin
  if (ListBox_PatternUnder.ItemIndex <> -1) and
    (ListBox_PatternOver.ItemIndex <> -1) and
    (Index <> -1) then
  begin
    if (ARect.Right <= ARect.Left) or (ARect.Bottom <= ARect.Top) then exit;
    BlendStr := (Control as TListBox).Items[Index];
    w := ARect.Right-ARect.Left;
    h := ARect.Bottom-ARect.Top;
    under := GetPattern(w,h,ListBox_PatternUnder.Items[ListBox_PatternUnder.ItemIndex]);
    over := GetPattern(w,h,ListBox_PatternOver.Items[ListBox_PatternOver.ItemIndex]);
    under.BlendImageOver(0,0,over,StrToBlendOperation(BlendStr));
    over.Free;
    if odSelected in State then DrawPatternHighlight(under);
    under.FontName := 'Arial';
    under.FontFullHeight := DoScaleY(12,OriginalDPI);
    under.FontQuality := fqFineAntialiasing;
    under.TextOut(1,1,BlendStr,BGRABlack);
    under.TextOut(0,0,BlendStr,BGRAWhite);
    AddCheckersIfNeeded(under);
    under.Draw((Control as TListBox).Canvas,ARect.Left,ARect.Top,True);
    under.Free;
  end;
end;

initialization
  {$I ublendop.lrs}

end.

