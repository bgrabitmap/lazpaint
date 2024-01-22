unit uhypocycloid;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Spin,
  LazPaintType, UStateType, UScripting, UResourceStrings, LCVectorOriginal, LCVectorPolyShapes;

const
  RenderName = 'hypocycloid';

type

  { TFHypocycloid }

  TFHypocycloid = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    Label_Amount: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    SpinEdit_CuspCount: TSpinEdit;
    TimerDisplay: TTimer;
    procedure Button_CancelClick(Sender: TObject);
    procedure Button_OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit_CuspCountChange(Sender: TObject);
    procedure TimerDisplayTimer(Sender: TObject);
  private
    FComposeShape: TComposedImageDifference;
    FUpdateLayer: boolean;
    FInitializing: boolean;
    FInstance: TLazPaintCustomInstance;
    FParameters: TVariableSet;
    FPreviewDone: boolean;
    procedure InitParams;
    procedure NeedPreview;
    procedure DisplayShape;
    procedure UpdateOriginal(AOriginal: TVectorOriginal; ACuspCount: integer);
    function IsCorrespondingOriginal: boolean;
  end;

function ShowHypocycloidDlg(AInstance: TLazPaintCustomInstance; AParameters: TVariableSet): TScriptResult;

// create a hypocycloid shape
function MakeHypocycloid(ASurfaceWidth, ASurfaceHeight: integer; ACuspCount: integer): TVectorShape;

implementation

uses LCScaleDPI, UMac, UImageAction, BGRABitmapTypes, UImageDiff, BGRATransform, Math;

{$R *.lfm}

function ShowHypocycloidDlg(AInstance: TLazPaintCustomInstance; AParameters: TVariableSet): TScriptResult;
var
  FHypocycloid: TFHypocycloid;
  doFound, somethingDone: boolean;
begin
  FHypocycloid:= TFHypocycloid.create(nil);
  FHypocycloid.FInstance := AInstance;
  FHypocycloid.FParameters := AParameters;
  try
    TImageActions(AInstance.ImageAction).Deselect;

    if Assigned(AParameters) and
      AParameters.Booleans['Validate'] then
    begin
      AInstance.Image.DoBegin;
      FHypocycloid.InitParams;
      FHypocycloid.DisplayShape;
      AInstance.Image.DoEnd(doFound, somethingDone);
      result := srOk;
    end else
    begin
      if FHypocycloid.showModal = mrOk then result := srOk
      else result := srCancelledByUser;
    end;
  finally
    FHypocycloid.free;
  end;
end;

function MakeHypocycloid(ASurfaceWidth, ASurfaceHeight: integer; ACuspCount: integer): TVectorShape;

  // center of the small circle
  function HypocycloidCenterAt(phi, nR, r: Double): TPointF;
  begin
    result.x := (nR - r) * cos(phi);
    result.y := (nR - r) * sin(phi);
  end;

  // hypocycloid position is on the small circle
  function HypocycloidAt(phi, nR, r: Double): TPointF;
  begin
    result := HypocycloidCenterAt(phi, nR, R)
    + PointF(  r * cos((nR - r) / r * phi),
             - r * sin((nR - r) / r * phi) );
  end;

var
  t: Double;
  shape: TCurveShape;
  center: TPointF;
  nR: Double; // radius of bounding circle
  r: Double; // radius of small circle
  nbPoints, i: integer;

begin
  center := PointF(ASurfaceWidth / 2,
                   ASurfaceHeight / 2);
  nR := Math.Min(ASurfaceWidth, ASurfaceHeight) * 0.4;
  r := nR / ACuspCount;

  shape := TCurveShape.Create(nil);
  shape.PenColor := CSSRed;
  shape.PenWidth := 3;
  shape.Closed := true;

  nbPoints := ACuspCount * 8;
  for i := 0 to nbPoints - 1 do
  begin
    t := i/nbPoints*2*Pi;
    shape.AddPoint(HypocycloidAt(t, nR, r) + center);
  end;

  result := shape;
end;

{ TFHypocycloid }

procedure TFHypocycloid.FormCreate(Sender: TObject);
begin
  ScaleControl(Self,OriginalDPI);

  Button_OK.Caption := rsOK;
  Button_Cancel.Caption := rsCancel;

  CheckOKCancelBtns(Button_OK{,Button_Cancel});
  CheckSpinEdit(SpinEdit_CuspCount);

  FComposeShape := nil;
  FPreviewDone := false;
end;

procedure TFHypocycloid.Button_OKClick(Sender: TObject);
begin
  TimerDisplay.Enabled := false;
  if not FPreviewDone then DisplayShape;
end;

procedure TFHypocycloid.Button_CancelClick(Sender: TObject);
begin
  TimerDisplay.Enabled := false;
  if Assigned(FComposeShape) then
  begin
    FInstance.Image.Undo;
    FComposeShape := nil;
  end;
end;

procedure TFHypocycloid.FormHide(Sender: TObject);
begin
  if Assigned(FComposeShape) then
  begin
    If ModalResult <> mrOk then
       FInstance.Image.Undo;
    FComposeShape := nil;
  end;
end;

procedure TFHypocycloid.FormShow(Sender: TObject);
begin
  InitParams;
  NeedPreview;
end;

procedure TFHypocycloid.SpinEdit_CuspCountChange(Sender: TObject);
begin
  NeedPreview;
end;

procedure TFHypocycloid.TimerDisplayTimer(Sender: TObject);
begin
  TimerDisplay.Enabled:= false;
  DisplayShape;
end;

procedure TFHypocycloid.InitParams;
var
  isOriginal: Boolean;
  layerIndex, cuspCount, errPos: Integer;
  cuspCountStr: RawByteString;
begin
  FInitializing := true;

  // default parameters
  cuspCount := FInstance.Config.DefaultCuspCount;

  // parameters from current layer
  layerIndex := FInstance.Image.CurrentLayerIndex;
  if IsCorrespondingOriginal then
  begin
    cuspCountStr := FInstance.Image.GetLayerRegistry(layerIndex, 'cusp-count');
    val(cuspCountStr, cuspCount, errPos);
    if errPos = 0 then
      SpinEdit_CuspCount.Value := cuspCount;
    FUpdateLayer:= true;
  end
  else
    FUpdateLayer:= false;

  // supplied parameters to script function
  if Assigned(FParameters) and
    FParameters.IsDefined('CuspCount') then
    cuspCount := FParameters.Integers['CuspCount'];

  SpinEdit_CuspCount.Value := cuspCount;

  FInitializing := false;
end;

procedure TFHypocycloid.NeedPreview;
begin
  TimerDisplay.Enabled := false;
  TimerDisplay.Enabled := true;
  FPreviewDone := false;
  Button_OK.Enabled := false;
end;

procedure TFHypocycloid.DisplayShape;
var
  original, prevOriginal: TVectorOriginal;
  curIndex, cuspCount: Integer;
begin
  FPreviewDone := true;

  // cancel the preview
  if Assigned(FComposeShape) then
  begin
    FInstance.Image.Undo;
    FComposeShape := nil;
  end;

  // chosen parameters
  cuspCount := SpinEdit_CuspCount.Value;

  // make a new preview
  FComposeShape := FInstance.Image.DoBegin;
  try
    // if there is already a corresponding layer
    if FUpdateLayer then
    begin
      curIndex := FInstance.Image.CurrentLayerIndex;
      prevOriginal := FInstance.Image.LayerOriginal[curIndex] as TVectorOriginal;

      // record changes made inside the original
      FInstance.Image.CurrentState.DiscardOriginalDiff:= false;
      UpdateOriginal(prevOriginal, cuspCount);
      FInstance.Image.CurrentState.DiscardOriginalDiff:= true;
    end else
    begin
      // create a new layer
      original := TVectorOriginal.Create;
      UpdateOriginal(original, cuspCount);

      // if current layer is empty, replace it
      if FInstance.Image.CurrentLayerEmpty then
      begin
        curIndex := FInstance.Image.CurrentLayerIndex;
        FInstance.Image.AddUndo(
          TReplaceLayerByCustomOriginalDifference.Create(FInstance.Image.CurrentState,
            FInstance.Image.CurrentLayerIndex, true, original));
        FInstance.Image.ImageMayChangeCompletely;
        FInstance.Image.LayerName[curIndex] := Caption;
      end
      else
      begin
        // otherwise add the new layer
        FInstance.Image.AddNewLayer(original, Caption, boTransparent, AffineMatrixIdentity);
        curIndex := FInstance.Image.CurrentLayerIndex;
      end;

      FInstance.Image.SetLayerRegistry(curIndex, 'render-name', RenderName);
    end;

    // update the parameters saved in the layer
    FInstance.Image.SetLayerRegistry(curIndex, 'cusp-count', IntToStr(cuspCount));

    Button_OK.Enabled := true;
  finally
    FInstance.Image.DoEnd(FComposeShape);
  end;
end;

procedure TFHypocycloid.UpdateOriginal(AOriginal: TVectorOriginal; ACuspCount: integer);
var
  shape, prevShape: TCurveShape;
  i: Integer;
begin
  // create prototype shape
  shape := MakeHypocycloid(FInstance.Image.Width, FInstance.Image.Height,
                           ACuspCount) as TCurveShape;

  // find the existing hypocycloid
  for i := 0 to AOriginal.ShapeCount-1 do
    if AOriginal.Shape[i] is TCurveShape then
    begin
      prevShape := AOriginal.Shape[i] as TCurveShape;
      prevShape.Clear;
      prevShape.AddPointRange(shape.GetPointRange);
      FreeAndNil(shape);
      break;
    end;

  // if the shape wasn't found, add the prototype
  if Assigned(shape) then
    AOriginal.AddShape(shape);
end;

function TFHypocycloid.IsCorrespondingOriginal: boolean;
var
  curIndex: Integer;
begin
  curIndex := FInstance.Image.CurrentLayerIndex;
  result :=
    (FInstance.Image.LayerOriginalClass[curIndex] = TVectorOriginal) and
    (FInstance.Image.GetLayerRegistry(curIndex, 'render-name') = RenderName)
end;

end.

