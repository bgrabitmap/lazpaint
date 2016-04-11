unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, OpenGLContext, BGRAOpenGL, BGLVirtualScreen, UGame;

type
  { TForm1 }

  TForm1 = class(TForm)
    BGLVirtualScreen1: TBGLVirtualScreen;
    BGLVirtualScreen2: TBGLVirtualScreen;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;

    procedure BGLVirtualScreen2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);

    { HANDLE MOUSE AND KEY STROKES }
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure BGLVirtualScreen1MouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure BGLVirtualScreen1MouseMove(Sender: TObject; {%H-}Shift: TShiftState;
      X, Y: Integer);
    procedure BGLVirtualScreen1MouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);

    { VIRTUAL SCREEN 1 }
    procedure BGLVirtualScreen1LoadTextures(Sender: TObject; BGLContext: TBGLContext);
    procedure BGLVirtualScreen1Redraw(Sender: TObject; BGLContext: TBGLContext);
    procedure BGLVirtualScreen1Elapse(Sender: TObject; BGLContext: TBGLContext; ElapsedMs: integer);
    procedure BGLVirtualScreen1FramesPerSecond(Sender: TObject; {%H-}BGLContext: TBGLContext; FramesPerSecond: integer);
    procedure BGLVirtualScreen1UnloadTextures(Sender: TObject; BGLContext: TBGLContext);

    { VIRTUAL SCREEN 2 }
    procedure BGLVirtualScreen2LoadTextures(Sender: TObject; BGLContext: TBGLContext);
    procedure BGLVirtualScreen2Redraw(Sender: TObject; BGLContext: TBGLContext);
    procedure BGLVirtualScreen2Elapse(Sender: TObject; BGLContext: TBGLContext; ElapsedMs: integer);
    procedure BGLVirtualScreen2FramesPerSecond(Sender: TObject; {%H-}BGLContext: TBGLContext; FramesPerSecond: integer);
    procedure BGLVirtualScreen2UnloadTextures(Sender: TObject; BGLContext: TBGLContext);
  protected
    procedure AdjustHeight;
  public
    GameContext1: TGameContext;
    GameContext2: TGameContext;
  end;

var
  Form1: TForm1; 

implementation

uses LCLType, BGRABitmapTypes;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Position := poScreenCenter;
  ClientWidth := 800;
  ClientHeight := 600;
  Constraints.MaxWidth := Width;

  GameContext1 := TGameContext.Create(False);
  GameContext2 := nil;
  { Textures are loaded in the LoadTextures event }

  AdjustHeight;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  { Textures must be freed while the virtual screens exist }
  if Assigned(GameContext1) then
  begin
    BGLVirtualScreen1.UnloadTextures;
    FreeAndNil(GameContext1);
  end;
  if Assigned(GameContext2) then
  begin
    BGLVirtualScreen2.UnloadTextures;
    FreeAndNil(GameContext2);
  end;
end;

procedure TForm1.BGLVirtualScreen2Click(Sender: TObject);
begin
  if not Assigned(GameContext2) then
    Button2Click(Sender);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  AdjustHeight;
end;

procedure TForm1.AdjustHeight;
begin
  if Assigned(GameContext2) then
  begin
    BGLVirtualScreen2.Height := (ClientHeight-Panel1.Height) div 2;
    BGLVirtualScreen2.Cursor := crDefault;
  end
  else
  begin
    BGLVirtualScreen2.Height := 8;
    BGLVirtualScreen2.Cursor := crHandPoint;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Button1.Enabled := false;
  Button2.Enabled := true;

  if Assigned(GameContext2) then
  begin
    BGLVirtualScreen2.UnloadTextures;
    FreeAndNil(GameContext2);
    AdjustHeight;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Button1.Enabled := true;
  Button2.Enabled := false;

  if not Assigned(GameContext2) then
  begin
    GameContext2 := TGameContext.Create(True);
    BGLVirtualScreen2.QueryLoadTextures; //query the event LoadTextures
    AdjustHeight;
  end;
end;

{---------------- HANDLE MOUSE AND KEY STROKES --------------}

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  GameContext1.KeyDown(Key,Shift);
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  GameContext1.KeyUp(Key,Shift);
end;

procedure TForm1.BGLVirtualScreen1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  GameContext1.MouseDown(Button,X,Y);
end;

procedure TForm1.BGLVirtualScreen1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  GameContext1.MouseMove(X,Y);
end;

procedure TForm1.BGLVirtualScreen1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  GameContext1.MouseMove(X,Y);
  GameContext1.MouseUp(Button);
end;


{------------------ VIRTUAL SCREEN 1 ----------------}

procedure TForm1.BGLVirtualScreen1LoadTextures(Sender: TObject;
  BGLContext: TBGLContext);
begin
  if Assigned(GameContext1) then
    GameContext1.LoadTextures(BGLContext);
end;

procedure TForm1.BGLVirtualScreen1Redraw(Sender: TObject;
  BGLContext: TBGLContext);
begin
  if Assigned(GameContext1) then
    GameContext1.Render(BGLContext);
end;

procedure TForm1.BGLVirtualScreen1Elapse(Sender: TObject; BGLContext: TBGLContext; ElapsedMs: integer);
begin
  if Assigned(GameContext1) then
    GameContext1.Elapse(BGLContext, ElapsedMs);
end;

procedure TForm1.BGLVirtualScreen1FramesPerSecond(Sender: TObject;
  BGLContext: TBGLContext; FramesPerSecond: integer);
begin
  if Assigned(GameContext1) then
    GameContext1.FPS := FramesPerSecond;
end;

procedure TForm1.BGLVirtualScreen1UnloadTextures(Sender: TObject;
  BGLContext: TBGLContext);
begin
  if Assigned(GameContext1) then
    GameContext1.UnloadTextures(BGLContext);
end;

{-------------------- VIRTUAL SCREEN 2 -----------------}

procedure TForm1.BGLVirtualScreen2LoadTextures(Sender: TObject;
  BGLContext: TBGLContext);
begin
  if Assigned(GameContext2) then
    GameContext2.LoadTextures(BGLContext);
end;

procedure TForm1.BGLVirtualScreen2Redraw(Sender: TObject;
  BGLContext: TBGLContext);
begin
  if Assigned(GameContext2) then
  begin
    //draw an horizontal line to seperate visually from the other game
    BGLContext.Canvas.Line(0,0,BGLContext.Width-1,0,BGRABlack,True);
    BGLContext.Canvas.Translate(0,1);
    GameContext2.Render(BGLContext);
  end else
  begin
    //draw a handle to indicate it is reduced
    BGLContext.Canvas.Fill(ColorToBGRA(ColorToRGB(clBtnFace)));
    BGLContext.Canvas.Line(1,3,BGLContext.Width-2,3, ColorToBGRA(ColorToRGB(clBtnHighlight)));
    BGLContext.Canvas.Line(1,4,BGLContext.Width-2,4, ColorToBGRA(ColorToRGB(clBtnShadow)));
  end;
end;

procedure TForm1.BGLVirtualScreen2Elapse(Sender: TObject;
  BGLContext: TBGLContext; ElapsedMs: integer);
begin
  if Assigned(GameContext2) then
    GameContext2.Elapse(BGLContext, ElapsedMs);
end;

procedure TForm1.BGLVirtualScreen2FramesPerSecond(Sender: TObject;
  BGLContext: TBGLContext; FramesPerSecond: integer);
begin
  if Assigned(GameContext2) then
    GameContext2.FPS := FramesPerSecond;
end;

procedure TForm1.BGLVirtualScreen2UnloadTextures(Sender: TObject;
  BGLContext: TBGLContext);
begin
  if Assigned(GameContext2) then
    GameContext2.UnloadTextures(BGLContext);
end;



end.

