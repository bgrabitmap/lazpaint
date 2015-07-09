unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  OpenGLContext, UGame;

type
  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControl1Paint(Sender: TObject);
    procedure OnAppIdle(Sender: TObject; var Done: Boolean);
  public
    OpenGLControl1: TOpenGLControl;
    GameContext: TGameContext;
  end;

var
  Form1: TForm1; 

implementation

uses LCLType;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenGLControl1:=TOpenGLControl.Create(Self);
  with OpenGLControl1 do begin
    Name:='OpenGLControl1';
    Align:=alClient;
    Parent:=Self;
    OnPaint:=@OpenGLControl1Paint;
    OnMouseDown:= @FormMouseDown;
    OnMouseUp:= @FormMouseUp;
    OnMouseMove := @FormMouseMove;
    AutoResizeViewport:=true;
  end;
  
  Application.AddOnIdleHandler(@OnAppIdle);

  Position := poScreenCenter;
  ClientWidth := 800;
  ClientHeight := 600;

  GameContext := TGameContext.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  GameContext.Free;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  If Key = VK_LEFT then GameContext.LeftKey := true;
  If Key = VK_RIGHT then GameContext.RightKey := true;
  If Key = VK_UP then GameContext.UpKey := true;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  If Key = VK_LEFT then GameContext.LeftKey := false;
  If Key = VK_RIGHT then GameContext.RightKey := false;
  If Key = VK_UP then GameContext.UpKey := false;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  GameContext.MouseDown(Button,X,Y);
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  GameContext.MouseMove(X,Y);
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  GameContext.MouseMove(X,Y);
  GameContext.MouseUp(Button);
end;

procedure TForm1.OpenGLControl1Paint(Sender: TObject);
begin
  GameContext.Render(OpenGLControl1.Width, OpenGLControl1.Height);
  GameContext.Elapse(double(OpenGLControl1.FrameDiffTimeInMSecs));
  OpenGLControl1.SwapBuffers;
end;

procedure TForm1.OnAppIdle(Sender: TObject; var Done: Boolean);
begin
  Done:=false;
  OpenGLControl1.Invalidate;
end;

end.

