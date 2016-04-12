unit Port0;

(*
  Alexander Shiryaev, 2014.11, 2016.04
*)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, COMTPorts, ExtCtrls;

type
  BYTE = System.Byte;
  INTEGER = System.Integer;

  TFPort0 = class(TForm)
    ComboBox1: TComboBox;
    Label1: TLabel;
    Timer1: TTimer;
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ComboBox1DropDown(Sender: TObject);
  private
    { Private declarations }
    p: COMTPorts.TPort;
    device: string;
    baud: INTEGER;
    procedure TryOpen;
    procedure LoadSettings;
    procedure SaveSettings;
  public
    { Public declarations }
    receive: COMTPorts.TReceive;
    procedure Send (id: BYTE; const a: array of BYTE; len: INTEGER);
    procedure SetBaud (newBaud: INTEGER);
  end;

var
  FPort0: TFPort0;

implementation

{$R *.dfm}

const
  parity = COMTPorts.parityNone;
  defaultBaud = COMTPorts.CBR_19200;

procedure TFPort0.LoadSettings;
  const max = 256;
  var f: INTEGER;
    x: Char;
    i, res: INTEGER;
    s: string;
begin
  f := SysUtils.FileOpen(SysUtils.ExtractFileDir( Application.ExeName ) + '\Port0cfg', fmOpenRead);
  if f > 0 then begin
    s := '';
    res := SysUtils.FileRead(f, x, 1); i := 0;
    while (res = 1) and (x <> #0) and (x <> #10) and (x <> #13) and (i < max) do begin
      s := s + x; Inc(i);
      res := SysUtils.FileRead(f, x, 1)
    end;
    SysUtils.FileClose(f);
    if i < max then begin
      device := s
    end
  end
end (* LoadSettings *);

procedure TFPort0.SaveSettings;
  var f, i: INTEGER;
begin
  f := SysUtils.FileCreate(SysUtils.ExtractFileDir( Application.ExeName ) + '\Port0cfg');
  if f > 0 then begin
    i := 0;
    while i < Length(device) do begin
      SysUtils.FileWrite(f, device[1+i], 1); Inc(i)
    end;
    SysUtils.FileClose(f)
  end
end (* SaveSettings *);

procedure TFPort0.Send (id: BYTE; const a: array of BYTE; len: INTEGER);
begin
  if p <> nil then begin
    COMTPorts.Send(p, id, a, len)
  end
end (* TFPort0.Send *);

procedure TFPort0.TryOpen;
  var res: INTEGER;
begin
  assert(p = nil);
  COMTPorts.OpenPort('\\.\' + ComboBox1.Text, baud, parity, p, res);
  if res = 0 then begin
    Label1.Caption := 'открыт';
    Label1.Color := Graphics.clAqua;
  end else begin
    assert(p = nil);
    Label1.Caption := 'закрыт';
    Label1.Color := Graphics.clBtnFace
  end
end (* TryOpen *);

procedure TFPort0.ComboBox1Change(Sender: TObject);
begin
  if device <> ComboBox1.Text then begin
    device := ComboBox1.Text;

    if p <> nil then begin
      COMTPorts.ClosePort(p);
      p := nil;
      Label1.Caption := 'закрыт';
      Label1.Color := Graphics.clBtnFace
    end;

    TryOpen;

    if p <> nil then begin
      SaveSettings
    end

  end
end;

procedure TFPort0.SetBaud (newBaud: INTEGER);
begin
  if newBaud <> baud then begin
    baud := newBaud;

    if p <> nil then begin
      COMTPorts.ClosePort(p);
      p := nil;
      Label1.Caption := 'закрыт';
      Label1.Color := Graphics.clBtnFace
    end;

    TryOpen

  end
end (* SetBaud *);

procedure TFPort0.FormCreate(Sender: TObject);
begin
  receive := nil;
  p := nil;
  device := '';
  baud := defaultBaud;
  ComboBox1DropDown(nil);
  if ComboBox1.Items.Count = 1 then begin
    device := ComboBox1.Items[0]
  end;
  LoadSettings;
  ComboBox1.Text := device;
  TryOpen
end;

procedure TFPort0.Timer1Timer(Sender: TObject);
begin
  if p <> nil then begin
    if Assigned(receive) then begin
      COMTPorts.Receive(p, receive)
    end
  end
end;

procedure TFPort0.ComboBox1DropDown(Sender: TObject);
  var sl: TStringList;
begin
  sl := TStringList.Create();
  COMTPorts.EnumComPorts(sl);
  ComboBox1.Items.Clear;
  ComboBox1.Items.AddStrings(sl);
  sl.Destroy
end;

end.
