unit FindWindowUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,CEFuncProc,ComCtrls, ExtCtrls, LResources, memscan,
  commonTypeDefs, math, win32proc, symbolhandler;

const wm_fw_scandone=wm_user+1;
type

  { TFindWindow }

  TFindWindow = class(TForm)
    scanvalue: TMemo;
    ProgressBar: TProgressBar;
    Panel1: TPanel;
    labelType: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    labelArray: TLabel;
    editStart: TEdit;
    EditStop: TEdit;
    rbText: TRadioButton;
    rbArByte: TRadioButton;
    cbUnicode: TCheckBox;
    gripper: TScrollBar;
    Timer1: TTimer;
    btnOK: TButton;
    btnCancel: TButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure scanvalueKeyDown(Sender: TObject; var Key: Word;
        Shift: TShiftState);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    memscan: TMemScan;
    procedure scandone(Sender: TObject);

  public
    { Public declarations }
    firstscan: boolean;
  end;

var
  FindWindow: TFindWindow;

implementation

uses MemoryBrowserFormUnit, ProcessHandlerUnit, Parsers, windows;

resourcestring
  rsNothingFound = 'Nothing found';
  rsTheSpecifiedRangeIsInvalid = 'The specified range is invalid';


procedure TFindWindow.scandone(sender: TObject);
var x: ptruint;
begin
  if TMemScan(sender).GetOnlyOneResult(x) then
  begin
    MemoryBrowser.memoryaddress:=x;
    modalresult:=mrok;
  end else
  begin
    MessageDlg(rsNothingFound, mtError, [mbok], 0);
  end;

  if memscan=sender then
    freeandnil(memscan);

  btnOK.enabled:=true;
end;

procedure TFindWindow.btnOKClick(Sender: TObject);
var startaddress,stopaddress: ptruint;
    valtype: TVariableType;
    i: integer;

    scantext,tmp:string;
    a:char;
begin
  if memscan<>nil then
    freeandnil(memscan);
  
  try
    startaddress:=StrToQWordEx('$'+editstart.text);
  except
    startaddress:=symhandler.getAddressFromName(editstart.text);
  end;

  try
    stopaddress:=StrToQWordEx('$'+editstop.text);
  except
    stopaddress:=symhandler.getAddressFromName(editstop.text);
  end;

  if(rbText.checked)then
    valtype:=vtString
  else
    valtype:=vtByteArray;

  if startaddress>stopaddress then
  begin  //xor swap
    startaddress:=startaddress xor stopaddress;
    stopaddress:=stopaddress xor startaddress;
    startaddress:=startaddress xor stopaddress;
  end;

  memscan:=TMemscan.create(nil);
  memscan.onlyone:=true;
  memscan.scanCopyOnWrite:=scanDontCare;
  memscan.scanExecutable:=scanDontCare;
  memscan.scanWritable:=scanDontCare;

  if(valtype=vtByteArray)then
  begin
    scantext:='';
    tmp:=UpperCase(scanvalue.text);
    for i:=Low(tmp) to High(tmp) do
    begin
        a:=tmp[i];
        if(((a>='0') and (a<='9')) or ((a>='A') and (a<='F')))then
            scantext:=scantext+a;
    end;
  end
  else
    scantext:=scanvalue.text;

  btnOK.enabled:=false;
end;

procedure TFindWindow.FormDestroy(Sender: TObject);
    begin
    if memscan<>nil then
      freeandnil(memscan);
  end;

end;

procedure TFindWindow.FormCreate(Sender: TObject);
var style:DWORD;
begin
  style:=GetWindowLong(gripper.Handle,GWL_STYLE);
  style:=style or SBS_SIZEGRIP;
  SetWindowLong(gripper.Handle,GWL_STYLE,style);
end;

procedure TFindWindow.btnCancelClick(Sender: TObject);
begin

end;

procedure TFindWindow.FormShow(Sender: TObject);
const EM_GETMARGINS=$d4;
var m: dword;

begin
  if firstscan then
  begin

    if WindowsVersion>=wvVista then
      m:=sendmessage(editstart.Handle, EM_GETMARGINS, 0,0)
    else
      m:=0;

    if processhandler.is64bit then
    begin
       //init just once if needed
       if (editstop.Text = '') or (editstart.Text = '') then  // if not initialized
       begin
      editstop.text:='7FFFFFFFFFFFFFFF';
          editstart.Text:='0000000000000000';
       end;
      editstart.clientwidth:=canvas.TextWidth('DDDDDDDDDDDDDDDD')+(m shr 16)+(m and $ffff);
    end
    else
    begin
      //init just once if needed
      if (editstop.Text = '') or (editstart.Text = '') then  // if not initialized
      begin
         editstop.text:='7FFFFFFF';
         editstart.Text:='00000000';
      end;
      editstart.clientwidth:=canvas.TextWidth('DDDDDDDD')+(m shr 16)+(m and $ffff);
    end;

    labelType.visible:=true;
    labelarray.visible:=true;
    rbtext.visible:=true;
    rbArByte.visible:=true;
    scanvalue.Visible:=true;
    btnOK.visible:=true;
    btnCancel.visible:=true;
    EditStop.visible:=true;
    editStart.visible:=true;
    label2.Visible:=true;
    label3.Visible:=true;
    Scanvalue.SetFocus;
  end
  else
  begin
    labelType.visible:=false;
    labelarray.visible:=false;
    rbtext.visible:=false;
    rbArByte.visible:=false;
    scanvalue.Visible:=false;
    btnOK.visible:=false;
    btnCancel.visible:=false;
    EditStop.visible:=false;
    editStart.visible:=false;
    label2.Visible:=false;
    label3.Visible:=false;
  end;

  btnok.AutoSize:=true;
  btnok.AutoSize:=false;

  btnCancel.autosize:=true;
  btnCancel.autosize:=false;

  btnok.width:=max(btnok.width, btncancel.width);
  btncancel.width:=max(btnok.width, btncancel.width);
end;

procedure TFindWindow.scanvalueKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
    if(key=Ord('A'))then
    begin
      if(ssCtrl in Shift)then
        scanvalue.SelectAll;
    end
    else if(VK_ESCAPE=key)then
    begin
      self.Close;
    end;
end;

procedure TFindWindow.Timer1Timer(Sender: TObject);
begin
  timer1.enabled:=false;
  btnOK.click;
  close;
end;

initialization
  {$i FindWindowUnit.lrs}

end.





















