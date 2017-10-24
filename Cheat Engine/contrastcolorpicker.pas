unit ContrastColorPicker;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TColorContrastPicker }

  TColorContrastPicker = class(TForm)
    btnOK: TButton;
    btnCANCEL: TButton;
    btnCUSTOM: TButton;
    cbCOLORS: TComboBox;
    ColorDialog1: TColorDialog;
    Label1: TLabel;
    lblCONTRAST1: TLabel;
    lblCONTRAST2: TLabel;
    lblCONTRAST3: TLabel;
    lblCONTRAST4: TLabel;
    lbl4: TLabel;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    procedure btnCANCELClick(Sender: TObject);
    procedure btnCUSTOMClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure cbCOLORSChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    procedure UpdateContrasts;
    procedure UpdateColorList;
  public
    CurrentHighlightColor:Tcolor;
  end;

var
  ColorContrastPicker: TColorContrastPicker;

implementation
uses windows,math;

type TColorEntry=record
    val:integer;
    name:string;
end;
const standard_colors:array[0..25] of TColorEntry =(
(val:clBlack;           name:'#000000';),
(val:clNone;            name:'None';),
(val:clHighlight;       name:'Highlight';),
(val:clHotLight;        name:'HotLight';),
(val:clBlack;			name:'Black';),
(val:clMaroon;			name:'Maroon';),
(val:clGreen;			name:'Green';),
(val:clOlive;			name:'Olive';),
(val:clNavy;			name:'Navy';),
(val:clPurple;			name:'Purple';),
(val:clTeal;			name:'Teal';),
(val:clGray;			name:'Gray';),
(val:clSilver;			name:'Silver';),
(val:clRed;				name:'Red';),
(val:clLime;			name:'Lime';),
(val:clYellow;			name:'Yellow';),
(val:clBlue;			name:'Blue';),
(val:clFuchsia;			name:'Fuchsia';),
(val:clAqua;			name:'Aqua';),
(val:clLtGray;			name:'LtGray';),
(val:clDkGray;			name:'DkGray';),
(val:clWhite;			name:'White';),
(val:clMoneyGreen;		name:'MoneyGreen';),
(val:clSkyBlue;			name:'SkyBlue';),
(val:clCream;			name:'Cream';),
(val:clMedGray;			name:'MedGray';)
);


{$R *.lfm}

function GetContrast(color_a,color_b:TColor):double;
  function GetComponent(e:TColor;i:integer):integer;
  begin
    //0=B,1=G,2=R
    result:=e shr (i*8);
    result:=result and $FF;
  end;
  function getsRGB(c:double):double;
  begin
    c:=c / 255;
    if(c<=0.03928)then
      result:=c/12.92
    else
      result:=Power((c+0.055)/1.055,2.4);
  end;
  function getL(e:TColor):double;
  begin
    //tcolor is BGR; sRGB formula below is RGB
    result:=(0.2126 * getsRGB(GetComponent(e,0))) + (0.7152 * getsRGB(GetComponent(e,1))) + (0.0722 * getsRGB(GetComponent(e,2)));
  end;
  function get_contrast(x,y:double):double;
  var a,b:double;
  begin
    a:=math.max(x,y);
    b:=math.min(x,y);
    result:=(a+0.05)/(b+0.05);
  end;
begin
  result:=get_contrast(getL(color_a),getL(color_b));
end;

function GetConvertedColor(c:TColor):TColor;
begin
  result:=c;
  case c of
    clNone:begin result:=GetSysColor(COLOR_3DFACE); end;
    clHotLight:begin result:=GetSysColor(COLOR_HOTLIGHT); end;
    clHighlight: begin result:=GetSysColor(COLOR_HIGHLIGHT); end;
  end;
end;

procedure TColorContrastPicker.UpdateContrasts;
var t_color,bg_color,r_color,converted_color:TColor;
  f:double;
  function get_str(val:double):string;
  begin
    result:=format('contrast=%f',[val]);
  end;
begin
    lbl2.Color:=CurrentHighlightColor;
    lbl3.Font.Color:=clRed;
    lbl4.Font.Color:=clRed;
    lbl4.Color:=CurrentHighlightColor;
    converted_color:=GetConvertedColor(CurrentHighlightColor);

    t_color:=GetSysColor(COLOR_WINDOWTEXT);
    bg_color:=GetSysColor(COLOR_3DFACE);
    r_color:=clRed;
    f:=GetContrast(t_color,bg_color);
    lblCONTRAST1.Caption:=get_str(f);
    f:=GetContrast(t_color,converted_color);
    lblCONTRAST2.Caption:=get_str(f);
    f:=GetContrast(r_color,bg_color);
    lblCONTRAST3.Caption:=get_str(f);
    f:=GetContrast(r_color,converted_color);
    lblCONTRAST4.Caption:=get_str(f);
end;

procedure TColorContrastPicker.UpdateColorList;
var i,val:integer;
begin
  if(cbCOLORS.Items.Count<=0)then
        exit;
  for i:=1 to cbCOLORS.Items.Count-1 do
  begin
      val:=Cardinal(cbCOLORS.Items.Objects[i]);
      if(val=CurrentHighlightColor)then
      begin
          cbCOLORS.ItemIndex:=i;
          exit;
      end;
  end;
  cbCOLORS.ItemIndex:=0;
  cbCOLORS.Items.Strings[0]:=Format('BGR=#%.6X',[CurrentHighlightColor]);
  cbCOLORS.Items.Objects[0]:=Pointer(CurrentHighlightColor);
end;

procedure TColorContrastPicker.FormShow(Sender: TObject);
var i:integer;
  s:string;
  val:integer;
begin
    cbCOLORS.Clear;
    for i:=Low(standard_colors) to High(standard_colors) do
    begin
        s:=standard_colors[i].name;
        val:=standard_colors[i].val;
        cbCOLORS.AddItem(s,Pointer(val));
    end;
    UpdateColorList;
    UpdateContrasts;
end;

procedure TColorContrastPicker.btnOKClick(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

procedure TColorContrastPicker.cbCOLORSChange(Sender: TObject);
var i,val:integer;
begin
  i:=cbCOLORS.ItemIndex;
  if(i<0)then
    exit;
  val:=Cardinal(cbCOLORS.Items.Objects[i]);
  CurrentHighlightColor:=val;
  UpdateContrasts;
end;

procedure TColorContrastPicker.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(key=VK_ESCAPE)then
    ModalResult:=mrCancel;
end;

procedure TColorContrastPicker.btnCANCELClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TColorContrastPicker.btnCUSTOMClick(Sender: TObject);
begin
  if(ColorDialog1.Execute)then
  begin
    CurrentHighlightColor:=ColorDialog1.Color;
    UpdateContrasts;
    UpdateColorList;
  end;
end;

end.

