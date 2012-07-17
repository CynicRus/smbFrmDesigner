unit main; 

//{$mode objfpc}{$H+}
{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  stdctrls, ExtCtrls, Menus, LCLIntf, LCLType, LMessages, Grids, design_frm,
  types,sclist,StrUtils,code;

type
  TControlsClassStandard = array [0..8] of TComponentClass;
  THControl = Class(TControl);


  { TCompForm }

  TCompForm = class(TForm)
    ppEdit: TEdit;
    GroupBox1: TGroupBox;
    lv: TListView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    SmbCompImages: TImageList;
    TabSheet1: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure ApplyChClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure lvAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure lvAdvancedCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure lvClick(Sender: TObject);
    procedure lvMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lvSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure OnExit(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
  private
    FButtonDown: TToolButton;
    FControlsClassPStd: TControlsClassStandard;
    function ComponentToSimba(cmp: TControl): TSimbaComponent;
    function GetButtonIndex: Integer;
    procedure ApplySimbaToComponent(smb: TSimbaComponent;cmp: TControl);
    { private declarations }
  public
    CompList: TSimbaComponentList;
    property ButtonIndex:Integer read GetButtonIndex;
    property ButtonDown:TToolButton read FButtonDown write FButtonDown;
    property ControlsClassPStd:TControlsClassStandard read FControlsClassPStd
      write FControlsClassPStd;
    function GetControlType(cmp: TControl): integer;
    procedure AddToStringGrid(cmp: TControl);//not used now
    procedure FormToSCList(form: TForm);
    procedure AddToStringGridEx(smb: TSimbaComponent);//не забыть проверку свойства tag
    procedure UpdateControlData();
    procedure SetControl(Sender: TObject);
    function MouseClickOnSubItem(rc: TRect; item: TListItem;x,y: integer): boolean;
  end;

var
  CompForm: TCompForm;
  ecomp: TComponent;
//  eitem: TListItem;//editing item
  curcomp: TControl;
  f: TDsgnForm;
  codefrm: TCodeGen;
  curitem: TComponent;
implementation

{$R *.lfm}
uses typinfo,rttiutils,commctrl;

function TrimCharLeft(const S: string; C: Char): string;
var
  i, l: Integer;
begin
  l := Length(S);
  i := 1;
   while (i <= l) and (S[i] = c) do
    Inc(i);
  Result := Copy(S, i, Maxint);
end;

//*********Properties editor********************
 Procedure GetProperties( Kinds: TTypeKinds; Source: TComponent; List: TStrings);
 var
   pList: PPropList;
   pInfo: TPropInfoList;
   Total, Props, i: Integer;
 begin
   PInfo:=TPropInfoList.Create(Source,Kinds);
   Total := GetTypeData(Source.ClassInfo).PropCount;
   GetMem(pList, sizeof(PPropInfo) * Total);
   try
   // Props := GetPropList( Source.ClassInfo, Kinds, pList );
     Props:=pInfo.Count;
      for i := 0 to Props-1 do
        begin
           if Pinfo.Items[i].Name = 'Caption' then
           List.AddObject(Pinfo.Items[i].Name, TObject(Pinfo.Items[i]));
           if Pinfo.Items[i].Name = 'Width' then
           List.AddObject(Pinfo.Items[i].Name, TObject(Pinfo.Items[i]));
           if Pinfo.Items[i].Name = 'Height' then
           List.AddObject(Pinfo.Items[i].Name, TObject(Pinfo.Items[i]));
           if Pinfo.Items[i].Name = 'Left' then
           List.AddObject(Pinfo.Items[i].Name, TObject(Pinfo.Items[i]));
           if Pinfo.Items[i].Name = 'Top' then
           List.AddObject(Pinfo.Items[i].Name, TObject(Pinfo.Items[i]));
           //if Pinfo.Items[i].Name = 'Font' then
           //List.AddObject(Pinfo.Items[i].Name, TObject(Pinfo.Items[i]));
           if Pinfo.Items[i].Name = 'Name' then
           List.AddObject(Pinfo.Items[i].Name, TObject(Pinfo.Items[i]));
        end;
   finally
    FreeMem(pList, sizeof(PPropInfo) * Total);
  end;
 end;

 function GetProp(Cntrl: TObject; PropInfo: PPropInfo ): string;
 begin
  Result:= GetPropValue(Cntrl, PropInfo^.Name, true);
 end;

//
function ReplaceStr(const S, Srch, Replace: string): string;
var
  i: Integer;
  Source: string;
begin
  Source := S;
  Result := '';
  repeat
    i := Pos(UpperCase(Srch), UpperCase(Source));
    if i > 0 then
    begin
      Result := Result + Copy(Source, 1, i - 1) + Replace;
      Source := Copy(Source, i + Length(Srch), MaxInt);
    end
    else
      Result := Result + Source;
  until i <= 0;
end;

{ TCompForm }

procedure TCompForm.FormCreate(Sender: TObject);
begin
  CompList:=TSimbaComponentList.Create;
  Self.FControlsClassPStd[0] := nil;
  Self.FControlsClassPStd[1] := TLabel;
  Self.FControlsClassPStd[2] := TEdit;
  Self.FControlsClassPStd[3] := TImage;
  Self.FControlsClassPStd[4] := TButton;
  Self.FControlsClassPStd[5] := TCheckBox;
  Self.FControlsClassPStd[6] := TListBox;
  Self.FControlsClassPStd[7] := TComboBox;
  Self.FControlsClassPStd[8] := TProgressBar;
  //eitem:=nil;
  f:=TDsgnForm.Create(self);
  f.Parent:=CompForm.Panel1;
  f.Left:=0;
  f.Top:=0;
 // f.OnShow:=SetControl;
  f.Show;
  ppEdit.OnExit:=OnExit;
 // AddToStringGrid(f);
 CompList.AddItem(ComponentToSimba(f),0);
end;

procedure TCompForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 //ppEdit.Free;
 if Assigned(f) then
  f.Free;
end;

procedure TCompForm.ApplyChClick(Sender: TObject);
begin
  SetControl(Sender);
end;


procedure TCompForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  cmp: TComponent;
begin
  if (Key = VK_DELETE) then begin
  //if (sor.Selected) and (Assigned(sor.SelectControl)) then begin
 // FindComponent(sor.SelectControl.name).Destroy;
    cmp:=f.sor.SelectControl;
    f.sor.Selected:=false;
    f.sor.SelectControl:=nil;
    //Assigned(cmp);
   // showmessage(cmp.name);
   cmp.Free;

  end;
end;

procedure TCompForm.FormKeyPress(Sender: TObject; var Key: char);
begin

end;

procedure TCompForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
   Msg: Cardinal;
   Code: Cardinal;
   I, ScrollLines: Integer;
   ScrollBoxCursosPos: TPoint;
 begin   //position of the mouse cursor related to TScrollBox
     ScrollBoxCursosPos := scrollbox1.ScreenToClient(Mouse.CursorPos);
     if (PtInRect(scrollbox1.ClientRect, ScrollBoxCursosPos)) then // мышь(курсор) над ScrollBox
     begin
       Handled := True;
       If ssShift In Shift Then //зажат  Shift ?
         msg := LM_HSCROLL// да - горизонтально
       Else
         msg := LM_VSCROLL; //  нет - вертикальный скрол
      If WheelDelta < 0 Then // направление "кручения" колеса мыши
         code := SB_LINEDOWN
       Else
         code := SB_LINEUP;
       ScrollLines:= Mouse.WheelScrollLines * 3; // сколько строк крутим(чувствительность)
       for I:= 1 to ScrollLines do  // собственно курчение
         scrollbox1.Perform(Msg, Code, 0);
       scrollbox1.Perform(Msg, SB_ENDSCROLL, 0); // завершение прокрутки
     end;
 end;

procedure TCompForm.FormShow(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
 // DsgnForm.Show;
end;

procedure TCompForm.lvAdvancedCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  if Stage = cdPrePaint then
  begin
      Sender.Canvas.Brush.Color := clWhite;
  end else Sender.Canvas.Brush.Color := clBtnFace;
end;

procedure TCompForm.lvAdvancedCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  if Stage = cdPrePaint then
  begin
    if SubItem = 1 then
      Sender.Canvas.Brush.Color := clWhite
    else
      Sender.Canvas.Brush.Color := clBtnFace;
  end;
end;

procedure TCompForm.lvClick(Sender: TObject);
begin

end;

procedure TCompForm.lvMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  _i : integer;
  _rect: TRect;
  _item: TListItem;
  _left: integer;
begin
  try
  _item := lv.GetItemAt(5, Y);
   if not Assigned(_item) then exit;
  _rect := _item.DisplayRect(drLabel);
   if Assigned(_item) then
       with lv do begin
        _left := Columns[0].Width;
       for _i := 1 to Columns.Count - 1 do
        if (X > _left) and (X <= _left + Columns[_i].Width) then
        begin
         _rect.Left := _left;
         _rect.Right:= _left + Columns[_i].Width;
          ppedit.Tag := _item.Index*100 + _i-1;
          ppedit.Text := _item.SubItems[_i-1];
          ppedit.SetBounds(_rect.Left + Left, _rect.Top + Top, _rect.Right - _rect.Left, _rect.Bottom - _rect.Top);
          ppedit.Visible := true;
          ppedit.SetFocus;
          Exit;
       end
         else
        inc(_left, lv.Columns[_i].Width)
end
   except
end;
 end;





procedure TCompForm.lvSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin


end;


procedure TCompForm.MenuItem10Click(Sender: TObject);
begin
  if Assigned(codefrm) then
  codefrm.Free;
  codefrm:=TCodeGen.Create(self);
  //CompList.ChangeElement(ComponentToSimba(f),0);
  //ShowMessage(IntToStr(CompList.Count));
  FormToSCList(f);
  codefrm.CreateScript(CompList);
  codefrm.Show;
end;

procedure TCompForm.MenuItem2Click(Sender: TObject);
begin
  if Assigned(f) then
   f.Free;
  if assigned(complist) then
   complist.Free;
  complist:=TSimbaComponentList.Create;
  f:=TDsgnForm.Create(self);
  f.Parent:=CompForm.Panel1;
  f.Left:=0;
  f.Top:=0;
  f.Show;
end;


procedure TCompForm.OnExit(Sender: TObject);
begin
 try
 lv.Items[ppEdit.Tag div 100].SubItems[ppEdit.Tag mod 100] := ppEdit.Text;
 SetPropValue( curitem, lv.Items[ppEdit.Tag div 100].Caption, lv.Items[ppEdit.Tag div 100].SubItems[ppEdit.Tag mod 100]);
 ppEdit.Visible := false;
 except
 ppEdit.Visible:=false;
 end;
end;



procedure TCompForm.ToolButton1Click(Sender: TObject);
begin
 f.sor.Selected:=false;
  f.sor.SelectControl:=nil;
  Self.FButtonDown := nil;
  if (Sender is TToolButton) then begin
    Self.FButtonDown := TToolButton(Sender);
   end;
end;


function TCompForm.GetButtonIndex: Integer;
begin
 Result := -1;
  if Assigned(Self.FButtonDown) then begin
    Result := Self.FButtonDown.Tag
  end;
end;
function TCompForm.GetControlType(cmp: TControl): integer;
begin
 Result := -1;
 if Assigned(cmp) then
  begin
   if CompareText(cmp.ClassName, 'TDsgnForm') = 0 then
    result:=0;
   if CompareText(cmp.ClassName, 'TLabel') = 0 then
    result:=1;
   if CompareText(cmp.ClassName, 'TEdit') = 0 then
    result:=2;
   if CompareText(cmp.ClassName, 'TImage') = 0 then
    result:=3;
   if CompareText(cmp.ClassName, 'TButton') = 0 then
    result:=4;
   if CompareText(cmp.ClassName, 'TCheckBox') = 0 then
    result:=5;
   if CompareText(cmp.ClassName, 'TListBox') = 0 then
    result:=6;
   if CompareText(cmp.ClassName, 'TComboBox') = 0 then
    result:=7;
   if CompareText(cmp.ClassName, 'TProgressBar') = 0 then
    result:=8;
 end;
end;

function TCompForm.ComponentToSimba(cmp: TControl): TSimbaComponent;
var
   smb: TSimbaComponent;
begin
   smb.caption:=cmp.Caption;
   smb.top:=cmp.Top;
   smb.width:=cmp.Width;
   smb.left:=cmp.Left;
   smb.heigth:=cmp.Height;
   smb.classname:=cmp.ClassName;
   smb.compname:=cmp.Name;
   smb.fontcolor:=cmp.Font.Color;
   if (CompareText(cmp.Font.Name,'default'))=0 then
   smb.fontname:=cmp.Font.Name else smb.fontname:=#39+cmp.Font.Name+#39;
   smb.fontsize:=cmp.Font.Size;
   if (CompareText(cmp.ClassName,'TImage')) = 0 then
    begin
     smb.img.switcher:=true;
     smb.img.path:=String(PString(cmp.Tag));
    end;
   result:=smb;
end;

procedure TCompForm.ApplySimbaToComponent(smb: TSimbaComponent; cmp: TControl);
begin
 cmp.Caption:=smb.caption;
 cmp.Top:=smb.top;
 cmp.Width:=smb.width;
 cmp.Left:=smb.left;
 cmp.Height:=smb.heigth;
 cmp.Font.Color:=smb.fontcolor;
 cmp.Font.Name:=smb.fontname;
 cmp.Font.Size:=smb.fontsize;
end;

procedure TCompForm.AddToStringGrid(cmp: TControl);
begin
  //cursmb:=ComponentToSimba(cmp);
 // CompList.AddItem(smb,0);
 curcomp:=cmp;
  {with StringGrid1 do begin
    Cells[1,1]:=curcomp.ClassName;
    Cells[1,2]:=curcomp.caption;
    Cells[1,3]:=IntToStr(curcomp.top);
    Cells[1,4]:=IntToStr(curcomp.left);
    Cells[1,5]:=IntToStr(curcomp.width);
    Cells[1,6]:=IntToStr(curcomp.Height);
    Cells[1,7]:=curcomp.Font.Name;
    cells[1,8]:=IntToStr(curcomp.font.size);
    Cells[1,9]:=ColorToString(curcomp.font.color);
  end;}
end;

procedure TCompForm.FormToSCList(form: TForm);
var
   i: integer;
begin
 if assigned(complist) then
   complist.Free;
  complist:=TSimbaComponentList.Create;
  complist.AddItem(ComponentToSimba(form),0);
  for i := 0 to form.ControlCount - 1 do
   begin
     CompList.AddItem(ComponentToSimba(Form.Controls[i]),i+1);
   end;
end;

procedure TCompForm.AddToStringGridEx(smb: TSimbaComponent);
begin
 { with StringGrid1 do begin
    Cells[1,1]:PropList:=TStringList.Create;
  curitem:=TPersistent(Sender);
  GetProperties([tkInteger,tkString, tkChar, tkEnumeration, tkFloat,
     tkSet, tkWChar, tkLString,tkUString,tkUChar, tkWString,
    tkVariant, tkArray, tkInt64 ],curitem, PropList);
  PropList.SaveToFile('C:/test.txt');          =smb.ClassName;
    Cells[1,2]:=smb.caption;
    Cells[1,3]:=IntToStr(smb.top);
    Cells[1,4]:=IntToStr(smb.left);
    Cells[1,5]:=IntToStr(smb.width);
    Cells[1,6]:=IntToStr(smb.heigth);
    Cells[1,7]:=smb.fontname;
    cells[1,8]:=IntToStr(smb.fontsize);
    Cells[1,9]:=ColorToStr(smb.fontcolor);
  end;}
end;


procedure TCompForm.UpdateControlData();
begin

end;

procedure TCompForm.SetControl(Sender: TObject);
var
   PropList: TStringList;
   i,j: integer;
   Kinds: TTypeKinds;
begin
  lv.Clear;
  PropList:=TStringList.Create;
  curitem:=TComponent(Sender);
   //curitem:=f;
 // GetProperties([tkInteger,tkString,tkClass, tkChar, tkEnumeration, tkFloat,
 //    tkSet, tkWChar, tkLString,tkUString,tkUChar, tkWString,
 //  tkVariant, tkArray, tkInt64 ],curitem, PropList);
   GetProperties(tkProperties,CurItem,PropList);
   //GetProperties(Kinds,curitem,PropList);
 // PropList.SaveToFile('C:/test.txt');
  for i:= 0 to PropList.count-1 do begin
    with lv.Items.Add do begin
      Caption:= PropList[i];
      try
        SubItems.Add(GetPropValue(curitem, PropList[i]));
      except
      end;
    end;
end;
end;

function TCompForm.MouseClickOnSubItem(rc: TRect; item: TListItem; x, y: integer
  ): boolean;
begin

end;










end.

