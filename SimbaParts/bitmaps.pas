{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2012 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

    Bitmaps class for the Mufasa Macro Library
}

unit bitmaps;

{$mode objfpc}{$H+}
{$Inline on}
interface
uses
  Classes, SysUtils, FPImage,IntfGraphics,graphtype,MufasaTypes,MufasaBase,graphics;

type

  { TMufasaBitmap }
  TMufasaBitmap = class(TObject)
  private
    w,h : integer;
    FTransparentColor : TRGB32;
    FTransparentSet : boolean;
    FIndex : integer;
    FName : string;

    { True if we do not own FData }
    FExternData: boolean;
  public
    OnDestroy : procedure(Bitmap : TMufasaBitmap) of object;
    //FakeData : array of TRGB32;
    FData : PRGB32;
    property Name : string read FName write FName;
    property Index : integer read FIndex write FIndex;

    procedure SetSize(AWidth,AHeight : integer);
    procedure StretchResize(AWidth,AHeight : integer);

    property Width : Integer read w;
    property Height : Integer read h;

    procedure SetPersistentMemory(mem: PtrUInt; awidth, aheight: integer);
    procedure ResetPersistentMemory;

    function PointInBitmap(x,y : integer) : boolean;
    procedure ValidatePoint(x,y : integer);
    function SaveToFile(const FileName : string) :boolean;
    procedure LoadFromFile(const FileName : string);
    procedure Rectangle(const Box : TBox;FillCol : TColor);
    procedure FloodFill(const StartPT : TPoint; const SearchCol, ReplaceCol : TColor);
    procedure FastSetPixel(x,y : integer; Color : TColor);
    procedure FastSetPixels(Points : TPointArray; Colors : TIntegerArray);
    procedure DrawATPA(ATPA : T2DPointArray; Colors : TIntegerArray);overload;
    procedure DrawATPA(ATPA : T2DPointArray);overload;
    procedure DrawTPA(Points : TPointArray; Color : TColor);
    procedure DrawToCanvas(x,y : integer; Canvas : TCanvas);
    function CreateTPA(SearchCol : TColor) : TPointArray;
    function FastGetPixel(x,y : integer) : TColor;
    function FastGetPixels(Points : TPointArray) : TIntegerArray;
    function GetAreaColors(xs,ys,xe,ye : integer) : T2DIntArray;
    function GetHSLValues(xs, ys, xe, ye: integer): T2DHSLArray;
    procedure FastDrawClear(Color : TColor);
    procedure FastDrawTransparent(x, y: Integer; TargetBitmap: TMufasaBitmap);
    procedure FastReplaceColor(OldColor, NewColor: TColor);
    procedure CopyClientToBitmap(MWindow : TObject;Resize : boolean; xs, ys, xe, ye: Integer);overload;
    procedure CopyClientToBitmap(MWindow : TObject;Resize : boolean;x,y : integer; xs, ys, xe, ye: Integer);overload;
    procedure RotateBitmap(angle: Extended;TargetBitmap : TMufasaBitmap );
    procedure Desaturate(TargetBitmap : TMufasaBitmap); overload;
    procedure Desaturate;overload;
    procedure GreyScale(TargetBitmap : TMufasaBitmap);overload;
    procedure GreyScale;
    procedure Brightness(TargetBitmap : TMufasaBitmap; br : integer); overload;
    procedure Brightness(br: integer);overload;
    procedure Contrast(TargetBitmap : TMufasaBitmap; co : Extended);overload;
    procedure Contrast(co: Extended);overload;
    procedure Invert(TargetBitmap : TMufasaBitmap);overload;
    procedure Invert;overload;
    procedure Posterize(TargetBitmap : TMufasaBitmap; Po : integer);overload;
    procedure Posterize(Po : integer);overload;
    procedure Convolute(TargetBitmap : TMufasaBitmap; Matrix : T2DExtendedArray);
    function Copy(const xs,ys,xe,ye : integer) : TMufasaBitmap; overload;
    function Copy: TMufasaBitmap;overload;
    function ToTBitmap: TBitmap;
    function ToString : string;
    function RowPtrs : TPRGB32Array;
    procedure LoadFromTBitmap(bmp: TBitmap);
    procedure LoadFromRawImage(RawImage: TRawImage);
    function CreateTMask : TMask;
    procedure SetTransparentColor(Col : TColor);
    function GetTransparentColor : TColor;
    property TransparentColorSet : boolean read FTransparentSet;
    procedure SetAlphaValue(const value : byte);
    constructor Create;
    destructor Destroy;override;
  end;
  PMufasaBitmap = ^TMufasaBitmap;
  TMufasaBmpArray = Array of TMufasaBitmap;
  { TMBitmaps }
  TMBitmaps = class(TObject)
  protected
    Client : TObject;
    FreeSpots : Array of integer;
    BmpArray : TMufasaBmpArray;
    BmpsCurr,BmpsHigh,FreeSpotsHigh,FreeSpotsLen : integer;
    function GetNewIndex : integer;
  public
    function GetBMP(Index : integer) : TMufasaBitmap;
    property Bmp[Index : integer]: TMufasaBitmap read GetBMP; default;
    function CreateBMP(w, h: integer): Integer;
    function ExistsBMP(Index : integer) : boolean;
    function AddBMP(_bmp: TMufasaBitmap): Integer;
    function CopyBMP( Bitmap : integer) : Integer;
    function CreateMirroredBitmap(bitmap: Integer; MirrorStyle : TBmpMirrorStyle): Integer;
    function CreateBMPFromFile(const Path : string) : integer;
    function CreateBMPFromString(width,height : integer; Data : string) : integer;overload;
    function CreateBMPFromString(BmpName : string; width,height : integer; Data : string) : integer;overload;
    procedure FreeBMP( Number : integer);
    constructor Create(Owner : TObject);
    destructor Destroy;override;
  end;

  Procedure ArrDataToRawImage(Ptr: PRGB32; Size: TPoint; out RawImage: TRawImage);
  function CalculatePixelShift(Bmp1,Bmp2 : TMufasaBitmap; CompareBox : TBox) : integer;
  function CalculatePixelShiftTPA(Bmp1, Bmp2: TMufasaBitmap; CPoints: TPointArray): integer;
  function CalculatePixelTolerance(Bmp1,Bmp2 : TMufasaBitmap; CompareBox : TBox; CTS : integer) : extended;
  function CalculatePixelToleranceTPA(Bmp1, Bmp2: TMufasaBitmap; CPoints: TPointArray; CTS: integer): extended;
implementation

uses
  paszlib,DCPbase64,math, client,tpa,
  colour_conv,IOManager,mufasatypesutil,FileUtil;

// Needs more fixing. We need to either copy the memory ourself, or somehow
// find a TRawImage feature to skip X bytes after X bytes read. (Most likely a
// feature)
Procedure ArrDataToRawImage(Ptr: PRGB32; Size: TPoint; out RawImage: TRawImage);
Begin
  RawImage.Init; { Calls raw.Description.Init as well }

  RawImage.Description.PaletteColorCount:=0;
  RawImage.Description.MaskBitsPerPixel:=0;
  RawImage.Description.Width := Size.X;
  RawImage.Description.Height:= Size.Y;

  RawImage.Description.Format := ricfRGBA;
  RawImage.Description.ByteOrder := riboLSBFirst;
  RawImage.Description.BitOrder:= riboBitsInOrder; // should be fine
  RawImage.Description.Depth:=24;
  RawImage.Description.BitsPerPixel:=32;
  RawImage.Description.LineOrder:=riloTopToBottom;
  RawImage.Description.LineEnd := rileDWordBoundary;

  RawImage.Description.RedPrec := 8;
  RawImage.Description.GreenPrec:= 8;
  RawImage.Description.BluePrec:= 8;
  RawImage.Description.AlphaPrec:=0;


  RawImage.Description.RedShift:=16;
  RawImage.Description.GreenShift:=8;
  RawImage.Description.BlueShift:=0;

  RawImage.DataSize := RawImage.Description.Width * RawImage.Description.Height
                       * (RawImage.Description.bitsperpixel shr 3);
  RawImage.Data := PByte(Ptr);
End;

function CalculatePixelShift(Bmp1, Bmp2: TMufasaBitmap; CompareBox: TBox): integer;
var
  x,y : integer;
  w1,w2 : integer;
begin
  Bmp1.ValidatePoint(comparebox.x1,comparebox.y1);
  Bmp1.ValidatePoint(comparebox.x2,comparebox.y2);
  Bmp2.ValidatePoint(comparebox.x1,comparebox.y1);
  Bmp2.ValidatePoint(comparebox.x2,comparebox.y2);
  Bmp1.SetAlphaValue(0);
  Bmp2.SetAlphaValue(0);
  w1 := bmp1.Width;
  w2 := bmp2.width;
  result := 0;
  for y := CompareBox.y1 to CompareBox.y2 do
    for x := CompareBox.x1 to CompareBox.x2 do
      if LongWord(Bmp1.FData[y * w1 + x]) <> LongWord(Bmp2.Fdata[y * w2 + x]) then
        inc(result);
end;

function CalculatePixelShiftTPA(Bmp1, Bmp2: TMufasaBitmap; CPoints: TPointArray): integer;
var
  i : integer;
  bounds: TBox;
  w1,w2 : integer;
begin
  bounds := GetTPABounds(CPoints);
  Bmp1.ValidatePoint(bounds.x1,bounds.y1);
  Bmp1.ValidatePoint(bounds.x2,bounds.y2);
  Bmp2.ValidatePoint(bounds.x1,bounds.y1);
  Bmp2.ValidatePoint(bounds.x2,bounds.y2);
  Bmp1.SetAlphaValue(0);
  Bmp2.SetAlphaValue(0);
  w1 := bmp1.width;
  w2 := bmp2.width;
  result := 0;
  for i := 0 to High(CPoints) do
    if LongWord(Bmp1.FData[CPoints[i].y * w1 + CPoints[i].x]) <>
        LongWord(Bmp2.Fdata[CPoints[i].y * w2 + CPoints[i].x]) then
      inc(result);
end;

//CTS 0 counts the average difference in R,G,B per pixel
//CTS 1 counts the average difference using SQRT(Sqr(r) + sqr(g)+sqr(b));
function CalculatePixelTolerance(Bmp1, Bmp2: TMufasaBitmap; CompareBox: TBox;
  CTS: integer): extended;
var
  x,y : integer;
  w1,w2 : integer;
  Diff : int64;
begin
  Bmp1.ValidatePoint(comparebox.x1,comparebox.y1);
  Bmp1.ValidatePoint(comparebox.x2,comparebox.y2);
  Bmp2.ValidatePoint(comparebox.x1,comparebox.y1);
  Bmp2.ValidatePoint(comparebox.x1,comparebox.y1);
  Bmp1.SetAlphaValue(0);
  Bmp2.SetAlphaValue(0);
  w1 := bmp1.Width;
  w2 := bmp2.width;
  result := 0;
  if not InRange(CTS,0,1) then
    raise exception.CreateFmt('CTS Passed to CalculateTolerance must be in [0..1], it currently is %d',[CTS]);
  case CTS of
    0 : begin
          Diff := 0;
          for y := CompareBox.y1 to CompareBox.y2 do
            for x := CompareBox.x1 to CompareBox.x2 do
            begin
              Diff := Diff + abs(Bmp1.FData[y * w1 + x].r-Bmp2.Fdata[y * w2 + x].r) +
                             abs(Bmp1.FData[y * w1 + x].g-Bmp2.Fdata[y * w2 + x].g) +
                             abs(Bmp1.FData[y * w1 + x].b-Bmp2.Fdata[y * w2 + x].b);
            end;
          Result := Diff / (3 * (CompareBox.x2 - CompareBox.x1 + 1) * (CompareBox.y2-CompareBox.y1 + 1)); //We want the value for the whole Pixel; so divide by 3 (RGB)
        end;
    1 : begin
          for y := CompareBox.y1 to CompareBox.y2 do
            for x := CompareBox.x1 to CompareBox.x2 do
              Result := Result + Sqrt(Sqr(Bmp1.FData[y * w1 + x].r-Bmp2.Fdata[y * w2 + x].r) +
                                      Sqr(Bmp1.FData[y * w1 + x].g-Bmp2.Fdata[y * w2 + x].g) +
                                      Sqr(Bmp1.FData[y * w1 + x].b-Bmp2.Fdata[y * w2 + x].b));
          Result := Result / ((CompareBox.x2 - CompareBox.x1 + 1) * (CompareBox.y2-CompareBox.y1 + 1)); //We want the value for the whole Pixel;
        end;
  end;
end;

function CalculatePixelToleranceTPA(Bmp1, Bmp2: TMufasaBitmap; CPoints: TPointArray;
  CTS: integer): extended;
var
  i : integer;
  bounds: TBox;
  w1,w2 : integer;
  Diff : int64;
begin
  bounds := GetTPABounds(CPoints);
  Bmp1.ValidatePoint(bounds.x1,bounds.y1);
  Bmp1.ValidatePoint(bounds.x2,bounds.y2);
  Bmp2.ValidatePoint(bounds.x1,bounds.y1);
  Bmp2.ValidatePoint(bounds.x2,bounds.y2);
  Bmp1.SetAlphaValue(0);
  Bmp2.SetAlphaValue(0);
  w1 := bmp1.Width;
  w2 := bmp2.width;
  result := 0;
  if not InRange(CTS,0,1) then
    raise exception.CreateFmt('CTS Passed to CalculateTolerance must be in [0..1], it currently is %d',[CTS]);
  case CTS of
    0 : begin
          Diff := 0;
          for i := 0 to High(CPoints) do
            begin
              Diff := Diff + abs(Bmp1.FData[CPoints[i].y * w1 + CPoints[i].x].r-Bmp2.Fdata[CPoints[i].y * w2 + CPoints[i].x].r) +
                             abs(Bmp1.FData[CPoints[i].y * w1 + CPoints[i].x].g-Bmp2.Fdata[CPoints[i].y * w2 + CPoints[i].x].g) +
                             abs(Bmp1.FData[CPoints[i].y * w1 + CPoints[i].x].b-Bmp2.Fdata[CPoints[i].y * w2 + CPoints[i].x].b);
            end;
          Result := Diff / (3 * (bounds.x2 - bounds.x1 + 1) * (bounds.y2-bounds.y1 + 1)); //We want the value for the whole Pixel; so divide by 3 (RGB)
        end;
    1 : begin

          for i := 0 to High(CPoints) do
            Result := Result + Sqrt(Sqr(Bmp1.FData[CPoints[i].y * w1 + CPoints[i].x].r-Bmp2.Fdata[CPoints[i].y * w2 + CPoints[i].x].r) +
                                    Sqr(Bmp1.FData[CPoints[i].y * w1 + CPoints[i].x].g-Bmp2.Fdata[CPoints[i].y * w2 + CPoints[i].x].g) +
                                    Sqr(Bmp1.FData[CPoints[i].y * w1 + CPoints[i].x].b-Bmp2.Fdata[CPoints[i].y * w2 + CPoints[i].x].b));
          Result := Result / ((bounds.x2 - bounds.x1 + 1) * (bounds.y2-bounds.y1 + 1)); //We want the value for the whole Pixel;
        end;
  end;
end;

function Min(a,b:integer) : integer;
begin
  if a < b then
    result := a
  else
    result := b;
end;

{ TMBitmaps }

function TMBitmaps.GetNewIndex: integer;
begin
  if BmpsCurr < BmpsHigh then
  begin;
    inc(BmpsCurr);
    Result := BmpsCurr;
  end else if (FreeSpotsHigh > -1) then
  begin;
    Result := FreeSpots[FreeSpotsHigh];
    dec(FreeSpotsHigh);
  end else
  begin;
    SetLength(BmpArray, BmpsHigh + 6);
    BmpsHigh := BmpsHigh + 5;
    inc(BmpsCurr);
    Result := BmpsCurr;
  end;
end;

function TMBitmaps.GetBMP(Index: integer): TMufasaBitmap;
begin
  Result := nil;
  if (Index >= 0) and (Index <= BmpsCurr) then
    if BmpArray[Index] <> nil then
      Result := BmpArray[Index];
  if Result = nil then
    raise Exception.CreateFmt('The bitmap[%d] does not exist',[Index]);
end;

function TMBitmaps.CreateBMP(w,h : integer): Integer;
begin
  result := GetNewIndex;
  BmpArray[Result] := TMufasaBitmap.Create;
  BmpArray[Result].SetSize(w,h);
  BmpArray[Result].Index:= Result;
end;

function TMBitmaps.AddBMP(_bmp: TMufasaBitmap): Integer;
begin
  Result := GetNewIndex;
  BmpArray[Result] := _bmp;
  BmpArray[result].Index:= Result;
end;

function TMBitmaps.CopyBMP(Bitmap: integer): Integer;
var
  InputBMP : TMufasaBitmap;
  OutputBMP : TMUfasaBitmap;
begin
  InputBMP := GetBMP(Bitmap);
  Result := CreateBMP(InputBmp.w,InputBMP.h);
  OutputBMP := GetBMP(Result);
  Move(InputBMP.FData[0],OutPutBMP.FData[0],InputBMP.w * InputBMP.h * SizeOf(TRGB32));
end;

function TMBitmaps.CreateMirroredBitmap(bitmap: Integer;
  MirrorStyle: TBmpMirrorStyle): Integer;
var
  w,h : integer;
  y,x : integer;
  Source,Dest : PRGB32;
begin
  Source := Bmp[Bitmap].FData;
  w := BmpArray[Bitmap].Width;
  h := BmpArray[Bitmap].Height;
  if MirrorStyle = MirrorLine then
    Result := CreateBMP(h,w)
  else
    Result := CreateBMP(w,h);
  Dest := BmpArray[Result].FData;
  case MirrorStyle of
    MirrorWidth :  for y := (h-1) downto 0 do
                     for x := (w-1) downto 0 do
                       Dest[y*w+x] := Source[y*w+w-1-x];
    MirrorHeight : for y := (h-1) downto 0 do
                    Move(Source[y*w],Dest[(h-1 - y) * w],w*SizeOf(TRGB32));
    MirrorLine :  for y := (h-1) downto 0 do
                     for x := (w-1) downto 0 do
                       Dest[x*h+y] := Source[y*w+x];

  end;
//Can be optmized, this is just proof of concept
end;

function TMBitmaps.CreateBMPFromFile(const Path: string): integer;
begin
  Result := CreateBMP(0,0);
  try
    BmpArray[result].LoadFromFile(Path);
  except
    FreeBMP(Result);
    Result := -1; // meh
    raise;
  end;
end;

function HexToInt(HexNum: string): LongInt;inline;
begin
   Result:=StrToInt('$' + HexNum);
end;

function TMBitmaps.ExistsBMP(Index : integer) : boolean;
begin
  result := false;
  if (Index >= 0) and (Index <= BmpsCurr) then
      result := Assigned(BmpArray[Index]);
end;

function TMBitmaps.CreateBMPFromString(width, height: integer; Data: string): integer;
var
  I,II: LongWord;
  DestLen : LongWord;
  Dest,Source : string;
  DestPoint, Point : PByte;
  MufRaw : PRGB24;
  MufDest : PRGB32;


begin
  Result := CreateBMP(width,height);
  if (Data <> '') and (Length(Data) <> 6) then
  begin;
    Point := Pointer(BmpArray[Result].FData);
    if (Data[1] = 'b') or (Data[1] = 'm') then
    begin;
      Source := Base64DecodeStr(Copy(Data,2,Length(Data) - 1));
      Destlen := Width * Height * 3;
      Setlength(Dest,DestLen);
      if uncompress(PChar(Dest),Destlen,pchar(Source), Length(Source)) = Z_OK then
      begin;
        if data[1] = 'm' then //Our encrypted bitmap! Winnor.
        begin
          MufRaw:= @Dest[1];
          MufDest:= PRGB32(Point);
          for i := width * height - 1 downto 0 do
          begin
            MufDest[i].R:= MufRaw[i].R;
            MufDest[i].G := MufRaw[i].G;
            MufDest[i].B := MufRaw[i].B;
          end;
        end else
        if Data[1] = 'b'then
        begin
          DestPoint := @Dest[1];
          i := 0;
          ii := 2;
          Dec(DestLen);
          if DestLen > 2 then
          begin;
            while (ii < DestLen) do
            Begin;
              Point[i]:= DestPoint[ii+2];
              Point[i+1]:= DestPoint[ii+1];
              Point[i+2]:= DestPoint[ii];
              ii := ii + 3;
              i := i + 4;
            end;
            Point[i] := DestPoint[1];
            Point[i+1] := DestPoint[0];
            Point[i+2] := DestPoint[ii];
          end else if (Width = 1) and (Height =1 ) then
          begin;
            Point[0] := DestPoint[1];
            Point[1] := DestPoint[0];
            Point[2] := DestPoint[2];
          end;
        end;
      end;
    end else if Data[1] = 'z' then
    begin;
      Destlen := Width * Height * 3 *2;
      Setlength(Dest,DestLen);
      ii := (Length(Data) - 1) div 2;
      SetLength(Source,ii);
      for i := 1 to ii do
        Source[i] := Chr(HexToInt(Data[i * 2] + Data[i * 2+1]));
      if uncompress(PChar(Dest),Destlen,pchar(Source), ii) = Z_OK then
      begin;
        ii := 1;
        i := 0;
        while (II < DestLen) do
        begin;
          Point[i+2]:= HexToInt(Dest[ii] + Dest[ii + 1]);
          Point[i+1]:= HexToInt(Dest[ii+2] + Dest[ii + 3]);
          Point[i]:= HexToInt(Dest[ii+4] + Dest[ii + 5]);
          ii := ii + 6;
          i := i + 4;
        end;
      end;
    end else if LongWord(Length(Data)) = LongWord((Width * Height * 3 * 2)) then
    begin;
      ii := 1;
      i := 0;
      Destlen := Width * Height * 3 * 2;
      while (II < DestLen) do
      begin;
        Point[i+2]:= HexToInt(Data[ii] + Data[ii + 1]);
        Point[i+1]:= HexToInt(Data[ii+2] + Data[ii + 3]);
        Point[i]:= HexToInt(Data[ii+4] + Data[ii + 5]);
        ii := ii + 6;
        i := i + 4;
      end;
    end;
  end else
  begin;
    if Length(data) = 6 then
      BmpArray[Result].FastDrawClear(HexToInt(Data));
//    else
//      FastDrawClear(Result,clBlack);
  end;
end;

function TMBitmaps.CreateBMPFromString(BmpName: string; width, height: integer;
  Data: string): integer;
begin
  Result := Self.CreateBMPFromString(width,height,data);
  Bmp[Result].Name:= BmpName;

end;

procedure TMBitmaps.FreeBMP(Number: integer);
var
  ToDestroy : TMufasaBitmap;
begin
  ToDestroy := GetBMP(Number);
  if Number = BmpsCurr then
    Dec(BmpsCurr)
  else
  begin;
    inc(FreeSpotsHigh);
    if FreeSpotsHigh = FreeSpotsLen then
    begin;
      inc(FreeSpotsLen);
      SetLength(FreeSpots, FreeSpotsLen);
    end;
    FreeSpots[FreeSpotsHigh] := Number;
  end;
  //Just for testing purposes
  {$ifdef mDebug}
  if ToDestroy.BmpName = '' then
    mDebug(Format('BMP[%d] has been freed.',[number]))
  else
    mDebug(Format('BMP[%s] has been freed.',[ToDestroy.BmpName]));
  {$endif}
  ToDestroy.Free;
  BmpArray[number] := nil;
end;

{
  Saves a bitmap to a file.
  If Filename ends with 'png' it will save as a PNG file,
  if Filename ends with 'jpg' or 'jpeg' it will save as JPG,
  otherwise BMP is assumed.
}

function TMufasaBitmap.SaveToFile(const FileName: string): boolean;
var
  rawImage : TRawImage;
  Bmp : TLazIntfImage;
  png: TPortableNetworkGraphic;
  jpg: TJPEGImage;
begin
  ArrDataToRawImage(FData,Point(w,h),RawImage);
  result := true;
  try
    if RightStr(Filename, 3) = 'png' then
    begin
      png := TPortableNetworkGraphic.Create;
      png.LoadFromRawImage(RawImage, False);
      png.SaveToFile(UTF8ToSys(Filename));
      png.Free;
    end else if (RightStr(Filename, 3) = 'jpg') or (RightStr(Filename, 4) = 'jpeg') then
    begin
      jpg := TJPEGImage.Create;
      jpg.LoadFromRawImage(RawImage, False);
      jpg.SaveToFile(UTF8ToSys(Filename));
      jpg.Free;
    end else // Assume .bmp
    begin
      Bmp := TLazIntfImage.Create(RawImage,false);
      Bmp.SaveToFile(UTF8ToSys(FileName));
      Bmp.Free;
    end;
  except
    result := false;
  end;
end;

procedure TMufasaBitmap.LoadFromFile(const FileName: string);
var
  LazIntf : TLazIntfImage;
  RawImageDesc : TRawImageDescription;
begin
  try
    LazIntf := TLazIntfImage.Create(0,0);
    RawImageDesc.Init_BPP32_B8G8R8_BIO_TTB(LazIntf.Width,LazIntf.Height);
    LazIntf.DataDescription := RawImageDesc;
    LazIntf.LoadFromFile(UTF8ToSys(FileName));
    if Assigned(FData) then
      Freemem(FData);
    Self.W := LazIntf.Width;
    Self.H := LazIntf.Height;
    FData := GetMem(Self.W*Self.H*SizeOf(TRGB32));
    Move(LazIntf.PixelData[0],FData[0],w*h*sizeOf(TRGB32));
  finally
    LazIntf.Free;
  end;
end;

function RGBToBGR(Color : TColor) : TRGB32; inline;
begin;
  Result.R := Color and $ff;
  Result.G := Color shr 8 and $ff;
  Result.B := Color shr 16 and $ff;
  Result.A := 0;
end;

procedure TMufasaBitmap.Rectangle(const Box: TBox;FillCol: TColor);
var
  y : integer;
  Col : Longword;
  Size : longword;
begin
  if (Box.x1 < 0) or (Box.y1 < 0) or (Box.x2 >= self.w) or (Box.y2 >= self.h) then
    raise exception.Create('The Box you passed to Rectangle exceed the bitmap''s bounds');
  if (box.x1 > box.x2) or (Box.y1 > box.y2) then
    raise exception.CreateFmt('The Box you passed to Rectangle doesn''t have normal bounds: (%d,%d) : (%d,%d)',
                               [Box.x1,box.y1,box.x2,box.y2]);
  col :=  Longword(RGBToBGR(FillCol));
  Size := Box.x2 - box.x1 + 1;
  for y := Box.y1 to Box.y2 do
    FillDWord(FData[y * self.w + Box.x1],size,Col);
end;

procedure TMufasaBitmap.FloodFill(const StartPT: TPoint; const SearchCol,
  ReplaceCol: TColor);
var
  Stack : TPointArray;
  SIndex : Integer;
  CurrX,CurrY : integer;
  Search,Replace : LongWord;
procedure AddToStack(x,y : integer);inline;
begin
  if LongWord(FData[y * w + x]) = Search then
  begin
    LongWord(FData[y * w + x]) := Replace;
    Stack[SIndex].x := x;
    Stack[SIndex].y := y;
    inc(SIndex);
  end;
end;
begin
  ValidatePoint(StartPT.x,StartPT.y);
  Search := LongWord(RGBToBGR(SearchCol));
  Replace := LongWord(RGBToBGR(ReplaceCol));
  SetAlphaValue(0);
  if LongWord(FData[StartPT.y * w + StartPT.x]) <> Search then //Only add items to the stack that are the searchcol.
    Exit;
  SetLength(Stack,w * h);
  SIndex := 0;
  AddToStack(StartPT.x,StartPT.y);
  SIndex := 0;
  while (SIndex >= 0) do
  begin;
    CurrX := Stack[SIndex].x;
    Curry := Stack[SIndex].y;
    if (CurrX > 0) and (CurrY > 0)         then AddToStack(CurrX - 1, CurrY - 1);
    if (CurrX > 0)                         then AddToStack(CurrX - 1, CurrY);
    if (CurrX > 0) and (CurrY + 1 < h)     then AddToStack(CurrX - 1, CurrY + 1);
    if (CurrY + 1 < h)                     then AddToStack(CurrX   ,  CurrY + 1);
    if (CurrX + 1 < w) and (CurrY + 1 < h) then AddToStack(CurrX + 1, CurrY + 1);
    if (CurrX + 1 < w)                     then AddToStack(CurrX + 1, CurrY    );
    if (CurrX + 1 < w) and (CurrY > 0)     then AddToStack(CurrX + 1, CurrY - 1);
    if (CurrY > 0)                         then AddToStack(CurrX    , CurrY - 1);
    Dec(SIndex);
  end;
end;

function TMufasaBitmap.Copy: TMufasaBitmap;
begin
  Result := TMufasaBitmap.Create;
  Result.SetSize(self.Width, self.Height);
  Move(self.FData[0], Result.FData[0],self.w * self.h * SizeOf(TRGB32));
end;

function TMufasaBitmap.Copy(const xs, ys, xe, ye: integer): TMufasaBitmap;
var
  i : integer;
begin
  ValidatePoint(xs,ys);
  ValidatePoint(xe,ye);
  Result := TMufasaBitmap.Create;
  Result.SetSize(xe-xs+1, ye-ys+1);
  for i := ys to ye do
    Move(self.FData[i * self.w + xs], Result.FData[(i-ys) * result.w],result.Width * SizeOf(TRGB32));
end;

function TMufasaBitmap.ToTBitmap: TBitmap;

var
  tr:TRawImage;

begin
  Result := TBitmap.Create;
  ArrDataToRawImage(Self.Fdata, point(self.width,self.height), tr);
  Result.LoadFromRawImage(tr, false);
end;

function TMufasaBitmap.ToString: string;
var
  i : integer;
  DestLen : longword;
  DataStr : string;
  CorrectData : PRGB24;
begin
  SetLength(DataStr,w*h*3);
  CorrectData:= PRGB24(@DataStr[1]);
  for i := w*h - 1 downto 0 do
  begin
    CorrectData[i].R := FData[i].R;
    CorrectData[i].G := FData[i].G;
    CorrectData[i].B := FData[i].B;
  end;
  DestLen := BufferLen;
  if compress(BufferString,destlen,PChar(DataStr),w*h*3) = Z_OK then
  begin;
    SetLength(DataStr,DestLen);
    move(bufferstring[0],dataStr[1],DestLen);
    result := 'm' + Base64EncodeStr(datastr);
    SetLength(datastr,0);
  end;
end;

function TMufasaBitmap.RowPtrs: TPRGB32Array;
var
  I : integer;
begin;
  setlength(result,h);
  for i := 0 to h - 1 do
    result[i] := FData + w * i;
end;

procedure TMufasaBitmap.LoadFromRawImage(RawImage: TRawImage);

var
  x,y: integer;
  _24_old_p: PByte;
  rs,gs,bs:byte;
  data: PRGB32;


begin
  // clear data
  Self.SetSize(0,0);

  if (RawImage.Description.BitsPerPixel <> 24) and (RawImage.Description.BitsPerPixel <> 32) then
    raise Exception.CreateFMT('TMufasaBitmap.LoadFromRawImage - BitsPerPixel is %d', [RawImage.Description.BitsPerPixel]);

  {writeln('Bits per pixel: ' + Inttostr(RawImage.Description.BitsPerPixel));   }
  if RawImage.Description.LineOrder <> riloTopToBottom then
    raise Exception.Create('TMufasaBitmap.LoadFromRawImage - LineOrder is not riloTopToBottom');

 { writeln(format('LineOrder: theirs: %d, ours: %d', [RawImage.Description.LineOrder, riloTopToBottom]));  }


 // Todo, add support for other alignments.
 { if RawImage.Description.LineEnd <> rileDWordBoundary then
    raise Exception.Create('TMufasaBitmap.LoadFromRawImage - LineEnd is not rileDWordBoundary');         }

  //writeln(format('LineEnd: t', [RawImage.Description.LineEnd]));

  if RawImage.Description.Format<>ricfRGBA then
    raise Exception.Create('TMufasaBitmap.LoadFromRawImage - Format is not ricfRGBA');

  // Set w,h and alloc mem.
  Self.SetSize(RawImage.Description.Width, RawImage.Description.Height);

  {writeln(format('Image size: %d, %d', [w,h]));  }
  rs := RawImage.Description.RedShift shr 3;
  gs := RawImage.Description.GreenShift shr 3;
  bs := RawImage.Description.BlueShift shr 3;
 { writeln(format('Shifts(R,G,B): %d, %d, %d', [rs,gs,bs]));
  writeln(format('Bits per line %d, expected: %d',
  [RawImage.Description.BitsPerLine, RawImage.Description.BitsPerPixel * self.w]));
  }


  if RawImage.Description.BitsPerPixel = 32 then
    Move(RawImage.Data[0], Self.FData[0],  self.w * self.h * SizeOf(TRGB32))
  else
  begin
    //FillChar(Self.FData[0], self.w * self.h * SizeOf(TRGB32), 0);
    data := self.FData;

    _24_old_p := RawImage.Data;
    for y := 0 to self.h -1 do
    begin
      for x := 0 to self.w -1 do
      begin
        // b is the first byte in the record.
        data^.b := _24_old_p[bs];
        data^.g := _24_old_p[gs];
        data^.r := _24_old_p[rs];
        data^.a := 0;

        inc(_24_old_p, 3);
        inc(data);
      end;

      case RawImage.Description.LineEnd of
        rileTight, rileByteBoundary: ; // do nothing
        rileWordBoundary:
          while (_24_old_p - RawImage.Data) mod 2 <> 0 do
            inc(_24_old_p);
        rileDWordBoundary:
          while (_24_old_p - RawImage.Data) mod 4 <> 0 do
            inc(_24_old_p);
        rileQWordBoundary:
          while (_24_old_p - RawImage.Data) mod 4 <> 0 do
            inc(_24_old_p);
        rileDQWordBoundary:
          while (_24_old_p - RawImage.Data) mod 8 <> 0 do
            inc(_24_old_p);
        end;
    end;
  end;
end;

procedure TMufasaBitmap.LoadFromTBitmap(bmp: TBitmap);

begin
//  bmp.BeginUpdate();
  LoadFromRawImage(bmp.RawImage);
//  bmp.EndUpdate();
end;

procedure TMufasaBitmap.FastSetPixel(x, y: integer; Color: TColor);
begin
  ValidatePoint(x,y);
  FData[y*w+x] := RGBToBGR(Color);
end;

procedure TMufasaBitmap.FastSetPixels(Points: TPointArray; Colors: TIntegerArray);
var
  i,len : integer;
  Box : TBox;
begin
  len := High(Points);
  if Len <> High(colors) then
    Raise Exception.CreateFMT('TPA/Colors Length differ',[]);
  Box := GetTPABounds(Points);
  if (Box.x1 < 0) or (Box.y1 < 0) or (Box.x2 >= self.w) or (Box.y2 >= self.h) then
    raise exception.Create('The Points you passed to FastSetPixels exceed the bitmap''s bounds')
  else
    for i := 0 to len do
      FData[Points[i].y * w + Points[i].x] := RGBToBGR(Colors[i]);
end;

procedure TMufasaBitmap.DrawATPA(ATPA: T2DPointArray; Colors: TIntegerArray);
var
  lenTPA,lenATPA : integer;
  i,ii : integer;
  Color : TRGB32;
  Box : TBox;
begin
  lenATPA := High(ATPA);
  if LenATPA <> High(colors) then
    Raise Exception.CreateFMT('TPA/Colors Length differ -> %d : %d',[LenATPA + 1,High(Colors) + 1]);
  for i := 0 to lenATPA do
  begin;
    lenTPA := High(ATPA[i]);
    Color := RGBToBGR(Colors[i]);
    Box := GetTPABounds(ATPA[i]);
    if (Box.x1 < 0) or (Box.y1 < 0) or (Box.x2 >= self.w) or (Box.y2 >= self.h) then
      raise exception.Create('The Points you passed to DrawATPA exceed the bitmap''s bounds')
    else
      for ii := 0 to lenTPA do
        FData[ATPA[i][ii].y * w + ATPA[i][ii].x] := Color;
  end;
end;


procedure TMufasaBitmap.DrawATPA(ATPA: T2DPointArray);
var
  Colors : TIntegerArray;
  i,len : integer;
begin
  len := high(ATPA);
  SetLength(colors,len+1);
  for i := 0 to len do
    Colors[i] := Random(clwhite);
  DrawATPA(ATPA,Colors);
end;

procedure TMufasaBitmap.DrawTPA(Points: TPointArray; Color: TColor);
begin
  DrawATPA(ConvArr([Points]),ConvArr([Color]));
end;

procedure TMufasaBitmap.DrawToCanvas(x,y : integer; Canvas: TCanvas);
var
  Bitmap : Graphics.TBitmap;
begin
  Bitmap := Self.ToTBitmap;
  Canvas.Draw(x,y,Bitmap);
  Bitmap.free;
end;

function TMufasaBitmap.CreateTPA(SearchCol: TColor): TPointArray;
var
  x,y,L : Integer;
  StartPtr : PRGB32;
  Search : TRGB32;
begin
  SetLength(Result,self.Width * Self.Height);
  L := 0;
  Search := RGBToBGR(SearchCol);
  StartPtr := Self.FData;
  For y := 0 to Self.h - 1 do
    For x := 0 to self.w - 1 do
    begin
      StartPtr^.A := 0;
      if LongWord(StartPtr^) = LongWord(Search) then
      begin;
        L := L + 1;
        Result[L].x := x;
        Result[L].y := y;
      end;
    end;
  SetLength(Result,L + 1);
end;




function TMufasaBitmap.FastGetPixel(x, y: integer): TColor;
begin
  ValidatePoint(x,y);
  Result := BGRToRGB(FData[y*w+x]);
end;

function TMufasaBitmap.FastGetPixels(Points: TPointArray): TIntegerArray;
var
  i,len : integer;
  Box  : TBox;
begin
  len := high(Points);
  Box := GetTPABounds(Points);
  if (Box.x1 < 0) or (Box.y1 < 0) or (Box.x2 >= self.w) or (Box.y2 >= self.h) then
    raise exception.Create('The Points you passed to FastGetPixels exceed the bitmap''s bounds');
  SetLength(result,len+1);
  for i := 0 to len do
    Result[i] := BGRToRGB(FData[Points[i].y*w + Points[i].x]);
end;

function TMufasaBitmap.GetAreaColors(xs, ys, xe, ye : integer): T2DIntArray;
var
  x,y : integer;
begin
  ValidatePoint(xs,ys);
  ValidatePoint(xe,ye);
  setlength(result,xe-xs+1,ye-ys+1);
  for x := xs to xe do
    for y := ys to ye do
      result[x-xs][y-ys] := BGRToRGB(FData[y*w+x]);
end;

function TMufasaBitmap.GetHSLValues(xs, ys, xe, ye: integer): T2DHSLArray;
var
  x, y: integer;
  R, G, B, C: integer;
begin
  ValidatePoint(xs,ys);
  ValidatePoint(xe,ye);
  setlength(result,ye-ys+1,xe-xs+1);
  for y := ys to ye do
    for x := xs to xe do
    begin                                                   { REWRITE THIS }
      RGBToHSL(FData[y*w+x].R, FData[y*w+x].G, FData[y*w+x].B,
               Result[y-ys][x-xs].H, Result[y-ys][x-xs].S,
               Result[y-ys][x-xs].L);
    end;
end;

procedure TMufasaBitmap.SetTransparentColor(Col: TColor);
begin
  self.FTransparentSet:= True;
  self.FTransparentColor:= RGBToBGR(Col);
end;

function TMufasaBitmap.GetTransparentColor: TColor;
begin
  if FTransparentSet then
    Result := BGRToRGB(FTransparentColor)
  else
    raise Exception.CreateFmt('Transparent color for Bitmap[%d] isn''t set',[index]);
end;

procedure TMufasaBitmap.SetAlphaValue(const value: byte);
var
  i : integer;
begin
  for i := w * h - 1 downto 0 do
    FData[i].A:= Value;
end;

procedure TMufasaBitmap.FastDrawClear(Color: TColor);
var
  i : integer;
  Rec : TRGB32;
begin
  Rec := RGBToBGR(Color);
  if h > 0 then
  begin;
    for i := (w-1) downto 0 do
      FData[i] := Rec;
    for i := (h-1) downto 1 do
      Move(FData[0],FData[i*w],w*SizeOf(TRGB32));
  end;
end;

procedure TMufasaBitmap.FastDrawTransparent(x, y: Integer;
  TargetBitmap: TMufasaBitmap);
var
  MinW,MinH,TargetW,TargetH : Integer;
  loopx,loopy : integer;
begin
  TargetBitmap.ValidatePoint(x,y);
  TargetW := TargetBitmap.Width;
  TargetH := TargetBitmap.height;
  MinW := Min(w-1,TargetW-x-1);
  MinH := Min(h-1,TargetH-y-1);
  if FTransparentSet then
  begin;
    for loopy := 0 to MinH do
      for loopx := 0 to MinW do
      begin;
        FData[loopy * w + loopx].A := 0;
        if LongWord(FData[loopy * w + loopx]) <> LongWord(FTransparentColor) then
          TargetBitmap.FData[(loopy + y) * TargetW + loopx + x] := FData[Loopy * w + loopx];
      end;
  end
  else
    for loopy := 0 to MinH do
      Move(FData[loopy*w],TargetBitmap.FData[(loopy+y) * TargetW + x],(MinW+1) * SizeOf(TRGB32));

end;

procedure TMufasaBitmap.FastReplaceColor(OldColor, NewColor: TColor);
var
  OldCol,NewCol : TRGB32;
  i : integer;
begin
  OldCol := RGBToBGR(OldColor);
  NewCol := RGBToBGR(NewColor);
  for i := w*h-1 downto 0 do
  begin
    FData[i].a := 0;
    if LongWord(FData[i]) = LongWord(OldCol) then
      FData[i] := NewCol;
  end;
end;

procedure TMufasaBitmap.CopyClientToBitmap(MWindow : TObject;Resize : boolean; xs, ys, xe, ye: Integer);
var
  y : integer;
  wi,hi : integer;
  PtrRet : TRetData;
begin
  if Resize then
    Self.SetSize(xe-xs+1,ye-ys+1);

  wi := Min(xe-xs + 1,Self.w);
  hi := Min(ye-ys + 1,Self.h);

  PtrRet := TIOManager_Abstract(MWindow).ReturnData(xs,ys,wi,hi);

  for y := 0 to (hi-1) do
    Move(PtrRet.Ptr[y * PtrRet.RowLen], FData[y * self.w],wi * SizeOf(TRGB32));
  TIOManager_Abstract(MWindow).FreeReturnData;
end;

procedure TMufasaBitmap.CopyClientToBitmap(MWindow: TObject; Resize: boolean;
  x, y: integer; xs, ys, xe, ye: Integer);
var
  yy : integer;
  wi,hi : integer;
  PtrRet : TRetData;
begin
  if Resize then
    Self.SetSize(xe-xs+1 + x,ye-ys+1 + y);
  ValidatePoint(x,y);
  wi := Min(xe-xs + 1 + x,Self.w)-x;
  hi := Min(ye-ys + 1 + y,Self.h)-y;
  PtrRet := TIOManager_Abstract(MWindow).ReturnData(xs,ys,wi,hi);
  for yy := 0 to (hi-1) do
    Move(PtrRet.Ptr[yy * (PtrRet.RowLen)], FData[(yy + y) * self.w + x],wi * SizeOf(TRGB32));
  TIOManager_Abstract(MWindow).FreeReturnData;
end;


function RotatePointEdited(p: TPoint; angle, mx, my: Extended): TPoint;

begin
  Result.X := Ceil(mx + cos(angle) * (p.x - mx) - sin(angle) * (p.y - my));
  Result.Y := Ceil(my + sin(angle) * (p.x - mx) + cos(angle) * (p.y- my));
end;

//Scar rotates unit circle-wise.. Oh, scar doesnt update the bounds, so kinda crops ur image.
procedure TMufasaBitmap.RotateBitmap(angle: Extended;TargetBitmap : TMufasaBitmap );
var
  NewW,NewH : integer;
  CosAngle,SinAngle : extended;
  MinX,MinY,MaxX,MaxY : integer;
  i : integer;
  x,y : integer;
  OldX,OldY : integer;
  MiddlePoint : TPoint;
  NewCorners : array[1..4] of TPoint; //(xs,ye);(xe,ye);(xe,ys);(xs,ys)
begin
  MiddlePoint := Point((w-1) div 2,(h-1) div 2);
  CosAngle := Cos(Angle);
  SinAngle := Sin(Angle);
  MinX := MaxInt;
  MinY := MaxInt;
  MaxX := 0;
  MaxY := 0;
  NewCorners[1]:= RotatePointEdited(Point(0,h-1),angle,middlepoint.x,middlepoint.y);
  NewCorners[2]:= RotatePointEdited(Point(w-1,h-1),angle,middlepoint.x,middlepoint.y);
  NewCorners[3]:= RotatePointEdited(Point(w-1,0),angle,middlepoint.x,middlepoint.y);
  NewCorners[4]:= RotatePointEdited(Point(0,0),angle,middlepoint.x,middlepoint.y);
  for i := 1 to 4 do
  begin;
    if NewCorners[i].x > MaxX then
      MaxX := NewCorners[i].x;
    if NewCorners[i].Y > MaxY then
      MaxY := NewCorners[i].y;
    if NewCorners[i].x < MinX then
      MinX := NewCorners[i].x;
    if NewCorners[i].y < MinY then
      MinY := NewCorners[i].y;
  end;
  mDebugLn(Format('Min: (%d,%d) Max : (%d,%d)',[MinX,MinY,MaxX,MaxY]));
  NewW := MaxX - MinX+1;
  NewH := MaxY - MinY+1;
  mDebugLn(format('New bounds: %d,%d',[NewW,NewH]));
  TargetBitmap.SetSize(NewW,NewH);
  for y := NewH - 1 downto 0 do
    for x := NewW - 1 downto 0 do
    begin;
      Oldx := Round(MiddlePoint.x + CosAngle * (x + MinX-MiddlePoint.x) - SinAngle * (y + MinY - MiddlePoint.y));
      Oldy := Round(MiddlePoint.y + SinAngle * (x + MinX-MiddlePoint.x) + CosAngle * (y + MinY-MiddlePoint.y));
      if not ((Oldx <0) or (Oldx >= w) or (Oldy < 0) or (Oldy >= h)) then
        TargetBitmap.FData[ y * NewW + x] := Self.FData[OldY * W + OldX];
    end;
end;

procedure TMufasaBitmap.Desaturate;
var
  I : integer;
  He,Se,Le : extended;
  Ptr : PRGB32;
begin
  Ptr := FData;
  for i := (h*w-1) downto 0 do
  begin;
    RGBToHSL(Ptr^.R,Ptr^.G,Ptr^.B,He,Se,Le);
    HSLtoRGB(He,0.0,Le,Ptr^.R,Ptr^.G,Ptr^.B);
    inc(ptr);
  end;
end;

procedure TMufasaBitmap.Desaturate(TargetBitmap: TMufasaBitmap);
var
  I : integer;
  He,Se,Le : extended;
  PtrOld,PtrNew : PRGB32;
begin
  TargetBitmap.SetSize(w,h);
  PtrOld := Self.FData;
  PtrNew := TargetBitmap.FData;
  for i := (h*w-1) downto 0 do
  begin;
    RGBToHSL(PtrOld^.R,PtrOld^.G,PtrOld^.B,He,Se,Le);
    HSLtoRGB(He,0.0,Le,PtrNew^.R,PtrNew^.G,PtrNew^.B);
    inc(ptrOld);
    inc(PtrNew);
  end;
end;

procedure TMufasaBitmap.GreyScale(TargetBitmap: TMufasaBitmap);
var
  I : integer;
  Lum : byte;
  PtrOld,PtrNew : PRGB32;
begin
  TargetBitmap.SetSize(w,h);
  PtrOld := Self.FData;
  PtrNew := TargetBitmap.FData;
  for i := (h*w-1) downto 0 do
  begin;
    Lum := Round(PtrOld^.r * 0.3 + PtrOld^.g * 0.59 + PtrOld^.b * 0.11);
    PtrNew^.r := Lum;
    PtrNew^.g := Lum;
    PtrNew^.b := Lum;
    inc(ptrOld);
    inc(PtrNew);
  end;
end;

procedure TMufasaBitmap.GreyScale;
var
  I : integer;
  Lum : Byte;
  Ptr: PRGB32;
begin
  Ptr := Self.FData;
  for i := (h*w-1) downto 0 do
  begin;
    Lum := Round(Ptr^.r * 0.3 + Ptr^.g * 0.59 + Ptr^.b * 0.11);
    Ptr^.r := Lum;
    Ptr^.g := Lum;
    Ptr^.b := Lum;
    inc(ptr);
  end;
end;

function BrightnessAdjust(Col:  byte; br : integer): byte;inline;
var
  temp : integer;
begin;
  Temp := Col + Br;
  if temp < 0 then
    temp := 0
  else if temp > 255 then
    temp := 255;
  result := temp;
end;
procedure TMufasaBitmap.Brightness(br: integer);
var
  I : integer;
  Ptr: PRGB32;
begin
  Ptr := Self.FData;
  for i := (h*w-1) downto 0 do
  begin;
    Ptr^.r := BrightnessAdjust(Ptr^.r,br);
    Ptr^.g := BrightnessAdjust(Ptr^.g,br);
    Ptr^.b := BrightnessAdjust(Ptr^.b,br);
    inc(ptr);
  end;
end;

procedure TMufasaBitmap.Brightness(TargetBitmap: TMufasaBitmap; br: integer);
var
  I : integer;
  PtrOld,PtrNew : PRGB32;
begin
  TargetBitmap.SetSize(w,h);
  PtrOld := Self.FData;
  PtrNew := TargetBitmap.FData;
  for i := (h*w-1) downto 0 do
  begin;
    PtrNew^.r := BrightnessAdjust(PtrOld^.r,br);
    PtrNew^.g := BrightnessAdjust(PtrOld^.g,br);
    PtrNew^.b := BrightnessAdjust(PtrOld^.b,br);
    inc(ptrOld);
    inc(PtrNew);
  end;
end;

const
  Grey = 128;
function ContrastAdjust(Col:  byte; co : extended): byte;inline;
var
  temp : integer;
begin;
  Temp := floor((col - Grey) * co) + grey;
  if temp < 0 then
    temp := 0
  else if temp > 255 then
    temp := 255;
  result := temp;
end;

procedure TMufasaBitmap.Contrast(co: Extended);
var
  I : integer;
  Ptr: PRGB32;
begin
  Ptr := Self.FData;
  for i := (h*w-1) downto 0 do
  begin;
    Ptr^.r := ContrastAdjust(Ptr^.r,co);
    Ptr^.g := ContrastAdjust(Ptr^.g,co);
    Ptr^.b := ContrastAdjust(Ptr^.b,co);
    inc(ptr);
  end;
end;

procedure TMufasaBitmap.Contrast(TargetBitmap: TMufasaBitmap; co: Extended);
var
  I : integer;
  PtrOld,PtrNew : PRGB32;
begin
  TargetBitmap.SetSize(w,h);
  PtrOld := Self.FData;
  PtrNew := TargetBitmap.FData;
  for i := (h*w-1) downto 0 do
  begin;
    PtrNew^.r := ContrastAdjust(PtrOld^.r,co);
    PtrNew^.g := ContrastAdjust(PtrOld^.g,co);
    PtrNew^.b := ContrastAdjust(PtrOld^.b,co);
    inc(ptrOld);
    inc(PtrNew);
  end;
end;

procedure TMufasaBitmap.Invert;
var
  i : integer;
begin
  for i := (h*w-1) downto 0 do
  begin;
    Self.FData[i].r := not Self.FData[i].r;
    Self.FData[i].g := not Self.FData[i].g;
    Self.Fdata[i].b := not Self.FData[i].b;
  end;
end;

procedure TMufasaBitmap.Invert(TargetBitmap: TMufasaBitmap);
var
  I : integer;
  PtrOld,PtrNew : PRGB32;
begin
  TargetBitmap.SetSize(w,h);
  PtrOld := Self.FData;
  PtrNew := TargetBitmap.FData;
  for i := (h*w-1) downto 0 do
  begin;
    PtrNew^.r := not PtrOld^.r;
    PtrNew^.g := not PtrOld^.g;
    PtrNew^.b := not PtrOld^.b;
    inc(ptrOld);
    inc(PtrNew);
  end;
end;

procedure TMufasaBitmap.Posterize(TargetBitmap: TMufasaBitmap; Po: integer);
var
  I : integer;
  PtrOld,PtrNew : PRGB32;
begin
  if not InRange(Po,1,255) then
    Raise exception.CreateFmt('Posterize Po(%d) out of range[1,255]',[Po]);
  TargetBitmap.SetSize(w,h);
  PtrOld := Self.FData;
  PtrNew := TargetBitmap.FData;
  for i := (h*w-1) downto 0 do
  begin;
    PtrNew^.r := min(Round(PtrOld^.r / po) * Po, 255);
    PtrNew^.g := min(Round(PtrOld^.g / po) * Po, 255);
    PtrNew^.b := min(Round(PtrOld^.b / po) * Po, 255);
    inc(ptrOld);
    inc(PtrNew);
  end;
end;

procedure TMufasaBitmap.Posterize(Po: integer);
var
  I : integer;
  Ptr: PRGB32;
  {a:integer; }
begin
  if not InRange(Po,1,255) then
    Raise exception.CreateFmt('Posterize Po(%d) out of range[1,255]',[Po]);
  Ptr := Self.FData;
  for i := (h*w-1) downto 0 do
  begin;
   { a := round(ptr^.r / po);
    a := a * po;
    ptr^.r := min(a,255);
    a := round(ptr^.g / po);
    a := a * po;
    ptr^.g := min(a,255);
    a := round(ptr^.b / po);
    a := a * po;
    ptr^.b := min(a,255);      }
    ptr^.r := min(Round(ptr^.r / po) * Po, 255);
    ptr^.g := min(Round(ptr^.g / po) * Po, 255);
    ptr^.b := min(Round(ptr^.b / po) * Po, 255);
    inc(ptr);
  end;
end;

procedure TMufasaBitmap.Convolute(TargetBitmap : TMufasaBitmap; Matrix: T2DExtendedArray);
var
  x,y,xx,yy : integer;
  mw,mh : integer; //Matrix-Width/Matrix-height;
  Row,RowT : TPRGB32Array;
  R,G,B : extended;
  midX,midY : integer;
  xmax,ymax : integer;
begin
  mw := Length(Matrix);
  mh := Length(Matrix[0]);
  if ((mw mod 2) = 0) or ((mh mod 2) = 0) then
    exit;
  TargetBitmap.SetSize(w,h);
  Row := RowPtrs;
  RowT := TargetBitmap.RowPtrs; //Target
  midX := mw div 2;
  midY := mh div 2;
  xmax := w-1-midX;
  ymax := h-1-midY;
  dec(mw); dec(mh); //Faster in loop.
  for x := midx to xmax do
    for y := midy to ymax do
    begin
      R := 0;
      G := 0;
      B := 0;
      for xx := mw downto 0 do // for xx := x - midx to x + midx do
        for yy := mh downto 0 do
        begin
          r := r + Row[y+yy-midy][x+xx-midx].r * Matrix[xx][yy];
          g := g + Row[y+yy-midy][x+xx-midx].g * Matrix[xx][yy];
          b := b + Row[y+yy-midy][x+xx-midx].b * Matrix[xx][yy];
        end;
      RowT[y][x].r := round(r);
      RowT[y][x].g := round(g);
      RowT[y][x].b := round(b);
    end;
end;

function TMufasaBitmap.CreateTMask: TMask;
var
  x,y : integer;
  dX,dY : integer;
begin
  Result.BlackHi:= -1;
  Result.WhiteHi:= -1;
  Result.W := Self.Width;
  Result.H := Self.Height;
  SetLength(result.Black,w*h);
  SetLength(result.White,w*h);
  dX := w-1;
  dY := h-1;
  for y := 0 to dY do
    for x := 0 to dX do
    //Check for non-white/black pixels? Not for now atleast.
      if FData[y*w+x].r = 255 then
      begin;
        inc(Result.WhiteHi);
        Result.White[Result.WhiteHi].x := x;
        Result.White[Result.WhiteHi].y := y;
      end else
      begin;
        inc(Result.BlackHi);
        Result.Black[Result.BlackHi].x := x;
        Result.Black[Result.BlackHi].y := y;
      end;
  SetLength(result.Black,Result.BlackHi+1);
  SetLength(result.White,Result.WhiteHi+1);
end;



constructor TMBitmaps.Create(Owner: TObject);
begin
  inherited Create;
  SetLength(BmpArray,50);
  SetLength(FreeSpots, 50);
  FreeSpotsLen := 50;
  BmpsHigh := 49;
  BmpsCurr := -1;
  FreeSpotsHigh := -1;
  Self.Client := Owner;
end;

destructor TMBitmaps.Destroy;
var
  I : integer;
  WriteStr : string;
begin
  WriteStr := '[';
  for i := 0 to BmpsCurr do
    if BmpArray[i] <> nil then
    begin;
      if BmpArray[i].Name = '' then
        WriteStr := WriteStr + inttostr(i) + ', '
      else
        WriteStr := WriteStr + bmpArray[i].Name + ', ';
      FreeAndNil(BmpArray[i]);
    end;
  if WriteStr <> '[' then  //Has unfreed bitmaps
  begin
    SetLength(WriteStr,length(WriteStr)-1);
    WriteStr[Length(writeStr)] := ']';
    TClient(Client).Writeln(Format('The following bitmaps were not freed: %s',[WriteStr]));
  end;
  SetLength(BmpArray,0);
  SetLength(FreeSpots,0);
  inherited Destroy;
end;


{ TMufasaBitmap }
procedure TMufasaBitmap.SetSize(Awidth, Aheight: integer);
var
  NewData : PRGB32;
  i,minw,minh : integer;
begin
  if FExternData then
    raise Exception.Create('Cannot resize a bitmap with FExternData = True!');

  if (AWidth <> w) or (AHeight <> h) then
  begin
    if AWidth*AHeight <> 0 then
    begin;
      NewData := GetMem(AWidth * AHeight * SizeOf(TRGB32));
      FillDWord(NewData[0],AWidth*AHeight,0);
    end
    else
      NewData := nil;
    if Assigned(FData) and Assigned(NewData) and (w*H <> 0) then
    begin;
      minw := Min(AWidth,w);
      minh := Min(AHeight,h);
      for i := 0 to minh - 1 do
        Move(FData[i*w],Newdata[i*AWidth],minw * SizeOf(TRGB32));
    end;
    if Assigned(FData) then
      FreeMem(FData);
    FData := NewData;
    w := AWidth;
    h := AHeight;
  end;
end;

procedure TMufasaBitmap.StretchResize(AWidth, AHeight: integer);
var
  NewData : PRGB32;
  x,y : integer;
begin
  if FExternData then
    raise Exception.Create('Cannot resize a bitmap with FExternData = True!');

  if (AWidth <> w) or (AHeight <> h) then
  begin;
    if AWidth*AHeight <> 0 then
    begin;
      NewData := GetMem(AWidth * AHeight * SizeOf(TRGB32));
      FillDWord(NewData[0],AWidth*AHeight,0);
    end
    else
      NewData := nil;
    if Assigned(FData) and Assigned(NewData) and (w*H <> 0) then
    begin;
      for y := 0 to AHeight - 1 do
        for x := 0 to AWidth -1 do
          NewData[y*AWidth + x] := FData[((y * h)div aheight) * W+ (x * W) div awidth];
    end;
    if Assigned(FData) then
      FreeMem(FData);
    FData := NewData;
    w := AWidth;
    h := AHeight;
  end;
end;

procedure TMufasaBitmap.SetPersistentMemory(mem: PtrUInt; awidth, aheight: integer);
begin
  SetSize(0, 0);
  FExternData := True;
  w := awidth;
  h := aheight;

  FData := PRGB32(mem);
end;

procedure TMufasaBitmap.ResetPersistentMemory;
begin
  if not FExternData then
    raise Exception.Create('ResetPersistentMemory: Bitmap is not persistent (FExternData = False)');

  FExternData := False;
  FData := nil;

  SetSize(0, 0);
end;

function TMufasaBitmap.PointInBitmap(x, y: integer): boolean;
begin
  result := ((x >= 0) and (x < w) and (y >= 0) and (y < h));
end;

procedure TMufasaBitmap.ValidatePoint(x, y: integer);
begin
  if not(PointInBitmap(x,y)) then
    raise Exception.CreateFmt('You are accessing an invalid point, (%d,%d) at bitmap[%d]',[x,y,index]);
end;

constructor TMufasaBitmap.Create;
begin
  inherited Create;
  Name:= '';
  FTransparentSet:= False;
  SetSize(0,0);

  FExternData := False;

  {FData:= nil;
  w := 0;
  h := 0; }
end;

destructor TMufasaBitmap.Destroy;
begin
  if Assigned(OnDestroy) then
    OnDestroy(Self);

  if Assigned(FData) and not FExternData then
    Freemem(FData);

  inherited Destroy;
end;

end.

