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

    MufasaBase for the Mufasa Macro Library
}

unit mufasabase;

{$mode objfpc}{$H+}

interface
{$undef mDebug}

uses
  Classes, SysUtils{$ifdef MSWindows},windows{$endif};

//{$I Simba.inc}

const
    SimbaVersion = 1000;
    SimbaMajor = 1000; // this should be 980 even if SimbaVersion is 981, etc

    SimbaURL =     {$IFDEF WINDOWS}
                    {$IFDEF CPUI386}
                    'http://simba.villavu.com/bin/Windows/x86/Stable/'
                    {$IFDEF NOTPORTABLE}+ 'SystemWide/'{$ENDIF}
                    {$ELSE}
                    'http://simba.villavu.com/bin/Windows/x86_64/Stable/'
                    {$IFDEF NOTPORTABLE}+ 'SystemWide/'{$ENDIF}
                    {$ENDIF}
                  {$ELSE}
                    {$IFDEF CPUI386}
                    'http://simba.villavu.com/bin/Linux/x86/Stable/'
                    {$IFDEF NOTPORTABLE}+ 'SystemWide/'{$ENDIF}
                    {$ELSE}
                    'http://simba.villavu.com/bin/Linux/x86_64/Stable/'
                    {$IFDEF NOTPORTABLE}+ 'SystemWide/'{$ENDIF}
                    {$ENDIF}
                  {$ENDIF};
    FontURL = 'http://simba.villavu.com/bin/Fonts/';

procedure mDebugLn( s : string);overload;
procedure mDebugLn( s : string; f : array of const);overload;
procedure InitmDebug;
procedure FreemDebug;
implementation

var
  CanDebug : boolean = false;

procedure mDebugLn(s: string);
begin
  if CanDebug then
    Writeln(s);
end;

procedure mDebugLn(s: string; f: array of const); overload;
begin
  mDebugLn(format(s,f));
end;

procedure InitmDebug;
begin
  CanDebug := true;
end;

procedure FreemDebug;
begin
  CanDebug := false;
end;

end.

