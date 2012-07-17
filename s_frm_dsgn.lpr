program s_frm_dsgn;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, selectonruntime, design_frm, sclist, code;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TCompForm, CompForm);
 // Application.CreateForm(TCodeGen, CodeGen);
 // Application.CreateForm(TDsgnForm, DsgnForm);
  Application.Run;
end.

