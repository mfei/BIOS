%let progname = tlc006.sas;
* input: tlc.dat
* output:
* xref:
* does: Various models

*******************************************************************;
title1 "&progname Treatment of Lead Exposed Children (TLC) Trial";

filename INF   "tlc.dat";
ods pdf file = "tlc006.pdf";

*******************************************************************;

data A;
  infile INF  firstobs=2;
  input id group $ lead0 lead1 lead4 lead6;
  y = lead0; time = 0; output;
  y = lead1; time = 1; output;
  y = lead4; time = 4; output;
  y = lead6; time = 6; output;
  keep id y time group;

  label id    = "Subject ID"
  label y     = "Blood lead level (mg/dL)";
  label time  = "Time: 0, 1, 4, 6 (weeks)";
  label group = "Group: A=active, P=placebo";
  run;

*******************************************************************;

* Define indicator variables;
data A;
  set A;
  t0 = (time = 0);
  t1 = (time = 1);
  t4 = (time = 4);
  t6 = (time = 6);
  t146 = 1 - t0;
  timex = time;
  active = (group = "A");

  label t0 = "I(Time=0)";
  label t1 = "I(Time=1)";
  label t4 = "I(Time=4)";
  label t6 = "I(Time=6)";
  label t146   = "I(Time = 1 or 4 or 6)";
  label timex  = "Time: 0, 1, 4, 6 (weeks)";
  label active = "I(Active group)";
  run;

*******************************************************************;

title2 "M1";
proc mixed data = A method = ml;
  class id group time;
  model y = group | time / s;
  repeated time / type = un group = group subject = id r=1,2 rcorr;
  run;

title2 "M2";
proc mixed data = A method = ml;
  class id group time;
  model y = group | time / s;
  repeated time / type = un subject = id r rcorr;
  run;
*******************************************************************;

title2 "M3";
proc mixed data = A method = ml;
  class id group time;
  model y = active timex active * timex / s;
  repeated time / type = un subject = id r rcorr;
  run;

title2 "M4";
proc mixed data = A method = ml;
  class id group time;
  model y =       timex active * timex / s;
  repeated time / type = un subject = id r rcorr;
  run;

title2 "M5";
proc mixed data = A method = ml;
  class id group time;
  model y = group t146 group * t146 / s;
  repeated time / type = un subject = id r rcorr;
  run;

title2 "M6";
proc mixed data = A method = ml;
  class id group time;
  model y =       t146 active * t146 / s;
  repeated time / type = un subject = id r rcorr;
  run;


title2 "M7";
proc mixed data = A method = ml;
  class id group time;
  model y = t1 t4 t6 t1 * active t4 * active  t6 * active  / s;
  repeated time / type = un subject = id r rcorr;
  run;

