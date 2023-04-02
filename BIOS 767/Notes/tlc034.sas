%let progname = tlc034.sas;
%let pdfout   = tlc034.pdf;
* input: tlc.dat
* output:
* xref:
* does: Profile analysis, sections 5.4 and 5.7
        Treatment of Lead Exposed Children Trial (TLC)
        A randomized trial
        Analysis of Response Profiles of Blood Lead Levels
        Illustrates 4 ways of using the baseline response (page 128)

*******************************************************************;
title1 "&progname Treatment of Lead Exposed Children (TLC) Trial";

filename INF "tlc.dat";
ods pdf file="&pdfout";

*******************************************************************;

data A;
  infile INF  firstobs=2;
  input id group $ lead0 lead1 lead4 lead6;
  baseline = lead0;
  y = lead0; time = 0; output;
  y = lead1; time = 1; output;
  y = lead4; time = 4; output;
  y = lead6; time = 6; output;
  drop lead0 lead1 lead4 lead6;
  run;
 

*******************************************************************;
* Chapter 5, Section 5.4;

title2 "1. (T5.3-5.5) Analysis of Response Profiles of data on Blood Lead Levels";

* Comment out the sorting to change reference levels for group and time;

proc sort data=A;
  by group id descending time;
  run;

proc mixed data = A  order=data;
  class id group time;
  model y = group time group*time / s chisq;
  repeated time / type=un subject=id r;
  run;

*******************************************************************;
* Chapter 5, Section 5.7;

* Create dummy variables for group and time;
data A;
  set A;
  succimer = (group = "A");
  t0 = (time = 0);
  t1 = (time = 1);
  t4 = (time = 4);
  t6 = (time = 6);
  run;

title2 "2. (T5.7) Analysis of Response Profiles assuming Equal Mean Blood Lead Levels at Baseline";

proc mixed data = A order=data;
     class id time;
     model y = t1 t4 t6 succimer*t1 succimer*t4 succimer*t6 / s chisq;
     repeated time / type=un subject=id r;
     contrast '3 DF Test of Interaction' 
          succimer*t1 1, succimer*t4 1, succimer*t6 1 / chisq;

run;
 
*******************************************************************;

* Analysis of Response Profiles of Changes in Response from Baseline;

data B;
  set A;
  if (time > 0);
  change    = y - baseline;
  cbaseline = baseline - 26.406;
  label cbaseline = "Centered baseline";
  run;


title2 "3. (T5.8) Analysis of Response Profiles of Changes from Baseline";

proc mixed data=B  order=data;
     class id time;
     model change = succimer t4 t6  succimer*t4 succimer*t6 / s chisq;
     repeated time / type=un subject=id r;
     contrast '3 DF Test of Main Effect and Interaction' 
          succimer 1, succimer*t4 1, succimer*t6 1 / chisq;

run;

*******************************************************************;

* Analysis of Response Profiles of Adjusted Changes in Response from Baseline;

title2 "4. (T5.9) Analysis of Response Profiles of Adjusted Changes from Baseline";

proc mixed data = B order=data;
     class id time;
     model change = cbaseline succimer t4 t6  succimer*t4 succimer*t6 / s chisq;
     repeated time / type=un subject=id r;
     contrast '3 DF Test of Main Effect and Interaction' 
          succimer 1, succimer*t4 1, succimer*t6 1 / chisq;

run;

*******************************************************************;

title2 "5. Unequal cov matrices in the two groups";

proc mixed data = B order=data;
     class id time group;
     model change = cbaseline succimer t4 t6  succimer*t4 succimer*t6 / s chisq;
     repeated time / type=un subject=id r=1,100  group=group;
     contrast '3 DF Test of Main Effect and Interaction' 
          succimer 1, succimer*t4 1, succimer*t6 1 / chisq;

run;

*******************************************************************;

title2 "6. REML likelihood ratio tests for BETA are NOT valid";

proc mixed data = B order=data;
     class id time group;
     model change = cbaseline t4 t6 / s chisq;
     repeated time / type=un subject=id r=1,100  group=group;

run;

*******************************************************************;

title2 "7A. Full likelihood ratio tests for BETA are valid";

proc mixed data = B order=data method=ML;
     class id time group;
     model change = cbaseline succimer t4 t6  succimer*t4 succimer*t6 / s chisq;
     repeated time / type=un subject=id r=1,100  group=group;
     contrast '3 DF Test of Main Effect and Interaction' 
          succimer 1, succimer*t4 1, succimer*t6 1 / chisq;

run;

*******************************************************************;

title2 "7B. Full likelihood ratio tests for BETA are valid";

proc mixed data = B order=data method=ML;
     class id time group;
     model change = cbaseline t4 t6 / s chisq;
     repeated time / type=un subject=id r=1,100  group=group;

run;

*******************************************************************;

title2 "7C. GEE, Wald test for BETA";

proc genmod data = B;
     class id time group;
     model change = cbaseline succimer t4 t6  succimer*t4 succimer*t6;
     repeated subject = id / withinsubject = time type=un corrw;
     contrast '3 DF Test of Main Effect and Interaction' 
          succimer 1, succimer*t4 1, succimer*t6 1 / wald ;

run;

*******************************************************************;
title2 "8. Autoregressive using actual time";

proc mixed data = B order=data;
     class id time group;
     model change = cbaseline succimer t4 t6  succimer*t4 succimer*t6 / s chisq;
     repeated time / type=SP(EXP)(time) subject=id r group=group;
     contrast '3 DF Test of Main Effect and Interaction' 
          succimer 1, succimer*t4 1, succimer*t6 1 / chisq;

run;
ods pdf close;
