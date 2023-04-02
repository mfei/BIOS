%let progname = tlc010.sas;
%let pdfout   = tlc010.pdf;

* input: tlc.dat
* output:
* xref:
* does: The TLC Trial
        A mixed    model with a random intercept
        A marginal model with exch corr

        Mixed model, REML
        Mixed model, MLE
        Marginal model, REML
        Marginal model, MLE
        Marginal model, GEE

        A non-identifiable model

*******************************************************************;
title1 "&progname Treatment of Lead Exposed Children (TLC) Trial";

filename INF   "tlc.dat";
ods pdf file = "&pdfout";

*******************************************************************;

data A;
  infile INF  firstobs=2;
  input id group $ lead0 lead1 lead4 lead6;
  baseline = lead0;
  lead = lead0; time = 0; output;
  lead = lead1; time = 1; output;
  lead = lead4; time = 4; output;
  lead = lead6; time = 6; output;
  keep id group time lead;
  label lead = "Blood lead level (ug/dL)";
  label time = "Time (week)";
  run;
 
*******************************************************************;

* Define indicator variables;
data A;
  set A;
  timefactor = time;
  t0 = (time = 0);
  t1 = (time = 1);
  t4 = (time = 4);
  t6 = (time = 6);
  t146 = 1 - t0;
  active = (group = "A");

  label t0 = "I(Time=0)";
  label t1 = "I(Time=1)";
  label t4 = "I(Time=4)";
  label t6 = "I(Time=6)";
  label t146   = "I(Time = 1 or 4 or 6)";
  label active = "I(Active group)";
  run;

*******************************************************************;

%let xvars = t1 t4 t6 t1 * active t4 * active  t6 * active;

title2 "1. Mixed model with a random intercept, REML";
proc mixed data = A;
  class id group timefactor;
  model lead = &xvars   / s ;
  random intercept / subject = id type = un g gcorr v vcorr;
  run;

title2 "2. Mixed model with a random intercept, MLE";
proc mixed data = A method = ML;
  class id group timefactor;
  model lead = &xvars   / s ;
  random intercept / subject = id type = un g gcorr v vcorr;
  run;

title2 "3. Marginal model with CS cov matrix, REML";
proc mixed data = A;
  class id group timefactor;
  model lead = &xvars / s ;
  repeated timefactor    / subject = id type = cs r rcorr;
  run;

title2 "4. Marginal model with CS cov matrix, MLE";
proc mixed data = A method = ML;
  class id group timefactor;
  model lead = &xvars / s ;
  repeated timefactor    / subject = id type = cs r rcorr;
  run;

title2 "5. Marginal model with exchangeable correlation, GEE";
proc genmod data = A;
  class id group timefactor;
  model lead = &xvars;
  repeated subject = id 
    / within = timefactor type = exch      modelse;
  run;

title2 "6. A non-identifiable model";
proc mixed data = A;
  class id group timefactor;
  model lead = &xvars   / s ;
  random intercept / subject = id type = un g gcorr v vcorr;
  repeated timefactor    / subject = id type = cs r rcorr;
  run;

title2 "7. Marginal model, CS corr matrix, MLE";
proc mixed data = A method = ML;
  class id group timefactor;
  model lead = &xvars / s ;
  repeated timefactor / subject = id type = csh r rcorr;
  run;

title2 "8. Marginal model, CS corr matrix per group, MLE";
proc mixed data = A method = ML;
  class id group timefactor;
  model lead = &xvars / s ;
  repeated timefactor / subject = id type = csh group = group r rcorr;
  run;


