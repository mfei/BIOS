%let progname = fat004.sas;
%let pdfout   = fat004.pdf;

* input: fat.dat
* output:
* xref:
* does:
        MIT Growth and Development Study, mixed models
        Random intercept and slopes before and after menarche ($q=3$).
        Also a separate fit for subject 14 only

*******************************************************************;
title "&progname: MIT Growth and Development Study, mixed models";

filename INF  "fat.dat";
*******************************************************************;

data A;
  infile INF firstobs = 2;
  input id age agemen time pbf;
  time_bef = time * (time <= 0);
  time_aft = time * (time >  0);

  label
    id     = "Subject ID"
    age    = "Current age (years)"
    agemen = "Age at menarche (years)"
    time   = "Time relative to menarche (years)"
    pbf    = "Percent body fat"
    time_bef = "Time before menarche (years)"
    time_aft = "Time after  menarche (years)"
  ;
  run;

*******************************************************************;

ods output solutionR = EBLUP;  * output table of empirical BLUP;

ods listing close; * silent;

title2 "Random intercept and slopes before and after menarche (q=3)";

proc mixed data = A method = reml noclprint = 10;
  class id; 
  model pbf  = time_bef time_aft
          / outp  = OUTP  s
            outpm = OUTPM;
  random intercept time_bef time_aft
          / subject = id type = un g solution; * random / solution -> EBLUP;
run;

ods listing; * talk;

***********************************************;

ods pdf file = "&pdfout";

*******************************************************************;

title2 "Fit a model to subject 14 only";

proc reg data = A;
     where (id = 14);
     model pbf = time_bef time_aft ;
     output out = B p = fitted r = resid;
     run;

proc print data = B; 
  var id time pbf fitted  resid;
  run;

title2 "EBLUP dataset"; 

proc print data = EBLUP;
    var  id effect estimate stderrpred tvalue probt;
    where (id = 14);
    run;

proc sort data = EBLUP; by effect; run;
proc means data = EBLUP;
    by effect;
    run;

***********************************************;

title2 "OUTPM dataset (fitted marginal means,  X*BETA)";

proc print data = OUTPM;
    where (id = 14);
    var  id pred resid stderrpred;
    run;

***********************************************;
title2 "OUTP dataset (subject-specific prediction, X*BETA + Z*b)";

proc print data = OUTP ;
    where (id = 14);
    var  id pred resid stderrpred;
    run;

*******************************************************************;
title2 "Look at estimates from proc mixed";
proc mixed data = A method = reml noclprint = 10 covtest;
  class id; 
  model pbf  = time_bef time_aft / s;
  random intercept time_bef time_aft
          / subject = id type = un g v=14;
run;

