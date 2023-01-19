libname q3 "C:\Users\elaineek\OneDrive - University of North Carolina at Chapel Hill\Classes\Qualifying Exam Prep\Applied\2018\Question 3";

filename INF "C:\Users\elaineek\OneDrive - University of North Carolina at Chapel Hill\Classes\Qualifying Exam Prep\Applied\2018\Question 3\VIRUS.dat";

data q3.virus;
  infile INF firstobs=2 DLM=',';
  input CountyID HorseCases NumFarms PopDensity BirdRate;
  lfarms = log(NumFarms);
run;

/* A */
proc means data = q3.virus n nmiss mean min max std;
var HorseCases;
run;

proc univariate data = q3.virus;
var HorseCases;
run;

/* B */
proc genmod data = q3.virus;
model HorseCases = BirdRate PopDensity BirdRate*PopDensity / dist = poisson link = log offset = lfarms type3;
contrast 'Bird Rate Test' BirdRate 1, BirdRate*PopDensity 1 / wald;
run;

/* C */

proc genmod data = q3.virus;
model HorseCases = BirdRate PopDensity BirdRate*PopDensity / dist = nb offset = lfarms type3;
contrast 'Bird Rate Test' BirdRate 1, BirdRate*PopDensity 1 / wald;
run;

/* D */

proc means data = q3.virus median;
var BirdRate PopDensity;
run;

data q3.virus_dichot;
set q3.virus;
BirdRateDichot = (BirdRate <= 0.000056150);
PopDensityDichot = (PopDensity <= 74.2);
run;

proc sgplot data = q3.virus_dichot;
histogram HorseCases;
run;

proc sort data = q3.virus_dichot;
by PopDensityDichot BirdRateDichot;
run;

proc sgpanel data = q3.virus_dichot;
panelby PopDensityDichot BirdRateDichot;
histogram HorseCases / binstart = 0 binwidth = 1 scale = count;
run;

proc genmod data = q3.virus;
model HorseCases = BirdRate PopDensity BirdRate*PopDensity / dist = zip offset = lfarms type3;
zeromodel BirdRate;
run;
proc genmod data = q3.virus;
model HorseCases = BirdRate PopDensity BirdRate*PopDensity / dist = zip offset = lfarms type3;
zeromodel BirdRate PopDensity;
run;

proc genmod data = q3.virus;
model HorseCases = BirdRate PopDensity BirdRate*PopDensity / dist = zip offset = lfarms type3;
zeromodel BirdRate PopDensity BirdRate*PopDensity;
output out=zip predicted=pred pzero=pzero;
ods output Modelfit=fit;
run;

data fit;
set fit(where=(criterion="Scaled Pearson X2"));
format pvalue pvalue6.4;
pvalue=1-probchi(value,df);
run;
proc print data=fit noobs;
var criterion value df pvalue;
run;

/* E */
proc genmod data = q3.virus;
model HorseCases = BirdRate PopDensity BirdRate*PopDensity / dist = zinb offset = lfarms type3;
zeromodel BirdRate PopDensity BirdRate*PopDensity;
output out=zip predicted=pred pzero=pzero;
ods output Modelfit=fit;
run;

data fit;
set fit(where=(criterion="Scaled Pearson X2"));
format pvalue pvalue6.4;
pvalue=1-probchi(value,df);
run;
proc print data=fit noobs;
var criterion value df pvalue;
run;

proc genmod data = q3.virus;
model HorseCases = BirdRate PopDensity BirdRate*PopDensity / dist = zinb offset = lfarms type3;
zeromodel BirdRate;
output out=zip predicted=pred pzero=pzero;
ods output Modelfit=fit;
run;

data fit;
set fit(where=(criterion="Scaled Pearson X2"));
format pvalue pvalue6.4;
pvalue=1-probchi(value,df);
run;
proc print data=fit noobs;
var criterion value df pvalue;
run;

