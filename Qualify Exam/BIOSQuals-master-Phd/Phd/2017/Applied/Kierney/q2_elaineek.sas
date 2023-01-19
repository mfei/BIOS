libname q2 "C:\Users\elaineek\OneDrive - University of North Carolina at Chapel Hill\Classes\Qualifying Exam Prep\Applied\2017\Question 2";

filename INF "C:\Users\elaineek\OneDrive - University of North Carolina at Chapel Hill\Classes\Qualifying Exam Prep\Applied\2017\Question 2\CHILD.dat";

data q2.child;
  infile INF firstobs=2;
  input RX $ Y0 Y1 AGE SEX $ RACE $ PARENT_WT;
  id = _N_;
run;

/* A */

ods pdf file = "C:\Users\elaineek\OneDrive - University of North Carolina at Chapel Hill\Classes\Qualifying Exam Prep\Applied\2017\Question 2\a.pdf";
proc means data = q2.child nway;
var AGE;
run;

proc means data = q2.child nway;
class RX;
var AGE;
run;

proc freq data = q2.child;
tables RX*RACE RX*SEX RX*Y0 RX*PARENT_WT RACE SEX Y0 PARENT_WT;
tables RX*Y1 / missing;
run;
ods pdf close;

/* C */

ods trace on;
proc logistic data = q2.child;
where ^MISSING(Y1);
class Y0(ref="0") sex race parent_wt(ref="0") rx(ref="Control") / param = ref;
model Y1(event="1") = rx y0 sex race parent_wt age / covb;
run;
ods trace off;
ods pdf close;

/* D */

proc mi data = q2.child seed = 7062020 nimpute = 25 out = mifile;
	var rx y0 age sex race parent_wt y1;
	class rx y0 sex race parent_wt y1;
	monotone logistic(y1=rx y0 age sex race parent_wt);
run;

proc logistic data = mifile;
by _imputation_;
class Y0(ref="0") sex race parent_wt(ref="0") rx(ref="Control") / param = ref;
model Y1(event="1") = rx y0 sex race parent_wt age / covb;
ods output ParameterEstimates=beta OddsRatios=OR covb=varbeta;
run;

proc mianalyze parms = beta;
modeleffects intercept rx y0 sex race parent_wt age;
run;
