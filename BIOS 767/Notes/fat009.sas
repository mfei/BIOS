%let progname = fat009.sas;
* input: fat.dat
* output: 
* xref:
* does: Chapter 13
        MIT Growth and Development Study, marginal models
        Diagnostics 

*******************************************************************;
title "&progname: MIT Growth and Development Study, marginal models";

filename INF  "fat.dat";       * input data;
%let pdfout = "fat009.pdf";
*******************************************************************;

data FAT;
  retain intercept 1;
  infile INF firstobs = 2;
  input id age agemen time pbf;
  time_0 = max(time,0);

  label
    id     = "Subject ID"
    age    = "Current Age (years)"
    agemen = "Age at menarche (years)"
    time   = "Time relative to Menarche (years)"
    time_0 = "Time if > 0"
    pbf    = "Percent Body Fat"
  ;
  run;


proc sort data = FAT; by id age;  run; * make sure of the sort order;

%let Xvars = time ;                      * "intercept" is implicit;
%let Yvar  = pbf;                        * response variable;

*******************************************************************;

ods graphics on;
ods pdf file = &pdfout;

title2 "1. Deletion diagnostics";

ods listing close;
ods output obstats = OBS;
proc genmod data = FAT  plots = all;
     class id;
     model &Yvar = &xvars / diagnostics  ;
     repeated subject = id / type = exch ;
     run;
ods listing;

proc contents data = OBS;
  run;

proc print data = OBS;
  var id clusterleverage clustercookd clusterdfit leverage cookd;
  run;

********************************************************************;

title2 "2. Check functional form of covariates ";

proc genmod data = FAT;
     class id;
     model &yvar = time ;
     repeated subject = id / type = exch ;
     assess var=(time) /  resample=1000;
     run;

ods graphics off;
