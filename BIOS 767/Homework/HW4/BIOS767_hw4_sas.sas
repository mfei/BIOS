/*******************************************************************
*
* Project            : BIOS 767 hw4
*
* Program name       : BIOS767_hw4_Eunchong_Kang.sas
*
* Author             : Eunchong Kang
*
* Date created       : 2022-03-27
*
* Modification log   : 2022-03-28 (Modified for submission)
*
********************************************************************/


/*** Data Import ***/
*LIBNAME hw4 "/home/u49480214/BIOS767/hw4";

/*
proc import datafile="/home/u49480214/BIOS767/hw4/monkey.dat" dbms=tab out=hw4.monkey;
	getnames=yes;
run;
*/

/*** Q(B) ***/
/* Data Transformation */
data monkey_qb;
	set hw4.monkey;
	
	change = carnt9-carnt0;
run;

/* Analysis */
ods output influence = influence_b;
proc mixed data = monkey_qb;
	class group(ref="1");
	model change = group / solution vciry outpm=residuals_b influence;
run;
quit;

/* Diagnostics */
/* data for scaled residuals */
data residuals_b;
	set residuals_b;
	
	absScaledResid = abs(ScaledResid);
	keep id group absScaledResid;
run; 

proc sort data=residuals_b;
	by descending absScaledResid; 
run;

/* data for Cook's D*/
proc sort data=influence_b; by descending CookD; run;


/*** Q(C) ***/
/* Data Transformation*/
data monkey_qc1;
	set hw4.monkey(keep=id group carnt0-carnt9);
	
	array carnt{9} carnt1-carnt9;
		do i=1 to 9;
			carnt{i} = carnt{i} - carnt0;
		end;
	drop i carnt0;
	
run;

proc transpose data=monkey_qc1 out=monkey_qc2;
	by id group;
run;

data monkey_qc3;
 	set monkey_qc2(rename=(col1=carnt_change));
	week = input(substr(_name_, 6), 5.);
	cweek = put(week, 8.);
 	drop _name_;
run; 

/* Analysis */
ods output influence = influence_c;
proc mixed data = monkey_qc3 method = reml;
	class group cweek id;
	model carnt_change = group week*group
		/ ddfm=kenwardroger noint solution vciry outpm=residuals_c influence(iter=10 effect=id);
	random intercept / subject=id g gcorr v vcorr;
	repeated cweek / subject=id group=group r rcorr;
	
	contrast "Difference in trajectories" group 1 -1, week*group 1 -1;
run;
quit;

/* Diagnostics */
/* data for scaled residuals */
data residuals_c;
	set residuals_c;
	
	absScaledResid = abs(ScaledResid);
	keep id group week absScaledResid;
run; 

proc sort data=residuals_c;
	by descending absScaledResid; 
run;

/* data for Cook's D*/
proc sort data=influence_c; by descending CookD; run;


/** Sensitiviy Analysis **/
data monkey_qc4;
 	set monkey_qc3;
 	
 	if id=5 and week=8 then carnt_change=.;
 	if id=11 and week=6 then carnt_change=.;
run; 


proc mixed data = monkey_qc4 method = reml;
	class group(ref="1") cweek id;
	model carnt_change = group week*group
		/ ddfm=kenwardroger noint solution vciry outpm=residuals_c influence(iter=10 effect=id);
	random intercept / subject=id g gcorr v vcorr;
	repeated cweek / subject=id group=group r rcorr;
	
	contrast "Difference in trajectories" group 1 -1, week*group 1 -1;
run;

