/* 
2019 Qualifying Exam Question 4
N=200
Response category: 1=PD (worst), 2=SD, 3=PR, 4=CR
SubA: 1=A1, 2=A2, 3=A3
SubB: 1=B1, 2=B2, 3=B3, 4=B4
SubC: 1=C1, 2=C2
Arm: 1=Folfirinox, 2=Gemcitabine + Abraxane
*/

proc import datafile= "C:\Users\abatorsk\Documents\My SAS Files\Comp\A2019\subtypeResponse.txt"
	out=sub
	dbms=dlm
	replace;
	delimeter='09'x;
	label subA = "Subtyping A" subB= "Subtyping B" subC= "Subtyping C" responseCat="Response";
run;

* Part A, summarize the results;
* i. create table that shows proportion of subjects in each response category by subtype (pooling across arms), 
	repeat for each subtype approach, summarize results for researchers;
proc freq data=sub; 
	table responseCat*subA responseCat*subB responseCat*subC/ /*plots=mosaic plot chisq cmh*/ norow nofreq nopercent;
run;

proc freq data=sub; 
	table arm*responseCat*subA arm*responseCat*subB arm*responseCat*subC/ plots=mosaic plot chisq cmh;
run;
* ii. create single figure illustrating proportion of participants with partial or complete response (response=3 or 4)
	by subtype for each subtyping approach - summarize results;
data A2;
	set sub;
	if responseCat = 3 or responseCat = 4 then prcr = 1;
	else prcr = 0;
	drop responseCat;
run;

proc freq data=A2;
	tables prcr*subA prcr*subB prcr*subC/ plots=mosaic plot chisq cmh;
run;

proc freq data=A2;
	tables arm*prcr*subA arm*prcr*subB arm*prcr*subC/ plots=mosaic plot chisq cmh;
run;

* iii. create single figure with 3 panels that depicts overlap in subtypes between approach A vs B, A vs C, B vs C - summarize;
proc freq data=sub; 
	table subA*subB subB*subC subA*subC/ plots=mosaic plot norow nofreq nopercent;
run;

* Part B: i. Fit proportional odds model using subtype as only predictor - repeat for each subtyping approach
	check model assumptions using appropriate diagnostics;

proc logistic data=sub descending;
	class subA (ref="1") responseCat (ref="1") / param=ref;
	model responseCat = subA / scale=none aggregate;
run;

proc logistic data=sub descending;
	class subB (ref="1") responseCat (ref="1") / param=ref;
	model responseCat = subB / scale=none aggregate;
run;

proc logistic data=sub descending;
	class subC (ref="1") responseCat (ref="1") / param=ref;
	model responseCat = subC / scale=none aggregate;
run;

* ii. Perform hypothesis test evaluating null hypothesis that subtype is not associated with response category, one for each
	subtyping approach. ;



* Part E: Repeat b and d including arm;

proc logistic data=sub descending;
	class subA (ref="1") responseCat (ref="1") arm (ref="1")/ param=ref;
	model responseCat = subA arm/ scale=none aggregate;
run;

proc logistic data=sub descending;
	class subA (ref="1") responseCat (ref="1") arm (ref="1")/ param=ref;
	model responseCat = subA arm subA*arm/ scale=none aggregate;
run;

proc logistic data=sub descending;
	class subB (ref="1") responseCat (ref="1") arm (ref="1") / param=ref;
	model responseCat = subB arm/ scale=none aggregate;
run;

proc logistic data=sub descending;
	class subB (ref="1") responseCat (ref="1") arm (ref="1") / param=ref;
	model responseCat = subB arm subB*arm/ scale=none aggregate;
run;

proc logistic data=sub descending;
	class subC (ref="1") responseCat (ref="1") arm (ref="1") / param=ref;
	model responseCat = subC arm/ scale=none aggregate;
run;

proc logistic data=sub descending;
	class subC (ref="1") responseCat (ref="1") arm (ref="1") / param=ref;
	model responseCat = subC arm subC*arm/ scale=none aggregate;
run;
