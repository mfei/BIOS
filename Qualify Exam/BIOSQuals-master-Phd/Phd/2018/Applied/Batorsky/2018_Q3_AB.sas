/*
Year 2018 Problem 3
Input: virus.dat
Variables: CountyID - county identifier; HorseCases - number of cases of virus in horses; NumFarms - number of farms;
						PopDensity - human population density; BirdRate - standardized bird virus rate (bird virus count / human population)
Derived variables: 	horserate is horse cases divided by number of farms in the county
					lfarm is the offset, log(number of farms in the county)

Instructions: 	
*/

* read in data;
data virus;
	infile "C:\Users\abatorsk\Documents\My SAS Files\Comp\A2018\virus.dat" firstobs=2 dlm=",";
	input CountyID HorseCases NumFarms PopDensity BirdRate;
	horserate = HorseCases/NumFarms;
	lfarm = log(NumFarms);
	birdcounts = BirdRate*PopDensity;
run;

* Part A: Describe distribution of horse virus counts using quantitative and visual methods;

* create histogram of counts;
title1 "Distribution of Horse Virus Cases";
ods graphics on;
proc univariate data=virus noprint;
	histogram HorseCases/odstitle="Distribution of Horse Virus Cases";
	inset N = "Number of Counties" MEAN MEDIAN STD="Standard Deviation" /position=ne;
run;
title1;

* evaluate proportion of zeros;
data zeros;
	set virus;
	hzero=.;
	if HorseCases = 0 then hzero = 1;
	else hzero = 0;
	bzero=.;
	if BirdRate = 0 then bzero = 1;
	else bzero = 0;
run;
proc freq data=zeros; table hzero bzero/missing; run; *There are 26/46 horse zeros, which is ~57%, and 8/46 bird zeros, which is 17%;

* evaluate distribution given not zero;
proc sort data=zeros; by hzero; run;
proc univariate data=zeros;
	var HorseCases;
	by hzero;
run;

* evaluate correlation between horse virus counts and bird virus and population density;
proc corr data=zeros;
	var horserate HorseCases BirdRate PopDensity birdcounts hzero bzero;
run;

* Part B: Fit Poisson regression model to std horse rate using main effects for bird rate, pop density and their interaction;
title1 "Poisson model: standardized horse rate = standardized bird rate + population density + interaction";
proc genmod data=virus;
	model HorseCases = BirdRate PopDensity BirdRate*PopDensity / dist=poisson link=log type3 offset=lfarm;
	contrast "b1=b3=0" BirdRate 1, 
						PopDensity*BirdRate 1/wald;
run;
title1;
* Part C: Use negative binomial regression to account for overdispersion;
title1 "Negative Binomial model: standardized horse rate = standardized bird rate + population density + interaction";
proc genmod data=virus;
	model HorseCases = BirdRate PopDensity BirdRate*PopDensity / dist=negbin link=log type3 offset=lfarm;
	contrast "b1=b3=0" BirdRate 1, 
						PopDensity*BirdRate 1/wald;
run;
title1;
* Part D: Use zero inflated poisson model;
title1 "Zero-inflated Poisson Model";
proc genmod data=virus;
	model HorseCases = BirdRate PopDensity BirdRate*PopDensity / dist=zip type3 offset=lfarm;
	zeromodel BirdRate PopDensity BirdRate*PopDensity;
		contrast "b1=b3=0" BirdRate 1, 
						PopDensity*BirdRate 1/wald;
	output out=zip predicted=pred pzero=pzero;
run;
title1;
* Part E: Use zero inflated negative binomial model;
title1 "Zero-inflated Negative Binomial Model";
proc genmod data=virus;
	model HorseCases = BirdRate PopDensity BirdRate*PopDensity / dist=zinb type3 offset=lfarm;
	zeromodel BirdRate PopDensity BirdRate*PopDensity;
	contrast "b1=b3=0" BirdRate 1, 
						PopDensity*BirdRate 1/wald;
	output out=zip predicted=pred pzero=pzero;
run;
title1;
/*
* try using bzero as predictor of hzero;
title1 "Zero-inflated Negative Binomial Model";
proc genmod data=zeros;
	model HorseCases = BirdRate PopDensity BirdRate*PopDensity / dist=zinb type3 offset=lfarm;
	zeromodel bzero;
	output out=zip predicted=pred pzero=pzero;
run;
title1;
*/

***IMPORTANT NOTE - for zero-inflated models the first set of regression parameters are the estimates for the model
	the second set are a test of which variables are associated with the outcome being zero;
