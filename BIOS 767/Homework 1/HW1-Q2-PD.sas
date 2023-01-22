*********************************************************************                             
*                                                                   
*  PROGRAM DESCRIPTION: Starter code for HW1, Question 2, Part D;
*                                                                   
*-------------------------------------------------------------------
*  JOB NAME:       HW1-Q2-PD.sas                                   
*  LANGUAGE:       SAS, VERSION 9.4                                  
*                                                                   
*  NAME:           Matthew Psioda                               
*  DATE COMPLETE:  2020-01-18                                           
*-------------------------------------------------------------------
*                                                                   
*  Modification History:       
*                                                                                                                         
*  NAME:                         << Insert Name of Primary Programmer >>                               
*  DATE COMPLETE:                << YYYY-MM-DD >>   
*  DESCRIPTION OF MODIFICATION:  << Please insert 2-3 sentences >>                                                               
********************************************************************;

proc IML;
  rhoVec = t(do(0,0.99,0.01));                      ** set the correlation grid;
  plotDataIML = J(nrow(rhoVec),2,0);
	
  do r = 1 to nrow(rhoVec);
	rho        = rhoVec[r];                         ** extract the rth correlation;
	n          = 10;                                ** set the sample size;
	x          = J(n,1,1);                          ** construct the design matrix;
	Sigma      = J(n,n,rho) + I(n)*(1-rho);         ** construct the covariance matrix;
	betaHatSD  = sqrt(inv(t(x)*inv(Sigma)*x));      ** compute the standard deviation;

	plotDataIML[r,] = rho||betaHatSD;               ** store current results in plotDataIML matrix;
  end;

  create plotData from plotDataIML[c={"rho" "stdErr"}]; ** write out a SAS dataset;
  	append from plotDataIML;
  close plotData;
quit;


proc sgplot data = plotData;
	series x = rho y = stdErr;
	yaxis values=(0 to 1 by 0.1);
	refline %sysfunc(sqrt(1/10)) / axis=y; 
	label rho    = 'True Correlation' stdErr = 'True Standard Deviation';
run;
