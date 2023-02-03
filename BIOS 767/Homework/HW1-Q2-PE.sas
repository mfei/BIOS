*********************************************************************                             
*                                                                   
*  PROGRAM DESCRIPTION: Starter code for HW1, Question 2, Part E;
*                                                                   
*-------------------------------------------------------------------
*  JOB NAME:       HW1-Q2-PE.sas                                   
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
 call randseed(1);

  n     = 10; 
  rho   = 1 / (n-1);
  Sigma = J(n,n,rho) + I(n)*(1-rho);
  mu    = J(1,n,0);

  nSims = 50000;                         ** set number of simulations;
  y     = randNormal(nSims,mu,Sigma);    ** generate NSIMS random samples (row=sample);

  Z     = y[,:]/sqrt(1/n);               ** compute row means and standardize with incorrect SD;

  pVal  = 2*cdf('normal',-abs(z));       ** compute two-sided p-value based on known variance;

  rejRate = (pVal<0.05)[:];              ** compute proportion of samples rejected;

  print rejRate[l="Type I Error Rate"];

quit;

  


