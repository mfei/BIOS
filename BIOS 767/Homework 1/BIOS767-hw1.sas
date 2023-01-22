proc IML;
 call randseed(1);
  nVec = t(do(10,100,5));                      ** set the sample size grid;
  plotDataIML = J(nrow(nVec),2,0);
	
  do r = 1 to nrow(nVec);
	n        = nVec[r];                   ** extract the rth sample size;
	rho   = 1 / (n-1);
	Sigma = J(n,n,rho) + I(n)*(1-rho);
	mu    = J(1,n,0);
	nSims = 50000;                         ** set number of simulations;
  y     = randNormal(nSims,mu,Sigma);    ** generate NSIMS random samples (row=sample);

  Z     = y[,:]/sqrt(1/n);               ** compute row means and standardize with incorrect SD;

  pVal  = 2*cdf('normal',-abs(z));       ** compute two-sided p-value based on known variance;

  rejRate = (pVal<0.05)[:];
 
	plotDataIML[r,] = n||rejRate;   ** store current results in plotDataIML matrix;
  end;

  create plotData from plotDataIML[c={"n" "rejRate"}]; ** write out a SAS dataset;
  	append from plotDataIML;
  close plotData;
quit;


proc sgplot data = plotData;
	series x = n y = rejRate;
	yaxis values=(0 to 1 by 0.1);
	refline %sysfunc(sqrt(1/10)) / axis=y; 
	label n    = 'Sample Size' rejRate = 'Type I error rate';
run;
