/* 2020-10. Global statement added to transfer lambda as macro variable */
 /****************************************************************************
 *                                                                           *
 * SAS Macro BoxCox_Survey finds the best Box-Cox transformation of survey   *
 * data by minimizing the SSE of a normal probability plot.                  *
 *                                                                           *
 * if sampling weights are specified, the normal probability plot is based   *
 * on a weighted empirical distribution. Otherwise, it is based on an        *
 * unweighted empirical distribution.                                        *
 *                                                                           *
 *****************************************************************************
 *                                                                           *
 * Macro Parameters:                                                         *
 *                                                                           *
 *    Required Parameters:                                                   *
 *       data          = name of SAS data set containing the data to be      *
 *                       analyzed.                                           *
 *       subject       = name of the variable that uniquely identifies each  *
 *                       subject (i.e., ID variable).                        *
 *       var           = name of variable for which means and percentiles    *
 *                       are to be estimated. The values for this variable   *
 *                       need to be positive and will be Box-Cox transformed *
 *                       and the transformed values will be used as the      *
 *                       reponse variable in a linear regression model.      *
 *                                                                           *
 *    Optional Parameters:                                                   *
 *       covars        = list of covariates used in the linear regression    *
 *                       model for the Box-Cox transformed positive recall   *
 *                       values.                                             *
 *       weight        = name of the sampling weight variable, if the data   *
 *                       is from a complex survey with weights.              *
 *                       by default, the macro assumes equal weights.        *
 *       print         = Y to print the best Box-Cox transformation.         *
 *                     = N to supress printing the the best Box-Cox          *
 *                       transformation.                                     *
 *                     = V (verbose) to print distributions before and after *
 *                       Box-Cox transformation (using proc univariate).     *
 *                       by default, print = Y.                              *
 *       plot          = Y to print normal probability plots before and      *
 *                       after Box-Cox transformation (using proc gplot).    *
 *                     = N to supress printing normal probability plots.     *
 *                       by default, plot = N.                               *
 *       ntitle        = number of titles defined by the user.               *
 *                       by default, ntitle = 2.                             *
 *                                                                           *
 *****************************************************************************
 *                                                                           *
 * Output Data Sets:                                                         *
 *                                                                           *
 *   _lambda = data set containing the best Box-Cox transformation.          *
 *             _lambda contains the following variables:                     *
 *                                                                           *
 *                lambda_&var  = Box-Cox transformation parameter for        *
 *                               variable &var.                              *
 *                sse          = sum of squared errors for the normal        *
 *                               probability plot.                           *
 *                                                                           *
 *                                                                           *
 *   &data = input data set containing the data to be analyzed.              *
 *           The following variables are added to &data:                     *
 *                                                                           *
 *                lambda_&var  = Box-Cox transformation parameter for        *
 *                               variable &var.                              *
 *                boxcox_&var  = Box-Cox transformation of variable &var.    *
 *                                                                           *
 ****************************************************************************/

%macro BoxCox_Survey (data    = ,
                      subject = ,
                      var     = ,
                      covars  = ,
                      weight  = ,
                      print   = Y,
                      plot    = N,
                      ntitle  = 2);

%let print   = %upcase(%substr(&print,1,1));
%let plot    = %upcase(%substr(&plot,1,1));
%let ntitle1 = %eval(&ntitle+1);
ods select none;
%do lambda_index = 0 %to 100;

  %let lambda = %sysevalf(&lambda_index / 100);

  data regdat;
    set &data;

    %if &lambda = 0 %then %do;
      g_r1 = log(&var);
    %end;
    %else %do;
      g_r1 = ((&var)**&lambda - 1) / &lambda;
    %end;
  run;

  /* get the residuals. */

  proc reg data=regdat noprint;
    model g_r1 = &covars;
    output out=regout r=resid;
  run;

  /* calculate weighted percentiles. */

  proc univariate data=regout noprint;
    %if (&weight  ^= %str()) %then freq &weight %str(;);
    %if (&subject ^= %str()) %then id &subject %str(;);
    var resid;
    output out=_pctile pctlpts=1 to 99 by 1 pctlpre=T;
    title&ntitle1 "Distribution After Box-Cox Transformation";
  run;

  /* find best box-cox transformation to normality by minimizing */
  /* the SSE of a normal probability plot.                       */
/* DIDIER BRASSARD 2020-10 - added a global statement to transfer lambda */
%global best_lambda ;
  data best_plotdata (keep=Z0 T1 rename=(Z0=Z T1=T));

    set _pctile;

    array Z  (*) Z1-Z99;
    array T  (*) T1-T99;
    array CP (*) CP1-CP99;

    do i=1 to dim(z);
      Z(i) = probit((i - 0.375) / 99.25);
    end;

    MZ  = mean(of Z1-Z99);
    SZZ = CSS(of Z1-Z99);

    %if &lambda = 0 %then %do;
      best_SSE    = SZZ;
      best_lambda = 0;
    %end;
    %else %do;
      best_SSE    = &best_SSE;
      best_lambda = &best_lambda;
    %end;

    MT  = mean(of T1-T99);
    STT = CSS(of T1-T99);

    do i = 1 to 99;
      CP(i) = (T(i) - MT) * (Z(i) - MZ);
    end;

    STZ = sum(of CP1-CP99);
    SSE = SZZ - ((STZ**2)/STT);

    if (SSE < best_SSE) then do;
      best_SSE     = SSE;
      best_lambda  = &lambda;

      do i = 1 to 99;
        Z0 = Z(i);
        T1 = T(i);
        output best_plotdata;
      end;
    end;

    call symputx('best_SSE', best_SSE);
    call symputx('best_lambda', best_lambda);

  run;

  data _null_;
    if eof then call symput('empty', 1);
    else call symput('empty', 0);
    stop;
    set best_plotdata end=eof;
  run;

  %if &empty = 0 %then %do;
    data _plotdata;
      set best_plotdata;
    run;
  %end;

%end;

proc datasets nolist;
  delete regdat regout best_plotdata;
run;

data _lambda;
  lambda_&var = &best_lambda;
  SSE         = &best_SSE;
  label lambda_&var = "Lambda";
run;

data _plotdata;
  set _plotdata;
  lambda_&var = &best_lambda;
  SSE         = &best_SSE;
  label lambda_&var = "Lambda";
run;
ods select all;
/* print best box-cox transformation. */

%if (&print ^= N) %then %do;
  proc print data=_lambda label noobs;
    var lambda_&var sse;
    title&ntitle1 "Box-Cox Transformation That Minimizes SSE of a Normal Probality Plot";
  run;
%end;

 /* box-cox transform the data. */

data &data;
  set &data;
  if (_n_ = 1) then set _lambda(drop=sse);

  if (lambda_&var = 0) then boxcox_&var = log(&var);
  else boxcox_&var = (&var**lambda_&var - 1) / lambda_&var;
run;

 /* print distribution of box-cox transformed variable. */

%if (&print = V) %then %do;
  proc univariate data=&data plot nobyplot normal nextrobs=10 nextrval=10;
    by lambda_&var;
    %if (&weight  ^= %str()) %then freq &weight %str(;);
    %if (&subject ^= %str()) %then id &subject %str(;);
    var boxcox_&var;
    output out=_pctls mean=Mean std=StdDev min=Min max=Max
           pctlpts=1, 5 to 95 by 5, 99 pctlpre=Pct;
    title&ntitle1 "Distribution After Box-Cox Transformation";
  run;
%end;

 /* print normal probability plots. */

%if (&plot ^= N) %then %do;

/* homemade */
  proc sgplot data=_plotdata noautolegend;
    reg y=T x=Z / lineattrs=(color=red pattern=longdash)  markerattrs=(color=black size=15 ) ;
    label T='T';
    title1 "Normal Probability Plot After Box-Cox Transformation";
  run;
  Title1;

/* original */

/*  symbol1 color=black value=x interpol=r; */

/*   goptions gsfmode=append; */

/*   proc plot data=_plotdata; */
/*     by lambda_&var; */
/*     plot T*Z; */
/*     label T='T'; */
/*     title&ntitle1 "Normal Probability Plot After Box-Cox Transformation"; */
/*   run; */
/*   quit; */

%end;

title&ntitle1;

%mend BoxCox_Survey;

 /*** end of macro BoxCox_Survey *****************************************/
