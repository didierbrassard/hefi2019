 /****************************************************************************
 *                                                                           *
 * SAS Macro Percentiles_Survey estimates means, standard deviations and     *
 * percentiles for survey data.                                              *
 *                                                                           *
 * if sampling weights are specified, estimates are based on a weighted      *
 * empirical distribution. Otherwise, they are based on an unweighted        *
 * empirical distribution.                                                   *
 *                                                                           *
 *****************************************************************************
 *                                                                           *
 * Macro Parameters:                                                         *
 *                                                                           *
 *    Required Parameters:                                                   *
 *       data          = name of SAS data set containing the data to be      *
 *                       analyzed.                                           *
 *       var           = name of variable for which means and percentiles    *
 *                       are to be estimated.                                *
 *                                                                           *
 *    Optional Parameters:                                                   *
 *       byvar         = list of variables which define by-groups in the     *
 *                       data. means and percentles will be estimated        *
 *                       separately for each by-group.                       *
 *       weight        = name of the sampling weight variable, if the data   *
 *                       is from a complex survey with weights.              *
 *                       by default, the macro assumes equal weights.        *
 *       cutpoints     = list of values for which cut-point probabilities    *
 *                       are to be calculated (Prob(X <= cut-point)).        *
 *                       by default no cut-point probabilites are calculted. *
 *       print         = Y to print means and percentiles.                   *
 *                       N to supress printing means and percentiles.        *
 *                       by default, print = Y.                              *
 *       ntitle        = number of titles defined by the user.               *
 *                       by default, ntitle = 2.                             *
 *                                                                           *
 *****************************************************************************
 *                                                                           *
 * Output Data Set:                                                          *
 *                                                                           *
 *   _percentiles = data set containing estimated means and percentiles.     *
 *                  _percentiles contains the following variables:           *
 *                                                                           *
 *                    mean     = estimated mean.                             *
 *                    variance = estimated variance.                         *
 *                    stddev   = estimated standard distribution.            *
 *                    min      = minimum value.                              *
 *                    max      = maximum value.                              *
 *                    Pctile1  = estimated first percentile.                 *
 *                    Pctile2  = estimated second percentiile.               *
 *                    ...                                                    *
 *                    Pctile99 = estimated 99th percentile.                  *
 *                                                                           *
 *                 if cut-points are specified, then _percentiles also       *
 *                 contains the following variables:                         *
 *                                                                           *
 *                    Prob1    = probability for first cut-point.            *
 *                    Prob2    = probability for second cut-point.           *
 *                    ...                                                    *
 *                                                                           *
 *                 if by-group variables are specified, they are also        *
 *                 included in data set _percentiles.                        *
 *                                                                           *
 ****************************************************************************/

%macro Percentiles_Survey (data       = ,
                           byvar      = ,
                           var        = ,
                           weight     = ,
                           cutpoints  = ,
                           print      = Y,
                           ntitle     = 2);

%let print = %upcase(%substr(&print,1,1));

%if (&byvar ^= %str()) %then %do;
  proc sort data=&data;  by &byvar;  run;
  %end;

 /* calculate percentiles. */

proc univariate data=&data %if (&weight ^= %str()) %then vardef=weight; noprint;
  %if (&byvar ^= %str()) %then by &byvar %str(;);
  %if (&weight ^= %str()) %then weight &weight %str(;);
  var &var;
  output out=_percentiles(keep=&byvar Mean StdDev Variance Min Max Pctile1-Pctile99)
         mean=Mean std=StdDev var=Variance min=Min max=Max pctlpts=1 to 99 by 1 pctlpre=Pctile;
  run;

 /* calculate cut-point probabilities. */

%let ncutpt = 0;

%if (&cutpoints ^= %str()) %then %do;

data &data;
  set &data;

  %let i = 1;
  %let cp = %scan(&cutpoints,&i,%str( ));

  %do %while (&cp ^= %str());
    %let ncutpt = &i;
    if (&var > .z & &var <= &cp) then Prob&i = 1;
    else if (&var > .z) then Prob&i = 0;
    %let i = %eval(&i + 1);
    %let cp = %scan(&cutpoints,&i,%str( ));
    %end;
  run;

proc means data=&data noprint;
  %if (&byvar ^= %str()) %then by &byvar %str(;);
  %if (&weight ^= %str()) %then weight &weight %str(;);
  var Prob1-Prob&ncutpt;
  output out=_prob(keep=&byvar Prob1-Prob&ncutpt) mean=;
  run;

data _percentiles;
  merge _percentiles _prob;
    %if (&byvar ^= %str()) %then by &byvar %str(;);
  run;

%end; /* %if (&cutpoints ^= %str()) %then %do  */

 /* get rid of labels. */

data _percentiles;
  set _percentiles;
  label Mean         = " "
        StdDev       = " "
        Variance     = " "
        Min          = " "
        Max          = " "
      %do i = 1 %to 99;
        Pctile&i = " "
        %end;
      %do i = 1 %to &ncutpt;
        Prob&i       = " "
        %end;
	;
  run;

proc transpose data=_percentiles out=_percentiles2(rename=(col1=value)) name=Statistic;
  %if (&byvar ^= %str()) %then by &byvar %str(;);
  var mean variance stddev min max pctile1-pctile99 %if (&cutpoints ^= %str()) %then Prob1-Prob&ncutpt; %str(;)
  run;

%if (&print ^= N) %then %do;
  proc print data=_percentiles2;
    %if (&byvar ^= %str()) %then %do;
      by &byvar;
      pageby &byvar;
      %end;
    id Statistic;
    %if (&cutpoints = %str()) %then title%eval(&ntitle+1) "Estimated Means and Percentiles" %str(;);
    %else title%eval(&ntitle+1) "Estimated Means, Percentiles and Cut-Point Probabilites" %str(;);
    run;
  %end;

title%eval(&ntitle+1);

%mend Percentiles_Survey;

 /*** end of macro Percentiles_Survey ************************************/
