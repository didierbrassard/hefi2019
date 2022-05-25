
/*****************************************************************************/
/* Global macro variables are declared and used to output variable lists.    */ 
/*****************************************************************************/

%global conday_var_list  stdbc_epis_var_list  stdbc_daily_var_list  std_continuous_covar_list;


/*****************************************************************************/
/*****************************************************************************/
/*                                                                           */
/*                  STD_COV_BOXCOX24HR_CONDAY_MINAMT MACRO                   */
/*                                                                           */
/*****************************************************************************/
/*                       VERSION 2.0          1/5/2015                       */
/*                                                                           */
/*                                                                           */
/* The STD_COV_BOXCOX24HR_CONDAY_MINAMT macro is used to standardize         */
/* continuous covariates, transform and standardize reported nonzero amounts */
/* (e.g. from 24-hour recalls), and create variables needed for the          */
/* MULTIVAR_MCMC macro.                                                      */
/*                                                                           */
/* Continuous covariates are standardized to have a mean of 0 and a variance */
/* of 1, and these new standardized variables are named by adding the prefix */
/* "std_" to the original variable name.                                     */
/*                                                                           */
/* For dietary components consumed episodically, consumption-day indicator   */
/* variables are created, and these new variables are named by adding the    */
/* prefix "conday_" to the original variable name.  Also, the following      */
/* transformation approach, used by Zhang et al. (2011a, 2011b), is          */
/* performed.  First, a suitable Box-Cox transformation is applied to the    */
/* nonzero values.  Second, the Box-Cox transformed values are centered and  */
/* scaled to obtain an approximate normal distribution with a mean of 0 and  */
/* a variance of 2.  These new variables are named by adding the prefix      */
/* "stdbc_" to the original variable name.                                   */ 
/*                                                                           */
/* For dietary components that are consumed nearly every day, as specified   */
/* by the user, zero values are replaced with minimum amount values provided */
/* by the user or calculated as half of the observed nonzero minimum amount. */
/* Then the following transformation approach, used by Zhang et al. (2011a,  */
/* 2011b), is performed for the dietary components that are non-episodically */
/* consumed.  First, a suitable Box-Cox transformation is applied.  Second,  */
/* the Box-Cox transformed values are centered and scaled to obtain an       */
/* approximate normal distribution with a mean of 0 and a variance of 2.     */
/* These new variables are named by adding the prefix "stdbc_" to the        */
/* original variable name.                                                   */ 
/*                                                                           */
/* References:                                                               */
/*                                                                           */
/*   Zhang S, Krebs-Smith SM, Midthune D, Perez A, Buckman DW, Kipnis V,     */
/*   Freedman LS, Dodd KW, Carroll RJ. Fitting a bivariate measurement error */
/*   model for episodically consumed dietary components. Int J Biostat       */
/*   2011;7(1):Article 1.                                                    */
/*                                                                           */
/*   Zhang S, Midthune D, Guenther PM, Krebs-Smith SM, Kipnis V, Dodd KW,    */
/*   Buckman DW, Tooze JA, Freedman L, Carroll RJ. A new multivariate        */
/*   measurement error model with zero-inflated dietary data, and its        */
/*   application to dietary assessment. Ann Appl Stat 2011 Jun;5(2B):        */
/*   1456-87.                                                                */
/*                                                                           */
/*                                                                           */
/* The syntax for calling the STD_COV_BOXCOX24HR_CONDAY_MINAMT macro is:     */
/*                                                                           */
/* %std_cov_boxcox24hr_conday_minamt(data                            =,      */
/*                                   prestand_continuous_covars      =,      */
/*                                   rec24hr_epis_vars               =,      */
/*                                   rec24hr_daily_vars              =,      */
/*                                   boxcox_tran_lambda_data         =,      */
/*                                   minamount_data                  =,      */
/*                                   print                           =,      */
/*                                   titles                          =       */
/*                                   );                                      */
/*                                                                           */
/*  where                                                                    */
/*                                                                           */
/*  "data"                        Specifies an input data set that includes  */
/*                                one or more observations for each subject. */
/*                                                                           */
/*  "prestand_continuous_covars"  Specifies a list of continuous covariates  */
/*                                that will be standardized to have a mean   */
/*                                of 0 and a variance of 1.                  */
/*                                                                           */
/*  "rec24hr_epis_vars"           Specifies a list of 24-hour recall         */
/*                                variables for dietary components consumed  */
/*                                episodically.                              */
/*                                                                           */
/*  "rec24hr_daily_vars"          Specifies a list of 24-hour recall         */
/*                                variables for dietary components consumed  */
/*                                every day or nearly every day.             */
/*                                                                           */
/*  "boxcox_tran_lambda_data"     Specifies an input data set that includes  */ 
/*                                the following two variables:               */
/*                                "tran_paramindex"                          */
/*                                    an index value of 1, 2, ..., Q where   */
/*                                    Q is the total number of dietary       */
/*                                    components specified.  The values 1,   */
/*                                    2, ..., Q should be assigned according */
/*                                    to the order of the dietary components */
/*                                    as specified for the                   */
/*                                    "rec24hr_epis_vars" and                */
/*                                    "rec24hr_daily_vars" macro parameters, */
/*                                    and the "rec24hr_epis_vars" dietary    */
/*                                    components should precede the          */
/*                                    "rec24hr_daily_vars" dietary           */
/*                                    components.                            */
/*                                "tran_lambda"                              */
/*                                    a Box-Cox transformation parameter     */
/*                                    value for the corresponding dietary    */
/*                                    component.                             */
/*                                The records in this data set should be     */
/*                                sorted according to the order described    */
/*                                for the "tran_paramindex" variable.        */
/*                                                                           */
/*  "minamount_data"              Specifies an optional input data set that  */
/*                                includes the following two variables:      */
/*                                "tran_paramindex"                          */
/*                                    an index value of 1, 2, ..., Q where   */
/*                                    Q is the total number of dietary       */
/*                                    components specified.  The values 1,   */
/*                                    2, ..., Q should be assigned according */
/*                                    to the order described for the         */
/*                                    "boxcox_tran_lambda_data" macro        */
/*                                    parameter.                             */
/*                                "minamount"                                */
/*                                    a minimum amount value for the         */
/*                                    corresponding dietary component.       */
/*                                The records in this data set should be     */
/*                                sorted according to the order described    */
/*                                for the "tran_paramindex" variable.        */
/*                                                                           */
/*  "print"                       If "print=y" or "print=Y" then macro       */ 
/*                                results are printed.  The default value is */
/*                                "y".                                       */ 
/*                                                                           */
/*  "titles"                      Specifies the number of title lines to be  */
/*                                reserved for the user's titles.  Two       */
/*                                additional title lines are used by the     */
/*                                macro.  The default value is "0".          */
/*                                                                           */
/*****************************************************************************/
/*                                                                           */
/* Macro Output:                                                             */
/*                                                                           */
/* The new variables and the original variables are saved in a SAS data set  */
/* named "stdcov_stdbc24hr_conday_out" which can be used as the input data   */
/* set for the MULTIVAR_MCMC macro.                                          */
/*                                                                           */
/* The following global macro variables are declared and used to output      */ 
/* variable lists that can be used to specify the lists of input variables   */
/* needed for the MULTIVAR_MCMC macro:                                       */
/*     conday_var_list                                                       */
/*     stdbc_epis_var_list                                                   */
/*     stdbc_daily_var_list                                                  */
/*     std_continuous_covar_list.                                            */
/*                                                                           */
/* The macro also saves the following variables in a SAS data set named      */
/* "backtran_out" which can be used in subsequent analysis steps that        */
/* require back-transformation:                                              */ 
/*     tran_paramindex tran_lambda tran_center tran_scale minamount.         */
/*                                                                           */
/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/


%macro std_cov_boxcox24hr_conday_minamt(data                            =, 
                                        prestand_continuous_covars      =,
                                        rec24hr_epis_vars               =,
                                        rec24hr_daily_vars              =,
                                        boxcox_tran_lambda_data         =,   
                                        minamount_data                  =,   
                                        print                           = y,
                                        titles                          = 0
                                        );                 




  /**********************************************************************************/
  /*** Ensure user macro call does not overwrite default values with a null value ***/  
  /**********************************************************************************/

  %if (&print=%str()) %then %let print = y;
  %if (&titles=%str()) %then %let titles = 0;
  

  /**************************************************/
  /*** Prepare variables for the stdize procedure ***/ 
  /**************************************************/


  %let std_continuous_covar_list = ;
  %let cc_prestand_continuous_covars = 0;
  
  %do %while (%quote(%scan(&prestand_continuous_covars, %eval(&cc_prestand_continuous_covars + 1), %str( ))) ^= %str());
    %let cc_prestand_continuous_covars = %eval(&cc_prestand_continuous_covars + 1);
    %let prestand_continuous_covar&cc_prestand_continuous_covars = %scan(&prestand_continuous_covars, &cc_prestand_continuous_covars, %str( ));
    %let std_continuous_covar_list = &std_continuous_covar_list  std_&&prestand_continuous_covar&cc_prestand_continuous_covars;
  %end;


  %let stdbc_rec24hr_epis_vars = ;
  %let h_rec24hr_epis_vars = 0;
  
  %do %while (%quote(%scan(&rec24hr_epis_vars, %eval(&h_rec24hr_epis_vars + 1), %str( ))) ^= %str());
    %let h_rec24hr_epis_vars = %eval(&h_rec24hr_epis_vars + 1);
    %let rec24hr_epis_var&h_rec24hr_epis_vars = %scan(&rec24hr_epis_vars, &h_rec24hr_epis_vars, %str( ));
    %let stdbc_rec24hr_epis_vars = &stdbc_rec24hr_epis_vars  stdbc_&&rec24hr_epis_var&h_rec24hr_epis_vars;
  %end;


  %let stdbc_rec24hr_daily_vars = ;
  %let k_rec24hr_daily_vars = 0;
  
  %do %while (%quote(%scan(&rec24hr_daily_vars, %eval(&k_rec24hr_daily_vars + 1), %str( ))) ^= %str());
    %let k_rec24hr_daily_vars = %eval(&k_rec24hr_daily_vars + 1);
    %let rec24hr_daily_var&k_rec24hr_daily_vars = %scan(&rec24hr_daily_vars, &k_rec24hr_daily_vars, %str( ));
    %let stdbc_rec24hr_daily_vars = &stdbc_rec24hr_daily_vars  stdbc_&&rec24hr_daily_var&k_rec24hr_daily_vars;
  %end;


  %let totdietcomp = %eval(&h_rec24hr_epis_vars + &k_rec24hr_daily_vars);

  %let sqroot2 = 1.414213562373095;
                 



  data notneg24hr_data ckmiss_24hr_covars cknegative24hr;
    set &data;

    %let conday_var_list = ;     
    %let obsgt0_var_list = ;     
    %let amt_epis_var_list = ;    
    %let amt_daily_var_list = ;   
    %let obs_min_amt_var_list = ;    
    %let dietcompj = 0;

    if (nmiss(of  &prestand_continuous_covars  &rec24hr_epis_vars  &rec24hr_daily_vars) ^= 0) 
      then output ckmiss_24hr_covars;
    else do;
  
    
      /*** Episodically consumed dietary components ***/
  
      %if (&h_rec24hr_epis_vars ^= 0) %then %do;
        %do hh_epis = 1 %to &h_rec24hr_epis_vars;

          %let dietcompj = %eval(&dietcompj + 1);

          %let conday_var_list = &conday_var_list  conday_&&rec24hr_epis_var&hh_epis;     
          %let amt_epis_var_list = &amt_epis_var_list  amt_&&rec24hr_epis_var&hh_epis;    
          %let obs_min_amt_var_list = &obs_min_amt_var_list  obs_min_amt_var&dietcompj;    
     
          if (&&rec24hr_epis_var&hh_epis < 0) then do;
            output cknegative24hr; 
            delete;
          end; 
          else if (&&rec24hr_epis_var&hh_epis = 0) then do;
            conday_&&rec24hr_epis_var&hh_epis = 0;
            amt_&&rec24hr_epis_var&hh_epis = .;
          end;
          else do;
            conday_&&rec24hr_epis_var&hh_epis = 1;
            amt_&&rec24hr_epis_var&hh_epis = &&rec24hr_epis_var&hh_epis;
          end;
     
        %end;
      %end;
        
  
      /*** Daily consumed dietary components ***/
  
      %if (&k_rec24hr_daily_vars ^= 0) %then %do;
        %do kk_daily = 1 %to &k_rec24hr_daily_vars;

          %let dietcompj = %eval(&dietcompj + 1);

          %let obsgt0_var_list = &obsgt0_var_list  obsgt0_&&rec24hr_daily_var&kk_daily;     
          %let amt_daily_var_list = &amt_daily_var_list  amt_&&rec24hr_daily_var&kk_daily;   
          %let obs_min_amt_var_list = &obs_min_amt_var_list  obs_min_amt_var&dietcompj;   
        
          if (&&rec24hr_daily_var&kk_daily < 0) then do;
            output cknegative24hr; 
            delete;
          end; 
          else if (&&rec24hr_daily_var&kk_daily = 0) then do;
            obsgt0_&&rec24hr_daily_var&kk_daily = 0;
            amt_&&rec24hr_daily_var&kk_daily = .;
          end;
          else do;
            obsgt0_&&rec24hr_daily_var&kk_daily = 1;
            amt_&&rec24hr_daily_var&kk_daily = &&rec24hr_daily_var&kk_daily;
          end;
        
        %end;
      %end;
  
      /***************************/
  
      output notneg24hr_data;
  
    end;  
    
  run;  




  %if (&totdietcomp ^= 0) %then %do;


    %if (&minamount_data = %str()) %then %do;
  
      %let minamount_data = half_obs_min_amt;
   
    
      /*** Calculate minimum amounts on consumption days ***/
      
      
      proc means data=notneg24hr_data n nmiss mean std min max 
        %if %upcase(&print)^=Y %then %do;
          noprint;
        %end;
        ;
        var &amt_epis_var_list  &amt_daily_var_list;
        output out=observed_minamount min = &obs_min_amt_var_list;
        title%eval(&titles+1) "Observed Minimum Amounts on Consumption Days";
      run;
      title%eval(&titles+1);


      %if %upcase(&print)=Y %then %do;
        proc means data=notneg24hr_data n nmiss mean std min max;
          var &conday_var_list  
              &obsgt0_var_list
              ;
          title%eval(&titles+1) "Consumption Days for Episodically Consumed Dietary Components and Initial Consumption"; 
          title%eval(&titles+2) "Days for Daily Consumed Dietary Components before Replacing Zeros with a Minimal Value";
        run;
        title%eval(&titles+1);
        title%eval(&titles+2);   
      %end;
      
      
      data &minamount_data(keep = tran_paramindex minamount);
        set observed_minamount;
      
        array obs_min_amt(&totdietcomp) &obs_min_amt_var_list;
        
        do tran_paramindex = 1 to &totdietcomp;
          minamount = 0.5 * obs_min_amt(tran_paramindex);
          output;
        end;
        
      run;
   
    %end;  


    /*** Create 1 record data set with lambda and minimum amount values ***/
  
  
    data lambda_minamt_multirec;
      merge &minamount_data(keep = tran_paramindex minamount)
            &boxcox_tran_lambda_data(keep = tran_paramindex tran_lambda)
            ;
      by tran_paramindex;
    run;


    data rec_lambda_minamt_1record(drop = tran_paramindex tran_lambda minamount);
      set lambda_minamt_multirec; 
      by tran_paramindex;
  
      %if (&totdietcomp = 1) %then %do;
        
        rec_lambda1 = tran_lambda;
        minamount1 = minamount;
            
      %end;
      %else %do;
  
        retain rec_lambda1 - rec_lambda&totdietcomp  minamount1 - minamount&totdietcomp;
  
        %do dietcompj = 1 %to &totdietcomp;
          if (tran_paramindex = &dietcompj) then do;
            rec_lambda&dietcompj = tran_lambda;
            minamount&dietcompj = minamount;
          end;
        %end;
      
      %end;
  
      if (tran_paramindex = &totdietcomp) then output rec_lambda_minamt_1record;
  
    run;


  %end;  
  
 
 
  
  data stdize_data;
    %if (&totdietcomp ^= 0) %then %do;
      if (_n_ = 1) then set rec_lambda_minamt_1record;
    %end;  
    set notneg24hr_data;

    %let stdbc_epis_var_list = ;    
    %let stdbc_daily_var_list = ;   
    %let dietcompj = 0;

          
    /*** Continuous covariates ***/

    %if (&cc_prestand_continuous_covars ^= 0) %then %do;
          
      array prestand(&cc_prestand_continuous_covars) &prestand_continuous_covars;
      array stand(&cc_prestand_continuous_covars)    &std_continuous_covar_list;
      
      do cov_i = 1 to &cc_prestand_continuous_covars;
        stand(cov_i) = prestand(cov_i); 
      end;

      drop cov_i;
 
    %end;

  
    /*** Episodically consumed dietary components ***/

    %if (&h_rec24hr_epis_vars ^= 0) %then %do;
      %do hh_epis = 1 %to &h_rec24hr_epis_vars;

        drop &amt_epis_var_list;

        %let stdbc_epis_var_list = &stdbc_epis_var_list  stdbc_&&rec24hr_epis_var&hh_epis;    
        %let dietcompj = %eval(&dietcompj + 1);
   
        if conday_&&rec24hr_epis_var&hh_epis = 0 then stdbc_&&rec24hr_epis_var&hh_epis = .;
        else if conday_&&rec24hr_epis_var&hh_epis = 1 then do;

          if (rec_lambda&dietcompj = 0) then stdbc_&&rec24hr_epis_var&hh_epis = log(&&rec24hr_epis_var&hh_epis);
          else stdbc_&&rec24hr_epis_var&hh_epis = (( (&&rec24hr_epis_var&hh_epis)**rec_lambda&dietcompj ) - 1) / rec_lambda&dietcompj;

        end;
   
      %end;
    %end;
      

    /*** Daily consumed dietary components ***/

    %if (&k_rec24hr_daily_vars ^= 0) %then %do;
      %do kk_daily = 1 %to &k_rec24hr_daily_vars;

        drop &amt_daily_var_list;

        %let stdbc_daily_var_list = &stdbc_daily_var_list  stdbc_&&rec24hr_daily_var&kk_daily;   
        %let dietcompj = %eval(&dietcompj + 1);
      
        if (&&rec24hr_daily_var&kk_daily = 0) then &&rec24hr_daily_var&kk_daily = minamount&dietcompj;

        /*** Values below zero have been deleted and zero values have been replaced with a minimum value so condition should always be true ***/
        if (&&rec24hr_daily_var&kk_daily > 0) then do;
        
          if (rec_lambda&dietcompj = 0) then stdbc_&&rec24hr_daily_var&kk_daily = log(&&rec24hr_daily_var&kk_daily);
          else stdbc_&&rec24hr_daily_var&kk_daily = (( (&&rec24hr_daily_var&kk_daily)**rec_lambda&dietcompj ) - 1) / rec_lambda&dietcompj;

        end;
      
      %end;
    %end;

    /***************************/

    output stdize_data;

    
  run;  

  
    
  
  /*****************************/
  /*** Standardize variables ***/
  /*****************************/

  
  %let centscal_dataset_list = ;
  %let tr_centscal_dataset_list = ;
  %let dietcompj = 0;

          
  /*** Continuous covariates ***/

  %if (&cc_prestand_continuous_covars ^= 0) %then %do;
        
    proc stdize data=stdize_data nomiss method=std outstat=centscal_cov out=stdize_data;
      var &std_continuous_covar_list;
    run;

    %if %upcase(&print)=Y %then %do;

      proc means data=stdize_data n nmiss mean std min max;
        var &std_continuous_covar_list;
        title%eval(&titles+1) "Standardized Continuous Covariates";
      run;
      title%eval(&titles+1);
    
    %end; 

  %end;


  /*** Episodically consumed dietary components ***/
  
  %if (&h_rec24hr_epis_vars ^= 0) %then %do;
    %do hh_epis = 1 %to &h_rec24hr_epis_vars;

      %let dietcompj = %eval(&dietcompj + 1);
 
      proc stdize data=stdize_data nomiss method=std mult=&sqroot2 outstat=centscal_dietcomp&dietcompj out=stdize_data;
        var stdbc_&&rec24hr_epis_var&hh_epis; 
      run;
        
      proc transpose data=centscal_dietcomp&dietcompj out=tr_centscal_dietcomp&dietcompj(drop = _name_);
        id _type_;
        var stdbc_&&rec24hr_epis_var&hh_epis;
      run;

      %let centscal_dataset_list = &centscal_dataset_list  centscal_dietcomp&dietcompj;
      %let tr_centscal_dataset_list = &tr_centscal_dataset_list  tr_centscal_dietcomp&dietcompj;
     
    %end;
  %end;
        
  
  /*** Daily consumed dietary components ***/
 
  %if (&k_rec24hr_daily_vars ^= 0) %then %do;
    %do kk_daily = 1 %to &k_rec24hr_daily_vars;

      %let dietcompj = %eval(&dietcompj + 1);
   
      proc stdize data=stdize_data nomiss method=std mult=&sqroot2 outstat=centscal_dietcomp&dietcompj out=stdize_data;
        var stdbc_&&rec24hr_daily_var&kk_daily; 
      run;
        
      proc transpose data=centscal_dietcomp&dietcompj out=tr_centscal_dietcomp&dietcompj(drop = _name_);
        id _type_;
        var stdbc_&&rec24hr_daily_var&kk_daily;
      run;
  
      %let centscal_dataset_list = &centscal_dataset_list  centscal_dietcomp&dietcompj;
      %let tr_centscal_dataset_list = &tr_centscal_dataset_list  tr_centscal_dietcomp&dietcompj;
      
    %end;
  %end;
 
      
  /***************/
    
    
  data stdcov_stdbc24hr_conday_out;
    set stdize_data;
  
    /*** Assign zeros for nonconsumption days ***/ 

    %if (&h_rec24hr_epis_vars ^= 0) %then %do;
      %do hh_epis = 1 %to &h_rec24hr_epis_vars;
    
        if conday_&&rec24hr_epis_var&hh_epis = 0 then stdbc_&&rec24hr_epis_var&hh_epis = 0;
   
      %end;
    %end;
 
  run;


    
    
  %if (&totdietcomp ^= 0) %then %do;

    %if %upcase(&print)=Y %then %do;

      proc means data=stdcov_stdbc24hr_conday_out n nmiss mean std min max;
        var &conday_var_list  
            &stdbc_epis_var_list  
            &stdbc_daily_var_list 
            ;
        title%eval(&titles+1) "Consumption-Day Indicators for Episodically Consumed Components and Standardized Recall Variables";
      run;
      title%eval(&titles+1);

    %end; 

    
    
    
    /************************************************************/
    /*** Create data set of variables for back-transformation ***/
    /************************************************************/
    
    
    data backtran_center_scale(keep = tran_paramindex tran_center tran_scale);
      set &tr_centscal_dataset_list;
    
      tran_paramindex = _n_;
      tran_center = location;
      tran_scale = scale / mult;
    
    run;
    
    
    data backtran_out(keep = tran_paramindex tran_lambda tran_center tran_scale minamount);
      merge &boxcox_tran_lambda_data(keep = tran_paramindex tran_lambda) 
            backtran_center_scale
            &minamount_data(keep = tran_paramindex minamount) 
            ;
      by tran_paramindex;
    run;
      
      
    %if %upcase(&print)=Y %then %do;
    
      proc print data=backtran_out;
        title%eval(&titles+1) "Variables for Back-Transformation";
      run;
      title%eval(&titles+1);
    
    %end; 
      
      
    /*********************************************************/
    
    
    proc datasets lib=work nolist;
      delete ckmiss_24hr_covars                   
             rec_lambda_minamt_1record                  
             stdize_data     
             &centscal_dataset_list 
             &tr_centscal_dataset_list 
             backtran_center_scale    
             ;
    quit;

        
  %end; 
  %else %do; 


    proc datasets lib=work nolist;
      delete ckmiss_24hr_covars                   
             stdize_data     
             ;
    quit;


  %end; 
  



%mend std_cov_boxcox24hr_conday_minamt;  /*** End macro ***/
