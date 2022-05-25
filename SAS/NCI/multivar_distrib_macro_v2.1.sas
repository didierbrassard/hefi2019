
/*****************************************************************************/
/*****************************************************************************/
/*                                                                           */
/*                          MULTIVAR_DISTRIB MACRO                           */
/*                                                                           */
/*****************************************************************************/
/*                        VERSION 2.1             9/28/2017                  */
/*                                                                           */
/*                                                                           */
/* The MULTIVAR_DISTRIB macro uses parameter estimates from the              */
/* MULTIVAR_MCMC macro to generate a multivariate Monte Carlo distribution   */
/* of the usual intakes for the dietary components specified in the          */
/* multivariate measurement error model fit using the MULTIVAR_MCMC macro.   */
/* The MULTIVAR_DISTRIB macro can also use values that were stored using     */
/* the macro parameter "optional_iml_store_names" of the MULTIVAR_MCMC       */
/* macro.  The MULTIVAR_DISTRIB macro allows specification of 1 or 2 lists   */
/* of covariates.  If 2 lists of covariates are specified, the usual intake  */ 
/* is calculated as the weighted average of the usual intake calculated      */
/* using covariate list 1 and the usual intake calculated using covariate    */
/* list 2.  This weighted average is calculated using the values specified   */
/* for the "set_value_for_weight_cov_list1" and                              */
/* "set_value_for_weight_cov_list2" macro parameters.                        */
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
/* The syntax for calling the MULTIVAR_DISTRIB macro is:                     */
/*                                                                           */
/* %multivar_distrib(multivar_mcmc_out_lib           =,                      */
/*                   multivar_mcmc_out_store_label   =,                      */
/*                   t_weightavg_covariates_list1    =,                      */
/*                   t_weightavg_covariates_list2    =,                      */
/*                   set_value_for_weight_cov_list1  =,                      */
/*                   set_value_for_weight_cov_list2  =,                      */
/*                   nev_consumers_epis1             =,                      */
/*                   covars_prob_consumer_epis1      =,                      */
/*                   optional_input_data             =,                      */
/*                   optional_input_data_var_list    =,                      */
/*                   optional_input_mcmc_u_out_data  =,                      */
/*                   additional_output_var_list      =,                      */
/*                   additional_output_subject_var   =,                      */
/*                   output_mcmc_weight_var          =,                      */
/*                   set_seed_distrib                =,                      */
/*                   set_number_monte_carlo_rand_obs =,                      */
/*                   print                           =                       */
/*                   );                                                      */
/*                                                                           */
/*  where                                                                    */
/*                                                                           */
/*  "multivar_mcmc_out_lib"            Specifies the same SAS library that   */
/*                                     was specified for the "out_lib" macro */ 
/*                                     parameter of the MULTIVAR_MCMC macro. */ 
/*                                     The MULTIVAR_DISTRIB macro will use   */
/*                                     the parameter estimates and can use   */
/*                                     covariates that were stored by the    */
/*                                     MULTIVAR_MCMC macro.  Covariates can  */
/*                                     be stored using the                   */
/*                                     "optional_iml_store_names" macro      */ 
/*                                     parameter of the MULTIVAR_MCMC macro. */
/*                                                                           */
/*  "multivar_mcmc_out_store_label"    Specifies the same label that was     */
/*                                     specified for the "out_store_label"   */ 
/*                                     macro parameter of the MULTIVAR_MCMC  */ 
/*                                     macro.  The MULTIVAR_DISTRIB macro    */
/*                                     will use the parameter estimates and  */
/*                                     can use covariates that were stored   */
/*                                     by the MULTIVAR_MCMC macro.           */
/*                                     Covariates can be stored using the    */
/*                                     "optional_iml_store_names" macro      */ 
/*                                     parameter of the MULTIVAR_MCMC macro. */
/*                                                                           */
/*  "t_weightavg_covariates_list1"     Specifies a list of covariates that   */
/*                                     correspond to the beta parameter      */
/*                                     estimates from the multivariate       */
/*                                     measurement error model fit using the */
/*                                     MULTIVAR_MCMC macro.  The             */
/*                                     calculations in the MULTIVAR_DISTRIB  */
/*                                     macro are based on the assumption     */
/*                                     that the multivariate measurement     */
/*                                     error model was fit using the same    */
/*                                     set of covariates for each part of    */
/*                                     the model.  The MULTIVAR_DISTRIB      */ 
/*                                     macro creates "constant0" which can   */
/*                                     be used as a variable that equals 0   */
/*                                     for each observation, and the macro   */
/*                                     creates "constant1" which can be used */
/*                                     as a variable that equals 1 for each  */ 
/*                                     observation.  "Constant0" and         */
/*                                     "constant1" can be used when          */
/*                                     specifying covariates of interest.    */ 
/*                                     If the optional macro parameter       */
/*                                     "t_weightavg_covariates_list2" is     */ 
/*                                     used to specify a second list of      */
/*                                     covariates, then usual intake is      */
/*                                     calculated as the weighted average of */
/*                                     the usual intake calculated using     */ 
/*                                     covariate list 1 and the usual intake */
/*                                     calculated using covariate list 2.    */
/*                                     This weighted average is calculated   */
/*                                     using the values specified for the    */
/*                                     "set_value_for_weight_cov_list1" and  */
/*                                     "set_value_for_weight_cov_list2"      */
/*                                     macro parameters.  If the second list */
/*                                     of covariates is not specified, then  */
/*                                     the Monte Carlo distribution of usual */
/*                                     intakes is only based on the          */
/*                                     covariates specified for the          */
/*                                     "t_weightavg_covariates_list1" macro  */
/*                                     parameter.                            */
/*                                                                           */
/*  "t_weightavg_covariates_list2"     Specifies an optional second list of  */
/*                                     covariates.  If this list of          */
/*                                     covariates is specified, the usual    */
/*                                     intake is calculated as the weighted  */
/*                                     average of the usual intake           */
/*                                     calculated using covariate list 1 and */
/*                                     the usual intake calculated using     */
/*                                     covariate list 2.  This weighted      */
/*                                     average is calculated using the       */
/*                                     values specified for the              */
/*                                     "set_value_for_weight_cov_list1" and  */
/*                                     "set_value_for_weight_cov_list2"      */
/*                                     macro parameters.                     */
/*                                                                           */
/*  "set_value_for_weight_cov_list1"   Specifies a value that is used to     */
/*                                     calculate a weighted average when 2   */
/*                                     covariate lists are provided as       */
/*                                     described for the                     */
/*                                     "t_weightavg_covariates_list1" and    */
/*                                     "t_weightavg_covariates_list2" macro  */
/*                                     parameters.  The value specified is   */
/*                                     used as the numerator of the weight.  */
/*                                     The denominator of the weight is      */
/*                                     defined as the sum of the values from */
/*                                     the "set_value_for_weight_cov_list1"  */
/*                                     and "set_value_for_weight_cov_list2"  */
/*                                     macro parameters.  The default value  */
/*                                     is 1 which corresponds to the         */
/*                                     situation where the                   */
/*                                     "t_weightavg_covariates_list1" macro  */ 
/*                                     parameter is used to provide          */
/*                                     covariate list 1 and the              */
/*                                     "t_weightavg_covariates_list2" macro  */
/*                                     parameter is not used.                */
/*                                                                           */
/*  "set_value_for_weight_cov_list2"   Specifies a value that is used to     */
/*                                     calculate a weighted average when 2   */
/*                                     covariate lists are provided as       */
/*                                     described for the                     */
/*                                     "t_weightavg_covariates_list1" and    */
/*                                     "t_weightavg_covariates_list2" macro  */
/*                                     parameters.  The value specified is   */
/*                                     used as the numerator of the weight.  */
/*                                     The denominator of the weight is      */
/*                                     defined as the sum of the values from */
/*                                     the "set_value_for_weight_cov_list1"  */
/*                                     and "set_value_for_weight_cov_list2"  */
/*                                     macro parameters.  The default value  */
/*                                     is 0 which corresponds to the         */
/*                                     situation where the                   */
/*                                     "t_weightavg_covariates_list2" macro  */
/*                                     parameter is not used.                */
/*                                                                           */
/*  "nev_consumers_epis1"              Specifies the same setting that was   */
/*                                     specified for the                     */
/*                                     "nev_consumers_epis1" macro parameter */
/*                                     of the MULTIVAR_MCMC macro.  The      */
/*                                     default value is "n".                 */
/*                                                                           */
/*  "covars_prob_consumer_epis1"       Specifies the same list of covariates */
/*                                     that was specified for the            */
/*                                     "covars_prob_consumer_epis1"          */
/*                                     macro parameter of the MULTIVAR_MCMC  */
/*                                     macro.                                */
/*                                                                           */
/*  "optional_input_data"              Specifies an optional SAS data set    */
/*                                     that provides the additional input    */
/*                                     variables listed in the               */
/*                                     "optional_input_data_var_list" macro  */
/*                                     parameter.  If this data set is       */
/*                                     specified and the                     */
/*                                     "additional_output_subject_var" macro */
/*                                     parameter specifies a subject         */
/*                                     identification variable, then the     */
/*                                     subject identification variable is    */
/*                                     included in the output Monte Carlo    */
/*                                     data set.  When an optional input     */
/*                                     data set is not specified, the Monte  */
/*                                     Carlo "mc_t_distrib_out" data set     */
/*                                     includes the "weight_nw_sumw"         */
/*                                     variable created and stored by the    */
/*                                     MULTIVAR_MCMC macro (see the          */
/*                                     MULTIVAR_MCMC "weight_var" macro      */
/*                                     parameter for details).  When an      */
/*                                     optional input data set is specified, */
/*                                     an advanced user can use the          */
/*                                     "output_mcmc_weight_var" macro        */
/*                                     parameter to include the              */
/*                                     "weight_nw_sumw" variable in the      */
/*                                     "mc_t_distrib_out" data set if        */
/*                                     appropriate.  When an optional input  */
/*                                     data set and both the                 */
/*                                     "additional_output_var_list" and      */
/*                                     "additional_output_subject_var" macro */
/*                                     parameters are specified, an advanced */
/*                                     user can include these additional     */
/*                                     variables in the "mc_t_distrib_out"   */
/*                                     data set.                             */
/*                                                                           */
/*  "optional_input_data_var_list"     Specifies optional input variables.   */
/*                                     For details, see the description for  */
/*                                     the "optional_input_data" macro       */
/*                                     parameter.  This variable list should */
/*                                     not include names of variables stored */
/*                                     by the MULTIVAR_MCMC macro since the  */ 
/*                                     stored values are brought into the    */ 
/*                                     macro as described for the            */ 
/*                                     "multivar_mcmc_out_lib" and           */ 
/*                                     "multivar_mcmc_out_store_label" macro */ 
/*                                     parameters.                           */
/*                                                                           */
/*  "optional_input_mcmc_u_out_data"   Specifies an optional SAS data set    */
/*                                     that provides u matrices saved from   */
/*                                     iterations of the MULTIVAR_MCMC       */
/*                                     macro.  Considering the MULTIVAR_MCMC */
/*                                     "set_number_post_mcmc_u_out" macro    */
/*                                     parameter, the variables required for */
/*                                     this input data set include:          */
/*                                     - "Iteration" which indexes the first */
/*                                       u matrix through last u matrix in   */
/*                                       the data set,                       */ 
/*                                     - the variable specified by the       */
/*                                       "subject" macro parameter from the  */ 
/*                                       MULTIVAR_MCMC macro,                */
/*                                     - u_col1, u_col2, ..., u_colL where L */
/*                                       is the number of columns of the u   */
/*                                       matrix.  L is defined in the        */
/*                                       description of the                  */ 
/*                                       "out_store_label" macro parameter   */ 
/*                                       of the MULTIVAR_MCMC macro.         */ 
/*                                     The MULTIVAR_DISTRIB macro will use   */
/*                                     the first (i.e. iteration 1) u matrix */
/*                                     under the assumption that the rows of */
/*                                     this matrix are still sorted in the   */
/*                                     same order as when the MULTIVAR_MCMC  */
/*                                     macro generated this u matrix, and    */
/*                                     the MULTIVAR_DISTRIB macro will       */
/*                                     generate and append additional rows   */
/*                                     to this matrix as needed to obtain    */
/*                                     the dimensions as specified and       */
/*                                     implied by other user input to this   */
/*                                     macro.  The MULTIVAR_DISTRIB macro    */ 
/*                                     will use this same approach for each  */ 
/*                                     subsequent u matrix and iteration.    */
/*                                                                           */
/*  "additional_output_var_list"       Specifies additional output           */
/*                                     variables.  For details, see the      */
/*                                     description for the                   */
/*                                     "optional_input_data" macro           */
/*                                     parameter.                            */
/*                                                                           */
/*  "additional_output_subject_var"    Specifies a subject identification    */
/*                                     variable to be included in the output */
/*                                     Monte Carlo data set.  For details,   */
/*                                     see the description for the           */
/*                                     "optional_input_data" macro           */
/*                                     parameter.                            */
/*                                                                           */
/*  "output_mcmc_weight_var"           When an optional input data set is    */
/*                                     specified, an advanced user can       */ 
/*                                     specify "output_mcmc_weight_var=y" or */
/*                                     "output_mcmc_weight_var=Y" to include */
/*                                     the "weight_nw_sumw" variable in the  */
/*                                     "mc_t_distrib_out" data set if        */
/*                                     appropriate.  For details, see the    */
/*                                     description for the                   */
/*                                     "optional_input_data" macro           */
/*                                     parameter.  The default value is "n". */
/*                                                                           */
/*  "set_seed_distrib"                 Specifies a seed for random number    */
/*                                     generation for the Monte Carlo method */
/*                                     used in the macro.  If                */
/*                                     "set_seed_distrib" is not specified,  */ 
/*                                     the SAS IML procedure will generate   */
/*                                     an initial seed value from the system */
/*                                     clock.                                */
/*                                                                           */
/*  "set_number_monte_carlo_rand_obs"  Specifies the number of observations  */
/*                                     of the random effects vector to       */
/*                                     generate for an individual.  If an    */
/*                                     "optional_input_mcmc_u_out_data" SAS  */
/*                                     data set is specified, then the       */
/*                                     "set_number_monte_carlo_rand_obs"     */
/*                                     macro parameter is not used, and the  */
/*                                     number of observations of the random  */
/*                                     effects vector is determined by the   */
/*                                     maximum value of the "Iteration"      */
/*                                     index variable.  Additional details   */
/*                                     are provided in the description of    */
/*                                     the "optional_input_mcmc_u_out_data"  */
/*                                     macro parameter.  If an               */ 
/*                                     "optional_input_mcmc_u_out_data" SAS  */ 
/*                                     data set is not specified, the        */
/*                                     number of observations in the Monte   */
/*                                     Carlo data set is the product of the  */
/*                                     "number of individuals" and the       */
/*                                     specified value for the               */
/*                                     "set_number_monte_carlo_rand_obs"     */
/*                                     macro parameter.  The default value   */
/*                                     is 500.                               */
/*                                                                           */
/*  "print"                            If "print=n" or "print=N" then macro  */
/*                                     information is not printed.           */
/*                                     Otherwise the macro prints            */
/*                                     information regarding the IML storage */
/*                                     catalog used and prints some stored   */
/*                                     and specified values.  The default    */
/*                                     value is "y".                         */
/*                                                                           */
/*****************************************************************************/
/*                                                                           */
/* Macro Output Data Set:  mc_t_distrib_out                                  */
/*                                                                           */
/* The MULTIVAR_DISTRIB macro produces a SAS data set "mc_t_distrib_out"     */
/* that includes usual intake variables named:                               */
/*          mc_t1   mc_t2   ...   mc_tp                                      */
/* where p = num_epis_diet_comp + num_daily_diet_comp, and the dietary       */
/* component order (1, 2, ..., p) is equivalent to the order used when the   */
/* multivariate measurement error model was fit using the MULTIVAR_MCMC      */
/* macro.  When the "t_weightavg_covariates_list2" macro parameter is not    */
/* used, the output data set "mc_t_distrib_out" includes the following       */
/* variables used to calculate the usual intake variables:                   */
/*          mc_prob1   mc_prob2   ...   mc_probq                             */
/*          mc_amount1   mc_amount2   ...   mc_amountq                       */
/* where q = num_epis_diet_comp, and these variables correspond to the       */ 
/* probability to consume and the intake amount on consumption days.         */ 
/*                                                                           */
/* When the "optional_input_data" macro parameter is not used to specify an  */
/* optional input data set, the "mc_t_distrib_out" data set also             */
/* includes the variable:                                                    */
/*          weight_nw_sumw                                                   */
/* created and stored by the MULTIVAR_MCMC macro (see the MULTIVAR_MCMC      */
/* "weight_var" macro parameter for details).                                */
/*                                                                           */
/* Additional variables can be stored in the "mc_t_distrib_out" data set     */
/* (see the macro parameter descriptions for details).                       */
/*                                                                           */
/* If an "optional_input_mcmc_u_out_data" SAS data set is not specified, the */
/* number of observations in the output data set is the product of the       */
/* "number of individuals" and the specified value for the                   */
/* "set_number_monte_carlo_rand_obs" macro parameter (see the macro          */
/* parameter descriptions for details).                                      */
/*                                                                           */
/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/


%macro multivar_distrib(multivar_mcmc_out_lib           =,
                        multivar_mcmc_out_store_label   =, 
                        t_weightavg_covariates_list1    =, 
                        t_weightavg_covariates_list2    =, 
                        set_value_for_weight_cov_list1  = 1,
                        set_value_for_weight_cov_list2  = 0,
                        nev_consumers_epis1             = n,
                        covars_prob_consumer_epis1      =,
                        optional_input_data             =,
                        optional_input_data_var_list    =, 
                        optional_input_mcmc_u_out_data  =,
                        additional_output_var_list      =, 
                        additional_output_subject_var   =, 
                        output_mcmc_weight_var          = n, 
                        set_seed_distrib                =,
                        set_number_monte_carlo_rand_obs = 500, 
                        print                           = y  
                        );




  /**********************************************************************************/
  /*** Ensure user macro call does not overwrite default values with a null value ***/  
  /**********************************************************************************/

  %if (&set_value_for_weight_cov_list1=%str()) %then %let set_value_for_weight_cov_list1 = 1;
  %if (&set_value_for_weight_cov_list2=%str()) %then %let set_value_for_weight_cov_list2 = 0;
  %if (&nev_consumers_epis1=%str()) %then %let nev_consumers_epis1 = n;
  %if (&output_mcmc_weight_var=%str()) %then %let output_mcmc_weight_var = n;
  %if (&set_number_monte_carlo_rand_obs=%str()) %then %let set_number_monte_carlo_rand_obs = 500;
  %if (&print=%str()) %then %let print = y;
  

  /***************************/
  /*** Process macro input ***/
  /***************************/

  %let x_covariates_list1 = ;
  %let x_covariates_list2 = ;
  %let pp_t_weightavg_covar = 0;
  
  %do %while (%quote(%scan(&t_weightavg_covariates_list1, %eval(&pp_t_weightavg_covar + 1), %str( ))) ^= %str());

    %let pp_t_weightavg_covar = %eval(&pp_t_weightavg_covar + 1);

    %let t_weightavg_list1_covar&pp_t_weightavg_covar = %scan(&t_weightavg_covariates_list1, &pp_t_weightavg_covar, %str( ));
    %if (&pp_t_weightavg_covar = 1) %then %let x_covariates_list1 = &&t_weightavg_list1_covar&pp_t_weightavg_covar;
    %else %let x_covariates_list1 = &x_covariates_list1  %str( || )  &&t_weightavg_list1_covar&pp_t_weightavg_covar;

    %if (&t_weightavg_covariates_list2^=%str()) %then %do;
      %let t_weightavg_list2_covar&pp_t_weightavg_covar = %scan(&t_weightavg_covariates_list2, &pp_t_weightavg_covar, %str( ));
      %if (&pp_t_weightavg_covar = 1) %then %let x_covariates_list2 = &&t_weightavg_list2_covar&pp_t_weightavg_covar;
      %else %let x_covariates_list2 = &x_covariates_list2  %str( || )  &&t_weightavg_list2_covar&pp_t_weightavg_covar;
    %end;

  %end;


  %if (%upcase(&nev_consumers_epis1)=Y) %then %do;  
      
    /*** Allow for never-consumers ***/

    %let set_num_nev_consumers_vars=1; 
  
    %let g_covariates_list = ;
    %let ww_g_covar = 0;
    
    %do %while (%quote(%scan(&covars_prob_consumer_epis1, %eval(&ww_g_covar + 1), %str( ))) ^= %str());
   
      %let ww_g_covar = %eval(&ww_g_covar + 1);
   
      %let g_covariate&ww_g_covar = %scan(&covars_prob_consumer_epis1, &ww_g_covar, %str( ));
      %if (&ww_g_covar = 1) %then %let g_covariates_list = &&g_covariate&ww_g_covar;
      %else %let g_covariates_list = &g_covariates_list  %str( || )  &&g_covariate&ww_g_covar;
   
    %end;

  %end;
  %else %let set_num_nev_consumers_vars=0;  




  /*****************************************************************************************************/
  /*** Reset the storage and load stored values from the MCMC calculations.  Create macro variables. ***/
  /*****************************************************************************************************/

  proc iml;

    reset storage=&multivar_mcmc_out_lib..iml&multivar_mcmc_out_store_label;
      
    load;


    num_dietcomp = num_epis_diet_comp + num_daily_diet_comp;
    call symputx("totdietcomp", num_dietcomp);

    %if (&t_weightavg_covariates_list2=%str()) %then %do;
      call symputx("numepisdietcomp", num_epis_diet_comp);
    %end;

    call symputx("totvars", num_rows_covmat);

  quit;



  
  /*****************************************************************************************************/
  /*****************************************************************************************************/

  proc iml;

    start gbc_inverse(z_vector, bc_lambda_scalar, minamt_scalar);

      /*** Computes the inverse of the Box-Cox transformation and ensures result >= minimum amount ***/

      if bc_lambda_scalar = 0 then do;
        notneg_gbc_inv = exp(z_vector);
      end;
      else do;
        temp_notneg = (1 + bc_lambda_scalar # z_vector) <> 0;
        notneg_gbc_inv = temp_notneg ## (1 / bc_lambda_scalar);
      end;
      
      gbc_inv = notneg_gbc_inv <> minamt_scalar;

      return (gbc_inv);
        
    finish gbc_inverse;


    start backtransform(n, tran_lambda_j, tran_center_j, tran_scale_j, xbeta_u_j, sigmae_jj, minamt_j);

      /*** When lambda is zero, compute the exact backtransformation for the dietary component ***/
      /*** When lambda is not zero, compute the 9 point backtransformation for the dietary component ***/
      
      /*** Abscissas and weights ***/ 

      x9pt = {-2.1, -1.3, -0.8, -0.5, 0.0, 0.5, 0.8, 1.3, 2.1};
      w9pt_given = {0.063345, 0.080255, 0.070458, 0.159698, 0.252489, 0.159698, 0.070458, 0.080255, 0.063345};
      w9pt = w9pt_given / sum(w9pt_given);


      gstar = j(n, 1, 0);

      if tran_lambda_j = 0 then do;
        notneg_gstar = exp( tran_center_j + tran_scale_j # xbeta_u_j + ((tran_scale_j##2) # sigmae_jj / 2) );
        gstar = notneg_gstar <> minamt_j;
      end;
      else do;
        do qq = 1 to 9;
        
          bc_qq9pt = tran_center_j + tran_scale_j # (xbeta_u_j + x9pt[qq, 1] # (sigmae_jj)##0.5);
          g_inv_qq9pt = gbc_inverse(bc_qq9pt, tran_lambda_j, minamt_j);
          
          gstar = gstar + w9pt[qq, 1] # g_inv_qq9pt;
       
        end;
      end;  

      return (gstar);
        
    finish backtransform;




    /*******************************************************************************************/
    /*** Reset the storage, load stored values from the MCMC calculations, and call randseed ***/
    /*******************************************************************************************/
    
    reset storage=&multivar_mcmc_out_lib..iml&multivar_mcmc_out_store_label;
      
    load;


    %if (&set_seed_distrib ^= %str()) %then %do;
      call randseed(&set_seed_distrib);
    %end;
 



    /*************************************************************************************************/
    /*** Read optional input data set, calculate sample size, check MCMC weight variable dimension ***/
    /*************************************************************************************************/
 
    %if (&optional_input_data ^= %str()) %then %do;

      use &optional_input_data  var _all_  nobs n_sample_size;

      %if %upcase(&print)^=N %then %do;
        print "Output from the Show Contents Statement for optional_input_data = &optional_input_data",,;
        show contents;
      %end;
 
      free &optional_input_data_var_list &additional_output_subject_var; 
      read all var{&optional_input_data_var_list &additional_output_subject_var};


      m1_stored_sample_size = nrow(weight_nw_sumw);
      m2_sample_size = n_sample_size - m1_stored_sample_size;   

      call symputx("stored_sample_size", m1_stored_sample_size);
      call symputx("dataset_sample_size", n_sample_size);

      %let keep_mcmc_weight_var = &output_mcmc_weight_var;

      %if (%upcase(&output_mcmc_weight_var)=Y) & (&stored_sample_size^=&dataset_sample_size) %then %do; 
        %put *** MULTIVAR_DISTRIB Macro Note:  optional_input_data and stored MCMC weight variable have unequal row dimensions, so output_mcmc_weight_var ignored.;
        %let keep_mcmc_weight_var = n;   
      %end;

      
    %end;
    %else %do;
      n_sample_size = nrow(weight_nw_sumw);
      m1_stored_sample_size = n_sample_size;
      m2_sample_size = 0;   
      %let keep_mcmc_weight_var = y;
    %end;
      



    /*********************************************************/
    /*** Perform initial calculations and create variables ***/
    /*********************************************************/
    

    num_dietcomp = num_epis_diet_comp + num_daily_diet_comp;
    one_plus_num_epis_diet_comp = num_epis_diet_comp + 1;

    constant0 = j(n_sample_size, 1, 0);
    constant1 = j(n_sample_size, 1, 1);


    /*************/

  
    t_weightavg_w_given = {&set_value_for_weight_cov_list1  &set_value_for_weight_cov_list2};
    sum_t_weightavg_w_given = sum(t_weightavg_w_given);
    t_weightavg_weight1 = &set_value_for_weight_cov_list1 / sum_t_weightavg_w_given;
 
    xmatrix_cov_list1 = &x_covariates_list1;
    xbeta_cov_list1 = j(n_sample_size, &totvars, .);

    %do varj = 1 %to &totvars;
      xbeta_cov_list1[, &varj] = (xmatrix_cov_list1 * beta&varj._mean);
    %end;


    %if (&t_weightavg_covariates_list2^=%str()) %then %do;

      t_weightavg_weight2 = &set_value_for_weight_cov_list2 / sum_t_weightavg_w_given;

      xmatrix_cov_list2 = &x_covariates_list2;
      xbeta_cov_list2 = j(n_sample_size, &totvars, .);

      %do varj = 1 %to &totvars;
        xbeta_cov_list2[, &varj] = (xmatrix_cov_list2 * beta&varj._mean);
      %end;

    %end;


    %if (&set_num_nev_consumers_vars = 1) %then %do;
      
      /*** Allow for never-consumers ***/
  
      /*** Covariates used to model the probability of being  ***/
      /*** a consumer of the first episodically consumed food ***/

      gmatrix_1strep_prconep1 = &g_covariates_list;
  
      gmatrix_1strep_prconep1_alpha1 = gmatrix_1strep_prconep1 * alpha1_mean;
      prob_consumer_nx1_epis1 = cdf('normal', gmatrix_1strep_prconep1_alpha1);

    %end;

  
    /*************/
  

    call eigen(eigvals_sigmau, eigvecs_sigmau, sigmau_mean);
    eigvalshalf_sigmau = eigvals_sigmau##0.5;
    sqrt_sigmau = eigvecs_sigmau * diag(eigvalshalf_sigmau) * t(eigvecs_sigmau);




    /*************/
    /*** Print ***/
    /*************/

    %if %upcase(&print)^=N %then %do;

    
      print "**************** Output from the Show Storage Statement ****************",,;
      show storage;

      print "**************** Stored Values and Dimensions ****************",, 
        num_epis_diet_comp    num_daily_diet_comp    num_rows_covmat,,   
        n_sample_size[label="Number of Individuals"],,
        %if (&optional_input_mcmc_u_out_data = %str()) %then %do;
          "Macro Parameter:  set_number_monte_carlo_rand_obs  &set_number_monte_carlo_rand_obs",,
        %end;
        ;
 
    %end;   


    /***************************************************************/
    /*** Generate random effects and create Monte Carlo data set ***/
    /***************************************************************/


  %if (&optional_input_mcmc_u_out_data ^= %str()) %then %do;

    use &multivar_mcmc_out_lib..&optional_input_mcmc_u_out_data;
    read all var {iteration}; 
    replicates_mcmc_u_out_data = max(iteration);
    close &multivar_mcmc_out_lib..&optional_input_mcmc_u_out_data;

    do b = 1 to replicates_mcmc_u_out_data;

      use &multivar_mcmc_out_lib..&optional_input_mcmc_u_out_data  where(iteration=b);
      
      %if %upcase(&print)^=N %then %do;
        if b=1 then do;
          print "Output from the Show Contents Statement for optional_input_mcmc_u_out_data = &optional_input_mcmc_u_out_data",,;
          show contents;
        end; 
      %end;
      
      read all  var {%do varj = 1 %to &totvars;
                       u_col&varj  
                     %end;  
                     } into umatrix;

      close &multivar_mcmc_out_lib..&optional_input_mcmc_u_out_data;

      if m2_sample_size > 0 then do;
       
        z_u = j(m2_sample_size, num_rows_covmat, .);
        call randgen(z_u, 'normal');
        u_m2 = z_u * sqrt_sigmau;

        u = umatrix // u_m2;

      end;
      else if m2_sample_size = 0 then u = umatrix;

  %end;
  %else %do;

    do b = 1 to &set_number_monte_carlo_rand_obs;

      z_u = j(n_sample_size, num_rows_covmat, .);
      call randgen(z_u, 'normal');
      u = z_u * sqrt_sigmau;

  %end;
      

      xbeta_u_cov_list1 = xbeta_cov_list1 + u;
      %if (&t_weightavg_covariates_list2^=%str()) %then %do;
        xbeta_u_cov_list2 = xbeta_cov_list2 + u;
      %end; 


      replic_b_mc_t = j(n_sample_size, num_dietcomp, .); 
      %if (&t_weightavg_covariates_list2=%str()) %then %do;
        replic_b_mc_prob = j(n_sample_size, num_epis_diet_comp, .);
        replic_b_mc_amount = j(n_sample_size, num_epis_diet_comp, .);
      %end;
      

      /*** Calculate t using a 9 point approximation for episodically consumed dietary components ***/

      do dietcompj_epis = 1 to num_epis_diet_comp;

        var2jamt = 2 # dietcompj_epis;   /*** Define index variable ***/
        var2jminus1prob = var2jamt - 1;  /*** Define index variable ***/

        /*** Covariate list 1 ***/
       
        g_star = backtransform(n_sample_size, tran_lambda[dietcompj_epis, 1], tran_center[dietcompj_epis, 1], 
                               tran_scale[dietcompj_epis, 1], xbeta_u_cov_list1[, var2jamt], sigmae_mean[var2jamt, var2jamt], 
                               minamount[dietcompj_epis, 1]); 

        norm_cum = cdf('normal', xbeta_u_cov_list1[, var2jminus1prob]); 
        t_cov_list1 = norm_cum # g_star; 


        %if (&t_weightavg_covariates_list2^=%str()) %then %do;

          /*** Covariate list 2 ***/
       
          g_star = backtransform(n_sample_size, tran_lambda[dietcompj_epis, 1], tran_center[dietcompj_epis, 1], 
                                 tran_scale[dietcompj_epis, 1], xbeta_u_cov_list2[, var2jamt], sigmae_mean[var2jamt, var2jamt], 
                                 minamount[dietcompj_epis, 1]); 
          
          norm_cum = cdf('normal', xbeta_u_cov_list2[, var2jminus1prob]); 
          t_cov_list2 = norm_cum # g_star; 
        
          /************************/

        
          replic_b_mc_t[, dietcompj_epis] = t_weightavg_weight1 # t_cov_list1 + t_weightavg_weight2 # t_cov_list2;

          %let replic_b_mc_probamount_label = ;

        %end; 
        %else %do;       

          replic_b_mc_t[, dietcompj_epis] = t_cov_list1;

          replic_b_mc_prob[, dietcompj_epis] = norm_cum;
          replic_b_mc_amount[, dietcompj_epis] = g_star;

          %let replic_b_mc_probamount_label = %str( || replic_b_mc_prob || replic_b_mc_amount);

        %end; 

      end;


      %if (&set_num_nev_consumers_vars = 1) %then %do;  
      
        /*** Allow for never-consumers ***/
     
        ss_unifnx1 = j(n_sample_size, 1, .);
        call randgen(ss_unifnx1, 'uniform');

        t_epis1_temp = replic_b_mc_t[, 1];   
        replic_b_mc_t[, 1] = t_epis1_temp # (ss_unifnx1 <= prob_consumer_nx1_epis1);

        %if (&t_weightavg_covariates_list2=%str()) %then %do;

          prob_epis1_temp = replic_b_mc_prob[, 1];   
          replic_b_mc_prob[, 1] = prob_epis1_temp # (ss_unifnx1 <= prob_consumer_nx1_epis1);

          amount_epis1_temp = replic_b_mc_amount[, 1];   
          replic_b_mc_amount[, 1] = (amount_epis1_temp # (ss_unifnx1 <= prob_consumer_nx1_epis1)) + (-99 # (ss_unifnx1 > prob_consumer_nx1_epis1));

        %end;
      
      %end;




      /*** Calculate t using a 9 point approximation for daily consumed dietary components ***/

      do dietcompj_daily = one_plus_num_epis_diet_comp to num_dietcomp;

        varkdailyamt = dietcompj_daily + num_epis_diet_comp;  /*** Define index variable ***/


        /*** Covariate list 1 ***/
    
        g_star = backtransform(n_sample_size, tran_lambda[dietcompj_daily, 1], tran_center[dietcompj_daily, 1], 
                               tran_scale[dietcompj_daily, 1], xbeta_u_cov_list1[, varkdailyamt], sigmae_mean[varkdailyamt, varkdailyamt], 
                               minamount[dietcompj_daily, 1]); 

        t_cov_list1 = g_star; 


        %if (&t_weightavg_covariates_list2^=%str()) %then %do;

          /*** Covariate list 2 ***/
        
          g_star = backtransform(n_sample_size, tran_lambda[dietcompj_daily, 1], tran_center[dietcompj_daily, 1], 
                                 tran_scale[dietcompj_daily, 1], xbeta_u_cov_list2[, varkdailyamt], sigmae_mean[varkdailyamt, varkdailyamt], 
                                 minamount[dietcompj_daily, 1]); 
        
          t_cov_list2 = g_star; 
        
        
          /************************/
 
        
          replic_b_mc_t[, dietcompj_daily] = t_weightavg_weight1 # t_cov_list1 + t_weightavg_weight2 # t_cov_list2;


        %end; 
        %else %do;       

          replic_b_mc_t[, dietcompj_daily] = t_cov_list1;

        %end; 
       
       
      end;


      /*******************************************************************/


      mc_replicate = j(n_sample_size, 1, b);


      %if (&optional_input_data = %str()) %then %do;

        %let othervars_quotedlist = "mc_replicate" "weight_nw_sumw";
        replic_b_mc_t_outmatrix = mc_replicate || weight_nw_sumw || replic_b_mc_t &replic_b_mc_probamount_label;

      %end; 
      %else %do; 

        %if (&additional_output_subject_var ^= %str()) %then %do;

          %if (%upcase(&keep_mcmc_weight_var)=Y) %then %do;
            %let othervars_quotedlist = "mc_replicate" "&additional_output_subject_var" "weight_nw_sumw";
            replic_b_mc_t_outmatrix = mc_replicate || &additional_output_subject_var || weight_nw_sumw || replic_b_mc_t &replic_b_mc_probamount_label;
          %end; 
          %else %do;
            %let othervars_quotedlist = "mc_replicate" "&additional_output_subject_var";
            replic_b_mc_t_outmatrix = mc_replicate || &additional_output_subject_var || replic_b_mc_t &replic_b_mc_probamount_label;
          %end; 

        %end; 
        %else %do;

          %if (%upcase(&keep_mcmc_weight_var)=Y) %then %do;
            %let othervars_quotedlist = "mc_replicate" "weight_nw_sumw";
            replic_b_mc_t_outmatrix = mc_replicate || weight_nw_sumw || replic_b_mc_t &replic_b_mc_probamount_label;
          %end; 
          %else %do;
            %let othervars_quotedlist = "mc_replicate";
            replic_b_mc_t_outmatrix = mc_replicate || replic_b_mc_t &replic_b_mc_probamount_label;
          %end; 

        %end; 

      %end; 




      mc_t_outmatrix = mc_t_outmatrix // replic_b_mc_t_outmatrix;

      /*******************************************************************/


    end;


        

    /*************************************************************************************/ 
    /*************************************************************************************/ 


    othervarslabel = {&othervars_quotedlist};
    mc_tlabel = "mc_t1":"mc_t&totdietcomp";  
 

    %if (&t_weightavg_covariates_list2=%str()) %then %do;

      mc_problabel = "mc_prob1":"mc_prob&numepisdietcomp";  
      mc_amountlabel = "mc_amount1":"mc_amount&numepisdietcomp";  
 
      varnames = othervarslabel || mc_tlabel || mc_problabel || mc_amountlabel;
 
    %end;
    %else %do;

      varnames = othervarslabel || mc_tlabel;
    
    %end;
    
   
    create mc_t_distrib_out from mc_t_outmatrix [colname=varnames];
    append from mc_t_outmatrix;
    close mc_t_distrib_out;


  quit;




  %if (&set_num_nev_consumers_vars = 1) %then %do;  
  
    /*** Allow for never-consumers ***/
 
    data mc_t_distrib_out;
      set mc_t_distrib_out; 
      if mc_amount1 = -99 then mc_amount1=.;
    run;
  
  %end;


  %if ((&optional_input_data ^= %str()) & (&additional_output_var_list ^= %str()) & (&additional_output_subject_var ^= %str())) %then %do;

    proc sort data = mc_t_distrib_out;
      by &additional_output_subject_var mc_replicate;
    run;
   
    data mc_t_distrib_out;
      merge mc_t_distrib_out &optional_input_data(keep = &additional_output_subject_var &additional_output_var_list); 
      by &additional_output_subject_var;
    run;

    proc sort data = mc_t_distrib_out;
      by mc_replicate &additional_output_subject_var;
    run;
   
  %end;


%mend;
