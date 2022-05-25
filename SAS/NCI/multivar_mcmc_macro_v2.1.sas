/*****************************************************************************/
/*****************************************************************************/
/*                                                                           */
/*                            MULTIVAR_MCMC MACRO                            */
/*                                                                           */
/*****************************************************************************/
/*                        VERSION 2.1       9/26/2017                        */
/*                                                                           */
/*                                                                           */
/* The MULTIVAR_MCMC macro is used to fit a multivariate measurement error   */
/* model that can include episodically consumed dietary components and       */
/* non-episodically consumed dietary components.  The measurement error      */ 
/* model is fit using the Markov Chain Monte Carlo (MCMC) algorithm from     */
/* Zhang et al. (2011a, 2011b).  This methodology and macro also allow for   */
/* survey-weighted MCMC computations.  Output from this macro can be saved   */
/* and used in subsequent analysis steps such as the estimation of           */
/* multivariate usual intake distributions and the use of regression         */
/* calibration in health outcome models to assess diet and health            */
/* relationships.                                                            */
/*                                                                           */
/* The input data for this macro can be prepared using the                   */
/* STD_COV_BOXCOX24HR_CONDAY_MINAMT macro which is used to standardize       */
/* continuous covariates, transform and standardize reported nonzero amounts */
/* (e.g. from 24-hour recalls), and create variables needed for the          */
/* MULTIVAR_MCMC macro.                                                      */
/*                                                                           */
/* The calculations performed by the STD_COV_BOXCOX24HR_CONDAY_MINAMT macro  */
/* are summarized as follows:                                                */
/*    1.  Continuous covariates are standardized to have a mean of 0 and a   */
/*        variance of 1.                                                     */
/*    2.  For dietary components consumed episodically, consumption-day      */
/*        indicator variables are created, and the following transformation  */
/*        approach, used by Zhang et al. (2011a, 2011b), is performed.       */
/*        First, a suitable Box-Cox transformation is applied to the nonzero */
/*        values.  Second, the Box-Cox transformed values are centered and   */
/*        scaled to obtain an approximate normal distribution with a mean of */
/*        0 and a variance of 2.                                             */ 
/*    3.  For dietary components that are consumed nearly every day, as      */
/*        specified by the user, zero values are replaced with minimum       */
/*        amount values provided by the user or calculated as half of the    */
/*        observed nonzero minimum amount.  Then the following               */
/*        transformation approach, used by Zhang et al. (2011a, 2011b), is   */
/*        performed for the dietary components that are non-episodically     */
/*        consumed.  First, a suitable Box-Cox transformation is applied.    */
/*        Second, the Box-Cox transformed values are centered and scaled to  */
/*        obtain an approximate normal distribution with a mean of 0 and a   */
/*        variance of 2.                                                     */
/*                                                                           */
/* The MULTIVAR_MCMC macro requires SAS IML version 9.2 or higher.           */
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
/* The syntax for calling the MULTIVAR_MCMC macro is:                        */
/*                                                                           */
/* %multivar_mcmc(data                        =,                             */
/*                subject                     =,                             */
/*                weight_var                  =,                             */
/*                repeat                      =,                             */
/*                conday_epis_vars            =,                             */
/*                gst_rec24hr_epis_vars       =,                             */
/*                gst_rec24hr_daily_vars      =,                             */
/*                covars_epis_prob            =,                             */
/*                covars_epis_amt             =,                             */
/*                covars_daily_amt            =,                             */
/*                nev_consumers_epis1         =,                             */
/*                covars_prob_consumer_epis1  =,                             */
/*                set_seed_mcmc               =,                             */
/*                set_number_mcmc_iterations  =,                             */
/*                set_number_burn_iterations  =,                             */
/*                set_thin                    =,                             */
/*                prior_sigmau_mean_data      =,                             */
/*                sigmau_constant             =,                             */
/*                gen_inverse                 =,                             */
/*                print                       =,                             */
/*                titles                      =,                             */
/*                std_print_store             =,                             */
/*                notes_print                 =,                             */
/*                out_lib                     =,                             */
/*                out_store_label             =,                             */
/*                out_save_label_max5char     =,                             */
/*                set_number_saved_out_data   =,                             */
/*                save_mcmc_u_out_data        =,                             */
/*                set_number_post_mcmc_u_out  =,                             */
/*                traceplots_method1_gpath    =,                             */ 
/*                traceplots_method2_file_pdf =,                             */
/*                optional_iml_store_data     =,                             */
/*                optional_iml_store_names    =                              */
/*                );                                                         */
/*                                                                           */
/*  where                                                                    */
/*                                                                           */
/*  "data"                        Specifies an input data set that includes  */
/*                                one or more observations for each subject. */
/*                                An adequate number of subjects should have */
/*                                at least two days of reported consumption  */
/*                                for episodically consumed dietary          */
/*                                components, so covariance matrix           */
/*                                parameters can be estimated.               */
/*                                                                           */
/*  "subject"                     Specifies a variable that uniquely         */
/*                                identifies each subject.                   */
/*                                                                           */
/*  "weight_var"                  Specifies a weight variable that is used   */
/*                                to create a new variable "weight_nw_sumw"  */
/*                                defined as:                                */
/*                                    the number of subjects, n, multiplied  */
/*                                    by the input weight value and divided  */
/*                                    by the sum of the input weight values. */ 
/*                                If "weight_var" is not specified, an       */
/*                                unweighted analysis is performed where the */
/*                                value of "weight_nw_sumw" is 1 for each    */
/*                                subject.                                   */
/*                                                                           */
/*  "repeat"                      Specifies a variable that indexes repeated */
/*                                observations for each subject.             */
/*                                                                           */
/*  "conday_epis_vars"            Specifies a list of variables that         */ 
/*                                includes a consumption-day indicator       */
/*                                variable for each episodically consumed    */
/*                                dietary component.                         */
/*                                                                           */
/*  "gst_rec24hr_epis_vars"       Specifies a list of variables that         */ 
/*                                includes an appropriately transformed      */
/*                                24-hour recall variable for each           */
/*                                episodically consumed dietary component.   */
/*                                With the priors used in the MCMC, the      */
/*                                following transformation approach has been */
/*                                used by Zhang et al. (2011a, 2011b).       */
/*                                First, a suitable Box-Cox transformation   */
/*                                is applied to the nonzero values.          */
/*                                Second, the Box-Cox transformed values are */
/*                                centered and scaled to obtain an           */
/*                                approximate normal distribution with a     */
/*                                mean of 0 and a variance of 2.             */
/*                                                                           */
/*  "gst_rec24hr_daily_vars"      Specifies a list of variables that         */ 
/*                                includes an appropriately transformed      */
/*                                24-hour recall variable for each daily     */
/*                                consumed dietary component.                */
/*                                With the priors used in the MCMC, the      */
/*                                following transformation approach has been */
/*                                used by Zhang et al. (2011a, 2011b).       */
/*                                First, a suitable Box-Cox transformation   */
/*                                is applied.                                */
/*                                Second, the Box-Cox transformed values are */
/*                                centered and scaled to obtain an           */
/*                                approximate normal distribution with a     */
/*                                mean of 0 and a variance of 2.             */
/*                                                                           */
/*  "covars_epis_prob"            Specifies a list of covariates for each of */ 
/*                                the consumption-day indicator variables    */
/*                                for the episodically consumed dietary      */
/*                                components.  If one list of covariates is  */
/*                                given, these covariates will be used for   */
/*                                all of the episodically consumed dietary   */
/*                                components.  More generally, separate      */
/*                                lists of covariates can be specified for   */
/*                                each dietary component if each list is     */
/*                                preceded with the following keyword:       */
/*                                STARTCOVARLIST.  To include an intercept,  */
/*                                the list of covariates should include a    */
/*                                constant such as the variable "constant1"  */
/*                                that is created by the macro and equals 1  */
/*                                for each observation of the input data     */
/*                                set.                                       */
/*                                                                           */
/*  "covars_epis_amt"             Specifies a list of covariates for each of */ 
/*                                the transformed 24-hour recall variables   */
/*                                for the episodically consumed dietary      */
/*                                components.  If one list of covariates is  */
/*                                given, these covariates will be used for   */
/*                                all of the episodically consumed dietary   */
/*                                components.  More generally, separate      */
/*                                lists of covariates can be specified for   */
/*                                each dietary component if each list is     */
/*                                preceded with the following keyword:       */
/*                                STARTCOVARLIST.  To include an intercept,  */
/*                                the list of covariates should include a    */
/*                                constant such as the variable "constant1"  */
/*                                that is created by the macro and equals 1  */
/*                                for each observation of the input data     */
/*                                set.                                       */
/*                                                                           */
/*  "covars_daily_amt"            Specifies a list of covariates for each of */ 
/*                                the transformed 24-hour recall variables   */
/*                                for the daily consumed dietary components. */
/*                                If one list of covariates is given, these  */
/*                                covariates will be used for all of the     */
/*                                daily consumed dietary components.  More   */
/*                                generally, separate lists of covariates    */
/*                                can be specified for each dietary          */
/*                                component if each list is preceded with    */
/*                                the following keyword:                     */
/*                                STARTCOVARLIST.  To include an intercept,  */
/*                                the list of covariates should include a    */
/*                                constant such as the variable "constant1"  */
/*                                that is created by the macro and equals 1  */
/*                                for each observation of the input data     */
/*                                set.                                       */
/*                                                                           */
/*  "nev_consumers_epis1"         If "nev_consumers_epis1=y" or              */ 
/*                                "nev_consumers_epis1=Y" then the           */ 
/*                                calculations will allow for never-         */
/*                                consumers for the first dietary component  */
/*                                listed in the "conday_epis_vars" and the   */
/*                                "gst_rec24hr_epis_vars" variable lists.    */ 
/*                                The default value is "n".                  */ 
/*                                                                           */
/*  "covars_prob_consumer_epis1"  Specifies a list of covariates used to     */
/*                                model the probability of being a consumer  */ 
/*                                of the first dietary component listed in   */
/*                                the "conday_epis_vars" and the             */
/*                                "gst_rec24hr_epis_vars" variable lists.    */ 
/*                                To include an intercept, the list of       */
/*                                covariates should include a constant such  */
/*                                as the variable "constant1" that is        */
/*                                created by the macro and equals 1 for each */
/*                                observation of the input data set.  This   */
/*                                list of covariates is only used if         */ 
/*                                "nev_consumers_epis1=y" or                 */ 
/*                                "nev_consumers_epis1=Y".                   */ 
/*                                                                           */
/*  "set_seed_mcmc"               Specifies a seed for random number         */
/*                                generation in the MCMC algorithm.  If      */
/*                                "set_seed_mcmc" is not specified, the SAS  */
/*                                IML procedure will generate an initial     */
/*                                seed value from the system clock.          */
/*                                                                           */
/*  "set_number_mcmc_iterations"  Specifies the total number of iterations   */
/*                                for the MCMC algorithm including the       */
/*                                burn-in.  The default value is 12000.      */
/*                                                                           */
/*  "set_number_burn_iterations"  Specifies the burn-in value for the MCMC   */
/*                                algorithm.  The default value is 2000.     */
/*                                                                           */
/*  "set_thin"                    Specifies the thinning value for the MCMC  */
/*                                algorithm.  The default value is 25.       */
/*                                                                           */
/*  "prior_sigmau_mean_data"      Specifies an optional SAS data set used to */
/*                                define the mean of the prior distribution  */
/*                                for the sigmau covariance matrix.  The     */
/*                                values from this SAS data set are read     */
/*                                into a SAS IML procedure matrix,           */
/*                                prior_sigmau_mean, so the optional input   */
/*                                SAS data set can only include the values   */
/*                                needed to define this matrix.  Also, these */
/*                                values should be appropriately arranged    */
/*                                with each data set variable corresponding  */
/*                                to a column of this matrix.  Advanced      */
/*                                users can use this macro parameter in      */
/*                                conjunction with the "sigmau_constant"     */
/*                                macro parameter.  The description for the  */
/*                                "out_store_label" macro parameter and      */
/*                                references provide notational details.     */
/*                                                                           */
/*  "sigmau_constant"             If "sigmau_constant=y" or                  */ 
/*                                "sigmau_constant=Y" then the sigmau        */
/*                                covariance matrix is not updated during    */
/*                                the MCMC iterative calculations.  Advanced */
/*                                users can use this macro parameter in      */
/*                                conjunction with the                       */ 
/*                                "prior_sigmau_mean_data" macro parameter.  */
/*                                The description for the "out_store_label"  */
/*                                macro parameter and the references provide */
/*                                notational details.  The default value is  */
/*                                "n".                                       */ 
/*                                                                           */
/*  "gen_inverse"                 If "gen_inverse=n" or "gen_inverse=N" then */
/*                                the SAS IML "inv" function is used to      */
/*                                compute the inverse of matrices as needed  */
/*                                throughout the algorithm.  This            */
/*                                specification may save computational time, */
/*                                but the user may encounter numerical       */
/*                                problems and an error message regarding a  */
/*                                matrix that should be non-singular.  The   */
/*                                default value is "y".  By default, the SAS */
/*                                IML "ginv" function is used to compute the */
/*                                Moore-Penrose generalized inverse of       */
/*                                matrices as needed throughout the          */
/*                                algorithm.                                 */
/*                                                                           */
/*  "print"                       If "print=n" or "print=N" then macro       */
/*                                results are not printed.  The default      */
/*                                value is "y".                              */
/*                                                                           */
/*  "titles"                      Specifies the number of title lines to be  */
/*                                reserved for the user's titles.  One       */
/*                                additional title line is used by the       */
/*                                macro.  The default value is "0".          */
/*                                                                           */
/*  "std_print_store"             If "std_print_store=y" or                  */ 
/*                                "std_print_store=Y" then sample standard   */ 
/*                                deviations are printed and stored in the   */
/*                                IML storage catalog.  The default value is */
/*                                "n" because some analyses require more     */ 
/*                                sophisticated methods of variance          */ 
/*                                estimation.  For example, replication      */
/*                                methods such as balanced repeated          */
/*                                replication (BRR) are used for analysis of */ 
/*                                data from a survey with complex sampling.  */
/*                                                                           */
/*  "notes_print"                 If "notes_print=n" or "notes_print=N" then */
/*                                notes are not printed to the SAS log.  The */
/*                                default value is "y".                      */
/*                                                                           */
/*  "out_lib"                     Specifies a SAS library that the macro     */
/*                                uses when saving the IML storage catalog   */
/*                                file and optional SAS data sets.           */
/*                                The description for the "out_store_label"  */
/*                                macro parameter includes additional        */
/*                                details regarding the IML storage catalog. */
/*                                The following SAS output data sets         */
/*                                    "multivar_mcmc_samples_out"            */                                  
/*                                    "multivar_mcmc_samples_u_out"          */
/*                                    "multivar_post_mcmc_u_out"             */
/*                                are requested using the                    */
/*                                "set_number_saved_out_data" and            */
/*                                "save_mcmc_u_out_data" and                 */ 
/*                                "set_number_post_mcmc_u_out" macro         */
/*                                parameters and are saved in the library    */
/*                                specified by the "out_lib" macro           */ 
/*                                parameter.                                 */ 
/*                                                                           */
/*  "out_store_label"             Specifies a label for the SAS IML storage  */
/*                                catalog.  The catalog name is defined by   */
/*                                adding the prefix "iml" to the user        */
/*                                supplied label.  The catalog is stored in  */
/*                                the library specified by the "out_lib"     */
/*                                macro parameter.  After deleting the       */
/*                                current contents of the catalog, the       */
/*                                following matrices (including vectors and  */
/*                                scalars) are stored.                       */
/*                                "weight_nw_sumw"                           */
/*                                    The number of subjects, n, multiplied  */
/*                                    by the input weight value and divided  */
/*                                    by the sum of the input weight values. */ 
/*                                "num_epis_diet_comp"                       */
/*                                    The number of episodically consumed    */
/*                                    dietary components.                    */
/*                                "num_daily_diet_comp"                      */
/*                                    The number of daily consumed dietary   */
/*                                    components.                            */
/*                                "num_rows_covmat"                          */
/*                                    The number of rows (or columns) of the */
/*                                    LxL covariance matrices, sigmau_mean   */
/*                                    and sigmae_mean, where                 */
/*                                    L = (2 * num_epis_diet_comp)           */
/*                                         + num_daily_diet_comp.            */ 
/*                                "beta1_mean", ..., "betaL_mean"            */
/*                                "sigmau_mean"                              */
/*                                "sigmae_mean"                              */
/*                                    Sample means calculated using          */
/*                                    generated values from MCMC iterations  */
/*                                    selected according to the              */
/*                                    "set_number_burn_iterations" and       */ 
/*                                    "set_thin" macro parameters.  The rows */
/*                                    of the LxL covariance matrices,        */
/*                                    sigmau_mean and sigmae_mean, are       */
/*                                    arranged, so the episodically consumed */
/*                                    dietary components precede the daily   */
/*                                    consumed dietary components.  For      */
/*                                    example, 3 episodically consumed       */
/*                                    dietary components and 2 daily         */
/*                                    consumed dietary components yield      */
/*                                    (2*3 + 2 = 8 = L) rows sorted as:      */
/*                                    1. consumption probability (epis 1),   */
/*                                    2. amount consumed         (epis 1),   */
/*                                    3. consumption probability (epis 2),   */
/*                                    4. amount consumed         (epis 2),   */
/*                                    5. consumption probability (epis 3),   */
/*                                    6. amount consumed         (epis 3),   */
/*                                    7. amount consumed         (daily 1),  */
/*                                    8. amount consumed         (daily 2),  */
/*                                "beta1_std", ..., "betaL_std"              */
/*                                "sigmau_std"                               */
/*                                "sigmae_std"                               */
/*                                    Sample standard deviations calculated  */
/*                                    using generated values from MCMC       */
/*                                    iterations selected according to the   */
/*                                    "set_number_burn_iterations" and       */
/*                                    "set_thin" macro parameters.  These    */
/*                                    sample standard deviations are stored  */
/*                                    when "std_print_store=y" or            */
/*                                    "std_print_store=Y".                   */
/*                                                                           */
/*  "out_save_label_max5char"     Specifies an optional label to append to   */
/*                                the end of the following SAS output data   */
/*                                set names:                                 */
/*                                    "multivar_mcmc_samples_out"            */                                  
/*                                    "multivar_mcmc_samples_u_out"          */
/*                                    "multivar_post_mcmc_u_out".            */
/*                                These SAS output data sets are requested   */
/*                                using the                                  */ 
/*                                "set_number_saved_out_data" and            */
/*                                "save_mcmc_u_out_data" and                 */ 
/*                                "set_number_post_mcmc_u_out" macro         */
/*                                parameters and are saved in the library    */
/*                                specified by the "out_lib" macro           */ 
/*                                parameter.  Only the first 5 characters of */ 
/*                                the label are used.                        */
/*                                                                           */
/*  "set_number_saved_out_data"   Specifies the number of iterations to save */
/*                                in a SAS output data set                   */
/*                                "multivar_mcmc_samples_out" that is        */
/*                                saved in the library specified by the      */
/*                                "out_lib" macro parameter.  The data set   */
/*                                includes MCMC samples for the Beta,        */
/*                                Sigma_u, and Sigma_e parameters with       */ 
/*                                values selected from among the iterations  */
/*                                specified according to the                 */
/*                                "set_number_burn_iterations" and           */ 
/*                                "set_thin" macro parameters.  The output   */
/*                                data set name can be extended to include   */ 
/*                                an optional label specified using the      */ 
/*                                "out_save_label_max5char" macro parameter. */ 
/*                                The default value is "0".                  */
/*                                                                           */
/*  "save_mcmc_u_out_data"        If "save_mcmc_u_out_data=y" or             */ 
/*                                "save_mcmc_u_out_data=Y" then the MCMC     */ 
/*                                samples for the u matrix are saved in a    */
/*                                SAS output data set                        */
/*                                "multivar_mcmc_samples_u_out" in the       */
/*                                library specified by the "out_lib" macro   */
/*                                parameter.  The variables include:         */
/*                                    - "Iteration" which identifies the     */
/*                                      MCMC sample,                         */
/*                                    - the variable specified by the        */
/*                                      "subject" macro parameter,           */
/*                                    - u_col1, u_col2, ..., u_colL where L  */
/*                                      is the number of columns of the u    */
/*                                      matrix.  L is defined in the         */
/*                                      description of the "out_store_label" */
/*                                      macro parameter.                     */ 
/*                                The data set includes MCMC samples         */
/*                                selected from among the iterations         */
/*                                specified according to the                 */
/*                                "set_number_burn_iterations" and           */ 
/*                                "set_thin" macro parameters.  The output   */
/*                                data set name can be extended to include   */ 
/*                                an optional label specified using the      */ 
/*                                "out_save_label_max5char" macro parameter. */ 
/*                                The default value is "n".                  */
/*                                                                           */
/*  "set_number_post_mcmc_u_out"  Specifies the number of post MCMC          */
/*                                iterations and the number of post MCMC     */ 
/*                                samples for the u matrix that are saved in */
/*                                a SAS output data set                      */
/*                                "multivar_post_mcmc_u_out" in the          */
/*                                library specified by the "out_lib" macro   */
/*                                parameter.  The variables include:         */
/*                                    - "Iteration" which identifies the     */
/*                                      post MCMC sample,                    */
/*                                    - the variable specified by the        */
/*                                      "subject" macro parameter,           */
/*                                    - u_col1, u_col2, ..., u_colL where L  */
/*                                      is the number of columns of the u    */
/*                                      matrix.  L is defined in the         */
/*                                      description of the "out_store_label" */
/*                                      macro parameter.                     */ 
/*                                The output data set name can be extended   */ 
/*                                to include an optional label specified     */ 
/*                                using the "out_save_label_max5char" macro  */
/*                                parameter.  The default value is "0".      */
/*                                                                           */
/*  "traceplots_method1_gpath"    Specifies a valid SAS fileref to indicate  */
/*                                a folder used for storing the MCMC trace   */
/*                                plot files.  The fileref is used by the    */
/*                                "gpath" option of the "ods listing"        */
/*                                statement in the SAS Output Delivery       */
/*                                System (ODS).  Each trace plot file        */
/*                                includes up to 4 trace plots               */
/*                                (i.e. 4 panels).  Example file names       */
/*                                include:                                   */
/*                                    "Beta_Trace_Plot_Panels4_Image.png"    */
/*                                    "Sigmau_Trace_Plot_Panels4_Image.png"  */
/*                                    "Sigmae_Trace_Plot_Panels4_Image.png"  */
/*                                and SAS ODS will add an image index number */
/*                                to ensure that each file produced has a    */
/*                                unique name.  MCMC trace plots are         */
/*                                produced for all of the Beta, Sigma_u, and */
/*                                Sigma_e parameters.  The plots include     */
/*                                MCMC samples selected according to the     */
/*                                "set_number_burn_iterations" and           */
/*                                "set_thin" macro parameters.  If no value  */
/*                                is specified, these trace plot files are   */
/*                                not produced.  Trace plots can be produced */
/*                                using another method as described for the  */
/*                                "traceplots_method2_file_pdf" macro        */
/*                                parameter.                                 */
/*                                                                           */
/*  "traceplots_method2_file_pdf" Specifies a file name with a "pdf"         */
/*                                extension, such as                         */
/*                                "example1.traceplot.pdf".  This pdf file   */
/*                                will include the MCMC trace plots for all  */
/*                                of the Beta, Sigma_u, and Sigma_e          */
/*                                parameters.  The plots include MCMC        */
/*                                samples selected according to the          */
/*                                "set_number_burn_iterations" and           */ 
/*                                "set_thin" macro parameters.  If no value  */ 
/*                                is specified, this trace plot file is not  */ 
/*                                produced.  Trace plots can be produced     */
/*                                using another method as described for the  */
/*                                "traceplots_method1_gpath" macro           */
/*                                parameter.  The pdf file produced by trace */
/*                                plot method 2 tends to have a much larger  */
/*                                file size when compared to the set of      */
/*                                files produced by trace plot method 1.     */ 
/*                                                                           */
/*  "optional_iml_store_data"     Specifies an optional SAS data set that    */
/*                                includes useful variables to be stored in  */ 
/*                                the IML storage catalog.  The names of     */
/*                                these variables must be specified in the   */
/*                                "optional_iml_store_names" macro           */
/*                                parameter.                                 */
/*                                                                           */
/*  "optional_iml_store_names"    Specifies a list of optional matrices      */
/*                                (including vectors and scalars) to be      */
/*                                stored in the IML storage catalog.  This   */
/*                                macro parameter can be used in conjunction */
/*                                with the "optional_iml_store_data" macro   */
/*                                parameter, and it can be used by advanced  */
/*                                users that want to save values that are    */
/*                                defined in the SAS IML procedure within    */
/*                                the macro.                                 */
/*                                                                           */
/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

%macro multivar_mcmc(data                        =,
                     subject                     =,
                     weight_var                  =,  
                     repeat                      =, 
                     conday_epis_vars            =,
                     gst_rec24hr_epis_vars       =,
                     gst_rec24hr_daily_vars      =,
                     covars_epis_prob            =,
                     covars_epis_amt             =,
                     covars_daily_amt            =,
                     nev_consumers_epis1         = n,
                     covars_prob_consumer_epis1  =,
                     set_seed_mcmc               =,
                     set_number_mcmc_iterations  = 12000,
                     set_number_burn_iterations  = 2000,
                     set_thin                    = 25, 
                     prior_sigmau_mean_data      =,
                     sigmau_constant             = n,
                     gen_inverse                 = y,
                     print                       = y,  
                     titles                      = 0,
                     std_print_store             = n,
                     notes_print                 = y,
                     out_lib                     =,
                     out_store_label             =,
                     out_save_label_max5char     =,
                     set_number_saved_out_data   = 0,
                     save_mcmc_u_out_data        = n,
                     set_number_post_mcmc_u_out  = 0, 
                     traceplots_method1_gpath    =,  
                     traceplots_method2_file_pdf =,
                     optional_iml_store_data     =,
                     optional_iml_store_names    =
                     );                 




  /**********************************************************************************/
  /*** Ensure user macro call does not overwrite default values with a null value ***/  
  /**********************************************************************************/

  %if (&nev_consumers_epis1=%str()) %then %let nev_consumers_epis1 = n;
  %if (&set_number_mcmc_iterations=%str()) %then %let set_number_mcmc_iterations = 12000;
  %if (&set_number_burn_iterations=%str()) %then %let set_number_burn_iterations = 2000;
  %if (&set_thin=%str()) %then %let set_thin = 25;
  %if (&sigmau_constant=%str()) %then %let sigmau_constant = n;
  %if (&gen_inverse=%str()) %then %let gen_inverse = y;
  %if (&print=%str()) %then %let print = y;
  %if (&titles=%str()) %then %let titles = 0;
  %if (&std_print_store=%str()) %then %let std_print_store = n;
  %if (&notes_print=%str()) %then %let notes_print = y;
  %if (&set_number_saved_out_data=%str()) %then %let set_number_saved_out_data = 0;
  %if (&save_mcmc_u_out_data=%str()) %then %let save_mcmc_u_out_data = n;
  %if (&set_number_post_mcmc_u_out=%str()) %then %let set_number_post_mcmc_u_out = 0;
  %if %length(&out_save_label_max5char) gt 5 %then %let out_save_label_max5char = %substr(&out_save_label_max5char,1,5);

  /**************************************************************************************/
  /*** Process macro input and create temporary input data set and constant1 variable ***/
  /**************************************************************************************/

  %if (%upcase(&notes_print)=Y) %then %do;
    options notes;
  %end;
  %else %if (%upcase(&notes_print)=N) %then %do;
    options nonotes;
  %end;
  %else %put *** MULTIVAR_MCMC Macro Error:  Unknown value for notes_print.;


  %if (%upcase(&nev_consumers_epis1)=Y) %then %let set_num_nev_consumers_vars=1;
  %else %let set_num_nev_consumers_vars=0;  


  %if (%upcase(&gen_inverse)=N) %then %let gen_inverse_label=;
  %else %let gen_inverse_label=g;

  
  data temp_input_data;
    set &data;
    constant1 = 1;
  run;


  /****************************************/
  /*** Calculate maximum number of days ***/  
  /****************************************/

  proc means data=temp_input_data(keep = &repeat)  n nmiss mean std min max noprint;
    var &repeat;
    output out = keep_repeat_max(keep = repeat_max)
           max = repeat_max
           / autoname
           ;
    title%eval(&titles+1) "Number of Days";
  run;
  title%eval(&titles+1);


  /**************************************************************/
  /*** Assign macro variables using input parameters and data ***/  
  /**************************************************************/

  data _null_;
    if _n_ = 1 then set keep_repeat_max; 
    set temp_input_data(obs=1);

    %if (&conday_epis_vars = %str()) %then %do;
      dim_ar_conday = 0;
    %end;
    %else %do;
      array ar_conday(*)  &conday_epis_vars; 
      dim_ar_conday = dim(ar_conday);
    %end;

    %if (&gst_rec24hr_epis_vars = %str()) %then %do;
      dim_ar_24epis = 0;
    %end;
    %else %do;
      array ar_24epis(*)  &gst_rec24hr_epis_vars; 
      dim_ar_24epis = dim(ar_24epis);
    %end;

    %if (&gst_rec24hr_daily_vars = %str()) %then %do;
      dim_ar_24daily = 0;
    %end;
    %else %do;
      array ar_24daily(*)  &gst_rec24hr_daily_vars; 
      dim_ar_24daily = dim(ar_24daily);
    %end;




    %if (&covars_epis_prob = %str()) %then %do;
      dim_ar_covars_prob = 0;
    %end;
    %else %do;
      array ar_covars_prob(*)  &covars_epis_prob; 
      dim_ar_covars_prob = dim(ar_covars_prob);
    %end;

    %if (&covars_epis_amt = %str()) %then %do;
      dim_ar_covars_epamt = 0;
    %end;
    %else %do;
      array ar_covars_epamt(*)  &covars_epis_amt; 
      dim_ar_covars_epamt = dim(ar_covars_epamt);
    %end;

    %if (&covars_daily_amt = %str()) %then %do;
      dim_ar_covars_damt = 0;
    %end;
    %else %do;
      array ar_covars_damt(*)  &covars_daily_amt; 
      dim_ar_covars_damt = dim(ar_covars_damt);
    %end;

    %if (&set_num_nev_consumers_vars = 1) %then %do;
      
      /*** Allow for never-consumers ***/
  
      %if (&covars_prob_consumer_epis1 = %str()) %then %do;
        dim_ar_covars_prconep1 = 0;
      %end;
      %else %do;
        array ar_covars_prconep1(*)  &covars_prob_consumer_epis1; 
        dim_ar_covars_prconep1 = dim(ar_covars_prconep1);
      %end;
    %end;
                   
                     
                     
  
    call symputx("maxnumdays", put(repeat_max, 5.));


    /*** For convenience, naming convention uses foods ***/
    call symputx("totcondayfoods", put(dim_ar_conday, 5.));
    call symputx("totfoods", put(dim_ar_24epis, 5.));


    /*** For convenience, naming convention uses nutrients ***/
    call symputx("totnutrients", put(dim_ar_24daily, 5.));


    call symputx("tot_covars_prob", put(dim_ar_covars_prob, 5.));
    call symputx("tot_covars_epamt", put(dim_ar_covars_epamt, 5.));
    call symputx("tot_covars_damt", put(dim_ar_covars_damt, 5.));
    %if (&set_num_nev_consumers_vars = 1) %then %do;
      
      /*** Allow for never-consumers ***/
  
      call symputx("tot_covars_prconep1", put(dim_ar_covars_prconep1, 5.));

    %end;
  run;


  %let totfoodsminus1 = %eval(&totfoods - 1);
  %let totfoodsplus1 = %eval(&totfoods + 1);
  %let totfoodvars = %eval(2*&totfoods);
  %let totfoodvarsplus1 = %eval(&totfoodvars + 1);
  %let totnutrvars = &totnutrients;
  %let totdietcomp = %eval(&totfoods + &totnutrients);
  %let totvars = %eval(&totfoodvars + &totnutrvars);


  %if (%upcase(&notes_print)=Y) %then %do;
    %put *** MULTIVAR_MCMC Macro Note:  The number of 24-hour recalls (i.e. maximum number of days) is &maxnumdays;
    %put *** MULTIVAR_MCMC Macro Note:  The number of episodically consumed dietary components is &totfoods; 
  %end;

  %if (&totfoods ^= &totcondayfoods) %then 
    %put *** MULTIVAR_MCMC Macro Error:  The number of episodically consumed dietary components does not equal the number of consumption-day indicator variables, &totcondayfoods;  

  %if (%upcase(&notes_print)=Y) %then %do;
    %put *** MULTIVAR_MCMC Macro Note:  The number of response variables for episodically consumed dietary components is &totfoodvars;
    %put *** MULTIVAR_MCMC Macro Note:  The number of daily consumed dietary components is &totnutrients;  
    %put *** MULTIVAR_MCMC Macro Note:  The number of response variables for daily consumed dietary components is &totnutrvars;  
    %put *** MULTIVAR_MCMC Macro Note:  The total number of response variables is &totvars;

    %put *** MULTIVAR_MCMC Macro Note:  The dimension of the covars_epis_prob input parameter is &tot_covars_prob;
    %put *** MULTIVAR_MCMC Macro Note:  The dimension of the covars_epis_amt input parameter is &tot_covars_epamt;
    %put *** MULTIVAR_MCMC Macro Note:  The dimension of the covars_daily_amt input parameter is &tot_covars_damt;

    %if (&set_num_nev_consumers_vars = 1) %then %do;

      /*** Allow for never-consumers ***/
 
      %put *** MULTIVAR_MCMC Macro Note:  The dimension of the covars_prob_consumer_epis1 input parameter is &tot_covars_prconep1;

    %end;

  %end;


  /**********************************************/
  /*** Prepare macro variables for covariates ***/
  /**********************************************/

  %if (&totfoods ^= 0) %then %do;

    /*** Initialize macro variables for probability and amount covariates for episodically consumed foods ***/
  
    %do foodhh = 1 %to &totfoods;
      %let covars_prob_&foodhh = ;
      %let covars_epamt_&foodhh = ;
    %end;
  
  
    /*** Probability covariates for episodically consumed foods ***/

    %let foodp = 0;
    
    %do covindex = 1 %to &tot_covars_prob;
      %let currentcov = %scan(&covars_epis_prob, &covindex, %str( ));
      %if ((&covindex = 1) & (%upcase(&currentcov) ^= STARTCOVARLIST)) %then %do; 
        %let foodp = 1;
        %let covars_prob_1 = &currentcov;
      %end; 
      %else %if (%upcase(&currentcov) = STARTCOVARLIST) %then %let foodp = %eval(&foodp + 1);
      %else %if (&foodp <= &totfoods) %then %let covars_prob_&foodp = &&covars_prob_&foodp &currentcov;
    %end;
    
    %if ((&foodp = 1) & (&totfoods ^= 1)) %then %do;
      %do foodr = 2 %to &totfoods;
        %let covars_prob_&foodr = &covars_prob_1;
      %end;
    %end;  
  
  
    /*** Amount covariates for episodically consumed foods ***/
  
    %let fooda = 0;
    
    %do covindex = 1 %to &tot_covars_epamt;
      %let currentcov = %scan(&covars_epis_amt, &covindex, %str( ));
      %if ((&covindex = 1) & (%upcase(&currentcov) ^= STARTCOVARLIST)) %then %do; 
        %let fooda = 1;
        %let covars_epamt_1 = &currentcov;
      %end;  
      %else %if (%upcase(&currentcov) = STARTCOVARLIST) %then %let fooda = %eval(&fooda + 1);
      %else %if (&fooda <= &totfoods) %then %let covars_epamt_&fooda = &&covars_epamt_&fooda &currentcov;
    %end;
    
    %if ((&fooda = 1) & (&totfoods ^= 1)) %then %do;
      %do foodb = 2 %to &totfoods;
        %let covars_epamt_&foodb = &covars_epamt_1;
      %end;
    %end;

      
    /***********************************/
    
    %do foodjj = 1 %to &totfoods;

      %if (&&covars_prob_&foodjj = %str()) %then %put *** MULTIVAR_MCMC Macro Error:  Covariates for probability are not specified for episodically consumed dietary component &foodjj; 
      %else %if (%upcase(&notes_print)=Y) %then %put *** MULTIVAR_MCMC Macro Note:  covars_prob_&foodjj are &&covars_prob_&foodjj;

      %if (&&covars_epamt_&foodjj = %str()) %then %put *** MULTIVAR_MCMC Macro Error:  Covariates for amount are not specified for episodically consumed dietary component &foodjj; 
      %else %if (%upcase(&notes_print)=Y) %then %put *** MULTIVAR_MCMC Macro Note:  covars_epamt_&foodjj are &&covars_epamt_&foodjj;

    %end;
  
  %end;
      
      
  /**********************************************/
  /**********************************************/
  
  %if (&totnutrients ^= 0) %then %do;

    /*** Initialize macro variables for amount covariates for daily consumed nutrients or foods ***/
  
    %do nutrhh = 1 %to &totnutrients;
      %let covars_damt_&nutrhh = ;
    %end;
  
  
    /*** Amount covariates for daily consumed nutrients or foods ***/
  
    %let nutra = 0;
    
    %do covindex = 1 %to &tot_covars_damt;
      %let currentcov = %scan(&covars_daily_amt, &covindex, %str( ));
      %if ((&covindex = 1) & (%upcase(&currentcov) ^= STARTCOVARLIST)) %then %do; 
        %let nutra = 1;
        %let covars_damt_1 = &currentcov;
      %end; 
      %else %if (%upcase(&currentcov) = STARTCOVARLIST) %then %let nutra = %eval(&nutra + 1);
      %else %if (&nutra <= &totnutrients) %then %let covars_damt_&nutra = &&covars_damt_&nutra &currentcov;
    %end;
    
    %if ((&nutra = 1) & (&totnutrients ^= 1)) %then %do;
      %do nutrb = 2 %to &totnutrients;
        %let covars_damt_&nutrb = &covars_damt_1;
      %end;
    %end;

      
    /***********************************/
    
    %do nutrjj = 1 %to &totnutrients;
      %if (&&covars_damt_&nutrjj = %str()) %then %put *** MULTIVAR_MCMC Macro Error:  Covariates for amount are not specified for daily consumed dietary component &nutrjj; 
      %else %if (%upcase(&notes_print)=Y) %then %put *** MULTIVAR_MCMC Macro Note:  covars_damt_&nutrjj are &&covars_damt_&nutrjj;
    %end;
  
  %end;




  data _null_;
    set temp_input_data(obs=1);


    /*** Probability and amount covariates for episodically consumed foods ***/

    %if (&totfoods ^= 0) %then %do;
   
      %do foodjj = 1 %to &totfoods;

        array ar_covars_prob_&foodjj(*)  &&covars_prob_&foodjj; 
        dim_ar_covars_prob_&foodjj = dim(ar_covars_prob_&foodjj);
   
        array ar_covars_epamt_&foodjj(*)  &&covars_epamt_&foodjj; 
        dim_ar_covars_epamt_&foodjj = dim(ar_covars_epamt_&foodjj);
   
        call symputx("tot_covars_prob_&foodjj", put(dim_ar_covars_prob_&foodjj, 5.));
        call symputx("tot_covars_epamt_&foodjj", put(dim_ar_covars_epamt_&foodjj, 5.));
   
      %end;

    %end;


    /*** Amount covariates for daily consumed nutrients or foods ***/
  
    %if (&totnutrients ^= 0) %then %do;
    
      %do nutrjj = 1 %to &totnutrients;

        array ar_covars_damt_&nutrjj(*)  &&covars_damt_&nutrjj; 
        dim_ar_covars_damt_&nutrjj = dim(ar_covars_damt_&nutrjj);

        call symputx("tot_covars_damt_&nutrjj", put(dim_ar_covars_damt_&nutrjj, 5.));

      %end;
    
    %end;
    
  run;




  %let varpp = 0;


  %if (&totfoods ^= 0) %then %do;
    %do foodjj = 1 %to &totfoods;

      %if (%upcase(&notes_print)=Y) %then %do;
        %put *** MULTIVAR_MCMC Macro Note:  The number of covariates from covars_epis_prob for episodically consumed component &foodjj is &&tot_covars_prob_&foodjj;
        %put *** MULTIVAR_MCMC Macro Note:  The number of covariates from covars_epis_amt for episodically consumed component &foodjj is &&tot_covars_epamt_&foodjj;
      %end; 

      %let varpp = %eval(&varpp + 1);
      %let param_count_beta&varpp = &&tot_covars_prob_&foodjj;
      
      %let varpp = %eval(&varpp + 1);
      %let param_count_beta&varpp = &&tot_covars_epamt_&foodjj;

    %end;
  %end;


  %if (&totnutrients ^= 0) %then %do;
    %do nutrjj = 1 %to &totnutrients;

      %if (%upcase(&notes_print)=Y) %then %do;
        %put *** MULTIVAR_MCMC Macro Note:  The number of covariates from covars_daily_amt for daily consumed component &nutrjj is &&tot_covars_damt_&nutrjj;
      %end; 

      %let varpp = %eval(&varpp + 1);
      %let param_count_beta&varpp = &&tot_covars_damt_&nutrjj;

    %end;
  %end;


  %if (&set_num_nev_consumers_vars = 1) %then %do;

    /*** Allow for never-consumers ***/

    %if (%upcase(&notes_print)=Y) %then %do;
      %put *** MULTIVAR_MCMC Macro Note:  The number of covariates from covars_prob_consumer_epis1 is &tot_covars_prconep1;
    %end; 

  %end;



      
  /**********************************************/
  /**********************************************/
  /**********************************************/




  proc sort data=temp_input_data;
    by &subject &repeat;
  run;


  data mcmc_subj1recdata(drop = &repeat &conday_epis_vars &gst_rec24hr_epis_vars &gst_rec24hr_daily_vars);
    if _n_ = 1 then set keep_repeat_max; 
    set temp_input_data;
    by &subject &repeat;


    /***********************************************************/
    /*** Recall variables and covariates from input data set ***/
    /***********************************************************/


    %if (&set_num_nev_consumers_vars = 1) %then %do;

      /*** Allow for never-consumers ***/
 
      array ar_covars_prconep1(&tot_covars_prconep1)  &covars_prob_consumer_epis1; 

    %end;


    %if (&totfoods ^= 0) %then %do;


      /*** Consumption-day indicator variables for episodically consumed foods ***/

      array ar_conday(&totfoods) &conday_epis_vars;
          

      /*** Amounts for episodically consumed foods ***/

      array ar_24epis(&totfoods) &gst_rec24hr_epis_vars;


      /*** Probability and amount covariates for episodically consumed foods ***/

      %do foodjj = 1 %to &totfoods;
        array ar_covars_prob_&foodjj(&&tot_covars_prob_&foodjj)  &&covars_prob_&foodjj; 
        array ar_covars_epamt_&foodjj(&&tot_covars_epamt_&foodjj)  &&covars_epamt_&foodjj; 
      %end;
          
    %end;

    
    %if (&totnutrients ^= 0) %then %do;


      /*** Amounts for daily consumed nutrients or foods ***/

      array ar_24daily(&totnutrients) &gst_rec24hr_daily_vars;


      /*** Amount covariates for daily consumed nutrients or foods ***/

      %do nutrjj = 1 %to &totnutrients;
        array ar_covars_damt_&nutrjj(&&tot_covars_damt_&nutrjj)  &&covars_damt_&nutrjj; 
      %end;

    %end;  


    /*********************************************************************************/
    /*** Prepare response variables and covariates and output 1 record per subject ***/
    /*********************************************************************************/


    %if (&set_num_nev_consumers_vars = 1) %then %do;

      /*** Allow for never-consumers ***/
 
      %if (&tot_covars_prconep1 = 1) %then %do;  
        %let retain_covars_prconep1 = g_1strep_prconep1_covar1;
        array ar_covars_1strep_prconep1(&tot_covars_prconep1) g_1strep_prconep1_covar1;
      %end;
      %else %do;
        %let retain_covars_prconep1 =  
                g_1strep_prconep1_covar1 - g_1strep_prconep1_covar&tot_covars_prconep1;
        array ar_covars_1strep_prconep1(&tot_covars_prconep1) 
                g_1strep_prconep1_covar1 - g_1strep_prconep1_covar&tot_covars_prconep1;
      %end;

      if first.&subject then do;
        do covar_prconep1 = 1 to &tot_covars_prconep1;
          ar_covars_1strep_prconep1(covar_prconep1) = ar_covars_prconep1(covar_prconep1);
        end;
      end;

    %end;


    %if (&totfoods ^= 0) %then %do;

      %let retain_conday_var = ;
      %let retain_24epis_var = ;

      %do foodh = 1 %to &totfoods;
        %let retain_covars_prob_&foodh._var = ;
        %let retain_covars_epamt_&foodh._var = ;
      %end;

    %end;

 
    %if (&totnutrients ^= 0) %then %do;

      %let retain_24daily_var = ;

      %do nutrjj = 1 %to &totnutrients;
        %let retain_covars_damt_&nutrjj._var = ;
      %end;

    %end;




    %if (&maxnumdays = 1) %then %do;

      retain day_avail_indicat_k1;

    %end;
    %else %do;

      retain day_avail_indicat_k1 - day_avail_indicat_k&maxnumdays;

    %end;



    
    if first.&subject then do;
      %do dayk = 1 %to &maxnumdays;
        day_avail_indicat_k&dayk = 0;
      %end;
    end;




    %do dayk = 1 %to &maxnumdays;

      if (&repeat = &dayk) then day_avail_indicat_k&dayk = 1;



 
      /*** Response variables and covariates for episodically consumed foods ***/

      %if (&totfoods ^= 0) %then %do;

        %if (&totfoods = 1) %then %do;  

          %let retain_conday_var = &retain_conday_var wk&dayk.condayfood1;
          array ar_conday_k&dayk(&totfoods) wk&dayk.condayfood1;

        
          %let retain_24epis_var = &retain_24epis_var wk&dayk.amtfood1;
          array ar_24epis_k&dayk(&totfoods) wk&dayk.amtfood1;

        %end;
        %else %do;

          %let retain_conday_var = &retain_conday_var wk&dayk.condayfood1 - wk&dayk.condayfood&totfoods;
          array ar_conday_k&dayk(&totfoods) wk&dayk.condayfood1 - wk&dayk.condayfood&totfoods;
        
          %let retain_24epis_var = &retain_24epis_var wk&dayk.amtfood1 - wk&dayk.amtfood&totfoods;
          array ar_24epis_k&dayk(&totfoods) wk&dayk.amtfood1 - wk&dayk.amtfood&totfoods;

        %end;


        %do foodh = 1 %to &totfoods;

          %if (&&tot_covars_prob_&foodh = 1) %then %do;  
            %let retain_covars_prob_&foodh._var = &&retain_covars_prob_&foodh._var x_prob_&foodh._k&dayk._covar1;
            array ar_covars_prob_&foodh._k&dayk(&&tot_covars_prob_&foodh) x_prob_&foodh._k&dayk._covar1;
          %end;
          %else %do;
            %let retain_covars_prob_&foodh._var = &&retain_covars_prob_&foodh._var 
                    x_prob_&foodh._k&dayk._covar1 - x_prob_&foodh._k&dayk._covar&&tot_covars_prob_&foodh;
            array ar_covars_prob_&foodh._k&dayk(&&tot_covars_prob_&foodh) 
                    x_prob_&foodh._k&dayk._covar1 - x_prob_&foodh._k&dayk._covar&&tot_covars_prob_&foodh;
          %end;
        
 
          %if (&&tot_covars_epamt_&foodh = 1) %then %do;  
            %let retain_covars_epamt_&foodh._var = &&retain_covars_epamt_&foodh._var x_epamt_&foodh._k&dayk._covar1;
            array ar_covars_epamt_&foodh._k&dayk(&&tot_covars_epamt_&foodh) x_epamt_&foodh._k&dayk._covar1;
          %end;
          %else %do;
            %let retain_covars_epamt_&foodh._var = &&retain_covars_epamt_&foodh._var 
                    x_epamt_&foodh._k&dayk._covar1 - x_epamt_&foodh._k&dayk._covar&&tot_covars_epamt_&foodh;
            array ar_covars_epamt_&foodh._k&dayk(&&tot_covars_epamt_&foodh) 
                    x_epamt_&foodh._k&dayk._covar1 - x_epamt_&foodh._k&dayk._covar&&tot_covars_epamt_&foodh;
          %end;
 
        %end;


        if first.&subject then do;

          do foodjj = 1 to &totfoods;
            ar_conday_k&dayk.(foodjj) = 0;
            ar_24epis_k&dayk.(foodjj) = 0;
          end;

          %do foodkk = 1 %to &totfoods;
      
            do covarprob = 1 to &&tot_covars_prob_&foodkk;
              ar_covars_prob_&foodkk._k&dayk(covarprob) = 0;
            end;
      
            do covarepamt = 1 to &&tot_covars_epamt_&foodkk;
              ar_covars_epamt_&foodkk._k&dayk(covarepamt) = 0;
            end;
      
          %end;

        end;


        if (&repeat = &dayk) then do;

          do foodhh = 1 to &totfoods;
            ar_conday_k&dayk.(foodhh) = ar_conday(foodhh);
            ar_24epis_k&dayk.(foodhh) = ar_24epis(foodhh);
          end;

          %do foodi = 1 %to &totfoods;
      
            do covarprob = 1 to &&tot_covars_prob_&foodi;
              ar_covars_prob_&foodi._k&dayk(covarprob) = ar_covars_prob_&foodi(covarprob);
            end;
      
            do covarepamt = 1 to &&tot_covars_epamt_&foodi;
              ar_covars_epamt_&foodi._k&dayk(covarepamt) = ar_covars_epamt_&foodi(covarepamt);
            end;
      
          %end;

        end;

      %end;



      
      /*** Response variables and covariates for daily consumed nutrients or foods ***/
    
      %if (&totnutrients ^= 0) %then %do;

        %if (&totnutrients = 1) %then %do;  
          %let retain_24daily_var = &retain_24daily_var wk&dayk.amtnutr1;
          array ar_24daily_k&dayk(&totnutrients) wk&dayk.amtnutr1;
        %end;
        %else %do;
          %let retain_24daily_var = &retain_24daily_var wk&dayk.amtnutr1 - wk&dayk.amtnutr&totnutrients;
          array ar_24daily_k&dayk(&totnutrients) wk&dayk.amtnutr1 - wk&dayk.amtnutr&totnutrients;
        %end;

   
        %do nutrjj = 1 %to &totnutrients;

          %if (&&tot_covars_damt_&nutrjj = 1) %then %do;  
            %let retain_covars_damt_&nutrjj._var = &&retain_covars_damt_&nutrjj._var x_damt_&nutrjj._k&dayk._covar1;
            array ar_covars_damt_&nutrjj._k&dayk(&&tot_covars_damt_&nutrjj) x_damt_&nutrjj._k&dayk._covar1;
          %end;
          %else %do;
            %let retain_covars_damt_&nutrjj._var = &&retain_covars_damt_&nutrjj._var 
                    x_damt_&nutrjj._k&dayk._covar1 - x_damt_&nutrjj._k&dayk._covar&&tot_covars_damt_&nutrjj;
            array ar_covars_damt_&nutrjj._k&dayk(&&tot_covars_damt_&nutrjj) 
                    x_damt_&nutrjj._k&dayk._covar1 - x_damt_&nutrjj._k&dayk._covar&&tot_covars_damt_&nutrjj;
          %end;

        %end;


        if first.&subject then do;

          do nutrientjj = 1 to &totnutrients;
            ar_24daily_k&dayk.(nutrientjj) = 0;
          end;

          %do nutrkk = 1 %to &totnutrients;
      
            do covardamt = 1 to &&tot_covars_damt_&nutrkk;
              ar_covars_damt_&nutrkk._k&dayk(covardamt) = 0;
            end;
      
          %end;

        end;


        if (&repeat = &dayk) then do;

          do nutrienthh = 1 to &totnutrients;
            ar_24daily_k&dayk.(nutrienthh) = ar_24daily(nutrienthh);
          end;

          %do nutrj = 1 %to &totnutrients;
      
            do covardamt = 1 to &&tot_covars_damt_&nutrj;
              ar_covars_damt_&nutrj._k&dayk(covardamt) = ar_covars_damt_&nutrj(covardamt);
            end;
      
          %end;

        end;

      %end;  


    %end;  

    
    /*** Retain and output statements for one record per subject data set ***/


    %if (&set_num_nev_consumers_vars = 1) %then %do;

      /*** Allow for never-consumers ***/
 
      retain &retain_covars_prconep1;

    %end;


    %if (&totfoods ^= 0) %then %do;

      retain &retain_conday_var;
      retain &retain_24epis_var;

      %do foodh = 1 %to &totfoods;
        retain &&retain_covars_prob_&foodh._var;
        retain &&retain_covars_epamt_&foodh._var;
      %end;

    %end;

 
    %if (&totnutrients ^= 0) %then %do;

      retain &retain_24daily_var;

      %do nutrjj = 1 %to &totnutrients;
        retain &&retain_covars_damt_&nutrjj._var;
      %end;

    %end;
    
    
    if last.&subject then output mcmc_subj1recdata;      

  run;

  
  /*********************************************************/
  
  proc datasets lib=work nolist;
    delete temp_input_data keep_repeat_max;
  quit;

  /*********************************************************/
  
  
  proc iml;

    
    /*** One or more episodically consumed foods ***/
    
    %if (&totfoods ^= 0) %then %do;


      %if (&set_num_nev_consumers_vars = 1) %then %do;  

        /*** Allow for never-consumers ***/

        start update_conni(n, nxdays_avail_indicat, anyconsumption_indicat_foodh, 
                           gmatrix_1strep_prconeph_alphah, prob_consumer_nx1_epish                     
                           %do dayk = 1 %to &maxnumdays; 
                             , xbeta_u_k&dayk 
                           %end;
                           );

          prob_noconsdays = ((1 - cdf('normal', xbeta_u_k1[, 1])) ## nxdays_avail_indicat[, 1])
             %if (&maxnumdays ^= 1) %then %do;
               %do dayk = 2 %to &maxnumdays;
                 # ((1 - cdf('normal', xbeta_u_k&dayk[, 1])) ## nxdays_avail_indicat[, &dayk])  
               %end;
             %end;
             ;

          prob_neverconsumer_nx1_epish = 1 - prob_consumer_nx1_epish; 
          nevcon_ppi = prob_neverconsumer_nx1_epish / (prob_neverconsumer_nx1_epish + (prob_consumer_nx1_epish # prob_noconsdays)); 

          genww1  = gen_truncated_normals(-gmatrix_1strep_prconeph_alphah, 50);
          genww2  = gen_truncated_normals(gmatrix_1strep_prconeph_alphah, 50);

          rrunifnx1 = j(n, 1, .);
          call randgen(rrunifnx1, 'uniform');


          conninew = gmatrix_1strep_prconeph_alphah 
                     + (anyconsumption_indicat_foodh # genww1) 
                     + ((1-anyconsumption_indicat_foodh) # ( ((rrunifnx1 > nevcon_ppi) # genww1) - ((rrunifnx1 <= nevcon_ppi) # genww2) ));


          return (conninew);

        finish update_conni; 
          
      %end;  






      start update_wmatrix(n, dk, nxdays_avail_indicat_k, wmatrix_k
                           %do foodg = 1 %to &totfoods;
                             , wk_condayfood&foodg
                           %end;
                           , isigmae, umatrix, xbeta_u_k, numgen
                           );
      
        wmatrixnew_k  = wmatrix_k;   
        
        
    
        %do foodh = 1 %to &totfoods;
    
          /*****************************************/
          /*** Update w_{i, food_consump_var, k} ***/
          /*****************************************/
    
          %let varnum = %eval(2 * &foodh - 1);  
          
          c2 = 1 / isigmae[&varnum, &varnum];
          c1 = isigmae[&varnum, &varnum] # xbeta_u_k[, &varnum];
        
        
          %do varc = 1 %to &totvars;  
            %if (&varc ^= &varnum) %then %do;
    
              qq = (wmatrix_k[, &varc] - xbeta_u_k[, &varc]);
              c1 = c1 - (isigmae[&varnum, &varc] # qq);
          
            %end;
          %end;
      
      
          mu      = c2 # c1;
          sigma   = sqrt(c2);
          mu_div_sigma = mu / sigma;

          genww1  = gen_truncated_normals(-mu_div_sigma, numgen);
          genww2  = gen_truncated_normals(mu_div_sigma, numgen);
          
          wmatrixnew_k[, &varnum] = (mu + (sigma # ((wk_condayfood&foodh # genww1) - ((1 - wk_condayfood&foodh) # genww2))))
                                     # nxdays_avail_indicat_k;
          
        
          /*************************************************************************/
          /*** Update w_{i, food_amt_var, k} when w_{i, food_consump_var, k} < 0 ***/
          /*************************************************************************/
          
          %let varnum =  %eval(2 * &foodh);  
          
          c2      = 1 / isigmae[&varnum, &varnum];
          c1      = isigmae[&varnum, &varnum] # xbeta_u_k[, &varnum];
    
          
          %do vard = 1 %to &totvars;  
            %if (&vard ^= &varnum) %then %do;
    
              qq = wmatrixnew_k[, &vard] - xbeta_u_k[, &vard];
              c1 = c1 - (isigmae[&varnum, &vard] # qq);
          
            %end;
          %end;
            
        
          mu       = c2 # c1;
          sigma    = sqrt(c2);
          znormnx1 = j(n, 1, .);
          call randgen(znormnx1, 'normal');
          wmatrixnew_k[, &varnum] = mu + (sigma # znormnx1);
          
          
          wmatrixnew_k[, &varnum] = ((wmatrix_k[, &varnum] # wk_condayfood&foodh) + (wmatrixnew_k[, &varnum] # (1-wk_condayfood&foodh))) 
                                      # nxdays_avail_indicat_k;
          
        %end;
        
        return (wmatrixnew_k);
      
      finish update_wmatrix; 
    
    
    
    
    
    
      start gen_truncated_normals(trunc_value, numgen);
      
        /*****************************************************************/
        /*** This function generates standard normals truncated from   ***/
        /*** the left at trunc_value using a rejection sampler devised ***/
        /*** by C. P. Robert,                                          ***/
        /*** Statistics and Computing, 1995, Volume 5, pp. 121-125.    ***/
        /***                                                           ***/
        /*** The rejection sampler is only used for truncation values  ***/
        /*** > 0, because it is remarkably bad for truncation values   ***/
        /*** < 0, i.e., it tends to stay where it starts in this case. ***/
        /***                                                           ***/
        /*** Input:                                                    ***/
        /***      trunc_value: the random variable is truncated        ***/
        /***                   at the left from trunc_value, a vector. ***/
        /***      numgen:      number of attempts.                     ***/
        /***                                                           ***/
        /*** Output:                                                   ***/
        /***      truncated normals with the same dimension as         ***/
        /***      trunc_value.                                         ***/
        /*****************************************************************/
    
        eps = 0.000000000000001;
        ntrunc = nrow(trunc_value);
        alphatrnorm = (trunc_value + sqrt(4 + (trunc_value ## 2))) / 2;  
        thesign = (trunc_value >= 0); 
        genww = trunc_value # (trunc_value > 0);
        znormntruncx1 = j(ntrunc, 1, .);
        call randgen(znormntruncx1, 'normal');
        temp2 = znormntruncx1;


        do jj = 1 to numgen;
    
          yunifntruncx1 = j(ntrunc, 1, .);
          call randgen(yunifntruncx1, 'uniform');
          /*** Randgen uniform result should always be in (0,1) not [0,1] ***/
          xicand = trunc_value - ( (1 / alphatrnorm) # log(yunifntruncx1));
    
          yunifntruncx1 = j(ntrunc, 1, .);
          call randgen(yunifntruncx1, 'uniform');
          mmmm = (yunifntruncx1 < exp(-.5 # ( (xicand - alphatrnorm) ## 2)));
    
          temp1 = (xicand  # (mmmm = 1)) + (genww # (mmmm = 0));
          znormntruncx1 = j(ntrunc, 1, .);
          call randgen(znormntruncx1, 'normal');
          ssss = znormntruncx1;
          temp2 = (temp2 # (ssss < trunc_value)) + (ssss # (ssss >= trunc_value));
          genww = (temp2 # (thesign = 0)) + (temp1 # (thesign = 1));
    
        end;
    
        genww  = (genww # (genww > trunc_value)) + ((trunc_value + eps) # (genww <= trunc_value));
    
    
        return (genww);
      
      finish gen_truncated_normals;
    
    
    
    
    
    
      /*** Two or more episodically consumed foods ***/
    
      %if (&totfoods ^= 1) %then %do;

        start update_r(sum_weightnumrecalls, i, r, rpossible, rspacing, theta, v, w_crossresid_ksum);
        
          /*** Metropolis-Hastings step for r[i, 1] ***/
        
          rmin  = min(rpossible, r[i, 1]);
          rmax  = max(rpossible, r[i, 1]);
          rcurr = r;
          rcand = r;
          ricurr = r[i, 1];
        
          ssunif = j(1, 1, .);
          call randgen(ssunif, 'uniform');
        
          if ricurr <= rmin then do;
              ricand = (ricurr # (ssunif <= 0.33)) + ((ricurr + rspacing) # (ssunif > 0.33) # (ssunif <= 0.66)) 
                       + ((ricurr + (2 # rspacing)) # (ssunif > 0.66));
          end;
          else if ricurr >= rmax then do;
              ricand = (ricurr # (ssunif <= 0.33)) + ((ricurr - rspacing) # (ssunif > 0.33) # (ssunif <= 0.66)) 
                       + ((ricurr - (2 # rspacing)) # (ssunif > 0.66));
          end;
          else do;
            ricand = (ricurr # (ssunif <= 0.33)) + ((ricurr + rspacing) # (ssunif > 0.33) # (ssunif <= 0.66)) 
                     + ((ricurr - rspacing) # (ssunif > 0.66));
          end;
          rcand[i, 1] = ricand;
          
          if ((ricand >= -1) & (ricand <= 1)) then do;
          
            gofsigmaecurr = formgofsigmaev(rcurr, theta, v, w_crossresid_ksum);
            gofsigmaecand = formgofsigmaev(rcand, theta, v, w_crossresid_ksum);

            gg = gofsigmaecand - gofsigmaecurr;

            /*** Ensure valid argument for log function ***/
            
            if (ricand = -1) then ricand     = -0.999999999999;       
            else if (ricand = 1) then ricand =  0.999999999999;   
 
            if (ricurr = -1) then ricurr     = -0.999999999999;       
            else if (ricurr = 1) then ricurr =  0.999999999999;   

            /**********************************************/

            gg = gg - (0.5 # sum_weightnumrecalls # log(1 - (ricand##2))) + (0.5 # sum_weightnumrecalls  # log(1 - (ricurr##2)));

            if (gg ^= .) then do;
              if (gg < 0) then tempgg = exp(gg);
              else tempgg = 1;
            end;
            else tempgg = .;
            
            gg = tempgg;
            
            ss2unif = j(1, 1, .);
            call randgen(ss2unif, 'uniform');
            if (gg ^= .) then rinew = (ricand # (ss2unif <= gg)) + (ricurr # (ss2unif > gg));
            else rinew = .;

          end;
          else rinew = ricurr;

          gg = .;
          
          
          return (rinew);
        
        finish update_r;
        
        
        
        
        
        
        start update_theta(i, r, theta, thetapossible, thetaspacing, v, w_crossresid_ksum);
        
          /*** Metropolis-Hastings step for theta[i, 1] ***/
        
          thetamin      = min(thetapossible, theta[i, 1]);
          thetamax      = max(thetapossible, theta[i, 1]);
          thetacurr     = theta;
          thetacand     = theta;
          thetaicurr    = theta[i, 1];
        
          ssunif = j(1, 1, .);
          call randgen(ssunif, 'uniform');
        
          if thetaicurr <= thetamin then do;
            thetaicand = (thetaicurr # (ssunif <= 0.33)) + ((thetaicurr + thetaspacing) # (ssunif > 0.33) # (ssunif <= 0.66)) 
                         + ((thetaicurr + (2 # thetaspacing)) # (ssunif > 0.66));
          end;
          else if thetaicurr >= thetamax then do;
            thetaicand = (thetaicurr # (ssunif <= 0.33)) + ((thetaicurr - thetaspacing) # (ssunif > 0.33) # (ssunif <= 0.66)) 
                         + ((thetaicurr - (2 # thetaspacing)) # (ssunif > 0.66));
          end;
          else do;
            thetaicand = (thetaicurr # (ssunif <= 0.33)) + ((thetaicurr + thetaspacing) # (ssunif > 0.33) # (ssunif <= 0.66)) 
                         + ((thetaicurr - thetaspacing) # (ssunif > 0.66));
          end;
          thetacand[i, 1] = thetaicand;
          
          gofsigmaecurr = formgofsigmaev(r, thetacurr, v, w_crossresid_ksum);
          gofsigmaecand = formgofsigmaev(r, thetacand, v, w_crossresid_ksum);
          
          tempdiff = gofsigmaecand - gofsigmaecurr;
          if (tempdiff ^= .) then do;
            if (tempdiff < 0) then gg = exp(tempdiff);
            else gg = 1;
          end;
          else gg = .;

          
          ss2unif = j(1, 1, .);
          call randgen(ss2unif, 'uniform');
          if (gg ^= .) then thetainew = (thetaicand # (ss2unif <= gg)) + (thetaicurr # (ss2unif > gg));
          else thetainew = .;

          gg = .;
        
        
          return (thetainew);
        
        finish update_theta;

        
      %end;  /*** End two or more episodically consumed foods ***/ 
    %end;  /*** End one or more episodically consumed foods ***/
 






    start update_vii(sum_weightnumrecalls, i, 
                     %if ((&totfoods ^= 0) & (&totfoods ^= 1)) %then %do;
                       r, theta, 
                     %end;
                     v, w_crossresid_ksum
                     ) global(jjmcmc);

      /*** Metropolis-Hastings step for v[i, i] ***/
    
      vcurr         = v;
      vcand         = v;
      viicurr       = v[i, i];

      rriiunif      = j(1, 1, .);
      call randgen(rriiunif, 'uniform');
      viicand       = v[i, i] + (0.4 # (rriiunif - 0.5));
      vcand[i, i]   = viicand;

      if ((viicand >= -3) & (viicand <= 3)) then do;

        gofsigmaecurr = formgofsigmaev(
                                       %if ((&totfoods ^= 0) & (&totfoods ^= 1)) %then %do;
                                         r, theta, 
                                       %end;
                                       vcurr, w_crossresid_ksum
                                       );
        gofsigmaecand = formgofsigmaev(
                                       %if ((&totfoods ^= 0) & (&totfoods ^= 1)) %then %do;
                                         r, theta, 
                                       %end; 
                                       vcand, w_crossresid_ksum
                                       );

        gg = gofsigmaecand - gofsigmaecurr;

        /*** Ensure valid argument for log function ***/
        
        if (viicand = 0) then viicand = 0.000001;  /*** value gets squared ***/

        if (viicurr = 0) then viicurr = 0.000001;  /*** value gets squared ***/

        /**********************************************/
        
        gg = gg - (0.5 # sum_weightnumrecalls # log(viicand##2)) + (0.5 # sum_weightnumrecalls # log(viicurr##2));

        if (gg ^= .) then do;
          if (gg < 0) then tempgg = exp(gg);
          else tempgg = 1;
        end;
        else tempgg = .;
        
        gg = tempgg;
        
        vviiunif = j(1, 1, .);
        call randgen(vviiunif, 'uniform');
        if (gg ^= .) then viinew = (viicand # (vviiunif <= gg)) + (viicurr # (vviiunif > gg));
        else viinew = .;

      end;
      else viinew = viicurr;

      gg = .;

  
      return (viinew);
    
    finish update_vii;


  



    start update_vij(i, j, 
                     %if ((&totfoods ^= 0) & (&totfoods ^= 1)) %then %do;
                       r, theta, 
                     %end;
                     v, w_crossresid_ksum
                     ) global(jjmcmc);


      /*** Metropolis-Hastings step for v[i, j] ***/
      
      vcurr         = v;
      vcand         = v;
      vijcurr       = v[i, j];

      rrijunif      = j(1, 1, .);
      call randgen(rrijunif, 'uniform');
      vijcand       = v[i, j] + (0.4 # (rrijunif - 0.5));
      vcand[i, j]   = vijcand;

      if ((vijcand >= -3) & (vijcand <= 3)) then do;

        gofsigmaecurr = formgofsigmaev(
                                       %if ((&totfoods ^= 0) & (&totfoods ^= 1)) %then %do;
                                         r, theta, 
                                       %end;
                                       vcurr, w_crossresid_ksum
                                       );
        gofsigmaecand = formgofsigmaev(
                                       %if ((&totfoods ^= 0) & (&totfoods ^= 1)) %then %do;
                                         r, theta, 
                                       %end; 
                                       vcand, w_crossresid_ksum
                                       );
  
        gg = gofsigmaecand - gofsigmaecurr;

        if (gg ^= .) then do;
          if (gg < 0) then tempgg = exp(gg);
          else tempgg = 1;
        end;
        else tempgg = .;
        
        gg = tempgg;
        
        vvijunif = j(1, 1, .);
        call randgen(vvijunif, 'uniform');
        if (gg ^= .) then vijnew = (vijcand # (vvijunif <= gg)) + (vijcurr # (vvijunif > gg));
        else vijnew = .;

      end;
      else vijnew = vijcurr;

      gg = .;


      return (vijnew);
    
    finish update_vij;






    start formgofsigmaev(
                         %if ((&totfoods ^= 0) & (&totfoods ^= 1)) %then %do;
                           r, theta, 
                         %end;
                         v, w_crossresid_ksum
                         );

      /*** One or more episodically consumed foods ***/

      %if (&totfoods ^= 0) %then %do;

        v[1, 1] = 1;
        v[2, 1] = 0;


        /*** Two or more episodically consumed foods ***/
      
        %if (&totfoods ^= 1) %then %do;
      
          v[3, 1] = r[1, 1] # sin(theta[1, 1]);
          v[3, 2] = r[1, 1] # cos(theta[1, 1]);

      
          /*** Three or more episodically consumed foods ***/
          
          %if (&totfoods ^= 2) %then %do;
      
            do k = 2 to &totfoodsminus1;
         
              twokplus1 = 2#k + 1;
              kminus1sq = (k - 1)##2;
              twokminus1 = 2#k - 1;
            
              v[twokplus1, 1] = r[k, 1] # sin(theta[kminus1sq + 1, 1]);
             
              do j = 2 to twokminus1;
            
                v[twokplus1, j] = r[k, 1] # sin(theta[kminus1sq + j, 1]) ;
                
                do jj = 1 to (j-1);
                  v[twokplus1, j] = v[twokplus1, j] # cos(theta[kminus1sq + jj, 1]);
                end;
            
              end;
             
              v[twokplus1, 2#k] = r[k, 1];
            
              do jj = 1 to twokminus1;
                v[twokplus1, 2#k] = v[twokplus1, 2#k] # cos(theta[kminus1sq + jj, 1]);
              end;
            
            end;
      
          %end;  /*** End three or more episodically consumed foods ***/
          
          do k = 3 to (&totfoodvars - 1) by 2;
            v[k, k]  = sqrt(1 - (r[(k - 1)/2, 1])##2);
            v[k+1, k] = -(sum( v[k, 1:(k-1)] # v[k+1, 1:(k-1)] )) /  v[k, k];
          end;
      
        %end;  /*** End two or more episodically consumed foods ***/

      %end;  /*** End one or more episodically consumed foods ***/


      
      sigmae = v * t(v);
      isigmae = &gen_inverse_label.inv(sigmae);
      
      isge_w_crossresid_ksum = isigmae # w_crossresid_ksum;
      gofsigmaev = -0.5 # (sum(isge_w_crossresid_ksum));
  
  
      return (gofsigmaev);
    
    finish formgofsigmaev;






    start update_beta(jj, pjj, sum_day_subj_wt_pxp_xmatrixjj   
                      %do dayk = 1 %to &maxnumdays;
                        , t_wt_avail_xmatrixjj_k&dayk, wmatrix_k&dayk, xbeta_k&dayk  
                      %end;
                      %if (&set_num_nev_consumers_vars = 1) %then %do;  
                        /*** Allow for never-consumers ***/
                        %do dayk = 1 %to &maxnumdays; 
                          , xmatrixjj_k&dayk 
                        %end;
                        , beta1_curr, nxdays_avail_indicat, conni1_notpos_isnevcon
                      %end;
                      , isigmae, umatrix, iprior_betajj_cov, iprior_cov_prior_mean_betajj 
                      );
  
      
      c1 = iprior_cov_prior_mean_betajj + (isigmae[jj, jj] # 
                  ( t_wt_avail_xmatrixjj_k1 * (wmatrix_k1[, jj] - umatrix[, jj])
                  
                    %if (&maxnumdays ^= 1) %then %do;
                      %do dayk = 2 %to &maxnumdays;
                        + t_wt_avail_xmatrixjj_k&dayk * (wmatrix_k&dayk[, jj] - umatrix[, jj])
                      %end;
                    %end;
                    ));


      %do varh = 1 %to &totvars;
        if jj ^= &varh then do;      
          c1 = c1 + (isigmae[jj, &varh] # 
                      ( t_wt_avail_xmatrixjj_k1
                        * (wmatrix_k1[, &varh] - xbeta_k1[, &varh] - umatrix[, &varh])
                           
                        %if (&maxnumdays ^= 1) %then %do;
                          %do dayk = 2 %to &maxnumdays;
                            + t_wt_avail_xmatrixjj_k&dayk
                               * (wmatrix_k&dayk[, &varh] - xbeta_k&dayk[, &varh] - umatrix[, &varh])
                          %end;
                        %end;
                        ));
        end;
      %end;

      
      inverse_c2 = iprior_betajj_cov + (isigmae[jj, jj] # sum_day_subj_wt_pxp_xmatrixjj);     
      c2 = &gen_inverse_label.inv(inverse_c2);


      /*************************************************************************************/
      /*************************************************************************************/
      /*************************************************************************************/
      %if (&set_num_nev_consumers_vars = 1) %then %do;  
      
        /*** Allow for never-consumers ***/
  
        if (jj = 1) then do;

          betajj_curr = beta1_curr;
          connijj_notpos_isnevcon = conni1_notpos_isnevcon;
       
          znormpjjx1 = j(pjj, 1, .);
          call randgen(znormpjjx1, 'normal');

          sigma_metrop = 2 # c2;          /*** Considering m # c2, we selected m=2 ***/
          call eigen(eigvals_sigma_metrop, eigvecs_sigma_metrop, sigma_metrop);
          eigvalshalf = eigvals_sigma_metrop ## 0.5;
          sqrtm_sigma_metrop = eigvecs_sigma_metrop * diag(eigvalshalf) * t(eigvecs_sigma_metrop);
         
          betajj_cand = betajj_curr + (sqrtm_sigma_metrop * znormpjjx1);
         
          logfac1_cand = t(c1) * betajj_cand - 0.5 # t(betajj_cand) * inverse_c2 * betajj_cand;
          logfac1_curr = t(c1) * betajj_curr - 0.5 # t(betajj_curr) * inverse_c2 * betajj_curr;
         
          logfac2_cand = (-1) # t(nxdays_avail_indicat[, 1]) 
                              * (connijj_notpos_isnevcon # (log(0.000000000001 <> (1 - cdf('normal', (xmatrixjj_k1 * betajj_cand + umatrix[, jj]))))))
                        %if (&maxnumdays ^= 1) %then %do;
                          %do dayk = 2 %to &maxnumdays;
                            - (t(nxdays_avail_indicat[, &dayk]) 
                                * (connijj_notpos_isnevcon # (log(0.000000000001 <> (1 - cdf('normal', (xmatrixjj_k&dayk * betajj_cand + umatrix[, jj])))))) 
                               )
                          %end;
                        %end;
                        ;
                        
          logfac2_curr = (-1) # t(nxdays_avail_indicat[, 1]) 
                              * (connijj_notpos_isnevcon # (log(0.000000000001 <> (1 - cdf('normal', (xbeta_k1[, jj] + umatrix[, jj]))))))
                        %if (&maxnumdays ^= 1) %then %do;
                          %do dayk = 2 %to &maxnumdays;
                            - (t(nxdays_avail_indicat[, &dayk]) 
                                * (connijj_notpos_isnevcon # (log(0.000000000001 <> (1 - cdf('normal', (xbeta_k&dayk[, jj] + umatrix[, jj])))))) 
                               )
                          %end;
                        %end;
                        ;
        

          tempdiff = logfac1_cand + logfac2_cand - logfac1_curr - logfac2_curr;
          if (tempdiff ^= .) then do;
            if (tempdiff < 0) then gghh = exp(tempdiff);
            else gghh = 1;
          end;
          else gghh = .;

     
          ss3unif = j(1, 1, .);
          call randgen(ss3unif, 'uniform');
   
          if (gghh ^= .) then betanewjj = (betajj_cand # (ss3unif <= gghh)) + (betajj_curr # (ss3unif > gghh));
          else betanewjj = .;
          
          gghh = .;
    
        end;
        else  /*** When allowing never-consumers, this statement is completed by the following do group ***/ 
      %end;    
        do;   /*** When not allowing never-consumers, this do group is executed unconditionally ***/     

          znormpjjx1 = j(pjj, 1, .);
          call randgen(znormpjjx1, 'normal');
          call eigen(eigvalsc2, eigvecsc2, c2);
          eigvalshalf = eigvalsc2 ## 0.5;
          sqrtm_c2 = eigvecsc2 * diag(eigvalshalf) * t(eigvecsc2);
         
          betanewjj = (c2 * c1) + (sqrtm_c2 * znormpjjx1);
         
        end;    
      /*************************************************************************************/
      /*************************************************************************************/
      /*************************************************************************************/

  
      return (betanewjj);
    
    finish update_beta;






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





  
    /*****************************************************************/
    /*** Read data and prepare variables for the mcmc calculations ***/
    /*****************************************************************/
    
   
    use mcmc_subj1recdata  var _all_  nobs n;

    read all;




    /*** Read optional data set used to define the mean of the prior distribution for the sigmau covariance matrix ***/

    %if (&prior_sigmau_mean_data ^= %str()) %then %do;

      use &prior_sigmau_mean_data  var _all_;
      
      %if %upcase(&print)^=N %then %do;
        print "Output from the Show Datasets Statement for prior_sigmau_mean_data = &prior_sigmau_mean_data",,;
        show datasets;
      %end;
      
      %if %upcase(&print)^=N %then %do;
        print "Output from the Show Contents Statement for prior_sigmau_mean_data = &prior_sigmau_mean_data",,;
        show contents;
      %end;
      
      read all var _all_ into prior_sigmau_mean;

    %end;  




    /***********************************************************************************/
      



    %if (&set_seed_mcmc ^= %str()) %then %do;
      call randseed(&set_seed_mcmc);
    %end;
    
    
    nmcmc = &set_number_mcmc_iterations;
    nburn = &set_number_burn_iterations;
    nburn_plus1 = nburn + 1;
    npostmcmc = &set_number_post_mcmc_u_out;
    nmcmcpluspost = nmcmc + npostmcmc;


    /*** Create nxdays matrix of indicators of recall availability. ***/
    /***                                                            ***/
    /*** For calculations allowing for never-consumers, create an   ***/
    /*** indicator for each individual with consumption for food 1. ***/
    
    nxdays_avail_indicat = j(n, &maxnumdays, 0);

    %if (&set_num_nev_consumers_vars = 1) %then %do;

      /*** Allow for never-consumers ***/
 
      noconsumption_indicat_food1 = j(n, 1, 1);

    %end;

    %do dayk = 1 %to &maxnumdays; 

      nxdays_avail_indicat[, &dayk] = day_avail_indicat_k&dayk;

      %if (&set_num_nev_consumers_vars = 1) %then %do;

        /*** Allow for never-consumers ***/
 
        noconsumption_indicat_food1 = noconsumption_indicat_food1 # (1 - wk&dayk.condayfood1);

      %end;

    %end;

    %if (&set_num_nev_consumers_vars = 1) %then %do;

      /*** Allow for never-consumers ***/
 
      anyconsumption_indicat_food1 = 1 - noconsumption_indicat_food1;

    %end;

  
    /*** Create number of recalls variable and row index used in calculations ***/

    numrecalls = nxdays_avail_indicat[, +];

    %do totrecalls = 1 %to &maxnumdays;

      rows_numrecalls_&totrecalls = loc(numrecalls = &totrecalls);
        
    %end;


    /*** Create weight variable and related sum used in calculations ***/
    
    %if ((&weight_var=%str()) | (&weight_var=%str( ))) %then %do;
      weight_nw_sumw = j(n, 1, 1);
    %end;
    %else %do;
      weight_nw_sumw = n # &weight_var / sum(&weight_var);
    %end;


    sum_weightnumrecalls = sum( weight_nw_sumw # numrecalls);


    /*** Create (n x max number of days) matrix with elements set to zero if day is missing or set to weight value otherwise ***/
    
    wt_nxdays_avail_indicat = weight_nw_sumw # nxdays_avail_indicat;



  
    saveiter = &set_thin;
    denomthinct = 0;
  
    printcounter1 = 0;

 


    /*************************************/
    /*** Define the covariate matrices ***/
    /*************************************/


    %if (&set_num_nev_consumers_vars = 1) %then %do;

      /*** Allow for never-consumers ***/
 
      /*** Covariates used to model the probability of being  ***/
      /*** a consumer of the first episodically consumed food ***/

      gmatrix_1strep_prconep1 = j(n, &tot_covars_prconep1, .);

      %do covarh_g_ep1 = 1 %to &tot_covars_prconep1;
        gmatrix_1strep_prconep1[, &covarh_g_ep1] = g_1strep_prconep1_covar&covarh_g_ep1;
      %end;

      ncolgmatrix_1strep_prconep1 = ncol(gmatrix_1strep_prconep1);

    %end;



    
    %do dayk = 1 %to &maxnumdays;

      /*** Probability and amount covariates for episodically consumed foods ***/

      %if (&totfoods ^= 0) %then %do;
        %do foodh = 1 %to &totfoods;
          
          %let foodvarb = %eval(2 * &foodh);
          %let foodvara = %eval(&foodvarb - 1);

        
          xmatrix&foodvara._k&dayk = j(n, &&tot_covars_prob_&foodh, .);

          %do covara = 1 %to &&tot_covars_prob_&foodh;
            xmatrix&foodvara._k&dayk[, &covara] = x_prob_&foodh._k&dayk._covar&covara;
          %end;


          xmatrix&foodvarb._k&dayk = j(n, &&tot_covars_epamt_&foodh, .);
          
          %do covarb = 1 %to &&tot_covars_epamt_&foodh;
            xmatrix&foodvarb._k&dayk[, &covarb] = x_epamt_&foodh._k&dayk._covar&covarb;
          %end;


        %end;
      %end;

        
      /*** Amount covariates for daily consumed nutrients or foods ***/
      
      %if (&totfoodvars ^= &totvars) %then %do;
        %do nutrvar = &totfoodvarsplus1 %to &totvars;

          %let nutrj = %eval(&nutrvar - &totfoodvars);

        
          xmatrix&nutrvar._k&dayk = j(n, &&tot_covars_damt_&nutrj, .);
          
          %do covarnutr = 1 %to &&tot_covars_damt_&nutrj;
            xmatrix&nutrvar._k&dayk[, &covarnutr] = x_damt_&nutrj._k&dayk._covar&covarnutr;
          %end;

        
        %end;
      %end;  

         
    %end;

  
    %do varj = 1 %to &totvars;
      ncolxmatrix&varj = ncol(xmatrix&varj._k1);
    %end;




    %if (&set_num_nev_consumers_vars = 1) %then %do; 

      /*** Allow for never-consumers ***/
 
      /*****************************************************************************/
      /*** Prior and starting values for consumer probability parameters, alpha. ***/
      /***                                                                       ***/
      /*** Preliminary calculations for alpha and conni updates.                 ***/
      /*****************************************************************************/
     
      prior_alpha1_mean = j(ncolgmatrix_1strep_prconep1, 1, 0);
      alpha1 = prior_alpha1_mean;
      prior_alpha1_cov = i(ncolgmatrix_1strep_prconep1);
      iprior_alpha1_cov = &gen_inverse_label.inv(prior_alpha1_cov);
   
      iprior_cov_prior_mean_alpha1 = iprior_alpha1_cov * prior_alpha1_mean;

      t_weight_gmatrix_1strep_prconep1 = t(weight_nw_sumw # gmatrix_1strep_prconep1);

      c2_alpha1 = &gen_inverse_label.inv(iprior_alpha1_cov + (t_weight_gmatrix_1strep_prconep1 * gmatrix_1strep_prconep1));

      call eigen(eigvals_c2_alpha, eigvecs_c2_alpha, c2_alpha1);
      eigvalshalf = eigvals_c2_alpha ## 0.5;
      sqrtm_c2_alpha1 = eigvecs_c2_alpha * diag(eigvalshalf) * t(eigvecs_c2_alpha);
  
      gmatrix_1strep_prconep1_alpha1 = gmatrix_1strep_prconep1 * alpha1;
      prob_consumer_nx1_epis1 = cdf('normal', gmatrix_1strep_prconep1_alpha1);

    %end;   
     



    /*************************************************/
    /*** Prior and starting values for beta.       ***/
    /***                                           ***/
    /*** Preliminary calculations for beta update. ***/
    /*************************************************/
   
    %do varj = 1 %to &totvars;
 
      prior_beta&varj._mean = j(ncolxmatrix&varj, 1, 0);
      beta&varj = prior_beta&varj._mean;
      prior_beta&varj._cov = 100 # i(ncolxmatrix&varj);
      iprior_beta&varj._cov = &gen_inverse_label.inv(prior_beta&varj._cov);

      iprior_cov_prior_mean_beta&varj = iprior_beta&varj._cov * prior_beta&varj._mean;

      sum_day_subj_wt_pxp_xmatrix&varj = j(ncolxmatrix&varj, ncolxmatrix&varj, 0);

      %do dayk = 1 %to &maxnumdays; 

        t_wt_avail_xmatrix&varj._k&dayk = t(wt_nxdays_avail_indicat[, &dayk] # xmatrix&varj._k&dayk);

        sum_day_subj_wt_pxp_xmatrix&varj = sum_day_subj_wt_pxp_xmatrix&varj 
                                           + t_wt_avail_xmatrix&varj._k&dayk * xmatrix&varj._k&dayk;

      %end;

    %end;   
    



    /**************************************************************/
    /*** Prior, starting values, and other variables for sigmae ***/
    /**************************************************************/

    %if ((&totfoods ^= 0) & (&totfoods ^= 1)) %then %do;

      initspacing   = 2 # 0.99 / 40;
    
      rspacing  = initspacing;
      rpossible = do(-0.99, 0.99, rspacing);
    
      pi = arcos(-1);
      thetapossible = pi # (do(-0.99, 0.99, initspacing));
      thetaspacing  = thetapossible[2] - thetapossible[1];
    
      
      r = j(&totfoodsminus1, 1, 0);
    
      lengththeta = (&totfoodsminus1)##2;
      theta = j(lengththeta, 1, 0);
    
    %end;

    
    v = i(&totvars);
    
    sigmae = v * t(v);
    isigmae = &gen_inverse_label.inv(sigmae);
      



    /********************************************/
    /*** Prior and starting values for sigmau ***/
    /********************************************/
   
    %if (&prior_sigmau_mean_data = %str()) %then %do;
      prior_sigmau_mean = j(&totvars, &totvars, 0.5) - diag(j(&totvars, &totvars, 0.5)) + i(&totvars);
    %end;

    prior_sigmau_doff = 1 + 1 + nrow(prior_sigmau_mean);
    sigmau = prior_sigmau_mean;
    isigmau = &gen_inverse_label.inv(sigmau);

   

  
    /**************************************************************************************/
    /*** Starting values for the umatrix and prepare variable names for output data set ***/
    /**************************************************************************************/
    
    dimu = ncol(sigmau);
    znormnxdimu = j(n, dimu, .);
    call randgen(znormnxdimu, 'normal');
    call eigen(eigvalssigmau, eigvecssigmau, sigmau);
    eigvalshalf = eigvalssigmau ## 0.5;
    sqrtm_sigmau = eigvecssigmau * diag(eigvalshalf) * t(eigvecssigmau);
    umatrix = znormnxdimu * sqrtm_sigmau;
  
       
    %if %upcase(&save_mcmc_u_out_data)=Y %then %do;

      trace_afterburn_thin_u_colnames = {"Iteration"} || {"&subject"} 
        || { 
            %do varj = 1 %to &totvars;
              "u_col&varj" 
            %end;  
            };

    %end;

    %if (&set_number_post_mcmc_u_out ^= 0) %then %do;

      post_mcmc_u_colnames = {"Iteration"} || {"&subject"} 
        || { 
            %do varj = 1 %to &totvars;
              "u_col&varj" 
            %end;  
            };

    %end;




    /***************************************************************************************/
    /*** Starting values for w_{ijk} and initialization of matrices used throughout mcmc ***/
    /***************************************************************************************/
  
    %do dayk = 1 %to &maxnumdays;


      wmatrixs_k&dayk = j(n, &totvars, 0);

        
      /*** Initialize matrices used throughout mcmc.  Matrices are updated in other code after each u and beta update. ***/

      xbeta_k&dayk = j(n, &totvars, .);
      xbeta_u_k&dayk = j(n, &totvars, .);
      
      %do varj = 1 %to &totvars;

        xbeta_k&dayk[, &varj] = (xmatrix&varj._k&dayk * beta&varj);
      
        xbeta_u_k&dayk[, &varj] = xbeta_k&dayk[, &varj] + umatrix[, &varj];

      %end;
     
      /****************************************************************************************/


      %if (&totfoods ^= 0) %then %do;

        %do foodh = 1 %to &totfoods;
          
          %let foodvarb = %eval(2 * &foodh);
          %let foodvara = %eval(&foodvarb - 1);
        
          /*** Positive values for consumption-day and negative values for nonconsumption-day ***/
        
          znormnx1 = j(n, 1, .);
          call randgen(znormnx1, 'normal');

          dk = &dayk;

          nxdays_avail_indicat_k = nxdays_avail_indicat[, &dayk];

          wmatrixs_k&dayk[, &foodvara]  = ((2 # wk&dayk.condayfood&foodh - 1) 
                                            # (abs(xbeta_u_k&dayk[, &foodvara] + znormnx1))) # nxdays_avail_indicat_k;
     
          /*** Amounts for episodically consumed foods ***/
          
          wmatrixs_k&dayk[, &foodvarb]  = wk&dayk.amtfood&foodh;
      
        %end;
      %end;

        
      /*** Amounts for nutrients or foods consumed daily ***/
      
      %if (&totfoodvars ^= &totvars) %then %do;
        %do nutrvar = &totfoodvarsplus1 %to &totvars;

          %let nutrj = %eval(&nutrvar - &totfoodvars);
        
          wmatrixs_k&dayk[, &nutrvar]  = wk&dayk.amtnutr&nutrj;
        
        %end;
      %end;  
      
      /************************************************************/

    
      %if (&totfoods ^= 0) %then %do;

        numgen = 20;
      
        wmatrixnew_k&dayk = update_wmatrix(n, dk, nxdays_avail_indicat_k, wmatrixs_k&dayk
                                           %do foodg = 1 %to &totfoods;
                                             , wk&dayk.condayfood&foodg
                                           %end;
                                           , isigmae, umatrix, xbeta_u_k&dayk , numgen
                                           );
        
        wmatrix_k&dayk = wmatrixnew_k&dayk;

      %end;
      %else %do;
      
        wmatrix_k&dayk = wmatrixs_k&dayk;
     
      %end;

      
    %end;




    /*********************************************************/
    /*** Initialize sums, sums of squares, and mcmc traces ***/
    /*********************************************************/


    %if (&set_num_nev_consumers_vars = 1) %then %do;  
  
      /*** Allow for never-consumers ***/

      alpha1_sum = j(ncolgmatrix_1strep_prconep1, 1, 0);
      alpha1_sumsq = j(ncolgmatrix_1strep_prconep1, 1, 0);

      prob_consumer_epis1_sum = j(1, 1, 0);
      prob_consumer_epis1_sumsq = j(1, 1, 0);
   
    %end;


    %do varj = 1 %to &totvars;
      beta&varj._sum = j(ncolxmatrix&varj, 1, 0);
      beta&varj._sumsq = j(ncolxmatrix&varj, 1, 0);
    %end;
  
    sigmau_sum   = j(&totvars, &totvars, 0);
    sigmau_sumsq = j(&totvars, &totvars, 0);
   
    sigmae_sum   = j(&totvars, &totvars, 0);
    sigmae_sumsq = j(&totvars, &totvars, 0);


    index_afterburn_thin = do(nburn_plus1, &set_number_mcmc_iterations, &set_thin);
    length_index_afterburn_thin = ncol(index_afterburn_thin);


    %if (&set_num_nev_consumers_vars = 1) %then %do;  
  
      /*** Allow for never-consumers ***/
  
      alpha1_trace = j(length_index_afterburn_thin, ncolgmatrix_1strep_prconep1, .);
      prob_consumer_epis1_trace = j(length_index_afterburn_thin, 1, .);
   
    %end;


    %do varj = 1 %to &totvars;
     
      beta&varj._trace = j(length_index_afterburn_thin, ncolxmatrix&varj, .);
      
      %do vark = 1 %to &varj;

        sigmau_row&varj._col&vark._trace = j(length_index_afterburn_thin, 1, .);

        sigmae_row&varj._col&vark._trace = j(length_index_afterburn_thin, 1, .);

      %end;
    %end;




    /************************************************************************************/
    /*** Start the mcmc *****************************************************************/
    /************************************************************************************/
  
    
    %if %upcase(&print)^=N %then %do;
      print "MCMC Output";
    %end;
    do jjmcmc = 1 to nmcmcpluspost;

    
      %if (&totfoods ^= 0) %then %do;


        %if (&set_num_nev_consumers_vars = 1) %then %do;  
  
          /*** Allow for never-consumers ***/
  
  
          /********************/
          /*** Update conni ***/
          /********************/
  
          conninew = update_conni(n, nxdays_avail_indicat, anyconsumption_indicat_food1, 
                                  gmatrix_1strep_prconep1_alpha1, prob_consumer_nx1_epis1                     
                                  %do dayk = 1 %to &maxnumdays; 
                                    , xbeta_u_k&dayk 
                                  %end;
                                  );
          
          conni1 = conninew;        
          conni1_notpos_isnevcon = (conni1 <= 0); 
  
   
          /******************************************************/
          /*** Update consumer probability parameters, alpha, ***/
          /*** and consumer probabilities                     ***/
          /******************************************************/
          
          c1_alpha1 = iprior_cov_prior_mean_alpha1 + (t_weight_gmatrix_1strep_prconep1 * conni1);
          
          znormgcolx1 = j(ncolgmatrix_1strep_prconep1, 1, .);
          call randgen(znormgcolx1, 'normal');
          alpha1 = (c2_alpha1 * c1_alpha1) + (sqrtm_c2_alpha1 * znormgcolx1);
     
          gmatrix_1strep_prconep1_alpha1 = gmatrix_1strep_prconep1 * alpha1;
          prob_consumer_nx1_epis1 = cdf('normal', gmatrix_1strep_prconep1_alpha1);
     
     
        %end;




        /**********************/
        /*** Update w_{ijk} ***/
        /**********************/

        numgen = 5;

        %do dayk = 1 %to &maxnumdays;
    
          dk = &dayk;

          nxdays_avail_indicat_k = nxdays_avail_indicat[, &dayk];


          wmatrixnew_k&dayk = update_wmatrix(n, dk, nxdays_avail_indicat_k, wmatrix_k&dayk
                                             %do foodg = 1 %to &totfoods;
                                               , wk&dayk.condayfood&foodg
                                             %end;
                                             , isigmae, umatrix, xbeta_u_k&dayk , numgen
                                             );
    
          wmatrix_k&dayk = wmatrixnew_k&dayk;

    
        %end;
      %end;

      


      if (jjmcmc <= nmcmc) then do;

        /************************/
        /*** Calculate W-XB-U ***/
        /************************/
       
        w_crossresid_ksum = j(&totvars, &totvars, 0);
       
        %do dayk = 1 %to &maxnumdays; 
       
          qq&dayk = (wmatrix_k&dayk - xbeta_u_k&dayk);
          qqw&dayk = wt_nxdays_avail_indicat[, &dayk] # qq&dayk;
       
          w_crossresid_ksum = w_crossresid_ksum + t(qqw&dayk) * qq&dayk;
       
        %end;
       

    

        /**********************/
        /*** Update isigmae ***/
        /**********************/
  
        /*** One or more episodically consumed foods ***/
  
        %if (&totfoods ^= 0) %then %do;
  
          /*** Two or more episodically consumed foods ***/
  
          %if (&totfoods ^= 1) %then %do;
         
            do i = 1 to &totfoodsminus1;
              rinew = update_r(sum_weightnumrecalls, i, r, rpossible, rspacing, theta, v, w_crossresid_ksum);
              r[i, 1]  =  rinew; 
            end;
         
            do i = 1 to lengththeta;
              thetainew = update_theta(i, r, theta, thetapossible, thetaspacing, v, w_crossresid_ksum);
              theta[i, 1]    =  thetainew; 
            end;
         
          %end;  /*** End two or more episodically consumed foods ***/
  
        
          do i = 2 to &totfoodvars by 2;
            viinew = update_vii(sum_weightnumrecalls, i, 
                                %if (&totfoods ^= 1) %then %do;
                                  r, theta, 
                                %end;
                                v, w_crossresid_ksum
                                );
            v[i, i] = viinew;
          end;
  
        %end;  /*** End one or more episodically consumed foods ***/
  
        
        /*** Nutrient or food consumed daily ***/
  
        %if (&totfoodvars ^= &totvars) %then %do;
          do i = &totfoodvarsplus1 to &totvars;
            viinew = update_vii(sum_weightnumrecalls, i, 
                                %if ((&totfoods ^= 0) & (&totfoods ^= 1)) %then %do;
                                  r, theta, 
                                %end;
                                v, w_crossresid_ksum
                                );
            v[i, i] = viinew;
          end;
        %end;  /*** End nutrient or food consumed daily ***/
  
        
        /*** Two or more episodically consumed foods ***/
  
        %if ((&totfoods ^= 0) & (&totfoods ^= 1)) %then %do;
          do i = 4 to &totfoodvars by 2;
            do j = 1 to (i-2);   
              vijnew = update_vij(i, j, r, theta, v, w_crossresid_ksum);
              v[i, j] = vijnew;
            end;
          end;
        %end;  /*** End two or more episodically consumed foods ***/
  
  
        /*** Nutrient or food consumed daily ***/
  
        %if (&totfoodvars ^= &totvars) %then %do;
          do i = &totfoodvarsplus1 to &totvars;
            do j = 1 to (i-1);   
              vijnew = update_vij(i, j, 
                                  %if ((&totfoods ^= 0) & (&totfoods ^= 1)) %then %do;
                                    r, theta, 
                                  %end;
                                  v, w_crossresid_ksum
                                  );
              v[i, j] = vijnew;
            end;
          end;
        %end;  /*** End nutrient or food consumed daily ***/
        
  
        /*** Two or more episodically consumed foods ***/
  
        %if ((&totfoods ^= 0) & (&totfoods ^= 1)) %then %do;
          v[3, 1] = r[1, 1] # sin(theta[1, 1]);
          v[3, 2] = r[1, 1] # cos(theta[1, 1]);
  
        
          /*** Three or more episodically consumed foods ***/
            
          %if (&totfoods ^= 2) %then %do;
  
            do k = 2 to &totfoodsminus1;
         
              twokplus1 = 2#k + 1;
              kminus1sq = (k - 1)##2;
              twokminus1 = 2#k - 1;
              
              v[twokplus1, 1] = r[k, 1] # sin(theta[kminus1sq + 1, 1]);
             
              do j = 2 to twokminus1;
         
                v[twokplus1, j] = r[k, 1] # sin(theta[kminus1sq + j, 1]);
         
                do jj = 1 to (j-1);
                  v[twokplus1, j] = v[twokplus1, j] # cos(theta[kminus1sq + jj, 1]);
                end;
         
              end;
         
              v[twokplus1, 2#k] = r[k, 1];
         
              do jj = 1 to twokminus1;
                v[twokplus1, 2#k] = v[twokplus1, 2#k] # cos(theta[kminus1sq + jj, 1]);
              end;
         
            end;
   
          %end;  /*** End three or more episodically consumed foods ***/
            
          do k = 3 to (&totfoodvars - 1) by 2;
            v[k, k]  = sqrt(1 - (r[(k - 1)/2, 1])##2);
            v[k+1, k] = -(sum( v[k, 1:(k-1)] # v[k+1, 1:(k-1)] )) /  v[k, k];
          end;
  
        %end;  /*** End two or more episodically consumed foods ***/
  
  
   
        sigmae = v * t(v);
        isigmae = &gen_inverse_label.inv(sigmae);
    
      
      
  
        /**********************/
        /*** Update isigmau ***/
        /**********************/
  
        %if %upcase(&sigmau_constant)^=Y %then %do;
    
           
          aa2 = ( (prior_sigmau_doff-nrow(sigmau)-1) # prior_sigmau_mean) + (t(weight_nw_sumw # umatrix) * umatrix); 
          bb2 = prior_sigmau_doff + n;
          
          aa2 = aa2 + (.0011 # (1 # n) # i(&totvars));
          bb2 = bb2 + (.0011 # (1 # n));
          aa2 = (aa2 + t(aa2)) / 2; 
          
          aa2_div_bb2 = aa2 / bb2;
    
  
          if min(eigval(aa2_div_bb2)) <= 0.001 then do;
            printcounter1 = printcounter1 + 1;
            if printcounter1 = 1 then print "Problem with sigmau at step" jjmcmc, sigmau, sigmae, aa2_div_bb2;
          end;
          else do;
    
            inv_aa2 = &gen_inverse_label.inv(aa2);  
            tempisigmau = randwishart(1, bb2, inv_aa2);
            isigmau = shape(tempisigmau, &totvars, &totvars, .);
    
            sigmau  = &gen_inverse_label.inv(isigmau);
      
          end;
    
  
        %end;


      end;  /* end of code that is executed for mcmc iterations and not executed for post mcmc iterations */ 




      /**********************/
      /*** Update umatrix ***/
      /**********************/

      %if (&set_num_nev_consumers_vars = 1) %then %do;

        /*** Allow for never-consumers ***/
 
        umatrix_curr = umatrix; 
        umatrix_cand = umatrix; 

      %end;

      sumss = ((wmatrix_k1 - xbeta_k1) # nxdays_avail_indicat[, 1])

              %if (&maxnumdays ^= 1) %then %do;
                %do dayk = 2 %to &maxnumdays;
                    + ((wmatrix_k&dayk - xbeta_k&dayk) # nxdays_avail_indicat[, &dayk])
                %end;
              %end;
              ;     
      
      c1  = sumss * isigmae;


      znormnxtotvars = j(n, &totvars, .);
      call randgen(znormnxtotvars, 'normal');


      %do totrecalls = 1 %to &maxnumdays;
          
        if ( nrow(rows_numrecalls_&totrecalls) > 0 ) then do;

          c2  = &gen_inverse_label.inv(isigmau + (&totrecalls # isigmae));
            
          call eigen(eigvalsc2, eigvecsc2, c2);
          eigvalshalf = eigvalsc2 ## 0.5;
          sqrtm_c2 = eigvecsc2 * diag(eigvalshalf) * t(eigvecsc2);

          %if (&set_num_nev_consumers_vars = 1) %then %do;

            /*** Allow for never-consumers ***/
 
            umatrix_cand[rows_numrecalls_&totrecalls, ] = (c1[rows_numrecalls_&totrecalls, ] * c2) 
                                                           + znormnxtotvars[rows_numrecalls_&totrecalls, ] * sqrtm_c2;

          %end;
          %else %do;

            umatrix[rows_numrecalls_&totrecalls, ] = (c1[rows_numrecalls_&totrecalls, ] * c2) 
                                                      + znormnxtotvars[rows_numrecalls_&totrecalls, ] * sqrtm_c2;

          %end;
        
        end;
          
      %end;


      %if (&set_num_nev_consumers_vars = 1) %then %do;
      
        /*** Allow for never-consumers ***/
  
        rows_conni1_notpos_isnevcon_eq1 = loc(conni1_notpos_isnevcon = 1);

        if nrow(rows_conni1_notpos_isnevcon_eq1) = 0 then umatrix = umatrix_cand;
        else do;
  
          logfac2_cand = constant1;
          logfac2_curr = constant1;
          
          logfac2_cand[rows_conni1_notpos_isnevcon_eq1, ] = (-1) # (nxdays_avail_indicat[rows_conni1_notpos_isnevcon_eq1, 1] 
                                                     # (log(0.000000000001 <> (1 - cdf('normal', (xbeta_k1[rows_conni1_notpos_isnevcon_eq1, 1] + umatrix_cand[rows_conni1_notpos_isnevcon_eq1, 1])))))
                                                     %if (&maxnumdays ^= 1) %then %do;
                                                       %do dayk = 2 %to &maxnumdays;
                                                         + nxdays_avail_indicat[rows_conni1_notpos_isnevcon_eq1, &dayk] 
                                                           # (log(0.000000000001 <> (1 - cdf('normal', (xbeta_k&dayk[rows_conni1_notpos_isnevcon_eq1, 1] + umatrix_cand[rows_conni1_notpos_isnevcon_eq1, 1]))))) 
                                                       %end;
                                                     %end;
                                                     );
  
          logfac2_curr[rows_conni1_notpos_isnevcon_eq1, ] = (-1) # (nxdays_avail_indicat[rows_conni1_notpos_isnevcon_eq1, 1] 
                                                     # (log(0.000000000001 <> (1 - cdf('normal', xbeta_u_k1[rows_conni1_notpos_isnevcon_eq1, 1]))))
                                                     %if (&maxnumdays ^= 1) %then %do;
                                                       %do dayk = 2 %to &maxnumdays;
                                                         + nxdays_avail_indicat[rows_conni1_notpos_isnevcon_eq1, &dayk] 
                                                           # (log(0.000000000001 <> (1 - cdf('normal', xbeta_u_k&dayk[rows_conni1_notpos_isnevcon_eq1, 1])))) 
                                                       %end;
                                                     %end;
                                                     );


          tempnx1diff = 0 >< (logfac2_cand - logfac2_curr);
          con1_nevconmetrop = exp(tempnx1diff);
       
          ss4unifnx1 = j(n, 1, .);
          call randgen(ss4unifnx1, 'uniform');
  
  
          umatrix = umatrix_cand # (ss4unifnx1 <= con1_nevconmetrop) + umatrix_curr # (ss4unifnx1 > con1_nevconmetrop);

 
        end;


      %end;

    


      if (jjmcmc <= nmcmc) then do;

        /*******************/
        /*** Update beta ***/
        /*******************/
  
        %do varj = 1 %to &totvars;
  
          betanew&varj = update_beta(&varj, ncolxmatrix&varj, sum_day_subj_wt_pxp_xmatrix&varj 
                                     %do dayk = 1 %to &maxnumdays;
                                       , t_wt_avail_xmatrix&varj._k&dayk, wmatrix_k&dayk, xbeta_k&dayk  
                                     %end;
                                     %if (&set_num_nev_consumers_vars = 1) %then %do;  
                                       /*** Allow for never-consumers ***/
                                       %do dayk = 1 %to &maxnumdays; 
                                         , xmatrix&varj._k&dayk  
                                       %end;
                                       , beta1, nxdays_avail_indicat, conni1_notpos_isnevcon
                                     %end;
                                     , isigmae, umatrix, iprior_beta&varj._cov, iprior_cov_prior_mean_beta&varj 
                                     );
  
          beta&varj = betanew&varj;        
  
  
          /********************************************/
          /*** Update matrices used throughout mcmc ***/
          /********************************************/
          
          %do dayk = 1 %to &maxnumdays;
  
            xbeta_k&dayk[, &varj] = (xmatrix&varj._k&dayk * beta&varj);
          
            xbeta_u_k&dayk[, &varj] = xbeta_k&dayk[, &varj] + umatrix[, &varj];
          
          %end;
          
          /********************************************/
  
  
        %end;
        
  
  
  
        /********************************************************/
        /*** Calculate sums, sums of squares, and mcmc traces ***/
        /********************************************************/
        
        if jjmcmc > nburn then do;
          if saveiter = &set_thin then do;  /*** saveiter is initialized at thinning value, so (nburn + 1) is the first iteration used ***/ 
  
     
            denomthinct = denomthinct + 1;
  
  
            %if (&set_num_nev_consumers_vars = 1) %then %do;  
          
              /*** Allow for never-consumers ***/
        
              alpha1_sum = alpha1_sum + alpha1; 
              alpha1_sumsq = alpha1_sumsq + (alpha1)##2; 
  
              prob_consumer_epis1 = (t(prob_consumer_nx1_epis1) * constant1) / n;
              prob_consumer_epis1_sum = prob_consumer_epis1_sum + prob_consumer_epis1;
              prob_consumer_epis1_sumsq = prob_consumer_epis1_sumsq + (prob_consumer_epis1)##2;
           
            %end;
  
     
            %do varj = 1 %to &totvars;
              beta&varj._sum = beta&varj._sum + beta&varj;
              beta&varj._sumsq = beta&varj._sumsq + (beta&varj)##2;
            %end;
  
            sigmau_sum = sigmau_sum + sigmau;
            sigmau_sumsq = sigmau_sumsq + sigmau##2;
     
            sigmae_sum = sigmae_sum + sigmae;
            sigmae_sumsq = sigmae_sumsq + sigmae##2;
  
  
            /*** Calculate mcmc traces ***/
  
            %if (&set_num_nev_consumers_vars = 1) %then %do;  
          
              /*** Allow for never-consumers ***/
          
              alpha1_trace[denomthinct, ] = t(alpha1); 
              prob_consumer_epis1_trace[denomthinct, ] = prob_consumer_epis1;
           
            %end;
  
  
            %do varj = 1 %to &totvars;
             
              beta&varj._trace[denomthinct, ] = t(beta&varj);
          
              %do vark = 1 %to &varj;
           
                sigmau_row&varj._col&vark._trace[denomthinct, ] = sigmau[&varj, &vark];
  
                sigmae_row&varj._col&vark._trace[denomthinct, ] = sigmae[&varj, &vark];
                                 
              %end;
            %end;
  
  
            /*****************************/
  
            %if %upcase(&save_mcmc_u_out_data)=Y %then %do;
   
              /**********************/
              /*** write u matrix ***/
              /**********************/
   
              trace_afterburn_thin_u_outmatrix = j(n, 1, denomthinct) || &subject || umatrix;
  
              /*** Create and update data set ***/
  
              if (denomthinct = 1) then do;
                create &out_lib..multivar_mcmc_samples_u_out&out_save_label_max5char from trace_afterburn_thin_u_outmatrix [colname=trace_afterburn_thin_u_colnames];
              end;
              else do;
                edit &out_lib..multivar_mcmc_samples_u_out&out_save_label_max5char;
              end;
   
              append from trace_afterburn_thin_u_outmatrix;
  
              close &out_lib..multivar_mcmc_samples_u_out&out_save_label_max5char;
  
            %end;
        
  
            /*****************************/
  
  
            saveiter = 0;
      
          end;
      
          saveiter = saveiter + 1;
      
        end;
    
  
 
     
        /************************************************************************************/
        /************************************************************************************/
     
        /*** Last iteration of mcmc loop ***/ 
        if (jjmcmc = nmcmc) then do;    
  
   
          %if (&set_num_nev_consumers_vars = 1) %then %do;  
          
            /*** Allow for never-consumers ***/
          
            alpha1_mean = alpha1_sum / denomthinct;
            %if %upcase(&std_print_store)=Y %then %do;
              alpha1_std = ((alpha1_sumsq - denomthinct # alpha1_mean##2) / (denomthinct - 1))##0.5;
            %end;
      
            prob_consumer_epis1_mean = prob_consumer_epis1_sum / denomthinct;
            %if %upcase(&std_print_store)=Y %then %do;
              prob_consumer_epis1_std = ((prob_consumer_epis1_sumsq - denomthinct # prob_consumer_epis1_mean##2) / (denomthinct - 1))##0.5;
            %end;
          
          %end;
      
      
          %do varj = 1 %to &totvars;

            beta&varj._mean = beta&varj._sum / denomthinct;
            beta&varj = beta&varj._mean;

            %do dayk = 1 %to &maxnumdays;
           
              xbeta_k&dayk[, &varj] = (xmatrix&varj._k&dayk * beta&varj);
            
              xbeta_u_k&dayk[, &varj] = xbeta_k&dayk[, &varj] + umatrix[, &varj];
            
            %end;
            
            %if %upcase(&std_print_store)=Y %then %do;
              beta&varj._std = ((beta&varj._sumsq - denomthinct # beta&varj._mean##2) / (denomthinct - 1))##0.5;
            %end;

          %end;
      
      
          sigmau_mean = sigmau_sum / denomthinct;
          sigmau = sigmau_mean;
          isigmau = &gen_inverse_label.inv(sigmau);
          %if (%upcase(&std_print_store)=Y) & (%upcase(&sigmau_constant)^=Y) %then %do;
            sigmau_std  = ((sigmau_sumsq - denomthinct # sigmau_mean##2) / (denomthinct - 1))##0.5;
          %end;
      
      
          sigmae_mean = sigmae_sum / denomthinct;
          sigmae = sigmae_mean;
          isigmae = &gen_inverse_label.inv(sigmae);
          %if %upcase(&std_print_store)=Y %then %do;
            sigmae_std  = ((sigmae_sumsq - denomthinct # sigmae_mean##2) / (denomthinct - 1))##0.5;
          %end;
      
      
      
       
          %if %upcase(&print)^=N %then %do;
            print 
      
              %if (&set_num_nev_consumers_vars = 1) %then %do;  
              
                /*** Allow for never-consumers ***/
            
                prob_consumer_epis1_mean 
                %if %upcase(&std_print_store)=Y %then %do;
                  prob_consumer_epis1_std 
                %end;
             
                alpha1_mean 
                %if %upcase(&std_print_store)=Y %then %do;
                  alpha1_std 
                %end;
              
              %end;
              ,,,
      
              %do varj = 1 %to &totvars;
      
                beta&varj._mean 
      
                %if %upcase(&std_print_store)=Y %then %do;
                  beta&varj._std 
                %end;
      
              %end;
              ,,,
      
              sigmau_mean,,,          
      
              %if (%upcase(&std_print_store)=Y) & (%upcase(&sigmau_constant)^=Y) %then %do;
                sigmau_std,,, 
              %end;
      
              sigmae_mean,,, 
      
              %if %upcase(&std_print_store)=Y %then %do;
                sigmae_std,,,
              %end;
      
              ; 
          %end;   
      
      
          /*************************************/
          /*** Additional values for storage ***/
          /*************************************/
      
          num_epis_diet_comp = &totfoods;
          num_daily_diet_comp = &totnutrients;
          num_rows_covmat = &totvars;
      
      
          /**************************************/
          /*** Read optional storage data set ***/
          /**************************************/
          
          %if (&optional_iml_store_data ^= %str()) %then %do;
      
            use &optional_iml_store_data  var _all_;
      
            %if %upcase(&print)^=N %then %do;
              print "Output from the Show Contents Statement for optional_iml_store_data = &optional_iml_store_data",,;
              show contents;
            %end;
            
            read all;
            
          %end;
      
      
          /************************************/
          /*** Save values into IML catalog ***/
          /************************************/
          
          reset storage=&out_lib..iml&out_store_label;
      
          remove _all_;
      
          store 
               
            weight_nw_sumw
            
            num_epis_diet_comp
            num_daily_diet_comp
            num_rows_covmat
      
            %if (&set_num_nev_consumers_vars = 1) %then %do;  
            
              /*** Allow for never-consumers ***/
          
              prob_consumer_epis1_mean 
              %if %upcase(&std_print_store)=Y %then %do;
                prob_consumer_epis1_std 
              %end;
           
              alpha1_mean 
              %if %upcase(&std_print_store)=Y %then %do;
                alpha1_std 
              %end;
            
            %end;
      
            %do varj = 1 %to &totvars;
              beta&varj._mean 
              %if %upcase(&std_print_store)=Y %then %do;
                beta&varj._std 
              %end;
            %end;
      
            sigmau_mean
            %if (%upcase(&std_print_store)=Y) & (%upcase(&sigmau_constant)^=Y) %then %do;
              sigmau_std
            %end;
      
            sigmae_mean 
            %if %upcase(&std_print_store)=Y %then %do;
              sigmae_std
            %end;
      
            &optional_iml_store_names
            ;
      
          %if %upcase(&print)^=N %then %do;
            print "Output from the Show Storage Statement",,;
            show storage;
          %end;
      
      
      
      
          /**************************************************************************************/
          /*** Prepare data for multivar_mcmc_samples_out output data set and for trace plots ***/
          /**************************************************************************************/
      
          %if ((&traceplots_method1_gpath^=%str()) | (&traceplots_method2_file_pdf^=%str())) %then %let traceplot_requested=Y;
          %else %let traceplot_requested=N;
         
          %if ((%upcase(&traceplot_requested)=Y) | (&set_number_saved_out_data ^= 0)) %then %do;
      
      
            /*** Initialize output matrix and column names vector ***/
            
            total_thinned_iterations = j(length_index_afterburn_thin, 1, length_index_afterburn_thin);
            iteration = t(1:length_index_afterburn_thin); 
           
            trace_afterburn_thin_outmatrix = iteration || total_thinned_iterations;
            trace_afterburn_thin_colnames = { "Iteration" "total_thinned_iterations" };
          
         
            %if (&set_num_nev_consumers_vars = 1) %then %do;  
           
              /*** Allow for never-consumers ***/
           
              /*** Consumer probability and alpha1 ***/
              
              %let dd_alpha_trace_var_list = &tot_covars_prconep1;
              %let cc_probcon_alpha_trace_var_list = %eval(1 + &dd_alpha_trace_var_list);
              %let alpha_trace_var_list = ;
         
              %do alpha_col_zz = 1 %to &tot_covars_prconep1;
                %let alpha_trace_var_list = &alpha_trace_var_list alpha1_param&alpha_col_zz._trace;
              %end;
       
              %let probcon_alpha_trace_var_list = prob_consumer_epis1_trace &alpha_trace_var_list;
           
              trace_afterburn_thin_outmatrix = trace_afterburn_thin_outmatrix || prob_consumer_epis1_trace || alpha1_trace;
              trace_afterburn_thin_colnames = trace_afterburn_thin_colnames || {"prob_consumer_epis1_trace"  
                                                                                %do alpha_col_zz = 1 %to &tot_covars_prconep1;
                                                                                  "alpha1_param&alpha_col_zz._trace"
                                                                                %end;
                                                                                };
          
            %end;
      
         
            /*** Betaj ***/
            
            %let qq_beta_trace_var_list = 0;
            %let beta_trace_var_list = ;
           
            %do varj = 1 %to &totvars;
            
              %let qq_beta_trace_var_list = %eval(&qq_beta_trace_var_list + &&param_count_beta&varj);
              %do beta_col_pp = 1 %to &&param_count_beta&varj;
                %let beta_trace_var_list = &beta_trace_var_list beta&varj._param&beta_col_pp._trace;
              %end;
           
              trace_afterburn_thin_outmatrix = trace_afterburn_thin_outmatrix || beta&varj._trace;
              trace_afterburn_thin_colnames = trace_afterburn_thin_colnames || {
                                                                                %do beta_col_pp = 1 %to &&param_count_beta&varj;
                                                                                  "beta&varj._param&beta_col_pp._trace"
                                                                                %end;
                                                                                };
           
            %end;
      
      
            /*** Sigmau ***/
        
            %let rr_sigmau_trace_var_list = 0;
            %let sigmau_trace_var_list = ;
        
            %do varj = 1 %to &totvars;
              %do vark = 1 %to &varj;
        
                %let rr_sigmau_trace_var_list = %eval(&rr_sigmau_trace_var_list + 1);
                %let sigmau_trace_var_list = &sigmau_trace_var_list sigmau_row&varj._col&vark._trace;
        
                trace_afterburn_thin_outmatrix = trace_afterburn_thin_outmatrix || sigmau_row&varj._col&vark._trace;
                trace_afterburn_thin_colnames = trace_afterburn_thin_colnames || {"sigmau_row&varj._col&vark._trace"};
        
              %end;
            %end;
      
      
            /*** Sigmae ***/
         
            %let ss_sigmae_trace_var_list = 0;
            %let sigmae_trace_var_list = ;
         
            %do varj = 1 %to &totvars;
              %do vark = 1 %to &varj;
      
                %let keep_param_plot = y;
                
                %if (&totfoods ^= 0) %then %do;
                  %do even_index_foodvar = 2 %to &totfoodvars %by 2;
                    %let odd_index_foodvar = %eval(&even_index_foodvar - 1); 
                    %if ((&vark = &odd_index_foodvar) & ((&varj = &odd_index_foodvar) | (&varj = &even_index_foodvar))) 
                      %then %let keep_param_plot = n;
                  %end;
                %end;
      
                %if (%upcase(&keep_param_plot)=Y) %then %do;
                  %let ss_sigmae_trace_var_list = %eval(&ss_sigmae_trace_var_list + 1);
                  %let sigmae_trace_var_list = &sigmae_trace_var_list sigmae_row&varj._col&vark._trace;
                %end;
            
                trace_afterburn_thin_outmatrix = trace_afterburn_thin_outmatrix || sigmae_row&varj._col&vark._trace;
                trace_afterburn_thin_colnames = trace_afterburn_thin_colnames || {"sigmae_row&varj._col&vark._trace"};
         
              %end;
            %end;
      
      
            /*** Create data set ***/
        
            create trace_afterburn_thin_data from trace_afterburn_thin_outmatrix [colname=trace_afterburn_thin_colnames];
            append from trace_afterburn_thin_outmatrix;
            close trace_afterburn_thin_data;
      
          %end;
    
  
        end;  /*** End of code that is executed for last mcmc iteration ***/ 
      end;  /*** End of code that is executed for mcmc iterations and not executed for post mcmc iterations */ 
      else do;

  
        %do varj = 1 %to &totvars;
          %do dayk = 1 %to &maxnumdays;
            xbeta_u_k&dayk[, &varj] = xbeta_k&dayk[, &varj] + umatrix[, &varj];
          %end;
        %end;
          

        /*****************************/
  
        %if (&set_number_post_mcmc_u_out ^= 0) %then %do;
 
          /********************************/
          /*** write post mcmc u matrix ***/
          /********************************/
 
          
          post_mcmc_iteration = jjmcmc - nmcmc;
          post_mcmc_u_outmatrix = j(n, 1, post_mcmc_iteration) || &subject || umatrix;
 
          /*** Create and update data set ***/
 
          if (post_mcmc_iteration = 1) then do;
            create &out_lib..multivar_post_mcmc_u_out&out_save_label_max5char from post_mcmc_u_outmatrix [colname=post_mcmc_u_colnames];
          end;
          else do;
            edit &out_lib..multivar_post_mcmc_u_out&out_save_label_max5char;
          end;
 
          append from post_mcmc_u_outmatrix;
 
          close &out_lib..multivar_post_mcmc_u_out&out_save_label_max5char;
 
        %end;
    
        /*****************************/


      end;  /*** End of code that is executed for post mcmc iterations ***/ 
    
    end;  /*** End mcmc plus post mcmc loop ***/



  quit;
  run;
  



  /********************************************************/
  /*** Create multivar_mcmc_samples_out output data set ***/
  /********************************************************/

  %if (&set_number_saved_out_data ^= 0) %then %do;

    data &out_lib..multivar_mcmc_samples_out&out_save_label_max5char (drop = number_notsaved total_thinned_iterations);
      set trace_afterburn_thin_data;
      
      number_notsaved = total_thinned_iterations - &set_number_saved_out_data;
      if iteration > number_notsaved then output &out_lib..multivar_mcmc_samples_out&out_save_label_max5char;
    
    run;

  %end;  
     



  /**************************/
  /*** Create trace plots ***/
  /**************************/

  %if (&traceplots_method1_gpath^=%str()) %then %do;
    ods listing gpath = &traceplots_method1_gpath;
    run;
    %if (&traceplots_method2_file_pdf^=%str()) %then %do;
      ods pdf file="&traceplots_method2_file_pdf." notoc;
      run;
    %end;
  %end;
  %else %if (&traceplots_method2_file_pdf^=%str()) %then %do;
    ods listing close; 
    run; 
    ods pdf file="&traceplots_method2_file_pdf." notoc ; 
    run;
  %end;
  

  %if %upcase(&traceplot_requested)=Y %then %do;

    data paramvarlabels1rec(drop = qq_beta rr_sigmau ss_sigmae char_leadingzeros_mm
                                   %if (&set_num_nev_consumers_vars = 1) %then %do;  
                                     /*** Allow for never-consumers ***/
                                     dd_alpha 
                                   %end;
                                   );
    
      length char_leadingzeros_mm $ 8;


      %if (&set_num_nev_consumers_vars = 1) %then %do;  
     
        /*** Allow for never-consumers ***/

     
        /*** Create variables for trace plot labels and sorting ***/ 
        /*** Consumer probability and alpha1 ***/


        probcon_var_label = "Prob_Consumer_Epis1_Trace_Plot";

      
        dd_alpha = &dd_alpha_trace_var_list;
      
        %do mm = 1 %to &dd_alpha_trace_var_list;
      
          if (dd_alpha < 9.5) then char_leadingzeros_mm = "&mm";
          else if (dd_alpha < 99.5) then char_leadingzeros_mm = put(&mm, z2.);
          else if (dd_alpha < 999.5) then char_leadingzeros_mm = put(&mm, z3.);
          else if (dd_alpha < 9999.5) then char_leadingzeros_mm = put(&mm, z4.);
          else if (dd_alpha < 99999.5) then char_leadingzeros_mm = put(&mm, z5.);
          else char_leadingzeros_mm = "&mm";
          
          alpha_var_label&mm = "Alpha_Trace_Plot_" || strip(char_leadingzeros_mm) || "____" || "%quote(%scan(&alpha_trace_var_list, &mm, %str( )))";

      
        %end;
      %end;
    
    
      /*** Create variables for trace plot labels and sorting ***/ 
      /*** Betaj ***/
    
      qq_beta = &qq_beta_trace_var_list;
    
      %do mm = 1 %to &qq_beta_trace_var_list;
    
        if (qq_beta < 9.5) then char_leadingzeros_mm = "&mm";
        else if (qq_beta < 99.5) then char_leadingzeros_mm = put(&mm, z2.);
        else if (qq_beta < 999.5) then char_leadingzeros_mm = put(&mm, z3.);
        else if (qq_beta < 9999.5) then char_leadingzeros_mm = put(&mm, z4.);
        else if (qq_beta < 99999.5) then char_leadingzeros_mm = put(&mm, z5.);
        else char_leadingzeros_mm = "&mm";
        
        beta_var_label&mm = "Beta_Trace_Plot_" || strip(char_leadingzeros_mm) || "____" || "%quote(%scan(&beta_trace_var_list, &mm, %str( )))";
    
      %end;
    
    
      /*** Create variables for trace plot labels and sorting ***/ 
      /*** Sigmau ***/
      
      rr_sigmau = &rr_sigmau_trace_var_list;
      
      %do mm = 1 %to &rr_sigmau_trace_var_list;
      
        if (rr_sigmau < 9.5) then char_leadingzeros_mm = "&mm";
        else if (rr_sigmau < 99.5) then char_leadingzeros_mm = put(&mm, z2.);
        else if (rr_sigmau < 999.5) then char_leadingzeros_mm = put(&mm, z3.);
        else if (rr_sigmau < 9999.5) then char_leadingzeros_mm = put(&mm, z4.);
        else if (rr_sigmau < 99999.5) then char_leadingzeros_mm = put(&mm, z5.);
        else char_leadingzeros_mm = "&mm";
        
        sigmau_var_label&mm = "Sigmau_Trace_Plot_" || strip(char_leadingzeros_mm) || "____" || "%quote(%scan(&sigmau_trace_var_list, &mm, %str( )))";
      
      %end;
      
      
      /*** Create variables for trace plot labels and sorting ***/ 
      /*** Sigmae ***/

      ss_sigmae = &ss_sigmae_trace_var_list;
   
      %do mm = 1 %to &ss_sigmae_trace_var_list;
   
        if (ss_sigmae < 9.5) then char_leadingzeros_mm = "&mm";
        else if (ss_sigmae < 99.5) then char_leadingzeros_mm = put(&mm, z2.);
        else if (ss_sigmae < 999.5) then char_leadingzeros_mm = put(&mm, z3.);
        else if (ss_sigmae < 9999.5) then char_leadingzeros_mm = put(&mm, z4.);
        else if (ss_sigmae < 99999.5) then char_leadingzeros_mm = put(&mm, z5.);
        else char_leadingzeros_mm = "&mm";
        
        sigmae_var_label&mm = "Sigmae_Trace_Plot_" || strip(char_leadingzeros_mm) || "____" || "%quote(%scan(&sigmae_trace_var_list, &mm, %str( )))";
   
      %end;
   
   
      output paramvarlabels1rec;
   
    run;  

  
    options nonotes;
   
 
 
 
    %if (&set_num_nev_consumers_vars = 1) %then %do;  
     
      /*** Allow for never-consumers ***/


      ods graphics on / noborder noantialias width=720px imagename="Prob_Con_Alpha_Trace_Plot_Panels4_Image" ;  
      run;           
     
      %do plot_first_each_page = 1 %to &cc_probcon_alpha_trace_var_list %by 4; 
    
          
        data alpha_paneldata(keep = iteration var_c parameter mcmc_sample);
          if _n_=1 then set paramvarlabels1rec;
          set trace_afterburn_thin_data;
      
          length parameter $ 65;
      
      
          /*** Consumer probability and alpha1 ***/
      
          array probcon_alpha_var_list(&cc_probcon_alpha_trace_var_list)  &probcon_alpha_trace_var_list;
          array probcon_alpha_var_label(&cc_probcon_alpha_trace_var_list) probcon_var_label alpha_var_label1 - alpha_var_label&dd_alpha_trace_var_list;
        
          minval_plot_last_each_page = min(&plot_first_each_page + 3, &cc_probcon_alpha_trace_var_list);
    
          do var_c = &plot_first_each_page to minval_plot_last_each_page;
        
            MCMC_Sample = probcon_alpha_var_list(var_c);
            parameter = probcon_alpha_var_label(var_c);
        
            output alpha_paneldata;
        
          end;
        
        run;
      
    
    
    
        proc sort data=alpha_paneldata;
          by var_c iteration;
        run;
         
        
        proc sgpanel data=alpha_paneldata;
          panelby parameter / rows=4 columns=1 uniscale=column novarname;
          series x=iteration y=mcmc_sample;
          title " ";
        run;
     
     
      %end;     
    %end;


 
 
    ods graphics on / noborder noantialias width=720px imagename="Beta_Trace_Plot_Panels4_Image";  
    run;           
   
    %do plot_first_each_page = 1 %to &qq_beta_trace_var_list %by 4; 
  
        
      data beta_paneldata(keep = iteration var_q parameter mcmc_sample);
        if _n_=1 then set paramvarlabels1rec;
        set trace_afterburn_thin_data;
    
        length parameter $ 65;
    
    
        /*** Betaj ***/
    
        array beta_var_list(&qq_beta_trace_var_list)  &beta_trace_var_list;
        array beta_var_label(&qq_beta_trace_var_list)  beta_var_label1 - beta_var_label&qq_beta_trace_var_list;
      
        minval_plot_last_each_page = min(&plot_first_each_page + 3, &qq_beta_trace_var_list);
  
        do var_q = &plot_first_each_page to minval_plot_last_each_page;
      
          MCMC_Sample = beta_var_list(var_q);
          parameter = beta_var_label(var_q);
      
          output beta_paneldata;
      
        end;
      
      run;
    
  
  
  
      proc sort data=beta_paneldata;
        by var_q iteration;
      run;
       
      
      proc sgpanel data=beta_paneldata;
        panelby parameter / rows=4 columns=1 uniscale=column novarname;
        series x=iteration y=mcmc_sample;
        title " ";
      run;
   
   
    %end;     

     
    

    ods graphics on / noborder noantialias width=720px imagename="Sigmau_Trace_Plot_Panels4_Image";  
    run;           
   
    %do plot_first_each_page = 1 %to &rr_sigmau_trace_var_list %by 4; 
    
      
      data sigmau_paneldata(keep = iteration var_r parameter mcmc_sample);
        if _n_=1 then set paramvarlabels1rec;
        set trace_afterburn_thin_data;
    
        length parameter $ 65;
    
    
        /*** Sigmau ***/
    
        array sigmau_var_list(&rr_sigmau_trace_var_list)  &sigmau_trace_var_list;
        array sigmau_var_label(&rr_sigmau_trace_var_list)  sigmau_var_label1 - sigmau_var_label&rr_sigmau_trace_var_list;
      
        minval_plot_last_each_page = min(&plot_first_each_page + 3, &rr_sigmau_trace_var_list);
   
        do var_r = &plot_first_each_page to minval_plot_last_each_page;
      
          MCMC_Sample = sigmau_var_list(var_r);
          parameter = sigmau_var_label(var_r);
      
          output sigmau_paneldata;
      
        end;
    
    
      run;
      
    
    
    
      proc sort data=sigmau_paneldata;
        by var_r iteration;
      run;
    
    
      proc sgpanel data=sigmau_paneldata;
        panelby parameter / rows=4 columns=1 uniscale=column novarname;
        series x=iteration y=mcmc_sample;
        title " ";
      run;
      
      
    %end;     
    
   
   
   
    ods graphics on / noborder noantialias width=720px imagename="Sigmae_Trace_Plot_Panels4_Image";  
    run;           
   
    %do plot_first_each_page = 1 %to &ss_sigmae_trace_var_list %by 4; 
    
      
      data sigmae_paneldata(keep = iteration var_s parameter mcmc_sample);
        if _n_=1 then set paramvarlabels1rec;
        set trace_afterburn_thin_data;
    
        length parameter $ 65;
    
    
        /*** Sigmae ***/
    
        array sigmae_var_list(&ss_sigmae_trace_var_list)  &sigmae_trace_var_list;
        array sigmae_var_label(&ss_sigmae_trace_var_list)  sigmae_var_label1 - sigmae_var_label&ss_sigmae_trace_var_list;
      
        minval_plot_last_each_page = min(&plot_first_each_page + 3, &ss_sigmae_trace_var_list);
   
        do var_s = &plot_first_each_page to minval_plot_last_each_page;
      
          MCMC_Sample = sigmae_var_list(var_s);
          parameter = sigmae_var_label(var_s);
      
          output sigmae_paneldata;
      
        end;
      
      run;
      
    
    
    
      proc sort data=sigmae_paneldata;
        by var_s iteration;
      run;
    
      
      proc sgpanel data=sigmae_paneldata;
        panelby parameter / rows=4 columns=1 uniscale=column novarname;
        series x=iteration y=mcmc_sample;
        title " ";
      run;
       
      
    %end; 
    
        
  %end;     
   

  %if (&traceplots_method1_gpath^=%str()) %then %do;
    ods graphics off;
    run;              
    %if (&traceplots_method2_file_pdf^=%str()) %then %do;
      ods pdf close;    
      run;                           
    %end;
  %end;
  %else %if (&traceplots_method2_file_pdf^=%str()) %then %do;
    ods graphics off;
    run;           
    ods pdf close; 
    run;           
    ods listing;   
    run;           
  %end;


  options notes;


%mend multivar_mcmc;  /*** End macro ***/
