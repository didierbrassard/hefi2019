/* !! PLEASE NOTE THAT THIS EXAMPLE CODE IS PROVIDED `AS IS`, WITHOUT 
	WARRANTY OF ANY KIND, EXPRESS OR IMPLIED. THIS CODE INVOLVES THE SOLE
	RESPONSABILITY OF THE AUTHOR AND IS NOT ENDORSED BY STATISTICS CANADA
	OR HEALTH CANADA. !! */

 /*************************************************************************/
 /*                                                                       */
 /*                     CCHS 2015 - Nutrition (PUMF)                      */
 /*                                                                       */
 /*       Obtain usual intakes based on the NCI multivariate method       */
 /*                                                                       */
 /*                        Author: Didier Brassard                        */
 /*                                                                       */
 /*                               Version 1                               */
 /*                               25MAY2022                               */
 /*                                                                       */
 /* NOTE: This code assumes that <01_CCHS2015_Data_preparation.sas> and   */
 /* <03A_CCHS2015_DescriptiveU> were executed beforehand.                 */
 /*                                                                       */
 /*************************************************************************/

/* Objective: dietary intake data measured using the 24-h recall are 
	affected by (within-indiviudal) random errors which can cause under-
	or over-estimation of intakes in a given distribution. The National 
	Cancer Institute Method can be used to mitigate random errors when
	interested in multivariate distribution. */

 /*************************************************************************/
 /*                                                                       */
 /*             General set-up: location of files, libraries              */
 /*                                                                       */
 /*************************************************************************/

/* TO DO: indicate location of project folder */
	%let path = C:/Users/DIBRA22/Documents/hefi2019/ ;	

/* AUTOMATIC: Create a temporary folder to save large files */
	options dlcreatedir;
	libname temp "&path./Example_SAS_cchs/Temp/";

/* AUTOMATIC: Folder tree for the NCI multivariate method*/
	libname baselib "&path./Example_SAS_cchs/NCI/MCMC/Model/";
	libname reslib "&path./Example_SAS_cchs/NCI/MCMC/Results/";
	
/* AUTOMATIC: load the HEFI-2019 scoring algorithm (update path if macro is stored elsewhere) */
	%include "&path./SAS/hefi2019.scoring.macro.sas" ;
	
/* AUTOMATIC load macro needed to apply the NCI method */
	/* percentiles*/ %include "&path./SAS/NCI/percentiles_survey.macro.v1.1.sas";

	/* Available at: https://prevention.cancer.gov/research-groups/biometry/measurement-error-impact/software-measurement-error/several-regularly-consumed-or-0 */

 /*************************************************************************/
 /*                                                                       */
 /*           Format output of <multivar_distrib> for analysis            */
 /*                                                                       */
 /*************************************************************************/

	data temp.mc_t_distrib_out0;
		set baselib.mc_t_distrib_out0 ;
	
	* rename variable names to avoid errors;
		array generic_name(*) mc_t1 - mc_t15;
		array actual_name (*) wg pfpb otherbevs milk vf rg pfab otherfoods water sfa mufa pufa freesugars sodium energy;
		
		/* note: episodic variables are always before daily variables. Otherwise, the variables
			order is the same as written in the <std_cov_boxcox24hr_conday_minamt> macro */
	
		do i=1 to dim(generic_name);
			actual_name(i) = generic_name(i);
		end;
		
		drop i mc_t1 - mc_t15;
	
	* divide the weight variable by the number of pseudo-individuals ;
	
		weight_nw_sumw_div = weight_nw_sumw / 100 ;
	
		/* note: the denominator must be consistent with the value of the
			<set_number_monte_carlo_rand_obs> parameter in the <multivar_distrib> macro */
	
	run;


 /*************************************************************************/
 /*                                                                       */
 /*           Calculate HEFI-2019 scores based on usual intakes           */
 /*                                                                       */
 /*************************************************************************/

/* scores are calculated using the HEFI-2019 scoring algorithm */

	%HEFI2019(indata          = temp.mc_t_distrib_out0,
			  vegfruits       = vf,
			  wholegrfoods	  = wg,
			  nonwholegrfoods = rg,
			  profoodsanimal  = pfab,
			  profoodsplant	  = pfpb,
			  otherfoods	  = otherfoods,
			  waterhealthybev = water,
			  unsweetmilk	  = milk,
			  unsweetplantbevpro = 0, /* not reported in the current sample */
			  otherbeverages  = otherbevs,
			  mufat			  = mufa,
			  pufat			  = pufa,
			  satfat		  = sfa,
			  freesugars	  = freesugars,
			  sodium		  = sodium,
			  energy		  = energy,
			  outdata         = temp.mc_t_distrib_out0
		  	);

 /*************************************************************************/
 /*                                                                       */
 /*       Examine correlation between scores based on usual intakes       */
 /*                                                                       */
 /*************************************************************************/

	title1 "Pearson correlation among HEFI-2019 component scores in teenagers and children from the CCHS 2015 - Nutrition";
	proc corr data=temp.mc_t_distrib_out0 alpha ;
	var HEFI2019C1_VF HEFI2019C2_WHOLEGR HEFI2019C3_GRRATIO HEFI2019C4_PROFOODS 
		HEFI2019C5_PLANTPRO HEFI2019C6_BEVERAGES HEFI2019C7_FATTYACID HEFI2019C8_SFAT 
		HEFI2019C9_FREESUGARS HEFI2019C10_SODIUM;
	weight weight_nw_sumw_div; 
	run;
	title1;
	
	title1 "Pearson correlation between the total HEFI-2019 score and energy intake in teenagers and children from the CCHS 2015 - Nutrition";
	proc corr data=temp.mc_t_distrib_out0 ;
	var HEFI2019_TOTAL_SCORE ;
	with energy; 
	weight weight_nw_sumw_div; 
	run;
	title1;	

 /*************************************************************************/
 /*                                                                       */
 /*      Examine distribution of total score based on usual intakes       */
 /*                                                                       */
 /*************************************************************************/

/* Distribution of scores based on usual intakes can be obtained using the
	<Percentiles_Survey> macro */
	
	%percentiles_Survey(data      = temp.mc_t_distrib_out0,
	                    byvar     = ,
	                    var       = HEFI2019_TOTAL_SCORE,
	                    weight    = weight_nw_sumw_div,
	                    cutpoints = 42.9, /* Look at Pr(X<43), the average score in CCHS 2015 - Nutrition */
	                    print     = n,
	                    ntitle    = 1
	                    );

/* Save output data for further analysis */
	data reslib.distrib_total_w0;
	retain replicate drig group;
		set _percentiles;
	length group $ 8 ;
	replicate =0;
	group     = "all";
	drig      = .;
	run;

	data reslib.distrib_total_t0;
	retain replicate drig group;
		set _percentiles2;
	length group $ 8 ;
	replicate =0;
	group     = "all";
	drig      = .;
	* add percentile values; 
	if index(Statistic,'Pctile')>0 then 
		p = input( compress(Statistic,,'a'), 10.) ;
	else p=.;
	run;

/* Show some values of the distribution */

	title1 "Distribution of total HEFI-2019 score based on usual intakes in teenagers and children from the CCHS 2015 - Nutrition";
	
	proc print data=reslib.distrib_total_w0;
	format Mean StdDev Pctile1 Pctile25 Pctile50 Pctile75 Pctile95 Pctile99 NUM4.1;
	id replicate group;
	var Mean StdDev Pctile1 Pctile25 Pctile50 Pctile75 Pctile95 Pctile99 ;
	run;
	
	title1;

/* Plot the distribution */
	title1 "Distribution of total HEFI-2019 score based on usual intakes in teenagers and children from the CCHS 2015 - Nutrition";
	
	proc sgplot data=reslib.distrib_total_t0;
	where not missing(p) ; /* to show use only percentile*/
	histogram value;
	density value ;
	density value / type=kernel;
	xaxis label="Total HEFI-2019 score (/80), pts" ;
	run;	
	
	proc sgplot data=reslib.distrib_total_t0;
	where not missing(p) ; /* to show use only percentile*/
	series x=value y=p ;
	xaxis label="Total HEFI-2019 score (/80), pts" ;
	yaxis label="Percentiles";
	run;

	title1;

 /*************************************************************************/
 /*                                                                       */
 /*  Examine distribution of total score based on usual intakes, by DRI   */
 /*                                                                       */
 /*************************************************************************/

/* Distribution of scores based on usual intakes can be obtained using the
	<Percentiles_Survey> macro */
	
	%percentiles_Survey(data      = temp.mc_t_distrib_out0,
	                    byvar     = drig,
	                    var       = HEFI2019_TOTAL_SCORE,
	                    weight    = weight_nw_sumw_div,
	                    cutpoints = 42.9, /* Look at Pr(X<43), the average score in CCHS 2015 - Nutrition */
	                    print     = n,
	                    ntitle    = 1
	                    );

/* Save output data for further analysis */
	data reslib.distrib_total_drig_w0;
	retain replicate drig group;
		set _percentiles;
	length group $ 8 ;
	replicate =0;
	group     = "drig";
	run;

	data reslib.distrib_total_drig_t0;
	retain replicate drig group;
		set _percentiles2;
	length group $ 8 ;
	replicate =0;
	group     = "drig";
	* add percentile values; 
	if index(Statistic,'Pctile')>0 then 
		p = input( compress(Statistic,,'a'), 10.) ;
	else p=.;
	run;

/* Define format for the DRI groups */
	proc format;
	value drigfmt 
	2 = "2 TO 3 YEARS"
	3 = "4 TO 8 YEARS"
	4 = "MALE, 9 TO 13 YEARS"
	5 = "FEMALE, 9 TO 13 YEARS"
	6 = "MALE, 14 TO 18 YEARS"
	7 = "FEMALE, 14 TO 18 YEARS";
	run;

/* Show some values of the distribution */
	title1 "Distribution of total HEFI-2019 score based on usual intakes in teenagers and children from the CCHS 2015 - Nutrition";
	
	proc print data=reslib.distrib_total_drig_w0;
	format drig drigfmt. Mean StdDev Pctile1 Pctile25 Pctile50 Pctile75 Pctile95 Pctile99 NUM4.1;
	id replicate drig;
	var Mean StdDev Pctile1 Pctile25 Pctile50 Pctile75 Pctile95 Pctile99 ;
	run;
	
	title1;

/* Plot the distributions */
	title1 "Distribution of total HEFI-2019 score based on usual intakes in teenagers and children from the CCHS 2015 - Nutrition";
	title2 "by DRI groups";
	proc sgplot data=reslib.distrib_total_drig_t0;
	format drig drigfmt. ;
	where not missing(p) ; /* to show use only percentile*/
	series x=value y=p / group=drig;
	xaxis label="Total HEFI-2019 score (/80), pts" ;
	yaxis label="Percentiles";
	run;
	title1;
	title2;

 /*************************************************************************/
 /*                                                                       */
 /*    Examine distribution of component scores based on usual intakes    */
 /*                                                                       */
 /*************************************************************************/

/* To facilitate analysis, a short macro is used to loop through hefi components */
	%macro loop(list);
		%do ith=1 %to %sysfunc(countw(&list)) ;
		
		%let component = %scan(&list, &ith);
	
		%percentiles_Survey(data      = temp.mc_t_distrib_out0,
		                    byvar     = ,
		                    var       = &component ,
		                    weight    = weight_nw_sumw_div,
		                    cutpoints = ,
		                    print     = n,
		                    ntitle    = 1
		                    );
	
		data c_w&ith.;
			set _percentiles;
		length varname $ 32;
		varname = "&component";
		id = &ith ;
		run;
		
		data c_t&ith.;
			set _percentiles2;
		length varname $ 32;
		varname = "&component";
		id = &ith ;
		run;
		
		%end; /* end of loop */
	
	%mend loop;

/* Now, call the loop using the names of all HEFI-2019 components */
	%loop(list=HEFI2019C1_VF HEFI2019C2_WHOLEGR HEFI2019C3_GRRATIO HEFI2019C4_PROFOODS 
		HEFI2019C5_PLANTPRO HEFI2019C6_BEVERAGES HEFI2019C7_FATTYACID HEFI2019C8_SFAT 
		HEFI2019C9_FREESUGARS HEFI2019C10_SODIUM) ;
		
/* Save output data for further analysis */
	data reslib.distrib_sub_w0;
	retain replicate drig group id varname;
		set c_w1-c_w10;
	length group $ 8 ;
	replicate =0;
	drig      = .;
	group     = "all";
	run;

	data reslib.distrib_sub_t0;
	retain replicate drig group id varname;
		set c_t1-c_t10;
	length group $ 8 ;
	replicate =0;
	drig      = .;
	group     = "all";
	* add percentile values; 
	if index(Statistic,'Pctile')>0 then 
		p = input( compress(Statistic,,'a'), 10.) ;
	else p=.;
	run;

/* clean temporary data */
	proc datasets lib=work nolist nodetails; 
	delete c_w1-c_w10 c_t1-c_t10;
	run;
	

/* Show some values of the distribution */
	title1 "Distribution of HEFI-2019 component scores based on usual intakes in teenagers and children from the CCHS 2015 - Nutrition";
	
	proc print data=reslib.distrib_sub_w0;
	format Mean StdDev Pctile1 Pctile25 Pctile50 Pctile75 Pctile95 Pctile99 NUM4.1;
	id replicate id varname;
	var Mean StdDev Pctile1 Pctile25 Pctile50 Pctile75 Pctile95 Pctile99 ;
	run;
	
	title1;
	
/* Many more results can be obtained based on the <mc_t_distrib_out> data! */

	/* note: confidence interval for all data above can be estimated
		by looping code <03A_CCHS2015_DescriptiveU.sas> through the 500 bootstrap
		weight replicates, and calculating the standard deviation of the resulting
		sampling distribution (i.e., `bootstrap` standard errors) for each statistics .*/

 /*************************************************************************/
 /*                                                                       */
 /*                      End of results demontration                      */
 /*                                                                       */
 /*************************************************************************/
