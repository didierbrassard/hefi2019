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
 /* NOTE: This code assumes that <01_CCHS2015_Data_preparation.sas>       */
 /* was executed beforehand.                                              */
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
	
/* TO DO: update file location to save trace plots output from the <multivar_mcmc> macro.
	See parameter <traceplots_method2_file_pdf> around lines 972 and 1861 */
	
/* TO DO: indicate location of cchs files (could be the same as <path>) */
	%let pathCCHS = C:/Users/DIBRA22/Documents/CCHS_Nutrition_2015_PUMF/;
	
	/* note: data pertaining to the 2015 Canadian Community Health Survey -
		Nutrition (Public Use Microdata Files) are available upon request to
		Statistics Canada online: https://www150.statcan.gc.ca/n1/en/catalogue/82M0024X2018001 */	

/* AUTOMATIC: library to retrieve files in project folder */
	options dlcreatedir;
	libname proj "&path./Example_SAS_cchs/";
	libname fmtdata "&path./Example_SAS_cchs/Fmtdata/";

/* AUTOMATIC: Folder tree suggestion to save output for the NCI multivariate method*/
	options dlcreatedir;
	libname nci "&path./Example_SAS_cchs/NCI/" ;
	libname mcmc "&path./Example_SAS_cchs/NCI/MCMC/";
	libname baselib "&path./Example_SAS_cchs/NCI/MCMC/Model/";
	libname chain "&path./Example_SAS_cchs/NCI/MCMC/Markovchains/";
	libname reslib "&path./Example_SAS_cchs/NCI/MCMC/Results/";
	
/* AUTOMATIC: load macros needed to apply the NCI multivariate method */
	/* boxcox svy */ %include "&path./SAS/NCI/boxcox_survey.macro.v1.2.sas";
	/* std boxcox */ %include "&path./SAS/NCI/std_cov_boxcox24hr_conday_minamt_macro_v2.0.sas";
	/* multi MCMC */ %include "&path./SAS/NCI/multivar_mcmc_macro_v2.1.sas";
	/* multi dist */ %include "&path./SAS/NCI/multivar_distrib_macro_v2.1.sas";

	/* Available at: https://prevention.cancer.gov/research-groups/biometry/measurement-error-impact/software-measurement-error/several-regularly-consumed-or-0 */
	
 /*************************************************************************/
 /*                                                                       */
 /*         Prepare an input data set for the multivariate method         */
 /*                                                                       */
 /*************************************************************************/

/* 1) Combine dietary intakes of the first recall with sociodemo data */
	data intake_and_sociodeom(drop=nonzero_energy);
		merge fmtdata.intake_per24hr(in=a)
			  fmtdata.hs_nci(in=b keep=adm_rno suppid wts_p r24_weekend drig sex);
		by adm_rno suppid;
	* remove 24-h recall with 0 energy intake ;
		if energy=0 then delete ;
	* keep adm_rno present in both data + single 24-h recall only ;
		if (a and b) then output;
	run;
	
	/* note: sample size of respondents 2y+ for both 24-h recall = 27,529 */
	
/* 2) Re-code covariates as dummy variables, select final sample and output data*/

	data preNCI;
		set intake_and_sociodeom (rename=(r24_weekend=_r24_weekend));
	* Dummy variable for sequence of 24-h recalls ;
	if suppid=2 then seq2=1;
		else seq2=0;
	
	* Dummy variable for weekend 24-h recalls ;
	if _r24_weekend = 1 then r24_weekend=1;
		else r24_weekend =0;
	drop _r24_weekend;
	
	* Dummy variable for age categories, according to Dietary Reference Intake (DRI) groups;
	if drig = 2 then agec_2to3=1;
		else agec_2to3=0;
	if drig = 3 then agec_4to8=1;
		else agec_4to8=0;
	if drig in (4 5) then agec_9to13=1;
		else agec_9to13=0;
	if drig in (6 7) then agec_14to18=1;
		else agec_14to18=0;
	
	* Change variable name for sampling weights to be consistent with bootstrap weights ;
	rename wts_p = bsw0 ;
		
		/* note: sampling weights variable name is <WTS_M> in CCHS master files */
		
	* For this example, filter to keep only respondents between 2 and 18 y of age;
	if drig > 7 then delete;
	run;
	
/* 3) Confirm age categories recoding */

	options fmtsearch=(fmtdata.hsfmt);
	
	proc freq data=preNCI;
	format drig DRIgFMT. ;
	table drig drig * agec: ;
	run;

/* 4) Look at intakes on a given day */
	proc means data=preNCI n mean min p25 p50 p75 max stackods maxdec=1;
	class suppid; 
	var wg pfpb otherbevs milk vf rg pfab otherfoods water sfa mufa pufa freesugars sodium energy ;
	weight bsw0 ;
	run;

 /*************************************************************************/
 /*                                                                       */
 /*            NCI Multivariate method based on MCMC algorithm            */
 /*                                                                       */
 /*************************************************************************/

/* In this example, a stratified modeling approach (by sex) is used along
	with consideration of covariates (age groups). This combination allows
	the measurement error model parameters to change according to strata (i.e., sex).
	Because sex and age groups were considered in the measurement error model,
	one can then output distribution of usual intakes for these subgroups. */

 /*************************************************************************/
 /*              Stratum selection based on STRATA=sex (1/2)              */
 /*************************************************************************/

* Output data of participants (adm_rno) in stratum 1 only ;
	data _tempstratum1 ;
	set preNCI ;
	if (sex = 1) then output;
	run;
	
	proc sort ;
	by adm_rno ;
	run;
	
 /*************************************************************************/
 /*                   Find best Box-Cox transformations                   */
 /*************************************************************************/
	
	* Output data for the first 24-h dietary recall completed;
		proc sort data = _tempstratum1 nodupkey out=inboxcox;
		by adm_rno ;
		run;
	
	* Make sure the macro variable <best_lambda> is available outside <boxcox_survey>;
		%global best_lambda ;
	
	/********************************************************************/
	/*   Dietary constituents #1 - wg: min. amount and lambda           */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum1 min noprint;
		var wg ;
		where wg > 0;
		output out=_min1(keep=minamount) min=minamount ;
		run;
		
		data _min1;
		set _min1;
		minamount=minamount/2;
		tran_paramindex=1;
		length varname $ 32;
		varname="wg";
		run;
		
		data _temp1;
		set inboxcox(keep=adm_rno bsw0 wg seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (wg > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp1, 
		 subject = adm_rno, 
		 var     = wg,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 ); 
	options notes source;
	
	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x1;
		length varname $ 32;
		tran_paramindex=1;
		varname="wg";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.2;
		run;
	
	
		/* note: these steps are repeated for all dietary constituents of the HEFI-2019 */
	
	/********************************************************************/
	/*   Dietary constituents #2 - pfpb: min. amount and lambda         */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum1 min noprint;
		var pfpb ;
		where pfpb > 0;
		output out=_min2(keep=minamount) min=minamount ;
		run;
		
		data _min2;
		set _min2;
		minamount=minamount/2;
		tran_paramindex=2;
		length varname $ 32;
		varname="pfpb";
		run;
		
		data _temp2;
		set inboxcox(keep=adm_rno bsw0 pfpb seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (pfpb > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp2, 
		 subject = adm_rno, 
		 var     = pfpb,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 ); 
	options notes source;
	
	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x2;
		length varname $ 32;
		tran_paramindex=2;
		varname="pfpb";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.2;
		run;
	
	
	/********************************************************************/
	/*   Dietary constituents #3 - otherbevs: min. amount and lambda    */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum1 min noprint;
		var otherbevs ;
		where otherbevs > 0;
		output out=_min3(keep=minamount) min=minamount ;
		run;
		
		data _min3;
		set _min3;
		minamount=minamount/2;
		tran_paramindex=3;
		length varname $ 32;
		varname="otherbevs";
		run;
		
		data _temp3;
		set inboxcox(keep=adm_rno bsw0 otherbevs seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (otherbevs > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp3, 
		 subject = adm_rno, 
		 var     = otherbevs,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 );
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x3;
		length varname $ 32;
		tran_paramindex=3;
		varname="otherbevs";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.13;
		run;
	
	
	/********************************************************************/
	/*   Dietary constituents #4 - milk: min. amount and lambda         */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum1 min noprint;
		var milk ;
		where milk > 0;
		output out=_min4(keep=minamount) min=minamount ;
		run;
		
		data _min4;
		set _min4;
		minamount=minamount/2;
		tran_paramindex=4;
		length varname $ 32;
		varname="milk";
		run;
		
		data _temp4;
		set inboxcox(keep=adm_rno bsw0 milk seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (milk > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp4, 
		 subject = adm_rno, 
		 var     = milk,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 ); 
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x4;
		length varname $ 32;
		tran_paramindex=4;
		varname="milk";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.41;
		run;
	
	
	/********************************************************************/
	/*   Dietary constituents #5 - vf: min. amount and lambda           */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum1 min noprint;
		var vf ;
		where vf > 0;
		output out=_min5(keep=minamount) min=minamount ;
		run;
		
		data _min5;
		set _min5;
		minamount=minamount/2;
		tran_paramindex=5;
		length varname $ 32;
		varname="vf";
		run;
		
		data _temp5;
		set inboxcox(keep=adm_rno bsw0 vf seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (vf > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp5, 
		 subject = adm_rno, 
		 var     = vf,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 ); 
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x5;
		length varname $ 32;
		tran_paramindex=5;
		varname="vf";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.37;
		run;
	
	/********************************************************************/
	/*   Dietary constituents #6 - rg: min. amount and lambda           */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum1 min noprint;
		var rg ;
		where rg > 0;
		output out=_min6(keep=minamount) min=minamount ;
		run;
		
		data _min6;
		set _min6;
		minamount=minamount/2;
		tran_paramindex=6;
		length varname $ 32;
		varname="rg";
		run;
		
		data _temp6;
		set inboxcox(keep=adm_rno bsw0 rg seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (rg > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp6, 
		 subject = adm_rno, 
		 var     = rg,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 );
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x6;
		length varname $ 32;
		tran_paramindex=6;
		varname="rg";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.36;
		run;
	
	
	/********************************************************************/
	/*   Dietary constituents #7 - pfab: min. amount and lambda         */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum1 min noprint;
		var pfab ;
		where pfab > 0;
		output out=_min7(keep=minamount) min=minamount ;
		run;
		
		data _min7;
		set _min7;
		minamount=minamount/2;
		tran_paramindex=7;
		length varname $ 32;
		varname="pfab";
		run;
		
		data _temp7;
		set inboxcox(keep=adm_rno bsw0 pfab seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (pfab > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp7, 
		 subject = adm_rno, 
		 var     = pfab,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 );
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x7;
		length varname $ 32;
		tran_paramindex=7;
		varname="pfab";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		*tran_lambda=0.26;
		run;
	
	
	/********************************************************************/
	/*   Dietary constituents #8 - otherfoods: min. amount and lambda   */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum1 min noprint;
		var otherfoods ;
		where otherfoods > 0;
		output out=_min8(keep=minamount) min=minamount ;
		run;
		
		data _min8;
		set _min8;
		minamount=minamount/2;
		tran_paramindex=8;
		length varname $ 32;
		varname="otherfoods";
		run;
		
		data _temp8;
		set inboxcox(keep=adm_rno bsw0 otherfoods seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (otherfoods > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp8, 
		 subject = adm_rno, 
		 var     = otherfoods,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 );
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x8;
		length varname $ 32;
		tran_paramindex=8;
		varname="otherfoods";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.26;
		run;
	
	
	/********************************************************************/
	/* Dietary constituents #9 - water: min. amount and lambda          */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum1 min noprint;
		var water ;
		where water > 0;
		output out=_min9(keep=minamount) min=minamount ;
		run;
		
		data _min9;
		set _min9;
		minamount=minamount/2;
		tran_paramindex=9;
		length varname $ 32;
		varname="water";
		run;
		
		data _temp9;
		set inboxcox(keep=adm_rno bsw0 water seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (water > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp9, 
		 subject = adm_rno, 
		 var     = water,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 ); 
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x9;
		length varname $ 32;
		tran_paramindex=9;
		varname="water";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.39;
		run;
	
	/********************************************************************/
	/*   Dietary constituents #10 - sfa: min. amount and lambda         */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum1 min noprint;
		var sfa ;
		where sfa > 0;
		output out=_min10(keep=minamount) min=minamount ;
		run;
		
		data _min10;
		set _min10;
		minamount=minamount/2;
		tran_paramindex=10;
		length varname $ 32;
		varname="sfa";
		run;
		
		data _temp10;
		set inboxcox(keep=adm_rno bsw0 sfa seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (sfa > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp10, 
		 subject = adm_rno, 
		 var     = sfa,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 ); 
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x10;
		length varname $ 32;
		tran_paramindex=10;
		varname="sfa";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.25;
		run;
	
	
	/********************************************************************/
	/*   Dietary constituents #11 - mufa: min. amount and lambda        */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum1 min noprint;
		var mufa ;
		where mufa > 0;
		output out=_min11(keep=minamount) min=minamount ;
		run;
		
		data _min11;
		set _min11;
		minamount=minamount/2;
		tran_paramindex=11;
		length varname $ 32;
		varname="mufa";
		run;
		
		data _temp11;
		set inboxcox(keep=adm_rno bsw0 mufa seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (mufa > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp11, 
		 subject = adm_rno, 
		 var     = mufa,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 ); 
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x11;
		length varname $ 32;
		tran_paramindex=11;
		varname="mufa";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.24;
		run;
	
	
	/********************************************************************/
	/*   Dietary constituents #12 - pufa: min. amount and lambda        */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum1 min noprint;
		var pufa ;
		where pufa > 0;
		output out=_min12(keep=minamount) min=minamount ;
		run;
		
		data _min12;
		set _min12;
		minamount=minamount/2;
		tran_paramindex=12;
		length varname $ 32;
		varname="pufa";
		run;
		
		data _temp12;
		set inboxcox(keep=adm_rno bsw0 pufa seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (pufa > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp12, 
		 subject = adm_rno, 
		 var     = pufa,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 ); 
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x12;
		length varname $ 32;
		tran_paramindex=12;
		varname="pufa";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.16;
		run;
	
	
	/********************************************************************/
	/*   Dietary constituents #13 - freesugars: min. amount and lambda  */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum1 min noprint;
		var freesugars ;
		where freesugars > 0;
		output out=_min13(keep=minamount) min=minamount ;
		run;
		
		data _min13;
		set _min13;
		minamount=minamount/2;
		tran_paramindex=13;
		length varname $ 32;
		varname="freesugars";
		run;
		
		data _temp13;
		set inboxcox(keep=adm_rno bsw0 freesugars seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (freesugars > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp13, 
		 subject = adm_rno, 
		 var     = freesugars,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 ); 
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x13;
		length varname $ 32;
		tran_paramindex=13;
		varname="freesugars";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.32;
		run;
	
	
	/********************************************************************/
	/*   Dietary constituents #14 - sodium: min. amount and lambda      */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum1 min noprint;
		var sodium ;
		where sodium > 0;
		output out=_min14(keep=minamount) min=minamount ;
		run;
		
		data _min14;
		set _min14;
		minamount=minamount/2;
		tran_paramindex=14;
		length varname $ 32;
		varname="sodium";
		run;
		
		data _temp14;
		set inboxcox(keep=adm_rno bsw0 sodium seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (sodium > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp14, 
		 subject = adm_rno, 
		 var     = sodium,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 );
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x14;
		length varname $ 32;
		tran_paramindex=14;
		varname="sodium";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.27;
		run;
	
	
	/********************************************************************/
	/*   Dietary constituents #15 - energy: min. amount and lambda      */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum1 min noprint;
		var energy ;
		where energy > 0;
		output out=_min15(keep=minamount) min=minamount ;
		run;
		
		data _min15;
		set _min15;
		minamount=minamount/2;
		tran_paramindex=15;
		length varname $ 32;
		varname="energy";
		run;
		
		data _temp15;
		set inboxcox(keep=adm_rno bsw0 energy seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (energy > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp15, 
		 subject = adm_rno, 
		 var     = energy,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 ); 
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x15;
		length varname $ 32;
		tran_paramindex=15;
		varname="energy";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.19;
		run;
	
	/*****************************************************************/
	/*         Append data sets of all dietary constituents          */
	/*****************************************************************/
	
		data work.xlambdas_s1_0;
		set _x1-_x15;
		run;
		
		data work.xminamount_s1_0;
		set _min1-_min15;
		label minamount= "Min. non zero amount divided by 2";
		run;

 /*************************************************************************/
 /*      Standardize data according to lambda and min. amount values      */
 /*************************************************************************/

	/* note: for reproducibility purpose, the variable order in <rec24hr_epis_vars> 
		and <rec24hr_daily_vars> must be the same to obtain the same results.
		Failure to keep the order could result in small differences across
		replications due to the inherently random features of MCMC methods. */

* Call the <std_cov_boxcox24hr_conday_minamt> macro to standardize data; 
	%std_cov_boxcox24hr_conday_minamt(
	 data                       = _tempstratum1, 
	 prestand_continuous_covars =  , 
	 rec24hr_epis_vars          = wg pfpb otherbevs milk, 
	 rec24hr_daily_vars         = vf rg pfab otherfoods water sfa mufa pufa freesugars sodium energy, 
	 boxcox_tran_lambda_data    = xlambdas_s1_0, 
	 minamount_data             = xminamount_s1_0, 
	 print                      = y, 
	 titles                     = 3 );
	
	/* note: the data <stdcov_stdbc24hr_conday_out> is now ready as input
		for the <multivar_mcmc> macro, including Box-Cox standardised variables.
		The following global macro variables are also created and can be used 
		in the <multivar_mcmc> macro. For demonstration purpose, names are written
		without refering to these macro variables. */

	%put # &=conday_var_list; /* variable list for <conday_epis_vars>*/
	%put # &=stdbc_epis_var_list; /* variable list for <gst_rec24hr_epis_vars> */
	%put # &=stdbc_daily_var_list; /* variable list for <gst_rec24hr_daily_vars> */
	%put # &=std_continuous_covar_list;	/* nothing here since no continuous covariates used in <prestand_continuous_covars> */

 /*************************************************************************/
 /*             Fit the multivariate measurement error model              */
 /*************************************************************************/

* Call the <multivar_mcmc> macro to fit the measurement error model; 
  title1 "Fit Multivariate Measurement Error Model Using MCMC with 24-Hour Recall as Main Instrument"; 
	%multivar_mcmc(
	 data                        = stdcov_stdbc24hr_conday_out, 
	 subject                     = adm_rno, 
	 weight_var                  = bsw0 , 
	 repeat                      = suppid, 
	 conday_epis_vars            = conday_wg  conday_pfpb  conday_otherbevs  conday_milk, 
	 gst_rec24hr_epis_vars       = stdbc_wg  stdbc_pfpb  stdbc_otherbevs  stdbc_milk, 
	 gst_rec24hr_daily_vars      = stdbc_vf  stdbc_rg  stdbc_pfab  stdbc_otherfoods  stdbc_water  stdbc_sfa  stdbc_mufa  
	                               stdbc_pufa stdbc_freesugars stdbc_sodium  stdbc_energy, 
	 covars_epis_prob            = constant1 seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18   , 
	 covars_epis_amt             = constant1 seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18   , 
	 covars_daily_amt            = constant1 seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18   , 
	 set_seed_mcmc               = 42941, 
	 set_number_mcmc_iterations  = 7000, 
	 set_number_burn_iterations  = 3000,
	 set_thin                    = 10, 
	 prior_sigmau_mean_data      = , 
	 sigmau_constant             = , 
	 gen_inverse                 = y, 
	 print                       = y, 
	 titles                      = 1, 
	 std_print_store             = y, 
	 notes_print                 = y, 
	 out_lib                     = baselib, 
	 out_store_label             = mcmc_s1_rep0, 
	 out_save_label_max5char     = s10, 
	 set_number_saved_out_data   = , 
	 save_mcmc_u_out_data        = y, 
	 set_number_post_mcmc_u_out  = , 
	 traceplots_method1_gpath    = , 
	 traceplots_method2_file_pdf = %str(C:/Users/DIBRA22/Documents/hefi2019/Example_SAS_cchs/NCI/MCMC/Markovchains/trace_rep0_s1.pdf) , 
	 optional_iml_store_data     = backtran_out, 
	 optional_iml_store_names    = constant1 seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 
	                               tran_paramindex tran_lambda tran_center tran_scale minamount );

* Save lambdas (Box-Cox transformation lambda values) and minimum amount data ;
	data baselib.backtran_out0_s1;
	retain replicate sex;
	set work.backtran_out;
		* indicate replicate number, current stratum value, variable name;
		replicate = 0;
		sex = 1;
		length varname $ 32 ;
		if _N_=1 then varname = "wg" ;
		if _N_=2 then varname = "pfpb" ;
		if _N_=3 then varname = "otherbevs" ;
		if _N_=4 then varname = "milk" ;
		if _N_=5 then varname = "vf" ;
		if _N_=6 then varname = "rg" ;
		if _N_=7 then varname = "pfab" ;
		if _N_=8 then varname = "otherfoods" ;
		if _N_=9 then varname = "water" ;
		if _N_=10 then varname = "sfa" ;
		if _N_=11 then varname = "mufa" ;
		if _N_=12 then varname = "pufa" ;
		if _N_=13 then varname = "freesugars" ;
		if _N_=14 then varname = "sodium" ;
		if _N_=15 then varname = "energy" ;
	run;

 /*************************************************************************/
 /*               Perform simulation of pseudo-individuals                */
 /*************************************************************************/

* Prepare an input data for the <optional_input_data> option in <multivar_distrib>;
	proc sort data=_tempstratum1 nodupkey out=optional_input_data(keep=adm_rno sex 
			bsw0 agec_4to8 agec_9to13 agec_14to18 sex drig);
		by adm_rno;
	run;

 * Call the <multivar_distrib> macro to simulate usual intakes for pseudo-individuals; 
	%multivar_distrib(
	 multivar_mcmc_out_lib           = baselib ,  
	 multivar_mcmc_out_store_label   = mcmc_s1_rep0, 
	 t_weightavg_covariates_list1    = constant1 constant0 constant0 agec_4to8 agec_9to13 agec_14to18 ,  
	 t_weightavg_covariates_list2    = constant1 constant0 constant1 agec_4to8 agec_9to13 agec_14to18 , 
	 set_value_for_weight_cov_list1  = 4, 
	 set_value_for_weight_cov_list2  = 3, 
	 optional_input_data             = optional_input_data , 
	 optional_input_data_var_list    = , 
	 optional_input_mcmc_u_out_data  = , 
	 additional_output_var_list      = sex drig , 
	 additional_output_subject_var   = adm_rno , 
	 output_mcmc_weight_var          = y , 
	 set_seed_distrib                = 89009890, 
	 set_number_monte_carlo_rand_obs = 100,  
	 print                           = y 
	 ); 

 /*************************************************************************/
 /*               Save the output and clean temporary data                */
 /*************************************************************************/

* Save the Monte Carlo simulation data for current stratum;
	data baselib.mc_t_distrib_out0_s1;
		set mc_t_distrib_out;
	run;

* delete temporary data sets ;
	proc datasets lib=work nolist nodetails;
	delete mc_t_distrib_out optional_input_data stdcov_stdbc24hr_conday_out xlambdas_: 
	xminamount_: inboxcox backtran_out _temp: _min: _x: _pctile _plotdata _lambda;
	run;

	/* note: the steps above are repeated for the next stratum */
	
 /*************************************************************************/
 /*              Stratum selection based on STRATA=sex (2/2)              */
 /*************************************************************************/

* Output data of participants (adm_rno) in stratum 1 only ;
	data _tempstratum2 ;
	set preNCI ;
	if (sex = 2) then output;
	run;
	
	proc sort ;
	by adm_rno ;
	run;
	
 /*************************************************************************/
 /*                   Find best Box-Cox transformations                   */
 /*************************************************************************/
	
	* Output data for the first 24-h dietary recall completed;
		proc sort data = _tempstratum2 nodupkey out=inboxcox;
		by adm_rno ;
		run;
	
	* Make sure the macro variable <best_lambda> is available outside <boxcox_survey>;
		%global best_lambda ;
	
	/********************************************************************/
	/*   Dietary constituents #1 - wg: min. amount and lambda           */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum2 min noprint;
		var wg ;
		where wg > 0;
		output out=_min1(keep=minamount) min=minamount ;
		run;
		
		data _min1;
		set _min1;
		minamount=minamount/2;
		tran_paramindex=1;
		length varname $ 32;
		varname="wg";
		run;
		
		data _temp1;
		set inboxcox(keep=adm_rno bsw0 wg seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (wg > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp1, 
		 subject = adm_rno, 
		 var     = wg,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 ); 
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x1;
		length varname $ 32;
		tran_paramindex=1;
		varname="wg";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.15;
		run;
	
		/* note: these steps are repeated for all dietary constituents of the HEFI-2019 */
	
	/********************************************************************/
	/*   Dietary constituents #2 - pfpb: min. amount and lambda         */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum2 min noprint;
		var pfpb ;
		where pfpb > 0;
		output out=_min2(keep=minamount) min=minamount ;
		run;
		
		data _min2;
		set _min2;
		minamount=minamount/2;
		tran_paramindex=2;
		length varname $ 32;
		varname="pfpb";
		run;
		
		data _temp2;
		set inboxcox(keep=adm_rno bsw0 pfpb seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (pfpb > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp2, 
		 subject = adm_rno, 
		 var     = pfpb,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 ); 
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x2;
		length varname $ 32;
		tran_paramindex=2;
		varname="pfpb";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.18;
		run;
	
	
	/********************************************************************/
	/*   Dietary constituents #3 - otherbevs: min. amount and lambda    */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum2 min noprint;
		var otherbevs ;
		where otherbevs > 0;
		output out=_min3(keep=minamount) min=minamount ;
		run;
		
		data _min3;
		set _min3;
		minamount=minamount/2;
		tran_paramindex=3;
		length varname $ 32;
		varname="otherbevs";
		run;
		
		data _temp3;
		set inboxcox(keep=adm_rno bsw0 otherbevs seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (otherbevs > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp3, 
		 subject = adm_rno, 
		 var     = otherbevs,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 ); 
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x3;
		length varname $ 32;
		tran_paramindex=3;
		varname="otherbevs";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.45;
		run;
	
	
	/********************************************************************/
	/*   Dietary constituents #4 - milk: min. amount and lambda         */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum2 min noprint;
		var milk ;
		where milk > 0;
		output out=_min4(keep=minamount) min=minamount ;
		run;
		
		data _min4;
		set _min4;
		minamount=minamount/2;
		tran_paramindex=4;
		length varname $ 32;
		varname="milk";
		run;
		
		data _temp4;
		set inboxcox(keep=adm_rno bsw0 milk seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (milk > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp4, 
		 subject = adm_rno, 
		 var     = milk,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 ); 
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x4;
		length varname $ 32;
		tran_paramindex=4;
		varname="milk";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.48;
		run;
	
	
	/********************************************************************/
	/*   Dietary constituents #5 - vf: min. amount and lambda           */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum2 min noprint;
		var vf ;
		where vf > 0;
		output out=_min5(keep=minamount) min=minamount ;
		run;
		
		data _min5;
		set _min5;
		minamount=minamount/2;
		tran_paramindex=5;
		length varname $ 32;
		varname="vf";
		run;
		
		data _temp5;
		set inboxcox(keep=adm_rno bsw0 vf seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (vf > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp5, 
		 subject = adm_rno, 
		 var     = vf,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 ); 
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x5;
		length varname $ 32;
		tran_paramindex=5;
		varname="vf";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.45;
		run;
	
	/********************************************************************/
	/*   Dietary constituents #6 - rg: min. amount and lambda           */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum2 min noprint;
		var rg ;
		where rg > 0;
		output out=_min6(keep=minamount) min=minamount ;
		run;
		
		data _min6;
		set _min6;
		minamount=minamount/2;
		tran_paramindex=6;
		length varname $ 32;
		varname="rg";
		run;
		
		data _temp6;
		set inboxcox(keep=adm_rno bsw0 rg seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (rg > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp6, 
		 subject = adm_rno, 
		 var     = rg,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 );
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x6;
		length varname $ 32;
		tran_paramindex=6;
		varname="rg";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.26;
		run;
	
	
	/********************************************************************/
	/*   Dietary constituents #7 - pfab: min. amount and lambda         */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum2 min noprint;
		var pfab ;
		where pfab > 0;
		output out=_min7(keep=minamount) min=minamount ;
		run;
		
		data _min7;
		set _min7;
		minamount=minamount/2;
		tran_paramindex=7;
		length varname $ 32;
		varname="pfab";
		run;
		
		data _temp7;
		set inboxcox(keep=adm_rno bsw0 pfab seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (pfab > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp7, 
		 subject = adm_rno, 
		 var     = pfab,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 );
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x7;
		length varname $ 32;
		tran_paramindex=7;
		varname="pfab";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.33;
		run;
	
	
	/********************************************************************/
	/*   Dietary constituents #8 - otherfoods: min. amount and lambda   */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum2 min noprint;
		var otherfoods ;
		where otherfoods > 0;
		output out=_min8(keep=minamount) min=minamount ;
		run;
		
		data _min8;
		set _min8;
		minamount=minamount/2;
		tran_paramindex=8;
		length varname $ 32;
		varname="otherfoods";
		run;
		
		data _temp8;
		set inboxcox(keep=adm_rno bsw0 otherfoods seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (otherfoods > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp8, 
		 subject = adm_rno, 
		 var     = otherfoods,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 );
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x8;
		length varname $ 32;
		tran_paramindex=8;
		varname="otherfoods";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.29;
		run;
	
	
	/********************************************************************/
	/* Dietary constituents #9 - water: min. amount and lambda          */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum2 min noprint;
		var water ;
		where water > 0;
		output out=_min9(keep=minamount) min=minamount ;
		run;
		
		data _min9;
		set _min9;
		minamount=minamount/2;
		tran_paramindex=9;
		length varname $ 32;
		varname="water";
		run;
		
		data _temp9;
		set inboxcox(keep=adm_rno bsw0 water seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (water > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp9, 
		 subject = adm_rno, 
		 var     = water,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 ); 
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x9;
		length varname $ 32;
		tran_paramindex=9;
		varname="water";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.38;
		run;
	
	/********************************************************************/
	/*   Dietary constituents #10 - sfa: min. amount and lambda         */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum2 min noprint;
		var sfa ;
		where sfa > 0;
		output out=_min10(keep=minamount) min=minamount ;
		run;
		
		data _min10;
		set _min10;
		minamount=minamount/2;
		tran_paramindex=10;
		length varname $ 32;
		varname="sfa";
		run;
		
		data _temp10;
		set inboxcox(keep=adm_rno bsw0 sfa seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (sfa > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp10, 
		 subject = adm_rno, 
		 var     = sfa,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 ); 
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x10;
		length varname $ 32;
		tran_paramindex=10;
		varname="sfa";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.11;
		run;
	
	
	/********************************************************************/
	/*   Dietary constituents #11 - mufa: min. amount and lambda        */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum2 min noprint;
		var mufa ;
		where mufa > 0;
		output out=_min11(keep=minamount) min=minamount ;
		run;
		
		data _min11;
		set _min11;
		minamount=minamount/2;
		tran_paramindex=11;
		length varname $ 32;
		varname="mufa";
		run;
		
		data _temp11;
		set inboxcox(keep=adm_rno bsw0 mufa seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (mufa > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp11, 
		 subject = adm_rno, 
		 var     = mufa,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 ); 
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x11;
		length varname $ 32;
		tran_paramindex=11;
		varname="mufa";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.2;
		run;
	
	
	/********************************************************************/
	/*   Dietary constituents #12 - pufa: min. amount and lambda        */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum2 min noprint;
		var pufa ;
		where pufa > 0;
		output out=_min12(keep=minamount) min=minamount ;
		run;
		
		data _min12;
		set _min12;
		minamount=minamount/2;
		tran_paramindex=12;
		length varname $ 32;
		varname="pufa";
		run;
		
		data _temp12;
		set inboxcox(keep=adm_rno bsw0 pufa seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (pufa > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp12, 
		 subject = adm_rno, 
		 var     = pufa,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 ); 
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x12;
		length varname $ 32;
		tran_paramindex=12;
		varname="pufa";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.13;
		run;
	
	
	/********************************************************************/
	/*   Dietary constituents #13 - freesugars: min. amount and lambda  */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum2 min noprint;
		var freesugars ;
		where freesugars > 0;
		output out=_min13(keep=minamount) min=minamount ;
		run;
		
		data _min13;
		set _min13;
		minamount=minamount/2;
		tran_paramindex=13;
		length varname $ 32;
		varname="freesugars";
		run;
		
		data _temp13;
		set inboxcox(keep=adm_rno bsw0 freesugars seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (freesugars > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp13, 
		 subject = adm_rno, 
		 var     = freesugars,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 ); 
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x13;
		length varname $ 32;
		tran_paramindex=13;
		varname="freesugars";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.38;
		run;
	
	
	/********************************************************************/
	/*   Dietary constituents #14 - sodium: min. amount and lambda      */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum2 min noprint;
		var sodium ;
		where sodium > 0;
		output out=_min14(keep=minamount) min=minamount ;
		run;
		
		data _min14;
		set _min14;
		minamount=minamount/2;
		tran_paramindex=14;
		length varname $ 32;
		varname="sodium";
		run;
		
		data _temp14;
		set inboxcox(keep=adm_rno bsw0 sodium seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (sodium > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp14, 
		 subject = adm_rno, 
		 var     = sodium,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 );
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x14;
		length varname $ 32;
		tran_paramindex=14;
		varname="sodium";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.28;
		run;
	
	
	/********************************************************************/
	/*   Dietary constituents #15 - energy: min. amount and lambda      */
	/********************************************************************/
	
	* 1) Get minimum non-zero consumption amount ;
		proc means data=_tempstratum2 min noprint;
		var energy ;
		where energy > 0;
		output out=_min15(keep=minamount) min=minamount ;
		run;
		
		data _min15;
		set _min15;
		minamount=minamount/2;
		tran_paramindex=15;
		length varname $ 32;
		varname="energy";
		run;
		
		data _temp15;
		set inboxcox(keep=adm_rno bsw0 energy seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 );
		if (energy > 0) then output;
		run;
	
	* 2) Call the <boxcox_survey> macro to find best normal transformation ;
	options nonotes nosource;
		%boxcox_survey(
		 data    = _temp15, 
		 subject = adm_rno, 
		 var     = energy,  
		 covars  = seq2 r24_weekend  agec_4to8 agec_9to13 agec_14to18 ,  
		 weight  = bsw0,  
		 print   = N, 
		 plot    = N, 
		 ntitle  = 4 ); 
	options notes source;

	*3) Save <best_lambda> macro variable from <boxcox_survey> for current dietary constituent ;
		data _x15;
		length varname $ 32;
		tran_paramindex=15;
		varname="energy";
		tran_lambda=&best_lambda ;
		* The value of best_lambda was ... ;
		* tran_lambda=0.18;
		run;
	
	/*****************************************************************/
	/*         Append data sets of all dietary constituents          */
	/*****************************************************************/
	
		data work.xlambdas_s2_0;
		set _x1-_x15;
		run;
		
		data work.xminamount_s2_0;
		set _min1-_min15;
		label minamount= "Min. non zero amount divided by 2";
		run;

 /********************************************************************/
 /*   Standardize data according to lambda and min. amount values    */
 /********************************************************************/

* Call the <std_cov_boxcox24hr_conday_minamt> macro to standardize data and continuous covariates, if any; 
	%std_cov_boxcox24hr_conday_minamt(
	 data                       = _tempstratum2, 
	 prestand_continuous_covars =  , 
	 rec24hr_epis_vars          = wg pfpb otherbevs milk, 
	 rec24hr_daily_vars         = vf rg pfab otherfoods water sfa mufa pufa freesugars sodium energy, 
	 boxcox_tran_lambda_data    = xlambdas_s2_0, 
	 minamount_data             = xminamount_s2_0, 
	 print                      = y, 
	 titles                     = 3 ); 

	/* note: for reproducibility purpose, the variable order in <rec24hr_epis_vars> 
		and <rec24hr_daily_vars> must be the same to obtain the same results.
		Failure to keep the order could result in small differences across
		replications due to the inherently random features of MCMC methods. */

 /*************************************************************************/
 /*             Fit the multivariate measurement error model              */
 /*************************************************************************/
 
 * Call the <multivar_mcmc> macro to fit the measurement error model; 
  title1 "Fit Multivariate Measurement Error Model Using MCMC with 24-Hour Recall as Main Instrument"; 
	%multivar_mcmc(
	 data                        = stdcov_stdbc24hr_conday_out, 
	 subject                     = adm_rno, 
	 weight_var                  = bsw0 , 
	 repeat                      = suppid, 
	 conday_epis_vars            = conday_wg  conday_pfpb  conday_otherbevs  conday_milk, 
	 gst_rec24hr_epis_vars       = stdbc_wg  stdbc_pfpb  stdbc_otherbevs  stdbc_milk, 
	 gst_rec24hr_daily_vars      = stdbc_vf  stdbc_rg  stdbc_pfab  stdbc_otherfoods  stdbc_water  stdbc_sfa  stdbc_mufa  
	stdbc_pufa  stdbc_freesugars  stdbc_sodium  stdbc_energy, 
	 covars_epis_prob            = constant1 seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18   , 
	 covars_epis_amt             = constant1 seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18   , 
	 covars_daily_amt            = constant1 seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18   , 
	 set_seed_mcmc               = 42941, 
	 set_number_mcmc_iterations  = 7000,
	 set_number_burn_iterations  = 3000,
	 set_thin                    = 10,
	 prior_sigmau_mean_data      = , 
	 sigmau_constant             = , 
	 gen_inverse                 = y, 
	 print                       = y, 
	 titles                      = 1, 
	 std_print_store             = y, 
	 notes_print                 = y, 
	 out_lib                     = baselib, 
	 out_store_label             = mcmc_s2_rep0, 
	 out_save_label_max5char     = s20, 
	 set_number_saved_out_data   = , 
	 save_mcmc_u_out_data        = y, 
	 set_number_post_mcmc_u_out  = , 
	 traceplots_method1_gpath    = , 
	 traceplots_method2_file_pdf = %str(C:/Users/DIBRA22/Documents/hefi2019/Example_SAS_cchs/NCI/MCMC/Markovchains/trace_rep0_s2.pdf), 
	 optional_iml_store_data     = backtran_out, 
	 optional_iml_store_names    = constant1 seq2 r24_weekend agec_4to8 agec_9to13 agec_14to18 
	 							   tran_paramindex tran_lambda tran_center tran_scale minamount 
	 );


* Save lambdas (Box-Cox transformation lambda values) and minimum amount data ;
	data baselib.backtran_out0_s2;
	retain replicate sex;
	set work.backtran_out;
		* indicate replicate number, current stratum value, variable name;
		replicate = 0;
		sex = 2;
		length varname $ 32 ;
		if _N_=1 then varname = "wg" ;
		if _N_=2 then varname = "pfpb" ;
		if _N_=3 then varname = "otherbevs" ;
		if _N_=4 then varname = "milk" ;
		if _N_=5 then varname = "vf" ;
		if _N_=6 then varname = "rg" ;
		if _N_=7 then varname = "pfab" ;
		if _N_=8 then varname = "otherfoods" ;
		if _N_=9 then varname = "water" ;
		if _N_=10 then varname = "sfa" ;
		if _N_=11 then varname = "mufa" ;
		if _N_=12 then varname = "pufa" ;
		if _N_=13 then varname = "freesugars" ;
		if _N_=14 then varname = "sodium" ;
		if _N_=15 then varname = "energy" ;
	run;

 /*************************************************************************/
 /*               Perform simulation of pseudo-individuals                */
 /*************************************************************************/

* Prepare an input data for the <optional_input_data> option in <multivar_distrib>;
	proc sort data=_tempstratum2 nodupkey out=optional_input_data(keep= adm_rno sex bsw0 agec_4to8 
		agec_9to13 agec_14to18 sex drig );
	by adm_rno ;
	run;

* Call the <multivar_distrib> macro to simulate usual intakes for pseudo-individuals; 
	%multivar_distrib(
	 multivar_mcmc_out_lib           = baselib ,  
	 multivar_mcmc_out_store_label   = mcmc_s2_rep0, 
	 t_weightavg_covariates_list1    = constant1 constant0 constant0 agec_4to8 agec_9to13 agec_14to18 ,  
	 t_weightavg_covariates_list2    = constant1 constant0 constant1 agec_4to8 agec_9to13 agec_14to18 , 
	 set_value_for_weight_cov_list1  = 4, 
	 set_value_for_weight_cov_list2  = 3, 
	 optional_input_data             = optional_input_data , 
	 optional_input_data_var_list    = , 
	 optional_input_mcmc_u_out_data  = , 
	 additional_output_var_list      = sex drig  , 
	 additional_output_subject_var   = adm_rno , 
	 output_mcmc_weight_var          = y  , 
	 set_seed_distrib                = 89009890, 
	 set_number_monte_carlo_rand_obs = 100,  
	 print                           = y 
	 ); 

 /*************************************************************************/
 /*               Save the output and clean temporary data                */
 /*************************************************************************/

* Save the Monte Carlo simulation data for current stratum;
	data baselib.mc_t_distrib_out0_s2;
	set mc_t_distrib_out;
	run;

* delete temporary data sets ;
	proc datasets lib=work nolist nodetails;
	delete mc_t_distrib_out optional_input_data stdcov_stdbc24hr_conday_out xlambdas_: 
	xminamount_: inboxcox backtran_out _temp: _min: _x: _pctile _plotdata _lambda;
	run;

 /*************************************************************************/
 /*                                                                       */
 /*     Combine stratum-specific data (i.e., Monte Carlo simulations)     */
 /*                                                                       */
 /*************************************************************************/

* Append all stratum-specific <mc_t_distrib_out> data ;
	data baselib.mc_t_distrib_out0 ;
		set baselib.mc_t_distrib_out0_s1-baselib.mc_t_distrib_out0_s2;
	run;

* Append all stratum-specific <backtran_out> data ;
	data baselib.backtran_out0;
		set baselib.backtran_out0_s1-baselib.backtran_out0_s2;
	run;

* Delete temporary data;
	proc datasets lib=baselib nolist nodetails;
		delete backtran_out0_s1-backtran_out0_s2 ;
	run;
	
	proc datasets lib=work nolist nodetails;
		delete _tempstratum1-_tempstratum2 CKNEGATIVE24HR LAMBDA_MINAMT_MULTIREC NOTNEG24HR_DATA 
			MCMC_SUBJ1RECDATA trace_afterburn_thin_data sigmae_paneldata sigmau_paneldata beta_paneldata
			PARAMVARLABELS1REC _SGSORT_ ;
	run;

	* Output data from <multivar_mcmc> can also be deleted now to save disk space;
	/*proc datasets lib=baselib nolist nodetails ;
	delete multivar_mcmc_samples_u_outs10 multivar_mcmc_samples_u_outs20 ;
	run;*/
	
 /*************************************************************************/
 /*                                                                       */
 /*                    End of measurement error model                     */
 /*                                                                       */
 /*************************************************************************/
