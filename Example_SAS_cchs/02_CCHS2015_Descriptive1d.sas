/* !! PLEASE NOTE THAT THIS EXAMPLE CODE IS PROVIDED `AS IS`, WITHOUT 
	WARRANTY OF ANY KIND, EXPRESS OR IMPLIED. THIS CODE INVOLVES THE SOLE
	RESPONSABILITY OF THE AUTHOR AND IS NOT ENDORSED BY STATISTICS CANADA
	OR HEALTH CANADA. !! */

 /*************************************************************************/
 /*                                                                       */
 /*                     CCHS 2015 - Nutrition (PUMF)                      */
 /*                                                                       */
 /*         Descriptive statistics based on a single 24-h recall          */
 /*                                                                       */
 /*                        Author: Didier Brassard                        */
 /*                                                                       */
 /*                               Version 1                               */
 /*                               13APR2022                               */
 /*                                                                       */
 /* NOTE: This code assumes that <01_CCHS2015_Data_preparation.sas>       */
 /* was executed beforehand.                                              */
 /*                                                                       */
 /*************************************************************************/

 /*************************************************************************/
 /*                                                                       */
 /*             General set-up: location of files, libraries              */
 /*                                                                       */
 /*************************************************************************/

/* TO DO: indicate location of project folder */
	%let path = C:/Users/DIBRA22/Documents/hefi2019/ ;
	
/* TO DO: indicate location of cchs files (could be the same as <path>) */
	%let pathCCHS = C:/Users/DIBRA22/Documents/CCHS_Nutrition_2015_PUMF/;
	
	/* note: data pertaining to the 2015 Canadian Community Health Survey -
		Nutrition (Public Use Microdata Files) are available upon request to
		Statistics Canada online: https://www150.statcan.gc.ca/n1/en/catalogue/82M0024X2018001 */	

/* AUTOMATIC: library to retrieve files in project folder */
	libname proj "&path./Example_SAS_cchs/";
	libname fmt "&path./Example_SAS_cchs/Fmtdata/";
	
/* AUTOMATIC: load the HEFI-2019 scoring algorithm (update path if macro is stored elsewhere) */
	%include "&path./SAS/hefi2019.scoring.macro.sas" ;
	
 /*************************************************************************/
 /*                                                                       */
 /*            Estimate mean scores for the first 24-h recall             */
 /*                       (population ratio method)                       */
 /*                                                                       */
 /*                    See Freedman et al. J Nutr 2008                    */
 /*               https://pubmed.ncbi.nlm.nih.gov/18716176/               */
 /*                                                                       */
 /*************************************************************************/

/* 1) Combine dietary intakes of the first recall with sociodemo data */
	data intake_and_sociodeom(drop=nonzero_energy);
		merge fmt.intake_per24hr(in=a)
			  fmt.hs_nci(in=b keep=adm_rno suppid wts_p drig );
		by adm_rno suppid;
	* remove 24-h recall with 0 energy intake ;
		if energy=0 then delete ;
	* keep adm_rno present in both data + single 24-h recall only ;
		if (a and b) and suppid=1 then output;
	run;
	
	/* note: sample size of respondents 2y+ for first 24-h recall = 20,103 */

/* 2) Calculate mean intakes of HEFI-2019 dietary constituents, weighted using sampling weights */
	options fmtsearch=(fmt.hsfmt);
	proc means data=intake_and_sociodeom mean;
	format drig drigfmt.; 
	class drig ;
	var vf wg rg pfab pfpb otherfoods water milk plantbev otherbevs sfa mufa pufa freesugars sodium energy ;
	weight WTS_P;
	output out=popratio mean=;
	run;

	/* some formatting */
	data popratio(drop=_TYPE_ rename=(_FREQ_=numsubject));
		set popratio;
	*indicate the `all ` row ;
	if _TYPE_ = 0 then drig=0; 
	label _FREQ_ ="n, unweighted";
	run;

	/* note: sampling weights are applied, but variance ignored at this point */

/* 3) Apply the HEFI-2019 socring algorithm (see macro SAS file for details)*/

	%HEFI2019(indata             = popratio,
			  vegfruits          = vf,
			  wholegrfoods       = wg,
			  nonwholegrfoods    = rg,
			  profoodsanimal     = pfab,
			  profoodsplant      = pfpb,
			  otherfoods         = otherfoods,
			  waterhealthybev    = water,
			  unsweetmilk        = milk,
			  unsweetplantbevpro = plantbev,
			  otherbeverages     = otherbevs,
			  mufat              = mufa,
			  pufat              = pufa,
			  satfat             = sfa,
			  freesugars         = freesugars,
			  sodium             = sodium,
			  energy             = energy,
			  outdata            = popratio_scored
		  	);
		  
/* 4) Show results */
	proc print data=popratio_scored ;
	title1 "HEFI-2019 scores estimated using the population ratio method, by DRI group";
	title2 "first 24-h recall, CCHS 2015 - Nutrition (n=20,103)";
	id drig numsubject ;
	var hefi2019: ;
	run;
	title1;
	title2;

	/* note: based on Freedman et al. J Nutr 2008, the pop. ratio method scores are
		slightly biased vs. scores based on measurement-error corrected data. However,
		the bias is less than the mean score based on the average of all respondent scores. */

 /*************************************************************************/
 /*                                                                       */
 /*           Estimate population ratio scores and differences            */
 /*                                                                       */
 /*************************************************************************/

	/* goal: estimate mean scores in subgroups and calculate score differences */

/* 1) Combine dietary intakes of the first recall with sociodemo data */
	data intake_and_sociodeom(drop=nonzero_energy);
		merge fmt.intake_per24hr(in=a)
			  fmt.hs_nci(in=b keep=adm_rno suppid wts_p sex age smoking );
		by adm_rno suppid;
	* recode smoking as yes or no ;
		if smoking in (1 2) then smoking=1;
	* keep only adults 19 y +  ;
		if age < 19 then delete;
	* remove 24-h recall with 0 energy intake ;
		if energy=0 then delete ;
	* keep adm_rno present in both data + singel 24-h recall only ;
		if (a and b) and suppid=1 then output;
	run;

/* 2) Calculate mean intakes of HEFI-2019 dietary constituents, weighted using sampling weights */
	proc means data=intake_and_sociodeom mean;
	class smoking ;
	var vf wg rg pfab pfpb otherfoods water milk plantbev otherbevs sfa mufa pufa freesugars sodium energy ;
	weight wts_p ;
	output out=popratio_smk mean=;
	run;

	/* some formatting */
	data popratio_smk(drop=_TYPE_ rename=(_FREQ_=numsubject));
		set popratio_smk;
	*delete the `all ` row ;
	if _TYPE_ = 0 then delete;
	label _FREQ_ ="n, unweighted";
	run;

	/* note: sampling weights are applied, but variance ignored at this point */

/* 3) Apply the HEFI-2019 socring algorithm (see macro SAS file for details)*/

	%HEFI2019(indata             = popratio_smk,
			  vegfruits          = vf,
			  wholegrfoods       = wg,
			  nonwholegrfoods    = rg,
			  profoodsanimal     = pfab,
			  profoodsplant      = pfpb,
			  otherfoods         = otherfoods,
			  waterhealthybev    = water,
			  unsweetmilk        = milk,
			  unsweetplantbevpro = plantbev,
			  otherbeverages     = otherbevs,
			  mufat              = mufa,
			  pufat              = pufa,
			  satfat             = sfa,
			  freesugars         = freesugars,
			  sodium             = sodium,
			  energy             = energy,
			  outdata            = popratio_smk_scored
		  	);

/* 4) Calculate score differences according to smoking status */

	proc transpose data=popratio_smk_scored out=popratio_smk_scored_w prefix=smk_ ;
	var hefi2019: ;
	run;
	
	data smk_diff;
		set popratio_smk_scored_w(rename=(smk_1=non_smokers smk_2=smokers));
	Difference = smokers - non_smokers ;
	label _NAME_ = "Variable name" _LABEL_ = "Component name";
	rename _NAME_ = name _LABEL_ = label;
	run;
	
/* 5) show results */
	proc print data=smk_diff label;
	title1 "Mean HEFI-2019 scores estimated using the population ratio method, by smoking status";
	title2 "First 24-h recall, CCHS 2015 - Nutrition (n=20,103)";
	format non_smokers smokers difference 4.1;
	id name label ;
	var non_smokers smokers difference;
	label smokers = "Smokers"
		  non_smokers = "Non-smokers"
		  difference = "Difference"
		  ;
	run;
	title1;	
	title1;
	

	/* note: confidence interval for mean and mean differences can be estimated
		by looping step 2 to 4 through the 500 bootstrap weight replicates,
		and calculating the standard deviation of the resulting sampling distribution 
		(i.e., `bootstrap` standard errors). */
	
