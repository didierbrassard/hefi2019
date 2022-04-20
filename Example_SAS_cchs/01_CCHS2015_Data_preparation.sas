/* !! PLEASE NOTE THAT THIS EXAMPLE CODE IS PROVIDED `AS IS`, WITHOUT 
	WARRANTY OF ANY KIND, EXPRESS OR IMPLIED. THIS CODE INVOLVES THE SOLE
	RESPONSABILITY OF THE AUTHOR AND IS NOT ENDORSED BY STATISTICS CANADA
	OR HEALTH CANADA. !! */

 /*************************************************************************/
 /*                                                                       */
 /*                     CCHS 2015 - Nutrition (PUMF)                      */
 /*                                                                       */
 /*   Prepare a data suitable to apply the HEFI-2019 scoring algorithm    */
 /*                                                                       */
 /*                        Author: Didier Brassard                        */
 /*                                                                       */
 /*                              Version 1.1                              */
 /*                               12APR2022                               */
 /*                                                                       */
 /* This code uses the following data:                                    */
 /* - CCHS 2015 - Nutrition (PUMF)                                        */
 /* - Free sugars from foods                                              */
 /* - HEFI-2019 classification of foods                                   */
 /*                                                                       */
 /*************************************************************************/

 /*************************************************************************/
 /*                                                                       */
 /*             General set-up: location of files, libraries              */
 /*                                                                       */
 /*************************************************************************/

/* TO DO: indicate location of repository */
	%let path = C:/Users/DIBRA22/Documents/hefi2019/ ;

/* TO DO: indicate location of cchs files (could be the same as <path>) */
	%let pathCCHS = C:/Users/DIBRA22/Documents/CCHS_Nutrition_2015_PUMF/;
	
	/* note: data pertaining to the 2015 Canadian Community Health Survey -
		Nutrition (Public Use Microdata Files) are available upon request to
		Statistics Canada online: https://www150.statcan.gc.ca/n1/en/catalogue/82M0024X2018001 */	

/* AUTOMATIC: library to output files in project folder */
	options dlcreatedir; * option to create folder if not already done;
	libname proj "&path./Example_SAS_cchs/";
	libname fmt "&path./Example_SAS_cchs/Fmtdata/";
	libname temp "&path./Example_SAS_cchs/Temp/"; * temporary (large) files are saved there;
	
 /*************************************************************************/
 /*                                                                       */
 /*            Import and format HEFI-2019 classification data            */
 /*                                                                       */
 /*************************************************************************/

	/* note: assuming the file is at the <path> location,
		in a folder named <Example_SAS_cchs> */
		
/* 1) import file using proc import */
	options validvarname=v7;
	PROC IMPORT DATAFILE="&path./Example_SAS_cchs/HEFI2019_foodcat_2022-03.xlsx"
		DBMS=XLSX
		OUT=WORK.hefifoodcat replace;
		GETNAMES=YES;
		sheet="CAT";
	RUN;

/* 2) prepare classification to merge with other cchs files */
	data fmt.hefifoodcat(rename=(FoodRecipeCode = FID_CDE FDC_DEN = FDC_DEN_foodcat));
		set hefifoodcat;
	* format the RA_g variable as numerical ;
		if index(RA_g,"NA")>0 then grams_for_1RA = .;
		else grams_for_1RA = input(RA_g,10.);
	* recode hefi2019 foot categories for easier handling ;
		if HEFI2019Cat = "vegfruits" then hefi2019subgrp = 1;
		else if HEFI2019Cat = "wholegrfoods" then hefi2019subgrp = 2;
		else if HEFI2019Cat = "nonwholegrfoods" then hefi2019subgrp = 3;
		else if HEFI2019Cat = "profoodsanimal" then hefi2019subgrp = 4;
		else if HEFI2019Cat = "profoodsplant" then hefi2019subgrp = 5;
		else if HEFI2019Cat = "otherfoods" then hefi2019subgrp = 6;
		else if HEFI2019Cat = "waterhealthybev" then hefi2019subgrp = 7;
		else if HEFI2019Cat = "unsweetmilk" then hefi2019subgrp = 8;
		else if HEFI2019Cat = "unsweetplantbevpro" then hefi2019subgrp = 9;
		else if HEFI2019Cat = "otherbeverages" then hefi2019subgrp = 10;
		else  hefi2019subgrp = 99 ;
	* further classify one food with missing value for RA but can be considered in beverages nonetheless;
		if FoodRecipeCode = 5628 then hefi2019subgrp = 10 ;
	keep FoodRecipeCode FDC_DEN grams_for_1RA  hefi2019subgrp;
	run;
	
	proc sort data=fmt.hefifoodcat;
		by FID_CDE;
	run;
	
	/* make labels for hefi2019subgrp */
		proc format;
			value hefi2019subgrpfmt 
			1='1-Vegetables and fruits'
			2='2-Whole-grain foods'
			3='3-Non-whole grain foods'
			4='4-Protein foods, animal-based'
			5='5-Protein foods, plant-based'
			6='6-Other foods'
			7='7-Water and other healthy beverages'
			8='8-Milk, unsweetened'
			9='9-Plant-based beverages with proteins, unsweetened'
			10='10-Other beverages'
			99 = '99-Not considered (e.g., herbs, spices, fats, oils)'
			;
		run;
	
	/* Overview of all classified foods */
	proc freq data=fmt.hefifoodcat;
	title1 "Distribution of HEFI-2019 food categories among selected CNF items";
	format hefi2019subgrp hefi2019subgrpfmt.;
	table hefi2019subgrp;
	run;
	title1;

 /*************************************************************************/
 /*                                                                       */
 /*                  Import and format free sugars data                   */
 /*                                                                       */
 /*************************************************************************/

	/* note: data on free sugars is available at
		https://www.mdpi.com/article/10.3390/nu13051471/s1 (Rana et al. Nutrients 2021) */

	/* note: assuming the file is at the <path> location,
		in a folder named <Example_SAS_cchs> */

/* 1) import file using proc import */	
	options validvarname=v7;
	PROC IMPORT DATAFILE="&path./Example_SAS_cchs/nutrients-1173784-supplementary.xlsx"
		DBMS=XLSX
		OUT=WORK.freesugars replace;
		GETNAMES=YES;
	RUN;
	
/* 2) prepare free sugars data to merge with other cchs files */
	data fmt.freesugars(rename=(Name_English=FDC_DEN_freesugars));
	retain FID_CDE freesugars_per_g ;
		set freesugars(where=(not missing(Code)));
	
	* ensure code is numerical;
	FID_CDE = input(compress(Code,,'dk'),10.);
	
	* change to free sugars per 1 (one) gram ;
	freesugars_per_g = Estimate_free_sugar_amount__g_10 /100 ;
	keep FID_CDE freesugars_per_g Name_English ;
	run;

	proc sort data=fmt.freesugars;
		by FID_CDE;
	run;
	
 /*************************************************************************/
 /*                                                                       */
 /*   Import and format total nutrient intakes and sociodemo data [HS]    */
 /*                                                                       */
 /*************************************************************************/

	/* note: data pertaining to the 2015 Canadian Community Health Survey -
		Nutrition (Public Use Microdata Files) are available upon request to
		Statistics Canada online: https://www150.statcan.gc.ca/n1/en/catalogue/82M0024X2018001 */

	/* note: assuming the directory tree of CCHS 2015 - NUTRITION (PUMF) data
		is the default one. */

/* 1) Import HS_NCI data, save to fmt folder */
	data FMT.HS_NCI (rename=(ADMFW=r24_weekend ADMDD=r24_day ADM_MOI=r24_month R24DCNT=r24_nfoods
							GEO_PRV=province DHH_SEX=sex DHH_AGE=age DHHDDRI=drig EDUG21=education 
							FSDDWTG=foodwgt FSDDEKC=energy FSDDSOD=sodium 
							FSDDFAS=sfa FSDDFAM=mufa FSDDFAP=pufa
							));
	retain ADM_RNO SUPPID WTS_P ;
	%let datafid = "&pathCCHS./Data_Donnee/HS_NCI.txt";
	%include "&pathCCHS./SAS_SPSS/SAS/HS_NCI_i.sas";
	%include "&pathCCHS./SAS_SPSS/SAS/HS_NCI_lbe.sas";
	
	* recode missing values;
		* education level;
			if EDUG21 = 9 then EDUG21 =.;
	
	* recode smoking;
		if SMK_202 = 3 then smoking=0;
			else if SMK_202 = 2 then smoking=1;
				else if SMK_202 = 1 then smoking=2;
					else smoking=.;
			
			label smoking = "Type of smoker";
	
	* keep only respondents aged 2 years and older (i.e., target population of Canada`s food guide);
		if DHH_AGE lt 2 then delete;
		
	keep /* id */ ADM_RNO WTS_P
		 /* 24hr */ suppid ADMDD ADM_MOI R24DCNT
		 /*characteristics */ GEO_PRV DHH_AGE DHH_SEX DHHDDRI EDUG21 SMOKING ADMFW
		 /* dietary intakes*/ FSDDWTG FSDDEKC FSDDSOD FSDDFAT FSDDFAS FSDDFAM FSDDFAP;
	run;
	
	/* note: additional characteristics needed from the HS file would need to be added here */
	
	proc sort data=FMT.HS_NCI;
		by ADM_RNO SUPPID;
	run;

	/* format characteristics in main file */
	PROC FORMAT lib=fmt.hsfmt;
		value SexFMT
			1 = "Male"
			2 = "Female"
		;
		value DRIgFMT 
			0 = "All Canadians, 2 y and older"
			2 = "2 TO 3 YEARS"
			3 = "4 TO 8 YEARS"
			4 = "MALE, 9 TO 13 YEARS"
			5 = "FEMALE, 9 TO 13 YEARS"
			6 = "MALE, 14 TO 18 YEARS"
			7 = "FEMALE, 14 TO 18 YEARS"
			8 =	"MALE, 19 TO 30 YEARS"
			9 =	"FEMALE, 19 TO 30 YEARS"
			10 = "MALE, 31 TO 50 YEARS"
			11 = "FEMALE, 31 TO 50 YEARS"
			12 = "MALE, 51 TO 70 YEARS"
			13 = "FEMALE, 51 TO 70 YEARS"
			14 = "MALE, 71 OR OLDER"
			15 = "FEMALE, 71 OR OLDER"
		;
		value EducationFMT
			1 = "No diploma"
			2 = "Highschool"
			3 = "Trade school, some college"
			4 = "University"
		;
		value SmokingFMT
			0 = "Not smoking"
			1 = "Occasionally smoking"
			2 = "Daily smoking"
		;
		value R24_dayFMT
			1 = "Sunday"
			2 = "Monday"
			3 = "Tuesday"
			4 = "Wednesday"
			5 = "Thursday"
			6 = "Friday"
			7 = "Saturday"
		;
	run;

	/* check some freq for adequacy */
		options fmtsearch=(fmt.hsfmt);
		proc surveyfreq data=fmt.HS_NCI(where=(suppid=1));
		title1 "Some characteristics of CCHS 2015 - Nutrition respondents";
		format drig drigfmt. sex sexfmt. education educationfmt. smoking smokingfmt. ;
		table drig sex education smoking ;
		weight WTS_P;
		run;
		title1;

 /*************************************************************************/
 /*                                                                       */
 /*       Import and format food or ingredient level data [FID,FRL]       */
 /*                                                                       */
 /*************************************************************************/

/* 1) Import FID + FRL data, save to temp folder since these are large files*/
	data temp.fid;
		%let datafid = "&pathCCHS./Data_Donnee/FID.txt";
		%include "&pathCCHS./SAS_SPSS/SAS/FID_i.sas";
		%include "&pathCCHS./SAS_SPSS/SAS/FID_lbe.sas";
	run;
	
	data temp.frl;
		%let datafid = "&pathCCHS./Data_Donnee/FRL.txt";
		%include "&pathCCHS./SAS_SPSS/SAS/FRL_i.sas";
		%include "&pathCCHS./SAS_SPSS/SAS/FRL_lbe.sas";
	run;

/* 2) Append both data together and save to temp folder as FIDFRL */

	/* append both data */
		data temp.fidfrl;
			set temp.fid temp.frl;
		run;
		
	/* sort by subject (adm_rno), recall (suppid) and sequence of foods recall (seqid) */
		proc sort;
			by ADM_RNO SUPPID SEQID;
		run;

/* 3) import CFG data (useful to keep only `true` unique foods) and fdc data */

	/* import cfg-2007 data */
		data temp.cfg;
			%let datafid = "&pathCCHS./Data_Donnee/CFG.txt";
			%include "&pathCCHS./SAS_SPSS/SAS/CFG_i.sas";
			%include "&pathCCHS./SAS_SPSS/SAS/CFG_lbe.sas";
		run;

	/* sort by subject (adm_rno), recall (suppid) and sequence of foods recall (seqid) */
		proc sort;
			by ADM_RNO SUPPID SEQID;
		run;

/* 4) merge FIDFRL with CFG */

	/* note: fidfrl_cfg has 705,715 rows */
	
	data temp.fidfrl_cfg(drop=i);
	merge temp.fidfrl (in=a) temp.cfg(in=b);
		by ADM_RNO SUPPID SEQID;
	* recode missing values for nutrients;
	array nutrients(*) FID_WTG FID_EKC FID_CAR FID_FI FID_SUG FID_FAT FID_FAS FID_FAM FID_FAP FID_FAL FID_FAN FID_CHO FID_PRO FID_ALC FID_RAE FID_DMG FID_C FID_THI FID_RIB FID_NIA FID_B6 FID_B12 FID_FON FID_FOA FID_DFE FID_FOL FID_CAL FID_PHO FID_MAG FID_IRO FID_ZIN FID_SOD FID_POT FID_CAF FID_MOI FID_DHA FID_DPA FID_EPA FID_ODA ;
		do i=1 to dim(nutrients);
			if nutrients(i) gt 99999 then nutrients(i)=.;
		end;
	* only keep CFG food rows (exclude recipes which would double-count some foods);		
	if b then output; 
	run;
	
	proc sort data=temp.fidfrl_cfg;
		by FID_CDE;
	run;

/* 5) delete temporary files to save disk space */
	proc datasets lib=temp nolist nodetails;
		delete fid frl fidfrl cfg;
	run;

 /*************************************************************************/
 /*                                                                       */
 /*         Merge hefi2019 classification, free sugars and fidfrl         */
 /*                                                                       */
 /*************************************************************************/

	data fmt.cchs24hr_detailed ;
		merge temp.fidfrl_cfg(in=cchs keep=FID_CDE ADM_RNO SUPPID FID_WTG)
			  fmt.freesugars
			  fmt.hefifoodcat;
		by FID_CDE;
	* Calculate the number of reference amounts consumed;
		if (not missing(FID_WTG)) AND (not missing(grams_for_1RA)) then 
			ra_qty = FID_WTG / grams_for_1RA;
		else ra_qty =.;
	
	* Calculate intake of free sugars ;
		if (not missing(FID_WTG)) AND (not missing(freesugars_per_g)) then 
			freesugars = FID_WTG * freesugars_per_g ;
		else freesugars = .;
		
	* keep only foods reported in CCHS 2015 - Nutrition; 
	if cchs then output; 
	run;
	
	/* confirm reported intakes are classified */
	proc freq data=fmt.cchs24hr_detailed;
	title1 "HEFI-2019 categories distribution among reported dietary intakes in CCHS 2015 - Nutrition";
	footnote1 "Frequencies do not reflect unique respondent";
	format hefi2019subgrp hefi2019subgrpfmt.;
	table hefi2019subgrp;
	run;
	title1;
	footnote1;
	
 /*************************************************************************/
 /*                                                                       */
 /*           Calculate intake per respondent, recall, category           */
 /*                                                                       */
 /*************************************************************************/

/* 1) Calculate total food intake (RA) per respondent, recall and category */
	
	/* 1.1 sort data*/
	proc sort data=fmt.cchs24hr_detailed ;
	by ADM_RNO SUPPID hefi2019subgrp; 
	run;
	
	/* 1.2 Sum RA per participant, recall and hefi categories */
	proc means data=fmt.cchs24hr_detailed sum noprint ;
	by ADM_RNO SUPPID hefi2019subgrp;
	var ra_qty;  
	where hefi2019subgrp not in (. 7 8 9 10 99) ; * exclude rows not considered in foods;
	output out=food_sum sum=sum_ra_qty ; 
	run;
	
	/* 1.3 transpose summed intakes (tall->wide) */
	proc transpose data=food_sum out=food_sum_t prefix=hefi_; *define common prefix ;
	by ADM_RNO SUPPID ;
	var sum_ra_qty;
	id hefi2019subgrp;
	run;

	/* note: missing data indicates that a given food category was not reported */
	
	/* 1.4 format the `wide` (i.e. transposed) output */
	data food_sum_t(drop=i hefi_1-hefi_6);
	retain hefi_1 hefi_2 hefi_3 hefi_4 hefi_5 hefi_6 ;
		set food_sum_t;
	* change numbering to more explicit names and missing to zero;
	array hefivarsNo(*) hefi: ;
	array hefivarsDesc(*) vf wg rg pfab pfpb otherfoods  ;
	do i=1 to dim(hefivarsDesc);
		if missing(hefivarsNo(i)) then hefivarsDesc(i) = 0;
		else  hefivarsDesc(i) = hefivarsNo(i);
	end;
	run;
	
	/* 1.5 Confirm that final output for foods is consistent with expectations */
	title1 "Food sum overview";
	proc print data=food_sum_t(obs=5) ;run;
	
	proc freq data=food_sum_t;
	table SUPPID ;
	run;

	/* note: respondent that reported no foods would not be counted here */
	
	proc means data=food_sum_t n nmiss mean min max stackods;
	class suppid;
	var vf wg rg pfab pfpb otherfoods ;
	run;

	title1;

/* 2) Calculate total beverage intake (grams) per respondent, recall and category */
	
	/* 2.1 sort data*/
	proc sort data=fmt.cchs24hr_detailed ;
	by ADM_RNO SUPPID hefi2019subgrp; 
	run;
	
	/* 2.2 Sum RA per participant, recall and hefi categories */
	proc means data=fmt.cchs24hr_detailed sum noprint ;
	by ADM_RNO SUPPID hefi2019subgrp;
	var FID_WTG;  
	where hefi2019subgrp in (7 8 9 10) ; * exclude rows not considered in beverages;
	output out=bev_sum sum=sum_grams ; 
	run;
	
	/* 2.3 transpose summed intakes (tall->wide) */
	proc transpose data=bev_sum out=bev_sum_t prefix=hefi_; *define common prefix ;
	by ADM_RNO SUPPID ;
	var sum_grams;
	id hefi2019subgrp;
	run;

	/* note: missing data indicates that a given food category was not reported */
	
	/* 2.4 format the `wide` (i.e. transposed) output */
	data bev_sum_t(drop=i hefi_7-hefi_10);
	retain hefi_7 hefi_8 hefi_9 hefi_10 ;
		set bev_sum_t;
	* change numbering to more explicit names and missing to zero;
	array hefivarsNo(*) hefi: ;
	array hefivarsDesc(*) water milk plantbev otherbevs  ;
	do i=1 to dim(hefivarsDesc);
		if missing(hefivarsNo(i)) then hefivarsDesc(i) = 0;
		else  hefivarsDesc(i) = hefivarsNo(i);
	end;
	run;
	
	/* 2.5 Confirm that final output for foods is consistent with expectations */
	title1 "Beverages sum overview";
	proc print data=bev_sum_t(obs=5) ;run;
	
	proc freq data=bev_sum_t;
	table SUPPID ;
	run;

	/* note: respondent that reported no beverages would not be counted here */
	
	proc means data=bev_sum_t n nmiss mean min max stackods;
	class suppid; 
	var water milk plantbev otherbevs  ;
	run;

	title1;

/* 3) Calculate total free sugars (grams) per respondent, recall and category */

	/* 3.1 Sum grams per participant, recall */
	proc means data=fmt.cchs24hr_detailed sum noprint ;
	by ADM_RNO SUPPID ;
	var freesugars;
	output out=freesug_sum sum=freesugars; 
	run;

	/* 3.2 Confirm that final output for foods is consistent with expectations */
	title1 "Free sugars sum overview";
	proc print data=freesug_sum(obs=5) ;run;
	
	proc freq data=freesug_sum;
	table SUPPID ;
	run;

	/* note: respondent that reported no beverages would not be counted here */
	
	proc means data=freesug_sum n nmiss mean min max stackods;
	class suppid; 
	var freesugars  ;
	run;
	
	title1;
	
 /*************************************************************************/
 /*                                                                       */
 /*            Merge total nutrient, food and beverage intakes            */
 /*                                                                       */
 /*************************************************************************/

	/* note: goal is to have a single data with 1 row per participant per recall 
		where columns are the dietary constituents of the hefi-2019 */

/* Merge all data into one by subject and recall number */
	data fmt.intake_per24hr(drop=i);
		merge food_sum_t
			  bev_sum_t
			  freesug_sum 
			  fmt.hs_nci(in=hs keep=ADM_RNO SUPPID energy sfa mufa pufa sodium);
		by ADM_RNO SUPPID;
	
	* any missing value for a dietary constituent is due to not consuming a given category  ;
		array all(*) _NUMERIC_ ;
		do i=1 to dim(all);
			if missing(all(i)) then all(i) = 0;		
		end;
	
	* flag null reported energy intake ;
		nonzero_energy=0;
		if energy>0 then nonzero_energy=1;
		
	* add labels to dietary constituents ;
	label 
		vf   = "Vegetables and fruits, RA/d"
		wg   = "Whole grain foods, RA/d"
		rg   = "Refined grain foods, RA/d"
		pfab = "Protein foods, animal-based, RA/d (excludes bev.)"
		pfpb = "Protein foods, plant-based, RA/d (excludes bev.)"
		otherfoods = "Other foods, RA/d"
		water     = "Water and unsweetened beverages, g/d"
		milk      = "Unsweetened milk, g/d"
		plantbev  = "Unsweetened plant-based bev. with protein, g/d"
		otherbevs = "Other (sweetened) beverages, g/d" 
		freesugars = "Total free sugars, g/d"
		;
	
	* drop temporary variables from proc transpose, proc means, etc; 
	drop _: ;
	
	* keep only respondent aged 2y and older from the HS file;
	if hs then output;
	RUN;

 /*************************************************************************/
 /*                                                                       */
 /*                   Overview of `raw` dietary intakes                   */
 /*                                                                       */
 /*************************************************************************/

/* 1) Define a macro variable for all dietary constituents of the HEFI-2019 */
	%let hefi2019_vars = vf wg rg pfab pfpb otherfoods water milk plantbev otherbevs sfa mufa pufa freesugars sodium energy;

/* 2) Define format for zeros */
	proc format;
	value zerofmt
		0-<0.001 = "Zero"
		0.001-high = "Non-zero"
		;
	run;

/* 3) Descriptive statistics */
	proc freq data=fmt.intake_per24hr ;
	format &hefi2019_vars zerofmt. ; 
	table &hefi2019_vars ;
	where suppid=1 and nonzero_energy=1; * first 24-h recall only, non-zero energy intake;
	ods output OneWayFreqs=pZero(where=(CumPercent < 99.9));
	run;
	
	proc sort data=pZero;
		by descending percent;
	run;
	
	proc print data=pZero;
	title1 "Proportion of zero intake among HEFI-2019 dietary constituents for first 24-h recall, CCHS 2015 - Nutrition (n=20,103)";
	id table;
	var Frequency Percent ;
	run;
	title1;

/* delete temporary files to save disk space */
	proc datasets lib=temp nolist nodetails;
		delete fidfrl_cfg;
	run;

/* end of code 1 */