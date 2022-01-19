/*************************************************************************/
/*                                                                       */
/*              THE HEALTHY EATING FOOD INDEX SCORING MACRO              */
/*                     VERSION 2.0        2022-01                        */
/*                                                                       */
/*             https://github.com/didierbrassard/hefi2019/               */
/*************************************************************************/
/*                                                                       */
/* This macro scores dietary constituents provided in the input data set */
/* according to the Healthy Eating Food Index (HEFI)-2019 scoring        */
/* algorithm. See Brassard et al., APNM 2022 for additional details and  */
/* information on classification of foods.                               */
/*                                                                       */
/* The original variables are kept in the output data. The new variables */
/* created include density of intakes (i.e., ratios of dietary           */
/* constituents), the total HEFI-2019 score and component scores.        */
/*                                                                       */
/* Of note, when no foods, beverages or energy are reported, ratios are  */
/* not calculated and a score of 0 is assigned to the corresponding      */
/* components. Missing data for any dietary constituent will result in   */
/* missing component score(s) and total score.                           */
/*                                                                       */
/* Suggested layout for the input dataset:                               */
/* The macro should ideally be applied to a dataset in the long format,  */
/* where observations are rows and dietary constituents are columns.     */
/*                                                                       */
/*************************************************************************/
/*                                                                       */
/* The syntax for calling the HEFI2019 macro is :                        */
/*                                                                       */
/* %HEFI2019(indata                 =,                                   */
/*          vegwfruits              =,                                   */
/*          wholegrfoods            =,                                   */
/*          nonwholegrfoods         =,                                   */
/*          profoodsanimal          =,                                   */
/*          profoodsplant           =,                                   */
/*          otherfoods              =,                                   */
/*          mufat                   =,                                   */
/*          pufat                   =,                                   */
/*          satfat                  =,                                   */
/*          sugars                  =,                                   */
/*          kcal                    =,                                   */
/*          sodium                  =,                                   */
/*          water_and_other_healthy =,                                   */
/*          unsweetmilk             =,                                   */
/*          unsweetplantbevpro      =,                                   */
/*          otherbev                =,                                   */
/*          outdata                 =                                    */
/*          );                                                           */
/*                                                                       */
/* where                                                                 */
/*                                                                       */
/* indata                  = input dataset with dietary constituents     */
/* vegwfruits              = RAs* from vegetables and fruits (excludes   */
/*                           fruit juices)                               */
/* wholegrfoods            = RAs from whole-grain foods                  */
/* nonwholegrfoods         = RAs from non-whole grain foods              */
/* profoodsanimal          = RAs from animal-based protein foods         */
/* profoodsplant           = RAs from plant-based protein foods          */
/* otherfoods              = RAs from all others foods (i.e., not        */
/*                           considered in the above variables)          */
/* mufat                   = Grams of fat from monounsaturated fats      */
/* pufat                   = Grams of fat from polyunsaturated fats      */
/* satfat                  = Grams of fat from saturated fats            */
/* sugars                  = Grams of free sugars                        */
/* kcal                    = Total energy intake, kcal                   */
/* sodium                  = Milligrams of sodium                        */
/* water_and_other_healthy = Grams of water and other healthy beverages  */
/*                         (see definition in Brassard et al. APNM 2022) */
/* unsweetmilk             = Grams of unsweetened milk (all % M.F.)      */
/* unsweetplantbevpro      = Grams of unsweetened plant-based beverages  */
/* otherbev                = Grams of all other beverages (artificially- */
/*                           or sugar-sweetened beverages, juices,       */
/*                           sweetened milk or plant-based beverages,    */
/*                           ... etc.)                                   */
/*                          (see definition in Brassard et al. APNM 2022)*/
/* outdata                 = Name of the output data with HEFI-2019      */
/*                           scores, component scores, and density of    */
/*                           intakes.                                    */
/*                                                                       */
/*  * RAs indicate reference amounts (Brassard et al., APNM 2022).       */
/*                                                                       */
/* Caution:  variable names "unsweetmilk_RA", "unsweetplantbevpro_RA",   */
/*   "totfoodsRA", "totgrain", "totpro", "totbev", "unsatfat",           */
/*   "RATIO_VF", "RATIO_WGTOT", "RATIO_WGGR", "RATIO_PRO",               */
/*   "RATIO_PLANT", "RATIO_FA", "RATIO_BEV", "SFA_PERC", "SUG_PERC",     */
/*   "SODDEN", "FATmin", "FATmax", "SFAmin", "SFAmax", "SUGmin",         */
/*   "SUGmax", "SODmin", "SODmax" are reserved for this macro.           */
/*                                                                       */
/*************************************************************************/
/*                                                                       */
/* References:                                                           */
/*                                                                       */
/*   Brassard D, Elvidge Munene LA, St-Pierre S, Guenther PM,            */
/*   Kirkpatrick SI, Slater J, et al. Development of the Healthy Eating  */
/*   Food Index (HEFI)-2019 measuring adherence to Canada's Food Guide   */
/*   2019 recommendations on healthy food choices. 2022.                 */
/*   https://doi.org/10.1139/apnm-2021-0415                              */
/*                                                                       */
/*   Brassard D, Elvidge Munene LA, St-Pierre S, Gonzalez A, Guenther PM,*/
/*   Jessri M, et al. Evaluation of the Healthy Eating Food Index (HEFI)-*/
/*   2019 measuring adherence to Canada's Food Guide 2019 recommendations*/
/*   on healthy food choices. Appl Physiol Nutr Metab. 2022.             */
/*   https://doi.org/10.1139/apnm-2021-0416                              */
/*                                                                       */
/*************************************************************************/
 
%macro HEFI2019 (indata=,vegwfruits=,wholegrfoods=,nonwholegrfoods=,profoodsanimal=,profoodsplant=,otherfoods=,mufat=,
pufat=,satfat=,sugars=,kcal=,sodium=,water_and_other_healthy=,unsweetmilk=,unsweetplantbevpro=,otherbev=,
outdata=); 
 
%PUT # Healthy Eating Food Index-2019 Scoring Algorithm SAS version 2.0 ;

/******************************************/
/*       Confirm correct macro call       */
/******************************************/
%if %sysevalf(%superq(indata)=,boolean) %then %do;
	%put ERROR: Input dataset was not defined. Indicate a dataset name to the <indata> macro parameter;
	%return;
%end;
%if %sysevalf(%superq(outdata)=,boolean) %then %do;
	%put ## <Outdata> was not defined. &indata is used as default. ;
	%local outdata;
	%let outdata=&indata;
%end;
%local dietaryconstituents kth ;
%let dietaryconstituents=vegwfruits wholegrfoods nonwholegrfoods profoodsanimal profoodsplant otherfoods mufat 
pufat satfat sugars kcal sodium water_and_other_healthy unsweetmilk unsweetplantbevpro otherbev;
%do kth=1 %to %sysfunc(countw(&dietaryconstituents,' '));
	%local x&kth ;
	%let x&kth = %scan(&dietaryconstituents,&kth,%str( ));
	%if %sysevalf(%superq(&&x&kth)=,boolean) %then %put WARNING: Dietary constituent <%scan(&dietaryconstituents,&kth,%str( ))> was not defined. This could cause unexpected results;
%end;

/******************************************/
/*           Scoring algorithm            */
/******************************************/

data &outdata; 
         set &indata; 
 
/* calculate reference amounts from unsweetened milk and  
         unsweetened plant-based beverages protein foods,  
         assuming average of 258g per RA*/

	unsweetmilk_RA = &unsweetmilk / 258 ; 
	unsweetplantbevpro_RA  = &unsweetplantbevpro  / 258 ; 
 
/* sum total reference amounts from foods and protein beverages */
	totfoodsRA = &vegwfruits + &wholegrfoods + &nonwholegrfoods +  
				 &profoodsanimal + &profoodsplant + &otherfoods +  
				 unsweetmilk_RA + unsweetplantbevpro_RA; 
 
/********************************************/
/* Component 1 - Vegetables and fruits      */
/********************************************/
	/* ratio */
	if totfoodsRA > 0 then RATIO_VF = &vegwfruits / totfoodsRA ;
	else RATIO_VF=.;

	/* score */
	if not missing(RATIO_VF) then do;
		HEFI2019C1_VF = 20 * ( RATIO_VF / 0.50 );
		if HEFI2019C1_VF > 20 then HEFI2019C1_VF = 20;
	end;
	else do;
		HEFI2019C1_VF=.;
	end;

/********************************************/
/* Component 2 -  Whole-grain foods         */
/********************************************/
	/* ratio */
	if totfoodsRA > 0 then RATIO_WGTOT = &wholegrfoods / totfoodsRA ;
	else RATIO_WGTOT=.;
	 
	/* score */
	if not missing(RATIO_WGTOT) then do;
		HEFI2019C2_WHOLEGR = 5 * (RATIO_WGTOT / 0.25) ; 
		if HEFI2019C2_WHOLEGR > 5 then HEFI2019C2_WHOLEGR = 5;
	end;
	else do;
		HEFI2019C2_WHOLEGR=.;
	end;
                   
 
/********************************************/
/* Component 3 -  Grain foods ratio         */
/********************************************/
	/* total */
	totgrain = &wholegrfoods + &nonwholegrfoods ; 
	                   
	/* ratio */
	if totgrain > 0 then RATIO_WGGR = &wholegrfoods / totgrain ;
	else RATIO_WGGR=.;
	 
	/* score */
	if not missing(RATIO_WGGR) then do;
		HEFI2019C3_GRRATIO = 5 * (RATIO_WGGR) ; 
		if HEFI2019C3_GRRATIO > 5 then HEFI2019C3_GRRATIO = 5; 
	end;
	else do;
		HEFI2019C3_GRRATIO=.;
	end;
	
	if totgrain = 0 then HEFI2019C3_GRRATIO = 0;
	
/*******************************************/
/* Component 4 - Protein foods             */
/*******************************************/
	/* total */
	totpro = &profoodsanimal + &profoodsplant + unsweetmilk_RA + unsweetplantbevpro_RA ; 
	 
	/* ratio */
	if totfoodsRA > 0 then RATIO_PRO = totpro / totfoodsRA ;
	else RATIO_PRO=.;
	          
	/* score */
	if not missing(RATIO_PRO) then do;
		HEFI2019C4_PROFOODS = 5 * (RATIO_PRO / 0.25); 
		if HEFI2019C4_PROFOODS > 5 then HEFI2019C4_PROFOODS = 5; 
	end;
	else do;
		HEFI2019C4_PROFOODS=.;
	end;
	 
/*******************************************/
/* Component 5 - Plant-based protein foods */
/*******************************************/
	/* ratio */
	if totpro > 0 then RATIO_PLANT = (&profoodsplant+unsweetplantbevpro_RA) / totpro ;
	else RATIO_PLANT=.;
      
	/* score */
	if not missing(RATIO_PLANT) then do; 
		HEFI2019C5_PLANTPRO = 5 * (RATIO_PLANT / 0.50000001); 
		if HEFI2019C5_PLANTPRO > 5 then HEFI2019C5_PLANTPRO = 5; 
	end;
	else do;
		HEFI2019C5_PLANTPRO=.;
	end;
	
	if totpro=0 then HEFI2019C5_PLANTPRO=0;

/* no foods reported */
	if totfoodsRA = 0 then do;
		HEFI2019C1_VF       = 0;
		HEFI2019C2_WHOLEGR  = 0;
		HEFI2019C4_PROFOODS = 0;
	end;
          
/********************************************/
/* Component 6 - Beverages                  */
/********************************************/
	/* total */
	totbev = &water_and_other_healthy + &unsweetmilk + &unsweetplantbevpro + &otherbev ; 
      
	/* ratio */
	if totbev > 0 then RATIO_BEV = (&water_and_other_healthy + &unsweetmilk + &unsweetplantbevpro ) / totbev ;
	else RATIO_BEV=.;
      
	/* score */
	if not missing(RATIO_BEV) then do;
		HEFI2019C6_BEVERAGES = 10 * (RATIO_BEV ); 
		if HEFI2019C6_BEVERAGES > 10 then HEFI2019C6_BEVERAGES = 10; 
	end;
	else do;
		HEFI2019C6_BEVERAGES=.;
	end;
	
	if totbev=0 then HEFI2019C6_BEVERAGES = 0;
	
/*******************************************/
/* Component 7 - Ratio of unsaturated fats */
/*******************************************/
	/* input limits */
	FATmin=1.1 ; 
	FATmax=2.6 ; 
      
	/* sum */
	unsatfat = &mufat + &pufat ; 
      
	/* ratio */
	if (&satfat > 0 and not missing (unsatfat)) then RATIO_FA = unsatfat / &satfat ;
	else RATIO_FA=.;
      
	/* score */
	if &satfat=0 and unsatfat=0 then HEFI2019C7_FATTYACID=0; 
	else if &satfat=0 and unsatfat>0 then HEFI2019C7_FATTYACID=5; 
	else if RATIO_FA >= FATmax then HEFI2019C7_FATTYACID=5; 
	else if RATIO_FA <= FATmin then HEFI2019C7_FATTYACID=0; 
	else  HEFI2019C7_FATTYACID=5* ( (RATIO_FA-FATmin) / (FATmax-FATmin) );
		
/********************************/
/* Component 8 - Saturated fats */
/********************************/
	/* input limits */
	SFAmin=10; 
	SFAmax=15; 
               
	/* ratio */
	if (&kcal > 0 and not missing(&satfat)) then SFA_PERC = 100 * (&satfat * 9 / &kcal);
	else SFA_PERC=.;
      
	/* score */
    if not missing(SFA_PERC) then do;
		if SFA_PERC < SFAmin then HEFI2019C8_SFAT = 5; 
		else if SFA_PERC >= SFAmax then HEFI2019C8_SFAT=0; 
		else HEFI2019C8_SFAT = 5 - ( 5* (SFA_PERC-SFAmin) / (SFAmax-SFAmin) ); 
	end;
	else do;
		HEFI2019C8_SFAT=.;
	end;
	
/********************************/
/* Component 9 - Free sugars    */
/********************************/
	/* input limits */
	SUGmin=10; 
	SUGmax=20; 
               
	/* ratio */
	if (&kcal > 0 and not missing(&sugars)) then SUG_PERC = 100 * (&sugars * 4 / &kcal);
	else SUG_PERC=.;
      
	/* score */
    if not missing(SUG_PERC) then do;
		if SUG_PERC < SUGmin then HEFI2019C9_SUGARS = 10; 
		else if SUG_PERC >= SUGmax then HEFI2019C9_SUGARS=0; 
		else HEFI2019C9_SUGARS = 10 - ( 10* (SUG_PERC-SUGmin) / (SUGmax-SUGmin) ); 
	end;
	else do;
		HEFI2019C9_SUGARS=.;
	end;
	
/********************************/
/* Component 10 - Sodium        */
/********************************/
	/* input limits */
	SODmin=0.9; 
	SODmax=2.0; 
      
	/* ratio */
	if (&kcal > 0 and not missing(&sodium)) then SODDEN = &sodium / &kcal;
	else SODDEN=.;
      
	/* score */
	if not missing(SODDEN) then do;
		if SODDEN < SODmin then HEFI2019C10_SODIUM = 10; 
		else if SODDEN >= SODmax then HEFI2019C10_SODIUM=0; 
		else HEFI2019C10_SODIUM = 10 - ( 10* (SODDEN-SODmin) / (SODmax-SODmin) );
	end;
	else do;
		HEFI2019C10_SODIUM=.;
	end;
 
/* Zero energy reported */
		if &kcal = 0 then do; 
          HEFI2019C8_SFAT=0; 
          HEFI2019C9_SUGARS=0; 
          HEFI2019C10_SODIUM=0; 
         end; 
 
/* Calculate the Healthy Eating Food Index total score (i.e., the sum of its component scores) */
	HEFI2019_TOTAL_SCORE = HEFI2019C1_VF + HEFI2019C2_WHOLEGR + HEFI2019C3_GRRATIO + HEFI2019C4_PROFOODS +
		HEFI2019C5_PLANTPRO + HEFI2019C6_BEVERAGES + HEFI2019C7_FATTYACID + HEFI2019C8_SFAT +
		HEFI2019C9_SUGARS + HEFI2019C10_SODIUM ; 
 
LABEL  
         HEFI2019_TOTAL_SCORE = "Total Healthy Eating Food Index (/80)"
         HEFI2019C1_VF = 'HEFI2019 C1 Vegetables and fruits'
         HEFI2019C2_WHOLEGR = 'HEFI2019 C2 Whole-grain foods'
         HEFI2019C3_GRRATIO = 'HEFI2019 C3 Grain foods ratio'
         HEFI2019C4_PROFOODS = 'HEFI2019 C4 Protein foods'
         HEFI2019C5_PLANTPRO = 'HEFI2019 C5 Plant-based protein foods'
         HEFI2019C6_BEVERAGES = 'HEFI2019 C6 Beverages'
         HEFI2019C7_FATTYACID = 'HEFI2019 C7 Fatty acids ratio'
         HEFI2019C8_SFAT = 'HEFI2019 C8 Saturated fats'
         HEFI2019C9_SUGARS = 'HEFI2019 C9 Free sugars'
         HEFI2019C10_SODIUM = 'HEFI2019 C10 Sodium'
          
         RATIO_VF='Ratio of vegetable and fruits over total foods'
         RATIO_WGTOT='Ratio of whole-grain foods over total foods'
         RATIO_WGGR='Ratio of whole-grain foods over total grains'
         RATIO_PRO='Ratio of protein foods over total foods'
         RATIO_PLANT='Ratio of plant-based over protein foods'
         RATIO_FA='Ratio of unsaturated over saturated fats'
         RATIO_BEV='Ratio of beverages over total beverages'
         SFA_PERC='Percent of calories from sat fat'
         SUG_PERC='Percent of calories from free sugars'
         SODDEN='Ratio of sodium per 1000 kcal'
         totfoodsRA='Total foods (RA/d)'
; 

FORMAT HEFI2019: 8.1;

DROP unsweetmilk_RA unsweetplantbevpro_RA totgrain totpro unsatfat FATmin FATmax totbev SFAmin SFAmax SUGmin SUGmax SODmin SODmax ;

run;
%mend HEFI2019; 

/*******************************************************************/
/*                   END OF THE SCORING MACRO                      */
/*******************************************************************/
