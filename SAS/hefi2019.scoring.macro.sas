/*************************************************************************/
/*                                                                       */
/*              THE HEALTHY EATING FOOD INDEX SCORING MACRO              */
/*                 Apply the HEFI-2019 scoring algorithm                 */
/*                                                                       */
/* Version: 2.1                                                          */
/* Date: 2022-03                                                         */
/* Details: https://github.com/didierbrassard/hefi2019/                  */
/*                                                                       */
/*************************************************************************/
/*                                                                       */
/* This macro scores dietary constituents provided in the input data set */
/* according to the Healthy Eating Food Index (HEFI)-2019 scoring        */
/* algorithm. See Brassard et al., APNM 2022 for additional details and  */
/* information on classification of foods.                               */
/*                                                                       */
/* The original variables are kept in the output data. The new variables */
/* the macro creates include density of intakes (i.e., ratios of dietary */
/* constituents), the total HEFI-2019 score and component scores.        */
/*                                                                       */
/* Of note, when no foods, beverages or energy are reported, ratios are  */
/* not calculated and a score of 0 is assigned to the corresponding      */
/* components. Missing data for any dietary constituent will result in   */
/* missing component score(s) and total score.                           */
/*                                                                       */
/* Suggested layout for the input dataset:                               */
/* The macro should ideally be applied to a dataset where rows           */
/* correspond to individuals and dietary constituents are columns.       */
/*                                                                       */
/*************************************************************************/
/*                                                                       */
/* The syntax for calling the HEFI2019 macro is :                        */
/*                                                                       */
/* %HEFI2019(indata                 =,                                   */
/*          vegfruits               =,                                   */
/*          wholegrfoods            =,                                   */
/*          nonwholegrfoods         =,                                   */
/*          profoodsanimal          =,                                   */
/*          profoodsplant           =,                                   */
/*          otherfoods              =,                                   */
/*          waterhealthybev         =,                                   */
/*          otherbeverages          =,                                   */
/*          mufat                   =,                                   */
/*          pufat                   =,                                   */
/*          satfat                  =,                                   */
/*          freesugars              =,                                   */
/*          sodium                  =,                                   */
/*          unsweetmilk             =,                                   */
/*          unsweetplantbevpro      =,                                   */
/*          energy                  =,                                   */
/*          outdata                 =                                    */
/*          );                                                           */
/*                                                                       */
/* where                                                                 */
/*                                                                       */
/* indata                  = input dataset with dietary constituents     */
/* vegwfruits              = Reference amounts (RA) from vegetables and  */
/*                           fruits (excludes fruit juices)              */
/* wholegrfoods            = RA from whole-grain foods                   */
/* nonwholegrfoods         = RA from non-whole grain foods               */
/* profoodsanimal          = RA from animal-based protein foods          */
/* profoodsplant           = RA from plant-based protein foods           */
/* otherfoods              = RA from all others foods (i.e., not         */
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
/* outdata                 = Name of the output data set with HEFI-2019  */
/*                           scores, component scores, and density of    */
/*                           intakes.                                    */
/*                                                                       */
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
/*  Brassard D, Elvidge Munene LA, St-Pierre S, Guenther PM,             */
/*  Kirkpatrick SI, Slater J, et al. Development of the Healthy Eating   */
/*  Food Index (HEFI)-2019 measuring adherence to Canada's Food Guide    */
/*  2019 recommendations on healthy food choices. 2022.                  */
/*  https://doi.org/10.1139/apnm-2021-0415                               */
/*                                                                       */
/*  Brassard D, Elvidge Munene LA, St-Pierre S, Gonzalez A, Guenther PM, */
/*  Jessri M, et al. Evaluation of the Healthy Eating Food Index (HEFI)- */
/*  2019 measuring adherence to Canada's Food Guide 2019 recommendations */
/*  on healthy food choices. Appl Physiol Nutr Metab. 2022.              */
/*  https://doi.org/10.1139/apnm-2021-0416                               */
/*                                                                       */
/*************************************************************************/

%macro HEFI2019 (indata=, vegfruits=, wholegrfoods=, nonwholegrfoods=, 
		profoodsanimal=, profoodsplant=, otherfoods=, waterhealthybev=, unsweetmilk=, 
		unsweetplantbevpro=, otherbeverages=, mufat=, pufat=, satfat=, freesugars=, 
		sodium=, energy=, outdata=);

%PUT # Healthy Eating Food Index-2019 Scoring Algorithm SAS version 2.1 ;

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
%let dietaryconstituents=vegfruits wholegrfoods nonwholegrfoods profoodsanimal profoodsplant otherfoods 
waterhealthybev unsweetmilk unsweetplantbevpro otherbeverages
mufat pufat satfat freesugars sodium energy;
%do kth=1 %to %sysfunc(countw(&dietaryconstituents,' '));
	%local x&kth ;
	%let x&kth = %scan(&dietaryconstituents,&kth,%str( ));
	%if %sysevalf(%superq(&&x&kth)=,boolean) %then %put WARNING: Dietary constituent <%scan(&dietaryconstituents,&kth,%str( ))> was not defined. This could cause unexpected results;
%end;

/* Indicate gram per RA for unsweetened milk and
	unsweetened plant-based beverages with enough protein */
	%local probev_gram_per_RA;
	%let probev_gram_per_RA = 258;

/******************************************/
/*           Scoring algorithm            */
/******************************************/

DATA &outdata; 
         SET &indata; 
 
/* calculate reference amounts from unsweetened milk and  
         unsweetened plant-based beverages protein foods */
        
	if not missing(&unsweetmilk) then
		unsweetmilk_RA = &unsweetmilk / &probev_gram_per_RA ;
	else unsweetmilk_RA = . ;
       
	if not missing(&unsweetplantbevpro) then 
		unsweetplantbevpro_RA  = &unsweetplantbevpro  / &probev_gram_per_RA ; 
	else unsweetplantbevpro_RA  = .;
 
/* sum total reference amounts from foods and protein beverages */
	totfoodsRA = &vegfruits + &wholegrfoods + &nonwholegrfoods +
				 &profoodsanimal + &profoodsplant + &otherfoods +
				 unsweetmilk_RA + unsweetplantbevpro_RA;          
 
 /************************************************/
 /* Component 1 - Vegetables and fruits          */
 /************************************************/
	if totfoodsRA > 0 then do;
	/* ratio */
	    RATIO_VF = &vegfruits / totfoodsRA ;
	
	/* score */
		HEFI2019C1_VF    = 20 * ( RATIO_VF / 0.50 );
		if HEFI2019C1_VF > 20 then HEFI2019C1_VF = 20;
	end;
	else do;
	   RATIO_VF     =.;
	   HEFI2019C1_VF=.;
	end;

 /************************************************/
 /* Component 2 -  Whole-grain foods             */
 /************************************************/
	if totfoodsRA > 0 then do;
	/* ratio */
		RATIO_WGTOT = &wholegrfoods / totfoodsRA ;
		
	/* score */
		HEFI2019C2_WHOLEGR = 5 * (RATIO_WGTOT / 0.25) ; 
		if HEFI2019C2_WHOLEGR > 5 then HEFI2019C2_WHOLEGR = 5;
	end;
	else do;
		RATIO_WGTOT       =.;
		HEFI2019C2_WHOLEGR=.;
	end;
                   
 
 /************************************************/
 /* Component 3 -  Grain foods ratio             */
 /************************************************/
	/* total */
	totgrain = &wholegrfoods + &nonwholegrfoods ; 
	                   
	if not missing(totgrain) and totgrain > 0 then do;
	/* ratio */
		RATIO_WGGR = &wholegrfoods / totgrain ;
	 
	/* score */
		HEFI2019C3_GRRATIO = 5 * (RATIO_WGGR) ; 
		if HEFI2019C3_GRRATIO > 5 then HEFI2019C3_GRRATIO = 5;
	end;
	else if totgrain = 0 then do;
		RATIO_WGGR         =.;
		HEFI2019C3_GRRATIO = 0;
	end;
	else do;
		RATIO_WGGR        =.;
		HEFI2019C3_GRRATIO=.;
	end;
	
 /************************************************/
 /* Component 4 - Protein foods                  */
 /************************************************/
	/* total */
	totpro = &profoodsanimal + &profoodsplant + unsweetmilk_RA + unsweetplantbevpro_RA ; 
	 
	if totfoodsRA > 0 then do;
	/* ratio */
		RATIO_PRO = totpro / totfoodsRA ;
	          
	/* score */
		HEFI2019C4_PROFOODS = 5 * (RATIO_PRO / 0.25); 
		if HEFI2019C4_PROFOODS > 5 then HEFI2019C4_PROFOODS = 5; 
	end;
	else do;
		RATIO_PRO          =.;
		HEFI2019C4_PROFOODS=.;
	end;
	 
 /************************************************/
 /* Component 5 - Plant-based protein foods      */
 /************************************************/

	if not missing(totpro) and totpro > 0 then do;
	/* ratio */
		RATIO_PLANT = (&profoodsplant+unsweetplantbevpro_RA) / totpro ;
      
	/* score */
		HEFI2019C5_PLANTPRO = 5 * (RATIO_PLANT / 0.50000001); 
		if HEFI2019C5_PLANTPRO > 5 then HEFI2019C5_PLANTPRO = 5; 
	end;
	else if totpro=0 then do;
		RATIO_PLANT        =.;
		HEFI2019C5_PLANTPRO=0;
	end;
	else do;
		RATIO_PLANT        =.;
		HEFI2019C5_PLANTPRO=.;
	end;

/* zero food reported */
	if totfoodsRA = 0 then do;
		HEFI2019C1_VF       = 0;
		HEFI2019C2_WHOLEGR  = 0;
		HEFI2019C4_PROFOODS = 0;
	end;
          
 /************************************************/
 /* Component 6 - Beverages                      */
 /************************************************/
	/* total */
	totbev = &waterhealthybev + &unsweetmilk + &unsweetplantbevpro + &otherbeverages ; 
      
	if not missing(totbev) and totbev > 0 then do;
	/* ratio */
		RATIO_BEV = (&waterhealthybev + &unsweetmilk + &unsweetplantbevpro ) / totbev ;
      
	/* score */
		HEFI2019C6_BEVERAGES = 10 * (RATIO_BEV ); 
		if HEFI2019C6_BEVERAGES > 10 then HEFI2019C6_BEVERAGES = 10; 
	end;
	else if totbev = 0 then do;
		RATIO_BEV           =.;
		HEFI2019C6_BEVERAGES=0;
	end;
	else do;
		RATIO_BEV           =.;
		HEFI2019C6_BEVERAGES=.;	
	end;
	
 /************************************************/
 /* Component 7 - Ratio of unsaturated fats      */
 /************************************************/
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
		
 /************************************************/
 /* Component 8 - Saturated fats                 */
 /************************************************/
	/* input limits */
	SFAmin=10; 
	SFAmax=15; 
               
	/* ratio */
	if (&energy > 0 and not missing(&satfat)) then SFA_PERC = 100 * (&satfat * 9 / &energy);
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
	
 /************************************************/
 /* Component 9 - Free sugars                    */
 /************************************************/
	/* input limits */
	SUGmin=10; 
	SUGmax=20; 
               
	/* ratio */
	if (&energy > 0 and not missing(&freesugars)) then SUG_PERC = 100 * (&freesugars * 4 / &energy);
	else SUG_PERC=.;
      
	/* score */
    if not missing(SUG_PERC) then do;
		if SUG_PERC < SUGmin then HEFI2019C9_FREESUGARS = 10; 
		else if SUG_PERC >= SUGmax then HEFI2019C9_FREESUGARS=0; 
		else HEFI2019C9_FREESUGARS = 10 - ( 10* (SUG_PERC-SUGmin) / (SUGmax-SUGmin) ); 
	end;
	else do;
		HEFI2019C9_FREESUGARS=.;
	end;
	
 /************************************************/
 /* Component 10 - Sodium                        */
 /************************************************/
	/* input limits */
	SODmin=0.9; 
	SODmax=2.0; 
      
	/* ratio */
	if (&energy > 0 and not missing(&sodium)) then SODDEN = &sodium / &energy;
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
		if &energy = 0 then do; 
          HEFI2019C8_SFAT=0; 
          HEFI2019C9_FREESUGARS=0; 
          HEFI2019C10_SODIUM=0; 
         end; 

 /************************************************/
 /*             Total HEFI-2019 score            */
 /************************************************/

/* Calculate the Healthy Eating Food Index total score (i.e., the sum of its component scores) */
	HEFI2019_TOTAL_SCORE = HEFI2019C1_VF + HEFI2019C2_WHOLEGR + HEFI2019C3_GRRATIO + HEFI2019C4_PROFOODS +
		HEFI2019C5_PLANTPRO + HEFI2019C6_BEVERAGES + HEFI2019C7_FATTYACID + HEFI2019C8_SFAT +
		HEFI2019C9_FREESUGARS + HEFI2019C10_SODIUM ; 
 
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
         HEFI2019C9_FREESUGARS = 'HEFI2019 C9 Free sugars'
         HEFI2019C10_SODIUM = 'HEFI2019 C10 Sodium'
          
         RATIO_VF='Ratio of vegetable and fruits over total foods'
         RATIO_WGTOT='Ratio of whole-grain foods over total foods'
         RATIO_WGGR='Ratio of whole-grain foods over total grains'
         RATIO_PRO='Ratio of protein foods over total foods'
         RATIO_PLANT='Ratio of plant-based over protein foods'
         RATIO_FA='Ratio of unsaturated over saturated fats'
         RATIO_BEV='Ratio of beverages over total beverages'
         SFA_PERC='Percent of calories from saturated fat'
         SUG_PERC='Percent of calories from free sugars'
         SODDEN='Ratio of sodium per 1000 kcal'
         totfoodsRA='Total foods (RA/d)'
; 

FORMAT HEFI2019: 8.1;

DROP unsweetmilk_RA unsweetplantbevpro_RA totgrain totpro unsatfat
	 FATmin FATmax totbev SFAmin SFAmax SUGmin SUGmax SODmin SODmax ;

RUN;
%mend HEFI2019; 

/*******************************************************************/
/*                   END OF THE SCORING MACRO                      */
/*******************************************************************/
