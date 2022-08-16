********************************************************************
****************************Read in Data****************************
********************************************************************;

*read in data;
proc import datafile = 'nsch_ts_data.csv'
 out = nsch
 dbms = CSV
 replace;
run;

data nsch;
set nsch;
 if year = '2016' then year2 = '1';
 if year = '2017' then year2 = '2';
 if year = '2018' then year2 = '3';
 if year = '2019' then year2 = '4';
 if year = '2020' then year2 = '5';
run;

********************************************************************
*********Full Mixed Effect Model with State Random Intercept********
********************************************************************;

proc glimmix data=nsch method=QUAD;
 class year(ref='2016') year2(ref='1') age(ref='8') gender(ref='male') race(ref='white') educ(ref='Coll') birthWeight(ref='notLow')	preterm(ref='0') asd(ref='0')  adhd(ref='0') english(ref='1') fpl(ref='400_FPL_above') intdis(ref='0') antibully_law_policy_2018(ref='1') prior_auth_2015(ref='1') medicaid_expansion_2019(ref  ='1')	medicaid_expansion_2016(ref='1') schoolwelln_policy_2013(ref='1') schoolnutri_policy_2013(ref='1') region FIPSST;
 model ts = year2 gender age gender*age somecoll_2019 Pediatric_Emergency_Medicine somecoll_2019*Pediatric_Emergency_Medicine
 /link=logit dist=bin s obsweight=FWC covb;
 random intercept/subject = FIPSST s;
 covtest indep;
 output out=out_data pred(ILINK)=phat STDERR(ILINK)=stderr pred(NOBLUP ILINK)=phat2 STDERR(NOBLUP ILINK)=stderr2;
 ods output ParameterEstimates = fixed_effects_estimates;
 ods output CovB = fixed_effects_covariance;
 ods output SolutionR = random_effects_estimates;
run;

proc export data=fixed_effects_estimates
     outfile="fixed_effects_estimates.csv"
     dbms=csv 
     replace;
run;

proc export data=fixed_effects_covariance
     outfile="fixed_effects_covariance.csv"
     dbms=csv 
     replace;
run;

proc export data=random_effects_estimates
     outfile="random_effects_estimates.csv"
     dbms=csv 
     replace;
run;

********************************************************************
*********Empty Mixed Effect Model with State Random Intercept*******
********************************************************************;

proc glimmix data=nsch method=QUAD;
 class year(ref='2016') year2(ref='1') age(ref='8') gender(ref='male') race(ref='white') educ(ref='Coll') birthWeight(ref='notLow')	preterm(ref='0') asd(ref='0')  adhd(ref='0') english(ref='1') fpl(ref='400_FPL_above') intdis(ref='0') antibully_law_policy_2018(ref='1') prior_auth_2015(ref='1') medicaid_expansion_2019(ref  ='1')	medicaid_expansion_2016(ref='1') schoolwelln_policy_2013(ref='1') schoolnutri_policy_2013(ref='1') region FIPSST;
 model ts = /link=logit dist=bin s obsweight=FWC covb;
 random intercept/subject = FIPSST s;
 covtest indep;
 output out=out_data pred(ILINK)=phat STDERR(ILINK)=stderr pred(NOBLUP ILINK)=phat2 STDERR(NOBLUP ILINK)=stderr2;
 ods output ParameterEstimates = fixed_effects_estimates;
 ods output CovB = fixed_effects_covariance;
 ods output SolutionR = random_effects_estimates;
run;

proc export data=fixed_effects_estimates
     outfile="fixed_effects_estimates.csv"
     dbms=csv 
     replace;
run;

proc export data=fixed_effects_covariance
     outfile="fixed_effects_covariance.csv"
     dbms=csv 
     replace;
run;

proc export data=random_effects_estimates
     outfile="random_effects_estimates.csv"
     dbms=csv 
     replace;
run;

