

/**********************************************************************
Project    : Association Between Women's Empowerment and Mental Health
             Symptoms in Bangladesh
Dataset    : Bangladesh Demographic and Health Survey (BDHS) 2022
Software   : Stata 17
Authors     : Md Salek Miah,Mohammod Ohid Ullah                                       

Description:
This do-file replicates the analytical workflow for examining the
associations between SWPER domains (attitudes towards violence,
social independence, and decision-making autonomy) and mental health
outcomes (depression and anxiety) among ever-married women aged 15–49
in Bangladesh.

Methodology follows:
- Ewerling et al. (2017, 2020) SWPER Global methodology
- Miah et al. (2025) analytical framework for mental health

Outcomes:
    - Depression (PHQ-9 ≥ 10)
    - Anxiety (GAD-7 ≥ 10)

Main Exposures (SWPER Domains):
    - Attitudes towards violence
    - Social independence
    - Decision-making autonomy

Covariates (as per DAG and prior literature):
    - Women's age, pregnancy status, pregnancy loss history
    - Household wealth, employment, residence, household size
    - Sex of household head, asset ownership, housing materials
    - Internet use

**********************************************************************/


set maxvar 30000

use "D:\Research\BDHS Research\Cross country analysis\SWEPER Index\data\Pooled_all_countries_harmonized.dta", clear





	
********************************************************************************
* ANALYSIS: EMPOWERMENT AND MENTAL HEALTH OUTCOMES
* Fixed for SWPER missing values - Using existing tertiles
********************************************************************************

* Load pooled harmonized dataset
use "D:\Research\BDHS Research\Cross country analysis\SWEPER Index\data\Pooled_all_countries_harmonized.dta", clear

********************************************************************************
* SECTION 1: UNDERSTAND SAMPLE AND CREATE ANALYSIS INDICATOR
********************************************************************************

* Create analysis sample indicator (non-missing SWPER tertiles)
gen analysis_sample = 1 if !missing(attitude_violence_tertile) & !missing(social_independence_tertile) & !missing(decision_autonomy_tertile)
label define sample_lbl 1 "Complete SWPER data"
label values analysis_sample sample_lbl
label variable analysis_sample "Analysis Sample (Complete SWPER Data)"

* Check analysis sample by country
tab country_numeric analysis_sample, row

* Check sample sizes
display "Total observations: " _N
display "Analysis sample (complete SWPER): " _N if analysis_sample==1

********************************************************************************
* SECTION 2: REDEFINE VARIABLES FOR ANALYSIS SAMPLE
********************************************************************************

* Create numeric country variable for regression
encode country, gen(country_numeric)
label variable country_numeric "Country"

* Create composite SWPER empowerment index using the continuous variables
egen swper_composite = rowmean(attitude_violence social_independence decision_autonomy)
label variable swper_composite "SWPER Composite Empowerment Index"

* Create tertiles for composite index (using analysis sample only)
xtile swper_empowerment_cat = swper_composite if analysis_sample==1, nq(3)
label define swper_cat 1 "Low" 2 "Medium" 3 "High", replace
label values swper_empowerment_cat swper_cat
label variable swper_empowerment_cat "SWPER Composite Empowerment (categorical)"

* Check distribution
tab swper_empowerment_cat

********************************************************************************
* SECTION 3: SET SURVEY DESIGN FOR ANALYSIS SAMPLE
********************************************************************************

* Set survey design for analysis sample
svyset [pw=WGT] if analysis_sample==1, psu(v021) strata(v022)



/*
	
	
********************************************************************************
* COUNTRY-SPECIFIC ANALYSIS - SIMPLIFIED STYLE
* Using SWPER tertiles as main predictors
********************************************************************************

* Load pooled harmonized dataset
use "D:\Research\BDHS Research\Cross country analysis\SWEPER Index\data\Pooled_all_countries_harmonized.dta", clear

* Create numeric country variable
encode country, gen(country_numeric)

* Set survey design for analysis sample (complete SWPER data)
gen analysis_sample = 1 if !missing(attitude_violence_tertile) & !missing(social_independence_tertile) & !missing(decision_autonomy_tertile)
svyset [pw=WGT] if analysis_sample==1, psu(v021) strata(v022)
*/





keep if !missing(attitude_violence) & ///
        !missing(social_independence) & ///
        !missing(decision_autonomy)







********************************************************************************
* ANALYSIS: EMPOWERMENT AND MENTAL HEALTH OUTCOMES
* Pooled and Country-Specific Analysis
**
********************************************************************************
* SECTION 1: DESCRIPTIVE ANALYSIS
********************************************************************************

* 1.1 Overall prevalence of depression and anxiety
svy: proportion depression anxiety

* 1.2 Prevalence by country
svy: tab country depression, row format(%9.2f)
svy: tab country anxiety, row format(%9.2f)

* 1.3 Prevalence by SWPER categories (pooled)
svy: tab attitude_violence_pooled_cat depression, row format(%9.2f)
svy: tab social_independence_pooled_cat depression, row format(%9.2f)
svy: tab decision_autonomy_pooled_cat depression, row format(%9.2f)

svy: tab attitude_violence_pooled_cat anxiety, row format(%9.2f)
svy: tab social_independence_pooled_cat anxiety, row format(%9.2f)
svy: tab decision_autonomy_pooled_cat anxiety, row format(%9.2f)

* 1.4 Prevalence by SWPER composite categories
svy: tab swper_empowerment_cat depression, row format(%9.2f)
svy: tab swper_empowerment_cat anxiety, row format(%9.2f)





********************************************************************************
* TABLE 1 STYLE OUTPUT (N + ROW % + P-VALUE)
********************************************************************************

* Depression
foreach var in ///
attitude_violence_pooled_cat ///
social_independence_pooled_cat ///
decision_autonomy_pooled_cat ///
women_age ///
num_children ///
currently_pregnant ///
pregnancy_loss ///
wealth_cat ///
currently_working ///
area ///
hh_size_cat ///
hh_head_sex ///
hh_assets ///
hh_materials ///
internet_use ///
country_numeric {

    di "------------------------------------------------------------"
    di "Variable: `var' (Depression)"
    svy: tab `var' depression, row count pearson format(%9.2f)
}

********************************************************************************
* Anxiety
********************************************************************************

foreach var in ///
attitude_violence_pooled_cat ///
social_independence_pooled_cat ///
decision_autonomy_pooled_cat ///
women_age ///
num_children ///
currently_pregnant ///
pregnancy_loss ///
wealth_cat ///
currently_working ///
area ///
hh_size_cat ///
hh_head_sex ///
hh_assets ///
hh_materials ///
internet_use ///
country_numeric {

    di "------------------------------------------------------------"
    di "Variable: `var' (Anxiety)"
    svy: tab `var' anxiety, row count pearson format(%9.2f)
}




proportion depression  occ_resp

proportion anxiety  occ_resp







********************************************************************************
* SECTION 2: REDEFINE SWPER CATEGORIES USING EXISTING VARIABLES
********************************************************************************

* Use country-specific tertiles (already created in harmonization)
* These are: attitude_violence_tertile, social_independence_tertile, decision_autonomy_tertile

* Check these variables
tab attitude_violence_tertile
tab social_independence_tertile
tab decision_autonomy_tertile





* ******************** MODEL 1: UNADJUSTED - INDIVIDUAL SWPER COMPONENTS ********************

* This model examines the association between depression
* and the three SWPER components.

svy: logistic depression i.attitude_violence_pooled_cat i.social_independence_pooled_cat i.decision_autonomy_pooled_cat

* svy: = accounts for the survey design.
* logistic = performs binary logistic regression.
* depression = outcome variable.
* i. = treats the variable as categorical.



* ******************** MODEL 2: ADJUSTED FOR DEMOGRAPHIC CONTROLS ********************

* This model adds demographic variables to Model 1.

svy: logistic depression i.attitude_violence_pooled_cat i.social_independence_pooled_cat i.decision_autonomy_pooled_cat ///
i.women_age  i.num_children i.currently_pregnant i.pregnancy_loss

* women_age = woman's age.
* num_children = number of children.
* currently_pregnant = current pregnancy status.
* pregnancy_loss = pregnancy loss status.
* These variables control for demographic differences.



* ******************** MODEL 3: ADJUSTED FOR DEMOGRAPHIC + SOCIOECONOMIC CONTROLS ********************

* This model adds socioeconomic variables to Model 2.

svy: logistic depression i.attitude_violence_pooled_cat i.social_independence_pooled_cat i.decision_autonomy_pooled_cat ///
i.women_age  i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working

* wealth_cat = household wealth category.
* currently_working = current working status.
* These variables control for socioeconomic differences.




* ******************** MODEL 4: ADJUSTED FOR DEMOGRAPHIC + SOCIOECONOMIC + HOUSEHOLD CONTROLS ********************

* This model adds household-level variables to Model 3.

svy: logistic depression i.attitude_violence_pooled_cat i.social_independence_pooled_cat i.decision_autonomy_pooled_cat ///
i.women_age i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working  ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use

* area = residence/area.
* hh_size_cat = household size.
* hh_head_sex = sex of household head.
* hh_assets = household assets.
* hh_materials = household materials.
* internet_use = internet use.
* These variables control for household characteristics.


* Saves the Model 4 results as m4_dep_pooled.


* ******************** MODEL 5: FULL MODEL WITH COUNTRY FIXED EFFECTS ********************

* This is the final fully adjusted model.
* It includes all variables from Models 1-4
* and additionally controls for country.

svy: logistic depression i.attitude_violence_pooled_cat i.social_independence_pooled_cat i.decision_autonomy_pooled_cat ///
i.women_age  i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working  ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use ///
i.country_numeric

* country_numeric = country variable.
* i.country_numeric creates country indicator variables.
* This controls for differences between countries.





* ******************** VIF: CHECK MULTICOLLINEARITY ********************

* This model includes all main predictors and control variables.
* VIF is used to check whether the independent variables
* are highly correlated with each other.

regress attitude_violence_pooled_cat i.social_independence_pooled_cat i.decision_autonomy_pooled_cat ///
i.women_age  i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working  ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use

* regress = runs a linear regression for checking multicollinearity.
* The outcome variable here is used only for the VIF calculation.
* The main purpose is to examine the correlation among predictors.

vif

* vif = calculates the Variance Inflation Factor for the predictors.
* Lower VIF values indicate less multicollinearity.
* A VIF around 1 indicates very little correlation.
* VIF values above 5 may indicate a potential multicollinearity problem.
* VIF values above 10 are commonly considered a serious concern.


* ******************** HOSMER-LEMESHOW (HL) GOODNESS-OF-FIT TEST ********************

* This model uses the same predictors as the final adjusted model.
* The HL test evaluates how well the logistic regression model
* fits the observed data.

logistic anxiety i.attitude_violence_pooled_cat i.social_independence_pooled_cat i.decision_autonomy_pooled_cat ///
i.women_age  i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working  ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use

* logistic = performs binary logistic regression.
* anxiety = binary outcome variable.
* i. = treats the predictors as categorical variables.


estat gof, group(10) table

* estat gof = performs the goodness-of-fit test.
* group(10) = divides observations into 10 groups based on predicted risk.
* table = displays the goodness-of-fit results in a table.
*
* Interpretation:
* p > 0.05 = no evidence of poor model fit.
* p < 0.05 = evidence that the model may not fit the data well.








* ******************** MODEL 1: UNADJUSTED - INDIVIDUAL SWPER COMPONENTS ********************

* This model examines the association between depression
* and the three SWPER components.

svy: logistic depression i.attitude_violence_pooled_cat i.social_independence_pooled_cat i.decision_autonomy_pooled_cat

* svy: = accounts for the survey design.
* logistic = performs binary logistic regression.
* depression = outcome variable.
* i. = treats the variable as categorical.


* ******************** MODEL 2: ADJUSTED FOR DEMOGRAPHIC CONTROLS ********************

* This model adds demographic variables to Model 1.

svy: logistic depression i.attitude_violence_pooled_cat i.social_independence_pooled_cat i.decision_autonomy_pooled_cat ///
i.women_age  i.num_children i.currently_pregnant i.pregnancy_loss

* women_age = woman's age.
* num_children = number of children.
* currently_pregnant = current pregnancy status.
* pregnancy_loss = pregnancy loss status.
* These variables control for demographic differences.


* ******************** MODEL 3: ADJUSTED FOR DEMOGRAPHIC + SOCIOECONOMIC CONTROLS ********************

* This model adds socioeconomic variables to Model 2.

svy: logistic depression i.attitude_violence_pooled_cat i.social_independence_pooled_cat i.decision_autonomy_pooled_cat ///
i.women_age  i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working

* wealth_cat = household wealth category.
* currently_working = current working status.
* These variables control for socioeconomic differences.


* ******************** MODEL 4: ADJUSTED FOR DEMOGRAPHIC + SOCIOECONOMIC + HOUSEHOLD CONTROLS ********************

* This model adds household-level variables to Model 3.

svy: logistic depression i.attitude_violence_pooled_cat i.social_independence_pooled_cat i.decision_autonomy_pooled_cat ///
i.women_age i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working  ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use

* area = residence/area.
* hh_size_cat = household size.
* hh_head_sex = sex of household head.
* hh_assets = household assets.
* hh_materials = household materials.
* internet_use = internet use.
* These variables control for household characteristics.


* ******************** MODEL 5: FULL MODEL WITH COUNTRY FIXED EFFECTS ********************

* This is the final fully adjusted model.
* It includes all variables from Models 1-4
* and additionally controls for country.

svy: logistic depression i.attitude_violence_pooled_cat i.social_independence_pooled_cat i.decision_autonomy_pooled_cat ///
i.women_age  i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working  ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use ///
i.country_numeric

* country_numeric = country variable.
* i.country_numeric creates country indicator variables.
* This controls for differences between countries.


* ******************** DEPRESSION: UNADJUSTED MODEL ********************

* This is the unadjusted analysis for depression.
* It includes only the three SWPER components.

svy: logistic depression i.attitude_violence_pooled_cat i.social_independence_pooled_cat i.decision_autonomy_pooled_cat

* svy: = accounts for the survey design.
* logistic = binary logistic regression.
* depression = outcome variable.


* ******************** DEPRESSION: ADJUSTED MODEL ********************

* This model adjusts for demographic, socioeconomic,
* and household characteristics.

svy: logistic depression i.attitude_violence_pooled_cat i.social_independence_pooled_cat i.decision_autonomy_pooled_cat ///
i.women_age  i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working  ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use

* This model provides the adjusted association between
* SWPER components and depression.


* ******************** DEPRESSION: POOLED MODEL ********************

* This is the fully adjusted pooled model.
* It additionally controls for country differences.

svy: logistic depression i.attitude_violence_pooled_cat i.social_independence_pooled_cat i.decision_autonomy_pooled_cat ///
i.women_age  i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working  ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use ///
i.country_numeric

* country_numeric = country variable.
* Adding country_numeric controls for differences between countries.
* This is the final pooled model.




* ******************** GOODNESS-OF-FIT TEST: DEPRESSION ********************

* This test evaluates how well the logistic regression model
* fits the observed depression data.

logistic depression i.attitude_violence_pooled_cat i.social_independence_pooled_cat i.decision_autonomy_pooled_cat ///
i.women_age i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use

* logistic = performs binary logistic regression.
* depression = outcome variable.
* The model includes the same predictors as the adjusted model.

estat gof, group(10) table

* estat gof = performs the goodness-of-fit test.
* group(10) = divides observations into 10 groups based on predicted risk.
* table = displays the goodness-of-fit results in a table.
*
* Interpretation:
* p > 0.05 = no evidence of poor model fit.
* p < 0.05 = evidence of poor model fit.




* ******************** GOODNESS-OF-FIT TEST: POOLED DEPRESSION MODEL ********************

* This is the goodness-of-fit test for the final pooled model.
* Country fixed effects are included.

logistic depression i.attitude_violence_pooled_cat i.social_independence_pooled_cat i.decision_autonomy_pooled_cat ///
i.women_age i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use ///
i.country_numeric

* country_numeric = country fixed effects.
* This controls for differences between countries.

estat gof, group(10) table

* estat gof = performs the goodness-of-fit test.
* group(10) = creates 10 groups based on predicted probabilities.
* table = displays the test results.
*
* Interpretation:
* p > 0.05 = model has an acceptable fit.
* p < 0.05 = model may have poor fit.





* ******************** SECTION 7: COUNTRY-SPECIFIC ANALYSIS ********************

* ******************** BANGLADESH (1) - DEPRESSION ********************

* This model examines the association between SWPER components
* and depression among women in Bangladesh only.

svy: logistic depression ///
i.attitude_violence_tertile i.social_independence_tertile i.decision_autonomy_tertile ///
i.women_age i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use ///
if country_numeric == 1

* if country_numeric == 1 = includes Bangladesh only.
* The model is adjusted for demographic, socioeconomic,
* and household characteristics.

estimates store dep_bd
* Saves the Bangladesh depression model as dep_bd.


* ******************** LESOTHO (2) - DEPRESSION ********************

* This model examines the association between SWPER components
* and depression among women in Lesotho only.

svy: logistic depression ///
i.attitude_violence_tertile i.social_independence_tertile i.decision_autonomy_tertile ///
i.women_age i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use ///
if country_numeric == 2

* if country_numeric == 2 = includes Lesotho only.
* The model is adjusted for demographic, socioeconomic,
* and household characteristics.

estimates store dep_ls
* Saves the Lesotho depression model as dep_ls.


* ******************** MOZAMBIQUE (3) - DEPRESSION ********************

* This model examines the association between SWPER components
* and depression among women in Mozambique only.

svy: logistic depression ///
i.attitude_violence_tertile i.social_independence_tertile i.decision_autonomy_tertile ///
i.women_age i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use ///
if country_numeric == 3

* if country_numeric == 3 = includes Mozambique only.
* The model is adjusted for demographic, socioeconomic,
* and household characteristics.

estimates store dep_mz
* Saves the Mozambique depression model as dep_mz.


* ******************** NEPAL (4) - DEPRESSION ********************

* This model examines the association between SWPER components
* and depression among women in Nepal only.

svy: logistic depression ///
i.attitude_violence_tertile i.social_independence_tertile i.decision_autonomy_tertile ///
i.women_age i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use ///
if country_numeric == 4

* if country_numeric == 4 = includes Nepal only.
* The model is adjusted for demographic, socioeconomic,
* and household characteristics.

estimates store dep_np
* Saves the Nepal depression model as dep_np.


* ******************** ZAMBIA (5) - DEPRESSION ********************

* This model examines the association between SWPER components
* and depression among women in Zambia only.

svy: logistic depression ///
i.attitude_violence_tertile i.social_independence_tertile i.decision_autonomy_tertile ///
i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use ///
if country_numeric == 5

* if country_numeric == 5 = includes Zambia only.
* The model is adjusted for the available demographic,
* socioeconomic, and household characteristics.

estimates store dep_zm
* Saves the Zambia depression model as dep_zm.



* ******************** SECTION 8: GOODNESS-OF-FIT TEST ********************

* This test checks how well the logistic regression model
* fits the observed depression data.


* ******************** BANGLADESH (1) - GOODNESS OF FIT ********************

logistic depression ///
i.attitude_violence_tertile i.social_independence_tertile i.decision_autonomy_tertile ///
i.women_age i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use ///
if country_numeric == 1

estat gof, group(10) table

* logistic = performs binary logistic regression.
* if country_numeric == 1 = Bangladesh only.
* estat gof = performs the goodness-of-fit test.
* group(10) = divides participants into 10 groups based on predicted risk.
* table = displays the goodness-of-fit results.
* p > 0.05 = no evidence of poor model fit.
* p < 0.05 = evidence of possible poor model fit.


* ******************** LESOTHO (2) - GOODNESS OF FIT ********************

logistic depression ///
i.attitude_violence_tertile i.social_independence_tertile i.decision_autonomy_tertile ///
i.women_age i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use ///
if country_numeric == 2

estat gof, group(10) table

* if country_numeric == 2 = Lesotho only.
* p > 0.05 = acceptable model fit.
* p < 0.05 = possible poor model fit.


* ******************** MOZAMBIQUE (3) - GOODNESS OF FIT ********************

logistic depression ///
i.attitude_violence_tertile i.social_independence_tertile i.decision_autonomy_tertile ///
i.women_age i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use ///
if country_numeric == 3

estat gof, group(10) table

* if country_numeric == 3 = Mozambique only.
* p > 0.05 = acceptable model fit.
* p < 0.05 = possible poor model fit.


* ******************** NEPAL (4) - GOODNESS OF FIT ********************

logistic depression ///
i.attitude_violence_tertile i.social_independence_tertile i.decision_autonomy_tertile ///
i.women_age i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use ///
if country_numeric == 4

estat gof, group(10) table

* if country_numeric == 4 = Nepal only.
* p > 0.05 = acceptable model fit.
* p < 0.05 = possible poor model fit.


* ******************** ZAMBIA (5) - GOODNESS OF FIT ********************

logistic depression ///
i.attitude_violence_tertile i.social_independence_tertile i.decision_autonomy_tertile ///
i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use ///
if country_numeric == 5

estat gof, group(10) table

* if country_numeric == 5 = Zambia only.
* p > 0.05 = acceptable model fit.
* p < 0.05 = possible poor model fit.




* ******************** SECTION 9: COUNTRY-SPECIFIC - ANXIETY MODELS ********************

* ******************** BANGLADESH (1) - ANXIETY ********************

* This model examines the association between SWPER components
* and anxiety among women in Bangladesh only.

svy: logistic anxiety ///
i.attitude_violence_tertile i.social_independence_tertile i.decision_autonomy_tertile ///
i.women_age i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use ///
if country_numeric == 1

* if country_numeric == 1 = Bangladesh only.
* The model is adjusted for demographic, socioeconomic,
* and household characteristics.

estimates store anx_bd
* Saves the Bangladesh anxiety model as anx_bd.


* ******************** LESOTHO (2) - ANXIETY ********************

* Lesotho is currently excluded from this analysis.
* The following model is commented out and will not run.

/*
svy: logistic anxiety ///
i.attitude_violence_tertile i.social_independence_tertile i.decision_autonomy_tertile ///
i.women_age i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use ///
if country_numeric == 2

estimates store anx_ls
*/


* ******************** MOZAMBIQUE (3) - ANXIETY ********************

* This model examines the association between SWPER components
* and anxiety among women in Mozambique only.

svy: logistic anxiety ///
i.attitude_violence_tertile i.social_independence_tertile i.decision_autonomy_tertile ///
i.women_age i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use ///
if country_numeric == 3

* if country_numeric == 3 = Mozambique only.
* The model is adjusted for demographic, socioeconomic,
* and household characteristics.

estimates store anx_mz
* Saves the Mozambique anxiety model as anx_mz.


* ******************** NEPAL (4) - ANXIETY ********************

* This model examines the association between SWPER components
* and anxiety among women in Nepal only.

svy: logistic anxiety ///
i.attitude_violence_tertile i.social_independence_tertile i.decision_autonomy_tertile ///
i.women_age i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use ///
if country_numeric == 4

* if country_numeric == 4 = Nepal only.
* The model is adjusted for demographic, socioeconomic,
* and household characteristics.

estimates store anx_np
* Saves the Nepal anxiety model as anx_np.


* ******************** ZAMBIA (5) - ANXIETY ********************

* This model examines the association between SWPER components
* and anxiety among women in Zambia only.

svy: logistic anxiety ///
i.attitude_violence_tertile i.social_independence_tertile i.decision_autonomy_tertile ///
i.women_age  i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use ///
if country_numeric == 5

* if country_numeric == 5 = Zambia only.
* The model is adjusted for the available demographic,
* socioeconomic, and household characteristics.

estimates store anx_zm
* Saves the Zambia anxiety model as anx_zm.



* ******************** SECTION 10: GOODNESS-OF-FIT - ANXIETY MODELS ********************

* This test checks how well each country-specific logistic regression
* model fits the observed anxiety data.


* ******************** BANGLADESH (1) - GOODNESS OF FIT ********************

logistic anxiety ///
i.attitude_violence_tertile i.social_independence_tertile i.decision_autonomy_tertile ///
i.women_age i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use ///
if country_numeric == 1

estat gof, group(10) table

* logistic = performs binary logistic regression.
* if country_numeric == 1 = Bangladesh only.
* estat gof = performs the goodness-of-fit test.
* group(10) = divides participants into 10 groups based on predicted risk.
* table = displays the goodness-of-fit results.
* p > 0.05 = no evidence of poor model fit.
* p < 0.05 = evidence of possible poor model fit.


* ******************** LESOTHO (2) - GOODNESS OF FIT ********************

* Lesotho is excluded because the anxiety model was not estimated.


* ******************** MOZAMBIQUE (3) - GOODNESS OF FIT ********************

logistic anxiety ///
i.attitude_violence_tertile i.social_independence_tertile i.decision_autonomy_tertile ///
i.women_age i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use ///
if country_numeric == 3

estat gof, group(10) table

* if country_numeric == 3 = Mozambique only.
* p > 0.05 = acceptable model fit.
* p < 0.05 = possible poor model fit.


* ******************** NEPAL (4) - GOODNESS OF FIT ********************

logistic anxiety ///
i.attitude_violence_tertile i.social_independence_tertile i.decision_autonomy_tertile ///
i.women_age i.num_children i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use ///
if country_numeric == 4

estat gof, group(10) table

* if country_numeric == 4 = Nepal only.
* p > 0.05 = acceptable model fit.
* p < 0.05 = possible poor model fit.


* ******************** ZAMBIA (5) - GOODNESS OF FIT ********************

logistic anxiety ///
i.attitude_violence_tertile i.social_independence_tertile i.decision_autonomy_tertile ///
i.women_age i.currently_pregnant i.pregnancy_loss ///
i.wealth_cat i.currently_working ///
i.area i.hh_size_cat i.hh_head_sex i.hh_assets i.hh_materials i.internet_use ///
if country_numeric == 5

estat gof, group(10) table

* if country_numeric == 5 = Zambia only.
* p > 0.05 = acceptable model fit.
* p < 0.05 = possible poor model fit.


* ******************** END OF SCRIPT ********************

* Please review all variable names, model specifications,
* and coding carefully before running the analysis.
*
* If you find any typing error, coding error, or mistake,
* please contact the researcher for clarification.
*
* Thank you.

* ******************** END OF SCRIPT ********************



































