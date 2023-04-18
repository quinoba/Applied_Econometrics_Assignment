* APPLIED ECONOMETRICS, Spring 2023
// Load and update packages
//ssc install outreg2

//adoupdate
clear all

// set working directory to source file directory
cd "`dirname `"`c(current_do)''"'"

//log using assignment.log, replace 

****************************** Question1 ***************************************
// Read data
use "dataonfailscores.dta", clear

** 1. 
// generate dummies
gen Female = (gender == "Female")
gen English = (EnglishNative == "Yes")

** 2. 
// run a LPM
reg Fail Age Agrade BelowBGrade i.CountryCode WorkExperience Year2004 Year2005 Year2006 Year2007 post_grad Female English, vce(robust)
estadd local robust "Yes"
estimates store lpm, title(LPM)
** 4. 
// run Logit
logit Fail Age Agrade BelowBGrade i.CountryCode WorkExperience Year2004 Year2005 Year2006 Year2007 post_grad Female English, vce(robust)
estadd local robust "Yes"
estimates store logit_model, title(Logit)
// get marginal effect
eststo margin: margins, dydx(BelowBGrade Year2006) post
est store m1


** 5. 
// run Probit
probit Fail Age Agrade BelowBGrade i.CountryCode WorkExperience Year2004 Year2005 Year2006 Year2007 post_grad Female English, vce(robust)
estadd local robust "Yes"
estimates store probit_model, title(Probit)
// get marginal effect
eststo margin: margins, dydx(BelowBGrade Year2006) post
est store m2

//LaTex Table
esttab m1 m2 using "margins1.tex", replace

//LaTex Table
esttab lpm logit_model probit_model using table1.tex, cells(b(star fmt(3)) se(par fmt(2)))   ///
   legend collabels(none) varlabels(_cons constant)  ///
   stats(robust r2 N, fmt(0 3 0) label("Robust standard errors" R-sqr Observations)) ///
   addnote("Note: Robust standard errors in parentheses.") replace

** 6. 
// filter out observations of country 8
drop if CountryCode == 8
// generate dummy variable 
gen post_grad06 = (post_grad == 1 & Year2006 == 1)
// run LPM
reg Fail post_grad Year2006 post_grad06, vce(robust)
estadd local robust "Yes"
estimates store lpm_r, title(LPM)

//LaTex Table
esttab lpm_r using table1-2.tex, cells(b(star fmt(3)) se(par fmt(2)))   ///
   legend collabels(none) varlabels(_cons constant)  ///
   stats(robust r2 N, fmt(0 3 0) label("Robust standard errors" R-sqr Observations)) ///
   addnote("Note: Robust standard errors in parentheses.") replace


****************************** Question2 ***************************************

// Read data
use "datasetgasoline.dta", clear

** 1. 
// Sort and declare panel data using Country_Code and Year variables
sort Country_Code Year
xtset Country_Code Year

// Describe the main features of the panel data

xtdescribe
xtsum

// Load do file with code to generate LaTeX table
do summary_tables
xtsum2 Country_Code Year log_gaspcar log_incomepcap log_relgasprice log_carpcap
esttab e(mat_all), mlabels(none) labcol2(`e(obw)') varlabels(r2 " " r3 " ") tex, using summary_stats.tex, replace



** 2.
// Pooled OLS

reg log_gaspcar log_incomepcap log_relgasprice log_carpcap
estadd local individual "No"
estadd local year "No"
estadd local robust "No"
estimates store pooled_ols, title(Pooled OLS)

** 3.
// Estimate Random Effects model 
xtreg log_gaspcar log_incomepcap log_relgasprice log_carpcap, re
estadd local individual "No"
estadd local year "No"
estadd local robust "No"
estimates store random_effects, title(Random Effects)


** 4.
// Estimate Fixed Effects model 
xtreg log_gaspcar log_incomepcap log_relgasprice log_carpcap, fe
estadd local individual "Yes"
estadd local year "No"
estadd local robust "No"
estimates store fixed_effects, title(Fixed Effects)


** 5.
// Generate first-difference variables
gen d_log_gaspcar = d.log_gaspcar
gen d_log_incomepcap = d.log_incomepcap
gen d_log_relgasprice = d.log_relgasprice
gen d_log_carpcap = d.log_carpcap


// Estimate First-Differences model
reg d_log_gaspcar d_log_incomepcap d_log_relgasprice d_log_carpcap
estadd local individual "No"
estadd local year "No"
estadd local robust "No"
estimates store first_diff, title(First-Differences)



** 6.
// Generate LaTeX table with results
esttab pooled_ols random_effects fixed_effects first_diff using table2.tex, cells(b(star fmt(3)) se(par fmt(2)))   ///
   legend collabels(none) varlabels(_cons constant)  ///
   stats(individual year robust r2 N vce, fmt(0 0 0 3 0) label("Indivudal fixed effects" "Year fixed effect" "Robust standard errors" R-sqr Observations)) ///
   addnote("Note: Standard errors in parentheses.") replace
   

 
// Visualize table in Stata
estout pooled_ols random_effects fixed_effects first_diff, cells(b(star fmt(3)) se(par fmt(2)))   ///
   legend collabels(none) varlabels(_cons constant)  ///
   stats(individual year robust r2 N vce, fmt(0 0 0 3 0) label("Indivudal fixed effects" "Year fixed effect" "Robust standard errors" R-sqr Observations)) ///
    replace

   

** 7.
// Estimate Pooled OLS model with robust standard errors
reg log_gaspcar log_incomepcap log_relgasprice log_carpcap, vce(robust)
estadd local individual "No"
estadd local year "No"
estadd local robust "Yes"
estimates store pooled_ols_r, title(Pooled OLS)

// Estimate Random Effects model with robust standard errors
xtreg log_gaspcar log_incomepcap log_relgasprice log_carpcap, re vce(robust)
estadd local individual "No"
estadd local year "No"
estadd local robust "Yes"
estimates store random_effects_r, title(Random Effects)

// Estimate Fixed Effects model with robust standard errors
xtreg log_gaspcar log_incomepcap log_relgasprice log_carpcap, fe vce(robust)
estadd local individual "Yes"
estadd local year "No"
estadd local robust "Yes"
estimates store fixed_effects_r, title(Fixed Effects)

// Estimate First-Differences model with robust standard errors
regress d_log_gaspcar d_log_incomepcap d_log_relgasprice d_log_carpcap, vce(robust)
estadd local individual "No"
estadd local year "No"
estadd local robust "Yes"
estimates store first_diff_r, title(First-Differences)

	
// Generate LaTeX table with results
esttab pooled_ols_r random_effects_r fixed_effects_r first_diff_r using table3.tex, cells(b(star fmt(3)) se(par fmt(2)))   ///
   legend collabels(none) varlabels(_cons constant)  ///
   stats(individual year robust r2 N vce, fmt(0 0 0 3 0) label("Indivudal fixed effects" "Year fixed effect" "Robust standard errors" R-sqr Observations)) ///
   addnote("Note: Robust standard errors in parentheses.") replace
