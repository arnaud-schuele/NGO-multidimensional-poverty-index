*Project-MPI DO-FILE (STATA)

*Automated process for identification and aggregation, with respect to OPHI (2017)
*To be used with the Excel data set from the Project-MPI

* 1.Translate the variables into English

rename Annéedentrée Firstdistrib
rename Rangdansleprogramme Hhnbr
rename Initialesdelacommune Municipality
rename Groupe Group


* 2.Translate the indicators

*Right to food
rename Fréquencedesrepas Suffoffood
rename Stabilitéetaccèsdelalimenta Stabiloffood
rename Fréquencedepréparationdesrep Freqmealprep
rename Suffisanceenprotéines Suffofprot
rename Suffisanceenvitamines Suffofvit
rename Espacementdesnaissances Infantnutri
*Right to education
rename Tauxdescolarisationprimairen Childschrate
rename Formationdelamèredanslemén Complprimstudies
*Health & Hygiene
rename Lavagedesmains Freqhandwash
rename Sanitairesaméliorés Improvsanit
rename Accèsàleaupotable Drinkwater
*Standards of living
rename Conditiondelogement Housingconds
rename Possessionsmatérielles Matassets
rename Possessionsagricoles Landownership


* 3. Create the deprivation matrix, with the respective deprivation cutoffs
* Note: missing values are coded as non-deprived

*Right to food
recode   Suffoffood (2/4=1)(1=0)(.=0), gen(hh_d_Suffood)
recode   Stabiloffood (2/4=1)(1=0)(.=0), gen(hh_d_Stabil)
recode   Freqmealprep (3/4=1)(1/2=0)(.=0), gen(hh_d_Mealpre)
recode   Suffofprot (3/4=1)(1/2=0)(.=0), gen(hh_d_Sufpro)
recode   Suffofvit (3/4=1)(1/2=0)(.=0), gen(hh_d_Sufvit)
recode   Infantnutri (3/4=1)(1/2=0)(.=0), gen(hh_d_Infnutr)
*Right to education
recode   Childschrate (2/4=1)(1=0)(.=0), gen(hh_d_Schrate)
recode   Complprimstudies (3/4=1)(1/2=0)(.=0), gen(hh_d_Primstu)
*Health & Hygiene
recode   Freqhandwash (3/4=1)(1/2=0)(.=0), gen(hh_d_Handwsh)
recode   Improvsanit (3/4=1)(1/2=0)(.=0), gen(hh_d_Sanit)
recode   Drinkwater (3/4=1)(1/2=0)(.=0), gen(hh_d_Water)
*Standards of living
recode   Housingconds (3/4=1)(1/2=0)(.=0), gen(hh_d_Housing)
recode   Matassets (3/4=1)(1/2=0)(.=0), gen(hh_d_Assets)
recode   Landownership (3/4=1)(1/2=0)(.=0), gen(hh_d_Landown)


* 4.Disclose the missing values

mdesc


* 5. Correlation: CRAMER's V
* Cramer's V describes the association among indicators. It ranges between 0 and 1: 
* - 0 for the lowest possible association between variables, and
* - 1 for the largest possible association (OPHI, 2017). 

foreach x in hh_d_Suffood hh_d_Stabil hh_d_Mealpre hh_d_Sufpro hh_d_Sufvit hh_d_Infnutr hh_d_Schrate hh_d_Primstu hh_d_Handwsh hh_d_Sanit hh_d_Water hh_d_Housing hh_d_Assets hh_d_Landown {
foreach y in hh_d_Suffood hh_d_Stabil hh_d_Mealpre hh_d_Sufpro hh_d_Sufvit hh_d_Infnutr hh_d_Schrate hh_d_Primstu hh_d_Handwsh hh_d_Sanit hh_d_Water hh_d_Housing hh_d_Assets hh_d_Landown {
if "`x'" != "`y'" {
tab `x' `y', V
}
}
}


* 6. Redundancy 
* Note: the coefficient P is defined as the ratio between:
* - the proportion of people with simultaneous deprivation in any two indicators, and
* - the lowest proportion of deprivation of those indicators independently.
* The coefficient P takes values:
* - 0% when no one is identified as deprived in both indicators being considered, and
* - 100% when every individual who is deprived in the indicator with the lowest incidence 
* of deprivation, is also deprived on the other indicator (OPHI, 2017).

foreach var1 in hh_d_Suffood hh_d_Stabil hh_d_Mealpre hh_d_Sufpro hh_d_Sufvit hh_d_Infnutr hh_d_Schrate hh_d_Primstu hh_d_Handwsh hh_d_Sanit hh_d_Water hh_d_Housing hh_d_Assets hh_d_Landown {

	foreach var2 in hh_d_Suffood hh_d_Stabil hh_d_Mealpre hh_d_Sufpro hh_d_Sufvit hh_d_Infnutr hh_d_Schrate hh_d_Primstu hh_d_Handwsh hh_d_Sanit hh_d_Water hh_d_Housing hh_d_Assets hh_d_Landown {


		* Temporal variables for the matches and mismatches of two variables *
		generate temp_1 = (`var1'==0 & `var2'==0)
		sum temp_1
		gen dd00 = r(mean)

		generate temp_2 = (`var1'==0 & `var2'==1)
		sum temp_2
		gen dd01 = r(mean)

		generate temp_3 = (`var1'==1 & `var2'==0)
		sum temp_3
		gen dd10 = r(mean)

		generate temp_4 = (`var1'==1 & `var2'==1)
		sum temp_4
		gen dd11 = r(mean)

		
		* Compute uncensored headcount ratios *
		sum `var1'
		gen h1_`var1' = r(mean)
		sum `var2'
		gen h2_`var2' = r(mean)
		

		* Compute redundancy (coefficient P) *
		egen h_min_`var1'_`var2' = rowmin(h1_`var1' h2_`var2')
		gen P_`var1'_`var2' = dd11 / h_min_`var1'_`var2'
		
		
		* Alternative way to compute Cramer Vs (with weights) from these temporal variables *
		gen CV_`var1'_`var2' = (dd00*dd11 - dd01 *dd10)/ sqrt(h1_`var1'*(1-h1_`var1')*h2_`var2'*(1-h2_`var2'))

		drop temp* dd* h1_* h2_* h_min* 
		
		}

	
	* Present results *
	sum P_* CV_*
	}	

	
* 7. Uncensored Headcount Ratios

foreach var in hh_d_Suffood hh_d_Stabil hh_d_Mealpre hh_d_Sufpro hh_d_Sufvit hh_d_Infnutr hh_d_Schrate hh_d_Primstu hh_d_Handwsh hh_d_Sanit hh_d_Water hh_d_Housing hh_d_Assets hh_d_Landown {  

	sum    `var' 
	gen	 uncen_H_`var' = r(mean)*100
	lab var  uncen_H_`var'"Uncensored Headcount ratio"
	}
	
	
* 8. Setting respective weights 

foreach var in hh_d_Suffood hh_d_Stabil hh_d_Mealpre hh_d_Sufpro hh_d_Sufvit hh_d_Infnutr {	
	
	gen	 w_`var' = 1/9
	lab var  w_`var' "Weight `var'"
	}


foreach var in hh_d_Schrate hh_d_Primstu hh_d_Handwsh hh_d_Sanit hh_d_Water hh_d_Housing hh_d_Assets hh_d_Landown {	
	
	gen 	w_`var' = 1/24
	lab var  w_`var' "Weight `var'"
	}

	
* 9. Weighted deprivation matrix 

foreach var in hh_d_Suffood hh_d_Stabil hh_d_Mealpre hh_d_Sufpro hh_d_Sufvit hh_d_Infnutr hh_d_Schrate hh_d_Primstu hh_d_Handwsh hh_d_Sanit hh_d_Water hh_d_Housing hh_d_Assets hh_d_Landown {	

	gen	 g0_w_`var' = `var' * w_`var'
	lab var  g0_w_`var' "Weigthed Deprivation of `var'"
	}

	
* 10. Counting vector
 
egen	 c_vector = rowtotal(g0_w_*)
lab var  c_vector "Counting Vector"
tab	 c_vector , m


* 11. Identification using different poverty cutoffs (k)

forvalue k = 10(10)100 {

	gen	 multid_poor_`k' = (c_vector >= `k'/100)
	lab var  multid_poor_`k' "Poverty Identification with k=`k'%"
	}

	
* 12. Censored deprivation vector
* Note: generate the censored vector of individual weighted deprivation score, 'c(k)'

forvalue k = 10(10)100 {

	gen	 cens_c_vector_`k' = c_vector
	replace  cens_c_vector_`k' = 0 if multid_poor_`k'==0 
	}

	
* 13. M0, H and A for all the possible cutoffs
* Note: by computing the mean of the identification vector, the individual deprivation share, 
* and the individual censored c vector at any level of k, one can obtain the multidimensional headcount 
* ratio (H), the intensity of poverty among the poor (A), and the adjusted headcount ratio (M0), respectively.

sum  multid_poor_* , sep(15)

forvalue  k = 10(10)100 {
	sum  cens_c_vector_`k' if multid_poor_`k'==1 , sep(15)
	}

forvalue  k = 10(10)100 {
	sum  cens_c_vector_`k' , sep(15)
	}


* 14. M0, H and A for k = 30% 
* Note: k = 30% was chosen for the PASAB-MPI (example)

local k = 30


* 15. Censored deprivation matrix

foreach var in hh_d_Suffood hh_d_Stabil hh_d_Mealpre hh_d_Sufpro hh_d_Sufvit hh_d_Infnutr hh_d_Schrate hh_d_Primstu hh_d_Handwsh hh_d_Sanit hh_d_Water hh_d_Housing hh_d_Assets hh_d_Landown {

	gen	 g0_`k'_`var' = `var'
	replace  g0_`k'_`var' = 0 if multid_poor_`k'==0
	}

	
* 16. Headcount ration of multidimensional poverty for k = 30% (H) 

sum	 multid_poor_`k' 
gen	 H = r(mean)*100
lab var  H "Headcount Ratio (H): % Population in multidimensional poverty"


* 17. Intensity of poverty among the poor for k = 30% (A)

sum	 cens_c_vector_`k' if multid_poor_`k'==1
gen	 A = r(mean)*100
lab var  A  "Intensity of deprivation among the poor (A): Average % of weighted deprivations"


* 18. Adjusted headcount ratio for k = 30%  (M0)

sum	 cens_c_vector_`k' 
gen	 M0 = r(mean)
lab var  M0 "Adjusted Headcount Ratio (M0 = H*A): Range 0 to 1"


* 19. Censored headcount ratios

foreach var in hh_d_Suffood hh_d_Stabil hh_d_Mealpre hh_d_Sufpro hh_d_Sufvit hh_d_Infnutr hh_d_Schrate hh_d_Primstu hh_d_Handwsh hh_d_Sanit hh_d_Water hh_d_Housing hh_d_Assets hh_d_Landown {	

	sum	 g0_`k'_`var' 
	gen	 cen_H_`var' = r(mean)*100 
	lab var  cen_H_`var' "Censored Headcount Ratio"
	}

	sum  uncen_H_* cen_H_* , sep(15)

	
* 20. Dimensional breakdown: percentage contributions

foreach var  in hh_d_Suffood hh_d_Stabil hh_d_Mealpre hh_d_Sufpro hh_d_Sufvit hh_d_Infnutr hh_d_Schrate hh_d_Primstu hh_d_Handwsh hh_d_Sanit hh_d_Water hh_d_Housing hh_d_Assets hh_d_Landown {	

	gen	 perc_cont_`var' = (cen_H_`var' * w_`var') / M0
	lab var  perc_cont_`var' "Percentage contribution to M0"
	}

sum  perc_cont_* , sep(15)


* 21. Subgroup decomposition 
* Note: poverty can be decomposed by any variable of the data set used (here: Municipality)

* Uncensored Headcount Ratios by Municipality
tabstat  hh_d_*, by (Municipality)

* Incidence of Poverty (H) by Municipality
tabstat  multid_poor_`k' , by (Municipality)

* Intensity of Poverty (A) by Municipality
tabstat  cens_c_vector_`k' if multid_poor_`k'==1, by(Municipality)

* Adjusted Headcount Ratio (M0) by Municipality 
tabstat  cens_c_vector_`k', by(Municipality)

* Censored Headcount Ratios by region //regions are labeled from 1 to 10, so adjust r= accordingly as 1/10
foreach var  in hh_d_Suffood hh_d_Stabil hh_d_Mealpre hh_d_Sufpro hh_d_Sufvit hh_d_Infnutr hh_d_Schrate hh_d_Primstu hh_d_Handwsh hh_d_Sanit hh_d_Water hh_d_Housing hh_d_Assets hh_d_Landown {	
	forvalue  r = 1/10 {
		sum	 g0_`k'_`var' if Municipality==`r'
		gen	 cen_H_r`r'_`var' = r(mean)*100
		lab var  cen_H_r`r'_`var'  "Censored Headcount Ratio - Municipality `r'"
		}
	}
	
* 22. Contributions by Municipality

forvalue  r = 1/10 {
	foreach var in hh_d_Suffood hh_d_Stabil hh_d_Mealpre hh_d_Sufpro hh_d_Sufvit hh_d_Infnutr hh_d_Schrate hh_d_Primstu hh_d_Handwsh hh_d_Sanit hh_d_Water hh_d_Housing hh_d_Assets hh_d_Landown  {	
		sum	 cens_c_vector_`k' if Municipality==`r'
		loc	 M0_r`r' = r(mean)
		gen	 perc_cont_r`r'_`var' = (cen_H_r`r'_`var' * w_`var') / `M0_r`r''
		lab var  perc_cont_r`r'_`var' "Percentage contribution to M0 - Municipality `r'"
		}
	}

sum  perc_cont_r* , sep(14)

/*Alternatively, subgroup decomposition by groups  
* Note: decomposition used here to identify differences between the control group and the treated group at t1

* Uncensored Headcount Ratios by Group
tabstat  hh_d_*, by (Group)

* Incidence of Poverty (H) by Group
tabstat  multid_poor_`k' , by (Group)

* Intensity of Poverty (A) by Group
tabstat  cens_c_vector_`k' if multid_poor_`k'==1, by(Group)

* Adjusted Headcount Ratio (M0) by Group 
tabstat  cens_c_vector_`k', by(Group)

* Censored Headcount Ratios by Group
foreach var in hh_d_Suffood hh_d_Stabil hh_d_Mealpre hh_d_Sufpro hh_d_Sufvit hh_d_Infnutr hh_d_Schrate hh_d_Primstu hh_d_Handwsh hh_d_Sanit hh_d_Water hh_d_Housing hh_d_Assets hh_d_Landown {	
	forvalue r = 1/2 {
		sum 	 g0_`k'_`var' if Group ==`r'
		gen	 cen_H_r`r'_`var' = r(mean)*100
		lab var  cen_H_r`r'_`var'  "Censored Headcount Ratio - Group `r'"
		 }
	 }

sum   cen_H_r*, sep(14)


* Contributions by Group

forvalue r = 1/2 {
	foreach var in hh_d_Suffood hh_d_Stabil hh_d_Mealpre hh_d_Sufpro hh_d_Sufvit hh_d_Infnutr hh_d_Schrate hh_d_Primstu hh_d_Handwsh hh_d_Sanit hh_d_Water hh_d_Housing hh_d_Assets hh_d_Landown {	
		sum	 cens_c_vector_`k' if Group==`r'
		loc	 M0_r`r' = r(mean)
		gen	 perc_cont_r`r'_`var' = (cen_H_r`r'_`var' * w_`var') / `M0_r`r''
		lab var  perc_cont_r`r'_`var' "Percentage contribution to M0 - Group `r'"
		 }
	 }

sum  perc_cont_r* , sep(14)
*/
