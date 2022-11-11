** More stress less voice **
** Burciu & Hutter, 2022 **

	clear
	set more off
	cd "C:\"
	use WelleI_Final.dta, clear
	set scheme plotplainblind
	svyset [pweight=wgt_1], strata(cntry)

	** Recode Control Variables **
	
	*migration background 
	gen migration_bkg=0
	replace migration_bkg=1 if citizen==3 | citizen_mother==0 | citizen_father==0
	lab var migration_bkg "do you have a migration background?"
	lab val migration_bkg yesno
	
	*income
	recode income 1 2=0 3 4=1, gen(income_dy) 
	lab def label_income 0 "Good" 1 "Bad" 
	lab val income_dy label_income

	** Recode Resource Variables **
	
	*childcare
	gen child_care=.
	replace child_care=0 if help_childcare_dy==0 & home_child==0
	replace child_care=1 if help_childcare_dy==0 & home_child==1
	replace child_care=2 if help_childcare_dy==1 & home_child==0
	replace child_care=3 if help_childcare_dy==1 & home_child==1
	lab def child_lab 0 "no childcare" 1 "care for own child" 2 "care for others' child" 3 "care for onw and others child"
	lab val child_care child_lab


	*childcare for family/friends outside of the household
	gen outside_child=help_childcare_dy
	replace outside_child=0 if support_fam!=1
	egen outside_support=rowmax(help_shopping_dy help_general_dy)
	replace outside_support=0 if support_fam!=1

	*care work
	gen care_work=.
	replace care_work=0 if home_care==0 & outside_support==0
	replace care_work=1 if home_care==0 & outside_support==1
	replace care_work=2 if home_care==1 & outside_support==0
	replace care_work=3 if home_care==1 & outside_support==1
	lab def care_lab 0 "no care work" 1 "care outside household" 2 "care inside household" 3 "care inside and outside household"
	lab val care_work care_lab

	
	** Recode Incentive Variables **
	
	*income change
	recode income_chng 3 4 5=0 1 2=1 , gen(income_chng_lev2)
	lab var income_chng_lev2 "What was the impact of corona on finances in your household?"
	lab def label_income_chng 0 "no change/positive" 1 "negative", modify
	lab val income_chng_lev2 label_income_chng


	*health measures
	recode msr_health 3=0 1 2=1 4 5=2, gen(msr_health_lev3)
	lab def lab1_health 0 "health: appropriate" 1 "health: insufficient" 2 		"health: too strong", modify
	lab val msr_health_lev3 lab1_health 


	*economic measures
	recode msr_eco 3=0 1 2=1 4 5=2, gen(msr_eco_lev3)
	lab def lab1_economy 0 "economy: appropriate" 1 "eco: insufficient" 2 "eco: too strong", modify
	lab val msr_eco_lev3 lab1_economy 

*******************************************************************************

	** MAIN ANALYSIS **
	
	
	** Aggregate Levels of Participarion: Figure 1 **
	
	preserve

	foreach var of varlist help_civic_dy help_civic_past_dy pol_protest_dy 	pol_protest_past_dy pol_inst_dy pol_inst_past_dy pol_online_dy 			pol_online_past_dy {
	replace `var'=`var'*wgt_1 // to have a weighted values of participation. Given it's 1 and 0, it will either be the weight, or 0
}

	collapse (sum)  help_civic_dy help_civic_past_dy pol_protest_dy pol_protest_past_dy pol_inst_dy pol_inst_past_dy pol_online_dy pol_online_past_dy wgt_1, by (gndr)

	foreach var of varlist  help_civic_dy help_civic_past_dy pol_protest_dy pol_protest_past_dy pol_inst_dy pol_inst_past_dy pol_online_dy pol_online_past_dy {
	replace `var'=`var'*100/wgt_1
}
	drop wgt_1

	save "participation_share.dta", replace
	rename help_civic_dy pol_1
	rename pol_protest_dy pol_2
	rename pol_inst_dy pol_3
	rename pol_online_dy pol_4
	rename help_civic_past_dy past_1
	rename pol_protest_past_dy past_2
	rename pol_inst_past_dy past_3
	rename pol_online_past_dy past_4

	reshape long pol_ past_, i(gndr) j(participation_type)
	lab def label_part 1 "Civic engagemement" 2 "Protest" 3 "Institutional participation" 4 "Online participation"
	lab val participation_type label_part

	gen men=1 if gndr==1
	gen women=1 if gndr==2

	twoway bar pol_ participation_type, by(gndr) horizontal fintensity(inten20) || pcarrow participation_type past_ participation_type pol_, msize (medlarge) barbsize(medlarge) color(red)

	twoway bar pol_ gndr, by(participation_type) horizontal fintensity(inten20) || pcarrow gndr past_ gndr pol_, msize (medlarge) barbsize(medlarge) color(red)

	restore

	** The impact of gender and other social-structural variables on changes in participation during the pandemic **
		// control for age, education, migration background, east/west Germany
			
		gen past1=.
		replace past1=pol_inst_past_dy
		logit pol_inst_dy i.past1 i.gndr i.edu_lev3 i.age_level i.migration_bkg i.east_de i.income_chng_lev2 [pweight=wgt_1]
		margins, dydx(_all) post
		estimates store inst
		replace past1=pol_online_past_dy
		logit pol_online_dy i.past1 i.gndr i.edu_lev3 i.age_level i.migration_bkg i.east_de i.income_chng_lev2 [pweight=wgt_1]
		margins, dydx(_all) post
		estimates store online
		replace past1=pol_protest_past_dy
		logit pol_protest_dy i.past1 i.gndr i.edu_lev3 i.age_level i.migration_bkg i.east_de i.income_chng_lev2 [pweight=wgt_1]
		margins, dydx(_all) post
		estimates store protest
		replace past1=help_civic_past
		logit help_civic_dy i.past1 i.gndr i.edu_lev3 i.age_level i.migration_bkg i.east_de i.income_chng_lev2 [pweight=wgt_1]
		margins, dydx(_all) post
		estimates store civic
		coefplot  civic  online inst protest,  drop(*.past1 _cons) xline(0) levels(95) xlabel(, labsize(medsmall)) ylabel(, labsize(small) val) //Figure 2 - for the main text
		coefplot  civic  online inst protest,  drop( _cons) xline(0) levels(95) xlabel(, labsize(medsmall)) ylabel(, labsize(small) val) //for the appendix

		
	** The impact of changes in resources and incentives on participation in the pandemic **
		
		label var home_child "childcare at home"
		label var home_care "care work at home"
		label var outside_child "childcare fam/friends"
		label var outside_support "care work fam/friends"
		replace past1=pol_inst_past_dy
		logit pol_inst_dy  i.gndr i.edu_lev3 i.age_level i.migration_bkg i.east_de i.income_chng_lev2  i.msr_health_lev3 i.msr_eco_lev3 home_child home_care outside_child outside_support [pweight=wgt_1]
		margins, dydx(_all) post
		estimates store inst
		replace past1=pol_online_past_dy
		logit pol_online_dy  i.gndr i.edu_lev3 i.age_level i.migration_bkg i.east_de i.income_chng_lev2 i.msr_health_lev3 i.msr_eco_lev3 home_child home_care outside_child outside_support [pweight=wgt_1]
		margins, dydx(_all) post
		estimates store online
		replace past1=pol_protest_past_dy
		logit pol_protest_dy  i.gndr i.edu_lev3 i.age_level i.migration_bkg i.east_de i.income_chng_lev2 i.msr_health_lev3 i.msr_eco_lev3 home_child home_care outside_child outside_support [pweight=wgt_1]
		margins, dydx(_all) post 
		estimates store protest
		replace past1=help_civic_past
		logit help_civic_dy  i.gndr i.edu_lev3 i.age_level i.migration_bkg i.east_de i.income_chng_lev2 i.msr_health_lev3 i.msr_eco_lev3 home_child home_care outside_child outside_support [pweight=wgt_1]
		margins, dydx(_all) post
		estimates store civic

		coefplot  civic  online inst protest,  drop(past1  edu_lev3 age_level migration_bkg east_de income_chng_lev2  home_child home_care outside_child outside_support _cons) keep( *.gndr *.msr_health_lev3 *.msr_eco_lev3 ) xline(0) levels(95) xlabel(, labsize(medsmall)) ylabel(, labsize(small) val) title("incentives") name (figure3d)
		graph save Graph "C:\Users\roxan\Nextcloud\SolZiv-Polsoz\analyses\de-activation\graphs\figure3b.gph", replace
		coefplot civic  online inst protest,  drop(past  edu_lev3 age_level migration_bkg east_de income_chng_lev2 *.msr_health_lev3 *.msr_eco_lev3 _cons) keep( *.gndr  home_child home_care outside_child outside_support ) xline(0) levels(95) xlabel(, labsize(medsmall)) ylabel(, labsize(small) val) title("resources") name (figure3c)
		graph save Graph "C:\Users\roxan\Nextcloud\SolZiv-Polsoz\analyses\de-activation\graphs\figure3a.gph", replace

		graph combine figure3c figure3d //Figure 3

		
	** Mediation Analysis **
	
	*Mediator: childcare at home
	sem (home_child <- gndr)(pol_inst_dy <- gndr home_child) (pol_inst_dy <- pol_inst_past_dy) [pweight = wgt_1]
	estat teffects

	sem (home_child <- gndr)(pol_online_dy <- gndr home_child) (pol_online_dy <- pol_online_past_dy) [pweight = wgt_1]
	estat teffects

	sem (home_child <- gndr)(pol_protest_dy <- gndr home_child) (pol_protest_dy <- pol_protest_past_dy) [pweight = wgt_1]
	estat teffects

	sem (home_child <- gndr)(help_civic_dy <- gndr home_child)(help_civic_dy<- help_civic_past_dy) [pweight = wgt_1]
	estat teffects

	*Mediator: cildcare for family and friends
	sem (outside_child <- gndr)(pol_inst_dy <- gndr outside_child)  (pol_inst_dy <- pol_inst_past_dy) [pweight = wgt_1]
	estat teffects

	sem (outside_child <- gndr)(pol_online_dy <- gndr outside_child) (pol_online_dy <- pol_online_past_dy) [pweight = wgt_1]
	estat teffects
	
	sem (outside_child <- gndr)(pol_protest_dy <- gndr outside_child) (pol_protest_dy <- pol_protest_past_dy)  [pweight = wgt_1]
	estat teffects

	sem (outside_child <- gndr)(help_civic_dy <- gndr outside_child) (help_civic_dy<- help_civic_past_dy) [pweight = wgt_1]
	estat teffects

	*Mediator: care work at home
	sem (home_care <- gndr)(pol_inst_dy <- gndr home_care)  (pol_inst_dy <- pol_inst_past_dy) [pweight = wgt_1]
	estat teffects

	sem (home_care <- gndr)(pol_online_dy <- gndr home_care) (pol_online_dy <- pol_online_past_dy) [pweight = wgt_1]
	estat teffects

	sem (home_care <- gndr)(pol_protest_dy <- gndr home_care) (pol_protest_dy <- pol_protest_past_dy)  [pweight = wgt_1]
	estat teffects

	sem (home_care <- gndr)(help_civic_dy <- gndr home_care)(help_civic_dy<- help_civic_past_dy) [pweight = wgt_1]
	estat teffects

	*Mediator: care work for family/friends
	sem (outside_support <- gndr)(pol_inst_dy <- gndr outside_support)  (pol_inst_dy <- pol_inst_past_dy) [pweight = wgt_1]
	estat teffects

	sem (outside_support <- gndr)(pol_online_dy <- gndr outside_support) (pol_online_dy <- pol_online_past_dy)[pweight = wgt_1]
	estat teffects

	sem (outside_support <- gndr)(pol_protest_dy <- gndr outside_support) (pol_protest_dy <- pol_protest_past_dy)  [pweight = wgt_1]
	estat teffects

	sem (outside_support <- gndr)(help_civic_dy <- gndr outside_support)(help_civic_dy<- help_civic_past_dy) [pweight = wgt_1]
	estat teffects

	*Mediator: economical measures
	gen eco_strong=0
	replace eco_strong=1 if msr_eco_lev3==2

	sem (eco_strong <- gndr)(pol_inst_dy <- gndr eco_strong)  (pol_inst_dy <- pol_inst_past_dy) [pweight = wgt_1]
	estat teffects

	sem (eco_strong <- gndr)(pol_online_dy <- gndr eco_strong) (pol_online_dy <- pol_online_past_dy) [pweight = wgt_1]
	estat teffects

	sem (eco_strong <- gndr)(pol_protest_dy <- gndr eco_strong) (pol_protest_dy <- pol_protest_past_dy)  [pweight = wgt_1]
	estat teffects

	sem (eco_strong <- gndr)(help_civic_dy <- gndr eco_strong )(help_civic_dy<- help_civic_past_dy) [pweight = wgt_1]
	estat teffects
	
	*Mediator: health measures
	gen health_strong=0
	replace health_strong=1 if msr_health_lev3==2

	sem (health_strong <- gndr)(pol_inst_dy <- gndr health_strong)  (pol_inst_dy <- pol_inst_past_dy) [pweight = wgt_1]
	estat teffects

	sem (health_strong <- gndr)(pol_online_dy <- gndr health_strong) (pol_online_dy <- pol_online_past_dy) [pweight = wgt_1]
	estat teffects

	sem (health_strong <- gndr)(pol_protest_dy <- gndr health_strong)(pol_protest_dy <- pol_protest_past_dy) [pweight = wgt_1]
	estat teffects

	sem (health_strong <- gndr)(help_civic_dy <- gndr health_strong) (help_civic_dy<- help_civic_past_dy) [pweight = wgt_1]
	estat teffects
	
	** The impact of changes in resources and incentives on participation in the pandemic – split regressions for men and women **
	
replace past1=pol_inst_past_dy
logit pol_inst_dy  i.gndr i.edu_lev3 i.age_level i.migration_bkg i.east_de i.income_chng_lev2 home_child home_care outside_child outside_support  i.msr_health_lev3 i.msr_eco_lev3  if gndr==1 [pweight=wgt_1]
margins, dydx(_all) post
estimates store men
logit pol_inst_dy  i.gndr i.edu_lev3 i.age_level i.migration_bkg i.east_de i.income_chng_lev2 home_child home_care outside_child outside_support i.msr_health_lev3 i.msr_eco_lev3  if gndr==2 [pweight=wgt_1]
margins, dydx(_all) post
estimates store women
coefplot  men women,  drop(past1  edu_lev3 age_level migration_bkg east_de income_chng_lev2 _cons) keep( *.gndr *.msr_health_lev3 *.msr_eco_lev3 home_child home_care outside_child outside_support ) xline(0) levels(95) xlabel(, labsize(medsmall)) ylabel(, labsize(small) val) title("institutional") name (institutional)
graph save Graph "C:\Users\roxan\Nextcloud\SolZiv-Polsoz\analyses\de-activation\graphs\institutional.gph", replace

replace past1=pol_protest_past_dy
logit pol_protest_dy  i.gndr i.edu_lev3 i.age_level i.migration_bkg i.east_de i.income_chng_lev2 home_child home_care outside_child outside_support i.msr_health_lev3 i.msr_eco_lev3  if gndr==1 [pweight=wgt_1]
margins, dydx(_all) post
estimates store men
logit pol_protest_dy  i.gndr i.edu_lev3 i.age_level i.migration_bkg i.east_de i.income_chng_lev2 home_child home_care outside_child outside_support i.msr_health_lev3 i.msr_eco_lev3  if gndr==2 [pweight=wgt_1]
margins, dydx(_all) post
estimates store women
coefplot  men women,  drop(past1  edu_lev3 age_level migration_bkg east_de  income_chng_lev2 _cons) keep( *.gndr *.msr_health_lev3 *.msr_eco_lev3  home_child home_care outside_child outside_support ) xline(0) levels(95) xlabel(, labsize(medsmall)) ylabel(, labsize(small) val) title("protest") name (protest)
graph save Graph "C:\Users\roxan\Nextcloud\SolZiv-Polsoz\analyses\de-activation\graphs\protest.gph", replace

replace past1=pol_online_past_dy
logit pol_online_dy  i.gndr i.edu_lev3 i.age_level i.migration_bkg i.east_de i.income_chng_lev2 home_child home_care outside_child outside_support i.msr_health_lev3 i.msr_eco_lev3  if gndr==1 [pweight=wgt_1]
margins, dydx(_all) post
estimates store men
logit pol_online_dy  i.gndr i.edu_lev3 i.age_level i.migration_bkg i.east_de i.income_chng_lev2 home_child home_care outside_child outside_support i.msr_health_lev3 i.msr_eco_lev3  if gndr==2 [pweight=wgt_1]
margins, dydx(_all) post
estimates store women
coefplot  men women,  drop(past1  edu_lev3 age_level migration_bkg east_de  income_chng_lev2_cons) keep( *.gndr *.msr_health_lev3 *.msr_eco_lev3 home_child home_care outside_child outside_support ) xline(0) levels(95) xlabel(, labsize(medsmall)) ylabel(, labsize(small) val) title("online") name (online)
graph save Graph "C:\Users\roxan\Nextcloud\SolZiv-Polsoz\analyses\de-activation\graphs\online.gph", replace

replace past1=help_civic_past_dy
logit help_civic_dy  i.gndr i.edu_lev3 i.age_level i.migration_bkg i.east_de i.income_chng_lev2 home_child home_care outside_child outside_support i.msr_health_lev3 i.msr_eco_lev3  if gndr==1 [pweight=wgt_1]
margins, dydx(_all) post
estimates store men
logit help_civic_dy  i.gndr i.edu_lev3 i.age_level i.migration_bkg i.east_de i.income_chng_lev2 home_child home_care outside_child outside_support i.msr_health_lev3 i.msr_eco_lev3  if gndr==2 [pweight=wgt_1]
margins, dydx(_all) post
estimates store women
coefplot  men women,  drop(past1  edu_lev3 age_level migration_bkg east_de income_chng_lev2 _cons) keep( *.gndr *.msr_health_lev3 *.msr_eco_lev3 home_child home_care outside_child outside_support ) xline(0) levels(95) xlabel(, labsize(medsmall)) ylabel(, labsize(small) val) title("civic") name (civic)
graph save Graph "C:\Users\roxan\Nextcloud\SolZiv-Polsoz\analyses\de-activation\graphs\civic.gph", replace

graph combine civic online institutional protest //Figure 5

	** Interaction Analysis **

	*Childcare at home
logit pol_inst_dy i.gndr#i.home_child i.gndr i.home_child i.edu_lev3 i.age_level i.migration_bkg i.east_de [pweight=wgt_1]
margins i.gndr#home_child, post
estimates store inst

logit pol_online_dy i.gndr i.gndr#home_child i.home_child i.edu_lev3 i.age_level i.migration_bkg i.east_de [pweight=wgt_1]
margins i.gndr#home_child, post
estimates store online

logit pol_protest_dy i.gndr i.gndr#home_child i.home_child i.edu_lev3 i.age_level i.migration_bkg i.east_de [pweight=wgt_1]
margins i.gndr#home_child, post
estimates store protest

logit help_civic_dy i.gndr i.gndr#home_child i.home_child i.edu_lev3 i.age_level i.migration_bkg i.east_de [pweight=wgt_1]
margins i.gndr#home_child, post
estimates store civic

coefplot civic online inst protest,  xlabel(, labsize(medsmall)) ylabel(, labsize(small) val) xline(0) levels(95)

	*Childcare for fam/friends
logit pol_inst_dy i.gndr#outside_child i.gndr i.outside_child i.edu_lev3 i.age_level i.migration_bkg i.east_de [pweight=wgt_1]
margins i.gndr#outside_child, post
estimates store inst

logit pol_online_dy i.gndr#outside_child i.gndr i.outside_child i.edu_lev3 i.age_level i.migration_bkg i.east_de [pweight=wgt_1]
margins i.gndr#outside_child, post
estimates store online

logit pol_protest_dy i.gndr#outside_child i.gndr i.outside_child i.edu_lev3 i.age_level i.migration_bkg i.east_de [pweight=wgt_1]
margins i.gndr#outside_child, post
estimates store protest

logit help_civic_dy i.gndr#outside_child i.gndr i.outside_child i.edu_lev3 i.age_level i.migration_bkg i.east_de [pweight=wgt_1]
margins i.gndr#outside_child, post
estimates store civic

coefplot civic online inst protest,  xlabel(, labsize(medsmall)) ylabel(, labsize(small) val) xline(0) levels(95)

	*Care work at home
logit pol_inst_dy i.gndr#home_care i.gndr i.home_care i.edu_lev3 i.age_level i.migration_bkg i.east_de [pweight=wgt_1]
margins i.gndr#home_care, post
estimates store inst

logit pol_online_dy i.gndr#home_care i.gndr i.home_care i.edu_lev3 i.age_level i.migration_bkg i.east_de [pweight=wgt_1]
margins i.gndr#home_care, post
estimates store online

logit pol_protest_dy i.gndr#home_care i.gndr i.home_care i.edu_lev3 i.age_level i.migration_bkg i.east_de [pweight=wgt_1]
margins i.gndr#home_care, post
estimates store protest

logit help_civic_dy i.gndr#home_care i.gndr i.home_care i.edu_lev3 i.age_level i.migration_bkg i.east_de [pweight=wgt_1]
margins i.gndr#home_care, post
estimates store civic

coefplot civic online inst protest,  xlabel(, labsize(medsmall)) ylabel(, labsize(small) val) xline(0) levels(95)

	*Care work for friends and family
logit pol_inst_dy i.gndr#outside_support i.gndr i.outside_support i.edu_lev3 i.age_level i.migration_bkg i.east_de [pweight=wgt_1]
margins i.gndr#outside_support, post
estimates store inst

logit pol_online_dy i.gndr#outside_support i.gndr i.outside_support i.edu_lev3 i.age_level i.migration_bkg i.east_de [pweight=wgt_1]
margins i.gndr#outside_support, post
estimates store online

logit pol_protest_dy i.gndr#outside_support i.gndr i.outside_support i.edu_lev3 i.age_level i.migration_bkg i.east_de [pweight=wgt_1]
margins i.gndr#outside_support, post
estimates store protest

logit help_civic_dy i.gndr#outside_support i.gndr i.outside_support i.edu_lev3 i.age_level i.migration_bkg i.east_de [pweight=wgt_1]
margins i.gndr#outside_support, post
estimates store civic

coefplot civic online inst protest,  xlabel(, labsize(medsmall)) ylabel(, labsize(small) val) xline(0) levels(95)

graph combine interaction.gph interaction2.gph interaction3.gph interaction4.gph //Figure A13.1.

	*Economical Measures
logit pol_inst_dy i.gndr#msr_eco_lev3 i.gndr i.msr_eco_lev3 i.edu_lev3 i.age_level i.migration_bkg i.east_de [pweight=wgt_1]
margins i.gndr#msr_eco_lev3, post
estimates store inst

logit pol_online_dy i.gndr#msr_eco_lev3 i.gndr i.msr_eco_lev3 i.edu_lev3 i.age_level i.migration_bkg i.east_de [pweight=wgt_1]
margins i.gndr#msr_eco_lev3, post
estimates store online

logit pol_protest_dy i.gndr#msr_eco_lev3 i.gndr i.msr_eco_lev3 i.edu_lev3 i.age_level i.migration_bkg i.east_de [pweight=wgt_1]
margins i.gndr#msr_eco_lev3, post
estimates store protest

logit help_civic_dy i.gndr#msr_eco_lev3 i.gndr i.msr_eco_lev3 i.edu_lev3 i.age_level i.migration_bkg i.east_de [pweight=wgt_1]
margins i.gndr#msr_eco_lev3, post
estimates store civic

coefplot civic online inst protest,  xlabel(, labsize(medsmall)) ylabel(, labsize(small) val) xline(0) levels(95)

	*Health Measures
logit pol_inst_dy i.gndr#msr_health_lev3 i.gndr i.msr_health_lev3 i.edu_lev3 i.age_level i.migration_bkg i.east_de [pweight=wgt_1]
margins i.gndr#msr_health_lev3, post
estimates store inst

logit pol_online_dy i.gndr#msr_health_lev3 i.gndr i.msr_health_lev3 i.edu_lev3 i.age_level i.migration_bkg i.east_de [pweight=wgt_1]
margins i.gndr#msr_health_lev3, post
estimates store online

logit pol_protest_dy i.gndr#msr_health_lev3 i.gndr i.msr_health_lev3 i.edu_lev3 i.age_level i.migration_bkg i.east_de [pweight=wgt_1]
margins i.gndr#msr_health_lev3, post
estimates store protest

logit help_civic_dy i.gndr#msr_health_lev3 i.gndr i.msr_health_lev3 i.edu_lev3 i.age_level i.migration_bkg i.east_de [pweight=wgt_1]
margins i.gndr#msr_health_lev3, post
estimates store civic

coefplot civic online inst protest,  xlabel(, labsize(medsmall)) ylabel(, labsize(small) val) xline(0) levels(95)

graph combine interaction5.gph interaction6.gph //Figure A13.13
	
*******************************************************************************

	** APPENDIX **
	
	** Descriptive Statistics **
	
	*dependent variables
	sum help_civic_dy pol_protest_dy pol_inst_dy pol_online_dy 		help_civic_past_dy pol_protest_past_dy pol_inst_past_dy pol_online_past_dy

	*independent variables
	sum outside_child home_child outside_support home_care
	
	
	** Additive indices as DV for main models ** 	
	
	*Create variables
	gen pol_inst = pol_cntct_polit + pol_party
	gen pol_online = pol_post_online + pol_demo_online
	gen pol_protest = pol_demo + pol_demo_illegal
	gen help_civic = help_volunteer + help_donation
	
	gen pol_inst_past = pol_cntct_polit_past + pol_party_past
	gen pol_online_past = pol_post_online_past + pol_demo_online_past
	gen pol_protest_past = pol_demo_past + pol_demo_illegal_past
	gen help_civic_past = help_volunteer_past + help_donation_past
	
	*Frequency Distributions of Dependent Variables: Figure A4*
	hist pol_inst, percent color(gray) width(0.8)
	hist pol_online, percent color(gray) width(0.8)
	hist pol_protest, percent color(gray) width(0.8)
	hist help_civic, percent color(gray) width(0.8)
	
	*Social-Strcutural Variables
regress pol_inst pol_inst_past i.gndr i.edu_lev3 i.age_level i.migration_bkg i.east_de i.income_chng_lev2 [pweight=wgt_1]
regress pol_online pol_online_past i.gndr i.edu_lev3 i.age_level i.migration_bkg i.east_de i.income_chng_lev2 [pweight=wgt_1]
regress pol_protest pol_protest_past i.gndr i.edu_lev3 i.age_level i.migration_bkg i.east_de i.income_chng_lev2 [pweight=wgt_1]
regress help_civic help_civic_past i.gndr i.edu_lev3 i.age_level i.migration_bkg i.east_de i.income_chng_lev2 [pweight=wgt_1]

	*Resources and incentives
regress pol_inst pol_inst_past i.gndr i.edu_lev3 i.age_level i.migration_bkg i.east_de i.income_chng_lev2  i.msr_health_lev3 i.msr_eco_lev3 home_child home_care outside_child outside_support [pweight=wgt_1]
regress pol_online pol_online_past i.gndr i.edu_lev3 i.age_level i.migration_bkg i.east_de i.income_chng_lev2 i.msr_health_lev3 i.msr_eco_lev3 home_child home_care outside_child outside_support [pweight=wgt_1]
regress pol_protest pol_protest_past i.gndr i.edu_lev3 i.age_level i.migration_bkg i.east_de i.income_chng_lev2 i.msr_health_lev3 i.msr_eco_lev3 home_child home_care outside_child outside_support [pweight=wgt_1]
regress help_civic help_civic_past i.gndr i.edu_lev3 i.age_level i.migration_bkg i.east_de i.income_chng_lev2 i.msr_health_lev3 i.msr_eco_lev3 home_child home_care outside_child outside_support [pweight=wgt_1]

	
	** Main models with ages as a continuous variable **

	
	*Social-Structural Variables
logit pol_inst_dy i.pol_inst_past_dy i.gndr i.edu_lev3 c.age c.age#c.age i.migration_bkg i.east_de i.income_chng_lev2 [pweight=wgt_1]
margins, dydx(_all) post
logit pol_online_dy i.pol_online_past_dy i.gndr i.edu_lev3 c.age c.age#c.age i.migration_bkg i.east_de i.income_chng_lev2 [pweight=wgt_1]
margins, dydx(_all) post
logit pol_demo_dy i.pol_demo_past_dy i.gndr i.edu_lev3 c.age c.age#c.age i.migration_bkg i.east_de i.income_chng_lev2 [pweight=wgt_1]
margins, dydx(_all) post
logit help_civic_dy i.help_civic_past_dy i.gndr i.edu_lev3 c.age c.age#c.age i.migration_bkg i.east_de i.income_chng_lev2 [pweight=wgt_1]
margins, dydx(_all) post

	*Resources and incentives 
logit pol_inst_dy  i.gndr i.edu_lev3 c.age c.age#c.age i.migration_bkg i.east_de i.income_chng_lev2  i.msr_health_lev3 i.msr_eco_lev3 home_child home_care outside_child outside_support [pweight=wgt_1]
margins, dydx(_all) post
logit pol_online_dy  i.gndr i.edu_lev3 c.age c.age#c.age i.migration_bkg i.east_de i.income_chng_lev2 i.msr_health_lev3 i.msr_eco_lev3 home_child home_care outside_child outside_support [pweight=wgt_1]
margins, dydx(_all) post
logit pol_protest_dy  i.gndr i.edu_lev3 c.age c.age#c.age i.migration_bkg i.east_de i.income_chng_lev2 i.msr_health_lev3 i.msr_eco_lev3 home_child home_care outside_child outside_support [pweight=wgt_1]
margins, dydx(_all) post 
logit help_civic_dy  i.gndr i.edu_lev3 c.age c.age#c.age i.migration_bkg i.east_de i.income_chng_lev2 i.msr_health_lev3 i.msr_eco_lev3 home_child home_care outside_child outside_support [pweight=wgt_1]
margins, dydx(_all) post

	
		** Aggregate levels of care work and policy evaluations by gender **
		tab child_care gndr, col

		tab care_work gndr, col
		
		sum outside_child home_child outside_support home_care


	