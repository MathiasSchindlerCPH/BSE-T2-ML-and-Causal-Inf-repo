/********************************************************/
/*														*/
/*	TITLE:		PSET 3									*/
/*	COURSE: 	MACHINE LEARNING AND CAUSAL INFERENCE	*/
/* 	AUTHORS:	SCHINDLER								*/
/*	DATE:		MAR 9, 2022								*/
/*														*/
/********************************************************/

//Preamble
cls
clear all

cd "/Users/mathiasschindler/Library/Mobile Documents/com~apple~CloudDocs/BSE/_T2--ML & Causal Inf/-PSETs/PSET3"


**************************************************************************
*					BINARY OUTCOME MODEL
use "ALLBUS18.dta", clear
describe

//Data prep
tab pv01, m
tab pv01, m nolab

gen vote = inlist(pv01, 1, 2, 3, 4, 5, 6, 42, 66)
tab vote

drop if pv01<0


*1) Estimate naïve models
replace educ = 1 if educ == -9

// logit
logit vote i.sex age i.educ i.work, or
logit vote i.sex age i.educ i.work

// probit
probit vote i.sex age i.educ i.work


*2) Marginal effects
// logit
qui logit vote sex age educ work

margins, dydx(sex age) atmeans
margins, dydx(sex age) at(sex=1 age=26 work=1 educ=3)


// probit
qui probit vote sex age educ work

margins, dydx(sex age) atmeans
margins, dydx(sex age) at(sex=1 age=26 work=1 educ=3)


*3) Manual estimatation
*gen vote2 = (pv01!=91)

global regressors "i.sex age i.educ i.work"

program define my_sigmoid
    args Y Xb
    qui replace `Y' = -ln(1+exp(-`Xb')) if $ML_y1==1
    qui replace `Y' = -`Xb' - ln(1+exp(-`Xb')) if $ML_y1==0
end

ml model lf my_sigmoid (vote = $regressors)
ml maximize



**************************************************************************
*					MULTINOMIAL MODEL
use "ALLBUS18.dta", clear
describe

	//Data prep
		*Drop if not voted for CDU/CSU, SPD, Greens
		tab pv01, m
		tab pv01, m nolab

		drop if !inlist(pv01, 1, 2, 4)


*1) Naïve multi model
		mlogit pv01 i.sex age i.educ i.work, rrr
		//mlogit pv01 i.sex age i.educ i.work
		
		esttab using "out/21_naivelogit.tex", stats(r2_p n) se varwidth(30) modelwidth(10) label unstack eform replace



*2) Marginal effects
		qui mlogit pv01 i.sex age i.educ i.work, rrr
		margins, dydx(sex) atmeans
		margins, dydx(age) atmeans



*3) Manual estimatation
		program define my_multi_logit
			args lnf Xb2 Xb3
			quietly replace `lnf' = - ln(1+exp(`Xb2')+exp(`Xb3')) if $ML_y1==1
			quietly replace `lnf' = `Xb2' - ln(1+exp(`Xb2')+exp(`Xb3')) if $ML_y1==2
			quietly replace `lnf' = `Xb3' - ln(1+exp(`Xb2')+exp(`Xb3')) if $ML_y1==4
		end

		ml model lf my_multi_logit (xb2:pv01= $regressors) (xb3:= $regressors)
		ml maximize



*4) Conditional Logit
		//Data work:	
			keep respid pv01 sex age educ work cdu_asses spd_assess gre_assess
			
			//Generate variables for options
			gen y_mnl=1 if pv01==1
			replace y_mnl=2 if pv01==2
			replace y_mnl=3 if pv01==4
			
			//Generate choice variables
			foreach k of numlist 1/3 {
				gen y`k' = 0
				replace y`k' = 1 if y_mnl == `k'
			}

			//reshape wide to long
			reshape long y, i(respid) j(parties)
			order parties, b(y_mnl)
			drop y_mnl
			
			lab def partynames 1 "CDU/CSU" 2 "SPD" 3 "GREENS"
			lab val parties partynames
			
			
			//generate distance measure
			gen distance = .
			bysort respid: replace distance = cdu_asses if _n == 1
			bysort respid: replace distance = spd_assess if _n == 2
			bysort respid: replace distance = gre_assess if _n == 3

			
		//Estimate conditional logit
		asclogit y distance, case(respid) alternatives(parties) casevars(i.sex age i.educ i.work)


		esttab using "out/24_condlogit.tex", stats(r2_p) se varwidth(30) modelwidth(10) label unstack /*eform*/ replace


**************************************************************************
* Now we have to use all six parties
		cd "/Users/mathiasschindler/Library/Mobile Documents/com~apple~CloudDocs/BSE/_T2--ML & Causal Inf/-PSETs/PSET3"
		use "ALLBUS18.dta", clear
		describe

		//Data prep
		*Drop if not voted for CDU/CSU, SPD, Greens
		tab pv01, m
		tab pv01, m nolab

		drop if !inlist(pv01, 1, 2, 3, 4, 6, 42)


*5) Multi Logit #2
		use "ALLBUS18.dta", clear
		drop if !inlist(pv01, 1, 2, 3, 4, 6, 42)

		mlogit pv01 pa01 i.sex age i.educ i.work, rrr

		esttab using "out/25_mlogit.tex", stats(r2_p) se varwidth(30) modelwidth(10) label unstack eform replace


*6) Nested Logit
		//Create variables for long format
			cd "/Users/mathiasschindler/Library/Mobile Documents/com~apple~CloudDocs/BSE/_T2--ML & Causal Inf/-PSETs/PSET3"
			
			use "ALLBUS18.dta", clear
			drop if !inlist(pv01, 1, 2, 3, 4, 6, 42)
			
			keep respid pv01 pa01 sex age educ work
			order pv01, b(pa01)
			
			//Generate variables for options
			gen y_mnl=1 if pv01==1
			replace y_mnl=2 if pv01==2
			replace y_mnl=3 if pv01==3
			replace y_mnl=4 if pv01==4
			replace y_mnl=5 if pv01==6
			replace y_mnl=6 if pv01==42
			
			//Generate choice variables
			gen y1=0
			replace y1=1 if y_mnl==1

			gen y2=0
			replace y2=1 if y_mnl==2

			gen y3=0
			replace y3=1 if y_mnl==3
			
			gen y4=0
			replace y4=1 if y_mnl==4

			gen y5=0
			replace y5=1 if y_mnl==5

			gen y6=0
			replace y6=1 if y_mnl==6
			
			
			// Set to missing given type (easier delete later)
			nlogitgen type = pv01(left: 2|4|6, right: 1|3|42)
			
			replace y1 = . if type == 1 //CDU
			replace y3 = . if type == 1 //FDP
			replace y6 = . if type == 1 //AFD
			
			replace y2 = . if type == 2 //SPD
			replace y4 = . if type == 2 //GREEN
			replace y5 = . if type == 2 //LEFT
			
			
			//reshape wide to long
			reshape long y, i(respid) j(parties)
			order parties, b(y_mnl)
			drop y_mnl
			
			
			// Drop if y == . such that options are exclusive for 'left' and 'right'
			drop type
			drop if y == .
			
			
		//Set tree structure
		lab drop lb_type
		nlogitgen type = pv01(left: 2|4|6, right: 1|3|42)

			//Put labels on values
			lab def party_lab 1 "CDU/CSU" 2 "SPD" 3 "FDP" 4 "GREENS" 5 "LEFT" 6 "AFD"
			lab val parties party_lab


		//Visualize tree structure
		nlogittree pv01 type

		//Random sample to check code runs:
		/*gen random = runiform()
		drop if random <0.80

		save "temp/10pct-randomsmpl", replace
		use "temp/10pct-randomsmpl", clear
*/


		//Estimate nested logit
		nlogit y || type: pa01 || parties: i.sex age i.educ i.work, case(respid) iterate(50)

		esttab using "out/26_nlogit.tex", stats(r2_p n) se  varwidth(30) modelwidth(10) label unstack /*eform*/ replace



		//Marginal effects
		preserve 
			predict plevel1 plevel2, pr
			tab parties, summarize(plevel2)
			
			quietly summarize pa01
			gen delta = r(sd)/100
			quietly replace pa01 = pa01 + delta if parties == 4
			
			* Predict at the new values
			predict pnew1 pnew2, pr
			
			* Subtract the two predictions and divide by the amount of the change
			gen dpdbeach = (pnew2-plevel2)/delta
			
			* The AME is the average of the previous quantity
			tab parties, summarize(dpdbeach)
		restore 




*7) Experimentation with various models
	//7.1 Multinomial models
		use "ALLBUS18.dta", clear
		drop if !inlist(pv01, 1, 2, 3, 4, 6, 42)
		desc
			
		keep respid pv01 pa11 pa17 lm14 sex age educ work


	//7.1.1: pa11 ENVIRONMENT PROTECTION should be stronger 
		mlogit pv01 pa11 i.sex age i.educ i.work, rrr

		esttab using "out/27_model1.tex", stats(r2_p) se varwidth(30) modelwidth(10) label unstack eform replace


	//7.1.2: pa17 IMMIGRANTS GOOD FOR GERMAN ECONOMY (0/1)
		mlogit pv01 pa17 i.sex age i.educ i.work, rrr

		esttab using "out/27_model2.tex", stats(r2_p) se varwidth(30) modelwidth(10) label unstack eform replace


	//7.1.3: lm14 NEWSPAPER How many days a week [0, 1, 2, 3, 4, 5, 6, 7]
		mlogit pv01 lm14 i.sex age i.educ i.work, rrr

		esttab using "out/27_model3.tex", stats(r2_p) se varwidth(30) modelwidth(10) label unstack eform replace
