cd "D:\Mag2\semestre2\logiciel_economiste2\projet"
use "data.dta", clear
browse

************************************************************
*
* Nettoyage de la base de donnée
*
************************************************************

* On filtre la base sur les individus ayant un niveau chronique de covid soit clasiffication_final == 1
tab clasiffication_final
keep if clasiffication_final == 1

* On affiche les modalités de toutes les variables
codebook patient_type
codebook pneumonia pregnant diabetes copd inmsupr hipertension other_disease cardiovascular obesity renal_chronic tobacco

* On procède à un recodage de toutes les variables

replace patient_type = 0 if patient_type == 1
replace patient_type = 1 if patient_type == 2
replace pregnant = 0 if pregnant == 97| pregnant == 98| pregnant == 2
replace diabetes = 0 if diabetes == 99| diabetes == 98| diabetes == 2
replace copd = 0 if copd == 99| copd == 98| copd == 2
replace inmsupr = 0 if inmsupr == 99| inmsupr == 98| inmsupr == 2
replace hipertension  = 0 if hipertension == 99| hipertension == 98| hipertension == 2
replace other_disease   = 0 if other_disease  == 99| other_disease == 98| other_disease == 2
replace cardiovascular = 0 if cardiovascular == 99| cardiovascular == 98| cardiovascular == 2
replace obesity = 0 if obesity == 99| obesity == 98| obesity == 2
replace tobacco = 0 if tobacco == 99| tobacco == 98| tobacco == 2
replace asthma = 0 if asthma == 97| asthma == 98| asthma == 2
replace renal_chronic = 0 if renal_chronic == 97| renal_chronic == 98| renal_chronic == 2
replace pneumonia = 0 if pneumonia == 2
* sex = 1 : female , 0 : male
replace sex = 0 if sex == 2

* On recode aussi la variable d'intérêt 1=décès, 0= pas décès
codebook date_died
replace date_died = "0" if date_died == "9999-99-99"
replace date_died = "1" if date_died != "0"
* On change le type de la variable
encode date_died, gen(died)
recast byte died
label variable died "DIED"
label values died .
replace died = 0 if died == 1
replace died = 1 if died == 2
tab died
tab patient_type died

* Statistiques descriptives
tabstat sex pneumonia age pregnant diabetes copd asthma inmsupr hipertension other_disease cardiovascular obesity renal_chronic tobacco, by ( patient_type) stats(mean sd) col(stat)

tabstat patient_type died sex pneumonia age pregnant diabetes copd asthma inmsupr hipertension other_disease cardiovascular obesity renal_chronic tobacco, stats(mean sd) col(stat)

***************************************
* Analyse approfondir
****************************************

* Define treatment, outcome and independant variables
global treatment patient_type
global ylist died
global xlist sex pneumonia age pregnant diabetes copd asthma inmsupr hipertension other_disease cardiovascular obesity renal_chronic tobacco

* D'autres statistiques descriptives
describe $treatment $ylist $xlist
summarize $treatment $ylist $xlist
bysort $treatment: summarize $ylist $xlist

*Regression
******************************************************

*Probit Model 
probit $ylist $treatment $xlist 

*Logit Model
logit $ylist $treatment $xlist

*Marginal effect (at the mean and average marginal effect)
quietly probit $ylist $treatment $xlist 
margins, dydx(*) atmeans
margins, dydx(*)

quietly logit $ylist $treatment $xlist 
margins, dydx(*) atmeans
margins, dydx(*)

* Logistic model gives ODDS Ratio
logistic $ylist $treatment $xlist

* Predicted probabilities
quietly logit $ylist $treatment $xlist
predict plogit, pr

quietly probit $ylist $treatment $xlist 
predict pprobit, pr

summarize $ylist plogit pprobit


* Percent correctly predicted values
quietly logit $ylist $treatment $xlist
estat classification

quietly probit $ylist $treatment $xlist
estat classification

**************************************
*            Matching
**************************************

* Propensity score matching using psmatch2 package

* Install psmatch2 package
ssc install psmatch2

* Propensity score matching
psmatch2 $treatment $xlist, outcome($ylist) ate 

* Propensity score matching with logit instead of probit model
psmatch2 $treatment $xlist, outcome($ylist) logit

* Nearest neighbor matching - neighbor(number of neighbors)
psmatch2 $treatment $xlist, outcome($ylist) common neighbor(1)

* Radius matching - caliper(distance)
psmatch2 $treatment $xlist, outcome($ylist) common radius caliper(0.1)

* Kernel matching
psmatch2 $treatment $xlist, outcome($ylist) common kernel

* Bootstrapping 
set seed 0
bootstrap r(att): psmatch2 $treatment $xlist, outcome($ylist)

* Balancing - comparisons of treated and controls after matching
pstest 




***********************************************************
* Sans l'age
***********************************************************

use "data.dta", clear
browse

************************************************************
*
* Nettoyage de la base de donnée
*
************************************************************

* On filtre la base sur les individus ayant un niveau chronique de covid soit clasiffication_final == 1
tab clasiffication_final
keep if clasiffication_final == 1

* On procède à un recodage de toutes les variables

replace patient_type = 0 if patient_type == 1
replace patient_type = 1 if patient_type == 2
replace pregnant = 0 if pregnant == 97| pregnant == 98| pregnant == 2
replace diabetes = 0 if diabetes == 99| diabetes == 98| diabetes == 2
replace copd = 0 if copd == 99| copd == 98| copd == 2
replace inmsupr = 0 if inmsupr == 99| inmsupr == 98| inmsupr == 2
replace hipertension  = 0 if hipertension == 99| hipertension == 98| hipertension == 2
replace other_disease   = 0 if other_disease  == 99| other_disease == 98| other_disease == 2
replace cardiovascular = 0 if cardiovascular == 99| cardiovascular == 98| cardiovascular == 2
replace obesity = 0 if obesity == 99| obesity == 98| obesity == 2
replace tobacco = 0 if tobacco == 99| tobacco == 98| tobacco == 2
replace asthma = 0 if asthma == 97| asthma == 98| asthma == 2
replace renal_chronic = 0 if renal_chronic == 97| renal_chronic == 98| renal_chronic == 2
replace pneumonia = 0 if pneumonia == 2
* sex = 1 : female , 0 : male
replace sex = 0 if sex == 2

* On recode aussi la variable d'intérêt 1=décès, 0= pas décès
codebook date_died
replace date_died = "0" if date_died == "9999-99-99"
replace date_died = "1" if date_died != "0"
* On change le type de la variable
encode date_died, gen(died)
recast byte died
label variable died "DIED"
label values died .
replace died = 0 if died == 1
replace died = 1 if died == 2
tab died
tab patient_type died


***************************************
* Analyse approfondir
****************************************

* Define treatment, outcome and independant variables
global treatment patient_type
global ylist died
global xlist sex pneumonia pregnant diabetes copd asthma inmsupr hipertension other_disease cardiovascular obesity renal_chronic tobacco

**************************************
*            Matching
**************************************

* Propensity score matching using psmatch2 package

* Install psmatch2 package
ssc install psmatch2

* Propensity score matching
psmatch2 $treatment $xlist, outcome($ylist) ate 

* Propensity score matching with logit instead of probit model
psmatch2 $treatment $xlist, outcome($ylist) logit

* Nearest neighbor matching - neighbor(number of neighbors)
psmatch2 $treatment $xlist, outcome($ylist) common neighbor(1)

* Radius matching - caliper(distance)
psmatch2 $treatment $xlist, outcome($ylist) common radius caliper(0.1)

* Kernel matching
psmatch2 $treatment $xlist, outcome($ylist) common kernel

* Bootstrapping 
set seed 0
bootstrap r(att): psmatch2 $treatment $xlist, outcome($ylist)




***********************************************************
* Sans le sexe
***********************************************************

use "data.dta", clear
browse

************************************************************
*
* Nettoyage de la base de donnée
*
************************************************************

* On filtre la base sur les individus ayant un niveau chronique de covid soit clasiffication_final == 1
tab clasiffication_final
keep if clasiffication_final == 1


* On procède à un recodage de toutes les variables

replace patient_type = 0 if patient_type == 1
replace patient_type = 1 if patient_type == 2
replace pregnant = 0 if pregnant == 97| pregnant == 98| pregnant == 2
replace diabetes = 0 if diabetes == 99| diabetes == 98| diabetes == 2
replace copd = 0 if copd == 99| copd == 98| copd == 2
replace inmsupr = 0 if inmsupr == 99| inmsupr == 98| inmsupr == 2
replace hipertension  = 0 if hipertension == 99| hipertension == 98| hipertension == 2
replace other_disease   = 0 if other_disease  == 99| other_disease == 98| other_disease == 2
replace cardiovascular = 0 if cardiovascular == 99| cardiovascular == 98| cardiovascular == 2
replace obesity = 0 if obesity == 99| obesity == 98| obesity == 2
replace tobacco = 0 if tobacco == 99| tobacco == 98| tobacco == 2
replace asthma = 0 if asthma == 97| asthma == 98| asthma == 2
replace renal_chronic = 0 if renal_chronic == 97| renal_chronic == 98| renal_chronic == 2
replace pneumonia = 0 if pneumonia == 2
* sex = 1 : female , 0 : male
replace sex = 0 if sex == 2

* On recode aussi la variable d'intérêt 1=décès, 0= pas décès
codebook date_died
replace date_died = "0" if date_died == "9999-99-99"
replace date_died = "1" if date_died != "0"
* On change le type de la variable
encode date_died, gen(died)
recast byte died
label variable died "DIED"
label values died .
replace died = 0 if died == 1
replace died = 1 if died == 2
tab died
tab patient_type died


***************************************
* Analyse approfondir
****************************************

* Define treatment, outcome and independant variables
global treatment patient_type
global ylist died
global xlist age pneumonia pregnant diabetes copd asthma inmsupr hipertension other_disease cardiovascular obesity renal_chronic tobacco

**************************************
*            Matching
**************************************

* Propensity score matching using psmatch2 package

* Install psmatch2 package
ssc install psmatch2

* Propensity score matching
psmatch2 $treatment $xlist, outcome($ylist) ate 

* Propensity score matching with logit instead of probit model
psmatch2 $treatment $xlist, outcome($ylist) logit

* Nearest neighbor matching - neighbor(number of neighbors)
psmatch2 $treatment $xlist, outcome($ylist) common neighbor(1)

* Radius matching - caliper(distance)
psmatch2 $treatment $xlist, outcome($ylist) common radius caliper(0.1)

* Kernel matching
psmatch2 $treatment $xlist, outcome($ylist) common kernel

* Bootstrapping 
set seed 0
bootstrap r(att): psmatch2 $treatment $xlist, outcome($ylist)




***********************************************************
* Echantillon des hommes
***********************************************************

use "data.dta", clear

************************************************************
*
* Nettoyage de la base de donnée
*
************************************************************

* On filtre la base sur les individus ayant un niveau chronique de covid soit clasiffication_final == 1
tab clasiffication_final
keep if clasiffication_final == 1


* On procède à un recodage de toutes les variables

replace patient_type = 0 if patient_type == 1
replace patient_type = 1 if patient_type == 2
replace pregnant = 0 if pregnant == 97| pregnant == 98| pregnant == 2
replace diabetes = 0 if diabetes == 99| diabetes == 98| diabetes == 2
replace copd = 0 if copd == 99| copd == 98| copd == 2
replace inmsupr = 0 if inmsupr == 99| inmsupr == 98| inmsupr == 2
replace hipertension  = 0 if hipertension == 99| hipertension == 98| hipertension == 2
replace other_disease   = 0 if other_disease  == 99| other_disease == 98| other_disease == 2
replace cardiovascular = 0 if cardiovascular == 99| cardiovascular == 98| cardiovascular == 2
replace obesity = 0 if obesity == 99| obesity == 98| obesity == 2
replace tobacco = 0 if tobacco == 99| tobacco == 98| tobacco == 2
replace asthma = 0 if asthma == 97| asthma == 98| asthma == 2
replace renal_chronic = 0 if renal_chronic == 97| renal_chronic == 98| renal_chronic == 2
replace pneumonia = 0 if pneumonia == 2
* sex = 1 : female , 0 : male
replace sex = 0 if sex == 2

* On recode aussi la variable d'intérêt 1=décès, 0= pas décès
codebook date_died
replace date_died = "0" if date_died == "9999-99-99"
replace date_died = "1" if date_died != "0"
* On change le type de la variable
encode date_died, gen(died)
recast byte died
label variable died "DIED"
label values died .
replace died = 0 if died == 1
replace died = 1 if died == 2
tab died
tab patient_type died

* On constitue l'échantillon avec les hommes seuls
keep if sex == 0

***************************************
* Analyse approfondir
****************************************

* Define treatment, outcome and independant variables
global treatment patient_type
global ylist died
global xlist age pneumonia pregnant diabetes copd asthma inmsupr hipertension other_disease cardiovascular obesity renal_chronic tobacco

**************************************
*            Matching
**************************************

* Propensity score matching using psmatch2 package

* Install psmatch2 package
ssc install psmatch2

* Propensity score matching
psmatch2 $treatment $xlist, outcome($ylist) ate 

* Propensity score matching with logit instead of probit model
psmatch2 $treatment $xlist, outcome($ylist) logit

* Nearest neighbor matching - neighbor(number of neighbors)
psmatch2 $treatment $xlist, outcome($ylist) common neighbor(1)

* Radius matching - caliper(distance)
psmatch2 $treatment $xlist, outcome($ylist) common radius caliper(0.1)

* Kernel matching
psmatch2 $treatment $xlist, outcome($ylist) common kernel

* Bootstrapping 
set seed 0
bootstrap r(att): psmatch2 $treatment $xlist, outcome($ylist)



***********************************************************
* Echantillon des femmes
***********************************************************

use "data.dta", clear

************************************************************
*
* Nettoyage de la base de donnée
*
************************************************************

* On filtre la base sur les individus ayant un niveau chronique de covid soit clasiffication_final == 1
tab clasiffication_final
keep if clasiffication_final == 1


* On procède à un recodage de toutes les variables

replace patient_type = 0 if patient_type == 1
replace patient_type = 1 if patient_type == 2
replace pregnant = 0 if pregnant == 97| pregnant == 98| pregnant == 2
replace diabetes = 0 if diabetes == 99| diabetes == 98| diabetes == 2
replace copd = 0 if copd == 99| copd == 98| copd == 2
replace inmsupr = 0 if inmsupr == 99| inmsupr == 98| inmsupr == 2
replace hipertension  = 0 if hipertension == 99| hipertension == 98| hipertension == 2
replace other_disease   = 0 if other_disease  == 99| other_disease == 98| other_disease == 2
replace cardiovascular = 0 if cardiovascular == 99| cardiovascular == 98| cardiovascular == 2
replace obesity = 0 if obesity == 99| obesity == 98| obesity == 2
replace tobacco = 0 if tobacco == 99| tobacco == 98| tobacco == 2
replace asthma = 0 if asthma == 97| asthma == 98| asthma == 2
replace renal_chronic = 0 if renal_chronic == 97| renal_chronic == 98| renal_chronic == 2
replace pneumonia = 0 if pneumonia == 2
* sex = 1 : female , 0 : male
replace sex = 0 if sex == 1

* On recode aussi la variable d'intérêt 1=décès, 0= pas décès
codebook date_died
replace date_died = "0" if date_died == "9999-99-99"
replace date_died = "1" if date_died != "0"
* On change le type de la variable
encode date_died, gen(died)
recast byte died
label variable died "DIED"
label values died .
replace died = 0 if died == 1
replace died = 1 if died == 2
tab died
tab patient_type died

* On constitue l'échantillon avec les hommes seuls
keep if sex == 0

***************************************
* Analyse approfondir
****************************************

* Define treatment, outcome and independant variables
global treatment patient_type
global ylist died
global xlist age pneumonia pregnant diabetes copd asthma inmsupr hipertension other_disease cardiovascular obesity renal_chronic tobacco

**************************************
*            Matching
**************************************

* Propensity score matching using psmatch2 package

* Install psmatch2 package
ssc install psmatch2

* Propensity score matching
psmatch2 $treatment $xlist, outcome($ylist) ate 

* Propensity score matching with logit instead of probit model
psmatch2 $treatment $xlist, outcome($ylist) logit

* Nearest neighbor matching - neighbor(number of neighbors)
psmatch2 $treatment $xlist, outcome($ylist) common neighbor(1)

* Radius matching - caliper(distance)
psmatch2 $treatment $xlist, outcome($ylist) common radius caliper(0.1)

* Kernel matching
psmatch2 $treatment $xlist, outcome($ylist) common kernel

* Bootstrapping 
set seed 0
bootstrap r(att): psmatch2 $treatment $xlist, outcome($ylist)


***********************************************************
* Echantillon des moins de 85ans
***********************************************************

use "data.dta", clear

************************************************************
*
* Nettoyage de la base de donnée
*
************************************************************

* On filtre la base sur les individus ayant un niveau chronique de covid soit clasiffication_final == 1
tab clasiffication_final
keep if clasiffication_final == 1


* On procède à un recodage de toutes les variables

replace patient_type = 0 if patient_type == 1
replace patient_type = 1 if patient_type == 2
replace pregnant = 0 if pregnant == 97| pregnant == 98| pregnant == 2
replace diabetes = 0 if diabetes == 99| diabetes == 98| diabetes == 2
replace copd = 0 if copd == 99| copd == 98| copd == 2
replace inmsupr = 0 if inmsupr == 99| inmsupr == 98| inmsupr == 2
replace hipertension  = 0 if hipertension == 99| hipertension == 98| hipertension == 2
replace other_disease   = 0 if other_disease  == 99| other_disease == 98| other_disease == 2
replace cardiovascular = 0 if cardiovascular == 99| cardiovascular == 98| cardiovascular == 2
replace obesity = 0 if obesity == 99| obesity == 98| obesity == 2
replace tobacco = 0 if tobacco == 99| tobacco == 98| tobacco == 2
replace asthma = 0 if asthma == 97| asthma == 98| asthma == 2
replace renal_chronic = 0 if renal_chronic == 97| renal_chronic == 98| renal_chronic == 2
replace pneumonia = 0 if pneumonia == 2
* sex = 1 : female , 0 : male
replace sex = 0 if sex == 1

* On recode aussi la variable d'intérêt 1=décès, 0= pas décès
codebook date_died
replace date_died = "0" if date_died == "9999-99-99"
replace date_died = "1" if date_died != "0"
* On change le type de la variable
encode date_died, gen(died)
recast byte died
label variable died "DIED"
label values died .
replace died = 0 if died == 1
replace died = 1 if died == 2
tab died
tab patient_type died

* On constitue l'échantillon avec les hommes seuls
keep if age <= 85

***************************************
* Analyse approfondir
****************************************

* Define treatment, outcome and independant variables
global treatment patient_type
global ylist died
global xlist age pneumonia pregnant diabetes copd asthma inmsupr hipertension other_disease cardiovascular obesity renal_chronic tobacco

**************************************
*            Matching
**************************************

* Propensity score matching using psmatch2 package

* Install psmatch2 package
ssc install psmatch2

* Propensity score matching
psmatch2 $treatment $xlist, outcome($ylist) ate 

* Propensity score matching with logit instead of probit model
psmatch2 $treatment $xlist, outcome($ylist) logit

* Nearest neighbor matching - neighbor(number of neighbors)
psmatch2 $treatment $xlist, outcome($ylist) common neighbor(1)

* Radius matching - caliper(distance)
psmatch2 $treatment $xlist, outcome($ylist) common radius caliper(0.1)

* Kernel matching
psmatch2 $treatment $xlist, outcome($ylist) common kernel



***********************************************************
* Echantillon des plus de 85ans
***********************************************************

use "data.dta", clear

************************************************************
*
* Nettoyage de la base de donnée
*
************************************************************

* On filtre la base sur les individus ayant un niveau chronique de covid soit clasiffication_final == 1
tab clasiffication_final
keep if clasiffication_final == 1


* On procède à un recodage de toutes les variables

replace patient_type = 0 if patient_type == 1
replace patient_type = 1 if patient_type == 2
replace pregnant = 0 if pregnant == 97| pregnant == 98| pregnant == 2
replace diabetes = 0 if diabetes == 99| diabetes == 98| diabetes == 2
replace copd = 0 if copd == 99| copd == 98| copd == 2
replace inmsupr = 0 if inmsupr == 99| inmsupr == 98| inmsupr == 2
replace hipertension  = 0 if hipertension == 99| hipertension == 98| hipertension == 2
replace other_disease   = 0 if other_disease  == 99| other_disease == 98| other_disease == 2
replace cardiovascular = 0 if cardiovascular == 99| cardiovascular == 98| cardiovascular == 2
replace obesity = 0 if obesity == 99| obesity == 98| obesity == 2
replace tobacco = 0 if tobacco == 99| tobacco == 98| tobacco == 2
replace asthma = 0 if asthma == 97| asthma == 98| asthma == 2
replace renal_chronic = 0 if renal_chronic == 97| renal_chronic == 98| renal_chronic == 2
replace pneumonia = 0 if pneumonia == 2
* sex = 1 : female , 0 : male
replace sex = 0 if sex == 1

* On recode aussi la variable d'intérêt 1=décès, 0= pas décès
codebook date_died
replace date_died = "0" if date_died == "9999-99-99"
replace date_died = "1" if date_died != "0"
* On change le type de la variable
encode date_died, gen(died)
recast byte died
label variable died "DIED"
label values died .
replace died = 0 if died == 1
replace died = 1 if died == 2
tab died
tab patient_type died

* On constitue l'échantillon avec les hommes seuls
keep if age > 85

***************************************
* Analyse approfondir
****************************************

* Define treatment, outcome and independant variables
global treatment patient_type
global ylist died
global xlist age pneumonia pregnant diabetes copd asthma inmsupr hipertension other_disease cardiovascular obesity renal_chronic tobacco

**************************************
*            Matching
**************************************

* Propensity score matching using psmatch2 package

* Install psmatch2 package
ssc install psmatch2

* Propensity score matching
psmatch2 $treatment $xlist, outcome($ylist) ate 

* Propensity score matching with logit instead of probit model
psmatch2 $treatment $xlist, outcome($ylist) logit

* Nearest neighbor matching - neighbor(number of neighbors)
psmatch2 $treatment $xlist, outcome($ylist) common neighbor(1)

* Radius matching - caliper(distance)
psmatch2 $treatment $xlist, outcome($ylist) common radius caliper(0.1)

* Kernel matching
psmatch2 $treatment $xlist, outcome($ylist) common kernel

