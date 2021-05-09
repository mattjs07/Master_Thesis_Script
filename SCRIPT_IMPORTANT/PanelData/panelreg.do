cd C:\Users\matti\Desktop\Thesis\Data\R\Data




use df.dta, clear
keep if date >= 684
drop if missing(ouverture1)

gen T_left = (PBD/30.417) - episode_rac_numero_mois

xtset indiv date

global ylist iar

global xlist T_left indemnisation episode_rac_numero i.date

xtreg $ylist $xlist, fe vce(cluster kcala)

outreg2 using panelallv2.doc, tex replace ctitle(All) keep(T_left indemnisation episode_rac_numero) addtext(Country FE, YES, Year FE, YES)

xtreg $ylist $xlist if Duration == 1, fe vce(cluster kcala)

outreg2 using panelallv2.doc, tex append ctitle(Duration) keep(T_left indemnisation episode_rac_numero) addtext(Country FE, YES, Year FE, YES)


xtreg $ylist $xlist if Money == 1, fe vce(cluster kcala)
outreg2 using panelallv2.doc, tex append ctitle(Money) keep(T_left indemnisation episode_rac_numero) addtext(Country FE, YES, Year FE, YES)




xtreg $ylist $xlist if ouverture1 == 1, fe vce(cluster kcala)

outreg2 using panelallv2.doc, tex append ctitle(All opened) keep(T_left indemnisation episode_rac_numero) addtext(Country FE, YES, Year FE, YES)


xtreg $ylist $xlist if Duration == 1 & ouverture1 == 1, fe vce(cluster kcala)
 outreg2 using panelallv2.doc, tex append ctitle(Duration Opened) keep(T_left indemnisation episode_rac_numero) addtext(Country FE, YES, Year FE, YES)



xtreg $ylist $xlist if Money  == 1 & ouverture1 == 1, fe vce(cluster kcala)

outreg2 using panelallv2.doc, tex append ctitle(Money opened) keep(T_left indemnisation episode_rac_numero) addtext(Country FE, YES, Year FE, YES)


 



 hausman fixed random, sigmamore


 
