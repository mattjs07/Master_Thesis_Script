cd C:\Users\matti\Desktop\Thesis\Data\R\Data

use data_frame_abadie_v2.dta, clear
keep if date ==684


estrat_mult ouverture1 Money if Duration == 1 | Money == 1 | supercontrole == 1, potential(av_spell_24m) pred(femme age upper_2nd_edu higher_edu contrat_moins_12mois contrat_moins_3mois anciennete indemnisation PBD SJR married foreigner tx_chge tx_chge_jeunes proportion_de_ar proportion_de_ld proportion_de_sortants nombre_de nombre_de_rct) control(supercontrole) cov(femme age upper_2nd_edu higher_edu contrat_moins_12mois contrat_moins_3mois anciennete indemnisation PBD SJR married foreigner tx_chge tx_chge_jeunes proportion_de_ar proportion_de_ld proportion_de_sortants nombre_de nombre_de_rct)





keep if date == 684 & Neutral == 1 | Duration == 1

estrat_mult ouverture1 Duration, potential(av_spell_24m) pred(femme age upper_2nd_edu higher_edu contrat_moins_12mois contrat_moins_3mois anciennete indemnisation PBD SJR married foreigner tx_chge tx_chge_jeunes proportion_de_ar proportion_de_ld proportion_de_sortants nombre_de nombre_de_rct) control(Neutral) cov(femme age upper_2nd_edu higher_edu contrat_moins_12mois contrat_moins_3mois anciennete indemnisation PBD SJR married foreigner tx_chge tx_chge_jeunes proportion_de_ar proportion_de_ld proportion_de_sortants nombre_de nombre_de_rct)




estrat_mult ouverture1 Money, potential(av_spell_24m) pred(femme age upper_2nd_edu higher_edu contrat_moins_12mois contrat_moins_3mois anciennete indemnisation PBD SJR married foreigner tx_chge tx_chge_jeunes proportion_de_ar proportion_de_ld proportion_de_sortants nombre_de nombre_de_rct) control(supercontrole) cov(femme age upper_2nd_edu higher_edu contrat_moins_12mois contrat_moins_3mois anciennete indemnisation PBD SJR married foreigner tx_chge tx_chge_jeunes proportion_de_ar proportion_de_ld proportion_de_sortants nombre_de nombre_de_rct)


