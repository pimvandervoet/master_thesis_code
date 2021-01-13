*A) Download data files
* Fat file 2016 & 2012 and Longitudinal file from https://hrsdata.isr.umich.edu/data-products/rand

*B) Load Fat file 2016, select variables, remove missing observations on key outcomes of interest
set maxvar 100000

use "C:\Users\pimva\Documents\Studie\Thesis\Programming\A_Data_sources\h16f2a.dta"
keep plb029a plb029b plb029c plb029d plb029e plb029f plb030m1 plb030m2 plb030m3 plb030m4 plb030m5 plb030m6 plb030m7 plb030m8 plb030m9 plb030m10 pi859 pi864 pi869 pi834 pi837 pi841 pi844 pi907 pa019 pc129 pc130 pc116 pc117 pc118 pc123 pc223 pc224 pc225 px060_r pz216 pb027 pz266 px092 pa026 pz134 pj005m1 pj005m2 pj005m3 pj005m4 pj005m5 hhidpn ///*hhidpn

drop if missing(plb029a) & missing(plb029b) & missing(plb029c) & missing(plb029d) & missing(plb029e) & missing(plb029f) 
drop if missing(pi859) | missing(pi864) | missing(pi869)| missing(pi834)| missing(pi841)| missing(pi907) 

save C:\Users\pimva\Documents\Studie\Thesis\Programming\0_Data_loading\variable_selection_fat_file_2016

*C) Load Fat file 2012, select variables

use "C:\Users\pimva\Documents\Studie\Thesis\Programming\A_Data_sources\h12f2a.dta"
keep ni859 ni864 ni869 ni907 ni834 ni841 nc118 nc129 nc130 hhidpn

save C:\Users\pimva\Documents\Studie\Thesis\Programming\0_Data_loading\variable_selection_fat_file_2012

*D) Merge selection from 2012 into the 2016 Fat file (keep if full match or only in 2016 file)

merge 1:1 hhidpn using  "C:\Users\pimva\Documents\Studie\Thesis\Programming\0_Data_loading\variable_selection_fat_file_2016"
keep if _merge == 3 | _merge == 2
drop _merge
save C:\Users\pimva\Documents\Studie\Thesis\Programming\0_Data_loading\variable_selection_fat_file_both
 
*E) Load longitudinal files and select variables

use "C:\Users\pimva\Documents\Studie\Thesis\Programming\A_Data_sources\randhrs1992_2016v2.dta"
keep raracem raeduc rameduc r11mstat r13mstat r11smoken r13smoken s11drink s13drink hhidpn 

save C:\Users\pimva\Documents\Studie\Thesis\Programming\0_Data_loading\variable_selection_longitudinal_file 

*F) Merge selections from Fat file and longitudinal file based on hhidpn

merge 1:1 hhidpn using  "C:\Users\pimva\Documents\Studie\Thesis\Programming\0_Data_loading\variable_selection_fat_file_both"
keep if _merge == 3

save C:\Users\pimva\Documents\Studie\Thesis\Programming\0_Data_loading\merged_data
 

