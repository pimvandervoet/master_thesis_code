*A) Download data files
* Fat file 2016 and Longitudinal file from https://hrsdata.isr.umich.edu/data-products/rand

*B) Load Fat file, select variables, remove missing observations on key outcomes of interest
set maxvar 100000

use "C:\Users\pimva\Documents\Studie\Thesis\Git\masterthesis\Data\h16f2a.dta"
keep plb029a plb029b plb029c plb029d plb029e plb029f plb030m1 plb030m2 plb030m3 plb030m4 plb030m5 plb030m6 plb030m7 plb030m8 plb030m9 plb030m10 pi859 pi864 pi869 pi834 pi837 pi841 pi844 pi907 pa019 pc129 pc116 pc117 pc118 pc123 pc223 pc224 pc225 px060_r pz216 pb027 pz266 px092 pa026 hhidpn ///*hhidpn

drop if missing(plb029a) & missing(plb029b) & missing(plb029c) & missing(plb029d) & missing(plb029e) & missing(plb029f) 
drop if missing(pi859) | missing(pi864) | missing(pi869)| missing(pi834)| missing(pi841)| missing(pi907) 

save variable_selection_fat_file

*C) Load longitudinal files and select variables

use "C:\Users\pimva\Documents\Studie\Thesis\Git\masterthesis\Data\randhrs1992_2016v2.dta"
keep raracem raeduc rameduc hhidpn 

save variable_selection_longitudinal_file

*D) Merge selections from Fat file and longitudinal file based on hhidpn

 merge 1:1 hhidpn using  "C:\Users\pimva\Documents\Studie\Thesis\Git\masterthesis\Data\variable_selection_fat_file"
 keep if _merge == 3

 save merged_data
