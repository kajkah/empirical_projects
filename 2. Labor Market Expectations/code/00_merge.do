*==============================================================================*
* FINAL PROJECT - Macroeconomics with Micro Data
* Project title: Labor Market Expectations

* Author: Lea Best (best@ifo.de) & Karolina Hozova (s7809268@stud.uni-frankfurt.de)
* Date:   08.02.2024 
*==============================================================================*
* This do-file:  - loads al available micro datasets from the SCE that are stored 
*				 in the data folder and appends them 
* 				 - Loads the microdata from the SCE labor market survey and merges
*					it with the main data
* 				 - Stores the final, merged dataset as csv to the data folder
*==============================================================================*

*==============================================================================*
* SET PATH 
* You need to change the base path to folder in which data, code, and output folder are stored.
*==============================================================================*
global BasePath "\Users\karolina\Macro with Data\Final_Project\Best-Hozova" 


*==============================================================================*
* Settings 
*==============================================================================*
set excelxlsxlargefile on

* set data path
global datapath "${BasePath}\data"

*==============================================================================*
* Merge all available micro datasets
*==============================================================================*
import excel "${datapath}\frbny-sce-public-microdata-latest.xlsx", sheet("Data") cellrange(A2:HM48613) firstrow clear
save "${datapath}\part_3.dta", replace

import excel "${datapath}\FRBNY-SCE-Public-Microdata-Complete-17-19.xlsx", sheet("Data") cellrange(A2:HL47683) firstrow clear
save "${datapath}\part_2.dta", replace

import excel "${datapath}\FRBNY-SCE-Public-Microdata-Complete-13-16.xlsx", sheet("Data") cellrange(A2:HL56446) firstrow clear
save "${datapath}\part_1.dta", replace

append using "${datapath}\part_2.dta"
append using "${datapath}\part_3.dta"

save "${datapath}\complete.dta", replace

*==============================================================================*
* Add labor market module
*==============================================================================*
import excel "${datapath}\sce-labor-microdata-public", sheet("Data") cellrange(A2:DO28377) firstrow clear
merge 1:1 userid date using "${datapath}\complete.dta"

* drop labels from merge variable and rename to be able to load into matlab
label drop _merge
rename _merge merge

*==============================================================================*
* Drop unnecessary variables 
*==============================================================================*
keep date userid weight oo1_* Q10_* QNUM* Q47 Q36 Q32 Q33 Q38 Q34 Q35_* js5 js7 merge

*==============================================================================*
* Save final data as csv
*==============================================================================*
sort userid date
export delimited using "${datapath}\data_merged.csv", replace
 
*==============================================================================*
