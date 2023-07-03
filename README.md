# State-level maternal mortality by racial and ethnic group

This repo contains cleaned code used to generate estimates of maternal mortality in the United States by state, year, and racial and ethnic group, as reported in 'Trends in state-level maternal mortality by racial and ethnic group in the United States' published in the Journal of the American Medical Association (JAMA) on July 3rd, 2023.

* The script 'modeling.py' contains code to generate estimates of births and deaths by state.
* The script 'raking.R' contains code to rake birth and death estimates from the state level to the national level. 
* The script 'calculate_mmr.R' contains code to calculate maternal mortality from birth and death estimates. 

The code used to prepare the death inputs is located here: https://github.com/ihmeuw/USHD/tree/life_expectancy_race_ethnicity_2022/mortality/data_prep/counties and the code used to prepare the population inputs is located here: https://github.com/ihmeuw/USHD/tree/life_expectancy_race_ethnicity_2022/population/counties/prepped

To see other published code repositories for the CVD - USA Health Disparities team, please visit our [code homepage](https://github.com/ihmeuw/CVDdisparities).
