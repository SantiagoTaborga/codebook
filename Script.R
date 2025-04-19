# setup -------

library(tidyverse)
library(readxl)
library(haven)
library(janitor)
library(knitr)


## functions and lists to standardize names
rename_countries <- function(country) {
  case_when(
    country %in% c("Antigua & Barbuda") ~ "Antigua and Barbuda",
    country %in% c("Bahamas, The") ~ "Bahamas",
    country %in% c("Bolivia (Plurinational State of)") ~ "Bolivia",
    country %in% c("Bosnia & Herzegovina") ~ "Bosnia and Herzegovina",
    country %in% c("British Virgin Is.") ~ "British Virgin Islands",
    country %in% c("Brunei") ~ "Brunei Darussalam",
    country %in% c("Buthan") ~ "Bhutan",
    country %in% c("Cape Verde") ~ "Cabo Verde",
    country %in% c("Central African Rep.") ~ "Central African Republic",
    country %in% c("Congo, Dem, Rep,", "Congo, Dem. Rep.", "Congo, Democratic Republic", "Dem. Rep. of the Congo") ~ "Democratic Republic of the Congo",
    country %in% c("Congo", "Congo (Rep. of the)", "Congo, Rep,", "Congo, Rep.", "Congo, Repub. of the") ~ "Republic of the Congo",
    country %in% c("Cote d'Ivoire") ~ "Côte d'Ivoire",
    country %in% c("Curacao") ~ "Curaçao",
    country %in% c("Czechia") ~ "Czech Republic",
    country %in% c("Dominican Rep.") ~ "Dominican Republic",
    country %in% c("East Timor") ~ "Timor-Leste",
    country %in% c("Egypt, Arab Rep.", "Egypt, Arab Rep,") ~ "Egypt",
    country %in% c("Swaziland") ~ "Eswatini",
    country %in% c("Falkland (Malvinas) Is.", "Falkland Islands (Malvinas)") ~ "Falkland Islands",
    country %in% c("Gambia, The") ~ "Gambia",
    country %in% c("Hong Kong, China", "China, Hong Kong Special Administrative Region", "Hong Kong SAR, China") ~ "Hong Kong",
    country %in% c("Iran, Slamic Rep.", "Iran (Islamic Republic of)", "Iran, Islamic Rep,") ~ "Iran",
    country %in% c("Jersey, Channel Islands") ~ "Jersey",
    country %in% c("Kyrgyz Republic") ~ "Kyrgyzstan",
    country %in% c("Lao P.D.R.", "Lao PDR", "Lao People's Democratic Republic", "Laos, PDR") ~ "Laos",
    country %in% c("Luxemburg") ~ "Luxembourg",
    country %in% c("China, Macao Special Administrative Region", "Macao SAR, China", "Macao, China", "Macau") ~ "Macao",
    country %in% c("Macedonia, FYR") ~ "Macedonia",
    country %in% c("Republic of Moldova") ~ "Moldova",
    country %in% c("Micronesia (Federated States of)", "Micronesia, Fed. Sts.", "Micronesia, Fed. St.") ~ "Micronesia",
    country %in% c("Burma") ~ "Myanmar",
    country %in% c("Nepal (Republic of)") ~ "Nepal",
    country %in% c("Netherlands Antilles (former)", "Netherlands Antilles") ~ "Netherlands",
    country %in% c("Dem. People's Rep. of Korea", "Democratic People's Republic of Korea", "Korea, Dem. People's Rep.", "Korea, Dem. Rep.", "Korea, North") ~ "North Korea",
    country %in% c("Macedonia") ~ "North Macedonia",
    country %in% c("N. Mariana Islands") ~ "Northern Mariana Islands",
    country %in% c("Reunion") ~ "Réunion",
    country %in% c("St. Helena") ~ "Saint Helena",
    country %in% c("St. Kitts and Nevis", "Saint Kitts & Nevis") ~ "Saint Kitts and Nevis",
    country %in% c("St. Lucia") ~ "Saint Lucia",
    country %in% c("St. Martin (French part)") ~ "Saint Martin",
    country %in% c("St Pierre & Miquelon") ~ "Saint Pierre and Miquelon",
    country %in% c("St. Vincent and the Grenadines") ~ "Saint Vincent and the Grenadines",
    country %in% c("Sao Tome", "Sao Tome and Principe", "Sco Tomi and Principe", "Sao Tome & Principe") ~ "São Tomé and Principe",
    country %in% c("SerbiaMontenegro") ~ "Serbia",
    country %in% c("Slovak Republic") ~ "Slovakia",
    country %in% c("Korea, Rep.", "Korea (Rep. of)", "Korea, Rep,", "Korea, Rep.", "Korea, South", "Republic of Korea") ~ "South Korea",
    country %in% c("Russian Federation") ~ "Russia",
    country %in% c("Syrian Arab Republic") ~ "Syria",
    country %in% c("Taiwan, China", "Taiwan, Province of China") ~ "Taiwan",
    country %in% c("United Republic of Tanzania") ~ "Tanzania",
    country %in% c("Trinidad & Tobago") ~ "Trinidad and Tobago",
    country %in% c("TC<rkiye", "Turkey", "Turkiye") ~ "Türkiye",
    country %in% c("Turks & Caicos Is") ~ "Turks and Caicos Islands",
    country %in% c("United Kingdom of Great Britain and Northern Ireland") ~ "United Kingdom",
    country %in% c("United States of America") ~ "United States",
    country %in% c("Venezuela (Bolivarian Republic of)", "Venezuela, RB") ~ "Venezuela",
    country %in% c("Viet Nam") ~ "Vietnam",
    country %in% c("Virgin Islands (US)", "Virgin Islands") ~ "Virgin Islands (U.S.)",
    country %in% c("Wallis and Futuna Islands") ~ "Wallis and Futuna",
    country %in% c("Yemen, Rep.") ~ "Yemen",
    country %in% c("Gaza Strip", "Tokelau", "West Bank", "Western Sahara") ~ NA_character_,
    TRUE ~ country
  )
}

missing_regions <- c(
  "Vatican" = "WESTERN EUROPE",
  "Niue" = "OCEANIA",
  "Ascension" = "SUB-SAHARAN AFRICA",
  "Curaçao" = "LATIN AMER. & CARIB",
  "Falkland Islands" = "LATIN AMER. & CARIB",
  "State of Palestine" = "NEAR EAST",
  "Montenegro" = "EASTERN EUROPE",
  "Kosovo" = "EASTERN EUROPE",
  "South Sudan" = "SUB-SAHARAN AFRICA",
  "Channel Islands" = "WESTERN EUROPE",
  "Iran, Islamic Rep." = "NEAR EAST",
  "Saint Martin" = "LATIN AMER. & CARIB",
  "Macedonia" = "EASTERN EUROPE",
  "Bonaire, Sint Eustatius and Saba" = "LATIN AMER. & CARIB",
  "United States Virgin Islands" = "LATIN AMER. & CARIB"
)

## read relevant datasets -----

###
fixed_metrics <- read_csv("countries of the world.csv") %>%
  mutate(country = rename_countries(Country),
         region = Region,
         area = `Area (sq. mi.)`,
         coastline = as.numeric(gsub(",", ".", `Coastline (coast/area ratio)`)),
         .keep = "none") %>%
  filter(!is.na(country))

###
internet_penetration <- read_csv('individuals-using-the-internet_1741283929073.csv') %>%
  mutate(int_pen = dataValue,
         country = rename_countries(entityName),
         year = as.numeric(dataYear)) %>%
  select(country, year, int_pen) %>%
  filter(!is.na(country))

###
mobile_connectivity_index <- read_csv('MCI_Data_2024.csv') %>%
  rename(mci_index = Index,
         mci_infrastructure = Infrastructure,
         mci_affordability = Affordability,
         mci_cons_readiness = `Consumer Readiness`,
         mci_content_services = `Content and Services`
         ) %>%
  mutate(country = rename_countries(Country),
         year = as.numeric(Year)) %>%
  select(country, year, mci_index, mci_infrastructure, mci_affordability, mci_cons_readiness, mci_content_services) %>%
  filter(!is.na(country))


###
trade_to_GDP <- read_csv('API_NE.TRD.GNFS.ZS_DS2_en_csv_v2_75991.csv') %>%
  rename(country = `Country Name`) %>%
  mutate(country = rename_countries(country)) %>%
  pivot_longer(
    cols = `1960`:`2023`,
    names_to = "year",
    values_to = "trad_GDP"
  ) %>%
  select(country, year, trad_GDP) %>%
  mutate(year = as.numeric(year)) %>%
  filter(!is.na(country))

###
file_path <- "International_LPI_from_2007_to_2023_0.xlsx"
sheet_names <- excel_sheets("International_LPI_from_2007_to_2023_0.xlsx")
logistics_performance_index <- map_dfr(sheet_names, ~ read_excel(file_path, sheet = .x) %>%
                                         clean_names() %>%
                                         mutate(year = .x)
) %>%
  relocate(year, .after = country) %>%
  mutate(country = rename_countries(country),
         year = as.numeric(year)) %>%
  select(country, year, lpi_score) %>%
  filter(!is.na(country))

###
modern_renewables <- read_excel('Share of modern renewables database.xlsx') %>%
  rename(country = `Country/Region`) %>%
  mutate(country = rename_countries(country)) %>%
  pivot_longer(
    cols = `1990`:`2021`,
    names_to = "year",
    values_to = "ren_energy"
  ) %>%
  mutate (
    ren_energy = na_if(ren_energy, ".."),
    ren_energy = as.numeric(ren_energy),
    year = as.numeric(year)
  ) %>%
  filter(!is.na(country))

###
political_stability_index <- read_dta('wgidataset.dta') %>%
  select(countryname, year, indicator, estimate) %>%   # only keep relevant columns
  mutate(year = as.numeric(year)) %>%
  filter(!is.na(estimate)) %>%
  pivot_wider(
    names_from = indicator,
    values_from = estimate
  ) %>%
  mutate(
    country = rename_countries(countryname)
  ) %>%
  rename_with(~ paste0("psi_", .), -c(country, year)) %>%
  select(country, year, starts_with("psi_")) %>%
  filter(!is.na(country))

# combining into one table -------
df_list <- list(fixed_metrics, mobile_connectivity_index, internet_penetration, trade_to_GDP, logistics_performance_index, modern_renewables, political_stability_index)

merged_df <- reduce(df_list, full_join) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  filter(
    !country %in% c(
      "Arab World", "Africa (M49)", "Africa Eastern and Southern",
      "Africa Western and Central", "Americas (m49)", "Asia (M49)",
      "Australia and New Zealand (M49)", "Caribbean (M49)",
      "Caribbean small states", "Caucasus and Central Asia (MDG)",
      "Central America (M49)", "Central Asia (M49)",
      "Central Asia (M49) and Southern Asia (MDG=M49)",
      "Central Europe and the Baltics", "Developed regions (MDG)",
      "Developing regions (MDG)", "Early-demographic dividend",
      "East Asia & Pacific", "East Asia & Pacific (excluding high income)",
      "East Asia & Pacific (IDA & IBRD countries)", "Eastern Africa (M49)",
      "Eastern Asia (M49)", "Eastern Asia (M49) and South-eastern Asia (MDG=M49)",
      "Eastern Asia (MDG)", "Eastern Europe (M49)", "Euro area",
      "Europe (M49)", "Europe & Central Asia",
      "Europe & Central Asia (excluding high income)",
      "Europe & Central Asia (IDA & IBRD countries)", "European Union",
      "Fragile and conflict affected situations", "Heavily indebted poor countries (HIPC)",
      "High income", "IBRD only", "IDA & IBRD total", "IDA blend",
      "IDA only", "IDA total", "Landlocked developing countries (LLDCs)",
      "Late-demographic dividend", "Latin America & Caribbean",
      "Latin America & Caribbean (excluding high income)",
      "Latin America & the Caribbean (IDA & IBRD countries)",
      "Latin America and the Caribbean (MDG=M49)",
      "Least Developed Countries (LDCs)", "Least developed countries: UN classification",
      "Low & middle income", "Low income", "Lower middle income",
      "Melanesia (M49)", "Micronesia (M49)", "Middle Africa (M49)",
      "Middle East & North Africa", "Middle East & North Africa (excluding high income)",
      "Middle East & North Africa (IDA & IBRD countries)", "Middle income",
      "North America", "Northern Africa (M49)", "Northern Africa (MDG)",
      "Northern America (M49)", "Northern America (M49) and Europe (M49)",
      "Northern Europe (M49)", "Not available", "Not classified",
      "Oceania (M49)", "Oceania (M49) excluding Australia and New Zealand (M49)",
      "Oceania (MDG)", "OECD members", "Other small states",
      "Pacific island small states", "Polynesia (M49)", "Post-demographic dividend",
      "Pre-demographic dividend", "Sint Maarten (Dutch part)",
      "Small island developing States (SIDS)", "Small states",
      "South America (M49)", "South Asia", "South Asia (IDA & IBRD)",
      "South-eastern Asia (MDG=M49)", "Southern Africa (M49)",
      "Southern Asia (MDG=M49)", "Southern Europe (M49)", "Sub-Saharan Africa",
      "Sub-Saharan Africa (excluding high income)", "Sub-Saharan Africa (IDA & IBRD countries)",
      "Sub-Saharan Africa (M49)", "Sub-Saharan Africa (MDG)", "Upper middle income",
      "West Bank and Gaza", "Western Africa (M49)", "Western Asia (M49)",
      "Western Asia (M49) and Northern Africa (M49)", "Western Asia (MDG)",
      "Western Europe (M49)", "World"
    ),
    !grepl("Western Asia.*Northern Africa", country)
  ) %>%
  arrange(country)

rm(fixed_metrics, internet_penetration, mobile_connectivity_index, trade_to_GDP, 
   logistics_performance_index, modern_renewables, political_stability_index,
   file_path, sheet_names, rename_countries, df_list)


# Troubleshooting merging -----

merged_df <- internet_penetration %>%
  full_join(mobile_connectivity_index)  %>%
  full_join(trade_to_GDP) %>%
  full_join(logistics_performance_index) %>%
  full_join(modern_renewables) %>%
  full_join(political_stability_index) %>%
  full_join(fixed_metrics,
            by = "country",
            relationship = "many-to-many") %>%
  mutate(region = if_else(is.na(region), recode(country, !!!missing_regions), region),
         region = case_when(
           region == "LATIN AMER. & CARIB" ~ "Latin America and Caribbean",
           region == "NEAR EAST" ~ "Near East",
           region == "SUB-SAHARAN AFRICA" ~ "Sub-Saharan Africa",
           region == "WESTERN EUROPE" ~ "Western Europe",
           region == "EASTERN EUROPE" ~ "Eastern Europe",
           TRUE ~ str_to_title(region))
         ) %>%
  select(country, region, year, everything()) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  filter(
    !country %in% c(
      "Arab World", "Africa (M49)", "Africa Eastern and Southern",
      "Africa Western and Central", "Americas (m49)", "Asia (M49)",
      "Australia and New Zealand (M49)", "Caribbean (M49)",
      "Caribbean small states", "Caucasus and Central Asia (MDG)",
      "Central America (M49)", "Central Asia (M49)",
      "Central Asia (M49) and Southern Asia (MDG=M49)",
      "Central Europe and the Baltics", "Developed regions (MDG)",
      "Developing regions (MDG)", "Early-demographic dividend",
      "East Asia & Pacific", "East Asia & Pacific (excluding high income)",
      "East Asia & Pacific (IDA & IBRD countries)", "Eastern Africa (M49)",
      "Eastern Asia (M49)", "Eastern Asia (M49) and South-eastern Asia (MDG=M49)",
      "Eastern Asia (MDG)", "Eastern Europe (M49)", "Euro area",
      "Europe (M49)", "Europe & Central Asia",
      "Europe & Central Asia (excluding high income)",
      "Europe & Central Asia (IDA & IBRD countries)", "European Union",
      "Fragile and conflict affected situations", "Heavily indebted poor countries (HIPC)",
      "High income", "IBRD only", "IDA & IBRD total", "IDA blend",
      "IDA only", "IDA total", "Landlocked developing countries (LLDCs)",
      "Late-demographic dividend", "Latin America & Caribbean",
      "Latin America & Caribbean (excluding high income)",
      "Latin America & the Caribbean (IDA & IBRD countries)",
      "Latin America and the Caribbean (MDG=M49)",
      "Least Developed Countries (LDCs)", "Least developed countries: UN classification",
      "Low & middle income", "Low income", "Lower middle income",
      "Melanesia (M49)", "Micronesia (M49)", "Middle Africa (M49)",
      "Middle East & North Africa", "Middle East & North Africa (excluding high income)",
      "Middle East & North Africa (IDA & IBRD countries)", "Middle income",
      "North America", "Northern Africa (M49)", "Northern Africa (MDG)",
      "Northern America (M49)", "Northern America (M49) and Europe (M49)",
      "Northern Europe (M49)", "Not available", "Not classified",
      "Oceania (M49)", "Oceania (M49) excluding Australia and New Zealand (M49)",
      "Oceania (MDG)", "OECD members", "Other small states",
      "Pacific island small states", "Polynesia (M49)", "Post-demographic dividend",
      "Pre-demographic dividend", "Sint Maarten (Dutch part)",
      "Small island developing States (SIDS)", "Small states",
      "South America (M49)", "South Asia", "South Asia (IDA & IBRD)",
      "South-eastern Asia (MDG=M49)", "Southern Africa (M49)",
      "Southern Asia (MDG=M49)", "Southern Europe (M49)", "Sub-Saharan Africa",
      "Sub-Saharan Africa (excluding high income)", "Sub-Saharan Africa (IDA & IBRD countries)",
      "Sub-Saharan Africa (M49)", "Sub-Saharan Africa (MDG)", "Upper middle income",
      "West Bank and Gaza", "Western Africa (M49)", "Western Asia (M49)",
      "Western Asia (M49) and Northern Africa (M49)", "Western Asia (MDG)",
      "Western Europe (M49)", "World"
    ),
    !grepl("Western Asia.*Northern Africa", country)
  ) %>%
  arrange(country, year)

# run this
test <- merged_df %>% count(region)




