## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",size = 10,
  fig.height = 6,
  fig.width = 6
  
)


## ----include=FALSE------------------------------------------------------------
##Load package
library(vectorsurvR)


## ----results='hide', eval=F---------------------------------------------------
#  
#  token = getToken()
#  

## ----eval=F, echo=T-----------------------------------------------------------
#  #Example
#  collections = getArthroCollections(token, 2022,2023, 'mosquito',55)

## ----eval=F, echo=T-----------------------------------------------------------
#  #Example
#  pools = getPools(token, 2022,2023, 'mosquito')

## ----eval=F, echo=T-----------------------------------------------------------
#  #creates a file named "collections_18_23.csv" in your current directory
#  write.csv(x = collections, file = "collections_22_23.csv")
#  
#  #loads collections data
#  collections = read.csv("collections_22_23.csv")
#  

## -----------------------------------------------------------------------------
#Subset using column names or index number


colnames(sample_collections) #displays column names and associated index



#Subseting by name
head(sample_collections[c("collection_date", "species_display_name", "num_count")])

#by index
head(sample_collections[c(2, 4, 10)])

#to save a subset
collections_subset = sample_collections[c(2, 4, 10)]

## -----------------------------------------------------------------------------
#NOTE: library was loaded above
library(dplyr)

#Subsetting columns with 'select()'
sample_collections %>%
  dplyr::select(collection_date, species_display_name, num_count) %>% head()


## -----------------------------------------------------------------------------

#filtering with dplyr 'filter'
collections_pip = sample_collections %>%
  filter(species_display_name == "Cx pipiens")

#filtering multiple arguments using '%in%'
collections_pip_tar = sample_collections %>%
  filter(species_display_name %in% c("Cx pipiens", "Cx tarsalis"))


## -----------------------------------------------------------------------------
#groups by species and collection date and sums the number counted

sample_collections %>%
  group_by(collection_date, species_display_name) %>%
  summarise(sum_count = sum(num_count, na.rm = T)) %>%
  head()


#groups by species and collection date and takes the average the number counted

sample_collections %>%
  group_by(collection_date, species_display_name) %>%
  summarise(avg_count = mean(num_count, na.rm = T)) %>%
  head()

## -----------------------------------------------------------------------------
library(tidyr)

collections_wide = pivot_wider(
  sample_collections,
  names_from = c("species_display_name","sex_type"),
  values_from = "num_count"
)

## -----------------------------------------------------------------------------
getAbundance(
  sample_collections,
  interval = "Biweek",
  species_list = c("Cx tarsalis", "Cx pipiens"),
  trap_list = "CO2",
  species_separate = FALSE
)


## -----------------------------------------------------------------------------

getAbundanceAnomaly(sample_collections,
                    interval = "Biweek",
                    target_year = 2020,
                    species_list = c("Cx tarsalis", "Cx pipiens"),
                    trap_list = "CO2",
                    species_separate = FALSE) 

## -----------------------------------------------------------------------------
IR = getInfectionRate(sample_pools, 
                      interval = "Week",
                      target_disease = "WNV",
                      pt_estimate = "mle", 
                      scale = 1000,
                      species_list = c("Cx pipiens"),
                      trap_list = c("CO2","GRVD") )
IR

## -----------------------------------------------------------------------------


getVectorIndex(sample_collections, sample_pools, interval = "Biweek",
                           
                           target_disease = "WNV", pt_estimate = "bc-mle",
                           species_list=c("Cx tarsalis"), 
                           
                           trap_list =  c("CO2"))


## -----------------------------------------------------------------------------
getPoolsComparisionTable(
  sample_pools,
  interval = "Week",
  target_disease = "WNV",
  species_separate = T
)

## -----------------------------------------------------------------------------


library(kableExtra)

AbAnOutput = getAbundance(
  sample_collections,
  interval = "Biweek",
  
  species_list = c("Cx tarsalis", "Cx pipiens"),
  trap_list = "CO2",
  species_separate = FALSE
)
head(AbAnOutput)

#kable table where column names, font_size, style and much more can be customized

AbAnOutput %>%
  kbl(col.names = c("Disease Year", "Biweek", "Count", "Trap Events", "Abundance")) %>%
  kable_styling(
    bootstrap_options = "striped",
    font_size = 14,
    latex_options = "scale_down"
  ) %>%
  footnote(general = "Table X: Combined biweekly Abundance Calculation for Cx. tarsalis, pipiens in CO2 traps", general_title = "")


## -----------------------------------------------------------------------------
library(DT)

AbAnOutput %>%
  datatable(colnames =  c("Disease Year", "Biweek", "Count", "Trap Events", "Abundance"))

## -----------------------------------------------------------------------------
library(ggplot2)
library(lubridate)
#creates a month column and translates numerics
sample_collections$month = as.factor(month(sample_collections$collection_date))


collections_sums = sample_collections %>%
  filter(
    species_display_name %in% c(
      "Cx tarsalis",
      "Cx pipiens",
      "An freeborni",
      "Cs incidens",
      "Ae melanimon",
      "Cs inornata",
      "Cx stigmatosoma",
      "Cx erythrothorax",
      "Ae vexans",
      "I pacificus"
    )
  ) %>%
  group_by(month, species_display_name) %>%
  summarise(sum_count = sum(num_count, na.rm = T)) %>% arrange(desc(sum_count), .by_group = T)



#ggplot with dots a values for each species of interest

ggplot(data = collections_sums,
       aes(x = month, y = sum_count, color = species_display_name)) +
  geom_point()

#bar chart
ggplot(data = collections_sums,
       aes(x = month, y = sum_count, fill = species_display_name)) +
  geom_bar(stat = "identity")


#adding labels
ggplot(data = collections_sums,
       aes(x = month, y = sum_count, fill = species_display_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Mosquito Counts by Month and Species",
       x = "Month",
       y = "Sum of Mosquitoes",
       fill = "Species")

## -----------------------------------------------------------------------------



AbAnOut = getAbundanceAnomaly(
  sample_collections,
  interval = "Biweek",
  target_year = 2020,
  species_list = c("Cx tarsalis", "Cx pipiens"),
  species_separate = TRUE
)



AbAnOut_L = processAbunAnom(AbAnOut)

## -----------------------------------------------------------------------------


AbAnOut_L %>%  filter(Abundance_Type %in% c("2020_Abundance",
                                            "Five_Year_Avg")) %>%
  ggplot(aes(x = Biweek,
             y = Abundance_Calculation,
             color = Abundance_Type)) +
  geom_point() +
  geom_line() +
  facet_wrap( ~ species_display_name) +
  labs(title = "2020 Abundance Anomaly", y = "")


## -----------------------------------------------------------------------------
AbAnOut_L %>%
  filter(Abundance_Type == "Delta") %>%
  mutate(Change = ifelse(Abundance_Calculation > 0, "Increase", "Decrease")) %>%
  ggplot(aes(x = Biweek,
             y = Abundance_Calculation,
             fill = Change)) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ species_display_name) +
  labs(x = "Biweek",
       y = "Percent Change",
       title = "Relative Abundance 2023, % Change from 5-year average",
       fill = "Relative Change")


## -----------------------------------------------------------------------------
IR = getInfectionRate(
  sample_pools,
  interval = "Week",
  target_disease = "WNV",
  pt_estimate = "mle",
  species_list = c("Cx pipiens"),
  trap_list = c("CO2", "GRVD")
)

plotInfectionRate(InfRtOutput = IR, year = 2020)


## -----------------------------------------------------------------------------

table(sample_collections$trap_acronym, sample_collections$surv_year) %>%
  kbl(align = "c") %>%
  kable_paper(
    full_width = F,
    html_font = "arial",
    lightable_options = "striped",
  ) %>%
  add_header_above(c("Trap Type", "Years" = 5)) %>%
  footnote(general = "Table X: Traps deployed by year", general_title = "") %>%
  row_spec(c(3, 9, 10), background = "yellow") %>%
  column_spec(c(4), background = "orange")

