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
  species = c("Cx tarsalis", "Cx pipiens"),
  trap = "CO2",
  separate_by = NULL
)


## -----------------------------------------------------------------------------

getAbundanceAnomaly(sample_collections,
                    interval = "Biweek",
                    target_year = 2020,
                    species = c("Cx tarsalis", "Cx pipiens"),
                    trap = "CO2",
                    separate_by  = "species") 

## -----------------------------------------------------------------------------
getInfectionRate(sample_pools, 
                      interval = "Week",
                      target_disease = "WNV",
                      pt_estimate = "mle", 
                      scale = 1000,
                      species = c("Cx pipiens", "Cx tarsalis"),
                      trap = c("CO2"),
                      separate_by="species")




## -----------------------------------------------------------------------------
getVectorIndex(sample_collections, 
               sample_pools,
               interval = "Biweek",
               target_disease = "WNV",
               pt_estimate = "bc-mle", 
              
               separate_by = c("agency","species")) 
sample_collections%>%filter(species_display_name=="Cx tarsalis", trap_acronym=="CO2")

## -----------------------------------------------------------------------------
getPoolsComparisionTable(
  sample_pools,
  interval = "Week",
  target_disease = "WNV"
)

## -----------------------------------------------------------------------------


library(kableExtra)

AbAnOutput = getAbundance(
  sample_collections,
  interval = "Biweek",
  
  species = c("Cx tarsalis", "Cx pipiens"),
  trap = "CO2",
  separate_by = "species")

head(AbAnOutput)

#kable table where column names, font_size, style and much more can be customized

AbAnOutput %>%
  kbl() %>%
  kable_styling(
    bootstrap_options = "striped",
    font_size = 14,
    latex_options = "scale_down"
  ) %>%
  footnote(general = "Table X: Combined biweekly Abundance Calculation for Cx. tarsalis, pipiens in CO2 traps", general_title = "")


## -----------------------------------------------------------------------------
library(DT)

AbAnOutput %>%
  datatable(colnames =  c("Disease Year", "Biweek", "Count", "Species","Trap Type","Trap Events", "Abundance"))

## -----------------------------------------------------------------------------

table(vectorsurvR:::testing_collections$trap_acronym, vectorsurvR:::testing_collections$surv_year) %>%
  kbl(align = "c") %>%
  kable_paper(
    full_width = F,
    html_font = "arial",
    lightable_options = "striped",
  ) %>%
  add_header_above(c("Trap Type", "Years" = 6)) %>%
  footnote(general = "Table 3: Traps deployed by year", general_title = "") %>%
  row_spec(c(3, 9, 10), background = "yellow") %>%
  column_spec(c(4), background = "orange")

