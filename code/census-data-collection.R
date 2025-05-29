library(tidycensus)
library(dplyr)
library(tidyr)
library(sf)
library(readr)
library(purrr)


# Get Census accsess
readRenviron("~/.Renviron")  
api_key <- Sys.getenv("CENSUS_API_KEY")
census_api_key(api_key, install = TRUE, overwrite=TRUE)

# API key stored in R enviroment, to run without it, use: 
# census_api_key("YOUR_KEY_HERE", install = TRUE)
# readRenviron("~/.Renviron")

# Census variables 
vars_dec2020 <- load_variables(year = 2020, dataset = "pl", cache = TRUE)
head(vars_dec2020)

dec_vars <- c(
  total_pop        = "P2_001N", # !!Total:
  hispanic         = "P2_002N", # !!Total:!!Hispanic or Latino
  nonhisp_white    = "P2_005N", # !!Total:!!Not Hispanic or Latino:!!Population of one race:!!White alone
  nonhisp_black    = "P2_006N", # !!Total:!!Not Hispanic or Latino:!!Population of one race:!!Black or African American alone
  nonhisp_ai_an    = "P2_007N", # !!Total:!!Not Hispanic or Latino:!!Population of one race:!!American Indian and Alaska Native alone
  nonhisp_asian    = "P2_008N", # !!Total:!!Not Hispanic or Latino:!!Population of one race:!!Asian alone
  nonhisp_nh       = "P2_009N", # !!Total:!!Not Hispanic or Latino:!!Population of one race:!!Native Hawaiian and Other Pacific Islander alone
  nonhisp_other    = "P2_010N", # !!Total:!!Not Hispanic or Latino:!!Population of one race:!!Some Other Race alone
  nonhisp_two_more = "P2_011N"  # !!Total:!!Not Hispanic or Latino:!!Population of two or more races:
)

decennial_county_sf <- get_decennial(
  geography = "county",
  variables = dec_vars,
  year      = 2020,
  dataset   = "pl",
  geometry  = FALSE # not collecting geometry here because of the API bug
) %>%
  select(GEOID, NAME, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value)

# ACS Variables
vars_acs <- load_variables(2020, "acs5", cache = TRUE)

acs_income_sf <- get_acs(
  geography = "county",
  variables = c(median_income = "B19013_001"), # Estimate!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)
  year      = 2020,
  survey    = "acs5",
  geometry  = TRUE
) %>%
  select(GEOID, NAME, median_income = estimate, geometry)

# Separate name into county and state
acs_income_sf <- acs_income_sf %>%
  separate(NAME, into = c("county", "state"), sep = ", ")

# Join data on GEOID field
county_2020_sf <- acs_income_sf %>%
  left_join(decennial_county_sf, by = "GEOID")

# Export Data
st_write(
  county_2020_sf,
  "data/final/county_2020_data.shp",
  delete_layer = TRUE
)

df_out <- county_2020_sf %>%
  mutate(wkt = st_as_text(geometry)) %>%
  select(-geometry)   

df_out <- st_drop_geometry(df_out)

write_csv(df_out, "data/final/county_2020_wkt.csv")

# CENSUS BLOCK DATA

regions <- list(
  NYC      = tibble(state = "NY", counties = c("New York", "Kings", "Queens", "Bronx", "Richmond")),
  Philadelphia = tibble(state = "PA", counties = "Philadelphia"),
  Worcester    = tibble(state = "MA", counties = "Worcester")
)

# Prepare a list to collect each region's data
bg_list <- list()

for (nm in names(regions)) {
  cfg <- regions[[nm]]
  
  # PL counts, collapsing duplicates by summing
  pl_bg <- get_decennial(
    geography = "block group",
    dataset   = "pl",
    year      = 2020,
    state     = cfg$state,
    county    = cfg$counties,
    variables = dec_vars,
    geometry  = FALSE
  ) %>%
    pivot_wider(
      names_from   = variable,
      values_from  = value,
      values_fn    = list(value = sum),
      values_fill  = list(value = 0)
    )
  
  # ACS income + geometry
  acs_bg <- get_acs(
    geography = "block group",
    survey    = "acs5",
    year      = 2020,
    state     = cfg$state,
    county    = cfg$counties,
    variables = c(median_income = "B19013_001"),
    geometry  = TRUE
  ) %>%
    rename(median_income = estimate) %>%
    separate(NAME, into = c("blockgroup","tract","county","state"), sep = ", ", fill = "right")
  
  # join and tag
  bg_list[[nm]] <- acs_bg %>%
    left_join(pl_bg, by = "GEOID") %>%
    mutate(region = nm)
}

# merge the data
all_bg_sf <- bind_rows(bg_list)

# fill out missing values 
all_bg_sf <- all_bg_sf %>%
  mutate(across(c(names(dec_vars), "median_income", 'moe'), ~replace_na(.x, 0)))

# Export Data
st_write(
  all_bg_sf,
  "data/final/bg_2020_data.shp",
  delete_layer = TRUE
)

df_out <- all_bg_sf %>%
  mutate(wkt = st_as_text(geometry)) 

df_out <- st_drop_geometry(df_out)

write_csv(df_out, "data/final/bg_2020_wkt.csv")


