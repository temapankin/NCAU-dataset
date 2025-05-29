library(tidycensus)
library(dplyr)
library(tidyr)
library(sf)
library(readr)
library(purrr)

# Define ACS variables
acs_vars <- c(
  total_pop        = "B03002_001",  # Total population (universe)
  hispanic         = "B03002_012",  # Estimate!!Total:!!Hispanic or Latino:
  nonhisp_white    = "B03002_003",  # White alone, NOT Hispanic or Latino
  nonhisp_black    = "B03002_004",  # Black alone, NOT Hispanic or Latino
  nonhisp_ai_an    = "B03002_005",  # American Indian & Alaska Native alone, NOT Hispanic or Latino
  nonhisp_asian    = "B03002_006",  # Asian alone, NOT Hispanic or Latino
  nonhisp_nh       = "B03002_007",  # Native Hawaiian & Other Pacific Islander alone, NOT Hispanic
  nonhisp_other    = "B03002_008",  # Some other race alone, NOT Hispanic
  nonhisp_two_more = "B03002_009",  # Two or more races, NOT Hispanic
  median_income    = "B19013_001"   # Estimate!!Median household income in the past 12 months (in 2023 inflation-adjusted dollars)
)

# Pull ACS
county_acs <- get_acs(
  geography = "county",
  variables = acs_vars,
  year      = 2023,     # latest ACS 5-year available
  survey    = "acs5",
  geometry  = TRUE
) %>%
  # select the columns we need
  select(GEOID, NAME, variable, estimate, geometry) %>%
  # spread the variable names into real columns
  pivot_wider(
    names_from  = variable,
    values_from = estimate
  ) %>%
  separate(NAME, into = c("county","state"), sep = ", ", fill="right")

# Export Data
st_write(
  county_acs,
  "data/final/county_acs_2023.shp",
  delete_layer = TRUE
)

df_out <- county_acs %>%
  mutate(wkt = st_as_text(geometry)) 

df_out <- st_drop_geometry(df_out)

write_csv(df_out, "data/final/county_acs_2023_wkt.csv")

# BLOCK GROUP DATA
regions <- list(
  NYC          = list(state = "NY", counties = c("New York","Kings","Queens","Bronx","Richmond")),
  Philadelphia = list(state = "PA", counties = "Philadelphia"),
  Worcester    = list(state = "MA", counties = "Worcester")
)

# Loop over each region, pull & reshape
bg_list <- list()
for (nm in names(regions)) {
  cfg <- regions[[nm]]
  
  acs_bg <- get_acs(
    geography = "block group",
    survey    = "acs5",
    year      = 2023,
    state     = cfg$state,
    county    = cfg$counties,
    variables = acs_vars,
    geometry  = TRUE
  ) %>%
    select(GEOID, NAME, variable, estimate, geometry) %>%
    pivot_wider(
      names_from  = variable,
      values_from = estimate,
      values_fill = list(estimate = 0)
    ) %>%
    separate(NAME, into = c("blockgroup","tract","county","state"), sep = "; ", fill = "right") %>%
    mutate(region = nm)
  
  bg_list[[nm]] <- acs_bg
}

# Combine into one sf
all_bg_acs_sf <- bind_rows(bg_list)

all_bg_sf <- all_bg_acs_sf %>%
  mutate(across(c(names(acs_vars)), ~replace_na(.x, 0)))

st_write(
  all_bg_sf,
  "data/final/bg_acs_2023.shp",
  delete_layer = TRUE
)

df_out <- all_bg_sf %>%
  mutate(wkt = st_as_text(geometry)) 

df_out <- st_drop_geometry(df_out)

write_csv(df_out, "data/final/bg_acs_2023_wkt.csv")
