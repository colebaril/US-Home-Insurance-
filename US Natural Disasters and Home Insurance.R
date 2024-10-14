require(pacman)
p_load(tidyverse, here, janitor, usmap, patchwork)

state_lookup <- tibble(
  state_full = c("Alabama", "Alaska", "American Samoa", "Arizona", "Arkansas", 
                 "California", "Colorado", "Connecticut", "Delaware", 
                 "District of Columbia", "Florida", "Georgia", "Guam", 
                 "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", 
                 "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", 
                 "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", 
                 "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                 "New York", "North Carolina", "North Dakota", "Northern Mariana Islands", 
                 "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico", 
                 "Rhode Island", "South Carolina", "South Dakota", "Tennessee", 
                 "Texas", "Utah", "Vermont", "Virginia", "Virgin Islands", "Washington", 
                 "West Virginia", "Wisconsin", "Wyoming"),
  state_code = c("AL", "AK", "AS", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", 
                 "GA", "GU", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", 
                 "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
                 "NM", "NY", "NC", "ND", "MP", "OH", "OK", "OR", "PA", "PR", "RI", 
                 "SC", "SD", "TN", "TX", "UT", "VT", "VA", "VI", "WA", "WV", "WI", "WY")
)


# Hurricanes
hurricanes <- read_csv(here("Raw/hurricane_landfall_noaa_usa.csv")) |> 
  clean_names() |> 
  drop_na(landfall_state) |> 
  group_by(landfall_state) |> 
  summarise(n = n()) |> 
  rename("state" = "landfall_state")

hurricane_plot <- plot_usmap(data = hurricanes, values = "n", exclude = c("AK", "HI")) +
  scale_fill_viridis_c("Number of \nHurricane \nLandfalls", option = "cividis", na.value="white") +
  theme_void(base_size = 20) +
  labs(title = "Number of U.S. Hurricane Landfalls, 1850-2023") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Tornadoes 

area <- read_csv(here("Raw/us_land_area.csv")) |> 
  row_to_names(1) |> 
  rename("state" = 1) |> 
  mutate(state = iconv(state, from = "latin1", to = "UTF-8"),
         state = gsub("[^[:alnum:] ]", "", state)) |> 
  mutate(state = gsub("ÿ", "", state)) |> 
  left_join(state_lookup, by = c("state" = "state_full")) |> 
  select(-"state")

tornado <- read_csv(here("Raw/1950-2023_all_tornadoes.csv")) |> 
  group_by(st) |>
  summarise(n = n()) |>
  rename("state" = "st") |> 
  left_join(area, by = c("state" = "state_code")) |> 
  mutate(km2 = as.numeric(km2)) |> 
  mutate(tornadoes_per_100sqkm = (n / km2) * 100) |> 
  filter(state != "DC")

tornado_plot <- plot_usmap(data = tornado, values = "tornadoes_per_100sqkm", exclude = c("AK", "HI")) +
  scale_fill_viridis_c("Tornadoes per \n100 square \nkilometres", option = "plasma",
                       labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme_void(base_size = 20) +
  labs(title = "Number of U.S. Tornadoes, 1950-2023") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Insurance

insurance <- read_csv(here("Raw/us_home_insurance_rates.csv")) |> 
  clean_names()

insurance_plot <- plot_usmap(data = insurance, values = "average_annual_premium", exclude = c("AK", "HI")) +
  scale_fill_viridis_c("Average Home \nInsurance Premium",
                       labels=function(x) paste0("$", format(x, big.mark = ",", scientific = FALSE))) +
  theme_void(base_size = 20) +
  labs(title = "Average U.S. Home Insurance Premiums, 2024") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = c(1.1, 0.5))

# Adjusting patchwork layout
(hurricane_plot + tornado_plot) / insurance_plot +
  plot_layout(heights = c(1, 1), widths = c(1, 1)) +
  plot_annotation(
    title = "Weathering the Cost: How Hurricanes and Tornadoes Drive U.S. Home Insurance Premiums",
    caption = "Viz: Cole Baril - colebaril.ca | Data: NOAA & Bankrate | Software: R",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"))
  )

ggsave(plot = last_plot(), filename = "us_natural_disasters_home_insurance.png",
       dpi = 300,
       width = 20,
       height = 12)

