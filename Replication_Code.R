# Gabby Shaffer
# GEOG 490 
# DUE May 17, 2026 
# Replication Study
# Reference texts: Walker 2020 & Hanberry 2022

#----------------------------------------------
# Libraries: ggplot, Tidyverse, Tidycensus, tigris
library(tigris)
library(tidyverse)
library(sf)
library(crsuggest)
library(tidycensus)
library(tmap)
library(ggplot2)
library(scales)
library(ragg) # for help with tmap save
options(tigris_use_cache = TRUE)

#----------------------------------------------
#Working with the Inland Empire, California

# checking list of cbsas
cbsa <- core_based_statistical_areas()

# selecting inland empire as study area 
# Inland Empire = Riverside & San Bernardino counties in CA  
inland_empire <- core_based_statistical_areas(cb= TRUE, 
                                         year = 2020) %>% 
  filter(str_detect(NAME, "Riverside-San Bernardino-Ontario, CA"))

# getting the correct coordinate system before transformation
suggest_crs(inland_empire)

# CRS - NAD83 / California Zone 6 (26946) (meters)
ie_tracts <- tracts("CA", 
                    c("Riverside", 
                      "San Bernardino")) 

# getting total number of tracts & plotting them 
nrow(ie_tracts)
plot(ie_tracts$geometry)

# getting total population race/ethnicity 
two_county_stats <- get_decennial(geography = "county", 
                              state = "CA", 
                              county = c("Riverside", 
                                         "San Bernardino"),
                              variables = c(
                                Hispanic = "P2_002N",
                                White = "P2_005N", 
                                Black = "P2_006N", 
                                Native = "P2_007N", 
                                Asian = "P2_008N", 
                                total = "P2_001N"
                              ), 
                              year = 2020, 
                              geometry = TRUE) %>% 
  mutate(county = str_remove(NAME, "County, California"))

county_race_plot <- two_county_stats %>%
  filter(variable != "total") %>%
  ggplot(aes(x = county, y = value, fill = variable)) +
  geom_col(position = "stack") + 
  scale_fill_viridis_d(name = "Race/Ethnicity") + 
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Race/Ethnicity in Inland Empire, CA", 
       subtitle = "Decennial Census (2020)", 
       x = "County", 
       y = "Population") + 
  theme_minimal(base_size = 12.5, 
                base_family = "Verdana")

ggsave("race_ethnicity_chart.png", plot = county_race_plot,)

#----------------------------------------------
# Population Densities & Classifications 

# getting the census data based on inland empire counties 
ie_tpop <- get_decennial(geography = "tract", 
                         state = "CA", 
                         county = c("Riverside", 
                                    "San Bernardino"),
                         variables = "P1_001N", 
                         year = 2020, 
                         geometry = TRUE, 
                         keep_geo_vars = TRUE) %>%
  mutate(land_area = ALAND / 1e6) # adding column for land area in square km

# calculating density w/popdensity --> population / land area 
ie_density <- ie_tpop %>%
  mutate(pop_density = value/land_area) 

# setting column for categories 
ie_density <- ie_density %>%
  mutate(category = pop_density) 

# setting variables for 5 categories
ie_classes <- ie_density %>%
  mutate(category = case_when(
    category < 250 ~ "Exurban", 
    category < 550 ~ "Suburban Low", 
    category < 800 ~ "Suburban High", 
    category < 1900 ~ "Urban Low", 
    category >= 1900 ~ "Urban High"
  )) %>%
  mutate(category = factor(category, levels = c(
    "Exurban", "Suburban Low", "Suburban High", "Urban Low", "Urban High"
  )))

# mapping the categories with tmap 
categories_map <- tm_shape(ie_classes, unit = "m") + 
  tm_polygons(
    fill = "category", 
    fill.scale = tm_scale_categorical(
      values = "brewer.yl_gn_bu" 
    ),
    fill.legend = tm_legend(
      title = "Category", 
      position = tm_pos("right", "center")
    ),
    lwd = 0.3
  ) + 
  tm_title("Population Density Categories in the Inland Empire, CA", 
           size = 0.95, 
           ) +  
  tm_layout(
    frame = FALSE, 
    text.fontfamily = "Verdana")

# plotting the map
categories_map

# checking wd and making sure file saves in the correct place 
wd <-getwd()

# saving the map 
tmap_save(
  tm = categories_map,
  filename = file.path(wd, "ie_cat_map.png"),
  height = 5.5,
  width = 8,
  dpi = 300,
  device = ragg::agg_png
)

# checking if the image saved
file.exists("ie_cat_map.png")

#----------------------------------------------
# WORKING WITH WITH MEDIAN INCOME
# using 2020 acs5
variable_list <- load_variables(2020, "acs5", cache = TRUE)

# median income variable 
ie_median_income <- get_acs(
  geography = "tract", 
  state = "CA", 
  county = c("Riverside", 
             "San Bernardino"),
  variables = "B19013_001", 
  year = 2020, 
  geometry = TRUE
)

# joining with median income dataset
ie_median_income_joined <- ie_median_income %>%
  left_join(
    st_drop_geometry(ie_classes) %>% select(GEOID, category, pop_density),
    by = "GEOID"
  )

# summary stats for median income 
income_stats <- ie_median_income_joined %>%
  summarize(
    mean = mean(estimate, na.rm = TRUE),
    median = median(estimate, na.rm = TRUE),
    sd = sd(estimate, na.rm = TRUE),
    min = min(estimate, na.rm = TRUE),
    max = max(estimate, na.rm = TRUE)
  )

# making a scatterplot with median income with ggplot
income_scatterplot <- ie_median_income_joined %>%
  filter(!is.na(estimate), !is.na(pop_density)) %>%
  ggplot(aes(x= pop_density, 
                                    y = estimate, 
                                    fill = category)) + 
  geom_point(size = 3, alpha = 0.9, shape = 21, color = "black") +         
  scale_fill_brewer(palette = "YlGnBu",
                    name = "Density Category") +    
  labs(title = "Median household income", 
       subtitle = "Inland Empire Census Tracts", 
       x = "Population Density", 
       y = "ACS Median Income Esimate") +
  theme_minimal(base_size = 12.5, 
                base_family = "Verdana") +
  scale_y_continuous(labels = label_dollar())


ggsave(
  "ie_median_inc.png",
  plot = income_scatterplot, 
  width = 10,
  height = 7,
  units = "in",
  dpi = 300,
  bg = "white"
)


#----------------------------------------------
# SCATTERPLOT WITH COLLEGE DEGREES

# has college degree? 
ie_college <- get_acs(
  geography = "tract", 
  state = "CA", 
  county = c("Riverside", 
             "San Bernardino"),
  variables = "B15003_022", 
  year = 2020, 
  geometry = TRUE
)

# joining the datasets + getting pct with degree out of total pop
ie_college_joined <- ie_college %>%
  left_join(
    st_drop_geometry(ie_classes) %>% select(GEOID, category, pop_density, value),
    by = "GEOID"
  ) %>%
  mutate(tpop = value) %>%
  mutate(pct_degree = estimate / tpop)

# plotting data 
college_scatterplot <- ggplot(ie_college_joined, aes(x = pop_density, 
                              y = pct_degree, 
                              fill = category)) + 
  geom_point(size = 3, alpha = 0.9, shape = 21, color = "black") +         
  scale_fill_brewer(palette = "YlGnBu",
                    name = "Density Category") +    
  labs(title = "College Degree Holders in the Inland Empire by Census Tracts", 
       subtitle = "(Percent of total population)", 
       x = "Population Density", 
       y = "ACS College Rate") +
  theme_minimal(base_size = 12.5, 
                base_family = "Verdana")

ggsave(
  "ie_college.png",
  plot = college_scatterplot, 
  width = 10,
  height = 7,
  units = "in",
  dpi = 300,
  bg = "white"
)


#----------------------------------------------
# MAP WITH POVERTY RATES  

# mapping poverty rates
ie_poverty_status <- get_acs(
  geography = "tract", 
  state = "CA", 
  county = c("Riverside", 
             "San Bernardino"),
  variables = "B17001_002", 
  summary_var = "B17001_001",
  year = 2020, 
  geometry = TRUE
) %>%
  mutate(pov_rate = estimate/summary_est)

ie_pov_status_join <- ie_poverty_status %>%
  left_join(
    st_drop_geometry(ie_classes) %>% select(GEOID, category, pop_density),
    by = "GEOID"
  )

# dropping missing values before mapping 
ie_pov_status_join <- ie_pov_status_join[!is.na(ie_pov_status_join$pov_rate), ]

poverty_map <- tm_shape(ie_pov_status_join, unit = "m") + 
  tm_polygons(
    fill = "pov_rate", 
    fill.scale = tm_scale_intervals(
      style = "jenks", 
      values = "brewer.reds"
    ),
    fill.legend = tm_legend(title = "Rate Below Poverty Line"),
    col_alpha = 0.0
  ) + 
  tm_layout(
    main.title = "Poverty Rates in Inland Empire\nACS5, 2020", 
    main.title.size = 0.95,
    main.title.position = "center",
    frame = FALSE, 
    legend.outside = TRUE, 
    legend.outside.position = "right", 
    legend.position = tm_pos_out("right", "center"),
    text.fontfamily = "Verdana", 
    inner.margins = c(0, 0, -0.05, 0)
  )

poverty_map

tmap_save(
  tm = poverty_map,
  filename = file.path(wd, "ie_poverty_map.png"),
  height = 5.5,
  width = 8,
  dpi = 300,
  device = ragg::agg_png
)

# checking if the image saved
file.exists("ie_poverty_map.png")


#----------------------------------------------
# POPULATION PYRAMID, total pop by age without insurance 
# Health Insurance Coverage by Age: 

ie_female_no_insurance <- get_acs(
  geography = "tract", 
  state = "CA", 
  county = c("Riverside", 
             "San Bernardino"),
  variables = c(
    agesub6 = "B27001_033",
    age6_18 = "B27001_036", 
    age19_25 = "B27001_039", 
    age26_34 = "B27001_042", 
    age35_44 = "B27001_045", 
    age45_54 = "B27001_048", 
    age55_64 = "B27001_051",
    age65_74 = "B27001_054", 
    age75_up = "B27001_057"),
  year = 2020, 
  geometry = TRUE
)

ie_male_no_insurance <- get_acs(
  geography = "tract",
  state = "CA",
  county = c("Riverside", "San Bernardino"),
  variables = c(
    agesub6    = "B27001_005",
    age6_18    = "B27001_008",
    age19_25   = "B27001_011",
    age26_34   = "B27001_014",
    age35_44   = "B27001_017",
    age45_54   = "B27001_020",
    age55_64   = "B27001_023",
    age65_74   = "B27001_026",
    age75_up   = "B27001_029"),
  year = 2020,
  geometry = TRUE
)

# joining both with GEOID and variable name 
ie_both_no_insurance <- ie_female_no_insurance %>%
  left_join(
    st_drop_geometry(ie_male_no_insurance) %>% select(GEOID, variable, estimate, moe),
    by = c("GEOID", "variable"), 
    relationship = "many-to-many",
    suffix =c("_female", "_male")
  ) %>%
# getting total estimate by adding female + male from each age group 
  mutate(estimate_total = estimate_female + estimate_male) 

# now joining so I also have category/classes variable associated 
ie_both_no_insurance <- ie_both_no_insurance %>%
  left_join(
    st_drop_geometry(ie_classes) %>% select(GEOID, category, pop_density),
    by = "GEOID", 
  ) 

# grouping by category and variable 
ie_insurance_by_cat_age <- ie_both_no_insurance %>% 
  group_by(category, variable) %>%
  summarize(sum = sum(estimate_total, na.rm = TRUE)) 


# changing variable names & making sure they are in order for pyramid 
ie_insurance_by_cat_age <- ie_insurance_by_cat_age %>%
  mutate(variable = case_when(
  variable == "agesub6" ~ "Under 6", 
  variable == "age6_18" ~ "6 to 18", 
  variable == "age19_25" ~ "19 to 25", 
  variable == "age26_34" ~ "26 to 34", 
  variable == "age35_44" ~ "35 to 44", 
  variable == "age45_54" ~ "45 to 54", 
  variable == "age55_64" ~ "55 to 64", 
  variable == "age65_74" ~ "65 to 74", 
  variable == "age75_up" ~ "75 and up" 
)) %>%
  mutate(variable = fct_relevel(variable, 
                                "Under 6", 
                                "6 to 18", 
                                "19 to 25", 
                                "26 to 34",
                                "35 to 44", 
                                "45 to 54", 
                                "55 to 64", 
                                "65 to 74", 
                                "75 and up"
  ))

# now filtering to just compare urban and suburban by making 2 new objects 
# have to made one side negative to work with a population pyramid
ie_urban_insurance <- ie_insurance_by_cat_age %>%
  filter(category %in% c("Urban Low", "Urban High")) %>%
  mutate(sum = if_else(category == "Urban Low", -sum, sum)) 


ie_suburban_insurance <- ie_insurance_by_cat_age %>%
  filter(category %in% c("Suburban Low", "Suburban High")) %>% 
  mutate(sum = if_else(category == "Suburban Low", -sum, sum)) 
  
  
max(ie_suburban_insurance$sum)

# urban insurance pyramid 
urban_pyramid <- ggplot(ie_urban_insurance, 
                       aes(x = sum, 
                           y = variable, 
                           fill = category)) + 
  geom_col(width = 0.95, alpha = 0.75) + 
  theme_minimal(base_family = "Verdana", 
                base_size = 12) + 
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5)
  ) +
  scale_x_continuous(
    labels = ~ number_format(scale = .001, suffix = "k")(abs(.x)), 
    limits = 45000 * c(-1,1)
  ) + 
  scale_y_discrete()+ 
  scale_fill_manual(values = c("darkblue", "darkgreen")) + 
  labs(x = "Individuals without Insurance",
       y = "Age Groups from ACS Estimate", 
       title = "Individuals Without Insurance in Urban Settings\nin the Inland Empire, CA", 
       fill = "", 
       caption = "Sources: US Census Bureau PEP, tidycensus R package")

urban_pyramid

# now suburban
suburban_pyramid<- ggplot(ie_suburban_insurance, 
                       aes(x = sum, 
                           y = variable, 
                           fill = category)) + 
  geom_col(width = 0.95, alpha = 0.75) + 
  theme_minimal(base_family = "Verdana", 
                base_size = 12) + 
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5)
  ) +
  scale_x_continuous(
    labels = ~ number_format(scale = .001, suffix = "k")(abs(.x)), 
    limits = 8000 * c(-1,1)
  ) + 
  scale_y_discrete()+ 
  scale_fill_manual(values = c("blue", "forestgreen")) + 
  labs(x = "Individuals without Insurance",
       y = "Age Groups from ACS Estimate", 
       title = "Individuals Without Insurance in Suburban Settings\nin the Inland Empire, CA", 
       fill = "", 
       caption = "Sources: US Census Bureau PEP, tidycensus R package")

suburban_pyramid

ggsave("urban_pyramid.png", 
       plot = urban_pyramid, 
       width = 6,
       height = 6)
ggsave("suburban_pyramid.png",
       plot = suburban_pyramid, 
       width = 6,
       height = 6)


