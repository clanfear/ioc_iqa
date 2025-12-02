library(tidyverse)
library(broom) 

load(url("https://github.com/clanfear/built_environment_ce/raw/main/data/analytical_data/ccahs_block_analytical_unstd.RData")) #<<

glimpse(ccahs_block_analytical_unstd)

options(width = 90) # Making display a bit wider temporarily

ccahs_block_analytical_unstd |> select(where(is.numeric)) |>
  pivot_longer(everything()) |> group_by(name) |>
  summarize(across(value, list(mean = mean, sd = sd, min = min, max = max), .names = "{.fn}")) |>
  mutate(across(-name, ~round(.,2))) |> print(n=26)

options(width = 68) # Returning display width to default

neighb_model <- 
  lm(CRIME_violent_2004_2006 ~ CE_hlm_2001 + CE_hlm_1995 + 
       FAC_disadv_2000 + FAC_stability_2000 + FAC_hispimm_2000, 
     data = ccahs_block_analytical_unstd)



summary(neighb_model)

neighb_model |> tidy()

full_model <- 
  lm(CRIME_violent_2004_2006 ~ CE_hlm_2001 + CE_hlm_1995 + 
       FAC_disadv_2000 + FAC_stability_2000 + FAC_hispimm_2000 + density_ltdb_nc_2000 + 
       BE_pr_vacant_onstreet_block_2001 + BE_pr_abandoned_bld_onstreet_block_2001 + #<<
       BE_pr_commer_dest_onstreet_block_2001 + BE_pr_recreation_block_2001 + #<<
       BE_pr_parking_block_2001  + BE_pr_commercial_block_2001 +  #<<
       BE_pr_bar_onstreet_block_2001 + BE_pr_liquor_onstreet_block_2001 + #<< 
       density_block + I(density_block^2) + street_class_near, #<<
     data = ccahs_block_analytical_unstd)



tidy(full_model) |> mutate(across(-term, ~round(., 3)))

anova(neighb_model, full_model) |> tidy()

tidy(full_model) |> mutate(across(-term, ~round(., 3))) |> 
  filter(str_detect(term, "CE"))

full_noquad_model <- 
  lm(CRIME_violent_2004_2006 ~ CE_hlm_2001 + CE_hlm_1995 + 
       FAC_disadv_2000 + FAC_stability_2000 + FAC_hispimm_2000 + density_ltdb_nc_2000 + 
       density_block + street_class_near + 
       BE_pr_vacant_onstreet_block_2001 + BE_pr_abandoned_bld_onstreet_block_2001 +
       BE_pr_commer_dest_onstreet_block_2001 + BE_pr_recreation_block_2001 +
       BE_pr_parking_block_2001  + BE_pr_commercial_block_2001 + 
       BE_pr_bar_onstreet_block_2001 + BE_pr_liquor_onstreet_block_2001, 
     data = ccahs_block_analytical_unstd)
anova(full_noquad_model, full_model) |> tidy() |> mutate(p.value = round(p.value, 3))

est_vec <- full_model |> tidy() |> 
  filter(str_detect(term, "density_block")) |> 
  mutate(p.value = round(p.value, 3)) |>
  pull(estimate) |>
  round(2)

full_model |> tidy() |> 
  filter(str_detect(term, "density_block")) |> 
  mutate(p.value = round(p.value, 3))

ccahs_block_analytical_unstd |> pull(density_block) |> round(3) %>% 
  {setNames(c(min(.), median(.), max(.)), c("min", "mean", "max"))}

full_int_model <- 
  lm(CRIME_violent_2004_2006 ~ CE_hlm_2001 + CE_hlm_1995 + 
       FAC_disadv_2000 + FAC_stability_2000 + FAC_hispimm_2000 + density_ltdb_nc_2000 + 
       density_block + density_block_2 + street_class_near + 
       FAC_disadv_2000 * BE_pr_vacant_onstreet_block_2001 + #<<
       FAC_disadv_2000 * BE_pr_abandoned_bld_onstreet_block_2001 + #<<
       FAC_disadv_2000 * BE_pr_commer_dest_onstreet_block_2001 + #<<
       FAC_disadv_2000 * BE_pr_recreation_block_2001 + #<<
       FAC_disadv_2000 * BE_pr_parking_block_2001  + #<<
       FAC_disadv_2000 * BE_pr_commercial_block_2001 + #<<
       FAC_disadv_2000 * BE_pr_bar_onstreet_block_2001 + #<<
       FAC_disadv_2000 * BE_pr_liquor_onstreet_block_2001, #<<
     data = ccahs_block_analytical_unstd)
anova(full_int_model, full_model) |> tidy()

full_int_model |> tidy() |> print(n=26)

ab_coefs <- full_int_model |> tidy() |> filter(str_detect(term, "abandoned")) |> pull(estimate) |> round(2)

full_int_model |> tidy() |> filter(str_detect(term, "abandoned"))

ab_coefs <- full_int_model |> tidy() |> 
  filter(str_detect(term, "disadv")) |> 
  pull(estimate) |> round(2)

full_int_model |> 
  tidy() |> 
  filter(str_detect(term, "disadv")) |>
  select(term, estimate) |>
  print(width=40)

dis_coefs <- full_int_model |> 
  tidy() |> 
  filter(str_detect(term, "disadv")) %>%
  {setNames(.$estimate, str_remove_all(str_extract(.$term, "pr_.*_block"), "(pr_)|(_onstreet)|(_block)|(_dest)|(_bld)"))} |> round(2)
dis_names <- names(dis_coefs)

full_model |>
  augment() |>
  ggplot(aes(x = .fitted, 
             y = .resid)) + 
  geom_point() + 
  geom_smooth()
