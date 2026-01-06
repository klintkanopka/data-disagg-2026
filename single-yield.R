# single-yield.R

single_yield <- admissions |>
  select(hash_id, starts_with('ETH_'), admit, reg, term_cd) |>
  filter(admit==1) |>
  mutate(term_cd = as.numeric(str_sub(term_cd, 1, 2))) |>
  mutate(ETH_AA_multiracial = if_else(ETH_AA == 1 & ETH_RACECOUNT > 1, 1, 0),
         ETH_NHPI_multiracial = if_else(ETH_NHPI == 1 & ETH_RACECOUNT > 1, 1, 0)) |>
  mutate(ethnic_category = case_when(ETH_AA_NHPI_multiracial == 1 ~ 'AA/NHPI Multiracial',
                                     ETH_AA_multiracial == 1 ~ "AA Multiracial",
                                     ETH_NHPI_multiracial == 1 ~ 'NHPI Multiracial',
                                     ETH_AA_multiethnic == 1 ~ 'AA Multiethnic',
                                     ETH_NHPI_multiethnic == 1 ~'NHPI Multiethnic',
                                     ETH_single == 1 ~ 'Single Ethnic')) |>
  select(hash_id, admit, reg, term_cd, all_of(aa), all_of(nhpi), ethnic_category) |>
  pivot_longer(starts_with('ETH_'), names_to='category', values_to='value') |>
  na.omit() |>
  filter(ethnic_category != 'Single Ethnic') |>
  select(hash_id, ethnic_category, term_cd, admit, reg) |>
  distinct() |>
  group_by(ethnic_category, term_cd) |>
  summarize(yield_rate = mean(reg),
            n_reg = sum(reg),
            n_admitted = n(),
            .groups='drop') |>
  select(category=ethnic_category, term_cd, yield_rate, n_reg, n_admitted)


single_eth_yield <- admissions |>
  select(hash_id, term_cd, admit, reg, starts_with('ETH_')) |>
  filter(admit==1) |>
  mutate(term_cd = as.numeric(str_sub(term_cd, 1, 2))) |>
  mutate(ETH_AA_multiracial = if_else(ETH_AA == 1 & ETH_RACECOUNT > 1, 1, 0),
         ETH_NHPI_multiracial = if_else(ETH_NHPI == 1 & ETH_RACECOUNT > 1, 1, 0)) |>
  mutate(ethnic_category = case_when(ETH_AA_NHPI_multiracial == 1 ~ 'AA/NHPI Multiracial',
                                     ETH_AA_multiracial == 1 ~ "AA Multiracial",
                                     ETH_NHPI_multiracial == 1 ~ 'NHPI Multiracial',
                                     ETH_AA_multiethnic == 1 ~ 'AA Multiethnic',
                                     ETH_NHPI_multiethnic == 1 ~'NHPI Multiethnic',
                                     ETH_single == 1 ~ 'Single Ethnic')) |>
  select(hash_id, term_cd, admit, reg, all_of(aa), all_of(nhpi), ethnic_category) |>
  pivot_longer(starts_with('ETH_'), names_to='category', values_to='value') |>
  na.omit() |>
  filter(ethnic_category == 'Single Ethnic') |>
  filter(value==1) |>
  group_by(category, term_cd) |>
  summarize(yield_rate = mean(reg),
            n_reg = sum(reg),
            n_admitted = n(),
            .groups='drop') |>
  select(category, term_cd, yield_rate, n_reg, n_admitted)


single_yield <- bind_rows(single_yield, single_eth_yield) |>
  left_join(codebook, by='category') |>
  mutate(cat_name = if_else(is.na(cat_name), category, cat_name))

single_yield_wide <- single_yield |>
  pivot_wider(id_cols=c(category, cat_name),
              names_from=term_cd,
              values_from=c(yield_rate, n_reg, n_admitted),
              names_sep='_')

single_yield_wide[is.na(single_yield_wide)] <- 0

write_csv(single_yield, 'tab/single_yield_long.csv')
write_csv(single_yield_wide, 'tab/single_yield_wide.csv')
