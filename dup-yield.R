# dup-yield.R


dup_yield <- admissions |>
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
  select(hash_id, term_cd, reg, all_of(aa), all_of(nhpi), ethnic_category) |>
  pivot_longer(starts_with('ETH_'), names_to='category', values_to='value') |>
  filter(value != 0) |>
  group_by(category, ethnic_category, term_cd) |>
  summarize(yield_rate = mean(reg),
            n_reg = sum(reg),
            n_admitted = n(),
            .groups='drop') |>
  na.omit() |>
  left_join(codebook, by='category')


dup_yield_wide <- dup_yield |>
  pivot_wider(id_cols=c(category, cat_name, ethnic_category),
              names_from=term_cd,
              values_from=c(yield_rate, n_reg, n_admitted),
              names_sep='_')

dup_yield_wide[is.na(dup_yield_wide)] <- 0

write_csv(dup_yield, 'tab/dup_yield_long.csv')
write_csv(dup_yield_wide, 'tab/dup_yield_wide.csv')
