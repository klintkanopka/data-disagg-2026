# dup-admit.R


dup_admit <- admissions |>
  select(hash_id, term_cd, starts_with('ETH_'), admit) |>
  mutate(ETH_AA_multiracial = if_else(ETH_AA == 1 & ETH_RACECOUNT > 1, 1, 0),
         ETH_NHPI_multiracial = if_else(ETH_NHPI == 1 & ETH_RACECOUNT > 1, 1, 0)) |>
  mutate(ethnic_category = case_when(ETH_AA_NHPI_multiracial == 1 ~ 'AA/NHPI Multiracial',
                                     ETH_AA_multiracial == 1 ~ "AA Multiracial",
                                     ETH_NHPI_multiracial == 1 ~ 'NHPI Multiracial',
                                     ETH_AA_multiethnic == 1 ~ 'AA Multiethnic',
                                     ETH_NHPI_multiethnic == 1 ~'NHPI Multiethnic',
                                     ETH_single == 1 ~ 'Single Ethnic')) |>
  select(hash_id, term_cd, admit, all_of(aa), all_of(nhpi), ethnic_category) |>
  pivot_longer(starts_with('ETH_'), names_to='category', values_to='value') |>
  filter(value != 0) |>
  group_by(category, ethnic_category, term_cd) |>
  summarize(admit_rate = mean(admit),
            n_admitted = sum(admit),
            n_applicants = n(),
            .groups='drop') |>
  na.omit() |>
  left_join(codebook, by='category')


dup_admit_wide <- dup_admit |>
  pivot_wider(id_cols=c(category, cat_name, ethnic_category),
              names_from=term_cd,
              values_from=c(admit_rate, n_admitted, n_applicants),
              names_sep='_')

dup_admit_wide[is.na(dup_admit_wide)] <- 0

write_csv(dup_admit, 'tab/dup_admit_long.csv')
write_csv(dup_admit_wide, 'tab/dup_admit_wide.csv')
