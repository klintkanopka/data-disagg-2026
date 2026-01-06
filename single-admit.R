# pooled-admit.R

single_admit <- admissions |>
  select(hash_id, starts_with('ETH_'), admit, term_cd) |>
  mutate(term_cd = as.numeric(str_sub(term_cd, 1, 2))) |>
  mutate(ETH_AA_multiracial = if_else(ETH_AA == 1 & ETH_RACECOUNT > 1, 1, 0),
         ETH_NHPI_multiracial = if_else(ETH_NHPI == 1 & ETH_RACECOUNT > 1, 1, 0)) |>
  mutate(ethnic_category = case_when(ETH_AA_NHPI_multiracial == 1 ~ 'AA/NHPI Multiracial',
                                     ETH_AA_multiracial == 1 ~ "AA Multiracial",
                                     ETH_NHPI_multiracial == 1 ~ 'NHPI Multiracial',
                                     ETH_AA_multiethnic == 1 ~ 'AA Multiethnic',
                                     ETH_NHPI_multiethnic == 1 ~'NHPI Multiethnic',
                                     ETH_single == 1 ~ 'Single Ethnic')) |>
  select(hash_id, admit, term_cd, all_of(aa), all_of(nhpi), ethnic_category) |>
  pivot_longer(starts_with('ETH_'), names_to='category', values_to='value') |>
  na.omit() |>
  filter(ethnic_category != 'Single Ethnic') |>
  select(hash_id, ethnic_category, term_cd, admit) |>
  distinct() |>
  group_by(ethnic_category, term_cd) |>
  summarize(admit_rate = mean(admit),
            n_admitted = sum(admit),
            n_applicants = n(),
            .groups='drop') |>
  select(category=ethnic_category, term_cd, admit_rate, n_admitted, n_applicants)


single_eth_admit <- admissions |>
  select(hash_id, term_cd, admit, starts_with('ETH_')) |>
  mutate(term_cd = as.numeric(str_sub(term_cd, 1, 2))) |>
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
  na.omit() |>
  filter(ethnic_category == 'Single Ethnic') |>
  filter(value==1) |>
  group_by(category, term_cd) |>
  summarize(admit_rate = mean(admit),
            n_admitted = sum(admit),
            n_applicants = n(),
            .groups='drop') |>
  select(category, term_cd, admit_rate, n_admitted, n_applicants)


single_admit <- bind_rows(single_admit, single_eth_admit) |>
  left_join(codebook, by='category') |>
  mutate(cat_name = if_else(is.na(cat_name), category, cat_name))

single_admit_wide <- single_admit |>
  pivot_wider(id_cols=c(category, cat_name),
              names_from=term_cd,
              values_from=c(admit_rate, n_admitted, n_applicants),
              names_sep='_')

single_admit_wide[is.na(single_admit_wide)] <- 0

write_csv(single_admit, 'tab/single_admit_long.csv')
write_csv(single_admit_wide, 'tab/single_admit_wide.csv')
