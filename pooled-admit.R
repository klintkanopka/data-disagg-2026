# pooled-admit.R

pooled_admit <- admissions |>
  select(hash_id, term_cd, starts_with('ETH_'), admit, -ETH_RACECOUNT) |>
  pivot_longer(starts_with('ETH_'), names_to='category', values_to='value') |>
  filter(value != 0) |>
  group_by(category, term_cd) |>
  summarize(admit_rate = mean(admit),
            n_admitted = sum(admit),
            n_applicants = n(),
            .groups='drop') |>
  na.omit() |>
  left_join(codebook, by='category')


pooled_admit_wide <- pooled_admit |>
  pivot_wider(id_cols=c(category, cat_name),
              names_from=term_cd,
              values_from=c(admit_rate, n_admitted, n_applicants),
              names_sep='_')

pooled_admit_wide[is.na(pooled_admit_wide)] <- 0

write_csv(pooled_admit, 'tab/pooled_admit_long.csv')
write_csv(pooled_admit_wide, 'tab/pooled_admit_wide.csv')
