# pooled-yield.R

pooled_yield <- admissions |>
  select(hash_id, starts_with('ETH_'), admit, reg, term_cd) |>
  filter(admit==1) |>
  mutate(term_cd = as.numeric(str_sub(term_cd, 1, 2))) |>
  select(hash_id, term_cd, reg, all_of(aa), all_of(nhpi)) |>
  pivot_longer(starts_with('ETH_'), names_to='category', values_to='value') |>
  filter(value != 0) |>
  group_by(category, term_cd) |>
  summarize(yield_rate = mean(reg),
            n_reg = sum(reg),
            n_admitted = n(),
            .groups='drop') |>
  na.omit() |>
  left_join(codebook, by='category')


pooled_yield_wide <- pooled_yield |>
  pivot_wider(id_cols=c(category, cat_name),
              names_from=term_cd,
              values_from=c(yield_rate, n_reg, n_admitted),
              names_sep='_')

pooled_yield_wide[is.na(pooled_yield_wide)] <- 0

write_csv(pooled_yield, 'tab/pooled_yield_long.csv')
write_csv(pooled_yield_wide, 'tab/pooled_yield_wide.csv')
