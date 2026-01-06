# yield-rate.R

single_group_yield <- admissions |>
  select(all_of(aa), all_of(nhpi), admit, reg, ETH_single, term_cd) |>
  mutate(term_cd = as.numeric(str_sub(term_cd, 1, 2))) |>
  filter(ETH_single==1) |>
  select(-ETH_single) |>
  filter(admit==1) |>
  pivot_longer(starts_with('ETH_CD'), names_to='category', values_to='value') |>
  filter(value==1) |>
  group_by(category, term_cd) |>
  summarize(yield_rate = mean(reg),
            n_reg = sum(reg),
            n_admitted = n(),
            .groups='drop') |>
  mutate(type='single')

cat_single_group_yield <- admissions |>
  select(ETH_AA, ETH_NHPI, admit, reg, ETH_single, term_cd) |>
  mutate(term_cd = as.numeric(str_sub(term_cd, 1, 2))) |>
  filter(ETH_single==1) |>
  select(-ETH_single) |>
  filter(admit==1) |>
  pivot_longer(starts_with('ETH_'), names_to='category', values_to='value') |>
  filter(value==1) |>
  group_by(category, term_cd) |>
  summarize(yield_rate = mean(reg),
            n_reg = sum(reg),
            n_admitted = n(),
            .groups='drop') |>
  mutate(type='single')

dup_group_yield <- admissions |>
  select(all_of(aa), all_of(nhpi), admit, reg, term_cd) |>
  mutate(term_cd = as.numeric(str_sub(term_cd, 1, 2))) |>
  filter(admit==1) |>
  pivot_longer(starts_with('ETH_CD'), names_to='category', values_to='value') |>
  filter(value==1) |>
  group_by(category, term_cd) |>
  summarize(yield_rate = mean(reg),
            n_reg = sum(reg),
            n_admitted = n(),
            .groups='drop') |>
  mutate(type='duplicate')

cat_dup_group_yield <- admissions |>
  select(ETH_AA, ETH_NHPI, admit, reg, term_cd) |>
  mutate(term_cd = as.numeric(str_sub(term_cd, 1, 2))) |>
  filter(admit==1) |>
  pivot_longer(starts_with('ETH_'), names_to='category', values_to='value') |>
  filter(value==1) |>
  group_by(category, term_cd) |>
  summarize(yield_rate = mean(reg),
            n_reg = sum(reg),
            n_admitted = n(),
            .groups='drop') |>
  mutate(type='duplicate')


cat_yield <- bind_rows(cat_single_group_yield, cat_dup_group_yield) |>
  left_join(codebook, by='category')
yield <- bind_rows(single_group_yield, dup_group_yield) |>
  left_join(codebook, by='category')

write_csv(cat_yield, 'tab/cat_yield.csv')
write_csv(yield, 'tab/yield_rate.csv')
