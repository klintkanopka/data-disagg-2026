# admit-rate.R

single_group_admit <- admissions |>
  mutate(term_cd = as.numeric(str_sub(term_cd, 1, 2))) |>
  filter(ETH_single==1) |>
  pivot_longer(starts_with('ETH_CD'), names_to='category', values_to='value') |>
  filter(value==1) |>
  filter(category %in% c(aa,nhpi)) |>
  group_by(category, term_cd) |>
  summarize(admit_rate = mean(admit),
            n_admitted = sum(admit),
            n_applicants = n(),
            .groups='drop') |>
  mutate(type='single')

cat_single_group_admit <- admissions |>
  select(admit, ETH_single, ETH_AA, ETH_NHPI, term_cd) |>
  mutate(term_cd = as.numeric(str_sub(term_cd, 1, 2))) |>
  filter(ETH_single==1) |>
  select(-ETH_single) |>
  pivot_longer(starts_with('ETH_'), names_to='category', values_to='value') |>
  filter(value==1) |>
  group_by(category, term_cd) |>
  summarize(admit_rate = mean(admit),
            n_admitted = sum(admit),
            n_applicants = n(),
            .groups='drop') |>
  mutate(type='single')




cat_dup_group_admit <- admissions |>
  select(ETH_AA, ETH_NHPI, admit, term_cd) |>
  mutate(term_cd = as.numeric(str_sub(term_cd, 1, 2))) |>
  pivot_longer(starts_with('ETH_'), names_to='category', values_to='value') |>
  filter(value==1) |>
  group_by(category, term_cd) |>
  summarize(admit_rate = mean(admit),
            n_admitted = sum(admit),
            n_applicants = n(),
            .groups='drop') |>
  mutate(type='duplicate')


dup_group_admit <- admissions |>
  select(all_of(aa), all_of(nhpi), admit, term_cd) |>
  mutate(term_cd = as.numeric(str_sub(term_cd, 1, 2))) |>
  pivot_longer(starts_with('ETH_CD'), names_to='category', values_to='value') |>
  filter(value==1) |>
  group_by(category, term_cd) |>
  summarize(admit_rate = mean(admit),
            n_admitted = sum(admit),
            n_applicants = n(),
            .groups='drop') |>
  mutate(type='duplicate')


cat_admit <- bind_rows(cat_single_group_admit, cat_dup_group_admit) |>
  left_join(codebook, by='category')
admit <- bind_rows(single_group_admit, dup_group_admit) |>
  left_join(codebook, by='category')

write_csv(cat_admit, 'tab/cat_admit.csv')
write_csv(admit, 'tab/admit_rate.csv')
