# pooled-enrolled.R
# Constructs tables of pooled n enrolled, pell, and first gen status by year
# Outputs in both long and wide format

pooled_enrolled_18 <- enrollment |>
  select(hash_id, pell, first_gen, enrolled=enrolled_18f, starts_with('ETH_')) |>
  filter(enrolled==1) |>
  select(hash_id, pell, first_gen, all_of(aa), all_of(nhpi)) |>
  pivot_longer(starts_with('ETH_'), names_to='category', values_to='value') |>
  filter(value != 0) |>
  group_by(category) |>
  summarize(n = sum(value),
            pell= mean(pell),
            first_gen=mean(first_gen),
            .groups='drop') |>
  na.omit() |>
  mutate(year=18)

pooled_enrolled_19 <- enrollment |>
  select(hash_id, pell, first_gen, enrolled=enrolled_19f, starts_with('ETH_')) |>
  filter(enrolled==1) |>
  select(hash_id, pell, first_gen, all_of(aa), all_of(nhpi)) |>
  pivot_longer(starts_with('ETH_'), names_to='category', values_to='value') |>
  filter(value != 0) |>
  group_by(category) |>
  summarize(n = sum(value),
            pell= mean(pell),
            first_gen=mean(first_gen),
            .groups='drop') |>
  na.omit() |>
  mutate(year=19)

pooled_enrolled_20 <- enrollment |>
  select(hash_id, pell, first_gen, enrolled=enrolled_20f, starts_with('ETH_')) |>
  filter(enrolled==1) |>
  select(hash_id, pell, first_gen, all_of(aa), all_of(nhpi)) |>
  pivot_longer(starts_with('ETH_'), names_to='category', values_to='value') |>
  filter(value != 0) |>
  group_by(category) |>
  summarize(n = sum(value),
            pell= mean(pell),
            first_gen=mean(first_gen),
            .groups='drop') |>
  na.omit() |>
  mutate(year=20)

pooled_enrolled_21 <- enrollment |>
  select(hash_id, pell, first_gen, enrolled=enrolled_21f, starts_with('ETH_')) |>
  filter(enrolled==1) |>
  select(hash_id, pell, first_gen, all_of(aa), all_of(nhpi)) |>
  pivot_longer(starts_with('ETH_'), names_to='category', values_to='value') |>
  filter(value != 0) |>
  group_by(category) |>
  summarize(n = sum(value),
            pell= mean(pell),
            first_gen=mean(first_gen),
            .groups='drop') |>
  na.omit() |>
  mutate(year=21)

pooled_enrolled_22 <- enrollment |>
  select(hash_id, pell, first_gen, enrolled=enrolled_22f, starts_with('ETH_')) |>
  filter(enrolled==1) |>
  select(hash_id, pell, first_gen, all_of(aa), all_of(nhpi)) |>
  pivot_longer(starts_with('ETH_'), names_to='category', values_to='value') |>
  filter(value != 0) |>
  group_by(category) |>
  summarize(n = sum(value),
            pell= mean(pell),
            first_gen=mean(first_gen),
            .groups='drop') |>
  na.omit() |>
  mutate(year=22)


pooled_enrolled_long <- bind_rows(pooled_enrolled_18,
                                  pooled_enrolled_19,
                                  pooled_enrolled_20,
                                  pooled_enrolled_21,
                                  pooled_enrolled_22) |>
  left_join(codebook, by='category')

pooled_enrolled_wide <- pooled_enrolled_long |>
  pivot_wider(id_cols=c(category, cat_name),
              names_from=year,
              values_from=c(n, pell, first_gen),
              names_sep='_')

pooled_enrolled_wide[is.na(pooled_enrolled_wide)] <- 0

write_csv(pooled_enrolled_long, 'tab/pooled_enrolled_long.csv')
write_csv(pooled_enrolled_wide, 'tab/pooled_enrolled_wide.csv')

