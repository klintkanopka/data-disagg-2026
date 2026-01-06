# dup-enrolled.R
# Construct multiracial/multiethnic duplicate counts for each AA/NHPI ethnicity
# Ouput: figures and tables

dup_enrolled_18 <- enrollment |>
  select(hash_id, pell, first_gen, enrolled=enrolled_18f, starts_with('ETH_')) |>
  filter(enrolled==1) |>
  mutate(ETH_AA_multiracial = if_else(ETH_AA == 1 & ETH_RACECOUNT > 1, 1, 0),
         ETH_NHPI_multiracial = if_else(ETH_NHPI == 1 & ETH_RACECOUNT > 1, 1, 0)) |>
  mutate(ethnic_category = case_when(ETH_AA_NHPI_multiracial == 1 ~ 'AA/NHPI Multiracial',
                                     ETH_AA_multiracial == 1 ~ "AA Multiracial",
                                     ETH_NHPI_multiracial == 1 ~ 'NHPI Multiracial',
                                     ETH_AA_multiethnic == 1 ~ 'AA Multiethnic',
                                     ETH_NHPI_multiethnic == 1 ~'NHPI Multiethnic',
                                     ETH_single == 1 ~ 'Single Ethnic')) |>
  select(hash_id, pell, first_gen, all_of(aa), all_of(nhpi), ethnic_category) |>
  pivot_longer(starts_with('ETH_'), names_to='category', values_to='value') |>
  filter(value != 0) |>
  group_by(category, ethnic_category) |>
  summarize(n = sum(value),
            pell= mean(pell),
            first_gen=mean(first_gen),
            .groups='drop') |>
  na.omit() |>
  mutate(year=18) |>
  left_join(codebook, by='category')



ggplot(dup_enrolled_18, aes(x=n, y=reorder(cat_name, n), fill=ethnic_category)) +
  geom_col(position='fill') +
  scale_fill_viridis_d() +
  theme_minimal()

ggsave('fig/dup-enrolled-18.png', height=6, width=8)


dup_enrolled_19 <- enrollment |>
  select(hash_id, pell, first_gen, enrolled=enrolled_19f, starts_with('ETH_')) |>
  filter(enrolled==1) |>
  mutate(ETH_AA_multiracial = if_else(ETH_AA == 1 & ETH_RACECOUNT > 1, 1, 0),
         ETH_NHPI_multiracial = if_else(ETH_NHPI == 1 & ETH_RACECOUNT > 1, 1, 0)) |>
  mutate(ethnic_category = case_when(ETH_AA_NHPI_multiracial == 1 ~ 'AA/NHPI Multiracial',
                                     ETH_AA_multiracial == 1 ~ "AA Multiracial",
                                     ETH_NHPI_multiracial == 1 ~ 'NHPI Multiracial',
                                     ETH_AA_multiethnic == 1 ~ 'AA Multiethnic',
                                     ETH_NHPI_multiethnic == 1 ~'NHPI Multiethnic',
                                     ETH_single == 1 ~ 'Single Ethnic')) |>
  select(hash_id, pell, first_gen, all_of(aa), all_of(nhpi), ethnic_category) |>
  pivot_longer(starts_with('ETH_'), names_to='category', values_to='value') |>
  filter(value != 0) |>
  group_by(category, ethnic_category) |>
  summarize(n = sum(value),
            pell= mean(pell),
            first_gen=mean(first_gen),
            .groups='drop') |>
  na.omit() |>
  mutate(year=19) |>
  left_join(codebook, by='category')


ggplot(dup_enrolled_19, aes(x=n, y=reorder(cat_name, n), fill=ethnic_category)) +
  geom_col(position='fill') +
  scale_fill_viridis_d() +
  theme_minimal()

ggsave('fig/dup-enrolled-19.png', height=6, width=8)


dup_enrolled_20 <- enrollment |>
  select(hash_id, pell, first_gen, enrolled=enrolled_20f, starts_with('ETH_')) |>
  filter(enrolled==1) |>
  mutate(ETH_AA_multiracial = if_else(ETH_AA == 1 & ETH_RACECOUNT > 1, 1, 0),
         ETH_NHPI_multiracial = if_else(ETH_NHPI == 1 & ETH_RACECOUNT > 1, 1, 0)) |>
  mutate(ethnic_category = case_when(ETH_AA_NHPI_multiracial == 1 ~ 'AA/NHPI Multiracial',
                                     ETH_AA_multiracial == 1 ~ "AA Multiracial",
                                     ETH_NHPI_multiracial == 1 ~ 'NHPI Multiracial',
                                     ETH_AA_multiethnic == 1 ~ 'AA Multiethnic',
                                     ETH_NHPI_multiethnic == 1 ~'NHPI Multiethnic',
                                     ETH_single == 1 ~ 'Single Ethnic')) |>
  select(hash_id, pell, first_gen, all_of(aa), all_of(nhpi), ethnic_category) |>
  pivot_longer(starts_with('ETH_'), names_to='category', values_to='value') |>
  filter(value != 0) |>
  group_by(category, ethnic_category) |>
  summarize(n = sum(value),
            pell= mean(pell),
            first_gen=mean(first_gen),
            .groups='drop') |>
  na.omit() |>
  mutate(year=20) |>
  left_join(codebook, by='category')


ggplot(dup_enrolled_20, aes(x=n, y=reorder(cat_name, n), fill=ethnic_category)) +
  geom_col(position='fill') +
  scale_fill_viridis_d() +
  theme_minimal()

ggsave('fig/dup-enrolled-20.png', height=6, width=8)


dup_enrolled_21 <- enrollment |>
  select(hash_id, pell, first_gen, enrolled=enrolled_21f, starts_with('ETH_')) |>
  filter(enrolled==1) |>
  mutate(ETH_AA_multiracial = if_else(ETH_AA == 1 & ETH_RACECOUNT > 1, 1, 0),
         ETH_NHPI_multiracial = if_else(ETH_NHPI == 1 & ETH_RACECOUNT > 1, 1, 0)) |>
  mutate(ethnic_category = case_when(ETH_AA_NHPI_multiracial == 1 ~ 'AA/NHPI Multiracial',
                                     ETH_AA_multiracial == 1 ~ "AA Multiracial",
                                     ETH_NHPI_multiracial == 1 ~ 'NHPI Multiracial',
                                     ETH_AA_multiethnic == 1 ~ 'AA Multiethnic',
                                     ETH_NHPI_multiethnic == 1 ~'NHPI Multiethnic',
                                     ETH_single == 1 ~ 'Single Ethnic')) |>
  select(hash_id, pell, first_gen, all_of(aa), all_of(nhpi), ethnic_category) |>
  pivot_longer(starts_with('ETH_'), names_to='category', values_to='value') |>
  filter(value != 0) |>
  group_by(category, ethnic_category) |>
  summarize(n = sum(value),
            pell= mean(pell),
            first_gen=mean(first_gen),
            .groups='drop') |>
  na.omit() |>
  mutate(year=21) |>
  left_join(codebook, by='category')


ggplot(dup_enrolled_21, aes(x=n, y=reorder(cat_name, n), fill=ethnic_category)) +
  geom_col(position='fill') +
  scale_fill_viridis_d() +
  theme_minimal()

ggsave('fig/dup-enrolled-21.png', height=6, width=8)


dup_enrolled_22 <- enrollment |>
  select(hash_id, pell, first_gen, enrolled=enrolled_22f, starts_with('ETH_')) |>
  filter(enrolled==1) |>
  mutate(ETH_AA_multiracial = if_else(ETH_AA == 1 & ETH_RACECOUNT > 1, 1, 0),
         ETH_NHPI_multiracial = if_else(ETH_NHPI == 1 & ETH_RACECOUNT > 1, 1, 0)) |>
  mutate(ethnic_category = case_when(ETH_AA_NHPI_multiracial == 1 ~ 'AA/NHPI Multiracial',
                                     ETH_AA_multiracial == 1 ~ "AA Multiracial",
                                     ETH_NHPI_multiracial == 1 ~ 'NHPI Multiracial',
                                     ETH_AA_multiethnic == 1 ~ 'AA Multiethnic',
                                     ETH_NHPI_multiethnic == 1 ~'NHPI Multiethnic',
                                     ETH_single == 1 ~ 'Single Ethnic')) |>
  select(hash_id, pell, first_gen, all_of(aa), all_of(nhpi), ethnic_category) |>
  pivot_longer(starts_with('ETH_'), names_to='category', values_to='value') |>
  filter(value != 0) |>
  group_by(category, ethnic_category) |>
  summarize(n = sum(value),
            pell= mean(pell),
            first_gen=mean(first_gen),
            .groups='drop') |>
  na.omit() |>
  mutate(year=22) |>
  left_join(codebook, by='category')


ggplot(dup_enrolled_22, aes(x=n, y=reorder(cat_name, n), fill=ethnic_category)) +
  geom_col(position='fill') +
  scale_fill_viridis_d() +
  theme_minimal()

ggsave('fig/dup-enrolled-22.png', height=6, width=8)


dup_enrolled_long <- bind_rows(dup_enrolled_18, dup_enrolled_19, dup_enrolled_20, dup_enrolled_21, dup_enrolled_22)

dup_enrolled_wide <- dup_enrolled_long |>
  pivot_wider(id_cols=c(category, cat_name, ethnic_category),
              names_from=year,
              values_from=c(n, pell, first_gen),
              names_sep='_')

dup_enrolled_wide[is.na(dup_enrolled_wide)] <- 0

write_csv(dup_enrolled_long, 'tab/dup_enrolled_long.csv')
write_csv(dup_enrolled_wide, 'tab/dup_enrolled_wide.csv')
