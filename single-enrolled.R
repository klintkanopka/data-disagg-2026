# single-enrolled.R
# similar to dup-enrolled.R, but omits the "single ethnicity" category in favor
# of individual counts by AA/NHPI ethnicity


single_enrolled_18 <- enrollment |>
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
  na.omit() |>
  filter(ethnic_category != 'Single Ethnic') |>
  select(hash_id, ethnic_category, pell, first_gen) |>
  distinct() |>
  group_by(ethnic_category) |>
  summarize(n = n(),
            pell= mean(pell),
            first_gen=mean(first_gen),
            .groups='drop') |>
  select(category=ethnic_category, n, pell, first_gen) |>
  mutate(year=18)

single_eth_18 <- enrollment |>
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
  na.omit() |>
  filter(ethnic_category == 'Single Ethnic') |>
  filter(value==1) |>
  group_by(category) |>
  summarize(n = n(),
            pell= mean(pell),
            first_gen=mean(first_gen),
            .groups='drop') |>
  mutate(year=18)

single_enrolled_18 <- bind_rows(single_enrolled_18, single_eth_18) |>
  left_join(codebook, by='category') |>
  mutate(cat_name = if_else(is.na(cat_name), category, cat_name))

ggplot(single_enrolled_18, aes(x=n, y=reorder(cat_name, n))) +
  geom_col() +
  theme_minimal()

ggsave('fig/single-enrolled-18.png', height=6, width=8)


single_enrolled_19 <- enrollment |>
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
  na.omit() |>
  filter(ethnic_category != 'Single Ethnic') |>
  select(hash_id, ethnic_category, pell, first_gen) |>
  distinct() |>
  group_by(ethnic_category) |>
  summarize(n = n(),
            pell= mean(pell),
            first_gen=mean(first_gen),
            .groups='drop') |>
  select(category=ethnic_category, n, pell, first_gen) |>
  mutate(year=19)

single_eth_19 <- enrollment |>
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
  na.omit() |>
  filter(ethnic_category == 'Single Ethnic') |>
  filter(value==1) |>
  group_by(category) |>
  summarize(n = n(),
            pell= mean(pell),
            first_gen=mean(first_gen),
            .groups='drop') |>
  mutate(year=19)

single_enrolled_19 <- bind_rows(single_enrolled_19, single_eth_19) |>
  left_join(codebook, by='category') |>
  mutate(cat_name = if_else(is.na(cat_name), category, cat_name))

ggplot(single_enrolled_19, aes(x=n, y=reorder(cat_name, n))) +
  geom_col() +
  theme_minimal()

ggsave('fig/single-enrolled-19.png', height=6, width=8)


single_enrolled_20 <- enrollment |>
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
  na.omit() |>
  filter(ethnic_category != 'Single Ethnic') |>
  select(hash_id, ethnic_category, pell, first_gen) |>
  distinct() |>
  group_by(ethnic_category) |>
  summarize(n = n(),
            pell= mean(pell),
            first_gen=mean(first_gen),
            .groups='drop') |>
  select(category=ethnic_category, n, pell, first_gen) |>
  mutate(year=20)

single_eth_20 <- enrollment |>
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
  na.omit() |>
  filter(ethnic_category == 'Single Ethnic') |>
  filter(value==1) |>
  group_by(category) |>
  summarize(n = n(),
            pell= mean(pell),
            first_gen=mean(first_gen),
            .groups='drop') |>
  mutate(year=20)

single_enrolled_20 <- bind_rows(single_enrolled_20, single_eth_20) |>
  left_join(codebook, by='category') |>
  mutate(cat_name = if_else(is.na(cat_name), category, cat_name))

ggplot(single_enrolled_20, aes(x=n, y=reorder(cat_name, n))) +
  geom_col() +
  theme_minimal()

ggsave('fig/single-enrolled-20.png', height=6, width=8)


single_enrolled_21 <- enrollment |>
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
  na.omit() |>
  filter(ethnic_category != 'Single Ethnic') |>
  select(hash_id, ethnic_category, pell, first_gen) |>
  distinct() |>
  group_by(ethnic_category) |>
  summarize(n = n(),
            pell= mean(pell),
            first_gen=mean(first_gen),
            .groups='drop') |>
  select(category=ethnic_category, n, pell, first_gen) |>
  mutate(year=21)

single_eth_21 <- enrollment |>
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
  na.omit() |>
  filter(ethnic_category == 'Single Ethnic') |>
  filter(value==1) |>
  group_by(category) |>
  summarize(n = n(),
            pell= mean(pell),
            first_gen=mean(first_gen),
            .groups='drop') |>
  mutate(year=21)

single_enrolled_21 <- bind_rows(single_enrolled_21, single_eth_21) |>
  left_join(codebook, by='category') |>
  mutate(cat_name = if_else(is.na(cat_name), category, cat_name))

ggplot(single_enrolled_21, aes(x=n, y=reorder(cat_name, n))) +
  geom_col() +
  theme_minimal()

ggsave('fig/single-enrolled-21.png', height=6, width=8)


single_enrolled_22 <- enrollment |>
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
  na.omit() |>
  filter(ethnic_category != 'Single Ethnic') |>
  select(hash_id, ethnic_category, pell, first_gen) |>
  distinct() |>
  group_by(ethnic_category) |>
  summarize(n = n(),
            pell= mean(pell),
            first_gen=mean(first_gen),
            .groups='drop') |>
  select(category=ethnic_category, n, pell, first_gen) |>
  mutate(year=22)

single_eth_22 <- enrollment |>
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
  na.omit() |>
  filter(ethnic_category == 'Single Ethnic') |>
  filter(value==1) |>
  group_by(category) |>
  summarize(n = n(),
            pell= mean(pell),
            first_gen=mean(first_gen),
            .groups='drop') |>
  mutate(year=22)

single_enrolled_22 <- bind_rows(single_enrolled_22, single_eth_22) |>
  left_join(codebook, by='category') |>
  mutate(cat_name = if_else(is.na(cat_name), category, cat_name))

ggplot(single_enrolled_22, aes(x=n, y=reorder(cat_name, n))) +
  geom_col() +
  theme_minimal()

ggsave('fig/single-enrolled-22.png', height=6, width=8)

single_enrolled_long <- bind_rows(single_enrolled_18,
                                  single_enrolled_19,
                                  single_enrolled_20,
                                  single_enrolled_21,
                                  single_enrolled_22)

single_enrolled_wide <- single_enrolled_long |>
  pivot_wider(id_cols=c(category, cat_name),
              names_from=year,
              values_from=c(n, pell, first_gen),
              names_sep='_')

single_enrolled_wide[is.na(single_enrolled_wide)] <- 0

write_csv(single_enrolled_long, 'tab/single_enrolled_long.csv')
write_csv(single_enrolled_wide, 'tab/single_enrolled_wide.csv')

