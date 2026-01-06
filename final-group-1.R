# Group 1

# Figures 5A and 5B, with 6A and 6B are combined together so that it is AA&NHPI.
# Then, we also combine the bar charts, so that instead of 2 separate charts of
# unduplicated and duplicated, it is just one "two-tone" bar that is "Alone and
# in Combination", which is similar to the US Census.

# Can you redo it so that we get to figures - where both are disaggregated. One
# has the alone and in-combination, while the other has the 5 new multiracial
# categories we created.  What I was aiming to say, when "AA&NHPsI are combined
# together" is that the figures have both disaggregated ethnic categories,
# unlike the originals that were made for the report (see figures 5A, 5B, 6A,
# 6B in attached report, where 5A and 5B are AA and 6A and 6B are NHPI).
#
# Exactly the same as 5A/5B
#
#

# New multiethnic

color_1 <- "#2774AE"
color_2 <- "#FFD100"

comb_enrolled_22 <- enrollment |>
  select(
    hash_id,
    pell,
    first_gen,
    enrolled = enrolled_22f,
    starts_with('ETH_')
  ) |>
  filter(enrolled == 1) |>
  mutate(
    white = ETH_WHITE,
    black = ETH_BLACK,
    hispanic = ETH_LATINX,
    native = ETH_ANNA,
    swana = ETH_SWANA,
    asian = ETH_AA,
    nhpi = ETH_NHPI
  ) |>
  filter(asian + nhpi >= 1) |>
  select(hash_id, starts_with('ETH_CD_')) |>
  mutate(
    alone = if_else(rowSums(pick(-hash_id)) == 1, 'Alone', 'In Combination')
  ) |>
  select(hash_id, alone, all_of(aa), all_of(nhpi)) |>
  pivot_longer(
    starts_with('ETH_'),
    names_to = 'category',
    values_to = 'value'
  ) |>
  filter(value != 0) |>
  left_join(codebook, by = 'category') |>
  group_by(cat_name, alone) |>
  summarize(n = sum(value), .groups = 'drop')

write_csv(comb_enrolled_22, 'final-tab/group-1a.csv')

order_1a <- c(
  "Native Hawaiian",
  "Other Pacific Islander",
  "Samoan",
  "Chamorro",
  "Fijian",
  "Tongan",

  "Chinese",
  "Indian",
  "Vietnamese",
  "Filipinx",
  "Korean",
  "Taiwanese",
  "Japanese",
  "Pakistani",
  "Other Asian",
  "Indonesian",
  "Cambodian",
  "Thai",
  "Bangladeshi",
  "Malaysian",
  "Sri Lankan",
  "Laotian",
  "Hmong"
)


ggplot(
  comb_enrolled_22,
  aes(
    x = n,
    y = factor(cat_name, levels = rev(order_1a), ordered = TRUE),
    fill = factor(alone, levels = c("In Combination", "Alone")),
    label = n
  )
) +
  geom_col(position = 'stack') +
  geom_text_repel(direction = 'x', position = 'stack', force = 1.5) +
  scale_fill_manual(
    breaks = c('Alone', 'In Combination'),
    values = c(color_1, color_2)
  ) +
  labs(
    fill = NULL,
    y = NULL,
    x = 'N'
  ) +
  theme_minimal() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.position = "bottom"
  )

ggsave('final-fig/group-1a.png', height = 8, width = 10)


single_enrolled_22 <- enrollment |>
  select(
    hash_id,
    pell,
    first_gen,
    enrolled = enrolled_22f,
    starts_with('ETH_')
  ) |>
  filter(enrolled == 1) |>
  mutate(
    ETH_AA_multiracial = if_else(ETH_AA == 1 & ETH_RACECOUNT > 1, 1, 0),
    ETH_NHPI_multiracial = if_else(ETH_NHPI == 1 & ETH_RACECOUNT > 1, 1, 0)
  ) |>
  mutate(
    ethnic_category = case_when(
      ETH_AA_NHPI_multiracial == 1 ~ 'AA/NHPI Multiracial',
      ETH_AA_multiracial == 1 ~ "AA Multiracial",
      ETH_NHPI_multiracial == 1 ~ 'NHPI Multiracial',
      ETH_AA_multiethnic == 1 ~ 'AA Multiethnic',
      ETH_NHPI_multiethnic == 1 ~ 'NHPI Multiethnic',
      ETH_single == 1 ~ 'Single Ethnic'
    )
  ) |>
  select(hash_id, pell, first_gen, all_of(aa), all_of(nhpi), ethnic_category) |>
  pivot_longer(
    starts_with('ETH_'),
    names_to = 'category',
    values_to = 'value'
  ) |>
  na.omit() |>
  filter(ethnic_category != 'Single Ethnic') |>
  select(hash_id, ethnic_category, pell, first_gen) |>
  distinct() |>
  group_by(ethnic_category) |>
  summarize(
    n = n(),
    pell = mean(pell),
    first_gen = mean(first_gen),
    .groups = 'drop'
  ) |>
  select(category = ethnic_category, n, pell, first_gen) |>
  mutate(year = 22)

single_eth_22 <- enrollment |>
  select(
    hash_id,
    pell,
    first_gen,
    enrolled = enrolled_22f,
    starts_with('ETH_')
  ) |>
  filter(enrolled == 1) |>
  mutate(
    ETH_AA_multiracial = if_else(ETH_AA == 1 & ETH_RACECOUNT > 1, 1, 0),
    ETH_NHPI_multiracial = if_else(ETH_NHPI == 1 & ETH_RACECOUNT > 1, 1, 0)
  ) |>
  mutate(
    ethnic_category = case_when(
      ETH_AA_NHPI_multiracial == 1 ~ 'AA/NHPI Multiracial',
      ETH_AA_multiracial == 1 ~ "AA Multiracial",
      ETH_NHPI_multiracial == 1 ~ 'NHPI Multiracial',
      ETH_AA_multiethnic == 1 ~ 'AA Multiethnic',
      ETH_NHPI_multiethnic == 1 ~ 'NHPI Multiethnic',
      ETH_single == 1 ~ 'Single Ethnic'
    )
  ) |>
  select(hash_id, pell, first_gen, all_of(aa), all_of(nhpi), ethnic_category) |>
  pivot_longer(
    starts_with('ETH_'),
    names_to = 'category',
    values_to = 'value'
  ) |>
  na.omit() |>
  filter(ethnic_category == 'Single Ethnic') |>
  filter(value == 1) |>
  group_by(category) |>
  summarize(
    n = n(),
    pell = mean(pell),
    first_gen = mean(first_gen),
    .groups = 'drop'
  ) |>
  mutate(year = 22)

single_enrolled_22 <- bind_rows(single_enrolled_22, single_eth_22) |>
  left_join(codebook, by = 'category') |>
  mutate(cat_name = if_else(is.na(cat_name), category, cat_name))


order_1b <- c(
  "NHPI Multiracial",
  "Tongan",
  "Chamorro",
  "Native Hawaiian",
  "Fijian",
  "NHPI Multiethnic",
  "Other Pacific Islander",

  "Samoan",

  'AA/NHPI Multiracial',

  "Chinese",
  "AA Multiracial",
  "Indian",
  "AA Multiethnic",
  "Korean",
  "Vietnamese",
  "Filipinx",
  "Taiwanese",
  "Japanese",
  "Pakistani",
  "Other Asian",
  "Bangladeshi",
  "Cambodian",

  "Indonesian",
  "Sri Lankan",
  "Thai",
  "Laotian",
  "Hmong",
  "Malaysian"
)

ggplot(
  single_enrolled_22,
  aes(
    x = n,
    #y = reorder(cat_name, n),
    y = factor(cat_name, levels = rev(order_1b), ordered = TRUE),
    label = n
  )
) +
  geom_col(position = 'stack', fill = color_1) +
  geom_text(hjust = 0) +
  labs(y = NULL, x = 'N') +
  theme_minimal()

write_csv(single_enrolled_22, 'final-tab/group-1b.csv')
ggsave('final-fig/group-1b.png', height = 8, width = 10)
