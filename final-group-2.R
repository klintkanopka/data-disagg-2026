color_1 <- "#2774AE"
color_2 <- "#FFD100"


aa_enrollment_22 <- enrollment |>
  select(
    hash_id,
    reducedethnic = reducedethnic_22f,
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
  mutate(
    three_plus = if_else(
      white + black + hispanic + native + swana + asian + nhpi >= 3,
      1,
      0
    )
  ) |>
  filter(asian == 1) |>
  mutate(
    asian = if_else(
      white + black + hispanic + native + swana + asian + nhpi == 1,
      1,
      0
    ),
    white = if_else(three_plus == 1, 0, white),
    black = if_else(three_plus == 1, 0, black),
    hispanic = if_else(three_plus == 1, 0, hispanic),
    native = if_else(three_plus == 1, 0, native),
    swana = if_else(three_plus == 1, 0, swana),
    nhpi = if_else(three_plus == 1, 0, nhpi)
  ) |>
  select(white, black, asian, hispanic, native, swana, nhpi, three_plus) |>
  summarize(
    white = sum(white),
    black = sum(black),
    hispanic = sum(hispanic),
    native = sum(native),
    swana = sum(swana),
    asian = sum(asian),
    nhpi = sum(nhpi),
    three_plus = sum(three_plus)
  ) |>
  mutate(label = 'Asian American')

nhpi_enrollment_22 <- enrollment |>
  select(hash_id, enrolled = enrolled_22f, starts_with('ETH_')) |>
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
  mutate(
    three_plus = if_else(
      white + black + hispanic + native + swana + asian + nhpi >= 3,
      1,
      0
    )
  ) |>
  filter(nhpi == 1) |>
  mutate(
    nhpi = if_else(
      white + black + hispanic + native + swana + asian + nhpi == 1,
      1,
      0
    ),
    white = if_else(three_plus == 1, 0, white),
    black = if_else(three_plus == 1, 0, black),
    hispanic = if_else(three_plus == 1, 0, hispanic),
    native = if_else(three_plus == 1, 0, native),
    swana = if_else(three_plus == 1, 0, swana),
    asian = if_else(three_plus == 1, 0, asian)
  ) |>
  select(white, black, hispanic, native, swana, asian, nhpi, three_plus) |>
  summarize(
    white = sum(white),
    black = sum(black),
    hispanic = sum(hispanic),
    native = sum(native),
    swana = sum(swana),
    asian = sum(asian),
    nhpi = sum(nhpi),
    three_plus = sum(three_plus)
  ) |>
  mutate(label = 'Native Hawaiian and Pacific Islander')


comb_enrollment <- bind_rows(
  aa_enrollment_22,
  nhpi_enrollment_22
)

write_csv(comb_enrollment, 'final-tab/group-2.csv')

comb_enrollment |>
  pivot_longer(-label, names_to = 'group', values_to = 'n') |>
  mutate(
    group = factor(
      case_when(
        group == 'asian' ~ 'Asian American',
        group == 'black' ~ 'Black',
        group == 'hispanic' ~ 'Latine',
        group == 'native' ~ 'Native American',
        group == 'nhpi' ~ 'Native Hawaiian and Pacific Islander',
        group == 'swana' ~ 'SWANA',
        group == 'white' ~ 'White',
        group == 'three_plus' ~ 'Three or More',
        TRUE ~ group
      ),
      ordered = TRUE,
      levels = c(
        'Asian American',
        'Black',
        'Latine',
        'Native American',
        'Native Hawaiian and Pacific Islander',
        'SWANA',
        'White',
        'Three or More'
      )
    )
  ) |>
  ggplot(aes(x = label, y = n, fill = group)) +
  geom_col() +
  scale_fill_viridis_d() +
  facet_wrap(~label, scales = 'free') +
  labs(facet = NULL, fill = NULL, x = NULL, y = 'N') +
  theme_minimal() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.position = "bottom"
  )


ggsave('final-fig/group-2.png', height = 8, width = 10)
