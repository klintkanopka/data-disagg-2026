codebook <- read_excel(
  'data/updated/Admissions Race Ethnicity Codebook.xlsx',
  skip = 2,
  col_names = c('category', 'cat_name')
) |>
  mutate(cat_name = str_replace_all(cat_name, "'", '')) |>
  mutate(cat_name = str_squish(cat_name)) |>
  mutate(
    cat_name = case_when(
      category == 'ETH_CD_PI_FL' ~ 'Filipinx',
      category == 'ETH_CD_GC_FL' ~ 'Chamorro',
      category == 'ETH_CD_AN_FL' ~ 'Indian',
      category == 'ETH_CD_CH_FL' ~ 'Chinese',
      category == 'ETH_CD_JA_FL' ~ 'Japanese',
      category == 'ETH_CD_HW_FL' ~ 'Native Hawaiian',
      TRUE ~ cat_name
    )
  ) |>
  add_row(category = 'ETH_AA', cat_name = 'Asian') |>
  add_row(category = 'ETH_NHPI', cat_name = 'NHPI') |>
  add_row(category = 'ETH_WHITE', cat_name = 'White') |>
  add_row(category = 'ETH_BLACK', cat_name = 'Black') |>
  add_row(category = 'ETH_LATINX', cat_name = 'Latine') |>
  add_row(category = 'ETH_SWANA', cat_name = 'SWANA') |>
  add_row(category = 'ETH_ANNA', cat_name = 'ANNA') |>
  add_row(category = 'ETH_AA_multiethnic', cat_name = 'AA Multiethnic') |>
  add_row(category = 'ETH_NHPI_multiethnic', cat_name = 'NHPI Multiethnic') |>
  add_row(
    category = 'ETH_AA_NHPI_multiracial',
    cat_name = 'AA/NHPI Multiracial'
  ) |>
  add_row(category = 'ETH_SINGLE', cat_name = 'AA/NHPI Monoracial ') |>
  add_row(category = 'ETH_single', cat_name = 'AA/NHPI Monoracial ') |>
  add_row(category = 'ETH_RACECOUNT', cat_name = 'N RACES')


d_categories <- data.frame(
  race = c(
    rep('AA', length(aa)),
    rep('NHPI', length(nhpi)),
    rep('White', length(white)),
    rep('ANNA', length(anna)),
    rep('Latine', length(latinx)),
    rep('Black', length(black)),
    rep('SWANA', length(swana))
  ),
  category = c(aa, nhpi, white, anna, latinx, black, swana)
) |>
  left_join(codebook, by = 'category')

write_csv(d_categories, 'final-tab/appendix-A.csv')
