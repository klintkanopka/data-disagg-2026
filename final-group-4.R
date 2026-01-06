# native hawaiian ETH_CD_HW_FL

target_cat <- 'ETH_CD_HW_FL'

hw_codes <- data.frame(
  category = c(
    'ETH_SINGLE',
    aa,
    nhpi,
    'ETH_WHITE',
    'ETH_BLACK',
    'ETH_LATINX',
    'ETH_ANNA',
    'ETH_SWANA'
  )
) |>
  left_join(codebook, by = 'category')


# year 22

hw_enrollment_22 <- enrollment |>
  select(hash_id, enrolled = enrolled_22f, starts_with('ETH_')) |>
  filter(enrolled == 1) |>
  filter(get({{ target_cat }}) == 1) |>
  select(
    all_of(aa),
    all_of(nhpi),
    ETH_WHITE,
    ETH_BLACK,
    ETH_LATINX,
    ETH_ANNA,
    ETH_SWANA
  )

eth_cats <- if_else(rowSums(hw_enrollment_22) == 1, 1, 0)

hw_enrollment_22 <- hw_enrollment_22 |>
  select(-{{ target_cat }}) |>
  mutate(ETH_SINGLE = eth_cats, id = 1:nrow(hw_enrollment_22))

hw_enrollment_22 |>
  pivot_longer(-id, names_to = 'cat', values_to = 'value')

hw_cor_22 <- cor(
  select(hw_enrollment_22, -id, -ETH_SINGLE),
  method = 'spearman'
) |>
  data.frame() |>
  rownames_to_column('cat1') |>
  pivot_longer(-cat1, names_to = 'cat2', values_to = 'r') |>
  mutate(
    cat1 = factor(cat1, levels = hw_codes$category, labels = hw_codes$cat_name),
    cat2 = factor(
      cat2,
      levels = rev(hw_codes$category),
      labels = rev(hw_codes$cat_name)
    )
  ) |>
  filter(r < 1) |>
  na.omit()

ggplot(hw_cor_22, aes(x = cat1, y = cat2, fill = r)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_discrete(guide = guide_axis(angle = 60)) +
  labs(
    x = NULL,
    y = NULL,
    title = 'Ethnic Category Correlations for Native Hawaiian Student Enrollment'
  ) +
  coord_equal() +
  theme_minimal()

ggsave('final-fig/group-4b.png', height = 8, width = 10)

hw_count_22 <- crossprod(as.matrix(select(hw_enrollment_22, -id))) |>
  data.frame() |>
  rownames_to_column('cat1') |>
  pivot_longer(-cat1, names_to = 'cat2', values_to = 'count') |>
  mutate(
    cat1 = factor(cat1, levels = hw_codes$category, labels = hw_codes$cat_name),
    cat2 = factor(
      cat2,
      levels = rev(hw_codes$category),
      labels = rev(hw_codes$cat_name)
    )
  ) |>
  mutate(count = if_else(count > 0, count, NA)) |>
  na.omit()

ggplot(hw_count_22, aes(x = cat1, y = cat2, fill = count)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_discrete(guide = guide_axis(angle = 60)) +
  labs(
    x = NULL,
    y = NULL,
    title = 'Ethnic Category Counts for Native Hawaiian Student Enrollment'
  ) +
  coord_equal() +
  theme_minimal()

ggsave('final-fig/group-4a.png', height = 8, width = 10)

write_csv(hw_cor_22, 'final-tab/group-4b.csv')
write_csv(hw_count_22, 'final-tab/group-4a.csv')
