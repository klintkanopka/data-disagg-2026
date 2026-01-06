# aa-yield.R

aa_yield <- admissions |>
  select(hash_id, starts_with('ETH_'), admit, reg, term_cd) |>
  filter(admit==1) |>
  mutate(term_cd = as.numeric(str_sub(term_cd, 1, 2))) |>
  mutate(white = ETH_WHITE,
         black = ETH_BLACK,
         hispanic = ETH_LATINX,
         native = ETH_ANNA,
         swana = ETH_SWANA,
         asian = ETH_AA,
         nhpi = ETH_NHPI) |>
  mutate(three_plus = if_else(white+black+hispanic+native+swana+asian+nhpi >= 3, 1, 0)) |>
  filter(asian == 1) |>
  mutate(asian = if_else(white+black+hispanic+native+swana+asian+nhpi == 1, 1, 0),
         white = if_else(three_plus==1, 0, white),
         black = if_else(three_plus==1, 0, black),
         hispanic = if_else(three_plus==1, 0, hispanic) ,
         native = if_else(three_plus==1, 0, native),
         swana = if_else(three_plus==1, 0, swana),
         nhpi = if_else(three_plus==1, 0, nhpi)) |>
  select(white, black, asian, hispanic, native, swana, nhpi, three_plus, reg, term_cd) |>
  pivot_longer(c(-reg, -term_cd), names_to = 'group', values_to = 'value') |>
  filter(value==1) |>
  group_by(group, term_cd) |>
  summarize(yield_rate=mean(reg), .groups='drop')

write_csv(aa_yield, 'tab/aa_yield.csv')

ggplot(aa_yield, aes(x=term_cd, y=yield_rate, color=group)) +
  geom_line() +
  geom_point() +
  scale_color_viridis_d() +
  labs(title='multiracial asian yield rates') +
  theme_minimal()

ggsave('fig/aa-yield.png', height=6, width=8)
