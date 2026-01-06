# aa-admit.R

aa_admit <- admissions |>
  select(hash_id, starts_with('ETH_'), admit, term_cd) |>
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
  select(white, black, asian, hispanic, native, swana, nhpi, three_plus, admit, term_cd) |>
  pivot_longer(c(-admit, -term_cd), names_to = 'group', values_to = 'value') |>
  filter(value==1) |>
  group_by(group, term_cd) |>
  summarize(admit_rate=mean(admit), .groups='drop')

write_csv(aa_admit, 'tab/aa_admit.csv')

ggplot(aa_admit, aes(x=term_cd, y=admit_rate, color=group)) +
  geom_line() +
  geom_point() +
  scale_color_viridis_d() +
  labs(title='multiracial asian admit rates') +
  theme_minimal()

ggsave('fig/aa-admit.png', height=6, width=8)
