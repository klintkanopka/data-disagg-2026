# total

latinx_enr <- enrollment |>
  select(hash_id,
         enrolled_18f, enrolled_19f, enrolled_20f, enrolled_21f, enrolled_22f,
         starts_with('ETH_')) |>
  filter(enrolled_18f+enrolled_19f+enrolled_20f+enrolled_21f+enrolled_22f>=1) |>
  select(all_of(aa), all_of(nhpi), ETH_LATINX)


latinx_breakdown <- latinx_enr |>
  mutate(id = 1:nrow(latinx_enr)) |>
  pivot_longer(c(-id, -ETH_LATINX), names_to='category', values_to='val') |>
  filter(val==1) |>
  group_by(category) |>
  summarize(N = n(),
            N_LATINX = sum(ETH_LATINX),
            P_LATINX = mean(ETH_LATINX)) |>
  arrange(-P_LATINX) |>
  left_join(codebook, by='category') |>
  select(category, cat_name, N, N_LATINX, P_LATINX)

write_csv(latinx_breakdown, 'tab/latinx-breakdown.csv')
