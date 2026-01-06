# single-identity.R
# proportion of each AA/NHPI category identifying as single race and ethnicity

single_group <- admissions |>
  filter(ETH_AA + ETH_NHPI > 0) |>
  select(all_of(aa), all_of(nhpi), ETH_single) |>
  pivot_longer(starts_with('ETH_CD'), names_to='category', values_to='value') |>
  filter(value==1) |>
  group_by(category) |>
  summarize(single_cat = mean(ETH_single==1)) |>
  left_join(codebook, by='category')

ggplot(single_group, aes(x=single_cat, y=reorder(cat_name, single_cat),
             fill=if_else(category%in%aa, 'aa', 'nhpi'))) +
  geom_col() +
  scale_fill_viridis_d() +
  labs(y='category', x='proportion identifying with only a single ethnic category',
       fill='race') +
  theme_minimal()

write_csv(single_group, 'tab/single_group.csv')
ggsave('fig/single-group.png', height=6, width=8)
