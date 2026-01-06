# native hawaiian ETH_CD_HW_FL

target_cat <- 'ETH_CD_HW_FL'

hw_codes <- data.frame(category = c('ETH_SINGLE', aa, nhpi,
                                    'ETH_WHITE', 'ETH_BLACK', 'ETH_LATINX',
                                    'ETH_ANNA', 'ETH_SWANA')) |>
  left_join(codebook, by='category')

# total

hw_enrollment <- enrollment |>
  select(hash_id,
         enrolled_18f, enrolled_19f, enrolled_20f, enrolled_21f, enrolled_22f,
         starts_with('ETH_')) |>
  filter(enrolled_18f+enrolled_19f+enrolled_20f+enrolled_21f+enrolled_22f>=1) |>
  filter(get({{target_cat}})==1) |>
  select(all_of(aa), all_of(nhpi), ETH_WHITE, ETH_BLACK, ETH_LATINX, ETH_ANNA, ETH_SWANA)

eth_cats <- if_else(rowSums(hw_enrollment) == 1, 1, 0)

hw_enrollment <- hw_enrollment |>
  select(-{{target_cat}}) |>
  mutate(ETH_SINGLE = eth_cats)

hw_cor <- cor(select(hw_enrollment, -ETH_SINGLE),
              method='spearman') |>
  data.frame() |>
  rownames_to_column('cat1') |>
  pivot_longer(-cat1, names_to='cat2', values_to = 'r') |>
  mutate(cat1=factor(cat1, levels=hw_codes$category, labels=hw_codes$cat_name),
         cat2=factor(cat2, levels=rev(hw_codes$category), labels=rev(hw_codes$cat_name))) |>
  filter(r < 1) |>
  na.omit()

ggplot(hw_cor, aes(x=cat1, y=cat2, fill=r)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_discrete(guide = guide_axis(angle = 60)) +
  labs(x=NULL, y=NULL, title='Ethnic Category Correlations for Enrolled Hawaiians (2018-2022)') +
  coord_equal() +
  theme_minimal()

ggsave('fig/hawaiian-cor.png', height=6, width=8)

hw_count <- crossprod(as.matrix(hw_enrollment)) |>
  data.frame() |>
  rownames_to_column('cat1') |>
  pivot_longer(-cat1, names_to='cat2', values_to = 'count') |>
  mutate(cat1=factor(cat1, levels=hw_codes$category, labels=hw_codes$cat_name),
         cat2=factor(cat2, levels=rev(hw_codes$category), labels=rev(hw_codes$cat_name))) |>
  mutate(count = if_else(count > 0, count, NA)) |>
  na.omit()

ggplot(hw_count, aes(x=cat1, y=cat2, fill=count)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_discrete(guide = guide_axis(angle = 60)) +
  labs(x=NULL, y=NULL, title='Ethnic Category Counts for Enrolled Hawaiians (2018-2022)') +
  coord_equal() +
  theme_minimal()

ggsave('fig/hawaiian-count.png', height=6, width=8)

write_csv(hw_cor, 'tab/hawaiian-cor.csv')
write_csv(hw_count, 'tab/hawaiian-count.csv')

# year 18

hw_enrollment_18 <- enrollment |>
  select(hash_id, enrolled=enrolled_18f, starts_with('ETH_')) |>
  filter(enrolled==1) |>
  filter(get({{target_cat}})==1) |>
  select(all_of(aa), all_of(nhpi), ETH_WHITE, ETH_BLACK, ETH_LATINX, ETH_ANNA, ETH_SWANA)

eth_cats <- if_else(rowSums(hw_enrollment_18) == 1, 1, 0)

hw_enrollment_18 <- hw_enrollment_18 |>
  select(-{{target_cat}}) |>
  mutate(ETH_SINGLE = eth_cats,
         id = 1:nrow(hw_enrollment_18))

hw_enrollment_18 |>
  pivot_longer(-id, names_to='cat', values_to = 'value')

hw_cor_18 <- cor(select(hw_enrollment_18, -id, -ETH_SINGLE),
                 method='spearman') |>
  data.frame() |>
  rownames_to_column('cat1') |>
  pivot_longer(-cat1, names_to='cat2', values_to = 'r')  |>
  mutate(cat1=factor(cat1, levels=hw_codes$category, labels=hw_codes$cat_name),
         cat2=factor(cat2, levels=rev(hw_codes$category), labels=rev(hw_codes$cat_name))) |>
  filter(r < 1) |>
  na.omit()

ggplot(hw_cor_18, aes(x=cat1, y=cat2, fill=r)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_discrete(guide = guide_axis(angle = 60)) +
  labs(x=NULL, y=NULL, title='Ethnic Category Correlations for Enrolled Hawaiians (2018)') +
  coord_equal() +
  theme_minimal()

ggsave('fig/hawaiian-cor-18.png', height=6, width=8)

hw_count_18 <- crossprod(as.matrix(select(hw_enrollment_18, -id))) |>
  data.frame() |>
  rownames_to_column('cat1') |>
  pivot_longer(-cat1, names_to='cat2', values_to = 'count') |>
  mutate(cat1=factor(cat1, levels=hw_codes$category, labels=hw_codes$cat_name),
         cat2=factor(cat2, levels=rev(hw_codes$category), labels=rev(hw_codes$cat_name))) |>
  mutate(count = if_else(count > 0, count, NA)) |>
  na.omit()

ggplot(hw_count_18, aes(x=cat1, y=cat2, fill=count)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_discrete(guide = guide_axis(angle = 60)) +
  labs(x=NULL, y=NULL, title='Ethnic Category Counts for Enrolled Hawaiians (2018)') +
  coord_equal() +
  theme_minimal()

ggsave('fig/hawaiian-count-18.png', height=6, width=8)

write_csv(hw_cor_18, 'tab/hawaiian-cor-18.csv')
write_csv(hw_count_18, 'tab/hawaiian-count-18.csv')



# year 19

hw_enrollment_19 <- enrollment |>
  select(hash_id, enrolled=enrolled_19f, starts_with('ETH_')) |>
  filter(enrolled==1) |>
  filter(get({{target_cat}})==1) |>
  select(all_of(aa), all_of(nhpi), ETH_WHITE, ETH_BLACK, ETH_LATINX, ETH_ANNA, ETH_SWANA)

eth_cats <- if_else(rowSums(hw_enrollment_19) == 1, 1, 0)

hw_enrollment_19 <- hw_enrollment_19 |>
  select(-{{target_cat}}) |>
  mutate(ETH_SINGLE = eth_cats,
         id = 1:nrow(hw_enrollment_19))

hw_enrollment_19 |>
  pivot_longer(-id, names_to='cat', values_to = 'value')

hw_cor_19 <- cor(select(hw_enrollment_19, -id, -ETH_SINGLE),
                 method='spearman') |>
  data.frame() |>
  rownames_to_column('cat1') |>
  pivot_longer(-cat1, names_to='cat2', values_to = 'r') |>
  mutate(cat1=factor(cat1, levels=hw_codes$category, labels=hw_codes$cat_name),
         cat2=factor(cat2, levels=rev(hw_codes$category), labels=rev(hw_codes$cat_name))) |>
  filter(r < 1) |>
  na.omit()

ggplot(hw_cor_19, aes(x=cat1, y=cat2, fill=r)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_discrete(guide = guide_axis(angle = 60)) +
  labs(x=NULL, y=NULL, title='Ethnic Category Correlations for Enrolled Hawaiians (2019)') +
  coord_equal() +
  theme_minimal()

ggsave('fig/hawaiian-cor-19.png', height=6, width=8)

hw_count_19 <- crossprod(as.matrix(select(hw_enrollment_19, -id))) |>
  data.frame() |>
  rownames_to_column('cat1') |>
  pivot_longer(-cat1, names_to='cat2', values_to = 'count') |>
  mutate(cat1=factor(cat1, levels=hw_codes$category, labels=hw_codes$cat_name),
         cat2=factor(cat2, levels=rev(hw_codes$category), labels=rev(hw_codes$cat_name))) |>
  mutate(count = if_else(count > 0, count, NA)) |>
  na.omit()

ggplot(hw_count_19, aes(x=cat1, y=cat2, fill=count)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_discrete(guide = guide_axis(angle = 60)) +
  labs(x=NULL, y=NULL, title='Ethnic Category Counts for Enrolled Hawaiians (2019)') +
  coord_equal() +
  theme_minimal()

ggsave('fig/hawaiian-count-19.png', height=6, width=8)

write_csv(hw_cor_19, 'tab/hawaiian-cor-19.csv')
write_csv(hw_count_19, 'tab/hawaiian-count-19.csv')



# year 20

hw_enrollment_20 <- enrollment |>
  select(hash_id, enrolled=enrolled_20f, starts_with('ETH_')) |>
  filter(enrolled==1) |>
  filter(get({{target_cat}})==1) |>
  select(all_of(aa), all_of(nhpi), ETH_WHITE, ETH_BLACK, ETH_LATINX, ETH_ANNA, ETH_SWANA)

eth_cats <- if_else(rowSums(hw_enrollment_20) == 1, 1, 0)

hw_enrollment_20 <- hw_enrollment_20 |>
  select(-{{target_cat}}) |>
  mutate(ETH_SINGLE = eth_cats,
         id = 1:nrow(hw_enrollment_20))

hw_enrollment_20 |>
  pivot_longer(-id, names_to='cat', values_to = 'value')

hw_cor_20 <- cor(select(hw_enrollment_20, -id, -ETH_SINGLE),
                 method='spearman') |>
  data.frame() |>
  rownames_to_column('cat1') |>
  pivot_longer(-cat1, names_to='cat2', values_to = 'r') |>
  mutate(cat1=factor(cat1, levels=hw_codes$category, labels=hw_codes$cat_name),
         cat2=factor(cat2, levels=rev(hw_codes$category), labels=rev(hw_codes$cat_name))) |>
  filter(r < 1) |>
  na.omit()

ggplot(hw_cor_20, aes(x=cat1, y=cat2, fill=r)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_discrete(guide = guide_axis(angle = 60)) +
  labs(x=NULL, y=NULL, title='Ethnic Category Correlations for Enrolled Hawaiians (2020)') +
  coord_equal() +
  theme_minimal()

ggsave('fig/hawaiian-cor-20.png', height=6, width=8)

hw_count_20 <- crossprod(as.matrix(select(hw_enrollment_20, -id))) |>
  data.frame() |>
  rownames_to_column('cat1') |>
  pivot_longer(-cat1, names_to='cat2', values_to = 'count') |>
  mutate(cat1=factor(cat1, levels=hw_codes$category, labels=hw_codes$cat_name),
         cat2=factor(cat2, levels=rev(hw_codes$category), labels=rev(hw_codes$cat_name))) |>
  mutate(count = if_else(count > 0, count, NA)) |>
  na.omit()

ggplot(hw_count_20, aes(x=cat1, y=cat2, fill=count)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_discrete(guide = guide_axis(angle = 60)) +
  labs(x=NULL, y=NULL, title='Ethnic Category Counts for Enrolled Hawaiians (2020)') +
  coord_equal() +
  theme_minimal()

ggsave('fig/hawaiian-count-20.png', height=6, width=8)

write_csv(hw_cor_20, 'tab/hawaiian-cor-20.csv')
write_csv(hw_count_20, 'tab/hawaiian-count-20.csv')



# year 21

hw_enrollment_21 <- enrollment |>
  select(hash_id, enrolled=enrolled_21f, starts_with('ETH_')) |>
  filter(enrolled==1) |>
  filter(get({{target_cat}})==1) |>
  select(all_of(aa), all_of(nhpi), ETH_WHITE, ETH_BLACK, ETH_LATINX, ETH_ANNA, ETH_SWANA)

eth_cats <- if_else(rowSums(hw_enrollment_21) == 1, 1, 0)

hw_enrollment_21 <- hw_enrollment_21 |>
  select(-{{target_cat}}) |>
  mutate(ETH_SINGLE = eth_cats,
         id = 1:nrow(hw_enrollment_21))

hw_enrollment_21 |>
  pivot_longer(-id, names_to='cat', values_to = 'value')

hw_cor_21 <- cor(select(hw_enrollment_21, -id, -ETH_SINGLE),
                 method='spearman') |>
  data.frame() |>
  rownames_to_column('cat1') |>
  pivot_longer(-cat1, names_to='cat2', values_to = 'r') |>
  mutate(cat1=factor(cat1, levels=hw_codes$category, labels=hw_codes$cat_name),
         cat2=factor(cat2, levels=rev(hw_codes$category), labels=rev(hw_codes$cat_name))) |>
  filter(r < 1) |>
  na.omit()

ggplot(hw_cor_21, aes(x=cat1, y=cat2, fill=r)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_discrete(guide = guide_axis(angle = 60)) +
  labs(x=NULL, y=NULL, title='Ethnic Category Correlations for Enrolled Hawaiians (2021)') +
  coord_equal() +
  theme_minimal()

ggsave('fig/hawaiian-cor-21.png', height=6, width=8)

hw_count_21 <- crossprod(as.matrix(select(hw_enrollment_21, -id))) |>
  data.frame() |>
  rownames_to_column('cat1') |>
  pivot_longer(-cat1, names_to='cat2', values_to = 'count') |>
  mutate(cat1=factor(cat1, levels=hw_codes$category, labels=hw_codes$cat_name),
         cat2=factor(cat2, levels=rev(hw_codes$category), labels=rev(hw_codes$cat_name))) |>
  mutate(count = if_else(count > 0, count, NA)) |>
  na.omit()

ggplot(hw_count_21, aes(x=cat1, y=cat2, fill=count)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_discrete(guide = guide_axis(angle = 60)) +
  labs(x=NULL, y=NULL, title='Ethnic Category Counts for Enrolled Hawaiians (2021)') +
  coord_equal() +
  theme_minimal()

ggsave('fig/hawaiian-count-21.png', height=6, width=8)

write_csv(hw_cor_21, 'tab/hawaiian-cor-21.csv')
write_csv(hw_count_21, 'tab/hawaiian-count-21.csv')


# year 22

hw_enrollment_22 <- enrollment |>
  select(hash_id, enrolled=enrolled_22f, starts_with('ETH_')) |>
  filter(enrolled==1) |>
  filter(get({{target_cat}})==1) |>
  select(all_of(aa), all_of(nhpi), ETH_WHITE, ETH_BLACK, ETH_LATINX, ETH_ANNA, ETH_SWANA)

eth_cats <- if_else(rowSums(hw_enrollment_22) == 1, 1, 0)

hw_enrollment_22 <- hw_enrollment_22 |>
  select(-{{target_cat}}) |>
  mutate(ETH_SINGLE = eth_cats,
         id = 1:nrow(hw_enrollment_22))

hw_enrollment_22 |>
  pivot_longer(-id, names_to='cat', values_to = 'value')

hw_cor_22 <- cor(select(hw_enrollment_22, -id, -ETH_SINGLE),
                 method='spearman') |>
  data.frame() |>
  rownames_to_column('cat1') |>
  pivot_longer(-cat1, names_to='cat2', values_to = 'r') |>
  mutate(cat1=factor(cat1, levels=hw_codes$category, labels=hw_codes$cat_name),
         cat2=factor(cat2, levels=rev(hw_codes$category), labels=rev(hw_codes$cat_name))) |>
  filter(r < 1) |>
  na.omit()

ggplot(hw_cor_22, aes(x=cat1, y=cat2, fill=r)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_discrete(guide = guide_axis(angle = 60)) +
  labs(x=NULL, y=NULL, title='Ethnic Category Correlations for Enrolled Hawaiians (2022)') +
  coord_equal() +
  theme_minimal()

ggsave('fig/hawaiian-cor-22.png', height=6, width=8)

hw_count_22 <- crossprod(as.matrix(select(hw_enrollment_22, -id))) |>
  data.frame() |>
  rownames_to_column('cat1') |>
  pivot_longer(-cat1, names_to='cat2', values_to = 'count') |>
  mutate(cat1=factor(cat1, levels=hw_codes$category, labels=hw_codes$cat_name),
         cat2=factor(cat2, levels=rev(hw_codes$category), labels=rev(hw_codes$cat_name))) |>
  mutate(count = if_else(count > 0, count, NA)) |>
  na.omit()

ggplot(hw_count_22, aes(x=cat1, y=cat2, fill=count)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_discrete(guide = guide_axis(angle = 60)) +
  labs(x=NULL, y=NULL, title='Ethnic Category Counts for Enrolled Hawaiians (2022)') +
  coord_equal() +
  theme_minimal()

ggsave('fig/hawaiian-count-22.png', height=6, width=8)

write_csv(hw_cor_22, 'tab/hawaiian-cor-22.csv')
write_csv(hw_count_22, 'tab/hawaiian-count-22.csv')
