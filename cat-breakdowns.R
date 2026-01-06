
targets <- c(aa, nhpi)

for (i in seq_along(targets)){

  target_cat <- targets[i]

  target_enr <- enrollment |>
    select(hash_id,
           enrolled_18f, enrolled_19f, enrolled_20f, enrolled_21f, enrolled_22f,
           starts_with('ETH_')) |>
    filter(enrolled_18f+enrolled_19f+enrolled_20f+enrolled_21f+enrolled_22f>=1) |>
    filter(get({{target_cat}})==1) |>
    select(all_of(aa), all_of(nhpi), ETH_WHITE, ETH_BLACK, ETH_LATINX, ETH_ANNA, ETH_SWANA)

  target_enr$eth_cats <- rowSums(target_enr)

  target_enr <- target_enr |>
    select(-{{target_cat}}) |>
    mutate(ETH_SINGLE = if_else(eth_cats == 1, 1, 0))


  ggplot(target_enr, aes(x=eth_cats)) +
    geom_histogram(bins=max(target_enr$eth_cats)) +
    theme_minimal()

  N <- nrow(target_enr)

  target_breakdown <- target_enr |>
    select(-eth_cats) |>
    mutate(id = 1:nrow(target_enr)) |>
    pivot_longer(-id, names_to='category', values_to='val') |>
    filter(val==1) |>
    left_join(codebook, by='category')  |>
    group_by(id) |>
    summarize(ETH_COLLAPSED = str_flatten_comma(category),
              cat_collapsed=str_flatten_comma(cat_name)) |>
    group_by(ETH_COLLAPSED, cat_collapsed) |>
    summarize(n=n(), .groups='drop') |>
    mutate(p = n/N) |>
    arrange(-n)

  write_csv(target_breakdown, paste0('tab/', target_cat, '-breakdown.csv'))
}
