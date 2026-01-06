# aa-enrollment.R


aa_enrollment_18 <- enrollment |>
  select(hash_id, enrolled=enrolled_18f, starts_with('ETH_')) |>
  filter(enrolled==1) |>
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
  select(white, black, asian, hispanic, native, swana, nhpi, three_plus) |>
  summarize(white = sum(white), black=sum(black), hispanic=sum(hispanic),
            native=sum(native), swana=sum(swana),
            asian=sum(asian), nhpi=sum(nhpi), three_plus=sum(three_plus)) |>
  mutate(year = 18)


aa_enrollment_19 <- enrollment |>
  select(hash_id, enrolled=enrolled_19f, starts_with('ETH_')) |>
  filter(enrolled==1) |>
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
  select(white, black, asian, hispanic, native, swana, nhpi, three_plus) |>
  summarize(white = sum(white), black=sum(black), hispanic=sum(hispanic),
            native=sum(native), swana=sum(swana),
            asian=sum(asian), nhpi=sum(nhpi), three_plus=sum(three_plus)) |>
  mutate(year = 19)

aa_enrollment_20 <- enrollment |>
  select(hash_id, reducedethnic=reducedethnic_20f, enrolled=enrolled_20f, starts_with('ETH_')) |>
  filter(enrolled==1) |>
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
  select(white, black, asian, hispanic, native, swana, nhpi, three_plus) |>
  summarize(white = sum(white), black=sum(black), hispanic=sum(hispanic),
            native=sum(native), swana=sum(swana),
            asian=sum(asian), nhpi=sum(nhpi), three_plus=sum(three_plus)) |>
  mutate(year = 20)

aa_enrollment_21 <- enrollment |>
  select(hash_id, reducedethnic=reducedethnic_21f, enrolled=enrolled_21f, starts_with('ETH_')) |>
  filter(enrolled==1) |>
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
  select(white, black, asian, hispanic, native, swana, nhpi, three_plus) |>
  summarize(white = sum(white), black=sum(black), hispanic=sum(hispanic),
            native=sum(native), swana=sum(swana),
            asian=sum(asian), nhpi=sum(nhpi), three_plus=sum(three_plus)) |>
  mutate(year = 21)

aa_enrollment_22 <- enrollment |>
  select(hash_id, reducedethnic=reducedethnic_22f, enrolled=enrolled_22f, starts_with('ETH_')) |>
  filter(enrolled==1) |>
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
  select(white, black, asian, hispanic, native, swana, nhpi, three_plus) |>
  summarize(white = sum(white), black=sum(black), hispanic=sum(hispanic),
            native=sum(native), swana=sum(swana),
            asian=sum(asian), nhpi=sum(nhpi), three_plus=sum(three_plus)) |>
  mutate(year = 22)

aa_enrollment <- bind_rows(aa_enrollment_18, aa_enrollment_19, aa_enrollment_20, aa_enrollment_21, aa_enrollment_22)

write_csv(aa_enrollment, 'tab/aa_enrollment.csv')

aa_enrollment |>
  pivot_longer(-year, names_to='group', values_to='n') |>
  ggplot(aes(x=as.character(year), y=n, fill=group )) +
  geom_col() +
  scale_fill_viridis_d() +
  theme_minimal()


ggsave('fig/aa-enrollment.png', height=6, width=8)

