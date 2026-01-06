# nhpi-enrollment.R


nhpi_enrollment_18 <- enrollment |>
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
  filter(nhpi == 1) |>
  mutate(nhpi = if_else(white+black+hispanic+native+swana+asian+nhpi == 1, 1, 0),
         white = if_else(three_plus==1, 0, white),
         black = if_else(three_plus==1, 0, black),
         hispanic = if_else(three_plus==1, 0, hispanic) ,
         native = if_else(three_plus==1, 0, native),
         swana = if_else(three_plus==1, 0, swana),
         asian = if_else(three_plus==1, 0, asian)) |>
  select(white, black, hispanic, native, swana, asian, nhpi, three_plus) |>
  summarize(white = sum(white), black=sum(black), hispanic=sum(hispanic),
            native=sum(native), swana=sum(swana),
            asian=sum(asian), nhpi=sum(nhpi), three_plus=sum(three_plus)) |>
  mutate(year = 18)


nhpi_enrollment_19 <- enrollment |>
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
  filter(nhpi == 1) |>
  mutate(nhpi = if_else(white+black+hispanic+native+swana+asian+nhpi == 1, 1, 0),
         white = if_else(three_plus==1, 0, white),
         black = if_else(three_plus==1, 0, black),
         hispanic = if_else(three_plus==1, 0, hispanic) ,
         native = if_else(three_plus==1, 0, native),
         swana = if_else(three_plus==1, 0, swana),
         asian = if_else(three_plus==1, 0, asian)) |>
  select(white, black, hispanic, native, swana, asian, nhpi, three_plus) |>
  summarize(white = sum(white), black=sum(black), hispanic=sum(hispanic),
            native=sum(native), swana=sum(swana),
            asian=sum(asian), nhpi=sum(nhpi), three_plus=sum(three_plus)) |>
  mutate(year = 19)

nhpi_enrollment_20 <- enrollment |>
  select(hash_id, enrolled=enrolled_20f, starts_with('ETH_')) |>
  filter(enrolled==1) |>
  mutate(white = ETH_WHITE,
         black = ETH_BLACK,
         hispanic = ETH_LATINX,
         native = ETH_ANNA,
         swana = ETH_SWANA,
         asian = ETH_AA,
         nhpi = ETH_NHPI) |>
  mutate(three_plus = if_else(white+black+hispanic+native+swana+asian+nhpi >= 3, 1, 0)) |>
  filter(nhpi == 1) |>
  mutate(nhpi = if_else(white+black+hispanic+native+swana+asian+nhpi == 1, 1, 0),
         white = if_else(three_plus==1, 0, white),
         black = if_else(three_plus==1, 0, black),
         hispanic = if_else(three_plus==1, 0, hispanic) ,
         native = if_else(three_plus==1, 0, native),
         swana = if_else(three_plus==1, 0, swana),
         asian = if_else(three_plus==1, 0, asian)) |>
  select(white, black, hispanic, native, swana, asian, nhpi, three_plus) |>
  summarize(white = sum(white), black=sum(black), hispanic=sum(hispanic),
            native=sum(native), swana=sum(swana),
            asian=sum(asian), nhpi=sum(nhpi), three_plus=sum(three_plus)) |>
  mutate(year = 20)

nhpi_enrollment_21 <- enrollment |>
  select(hash_id, enrolled=enrolled_21f, starts_with('ETH_')) |>
  filter(enrolled==1) |>
  mutate(white = ETH_WHITE,
         black = ETH_BLACK,
         hispanic = ETH_LATINX,
         native = ETH_ANNA,
         swana = ETH_SWANA,
         asian = ETH_AA,
         nhpi = ETH_NHPI) |>
  mutate(three_plus = if_else(white+black+hispanic+native+swana+asian+nhpi >= 3, 1, 0)) |>
  filter(nhpi == 1) |>
  mutate(nhpi = if_else(white+black+hispanic+native+swana+asian+nhpi == 1, 1, 0),
         white = if_else(three_plus==1, 0, white),
         black = if_else(three_plus==1, 0, black),
         hispanic = if_else(three_plus==1, 0, hispanic) ,
         native = if_else(three_plus==1, 0, native),
         swana = if_else(three_plus==1, 0, swana),
         asian = if_else(three_plus==1, 0, asian)) |>
  select(white, black, hispanic, native, swana, asian, nhpi, three_plus) |>
  summarize(white = sum(white), black=sum(black), hispanic=sum(hispanic),
            native=sum(native), swana=sum(swana),
            asian=sum(asian), nhpi=sum(nhpi), three_plus=sum(three_plus)) |>
  mutate(year = 21)

nhpi_enrollment_22 <- enrollment |>
  select(hash_id, enrolled=enrolled_22f, starts_with('ETH_')) |>
  filter(enrolled==1) |>
  mutate(white = ETH_WHITE,
         black = ETH_BLACK,
         hispanic = ETH_LATINX,
         native = ETH_ANNA,
         swana = ETH_SWANA,
         asian = ETH_AA,
         nhpi = ETH_NHPI) |>
  mutate(three_plus = if_else(white+black+hispanic+native+swana+asian+nhpi >= 3, 1, 0)) |>
  filter(nhpi == 1) |>
  mutate(nhpi = if_else(white+black+hispanic+native+swana+asian+nhpi == 1, 1, 0),
         white = if_else(three_plus==1, 0, white),
         black = if_else(three_plus==1, 0, black),
         hispanic = if_else(three_plus==1, 0, hispanic) ,
         native = if_else(three_plus==1, 0, native),
         swana = if_else(three_plus==1, 0, swana),
         asian = if_else(three_plus==1, 0, asian)) |>
  select(white, black, hispanic, native, swana, asian, nhpi, three_plus) |>
  summarize(white = sum(white), black=sum(black), hispanic=sum(hispanic),
            native=sum(native), swana=sum(swana),
            asian=sum(asian), nhpi=sum(nhpi), three_plus=sum(three_plus)) |>
  mutate(year = 22)

nhpi_enrollment <- bind_rows(nhpi_enrollment_18,
                             nhpi_enrollment_19,
                             nhpi_enrollment_20,
                             nhpi_enrollment_21,
                             nhpi_enrollment_22)

write_csv(nhpi_enrollment, 'tab/nhpi_enrollment.csv')


nhpi_enrollment |>
  pivot_longer(-year, names_to='group', values_to='n') |>
  ggplot(aes(x=as.character(year), y=n, fill=group )) +
  geom_col() +
  scale_fill_viridis_d() +
  theme_minimal()

ggsave('fig/nhpi-enrollment.png', height=6, width=8)
