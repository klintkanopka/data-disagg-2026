# preproc-enrollment.R
# load and preprocess enrollment data by constructing racial categories


enrollment <- read_excel('data/updated/APB Enrollment Data v03 112823.xlsx') |>
  select(-starts_with('ETH')) |>
  mutate(across(starts_with('enrolled_'), ~if_else(is.na(.x) | .x=='NULL', 0, 1)) ) |>
  mutate(pell = if_else(ever_pell_fl=='Y', 1, 0)) |>
  mutate(first_gen = if_else(first_gen_bachelors =='First Gen', 1, 0))


enrollment_aanhpi <- read_excel('data/updated/APB Enrollment Data v03 112823.xlsx') |>
  select(all_of(aa), all_of(nhpi), all_of(white), all_of(black), all_of(latinx),
         all_of(swana), all_of(anna)) |>
  mutate(across(starts_with("ETH_"), ~if_else(is.na(.x) | .x=='NULL', 0, 1)) )

enrollment <- bind_cols(enrollment, enrollment_aanhpi)

aa_enr_tmp <- enrollment |>
  select(all_of(aa)) |>
  rowSums()

nhpi_enr_tmp <- enrollment |>
  select(all_of(nhpi)) |>
  rowSums()

white_enr_tmp <- enrollment |>
  select(all_of(white)) |>
  rowSums()

black_enr_tmp <- enrollment |>
  select(all_of(black)) |>
  rowSums()

latinx_enr_tmp <- enrollment |>
  select(all_of(latinx)) |>
  rowSums()

swana_enr_tmp <- enrollment |>
  select(all_of(swana)) |>
  rowSums()

anna_enr_tmp <- enrollment |>
  select(all_of(anna)) |>
  rowSums()

single_enr_tmp <- enrollment |>
  select(starts_with("ETH_CD_")) |>
  rowSums()

enrollment$ETH_single <- ifelse(single_enr_tmp == 1, 1, 0)
enrollment$ETH_AA <- ifelse(aa_enr_tmp==0, 0, 1)
enrollment$ETH_NHPI <- ifelse(nhpi_enr_tmp==0, 0, 1)

# new categories
enrollment$ETH_WHITE <- ifelse(white_enr_tmp==0, 0, 1)
enrollment$ETH_BLACK <- ifelse(black_enr_tmp==0, 0, 1)
enrollment$ETH_LATINX <- ifelse(latinx_enr_tmp==0, 0, 1)
enrollment$ETH_SWANA <- ifelse(swana_enr_tmp==0, 0, 1)
enrollment$ETH_ANNA <- ifelse(anna_enr_tmp==0, 0, 1)

enrollment$ETH_RACECOUNT <- enrollment$ETH_AA + enrollment$ETH_NHPI +
  enrollment$ETH_WHITE + enrollment$ETH_BLACK + enrollment$ETH_LATINX +
  enrollment$ETH_SWANA + enrollment$ETH_ANNA

enrollment$ETH_AA_multiethnic <- ifelse(aa_enr_tmp>1, 1, 0)
enrollment$ETH_NHPI_multiethnic <- ifelse(nhpi_enr_tmp>1, 1, 0)
enrollment$ETH_AA_NHPI_multiracial <- ifelse((enrollment$ETH_AA + enrollment$ETH_NHPI == 2) & enrollment$ETH_RACECOUNT==2, 1, 0)
