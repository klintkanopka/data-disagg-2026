# preproc-admissions.R

admissions <- read_excel('data/updated/Admissions Data v02 112823.xlsx',
                         col_types=rep('text', 75)) |>
  select(hash_id, term_cd, admit, reg, starts_with('ETH_')) |>
  mutate(term_cd = as.numeric(str_sub(term_cd, 1, 2))) |>
  mutate(across(starts_with("ETH_"), ~if_else(is.na(.x), 0, 1))) |>
  mutate(admit = if_else(admit=='Y', 1, 0),
         reg = if_else(reg=='Y', 1, 0))

aa_adm_tmp <- admissions |>
  select(all_of(aa)) |>
  rowSums()

nhpi_adm_tmp <- admissions |>
  select(all_of(nhpi)) |>
  rowSums()

white_adm_tmp <- admissions |>
  select(all_of(white)) |>
  rowSums()

black_adm_tmp <- admissions |>
  select(all_of(black)) |>
  rowSums()

latinx_adm_tmp <- admissions |>
  select(all_of(latinx)) |>
  rowSums()

swana_adm_tmp <- admissions |>
  select(all_of(swana)) |>
  rowSums()

anna_adm_tmp <- admissions |>
  select(all_of(anna)) |>
  rowSums()

single_adm_tmp <- admissions |>
  select(starts_with("ETH_CD_")) |>
  rowSums()

admissions$ETH_single <- ifelse(single_adm_tmp == 1, 1, 0)
admissions$ETH_AA <- ifelse(aa_adm_tmp==0, 0, 1)
admissions$ETH_NHPI <- ifelse(nhpi_adm_tmp==0, 0, 1)

# new categories
admissions$ETH_WHITE <- ifelse(white_adm_tmp==0, 0, 1)
admissions$ETH_BLACK <- ifelse(black_adm_tmp==0, 0, 1)
admissions$ETH_LATINX <- ifelse(latinx_adm_tmp==0, 0, 1)
admissions$ETH_SWANA <- ifelse(swana_adm_tmp==0, 0, 1)
admissions$ETH_ANNA <- ifelse(anna_adm_tmp==0, 0, 1)

admissions$ETH_RACECOUNT <- admissions$ETH_AA + admissions$ETH_NHPI +
  admissions$ETH_WHITE + admissions$ETH_BLACK + admissions$ETH_LATINX +
  admissions$ETH_SWANA + admissions$ETH_ANNA

admissions$ETH_AA_multiethnic <- ifelse(aa_adm_tmp>1, 1, 0)
admissions$ETH_NHPI_multiethnic <- ifelse(nhpi_adm_tmp>1, 1, 0)
admissions$ETH_AA_NHPI_multiracial <- ifelse((admissions$ETH_AA + admissions$ETH_NHPI == 2) & admissions$ETH_RACECOUNT==2, 1, 0)
