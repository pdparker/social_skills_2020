#' Extract and Merge Age 4 data
#'
#' @return data
#' @export
#'
age_4_data <- function(){
  #Age 4 data Kindergarten Cohort
  age_4_k  <- readit::readit(here::here("data","lsacgrk4.sas7bdat")) %>%
    select(cid = hicid, ses = csep, geo = csos, prior_sdq = casdqta,
           prior_social = capsoc, prior_peer = capeer, prior_conduct = caconda,
           iq = cppvt, indig1 =zf12m2, indig2 = zf12cm, gender = zf02m1, lang = cf11m2) %>%
    mutate(geo = ifelse(geo < 1, 'urban', 'rural'),
           indig = case_when(
             indig1 < 0 ~ NA_character_,
             indig2 < 0 ~ NA_character_,
             indig1 > 1 ~ 'indig',
             indig2 > 1 ~ 'indig',
             TRUE ~ 'nonIndig'
           ) %>% factor,
           cohort = 'K',
           gender = ifelse(gender == 1, "boy", "girl"),
           lang = ifelse(lang == '1201', 'eng', 'other')
    )
  #Age 4 data Birth Cohort
  age_4_b <- readit::readit(here::here("data","lsacgrb4.sas7bdat")) %>%
    select(cid = hicid, ses = csep, geo = csos, prior_sdq = casdqta,
           prior_social = capsoc, prior_peer = capeer, prior_conduct = caconda,
           iq = cppvt) %>%
    mutate(geo = ifelse(geo < 1, 'urban', 'rural'),
           cohort = 'B')
  #Age 0 Birth cohort only
  age_0_b <- readit::readit(here::here("data","lsacgrb0.sas7bdat")) %>%
    select(cid = hicid, indig1 =zf12m2, indig2 = zf12m3, gender = zf02m1, lang = af11m2) %>%
    mutate(indig = case_when(
      indig1 < 0 ~ NA_character_,
      indig2 < 0 ~ NA_character_,
      indig1 > 1 ~ 'indig',
      indig2 > 1 ~ 'indig',
      TRUE ~ 'nonIndig'
    ) %>% factor(),
    gender = ifelse(gender == 1, "boy", "girl"),
    lang = ifelse(lang == '1201', 'eng', 'other'))
  #Merge Data
  age_4_b <- right_join(age_4_b, age_0_b)
  age_4_data <- bind_rows(age_4_k, age_4_b)
  # Return data
  return(age_4_data)
}
