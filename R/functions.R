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
           iq = cppvt, indig1 =zf12m2, indig2 = zf12cm, gender = zf02m1, lang = cf11m2
           ) %>%
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
           iq = cppvt
           ) %>%
    mutate(geo = ifelse(geo < 1, 'urban', 'rural'),
           cohort = 'B'
           )
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
    lang = ifelse(lang == '1201', 'eng', 'other')
    )
  #Merge Data
  age_4_b <- left_join(age_4_b, age_0_b)
  age_4_data <- bind_rows(age_4_k, age_4_b)
  # Return data
  return(age_4_data)
}


#' Extract and Merge Age 8 data
#'
#' @return data
#' @export
age_8_data <- function(){
  # Age 8 data Kindergarten cohort
  age_8_k <- readit::readit(here::here("data","lsacgrk8.sas7bdat")) %>%
    select(cid = hicid, grade = epc06a1,state = estate,weight = eweight,
           stratum = stratum, par_sdq = easdqtb, par_social = eapsoc, par_peer = eapeer,
           par_conduct = eacondb,teach_sdq = etsdqtb,teach_social = etpsoc, 
           teach_peer = etpeer, teach_conduct = etcondb
           ) %>%
    mutate(cohort = 'K')
  # Age 8 data birth cohort
  age_8_b <- readit::readit(here::here("data","lsacgrb8.sas7bdat")) %>%
    select(cid = hicid, grade = epc06a1,state = estate,weight = eweight,
           stratum = stratum, par_sdq = easdqtb, par_social = eapsoc, 
           par_peer = eapeer, par_conduct = eacondb,
           teach_sdq = etsdqtb,teach_social = etpsoc, teach_peer = etpeer, teach_conduct = etcondb
           ) %>%
    mutate(cohort = 'B')
  # Merge data and return
  age_8_data <- bind_rows(age_8_k, age_8_b)
  return(age_8_data)
}


#' Extract NAPLAN Data
#'
#' @return data
#' @export
#'
child_achievement_data <- function(){
  child_achievement_data <- readit::readit(here::here("data","lsacnaplan.sas7bdat")) %>%
    select(cid = hicid, numeracy = y3num, read = y3read, writing = y3write, spelling = y3spel,
           grammar = y3gram,status = y3status
           ) %>%
    mutate(across(numeracy:grammar,
                  ~replace(., . < 0, NA)
                  )
           )
  
  return(child_achievement_data)
}

#' Extract NAPLAN School Data
#'
#' @param data age 4 data for ids 
#'
#' @return data
#' @export
#'
school_achievement_data <- function(data = age_4_data){
  # Read in school data
  tmp = readit::readit(here::here("data","lsac_myschool_gr.sas7bdat"))
  # Get required ids
  ids = data %>%
    select(cid, cohort) %>%
    group_split(cohort) %>%
    map(~pull(.,cid))
  # Extract school data for cohort K
  school_data_k <- tmp %>%
    filter(calendar_year == 2008 & HICID %in% ids[[2]]) %>%
    select(cid = HICID, sid = School_ID, sector = School_Sector_Code, ses_sch = School_ICSEA) %>%
    mutate(cohort = 'K')
  # Extract school data for cohort K
  sData2008B <- tmp %>%
    filter(calendar_year == 2010 & HICID %in% ids[[1]]) %>%
    select(cid = HICID, sid = School_ID, sector = School_Sector_Code, ses_sch = School_ICSEA)%>%
    mutate(cohort = 'B')
  # Merge and return
  school_achievement_data <- bind_rows(sData2008K, sData2008B)
  return(school_achievement_data)
}
