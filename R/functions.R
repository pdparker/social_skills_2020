# None Target Functions ####
z <- function(x) (x-mean(x,na.rm=TRUE))/sd(x, na.rm = TRUE)

#Taget functions ####
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
  age_4_data <- bind_rows(age_4_k, age_4_b) %>%
    select(-indig1, -indig2)
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
           ) 
  # Age 8 data birth cohort
  age_8_b <- readit::readit(here::here("data","lsacgrb8.sas7bdat")) %>%
    select(cid = hicid, grade = epc06a1,state = estate,weight = eweight,
           stratum = stratum, par_sdq = easdqtb, par_social = eapsoc, 
           par_peer = eapeer, par_conduct = eacondb,
           teach_sdq = etsdqtb,teach_social = etpsoc, teach_peer = etpeer, teach_conduct = etcondb
           )
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
    select(cid = hicid, ach_numeracy = y3num, ach_read = y3read, ach_writing = y3write, ach_spelling = y3spel,
           ach_grammar = y3gram,status = y3status
           ) %>%
    mutate(across(ach_numeracy:ach_grammar,
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
    select(cid = HICID, sid = School_ID, sector = School_Sector_Code, ses_sch = School_ICSEA) 
  # Extract school data for cohort b
  school_data_b <- tmp %>%
    filter(calendar_year == 2010 & HICID %in% ids[[1]]) %>%
    select(cid = HICID, sid = School_ID, sector = School_Sector_Code, ses_sch = School_ICSEA)
  # Merge and return
  school_achievement_data <- bind_rows(school_data_k, school_data_b)
  return(school_achievement_data)
}


data <- function(data_list = list()){
  child_data = reduce(data_list,left_join, by = "cid") %>%
    mutate(
      ses_sch = z(ses_sch),
      ach = psych::principal(.[, str_detect(names(.), "^ach_")])$score %>% as.vector
    ) %>%
    filter(status < 4,
           grade == 19,
           !is.na(sid)) %>%
    select(-starts_with("ach_"),
           -ends_with("sdq"),
           -grade,
           -status) %>%
    relocate(starts_with("prior"),.before = "par_social")
  return(child_data)
}

make_codebook <- function(data){
  dataMaid::makeCodebook(data,
                         file = here::here("data",
                                     glue::glue("{Sys.Date()}_codebook.Rmd")),
                         replace=TRUE)
  return(here::here("data",glue::glue("{Sys.Date()}_codebook.pdf")))
}

consort_flow <- function(data_list = list(age_4_data, age_8_data, child_achievement_data, school_achievement_data)) {
  child_data = reduce(data_list,left_join, by = "cid")
  
  total = nrow(child_data)
  excluded_grade = nrow(child_data %>% filter(grade != 19) )
  excluded_eligable = nrow(child_data %>% filter(status > 3) )
  excluded_home = nrow(child_data %>% filter(!is.na(sid)) )
  include = nrow(reduce(data_list,left_join, by = "cid") %>% filter(status < 4,grade == 19,!is.na(sid)))
  
  a1 <- glue::glue('Total available participants at age 4\n(n = {total})')
  b1 <- ''
  c1 <- ''
  d1 <- ''
  e1 <- glue::glue('Included for analysis\n(n = {include})')
  f1 <- 'Data linked with\nadministrative data (NAPLAN; MySchool)'
  
  a2 <- ''
  b2 <- glue::glue('Excluded because of\nwrong school grade (n = {excluded_grade})')
  c2 <- glue::glue('Excluded because of\nnot eligable for NAPLAN test (n = {excluded_eligable})')
  d2 <- glue::glue('Excluded because of\nout of school (n = {excluded_home})')
  e2 <- ''
  f2 <- ''
  
  #Create a node dataframe
  ndf <- create_node_df(
    n = 12,
    label = c(a1, b1, c1, d1, e1, f1, #Column 1
              a2, b2, c2, d2, e2, f2), #Column 2
    style = rep('solid',12),
    shape = rep('box',12),
    # style = c('solid', 'invis', 'invis','invis', 'solid', 'solid', #Column 1
    #           'invis', 'solid', 'solid', 'solid','invis', 'invis'), #Column 2
    # shape = c('box', 'point', 'point', 'point','box','box', #Column 1 
    #           'plaintext', 'box', 'box','box', 'point', 'point'), #Column 2
    # width = c(3, 0.001, 0.001, 0.001,3, 3, #Column 1
    #           2.5, 2.5, 2.5, 2.5, 0.001, 0.001), #Column 2
    # height = c(1, 0.001, 0.001, 0.001, 1, 1, #Column 1
    #            1, 1, 1,1, 0.001, 0.001), #Column 2
    fontsize = c(rep(10, 12)),
    fontname = c(rep('Times New Roman', 12)),
    #penwidth = 1.5,
    fixedsize = 'true')
  
  #Create an edge dataframe
  edf <- create_edge_df(
    from = c(1, 2, 3, 4, 5, #Column 1
             7, 8, 9, 10, 11, #Column 2
             2, 3, 4 #Horizontals
    ),
    to = c(2, 3, 4, 5, 6, #Column 1
           8, 9, 10, 11, 12,#Column 2
           7, 8, 9 #Horizontals
    ),
    arrowhead = c('none', 'none','none', 'normal', 'normal', #Column 1
                  'none', 'none', 'none', 'none','none', #Column 2
                  'normal', 'normal', 'normal' #Horizontals
    ),
    color = c('black', 'black', 'black', 'black', 'black', #Column 1
              '#00000000', '#00000000', '#00000000', '#00000000', '#00000000',#Column 2
              'black', 'black', 'black' #Horizontals
    ),
    constraint = c(rep('true', 10), #Columns
                   rep('false', 3) #Horizontals
    )
  )
  
  g <- create_graph(ndf,
                    edf,
                    attr_theme = NULL)
  #render_graph(g)
  return(g)
}


data_imp <- function(data){
  set.seed(1234)
  bound <- tibble(var = which(str_detect(names(data),"prior|teach|par_.*")),
                  left = 0, right = 10) %>%
    as.matrix()
  data_imp <- Amelia::amelia(as.data.frame(data), m = 5,
                           idvars = c('cid','weight','sid','stratum','state','sector'),
                           noms = c('geo', 'indig', 'gender', 'lang', 'cohort'),bounds = bound
  )
  return(data_imp)
}

models <- function(d = data_imp, outcome = c("peer","social","conduct"), source = c("par","teach"), m = 5,
                   delta = 0.95, iterations = 4000){
  # data preparation: add censoring#
  data = list()
  for (i in 1:m){
    tmp = data.frame(d$imputations[[i]])
    data[[i]] = tmp %>%
      select(
        outcome = glue::glue("{source}_{outcome}"),
        prior = glue::glue("prior_{outcome}"),
        ses_sch, ses, cohort,gender,geo,
        ach,sector, sid
      ) %>%
      mutate(
        censored = case_when(
          outcome == 0 ~ "left",
          outcome == 10 ~ "right",
          TRUE ~ 'none'
        )
      )
    # Run model
    m1 = brms::brm_multiple(outcome|cens(censored)~ses_sch + ses + prior + cohort + gender + geo + 
                               ach + sector +  (1|sid), data = data, cores =4,
                            control = list(adapt_delta = delta), iter = iterations)
    m2 = brms::brm_multiple(outcome|cens(censored)~ses_sch*ses + prior + cohort + gender + geo + 
                               ach + sector +  (1|sid), data = data, cores =4,
                             control = list(adapt_delta = delta), iter = iterations)
    return(
      list(linear = m1, interaction = m2)
      )
  }
  
  
  
}
