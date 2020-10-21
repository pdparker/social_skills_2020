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
  excluded_home = nrow(child_data %>% filter(is.na(sid)) )
  include = nrow(reduce(data_list,left_join, by = "cid") %>% filter(status < 4,grade == 19,!is.na(sid)))
  
  a1 <- glue::glue("\'Total available participants at age 4 \\n (n = {total})\'")
  b1 <- ''
  c1 <- ''
  d1 <- ''
  e1 <- glue::glue("\'Included for analysis\\n(n = {include})\'")
  f1 <- glue::glue("'Data linked with administrative data \\n (NAPLAN; MySchool)'")
  
  a2 <- ''
  b2 <- glue::glue("\'Excluded: \\nWrong school grade\\n (n = {excluded_grade})\'")
  c2 <- glue::glue("\'Excluded: \\nNot eligable for NAPLAN test\\n (n = {excluded_eligable})\'")
  d2 <- glue::glue("\'Excluded: \\nSchool Uncertain\\n (n = {excluded_home})\'")
  e2 <- ''
  f2 <- ''
  
  mod <- paste("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Times, shape = rectangle, fontsize = 12]        
      tab1 [label = '@@1', width = 2.5, height = 1]
      tab2 [label = '@@2', style = invis, shape = point, width = 0.001, height = 0.001]
      tab3 [label = '@@3', style = invis, shape = point, width = 0.001, height = 0.001]
      tab4 [label = '@@4', style = invis, shape = point, width = 0.001, height = 0.001]
      tab5 [label = '@@5', width = 2.5, height = 1]
      tab6 [label = '@@6', width = 2.5, height = 1]
      tab7 [label = '@@7', style = invis, shape = plaintext, width = 0.001, height = 0.001]
      tab8 [label = '@@8', width = 2.5, height = 1]
      tab9 [label = '@@9', width = 2.5, height = 1]
      tab10 [label = '@@10', width = 2.5, height = 1]
      tab11 [label = '@@11', style = invis, shape = point, width = 0.001, height = 0.001]
      tab12 [label = '@@12', style = invis, shape = point, width = 0.001, height = 0.001]

      # edge definitions with the node IDs
      edge[weight=2]
      tab1 -> tab2 [arrowhead = none, constraint = TRUE];
      tab2 -> tab3[arrowhead = none, constraint = TRUE];
      tab3 -> tab4[arrowhead = none, constraint = TRUE];
      tab4 -> tab5[constraint = TRUE];
      tab5 -> tab6[constraint = TRUE];
      
      tab7 -> tab8 [style = invis, constraint = TRUE];
      tab8 -> tab9 [style = invis, constraint = TRUE];
      tab9 -> tab10 [style = invis, constraint = TRUE];
      tab10 -> tab11 [style = invis, constraint = TRUE];
      tab11 -> tab12 [style = invis, constraint = TRUE];
      edge[weight=1]
      {rank=same; constraint = FALSE; tab2 -> tab8}
      {rank=same; constraint = FALSE; tab3 -> tab9}
      {rank=same; constraint = FALSE; tab4 -> tab10}
}
      
      [1]: ", a1,"
      [2]: ''
      [3]: ''
      [4]: ''
      [5]: ", e1,"
      [6]: ", f1,"
      [7]: ''
      [8]: ", b2,"
      [9]: ", c2,"
      [10]: ",d2,"
      [11]: ''
      [12]: ''
      ")

  DiagrammeR::grViz(mod) %>%
    DiagrammeRsvg::export_svg() %>%
    charToRaw %>%
    rsvg::rsvg_pdf(here::here("fig","flow.pdf"))
  
  return(here::here("fig","flow.pdf"))
  
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


linear_output <- function(model_list = list(conduct_teach, social_teach, peer_teach,
                          conduct_par, social_par, peer_par)) {
  out = model_list %>%
    map(`[[`,'linear') %>%
    map_dfr(broom.mixed::tidy, .id = "model") %>%
    filter(term == 'ses_sch') %>%
    mutate(model = case_when(
      model == 1 ~ "conduct_teacher",
      model == 2 ~ "social_teacher",
      model == 3 ~ "peer_teacher",
      model == 4 ~ "conduct_parent",
      model == 5 ~ "social_parent",
      model == 6 ~ "peer_parent"
    )) %>%
    tidyr::separate(model, into = c('Outcome', 'Report Source')) %>%
    select(Outcome,`Report Source`, Estimate = estimate,
           `-95% CI` = conf.low,`+95% CI` = conf.high)
  
  return(out)
}

interaction_output <- function(model_list = list(conduct_teach, social_teach, peer_teach,
                                            conduct_par, social_par, peer_par)) {
  out = model_list %>%
    map(`[[`,'interaction') %>%
    map_dfr(broom.mixed::tidy, .id = "model") %>%
    filter(term %in% c('ses_sch', 'ses_sch:ses') ) %>%
    mutate(model = case_when(
      model == 1 ~ "conduct_teacher",
      model == 2 ~ "social_teacher",
      model == 3 ~ "peer_teacher",
      model == 4 ~ "conduct_parent",
      model == 5 ~ "social_parent",
      model == 6 ~ "peer_parent"
    )) %>%
    tidyr::separate(model, into = c('Outcome', 'Report Source')) %>%
    select(Outcome,`Report Source`, Estimate = estimate,
           `-95% CI` = conf.low,`+95% CI` = conf.high)
  
  return(out)
}

plots <- function(model=conduct_teach){
  
  nm <- deparse(substitute(model)) %>%
    str_split(., "_",simplify = TRUE) %>% 
    str_to_sentence()
  
  source <- ifelse(nm[2] == "teach", "Teacher", "Parent")
  outcome <- case_when(
    nm[1] == 'Conduct' ~ "Conduct Problems",
    nm[1] == 'Social' ~ "Prosociality",
    nm[1] == 'Peer' ~ "Peer Problems",
  )
  
  conditions <- data.frame(ses = c(-2, 0, 2))
  
  p = plot(brms::conditional_effects(model$interaction,'ses_sch:ses',
                                 spaghetti = T, nsamples = 500, 
                                 int_conditions = conditions))[[1]] +
    labs(#title = glue::glue('{source} Reported {outcome}'),
         #subtitle = "<span style='color:red'>High (+2 SD)</span>, <span style='color:green'>Average</span>, and <span style='color:blue'>Low (-2 SD)</span> SES Children",
         color = "SES (Units: SD)",
         y = "Outcome (Units: censorded 0-10)",
         x = "School Average SES (Units: SD)") +
    tidyMB::theme_mb() +
    theme(
      legend.key = element_blank(),
      plot.subtitle = element_markdown(),
      legend.position = "bottom",
      legend.background = element_rect(fill="white", color = "white")
    ) 
  
  ggsave(plot = p, filename = here::here("fig", glue::glue("interaction_{nm[1]}_{nm[2]}.png")),
         dpi = 300, width = 6, height = 4)
  # 
  # return(here::here("fig", glue::glue("interaction_{nm[1]}_{nm[2]}.png")))
  # 
  return(p)
}


full_model_output <- function(model = conduct_teach) {
  
  nm <- deparse(substitute(model)) %>%
    str_split(., "_",simplify = TRUE) %>% 
    str_to_sentence()
  
  source <- ifelse(nm[2] == "teach", "Teacher", "Parent")
  outcome <- case_when(
    nm[1] == 'Conduct' ~ "Conduct Problems",
    nm[1] == 'Social' ~ "Prosociality",
    nm[1] == 'Peer' ~ "Peer Problems",
  )
  
  out = model %>%
    map_dfr(broom.mixed::tidy, .id = "model") %>%
    mutate(term = case_when(
      term == '(Intercept)' ~ "Intercept (Units: Censored 0-10)",
      term == 'ses_sch' ~ "School Average SES (Units: SD)",
      term == 'ses' ~ "SES (Units: SD)",
      term == 'prior' ~ glue::glue("Prior {outcome} (Units: 0-10)") %>% as.character(),
      term == 'cohortK' ~ "Cohort K",
      term == 'gendergirl' ~ "Girl",
      term == 'geourban' ~ "Urban",
      term == 'ach' ~ "Academic Achievement (Units: SD)",
      term == 'sectorNonMGovernment' ~ "Non-government School",
      term == 'sd__(Intercept)' ~ "Random Intercept (Units: SD)",
      term == 'sch_ses:ses' ~ "School Average SES by Student SES"
    )) %>%
    select(Predictor = term, Estimate = estimate,
           `-95% CI` = conf.low,`+95% CI` = conf.high) 
  
  return(out)
}

