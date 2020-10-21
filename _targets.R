library(targets)

source(here::here("R","functions.R"))
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("readit", "tidyverse", "dataMaid","here", "glue", "brms",
                            "DiagrammeR","DiagrammeR", "rsvg", "ggtext", "tarchetypes")
               )

tar_pipeline(
  tar_target(
    age_4_data,
    age_4_data()
  ) ,
  tar_target(
    age_8_data,
    age_8_data()
  ),
  tar_target(
    child_achievement_data,
    child_achievement_data()
  ),
  tar_target(
    school_achievement_data,
    school_achievement_data(data = age_4_data)
  ),
  tar_target(
    data,
    data(data_list = list(age_4_data, age_8_data, child_achievement_data, school_achievement_data))
  ),
  tar_target(
    codebook,
    make_codebook(data),
    format = "file"
  ),
  tar_target(
    flow,
    consort_flow(data_list = list(age_4_data, age_8_data, child_achievement_data, school_achievement_data)),
    format = "file"
  ),
  tar_target(
    data_imp,
    data_imp(data)
  ),
  tar_target(
    peer_par,
    models(data_imp, outcome = "peer", source = "par")
  ),
  tar_target(
    conduct_par,
    models(data_imp, outcome = "conduct", source = "par")
  ),
  tar_target(
    social_par,
    models(data_imp, outcome = "social", source = "par")
  ),
  tar_target(
    peer_teach,
    models(data_imp, outcome = "peer", source = "teach")
  ),
  tar_target(
    conduct_teach,
    models(data_imp, outcome = "conduct", source = "teach")
  ),
  tar_target(
    social_teach,
    models(data_imp, outcome = "social", source = "teach")
  ),
  tar_target(
    linear_table,
    linear_output(list(conduct_teach, social_teach, peer_teach,
                       conduct_par, social_par, peer_par))
  ),
  tar_target(
    interaction_table,
    interaction_output(list(conduct_teach, social_teach, peer_teach,
                       conduct_par, social_par, peer_par))
  ),
  tar_target(
    peer_par_plot,
    plots(peer_par)
  ),
  tar_target(
    conduct_par_plot,
    plots(conduct_par)
  ),
  tar_target(
    social_par_plot,
    plots(social_par)
  ),
  tar_target(
    peer_teach_plot,
    plots(peer_teach)
  ),
  tar_target(
    conduct_teach_plot,
    plots(conduct_teach)
  ),
  tar_target(
    social_teach_plot,
    plots(social_teach)
  ),
  tarchetypes::tar_render(manuscript,"manuscript.Rmd")
)
