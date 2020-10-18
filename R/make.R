# Make file for analysis ####
library(targets)
# Run all analysis
tar_make()
# Check for outdated targets
# tar_outdated()
# Visualize status of the project
#tar_glimpse()


tar_load(ends_with("teach"))
tar_load(ends_with("par"))

list(conduct_teach, social_teach, peer_teach,
     conduct_par, social_par, peer_par) %>%
  map(`[[`,1) %>%
  map_dfr(broom.mixed::tidy, .id = "model") %>%
  filter(term == 'ses_sch')


list(conduct_teach, social_teach, peer_teach,
     conduct_par, social_par, peer_par) %>%
  map(`[[`,2) %>%
  map_dfr(broom.mixed::tidy, .id = "model") %>%
  filter(term %in% c('ses_sch', 'ses_sch:ses') ) %>%
  mutate(sig = ifelse(
    sign(conf.low) == sign(conf.high), '*', ''
  ))

plot(ggeffects::ggpredict(conduct_teach$interaction,c('ses_sch', 'ses [-2,0,2]')))

plot(brms::marginal_effects(conduct_teach$interaction,'ses_sch:ses',
                            spaghetti = T, nsamples = 150),
     theme = theme_light() )
