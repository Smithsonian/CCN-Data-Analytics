# Import packages
library(tidyverse)
library(rjags)
library(ggmcmc)
library(lme4)
library(MuMIn)
library(rjags)
library(RColorBrewer)

# Import and clean dataset
# Data
cores <- read_csv("data/CCN_cores.csv")

methods <- read_csv("data/CCN_methods.csv")

depths <- read_csv("data/CCN_depthseries.csv")
depths <- read_csv("data/CCN_depthseries.csv", 
                   guess_max = nrow(depths)) %>% 
  left_join(cores) %>% 
  left_join(methods)

ooyang <- read_csv("data/ooyang_and_lee_2017/41467_2019_14120_MOESM7_ESM.csv") %>% 
  filter(`Study ID` == "This study") %>% 
  mutate(study_id = "Ooyang_et_al_2017",
         fraction_organic_matter = LOI/100,
         fraction_carbon = OC / 100,
         habitat = "mangrove",
         carbonate_removal_method = "total carbon difference after LOI") %>% 
  select(study_id, fraction_organic_matter, fraction_carbon, habitat, carbonate_removal_method)

bibs <- read_csv("data/CCN_study_citations.csv")

unique(depths$habitat)

depths <- depths %>% 
  filter(complete.cases(habitat))

depths$habitat[grepl("scrub shrub", depths$habitat)] <- "scrub/shrub"

unique(depths$habitat)
unique(depths$carbonate_removal_method)

carbonate_removal_methods_tab <- depths %>% 
  filter(complete.cases(fraction_carbon, fraction_organic_matter),
         habitat %in% c("marsh", "mangrove")) %>%
  select(study_id, carbonate_removal_method) %>% 
  distinct() %>% 
  arrange(study_id)
(carbonate_removal_methods_tab)

# which ones have no indication of carbonate removal
studies_to_keep <- carbonate_removal_methods_tab %>% 
  filter(complete.cases(carbonate_removal_method),
         ! carbonate_removal_method %in% c("none specified", "not specified", "carbonates not removed"))

depths_om_oc <- depths %>% 
  filter(complete.cases(fraction_carbon, fraction_organic_matter),
        ! habitat %in% c("unvegetated"),
        study_id %in% studies_to_keep$study_id
        #habitat %in% c("marsh", "mangrove")
         ) %>% 
  #filter(! study_id %in% c("Ceron-Breton_et_al_2011", "Drexler_et_al_2009",
  #                         "Keshta_et_al_2020", "Rodriguez_et_al_2022")) %>% 
  bind_rows(ooyang) %>% 
  mutate(fom2 = fraction_organic_matter^2) %>% 
  filter(fraction_carbon >= 0 & fraction_organic_matter >= 0 & fraction_carbon <= 1 & fraction_organic_matter<=1)

  # filter(carbonate_removal_method %in% c("direct acid treatment", "low carbonate soil", "acid fumigation"))

ggplot(depths_om_oc %>% filter(habitat == "seagrass"), aes(y = fraction_carbon, x = fraction_organic_matter)) +
  geom_point(aes(color = study_id))

ggplot(depths_om_oc %>% filter(habitat == "swamp"), aes(y = fraction_carbon, x = fraction_organic_matter)) +
  geom_point(aes(color = study_id))

nrow(depths_om_oc)

studies_w_more_than_one_method <- depths_om_oc %>% 
  select(study_id, habitat) %>% 
  distinct() %>% 
  group_by(study_id) %>% 
  summarise(n_habitats = n()) %>% 
  filter(n_habitats > 1)
(studies_w_more_than_one_method)

cores_and_sites <- depths_om_oc %>% 
  select(study_id, site_id, core_id) %>% 
  distinct()

nrow(cores_and_sites)

sites <- depths_om_oc %>% 
  select(study_id, site_id) %>% 
  distinct()

nrow(sites)

length(unique(depths_om_oc$study_id))

bib_ids <- bibs %>% 
  filter(study_id %in% depths_om_oc$study_id) %>% 
  select(-study_id) %>% 
  distinct()

write_csv(bib_ids, "study_bib_info.csv")

length(unique(bib_ids$bibliography_id))

carbonate_removal_methods_summary <- depths_om_oc %>% 
  filter(complete.cases(fraction_carbon, fraction_organic_matter),
         habitat %in% c("marsh", "mangrove")) %>%
  group_by(carbonate_removal_method) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(-n)

carbonate_removal_methods_summary$pct <- carbonate_removal_methods_summary$n / sum(carbonate_removal_methods_summary$n) * 100

(carbonate_removal_methods_summary)

write_csv(carbonate_removal_methods_summary, "OM and C Relationship/tabs/Carbonate_Removal_Summary.csv")

depths_om_oc2 <- depths_om_oc %>% 
  filter(study_id %in% studies_w_more_than_one_method$study_id)

# Where do we get studies with multiple systems
ggplot(depths_om_oc2, aes(x = fraction_organic_matter, y = fraction_carbon)) +
  geom_point(aes(color = habitat)) +
  theme_minimal() +
  facet_wrap(.~study_id) +
  xlab("fraction organic matter") +
  ylab("fraction carbon") +
  theme(axis.text.x = element_text (angle = 45, vjust = 1, hjust=1)) +
  scale_x_log10() +
  scale_y_log10() +
  geom_quantile()

# Where do we get studies with multiple systems
ggplot(depths_om_oc2, aes(x = fraction_organic_matter, y = fraction_carbon)) +
  geom_point(aes(shape = habitat)) +
  theme_minimal() +
  xlab("fraction organic matter") +
  ylab("fraction carbon") +
  theme(axis.text.x = element_text (angle = 45, vjust = 1, hjust=1)) +
  scale_x_log10() +
  scale_y_log10() +
  geom_quantile(aes(color = study_id))

habitat_sum <- depths_om_oc %>% 
  group_by(habitat) %>% 
  summarise(n = n()) %>% 
  arrange(-n)
(habitat_sum)

write_csv(habitat_sum, "OM and C Relationship/tabs/Habitat_Summary.csv")

# Jaggify dataset
depths_om_oc_jaggified <- depths_om_oc %>% 
  select(study_id, habitat, fraction_organic_matter, fraction_carbon) %>% 
  mutate(study_id = as.integer(factor(study_id)),
         habitat = as.integer(factor(habitat, levels = c("marsh", "mangrove", "seagrass", "scrub/shrub", "swamp", "mudflat")))) %>% 
  mutate(i = 1:n())

om_oc_model <- "
model{

  # Hyper priors
  beta1_mu_global ~ dnorm(0.4, 1/0.4)
  beta1_tau_global ~ dgamma(0.001, 0.001)
  
  beta2_mu_global ~ dnorm(0.0025, 1/0.0025)
  beta2_tau_global ~ dgamma(0.001, 0.001)
  
  # Priors
  
  # Habitat specific regressions
  #beta1[1] ~ dnorm(0.4, 10000)
  #beta2[1] ~ dnorm(0.0025, 11111111)
  
  for (k in 1:K) {
    beta1[k] ~ dnorm(beta1_mu_global, beta1_tau_global)
    beta2[k] ~ dnorm(beta2_mu_global, beta2_tau_global)
  }
  
  # Study specific biases
  tau_study ~ dgamma(0.001,0.001)
  for (j in 1:J) {
    study_bias[j] ~ dnorm(0, tau_study)  
  }
  
  # Random error
  tau_random ~ dgamma(0.001, 0.001) 

  # Process model
  for (i in 1:n) {
    z_oc[i] <-  fraction_organic_matter[i]*beta1[habitat[i]] + fraction_organic_matter[i]*fraction_organic_matter[i]*beta2[habitat[i]]
    fraction_carbon[i] ~ dnorm(z_oc[i]+study_bias[study_id[i]], tau_random)
  }
  

}

"

data = list(n = max(depths_om_oc_jaggified$i),
            habitat = depths_om_oc_jaggified$habitat,
            K = max(depths_om_oc_jaggified$habitat),
            study_id = depths_om_oc_jaggified$study_id,
            J = max(depths_om_oc_jaggified$study_id),
            fraction_organic_matter = depths_om_oc_jaggified$fraction_organic_matter,
            fraction_carbon = depths_om_oc_jaggified$fraction_carbon)

j.model   <- jags.model(file = textConnection(om_oc_model),
                        data = data,
                        n.chains = 4)

var.out   <- coda.samples(model = j.model,
                          variable.names = c("beta1",
                                             "beta2",
                                             "tau_study",
                                             "tau_random",
                                             "beta1_mu_global",
                                             "beta2_mu_global",
                                             "beta1_tau_global",
                                             "beta2_tau_global"
                          ),
                          n.iter = 2000)

library(ggmcmc)

tidy_jags <- ggs(var.out)

ggs_traceplot(tidy_jags)
ggs_density(tidy_jags)

tidy_sum <- tidy_jags %>% 
  group_by(Parameter) %>% 
  filter(Iteration > 1000) %>% 
  summarise(mean = mean(value),
            se = sd(value),
            upper_CI = quantile(value, 0.975),
            lower_CI= quantile(value, 0.025))

(tidy_sum)

# Just get the model params 
param_iterations <- tidy_jags %>% 
  filter(grepl("beta", Parameter) & ! grepl("tau", Parameter)) %>% 
  # Separate into habitat code and beta
  separate(Parameter, into = c("Parameter", "habitat_code"), sep = "\\[") %>% 
  mutate(Parameter = str_remove_all(Parameter, "_mu_global"),
    habitat_code = as.numeric(str_remove_all(habitat_code, "\\]")),
         habitat_code = ifelse(is.na(habitat_code), 7, habitat_code)) %>% 
  spread(key = Parameter, value = value)

# Pivot
habitat_definiations <- data.frame(habitat_code = 1:7,
                                   habitat = c("marsh", "mangrove", "seagrass", "scrub/shrub", "swamp", "mudflat", "global"))


tidy_sum_output <- tidy_sum %>% 
  separate(Parameter, into = c("Parameter", "habitat_code"), sep = "\\[") %>% 
  mutate(habitat_code = as.numeric(str_remove_all(habitat_code, "\\]"))) %>% 
  left_join(habitat_definiations) %>% 
  arrange(habitat, Parameter)


write_csv(tidy_sum_output, "OM and C Relationship/tabs/Parameter_Summary_Table.csv")


target_predictions <- expand.grid(Iteration = 1000:2000,
                                  Chain = 1:4,
                                  habitat_code = 1:6,
                                  fraction_organic_matter = seq(0,1, by=0.01)) %>% 
  left_join(param_iterations) %>% 
  left_join(habitat_definiations) %>% 
  mutate(fraction_carbon = fraction_organic_matter*beta1 + fraction_organic_matter^2*beta2)


prediction_summaries <- target_predictions %>% 
  group_by(habitat, fraction_organic_matter) %>% 
  summarise(median = median(fraction_carbon),
            upper_ci = quantile(fraction_carbon, 0.975),
            lower_ci = quantile(fraction_carbon, 0.025))
prediction_summaries[prediction_summaries<0] <- 0

library(grafify)

ggplot(data = depths_om_oc, aes(x = fraction_organic_matter, y = fraction_carbon)) +
  geom_point(color="black", alpha=0.33) +
  theme_minimal() +
  facet_wrap(.~habitat) +
  geom_line(data = prediction_summaries, aes(y = median, color = habitat), lty=1) +
  geom_ribbon(data = prediction_summaries, aes(x = fraction_organic_matter,
                                               y = median,
                                               ymin = lower_ci,
                                               ymax = upper_ci, fill = habitat, color = habitat), alpha = 0.33) +
  xlab("fraction organic matter") +
  ylab("fraction carbon") +
  scale_color_grafify() +
  scale_fill_grafify() +
  theme(legend.position = "none")

ggsave("OM and C Relationship/figs/All_habitats_om_oc.jpg", width = 180, height =110, units = "mm")

ggplot(data = depths_om_oc, aes(x = fraction_organic_matter, y = fraction_carbon)) +
  # geom_point(color="black", alpha=0.33) +
  theme_minimal() +
  # facet_wrap(.~habitat) +
  geom_line(data = prediction_summaries, aes(y = median, color = habitat), lty=1) +
  # geom_ribbon(data = prediction_summaries, aes(x = fraction_organic_matter,
  #                                              y = median,
  #                                              ymin = lower_ci,
  #                                              ymax = upper_ci, color = habitat), alpha = 0.33, fill = NA) +
  xlab("fraction organic matter") +
  ylab("fraction carbon") +
  scale_color_grafify() +
  scale_fill_grafify()
ggsave("OM and C Relationship/figs/Median_Comparison.jpg", width = 90, height =60, units = "mm")


paramVis <- tidy_sum %>% 
  filter(grepl("beta", Parameter)&!grepl("tau", Parameter)) %>% 
  # Separate into habitat code and beta
  separate(Parameter, into = c("Parameter", "habitat_code"), sep = "\\[") %>% 
  mutate(habitat_code = as.numeric(str_remove_all(habitat_code, "\\]")),
         habitat_code = ifelse(is.na(habitat_code), 7, habitat_code),
         Parameter = str_remove_all(Parameter, "_mu_global")) %>% 
  left_join(habitat_definiations) %>% 
  mutate(habitat = factor(habitat, levels = c("mangrove", "marsh", "mudflat", "scrub/shrub", "seagrass", "swamp", "global"))) %>% 
  mutate(Parameter = recode(Parameter, "beta1"="slope param. 1", "beta2"="slope param. 2"))

ggplot(paramVis, aes(x = habitat, y = mean, color = habitat)) +
  geom_point() +
  geom_segment(aes(y = lower_CI, yend=upper_CI)) +
  facet_wrap(.~Parameter, scale = "free") +
  ylab("Parameter Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position = "none")  +
  scale_color_grafify() 

ggsave("OM and C Relationship/figs/Parameter_comparison.jpg", width = 180, height =90, units = "mm")

library(corrplot)
tidy_jags_pivot <- tidy_jags %>% 
  mutate(Parameter = recode(Parameter, 
                            "beta1[1]" = ":beta[list(1,mangrove)]",
                            "beta1[2]" = ":beta[list(1,marsh)]",
                            "beta1[3]" = ":beta[list(1,mudflat)]",
                            "beta1[4]" = ":beta[list(1,shrub)]",
                            "beta1[5]" = ":beta[list(1,seagrass)]",
                            "beta1[6]" = ":beta[list(1,swamp)]",
                            "beta2[1]" = ":beta[list(2,mangrove)]",
                            "beta2[2]" = ":beta[list(2,marsh)]",
                            "beta2[3]" = ":beta[list(2,mudflat)]",
                            "beta2[4]" = ":beta[list(2,shrub)]",
                            "beta2[5]" = ":beta[list(2,seagrass)]",
                            "beta2[6]" = ":beta[list(2,swamp)]",
                            "beta1_mu_global" = ":mu[list(hab,1)]",
                            "beta1_tau_global" = ":tau[list(hab,1)]",
                            "beta2_mu_global" = ":mu[list(hab,2)]",
                            "beta2_tau_global" = ":tau[list(hab,2)]",
                            "tau_random" = ":tau[sample]",
                            "tau_study"=":tau[study]"
                            )) %>% 
  spread(key = Parameter, value = value) %>% 
  select(-c(Iteration, Chain))

corr_tab <- cor(tidy_jags_pivot, method = "spearman")
write_csv(as.data.frame(round(corr_tab, 2)), "OM and C Relationship/tabs/Correlation_Table.csv")

corr <- round(corr_tab, 1) # rounded to one decimal point

jpeg(filename = "OM and C Relationship/figs/Parameter_Correlation_plot.jpg", width = 130, height = 130, bg = "white", units = "mm", res = 380)
corrplot(corr, method = "circle", type = "upper", diag = F,  tl.col="black", addCoef.col = "black") +
  scale_x_discrete(scales::label_parse)

dev.off()
  

# Significance analysis 
sig_analysis1 <- tidy_jags %>% 
  filter(grepl("beta", Parameter) & ! grepl("tau|mu", Parameter)) %>% 
  # Separate into habitat code and beta
  separate(Parameter, into = c("Parameter", "habitat_code"), sep = "\\[") %>% 
  mutate(Parameter = str_remove_all(Parameter, "_mu_global"),
         habitat_code = as.numeric(str_remove_all(habitat_code, "\\]")),
         habitat_code = ifelse(is.na(habitat_code), 7, habitat_code)) %>% 
  left_join(habitat_definiations)

sig_analysis2 <- tidy_jags %>% 
  filter(grepl("mu", Parameter)) %>% 
  # Separate into habitat code and beta
  mutate(Parameter = str_remove_all(Parameter, "_mu_global")) %>% 
  rename(global_mean = value)

sig_analysis <- sig_analysis1 %>% 
  left_join(sig_analysis2) %>%
  mutate(gt_null = as.numeric(value>global_mean),
         site_level_minus_global_mean = value-global_mean) %>% 
  group_by(Parameter, habitat) %>% 
  summarise(pct_gt_null = sum(gt_null)/n()*100,
            site_level_re_mean = mean(site_level_minus_global_mean),
            site_level_re_se = sd(site_level_minus_global_mean),
            site_level_re_lower_ci = quantile(site_level_minus_global_mean, 0.025),
            site_level_re_upper_ci = quantile(site_level_minus_global_mean, 0.975)
            ) %>% 
  mutate(pct_lt_null = 100 - pct_gt_null)
(sig_analysis)
write_csv(sig_analysis, "OM and C Relationship/tabs/Par_Sig_table.csv")


