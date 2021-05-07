# loading data from source file
source("data.R")
source("pkg.R")
#Here we define the incubation period and generation time based on 
#literature estimates for 
# Covid-19 (see here for the code that generates these estimates).

generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
options(mc.cores = 4)
set.seed(100)
# define reporting delay as lognormal with mean of 5 days and sd of 1 day in absence of
# evidence. If data on onset -> report then can use estimate_delay to estimate the delay
reporting_delay <- list(mean = convert_to_logmean(5, 1),
                        mean_sd = 0.1,
                        sd = convert_to_logsd(5, 1),
                        sd_sd = 0.1,
                        max = 20)
reporting_delay
df<-np_df %>%
  
  mutate(date=lubridate::mdy(date)) %>%
  mutate(per_day=
           ifelse(confirmed==0,
                  confirmed, 
                  confirmed -lag(confirmed)))

cases <- df %>%
  select(date,per_day) %>%
  filter(per_day > 50)

colnames(cases)<-c("date","confirm")

cases<-cases[250:nrow(cases),]

out <- epinow(reported_cases = cases, 
              generation_time = generation_time,
              delays = delay_opts(incubation_period, reporting_delay),
              rt = rt_opts(prior = list(mean = 1.5, sd = 0.5)),
              
              gp = gp_opts(basis_prop = 0.2),
             
              stan = stan_opts(future = TRUE, max_execution_time = 60 * 30),
              horizon = 14, 
              target_folder = "results",
              logs = file.path("logs", Sys.Date()),
              return_output = TRUE, 
              verbose = TRUE)
