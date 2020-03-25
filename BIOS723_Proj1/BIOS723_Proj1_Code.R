# install.packages("cowplot")
# install.packages("hrbrthemes")
# install.packages("gcookbook")
# install.packages("gridExtra")

library(tidyverse)
library(cowplot)
library(gridExtra)
################################################################################
#######################Section Zero. Exact distribution ##########################
################################################################################

qpois(0.95, 24)

qpois(0.95, 48)

qpois(0.95, 8)

qpois(0.95, 16)

qexp(p = 0.05, rate = 2)

qexp(p = 0.05, rate = 2/3)

################################################################################
#######################Section I. Part (a) simulation ##########################
################################################################################
seed_init <- 20200321
sim <- 10000
lambda <- 2
febrile_rate <- 1/3

# --- I.(a) Number of patients in 12 hours --- # 
N_pat_12 <- c()
for (iter in 1:sim) {
  set.seed(seed_init+iter)
  temp_time_interv <- rexp(1, lambda)
  max_hour <- temp_time_interv
  N_temp = 0
  while (max_hour <= 12) {
    
    N_temp = N_temp + 1
    temp_time_interv <- rexp(1, lambda)
    max_hour <- max_hour + temp_time_interv
    
  }
  N_pat_12 <- c(N_pat_12, N_temp)
}
mean(N_pat_12)
var(N_pat_12)
sd(N_pat_12)
sort(N_pat_12, decreasing = FALSE)[sim - sim*0.05]

patient_12 <- data.frame(N_pat_12=N_pat_12) %>%
  ggplot(., aes(x=N_pat_12)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1, 
                 fill = "#012169", 
                 color = "#0c2340", 
                 alpha=0.9, 
                 position="identity") +
#  geom_density(alpha=0.6) +
  labs(title="General patients: density plot of arrival counts in one 12-hour period", 
       x = "Arrival counts",
       y = "Density") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(color="red", size=20, face="bold.italic"),
        axis.title.x = element_text(color="blue", size=18, face="bold"),
        axis.title.y = element_text(color="#993333", size=18, face="bold")) +
  geom_vline(aes(xintercept=24),
             linetype="dashed",
             color = "red")+ 
  annotate(geom="text", x=27, size=5, y=0.09, label="E(N_{12})=24",
           color="red")+ 
  geom_vline(aes(xintercept=33),
             linetype="dashed",
             color = "red")+ 
  annotate(geom="text", size=5, x=38, y=0.09, label="P(N_{12}>=33)<=0.05",
           color="red")
patient_12

# --- I.(b) Number of patients in 24 hours --- # 
N_pat_24 <- c()
for (iter in 1:sim) {
  set.seed(seed_init+iter)
  temp_time_interv <- rexp(1, lambda)
  max_hour <- temp_time_interv
  N_temp = 0
  while (max_hour <= 24) {
    
    N_temp = N_temp + 1
    temp_time_interv <- rexp(1, lambda)
    max_hour <- max_hour + temp_time_interv
  }
  N_pat_24 <- c(N_pat_24, N_temp)
}
mean(N_pat_24)
var(N_pat_24)
sd(N_pat_24)
sort(N_pat_24, decreasing = FALSE)[sim - sim*0.05]
qpois(0.95, 48)


patient_24 <- data.frame(N_pat_24=N_pat_24) %>%
  ggplot(., aes(x=N_pat_24)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1, 
                 fill = "#012169", 
                 color = "#0c2340", 
                 alpha=0.9, 
                 position="identity") +
#  geom_density(alpha=0.6) +
  labs(title="General patients: density plot of arrival counts in one 24-hour period", 
       x = "Arrival counts",
       y = "Density") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(color="red", size=20, face="bold.italic"),
        axis.title.x = element_text(color="blue", size=18, face="bold"),
        axis.title.y = element_text(color="#993333", size=18, face="bold")) +
  geom_vline(aes(xintercept=48),
             linetype="dashed",
             color = "red")+ 
  annotate(geom="text", size=5, x=52, y=0.065, label="E(N_{24})=48",
           color="red")+ 
  geom_vline(aes(xintercept=60),
             linetype="dashed",
             color = "red")+ 
  annotate(geom="text", size=5, x=67, y=0.065, label="P(N_{24}>=60)<=0.05",
           color="red")
patient_24

patient12_24<-grid.arrange(patient_12, patient_24, nrow = 1)
ggsave(filename = "./Dropbox/Emory Courses/Spring 2020/BIOS 723/Project/Result/general_patient_count.png", 
      plot = patient12_24,
      width = 60, height = 30, units = "cm")

# --- I.(c) Number of febrile in 12 hours --- # 
N_feb_12 <- c()
for (iter in 1:sim) {
  set.seed(seed_init+iter)
  temp_time_interv <- rexp(1, lambda)
  max_hour <- temp_time_interv
  N_temp = 0
  while (max_hour <= 12) {
    if (rbinom(1,1,1/3) == 1) {
      N_temp = N_temp + 1
    }
    temp_time_interv <- rexp(1, lambda)
    max_hour <- max_hour + temp_time_interv
    
  }
  N_feb_12 <- c(N_feb_12, N_temp)
}
mean(N_feb_12)
var(N_feb_12)
sd(N_feb_12)
sort(N_feb_12, decreasing = FALSE)[sim - sim*0.05]

feb_patient_12 <- data.frame(N_feb_12=N_feb_12) %>%
  ggplot(., aes(x=N_feb_12)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1, 
                 fill = "#012169", 
                 color = "#0c2340", 
                 alpha=0.9, 
                 position="identity") +
  labs(title="Febrile patients: density plot of arrival counts in one 12-hour period", 
       x = "Arrival counts",
       y = "Density") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(color="red", size=20, face="bold.italic"),
        axis.title.x = element_text(color="blue", size=18, face="bold"),
        axis.title.y = element_text(color="#993333", size=18, face="bold")) +
  geom_vline(aes(xintercept=8),
             linetype="dashed",
             color = "red")+ 
  annotate(geom="text", x=10, size=5, y=0.16, label="E(M_{12})=8",
           color="red")+ 
  geom_vline(aes(xintercept=13),
             linetype="dashed",
             color = "red")+ 
  annotate(geom="text", size=5, x=16.5, y=0.16, label="P(M_{12}>=13)<=0.05",
           color="red")
feb_patient_12

# --- I.(d) Number of febrile in 24 hours --- # 
N_feb_24 <- c()
for (iter in 1:sim) {
  set.seed(seed_init+iter)
  temp_time_interv <- rexp(1, lambda)
  max_hour <- temp_time_interv
  N_temp = 0
  while (max_hour <= 24) {
    
    if (rbinom(1,1,1/3) == 1) {
      N_temp = N_temp + 1
    }
    temp_time_interv <- rexp(1, lambda)
    max_hour <- max_hour + temp_time_interv
    
  }
  N_feb_24 <- c(N_feb_24, N_temp)
}
mean(N_feb_24)
var(N_feb_24)
sd(N_feb_24)
sort(N_feb_24, decreasing = FALSE)[sim - sim*0.05]
qpois(0.95, 48)

feb_patient_24 <- data.frame(N_feb_24=N_feb_24) %>%
  ggplot(., aes(x=N_feb_24)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1, 
                 fill = "#012169", 
                 color = "#0c2340", 
                 alpha=0.9, 
                 position="identity") +
  #  geom_density(alpha=0.6) +
  labs(title="Febrile patients: density plot of arrival counts in one 24-hour period", 
       x = "Arrival counts",
       y = "Density") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(color="red", size=20, face="bold.italic"),
        axis.title.x = element_text(color="blue", size=18, face="bold"),
        axis.title.y = element_text(color="#993333", size=18, face="bold")) +
  geom_vline(aes(xintercept=16),
             linetype="dashed",
             color = "red")+ 
  annotate(geom="text", size=5, x=19, y=0.12, label="E(M_{24})=16",
           color="red")+ 
  geom_vline(aes(xintercept=23),
             linetype="dashed",
             color = "red")+ 
  annotate(geom="text", size=5, x=27.5, y=0.12, label="P(M_{24}>=23)<=0.05",
           color="red")
feb_patient_24

feb_patient12_24<-grid.arrange(feb_patient_12, feb_patient_24, nrow = 1)
ggsave(filename = "./Dropbox/Emory Courses/Spring 2020/BIOS 723/Project/Result/feb_patient_count.png", 
       plot = feb_patient12_24,
       width = 60, height = 30, units = "cm")

################################################################################
#######################Section II. Part b simulation ###########################
################################################################################

# --- II.(a) Number of sick patients in 24 hours --- # 
t = 24
sim = 5000
df_24_patient <- data.frame()
for (iter in 1:sim) {
  
  N = rpois(1, lambda*t)
  unifs = runif(N,0,t)
  arrivals = sort(unifs)
  
  df_temp <- data.frame(index = iter,
                        arrivals = c(0, arrivals)) %>%
    mutate(Diff = arrivals - lag(arrivals))
  df_24_patient <- rbind(df_24_patient, df_temp)
}
general_pat_time_interval <- df_24_patient %>%
  group_by(index) %>%
  summarise(mean_v=mean(Diff, na.rm = T),
            variance_v = var(Diff, na.rm = T),
            sd_v = sd(Diff, na.rm = T),
            N_v=n())
mean(general_pat_time_interval$mean_v)
mean(general_pat_time_interval$variance_v,na.rm = T)
quantile(general_pat_time_interval$mean_v, 0.05)

general_pat_time_interval <- data.frame(general_pat_time_interval, 
                                        stringsAsFactors = F)
general_pat_time_interval_plot <- 
  ggplot(general_pat_time_interval, aes(x=mean_v)) +
  geom_histogram(aes(y=..density..),
                 #binwidth = 0.025, 
                 fill = "#012169", 
                 color = "#0c2340", 
                 alpha=0.9) +
  geom_density( alpha=0.6)+
  labs(title="General patients: density plot of interarrival intervals", 
       x = "Interarrival intervals",
       y = "Density") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(color="red", size=20, face="bold.italic"),
        axis.title.x = element_text(color="blue", size=18, face="bold"),
        axis.title.y = element_text(color="#993333", size=18, face="bold")) +
  geom_vline(aes(xintercept=mean(general_pat_time_interval$mean_v)),
             linetype="dashed",
             color = "red")+ 
  annotate(geom="text", x=0.5+0.06, size=5, y=6, label="Simulated mean: 0.50",
           color="red")+ 
  geom_vline(aes(xintercept=quantile(general_pat_time_interval$mean_v, 0.05)[[1]]),
             linetype="dashed",
             color = "red")+ 
  annotate(geom="text", size=5, x=quantile(general_pat_time_interval$mean_v, 0.05)[[1]]-0.055, y=6, label="5% Quantile: 0.39",
           color="red")
general_pat_time_interval_plot


# # --- II.(b) Number of sick patients in 12 hours --- # 
# t = 12
# sim = 5000
# df_12_patient <- data.frame()
# for (iter in 1:sim) {
#   
#   N = rpois(1, lambda*t)
#   unifs = runif(N,0,t)
#   arrivals = sort(unifs)
#   
#   df_temp <- data.frame(index = iter,
#                         arrivals = c(0, arrivals)) %>%
#     mutate(Diff = arrivals - lag(arrivals))
#   df_12_patient <- rbind(df_12_patient, df_temp)
# }
# df2 <- df_12_patient %>%
#   group_by(index) %>%
#   summarise(mean=mean(Diff, na.rm = T),
#             variance = var(Diff, na.rm = T),
#             N=n())
# mean(df2$mean, na.rm = T)
# mean(df2$variance, na.rm = T)


# --- II.(c) Number of febrile in 12 hours --- # 
t = 24
sim = 5000
df_12_feb <- data.frame()
set.seed(123)
for (iter in 1:sim) {
  N = rpois(1, lambda*t)
  unifs = runif(N,0,t)

  feb_pat <- rbinom(N,1,1/3)
  while(sum(feb_pat==1)==1){
    feb_pat <- rbinom(N,1,1/3)
  }

  unifs_keep <- unifs[as.logical(feb_pat)]
  arrivals = sort(unifs_keep)

  df_temp <- data.frame(index = iter,
                        arrivals = c(0, arrivals)) %>%
    mutate(Diff = arrivals - lag(arrivals))
  df_12_feb <- rbind(df_12_feb, df_temp)
}
feb_pat_time_interval <- df_12_feb %>%
  group_by(index) %>%
  summarise(mean_v=mean(Diff, na.rm = T),
            variance=var(Diff, na.rm = T))
mean(feb_pat_time_interval$mean_v, na.rm = T)
mean(feb_pat_time_interval$variance, na.rm = T)

feb_pat_time_interval <- data.frame(feb_pat_time_interval, 
                                        stringsAsFactors = F)
feb_pat_time_interval_plot <- 
  ggplot(feb_pat_time_interval, aes(x=mean_v)) +
  geom_histogram(aes(y=..density..),
                 #binwidth = 0.025, 
                 fill = "#012169", 
                 color = "#0c2340", 
                 alpha=0.9) +
  geom_density( alpha=0.6)+
  labs(title="Febrile patients: density plot of interarrival intervals", 
       x = "Interarrival intervals",
       y = "Density") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(color="red", size=20, face="bold.italic"),
        axis.title.x = element_text(color="blue", size=18, face="bold"),
        axis.title.y = element_text(color="#993333", size=18, face="bold")) +
  geom_vline(aes(xintercept=mean(feb_pat_time_interval$mean_v)),
             linetype="dashed",
             color = "red")+ 
  annotate(geom="text", x=mean(feb_pat_time_interval$mean_v)+0.5, size=5, y=1.3, label="Simulated mean: 1.50",
           color="red")+ 
  geom_vline(aes(xintercept=quantile(feb_pat_time_interval$mean_v, 0.05)[[1]]),
             linetype="dashed",
             color = "red")+ 
  annotate(geom="text", size=5, x=quantile(feb_pat_time_interval$mean_v, 0.05)[[1]]-0.5, y=1.3, label="5% Quantile: \n0.99",
           color="red")
feb_pat_time_interval_plot

feb_patient12_24<-grid.arrange(general_pat_time_interval_plot, feb_pat_time_interval_plot, nrow = 1)
ggsave(filename = "./Dropbox/Emory Courses/Spring 2020/BIOS 723/Project/Result/time_interval.png", 
       plot = feb_patient12_24,
       width = 60, height = 30, units = "cm")

# 
# t = 12
# sim = 10000
# df_12_feb <- data.frame()
# #set.seed(runif(1, 0, 100000))
# set.seed(1)
# for (iter in 1:sim) {
#   
#   N = rpois(1, (lambda*t)/3)
#   unifs = runif(N,0,t)
#   arrivals = sort(unifs)
#   
#   df_temp <- data.frame(index = iter,
#                         arrivals = c(0, arrivals)) %>%
#     mutate(Diff = arrivals - lag(arrivals))
#   df_12_feb <- rbind(df_12_feb, df_temp)
# }
# df2 <- df_12_feb %>%
#   group_by(index) %>%
#   summarise(mean=mean(Diff, na.rm = T),
#             variance = var(Diff, na.rm = T),
#             med = median(Diff, na.rm = T),
#             N=n())
# mean(df2$mean, na.rm = T)
# mean(df2$variance, na.rm = T)
# 


################################################################################
#######################Section II. Part b simulation - SD ###########################
################################################################################

# --- II.(a) Number of sick patients in 24 hours --- # 
t = 24
sim = 5000
df_24_patient <- data.frame()
for (iter in 1:sim) {
  
  N = rpois(1, lambda*t)
  unifs = runif(N,0,t)
  arrivals = sort(unifs)
  
  df_temp <- data.frame(index = iter,
                        arrivals = c(0, arrivals)) %>%
    mutate(Diff = arrivals - lag(arrivals))
  df_24_patient <- rbind(df_24_patient, df_temp)
}
general_pat_time_interval <- df_24_patient %>%
  group_by(index) %>%
  summarise(mean_v=mean(Diff, na.rm = T),
            variance_v = var(Diff, na.rm = T),
            sd_v = sd(Diff, na.rm = T),
            N_v=n())
mean(general_pat_time_interval$mean_v)
mean(general_pat_time_interval$variance_v,na.rm = T)
mean(general_pat_time_interval$sd_v,na.rm = T)

quantile(general_pat_time_interval$mean_v, 0.05)
quantile(general_pat_time_interval$sd_v, 0.05)

general_pat_time_interval <- data.frame(general_pat_time_interval, 
                                        stringsAsFactors = F)
general_pat_time_interval_plot <- 
  ggplot(general_pat_time_interval, aes(x=sd_v)) +
  geom_histogram(aes(y=..density..),
                 #binwidth = 0.025, 
                 fill = "#012169", 
                 color = "#0c2340", 
                 alpha=0.9) +
  geom_density( alpha=0.6)+
  labs(title="General patients: density plot of standard deviation for interarrival intervals", 
       x = "Interarrival intervals",
       y = "Density") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(color="red", size=20, face="bold.italic"),
        axis.title.x = element_text(color="blue", size=18, face="bold"),
        axis.title.y = element_text(color="#993333", size=18, face="bold")) +
  geom_vline(aes(xintercept=mean(general_pat_time_interval$sd_v)),
             linetype="dashed",
             color = "red")+ 
  annotate(geom="text", x=0.6, size=5, y=6, label="Simulated mean of SD: 0.49",
           color="red")+ 
  geom_vline(aes(xintercept=quantile(general_pat_time_interval$sd_v, 0.05)[[1]]),
             linetype="dashed",
             color = "red")+ 
  annotate(geom="text", size=5, x=quantile(general_pat_time_interval$sd_v, 0.05)[[1]]-0.055, y=6, label="5% Quantile: 0.35",
           color="red")
general_pat_time_interval_plot


# # --- II.(b) Number of sick patients in 12 hours --- # 
# t = 12
# sim = 5000
# df_12_patient <- data.frame()
# for (iter in 1:sim) {
#   
#   N = rpois(1, lambda*t)
#   unifs = runif(N,0,t)
#   arrivals = sort(unifs)
#   
#   df_temp <- data.frame(index = iter,
#                         arrivals = c(0, arrivals)) %>%
#     mutate(Diff = arrivals - lag(arrivals))
#   df_12_patient <- rbind(df_12_patient, df_temp)
# }
# df2 <- df_12_patient %>%
#   group_by(index) %>%
#   summarise(mean=mean(Diff, na.rm = T),
#             variance = var(Diff, na.rm = T),
#             N=n())
# mean(df2$mean, na.rm = T)
# mean(df2$variance, na.rm = T)


# --- II.(c) Number of febrile in 12 hours --- # 
t = 24
sim = 5000
df_12_feb <- data.frame()
set.seed(123)
for (iter in 1:sim) {
  N = rpois(1, lambda*t)
  unifs = runif(N,0,t)
  
  feb_pat <- rbinom(N,1,1/3)
  while(sum(feb_pat==1)==1){
    feb_pat <- rbinom(N,1,1/3)
  }
  
  unifs_keep <- unifs[as.logical(feb_pat)]
  arrivals = sort(unifs_keep)
  
  df_temp <- data.frame(index = iter,
                        arrivals = c(0, arrivals)) %>%
    mutate(Diff = arrivals - lag(arrivals))
  df_12_feb <- rbind(df_12_feb, df_temp)
}
feb_pat_time_interval <- df_12_feb %>%
  group_by(index) %>%
  summarise(mean_v=mean(Diff, na.rm = T),
            variance=var(Diff, na.rm = T),
            sd_v = sd(Diff, na.rm = T),
            N=n())
mean(feb_pat_time_interval$mean_v, na.rm = T)
mean(feb_pat_time_interval$variance, na.rm = T)
mean(feb_pat_time_interval$sd_v, na.rm = T)

feb_pat_time_interval <- data.frame(feb_pat_time_interval, 
                                    stringsAsFactors = F)
feb_pat_time_interval_plot <- 
  ggplot(feb_pat_time_interval, aes(x=sd_v)) +
  geom_histogram(aes(y=..density..),
                 #binwidth = 0.025, 
                 fill = "#012169", 
                 color = "#0c2340", 
                 alpha=0.9) +
  geom_density( alpha=0.6)+
  labs(title="Febrile patients: density plot of standard deviation for interarrival intervals", 
       x = "Interarrival intervals",
       y = "Density") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(color="red", size=20, face="bold.italic"),
        axis.title.x = element_text(color="blue", size=18, face="bold"),
        axis.title.y = element_text(color="#993333", size=18, face="bold")) +
  geom_vline(aes(xintercept=mean(feb_pat_time_interval$sd_v)),
             linetype="dashed",
             color = "red")+ 
  annotate(geom="text", x=mean(feb_pat_time_interval$sd_v)+0.5, size=5, y=1.1, label="Simulated mean of SD: 1.41",
           color="red")+ 
  geom_vline(aes(xintercept=quantile(feb_pat_time_interval$sd_v, 0.05)[[1]]),
             linetype="dashed",
             color = "red")+ 
  annotate(geom="text", size=5, x=quantile(feb_pat_time_interval$sd_v, 0.05)[[1]]-0.2, y=1.1, label="5% Quantile: \n0.82",
           color="red")
feb_pat_time_interval_plot

feb_patient12_24<-grid.arrange(general_pat_time_interval_plot, feb_pat_time_interval_plot, nrow = 1)
ggsave(filename = "./Dropbox/Emory Courses/Spring 2020/BIOS 723/Project/Result/time_interval_standard_deviation.png", 
       plot = feb_patient12_24,
       width = 60, height = 30, units = "cm")

# 
# t = 12
# sim = 10000
# df_12_feb <- data.frame()
# #set.seed(runif(1, 0, 100000))
# set.seed(1)
# for (iter in 1:sim) {
#   
#   N = rpois(1, (lambda*t)/3)
#   unifs = runif(N,0,t)
#   arrivals = sort(unifs)
#   
#   df_temp <- data.frame(index = iter,
#                         arrivals = c(0, arrivals)) %>%
#     mutate(Diff = arrivals - lag(arrivals))
#   df_12_feb <- rbind(df_12_feb, df_temp)
# }
# df2 <- df_12_feb %>%
#   group_by(index) %>%
#   summarise(mean=mean(Diff, na.rm = T),
#             variance = var(Diff, na.rm = T),
#             med = median(Diff, na.rm = T),
#             N=n())
# mean(df2$mean, na.rm = T)
# mean(df2$variance, na.rm = T)
# 



################################################################################
#######################Section II. Part b simulation 72 hour ###########################
################################################################################

# --- II.(a) Number of sick patients in 24 hours --- # 
t = 120
sim = 5000
df_24_patient <- data.frame()
for (iter in 1:sim) {
  
  N = rpois(1, lambda*t)
  unifs = runif(N,0,t)
  arrivals = sort(unifs)
  
  df_temp <- data.frame(index = iter,
                        arrivals = c(0, arrivals)) %>%
    mutate(Diff = arrivals - lag(arrivals))
  df_24_patient <- rbind(df_24_patient, df_temp)
}
general_pat_time_interval <- df_24_patient %>%
  group_by(index) %>%
  summarise(mean_v=mean(Diff, na.rm = T),
            variance_v = var(Diff, na.rm = T),
            sd_v = sd(Diff, na.rm = T),
            N_v=n())
mean(general_pat_time_interval$mean_v)
mean(general_pat_time_interval$variance_v,na.rm = T)
quantile(general_pat_time_interval$mean_v, 0.05)

general_pat_time_interval <- data.frame(general_pat_time_interval, 
                                        stringsAsFactors = F)
general_pat_time_interval_plot <- 
  ggplot(general_pat_time_interval, aes(x=mean_v)) +
  geom_histogram(aes(y=..density..),
                 #binwidth = 0.025, 
                 fill = "#012169", 
                 color = "#0c2340", 
                 alpha=0.9) +
  geom_density( alpha=0.6)+
  labs(title="General patients: density plot of interarrival intervals on 120-hour interval", 
       x = "Interarrival intervals",
       y = "Density") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(color="red", size=20, face="bold.italic"),
        axis.title.x = element_text(color="blue", size=18, face="bold"),
        axis.title.y = element_text(color="#993333", size=18, face="bold")) +
  geom_vline(aes(xintercept=mean(general_pat_time_interval$mean_v)),
             linetype="dashed",
             color = "red")+ 
  annotate(geom="text", x=0.5+0.06, size=5, y=15, label="Simulated mean: 0.50",
           color="red")+ 
  geom_vline(aes(xintercept=quantile(general_pat_time_interval$mean_v, 0.05)[[1]]),
             linetype="dashed",
             color = "red")+ 
  annotate(geom="text", size=5, x=quantile(general_pat_time_interval$mean_v, 0.05)[[1]], y=15, label="5% Quantile: 0.45",
           color="red")
general_pat_time_interval_plot


# # --- II.(b) Number of sick patients in 12 hours --- # 
# t = 12
# sim = 5000
# df_12_patient <- data.frame()
# for (iter in 1:sim) {
#   
#   N = rpois(1, lambda*t)
#   unifs = runif(N,0,t)
#   arrivals = sort(unifs)
#   
#   df_temp <- data.frame(index = iter,
#                         arrivals = c(0, arrivals)) %>%
#     mutate(Diff = arrivals - lag(arrivals))
#   df_12_patient <- rbind(df_12_patient, df_temp)
# }
# df2 <- df_12_patient %>%
#   group_by(index) %>%
#   summarise(mean=mean(Diff, na.rm = T),
#             variance = var(Diff, na.rm = T),
#             N=n())
# mean(df2$mean, na.rm = T)
# mean(df2$variance, na.rm = T)


# --- II.(c) Number of febrile in 12 hours --- # 
t = 120
sim = 5000
df_12_feb <- data.frame()
set.seed(123)
for (iter in 1:sim) {
  N = rpois(1, lambda*t)
  unifs = runif(N,0,t)
  
  feb_pat <- rbinom(N,1,1/3)
  while(sum(feb_pat==1)==1){
    feb_pat <- rbinom(N,1,1/3)
  }
  
  unifs_keep <- unifs[as.logical(feb_pat)]
  arrivals = sort(unifs_keep)
  
  df_temp <- data.frame(index = iter,
                        arrivals = c(0, arrivals)) %>%
    mutate(Diff = arrivals - lag(arrivals))
  df_12_feb <- rbind(df_12_feb, df_temp)
}
feb_pat_time_interval <- df_12_feb %>%
  group_by(index) %>%
  summarise(mean_v=mean(Diff, na.rm = T),
            variance=var(Diff, na.rm = T))
mean(feb_pat_time_interval$mean_v, na.rm = T)
mean(feb_pat_time_interval$variance, na.rm = T)

feb_pat_time_interval <- data.frame(feb_pat_time_interval, 
                                    stringsAsFactors = F)
feb_pat_time_interval_plot <- 
  ggplot(feb_pat_time_interval, aes(x=mean_v)) +
  geom_histogram(aes(y=..density..),
                 #binwidth = 0.025, 
                 fill = "#012169", 
                 color = "#0c2340", 
                 alpha=0.9) +
  geom_density( alpha=0.6)+
  labs(title="Febrile patients: density plot of interarrival intervals on 120-hour interval", 
       x = "Interarrival intervals",
       y = "Density") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(color="red", size=20, face="bold.italic"),
        axis.title.x = element_text(color="blue", size=18, face="bold"),
        axis.title.y = element_text(color="#993333", size=18, face="bold")) +
  geom_vline(aes(xintercept=mean(feb_pat_time_interval$mean_v)),
             linetype="dashed",
             color = "red")+ 
  annotate(geom="text", x=mean(feb_pat_time_interval$mean_v), size=5, y=2.6, label="Simulated mean: 1.50",
           color="red")+ 
  geom_vline(aes(xintercept=quantile(feb_pat_time_interval$mean_v, 0.05)[[1]]),
             linetype="dashed",
             color = "red")+ 
  annotate(geom="text", size=5, x=quantile(feb_pat_time_interval$mean_v, 0.05)[[1]], y=2.6, label="5% Quantile: \n1.26",
           color="red")
feb_pat_time_interval_plot

feb_patient12_24<-grid.arrange(general_pat_time_interval_plot, feb_pat_time_interval_plot, nrow = 1)
ggsave(filename = "./Dropbox/Emory Courses/Spring 2020/BIOS 723/Project/Result/time_interval_120_hour.png", 
       plot = feb_patient12_24,
       width = 60, height = 30, units = "cm")

# 
# t = 12
# sim = 10000
# df_12_feb <- data.frame()
# #set.seed(runif(1, 0, 100000))
# set.seed(1)
# for (iter in 1:sim) {
#   
#   N = rpois(1, (lambda*t)/3)
#   unifs = runif(N,0,t)
#   arrivals = sort(unifs)
#   
#   df_temp <- data.frame(index = iter,
#                         arrivals = c(0, arrivals)) %>%
#     mutate(Diff = arrivals - lag(arrivals))
#   df_12_feb <- rbind(df_12_feb, df_temp)
# }
# df2 <- df_12_feb %>%
#   group_by(index) %>%
#   summarise(mean=mean(Diff, na.rm = T),
#             variance = var(Diff, na.rm = T),
#             med = median(Diff, na.rm = T),
#             N=n())
# mean(df2$mean, na.rm = T)
# mean(df2$variance, na.rm = T)
# 


# --- II.(a) Number of sick patients in 24 hours --- # 
t = 72
sim = 5000
df_24_patient <- data.frame()
for (iter in 1:sim) {
  
  N = rpois(1, lambda*t)
  unifs = runif(N,0,t)
  arrivals = sort(unifs)
  
  df_temp <- data.frame(index = iter,
                        arrivals = c(0, arrivals)) %>%
    mutate(Diff = arrivals - lag(arrivals))
  df_24_patient <- rbind(df_24_patient, df_temp)
}
general_pat_time_interval <- df_24_patient %>%
  group_by(index) %>%
  summarise(mean_v=mean(Diff, na.rm = T),
            variance_v = var(Diff, na.rm = T),
            sd_v = sd(Diff, na.rm = T),
            N_v=n())
mean(general_pat_time_interval$mean_v)
mean(general_pat_time_interval$variance_v,na.rm = T)
mean(general_pat_time_interval$sd_v,na.rm = T)

quantile(general_pat_time_interval$mean_v, 0.05)
quantile(general_pat_time_interval$sd_v, 0.05)

general_pat_time_interval <- data.frame(general_pat_time_interval, 
                                        stringsAsFactors = F)
general_pat_time_interval_plot <- 
  ggplot(general_pat_time_interval, aes(x=sd_v)) +
  geom_histogram(aes(y=..density..),
                 #binwidth = 0.025, 
                 fill = "#012169", 
                 color = "#0c2340", 
                 alpha=0.9) +
  geom_density( alpha=0.6)+
  labs(title="General patients: density plot of standard deviation for interarrival intervals", 
       x = "Interarrival intervals",
       y = "Density") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(color="red", size=20, face="bold.italic"),
        axis.title.x = element_text(color="blue", size=18, face="bold"),
        axis.title.y = element_text(color="#993333", size=18, face="bold")) +
  geom_vline(aes(xintercept=mean(general_pat_time_interval$sd_v)),
             linetype="dashed",
             color = "red")+ 
  annotate(geom="text", x=0.6, size=5, y=6, label="Simulated mean of SD: 0.49",
           color="red")+ 
  geom_vline(aes(xintercept=quantile(general_pat_time_interval$sd_v, 0.05)[[1]]),
             linetype="dashed",
             color = "red")+ 
  annotate(geom="text", size=5, x=quantile(general_pat_time_interval$sd_v, 0.05)[[1]]-0.055, y=6, label="5% Quantile: 0.35",
           color="red")
general_pat_time_interval_plot


# # --- II.(b) Number of sick patients in 12 hours --- # 
# t = 12
# sim = 5000
# df_12_patient <- data.frame()
# for (iter in 1:sim) {
#   
#   N = rpois(1, lambda*t)
#   unifs = runif(N,0,t)
#   arrivals = sort(unifs)
#   
#   df_temp <- data.frame(index = iter,
#                         arrivals = c(0, arrivals)) %>%
#     mutate(Diff = arrivals - lag(arrivals))
#   df_12_patient <- rbind(df_12_patient, df_temp)
# }
# df2 <- df_12_patient %>%
#   group_by(index) %>%
#   summarise(mean=mean(Diff, na.rm = T),
#             variance = var(Diff, na.rm = T),
#             N=n())
# mean(df2$mean, na.rm = T)
# mean(df2$variance, na.rm = T)


# --- II.(c) Number of febrile in 12 hours --- # 
t = 72
sim = 5000
df_12_feb <- data.frame()
set.seed(123)
for (iter in 1:sim) {
  N = rpois(1, lambda*t)
  unifs = runif(N,0,t)
  
  feb_pat <- rbinom(N,1,1/3)
  while(sum(feb_pat==1)==1){
    feb_pat <- rbinom(N,1,1/3)
  }
  
  unifs_keep <- unifs[as.logical(feb_pat)]
  arrivals = sort(unifs_keep)
  
  df_temp <- data.frame(index = iter,
                        arrivals = c(0, arrivals)) %>%
    mutate(Diff = arrivals - lag(arrivals))
  df_12_feb <- rbind(df_12_feb, df_temp)
}
feb_pat_time_interval <- df_12_feb %>%
  group_by(index) %>%
  summarise(mean_v=mean(Diff, na.rm = T),
            variance=var(Diff, na.rm = T),
            sd_v = sd(Diff, na.rm = T),
            N=n())
mean(feb_pat_time_interval$mean_v, na.rm = T)
mean(feb_pat_time_interval$variance, na.rm = T)
mean(feb_pat_time_interval$sd_v, na.rm = T)

feb_pat_time_interval <- data.frame(feb_pat_time_interval, 
                                    stringsAsFactors = F)
feb_pat_time_interval_plot <- 
  ggplot(feb_pat_time_interval, aes(x=sd_v)) +
  geom_histogram(aes(y=..density..),
                 #binwidth = 0.025, 
                 fill = "#012169", 
                 color = "#0c2340", 
                 alpha=0.9) +
  geom_density( alpha=0.6)+
  labs(title="Febrile patients: density plot of standard deviation for interarrival intervals", 
       x = "Interarrival intervals",
       y = "Density") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(color="red", size=20, face="bold.italic"),
        axis.title.x = element_text(color="blue", size=18, face="bold"),
        axis.title.y = element_text(color="#993333", size=18, face="bold")) +
  geom_vline(aes(xintercept=mean(feb_pat_time_interval$sd_v)),
             linetype="dashed",
             color = "red")+ 
  annotate(geom="text", x=mean(feb_pat_time_interval$sd_v)+0.5, size=5, y=1.1, label="Simulated mean of SD: 1.41",
           color="red")+ 
  geom_vline(aes(xintercept=quantile(feb_pat_time_interval$sd_v, 0.05)[[1]]),
             linetype="dashed",
             color = "red")+ 
  annotate(geom="text", size=5, x=quantile(feb_pat_time_interval$sd_v, 0.05)[[1]]-0.2, y=1.1, label="5% Quantile: \n0.82",
           color="red")
feb_pat_time_interval_plot

feb_patient12_24<-grid.arrange(general_pat_time_interval_plot, feb_pat_time_interval_plot, nrow = 1)
ggsave(filename = "./Dropbox/Emory Courses/Spring 2020/BIOS 723/Project/Result/time_interval_standard_deviation_72_hour.png", 
       plot = feb_patient12_24,
       width = 60, height = 30, units = "cm")

# 
# t = 12
# sim = 10000
# df_12_feb <- data.frame()
# #set.seed(runif(1, 0, 100000))
# set.seed(1)
# for (iter in 1:sim) {
#   
#   N = rpois(1, (lambda*t)/3)
#   unifs = runif(N,0,t)
#   arrivals = sort(unifs)
#   
#   df_temp <- data.frame(index = iter,
#                         arrivals = c(0, arrivals)) %>%
#     mutate(Diff = arrivals - lag(arrivals))
#   df_12_feb <- rbind(df_12_feb, df_temp)
# }
# df2 <- df_12_feb %>%
#   group_by(index) %>%
#   summarise(mean=mean(Diff, na.rm = T),
#             variance = var(Diff, na.rm = T),
#             med = median(Diff, na.rm = T),
#             N=n())
# mean(df2$mean, na.rm = T)
# mean(df2$variance, na.rm = T)
# 
