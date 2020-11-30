library(tidyverse)
library(dplyr)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

N <-1500
p*N
se <- sqrt(p*(1-p)/N)

brexit_polls <- brexit_polls %>% 
  mutate(x_hat = (spread+1)/2)
  
mean(brexit_polls$spread)
sd(brexit_polls$spread)
mean(brexit_polls$x_hat)
sd(brexit_polls$x_hat)

brexit_polls[1,]
ci <- mean(brexit_polls$x_hat) + c(-1,1)*qnorm(0.975)*sd(brexit_polls$x_hat)

ci <- 0.52 + c(-1,1) *qnorm(0.975)* sqrt(0.52*(1-0.52)/4772)

d <- -0.0381

june_polls <- brexit_polls %>%
  filter(enddate >= "2016-6-01") %>%
  mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize) )%>%
  mutate(se_spread = 2*se_x_hat) %>%
  mutate(lower = spread-qnorm(0.975)*se_spread, 
         upper = spread+qnorm(0.975)*se_spread)%>%
  mutate(hit_0 = lower<=0 & upper >=0) %>%
  mutate(hit_leave = lower >0)%>%
  mutate(hit = d >= lower & d <=upper)

mean(june_polls$hit)

june_polls %>% group_by(pollster) %>%
  summarize(n= n(), hit_rate = sum(hit)/n) %>%
  arrange(desc(hit_rate))

june_polls %>% ggplot(aes(poll_type, spread)) +geom_boxplot()

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)
combined_by_type <- combined_by_type %>%
   mutate ( lower = spread-qnorm(0.975)*2*sqrt(p_hat*(1-p_hat)/N),
            upper = spread+qnorm(0.975)*2*sqrt(p_hat*(1-p_hat)/N))

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

online_yes <- sum(brexit_hit$poll_type == "Online" & brexit_hit$hit==TRUE)
online_no  <- sum(brexit_hit$poll_type == "Online" & brexit_hit$hit==FALSE)
telephone_yes <- sum(brexit_hit$poll_type == "Telephone" & brexit_hit$hit==TRUE)
telephone_no   <- sum(brexit_hit$poll_type == "Telephone" & brexit_hit$hit==FALSE)


two_by_two <- tibble(hitstatus = c("yes","no"),
                                    online =c(online_yes, online_no),
                                    telephone =c(telephone_yes, telephone_no))


# online > telephone
hit_rate <- brexit_hit %>%
  group_by(poll_type) %>%
  summarize(avg = mean(hit))
hit_rate$avg[hit_rate$poll_type == "Online"] > hit_rate$avg[hit_rate$poll_type == "Telephone"]o

# statistically significant
chisq.test(brexit_chisq)$p.value < 0.05

hit_rate <- brexit_hit %>%
  group_by(poll_type) %>%
  summarize(avg =mean(hit))

hit_rate

odds_online <- two[1,2]/(two[1,1]+two[1,2])

brexit_polls %>% 
  ggplot(aes(enddate,spread, col=poll_type))+
  geom_smooth(method = "loess",span=0.4)+
  geom_point()+
  geom_hline(aes(yintercept = -0.038))

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

brexit_long %>%
  ggplot(aes(enddate,proportion, col=vote))+
  geom_smooth(method = "loess",span=0.3)+
  geom_point()+
  geom_hline(aes(yintercept = -0.038))

                                
  

  

  
  