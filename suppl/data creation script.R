cons_perf<-read_csv("suppl/cons_perf.csv")
therapist<-data.frame(
Therapist = c("Dustin", "Samantha", "Blaine", "Nikola", "Ricardo"),
attrition_prob = c(.2, .3, .4, .3, .4), 
recovery_prob = c(.3, .25, .5, .4, .2)
)

clinician<-cons_perf%>%
  select(StartDate, Therapist = Manager, Pull_Date = RecordedDate, Recruitment_Channel = DistributionChannel, Q1.1:Q2.3)%>%
  mutate(id = sample(100000:999999, replace = FALSE, size = nrow(.)), 
         Recruitment_Channel = sample(c("Email", "Medical Referral", "Friend Referral", "Google"), replace = TRUE, size = nrow(.)),
         Therapist = sample(c("Dustin", "Samantha", "Blaine", "Nikola", "Ricardo"), replace = TRUE, size = nrow(.)),
         Pull_Date = "8/21/2019")%>%
  left_join(therapist)%>%
  rowwise()%>%
  mutate(recovery = rbinom(n = 1, 1, recovery_prob),
         attrition = if_else(recovery==1, 0, as.double(rbinom(n = 1, size = 1, prob = attrition_prob))))%>%
  select(-attrition_prob, -recovery_prob)%>%
  select(id, everything())


clinician[rbinom(nrow(clinician), 1, prob = .14), "Q1.1"]<-NA
clinician[rbinom(nrow(clinician), 1, prob = .17), "Q1.2"]<-NA
clinician[rbinom(nrow(clinician), 1, prob = .03), "Q1.3"]<-NA
clinician[rbinom(nrow(clinician), 1, prob = .03), "Q2.1"]<-NA
clinician[rbinom(nrow(clinician), 1, prob = .08), "Q2.2"]<-NA
clinician[rbinom(nrow(clinician), 1, prob = .05), "Q2.3"]<-NA

write.csv(clinician, "suppl/clinician.csv")
