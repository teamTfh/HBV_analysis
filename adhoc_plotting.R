 
samp <- read_excel("./data/20260304_xm_aim_subjects.xlsx")
setDT(samp)

samp
samp[,Participant := str_replace(Participant, "NLV-", "")][, Participant := tolower(Participant)]


elisa_mergeMeta <- readRDS("./outs/elisa_mergeMeta_with_offsetVisitDates.rds")

glimpse(elisa_mergeMeta)
elisa_mergeMeta[!Visit %in% c("optional.v1", "optional.v2") & Study.arm == 'HBVnaive-Alum'][Participant %in% samp$Participant] %>%
  ggplot(aes(x = Visit, #DaysFromFirstVisit, 
               y = elisa.HBsAb)) + 
               geom_line(aes(group=Participant)
                         #, alpha=0.2
                 ) + geom_point(size=2#,alpha=0.6
                                )  + 
              # scale_color_manual(values = c("#E66100","#5D3A9B"))+ 
               scale_y_log10() +  
               ct2 + theme(text = element_text(size = 16)) + facet_wrap(~Participant) +
  geom_hline(yintercept  = 100, linetype = "dashed", color = "red")
