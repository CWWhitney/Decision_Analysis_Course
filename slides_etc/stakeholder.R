#install.packages("ggrepel")
library(ggplot2)
library(ggrepel)
library(ggthemes)
#Call data for stakeholders' expertise, willingness and potential contribution 
#in the decision analysis#

stakeholder<-read.csv("data/stakeholder.csv")
#1. Plot stakeholders' experience, availability and expertise in decision analysis ####

ggplot(data = stakeholder, aes(x=Experience, 
                               y=Availability, 
                               label =stakeholders, 
                               color=Expertise))+ 
  geom_point(aes(shape=Gender))+
  xlab("Relevant Experiences")+
 #label names of stakeholder and expand space to show full names
  scale_x_continuous(labels = paste(seq(0, 5, by = 1)), 
                     breaks = seq(0,5, by = 1), 
                     limits = c(0, 5),
                     expand = c(0,1))+
  scale_y_continuous(labels = paste(seq(0, 5, by = 1)), 
                     breaks = seq(0,5, by = 1), 
                     limits = c(0, 5),
                     expand = c(0,1))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))+
# Create line to categorize stakeholders
  geom_hline(yintercept=2.5, color="white", size=2)+
  geom_vline(xintercept=2.5, color="white", size=2)+
# Show all names of overlapped values
# https://ggrepel.slowkow.com/articles/examples.html
  geom_text_repel(box.padding = 0.3, max.overlaps = Inf, size=3)+
  annotate("text", label = "Potential core experts",
    x = 4.5, y = 3.2, size = 5, colour = "grey48")+
  annotate("text", label = "Resource persons",
           x = 4.5, y = 0.25, size = 5, colour = "grey48")
 # ggsave("expertplot.jpeg", dpi=400, height=7, width = 8)
 
#2. Stakeholder power and interest analysis in 2019####
# stakeholder<-read.csv("stakeholder.csv")
 Stakeholder_analysis_2019<-ggplot(data = stakeholder, aes(x=Interest_2019, y=Influence_2019, 
                                label=stakeholders,
                                color=Attitude_2019, size=Relevance_2019))+ 
   geom_point()+
   xlab("Perceived Interest (2019)")+
   ylab("Perceived Influence (2019)")+
   
   #label names of stakeholder and expand space to show full names
   scale_x_continuous(labels = paste(seq(0, 5, by = 1)), 
                      breaks = seq(0,5, by = 1), 
                      limits = c(0, 5),
                      expand = c(0,0.5))+
   scale_y_continuous(labels = paste(seq(0, 5, by = 1)), 
                      breaks = seq(0,5, by = 1), 
                      limits = c(0, 5),
                      expand = c(0,0.5))+
   theme(plot.margin = unit(c(1,1,1,1), "cm"))+
   scale_color_manual(breaks = c("Negative", "No information", "Positive"),
                      values=c("red", "blue", "dark green"))+
   # Create line to categorize stakeholders
   geom_hline(yintercept=2.5, color="white", size=2)+
   geom_vline(xintercept=2.5, color="white", size=2)+
   # Show all names of overlapped values
   # https://ggrepel.slowkow.com/articles/examples.html
   geom_text_repel(box.padding = 0.3, max.overlaps = Inf, size=3)+
   annotate("text", label = "Manage closely/Seek support",
            x = 4.0, y = 4.75, size = 4.0, colour = "grey48")+
   annotate("text", label = "Inform/Organize collectively",
            x = 4.0, y = 1.0, size = 4.0, colour = "grey48")+
   annotate("text", label = "Keep satisfied/Increase interest",
          x = 1.0, y = 4.75, size = 4.0, colour = "grey48")+
   annotate("text", label = "Inform/Increase interest/Dialogue",
            x = 1.0, y = 1.0, size = 4.0, colour = "grey48")
 # ggsave("stakeholder_analysis_2019.jpeg", dpi=400, height=7, width = 8)
 
 #2. Stakeholder power and interest analysis in 2020####
 # stakeholder<-read.csv("stakeholder.csv")
 ggplot(data = stakeholder, aes(x=Interest_2020, y=Influence_2020, 
                                label=stakeholders,
                                color=Attitude_2020, size=Relevance_2020))+ 
   geom_point()+
   xlab("Perceived Interest (2020)")+
   ylab("Perceived Influence (2020)")+
   
   #label names of stakeholder and expand space to show full names
   scale_x_continuous(labels = paste(seq(0, 5, by = 1)), 
                      breaks = seq(0,5, by = 1), 
                      limits = c(0, 5),
                      expand = c(0,0.5))+
   scale_y_continuous(labels = paste(seq(0, 5, by = 1)), 
                      breaks = seq(0,5, by = 1), 
                      limits = c(0, 5),
                      expand = c(0,0.5))+
   theme(plot.margin = unit(c(1,1,1,1), "cm"))+
   scale_color_manual(breaks = c("Negative", "No information", "Positive", "Neutral"),
                      values=c("red", "blue", "dark green", "thistle4"))+
   # Create line to categorize stakeholders
   geom_hline(yintercept=2.5, color="white", size=2)+
   geom_vline(xintercept=2.5, color="white", size=2)+
   # Show all names of overlapped values
   # https://ggrepel.slowkow.com/articles/examples.html
   geom_text_repel(box.padding = 0.3, max.overlaps = Inf, size=3)+
   annotate("text", label = "Manage closely/Seek support",
            x = 4.0, y = 4.75, size = 4.0, colour = "grey48")+
   annotate("text", label = "Inform/Organize collectively",
            x = 4.0, y = 1.0, size = 4.0, colour = "grey48")+
   annotate("text", label = "Keep satisfied/Increase interest",
            x = 0.75, y = 4.75, size = 4.0, colour = "grey48")+
   annotate("text", label = "Inform/Increase interest/Dialogue",
            x = 0.7, y = 1.0, size = 4.0, colour = "grey48")
 # ggsave("stakeholder_analysis_2020.jpeg", dpi=400, height=7, width = 8)
 


#https://stackoverflow.com/questions/15624656/label-points-in-geom-point
#https://intellipaat.com/community/16343/how-to-put-labels-over-geombar-for-each-bar-in-r-with-ggplot2




