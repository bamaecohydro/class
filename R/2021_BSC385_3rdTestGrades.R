#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: BSC 385 Exam 2 Stats
#Date: 3/9/2021
#Coder: Nate Jones
#Purpose: Create plots highlighting exam grades
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: Load workspace --------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear workspace
remove(list=ls())

#Load relevant libraries
library(tidyverse)
library(readxl)
library(patchwork)

#Load data
df<-read_xlsx('data/grades_3rdTest.xlsx')

#Clean up data
df<-df %>% 
  select(Exam_3='Exam_3 (**Webcam**) - Requires Respondus LockDown Browser [Total Pts: 40 Score] |2448942') %>% 
  mutate(
    Exam_3 = Exam_3/40*100)

# # A tibble: 116 x 4
# Student_ID Midterm_Grade Exam_1 Exam_2
# <dbl>         <dbl>  <dbl>  <dbl>

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: Plots -----------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Gobal plotting options
breaks_major<-c(40,50,60,70,80,90,100.01) - 0.01
breaks_minor<-c(60,63,67,70,73,77,80,83,87,90, 93, 97, 100) - 0.01
xlim<-c(39,101)
ylim<-c(0,45)

# #Plot distribution of grades
exam_grades<-df %>% 
  ggplot(aes(x=Exam_3)) + 
  #geom_density(fill = "steelblue4", alpha=0.75) +
  geom_histogram(
    col = "grey30",
    fill="steelblue4", 
    breaks = breaks_major) +
  geom_histogram(
    col = "grey30",
    fill="steelblue3", 
    breaks =breaks_minor) +
  geom_vline(
    xintercept = mean(df$Exam_3, na.rm=T), 
    col="darkorange", 
    lty=4, 
    lwd=2) +
  geom_vline(
    xintercept = median(df$Exam_3, na.rm=T), 
    col="darkred", 
    lty=2, 
    lwd=2) + 
  theme_bw() +
  xlim(xlim) +
  scale_y_continuous(
    limits = ylim,
    expand=c(0,0)) +
  ggtitle("A) Exam Grades") +
  ylab("Frequency") + 
  xlab("Exam Grades [%]") + 
  theme(
    title = element_text(size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10))

png("docs/exam_3_grades.png", width=5, height = 3.5, units="in", res=200)
exam_grades
dev.off()
