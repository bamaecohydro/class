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
df<-read_xlsx('data/grades.xlsx')

#Clean up data
df<-df %>% 
  rename(
    Midterm_Grade='Current Grade', 
    Student_ID = 'Student ID') %>% 
  mutate(
    Exam_1 = Exam_1/40*100,
    Exam_2 = Exam_2/40*100) %>% 
  select(Student_ID, Midterm_Grade, Exam_1, Exam_2, Midterm_Grade)

# # A tibble: 116 x 4
# Student_ID Midterm_Grade Exam_1 Exam_2
# <dbl>         <dbl>  <dbl>  <dbl>

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: Plots -----------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Gobal plotting options
breaks_major<-c(40,50,60,70,80,90,100,110) - 0.01
breaks_minor<-c(60,63,67,70,73,77,80,83,87,90, 93, 97, 100) - 0.01
xlim<-c(39,111)
ylim<-c(0,45)


#Plot distribution of grades
midterm<-df %>% 
  ggplot(aes(x=Midterm_Grade)) + 
  geom_histogram(
    col = "grey30",
    fill="steelblue4", 
    breaks =breaks_major) +
  geom_histogram(
    col = "grey30",
    fill="steelblue3", 
    breaks = breaks_minor) +
  geom_vline(
    xintercept = mean(df$Midterm_Grade, na.rm=T), 
    col="darkorange", 
    lty=4, 
    lwd=2) +
  geom_vline(
    xintercept = median(df$Midterm_Grade, na.rm=T), 
    col="darkred", 
    lty=2, 
    lwd=2) + 
  theme_bw() +
  xlim(xlim) +
  scale_y_continuous(
    limits = ylim,
    expand=c(0,0)) +
  ggtitle("A) Midterm Course Grades") +
  ylab("Frequency") + 
  xlab("Midterm Course Grades [%]") + 
  theme(
    title = element_text(size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10))

# #Plot distribution of grades
exam_2<-df %>% 
  ggplot(aes(x=Exam_2)) + 
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
    xintercept = mean(df$Exam_2, na.rm=T), 
    col="darkorange", 
    lty=4, 
    lwd=2) +
  geom_vline(
    xintercept = median(df$Exam_2, na.rm=T), 
    col="darkred", 
    lty=2, 
    lwd=2) + 
  theme_bw() +
  xlim(xlim) +
  scale_y_continuous(
    limits = ylim,
    expand=c(0,0)) +
  ggtitle("B) Exam 2 Grades") +
  ylab("Frequency") + 
  xlab("Exam 2 Grades [%]") + 
  theme(
    title = element_text(size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10))

#violin plot
vplot<-df %>% 
  select(Exam_1, Exam_2, Midterm_Grade, Student_ID) %>% 
  rename(
    'Exam 1' = Exam_1,
    'Exam 2' = Exam_2,
    'Midterm' = Midterm_Grade) %>% 
  pivot_longer(
    -Student_ID,
    values_to = 'score', 
    names_to = "exam") %>% 
  ggplot(aes(x = exam, y=score)) +
  geom_violin(
    fill=("steelblue4"), 
    alpha=0.7) +
  geom_jitter(
    pch=19, 
    alpha=0.5,
    position = position_jitter(0.1))+
  theme_bw() +
  ggtitle("C) Violin Plots of Grades") +
  ylab("Grades [%]") + 
  xlab(NULL) + 
  ylim(c(50,100)) +
  theme(
    title = element_text(size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)) + coord_flip()

#1:1 Plot
one2one<-df %>% 
  ggplot(aes(x=Exam_1, y=Exam_2)) +
  geom_abline(intercept = 0, slope = 1, col="grey30", lty=2) +
  geom_point(pch=21, col='grey30',fill="steelblue4", alpha=0.7, cex=3) + 
  theme_bw()+
  ggtitle("D) Exam Grades") +
  xlim(c(40,110)) + 
  ylim(c(40,110)) + 
  xlab("Exam 1 [%]") + 
  ylab("Exam 2 [%]") +
  theme(
    title = element_text(size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10))

#Slope plot
df %>% 
  select(Student_ID, Exam_1, Exam_2) %>% 
  pivot_longer(-Student_ID, names_to = 'Exam', values_to = 'Grade') %>% 
  ggplot(aes(x=Exam, y=Grade, group=Student_ID)) +
  geom_line() + 
  geom_point() +
  theme_bw() +
  theme(
    title = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12))

#
png("docs/grades.png", width=10, height = 7, units="in", res=200)
midterm + vplot + exam_2 + one2one +plot_layout(nrow=2)
dev.off()
