# This script was created by Heather in March 2021 to compile all data analyses
# for David's Variability project.
# Naming notes:
# HYA - healthy young adults
# HOA - healthy old adults
# MCI - mild cognitive impairment
# SCM - subjective memory complaints
# 'reaction' = Motor Processing
# 'cpt' = Attention
# 'complex_reasoning' = Abstract Matching
library(ggpubr)
library(gt)
library(data.table)
library(gridExtra)
# Need this for Levene's Test:
library(car)
# Need this for cohensD/etaSquared:
library(lsr)

# OLD ADULT DATA
# Set your working directory for wherever the old adult data live.
setwd("/Users/hayurob/Documents/_Roalf_Lab/7T_GluCEST_Age/PrevData/IIV_in_IOA/ADCOG")
# Set the following sources so you can use the functions below (summarySE, makeDF, makeRawTable
# mainANOVA, multipleComparisons)
source("summary_se_function.R")
source("other_functions.R")

# Load the demographics csv
# demos<-read.csv("ADcog_demos.csv")
demos<-read.csv("updated_demos.csv")

# Make DX_graph numeric, which will allow you to replace the values
# Make a new column with Group being either HOA or AOA (both MCI and SCM)
# demos$DX_new<-as.numeric(demos$DX_new)
# demos$DX_graph<-as.numeric(demos$DX_graph)
# demos$Group<-demos$DX_new
# demos$Group[demos$DX_new==2]<- 'HOA'
# demos$Group[demos$DX_new==3]<- 'AOA'
# demos$Group[demos$DX_new==4]<- 'AOA'
# Replace DX_graph values with either HOA, MCI, or SCM
# demos$DX_graph[demos$DX_new==2]<-'HOA'
# demos$DX_graph[demos$DX_new==3]<-'MCI'
# demos$DX_graph[demos$DX_new==4]<-'SCM'
# Replace numbers with corresponding gender/race values
# demos$Gender<-demos$Sex
# demos$Gender[demos$Sex==1]<- 'Male'
# demos$Gender[demos$Sex==2]<- 'Female'
# demos$Race[demos$Race==1]<- 'White'
# demos$Race[demos$Race==2]<- 'Black/African American'
# demos$Race[demos$Race==5]<- 'Multiracial'
# Make a new column with Group_2 being either HOA (HOA and SCM) or MCI
# demos$Group_2<-demos$DX_new
# demos$Group_2[demos$DX_new==2]<- 'HOA'
# demos$Group_2[demos$DX_new==3]<- 'MCI'
# demos$Group_2[demos$DX_new==4]<- 'HOA'
# write.csv(demos,file='/Users/hayurob/Documents/_Roalf_Lab/7T_GluCEST_Age/PrevData/IIV_in_IOA/ADCOG/updated_demos.csv')

# Set all the grouping variables as factors for following analyses.
demos$Group<-as.factor(demos$Group)
demos$DX_graph<-as.factor(demos$DX_graph)
demos$Group_2<-as.factor(demos$Group_2)

# Set new working directory to use the IIV scores from new calculation (IIV using
# all subjects).
setwd("/Users/hayurob/Documents/_Roalf_Lab/7T_GluCEST_Age/PrevData/IIV_in_IOA/IIV_CALC_ALL")

# Create strings of the tasks in order to use them for the following function, makeDF
reaction<-'reaction'
choice_reaction<-'choice_reaction'
simple_processing<-'simple_processing_speed'
attention<-'cpt'
complex_processing<-'complex_processing_speed'
abstract_matching<-'complex_reasoning'

# Make dataframes for each task, using the variables we just created.
# makeDF(x) where x is a string of the task (string comes from what the csv file is named)
reaction<-makeDF(reaction)
# There's one outlier, check with David, but right now set as NA
reaction$median_rt[18]<-NA
########
choice_reaction<-makeDF(choice_reaction)
simple_processing<-makeDF(simple_processing)
attention<-makeDF(attention)
complex_processing<-makeDF(complex_processing)
abstract_matching<-makeDF(abstract_matching)
# There's one outlier, check with David, but right now set as NA
abstract_matching$median_rt[18]<-NA

# Make raw data tables for each task and for each grouping, with Group being HOA v AOA (MCI + SCM),
# DX_graph being HOA v MCI v SCM, Group_2 being HOA (HOA + SCM) v MCI.
# makeRawTable(x,group,name) were x is the df, group is a string of the grouping variable (either 
# 'Group', 'DX_new', or 'Group_2), and name is a string of the task name for the title.
# The output is a raw data table that is saved as an rtf in your working directory.
makeRawTable(reaction,'Group','Motor Processing')
makeRawTable(reaction,'DX_graph','Motor Processing')
makeRawTable(reaction,'Group_2','Motor Processing')
makeRawTable(choice_reaction,'Group','Choice Reaction')
makeRawTable(choice_reaction,'DX_graph','Choice Reaction')
makeRawTable(choice_reaction,'Group_2','Choice Reaction')
makeRawTable(simple_processing,'Group','Simple Processing')
makeRawTable(simple_processing,'DX_graph','Simple Processing')
makeRawTable(simple_processing,'Group_2','Simple Processing')
makeRawTable(attention,'Group','Attention')
makeRawTable(attention,'DX_graph','Attention')
makeRawTable(attention,'Group_2','Attention')
makeRawTable(complex_processing,'Group','Complex Processing')
makeRawTable(complex_processing,'DX_graph','Complex Processing')
makeRawTable(complex_processing,'Group_2','Complex Processing')
makeRawTable(abstract_matching,'Group','Abstract Matching')
makeRawTable(abstract_matching,'DX_graph','Abstract Matching')
makeRawTable(abstract_matching,'Group_2','Abstract Matching')

# Do ANOVA for each task and for each grouping.
# mainANOVA(x,group,name) - same as makeRawTable.
# The output is an anova table that is saved as an rtf in your working directory.
mainANOVA(reaction,'Group','Motor Processing')
mainANOVA(reaction,'DX_graph','Motor Processing')
mainANOVA(reaction,'Group_2','Motor Processing')
mainANOVA(choice_reaction,'Group','Choice Reaction')
mainANOVA(choice_reaction,'DX_graph','Choice Reaction')
mainANOVA(choice_reaction,'Group_2','Choice Reaction')
mainANOVA(simple_processing,'Group','Simple Processing')
mainANOVA(simple_processing,'DX_graph','Simple Processing')
mainANOVA(simple_processing,'Group_2','Simple Processing')
mainANOVA(attention,'Group','Attention')
mainANOVA(attention,'DX_graph','Attention')
mainANOVA(attention,'Group_2','Attention')
mainANOVA(complex_processing,'Group','Complex Processing')
mainANOVA(complex_processing,'DX_graph','Complex Processing')
mainANOVA(complex_processing,'Group_2','Complex Processing')
mainANOVA(abstract_matching,'Group','Abstract Matching')
mainANOVA(abstract_matching,'DX_graph','Abstract Matching')
mainANOVA(abstract_matching,'Group_2','Abstract Matching')

# Follow up comparisons looking at HOA v MCI v SCM, add healthy data
# Follow up on directionality
multipleComparisons(reaction,'DX_graph')
multipleComparisons(choice_reaction,'DX_graph')
multipleComparisons(simple_processing,'DX_graph')
multipleComparisons(attention,'DX_graph')
multipleComparisons(complex_processing,'DX_graph')
multipleComparisons(abstract_matching,'DX_graph')

# Effect Sizes
cohen_all<-data.frame(Value=integer(),Group=character(),Task=character(),Type=character())
# hoa<-subset(reaction, DX_graph == 'HOA')
# mci<-subset(reaction, DX_graph == 'MCI')
# scm<-subset(reaction, DX_graph == 'SCM')
# hoa<-subset(choice_reaction, DX_graph == 'HOA')
# mci<-subset(choice_reaction, DX_graph == 'MCI')
# scm<-subset(choice_reaction, DX_graph == 'SCM')
# hoa<-subset(simple_processing, DX_graph == 'HOA')
# mci<-subset(simple_processing, DX_graph == 'MCI')
# scm<-subset(simple_processing, DX_graph == 'SCM')
# hoa<-subset(attention, DX_graph == 'HOA')
# mci<-subset(attention, DX_graph == 'MCI')
# scm<-subset(attention, DX_graph == 'SCM')
# hoa<-subset(complex_processing, DX_graph == 'HOA')
# mci<-subset(complex_processing, DX_graph == 'MCI')
# scm<-subset(complex_processing, DX_graph == 'SCM')
# hoa<-subset(abstract_matching, DX_graph == 'HOA')
# mci<-subset(abstract_matching, DX_graph == 'MCI')
# scm<-subset(abstract_matching, DX_graph == 'SCM')

# dv_type<-c('percent_correct','rtcr','IIV')
#   for (i in dv_type) {
#     print(i)
#     cohens_d1<-cohensD(x=hoa[[i]],y=mci[[i]])
#     cohens_d2<-cohensD(x=scm[[i]],y=mci[[i]])
#     cohens_d3<-cohensD(x=hoa[[i]],y=scm[[i]])
#     d1<-c(cohens_d1,'HOA v MCI','abstract_matching',paste(i,'',sep = ''))
#     d2<-c(cohens_d2,'SCM v MCI','abstract_matching',paste(i,'',sep = ''))
#     d3<-c(cohens_d3,'HOA v SCM','abstract_matching',paste(i,'',sep = ''))
#     cohen_all<-rbind(cohen_all,d1,d2,d3)
#     colnames(cohen_all)<-c('Value','Group','Task','Type')
#   }

cohen_all$Value<-as.numeric(cohen_all$Value)

# hoavmci_d<-subset(cohen_all,Group == 'HOA v MCI')
# hoavmci_iiv<-subset(hoavmci_d,Type == 'IIV')
# avg_hoavmci_iiv<-mean(hoavmci_iiv$Value)
# hoavmci_iiv<-rbind(hoavmci_iiv,avg_hoavmci_iiv)
# hoavmci_iiv$Group[7]<-'HOA v MCI'
# hoavmci_iiv$Task[7]<-'AVERAGE'
# hoavmci_iiv$Type[7]<-'IIV'
# write.csv(hoavmci_iiv, file = 'hoavmci_iiv_effectsize.csv', row.names = FALSE)
# 
# hoavmci_rt<-subset(hoavmci_d,Type == 'median_rt' | Type == 'rtcr')
# avg_hoavmci_rt<-mean(hoavmci_rt$Value)
# hoavmci_rt<-rbind(hoavmci_rt,avg_hoavmci_rt)
# hoavmci_rt$Group[7]<-'HOA v MCI'
# hoavmci_rt$Task[7]<-'AVERAGE'
# hoavmci_rt$Type[7]<-'rt'
# write.csv(hoavmci_rt, file = 'hoavmci_rt_effectsize.csv', row.names = FALSE)
# 
# hoavmci_pc<-subset(hoavmci_d,Type == 'percent_correct')
# avg_hoavmci_pc<-mean(hoavmci_pc$Value)
# hoavmci_pc<-rbind(hoavmci_pc,avg_hoavmci_pc)
# hoavmci_pc$Group[6]<-'HOA v MCI'
# hoavmci_pc$Task[6]<-'AVERAGE'
# hoavmci_pc$Type[6]<-'percent_correct'
# write.csv(hoavmci_pc, file = 'hoavmci_pc_effectsize.csv', row.names = FALSE)
# 
# hoavscm_d<-subset(cohen_all,Group == 'HOA v SCM')
# hoavscm_iiv<-subset(hoavscm_d,Type == 'IIV')
# avg_hoavscm_iiv<-mean(hoavscm_iiv$Value)
# hoavscm_iiv<-rbind(hoavscm_iiv,avg_hoavscm_iiv)
# hoavscm_iiv$Group[7]<-'HOA v SCM'
# hoavscm_iiv$Task[7]<-'AVERAGE'
# hoavscm_iiv$Type[7]<-'IIV'
# write.csv(hoavscm_iiv, file = 'hoavscm_iiv_effectsize.csv', row.names = FALSE)
# 
# hoavscm_rt<-subset(hoavscm_d,Type == 'median_rt' | Type == 'rtcr')
# avg_hoavscm_rt<-mean(hoavscm_rt$Value)
# hoavscm_rt<-rbind(hoavscm_rt,avg_hoavscm_rt)
# hoavscm_rt$Group[7]<-'HOA v SCM'
# hoavscm_rt$Task[7]<-'AVERAGE'
# hoavscm_rt$Type[7]<-'rt'
# write.csv(hoavscm_rt, file = 'hoavscm_rt_effectsize.csv', row.names = FALSE)
# 
# hoavscm_pc<-subset(hoavscm_d,Type == 'percent_correct')
# avg_hoavscm_pc<-mean(hoavscm_pc$Value)
# hoavscm_pc<-rbind(hoavscm_pc,avg_hoavscm_pc)
# hoavscm_pc$Group[6]<-'HOA v SCM'
# hoavscm_pc$Task[6]<-'AVERAGE'
# hoavscm_pc$Type[6]<-'percent_correct'
# write.csv(hoavscm_pc, file = 'hoavscm_pc_effectsize.csv', row.names = FALSE)
# 
# mcivscm_d<-subset(cohen_all,Group == 'SCM v MCI')
# mcivscm_iiv<-subset(mcivscm_d,Type == 'IIV')
# avg_mcivscm_iiv<-mean(mcivscm_iiv$Value)
# mcivscm_iiv<-rbind(mcivscm_iiv,avg_mcivscm_iiv)
# mcivscm_iiv$Group[7]<-'MCI v SCM'
# mcivscm_iiv$Task[7]<-'AVERAGE'
# mcivscm_iiv$Type[7]<-'IIV'
# write.csv(mcivscm_iiv, file = 'mcivscm_iiv_effectsize.csv', row.names = FALSE)
# 
# mcivscm_rt<-subset(mcivscm_d,Type == 'median_rt' | Type == 'rtcr')
# avg_mcivscm_rt<-mean(mcivscm_rt$Value)
# mcivscm_rt<-rbind(mcivscm_rt,avg_mcivscm_rt)
# mcivscm_rt$Group[7]<-'MCI v SCM'
# mcivscm_rt$Task[7]<-'AVERAGE'
# mcivscm_rt$Type[7]<-'rt'
# write.csv(mcivscm_rt, file = 'mcivscm_rt_effectsize.csv', row.names = FALSE)
# 
# mcivscm_pc<-subset(mcivscm_d,Type == 'percent_correct')
# avg_mcivscm_pc<-mean(mcivscm_pc$Value)
# mcivscm_pc<-rbind(mcivscm_pc,avg_mcivscm_pc)
# mcivscm_pc$Group[6]<-'SCM v MCI'
# mcivscm_pc$Task[6]<-'AVERAGE'
# mcivscm_pc$Type[6]<-'percent_correct'
# write.csv(mcivscm_pc, file = 'mcivscm_pc_effectsize.csv', row.names = FALSE)


source("other_functions.R")

# Make bar plot for HOA v MCI v SCM
# Load the csv files. NOTE: need to update these csv files with correct values.
reaction_time<-read.csv("reaction_time_graph.csv")
iiv<-read.csv("iiv_graph.csv")
percent_correct<-read.csv("percent_correct_graph.csv")

# Update reaction time and IIV graph values - only have to do once
# rt_cr<-summarySE(data=choice_reaction, measurevar="rtcr", groupvars='DX_graph', na.rm=T)
# iiv_cr<-summarySE(data=choice_reaction, measurevar="IIV", groupvars='DX_graph', na.rm=T)
# 
# rt_cp<-summarySE(data=complex_processing, measurevar="rtcr", groupvars='DX_graph', na.rm=T)
# iiv_cp<-summarySE(data=complex_processing, measurevar="IIV", groupvars='DX_graph', na.rm=T)
# 
# rt_am<-summarySE(data=abstract_matching, measurevar="rtcr", groupvars='DX_graph', na.rm=T)
# iiv_am<-summarySE(data=abstract_matching, measurevar="IIV", groupvars='DX_graph', na.rm=T)
# 
# rt_a<-summarySE(data=attention, measurevar="rtcr", groupvars='DX_graph', na.rm=T)
# iiv_a<-summarySE(data=attention, measurevar="IIV", groupvars='DX_graph', na.rm=T)
# 
# rt_r<-summarySE(data=reaction, measurevar="median_rt", groupvars='DX_graph', na.rm=T)
# iiv_r<-summarySE(data=reaction, measurevar="IIV", groupvars='DX_graph', na.rm=T)
# 
# rt_sp<-summarySE(data=simple_processing, measurevar="rtcr", groupvars='DX_graph', na.rm=T)
# iiv_sp<-summarySE(data=simple_processing, measurevar="IIV", groupvars='DX_graph', na.rm=T)
# 
# rt<-c(rt_cr$rtcr,rt_cp$rtcr,rt_am$rtcr,rt_a$rtcr,rt_r$median_rt,rt_sp$rtcr)
# rt<-round(rt,digits = 4)
# rt_se<-c(rt_cr$se,rt_cp$se,rt_am$se,rt_a$se,rt_r$se,rt_sp$se)
# rt_se<-round(rt_se,digits = 4)
# reaction_time$Value<-rt
# reaction_time$SE<-rt_se
# write.csv(reaction_time, file = 'reaction_time_graph.csv', row.names = FALSE)

# iiv_replace<-c(iiv_cr$IIV,iiv_cp$IIV,iiv_am$IIV,iiv_a$IIV,iiv_r$IIV,iiv_sp$IIV)
# iiv_replace<-round(iiv_replace,digits = 4)
# iiv_se<-c(iiv_cr$se,iiv_cp$se,iiv_am$se,iiv_a$se,iiv_r$se,iiv_sp$se)
# iiv_se<-round(iiv_se,digits = 4)
# iiv$Value<-iiv_replace
# iiv$SE<-iiv_se
# write.csv(iiv, file = 'iiv_graph.csv', row.names = FALSE)

# Prep the df for making the plot (i.e., changing task names to their correct names, 
# ranking them from easiest to hardest)
# reaction_time$Task<-as.character(reaction_time$Task)
# reaction_time[7:9,2]<-'Abstract Matching'
# reaction_time[10:12,2]<-'Attention'
# reaction_time[13:15,2]<-'Motor Processing'
# reaction_time$rank<-c(3,3,3,5,5,5,6,6,6,2,2,2,1,1,1,4,4,4)
# 
# iiv$Task<-as.character(iiv$Task)
# iiv$Group<-as.character(iiv$Group)
# iiv[7:9,2]<-'Abstract Matching'
# iiv[10:12,2]<-'Attention'
# iiv[13:15,2]<-'Motor Processing'
# iiv$rank<-c(3,3,3,5,5,5,6,6,6,2,2,2,1,1,1,4,4,4)
# iiv$Group[iiv$Group=='2SCM']<-'3SCM'
# iiv$Group[iiv$Group=='3MCI']<-'2MCI'

# Save bar plots as PDFs
pdf("/Users/hayurob/Documents/_Roalf_Lab/7T_GluCEST_Age/PrevData/IIV_in_IOA/IIV_CALC_ALL/rt_old_3_plot.pdf")
reaction_time_graph<-ggplot(reaction_time, aes(x=reorder(Task, rank), y=Value, fill=Group)) + 
  geom_bar(stat="identity", position="dodge", width=0.6) + xlab("Cognitive Test") + 
  ylab("Reaction Time") + geom_errorbar(aes(ymin=Value-SE, ymax=Value+SE), position="dodge", stat="identity", width=0.6) + 
  scale_fill_manual(name="Group", breaks=c("1HOA", "2MCI","3SCM"), labels=c("Healthy Old Adults", "Mild Cognitive Impairment","Subjective Memory Complaints"), 
                    values=c("skyblue1", "palevioletred4","lightsteelblue3")) + 
  geom_hline(yintercept=0)+theme(axis.text.x=element_text(angle=35, vjust=0.6,size=10),axis.text.y=element_text(size=10),
                                 axis.title.y =element_text(margin = margin(t=0,r=20,b=0,l=0),size=15),
                                 axis.title.x =element_text(margin = margin(t=20,r=0,b=0,l=0),size=15), 
                                 legend.position = c(0.3,0.8), legend.text = element_text(size=10)) + 
  scale_y_continuous(limits = c(NA, 3000)) +
  geom_signif(y_position=c(1000),xmin=c(2.8),xmax=c(3.2),annotation=c('*'),tip_length=0,textsize=6)
reaction_time_graph
dev.off()

pdf("/Users/hayurob/Documents/_Roalf_Lab/7T_GluCEST_Age/PrevData/IIV_in_IOA/IIV_CALC_ALL/iiv_old_3_plot.pdf")
iiv_graph<-ggplot(iiv, aes(x=reorder(Task, rank), y=Value, fill=Group)) + 
  geom_bar(stat="identity", position="dodge", width=0.6) + xlab("Cognitive Test") + 
  ylab("Intra-Individual Variability") + geom_errorbar(aes(ymin=Value-SE, ymax=Value+SE), position="dodge", stat="identity", width=0.6) + 
  scale_fill_manual(name="Group", breaks=c("1HOA", "2MCI","3SCM"), labels=c("Healthy Old Adults", "Mild Cognitive Impairment","Subjective Memory Complaints"), 
                    values=c("skyblue1", "palevioletred4","lightsteelblue3")) + 
  geom_hline(yintercept=0) + theme(axis.text.x=element_text(angle=35, vjust=0.6,size=10),axis.text.y=element_text(size=10),
                                   axis.title.y =element_text(margin = margin(t=0,r=20,b=0,l=0),size=15),
                                   axis.title.x =element_text(margin = margin(t=20,r=0,b=0,l=0),size=15), 
                                   legend.position = c(0.25,0.875), legend.text = element_text(size=10)) + 
  scale_y_continuous(limits = c(NA, 1.9)) + 
  geom_signif(y_position=c(1.6),xmin=c(5.8),xmax=c(6.2),annotation=c('*'),tip_length=0,textsize=6)
iiv_graph 
dev.off()

pdf("/Users/hayurob/Documents/_Roalf_Lab/7T_GluCEST_Age/PrevData/IIV_in_IOA/IIV_CALC_ALL/pc_old_3_plot.pdf")
pc_graph<-ggplot(percent_correct, aes(x=reorder(Task, rank), y=Value, fill=Group)) + 
  geom_bar(stat="identity", position="dodge", width=0.6) + xlab("Cognitive Test") + 
  ylab("Percent Correct") + geom_errorbar(aes(ymin=Value-SE, ymax=Value+SE), position="dodge", stat="identity", width=0.6) + 
  scale_fill_manual(name="Group", breaks=c("1HOA", "2MCI","3SCM"), labels=c("Healthy Old Adults", "Mild Cognitive Impairment","Subjective Memory Complaints"), 
                    values=c("skyblue1", "palevioletred4","lightsteelblue3")) + 
  geom_hline(yintercept=0) + theme(axis.text.x=element_text(angle=35, vjust=0.6,size=10),axis.text.y=element_text(size=10),
                                   axis.title.y =element_text(margin = margin(t=0,r=20,b=0,l=0),size=15),
                                   axis.title.x =element_text(margin = margin(t=20,r=0,b=0,l=0),size=15), 
                                   legend.position = c(0.25,0.875), legend.text = element_text(size=10)) + 
  scale_y_continuous(limits = c(NA, 1.3)) +
  geom_signif(y_position=c(1),xmin=c(3.8),xmax=c(4.2),annotation=c('*'),tip_length=0,textsize=6)
pc_graph 
dev.off()


# Make bar plot for HOA (incl SCM) v MCI
# Prep the data by getting summary stats for HOA including SCM and MCI for each task measure
# reaction_time<-data.frame()
# iiv<-data.frame()
# percent_correct<-data.frame()
# a<-summarySE(data=reaction, measurevar="median_rt", groupvars='Group_2', na.rm=T)
# a$task<-c('Motor Processing','Motor Processing')
# colnames(a)<-c("Group","N","Value","SD","SE","CI","Task")
# b<-summarySE(data=reaction, measurevar="IIV", groupvars='Group_2', na.rm=T)
# b$task<-c('Motor Processing','Motor Processing')
# colnames(b)<-c("Group","N","Value","SD","SE","CI","Task")
# 
# c<-summarySE(data=choice_reaction, measurevar="rtcr", groupvars='Group_2', na.rm=T)
# c$task<-c('Choice Reaction','Choice Reaction')
# colnames(c)<-c("Group","N","Value","SD","SE","CI","Task")
# d<-summarySE(data=choice_reaction, measurevar="IIV", groupvars='Group_2', na.rm=T)
# d$task<-c('Choice Reaction','Choice Reaction')
# colnames(d)<-c("Group","N","Value","SD","SE","CI","Task")
# 
# e<-summarySE(data=simple_processing, measurevar="rtcr", groupvars='Group_2', na.rm=T)
# e$task<-c('Simple Processing','Simple Processing')
# colnames(e)<-c("Group","N","Value","SD","SE","CI","Task")
# f<-summarySE(data=simple_processing, measurevar="IIV", groupvars='Group_2', na.rm=T)
# f$task<-c('Simple Processing','Simple Processing')
# colnames(f)<-c("Group","N","Value","SD","SE","CI","Task")
# 
# g<-summarySE(data=attention, measurevar="rtcr", groupvars='Group_2', na.rm=T)
# g$task<-c('Attention','Attention')
# colnames(g)<-c("Group","N","Value","SD","SE","CI","Task")
# h<-summarySE(data=attention, measurevar="IIV", groupvars='Group_2', na.rm=T)
# h$task<-c('Attention','Attention')
# colnames(h)<-c("Group","N","Value","SD","SE","CI","Task")
# 
# i<-summarySE(data=complex_processing, measurevar="rtcr", groupvars='Group_2', na.rm=T)
# i$task<-c('Complex Processing','Complex Processing')
# colnames(i)<-c("Group","N","Value","SD","SE","CI","Task")
# j<-summarySE(data=complex_processing, measurevar="IIV", groupvars='Group_2', na.rm=T)
# j$task<-c('Complex Processing','Complex Processing')
# colnames(j)<-c("Group","N","Value","SD","SE","CI","Task")
# 
# k<-summarySE(data=abstract_matching, measurevar="rtcr", groupvars='Group_2', na.rm=T)
# k$task<-c('Abstract Matching','Abstract Matching')
# colnames(k)<-c("Group","N","Value","SD","SE","CI","Task")
# l<-summarySE(data=abstract_matching, measurevar="IIV", groupvars='Group_2', na.rm=T)
# l$task<-c('Abstract Matching','Abstract Matching')
# colnames(l)<-c("Group","N","Value","SD","SE","CI","Task")
# 
# m<-summarySE(data=choice_reaction, measurevar="percent_correct", groupvars='Group_2', na.rm=T)
# m$task<-c('Choice Reaction','Choice Reaction')
# colnames(m)<-c("Group","N","Value","SD","SE","CI","Task")
# 
# n<-summarySE(data=simple_processing, measurevar="percent_correct", groupvars='Group_2', na.rm=T)
# n$task<-c('Simple Processing','Simple Processing')
# colnames(n)<-c("Group","N","Value","SD","SE","CI","Task")
# 
# o<-summarySE(data=attention, measurevar="percent_correct", groupvars='Group_2', na.rm=T)
# o$task<-c('Attention','Attention')
# colnames(o)<-c("Group","N","Value","SD","SE","CI","Task")
# 
# p<-summarySE(data=complex_processing, measurevar="percent_correct", groupvars='Group_2', na.rm=T)
# p$task<-c('Complex Processing','Complex Processing')
# colnames(p)<-c("Group","N","Value","SD","SE","CI","Task")
# 
# q<-summarySE(data=abstract_matching, measurevar="percent_correct", groupvars='Group_2', na.rm=T)
# q$task<-c('Abstract Matching','Abstract Matching')
# colnames(q)<-c("Group","N","Value","SD","SE","CI","Task")

# Put all the reaction time/IIV summary stats in one dataframe.
# Rank the tasks based on difficulty
# reaction_time<-rbind.data.frame(a,c,e,g,i,k)
# reaction_time$rank<-c(1,1,3,3,4,4,2,2,5,5,6,6)
# write.csv(reaction_time, file = 'rt_graph_2groups.csv', row.names = FALSE)
# iiv<-rbind.data.frame(b,d,f,h,j,l)
# iiv$rank<-c(1,1,3,3,4,4,2,2,5,5,6,6)
# write.csv(iiv, file = 'iiv_graph_2groups.csv', row.names = FALSE)
# percent_correct<-rbind.data.frame(m,n,o,p,q)
# percent_correct$rank<-c(2,2,3,3,1,1,4,4,5,5)
# write.csv(percent_correct, file = 'pc_graph_2groups.csv', row.names = FALSE)

reaction_time<-read.csv("rt_graph_2groups.csv")
iiv<-read.csv("iiv_graph_2groups.csv")
percent_correct<-read.csv("pc_graph_2groups.csv")

pdf("/Users/hayurob/Documents/_Roalf_Lab/7T_GluCEST_Age/PrevData/IIV_in_IOA/IIV_CALC_ALL/rt_old_2_plot.pdf")
reaction_time_graph<-ggplot(reaction_time, aes(x=reorder(Task, rank), y=Value, fill=Group)) + 
  geom_bar(stat="identity", position="dodge", width=0.6) + xlab("Cognitive Test") + 
  ylab("Reaction Time") + geom_errorbar(aes(ymin=Value-SE, ymax=Value+SE), position="dodge", stat="identity", width=0.6) + 
  scale_fill_manual(name="Group", breaks=c("HOA", "MCI"), labels=c("Healthy Old Adults (w/SMC)", "Mild Cognitive Impairment"), 
                    values=c("skyblue2", "palevioletred4")) + 
  geom_hline(yintercept=0)+theme(axis.text.x=element_text(angle=35, vjust=0.6,size=10),axis.text.y=element_text(size=10),
                                 axis.title.y =element_text(margin = margin(t=0,r=20,b=0,l=0),size=15),
                                 axis.title.x =element_text(margin = margin(t=20,r=0,b=0,l=0),size=15), 
                                 legend.position = c(0.3,0.8), legend.text = element_text(size=10)) + 
  scale_y_continuous(limits = c(NA, 3000)) +
  geom_signif(y_position=c(1000),xmin=c(2.8),xmax=c(3.2),annotation=c('*'),tip_length=0,textsize=6)
reaction_time_graph
dev.off()

pdf("/Users/hayurob/Documents/_Roalf_Lab/7T_GluCEST_Age/PrevData/IIV_in_IOA/IIV_CALC_ALL/iiv_old_2_plot.pdf")
iiv_graph<-ggplot(iiv, aes(x=reorder(Task, rank), y=Value, fill=Group)) + 
  geom_bar(stat="identity", position="dodge", width=0.6) + xlab("Cognitive Test") + 
  ylab("Intra-Individual Variability") + geom_errorbar(aes(ymin=Value-SE, ymax=Value+SE), position="dodge", stat="identity", width=0.6) + 
  scale_fill_manual(name="Group", breaks=c("HOA", "MCI"), labels=c("Healthy Old Adults (w/SMC)", "Mild Cognitive Impairment"), 
                    values=c("skyblue2", "palevioletred4")) + 
  geom_hline(yintercept=0) + theme(axis.text.x=element_text(angle=35, vjust=0.6,size=10),axis.text.y=element_text(size=10),
                                   axis.title.y =element_text(margin = margin(t=0,r=20,b=0,l=0),size=15),
                                   axis.title.x =element_text(margin = margin(t=20,r=0,b=0,l=0),size=15), 
                                   legend.position = c(0.25,0.875), legend.text = element_text(size=10)) + 
  scale_y_continuous(limits = c(NA, 1.9)) +
  geom_signif(y_position=c(1.6),xmin=c(5.8),xmax=c(6.2),annotation=c('*'),tip_length=0,textsize=6)
iiv_graph 
dev.off()

pdf("/Users/hayurob/Documents/_Roalf_Lab/7T_GluCEST_Age/PrevData/IIV_in_IOA/IIV_CALC_ALL/pc_old_2_plot.pdf")
pc_graph<-ggplot(percent_correct, aes(x=reorder(Task, rank), y=Value, fill=Group)) + 
  geom_bar(stat="identity", position="dodge", width=0.6) + xlab("Cognitive Test") + 
  ylab("Percent Correct") + geom_errorbar(aes(ymin=Value-SE, ymax=Value+SE), position="dodge", stat="identity", width=0.6) + 
  scale_fill_manual(name="Group", breaks=c("HOA", "MCI"), labels=c("Healthy Old Adults (w/SMC)", "Mild Cognitive Impairment"), 
                    values=c("skyblue2", "palevioletred4")) + 
  geom_hline(yintercept=0) + theme(axis.text.x=element_text(angle=35, vjust=0.6,size=10),axis.text.y=element_text(size=10),
                                   axis.title.y =element_text(margin = margin(t=0,r=20,b=0,l=0),size=15),
                                   axis.title.x =element_text(margin = margin(t=20,r=0,b=0,l=0),size=15), 
                                   legend.position = c(0.25,0.875), legend.text = element_text(size=10)) + 
  scale_y_continuous(limits = c(NA, 1.3)) +
  geom_signif(y_position=c(1,1),xmin=c(0.8,3.8),xmax=c(1.2,4.2),annotation=c('*','*'),tip_length=0,textsize=6)
pc_graph 
dev.off()

# Correlations for old subjects - only do scatter plots for those that are significant.
# Start with CCI and other scales
cor.test(demos$CCI, demos$MoCA.Total) #ns
cor.test(demos$CCI, demos$MMSE.Score)
cor.test(demos$CCI, demos$GDSTotal)
cor.test(demos$CCI, demos$CERADTotal)

pdf("/Users/hayurob/Documents/_Roalf_Lab/7T_GluCEST_Age/PrevData/IIV_in_IOA/IIV_CALC_ALL/scales_cor_plot.pdf")
ggscatter(demos, x = "MoCA.Total", y = "CCI", 
          add = "reg.line", conf.int = TRUE, 
          xlab = "MoCA Score", ylab = "CCI Score") +
  stat_cor(method = "pearson", cor.coef.name =c('r'), label.x = 13, label.y = 35)

ggscatter(demos, x = "MMSE.Score", y = "CCI", 
          add = "reg.line", conf.int = TRUE, 
          xlab = "MMSE Score", ylab = "CCI Score") +
  stat_cor(method = "pearson", cor.coef.name =c('r'), label.x = 20, label.y = 35)

ggscatter(demos, x = "GDSTotal", y = "CCI", 
          add = "reg.line", conf.int = TRUE,
          xlab = "GDS Score", ylab = "CCI Score") +
  stat_cor(method = "pearson", cor.coef.name =c('r'), label.x = 12, label.y = 35)

ggscatter(demos, x = "CERADTotal", y = "CCI", 
          add = "reg.line", conf.int = TRUE, 
          xlab = "CERAD Score", ylab = "CCI Score") +
  stat_cor(method = "pearson", cor.coef.name =c('r'), label.x = 40, label.y = 75)
dev.off()

# Group by color plot:
pdf("/Users/hayurob/Documents/_Roalf_Lab/7T_GluCEST_Age/PrevData/IIV_in_IOA/IIV_CALC_ALL/scales_cor_plot_bygroup.pdf")
ggscatter(demos, x = "MoCA.Total", y = "CCI", 
          add = "reg.line", conf.int = FALSE, color = "Group_2",
          xlab = "MoCA Score", ylab = "CCI Score") +
  stat_cor(aes(color = Group_2, label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
           label.x = 13,cor.coef.name =c('r'), show.legend = FALSE) +
  scale_color_discrete(
    name = 'Group', 
    labels = c('HOA', 'MCI') 
  ) 

ggscatter(demos, x = "MMSE.Score", y = "CCI",
          add = "reg.line", conf.int = FALSE, color = "Group_2",
          xlab = "MMSE Score", ylab = "CCI Score") +  
  stat_cor(aes(color = Group_2, label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
           label.x = 19,cor.coef.name =c('r'), show.legend = FALSE) +
  scale_color_discrete(
    name = 'Group', 
    labels = c('HOA', 'MCI') 
  ) 

ggscatter(demos, x = "GDSTotal", y = "CCI",
          add = "reg.line", conf.int = FALSE, color = "Group_2",
          xlab = "GDS Score", ylab = "CCI Score") +  
  stat_cor(aes(color = Group_2, label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
           label.x = 9, cor.coef.name =c('r'), show.legend = FALSE) +
  scale_color_discrete(
    name = 'Group', 
    labels = c('HOA', 'MCI') 
  ) 

ggscatter(demos, x = "CERADTotal", y = "CCI",
          add = "reg.line", conf.int = FALSE, color = "Group_2",
          xlab = "CERAD Score", ylab = "CCI Score") +
  stat_cor(aes(color = Group_2, label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
           label.x = 39,cor.coef.name =c('r'), show.legend = FALSE) +
  scale_color_discrete(
    name = 'Group', 
    labels = c('HOA', 'MCI') 
  ) 
dev.off()


# Then do CCI and task measures using adjusted p-value of 0.008
cor.test(reaction$median_rt, reaction$CCI) #ns
cor.test(reaction$IIV, reaction$CCI) #ns

cor.test(choice_reaction$percent_correct, choice_reaction$CCI) #close - 0.0082
cor.test(choice_reaction$rtcr, choice_reaction$CCI) #ns
cor.test(choice_reaction$IIV, choice_reaction$CCI) #ns

cor.test(complex_processing$percent_correct, complex_processing$CCI)
cor.test(complex_processing$rtcr, complex_processing$CCI) #ns
cor.test(complex_processing$IIV, complex_processing$CCI) #ns

cor.test(abstract_matching$percent_correct, abstract_matching$CCI) #ns
cor.test(abstract_matching$rtcr, abstract_matching$CCI) #ns
cor.test(abstract_matching$IIV, abstract_matching$CCI)

cor.test(attention$percent_correct, attention$CCI) #ns
cor.test(attention$rtcr, attention$CCI) #ns
cor.test(attention$IIV, attention$CCI) #ns

cor.test(simple_processing$percent_correct, simple_processing$CCI) #ns
cor.test(simple_processing$rtcr, simple_processing$CCI) #ns
cor.test(simple_processing$IIV, simple_processing$CCI) #close - 0.0088

pdf("/Users/hayurob/Documents/_Roalf_Lab/7T_GluCEST_Age/PrevData/IIV_in_IOA/IIV_CALC_ALL/task_cor_plot.pdf")
ggscatter(choice_reaction, x = "percent_correct", y = "CCI", 
          add = "reg.line", conf.int = TRUE,
          xlab = "Choice Reaction Percent Correct", ylab = "CCI Score") +
  stat_cor(method = "pearson", cor.coef.name =c('r'), label.x = 0.5, label.y = 30)

ggscatter(complex_processing, x = "percent_correct", y = "CCI", 
          add = "reg.line", conf.int = TRUE,
          xlab = "Complex Processing Percent Correct", ylab = "CCI Score") +
  stat_cor(method = "pearson", cor.coef.name =c('r'), label.x = 0.5, label.y = 30)

ggscatter(abstract_matching, x = "IIV", y = "CCI", 
          add = "reg.line", conf.int = TRUE, 
          xlab = "Abstract Matching IIV", ylab = "CCI Score") +
  stat_cor(method = "pearson", cor.coef.name =c('r'), label.x = 2, label.y = 30)

ggscatter(simple_processing, x = "IIV", y = "CCI", 
          add = "reg.line", conf.int = TRUE, 
          xlab = "Simple Processing IIV", ylab = "CCI Score") +
  stat_cor(method = "pearson", cor.coef.name =c('r'), label.x = 1.5, label.y = 26)
dev.off()

# Group by color? - ask if we need
# pdf("/Users/hayurob/Documents/_Roalf_Lab/7T_GluCEST_Age/PrevData/IIV_in_IOA/IIV_CALC_ALL/task_cor_plot.pdf")
# ggscatter(choice_reaction, x = "percent_correct", y = "CCI", 
#           add = "reg.line", conf.int = FALSE, color = "Group_2",
#           xlab = "Choice Reaction Percent Correct", ylab = "CCI Score") +
#   stat_cor(method = "pearson", cor.coef.name =c('r'), label.x = 0.5, label.y = 30)
# 
# ggscatter(complex_processing, x = "percent_correct", y = "CCI", 
#           add = "reg.line", conf.int = TRUE,
#           xlab = "Complex Processing Percent Correct", ylab = "CCI Score") +
#   stat_cor(method = "pearson", cor.coef.name =c('r'), label.x = 0.5, label.y = 30)
# 
# ggscatter(abstract_matching, x = "IIV", y = "CCI", 
#           add = "reg.line", conf.int = TRUE, 
#           xlab = "Abstract Matching IIV", ylab = "CCI Score") +
#   stat_cor(method = "pearson", cor.coef.name =c('r'), label.x = 2, label.y = 30)
# 
# ggscatter(simple_processing, x = "IIV", y = "CCI", 
#           add = "reg.line", conf.int = TRUE, 
#           xlab = "Simple Processing IIV", ylab = "CCI Score") +
#   stat_cor(method = "pearson", cor.coef.name =c('r'), label.x = 1.5, label.y = 26)
# dev.off()

# Look at variability by task
t.test(reaction$IIV,attention$IIV)
t.test(reaction$IIV,choice_reaction$IIV)
t.test(reaction$IIV,simple_processing$IIV)
t.test(reaction$IIV,complex_processing$IIV)
t.test(reaction$IIV,abstract_matching$IIV)


# HEALTHY DATA - do the same as above, but with HOA v HYA
# Reset demos dataframe.
setwd("/Users/hayurob/Documents/_Roalf_Lab/7T_GluCEST_Age/PrevData/IIV_in_IOA/ADCOG")
demos<-read.csv("updated_demos.csv")

# Note: HOA data in this directory does not include SCM, so we must add it.
# Set working directory to wherever the healthy data live.
# Source should reference summary_se_function.R and practice.R so set that path to wherever it lives.

# source("/Users/hayurob/Documents/_Roalf_Lab/7T_GluCEST_Age/PrevData/IIV_in_IOA/ADCOG/summary_se_function.R")
# source("/Users/hayurob/Documents/_Roalf_Lab/7T_GluCEST_Age/PrevData/IIV_in_IOA/ADCOG/other_functions.R")

setwd("/Users/hayurob/Documents/_Roalf_Lab/7T_GluCEST_Age/PrevData/IIV_in_IOA/ADCOG/healthy")
# Demographics
demos_2<-read.csv("Age.csv")
CCI<-read.csv("healthy_CCI.csv")
demos_2<-merge(demos_2, CCI, by="Subject", all=T)
colnames(demos_2)<-c("Subject","age","group","Race","Gender","CCI")

demos<-merge(demos,demos_2,all = TRUE)
demos$Group_2[demos$group==1]<- 'HYA'
demos<-subset(demos, Group_2=='HYA' | Group_2=='HOA', select = c(Subject,CCI,age,Race,Gender,Group_2,group))

# reaction_healthy<-read.csv('reaction.csv')
setwd("/Users/hayurob/Documents/_Roalf_Lab/7T_GluCEST_Age/PrevData/IIV_in_IOA/IIV_CALC_ALL")

# Create strings of the tasks in order to use them for the following function, makeDF
reaction<-'reaction'
choice_reaction<-'choice_reaction'
simple_processing<-'simple_processing_speed'
attention<-'cpt'
complex_processing<-'complex_processing_speed'
abstract_matching<-'complex_reasoning'

# Make data frames for each task, using the variables we just created.
# Note: have to add SCM to these functions
reaction<-makeDF(reaction)
# There's one outlier, check with David, but right now set as NA
reaction$median_rt[16]<-NA
choice_reaction<-makeDF(choice_reaction)
simple_processing<-makeDF(simple_processing)
attention<-makeDF(attention)
complex_processing<-makeDF(complex_processing)
abstract_matching<-makeDF(abstract_matching)
# There's one outlier, check with David, but right now set as NA
abstract_matching$median_rt[16]<-NA

# Make raw data tables for each task with group being HOA v HYA
makeRawTable(reaction,'Group_2','Motor Processing')
makeRawTable(choice_reaction,'Group_2','Choice Reaction')
makeRawTable(simple_processing,'Group_2','Simple Processing')
makeRawTable(attention,'Group_2','Attention')
makeRawTable(complex_processing,'Group_2','Complex Processing')
makeRawTable(abstract_matching,'Group_2','Abstract Matching')

# Do ANOVA for each task.
# Made a separate function for healthy because Age is not used as a covariate for this group.
healthyANOVA(reaction,'Group_2','Motor Processing')
healthyANOVA(choice_reaction,'Group_2','Choice Reaction')
healthyANOVA(simple_processing,'Group_2','Simple Processing')
healthyANOVA(attention,'Group_2','Attention')
healthyANOVA(complex_processing,'Group_2','Complex Processing')
healthyANOVA(abstract_matching,'Group_2','Abstract Matching')

# Do effect sizes for healthy data
cohen_all<-data.frame(Value=integer(),Group=character(),Task=character(),Type=character())
# hoa<-subset(reaction, Group_2 == 'HOA')
# hya<-subset(reaction, Group_2 == 'HYA')
# hoa<-subset(choice_reaction, Group_2 == 'HOA')
# hya<-subset(choice_reaction, Group_2 == 'HYA')
# hoa<-subset(simple_processing, Group_2 == 'HOA')
# hya<-subset(simple_processing, Group_2 == 'HYA')
# hoa<-subset(attention, Group_2 == 'HOA')
# hya<-subset(attention, Group_2 == 'HYA')
# hoa<-subset(complex_processing, Group_2 == 'HOA')
# hya<-subset(complex_processing, Group_2 == 'HYA')
# hoa<-subset(abstract_matching, Group_2 == 'HOA')
# hya<-subset(abstract_matching, Group_2 == 'HYA')
# 
# dv_type<-c('percent_correct','rtcr','IIV')
#   for (i in dv_type) {
#     print(i)
#     cohens<-cohensD(x=hoa[[i]],y=hya[[i]])
#     d1<-c(cohens,'HOA v HYA','abstract_matching',paste(i,'',sep = ''))
#     cohen_all<-rbind(cohen_all,d1)
#     colnames(cohen_all)<-c('Value','Group','Task','Type')
#   }

cohen_all$Value<-as.numeric(cohen_all$Value)

# hoavhya_iiv<-subset(cohen_all,Type == 'median_rt'|Type == 'rtcr')
# avg_hoavhya_iiv<-mean(hoavhya_iiv$Value)
# hoavhya_iiv<-rbind(hoavhya_iiv,avg_hoavhya_iiv)
# hoavhya_iiv$Group[7]<-'HOA v HYA'
# hoavhya_iiv$Task[7]<-'AVERAGE'
# hoavhya_iiv$Type[7]<-'reaction_time'
# write.csv(hoavhya_iiv, file = 'hoavhya_rt_effectsize.csv', row.names = FALSE)

# Make bar plot of healthy data
# Load the csv files.
reaction_time<-read.csv("reaction_time_graph_healthy.csv")
iiv<-read.csv("iiv_graph_healthy.csv")
percent_correct<-read.csv("percent_correct_graph_healthy.csv")

# Update reaction time and IIV graph values - only have to do once
# rt_cr<-summarySE(data=choice_reaction, measurevar="rtcr", groupvars='Group_2', na.rm=T)
# rt_cr<-rt_cr %>% arrange(N)
# iiv_cr<-summarySE(data=choice_reaction, measurevar="IIV", groupvars='Group_2', na.rm=T)
# iiv_cr<-iiv_cr %>% arrange(N)
# 
# rt_cp<-summarySE(data=complex_processing, measurevar="rtcr", groupvars='Group_2', na.rm=T)
# rt_cp<-rt_cp %>% arrange(N)
# iiv_cp<-summarySE(data=complex_processing, measurevar="IIV", groupvars='Group_2', na.rm=T)
# iiv_cp<-iiv_cp%>% arrange(N)
# 
# rt_am<-summarySE(data=abstract_matching, measurevar="rtcr", groupvars='Group_2', na.rm=T)
# rt_am<-rt_am %>% arrange(N)
# iiv_am<-summarySE(data=abstract_matching, measurevar="IIV", groupvars='Group_2', na.rm=T)
# iiv_am<-iiv_am %>% arrange(N)
# 
# rt_a<-summarySE(data=attention, measurevar="rtcr", groupvars='Group_2', na.rm=T)
# rt_a<-rt_a %>% arrange(N)
# iiv_a<-summarySE(data=attention, measurevar="IIV", groupvars='Group_2', na.rm=T)
# iiv_a<-iiv_a %>% arrange(N)
# 
# rt_r<-summarySE(data=reaction, measurevar="median_rt", groupvars='Group_2', na.rm=T)
# rt_r<-rt_r %>% arrange(N)
# iiv_r<-summarySE(data=reaction, measurevar="IIV", groupvars='Group_2', na.rm=T)
# iiv_r<-iiv_r %>% arrange(N)
# 
# rt_sp<-summarySE(data=simple_processing, measurevar="rtcr", groupvars='Group_2', na.rm=T)
# rt_sp<-rt_sp %>% arrange(N)
# iiv_sp<-summarySE(data=simple_processing, measurevar="IIV", groupvars='Group_2', na.rm=T)
# iiv_sp<-iiv_sp %>% arrange(N)
# 
# rt<-c(rt_cr$rtcr,rt_cp$rtcr,rt_am$rtcr,rt_a$rtcr,rt_r$median_rt,rt_sp$rtcr)
# rt<-round(rt,digits = 4)
# rt_se<-c(rt_cr$se,rt_cp$se,rt_am$se,rt_a$se,rt_r$se,rt_sp$se)
# rt_se<-round(rt_se,digits = 4)
# reaction_time$Value<-rt
# reaction_time$SE<-rt_se
# write.csv(reaction_time, file = 'reaction_time_graph_healthy.csv', row.names = FALSE)
# 
# iiv_replace<-c(iiv_cr$IIV,iiv_cp$IIV,iiv_am$IIV,iiv_a$IIV,iiv_r$IIV,iiv_sp$IIV)
# iiv_replace<-round(iiv_replace,digits = 4)
# iiv_se<-c(iiv_cr$se,iiv_cp$se,iiv_am$se,iiv_a$se,iiv_r$se,iiv_sp$se)
# iiv_se<-round(iiv_se,digits = 4)
# iiv$Value<-iiv_replace
# iiv$SE<-iiv_se
# write.csv(iiv, file = 'iiv_graph_healthy.csv', row.names = FALSE)

# Add correct names for tasks and order them based on difficulty.
# reaction_time$Name<-c('Choice Reaction','Choice Reaction','Complex Processing','Complex Processing',
#                       'Abstract Matching','Abstract Matching','Attention','Attention',
#                       'Motor Processing','Motor Processing','Simple Processing','Simple Processing')
# reaction_time$rank<-c(3,3,5,5,6,6,2,2,1,1,4,4)
# 
# iiv$Name<-c('Choice Reaction','Choice Reaction','Complex Processing','Complex Processing',
#             'Abstract Matching','Abstract Matching','Attention','Attention',
#             'Motor Processing','Motor Processing','Simple Processing','Simple Processing')
# iiv$rank<-c(3,3,5,5,6,6,2,2,1,1,4,4)

pdf("rt_barplot.pdf")
reaction_time_graph<-ggplot(reaction_time, aes(x= reorder(Name, rank), y=Value, fill=Group)) + 
  geom_bar(stat="Identity", position="dodge", width=0.6)+xlab("Cognitive Test") + 
  ylab("Reaction Time") + geom_errorbar(aes(ymin=Value-SE, ymax=Value+SE), position="dodge",stat="Identity", width=0.6) + 
  scale_fill_manual(name="Group", breaks=c("1HYA", "2HOA"), labels=c("Healthy Young Adults", "Healthy Old Adults (w/ SMC)"), 
                    values=c("lightseagreen","skyblue2")) + geom_hline(yintercept=0) + 
  theme(axis.text.x=element_text(angle=35, vjust=0.6, size=10),axis.text.y =element_text(size=10),
        axis.title.y =element_text(margin = margin(t=0,r=20,b=0,l=0),size=15),
        axis.title.x =element_text(margin = margin(t=20,r=0,b=0,l=0),size=15), 
        legend.position = c(0.3,0.8), legend.text = element_text(size=10)) + 
  scale_y_continuous(limits = c(NA, 3000)) +
  geom_signif(y_position=c(650,750,1250,800,2200),xmin=c(1.8,2.8,3.8,4.8,5.8),
              xmax=c(2.2,3.2,4.2,5.2,6.2),annotation=c('*','*','*','*','*'),tip_length=0,textsize=6)
reaction_time_graph
dev.off()

pdf("iiv_barplot.pdf")
iiv_graph<-ggplot(iiv, aes(x= reorder(Name, rank), y=Value, fill=Group)) + 
  geom_bar(stat="Identity", position="dodge", width=0.6)+xlab("Cognitive Test") + 
  ylab("Intra-Individual Variability") + geom_errorbar(aes(ymin=Value-SE, ymax=Value+SE), position="dodge",stat="Identity", width=0.6) + 
  scale_fill_manual(name="Group", breaks=c("1HYA", "2HOA"), labels=c("Healthy Young Adults", "Healthy Old Adults (w/SMC)"), 
                    values=c("lightseagreen","skyblue2")) + geom_hline(yintercept=0) + 
  theme(axis.text.x=element_text(angle=35, vjust=0.6,size=10),axis.text.y =element_text(size=10),
        axis.title.y =element_text(margin = margin(t=0,r=20,b=0,l=0),size=15),
        axis.title.x =element_text(margin = margin(t=20,r=0,b=0,l=0),size=15), 
        legend.position = c(0.25,0.875), legend.text = element_text(size=10)) + 
  scale_y_continuous(limits = c(NA, 1.9)) +
  geom_signif(y_position=c(0.9,1.25,1.25,1.25,1.25),xmin=c(0.8,2.8,3.8,4.8,5.8),
              xmax=c(1.2,3.2,4.2,5.2,6.2),annotation=c('*','*','*','*','*'),tip_length=0,textsize=6)
iiv_graph
dev.off()

pdf("pc_barplot.pdf")
percent_correct_graph<-ggplot(percent_correct, aes(x= reorder(Name, rank), y=Value, fill=Group)) + 
  geom_bar(stat="Identity", position="dodge", width=0.6)+xlab("Cognitive Test") + 
  ylab("Percent Correct") + geom_errorbar(aes(ymin=Value-SE, ymax=Value+SE), position="dodge",stat="Identity", width=0.6) + 
  scale_fill_manual(name="Group", breaks=c("1HYA", "2HOA"), labels=c("Healthy Young Adults", "Healthy Old Adults (w/SMC)"), 
                    values=c("lightseagreen","skyblue2")) + geom_hline(yintercept=0) + 
  theme(axis.text.x=element_text(angle=35, vjust=0.6,size = 10),axis.text.y =element_text(size=10),
        axis.title.y =element_text(margin = margin(t=0,r=20,b=0,l=0),size=15),
        axis.title.x =element_text(margin = margin(t=20,r=0,b=0,l=0),size=15), 
        legend.position = c(0.25,0.875), legend.text = element_text(size=10)) + 
  scale_y_continuous(limits = c(NA, 1.3)) +
  geom_signif(y_position=c(1),xmin=c(3.8),xmax=c(4.2),annotation=c('*'),tip_length=0,textsize=6)
percent_correct_graph
dev.off()

# Correlations for healthy subjects
# CCI and task measures using adjusted p-value of 0.008
cor.test(choice_reaction$percent_correct, choice_reaction$CCI) #ns
cor.test(choice_reaction$rtcr, choice_reaction$CCI) #ns
cor.test(choice_reaction$IIV, choice_reaction$CCI) #ns

cor.test(complex_processing$percent_correct, complex_processing$CCI) #ns
cor.test(complex_processing$rtcr, complex_processing$CCI) #ns
cor.test(complex_processing$IIV, complex_processing$CCI) #ns

cor.test(abstract_matching$percent_correct, abstract_matching$CCI) #ns
cor.test(abstract_matching$rtcr, abstract_matching$CCI) #ns
cor.test(abstract_matching$IIV, abstract_matching$CCI) #ns

cor.test(attention$percent_correct, attention$CCI) #ns
cor.test(attention$rtcr, attention$CCI) #ns
cor.test(attention$IIV, attention$CCI) #ns

cor.test(reaction$median_rt, reaction$CCI) #ns
cor.test(reaction$IIV, reaction$CCI) #ns

cor.test(simple_processing$percent_correct, simple_processing$CCI) #ns
cor.test(simple_processing$rtcr, simple_processing$CCI)
cor.test(simple_processing$IIV, simple_processing$CCI)

pdf("/Users/hayurob/Documents/_Roalf_Lab/7T_GluCEST_Age/PrevData/IIV_in_IOA/IIV_CALC_ALL/H_task_cor_plot.pdf")
ggscatter(simple_processing, x = "rtcr", y = "CCI", 
          add = "reg.line", conf.int = TRUE, 
          xlab = "Simple Processing Reaction Time", ylab = "CCI Score") +
  stat_cor(method = "pearson", cor.coef.name =c('r'), label.x = 500, label.y = 55)

plot<-ggscatter(simple_processing, x = "IIV", y = "CCI", 
          add = "reg.line", conf.int = TRUE, 
          xlab = "Simple Processing IIV", ylab = "CCI Score") +
  stat_cor(method = "pearson", cor.coef.name =c('r'), label.x = 0.5, label.y = 55)
ggpar(plot,xlim=c(0.5,1.75))
dev.off()

# Now separate out correlations by young and old
# Make old and young dataframes
old_choice_reaction<-subset(choice_reaction, Group_2 == 'HOA')
young_choice_reaction<-subset(choice_reaction, Group_2 == 'HYA')

old_complex_processing<-subset(complex_processing, Group_2 == 'HOA')
young_complex_processing<-subset(complex_processing, Group_2 == 'HYA')

old_abstract_matching<-subset(abstract_matching, Group_2 == 'HOA')
young_abstract_matching<-subset(abstract_matching, Group_2 == 'HYA')

old_attention<-subset(attention, Group_2 == 'HOA')
young_attention<-subset(attention, Group_2 == 'HYA')

old_reaction<-subset(reaction, Group_2 == 'HOA')
young_reaction<-subset(reaction, Group_2 == 'HYA')

old_simple_processing<-subset(simple_processing, Group_2 == 'HOA')
young_simple_processing<-subset(simple_processing, Group_2 == 'HYA')

# Old correlations - still using 0.008 threshold
cor.test(old_choice_reaction$percent_correct, old_choice_reaction$CCI) #ns
cor.test(old_choice_reaction$rtcr, old_choice_reaction$CCI) #ns
cor.test(old_choice_reaction$IIV, old_choice_reaction$CCI) #ns

cor.test(old_complex_processing$percent_correct, old_complex_processing$CCI) #ns
cor.test(old_complex_processing$rtcr, old_complex_processing$CCI) #ns
cor.test(old_complex_processing$IIV, old_complex_processing$CCI) #ns

cor.test(old_abstract_matching$percent_correct, old_abstract_matching$CCI) #ns
cor.test(old_abstract_matching$rtcr, old_abstract_matching$CCI) #ns
cor.test(old_abstract_matching$IIV, old_abstract_matching$CCI) #ns

cor.test(old_attention$percent_correct, old_attention$CCI) #ns
cor.test(old_attention$rtcr, old_attention$CCI) #ns
cor.test(old_attention$IIV, old_attention$CCI) #ns

cor.test(old_reaction$median_rt, old_reaction$CCI) #ns
cor.test(old_reaction$IIV, old_reaction$CCI) #ns

cor.test(old_simple_processing$percent_correct, old_simple_processing$CCI) #ns
cor.test(old_simple_processing$rtcr, old_simple_processing$CCI) #ns
cor.test(old_simple_processing$IIV, old_simple_processing$CCI) #ns

# Young correlations
cor.test(young_choice_reaction$percent_correct, young_choice_reaction$CCI) #ns
cor.test(young_choice_reaction$rtcr, young_choice_reaction$CCI) #ns
cor.test(young_choice_reaction$IIV, young_choice_reaction$CCI) 

pdf("/Users/hayurob/Documents/_Roalf_Lab/7T_GluCEST_Age/PrevData/IIV_in_IOA/IIV_CALC_ALL/HYA_task_cor_plot.pdf")
ggscatter(young_choice_reaction, x = "IIV", y = "CCI", 
          add = "reg.line", conf.int = TRUE, 
          xlab = "Choice Reaction IIV: HYA", ylab = "CCI Score") +
  stat_cor(method = "pearson", cor.coef.name =c('r'), label.x = 0.6, label.y = 65)
dev.off()

cor.test(young_complex_processing$percent_correct, young_complex_processing$CCI) #ns
cor.test(young_complex_processing$rtcr, young_complex_processing$CCI) #ns
cor.test(young_complex_processing$IIV, young_complex_processing$CCI) #ns

cor.test(young_abstract_matching$percent_correct, young_abstract_matching$CCI) #ns
cor.test(young_abstract_matching$rtcr, young_abstract_matching$CCI) #ns
cor.test(young_abstract_matching$IIV, young_abstract_matching$CCI) #ns

cor.test(young_attention$percent_correct, young_attention$CCI) #ns
cor.test(young_attention$rtcr, young_attention$CCI) #ns
cor.test(young_attention$IIV, young_attention$CCI) #ns

cor.test(young_reaction$median_rt, young_reaction$CCI) #ns
cor.test(young_reaction$IIV, young_reaction$CCI) #ns

cor.test(young_simple_processing$percent_correct, young_simple_processing$CCI) #ns
cor.test(young_simple_processing$rtcr, young_simple_processing$CCI) #ns
cor.test(young_simple_processing$IIV, young_simple_processing$CCI) #ns

# Look at variability by task
t.test(reaction$IIV,attention$IIV)
t.test(reaction$IIV,choice_reaction$IIV)
t.test(reaction$IIV,simple_processing$IIV)
t.test(reaction$IIV,complex_processing$IIV)
t.test(reaction$IIV,abstract_matching$IIV)

