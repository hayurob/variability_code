# This function creates a new data frame from csv files in your working directory.
# x = a string of the task name
# All tasks need their percent correct calculated EXCEPT reaction (aka motor processing)
makeDF <- function(x) {
  x_IIV<-read.csv( paste(x,'_IIV.csv',sep="") )
  x_IIV$Subject<-sub('(_).*$', '', x_IIV$Subject, perl=TRUE)
  x_scored<-read.csv(paste(x,'_scored_data.csv',sep=""))
  x_scored$Subject<-sub('(_).*$', '', x_scored$Subject, perl=TRUE)
  x<-merge(x_scored,x_IIV, by="Subject", all=T)
  x<-merge(x, demos, by="Subject", all.y=T)
  if (x[3,2] != 'reaction') {
    x$percent_correct<-((x$total_correct)/(x$total_correct+x$total_incorrect))
  }
  print(x)
}

# This function creates tables of the summarySE function output (Group, N, Mean, SD, SE, and CI)
# x = dataframe
# group = 'grouping variable'
# name = 'name of task'
makeRawTable <- function(x,group,name) {
  # the df reaction does not include percent_correct, so if looking at reaction, do this instead:
  if (x[3,2] == 'reaction') {
    rt_sum<-summarySE(data=x, measurevar="median_rt", groupvars=group, na.rm=T)
    iiv_sum<-summarySE(data=x, measurevar="IIV", groupvars=group, na.rm=T)
    colnames(rt_sum)<-c("Group","N","Mean","SD","SE","CI")
    colnames(iiv_sum)<-c("Group","N","Mean","SD","SE","CI")
    sum_stats<-rbind(rt_sum,iiv_sum)
    sum_stats[,-1:-2]<-round(sum_stats[,-1:-2],3)
    gt_table_sum<-gt(data = sum_stats)
    if (group != 'DX_graph') {
      gt_table_sum<-
        gt_table_sum %>%
        tab_row_group(
          group = "Reaction Time",
          rows = 1:2
        ) %>%
        tab_row_group(
          group = "IIV",
          rows = 3:4
        )

      gt_table_sum<-
        gt_table_sum %>%
        tab_header(
          title = paste(name,' Raw Data',sep="")
        )
    } else {
      gt_table_sum<-
        gt_table_sum %>%
        tab_row_group(
          group = "Reaction Time",
          rows = 1:3
        ) %>%
        tab_row_group(
          group = "IIV",
          rows = 4:6
        )

      gt_table_sum<-
        gt_table_sum %>%
        tab_header(
          title = paste(name,' Raw Data',sep="")
        )

    }
    
  } else {
    pc_sum<-summarySE(data=x, measurevar="percent_correct", groupvars=group, na.rm=T)
    rt_sum<-summarySE(data=x, measurevar="rtcr", groupvars=group, na.rm=T)
    iiv_sum<-summarySE(data=x, measurevar="IIV", groupvars=group, na.rm=T)
    colnames(pc_sum)<-c("Group","N","Mean","SD","SE","CI")
    colnames(rt_sum)<-c("Group","N","Mean","SD","SE","CI")
    colnames(iiv_sum)<-c("Group","N","Mean","SD","SE","CI")
    sum_stats<-rbind(pc_sum,rt_sum,iiv_sum)
    sum_stats[,-1:-2]<-round(sum_stats[,-1:-2],3)
    gt_table_sum<-gt(data = sum_stats)
    if (group != 'DX_graph') {
      gt_table_sum<-
        gt_table_sum %>%
        tab_row_group(
          group = "Percent Correct",
          rows = 1:2
        ) %>%
        tab_row_group(
          group = "Reaction Time",
          rows = 3:4
        ) %>%
        tab_row_group(
          group = "IIV",
          rows = 5:6
        )
      
      gt_table_sum<-
        gt_table_sum %>%
        tab_header(
          title = paste(name,' Raw Data',sep="")
        )
      
    } else {
      gt_table_sum<-
        gt_table_sum %>%
        tab_row_group(
          group = "Percent Correct",
          rows = 1:3
        ) %>%
        tab_row_group(
          group = "Reaction Time",
          rows = 4:6
        ) %>%
        tab_row_group(
          group = "IIV",
          rows = 7:9
        )
      
      gt_table_sum<-
        gt_table_sum %>%
        tab_header(
          title = paste(name,' Raw Data',sep="")
        )
    }
    
  }
  print(gt_table_sum)
  gtsave(gt_table_sum,paste(x[3,2],'_',group,'_rawdata.rtf',sep=''))
}

# Use Levene's Test - if passes, continue with ANOVA; if fails (is significant), use
# Kruskal-Wallis Test
mainANOVA <- function(x,group,name) {
  # again the reaction df is slightly different than the others
  a<-data.frame()
  k<-data.frame()
  if (x[3,2] == 'reaction') {
    dv_type<-c('median_rt','IIV')
    for (i in dv_type) {
      levene_test<-leveneTest(x[[i]],x[[group]])
      print(levene_test)
      if (levene_test[1,3]>=0.05) {
        #do anova - 
        print(i)
        i_aov<-aov(x[[i]]~x[[group]]+x$age+x$Sex+x$Race)
        i_anova<-Anova(i_aov,type="III")
        i_anova<-round(i_anova,3)
        rownames(i_anova)<-c("Intercept","Group","Age","Gender","Race","Residuals")
        setDT(i_anova, keep.rownames = TRUE)[]
        colnames(i_anova)<-c("Factor","Sum Sq","df","F","p")
        i_anova<-cbind.data.frame(i_anova,i)
        a<-rbind.data.frame(a,i_anova)
        print(a)
      } else {
        print(i)
        i_kwtest<-kruskal.test(x[[i]]~x[[group]])
        k<-rbind.data.frame(k,i_kwtest)
        print(k)
      }
    }
    
  } else {
    dv_type<-c('percent_correct','rtcr','IIV')
    for (i in dv_type) {
      levene_test<-leveneTest(x[[i]],x[[group]])
      print(levene_test)
      if (levene_test[1,3]>=0.05) {
        #do anova
        print(i)
        i_aov<-aov(x[[i]]~x[[group]]+x$age+x$Sex+x$Race)
        i_anova<-Anova(i_aov,type="III")
        i_anova<-round(i_anova,3)
        rownames(i_anova)<-c("Intercept","Group","Age","Gender","Race","Residuals")
        setDT(i_anova, keep.rownames = TRUE)[]
        colnames(i_anova)<-c("Factor","Sum Sq","df","F","p")
        i_anova<-cbind.data.frame(i_anova,i)
        a<-rbind.data.frame(a,i_anova)
        print(a)
      } else {
        print(i)
        i_kwtest<-kruskal.test(x[[i]]~x[[group]])
        k<-rbind.data.frame(k,i_kwtest)
        print(k)
      }
    }
  }
  table<-a[,1:5]
  gt_table_anova<-gt(data=table)
  for (j in seq(6,nrow(table),6)) {
    gt_table_anova<-
      gt_table_anova %>%
      tab_row_group(
        group = a[j,6],
        rows = (j-5):j
      )
  }
  
  gt_table_anova<-
    gt_table_anova %>%
    tab_header(
      title = paste(name,' ANOVA',sep="")
    )
  
  print(gt_table_anova)
  gtsave(gt_table_anova,paste(x[3,2],'_',group,'_anova.rtf',sep=''))
  
}

healthyANOVA <- function(x,group,name) {
  # again the reaction df is slightly different than the others
  a<-data.frame()
  k<-data.frame()
  if (x[3,2] == 'reaction') {
    dv_type<-c('median_rt','IIV')
    for (i in dv_type) {
      levene_test<-leveneTest(x[[i]],x[[group]])
      print(levene_test)
      if (levene_test[1,3]>=0.05) {
        #do anova - 
        print(i)
        i_aov<-aov(x[[i]]~x[[group]]+x$Gender+x$Race)
        i_anova<-Anova(i_aov,type="III")
        i_anova<-round(i_anova,3)
        rownames(i_anova)<-c("Intercept","Group","Gender","Race","Residuals")
        setDT(i_anova, keep.rownames = TRUE)[]
        colnames(i_anova)<-c("Factor","Sum Sq","df","F","p")
        i_anova<-cbind.data.frame(i_anova,i)
        a<-rbind.data.frame(a,i_anova)
        print(a)
      } else {
        print(i)
        i_kwtest<-kruskal.test(x[[i]]~x[[group]])
        k<-rbind.data.frame(k,i_kwtest)
        print(k)
      }
    }
  } else {
    dv_type<-c('percent_correct','rtcr','IIV')
    for (i in dv_type) {
      levene_test<-leveneTest(x[[i]],x[[group]])
      print(levene_test)
      if (levene_test[1,3]>=0.05) {
        #do anova
        print(i)
        i_aov<-aov(x[[i]]~x[[group]]+x$Gender+x$Race)
        i_anova<-Anova(i_aov,type="III")
        i_anova<-round(i_anova,3)
        rownames(i_anova)<-c("Intercept","Group","Gender","Race","Residuals")
        setDT(i_anova, keep.rownames = TRUE)[]
        colnames(i_anova)<-c("Factor","Sum Sq","df","F","p")
        i_anova<-cbind.data.frame(i_anova,i)
        a<-rbind.data.frame(a,i_anova)
        print(a)
      } else {
        print(i)
        i_kwtest<-kruskal.test(x[[i]]~x[[group]])
        k<-rbind.data.frame(k,i_kwtest)
        print(k)
      }
    }
  }
  table<-a[,1:5]
  gt_table_anova<-gt(data=table)
  for (j in seq(5,nrow(table),5)) {
    gt_table_anova<-
      gt_table_anova %>%
      tab_row_group(
        group = a[j,6],
        rows = (j-4):j
      )
  }
  
  gt_table_anova<-
    gt_table_anova %>%
    tab_header(
      title = paste(name,' ANOVA',sep="")
    )
  
  print(gt_table_anova)
  gtsave(gt_table_anova,paste(x[3,2],'_',group,'healthy_anova.rtf',sep=''))
  
}
# x = data
# a = group 1
# b = group 2
# c = optional group 3
multipleComparisons<- function(x,group) {
  hoa<-subset(x, DX_graph == 'HOA')
  mci<-subset(x, DX_graph == 'MCI')
  scm<-subset(x, DX_graph == 'SCM')
  if (x[3,2] == 'reaction'){
    dv_type<-c('median_rt','IIV')
    for (i in dv_type) {
      print(i)
      compare<-pairwise.t.test(x[[i]], x[[group]], p.adj = "none")
      print(compare)
      cohens_d1<-cohensD(x=hoa[[i]],y=mci[[i]])
      cohens_d2<-cohensD(x=scm[[i]],y=mci[[i]])
      cohens_d3<-cohensD(x=hoa[[i]],y=scm[[i]])
      cohen<-rbind(cohens_d1,cohens_d2,cohens_d3)
      colnames(cohen)<-c("Cohen's D")
      rownames(cohen)<-c("HOA v MCI:","SCM v MCI:","HOA v SCM:")
      print(cohen)
    }
  } else {
    dv_type<-c('percent_correct','rtcr','IIV')
    for (i in dv_type) {
      print(i)
      compare<-pairwise.t.test(x[[i]], x[[group]], p.adj = "none")
      print(compare)
      cohens_d1<-cohensD(x=hoa[[i]],y=mci[[i]])
      cohens_d2<-cohensD(x=scm[[i]],y=mci[[i]])
      cohens_d3<-cohensD(x=hoa[[i]],y=scm[[i]])
      cohen<-rbind(cohens_d1,cohens_d2,cohens_d3)
      colnames(cohen)<-c("Cohen's D")
      rownames(cohen)<-c("HOA v MCI:","SCM v MCI:","HOA v SCM:")
      print(cohen)
    }
  }
}













