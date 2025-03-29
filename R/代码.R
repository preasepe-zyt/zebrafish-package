rm(list=ls())
library(tidyverse)
library(readxl)
library(xlsx)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(dplyr)
library(ggbeeswarm)
library(cols4all)
library(cowplot)
library(extrafont)
loadfonts(device = "pdf")
#导入字体进入pdf
#showtext包可给定字体文件，加载到 R环境中，生成新的字体家族名字，后期调用这个名字设定字体，并且支持中文写入pdf不乱码
library(showtext)
showtext_auto(enable=TRUE)


zebrafish.be <- function(dat, ti, ng) {
    library(tidyverse)
    library(readxl)
    library(xlsx)
    library(ggplot2)
    library(reshape2)
    library(ggpubr)
    library(dplyr)
    if(class(dat) != "character"){
        return("this is not character")
    }else{
        abc <- as.data.frame(read_excel(dat))
    }
    if(class(nf) != "numeric"){
        return("nf is not numeric")
    }else{
        time <- ti #时间)
    }
    if(class(ng) != "numeric"){
        return("ng this is not numeric")
    }else{
        ngroup <- ng #鱼的条数
    }
    interval <- time-1
    abc <- as.data.frame(read_excel(dat))
    abc <- select(abc,c("aname","start","end","inadist","smldist","lardist"))
    arrange(abc, abc[,1]) -> abc
    for (i in 1:nrow(abc)) {reduce(abc[i,c("inadist","smldist","lardist")],sum) -> abc$min_distance[[i]]}
    for (i in 1:nrow(abc)) {as.numeric(abc[i,"min_distance"])/60 -> abc$second_speed[i]}
    for (i in 1:nrow(abc)) {if (i%%time==0) reduce(abc[(i-interval):i,c("inadist","smldist","lardist")],sum) -> abc$total_distance[i%/%time]}
    abc$total_distance[(nrow(abc)%/%time+1):nrow(abc)] <- "NA"
    write.xlsx(abc,"自动处理.xlsx")
    speed <- abc %>% select(c("aname","start","end","second_speed")) %>% mutate(time=paste(abc$start,abc$end,sep = "-"))
    speed$aname <- gsub("[0-9]","",speed$aname)
    speed$aname <-  case_when(speed$aname=="A" ~ "Ctl",
                                   speed$aname=="B" ~ "M",
                                   speed$aname=="C" ~ "Y",
                                   speed$aname=="D" ~ "100 µg/mL",
                                   speed$aname=="E" ~ "140 µg/mL",
                                   speed$aname=="F" ~ "180 µg/mL")
    speed_graph <- data.frame(ncol=ncol(speed), nrow=time*ngroup)
    for (i in 1:(nrow(abc)/(time*ngroup))){speed[(i*time*ngroup-159):(i*time*ngroup),] %>% cbind(speed_graph) -> speed_graph}
    graph <- list()
    group <- vector()
    for (i in 1:(nrow(abc)/time)) {if (i%%ngroup==0)  as.vector(abc$total_distance[(i-(ngroup-1)):i]) -> graph[[i]]}
    final_n <- vector()
    for (i in 1:(nrow(abc)/time)) {if (i%%ngroup  == 0 ) append(final_n,i) -> final_n}
    graph[final_n] %>% as.data.frame() -> graph2
    names(graph2) <- c(1:6)
    write.xlsx(graph2,"graph格式.xlsx")
    list(graph2,speed,speed_graph)
}

results1 <- zebrafish.be("1.xls", 20, 8)[[1]]
results2  <- zebrafish.be("2.xls", 20, 8)[[1]]
results3  <- zebrafish.be("3.xls", 20, 8)[[1]]

results1_s<- zebrafish.be("1.xls", 20, 8)[[2]]
results2_s  <- zebrafish.be("2.xls", 20, 8)[[2]]
results3_s  <- zebrafish.be("3.xls", 20, 8)[[2]]

results1 %>% bind_rows(results2) %>% bind_rows(results3) -> final_results__dis__dis
results1_s %>% bind_rows(results2_s) %>% bind_rows(results3_s) -> final_results__dis_speed
xlsx::write.xlsx(final_results__dis__dis,"final_results__dis__dis.xlsx")
xlsx::write.xlsx(final_results__dis_speed,"final_results__dis_speed.xlsx")


names(final_results__dis_speed) <- c("group",  "start","end","Speed", "time")
names(final_results__dis) <- c("Ctl", "M", "Y", "100 µg/mL", "140 µg/mL", "180 µg/mL")
final_results__dis<- final_results__dis %>% apply(2,as.numeric)
plot <- melt(final_results__dis)
names(plot) <- c("id", "group", "value")
my_comparisons <- list(c("Ctl", "Scopolamine"), c("10 µg/mL", "Scopolamine"), c("20 µg/mL", "Scopolamine"), c("40 µg/mL", "Scopolamine"), c("Alone", "Ctl"))

if(T){mytheme <- theme(plot.title = element_text(size = 20,color="black",hjust = 0.5,family="sans",face = "bold"),
                     axis.title = element_text(size = 20,color ="black", family="sans",face = "bold"),
                     axis.text = element_text(size= 20,color = "black", family="sans",face = "bold"),
                     #panel.grid.minor.y = element_blank(),
                     #panel.grid.minor.x = element_blank(),
                     axis.text.x = element_text(angle = 45,hjust = 1,family="sans",face = "bold" ),
                     #panel.grid=element_blank(),
                     legend.position = "right",
                     legend.text = element_text(size= 20, family="sans",face = "bold"),
                     legend.title= element_text(size= 20, family="sans",face = "bold"),
                     axis.text.y = element_text(margin = margin(0,0,0,0.2,'cm')),
                     axis.ticks.length.y = unit(.25, "cm")
                     #axis.ticks = element_line(linewidth = 2,size = 2)
    ) }

c4a_gui()
mycol <- c4a('bold',8) #自定义配色挑选

ggplot(plot, aes(x = group, y = value))+
    labs(y="Total distance (cm)",x= "",title = "")+
    geom_boxplot(plot, mapping=aes(x = group, y = value,fill=group),position=position_dodge(0.5),width=0.5,outlier.alpha = 0)+
    theme_classic() + mytheme +
    scale_color_manual(values = rev(mycol))+
    scale_y_continuous(breaks = c(0, 100, 200, 300,400, 500,600,700))+
    stat_compare_means(comparisons = my_comparisons,
                       label = "p.signif",
                       method = "wilcox.test",
                       hide.ns = F,
                       size = 4,
                       label.y = c(400,450,500,550,600)) -> p
p
ggsave("总距离.bmp", width = 10, height = 10, family="sans",units = "cm")
abc$time <- rep(c(1:30),48)
gsub("^[^-]+","",abc$aname) -> abc$group

ggplot(plot, aes(x = group, y = value,group=group))+
    labs(y="Total distance (cm)",x= "",title = "")+
    geom_boxplot(plot, mapping=aes(x = group, y = value, color=group),position=position_dodge(0.5),width=0.5,outlier.alpha = 0)+
    theme_classic()+
    mytheme +
    geom_jitter(aes(color=group))+
    scale_color_manual(values = rev(mycol))+
    scale_y_continuous(limits = c(0,700),breaks = c(0, 100, 200, 300,400, 500,600,700),expand = c(0,0))+
    stat_compare_means(comparisons = my_comparisons,
                       label = "p.signif",
                       method = "wilcox.test",
                       hide.ns = F,
                       size=7,
                       label.y = c(400,450,500,550,600))

ggsave("行为学.bmp", width = 10, height = 7)


ggplot(plot, aes(x = group, y = value))+
    labs(y="Total distance (cm)",x= "",title = "")+
    geom_beeswarm(aes(color = group), size = 2.2, alpha = 0.7,
                  cex = 1.3) +
    scale_color_manual(values = rev(mycol)) +
    theme_classic() + mytheme +
    stat_compare_means(comparisons = my_comparisons,
                       label = "p.signif",
                       method = "wilcox.test",
                       hide.ns = F,
                       size = 7) -> p2
p2
pdf("行为学（未取极端值）.jpeg", width = 20, height = 20)
plot_grid(p,p2,labels = c("A","B"),nrow = 2, ncol=2, label_size = 40, label_fontface = "bold")
dev.off()
ggsave("行为学.bmp", width = 15, height = 15)
apply(final_results__dis,2,mean) %>% bind_rows(apply(final_results__dis,2,sd)) -> outler

#速度
final_results__dis_speed$time <- gsub("1140-1200.019","1140-1200",final_results__dis_speed$time)
final_results__dis_speed$time <- gsub("1140-1200.02","1140-1200",final_results__dis_speed$time)

final_results__dis_speed$group <- final_results__dis_speed$group  %>% fct_inorder()
final_results__dis_speed$time <- final_results__dis_speed$time %>% fct_inorder()
ggplot(final_results__dis_speed, aes(x = time, y = Speed,group=group))+
    labs(y="Speed (cm/s)",x= "",title = "",fill="group",color="group")+
    geom_smooth(aes(color=final_results__dis_speed$group,fill=final_results__dis_speed$group),alpha = 0.2)+
    theme_classic()+
    scale_y_continuous(limits = c(0,0.3),breaks = c(0, 0.1,0.15,0.2,0.25,0.3),expand = c(0,0))+
    mytheme




