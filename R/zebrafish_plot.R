#' @title zebrafish_plot.
#' <div style="float: right; padding-left: 10px;">
#'   <img src="figures/plot.jpg" width="200"/>
#' </div>
#' @description visualization behavioral data.
#' @details Need to confirm dat, group, way.
#' @param dat, A data from zebrafish_be.
#' @param group, A vector for zebrafish group.
#' @param way, Speed or distance to analysis.
#' @return Plot.
#' @export
#' @import tidyverse
#' @import readxl
#' @import xlsx
#' @import ggplot2
#' @import reshape2
#' @import ggpubr
#' @import dplyr
#' @import ggbeeswarm
#' @import cols4all
#' @import cowplot
#' @import extrafont
#' @import showtext
#' @examples
#' a <- c("Ctl", "M", "Y", "100 µg/mL", "140 µg/mL", "180 µg/mL")
#' zebrafish_plot(results1_dis,a,"dis")
#' zebrafish_plot(results1_speed,a,"speed")
#'


zebrafish_plot <- function(dat, group, way){

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
)}

    if(way=="dis"){

    names(dat) <- group
    dat <- dat %>% apply(2,as.numeric)
    plot_dis <- melt(dat)
    names(plot_dis) <- c("id", "group", "value")
    mycol <- c4a('carto.pastel',11) #自定义配色挑选

    ggplot(plot_dis, aes(x = group, y = value))+
            labs(y="Total distance (cm)",x= "",title = "")+
            geom_beeswarm(aes(color = group), size = 2.2, alpha = 0.7,
                          cex = 1.3) +
            scale_color_manual(values = rev(mycol)) +
            theme_classic() + mytheme+
        scale_y_continuous(breaks = c(0, 100, 200, 300,400, 500,600,700))
    ggsave("dis_beeswarm.bmp", width = 6, height = 5,dpi=300)

    ggplot(plot_dis, aes(x = group, y = value,color=group))+
        labs(y="Total distance (cm)",x= "",title = "")+
        geom_boxplot(position=position_dodge(0.5),width=0.5,outlier.alpha = 0)+
        theme_classic() + mytheme +
        scale_color_manual(values = rev(mycol))+
        scale_y_continuous(breaks = c(0, 100, 200, 300,400, 500,600,700))
    ggsave("dis_box.bmp", width = 6, height = 5,dpi=300)

    ggplot(plot_dis, aes(x = group, y = value,color=group))+
        labs(y="Total distance (cm)",x= "",title = "")+
        geom_violin(position = position_dodge(width = .75),
                    size = 0.8,)+
        geom_boxplot(position=position_dodge(0.5),width=0.5,outlier.alpha = 0)+
        geom_point(position = position_jitterdodge(),size=2)+
        theme_classic() + mytheme +
        scale_color_manual(values = rev(mycol))+
        scale_y_continuous(breaks = c(0, 100, 200, 300,400, 500,600,700))
    ggsave("dis_com.bmp", width = 6, height = 5,dpi=300)

    }else if(way=="speed"){
    mycol <- c4a('carto.pastel',11) #自定义配色挑选
    dat$aname <- gsub("[0-9]","",dat$aname)
    dat$aname <-  case_when(dat$aname=="A" ~ group[1],
                                  dat$aname=="B" ~ group[2],
                                  dat$aname=="C" ~ group[3],
                                  dat$aname=="D" ~ group[4],
                                  dat$aname=="E" ~ group[5],
                                  dat$aname=="F" ~ group[6])

    dat$time <- gsub("1140-1200.019","1140-1200",dat$time)
    dat$time <- gsub("1140-1200.02","1140-1200",dat$time)

    dat$group <- as.factor(dat$aname)  %>% fct_inorder()
    dat$time <- as.factor(dat$time) %>% fct_inorder()
    ggplot(dat, aes(x = time, y = second_speed,group=group))+
            labs(y="Speed (cm/s)",x= "",title = "",fill="group",color="group")+
            geom_smooth(aes(color=group,fill=group),alpha = 0.2)+
            theme_classic()+
            scale_y_continuous(limits = c(0,0.3),breaks = c(0, 0.1,0.15,0.2,0.25,0.3),expand = c(0,0))+
            mytheme+scale_color_manual(values = rev(mycol))+
            mytheme+scale_fill_manual(values = rev(mycol))
    ggsave("speed.bmp", width = 10, height = 5,dpi=300)
    }else{
        print("Please enter 'dis' or 'speed' as the third parameter")
    }
}




