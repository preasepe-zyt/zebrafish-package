#' @title zebrafish_be.
#' <div style="float: right; padding-left: 10px;">
#'   <img src="figures/be.png" width="200"/>
#' </div>
#' @description  Dealing with behavioral data.
#' @details Need to confirm dat, ti, ng, way.
#' @param dat, A data frame to record behavioral data.
#' @param ti, Total time of behavioral test.
#' @param ng, Number of zebrafish group in behavioral test.
#' @param way, Speed or distance to analysis.
#' @return A matrix.
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
#' data(data)
#' results1_dis <- zebrafish_be(data[[1]], 20, 8, "dis")
#' results1_speed <- zebrafish_be(data[[1]], 20, 8, "speed")
#' results2_dis <- zebrafish_be(data[[2]], 20, 8, "dis")
#' results2_speed  <- zebrafish_be(data[[2]], 20, 8, "speed")
#' results3_dis <- zebrafish_be(data[[3]], 20, 8, "dis")
#' results3_speed  <- zebrafish_be(data[[3]], 20, 8, "speed")
#'


zebrafish_be <- function(dat, ti, ng, way) {
    # if(class(dat) %in% "data.frame"){
    #     stop("this is not data.frame")
    # }else{
    #     abc <- dat
    # }
    if(class(ti) != "numeric"){
        stop("nf is not numeric")
    }else{
        time <- ti #时间
    }
    if(class(ng) != "numeric"){
        stop("ng this is not numeric")
    }else{
        ngroup <- ng #鱼的条数
    }
    if(!(way %in% c("dis","speed"))){
        stop("Error: way must be 'dis' or 'speed'.")
    }
    interval <- time-1
    abc <- as.data.frame(dat)
    abc <- select(abc,c("aname","start","end","inadist","smldist","lardist"))
    arrange(abc, abc[,1]) -> abc
    for (i in 1:nrow(abc)) {reduce(abc[i,c("inadist","smldist","lardist")],sum) -> abc$min_distance[[i]]}
    for (i in 1:nrow(abc)) {as.numeric(abc[i,"min_distance"])/60 -> abc$second_speed[i]}
    for (i in 1:nrow(abc)) {if (i%%time==0) reduce(abc[(i-interval):i,c("inadist","smldist","lardist")],sum) -> abc$total_distance[i%/%time]}
    abc$total_distance[(nrow(abc)%/%time+1):nrow(abc)] <- "NA"

    speed <- abc %>% select(c("aname","start","end","second_speed")) %>% mutate(time=paste(abc$start,abc$end,sep = "-"))

    speed_graph <- data.frame(ncol=ncol(speed), nrow=time*ngroup)
    for (i in 1:(nrow(abc)/(time*ngroup))){speed[(i*time*ngroup-159):(i*time*ngroup),] %>% cbind(speed_graph) -> speed_graph}
    graph <- list()
    group <- vector()
    for (i in 1:(nrow(abc)/time)) {if (i%%ngroup==0)  as.vector(abc$total_distance[(i-(ngroup-1)):i]) -> graph[[i]]}
    final_n <- vector()
    for (i in 1:(nrow(abc)/time)) {if (i%%ngroup  == 0 ) append(final_n,i) -> final_n}
    dis  <-  graph[final_n] %>% as.data.frame()
    names(dis) <- c(1:6)
    write.xlsx(dis,"dis.xlsx")
    write.xlsx(speed,"speed.xlsx")
    if (way=="dis"){
        return(dis)
    }else if(way=="speed"){
        return(speed)
    }else{
        stop("Error: way must be 'dis' or 'speed'.")
    }

}


# results1 <- zebrafish.be("X1_raw.xls", 20, 8)[[1]]
# results2  <- zebrafish.be("X2_raw.xls", 20, 8)[[1]]
# results3  <- zebrafish.be("X1_raw.xls", 20, 8)[[1]]
#
# results1_s<- zebrafish.be("X1_raw.xls", 20, 8)[[2]]
# results2_s  <- zebrafish.be("X2_raw.xls", 20, 8)[[2]]
# results3_s  <- zebrafish.be("X3_raw.xls", 20, 8)[[2]]
#
# results1 %>% bind_rows(results2) %>% bind_rows(results3) -> final_results__dis__dis
# results1_s %>% bind_rows(results2_s) %>% bind_rows(results3_s) -> final_results__dis_speed
# xlsx::write.xlsx(final_results__dis__dis,"final_results__dis__dis.xlsx")
# xlsx::write.xlsx(final_results__dis_speed,"final_results__dis_speed.xlsx")
#
