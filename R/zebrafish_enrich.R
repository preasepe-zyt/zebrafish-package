#' @title zebrafish_enrich.
#' <div style="float: right; padding-left: 10px;">
#'   <img src="figures/enrich.jpg" width="200"/>
#' </div>
#' @description Conduct GO and KEGG enrichment analysis for zebrafish genes
#' @details Need to confirm dat, group, way.
#' @param dat, A vector for zebrafish genes.
#' @param class, The type of enrichment analysis for go or kegg.
#' @return Plot.
#' @export
#' @import org.Dr.eg.db
#' @import clusterProfiler
#' @import enrichplot
#' @import ggplot2
#' @import ggnewscale
#' @import DOSE
#' @import stringr
#' @import pathview
#' @import ggnewscale
#' @import R.utils
#' @import AnnotationDbi
#' @import stats4
#' @import BiocGenerics
#' @import S4Vectors
#' @examples
#' genes <- c("apoea", "appb", "psen1", "psen2", "mapta", "bin1", "clu", "picalma","trem2a", "abca7b", "cd33", "cr1l", "sorl1", "grn1", "sod1", "hspb1","gsk3ba", "bace1", "tnfa", "il6", "nlrp3", "cx3cr1")
#' zebrafish_enrich(genes,"go")
#' zebrafish_enrich(genes,"kegg")
#'

zebrafish_enrich <- function(dat,class){
    if(class=="go" || class=="GO"){

        pvalueFilter=0.05
        qvalueFilter=1
        showNum=10

        rt <- as.data.frame(dat)
        genes=as.vector(rt[,1])
        entrezIDs <- mget(genes, org.Dr.egSYMBOL2EG, ifnotfound= NA)
        entrezIDs <- as.character(entrezIDs)
        rt=cbind(rt,entrezID=entrezIDs)
        colnames(rt)=c("symbol","entrezID")
        rt2=rt[rt[,"entrezID"] != "NA",]
        gene <- rt2$entrezID
        gene2 <- unique(gene)

        colorSel="qvalue"
        if(qvalueFilter>0.05){
            colorSel="pvalue"
        }


        kk=enrichGO(gene = gene2,OrgDb = org.Dr.eg.db, pvalueCutoff = 1, qvalueCutoff = 1, ont="all", readable =T)
        kk@result <- kk@result
        GO=as.data.frame(kk)
        GO=GO[(GO$pvalue<pvalueFilter & GO$qvalue<qvalueFilter),]
        write.table(GO,file="GO.xls",sep="\t",quote=T,row.names = T)


        if(nrow(GO)<30){
            showNum=nrow(GO)
        }
        pdf("go_bubble.pdf", width = 15, height = 15)
        bub=dotplot(kk,showCategory = showNum, orderBy = "GeneRatio",split="ONTOLOGY", color = colorSel) +
            facet_grid(ONTOLOGY~., scale='free')+
            scale_y_discrete(labels=function(x) stringr::str_wrap(x, width=45))+
            theme(text=element_text(family = "sans"),
                  axis.text.y = element_text(size= 20,color = "black"),
                  axis.text.x = element_text(size= 20,color = "black"),
                  legend.text = element_text(size= 30),
                  legend.title= element_text(size= 30),
                  #axis.text.x.top = element_text(size= 30,face= "bold"),
                  #axis.text.y.left = element_text(size= 30,face= "bold"),
                  #strip.text.x = element_text(size = 30, face = "bold"),
                  strip.text.y = element_text(size = 20),#小标题大
                  legend.key.size = unit(3, "cm"),
                  axis.title.x.bottom =  element_text(size = 20),
                  axis.line = element_line(size = 0.8),
                  panel.border = element_rect(linewidth = 3),
                  axis.ticks.length = unit(0.5, "cm"))#图例大小
        print(bub)
        dev.off()

        pdf("go_cnet.pdf", width = 30, height = 20)
        af=setReadable(kk, 'org.Dr.eg.db', 'ENTREZID')
        p<-cnetplot(af, showCategory = showNum, categorySize="pvalue",circular = TRUE,
                      color.params = list(edge = TRUE, category ="#E64B35CC", gene =
                                              "#386CB0"),
                      cex.params = list(category_node = 2.5, gene_node = 1,
                                        category_label = 1, gene_label = 2.5),
                      shadowtext='none')+theme(text=element_text(family = "sans",size= 30),
                                               #axis.text.y = element_text(size= 30,color = "black",face= "bold"),
                                               #axis.text.x = element_text(size= 30,color = "black",face= "bold"),
                                               legend.text = element_text(size= 30),
                                               legend.title= element_text(size= 30),
                                               #axis.text.x.top = element_text(size= 30,face= "bold"),
                                               #axis.text.y.left = element_text(size= 30,face= "bold"),
                                               #strip.text.x = element_text(size = 30, face = "bold"),
                                               legend.key.size = unit(2.5, "cm"),
                                               #axis.title.x.bottom =  element_text(size = 30, face = "bold")
                      )
        p$layers[[3]]$aes_params$family <- "sans"
        p$layers[[3]]$aes_params$face <- "bold"
        p$layers[[3]]$aes_params$size <- 15
        print(p)
        dev.off()

    }else if(class=="kegg" || class=="KEGG"){

        pvalueFilter=0.05
        qvalueFilter=1
        showNum=10
        keggId="000dre20"

        rt <- as.data.frame(dat)
        genes=as.vector(rt[,1])
        entrezIDs <- mget(genes, org.Dr.egSYMBOL2EG, ifnotfound=NA)
        entrezIDs <- as.character(entrezIDs)
        rt=cbind(rt,entrezID=entrezIDs)
        colnames(rt)=c("symbol","entrezID")
        rt=rt[rt[,"entrezID"]!="NA",]
        gene=rt$entrezID
        gene=unique(gene)
        mycolorSel="qvalue"
        if(qvalueFilter>0.05){
            mycolorSel="pvalue"
        }
        kk <- enrichKEGG(gene = gene, organism = "dre", pvalueCutoff =1, qvalueCutoff =1)
        KEGG=as.data.frame(kk)
        KEGG$geneID=as.character(sapply(KEGG$geneID,function(x)paste(rt$symbol[match(strsplit(x,"/")[[1]],as.character(rt$entrezID))],collapse="/")))
        KEGG=KEGG[(KEGG$pvalue<pvalueFilter & KEGG$qvalue<qvalueFilter),]

        xlsx::write.xlsx(KEGG,file="KEGG.xls")


        if(nrow(KEGG)<showNum){
            showNum=nrow(KEGG)
        }
        pdf(file="KEGG_bubble.pdf",width = 15,height = 15)
        bub=dotplot(kk,showCategory = 10, orderBy = "GeneRatio", color = mycolorSel)+
            scale_y_discrete(labels=function(x) stringr::str_wrap(x, width=30))+
            theme(text=element_text(family = "sans"),
                  axis.text.y = element_text(size= 30,color = "black"),
                  axis.text.x = element_text(size= 30,color = "black"),
                  legend.text = element_text(size= 30),
                  legend.title= element_text(size= 30),
                  #axis.text.x.top = element_text(size= 30,face= "bold"),
                  #axis.text.y.left = element_text(size= 30,face= "bold"),
                  #strip.text.x = element_text(size = 30, face = "bold"),
                  strip.text.y = element_text(size = 30),#小标题大
                  legend.key.size = unit(3, "cm"),
                  axis.title.x.bottom =  element_text(size = 30),
                  axis.line = element_line(size = 0.8),
                  panel.border = element_rect(linewidth = 3),
                  axis.ticks.length = unit(0.5, "cm"))#图例大小
        print(bub)
        dev.off()


        pdf(file="KEGG_cnet.pdf",width = 30,height = 20)
        af=setReadable(kk, "org.Dr.eg.db", 'ENTREZID')
        p <- cnetplot(af,categorySize="pvalue",showCategory = showNum,circular = TRUE,
                      color.params = list(edge = TRUE, category = "#E64B35CC", gene =
                                              "#386CB0"),
                      cex.params = list(category_node = 2.5, gene_node = 1,
                                        category_label = 1, gene_label = 2.5),
                      shadowtext='none')+theme(text = element_text(family = "sans"),
                                               #axis.text.y = element_text(size= 30,mycolor = "black",face= "bold"),
                                               #axis.text.x = element_text(size= 30,mycolor = "black",face= "bold"),
                                               legend.text = element_text(size= 30),
                                               legend.title= element_text(size= 30),
                                               #axis.text.x.top = element_text(size= 30,face= "bold"),
                                               #axis.text.y.left = element_text(size= 30,face= "bold"),
                                               #strip.text.x = element_text(size = 30, face = "bold"),
                                               strip.text.y = element_text(size = 30),#小标题大
                                               legend.key.size = unit(2.5, "cm")
                                               #axis.title.x.bottom =  element_text(size = 30, face = "bold")
                      )
        p$layers[[3]]$aes_params$family <- "sans"
        p$layers[[3]]$aes_params$face <- "bold"
        p$layers[[3]]$aes_params$size <- 15
        print(p)
        dev.off()

        KEGG$Description <- factor(KEGG$Description, levels = rev(KEGG$Description))

        #基础富集条形图绘制：
        p <- ggplot() +
            geom_bar(data = KEGG[1:10,],
                     aes(x = -log10(pvalue), y = Description, fill=Description),
                     width = 1, #柱子宽度调整
                     stat = 'identity',
                     alpha=0.6) +
            theme_classic()+ scale_x_continuous(expand = c(0,0))+
            theme(axis.text.y = element_blank()) + #去掉y轴标签
            geom_text(data = KEGG[1:10,],
                      aes(x = 0.1, #用数值向量控制文本标签起始位置
                          y = Description,
                          label = Description),
                      size = 4.5,
                      hjust = 0)+#左对齐
            geom_text(data = KEGG[1:10,],
                      aes(x = 0.1, y = Description, label = geneID, color = Description),
                      size = 4,
                      fontface = 'italic', #geneID斜体
                      hjust = 0,
                      vjust = -1)+#垂直偏移，调整文本向下
            labs(x = '-Log10P',
                 y = 'KEGG', #用空格控制间隙，多次调整直至合适
                 title = 'Top 10 Enriched Pathways')+guides(fill = "none",color= "none")+
            theme(text=element_text(size = 20,family = "sans",face = "bold"),
                  legend.position = 'none',
                  plot.title = element_text(size = 20,family = "sans"),
                  axis.title = element_text(size = 20,family = "sans"),
                  axis.text = element_text(size = 20,family = "sans"),
                  axis.line = element_line(size = 0.8)
            )
        p




        #自定义配色：
        pdf("KEGG_rank.pdf",width = 15,height = 10)
        mycol <- c4a("brewer.gn_bu", 10)
        p6 <- p +
            scale_fill_manual(values = mycol) +
            scale_color_manual(values = mycol)
        print(p6)
        dev.off()

    }else{
    print("Please enter 'go','GO'  or 'kegg','KEGG' as the third parameter")}
}

