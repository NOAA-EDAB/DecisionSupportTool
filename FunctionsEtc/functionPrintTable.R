## function for printing a dataframe to pdf;
## Usage:
# pdf(file="Filename.pdf", width=11, height=8.5, paper="special")
# 
# grid.draw(PrintTable(Tbl=DataFrame1,
#                      Title="TableTitle",
#                      TitleFont=14))
# grid.newpage()
# grid.draw(PrintTable(Tbl=DataFrame,
#                      Title="TableTitle",
#                      TitleFont=14))
# dev.off()


PrintTable=function(Tbl, Title, TitleFont, BaseFont=12) {
  TB <- tableGrob(Tbl, theme=ttheme_default(base_size = BaseFont, base_colour = "black", 
                                            parse = FALSE, padding = unit(c(4, 4), "mm")))
  title <- textGrob(Title,gp=gpar(fontsize=TitleFont))
  padding <- unit(0.5,"line")
  TB <- gtable_add_rows(TB, heights = grobHeight(title) + padding, pos = 0)
  TB <- gtable_add_grob(x=TB, grobs=list(title), t=c(1), l=c(1), r=ncol(TB) )
}
