library(scrapeR)
library(RColorBrewer)
library(ggplot2)
library(plyr)

library(extrafont)
suppressWarnings(loadfonts())

n1.file <- "n1players.Rdata"
load(n1.file)

slams <- c("London-Finals.aspx", "Australian-Open.aspx", "Roland-Garros.aspx", "US-Open.aspx", "Wimbledon.aspx")
surfaces <- c("Reliability-Overall-Career-List.aspx", "Reliability-Hard-Career-List.aspx", "Reliability-Clay-Career-List.aspx", 	"Reliability-Grass-Career-List.aspx", "Reliability-Overall-Current-List.aspx",
	"Reliability-Clay-Current-List.aspx",
	"Reliability-Grass-Current-List.aspx", "Reliability-Hard-Current-List.aspx")

names(player1) <- sapply(player1, function(p) {
		p.splt <- strsplit(p, ",\\s")
		paste(gsub("(.*)\\s\\(.*\\)", "\\1", p.splt[[1]][2]), p.splt[[1]][1], sep=" ")
})

###  -------------------------------------------------------------------------------------------------------------  ###
###      HELPERS
###  -------------------------------------------------------------------------------------------------------------  ###

# ggplot2 theme no fuss

ggtheme <- {
  theme_bw() +
 #eliminates baground, gridlines, and chart border
 theme(
   plot.background = element_blank()
  ,panel.grid.major = element_blank()
  ,panel.grid.minor = element_blank()
  ,panel.border = element_blank()
  ,panel.background = element_blank()
 )
}


###  -------------------------------------------------------------------------------------------------------------  ###
###      1. get all grand slam titles
###  -------------------------------------------------------------------------------------------------------------  ###

atpurl.prefix <- "http://www.atpworldtour.com/Tennis/Tournaments/"

titles <- do.call(rbind, lapply(slams, function(s) {
	sname <- gsub(".aspx", "",s)
	url <- paste(atpurl.prefix, s, sep = "")
	html.tb <- readHTMLTable(url, stringsAsFactors = FALSE)[[1]]
	html.tb <- html.tb[,1:3]
	cbind(ddply(html.tb, .(Singles), summarize, title = length(Year)), tournament = sname)
}))

# hack to change all space
titles[,1]<- gsub("\\s", " ", titles[,1])
#titles <- titles[titles[,1] %in% names(player1),]

pl.subset <- names(which(tapply(titles$title, titles$Singles, sum) > 7))
pl.subset <- pl.subset[pl.subset %in% names(player1)]

tit <- titles[titles[,1] %in% pl.subset,]

sumTit <- tapply(tit$title, tit$Singles, sum)
tit$Singles <- factor(tit$Singles, levels = names(sumTit)[order(sumTit)])

# stacked bar chart
k <- ggplot(tit, aes(x = Singles, y = title, fill = tournament))
gp <- k + geom_bar() + theme_bw() + theme(panel.grid.major.x=element_blank(), panel.border = element_blank()) + theme(text=element_text(family="Georgia", 	size=12), axis.ticks.x = element_blank(), axis.text.x = element_text(vjust=-2)) + xlab("")

png("fig/grandSlams_%02d.png", width = 1480, height = 1000, res = 180)
gp + scale_fill_brewer(type ="qual") #+scale_fill_manual(values=c("#bdcdd4", "#F0E442", "#D55E00", "#005c94",  "#009E73"))
gp + scale_fill_manual(values=c("#bdcdd4", "#F0E442", "#D55E00", "#005c94",  "#009E73"))
dev.off()

pl <- unique(tit$Singles)
###  -------------------------------------------------------------------------------------------------------------  ###
###      2. by surface break down
###  -------------------------------------------------------------------------------------------------------------  ###


stat.prefix <- "http://www.atpworldtour.com/Reliability-Zone/"

stats <- do.call(rbind, lapply(surfaces, function(s) {
	surface <- gsub("Reliability-(.*)-(Career|Current)-List.aspx", "\\1",s)

	url <- paste(stat.prefix , s, sep = "")
	html.tb <- readHTMLTable(url, stringsAsFactors = FALSE)[[1]]
	html.tb <- html.tb[,c(2,6,8)]
	html.tb[,1]<- gsub("\\s", " ", html.tb[,1])

	html.tb <- html.tb[html.tb[,1] %in% as.character(pl),]

	html.tb <- cbind(html.tb, (do.call(rbind, strsplit(html.tb[,3], "-"))))
	cbind(html.tb, surface = surface)
}))