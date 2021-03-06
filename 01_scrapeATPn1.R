library(scrapeR)
library(xts)
library(RColorBrewer)
library(gplots)
library(ggplot2)
library(plyr)
library(extrafont)
loadfonts()


loadSavedN1players <- TRUE
file <- "allATP_1-100.Rdata"
n1.file <- "n1players.Rdata"

# atp plot: http://www.tennisfrontier.com/blogs/el-dude/talent-richness-of-the-atp-era/
# atp stats: http://www.tennis-x.com/stats/atprankhist.shtml
# http://stackoverflow.com/questions/6906661/ggplot2-make-missing-value-in-geom-tile-not-blank


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
###      1. load the vector of n1 players
###  -------------------------------------------------------------------------------------------------------------  ###

if(loadSavedN1players) {
	load(n1.file)
} else {
	load(file)

	###      1. load scraped tables with freaking all atp top 100 ranking data
	t <- as.Date(t, "%d.%m.%Y")
	t <- t[length(t):1]
	# inverse the list
	df <- df[length(df):1]

	# get only the player-rank values
	tb <- lapply(df, function(tb) {
		# discard table header
		tb <- tb[-1,]
		# create a name, ranking data.frame
		tb.splt <- strsplit(tb[,1], "\\\r\\\n(\\\t)+")

		table <- do.call(rbind, lapply(tb.splt, function(l) {
			if(length(l) == 3) data.frame(player = as.character(l[[3]]), rank = as.numeric(l[[1]])) else stop()
		}))

		if(is.null(table)) {
			NULL
		}
		else if(nrow(table) == 100 && table[,2] == 1:100) {
			as.character(table[,1])
		} else {
			as.character(table[match(1:100, table[,2]),1])
		}
	})

	###      2. Reduce data
	# discard empty tables
	lg <- sapply(tb, length)
	if(any(lg) != 100) {
		tb <- tb[lg==100]
		t <- t[lg==100]
	}
	stopifnot(length(t) == length(tb))

	tb <- do.call(cbind, tb)

	# get only players which ranked n° 1
	player1 <- unique(tb[1,])
	player1 <- player1[!is.na(player1)]
	if(!file.exists(n1.file)) save(player1, file = n1.file)
}

stopifnot(exists("player1"))

###  -------------------------------------------------------------------------------------------------------------  ###
###      2. load the ranking history of each n1 player
###  -------------------------------------------------------------------------------------------------------------  ###

player <-  structure(gsub("(^.*)\\s\\(.*\\)", "\\1", player1), names = gsub("^.*(\\(.{3}\\))", "\\1", player1))
atpurl.prefix <- "http://www.atpworldtour.com/Tennis/Players/Top-Players/"
atpurl.prefix2 <- "http://www.atpworldtour.com/Tennis/Players/"
atpurl.suffix <- ".aspx?t=rh"

#http://www.atpworldtour.com/Tennis/Players/Top-Players/Andy-Roddick.aspx?t=rh
#http://www.atpworldtour.com/Tennis/Players/Ro/A/Andy-Roddick.aspx
#http://www.atpworldtour.com/Tennis/Players/Na/I/Ilie-Nastase.aspx

n1.player <- lapply(player, function(p) {
	cat("\n", p)
	p.splt <- strsplit(p, ",\\s")
	url.player <- paste(atpurl.prefix, p.splt[[1]][2], "-", p.splt[[1]][1], atpurl.suffix, sep="")

	html.tb <- try(readHTMLTable(url.player, stringsAsFactors = FALSE), silent = T)

	if(class(html.tb) == "try-error") {
		# ugly hacks
		if( grepl("Newcombe", p)) {
			url.player <- paste(atpurl.prefix2, "Ne/J/John-D-Newcombe", atpurl.suffix, sep="")
			html.tb <- try(readHTMLTable(url.player, stringsAsFactors = FALSE), silent = T)
		} else if (grepl("Courier", p)) {
			url.player <- paste(atpurl.prefix2, "Co/J/Jim-S-Courier", atpurl.suffix, sep="")
			html.tb <- try(readHTMLTable(url.player, stringsAsFactors = FALSE), silent = T)
		} else {
			url.player <- paste(atpurl.prefix2, substr(p.splt[[1]][1], 0, 2), "/", substr(p.splt[[1]][2], 0, 1), "/",
				p.splt[[1]][2], "-", p.splt[[1]][1], atpurl.suffix, sep="")
			html.tb <- try(readHTMLTable(url.player, stringsAsFactors = FALSE), silent = T)
		}
	}
	stopifnot(is.list(html.tb), length(html.tb) == 1)
	html.tb <- html.tb[[1]]

	html <- getURL(url.player)
	html.list <- strsplit(html,"<ul ")[[1]]
	idx <- which(grepl("playerBioInfoList", html.list))
	dob <- gsub(".*(\\d{2}\\.\\d{2}\\.\\d{4}).*", "\\1", html.list[idx])

	list(atp = html.tb, dob = dob)
})
#save(n1.player, file="n1players_ranking.Rdata")

###  -------------------------------------------------------------------------------------------------------------  ###
###      3. load the ranking history of each n1 player
###  -------------------------------------------------------------------------------------------------------------  ###

atp <- lapply(n1.player, function(l) {
	stopifnot(names(l) == c("atp", "dob"))
	atp <- l[['atp']][1:2]
	atp <- atp[-c(1),]

	# discard empty ranking rows and T values
	idx <- !(atp[,2] == "")
	atp <- atp[idx,]
	#remove comma or trailing T
	atp[,2] <- gsub(",", "", atp[,2])
	atp[,2] <- gsub("T", "", atp[,2])
	zoo(as.numeric(atp[,2]), as.Date(atp[,1], format = "%d.%m.%Y"))
})
names(atp) <- player1

# get all the unique dates
dates <- as.Date(unique(unlist(sapply(atp, function(dff) index(dff)))))
dates <- dates[order(dates)]

# build a matrix of atp ranking with row = dates, col = player
n1.ranking <- zoo(matrix(nrow = length(dates), ncol = length(n1.player)), dates)
colnames(n1.ranking) <- player1

for (i in seq_along(atp)) {
	idx <- match(index(atp[[i]]), index(n1.ranking))
	n1.ranking[idx,i] <- atp[[i]]
}

###  -------------------------------------------------------------------------------------------------------------  ###
###      4. Plot heatmap
###  -------------------------------------------------------------------------------------------------------------  ###

date.reg <- c(seq(min(dates), max(dates), 7))

# ugly hack to add another date (for connors he was for one week n2, not visible otherwise)
date.reg <- c(date.reg, as.Date("23.08.1977", "%d.%m.%Y"))
date.reg <- date.reg[order(date.reg)]


idx <- findInterval(date.reg, index(n1.ranking), rightmost.closed = FALSE, all.inside = FALSE)
n1.eqranking <- zoo(unclass(n1.ranking)[idx,], date.reg)

ranks <- max(n1.eqranking, na.rm =T)

# ugly hack to fill ranking before 1985
dateLabel <- rep("", length(date.reg))
ii <- which(!duplicated(format(date.reg, format = "%Y")))
dateLabel[ii[-1]] <- format(date.reg[ii[-1]], "%Y")
dateLabel[ii[1]] <- format(date.reg[ii[1]], "%Y-%m-%d")



#png("fig/heatmap_atp.png", width = 1480, height = 1000, res = 200)
pdf("fig/heatmap_atp.pdf", width = 16, height = 9,  family="Georgia")
#svg("fig/heatmap_atp.svg", width = 16, height = 9,  family="Georgia", antialias= "none")

heatmap.2(t(log10(n1.eqranking)), Colv = NA, Rowv = NA, density.info = "none", dendrogram = "none", keysize = 1.1, margin = c(7,13),
	col = rev(colorRampPalette(brewer.pal(9,"Blues"))(255)), scale = "none", na.rm = T, cexCol = 0.9, trace= "none",
	lhei = c(0.8, 5), lwid = c(0.9,6), cexRow = 1.1, labCol = dateLabel, rowsep = 1:ncol(n1.eqranking), sepwidth = c(0, 0.01))
dev.off()


# age <- sapply(1:ncol(n1.ranking), function(i) as.numeric(diff(index(n1.ranking)[range(which(!is.na(n1.ranking[,i])))])) / 365.25 )

###  -------------------------------------------------------------------------------------------------------------  ###
###      5. Aging curve
###  -------------------------------------------------------------------------------------------------------------  ###

dob <- sapply(n1.player, function(l) {
	stopifnot(names(l) == c("atp", "dob"))
	as.Date(l[['dob']], format = "%d.%m.%Y")
})
# very ugly hack for
idxna <- sapply(dob, nchar) >=1
if(any(is.na(idxna)) && which(is.na(idxna))==21) {
	dob[[21]] <- as.Date("12.02.1980)", "%d.%M.%Y")
}

age.day <- do.call(cbind, lapply(dob, function(a) as.numeric(index(n1.ranking) - a)))
age <- age.day / 365.25

stopifnot(dim(age)==dim(n1.ranking))

n1.ar <- do.call(rbind, lapply(1:ncol(age), function(a) data.frame(age = age[,a], ranking = n1.ranking[,a], player = player[a])))
n1.ar <- n1.ar[which(!is.na(n1.ar$ranking)),]

# get the youngest and oldest age n1
n1.ar$color <- NA
#p.young <- n1.ar[which(n1.ar$age == min(n1.ar[n1.ar$ranking == 1,'age']) & n1.ar$ranking == 1), 'player']
iiidx <- which(n1.ar$age == max(n1.ar[n1.ar$ranking == 1,'age']) & n1.ar$ranking == 1)
p.old <- as.character(n1.ar[iiidx, 'player'])


n1.ar2 <- n1.ar[!n1.ar$player %in% levels(n1.ar$player)[1:4],]
n1.ar2$rage <- round(n1.ar2$age)
n1.ar3 <- ddply(n1.ar2, .(player,rage, color), summarize, yranking = mean(ranking, na.rm = T))
n1.ar3[n1.ar3$player == p.old,'color'] <- 'oldest'
#n1.ar3[n1.ar3$player == p.young,'color'] <- 'youngest'

# find the players still active
ip <- which(!is.na(n1.eqranking[nrow(n1.eqranking),]))
playerAge <- data.frame(player = c(player[ip], p.old ), age = c(sapply(dob[ip], function(d) Sys.Date()- d) / 365.25, n1.ar[iiidx, 'age']))



png("fig/agingRanking_%02d.png", width = 1480, height = 1000, res = 180)
ggplot(data = n1.ar3, aes(rage, yranking)) + geom_smooth(data = n1.ar2, aes(age, ranking), se = T, size = 1) + xlim(15, 35) + ylim (-30, 700) +
	 geom_line(aes(rage, yranking, group = player), colour = "grey", n = 100, se = F, size = 0.1) + ggtheme

ggplot(data = n1.ar3, aes(rage, yranking)) + geom_smooth(data = n1.ar2, aes(age, ranking), se = T, size = 1) + xlim(15, 35) + ylim (-30, 1000) +
 	 geom_smooth(aes(rage, yranking, group = player, colour = color), colour = "grey", n = 100, se = F, size = 0.15, span = 0.2) + ggtheme +
 	 geom_smooth(data =n1.ar3[!is.na(n1.ar3$color),], aes (rage, yranking, group = player, colour = color), n = 100, se = F, size = 0.2, span = 0.2) +
	 xlab("age") + ylab("yearly average ATP ranking")

ggplot(data = n1.ar3, aes(rage, yranking)) + geom_smooth(data = n1.ar2, aes(age, ranking), se = T, size = 1) + xlim(15, 35) + ylim (-30, 1000) +
	 geom_smooth(aes(rage, yranking, group = player, colour = color), colour = "grey", n = 100, se = F, size = 0.15, span = 0.2) + ggtheme +
	 geom_smooth(data =n1.ar3[!is.na(n1.ar3$color),], aes (rage, yranking, group = player, colour = color), n = 100, se = F, size = 0.2, span = 0.2)+
	 geom_vline(data = playerAge, aes(xintercept = age, colour = player), alpha = 0.5)

i <- 1
 while(i <= nlevels(n1.ar3$player)) {
	 if (i %% 5 == 0) {
		 p <- levels(n1.ar3$player)[(i-4):i]
		 #browser()
		 gp <- ggplot(data = n1.ar3[n1.ar3$player %in% p,], aes(rage, yranking)) + geom_smooth(data = n1.ar2, aes(age, ranking), se = T, size = 1) +
		 	xlim(15, 35) + ylim (-30, 700) + geom_smooth(aes(rage, yranking, group = player, group = player, colour = player),
			n = 100, se = F, size = 0.3, span = 0.2) + ggtheme

		 print(gp)

	 }
	 i <- i +1
}
dev.off()




