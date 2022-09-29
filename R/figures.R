globine <- c(2, 5, 10, 20, 30, 50, 100)
colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

mark_heatwaves <- function(x, boundary) {
  x$hot[1] <- FALSE
  x$hot[2] <- FALSE
  for (i in 3:(length(x$temp))) {
    if ((x$temp[i] >= boundary) & (x$temp[i-1] >= boundary) & (x$temp[i-2] >= boundary)) {
      x$hot[i] <- TRUE
      x$hot[i-1] <- TRUE
      x$hot[i-2] <- TRUE
    } else {
      x$hot[i] <- FALSE
    }
  }
  return(x)
}

heatwave_start_indices <- function(x) {
  indices <- c()
  for (i in 2:length(x$temp)) {
    if (x$hot[i] == TRUE & x$hot[i-1] == FALSE) {
      print(x$date[i])
      indices <- c(indices, i)
    }
  }
  return(indices)
}

plottemps <- function(station, index, boundary, ime, col) {
  print(station$date[index])
  daysbefore = 5
  daysafter = 15
  ip = index - daysbefore
  id = index + daysafter
  plot(station$date[ip:id], station$temp[ip:id], ylim=c(15, 35), ylab="T [\u00B0C]", xlab="Date", type="o", main=ime)
  #axis(1, at = station$date[ip:id], labels = seq(-daysbefore, daysafter, 1))
  points(station$date[ip:id], station$Ttla2cm[ip:id], type="b", col=colors[1], pch=0)
  points(station$date[ip:id], station$Ttla5cm[ip:id], type="b", col=colors[2], pch=19)
  points(station$date[ip:id], station$Ttla10cm[ip:id], type="b", col=colors[3], pch=0)
  points(station$date[ip:id], station$Ttla20cm[ip:id], type="b", col=colors[4], pch=19)
  points(station$date[ip:id], station$Ttla30cm[ip:id], type="b", col=colors[5], pch=0)
  points(station$date[ip:id], station$Ttla50cm[ip:id], type="b", col=colors[6], pch=19)
  points(station$date[ip:id], station$Ttla100cm[ip:id], type="b", col=colors[7], pch=0)
  abline(h=boundary, col="red")
  legend(station$date[index] + daysafter - 2.4, 22.7, legend=c(expression("T"[air2m]), expression("T"[soil2cm]), expression("T"[soil5cm]), expression("T"[soil10cm]), expression("T"[soil20cm]), expression("T"[soil30cm]), expression("T"[soil50cm]), expression("T"[soil100cm])), 
         col=col, lty=1, cex=0.84, pch=c(1, 0, 19, 0, 19, 0, 19, 0))
}

plotaverage <- function(station, boundary, averages, ymin, ymax, yleg, name) {
  daysbefore = 5
  daysafter = 15
  plot(seq(-daysbefore, daysafter), averages[[1]], ylim=c(ymin, ymax), ylab="T [\u00B0C]", xlab="Days after the beginning of the heatwave", type="o", main=name)
  for (i in 2:8) {
    pch = 19
    if (i %% 2 == 0) {
      pch = 0
    }
    points(seq(-daysbefore, daysafter), averages[[i]], type="b", col=colors[i-1], pch=pch)
  }
  abline(h=boundary, col="red")
}

plotslopes <- function(station, boundary, odpov, ymin, ymax, yleg, name) {
  daysbefore = 3
  daysafter = 10
  plot(seq(-daysbefore, daysafter-1)+0.5, odpov[[1]][(6-daysbefore):(5+daysafter)], ylim=c(ymin, ymax), ylab="slope [\u00B0C / day]", xlab="Days after the beginning of the heatwave", type="o", col=1, main=name)
  for (i in 2:8) {
    pch = 19
    if (i %% 2 == 0) {
      pch = 0
    }
    points(seq(-daysbefore, daysafter-1)+0.5, odpov[[i]][(6-daysbefore):(5+daysafter)], type="b", col=colors[i-1], pch=pch)
  }
  abline(h=boundary, col="red")
}

averagetemps <- function(stolpec, indices) {
  daysbefore = 5
  daysafter = 15
  vsote <- rep(0, daysbefore+daysafter+1)
  count <- rep(0, daysbefore+daysafter+1)
  
  for (j in -daysbefore:daysafter) {
    for (i in indices) {
      if (!is.na(stolpec[i+j])) {
        vsote[j+daysbefore+1] <- vsote[j+daysbefore+1] + stolpec[i+j]
        count[j+daysbefore+1] <- count[j+daysbefore+1] + 1
      }
    }
  }
  
  average <- c()
  for (l in 1:length(vsote)) {
    average[l] <- vsote[l] / count[l]
  }
  
  return(average)
}

averagedays <- function(station, indices) {
  averages_temp <- averagetemps(station$temp, indices)
  averages_Ttla2cm <- averagetemps(station$Ttla2cm, indices)
  averages_Ttla5cm <- averagetemps(station$Ttla5cm, indices)
  averages_Ttla10cm <- averagetemps(station$Ttla10cm, indices)
  averages_Ttla20cm <- averagetemps(station$Ttla20cm, indices)
  averages_Ttla30cm <- averagetemps(station$Ttla30cm, indices)
  averages_Ttla50cm <- averagetemps(station$Ttla50cm, indices)
  averages_Ttla100cm <- averagetemps(station$Ttla100cm, indices)
  averages <- list(averages_temp, averages_Ttla2cm, averages_Ttla5cm, averages_Ttla10cm, 
                    averages_Ttla20cm, averages_Ttla30cm, averages_Ttla50cm, averages_Ttla100cm)
  return(averages)
}

maximums <- function(averages) {
  daysbefore = 5
  maks <- c()
  for (i in 1:length(averages)) {
    maks[i] <- which.max(averages[[i]]) - 5
  }
  return(maks)
}

slopes <- function(averages) {
  slope1 <- c()
  slope2 <- c()
  slope3 <- c()
  slope4 <- c()
  slope5 <- c()
  slope6 <- c()
  slope7 <- c()
  slope8 <- c()
  for (j in 1:length(averages[[1]])-1) {
    slope1[j] <- averages[[1]][j+1] - averages[[1]][j]
    slope2[j] <- averages[[2]][j+1] - averages[[2]][j]
    slope3[j] <- averages[[3]][j+1] - averages[[3]][j]
    slope4[j] <- averages[[4]][j+1] - averages[[4]][j]
    slope5[j] <- averages[[5]][j+1] - averages[[5]][j]
    slope6[j] <- averages[[6]][j+1] - averages[[6]][j]
    slope7[j] <- averages[[7]][j+1] - averages[[7]][j]
    slope8[j] <- averages[[8]][j+1] - averages[[8]][j]
  }
  slope <- list(slope1, slope2, slope3, slope4, slope5, slope6, slope7, slope8)
  return(slope)
}

savefig <- function(name) {
  png(paste('figures/', name, '.png', sep=""), width = 9.2, height = 6, units = 'in', res = 300)
}

savepdf <- function(name) {
  grDevices::cairo_pdf(paste('figures/', name, '.pdf', sep=""), width = 9.2, height = 6)
}

saveeps <- function(name) {
  cairo_ps(paste('figures/', name, '.eps', sep=""), width = 9.2, height = 6)
}

# BILJE

t_bilje <- read.table("data/t-bilje.csv", header=TRUE, sep = ",", dec=".");
t_bilje$date <- as.Date(t_bilje$valid, "%d/%m/%Y") # convert date
tla_bilje <- read.table("data/tla-bilje.csv", header=TRUE, sep = ",", dec=".");
tla_bilje$date <- as.Date(paste(tla_bilje$leto, tla_bilje$mesec, tla_bilje$dan, sep="-"), "%Y-%m-%d") # convert day, month, year to date

summary(tla_bilje)
sum(is.na(tla_bilje))
summary(t_bilje)
sum(is.na(t_bilje))
tla_bilje_manjkajoci <- tla_bilje[rowSums(is.na(tla_bilje)) > 0,]
tla_bilje_manjkajoci$date

t_bilje <- mark_heatwaves(t_bilje, 25)
bilje <- merge(t_bilje, tla_bilje)
ind_bilje <- heatwave_start_indices(bilje)
averages_bilje <- averagedays(bilje, ind_bilje)
maks_bilje <- maximums(averages_bilje)
slopes_bilje <- slopes(averages_bilje)

# LJUBLJANA

t_ljubljana <- read.table("data/t-lj.csv", header=TRUE, sep = ",", dec=".");
t_ljubljana$date <- as.Date(t_ljubljana$valid, "%d/%m/%Y")
tla_ljubljana <- read.table("data/tla-lj.csv", header=TRUE, sep = ",", dec=".");
tla_ljubljana$date <- as.Date(paste(tla_ljubljana$leto, tla_ljubljana$mesec, tla_ljubljana$dan, sep="-"), "%Y-%m-%d")

summary(tla_ljubljana)
sum(is.na(tla_ljubljana))
summary(t_ljubljana)
sum(is.na(t_ljubljana))
tla_ljubljana_manjkajoci <- tla_ljubljana[rowSums(is.na(tla_ljubljana)) > 0,]
tla_ljubljana_manjkajoci$date

t_ljubljana <- mark_heatwaves(t_ljubljana, 24)
ljubljana <- merge(t_ljubljana, tla_ljubljana)
ind_ljubljana <- heatwave_start_indices(ljubljana)
averages_ljubljana <- averagedays(ljubljana, ind_ljubljana)
maks_ljubljana <- maximums(averages_ljubljana)
slopes_ljubljana <- slopes(averages_ljubljana)

# LESCE
t_lesce <- read.table("data/t-lesce.csv", header=TRUE, sep = ",", dec=".");
t_lesce$date <- as.Date(t_lesce$valid, "%d/%m/%Y")
tla_lesce <- read.table("data/tla-lesce.csv", header=TRUE, sep = ",", dec=".");
tla_lesce$date <- as.Date(paste(tla_lesce$leto, tla_lesce$mesec, tla_lesce$dan, sep="-"), "%Y-%m-%d")

summary(tla_lesce)
sum(is.na(tla_lesce))
summary(t_lesce)
sum(is.na(t_lesce))
tla_lesce_manjkajoci <- tla_lesce[rowSums(is.na(tla_lesce)) > 0,]
tla_lesce_manjkajoci$date

t_lesce <- mark_heatwaves(t_lesce, 22)
lesce <- merge(t_lesce, tla_lesce)
ind_lesce <- heatwave_start_indices(lesce)
averages_lesce <- averagedays(lesce, ind_lesce)
maks_lesce <- maximums(averages_lesce)
slopes_lesce <- slopes(averages_lesce)

# NOVO MESTO
t_nm <- read.table("data/t-nm.csv", header=TRUE, sep = ",", dec=".");
t_nm$date <- as.Date(t_nm$valid, "%d/%m/%Y")
tla_nm <- read.table("data/tla-nm.csv", header=TRUE, sep = ",", dec=".");
tla_nm$date <- as.Date(paste(tla_nm$leto, tla_nm$mesec, tla_nm$dan, sep="-"), "%Y-%m-%d")

summary(tla_nm)
sum(is.na(tla_nm))
summary(t_nm)
sum(is.na(t_nm))
tla_nm_manjkajoci <- tla_nm[rowSums(is.na(tla_nm)) > 0,]
tla_nm_manjkajoci$date

t_nm <- mark_heatwaves(t_nm, 24)
nm <- merge(t_nm, tla_nm)
ind_nm <- heatwave_start_indices(nm)
averages_nm <- averagedays(nm, ind_nm)
maks_nm <- maximums(averages_nm)
slopes_nm <- slopes(averages_nm)

length(ind_bilje)
bilje$date[ind_bilje]

length(ind_ljubljana)
ljubljana$date[ind_ljubljana]

length(ind_lesce)
lesce$date[ind_lesce]

length(ind_nm)
nm$date[ind_nm]


# FIGURES

savefig('example-bilje')
savepdf('example-bilje')
saveeps('example-bilje')
plottemps(bilje, ind_bilje[16], 25, "Bilje", col=c(1,colors))
bilje$date[ind_bilje[16]]
dev.off()

savefig('averages')
savepdf('averages')
saveeps('averages')
layout(matrix(c(1,2,3,4,5,3), 2, 3, byrow = TRUE), widths=c(3,3,1.2))
par(mar=c(5,4,4,2)+0.1)
plotaverage(bilje, 25, averages_bilje, 22, 32, 32, "Bilje")
plotaverage(ljubljana, 24, averages_ljubljana, 19, 29, 29, "Ljubljana - Bežigrad")
par(mar=c(5,0,4,1))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x="center", y="center", inset=0, legend=c(expression("T"[air2m]), expression("T"[soil2cm]), expression("T"[soil5cm]), expression("T"[soil10cm]), expression("T"[soil20cm]), expression("T"[soil30cm]), expression("T"[soil50cm]), expression("T"[soil100cm])), 
       col=c(1,colors), lty=1, cex=1, pch=c(1, 0, 19, 0, 19, 0, 19, 0))
par(mar=c(5,4,4,2)+0.1)
plotaverage(lesce, 22, averages_lesce, 17, 28, 28, "Lesce")
plotaverage(nm, 24, averages_nm, 18, 28, 28, "Novo mesto")
dev.off()

savefig('slopes')
savepdf('slopes')
saveeps('slopes')
layout(matrix(c(1,2,3,4,5,3), 2, 3, byrow = TRUE), widths=c(3,3,1.2))
par(mar=c(5,4,4,2)+0.1)
plotslopes(bilje, 0, slopes_bilje, -1, 2, 2, "Bilje")
plotslopes(ljubljana, 0, slopes_ljubljana, -1.2, 2, 2, "Ljubljana - Bežigrad")
par(mar=c(5,0,4,1))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x="center", y="center", inset=0, legend=c(expression("T"[air2m]), expression("T"[soil2cm]), expression("T"[soil5cm]), expression("T"[soil10cm]), expression("T"[soil20cm]), expression("T"[soil30cm]), expression("T"[soil50cm]), expression("T"[soil100cm])), 
       col=c(1,colors), lty=1, cex=1, pch=c(1, 0, 19, 0, 19, 0, 19, 0))
par(mar=c(5,4,4,2)+0.1)
plotslopes(lesce, 0, slopes_lesce, -1.2, 2, 2, "Lesce")
plotslopes(nm, 0, slopes_nm, -1.5, 2, 2, "Novo mesto")
dev.off()
