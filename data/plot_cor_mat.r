function (d) 
{
#This function plots the correlations of individual subject ratings
#with mean subject ratings for each word.
#d is a matrix output by get.cor.matrix
################################

    nw <- dim(d)[1] #Number of words
    nsj <- dim(d)[2] #Number of subjects
	
	#Generate blank plot
    plot(0, 0, type = "n", ylim = range(d), xlim = c(1, nw), 
        xlab = "Word", ylab = "Correlation")
		
	#Add individual subjects plotted as gray lines/dots
    for (i in c(1:nsj)) points(c(1:nw), d[, i], col = gray(0.8), 
        type = "o", pch = 16)
		
	#Add mean across subjects for each word as large black dot
    points(c(1:nw), rowMeans(d), pch = 16, cex = 2, col = 1)
	
    abline(h = 0) #Add horizontal line at r=0
}
