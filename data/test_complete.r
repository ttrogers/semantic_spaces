function (d, p = 0.1, r = 4, plt = T, w = fruits, jc = judgedcols) 
{
#This function does matrix completion and plots results for
#a random hold-out set
#d: word-by-color matrix containing mean ratings
#p: Proportion of cells to use for hold-outs
#r: Maximum rank of matrix
#plt: Flag, should results be plotted?
#w: Character vector containing words
###########################################

    wm <- matrix(w, dim(d)[1], dim(d)[2]) #Matrix indicating word in each cell, for plotting later
	jcm <- t(matrix(jc, dim(d)[2], dim(d)[1])) #Matrix indicating color for each cell, for plotting later
    trn <- d #Make training matrix
    ncells <- dim(d)[1] * dim(d)[2] #How many cells in matrix?
    trn[sample(c(1:ncells), floor(p * ncells))] <- NA  #Remove hold-outs from training matrix
    m <- softImpute(trn, type = "als", rank.max = r) #Fit model
    pred <- complete(trn, m) #Make predictions for held-out items
	
	#Generate plot
    if (plt) {
        plot(d[!is.na(trn)], pred[!is.na(trn)], xlab = "True", 
            ylab = "Predicted", pch = 16, col = gray(0.8), xlim = range(d), 
            ylim = range(pred))
        points(d[is.na(trn)], pred[is.na(trn)], pch = 16,
			   col=jcm[is.na(trn)])
        text(d[is.na(trn)], pred[is.na(trn)], label = wm[is.na(trn)], 
            cex = 0.7, col=jcm[is.na(trn)])
    }
    cor(d[is.na(trn)], pred[is.na(trn)])
}
