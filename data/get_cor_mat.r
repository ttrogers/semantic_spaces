function (d = alldat$FruitAssoc) 
{
#This function computes, for each subject and word, the
#correlation between the subject's ratings across colors
#and the mean of all other subject ratings across colors
#d is a 3D array containing the individual subject ratings
#formatted as in the original experiment
##########################

    nsj <- dim(d)[3] #Number of subjects
    nw <- dim(d)[2]  #Number of words
    o <- matrix(0, nw, nsj)  #Initialize output matrix
	
	#Loop over words and subjects to compute each correlation and
	#store in output matrix
    for (i in c(1:nw)) for (j in c(1:nsj)) {
        s <- d[, i, j]
        m <- rowMeans(d[, i, c(1:nsj) != j])
        o[i, j] <- cor(s, m)
    }
    o #Return output matrix
}
