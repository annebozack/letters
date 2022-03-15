# copy the OS Webster's 2nd International Dictionary to somewhere you can find it 
# cp /usr/share/dict/words /Documents

# load libraries
library(tidyverse)
library(cape)
library(circlize)
library(wesanderson)

# load words
words = read_delim('Documents/words.csv', delim = '\n', col_names = F)

# convert to lower case
words[,1] = sapply(1:nrow(words), function(i) tolower(words[i,1]))

alphMat = matrix(data = 0, nrow=26, ncol=26)
colnames(alphMat) = letters
rownames(alphMat) = letters

# create a matix of times letters appear together in the same word
for (i in 1:nrow(words)){
	vect = strsplit(words[[1]][i], '')[[1]]  # create vector of letters in the word
	vect = vect[vect %in% letters]  # remove non-letter elements
	if (length(vect) > 1){
		pairs = t(combn(vect, 2))  # create matrix of pairs of letters
		# fill in tow of matrix
		for (i in 1:nrow(pairs)){
			alphMat[pairs[i,1], pairs[i,2]] = alphMat[pairs[i,1], pairs[i,2]] + 1
		}
		# fill in bottom of matrix
		for (i in 1:nrow(pairs)){
			alphMat[pairs[i,2], pairs[i,1]] = alphMat[pairs[i,2], pairs[i,1]] + 1
		}	
	}
}

# only use the upper triangular matrix to avoid duplicates
alphMatUpper = alphMat
for (i in 1:nrow(alphMatUpper)){
	alphMatUpper[i,c(1:i)] = 0
}

# create a list of colors for each letter
pal = wes_palette("Zissou1", 27, type = 'continuous')
pal = rev(pal)

# assigning colors to vowels
# a, e, i, o, u
# assigning colors to curvy and straight letters
# c s g b d p q m n h f j r l t k v w x y z

grid.col = c("a" = pal[1],
"b" = pal[10], 
"c" = pal[7],
"d" = pal[11], 
"e" = pal[2], 
"f" = pal[17], 
"g" = pal[9],
"h" = pal[16],
"i" = pal[3],
"j" = pal[18],
"k" = pal[22], 
"l" = pal[20],
"m" = pal[14],
"n" = pal[15],
"o" = pal[4],
"p" = pal[12], 
"q" = pal[13],
"r" = pal[19],
"s" = pal[8],
"t" = pal[21],
"u" = pal[5],
"v" = pal[23], 
"w" = pal[24], 
"x" = pal[25],
"y" = pal[26],
"z" = pal[27])

# adjustment value for grid labels
adj.list = c("a" = -2,
"b" = -2, 
"c" = -2,
"d" = -2, 
"e" = -2, 
"f" = -4, 
"g" = -2,
"h" = -2,
"i" = -6,
"j" = -6,
"k" = -2, 
"l" = -6,
"m" = -1,
"n" = -2,
"o" = -2,
"p" = -2, 
"q" = -2,
"r" = -4,
"s" = -2,
"t" = -4,
"u" = -2,
"v" = -2, 
"w" = -1.5, 
"x" = -2,
"y" = -2,
"z" = -2)

# directory to save plots
dir.create('Documents/letters')

# plot background color
par(bg = '#2D2D2D')

# plot and save chord diagram looping through letters 
for (i in 1:length(grid.col)){
	col.mat = matrix(data = '#FFFFFF26', nrow=26, ncol=26)
	col.mat[i,] = grid.col[i]
	col.mat[,i] = grid.col[i]
	
	grid.col.1 = grid.col
	grid.col.1[i] = '#00000000'
	grid.col.2 = grid.col
	grid.col.2[c(1:26)] = '#00000000'
	grid.col.2[i] = grid.col[i]

	circos.clear()

	chordDiagram(alphMatUpper, grid.col = grid.col, col = col.mat, annotationTrack = "grid", 
    	preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(alphMatUpper))))))
    
	circos.track(track.index = 1, panel.fun = function(x, y) {
    	circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
        facing = "clockwise", niceFacing = TRUE, col=grid.col.1[CELL_META$sector.index], adj = c(0, 0.5))
	}, bg.border = NA)
	
	circos.track(track.index = 2, panel.fun = function(x, y) {
    	circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
        facing = "clockwise", niceFacing = TRUE, col=grid.col.2[CELL_META$sector.index], cex = 2, adj = c(adj.list[CELL_META$sector.index], 0.5))
	}, bg.border = NA)

	quartz.save(paste0('Documents/letters/', names(grid.col)[i], '.png'), type = 'png', dpi = 300)
}

# convert to gif using ImageMagick
# convert -delay 100 *.png letters.gif


# make a legend
colors = rev(wes_palette("Zissou1", 27, type = 'continuous'))
df.pal = data.frame(x = c(1:27), y = 1)

ggplot(df.pal, aes(factor(x), y, fill = factor(x))) + geom_tile() + scale_fill_manual(values = colors) + 
	theme(legend.position = "none") + 
	scale_x_discrete(labels = c('      vowels', '', '', '', '', 'curvy', '', '', '', '', '', 'curvey & straight', '', '', '', '', '', 'straight', '', '', '', '', '', '', 'straight & angled   ', '', '')) + 
	theme(plot.background = element_rect(fill = '#2D2D2D', color = '#2D2D2D'), 
	panel.background = element_rect(fill = '#2D2D2D', color = '#2D2D2D'),
	panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
	panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
	axis.ticks = element_blank(), axis.title = element_blank(), 
	axis.text.y = element_blank(), axis.text.x = element_text(size = 12, color = 'lightgray'))
quartz.save(paste0('/Users/annebozack/Documents/letters/legend.png'), type = 'png', dpi = 300)
