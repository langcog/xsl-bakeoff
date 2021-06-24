library("R.matlab")
abl = readMat("assoc_best_lex.mat")
length(unique(unlist(abl$best.a)))
sort(unique(unlist(abl$best.a)))
[1]   1   2   3   4   5   6   7   8   9  11  12  13  14  16  18  19  20  22  27  41  42  49  66  84  90  95 126 130
[29] 132 133 143 147 148 149 170 172 175 177 199 201 205 220 221 234 258 260 261 264 265 278 279 287 297 298 306 348
[57] 349 350 352 353 365 385 391 395 411 413 414 416

w = readMat("world.mat") # w$world[1], 2, etc 419 words, 22 objects
c = readMat("corpus.mat")
