# Liam's MCA code
# binary_matrix <-
#   round7[, c("nrad","bradb","NOrad", "ntv","btvb","NOtv", "nveh","bvehb","NOveh", "ncom","bcomb", "NOcom", "npho", "bphob","NOpho",  "nban",     "bbanb", "NOban")]
# #creating MCA
# mca_result <- MCA(binary_matrix,graph = TRUE)
# round7$assdex <-
#   mca_result$ind$coord[, 1]
# round7$assdex  <-
#   (round7$assdex  + abs(min(round7$assdex)))# no negative
# print(mca_result$var$coord[, 1])
# remove(mca_result)
# remove(binary_matrix)