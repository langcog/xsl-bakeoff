# Generate a few example trial orders with expected results for testing the models.

#' Get example ambiguous condition
#'
#' This function creates an example condition with two trials, and two word-object pairs
#' per trial. Each word-object pair appears only once, and thus each word has a 50% chance of 
#' being associated with the correct referent. The function returns a list of the trials
#' with nested words and objs per trial, as well as a vector of the conditional probability
#' of correctly selecting the correct referent given each word.
#'
#' @return A list of trials (with nested words and objects per trial) and perf (P(correct referent | word))
#' @export
get_example_ambiguous_condition = function() {
 	ord = list(trials = list(words = list(c(1,2),
 	                                      c(3,4)), 
 	                         objs = list(c(2,1),
 	                                     c(3,4))),
 	           perf = rep(0.5, 4)) # each word has 50% chance of being associated with correct referent
	return(ord)
}


#' Get example unambiguous condition
#'
#' This function creates an example condition with three trials and two word-object pairs
#' per trial. Each word-object pair appears twice, and so if the most frequently co-occurring
#' referent for each word is selected then all three words would be learned.
#' 
#' @export
get_example_unambiguous_condition = function() {
  ord = list(trials = list(words = list(c(1,2),
                                        c(1,3),
                                        c(3,2)), 
                           objs = list(c(2,1),
                                       c(1,3),
                                       c(2,3))),
             perf = rep(0.5, 3)) # each word has 50% chance of being associated with correct referent
  return(ord)
}
