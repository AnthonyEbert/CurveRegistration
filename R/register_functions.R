#' @export
register_functions <- function(obs, sim, method){

  reg_output <- fdasrvf::pair_align_functions(obs[,2], sim[,2], time = obs[,1], method = method)

  values <- reg_output$f2tilde

  #phase <- elastic.distance(obs[,2], sim[,2], time = obs[,1])

  return(list(values = values, gam = reg_output$gam))

}
