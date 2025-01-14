#' Transform parameters for fitting procedure
#'
#' @param theta data.frame with parameters named s and g and for transformed paramaters st and gt
#' @param gamma_positive boolean indicating whether gamma should be positive
#' @param inverse_transform boolean indicating wheter the tranformation should be inverted
#' @return tranformed parameters
#' @details
#' In the parameter the
#' The transformation is done based on parameter change where for example sigma = exp(sigma_transformed).
#' The trasformation is from sigma_transformed to sigma and the inverse transformation is from the other way around.
#' @export
transform_parameters <- function(theta,gamma_positive,inverse_transform=F){
  if(inverse_transform){
    if(gamma_positive){
      theta_transformed = data.frame(st = log(theta$s), gt = log(theta$g))
    } else{
      theta_transformed = data.frame(st = log(theta$s), gt = log(theta$g+0.1*exp(theta$s)))
    }
  } else{
    if(gamma_positive){
      theta_transformed = data.frame(s = exp(theta$st), g = exp(theta$gt))
    } else{
      theta_transformed = data.frame(s = exp(theta$st), g = -0.1*exp(theta$st) + exp(theta$gt))
    }
  }
}
