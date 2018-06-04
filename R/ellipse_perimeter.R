#' Calculate the length of the perimeter of an ellipse.
#'
#' @param r Length of the semi-major axis.
#' @param e The eccentricity.
#' @return The length of the perimeter of the ellipse with specified semi-major axis and eccentricity.
#' @examples
#' ellipse_perimeter(1, 1)
#' ellipse_perimeter(1)
#' @importFrom pracma ellipke
#' @export
ellipse_perimeter <- function(r, e = 0) {
  if (e < 0 || e > 1) stop("Invalid eccentricity!", call. = FALSE)
  if (r<= 0) stop("Invalid semi major axis!", call. = FALSE)
  if (e == 0) {
    2 * pi * r
  } else {
    4 * r * pracma::ellipke(e)$e
  }
}


