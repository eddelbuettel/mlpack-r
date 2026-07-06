#' @title Image Converter
#'
#' @description
#' A utility to load an image or set of images into a single dataset that can
#' then be used by other mlpack methods and utilities. This can also unpack an
#' image dataset into individual files, for instance after mlpack methods have
#' been used.
#'
#' @param input Image filenames which have to be loaded/saved (character
#'   vector).
#' @param channels Number of channels in the image.  Default value "0"
#'   (integer).
#' @param dataset Input matrix to save as images (numeric matrix).
#' @param height Height of the images.  Default value "0" (integer).
#' @param quality Compression of the image if saved as jpg (0-100). 
#'   Default value "90" (integer).
#' @param save Save a dataset as images.  Default value "FALSE" (logical).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#' @param width Width of the image.  Default value "0" (integer).
#'
#' @return A list with several components defining the class attributes:
#' \item{output}{Matrix to save images data to, Onlyneeded if you are
#'   specifying 'save' option (numeric matrix).}
#'
#' @details
#' This utility takes an image or an array of images and loads them to a matrix.
#' You can optionally specify the height "height" width "width" and channel
#' "channels" of the images that needs to be loaded; otherwise, these parameters
#' will be automatically detected from the image.
#' There are other options too, that can be specified such as "quality".
#' 
#' You can also provide a dataset and save them as images using "dataset" and
#' "save" as an parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' #  An example to load an image : 
#' # 
#' # \dontrun{
#' # Y <- image_converter(input=X, height=256, width=256, channels=3)
#' # }
#' # 
#' #  An example to save an image is :
#' # 
#' # \dontrun{
#' # image_converter(input=X, height=256, width=256, channels=3, dataset=Y,
#' #   save=TRUE)
#' # }
image_converter <- function(input,
                            channels = 0,
                            dataset = NA,
                            height = 0,
                            quality = 90,
                            save = FALSE,
                            verbose = getOption("mlpack.verbose", FALSE),
                            width = 0) {
  # Create parameters and timers objects.
  p <- CreateParams("image_converter")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamVecString(p, "input", input)

  SetParamInt(p, "channels", channels)

  if (!identical(dataset, NA)) {
    SetParamMat(p, "dataset", to_matrix(dataset), TRUE)
  }

  SetParamInt(p, "height", height)

  SetParamInt(p, "quality", quality)

  SetParamBool(p, "save", save)

  SetParamBool(p, "verbose", verbose)

  SetParamInt(p, "width", width)

  # Mark all output options as passed.
  SetPassed(p, "output")

  # Call the program.
  image_converter_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "output" = GetParamMat(p, "output")
  )

  # If output list is single element, flatten it.
  out <- out[[1]]

  # Add binding name as class to the output.
  class(out) <- c("mlpack_image", "mlpack_model_binding", "list")

  return(out)
}
