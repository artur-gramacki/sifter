#' A helper function that automatically calls sifter() function in a loop
#'
#' @param data_main TODO
#' @param data_art TODO
#' @param class_col TODO
#' @param internal_number_col TODO
#' @param num_of_classes TODO
#' @param labels_cex TODO
#' @param mai_bottom TODO
#' @param clust_algo TODO
#' @param dist_method TODO
#' @param aggl_method TODO
#' @param dir TODO
#' @param save_2D TODO
#'
#' @importFrom grDevices graphics.off dev.off png
#'
#' @return TODO
#' @export
#'
#' @examples
#' file_toy <- system.file(
#'  "extdata", "toy_example.csv", package = "sifter")
#'
#' file_toy_artefacts <- system.file(
#'   "extdata", "toy_example_artefacts.csv", package = "sifter")
#'
#' data <- read.csv(
#'   file = file_toy,
#'   dec = ".",
#'   sep = "\t",
#'   header = TRUE,
#'   stringsAsFactors = TRUE
#' )
#'
#' data_artefact <- read.csv(
#'   file = file_toy_artefacts,
#'   dec = ".",
#'   sep = "\t",
#'   header = TRUE,
#'   stringsAsFactors = TRUE
#' )
#'
#' sifter_toolbox(
#'   data_main = data,
#'   data_art = data_artefact[2,],
#'   class_col = 5,
#'   internal_number_col = 6,
#'   num_of_classes = 2,
#'   dir = "sifter_results",
#'   save_2D = TRUE
#' )
#'
sifter_toolbox <- function(
    data_main,
    data_art,
    class_col,
    internal_number_col,
    num_of_classes,
    labels_cex = 0.7,
    mai_bottom = 1.5,
    clust_algo = "hc",
    dist_method = "euclidean",
    aggl_method = "ward",
    dir = "figs_sifter",
    save_2D = FALSE
)
{

  if (!dir.exists(dir)) {
    dir.create(dir)
    message("'", dir, "' ", "direcory was created in the current direcory.")
  }

  art_no <- data_art[, internal_number_col]

  graphics.off()
  fig_name <- paste(
    dir,
    "/artefact_obs_No_",
    art_no,
    "_fig_01",
    ".png",
    sep = "")
  png(fig_name, width = 1800, height = 1000, pointsize = 18)
  out <- sifter(
    data_main = data_main,
    data_art = data_art,
    class_col = class_col,
    internal_number_col = internal_number_col,
    num_of_classes = num_of_classes,
    labels_cex = labels_cex,
    mai_bottom = mai_bottom,
    clust_algo = clust_algo,
    dist_method = dist_method,
    aggl_method = aggl_method,
    verbose = TRUE)
  dev.off()

  if (save_2D) {
    graphics.off()
    fig_name <- paste(
      dir,
      "/artefact_obs_No_",
      art_no,
      "_fig_01_2Dmap",
      ".png",
      sep = "")
    png(fig_name, width = 800, height = 800, pointsize = 18)
    print(out$cluster_plot)
    dev.off()
  }

  out$is_homogenous

  # First step
  results_step_by_step <-
    matrix(
      data = "",
      nrow = nrow(out$data_with_artefact) + 1,
      ncol = nrow(out$data_with_artefact)
    )

  temp1 <- paste(
    colnames(data_art[class_col]), ":",
    data_art[,class_col], " - ",
    colnames(data_art[internal_number_col]), ":",
    data_art[,internal_number_col],
    sep = ""
  )

  temp2 <- paste(
    out$data_with_artefact[,out$internal_number_col],
    ":",
    out$data_with_artefact[,class_col],
    sep = "")

  results_step_by_step[, 1] <- c(temp2, temp1)

  # Next steps
  i <- 1
  while (!out$is_homogenous) {
    r <- which(rownames(out$data_with_artefact) == out$artefact_row)
    data_main_2 <- out$data_with_artefact[-r, ]
    data_art_2 <- out$data_with_artefact[r, ]

    i <- i + 1

    graphics.off()
    fig_name <- paste(
      dir,
      "/artefact_obs_No_",
      art_no, "_fig_",
      formatC(i, width = 2, format = "d", flag = "0"),
      ".png",
      sep = "")
    png(fig_name, width = 1800, height = 1000, pointsize = 18)
    out <- sifter(
      data_main = data_main_2,
      data_art = data_art_2,
      class_col = class_col,
      internal_number_col = internal_number_col,
      num_of_classes = num_of_classes,
      labels_cex = labels_cex,
      mai_bottom = mai_bottom,
      clust_algo = clust_algo,
      dist_method = dist_method,
      aggl_method = aggl_method,
      verbose = TRUE)
    dev.off()

    if (save_2D) {
      graphics.off()
      fig_name <- paste(
        dir,
        "/artefact_obs_No_",
        art_no, "_fig_",
        formatC(i, width = 2, format = "d", flag = "0"),
        "_2Dmap",
        ".png",
        sep = "")
      png(fig_name, width = 800, height = 800, pointsize = 18)
      print(out$cluster_plot)
      dev.off()
    }

    out$is_homogenous

    size <- nrow(out$data_with_artefact)
    results_step_by_step[1:size, i] <-
      paste(out$data_with_artefact[,internal_number_col],
            ":",
            out$data_with_artefact[,out$class_col],
            sep = "")
    # Handling the case when the number of records in `data_with_artefact`
    # is SMALLER than `num_of_classes`
    if (size < num_of_classes) {
      break
    }
  }

  if (i == 1) {
    results_step_by_step <- results_step_by_step[, 1]
  } else {
    results_step_by_step <- results_step_by_step[, 1:i]
    colnames(results_step_by_step) <- c(paste("step_", seq(1:i), sep = ""))
  }

  write.table(
    x = results_step_by_step,
    file = paste(dir, "/artefact_obs_No_", art_no, "_results_step_by_step.csv", sep = ""),
    quote = FALSE,
    row.names = FALSE,
    sep = ";")

  message("Results are in the '", dir, "' directory.")

  return(invisible(0))
}
