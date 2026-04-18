#' The main function for sifting algorithm
#'
#' The function generates a series of Agglomerative Hierarchical Clustering (AHC) trees.
#'
#' @param data_main Input data. See \code{toy_example.csv} file available through
#' the \code{system.file()} function.
#'
#' @param data_art Input data with artefacts. See \code{toy_example_artefacts.csv}
#' file available through the \code{system.file()} function.
#'
#' @param class_col The column number in the input data that contains the class identifier.
#'
#' @param internal_number_col The column number in the input data that contains unique
#' IDs for each record.
#'
#' @param num_of_classes The number of classes into which the tree should be divided.
#'
#' @param cutting_at_height The height at which the tree is cut.
#'
#' @param labels_cex \code{cex} param for tree leaves/
#'
#' @param mai_bottom \code{mai} bottom param. Used when leaf labels are long and may
#' not fit entirely in the plot.
#'
#' @param clust_algo One of these:
#'  \itemize{
#'    \item \code{"hc"} - the standard hierarchical clustering
#'    \item \code{"agnes"} - an enhanced agglomerative hierarchical clustering algorithm
#'    \item \code{"diana"} - a divisive (top-down) hierarchical clustering algorithm
#'  }
#'
#' @param dist_method One of these:
#'  \itemize{
#'    \item \code{"euclidean"}
#'    \item \code{"maximum"}
#'    \item \code{"manhattan"}
#'    \item \code{"canberra"}
#'    \item \code{"binary"}
#'    \code{"minkowski"}
#'  }
#'
#' @param aggl_method
#' For \code{(clust_algo == "hc")} the acceptable values are:
#'  \itemize{
#'    \item \code{"ward"}
#'    \item \code{"single"}
#'    \item \code{"complete"}
#'    \item \code{"average"}
#'    \item \code{"mcquitty"}
#'    \item \code{"median"}
#'    \item \code{"centroid"}
#'  }
#'
#' For \code{(clust_algo == "agnes")} the acceptable values are:
#'  \itemize{
#'    \item \code{"ward"}
#'    \item \code{"single"}
#'    \item \code{"complete"}
#'    \item \code{"average"}
#'    \item \code{"weighted"}
#'    \item \code{"flexible"}
#'    \item \code{"gaverage"}
#'  }
#'
#' @param plot_2D If \code{TRUE} 2D cluster plot  will be displayed on the screen.
#'
#' @param verbose If \code{TRUE} results will be displayed on the screen (AHC tree and 2D cluster plot).
#'
#' @importFrom dendextend labels_colors rect.dendrogram cutree set
#' @importFrom RColorBrewer brewer.pal
#' @importFrom factoextra fviz_cluster
#' @importFrom cluster agnes diana
#' @importFrom grDevices colorRampPalette dev.copy
#' @importFrom ggplot2 geom_text geom_point aes
#' @importFrom graphics abline par
#' @importFrom utils write.table
#' @importFrom stats dist hclust as.dendrogram order.dendrogram
#'
#' @return
#'  \itemize{
#'    \item \code{cluster_plot}: a ggplot object representing the cluster plot is returned
#'    if either \code{sum_of_classes} or \code{cutting_at_height} is specified;
#'    otherwise, \code{NA} is returned.
#'
#'    \item \code{dend}: an object of class \code{"dendrogram"} is returned.
#'
#'    \item \code{hc}: depending on the value of the \code{clust_algo} parameter, the
#'          appropriate object is returned (\code{"hclust"}, \code{"agnes"}
#'          or \code{"diana"}).
#'
#'    \item \code{class_col}, \code{internal_number_col}, \code{artefact_row}
#'          \code{num_of_classes}, \code{cutting_at_height}:
#'     these are the values of variables passed to the function as inputs.
#'
#'    \item \code{group_with_artefact} The number of the group in which the artefact
#'    is located.
#'
#'    \item \code{is_homogenous} Returns \code{TRUE} when a homogeneous class has
#'    been achieved (as defined by the implemented algorithm).
#'
#'    \item \code{data_with_artefact} Returns the record with the artefact
#'    (the one passed to the function with the \code{data_art} parameter).
#'
#'    \item \code{artefact_position_in_tree} The artifact's position in the tree,
#'    counting leaves from the left.
#'  }
#' @export
#'
#' @examples
#' # Reading a very simple, small toy dataset.
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
#' print(data)
#' print(data_artefact)
#'
#' # Only the required parameters are given.
#' out <- sifter(
#'   data_main = data,
#'   class_col = 5,
#'   internal_number_col = 6
#' )
#'
#' # Specifying another clustering algorithm
#' # (one of the three supported by the function).
#' out <- sifter(
#'   data_main = data,
#'   class_col = 5,
#'   internal_number_col = 6,
#'   clust_algo = "agnes" # c("diana", "agnes", "hc")
#' )
#'
#' # Specify the number of classes.
#' out <- sifter(
#'   data_main = data,
#'   class_col = 5,
#'   internal_number_col = 6,
#'   num_of_classes = 2
#' )
#'
#' # Specifying the height at which we cut the tree.
#' out <- sifter(
#'   data_main = data,
#'   class_col = 5,
#'   internal_number_col = 6,
#'   cutting_at_height = 8
#' )
#'
#' # Specifying the height at which we cut the tree AND an artefact.
#' # The class with the artifact is surrounded by a blue frame.
#' out <- sifter(
#'   data_main = data,
#'   data_art = data_artefact[2,],
#'   class_col = 5,
#'   internal_number_col = 6,
#'   cutting_at_height = 10
#' )
#'
#' # Tree and 2D cluster plot.
#' out <- sifter(
#'   data_main = data,
#'   class_col = 5,
#'   internal_number_col = 6,
#'   num_of_classes = 2,
#'   data_art = data_artefact[2,],
#'   plot_2D = TRUE
#' )
#'
#'
sifter <- function(
    data_main,
    data_art = NULL,
    class_col,
    internal_number_col,
    num_of_classes = NULL,
    cutting_at_height = NULL,
    labels_cex = 0.7,
    mai_bottom = 1.5,
    clust_algo = "hc",
    dist_method = "euclidean",
    aggl_method = "ward",
    plot_2D = FALSE,
    verbose = TRUE
)
{

  # All columns outside the range c(1:internal_number_col) are ignored.
  data_main <- data_main[, c(1:internal_number_col)]
  data_art <- data_art[, c(1:internal_number_col)]

  if (!is.null(num_of_classes) && !is.null(cutting_at_height)) {
    stop("Specify exactly one of 'num_of_classes' or 'cutting_at_height'.")
  }

  if (!is.null(data_art)) {
    if(nrow(data_art) != 1) {
      stop("Only one row in `data_art` is allowed (i.e. only one artifact can be analyzed at a time).")
    }
  }

  if (clust_algo == "hc") {
    temp <- c("ward", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
    if (!any(aggl_method == temp)) {
      stop(
        "If 'clust_algo' = 'hc' then 'aggl_method' can be only one of the following:
      'ward', 'single', 'complete', 'average', 'mcquitty', 'median', 'centroid'")
    }
  }

  if (clust_algo == "agnes") {
    temp <- c("ward", "single", "complete", "average", "weighted", "flexible", "gaverage")
    if (!any(aggl_method == temp)) {
      stop(
        "If 'clust_algo' = 'agnes' then 'aggl_method' can be only one of the following:
      'ward', 'single', 'complete', 'average', 'weighted', 'flexible', 'gaverage'")
    }
  }

  # Check if the column names are identical.
  test <- colnames(data_main) == colnames(data_art)
  if (!all(test)) {
    stop("Column names in both `data_main` and `data_art` must be identical.")
  }

  gg_plot <- NA
  data_with_artefact <- NULL
  is_homogenous <- NULL
  group_with_artefact <- NULL
  artefact_position_in_tree <- NULL
  nrows_main <- nrow(data_main)
  labs_cex <- rep(labels_cex, nrows_main)

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # step 1 ----
  # If an artefact is provided, append it to the input data
  # 'data' variable
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if (!is.null(data_art))  {
    nrows_art <- nrow(data_art)
    data <- rbind(data_main, data_art)
    data[, class_col] <- droplevels(data[, class_col]) # drop unused levels
    rownames(data) <- seq(1:(nrows_main + nrows_art))
    artefact_row <- nrow(data)
  } else {
    data <- data_main
    artefact_row <- NA
  }

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # step 2 ----
  # Determining the distance matrix
  # Data clustering (using the selected method)
  # Dendrogram construction
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  dd <- stats::dist(data[, -c(class_col, internal_number_col)], method = dist_method)

  if (clust_algo == "hc") {
    if (aggl_method == "ward") aggl_method = "ward.D2"
    hc <- stats::hclust(dd, method = aggl_method)
  } else if (clust_algo == "agnes") {
    hc <- cluster::agnes(dd, method = aggl_method)
  } else if (clust_algo == "diana") {
    hc <- cluster::diana(dd)
  } else {
    stop("Unknown clustering algorithm. Supported ones: 'hc', 'diana', 'agnes'.")
  }

  dend <- stats::as.dendrogram(hc)

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # step 3 ----
  # Determining the position of the artifact in the tree
  # Artefact paint in black
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if (!is.null(data_art)) {
    artefact_position_in_tree <- which(hc$order == artefact_row)
  }

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # step 4 ----
  # Determining the number of classes if the cutting height of the tree is given
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if (!is.null(cutting_at_height)) {
    tree_h <- max(hc$height)
    if (tree_h < cutting_at_height) {
      msg <-
        paste("'cutting_at_height' cannot exceed the actual tree height. ",
              "The actual tree height is ", format(tree_h, digits = 3), ". ",
              "The 'cutting_at_height' provided by the user is ", format(cutting_at_height, digits = 3), ".",
              sep = "")
      stop(msg)
    }
    kk <- length(unique(dendextend::cutree(dend, h = cutting_at_height)))
    num_of_classes <- kk
  }

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # step 5 ----
  # Checking whether a homogeneous class has been achieved
  # Must be AFTER step 6 before dend labels are modified
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if (!is.null(num_of_classes) || !is.null(cutting_at_height)) {
    ct <- dendextend::cutree(dend, k = num_of_classes, order_clusters_as_data = FALSE)
    unname(ct)
    group_with_artefact <- unname(ct[artefact_position_in_tree])

    idx <- which(unname(ct) == group_with_artefact)
    a <- ct[idx]
    b <- names(a)
    c <- as.numeric(b)
    data_with_artefact <- data[c,]
    c <- as.character(data_with_artefact[, class_col])
    lc <- length(unique(c))
    if (lc == 2 || lc == 1) is_homogenous = TRUE else is_homogenous = FALSE
  }

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # step 6 ----
  # Determining the colors and labels of each class
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  classes <- data[, class_col]
  colors_to_use <- as.numeric(classes)
  colors_to_use <- colors_to_use[order.dendrogram(dend)]

  len <- length(unique(colors_to_use))
  if (len > 12) {
    colors.12 <- RColorBrewer::brewer.pal(n = 12, name = 'Paired')
    # Interpolate to have more colors
    colors <- grDevices::colorRampPalette(colors.12)(len)
  } else {
    colors <- c("red", "blue", "green", "brown", "tomato1", "seagreen", "brown2",
                "deeppink", "blueviolet", "orange2", "wheat", "orchid4")
  }

  final_colors <- colors[colors_to_use]
  final_colors[artefact_position_in_tree] <- "black"
  dendextend::labels_colors(dend) <- final_colors
  labs_cex[artefact_position_in_tree] <- labels_cex + 0.2

  # new labels
  my_labels <- paste(
    data[order.dendrogram(dend),internal_number_col],
    ':',
    data[order.dendrogram(dend),class_col],
    sep = "")

  # After this step, the leaf node labels are modified (class names are added)
  # plot(dend) to see the effect

  ### NOTE ###
  # When it was like below (commented), CRAN test generates the following error:
  # Error in labels(dend) <- my_labels : could not find function "labels<-"
  # labels(dend) <- my_labels
  #
  # This works without CRAN error
  dend <- dendextend::set(dend, "labels", my_labels)


  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # step 7 ----
  # Setting various graphic parameters of the tree
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  dend <- set(dend, "labels_cex", labs_cex)
  dend <- set(dend, "leaves_pch", 15)
  dend <- set(dend, "leaves_cex", 0.6)
  dend <- set(dend, "leaves_col", final_colors)
  dend <- set(dend, "branches_lwd", 2)
  dend <- set(dend, "branches_col", "black")

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # step 8 ----
  # 2D visualisation of data
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if (!is.null(num_of_classes) || !is.null(cutting_at_height)) {
    sub_grps <- cutree(hc, k = num_of_classes)

    gg_temp_1 <- fviz_cluster(list(data = data[, -c(class_col, internal_number_col)],
                                   cluster = sub_grps), geom = "point")

    xx <- gg_temp_1@data$x
    yy <- gg_temp_1@data$y

    gg_temp_2 <- gg_temp_1 +
      geom_text(aes(x = xx, y = yy, label = data[, internal_number_col]),
                size = 3, col = "black",  vjust = -0.8)

        # highlight artifact (different color and bigger in size)
    gg_plot <- gg_temp_2 +
      geom_text(x = xx[artefact_row],
                y = yy[artefact_row],
                label = data[artefact_row, internal_number_col],
                size = 3, col = "blue", vjust = -0.8) +
    geom_point(x = xx[artefact_row],
               y = yy[artefact_row],
              size = 4, col = "blue")
  }

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # verbose ----
  # verbose == TRUE: plot tree
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if (verbose) {
    par(mai = c(mai_bottom, 1, 1.2, 0)) # c(bottom, left, top, right)

    if (!is.na(artefact_row)) {
      main = paste(
        "Artefact: ", data[artefact_row, class_col],
        ", Internal number: ", data[artefact_row, internal_number_col],
        "\n clustering algorithm: ", clust_algo,
        "\n agglomerative method: ", aggl_method,
        "\n distance method: ", dist_method,
        sep = "")
    } else {
      main = paste(
        "clustering algorithm: ", clust_algo,
        "\n agglomerative method: ", aggl_method,
        "\n distance method: ", dist_method,
        sep = "")
    }

    plot(dend, main = main, cex.main = 0.9)

    if (!is.null(data_art) && (!is.null(num_of_classes) || !is.null(cutting_at_height))) {
      border <- rep("gray", num_of_classes)
      border[group_with_artefact] <- "blue"
    } else {
      border <- "grey"
    }

    if (!is.null(num_of_classes)) {
      rect.dendrogram(
        dend,
        k = num_of_classes,
        border = border,
        lty = 5,
        lwd = 1.5,
        lower_rect = -0.05
      )
    }
    if (!is.null(cutting_at_height)) {
      abline(h = cutting_at_height, col = "red", lwd = 2)
    }

    if (!is.null(gg_plot) & plot_2D == TRUE)  print(gg_plot)
  } # if (verbose)

  list(
    cluster_plot = gg_plot,
    dend = dend,
    hc = hc,
    class_col = class_col,
    internal_number_col = internal_number_col,
    artefact_row = artefact_row,
    num_of_classes = num_of_classes,
    cutting_at_height = cutting_at_height,
    group_with_artefact = group_with_artefact,
    is_homogenous = is_homogenous,
    data_with_artefact = data_with_artefact,
    artefact_position_in_tree = artefact_position_in_tree
  )
}
