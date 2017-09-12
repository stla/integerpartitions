.skewPartitionWeight <- function(skewpartition){
  .C("skewPartitionWeightR", rlist=skewpartition, l=length(skewpartition),
     result=0L)$result
}
.mkSkewPartition <- function(outer, inner){
  .C("mkSkewPartitionR", partition1 = list(outer), partition2=list(inner),
   result = list(0L))$result[[1L]]
}
.fromSkewPartition <- function(skewpartition){
  .C("fromSkewPartitionR", rlist=skewpartition, l=length(skewpartition),
     result=list(0L))$result[[1L]]
}

#' Weight of a skew partition
#'
#' Returns the weight of a skew partition, that is  the weight of the outer partition minus the the weight of the inner partition.
#'
#'
#' @param skewpartition a skew partition
#'
#' @return An integer.
#' @export
#'
#' @examples
#' skewPartitionWeight(list(c(2, 3), c(1,2)))
skewPartitionWeight <- function(skewpartition){
  skewpartition <- lapply(skewpartition, function(row){
    purrr::map_int(row, as.integer)
  })
  .skewPartitionWeight(skewpartition)
}

#' Make a skew partition
#'
#' Make a skew partition from its outer partition and its inner partition.
#'
#' @param outer a partition
#' @param inner a subpartition of the \code{outer} partition
#'
#' @return A skew partition.
#' @export
#'
#' @examples
#' mkSkewPartition(c(5,3), c(2,1))
mkSkewPartition <- function(outer, inner){
  outer <- purrr::map_int(outer, as.integer)
  if(.isPartition(outer)==0L){
    warning("`outer` is not a valid partition.")
  }
  inner <- purrr::map_int(inner, as.integer)
  if(.isPartition(inner)==0L){
    warning("`inner` is not a valid partition.")
  }
  if(.isSubPartitionOf(inner, outer)==0){
    stop("`inner` is not a subpartition of `outer`.")
  }
  .mkSkewPartition(outer, inner)
}

#' Outer and inner partition
#'
#' Returns the outer partition and the inner partition of a skew partition.
#'
#' @param skewpartition a skew partition
#'
#' @return A list of two partitions.
#' @export
#'
#' @examples
#' fromSkewPartition(list(c(2, 3), c(1,2)))
fromSkewPartition <- function(skewpartition){
  skewpartition <- lapply(skewpartition, function(row){
    purrr::map_int(row, as.integer)
  })
  setNames(.fromSkewPartition(skewpartition), c("outer", "inner"))
}
