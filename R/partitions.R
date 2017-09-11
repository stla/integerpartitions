.isPartition <- function(partition){
  .C("isPartitionR", partition=list(partition), result=0L)$result
}
.dualPartition <- function(partition){
  .C("dualPartitionR", partition=list(partition), result=list(0L))$result[[1L]]
}
.partitions <- function(n){
  .C("partitionsR", n=n, result=list(0L))$result[[1L]]
}
.asciiFerrersDiagram <- function(partition){
  .C("asciiFerrersDiagramR", partition=list(partition), result="")$result
}
.countAutomorphisms <- function(partition){
  .C("countAutomorphismsR", partition=list(partition), result=0L)$result
}
.dominates <- function(partition1, partition2){
  .C("dominatesR", partition1=list(partition1), partition2=list(partition2), result=0L)$result
}
.dominatedPartitions <- function(partition){
  .C("dominatedPartitionsR", partition=list(partition), result=list(0L))$result[[1L]]
}

#' Ferrers diagram of a partition
#'
#' Returns the Ferrers diagram of a partition.
#'
#' @param partition a partition of integers
#'
#' @return A character string.
#' @export
#' @useDynLib Partitions
#'
#' @examples
#' cat(ferrersDiagram(c(5,4,2)))
ferrersDiagram <- function(partition){
  .asciiFerrersDiagram(as.integer(partition))
}

#' Dual partition
#'
#' Returns the dual partition of a partition.
#'
#' @param partition a partition of integers
#'
#' @return A partition of integers.
#' @export
#'
#' @examples
#' dualPartition(c(5,4,2))
dualPartition <- function(partition){
  .dualPartition(as.integer(partition))
}

#' Number of automorphisms of a partition
#'
#' Count the number of automorphisms of a partition.
#'
#' @param partition partition of integers
#'
#' @return An integer.
#' @export
#' @importFrom purrr map_int
#'
#' @examples
#' countStandardYoungTableaux(c(5,3,1))
countAutomorphisms <- function(partition){
  partition <- purrr::map_int(partition, as.integer)
  if(.isPartition(partition)==0L){
    warning("You have not entered a valid partition.")
  }
  .countAutomorphisms(partition)
}

#' Partitions of an integer
#'
#' Returns the list of partitions of a given integer.
#'
#' @param n an integer
#'
#' @return A list of partitions.
#' @export
#'
#' @examples
#' partitions(4)
partitions <- function(n){
  .partitions(as.integer(n))
}


#' Dominance order
#'
#' Checks whether a partition dominates another one.
#'
#' @param partition1 a partition
#' @param partition2 a partition
#'
#' @return A logical value, whether \code{partition2} dominates \code{partition1}.
#' @export
#'
#' @examples
#' dominates(c(3,1), c(2,2))
dominates <- function(partition1, partition2){
  partition1 <- purrr::map_int(partition1, as.integer)
  if(.isPartition(partition1)==0L){
    warning("You have not entered a valid partition.")
  }
  partition2 <- purrr::map_int(partition2, as.integer)
  if(.isPartition(partition2)==0L){
    warning("You have not entered a valid partition.")
  }
  as.logical(.dominates(partition1, partition2))
}


#' Dominated partitions
#'
#' Returns the list of partitions dominated by the given partition.
#'
#' @param partition a partition
#'
#' @return A list of partitions.
#' @export
#'
#' @examples
#' dominatedPartitions(c(3,2))
dominatedPartitions <- function(partition){
  partition <- purrr::map_int(partition, as.integer)
  if(.isPartition(partition)==0L){
    warning("You have not entered a valid partition.")
  }
  .dominatedPartitions(partition)
}