#' ID3
#'
#' A Decision Tree implemented using the ID3 algorithm.
#'
#' @examples
#' clf <- ID3$new("monks")
#' clf$train(monks1_train[, -c(1,8)], monks1_train[,1])
#' clf$predict(monks1_test[, -c(1,8)])
#' print(clf$tree, "feature")
#' plot(clf$tree)
#'
#' @export

ID3 <- R6::R6Class("ID3",

  public = list(

    tree = NULL,
    max_depth = NULL,

    initialize = function(name = "root") {
      self$tree <- data.tree::Node$new(name)
    },

    train = function(features, labels, max_depth = ncol(features)) {
      self$max_depth <- max_depth
      feature_ranges <- list()
      for(i in 1:ncol(features)) {
        feature_ranges[[names(features)[i]]] <- unique(features[,i])
      }
      private$train_help(self$tree, features, labels, feature_ranges)
    },

    predict = function(features) {
      if(is.vector(features)) {
        # make single prediction
        private$predict_help(self$tree, features)
      } else {
        # make prediction for every row in data.frame
        preds <- vector()
        for(row in 1:nrow(features)) {
          preds <- c(preds, private$predict_help(self$tree, features[row,]))
        }
        preds
      }
    }

  ),

  private = list(

    entropy = function(labels) {
      ent_sum <- 0
      num_labels <- length(labels)
      for(val in unique(labels)) {
        num_val <- length(labels[labels == val])
        percent <- num_val/num_labels
        ent_sum <- ent_sum - percent*log(percent, 2)
      }
      ent_sum
    },

    info_gain = function(feature, labels) {
      ent_parent <- private$entropy(labels)
      ent_children <- 0
      for(val in unique(feature)) {
        relative_weight <- length(feature[feature == val])/length(feature)
        ent_children <- ent_children + relative_weight*private$entropy(labels[which(feature == val)])
      }
      ig <- ent_parent - ent_children
      ig
    },

    depth = function(node) {
      curr_depth <- 0
      while(!node$isRoot) {
        node <- node$parent
        curr_depth <- curr_depth + 1
      }
      curr_depth
    },

    train_help = function(node, features, labels, feature_ranges) {
      if(length(unique(labels)) == 1) {
        # class labels are all the same, so classify!
        leaf <- node$AddChild(unique(labels))
        node$feature <- "class"
      } else if(private$depth(node) >= self$max_depth) {
        # if no features left to split on, take mode of feature values
        leaf <- node$AddChild(names(which.max(table(labels))))
        node$feature <- "class"
      } else {
        # get feature with highest info gain on split
        # create children nodes for each value of feature
        # recursively call train_help on each child with proper subset of data
        max_ig <- -Inf
        max_f <- ""
        for(f in 1:ncol(features)) {
          split_on_f_ig <- private$info_gain(features[, f], labels)
          if(split_on_f_ig > max_ig) {
            max_ig <- split_on_f_ig
            max_f <- f
          }
        }
        node$feature <- names(features)[max_f]
        for(c in feature_ranges[[node$feature]]) {
          if(!any(features[, max_f] == c)) {
            child <- node$AddChild(c)
            leaf <- child$AddChild(names(which.max(table(labels))))
            child$feature <- "class"
          } else {
            child <- node$AddChild(c)
            private$train_help(child, features[features[,max_f]==c,-max_f, drop=FALSE], labels[features[,max_f] == c], feature_ranges)
          }
        }
      }
    },

    predict_help = function(node, features) {
      while(node$feature != "class") {
        for(n in node$children){
          if(n$name == features[node$feature]){
            node <- n
            break
          }
        }
      }
      node$children[[1]]$name
    }

  )

)
