library(igraph)
library(Rcpp)

Efficiency <- function(g){
  sp <- shortest.paths(g)
  #E <- mean(1/sp[upper.tri(sp)])#upper.tri(sp)取网络中节点之间最短距离矩阵sp的上三角矩阵，包括了所有节点的组合（除了其本身与本身的组合），再对这些最短距离求一个平均值，得到的值成为网络的传递效率
  cppFunction(
    'double mean_upper_cpp(NumericMatrix sp){
      int n = sp.nrow();
      int m = sp.ncol();
      double sum = 0.0;
      for (int i = 0; i < n; ++i) {
        for (int j = i + 1; j < m; ++j) {
          sum += 1.0 / sp(i, j);
        }
      }
      int count = 0;
      for (int k = 1; k < m; ++k) {
        count += k;
      }
      return sum / count;
    }'
  )
  E <- mean_upper_cpp(sp)
  return(E)
}
