//Includes/namespaces
#include <fstream>
#include <sstream>
#include <string>
#include <Rcpp.h>
using namespace Rcpp;

//' @title
//' Alternative to readLines that is faster
//' @description
//' This alternative is from \link{https://gist.github.com/hadley/6353939}
//'
//' @param x Integer Vector
//' @return
//' A logical vector, as per \code{duplicated}
//'
//' @export
//' @rdname duplicated
// [[Rcpp::export]]
CharacterVector readLinesRcpp(std::string path) {
  std::ifstream in(path.c_str());
  std::string contents;
  in.seekg(0, std::ios::end);
  contents.resize(in.tellg());
  in.seekg(0, std::ios::beg);
  in.read(&contents[0], contents.size());
  in.close();
  return(contents);
}
