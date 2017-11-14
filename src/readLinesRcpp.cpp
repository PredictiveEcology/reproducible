//Includes/namespaces
#include <fstream>
#include <sstream>
#include <string>
#include <Rcpp.h>
using namespace Rcpp;

//' @title
//' Alternative to readLines that is faster
//' @description
//' This alternative is from \url{https://gist.github.com/hadley/6353939}
//'
//' @param path Path to text file to read.
//' @return
//' Similar to \code{readLines}, except with explicit \code{\\n} embedded.
//'
//' @export
//' @rdname readLinesRcpp
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
