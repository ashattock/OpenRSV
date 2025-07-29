// ########################################################
// SUMMARISE
//
// Fast grouped summarisation of model output.
//
// Written by AJ Shattock (andrew.shattock@uwa.edu.au)
// ########################################################

#include <Rcpp.h>
using namespace Rcpp;

// --------------------------------------------------------
// Compute quantiles of a value by all defined groupings
// --------------------------------------------------------
// [[Rcpp::export]]
int summarise_cpp(
    DataFrame     df,
    StringVector  by,
    NumericVector q) {
  
  // ---- Interpret arguments ----
  
  // Convert from Rcpp vector of strings to SLT space
  std::vector<std::string> vars = as<std::vector<std::string>>(by);
  
  // Extract value column to summarise
  NumericVector values = df["value"];
  
  // Extract quantile bounds
  double q_lb = q[0];
  double q_ub = q[1];
  
  // ---- Construct grouping keys ----
  
  // NOTE: Can only group by single column in C++, so constructing new string var
  
  // Number of rows
  int nrows = df.nrows();
  
  // Preallocate vetor of keys
  std::vector<std::string> keys(nrows);
  
  // Iterate through grouping columns
  for (auto col : vars) {
    
    // Extract value of this grouping column
    CharacterVector col_data = df[col];
    
    // Append grouping value and add separator '|'
    for (int i = 0; i < nrows; ++i) {
      keys[i] += as<std::string>(col_data[i]) + "|";
    }
  }
  
  // ---- Group indices ----
  
  // Map from group key to vector of row indices belonging to that group
  std::map<std::string, std::vector<int>> group_map;
  for (int i = 0; i < nrows; ++i) {
    group_map[keys[i]].push_back(i);
  }
  
  
  
  
  
  
  
  
  for (const auto& pair : group_map) {
    Rcout << pair.first << ": ";
    for (int idx : pair.second) {
      Rcout << idx << " ";
    }
    Rcout << std::endl << std::flush;
  }
  
  return 1;
}

