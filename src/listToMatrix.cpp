#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
CharacterMatrix listToMatrix(List data, List names) {
  int rowCount = data.size(); // rows
  int validColumnCount = names.size(); // name count
  int indexColumnCount;
  List indexList = *new List();
  
  CharacterMatrix cMatrix = CharacterMatrix(rowCount, validColumnCount); 
  cMatrix.fill(NA_STRING);
  
  // Get data indices of valid names. Faster to look up by direct access than searching list for name.
  for(int c = 0; c < validColumnCount; c++)
  {
    indexList.push_back(as<List>(data[0]).findName(names[c]));
  }
  
  indexColumnCount = indexList.size();
  
  // Loop through data
  for(int i = 0; i < rowCount; i++)
  {
    // Loop through indices of valid names
    for(int j = 0; j < indexColumnCount; j++)
    {
      // If values are not null add to matrix
      if(!Rf_isNull((as<List>(data[i]))[as<int>(indexList[j])])) 
      {
        // Do not add vector from group concat
        if(TYPEOF((as<List>(data[i]))[as<int>(indexList[j])]) != VECSXP)
        {
          cMatrix(i,j) = (as<CharacterVector>((as<List>(as<List>(data[i]))[as<int>(indexList[j])])))[0];
        }
      }
    }
  }
  
  return cMatrix;
}
