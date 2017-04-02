/*
 * Description: PRL #2: Enumeration Sort
 * Author:      Daniel Klimaj (xklima22@stud.fit.vutbr.cz)
 */

//#include <mpi.h>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>

using namespace std;

vector<unsigned char>parseNumbers(string fname)
{
  vector<unsigned char> result;
  unsigned char current;
  fstream f;
  f.open(fname.c_str(), ios::in);
  if(!f.good())
  {
    cerr << "Failed to open file with numbers" << endl;
    return result;
  }
  else
  {
    while(!f.eof())
    {
      current = f.get();
      if(f.eof())
        break;
      result.push_back(current);
    }
  }
}

int main(int argc, char **argv)
{
  vector<unsigned char> numbers = parseNumbers("numbers");
  for(int i=0; i<numbers.size(); ++i)
    cout << (int)numbers[i] << endl;
  return 0;
}
