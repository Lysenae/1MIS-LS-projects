/*
 * Description: PRL #2: Enumeration Sort
 * Author:      Daniel Klimaj (xklima22@stud.fit.vutbr.cz)
 */

#include <mpi.h>
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

string ucVecJoin(vector<unsigned char> v)
{
  string s = "";
  for(unsigned i=0; i<v.size(); ++i)
  {
    s += to_string((int)v[i]);
    if(i < v.size()-1) s += " ";
  }
  return s;
}

int main(int argc, char **argv)
{
  int p_count;
  int p_id;

  MPI::Init(argc, argv);
  p_count = MPI::COMM_WORLD.Get_size();
  p_id    = MPI::COMM_WORLD.Get_rank();

  cout << "Processor: " << p_id << endl;

  // First processor
  if(p_id == 0)
  {
    vector<unsigned char> numbers = parseNumbers("numbers");
    cout << ucVecJoin(numbers) << endl;
  }
  else
  {
  }

  MPI::Finalize();
  return 0;
}
