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

const int REG_X = 1;
const int REG_Y = 2;
const int REG_Z = 3;
const int IDX_X = 4;
const int IDX_Y = 5;
const int SIZE  = sizeof(int);

struct ProcessorData
{
  int C;
  int X;
  int Y;
  int Z;
  int rx; // rank X
  int ry; // rank Y
};

void initProcessorData(ProcessorData &p_data)
{
  p_data.C  = 1;
  p_data.X  = -1;
  p_data.Y  = -1;
  p_data.Z  = -1;
  p_data.rx = -1;
  p_data.ry = -1;
}

vector<int>parseNumbers(string fname)
{
  vector<int> result;
  int current;
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

string vecJoin(vector<int> v)
{
  string s = "";
  for(unsigned int i=0; i<v.size(); ++i)
  {
    s += to_string(v[i]);
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

  // First processor
  if(p_id == 0)
  {
    vector<int> numbers = parseNumbers("numbers");
    cout << vecJoin(numbers) << endl;

    if(numbers.size()+1 != p_count)
    {
      cerr << "Size of numbers does not corresponds to number of preocessors" <<
        endl;
      MPI::COMM_WORLD.Abort(1);
    }

    for(int i=1; i<p_count; ++i)
    {
      cout << "Sending " << i << endl;
      MPI::COMM_WORLD.Send(&numbers[i-1], SIZE, MPI::INT, i, REG_X);
    }
  }
  else
  {
    ProcessorData p_data;
    initProcessorData(p_data);
    int x;
    MPI::COMM_WORLD.Recv(&x, SIZE, MPI::INT, 0, REG_X);
    cout << "Processor " << p_id << ": X: " << x << endl;
  }

  MPI::Finalize();
  return 0;
}
