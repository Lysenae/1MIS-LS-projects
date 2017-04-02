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

const int MASTER = 0;
const int REG_X  = 1;
const int REG_Y  = 2;
const int REG_Z  = 3;
const int IDX_X  = 4;
const int IDX_Y  = 5;

struct ProcessorData
{
  int p_id;
  int C;
  int X;
  int Y;
  int Z;
  int ix; // rank X
  int iy; // rank Y
};

void initProcessorData(int p_id, ProcessorData &p_data)
{
  p_data.p_id = p_id;
  p_data.C    = 1;
  p_data.X    = -1;
  p_data.Y    = -1;
  p_data.Z    = -1;
  p_data.ix   = -1;
  p_data.iy   = -1;
}

void printProcessorData(ProcessorData &pd)
{
  cout << "Processor " << pd.p_id << ": C: " << pd.C << ", X: " << pd.X
    << "[" << pd.ix << "],\tY: " << pd.Y << "[" << pd.iy << "],\tZ: " << pd.Z <<
    endl;
}

bool hasNextNeighbor(int rank, int proc_count)
{
  return rank < (proc_count-1);
}

int prevNeighbor(int rank)
{
  return rank -1;
}

int nextNeighbor(int rank)
{
  return rank + 1;
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
  int nums;

  MPI::Init(argc, argv);
  p_count = MPI::COMM_WORLD.Get_size();
  p_id    = MPI::COMM_WORLD.Get_rank();
  nums    = p_count - 1;

  // First processor
  if(p_id == 0)
  {
    vector<int> numbers = parseNumbers("numbers");
    cout << vecJoin(numbers) << endl;

    if(numbers.size() != nums)
    {
      cerr << "Size of numbers does not corresponds to number of preocessors" <<
        endl;
      MPI::COMM_WORLD.Abort(1);
    }

    for(int i=1; i<p_count; ++i)
    {
      int data = i;
      MPI::COMM_WORLD.Send(&numbers[i-1], 1, MPI::INT, i, REG_X);
      MPI::COMM_WORLD.Send(&data, 1, MPI::INT, i, IDX_X);
      MPI::COMM_WORLD.Send(&numbers[i-1], 1, MPI::INT, 1, REG_Y);
      MPI::COMM_WORLD.Send(&data, 1, MPI::INT, 1, IDX_Y);
    }
  }
  else // Other processors
  {
    ProcessorData p_data;
    initProcessorData(p_id, p_data);

    // Initialize X register with it's rank
    int x, ix, y;
    MPI::COMM_WORLD.Recv(&x, 1, MPI::INT, MASTER, REG_X);
    MPI::COMM_WORLD.Recv(&ix, 1, MPI::INT, MASTER, IDX_X);
    p_data.X  = x;
    p_data.ix = ix;

    for(unsigned int i=0; i<nums; i++)
    {
      int y, iy;
      MPI::COMM_WORLD.Recv(&y, 1, MPI::INT, prevNeighbor(p_id), REG_Y);
      MPI::COMM_WORLD.Recv(&iy, 1, MPI::INT, prevNeighbor(p_id), IDX_Y);
      p_data.Y  = y;
      p_data.iy = iy;
      cout << "Processor " << p_id << ": Receiving Y (" << y << "[" << iy << "])" << endl;

      if(p_data.X > p_data.Y)
      {
        p_data.C += 1;
      }
      // Increase counter if X rank is greater than Y rank if values are equal
      else if(p_data.X == p_data.Y && p_data.ix > p_data.iy)
      {
        p_data.C += 1;
      }

      if(hasNextNeighbor(p_id, p_count))
      {
        MPI::COMM_WORLD.Send(&y, 1, MPI::INT, nextNeighbor(p_id), REG_Y);
        MPI::COMM_WORLD.Send(&iy, 1, MPI::INT, nextNeighbor(p_id), IDX_Y);
      }
    }
    printProcessorData(p_data);
  }

  MPI::Finalize();
  return 0;
}
