/*
 * Description: PRL #3: Mesh Multiplication
 * Author:      Daniel Klimaj (xklima22@stud.fit.vutbr.cz)
 */

#include <mpi.h>

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <chrono>

using namespace std;

const int MTX_A  = 2;
const int MTX_B  = 3;

const bool GET_TIME = false;

/// Read numbers from specified file and return them as vector
vector<int>parseNumbers(string fname)
{
  vector<int> result;
  return result;
}

int main(int argc, char **argv)
{
  int p_count;
  int p_id;
  int nums;
  int data[2];
  int rslt;

  MPI::Init(argc, argv);
  p_count = MPI::COMM_WORLD.Get_size();
  p_id    = MPI::COMM_WORLD.Get_rank();

  // Master processor
  if(p_id == 0)
  {
  }

  MPI::Finalize();
  return 0;
}
