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

class Processor
{
public:
  Processor(int id, int rows, int cols);
  int cpu_above()      { return m_id < m_cols ? 0 : m_id - m_cols; }
  int cpu_below()      { return m_id + m_cols; }
  int cpu_left()       { return m_id + 1; }
  int cpu_right()      { return m_id % m_cols == 0 ? 0 : m_id - 1; }
  bool has_right_ngb() { return m_id % m_cols != m_cols - 1; }
  bool has_below_ngb() { return m_id < (m_rows * m_cols) - m_cols; }

private:
  int m_id;
  int m_rows;
  int m_cols;
  int m_c;
};

Processor::Processor(int id, int rows, int cols)
{
  m_id   = id;
  m_rows = rows;
  m_cols = cols;
}

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

  Processor proc(p_id, 3, 4);

  if(p_id == 0)
  {
    cout << "Count:     " << p_count << endl;
    cout << "CPU left:  " << proc.cpu_left() << endl;
    cout << "CPU right: " << proc.cpu_right() << endl;
    cout << "CPU below: " << proc.cpu_below() << endl;
    cout << "CPU above: " << proc.cpu_above() << endl;
    cout << "Has right: " << (proc.has_right_ngb() ? "true" : "false") << endl;
    cout << "Has below: " << (proc.has_below_ngb() ? "true" : "false") << endl;
  }

  MPI::Finalize();
  return 0;
}
