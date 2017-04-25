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
#include <cstdlib>

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

enum class MtxDim
{
  Row,
  Col
};

class Matrix
{
public:
  Matrix(string fname, MtxDim dim);
  bool is_valid() { return m_valid; }
  int rows()      { return m_valid ? m_rows : 0; }
  int cols()      { return m_valid ? m_cols : 0; }
  bool cell(int r, int c, int *val);
  void print();

private:
  vector<vector<int> *> m_mtx;
  int  m_rows;
  int  m_cols;
  bool m_valid;
};

Matrix::Matrix(string fname, MtxDim dim)
{
  vector<int> vcols;
  int cols      = 0;
  int dimi      = 0;
  bool parse_ok = true;

  m_cols  = 0;
  m_rows  = 0;
  m_valid = false;

  ifstream infile(fname);
  if(infile.good())
  {
    int i = 0;
    for(string line; getline(infile, line);)
    {
      if(i == 0) // First value in the file
      {
        try
        {
          dimi = stoi(line, nullptr, 10);
        }
        catch (const std::invalid_argument&)
        {
          parse_ok = false;
        }
        ++i;
        continue;
      }
      if(!parse_ok) break;

      // Parse matrix body
      m_mtx.push_back(new vector<int>());
      string tmps = "";
      int tmpi;
      size_t ptr;
      for(int j=0; j<line.size(); ++j)
      {
        if(isspace(line[j]) && tmps != "")
        {
          tmpi = stoi(tmps, &ptr, 10);
          if(ptr != tmps.size())
          {
            parse_ok = false;
            break;
          }
          m_mtx[m_mtx.size()-1]->push_back(tmpi);
          tmps = "";
        }
        else
        {
          tmps += line[j];
        }

        if(j == line.size()-1 && tmps != "")
        {
          tmpi = stoi(tmps, &ptr, 10);
          if(ptr != tmps.size())
          {
            parse_ok = false;
            break;
          }
          m_mtx[m_mtx.size()-1]->push_back(tmpi);
          tmps = "";
        }
      }
      if(!parse_ok) break;
      vcols.push_back(m_mtx[m_mtx.size()-1]->size());
      cols = m_mtx[m_mtx.size()-1]->size();
      ++i;
    }

    // Assign fields and compare rows/columns with first value in the file
    if(parse_ok)
    {
      m_valid = true;
      for(int c : vcols)
      {
        if(c != cols)
        {
          m_valid = false;
          break;
        }
      }
      m_rows  = vcols.size();
      m_cols  = cols;
      if((dim == MtxDim::Row && m_rows != dimi) ||
      (dim == MtxDim::Col && m_cols != dimi))
      {
        m_valid = false;
      }
    }
  }
}

bool Matrix::cell(int r, int c, int *val)
{
  if(r > 0 && r <= m_rows && c > 0 && c <= m_cols)
  {
    *val = m_mtx[r-1]->at(c-1);
    return true;
  }
  return false;
}

void Matrix::print()
{
  for(int i=0; i<m_mtx.size(); ++i)
  {
    for(int j=0; j<m_mtx[i]->size(); ++j)
    {
      cout << m_mtx[i]->at(j) << " ";
    }
    cout << endl;
  }
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
    Matrix A("mat1", MtxDim::Row);
    Matrix B("mat2", MtxDim::Col);
    cout << "A valid: " << (A.is_valid() ? "true" : "false") << endl;
    cout << "A rows: " << A.rows() << endl;
    cout << "A cols: " << A.cols() << endl;
    A.print();
    cout << "B valid: " << (B.is_valid() ? "true" : "false") << endl;
    cout << "B rows: " << B.rows() << endl;
    cout << "B cols: " << B.cols() << endl;
    B.print();
  }

  MPI::Finalize();
  return 0;
}
