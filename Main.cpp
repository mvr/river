#include <igl/readCSV.h>
#include <igl/triangle/triangulate.h>
#include <igl/viewer/Viewer.h>
#include <iostream>

#include "igl/lim/lim.h"

using namespace Eigen;
using namespace std;

// Mesh
Eigen::MatrixX2d V;

int main(int argc, char *argv[])
{
  using namespace std;
  using namespace Eigen;

  Eigen::MatrixXd outline;
  igl::readCSV("outline.csv",outline);

  Eigen::MatrixX2i E;
  E.resize(8,2);
  // E << 4,0, 0,1, 1,(outline.rows()-1),
  //     (outline.rows()-1),2, 2,3, 3,4;
  E << 0,1, 1,(outline.rows()-1), (outline.rows()-1),(outline.rows()-2),
      (outline.rows()-2),2, 2,3, 3,4, 4,5, 5,0;
  Eigen::MatrixXd H;

  Eigen::MatrixXd gridV;
  Eigen::MatrixXi gridF;

  // Triangulate
  igl::triangulate(outline,E,H,"a0.00005q",gridV,gridF);
  igl::writeOBJ("grid.off", gridV, gridF);

  // std::cout << gridV << std::endl;

  // Compute map
  Eigen::MatrixXd targets;
  igl::readCSV("targets.csv",targets);

  Eigen::MatrixX3d padded(gridV.rows(), 3);
  padded << gridV, MatrixXd::Zero(gridV.rows(),1);
  Eigen::MatrixX3d starting;
  starting = padded;

  SparseMatrix<double> C;
  Eigen::VectorXd b;

  int fixedCount = outline.rows() - 4;

  C.resize(2*fixedCount,padded.rows()*2);
  for(int i=0;i<fixedCount;i++)
  {
      C.insert(2*i,2*(i+4)) = 1;
      C.insert(2*i+1,2*(i+4)+1) = 1;
  }

  b.resize(2*fixedCount);
  for(int i=0;i<fixedCount;i++)
  {
      b(2*i) = targets.row(i)[0];
      b(2*i+1) = targets.row(i)[1];
  }

  // std::cout << C << std::endl;
  // std::cout << b << std::endl;

  igl::lim(starting,padded,gridF,C,b,1,1e-8,3000,true);
  igl::writeOBJ("transformed.off", starting, gridF);

  // Show mesh
  igl::Viewer viewer;
  viewer.data.set_mesh(starting, gridF);
  viewer.core.show_lines = true;
  viewer.core.lighting_factor = 0.0f;
  viewer.launch();
}
