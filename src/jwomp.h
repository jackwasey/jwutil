#ifndef OMP_H_
#define OMP_H_

#ifdef _OPENMP
#include <omp.h>
#endif

int getOmpMaxThreads();
int getOmpThreads();

#endif /* OMP_H_ */
