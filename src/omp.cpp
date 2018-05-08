// [[Rcpp::interfaces(r, cpp)]]

#ifdef _OPENMP
#include <omp.h>
#endif


// [[Rcpp::export]]
int getOmpMaxThreads() {
  int maxthreads = 0;
#ifdef _OPENMP
  maxthreads = omp_get_max_threads();
#endif
  return maxthreads;
}

// [[Rcpp::export]]
int getOmpThreads() {
  int threads = 0;
#ifdef _OPENMP
  omp_sched_t sched;
  omp_get_schedule(&sched, &threads);
#endif
  return threads;
}
