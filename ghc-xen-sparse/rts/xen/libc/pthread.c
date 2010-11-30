#include <sys/pthread.h>

int pthread_mutex_init(pthread_mutex_t *mutex __attribute__ ((unused)),
    const pthread_mutexattr_t *attr __attribute__ ((unused)))
{
  return 0;
}

int pthread_mutex_lock(pthread_mutex_t *mutex __attribute__ ((unused)))
{
  return 0;
}

int pthread_mutex_unlock(pthread_mutex_t *mutex __attribute__ ((unused)))
{
  return 0;
}
