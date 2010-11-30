#ifndef PTHREAD_H
#define PTHREAD_H

typedef int pthread_mutex_t;
typedef int pthread_mutexattr_t;

int pthread_mutex_init(pthread_mutex_t *mutex,
    const pthread_mutexattr_t *attr);
int pthread_mutex_lock(pthread_mutex_t *mutex);
int pthread_mutex_unlock(pthread_mutex_t *mutex);

#endif
