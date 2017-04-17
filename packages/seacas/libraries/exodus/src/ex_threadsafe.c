#include "exodusII.h"
#include "exodusII_int.h"

/* Global variable definitions */
pthread_once_t EX_first_init_g = PTHREAD_ONCE_INIT;
pthread_key_t  EX_errstk_key_g;
pthread_key_t  EX_cancel_key_g;

/* cancelability structure */
typedef struct EX_cancel_struct
{
  int          previous_state;
  unsigned int cancel_count;
} EX_cancel_t;

/* statically initialize block for pthread_once call used in initializing */
/* the first global mutex                                                 */
EX_api_t EX_g;

static void EX_key_destructor(void *key_val)
{
  if (key_val != NULL)
    free(key_val);
}

/*--------------------------------------------------------------------------
 * NAME
 *    EX_pthread_first_thread_init
 *
 * USAGE
 *    EX_pthread_first_thread_init()
 *
 * RETURNS
 *
 * DESCRIPTION
 *   Initialization of global API lock, keys for per-thread error stacks and
 *   cancallability information. Called by the first thread that enters the
 *   library.
 *
 * PROGRAMMER: Chee Wai LEE
 *             May 2, 2000
 *
 *--------------------------------------------------------------------------
 */
void EX_pthread_first_thread_init(void)
{
  EX_g.EX_libinit_g = EX_FALSE;

  /* initialize global API mutex lock */
  pthread_mutex_init(&EX_g.init_lock.atomic_lock, NULL);
  pthread_cond_init(&EX_g.init_lock.cond_var, NULL);
  EX_g.init_lock.lock_count = 0;

  /* initialize key for thread-specific error stacks */
  pthread_key_create(&EX_errstk_key_g, EX_key_destructor);

  /* initialize key for thread cancellability mechanism */
  pthread_key_create(&EX_cancel_key_g, EX_key_destructor);
}

/*--------------------------------------------------------------------------
 * NAME
 *    EX_mutex_lock
 *
 * USAGE
 *    EX_mutex_lock(&mutex_var)
 *
 * RETURNS
 *    0 on success and non-zero on error.
 *
 * DESCRIPTION
 *    Recursive lock semantics for HDF5 (locking) -
 *    Multiple acquisition of a lock by a thread is permitted with a
 *    corresponding unlock operation required.
 *
 * PROGRAMMER: Chee Wai LEE
 *             May 2, 2000
 *
 *--------------------------------------------------------------------------
 */
int EX_mutex_lock(EX_mutex_t *mutex)
{
  int ret_value = pthread_mutex_lock(&mutex->atomic_lock);

  if (ret_value)
    return ret_value;

  if (mutex->lock_count && pthread_equal(pthread_self(), mutex->owner_thread)) {
    /* already owned by self - increment count */
    mutex->lock_count++;
  }
  else {
    /* if owned by other thread, wait for condition signal */
    while (mutex->lock_count)
      pthread_cond_wait(&mutex->cond_var, &mutex->atomic_lock);

    /* After we've received the signal, take ownership of the mutex */
    mutex->owner_thread = pthread_self();
    mutex->lock_count   = 1;
  }

  return pthread_mutex_unlock(&mutex->atomic_lock);
}

/*--------------------------------------------------------------------------
 * NAME
 *    EX_mutex_unlock
 *
 * USAGE
 *    EX_mutex_unlock(&mutex_var)
 *
 * RETURNS
 *    0 on success and non-zero on error.
 *
 * DESCRIPTION
 *    Recursive lock semantics for HDF5 (unlocking) -
 *    Multiple acquisition of a lock by a thread is permitted with a
 *    corresponding unlock operation required.
 *
 * PROGRAMMER: Chee Wai LEE
 *             May 2, 2000
 *
 *--------------------------------------------------------------------------
 */
int EX_mutex_unlock(EX_mutex_t *mutex)
{
  int ret_value = pthread_mutex_lock(&mutex->atomic_lock);

  if (ret_value)
    return ret_value;

  mutex->lock_count--;

  ret_value = pthread_mutex_unlock(&mutex->atomic_lock);

  if (mutex->lock_count == 0) {
    int err;

    err = pthread_cond_signal(&mutex->cond_var);
    if (err != 0)
      ret_value = err;
  } /* end if */

  return ret_value;
} /* EX_mutex_unlock */

/*--------------------------------------------------------------------------
 * NAME
 *    EX_cancel_count_inc
 *
 * USAGE
 *    EX_cancel_count_inc()
 *
 * RETURNS
 *    0 on success non-zero error code on error.
 *
 * DESCRIPTION
 *    Creates a cancelation counter for a thread if it is the first time
 *    the thread is entering the library.
 *
 *    if counter value is zero, then set cancelability type of the thread
 *    to PTHREAD_CANCEL_DISABLE as thread is entering the library and store
 *    the previous cancelability type into cancelation counter.
 *    Increase the counter value by 1.
 *
 * PROGRAMMER: Chee Wai LEE
 *            May 2, 2000
 *
 *--------------------------------------------------------------------------
 */
int EX_cancel_count_inc(void)
{
  EX_cancel_t *cancel_counter;
  int          ret_value = EX_NOERR;

  cancel_counter = (EX_cancel_t *)pthread_getspecific(EX_cancel_key_g);

  if (!cancel_counter) {
    /*
*First time thread calls library - create new counter and associate
     * with key
     */
    cancel_counter = (EX_cancel_t *)calloc(1, sizeof(EX_cancel_t));

    if (!cancel_counter) {
      exerrval = EX_MEMFAIL;
      ex_err("ex_put_concat_sets", "ERROR: failed to allocate space for counter array.", exerrval);
      return (EX_FATAL);
    }

    ret_value = pthread_setspecific(EX_cancel_key_g, (void *)cancel_counter);
  }

  if (cancel_counter->cancel_count == 0)
    /* thread entering library */
    ret_value = pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &cancel_counter->previous_state);

  ++cancel_counter->cancel_count;

  return ret_value;
}

/*--------------------------------------------------------------------------
 * NAME
 *    EX_cancel_count_dec
 *
 * USAGE
 *    EX_cancel_count_dec()
 *
 * RETURNS
 *    0 on success and a non-zero error code on error.
 *
 * DESCRIPTION
 *    If counter value is one, then set cancelability type of the thread
 *    to the previous cancelability type stored in the cancelation counter.
 *    (the thread is leaving the library).
 *
 *    Decrement the counter value by 1.
 *
 * PROGRAMMER: Chee Wai LEE
 *             May 2, 2000
 *
 *--------------------------------------------------------------------------
 */
int EX_cancel_count_dec(void)
{
  register EX_cancel_t *cancel_counter;
  int                   ret_value = EX_NOERR;

  cancel_counter = (EX_cancel_t *)pthread_getspecific(EX_cancel_key_g);

  if (cancel_counter->cancel_count == 1)
    ret_value = pthread_setcancelstate(cancel_counter->previous_state, NULL);

  --cancel_counter->cancel_count;

  return ret_value;
}
