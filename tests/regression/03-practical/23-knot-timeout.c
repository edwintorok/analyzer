// SKIP (time-out with cb439e0, see https://github.com/goblint/analyzer/pull/502)
#include<pthread.h>

struct a {
  int b;
};

void c(struct a *g) {
  int x;
  struct a *e;
  while (x)
    e = &g[g->b];
}

void *f(void *arg) {
  struct a d;
  c(&d);
  return NULL;
}

int main() {
  pthread_t t;
  pthread_create(&t, NULL, f, NULL);
  return 0;
}
