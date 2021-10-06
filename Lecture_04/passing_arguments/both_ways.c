/* C passes by value */
#include <stdio.h>
void sub(float *x, float y) {
/* for x, its pointer is copied,
   for y, its value is copied */
   *x = *x + 1.0;
    y =  y + 1.0;
}
void main() {
    float a = 0.0;
    float b = 0.0;
    sub(&a, b);
    printf("%.1f %.1f\n", a, b);
    /* 1.0 0.0 */
}
