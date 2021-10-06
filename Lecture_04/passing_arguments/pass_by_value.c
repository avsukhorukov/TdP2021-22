#include <stdio.h>
void sub(int n) {
    printf("sub:  &n = %x\n", &n); /* 2a8114f4 */
    n = 5;
}
void main() {
    int i = 1;
    printf("main: &i = %x\n", &i); /* 2a8114dc */
    sub(i);
    printf("main:  i = %x\n", i); /* i is still 1 */
}