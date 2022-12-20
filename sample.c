#include <stdio.h>

int main() {
    int x[] = {1, 2, 3, 4, 5};
    int y = 0;
    for (int i = 0; i < 5; ++i) {
        y = y + x[i];
    }
    return 0;
}