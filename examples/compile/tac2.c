

/* função que soma e retorna (exemplo): */
int foo(int a, int b) {
    int s;
    s = a + b;
    return s;
}


/* Teste TAC: if-else, while, chamada de função, return e passagem de parâmetros */
void main(void) {
    int x;
    int y;
    x = 10;
    y = 0;

    if (x < 5) {
        y = y + 1;
    } else {
        y = y + 2;
    }

    while (x > 0) {
        y = y + x;
        x = x - 1;
    }

    /* chamada de função */
    foo(x, y);

    return;
}