
/* Declarações e uso correto de variáveis e função */


void foo(int a, int b) {
    int z;
    z = a + b;
    return;
}


void main(void) {
    int x;
    int y;
    x = 5;
    y = x + 2;

    /* chamada de função declarada abaixo */
    foo(x, y);

    return;
}
