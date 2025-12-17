/* Teste: precedência e associatividade */
void main(void) {
    int a;
    int b;
    int c;
    int d;
    int e;
    int f;
    int rr;
    int rrr;

    /* valores arbitrários; interessa a forma, não o valor */
    a = 1;
    b = 2;
    c = 3;
    d = 4;
    e = 5;
    f = 6;

    /* expressão que testa precedência e parênteses */
    rr = a + b * c - (d / e + f);

    /* expressão que testa associatividade à esquerda de '-' e '/' */
    rrr = a - b - c;   /* deve ser (a - b) - c, não a - (b - c) */

    return;
}
