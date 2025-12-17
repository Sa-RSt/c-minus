/* Teste TAC: gera temporários e respeita ordem das operações */
void main(void) {
    int a;
    int b;
    int c;
    int d;
    int e;
    int f;
    int r;

    a = 1;
    b = 2;
    c = 3;
    d = 4;
    e = 5;
    f = 6;

    /* expressão complexa que exige temporários e ordem correta */
    r = (a + b * c) - (d / e + f);

    return;
}
