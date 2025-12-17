/* Teste: distinguir palavras reservadas de identificadores semelhantes */
void main(void) {
  int iff; /* 'iff' deve ser IDENTIFICADOR, não IF */
  int ife; /* 'if1' idem */
  int Int; /* case-sensitive: 'Int' é IDENTIFICADOR, não INT (keyword) */
  int myifvar;
  iff = 001; /* Na tabela de simbolos, iff deve ser 1 (inteiro) */
  ife = iff + 2;
  Int = ife * 3;
  myifvar = Int + iff;
  if (myifvar < 20) {
    output(Int);
  } else {
    output(iff);
  }
}
