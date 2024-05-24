
/*
 * Cours "Sémantique et Application à la Vérification de programmes"
 *
 * Ecole normale supérieure, Paris, France / CNRS / INRIA
 */

void main(){
  int i = rand(-10,-1);
  int j = rand(-100, 0);
  assert(i+j < 0);
  assert(i*j>=0);
  int x = j+4;
  assert(x<=0); //@KO
}
