/*
 * Cours "Sémantique et Application à la Vérification de programmes"
 *
 * Ecole normale supérieure, Paris, France / CNRS / INRIA
 */

void main(){
  int i = rand(0, 100);
  if(i<=25 || i>=75){
    if(i<=50){
      assert(i<=30);
    }
    assert(i<=30); //@KO
  }
}