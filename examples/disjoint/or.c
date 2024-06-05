/*
 * Cours "Sémantique et Application à la Vérification de programmes"
 *
 * Ecole normale supérieure, Paris, France / CNRS / INRIA
 */

void main(){
  int i = rand(0, 3);
  if(i==1 || i==3){
    assert(i==1 || i==3);
  }
}