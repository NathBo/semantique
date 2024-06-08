/*
 * Cours "Sémantique et Application à la Vérification de programmes"
 *
 * Ecole normale supérieure, Paris, France / CNRS / INRIA
 */

void main(){
  int i = rand(0, 100);
  if(i%4==1 || i%4==0){
    i = i * 2;
    assert(i%8!=4);
  }
}