/*
 * Cours "Sémantique et Application à la Vérification de programmes"
 *
 * Ecole normale supérieure, Paris, France / CNRS / INRIA
 */

void main(){
  int i = rand(0, 100);
  if(i%10==2 || i%10==5){
    if(i%10==2){
      assert(i%10!=5);
    }
    else{
      assert(i%10!=5); //@KO
    }
  }
}