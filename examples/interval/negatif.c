void main(){
  int i = rand(2,7);
  int j = -i;
  assert(j == -5); //@KO
  assert(j == -5);
  assert(j == -4); //@KO
}
