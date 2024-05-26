void main(){
  int i = rand(1,20); // [1;7]
  int a = i+2;
  assert( a < 10 || a > 15);
  int b = a-4;
  assert(b < 10);
}
