void main(){
  int i11 = rand(1,20);
  int a = 2+i11;
  assert(a==13);
  
  int i10 = rand(1,20);
  int b = i10+3;
  assert(b==13);
  
  int i19 = rand(1,20);
  int c = i19-5;
  assert(c==14);
  
  int i21 = rand(1,30);
  int d = 30-i21;
  assert(d==9);
  
  int i2 = rand(1,20);
  int e = 2*i2;
  assert(e==4);

  int i3 = rand(1,20);
  int f = i3*4;
  assert(f==12);

  int i15 = rand(1,20);
  int g = i15/5;
  assert(g==3);

  int i4 = rand(1,20);
  int h = 20/i4;
  assert(h==5);
}
