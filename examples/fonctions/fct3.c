int f1(int x)   {
    return x+1;
}
int f2(int x)   {
    return f1(x)+2;
}
int f3(int x, int y)    {
    return f2(x)+y;
}

void main(){
  int i = f3(2,5);
  assert(i==10);
}
