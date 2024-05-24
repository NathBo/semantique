int foo(){
    int z = rand(1,10);
    return z;
}
int machin()    {
    int k = rand(-10,10);
    return k + foo();
}

void main(){
    assert(foo()<11);
    assert(machin()<21);
    assert(machin()>=-10);
    assert(machin()+machin()>=-20);
    assert(foo()<10); //@KO
}
