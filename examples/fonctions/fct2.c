void foo(){
    assert(true);
}

void main(){
    int c = 12345;
    foo();
    foo();
    foo();
    assert(false); //@KO
}
