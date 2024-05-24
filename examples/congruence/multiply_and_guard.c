

void main(){
    int i = rand(0,100);
    int j = rand(0,100);
    if (j%4==0){
        i=i*j;
        assert(i%2==0);
    }
}