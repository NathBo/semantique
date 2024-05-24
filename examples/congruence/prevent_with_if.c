

void main(){
    int i = rand(0,100);
    if (i%4==1){
        i=i+1;
        assert(i%2==0);
    }
}