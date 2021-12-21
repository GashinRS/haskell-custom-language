function fac(n){
    x = 1;
    if((n>=1)){
        x = (call fac((n-1)) * n); 
    }
    return x;
}

print(call fac(5));