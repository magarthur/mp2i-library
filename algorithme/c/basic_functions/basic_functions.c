int minimum(int* t, int n){
    int resultat = INT_MAX;
    for(int i = 0; i<=n-1;i++){
        if(t[i]< resultat){
            resultat = t[i];
        }
    }
    return resultat;
}
