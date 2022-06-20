# haskell-custom-language

This is a university project made @Ghent-University.

For this project a very basic custom language was written using Haskell, and two simple games were made in this language. 

The following is a basic example of code written in this custom language:
```js
function fac(n){
    x = 1;
    if((n>=1)){
        x = (call fac((n-1)) * n); 
    }
    return x;
}

print(call fac(5));
```

Which will give the output `5`.

## Games
The games that were written are snake and a simple block shooting game. 

![Screenshot 2022-06-20 120930](https://user-images.githubusercontent.com/107751576/174579717-98e817d9-f197-4460-a0f7-60d18d7256c4.jpg)

![image](https://user-images.githubusercontent.com/107751576/174579827-f023c2c8-8f56-4ab5-8561-7f460f283039.png)

