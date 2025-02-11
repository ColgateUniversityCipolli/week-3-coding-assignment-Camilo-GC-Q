# Solution

primeFactor = function(n) {
  factors = c()
  i = 2
  
  while(n > 1){
    while (n %% i == 0) {
      factors = c(factors, i)
      n = n/i
    }
    i = i + 1
    if (i * i > n){
      if (n > 1){
        factors = c(factors, n)
      }
      break
    }
  }
  return (factors)
}

product.of.unique.prime = function(n){
  factors = primeFactor(n)
  return (length(factors) == length(unique(factors)))
}

replacement = function(n){
  l = n - 1
  u = n + 1
  
  while(TRUE){
    if (product.of.unique.prime(l)){
      return (l)
    }
    if (product.of.unique.prime(u)){
      return (u)
    }
    l = l - 1
    u = u + 1
  }
}

nums = c(6, 10, 14, 15, 21, 
         22, 26, 33, 34, 35,
         38, 39, 46, 51, 55,
         57, 58, 62, 65, 69,
         75, 77, 82, 85, 86,
         87, 91, 93, 94, 95)

for (num in nums) {
  if (!product.of.unique.prime(num)){
    print(num)
    repl = replacement(num)
    print(paste("Replace ", num, "with", repl))
  }
}
