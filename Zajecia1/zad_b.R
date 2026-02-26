ocena_kredytowa = function(dochod, zadluzenie) {
  udzial = zadluzenie/dochod
  
  if (udzial < 0.30) {
    return("KREDYT PRZYZNANY")
  }
  
  if (udzial <= 0.50) {
    return("WYMAGA WERYFIKACJI")
  }
  
  return("KREDYT ODRZUCONY")
}

print(ocena_kredytowa(1000, 200))
print(ocena_kredytowa(1000, 400))
print(ocena_kredytowa(10000, 6000))
