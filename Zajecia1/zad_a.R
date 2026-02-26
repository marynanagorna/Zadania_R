przelicz_walute = function(kwota, kurs = 4.32) {
  pln = kwota*kurs
  return(pln)
}

print(przelicz_walute(200))
print(przelicz_walute(30, 4.55))
