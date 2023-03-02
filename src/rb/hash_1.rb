
def printName(jedi)
  p jedi[:name]
end

ahsoka = { id: 1, name: 'Ahsoka Tano' }

printName(ahsoka)
printName(id: 2, name: 'Aayla Secura')
printName :id => 3, :name => 'Yoda'
printName id: 4, name: 'Luke Skywalker'
