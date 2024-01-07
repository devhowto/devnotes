class Node:
    def __init__(self, value, next=None):
        self.value = value
        self.next = next


l = Node('a', Node('b', Node('c')))
print(l.value)
#=> a

print(l.next.value)
#=> b

print(l.next.next.value)
#=> c

print(l.next.next.next)
#=> None
