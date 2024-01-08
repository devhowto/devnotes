#
# 0 to 7
#
a, b, c, d, e, f, g, h = range(0, 8)

#
# A graph represented with an adacency list.
#
# In Python, a list is a dynamic array behind the covers.
#
N = [
        [b, c, d, e, f],        # a
        [c, e],                 # b
        [d],                    # c
        [e],                    # d
        [f],                    # e
        [c, g, h],              # f
        [f, h],                 # g
        [g, g]                  # h
    ]

print(e in N[a])
#=> True

# Degree.
print(len(N[f]))
#=> 3
