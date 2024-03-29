a, b, c, d, e, f, g, h = range(8)

#
# Graph represented with a dict.
#
N = [
        {b:2, c:1, d:3, e:9, f:4},  # a
        {c:4, e:3},                 # b
        {d:8},                      # c
        {e:7},                      # d
        {f:5},                      # e
        {c:2, g:2, h:2},            # f
        {f:1, h:6},                 # g
        {f:9, g:8}                  # h
    ]

##
# Neighborhood membership.
#
print(e in N[a])
#=> True

##
# Degree.
#
print(len(N[f]))
#=> 3

##
# Edge weight for (a, b).
#
print(N[a][b])
#=> 2
