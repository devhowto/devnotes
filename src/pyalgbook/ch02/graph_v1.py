#
# A graph using adacency sets.
#

a, b, c, d, e, f, g, h = range(8)

#
# N is an array containing sets.
#
N = [
        {b, c, d, e, f},    # a
        {c, e},             # b
        {d},                # c
        {e},                # d
        {f},                # e
        {c, g, h},          # f
        {f, h},             # g
        {f, g}              # h
    ]

print(e in N[a])
#=> True

# Degree.
print(len(N[f]))
#=> 3
