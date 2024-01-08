def is_mult_of(x, y):
    """Checks if `x` is a multiple of `y`.

    ASSUME: `y` is not 0 (zero).

    Parameters
    ----------
    x : int
    y : int

    Returns
    -------
    int
    """

    return x % y == 0

##
# Sum all numbers below num which are multiples of 3 or 5.
#
def sum_mults(num):
    """Sum all ints below `num` which are multiples of 3 or 5.

    Parameters
    ----------
    num : int

    Returns
    -------
    int
    """

    if num < 0:
        return 0

    total = 0;

    for n in range(1, num):
        if is_mult_of(n, 3) or is_mult_of(n, 5):
            total = total + n;

    return total


print(sum_mults(10));
