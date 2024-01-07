import cProfile

def sum_from_to(ini, end):
    return sum(range(ini, end + 1))

def main():
    total = sum_from_to(1, int(1e9))
    print(total)

cProfile.run('main()')
