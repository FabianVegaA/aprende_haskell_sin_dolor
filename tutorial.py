from typing import TypeVar


T = TypeVar("T")


def miFilter(cond: Callable[[T], bool], values: list[T]):
    return [value for value in values if cond(value)]


if __name__ == "__main__":
    print(miFilter(lambda x: x % 2 != 0, range(10)))
