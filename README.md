This is just a small repo for experimenting with variations on the idea demonstrated here: https://gist.github.com/nikodemus/b461ab9146a3397dd93e

The big picture here is that there are some situations that may require non-local control patterns that are most easily expressed using the condition system but only rely on a small subset of the condition system's capabilities, so they might get better performance by creating a simple watered-down condition system on top of catch/throw instead.
