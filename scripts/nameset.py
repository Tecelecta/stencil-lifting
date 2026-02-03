import os
import os.path as osp
import sys

_root_dir = osp.join(osp.curdir, "examples")

def benchmarks():
    flist = os.listdir(osp.join(_root_dir, "benchmarks"))
    return [f.split('.')[0] for f in flist]


def ablation():
    flist = os.listdir(osp.join(_root_dir, "ablation"))
    return [f.split('.')[0] for f in flist]


def stng():
    flist = os.listdir(osp.join(_root_dir, "stng-bench", "fort"))
    return [f.split('.')[0] for f in flist]


if __name__ == '__main__':
    print(benchmarks())
    print(ablation())
    print(stng())