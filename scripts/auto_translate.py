from fparser.two.parser import ParserFactory
from fparser.common.readfortran import FortranStringReader
from translate import TranslateContext
from time import time
import os
import sys

def translate_dir(src_dir, dst_dir):
    parser = ParserFactory().create()
    for root, _, files in os.walk(src_dir, topdown=False):
        for name in files:
            if name[-4:] != '.f90' and name[-2:] != '.f':
                continue
            fortran_path = root + '/' + name
            print("Translating: ", fortran_path)
            t0 = time()
            with open(fortran_path, 'r', encoding="UTF-8") as fortran_file:
                s = fortran_file.read().upper()
            ast = parser(FortranStringReader(s))
            sdsl_path = os.path.join(dst_dir, "%s.sdsl" % '.'.join(name.split('.')[:-1]))
            with open(sdsl_path, 'w', encoding="UTF-8") as sdsl_file:
                sdsl_file.write("// " + fortran_path + "\n\n")
                context = TranslateContext()
                lines = context.translate_Program(ast)
                sdsl_file.writelines(lines)
            print("Translation complete, %fs elapses" % (time() - t0))


if __name__ == "__main__":
    cwd = os.path.abspath(os.path.curdir)
    pdir = cwd.split(os.path.sep)[-1]
    if pdir != "stencil-lifting":
        print("please execute this script from the root dir of `stencil-lifting`\n"
              "i.e. cd <path/to/stencil-lifting> and execute `python scripts/auto_translate.py`")
        exit(1)
        
    dir_names = []
    if len(sys.argv) == 2:
        args = sys.argv[1].split(",")
        for d in args:
            dir_names.append(d)
        print(f"Using assigned benchmark: {','.join(dir_names)}")
    else:
        dir_names = ["benchmarks", "ablation", os.path.join("stng-bench", "fort")]

    out_tree = ['out', 'sdsl']
    dst_dir = cwd

    for step in out_tree:
        dst_dir = os.path.join(dst_dir, step)
        if not os.path.exists(dst_dir):
            os.mkdir(dst_dir)

    for dir_name in dir_names:
        print(f"Translating Fortran source code in \033[32m{dir_name}\033[0m ...")
        src_dir = os.path.join(cwd, 'examples', dir_name)

        translate_dir(src_dir, dst_dir)


