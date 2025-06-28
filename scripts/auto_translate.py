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
            if name.find('.f') < 0:
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
        print("""
              please execute this script from the root dir of `stencil-lifting`
              i.e. cd <path/to/stencil-lifting> and execute `python scripts/auto_translate.py`
              """)
        exit(1)
        
    src_dir = os.path.join(cwd, 'examples', 'benchmarks')
    dst_dir = os.path.join(cwd, 'out', 'sdsl')

    if not os.path.exists(dst_dir):
        os.mkdir(dst_dir)

    translate_dir(src_dir, dst_dir)


