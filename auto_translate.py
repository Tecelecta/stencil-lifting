from fparser.two.parser import ParserFactory
from fparser.common.readfortran import FortranStringReader
from translate import TranslateContext
from time import time
import os
import sys

if __name__ == "__main__":
    parser = ParserFactory().create()
    for root, dirs, files in os.walk(sys.argv[1], topdown=False):
        for name in files:
            if name.find('.f') < 0:
                continue
            fortran_path = root + '/' + name
            print("开始转换文件：", fortran_path)
            t0 = time()
            with open(fortran_path, 'r', encoding="UTF-8") as fortran_file:
                s = fortran_file.read().upper()
            ast = parser(FortranStringReader(s))
            sdsl_path = "./tmp/%s.sdsl" % '.'.join(name.split('.')[:-1])
            with open(sdsl_path, 'w', encoding="UTF-8") as sdsl_file:
                sdsl_file.write("// " + fortran_path + "\n\n")
                context = TranslateContext()
                lines = context.translate_Program(ast)
                sdsl_file.writelines(lines)
            print("转换完成，用时%f秒" % (time() - t0))
