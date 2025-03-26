import os
import sys
import subprocess as sp

if __name__ == "__main__":
    src_dir = sys.argv[2]
    dst_dir = sys.argv[3]
    sdslc = sys.argv[1]

    print()
    for root, dirs, files in os.walk(src_dir, topdown=False):
        for name in files:
            src = os.path.join(root, name)
            # === generate halide
            dst = os.path.join(dst_dir, name[:-5] + ".cpp")
            args = "--target=halide"

            # === generate cuda fortran
            # dst = os.path.join(dst_dir, name[:-5] + ".cuf")
            # args = "--target=cuda"
            
            cmd = [sdslc, args, src, "-o", dst]
            print("\033[32m" + ' '.join(cmd) + "\033[0m\n")
            ret = sp.run(cmd, capture_output=True, text=True)
            # ret = os.system(" ".join(cmd))
            # print("Process return value is %d" % ret)
            print(ret)
            #input()
