import os
import sys
import subprocess as sp
import json

def lift_dir(src_dir, dst_dir, sdslc):
    dic = {}
    for root, dirs, files in os.walk(src_dir, topdown=False):
        for fname in files:
            src = os.path.join(root, fname)
            kname = fname[:-5]
            # === generate halide
            dst = os.path.join(dst_dir, kname + ".hpp")
            args = "--target=halide"
            
            cmd = [sdslc, args, src, "-o", dst]
            print("\033[32m" + ' '.join(cmd) + "\033[0m\n")
            while True:
                ret = sp.run(cmd, capture_output=True, text=True)
                if ret.returncode == 0 or len(ret.stderr) > 0:
                    break
            print("Process return value: %d" % ret.returncode)

            if ret.returncode != 0:
                print("Output:\n %s" % ret.stderr)
            else:
                for console_out in ret.stdout.split('\n'):
                    # if len(console_out) > 0 and console_out[0] == '用':
                    #     lift_time = float(console_out[2:-1])
                    if console_out.find("Total-time:") == 0:
                        lift_time = float(console_out.split()[1])
                        dic[kname] = lift_time
                        break
    return dic
        


if __name__ == "__main__":
    cwd = os.path.abspath(os.path.curdir)
    pdir = cwd.split(os.path.sep)[-1]
    if pdir != "stencil-lifting":
        print("Please execute this script from the root dir of `stencil-lifting`\n"
              "i.e. cd <path/to/stencil-lifting> and execute `python scripts/auto_compile.py`")
        exit(1)

    src_dir = os.path.join(cwd, 'out', 'sdsl')
    dst_dir = os.path.join(cwd, 'out', 'lift')
    stat_dir = os.path.join(cwd, 'out', 'stat')
    sdslc = os.path.join(cwd, 'build', 'release', 'sdslc')

    if not os.path.exists(dst_dir):
        os.mkdir(dst_dir)

    if not os.path.exists(stat_dir):
        os.mkdir(stat_dir)

    dic1 = lift_dir(src_dir, dst_dir, sdslc)
    # src_a_dir = os.path.join(cwd, 'examples', 'supplement')
    # dic2 = lift_dir(src_a_dir, dst_dir, sdslc)

    out_log = os.path.join(cwd, 'out', 'stat', 'sl-lift.json')
    with open(out_log, 'w') as f:
        json.dump(dic1, f, indent=2)
        # json.dump(dic2, f, indent=2)
