import os
import sys
import subprocess as sp
import json
import argparse

HALIDE_DISTRIB = None

def build_dir(path, downscale: bool):
    proj_root = os.path.curdir
    os.chdir(path)
    print(f"Building gen code under \033[32m{path}\033[0m ...")
    cmd = ["make"]
    if downscale:
        cmd.append("SCALE=down")
    ret = sp.run(cmd)
    if ret.returncode != 0:
        print("Building failed")
        print(f"Output:\n {ret.stderr}")
    os.chdir(proj_root)


def exec_dir(path):
    dic = {}
    for root, dirs, files in os.walk(path, topdown=False):  
        for fname in files:
            kname = fname[:-7] 

            print(f"Profiling \033[32m{fname}\033[0m ...")
            # while True: 
            ret = sp.run([os.path.join(root, fname)], 
                            env={"LD_LIBRARY_PATH": os.path.join(HALIDE_DISTRIB, "lib")}, 
                            capture_output=True, text=True)
                # if ret.returncode == 0 or len(ret.stderr) > 0:
                #     break

            print("Process return value: %d" % ret.returncode)
            if ret.returncode != 0:
                print("Output:\n %s" % ret.stderr)
            else:
                subdic = {}
                for console_out in ret.stdout.split('\n'):
                    if console_out.find("legacy code") >= 0:
                        subdic['fort'] = console_out.split(' ')[-1][:-2]
                    if console_out.find("lifted cpu") >= 0:
                        subdic['hcpu'] = console_out.split(' ')[-1][:-2]
                    if console_out.find("lifted gpu") >= 0:
                        subdic['hgpu'] = console_out.split(' ')[-1][:-2]
                dic[kname] = subdic
    return dic


if __name__ == "__main__":
    cwd = os.path.abspath(os.path.curdir)
    pdir = cwd.split(os.path.sep)[-1]
    if pdir != "stencil-lifting":
        print("please execute this script from the root dir of `stencil-lifting`\n"
              "i.e. cd <path/to/stencil-lifting> and execute `python scripts/auto_translate.py`")
        exit(1)

    if "HALIDE_DISTRIB_PATH" not in os.environ.keys():
        print("Environment variable `HALIDE_DISTRIB_PATH` is not provided.\n"
              "Please set the variable as HALIDE_DISTRIB_PATH=<path/to/halide/distrib>\n" 
              "Otherwise default value is used\n")
        print("Press any key to continue or CTRL+C to abort...")
        input()
    else:
        HALIDE_DISTRIB = os.environ["HALIDE_DISTRIB_PATH"]

    parser = argparse.ArgumentParser(description="Available arguments")
    parser.add_argument("--bench", type=str, help="benchmark to evaluate (custom, stng, both)", default=None)
    parser.add_argument("--downscale", action='store_true', help="using downscaled evaluate")
    args = parser.parse_args()

    src_dirs = []
    if args.bench is not None:
        for d in sys.argv[1].split(","):
            src_dirs.append(d)
        print(f"Using assigned benchmark: {','.join(src_dirs)}")
    else:
        src_dirs = [os.path.join(cwd, 'examples', 'gen_code'),
                    os.path.join(cwd, 'examples', 'stng-bench', 'gen_code')]
    
    for sd in src_dirs:
        build_dir(sd, args.downscale)

    bin_dir = os.path.join(cwd, 'out', 'bin')
    obj = exec_dir(bin_dir)
    
    out_path = os.path.join(cwd, 'out', 'stat', 'sl-perf.json')
    with open(out_path, 'w') as f:
        json.dump(obj, f, indent=2)