import os
import sys
import subprocess as sp
import json

def exec_dir(path):
    dic = {}
    for root, dirs, files in os.walk(path, topdown=False):  
        for fname in files:
            kname = fname[:-7] 
            ret = sp.run([os.path.join(root, fname)], capture_output=True, text=True)
            
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
        print("""
              please execute this script from the root dir of `stencil-lifting`
              i.e. cd <path/to/stencil-lifting> and execute `python scripts/auto_translate.py`
              """)
        exit(1)

    src_dir = os.path.join(cwd, 'out', 'bin')
    obj = exec_dir(src_dir)
    
    out_path = os.path.join(cwd, 'out', 'stat', 'sl-perf.json')
    with open(out_path, 'w') as f:
        json.dump(obj, f, indent=2)