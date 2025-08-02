#!python.exe 
import os
import os.path as osp
import json
import nameset

import matplotlib
from matplotlib import pyplot as plt
from matplotlib.pyplot import Axes
import numpy as np

from typing import *

"""
Style table
"""
COLOR=["#1F77B4", "#FE7F0E", "#2BA02D", "#c85862", "#5898D5", "#83DED8", "#FFD966", "#FF9B9B"]
MARKERS=["o", "v", "^", "x", "s"]

title_style = {
    "fontfamily" : "Arial",
    "fontweight" : "bold",
    "size" : 13,
}

xlabel_style = {
    "fontfamily" : "Arial",
    "fontweight" : "bold",
    "size"       : 12,
}

ylabel_style = {
    "fontfamily" : "Arial",
    "fontweight" : "bold",
    "size"       : 12
}

xtick_style = {
    "fontfamily" : "Arial",
    "fontweight" : "bold",
    "size"       : 11,
    # "rotation"   : 90
}

ytick_style = {
    "fontfamily" : "Arial",
    "fontweight" : "bold",
    "size"       : 11
}

legend_style = {
    # 'loc' : 'upper right',
    'frameon' : False, 
    "prop": {
        'family':'Arial', 
        # 'weight':'bold', 
        'size'  : 10,
    }
}

annot_style = {
    "ha":"center",
    "va": "top", 
    "fontfamily": "Arial", 
    "fontweight": "bold",
    "fontsize": 11,
    "color": 'w',
    'zorder': 200
}

invalid_annot_style = {
    "ha":"center",
    "va": "bottom", 
    "fontfamily": "Arial", 
    "fontweight": 'bold',
    "fontsize": 11,
    "color": 'r',
    # 'rotation': 90,
    'zorder': 200
}

line_style = {
    ''
}

barh_style = bar_style = {
    "edgecolor" : 'k'
}

plot_style = {}

matplotlib.rcParams['pdf.fonttype'] = 42
matplotlib.rcParams['ps.fonttype'] = 42


def extract_label_and_array(dic: Dict, sort_with: Callable = None):
    """
    dict type input data parser
    """
    labels = []
    arrays = []
    sorter = []
    
    for v in dic.items():
        # if int(v[0][:-2]) < 10:
        sorter.append(v)
    
    if sort_with is not None:
        sorter.sort(key=sort_with)

    for p in sorter:
        labels.append(p[0]) 
        arrays.append(p[1])
        
    return labels, arrays


def varmean(ll: List[List]):
    def geo_mean_overflow(iterable):
        a = np.log(iterable)
        return np.exp(a.sum()/len(a))

    ret = []
    for l in ll:
        ret.append(geo_mean_overflow(l))
    return ret

def max_from_selected(arrays: List[np.ndarray], sel: np.ndarray = None):
    if sel is None:
        return max(flattern(arrays))

    ret = 0
    for ar in arrays:
        ret = max(ret, ar[sel].max())
    return ret

def build_xticks_large(num_x):
    x = 64
    ret = []
    for i in range(num_x):
        ret.append(f"{x}")
        x *= 2

    return ret

def build_xticks_small(num_x):
    ret = []
    for i in range(num_x):
        x = (i+1)*8
        ret.append(f"{x}")

    return ret

def build_yticks(xmin, xmax, preferred_ticks = 5):
    def _scale_up(domain, prefer):
        i=1
        while domain < prefer:
            domain *= 10
            i *= 10
        
        while domain > 10*prefer:
            domain = round(domain / 10)
            i /= 10
        return i

    domain = xmax - xmin
    _a = _scale_up(domain, preferred_ticks)
    domain *= _a
    xmin *= _a
    xmax *= _a
    if isinstance(domain, int):
        domain = domain + 0.0
    scale = round(domain, 0) # int(float(f"{domain:.1}"))
    while (domain + scale - 1e-6) // scale < preferred_ticks:
        cur_parts = (domain + scale - 1e-6) // scale
        div = False
        for i in range(preferred_ticks, 1, -1):
            if cur_parts * i <= preferred_ticks:
                scale //= i
                div = True
                break
        if not div:
            scale //= 2
        # if cur_parts * 6 == preferred_ticks:
        #     scale //= 3
        # elif cur_parts * 5 <= preferred_ticks:
        #     scale //= 5
        # else:
        #     scale //=2

    if _a == 1:
        return np.arange(xmin, (xmax+scale-1e-6)//scale*scale+1e-6, scale, dtype=np.int64)
    else:
        return np.arange(xmin, (xmax+scale-1e-6)//scale*scale+1e-6, scale) / _a


def pretty_num_label():
    pass

def custom_barwidth(group_size, x_tick, base_w):
    bar_norm = base_w / x_tick
    sp_norm = 1 - bar_norm
    bar_w = (bar_norm / group_size) * x_tick
    sp_w = (sp_norm / (group_size-1)) * x_tick if group_size > 1 else 0
    
    if group_size == 1:
        return bar_w, [0]

    b_s = []
    indices = np.arange(group_size) - (group_size-1) / 2
    for i in indices:
        b_s.append(i * (sp_w+bar_w))

    return bar_w, b_s


def flattern(lst):
    if isinstance(lst, Iterable):
        ret = []
        for subl in lst:
            ret += flattern(subl)
        return ret
    else:
        return [lst]

def append_anno_list(store: List, x, y, text: str, xlim = 1., ylim = None):
    to_rm = []
    for item in store:
        _x, _y, _ = item
        replace = False
        if abs(_x - x) < xlim:
            to_rm.append(item)
            if _x < x:
                _x -= 0.5*xlim
                x += 0.5*xlim
            else:
                _x += 0.5*xlim
                x -= 0.5*xlim
            replace = True

        if ylim is not None and abs(_y-y) < ylim:
            if not replace:
                to_rm.append(item)
            if _y < y:
                _y -= 0.5*ylim
                y += 0.5*ylim
            else:
                _y += 0.5*ylim
                y -= 0.5*ylim
            replace = True

        if replace:
            store.append((_x, _y, _))

    for rm in to_rm:
        store.remove(rm) 
    store.append((x, y, text))


def statistic_lifting_time(dic: Dict):
    
    def _guarded_append(listdict, key, item):
        if item is None:
            return 
        if not key in listdict.keys():
            listdict[key] = []
        listdict[key].append(item)
    
    def _guarded_lookup(listdict, key):
        if not key in listdict.keys():
            listdict[key] = {}


    ret = {}

    for stat in dic.values():
        if stat['time'] is None:
            continue
        dim = stat['dim']
        _guarded_lookup(ret, dim)
        _guarded_append(ret[dim], f"{stat['pt']*stat['nout']}", stat['time'])
    
    return ret
    

def render_markdown(header, dic, outfile):
    lines = ['|' + "|".join(header) + "|", 
             '|' + '--|'*len(header)]
    print(lines[0])
    print(lines[1])
    for k, row in dic.items():
        line = '|' + k + '|'
        for c in range(1,len(header)):
            kk = header[c]
            line += str(row[kk]) + '|'
        print(line)
        lines.append(line)

    with open(outfile, 'w') as f:
        f.writelines(lines)


def plot_rows(datalists, x_starts, figure, c):
    num_sub = len(datalists[0])
    axes = figure.subplots(len(datalists), num_sub)
    rotate = 0
    ha = "center"

    for datalist, x_start in zip(datalists, x_starts):
        base = datalists.index(datalist)
        if x_start < 64:
            build_xticks = build_xticks_small
        else:
            # rotate = 45
            # ha = "right"
            build_xticks = build_xticks_large

        for idx in range(num_sub):
            if len(datalists) == 1:
                ax = axes[idx]
            else:
                ax = axes[base][idx]
            dat = datalist[idx]
            num_bars = len(dat)
            ymax = max(dat)

            ax.bar(np.arange(num_bars), dat, 0.4, color=c, zorder=10)
            # for x, val in enumerate(dat):
            #     ax.text(x, val, f"{val:.2f}", ha="center", va="bottom", size=9, fontfamily="Arial", fontsize=10)
            ytk = build_yticks(0, ymax)
            ax.set_xticks(np.arange(num_bars), build_xticks(num_bars), rotation=rotate, ha=ha, fontfamily="Arial", fontweight="bold", fontsize=12)
            ax.set_yticks(ytk, fontfamily="Arial", fontweight="bold", fontsize=12)
            ax.set_ylim(0, ytk[-1])
            ax.grid(axis='y', color='0.8', zorder=0)
            ax.spines[:].set_zorder(20)
            # ax.set_ylim(0,ymax*1.1)
        if len(datalists) == 1: 
            axes[0].set_ylabel("Speedup", fontfamily="Arial", fontweight="bold", fontsize=12)
        else:
            axes[base][0].set_ylabel("Speedup", fontfamily="Arial", fontweight="bold", fontsize=12)


    # figure.supylabel("Speedup", fontfamily="Arial", fontweight="bold", fontsize=12, x=0.03)
    figure.supxlabel("Matrix Size (m = n)", fontfamily="Arial", fontweight="bold", fontsize=12, y=0.05)


def plot_one(ax: Axes, dats: List[np.ndarray], 
             colors: List[AnyStr] = COLOR,
             plot_name: str = 'bar', 
             markers: List[AnyStr] = MARKERS,
             invalid_val: Any = None,
             invalid_text: str = None, 
             bar_config: Tuple[float, float] = (0.6, 0.6), 
             upper_bound: float = None, 
             plot_value: bool = False, 
             select: np.ndarray = None,
             anno_fn: Callable = None):
    assert len(dats) <= len(colors)
    assert markers is None or len(dats) <= len(markers)

    if select is None:
        select = np.arange(len(dats[0]))

    bar_w, x_off = custom_barwidth(len(dats), *bar_config) 
    bars = []
    annos = []

    for idx, dat in enumerate(dats):
        dat = dat[select]
        xloc = np.arange(dat.size) + (x_off[idx] if plot_name in ['bar', 'barh'] else 0.)
        if invalid_val is not None:
            _dat = []
            _xloc = []
            for i, val in enumerate(dat):
                if val != invalid_val:
                    _dat.append(val)
                    _xloc.append(xloc[i])
                else:
                    annos.append((xloc[i], val, invalid_text))
                    if plot_name in ['bar', 'barh']:
                        _dat.append(0)
                        _xloc.append(xloc[i])
            dat = _dat
            xloc = _xloc

        if plot_name in ['bar', 'barh']:
            bars.append(eval(f'ax.{plot_name}(xloc, dat, bar_w, color=colors[idx], zorder=10, **{plot_name}_style)'))
        else:
            bars.append(eval(f'ax.{plot_name}(xloc, dat, color=colors[idx], marker=markers[idx], zorder=10, **{plot_name}_style)'))
            

        if plot_value:
            for x, val in zip(xloc, dat):
                # ax.text(x, val, f"{val:.2f}", **annot_style)
                # append_anno_list(annos, x, val, val if anno_fn is None else anno_fn(val), 
                #                  xlim=select.size*bar_w*1.5)
                if plot_name in ['bar', 'plot']:
                    annos.append((x, val, val if anno_fn is None else anno_fn(val)))
                elif plot_name == 'barh':
                    annos.append((val, x, val if anno_fn is None else anno_fn(val)))
        
        if upper_bound is not None:
            for x, val in zip(xloc, dat):
                if val > upper_bound:
                    # ax.text(x, upper_bound, f"{val:.2f}", **annot_style)
                    append_anno_list(annos, x, upper_bound, val if anno_fn is None else anno_fn(val), 
                                     xlim=select.size*bar_w*1.5)
                    
    for anno in annos:
        if invalid_text is not None and anno[2] == invalid_text:
            ax.text(*anno, **invalid_annot_style)
        elif anno[2] != 'FAILED':
            ax.text(*anno, **annot_style)

    return bars


def plot_one_violin(ax, dats, xlabs, colors):

    vio_width = 0.6
    ymax = ymin = 0.
    xticks = [int(x) for x in xlabs]

    legends = []
    handles = []
    for i, p in enumerate(dats.items()):
        xtick, dat = p
        # nx = max(nx, len(dat))
        means = varmean(dat)
        line, = ax.plot(xticks, means, 'o--', c=colors[i], zorder=10)
        handles.append(line)
        parts = ax.violinplot(dat, xticks, showmeans=False, showmedians=False, widths=vio_width)
        
        for pc in parts['bodies']:
            pc.set_facecolor(colors[i])
            pc.set_alpha(0.)

        for key, pc in parts.items():
            if key =='bodies':
                continue
            pc.set_color(colors[i])
            pc.set_clim((0.1,0.1))
        
        legends.append(xtick)
        ymin = min([ymin,] + flattern(dat))
        ymax = max([ymax,] + flattern(dat))

        # tick = ((int(float(f"{ymax-ymin:.1}")) + 4) // 5 + 4) // 5 * 5
        # ymax = (ymax+tick-1e-6) // tick * tick
    return xticks, build_yticks(ymin, ymax, 4), legends, handles


def plot_one_range(ax:Axes, dats, xticks, colors, yscal, ytick_round=0, select=None):
    """
    
    : dats: Dict[str: Tuple[List, List]] ublb
    """
    assert len(dats) <= len(colors)

    bar_w, x_off = custom_barwidth(len(dats), 0.7, 0.6)

    bars = []
    legends = []

    for idx, pair in enumerate(dats.items()):
        leg, rng = pair
        if select is None:
            select = np.arange(len(rng[0]))
        ub = np.array(rng[0])[select]
        lb = np.array(rng[1])[select]
        legends.append(leg)
        xloc = np.arange(len(lb)) + x_off[idx]
        bars.append(ax.bar(xloc, ub, bar_w, lb, edgecolor='k', color=colors[idx], zorder=10))

    return bars, legends


def axestyle(ax: Axes, xticks: Iterable, yticks: Iterable, 
             xlabel: AnyStr = None, ylabel :AnyStr = None, 
             xtick_label: Union[Iterable[AnyStr], Callable] = None, ytick_label: Union[Iterable[AnyStr], Callable] =None,
             legends=None, handles=None, xlim = None, ylim = None,
             style=0):

    ax.set_xlabel(xlabel, **xlabel_style)
    ax.set_ylabel(ylabel, **ylabel_style)

    if xtick_label is None:
        xtick_label = [f"{x}" for x in xticks]
    elif isinstance(xtick_label, Callable):
        xtick_label = map(xtick_label, xticks)

    if ytick_label is None:
        ytick_label = [f"{x}" for x in yticks]
    elif isinstance(ytick_label, Callable):
        ytick_label = map(ytick_label, yticks)

    xrange = abs(max(xticks) - min(xticks))
    yrange = abs(max(yticks) - min(yticks))
    if not xlim is None:
        ax.set(xlim=(xlim[0]*xrange, xlim[1]*xrange))
    
    if not ylim is None:
        ax.set(ylim=(ylim[0]*yrange, ylim[1]*yrange))

    # special reqs
    # ax.tick_params(length=0)
    if style == 0:
        ax.grid(axis='y', color='0.8', zorder=0)
        ax.spines[:].set_linewidth(2)
        ax.spines[:].set_zorder(100)
    
    elif style == 1:
        ax.grid(axis='x', color='0.8', zorder=0)
        ax.spines[:].set_linewidth(2)
        ax.spines[:].set_zorder(100)

    elif style == 2:
        ax.spines[:].set_linewidth(0)
        ax.spines['bottom'].set_linewidth(1.5)
        ax.spines['left'].set_linewidth(1.5)
        ax.spines['bottom'].set_zorder(100)
        ax.spines['left'].set_zorder(100)

    # if isinstance(xticks[0], float):
    #     xticks = [int(x) for x in xticks]
    # if isinstance(yticks[0], float):
    #     yticks = [int(y) for y in yticks]

    # normal reqs
    ax.set_xticks(xticks, xtick_label, **xtick_style)
    ax.set_yticks(yticks, ytick_label, **ytick_style)
    if legends is not None:
        if handles is not None:
            ax.legend(handles, legends, **legend_style)
        else:
            ax.legend(legends, **legend_style)


def grid_legend_external(ax, legends, handles):
    ax.set_xticks([])
    ax.set_yticks([])
    ax.spines[:].set_linewidth(0)
    ax.legend(handles,legends, **legend_style)


def figstyle(fig, labname, axes, label, label_style, is_plot_grid = False):
    if labname in ['x', 'y']:
        if isinstance(label, List):
            for ax, l in zip(axes, label):
                fn = eval(f"ax.set_{labname}label")
                fn(l, **label_style)
        elif label is not None:
            if is_plot_grid:
                fn = eval(f"fig.sup{labname}label")
                # fn(label, y=0.1, **label_style)
                fn(label, **label_style)
            else:
                fn = eval(f"axes[0].set_{labname}label")
                fn(label, **label_style)
    else: #title
        if isinstance(label, List):
            for ax, l in zip(axes, label):
                ax.set_title(l, label_style)
        elif label is not None:
            if is_plot_grid:
                fig.suptitle(label, label_style)
            else:
                axes[0].set_title(label, label_style)


def plot_perf(bench, c_index, input_prefix, output_prefix, figsize=[6,1.5]):
    xtick_style['size'] = 10
    ytick_style['size'] = 8
    title_style['size'] = 8
    xtick_style['fontweight'] = ytick_style['fontweight']= 'regular'
    xtick_style['rotation'] = 90
    
    def _guard_load(dic, k):
        if k in dic.keys():
            return dic[k]
        else:
            return 0.
        
    index_file = osp.join('scripts', 'nameset.json')
    with open(index_file, 'r') as f:
        nameset = json.load(f, parse_float=lambda x: float(x), parse_int=lambda x: int(x))
    knames = nameset[bench]
    
    perf_file = osp.join(input_prefix, 'sl-perf.json')
    with open(perf_file, 'r') as f:
        all_dats = json.load(f, parse_float=lambda x: float(x), parse_int=lambda x: int(x))
    
    dat_dict = {}
    skip = []
    for k,v in all_dats.items():
        if k in knames:
            dat_dict[k] = np.array([_guard_load(v,'fort'), 
                           _guard_load(v,'hcpu'), 
                           _guard_load(v,'hgpu')])

    fig, axes = plt.subplots(1, len(knames), figsize=figsize)
    
    for i in range(len(knames)):
        dat = dat_dict[knames[i]]
        ax = axes[i]
        hds = plot_one(ax, [dat], [COLOR[c_index]], bar_config=[.8,.8])
        axestyle(ax, np.arange(3), [0],
                 ytick_label=lambda x: f'{x:.2f}',
                 xtick_label=['Fort', 'HCPU', 'HGPU'],
                 handles=hds,
                 style=2)
        ax.set_yscale('log')
    
    figstyle(fig, 'y', axes, 'Execution time (s)', ylabel_style)
    figstyle(fig, None, axes, [x.replace('_','\n') for x in knames], ylabel_style)
    fig.subplots_adjust(left=0.046, bottom=0.346, right=.98, top=.736, wspace=.81)
    # plt.show()
    
    ofname = osp.join(output_prefix, f'{bench}-perf.pdf')
    fig.savefig(ofname, bbox_inches='tight')


def convert_tex():
    stng_time = 7
    complexity = -2
    dat = {}
    with open('input.tex', 'r') as f:
        for line in f.readlines():
            if line[0] == '%': 
                continue
            cols = [x.strip() for x in line.split('&')]
            if len(cols) < stng_time:
                continue
            bench_name = cols[1]
            lift_time = cols[stng_time]
            comp_str = cols[complexity]
            nout = int(cols[-3])
            if lift_time == '--':
                continue    
            else:
                lift_time = float(lift_time)
            dat[bench_name] = dict(
                dim=int(comp_str.split('-')[0][:-1]),
                pt=int(comp_str.split('-')[1][:-2]),
                nout=nout,
                time=lift_time
            )
    with open(osp.join('scripts','stng-perf.json'), 'w') as of:
        json.dump(dat, of, indent=2, ensure_ascii=True)


def plot_ablation(input_prefix, output_prefix):
    fig = plt.figure(figsize=[6,3], layout='constrained')
    axes = flattern(fig.subplots(2, 2))

    knames = nameset.ablation()
    with open(osp.join(input_prefix, 'sl-lift.json')) as f:
        lift_time_obj = json.load(f, parse_float=lambda x: float(x), parse_int=lambda x: int(x))
    
    prefix = ["jacobi", "conv", "heat", "heat27"]
    prefix_full = ["2d-5p", "2d-9p", "3d-7p", "3d-27p"]
    suffix = ["b", "p", "i", "u", "uo", "t", "to"]
    suffix_full = ["LF", "PD", "IB", "LU", "LU*", "LT", "LT*"]
    dats = [[None]*7, [None]*7, [None]*7, [None]*7]
    raws = [None]*4
    for k,v in lift_time_obj.items():
        # v *= 50
        if k in knames:
            pre_suf = k.split("_")
            for i, kn in enumerate(prefix):
                if kn == pre_suf[0]:
                    if len(pre_suf) == 1:
                        raws[i] = v
                        break
                    for j, on in enumerate(suffix):
                        if pre_suf[1] == on:
                            dats[i][j] = v
                            break
    for i in range(4):
        if raws[i] > dats[i][1]:
            tmp = raws[i]
            raws[i] = dats[i][1]
            dats[i][1] = tmp

    ytks = [
        np.arange(0,21,5),
        np.arange(0,31,10),
        np.arange(0,76,25),
        np.arange(0,81,20),
    ]
    for idx, ax in enumerate(axes):
        plot_one(ax, [np.array(dats[idx])], [COLOR[idx//2]], bar_config=[.8,.8])
        ax.plot(np.arange(-1,8), [raws[idx]]*9, '--', c='r', zorder=10)
        axestyle(ax, np.arange(7), ytks[idx],
                 ytick_label=lambda x: f'{x:.0f}',
                 xtick_label=suffix_full,
                 xlim=(-0.1,1.1))
    
    figstyle(fig, "y", axes, "Lifting Time (s)", ylabel_style, is_plot_grid=True)
    figstyle(fig, "x", axes, [None]*2 + ["Optimization Techniques"]*2, xlabel_style, is_plot_grid=False)
    figstyle(fig, 'title', axes, prefix_full, title_style)
    # plt.show()
    fig.savefig(osp.join(output_prefix, "ablation.pdf"))


def plot_violin(input_prefix, output_prefix):
    fig = plt.figure(figsize=[11,2],layout='constrained')
    axes = flattern(fig.subplots(1, 4, squeeze=False))
    
    with open(osp.join('scripts','stng-perf.json'), 'r') as f:
        stng_obj = json.load(f, parse_float=lambda x: float(x), parse_int=lambda x: int(x))
    
    item = stng_obj.pop("akl81") # purging abnormal result

    stng_stat = statistic_lifting_time(stng_obj)
    stng_obj["akl81"] = item

    for ax, dim in zip(axes[:2], range(2,4)):
        xlab, dat = extract_label_and_array(stng_stat[dim], sort_with=lambda x: int(x[0]))
        data = {
            "STNG" : dat
        }
        parts = plot_one_violin(ax, data, xlab, COLOR[:1])
        new_x_tick = build_yticks(0, (parts[0][-1]), 5)
        new_xlab = [str(round(x)) for x in new_x_tick]
        axestyle(ax, new_x_tick, parts[1], ylim = (0,1.), xtick_label=new_xlab, ytick_label=lambda x: f"{round(x)}")

    with open(osp.join(input_prefix, 'sl-lift.json'), 'r') as f:
        sl_obj = json.load(f, parse_float=lambda x: float(x), parse_int=lambda x: int(x))
        sl_dic = {}
        for k in sl_obj.keys():
            if k not in stng_obj.keys():
                continue
            time = sl_obj[k]
            sl_dic[k] = {}
            sl_dic[k]['time'] = time
            sl_dic[k]['dim'] = stng_obj[k]['dim']
            sl_dic[k]['pt'] = stng_obj[k]['pt']
            sl_dic[k]['nout'] = stng_obj[k]['nout']
        sl_stat = statistic_lifting_time(sl_dic)
    
    for ax, dim in zip(axes[2:], range(2,4)):
        xlab, dat = extract_label_and_array(sl_stat[dim], sort_with=lambda x: int(x[0]))
        data = {
            "Stencil-Lifting" : dat
        }
        parts = plot_one_violin(ax, data, xlab, COLOR[1:])
        if dim == 3:
            new_x_tick = build_yticks(0, (parts[0][-1]), 8)
        else:
            new_x_tick = build_yticks(0, (parts[0][-1]), 5)
        new_xlab = [str(round(x)) for x in new_x_tick]
        axestyle(ax, new_x_tick, parts[1], ylim = (0,1.), xtick_label=new_xlab, ytick_label=lambda x: f"{round(x)}")

    figstyle(fig, "y", axes, "Lifting Time (s)", ylabel_style, is_plot_grid=True)
    figstyle(fig, "x", axes, ['Complexity']*4, xlabel_style)
    figstyle(fig, 'title', axes, ["2D Stencil", "3D Stencil", "2D Stencil", "3D Stencil"], title_style)
    # plt.show()
    fig.savefig(osp.join(output_prefix, "scale.pdf"))


def plot_stng_tab(input_prefix, output_prefix):
    header = [
        "Kernel", 
        "Fortran Time (ms)", 
        "Halide CPU Time (ms)", 
        "Halide GPU Time (ms)", 
        "Lifting Time of STNG (s)",
        "Lifting Time of Stencil-Lifting (s)", 
        "Speedup over STNG"]

    knames = nameset.stng()
    
    base_file = osp.join('scripts', 'stng-perf.json')
    with open(base_file, 'r') as f:
        all_base = json.load(f, parse_float=lambda x: float(x), parse_int=lambda x: int(x)) 

    perf_file = osp.join(input_prefix, 'sl-perf.json')
    with open(perf_file, 'r') as f:
        all_perf = json.load(f, parse_float=lambda x: float(x), parse_int=lambda x: int(x))
    
    lift_file = osp.join(input_prefix, 'sl-lift.json')
    with open(lift_file, 'r') as f:
        all_lift = json.load(f, parse_float=lambda x: float(x), parse_int=lambda x: int(x))
    
    dic = {}
    for ker in knames:
        if all_base[ker]['time'] is None:
            continue
        row = {}
        row[header[1]] = all_perf[ker]['fort'] if ker in all_perf.keys() else "/"
        row[header[2]] = all_perf[ker]['hcpu'] if ker in all_perf.keys() else "/"
        row[header[3]] = all_perf[ker]['hgpu'] if ker in all_perf.keys() else "/"
        row[header[4]] = all_base[ker]['time'] if ker in all_base.keys() else "/"
        row[header[5]] = all_lift[ker] if ker in all_lift.keys() else "/"
        row[header[6]] = all_base[ker]['time'] / all_lift[ker] if ker in all_base.keys() else "/"
        dic[ker] = row
    
    ofname = osp.join(output_prefix, 'table-2.md')
    render_markdown(header, dic, ofname)


def plot_our_tab(input_prefix, output_prefix):
    header = [
        "Kernel", 
        "Fortran Time (ms)", 
        "Halide CPU Time (ms)", 
        "Halide GPU Time (ms)", 
        "Lifting Time of Stencil-Lifting (s)"]

    knames = nameset.benchmarks()
    
    base_file = osp.join('scripts', 'stng-perf.json')
    with open(base_file, 'r') as f:
        all_base = json.load(f, parse_float=lambda x: float(x), parse_int=lambda x: int(x)) 

    perf_file = osp.join(input_prefix, 'sl-perf.json')
    with open(perf_file, 'r') as f:
        all_perf = json.load(f, parse_float=lambda x: float(x), parse_int=lambda x: int(x))
    
    lift_file = osp.join(input_prefix, 'sl-lift.json')
    with open(lift_file, 'r') as f:
        all_lift = json.load(f, parse_float=lambda x: float(x), parse_int=lambda x: int(x))
    
    dic = {}
    for ker in knames:
        row = {}
        row[header[1]] = all_perf[ker]['fort'] if ker in all_perf.keys() else "/"
        row[header[2]] = all_perf[ker]['hcpu'] if ker in all_perf.keys() else "/"
        row[header[3]] = all_perf[ker]['hgpu'] if ker in all_perf.keys() else "/"
        row[header[4]] = all_lift[ker] if ker in all_lift.keys() else "/"
        dic[ker] = row
    
    ofname = osp.join(output_prefix, 'table-2.md')
    render_markdown(header, dic, ofname)


if __name__ == "__main__":
    cwd = osp.abspath(osp.curdir)
    pdir = cwd.split(osp.sep)[-1]
    if pdir != "stencil-lifting":
        print("""
              please execute this script from the root dir of `stencil-lifting`
              i.e. cd <path/to/stencil-lifting> and execute `python scripts/plot.py`
              """)
        exit(1)

    in_dir = osp.join(cwd, 'out', 'stat')
    out_dir = osp.join(cwd, 'out', 'fig')

    if not osp.exists(out_dir):
        os.mkdir(out_dir)
    
    plot_stng_tab(in_dir, out_dir)
    plot_our_tab(in_dir, out_dir)
    plot_ablation(in_dir, out_dir)
    plot_violin(in_dir, out_dir)