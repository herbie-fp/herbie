# python2 graph.py > tmp.tex ; pdflatex tmp.tex ; open tmp.pdf

PLOT_X = 8
PLOT_Y = 5

plot_stack = []

def push_plot_params(x,y):
    global PLOT_X
    global PLOT_Y
    plot_stack.append((PLOT_X, PLOT_Y))
    PLOT_X = x
    PLOT_Y = y

def pop_plot_params():
    global PLOT_X
    global PLOT_Y
    (PLOT_X, PLOT_Y) = plot_stack.pop()

PT_WEIGHT = "1pt"

def begin_doc():
    print """\\documentclass{standalone}
\\usepackage{tikz}
\\begin{document}"""

def end_doc():
    print "\\end{document}"

def begin_picture():
    print "\\begin{tikzpicture}"

def end_picture():
    print "\\end{tikzpicture}"

uniq = 0
def fresh():
    global uniq
    uniq += 1
    return "node" + str(uniq)

def draw_line(start, end, label=None, direction="left", opts=""):
    c = fresh()
    print "\\draw[%s] (%f, %f) -- (%f, %f);" % (opts, start[0], start[1], end[0], end[1])

    if label is not None:
        print "\\coordinate (%s) at (%f, %f) {};" % (c, start[0], start[1])
        anchor = "east" if direction == "left" else "north"
        print "\\node [%s of=%s,anchor=%s,node distance=0cm] {\\small %s};" % (direction, c, anchor, label)


def draw_axes(h_capt=None, v_capt=None, h_ticks=[], v_ticks=[], v_axis=True, title=None):
    axes_eps = 0.1
    tick_eps = 0.1
    axes_border = 1.05
    axes_x = axes_border * PLOT_X
    axes_y = axes_border * PLOT_Y

    h_lab = fresh()
    v_lab = fresh()

    if title is not None:
        print "\\node at (%f, %f) {%s};" % (axes_x / 2, axes_y, title)

    print "\\draw[->] (-%f, 0) -- coordinate (%s) (%f, 0);" % (axes_eps, h_lab, axes_x)
    if h_capt is not None:
        print "\\node [below of=%s] {%s};" % (h_lab, h_capt)

    if not v_axis:
        if v_capt is not None:
            print "\\node [anchor=south, rotate=90] at (%f,%f) {%s};" % (0, axes_y/2.0, v_capt)
    else:
        print "\\draw[->] (0, -%f) -- coordinate (%s) (0, %f);" % (axes_eps, v_lab, axes_y)
        if v_capt is not None:
            print "\\node [left of=%s,anchor=south, rotate=90] {%s};" % (v_lab, v_capt)

    for (x,label) in h_ticks:
        draw_line((x, -tick_eps), (x, tick_eps), label, "below")

    for (y,label) in v_ticks:
        draw_line((-tick_eps, y), (tick_eps, y), label, "left")


def draw_point(p):
    print "\\draw[fill=black] (%f,%f) circle (%s);" % (p[0], p[1], PT_WEIGHT)

def draw_rect(ll,ur, opts=""):
    print "\\draw[%s] (%f,%f) rectangle (%f,%f);" % (opts, ll[0], ll[1], ur[0], ur[1])

def draw_histo(data, n_buckets=10):
    data.sort()
    n = len(data)
    lo = data[0]
    hi = data[-1]
    rng = hi - lo

    histo = [[] for i in range(n_buckets)]

    for x in data:
        i = int(n_buckets * (float(x-lo) / rng))
        if i == n_buckets:
            i -= 1
        histo[i].append(x)

    top = max(map(len, histo))

    def to_plot_space(pt):
        scale = 1.0
        x = PLOT_X * (float(pt[0]) / (scale * n_buckets))
        y = PLOT_Y * (float(pt[1]) / (scale * top))
        return (x,y)

    corners = [(0,0)]

    for (i,b) in enumerate(histo):
        corners.append((i,len(b)))

    begin_picture()
    draw_axes()
    for i in range(1,len(corners)):
        prev = corners[i-1]
        cur = corners[i]
        draw_rect(to_plot_space((prev[0], 0)), to_plot_space(cur))
    end_picture()

def draw_mpfr_bits_cdf(data):
    data.sort()
    n = len(data)
    lo = data[0]
    hi = data[-1] * 1.05

    begin_picture()

    def to_plot_space(pt):
        x = PLOT_X * (float(pt[0]) / hi)
        y = PLOT_Y * (float(pt[1]) / n)
        return (x,y)

    pts = [(0,0)]
    for (i,d) in enumerate(data):
        x = d
        y = i+1
        pts.append((x,y))

    pts.append((hi, pts[-1][1]))
    for i in range(1,len(pts)):
        prev = pts[i-1]
        cur = pts[i]

        mid = (cur[0], prev[1])
        draw_line(to_plot_space(prev), to_plot_space(mid)) #, opts="thick")
        draw_line(to_plot_space(mid), to_plot_space(cur)) #, opts="thick")

    h_ticks = []
    for i in range(0,601,50):
        xp = to_plot_space((i,0))[0]
        h_ticks.append((xp, i))

    v_ticks = [(0,"0\\%")]
    for i in range(1,5):
        v_ticks.append((to_plot_space((0,(float(n)/4)*i))[1], "%d\\%%" % ((100 / 4)*i)))


    draw_axes(h_capt="Precision Required (bits)",
              v_capt="\\% of benchmarks",
              h_ticks=h_ticks,
              v_ticks=v_ticks)


    end_picture()

def draw_time_cdf(data):
    data.sort()

    n = len(data)
    hi = data[-1] * 1.05

    xscale = 1.0

    def to_plot_space(pt):
        x = PLOT_X * xscale * (float(pt[0]) / hi)
        y = PLOT_Y * (float(pt[1]) / n)
        return (x,y)

    begin_picture()

    pts = [(0,0)]
    for (i,d) in enumerate(data):
        x = d
        y = i+1
        pts.append((x,y))
    pts.append((hi, pts[-1][1]))

    for i in range(1, len(pts)):
        prev = pts[i-1]
        cur = pts[i]

        mid = (cur[0], prev[1])
        draw_line(to_plot_space(prev), to_plot_space(mid))
        draw_line(to_plot_space(mid), to_plot_space(cur))


    h_ticks = []
    for i in range(0,226,25):
        xp = to_plot_space((i,0))[0]
        h_ticks.append((xp, i))


    v_ticks = [(0,"0\\%")]
    for i in range(1,5):
        v_ticks.append((to_plot_space((0,(float(n)/4)*i))[1], "%d\\%%" % ((100 / 4)*i)))

    draw_axes(h_capt="Time to run Casio (s)",
              v_capt="\\% of benchmarks",
              h_ticks=h_ticks,
              v_ticks=v_ticks)

    end_picture()

def draw_overhead_cdf(iname, oname):
    left = read_time_data(iname)
    right = read_time_data(oname)

    assert len(left) == len(right)

    data = [y / x for (x,y) in zip(left, right)]

    data.sort()
    n = len(data)
    hi = data[-1] * 1.15

    xscale = 0.95
    def to_plot_space(pt):
        x = PLOT_X * (float(pt[0]) / hi)
        y = PLOT_Y * (float(pt[1]) / n)
        return (x,y)

    begin_picture()

    pts = [(0,0)]
    for (i,d) in enumerate(data):
        x = d
        y = i+1
        pts.append((x,y))
    pts.append((hi, pts[-1][1]))

    for i in range(1, len(pts)):
        prev = pts[i-1]
        cur = pts[i]

        mid = (cur[0], prev[1])
        draw_line(to_plot_space(prev), to_plot_space(mid))
        draw_line(to_plot_space(mid), to_plot_space(cur))


    h_ticks = []
    for i in [0.1, 0.5, 1.0, 2.0, 3.0, 4.0, 5.0]:
        xp = to_plot_space((i,0))[0]
        h_ticks.append((xp, i))


    v_ticks = [(0,"0\\%")]
    for i in range(1,5):
        v_ticks.append((to_plot_space((0,(float(n)/4)*i))[1], "%d\\%%" % ((100 / 4)*i)))

    draw_axes(h_capt="Casio overhead (ratio)",
              v_capt="\\% of benchmarks",
              h_ticks=h_ticks,
              v_ticks=v_ticks)

    end_picture()
    #print left
    #print right
    #print data



from math import *

def draw_sample_points(data):
    data = [(log10(x), log10(y)) for (x,y) in data]
    hix = max([x for (x,y) in data])
    hiy = max([y for (x,y) in data])

    lox = min([x for (x,y) in data])
    loy = min([y for (x,y) in data])

    def to_plot_space(pt):
        bump = 0.05
        xscale = 0.95
        yscale = 0.88
        x = PLOT_X * xscale * (((pt[0] - lox) / (hix - lox)) + bump)
        y = PLOT_Y * yscale * (((pt[1] - loy) / (hiy - loy)) + bump)
        return (x,y)

    begin_picture()
    for p in data:
        draw_point(to_plot_space(p))

    h_ticks = []
    def xlabel(x):
        if x == 1 or x == 30000:
            return True
        if x == 10000:
            return False
        if x % 10 != 0:
            return False
        return xlabel(x//10)

    for i in range(1,11) + range(10,101,10) + \
             range(100,1001,100) + range(1000,9001,1000) + \
             range(10000,30001,10000):
        x = i
        xp = to_plot_space((log10(x) + lox, 0))[0]

        h_ticks.append((xp, str(x) if xlabel(x) else None))


    v_ticks = [(0,'0')]

    def ylabel(y):
        return y == 0.1 or y == 1.0 or y == 2.0


    for i in range(1,11) + range(10,21,10):
        y = i / 10.0
        yp = to_plot_space((0, log10(y)))[1]
        v_ticks.append((yp, str(y) if ylabel(y) else None))

    draw_axes(h_capt="\\# points sampled",
              v_capt="Standard error",
              h_ticks=h_ticks,
              v_ticks=v_ticks)
    end_picture()

def draw_improvement_rectangles(data, v_capt=None, title=None):
    begin_picture()

    n = len(data)

    hi = max(max([p[0] for p in data]), max([p[1] for p in data]))

    def to_plot_space(p):
        x = PLOT_X * (float(p[0]) / hi)
        y = PLOT_Y * (float(p[1]) / (n + 1))
        return (x,y)

    for i in range(n):
        l = data[i][0]
        r = data[i][1]

        if abs(l - r) < 0.5:
            draw_point(to_plot_space(((l + r) / 2.0, i + 1)))
        elif l <= r:
            draw_rect(to_plot_space((l, i + 0.75)),
                      to_plot_space((r, i + 1.25)))
        else:
            draw_rect(to_plot_space((r, i + 0.75)),
                      to_plot_space((l, i + 1.25)), "fill=red")


    draw_axes(h_capt="Bits correct",
              v_capt=v_capt,
              h_ticks=[(to_plot_space((i,0))[0], str(i)) for i in range(0, 65, 8)],
              v_axis=False,
              title=title
    )
    end_picture()




def read_simple_data_file(name):
    ans = []
    with open(name) as f:
        for line in f:
            ans.append(float(line))
    return ans

import csv
def read_sample_points(name):
    with open(name, 'rb') as f:
        reader = csv.reader(f)
        header = reader.next()
        pts_col = header.index('pts')
        se_col = header.index('se')

        ans = []
        for row in reader:
            p = float(row[pts_col])
            s = float(row[se_col])
            if p > 0:
                ans.append((p,s))

        return ans

def read_time_data(name):
    column = 1

    with open(name, 'rb') as f:
        reader = csv.reader(f)

        ans = []
        for row in reader:
            ans.append(float(row[column]))
        return ans

def read_improvement_data(name):
    column = 3
    with open(name, 'rb') as f:
        reader = csv.reader(f)

        ans = []
        for row in reader:
            ans.append(float(row[column]))
    return ans

def summary(data):
    data.sort()
    s = sum(data)
    print 'min:\t' + str(min(data))
    print 'mean:\t' + str(float(s)/len(data))
    print 'median:\t' + str(data[len(data)/2])

    g = 1
    c = 0
    for x in data:
        if x > 0:
            g *= x
            c += 1
    g **= 1.0/c
    print 'g-mean:\t' + str(g)

    print 'max:\t' + str(max(data))

import sys

def usage():
    print "Usage:"
    print "\tbits\thow many bits of precision needed per benchmark?"
    print "\ttime\thow long does casio take?"
    print "\terr\thow does casio depend on number of points sampled?"
    print "\trect-f\thow does casio improve precision (float)"
    print "\trect-d\thow does casio improve precision (double)"
    print "\toverhead-f\thow does casio change performance (float)"
    print "\toverhead-d\thow does casio change performance (double)"

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print "which graph to make?"
        usage()
        sys.exit(1)
    if sys.argv[1] == "bits":
        begin_doc()
        draw_mpfr_bits_cdf(read_simple_data_file('mpfr-bits.csv'))
        end_doc()
    elif sys.argv[1] == "time":
        begin_doc()
        draw_time_cdf(read_simple_data_file('casio-runtime.csv'))
        end_doc()
    elif sys.argv[1] == "err":
        begin_doc()
        draw_sample_points(read_sample_points('sample-points.csv'))
        end_doc()
    elif sys.argv[1] == "rect-d":
        begin_doc()
        push_plot_params(8, 8)

        left = [64.0 - x for x in read_improvement_data('regimes.id.csv')]
        right = [64.0 - x for x in read_improvement_data('regimes.od.csv')]
        assert len(left) == len(right)

        data = zip(left, right)

        data.sort(key=lambda p: min(p[0], p[1]) , reverse=True)

        draw_improvement_rectangles(data, title="Double Precision")
        pop_plot_params()
        end_doc()
    elif sys.argv[1] == "rect-f":
        begin_doc()
        push_plot_params(8, 8)

        leftd = [64.0 - x for x in read_improvement_data('regimes.id.csv')]
        rightd = [64.0 - x for x in read_improvement_data('regimes.od.csv')]
        leftf = [64.0 - x for x in read_improvement_data('regimes.if.csv')]
        rightf = [64.0 - x for x in read_improvement_data('regimes.of.csv')]

        assert \
            len(leftd) == len(rightd) and \
            len(rightd) == len(leftf) and \
            len(leftf) == len(rightf)

        data = zip(leftd, rightd, leftf, rightf)

        data.sort(key=lambda p: min(p[0], p[1]) , reverse=True)
        data = list(map(lambda p: (p[2], p[3]), data))

        draw_improvement_rectangles(data, title="Single Precision", v_capt="Benchmark")
        pop_plot_params()
        end_doc()
    elif sys.argv[1] == "overhead-d":
        begin_doc()
        draw_overhead_cdf('regimes.id.csv', 'regimes.od.csv')
        end_doc()
    elif sys.argv[1] == "overhead-f":
        begin_doc()
        draw_overhead_cdf('regimes.if.csv', 'regimes.of.csv')
        end_doc()
    elif sys.argv[1] == "improve-summary":
        left = read_improvement_data('regimes.id.csv')
        right = read_improvement_data('regimes.od.csv')
        assert len(left) == len(right)

        data = []

        for (x,y) in zip(left, right):
            if y <= x:
                data.append(x-y)
            else:
                data.append(0)
        summary(data)
    elif sys.argv[1] == "overhead-summary":
        left = read_time_data('regimes.id.csv')
        right = read_time_data('regimes.od.csv')
        assert len(left) == len(right)

        data = []
        for (x,y) in zip(left, right):
            data.append(y / x)

        summary(data)
    else:
        print "unknown option: " + sys.argv[1]
        usage()
        sys.exit(1)
