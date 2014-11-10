# python2 graph.py > tmp.tex ; pdflatex tmp.tex ; open tmp.pdf
import sys

PLOT_X = 8
PLOT_Y = 3

plot_stack = []

TICKS = (8, 12)
def choose_ticks(max):
    step_min = max / float(TICKS[1])
    step_max = max / float(TICKS[0])

    # Now we find the "roundest" number in [step_min, step_max]
    DIVISORS = [0.1, 0.2, 0.25, 0.3, 0.4, 0.5,
                0.6, 0.75, 0.8,
                1, 1.5, 2, 3, 4, 5, 6,
                8, 10, 12, 14, 15, 16,
                20, 25, 30, 35, 40, 45, 50, 60,
                80, 100, 120, 140,
                150, 200, 250, 300, 350, 400, 500,
                600, 666.666, 700, 750, 800, 900, 1000]
    sys.stderr.write("%s < ? < %s\n" % (step_min, step_max))
    step = min(d for d in DIVISORS if step_min <= d <= step_max)

    if max / float(step) == int(max / float(step)):
        return frange(0, max, step) + [max]
    else:
        return frange(0, max, step)

def frange(min, max, step):
    return [(i + min) * step for i in range(0, int(max / step))]

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
    '''
    print """\\documentclass{standalone}
\\usepackage{tikz}
\\begin{document}"""
'''
    pass

def end_doc():
    #print "\\end{document}"
    pass

def begin_picture():
    print "\\begin{tikzpicture}[>=stealth]"

def end_picture():
    print "\\end{tikzpicture}"

uniq = 0
def fresh():
    global uniq
    uniq += 1
    return "node" + str(uniq)

def draw_line(start, end, label=None, direction="left", anchor=None, opts=""):
    c = fresh()
    print "\\draw[%s] (%f, %f) -- (%f, %f);" % (opts, start[0], start[1], end[0], end[1])

    if label is not None:
        print "\\coordinate (%s) at (%f, %f) {};" % (c, start[0], start[1])
        anchor = anchor or ("east" if direction == "left" else "north")
        print "\\node [%s of=%s,anchor=%s,node distance=0cm] {\\small %s};" % (direction, c, anchor, label)


def draw_axes(h_capt=None, v_capt=None, h_labels=[], v_labels=[], h_ticks=[], v_ticks=[], v_axis=True, title=None):
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

    for (y,label) in v_labels:
        draw_line((0, y), (0, y), label, "left")

    for (x,label) in h_labels:
        draw_line((x, 0), (x, 0), label, "rotate=90,below", anchor="east")

def draw_point(p):
    print "\\draw[fill=black] (%f,%f) circle (%s);" % (p[0], p[1], PT_WEIGHT)

def draw_rect(ll,ur, opts=""):
    print "\\draw[%s] (%f,%f) rectangle (%f,%f);" % (opts, ll[0], ll[1], ur[0], ur[1])

def draw_arrow(a, b, opts=""):
    eps = .06
    print "\\draw[%s,->] (%f,%f) -- (%f,%f);" % (opts, a[0], a[1], b[0], b[1])
    if a[0] == b[0]:
        print "\\draw[%s] (%f,%f) -- (%f,%f);" % (opts, a[0]-eps, a[1], a[0]+eps, a[1])
    else:
        print "\\draw[%s] (%f,%f) -- (%f,%f);" % (opts, a[0], a[1]-eps, a[0], a[1]+eps)

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
    for i in choose_ticks(hi):
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
    for i in choose_ticks(hi):
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

def draw_overhead_cdf(iname, oname, nriname=None, nroname=None):
    left = read_time_data(iname)
    right = read_time_data(oname)
    if nriname:
        rleft = read_time_data(nriname)
        rright = read_time_data(nroname)
    else:
        rleft = left
        rright = right

    assert len(left) == len(right)

    data = [y / x for (x,y) in zip(left, right)]
    rdata = [y / x for (x,y) in zip(rleft, rright)]

    #data, rdata = zip(*sorted(zip(data, rdata), key=lambda x: x[0]))
    data.sort()
    rdata.sort()
    n = len(data)
    hi = max(max(data), max(rdata)) * 1.1

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

    pts = [(0,0)]
    for (i,d) in enumerate(rdata):
        x = d
        y = i+1
        pts.append((x,y))
    pts.append((hi, pts[-1][1]))

    for i in range(1, len(pts)):
        prev = pts[i-1]
        cur = pts[i]

        mid = (cur[0], prev[1])
        draw_line(to_plot_space(prev), to_plot_space(mid), opts="gray")
        draw_line(to_plot_space(mid), to_plot_space(cur), opts="gray")


    h_ticks = []
    for i in choose_ticks(hi):
        xp = to_plot_space((i,0))[0]
        h_ticks.append((xp, i))

    draw_line(to_plot_space((1, 0)), to_plot_space((1, n)), opts="thin")

    v_ticks = [(0,"0\\%")]
    for i in range(1,5):
        v_ticks.append((to_plot_space((0,(float(n)/4)*i))[1], "%d\\%%" % ((100 / 4)*i)))

    draw_axes(h_capt="Casio overhead (ratio)",
              v_capt="\\% of benchmarks",
              h_ticks=h_ticks,
              v_ticks=v_ticks)

    end_picture()



from math import *

def draw_sample_points(data):
    data = [(log10(x), log10(y)) for (x,y) in data]
    hix = max([x for (x,y) in data])
    hiy = max([y for (x,y) in data])

    lox = min([x for (x,y) in data])
    loy = min(min([y for (x,y) in data]), -2)

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
             range(10000,20001,10000):
        x = i
        xp = to_plot_space((log10(x) + lox, 0))[0]

        h_ticks.append((xp, str(x) if xlabel(x) else None))


    v_ticks = [(0, "0.01")]

    def ylabel(y):
        return y == 0.1 or y == 1.0 or y == 2.0


    for i in range(2, 11) + range(10,101, 10) + range(100,201,100):
        y = i / 100.0
        yp = to_plot_space((0, log10(y)))[1]
        v_ticks.append((yp, str(y) if ylabel(y) else None))

    draw_axes(h_capt="\\# points sampled",
              v_capt="Standard error",
              h_ticks=h_ticks,
              v_ticks=v_ticks)
    end_picture()

def draw_improvement_rectangles(data, title=None, lhs=False):
    begin_picture()

    n = len(data)

    hi = int(max(max([p[0] for p in data]), max([p[1] for p in data])) + 0.5)

    def to_plot_space(p):
        x = PLOT_X * (float(p[0]) / hi)
        y = PLOT_Y * (float(p[1]) / (n + 1))
        return (x,y)

    for i in range(n):
        l = data[i][0]
        r = data[i][1]

        draw_line(to_plot_space((0, i+1)), to_plot_space(((l+r)/2, i+1)), opts="gray,thin")
        if abs(l - r) < 1:
            draw_point(to_plot_space(((l + r) / 2.0, i + 1)))
        elif l <= r:
            #draw_rect(to_plot_space((l, i + 0.75)),
            #          to_plot_space((r, i + 1.25)))
            draw_arrow(to_plot_space((l, i + 1)),
                       to_plot_space((r, i + 1)), "very thick")
        else:
            #draw_rect(to_plot_space((r, i + 0.75)),
            #          to_plot_space((l, i + 1.25)), "fill=red")\\
            draw_arrow(to_plot_space((l, i + 1)),
                       to_plot_space((r, i + 1)), "red,very thick")

    v_ticks = [(to_plot_space((0, i+1))[1], "{\\scriptsize %s}" % (data[i][-1])) for i in range(0, n)]

    draw_axes(h_capt="Bits correct (longer is better)",
              h_ticks=[(to_plot_space((i,0))[0], str(i)) for i in range(0, hi+1, 8)],
              v_labels=v_ticks,
              v_axis=False,
              title=title
    )
    end_picture()


def draw_regimes(names, tcid, tcod, nrid, nrod, title=None, v_capt=None):
    left = [64.0 - x for x in read_improvement_data(nrid)]
    other_left = [64.0 - x for x in read_improvement_data(tcid)]
    mid = [64.0 - x for x in read_improvement_data(nrod)]
    right = [64.0 - x for x in read_improvement_data(tcod)]

    true_left = [(x+y)/2 for (x,y) in zip(left,other_left)]

    
    n = len(true_left)
    assert len(mid) == n and len(right) == n


    data1 = []
    data2 = []

    for i in range(n):
        n = names[i]
        l = true_left[i]
        m = mid[i]
        r = right[i]

        if abs(r - m) > 1:
            data1.append((l, m, n))
            data2.append((m, r, n))

    data1, data2 = zip(*sorted(zip(data1, data2), key=lambda x: x[0][0]))

    hi = max(max([p[1] for p in data1]), max([p[1] for p in data2]))

    n = len(data1)
    assert n == len(data2)

    def to_plot_space(p):
        y = PLOT_Y * (float(p[0]) / hi)
        x = PLOT_X * (float(p[1]) / (n + 1))
        return (x,y)

    begin_picture()
    for i in range(n):
        l, m, _ = data1[i]
        m, r, _ = data2[i]

        draw_line(to_plot_space((0, i + 1)),
                  to_plot_space((m, i + 1)), opts="gray,thin")

        draw_point(to_plot_space((l, i + 1)))

        if abs(m - r) < 0.5:
            pass
        elif m <= r:
            draw_arrow(to_plot_space((m, i + 1)),
                       to_plot_space((r, i + 1)), "very thick,black")
        else:
            draw_arrow(to_plot_space((m, i + 1)),
                       to_plot_space((r, i + 1)), "very thick,red")

    draw_axes(v_capt="Bits Correct",
              v_ticks=[(to_plot_space((i,0))[1], str(i)) for i in range(0, 65, 8)],
              h_labels=[(to_plot_space((0, i+1))[0], data1[i][-1]) for i in range(n)],
              title=title
    )
    end_picture()


def read_names(name):
    ans = []
    with open(name) as f:
        reader = csv.reader(f)
        for line in reader:
            ans.append(line[1])
    return ans

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
    print ""
    print "\t-d DIR\tread from directory DIR"

if __name__ == '__main__':
    if "-d" in sys.argv:
        i = sys.argv.index("-d")
        dir = sys.argv[i + 1]
        del sys.argv[i:i+2]
    else:
        dir = "."
    
    if len(sys.argv) < 2:
        print "which graph to make?"
        usage()
        sys.exit(1)
    if sys.argv[1] == "bits":
        begin_doc()
        draw_mpfr_bits_cdf(read_simple_data_file(dir + '/mpfr-bits.csv'))
        end_doc()
    elif sys.argv[1] == "time":
        begin_doc()
        draw_time_cdf(read_simple_data_file(dir + '/casio-runtime.csv'))
        end_doc()
    elif sys.argv[1] == "err":
        begin_doc()
        draw_sample_points(read_sample_points(dir + '/sample-points.csv'))
        end_doc()
    elif sys.argv[1] == "rect-d":
        begin_doc()
        push_plot_params(12, 7)

        names = read_names(dir + "/tc.names.csv")
        left = [64.0 - x for x in read_improvement_data(dir + '/tc.id.csv')]
        right = [64.0 - x for x in read_improvement_data(dir + '/tc.od.csv')]
        assert len(left) == len(right)

        data = zip(left, right, names)

        data.sort(key=lambda p: min(p[0], p[1]) , reverse=True)

        draw_improvement_rectangles(data, title="Double Precision", lhs=True)
        pop_plot_params()
        end_doc()
    elif sys.argv[1] == "rect-f":
        begin_doc()
        push_plot_params(6, 7)

        names = read_names(dir + "/tc.names.csv")
        leftd = [64.0 - x for x in read_improvement_data(dir + '/tc.id.csv')]
        rightd = [64.0 - x for x in read_improvement_data(dir + '/tc.od.csv')]
        leftf = [32.0 - x for x in read_improvement_data(dir + '/tc.if.csv')]
        rightf = [32.0 - x for x in read_improvement_data(dir + '/tc.of.csv')]

        assert \
            len(leftd) == len(rightd) and \
            len(rightd) == len(leftf) and \
            len(leftf) == len(rightf)

        data = zip(leftd, rightd, leftf, rightf, names)

        data.sort(key=lambda p: min(p[0], p[1]) , reverse=True)
        data = [(p[2], p[3], p[-1]) for p in data]

        draw_improvement_rectangles(data, title="Single Precision")
        pop_plot_params()
        end_doc()
    elif sys.argv[1] == "overhead-d":
        begin_doc()
        draw_overhead_cdf(dir + '/tc.id.csv', dir + '/tc.od.csv',
                          dir + '/nr.id.csv', dir + '/nr.od.csv')
        end_doc()
    elif sys.argv[1] == "overhead-f":
        begin_doc()
        draw_overhead_cdf(dir + '/tc.if.csv', dir + '/tc.of.csv')
        end_doc()
    elif sys.argv[1] == "improve-summary":
        left = read_improvement_data(dir + '/tc.id.csv')
        right = read_improvement_data(dir + '/tc.od.csv')
        assert len(left) == len(right)

        data = []

        for (x,y) in zip(left, right):
            if y <= x:
                data.append(x-y)
            else:
                data.append(0)
        summary(data)
    elif sys.argv[1] == "overhead-summary":
        left = read_time_data(dir + '/tc.id.csv')
        right = read_time_data(dir + '/tc.od.csv')
        assert len(left) == len(right)

        data = []
        for (x,y) in zip(left, right):
            data.append(y / x)

        summary(data)
    elif sys.argv[1] == "regimes":
        begin_doc()
        names = read_names(dir + "/tc.names.csv")
        draw_regimes(names,
                     dir + '/tc.id.csv', dir + '/tc.od.csv',
                     dir + '/nr.id.csv', dir + '/nr.od.csv')
        end_doc()

        

        
    else:
        print "unknown option: " + sys.argv[1]
        usage()
        sys.exit(1)
