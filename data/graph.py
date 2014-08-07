# python2 graph.py > tmp.tex ; pdflatex tmp.tex ; open tmp.pdf

PLOT_X = 8
PLOT_Y = 6

PT_WEIGHT = "1pt"

def begin_doc():
    print """\\documentclass{article}
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
        print "\\node [%s of=%s,anchor=%s,node distance=0cm] {\\tiny %s};" % (direction, c, anchor, label)


def draw_axes(h_capt=None, v_capt=None, h_ticks=[], v_ticks=[]):
    axes_eps = 0.1
    tick_eps = 0.1
    axes_border = 1.05
    axes_x = axes_border * PLOT_X
    axes_y = axes_border * PLOT_Y

    h_lab = fresh()
    v_lab = fresh()

    print "\\draw[->] (-%f, 0) -- coordinate (%s) (%f, 0);" % (axes_eps, h_lab, axes_x)
    if h_capt is not None:
        print "\\node [below of=%s] {%s};" % (h_lab, h_capt)

    print "\\draw[->] (0, -%f) -- coordinate (%s) (0, %f);" % (axes_eps, v_lab, axes_y)
    if v_capt is not None:
        print "\\node [left of=%s,anchor=south, rotate=90] {%s};" % (v_lab, v_capt)

    for (x,label) in h_ticks:
        draw_line((x, -tick_eps), (x, tick_eps), label, "below")

    for (y,label) in v_ticks:
        draw_line((-tick_eps, y), (tick_eps, y), label, "left")


def draw_point(p):
    print "\\draw[fill=black] (%f,%f) circle (%s);" % (p[0], p[1], PT_WEIGHT)

def draw_rect(ll,ur):
    print "\\draw (%f,%f) rectangle (%f,%f);" % (ll[0], ll[1], ur[0], ur[1])

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
        draw_rect((prev[0], 0), cur)
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
    for i in range(0,551,50):
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
    hi = data[-1] * 1.15

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
    for i in range(0,351,50):
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

mpfr_data = [336,336,208,208,304,528,528,336,208,80,208,336,208,336,336,208,320,208,336,208,528,528,208,336,336,320,80,208]

time_data = [13.3,5.3,300,174,44.5,44,102,48.2,22.3,300,192,32.4,282,14.9,30.7,126,40.2,25.1,28.3,11.6,41.8,78,144,144,96,53.3,41.3,66,29.5]

def read_simple_data_file(name):
    ans = []
    with open(name) as f:
        for line in f:
            ans.append(float(line))
    return ans


if __name__ == '__main__':
    begin_doc()
    #draw_mpfr_bits_cdf(read_simple_data_file('mpfr-bits.csv'))
    #draw_histo(mpfr_data)
    draw_time_cdf(read_simple_data_file('casio-runtime.csv'))
    end_doc()
    #print read_mpfr()
