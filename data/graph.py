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

def draw_cdf(data):
    "given a list of numbers, print tikz for a cdf"
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
        (xp,yp) = to_plot_space((x, y))
        pts.append((x,y))

    pts.append((hi, pts[-1][1]))
    for i in range(1,len(pts)):
        prev = pts[i-1]
        cur = pts[i]

        mid = (cur[0], prev[1])
        draw_line(to_plot_space(prev), to_plot_space(mid), opts="thick")
        draw_line(to_plot_space(mid), to_plot_space(cur), opts="thick")

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

mpfr_data = [336,336,208,208,304,528,528,336,208,80,208,336,208,336,336,208,320,208,336,208,528,528,208,336,336,320,80,208]
if __name__ == '__main__':
    begin_doc()
    draw_cdf(mpfr_data)
    end_doc()
