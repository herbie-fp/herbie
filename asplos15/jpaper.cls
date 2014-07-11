\NeedsTeXFormat{LaTeX2e}
% \ProvidesClass{jpaper}[ASPLOS 2014 Submission template]
\ProvidesClass{jpaper}[2012/08/02 ISCA Submission template]

% jpaper provides two modes:
% In addition to the usual options leqno and fleqn, jpaper also provides the
% following additional flags (each of which can be turned on by specifying the
% flag as an option, or turned off by specifying "no"flag as an option):
% * lineno: Enables line numbering. This may be useful while editing.
% * pageno: Enables page numbering. This is useful for anything that is not
% * hyperref: Enables the hyperref package. While the hyperref package is
%   normally convenient for your readers, IEEE PDF-eXpress does not allow it.
%
% The default set of options is camera,nolineno,nopageno,nohyperref.

\RequirePackage{etoolbox}

\DeclareOption{leqno}{\input{leqno.clo}}
\DeclareOption{fleqn}{\input{fleqn.clo}}
\newtoggle{jp@lineno}
\newtoggle{jp@pageno}
\newtoggle{jp@hyperref}
\DeclareOption{lineno}{\toggletrue{jp@lineno}}
\DeclareOption{nolineno}{\togglefalse{jp@lineno}}
\DeclareOption{pageno}{\toggletrue{jp@pageno}}
\DeclareOption{nopageno}{\togglefalse{jp@pageno}}
\DeclareOption{hyperref}{\toggletrue{jp@hyperref}}
\DeclareOption{nohyperref}{\togglefalse{jp@hyperref}}
\ExecuteOptions{nolineno,pageno,hyperref}
\ProcessOptions

% Load the underlying article class.
\LoadClass[letterpaper,10pt,twocolumn]{article}


% Standard packages.
\RequirePackage[utf8]{inputenc}
\RequirePackage[T1]{fontenc}
\RequirePackage{verbatim}
\RequirePackage{fixltx2e}
\RequirePackage[usenames,dvipsnames,svgnames,table]{xcolor}

% Basic text typesetting options.
\RequirePackage{mathptmx}
\RequirePackage[scaled=0.833]{helvet}
\RequirePackage[scaled=0.875]{couriers}
\setlength{\parindent}{1em}
\RequirePackage{setspace}
\newcommand{\normalspacing}{}
\RequirePackage{microtype}

% High penalty for widows and orphans
\widowpenalty=300 \clubpenalty=300

% Page margins:
% 3/4" left and right, 1" top, 1 1/8" bottom.
\RequirePackage{geometry}
\geometry{lmargin=0.75in,rmargin=0.75in,tmargin=1in,bmargin=1in}

% 1/4" space between columns.
\setlength{\columnsep}{0.25in}

% Title formatting.
\RequirePackage{titling}
\setlength{\droptitle}{-3.5em}

\pretitle{\begin{center}\vspace{-.5in}
ASPLOS Submission \#\asplossubmissionnumber -- Confidential Draft -- Do Not Distribute!!\\
\vspace{.35in}
\normalfont\Large\bfseries}
\posttitle{\par\end{center}}
\preauthor{\begin{center}\normalfont\large%
    \begin{tabular}[t]{c}}
\postauthor{\end{tabular}\par\end{center}}
\predate{}
\postdate{}


% The abstract should be normally sized and italicized. Abstract leading
% should, in general, follow the normal leading.
\RequirePackage{abstract}
\newcommand{\abstractspacing}{\normalspacing}
\renewcommand{\abstractnamefont}{\normalfont\large\bfseries}
\renewcommand{\abstracttextfont}{\normalfont\itshape\abstractspacing}
\setlength{\absleftindent}{0in}
\setlength{\absrightindent}{0in}
\setlength{\abstitleskip}{-1.5em}

% Compress vertical space around section and subsection headings. Make
% subsubsection headings run-in (i.e. part of the following paragraph).
\RequirePackage[compact]{titlesec}
\titleformat{\section}[hang]{\normalfont\large\bfseries}{\thesection. }%
  {0pt}{}{}
\titleformat{\subsection}[hang]{\normalfont\bfseries}{\thesubsection. }%
  {0pt}{}{}
\titleformat{\subsubsection}[runin]{\normalfont\bfseries}{\thesubsubsection. }%
  {0pt}{}{}
\newlength{\sectionspace}
\setlength{\sectionspace}{1.5ex plus 0.25ex}
\titlespacing*{\section}{0pt}{\sectionspace}{\sectionspace}
\titlespacing*{\subsection}{0pt}{\sectionspace}{\sectionspace}
\titlespacing*{\subsubsection}{0pt}{\parskip}{0.25em plus 0.1em minus 0.05em}

% Compress itemize and enumerate environments.
\RequirePackage{enumerate}
\RequirePackage{enumitem}
\setlist{noitemsep, leftmargin=*, topsep=0pt, partopsep=0pt}

% Bibliography formatting.
\newcommand{\bibspacing}{\setstretch{0}}
\let\jp@bibliography\thebibliography
\renewcommand{\thebibliography}[1]{%
  \jp@bibliography{#1}%
  \setlength{\itemsep}{-0.25em} \footnotesize \bibspacing%
  \vspace{-0.25em}}
% This is IEEEtran's bibliography formatting control macro.
\def\bstctlcite{\@ifnextchar[{\@bstctlcite}{\@bstctlcite[@auxout]}}
\def\@bstctlcite[#1]#2{\@bsphack
  \@for\@citeb:=#2\do{%
    \edef\@citeb{\expandafter\@firstofone\@citeb}%
    \if@filesw\immediate\write\csname #1\endcsname%
      {\string\citation{\@citeb}}\fi}%
  \@esphack}

% Settings for figures and tables. Figure captions are placed below the figure,
% while table captions are placed above the table. All captions are sans-serif.
\RequirePackage[font={normalsize,sf,bf}]{caption}
\RequirePackage[position=bottom]{subfig}
\captionsetup[table]{aboveskip=0.5em, belowskip=0.5em}
\captionsetup[figure]{aboveskip=0.5em, belowskip=0em}
\captionsetup[subfloat]{font={small,sf}}
\setcounter{topnumber}{2}
\setcounter{dbltopnumber}{2}
\setcounter{bottomnumber}{2}
\setcounter{totalnumber}{4}
\renewcommand{\topfraction}{0.85}
\renewcommand{\dbltopfraction}{0.9}
\renewcommand{\bottomfraction}{0.85}
\renewcommand{\textfraction}{0.07}
\renewcommand{\floatpagefraction}{0.85}
\renewcommand{\dblfloatpagefraction}{0.85}

\setlength{\floatsep}{0.5em plus 0.2em minus 0.2em}
\setlength{\dblfloatsep}{0.5em plus 0.2em minus 0.2em}
\setlength{\textfloatsep}{0.5em plus 0.2em minus 0.2em}
\setlength{\dbltextfloatsep}{0.5em plus 0.2em minus 0.2em}

% Utility packages for floats and tables.
\RequirePackage{float}
\RequirePackage{graphicx}
\RequirePackage{booktabs}
\RequirePackage{multirow}

% Page numbering.
\iftoggle{jp@pageno}{%
  \pagestyle{plain}
  \newcommand{\jp@mt@pageno}{}}{%
  \pagestyle{empty}
  \newcommand{\jp@mt@pageno}{\thispagestyle{empty}}}

% Line numbering.
\iftoggle{jp@lineno}{%
  \RequirePackage{lineno}
  \renewcommand{\linenumberfont}{\normalfont\tt\small}
  \newcommand{\jp@mt@lineno}{\linenumbers}
}{%
  \newcommand{\jp@mt@lineno}{}
}

% Replace maketitle to invoke settings for leading, page numbering, and line
% numbering.
\let\jp@maketitle\maketitle
\renewcommand{\maketitle}{%
  \jp@maketitle%
  \jp@mt@pageno%
  \jp@mt@lineno%
  \normalspacing}

% Footnotes may not span pages.
\interfootnotelinepenalty=10000

% Formatting for margin notes (which you may find useful during editing).
\let\jp@marginpar\marginpar
\renewcommand\marginpar[1]{\jp@marginpar%
  [\raggedleft\singlespacing\footnotesize\color{red} #1]%
  {\raggedright\singlespacing\footnotesize\color{red} #1}}

\setlength{\marginparwidth}{0.65in}

% Link and URL formatting.
\iftoggle{jp@hyperref}{%
  \RequirePackage{hyperref}
  \hypersetup{%
    unicode=true,%
    pdfstartview={FitH},%
    colorlinks=true,%
    citecolor=red,%
    %filecolor=black,%
    %linkcolor=black,%
    %urlcolor=black
}}{}
\RequirePackage{url}

% Scientific units.
\RequirePackage[squaren]{SIunits}

\endinput
