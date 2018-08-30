\documentclass{article}
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{amsfonts}
\usepackage[margin=1in]{geometry}
\usepackage[breakable, skins, raster]{tcolorbox}
\usepackage{tabu}
\setlength{\parindent}{0pt}
% For llbracket and rrbracket
\usepackage{stmaryrd}

\newtcolorbox{note}[2][]{
	colback=black!2,
	colframe=black!40,
	fonttitle=\large\scshape,
	flushleft title,
	title=#2,
	#1                             
}

\newtcolorbox{example}[1][]
{
	enhanced,
	colback=black!2,
	%colframe=green!60!black,
	fonttitle=\large\sffamily\bfseries,
	title=Example,
	attach boxed title to top left={
		yshift=-\tcboxedtitleheight,
		yshifttext=-\tcboxedtitleheight,
		%xshift=-\tcboxedtitlewidth/5,
	}, #1
}

\begin{document}
◊(select* 'root doc)
\end{document}
