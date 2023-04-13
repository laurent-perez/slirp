#! /usr/bin/env isis-script

_auto_declare = 1;

private define legend(y, label) % {{{
{
   %variable x = log10([100000, 300000]);
   variable x = log10([10000, 30000]);
   _pgline(x, [y, y]);
   _pgptxt(x[-1] + 0.2, y, 0, 0, "\\fr " + label);
} % }}}

if (__argc != 2)
   usage("%s timing_file", path_basename(__argv[0]));

file = __argv[1];
stem = path_sans_extname( path_basename(file));
ps = stem+".ps";

tab = readtab(__argv[1]);
x =  tab.col1;

sizes  = Array_Type[4];
sizes[*] = x;

ratios = Array_Type[4];			% Speedup Ratios: sequential / parallel
ratios[0] = tab.col2 / tab.col3;
ratios[1] = tab.col4 / tab.col5;
ratios[2] = tab.col6 / tab.col7;
ratios[3] = tab.col12 / tab.col13;

ymax = ceil( max ([ratios[0], ratios[1], ratios[2], ratios[3]]) );
yrange(0.5, ymax);
xrange(min(x)/2, max(x));
line_y = log10(ymax) * log10( [.84, 1.0, 1.16, 1.32])/ log10(5);

% Find inflection points (where speed of parallel begins to overtake sequential)
infl = [where(ratios[0] >= 1.0)[0],
	where(ratios[1] >= 1.0)[0],
	where(ratios[2] >= 1.0)[0],
	where(ratios[3] >= 1.0)[0]];

% Which is last (i.e. denoting the slowest parallel speedup) ?
infl = max(infl);
infl_r = [ ratios[0][infl], ratios[1][infl], ratios[2][infl], ratios[3][infl]];
infl_r = where(infl_r == min(infl_r))[-1];
ratio = ratios[infl_r];

% Interpolate to Y = 1 so we can explicitly mark this inflection point
slope = (ratio[infl] - ratio[infl-1]) / (x[infl] - x[infl-1]);
infl_value =  (1 - ratio[infl]) / slope  + x[infl];
if (infl_value < x[0]) infl_value = x[0];

% Insert slowest inflection point into vectors for this curve
ratios[infl_r] = [ ratio[[0:infl-1]], 1, ratio[[infl:]] ];
sizes[infl_r] = [x[[0:infl-1]], infl_value, x[[infl:]]];

xlog; ylog;

() = open_plot(ps + "/cps");

set_line_width(3);
set_frame_line_width(3);
charsize(1.5);

#ifexists set_outer_viewport
   variable dims = struct {xmin, xmax, ymin, ymax};
   dims.xmin = 0.15; dims.xmax = 0.85; dims.ymin = 0.15; dims.ymax = 0.85;
   set_outer_viewport(dims);
#else
   _pgsvp(0.15, 0.85, 0.15, 0.85);
#endif

label(	"\\fr Array Size",
	"\\fr S-Lang Intrinsic / SLIRP Openmp",
%	"\\fr Worst Case Parallel Speedup: realized for length(array) >= " +
%				string(ceil(infl_value)));
	"\\fr " %+ stem
	);

% Draw symbol & line denoting slowest inflection point
pointstyle(-4); linestyle(4);
plot([infl_value, infl_value], [0,1]);
vmessage("Worst Case Parallel Speedup: realized for length(array) >= "+
						string(int(ceil(infl_value))));

pointstyle(-1);
linestyle(1); color(2); 	% solid red
oplot(sizes[0], ratios[0]);
legend(line_y[0], "sin");

linestyle(4); color(1); 	% dotted black
oplot(sizes[1], ratios[1]);
legend(line_y[1], "cos");

linestyle(2); color(4);		% dashed blue
oplot(sizes[2], ratios[2]);
legend(line_y[2], "log");

linestyle(3); color(3);		% dot-dash green
oplot(sizes[3], ratios[3]);
legend(line_y[3], "cos(x) + sin(x)");

vmessage("Postscript generated to "+ps);
