#!/usr/bin/python

import sys
import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl
import argparse
import re


mpl.rcParams.update({'font.size': 9})


re_str = {'depth' : 'Depths to Grid Block Tops[^K]+(K =\s+\d+\s+([0-9\.]+\s+)+)+',
	  'porosity' : 'Porosity, fraction[^K]+(K =\s+\d+\s+([0-9\.]+\s+)+)+',
	  'pressure' : 'RESERVOIR PRESSURE DISTRIBUTION[^K\)]+(K =\s+\d+\s+([0-9\.]+\s+)+)+',
	  'pressure_corr' : 'RESERVOIR PRESSURE DISTRIBUTION[^K]+CORRECTED[^K]+(K =\s+\d+\s+([0-9\.]+\s+)+)+',
	  'oilsat' : 'OIL SATURATION[^K]+(K =\s+\d+\s+([0-9\.]+\s+)+)+',
	  'gassat' : 'GAS SATURATION[^K]+(K =\s+\d+\s+([0-9\.]+\s+)+)+',
	  'pb' : 'BUBBLE POINT PRESSURE DISTRIBUTION[^K]+(K =\s+\d+\s+([0-9\.]+\s+)+)+',
	  'kro' : '\.+\s+Kro[^K]+(K =\s+\d+\s+([0-9\.]+\s+)+)+',
	  'krg' : '\.+\s+Krg[^K]+(K =\s+\d+\s+([0-9\.]+\s+)+)+',
	  'timestep' : 'Elapsed time.*?END OF SUMMARY REPORT',
	  'initialization' : 'INITIALIZATION DATA.*?END OF INITIALIZATION',
	  'layer' : '\s*(K =\s+\d+\s+([0-9\.]+\s+)+)',
	  'gridrow' : '\s*\d+\s+(?:\d+\.\d+\s+)+'}


re_dict = {k: re.compile(v, re.MULTILINE|re.S) for k,v in re_str.items()}
re_elapsed_time = re.compile('Elapsed time[^\d]*(\d+\.\d+)\s*')
re_getfloat = re.compile('\s*\d+\.?\d*', re.MULTILINE|re.S) 
			     

if __name__ == "__main__":


    sections = {}
    snames_dec = []
    
    ap = argparse.ArgumentParser(prog=sys.argv[0])
    ap.add_argument('--list-sections',
		    action='store_true',
		    help="list available grids and exit")
    ap.add_argument('-p',
		    action='store_true',		    
		    help="print section contents and exit")
    ap.add_argument('-s',
		    nargs=2,
		    type=int,
		    metavar='P',
		    help="output image size in inches (width height)",
		    default=[10, 5])
    ap.add_argument('-g', action='store_true', help="draw gridlines")
    #ap.add_argument('-w', action='store_true', help="show well symbols")
    #ap.add_argument('-wn', action='store_true', help="show well names")
    ap.add_argument('-cm',
		    nargs=1,
		    metavar='COLOR-MAP',
		    help="color-map name. See samples in the cmaps directory",
		    default=["hot"])    
    #ap.add_argument('-t', nargs=1, metavar='TITLE', help="figure title in quotes")
    ap.add_argument('FILE.OUT', help="boast 3 simulation output")
    ap.add_argument('section_name', nargs='?', help="boast 3 simulation output")
    ap.add_argument('IMAGE.PNG', nargs='?', help="output image", default=None)

    if len(sys.argv)<=1:
	ap.print_help()
	exit(-1)

    args = ap.parse_args()

    try:
	fd = open(getattr(args, 'FILE.OUT'), 'r')
    except IOError:
	print "Error: could not open %s for reading" % (getattr(args, 'FILE.OUT'),)
	exit(-1)

    sim = fd.read()
    fd.close()

    init = re_dict['initialization'].search(sim)
    if(not init):
	print "Parse error: unable to read initialization data"
	exit(-1)

    for sec in ['depth', 'porosity', 'pressure',
		'pressure_corr', 'oilsat', 'gassat', 'pb']:
	found = re_dict[sec].search(init.group())
	if(not found):
	    print "Parse error: unable to read initialization %s" %(sec,)
	    exit(-1)


	layers_found = re_dict['layer'].findall(found.group())

	for j, layer in enumerate(layers_found):
	    sname = sec + '_L' + str(j+1) + '_init'
	    sections[sname] = layer[0]
	    snames_dec += [(sec, j, -1, sname)]
	    

    timesteps = re_dict['timestep'].findall(sim)

    for i, timestep in enumerate(timesteps):

	timel = re_elapsed_time.search(timestep)
	if(not timel):
	    print "Parse error: unable to read elapsed time from timestep %s" %(i)
	    exit(-1)
	timel = float(timel.group(1))

	for sec in ['porosity', 'pressure', 'pressure_corr',
		    'oilsat', 'gassat', 'pb']:
	    found = re_dict[sec].search(timestep)
	    if(not found):
		print "Parse error: unable to read %s section in timestep %s" %(sec, i+1)
		exit(-1)

	    layers_found = re_dict['layer'].findall(found.group())

	    for j, layer in enumerate(layers_found):
		sname = sec + '_L' + str(j+1) + '_' + ("%.1f"%(timel)) + 'd'
		sections[sname] = layer[0]
		snames_dec += [(sec, j, timel, sname)]


    if(args.list_sections):

	print "Grid sections available in %s:" %(getattr(args, 'FILE.OUT'))

	ncols = 5
	snames = []
	for _, _, _, name in sorted(snames_dec):
	    snames += [name]

	if(len(snames)%ncols):
	    snames = snames + [""]*((len(snames)/ncols+1)*ncols-len(snames))

	snames = np.array(snames)
	snames = snames.reshape((ncols, len(snames)/ncols))
	snames = snames.T

	col_width = max(len(word) for row in snames for word in row) + 2
	for row in snames:
	    print "".join(word.ljust(col_width) for word in row)

	exit(0)



    if(sections.has_key(args.section_name)):
	grid_data = []

	sec = sections[args.section_name]

	gridrows = re_dict['gridrow'].findall(sec)
	if(not gridrows):
	    print "Parse error: unable to read any layers from section %s" % (args.section_name)
	    exit(-1)

	for i, row in enumerate(gridrows):
	    row = row.replace('\n','')
	    grid_data.append(map(float, re_getfloat.findall(row)[1:]))

    else:
	print "Error: section %s not found" % (args.section_name)
	exit(-1)

    if(args.p):
	print sec


    fig, ax = plt.subplots()
    cax = ax.imshow(grid_data,
		    cmap=plt.get_cmap(args.cm[0]),
		    interpolation='none')
    cbar = fig.colorbar(cax)
    xl = ax.get_xlim()
    yl = ax.get_ylim()

    if(args.g):
	for i in np.arange(xl[0], xl[1], 1):
	    ax.plot((i,i), (yl[0],yl[1]), color='black')

	for i in np.arange(yl[1], yl[0], 1):
	    ax.plot((xl[0], xl[1]), (i,i), color='black')

	
    ax.set_xlim(xl)
    ax.set_ylim(yl)

    if(not getattr(args, 'IMAGE.PNG')):
	plt.show()
    else:
	fig.set_size_inches(*args.s)
	fig.savefig(getattr(args, 'IMAGE.PNG'), dpi=200)

    
	
