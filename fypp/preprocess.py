#!/usr/bin/python
import fypp, tomli, os, time, argparse

def set_fypp_options( path_to_manifest='./' ):
    options = fypp.FyppOptions()
    options.indentation = 2
    options.file_var_root = path_to_manifest
    options.includes = [os.path.join( path_to_manifest, './fypp' )]
    options.modules = ['time']
    return options

def parse_manifest( filepath ):
    try:
        with open( filepath, mode='rb' ) as fp:
            tree = tomli.load( fp )
            ppinfo = tree['preprocess']['fypp']
    except:
        ppinfo = {}
    return ppinfo

def extract_files( dir, suffixes=['.fpp'] ):
    files = []
    for (dirpath, dirnames, filenames) in os.walk( dir ):
        selected_files = [filename if any([filename.endswith(suffix) for suffix in suffixes]) else '' for filename in filenames]
        files.extend( [(os.path.relpath(dirpath, start=dir), filename) for filename in filter(None, selected_files)] )
    return files

def convert_name_fypp_to_fortran( fypp_input_file ):
    return fypp_input_file.split('.f90.fpp')[0] + '_fypp.f90'

def convert_name_fortran_to_fypp( fortran_output_file ):
    return fortran_output_file.split('_fypp.f90')[0] + '.f90.fpp'

def get_fypp_header( filepath ):
    header = []
    try:
        with open( filepath, mode='r' ) as fp:
            line = 'a'
            while line:
                line = fp.readline()
                header.append( line )
                if line.startswith( '!END FYPP HEADER' ):
                    break
    except:
        header = []
    return header

def extract_from_header_original_file( header ):
    original_file = ''
    for line in header[::-1]:
        parts = line.strip().split('::')
        if len(parts) < 2:
            continue
        if any( [part.find('file') > 0 for part in parts[:-1]] ):
            original_file = parts[-1].strip()
    return original_file

def extract_from_header_time_preprocessed( header ):
    time_preprocessed = 0
    for line in header[::-1]:
        parts = line.strip().split('::')
        if len(parts) < 2:
            continue
        if any( [part.find('time') > 0 for part in parts[:-1]] ):
            time_preprocessed = time.mktime( time.strptime( parts[-1].strip(), '%Y-%m-%d %H:%M:%S UTC%z' ) )
    return time_preprocessed

def preprocess_file( input, output, preprocessor ):
    print( 'preprocessing '+input+' ...' )
    original_file = extract_from_header_original_file( get_fypp_header( output ) )
    if os.path.exists(output) and os.path.realpath(input) != os.path.realpath(original_file):
        print( '    Warning: Outputfile \''+output+'\' already exists but was created from a different input file \''+original_file+'\'.' )
    try:
        preprocessor.process_file( input, output )
    except fypp.FyppFatalError as e:
        print( '    Error: Fyp returned the following error:' )
        print( '        Mesage:  ' + e.msg ) 
        print( '        File:    ' + e.fname )
        print( '        Line(s): ' + str(e.span) )
    print( '    ... done' )

def main():
    arg_parser = argparse.ArgumentParser( description='Preprocess Fortran code using fpm and fypp.' )
    arg_parser.add_argument( '--manifest', '-m', help='Path to fpm manifest fpm.toml' )
    args = arg_parser.parse_args()
    path_to_manifest = os.path.dirname( os.path.relpath( os.path.realpath(args.manifest), start='./' ) )
    if not os.path.exists( args.manifest ):
        print( 'Error: Manifest not found.' )
        return
    ppinfo = parse_manifest( args.manifest )
    options = set_fypp_options( path_to_manifest )
    preprocessor = fypp.Fypp( options )
    for dirname in ppinfo['directories']:
        dirpath = os.path.join( path_to_manifest, dirname )
        files = extract_files( dirpath, ppinfo['suffixes'] )
        for (dirname, filename) in files:
            input = os.path.join( dirpath, dirname, filename )
            output = os.path.join( dirpath, dirname, convert_name_fypp_to_fortran(filename) )
            time_fypp_modified = os.path.getmtime( input )
            time_fortran_generated = extract_from_header_time_preprocessed( get_fypp_header( output ) )
            if time_fypp_modified > time_fortran_generated:
                preprocess_file( input, output, preprocessor )
    
if __name__ == "__main__":
    main()
