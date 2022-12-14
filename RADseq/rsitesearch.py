#!/usr/bin/env python3
# -*- coding: utf-8 -*-

#-------------------------------------------------------------------------------

'''This software has been developed by:

       GI Genética, Fisiología e Historia Forestal
       Dpto. Sistemas y Recursos Naturales
       ETSI Montes, Forestal y del Medio Natural
       Universidad Politécnica de Madrid
       https://github.com/ggfhf/

   Licence: GNU General Public Licence Version 3
'''

#-------------------------------------------------------------------------------

'''This source contains the program of the ddRADseqTools software package that
   searches the restriction sites of a genome, does a double digestion and writes
   a file in FASTA format with the fragments gotten.
'''
#-------------------------------------------------------------------------------

import gzip
import re
import sys

#-------------------------------------------------------------------------------

def main(argv):
    '''Main line of the program.'''

    # build the options dictionary
    options_dict = build_options()

    # it has been requested the help or to build a new config file
    for param in argv:
        # show the help and exit OK
        if param.startswith('--help'):
            print_help(options_dict)
            sys.exit(0)
        # build the config file and exit OK
        elif param.startswith('--config'):
            build_config(options_dict)
            sys.exit(0)

    # get the config file
    config_file = get_config_file(__file__)

    # get options from the config file and the input parameters
    options_dict = get_options(options_dict, config_file, argv)

    # get the restriction site sequences
    rsfile = options_dict['rsfile']['value']
    enzyme1 = options_dict['enzyme1']['value']
    enzyme2 = options_dict['enzyme2']['value']
    (ressite1_seq, ressite1_lcut_seq, ressite1_rcut_seq, ressite2_seq, ressite2_lcut_seq, ressite2_rcut_seq) = get_ressites(rsfile, enzyme1, enzyme2)

    # if the sequences of the restriction sites are different
    if ressite1_seq.upper() != ressite2_seq.upper():

        # get the fragments doing a double digestion of the genome
        Message.print('info', 'The enzymes have different restriction site sequences, so the fragments will correspond to a double digestion.')
        do_double_digest(options_dict)

    # if the sequences of the restriction sites are equal
    else:

        # get the fragments doing a single digestion of the genome
        Message.print('info', 'The enzymes have equal restriction site sequences, so the fragments will correspond to a single digestion.')
        do_single_digest(options_dict)

#-------------------------------------------------------------------------------

def do_double_digest(options_dict):
    '''Do in silico a double digest of the genome.'''

    genfile = options_dict['genfile']['GCA_000699005.1_ASM69900v1_genomic.fna']
    fragsfile = options_dict['fragsfile']['value']
    rsfile = options_dict['rsfile']['value']
    enzyme1 = options_dict['enzyme1']['value']
    enzyme2 = options_dict['enzyme2']['value']
    minfragsize = options_dict['minfragsize']['value']
    maxfragsize = options_dict['maxfragsize']['value']
    fragstfile = options_dict['fragstfile']['value']
    fragstinterval = options_dict['fragstinterval']['value']
    plot = options_dict['plot']['value']
    verbose = options_dict['verbose']['value']
    trace = options_dict['trace']['value']

    # set the verbose and trace status
    if verbose.upper() == 'YES':
        Message.set_verbose_status(True)
    else:
        Message.set_verbose_status(False)
    if trace.upper() == 'YES':
        Message.set_trace_status(True)
    else:
        Message.set_trace_status(False)

    # get the restriction site sequences
    (ressite1_seq, ressite1_lcut_seq, ressite1_rcut_seq, ressite2_seq, ressite2_lcut_seq, ressite2_rcut_seq) = get_ressites(rsfile, enzyme1, enzyme2)
    Message.print('trace', 'ressite1_seq: {0} - ressite1_lcut_seq: {1} - ressite1_rcut_seq: {2}'.format(ressite1_seq, ressite1_lcut_seq, ressite1_rcut_seq))
    Message.print('trace', 'ressite2_seq: {0} - ressite2_lcut_seq: {1} - ressite2_rcut_seq: {2}'.format(ressite2_seq, ressite2_lcut_seq, ressite2_rcut_seq))

    # get the restriction overhangs
    if len(ressite1_lcut_seq) >= len(ressite1_rcut_seq):
        resoverhang1_seq = get_reverse_complementary_sequence(ressite1_lcut_seq)
    else:
        resoverhang1_seq = ressite1_rcut_seq
    if len(ressite2_lcut_seq) >= len(ressite2_rcut_seq):
        resoverhang2_seq = ressite1_lcut_seq
    else:
        resoverhang2_seq = get_reverse_complementary_sequence(ressite2_rcut_seq)
    Message.print('trace', 'resoverhang1_seq: {0}'.format(resoverhang1_seq))
    Message.print('trace', 'resoverhang2_seq: {0}'.format(resoverhang2_seq))

    # get the list of sequences corresponding to each enzyme
    unambiguous_ressite1_seq_list = get_unambiguous_sequence_list(ressite1_seq.upper())
    unambiguous_ressite2_seq_list = get_unambiguous_sequence_list(ressite2_seq.upper())
    Message.print('trace', 'unambiguous_ressite1_seq_list: {0}'.format(unambiguous_ressite1_seq_list))
    Message.print('trace', 'unambiguous_ressite2_seq_list: {0}'.format(unambiguous_ressite2_seq_list))

    # open the genome file
    try:
        if genfile.endswith('.gz'):
            genfile_id = gzip.open(genfile, mode='rt', encoding='iso-8859-1')
        else:
            genfile_id = open(genfile, mode='r', encoding='iso-8859-1')
    except:
        raise ProgramError('F002', genfile)

    # open the fragments file
    try:
        fragsfile_id = open(fragsfile, mode='w', encoding='iso-8859-1')
    except:
        raise ProgramError('F002', fragsfile)

    # set the pattern of the head records (>locus_info)
    pattern = r'^>(.*)$'

    # initialize the count of the total fragments and written fragments
    total_fragments_count = 0
    written_fragments_count = 0

    # initialize the intervals
    intervals_dict = {}

    # initialize the GC distribution
    GC_distribution_dict = {}
 
    # read the first record
    record = genfile_id.readline()

    # while there are records
    while record != '':

        # process the head record 
        if record.startswith('>'):

            # extract the data 
            mo = re.search(pattern, record)
            locus_info = mo.group(1)

            # initialize the locus sequence of Watson strand
            watson_locus_seq = ''

            # read the next record
            record = genfile_id.readline()

        else:

            # control the FASTA format
            raise ProgramError('F003', genfile, 'FASTA')

        # while there are records and they are sequence
        while record != '' and not record.startswith('>'):

            # concatenate the record to the locus sequence of Watson strand
            watson_locus_seq += record.strip().upper()

            # read the next record
            record = genfile_id.readline()

        Message.print('trace', 'watson_locus_seq: {0}'.format(watson_locus_seq))

        # initialize the list of the restriction sites found of the first enzyme in the Watson strand
        ressite1_positions_list = []

        # build the list position found corresponding to first enzyme in the Watson strand
        for unambigous_ressite1_seq in unambiguous_ressite1_seq_list:
            Message.print('trace', 'unambigous_ressite1_seq: {0}'.format(unambigous_ressite1_seq))
            for m in re.finditer(r'(?=({0}))'.format(unambigous_ressite1_seq), watson_locus_seq):
                ressite1_positions_list.append(m.start())

        # sort the list position found corresponding to first enzyme in the Watson strand
        ressite1_positions_list.sort()
        Message.print('trace', 'ressite1_positions_list: {0}'.format(ressite1_positions_list))

        # for each restriction site of the first enzyme in the Watson strand, verify if there is a cut with the second enzyme
        for i in range(len(ressite1_positions_list)):

            # initialize the list of restriction site of the second enzyme from the restriction site of the first enzyme in the Watson strand
            ressite2_positions_list = []
         
            # search the next restriction site of every unambiguous sequence of the second enzyme from the restriction site of the first enzyme in the Watson strand
            for unambigous_ressite2_seq in unambiguous_ressite2_seq_list:
                Message.print('trace', 'unambigous_ressite2_seq: {0}'.format(unambigous_ressite2_seq))
                unambigous_ressite2_position = watson_locus_seq.find(unambigous_ressite2_seq, (ressite1_positions_list[i] + len(ressite1_seq)))
                if unambigous_ressite2_position != -1:
                    ressite2_positions_list.append(unambigous_ressite2_position)

            # if any restriction site of the second enzyme is not found, exit of the while loop because there is not cut
            if ressite2_positions_list == []:
                break; 

            # calculate the next restriction site of every unambiguous sequence of the second enzyme from the restriction site of the first enzyme in the Watson strand
            ressite2_position = min(ressite2_positions_list)
            Message.print('trace', 'ressite1_positions_list[i]: {0} - ressite2_positions_list: {1} - ressite2_position: {2}'.format(ressite1_positions_list[i], ressite2_positions_list, ressite2_position))

            # if a restriction site of the second enzyme is found and this is previous to a restriction site of the first enzyme
            if i == (len(ressite1_positions_list) - 1) or ressite2_position < ressite1_positions_list[i + 1]:

                # add 1 to the count of total fragments
                total_fragments_count += 1

                # calculate the start and end positions of the fragment in the genome
                start_position = ressite1_positions_list[i] + len(ressite1_seq) - len(resoverhang1_seq)
                end_position = ressite2_position + len(resoverhang2_seq)

                # get the genome insert
                fragment_seq = watson_locus_seq[start_position:end_position].upper()

                # calculate the fragment length
                fragment_len = len(fragment_seq)

                # calculate the GC rate and the N count
                (GC_rate, N_count) = get_GC_N_data(fragment_seq)
                GC_rate_formatted = '{0:3.2f}'.format(GC_rate)

                # if the fragment length is between the lower and the upper loci fragments size
                if minfragsize <= fragment_len <= maxfragsize:

                    # add 1 to the count of fragments written
                    written_fragments_count += 1

                    # write the FASTA head and fragment in the fragments file
                    fragsfile_id.write('>fragment: {0:d} | length: {1:d} | GC: {2} | strand: {3} | start: {4:d} | end: {5:d} | locus: {6}\n'.format(written_fragments_count, fragment_len, GC_rate_formatted, '+', start_position + 1, end_position,  locus_info))
                    fragsfile_id.write('{0}\n'.format(fragment_seq))
                    Message.print('trace', 'fragment_seq: {0}'.format(fragment_seq))

                    # notify the reads have been written
                    Message.print('verbose', '\rFragments written ... {0:9d}'.format(written_fragments_count))

                    # update the GC distribution
                    GC_distribution_dict[GC_rate_formatted] = GC_distribution_dict.get(GC_rate_formatted, 0) + 1

                # update the intervals with the fragment length
                intervals_dict = update_fragments_intervals(intervals_dict, fragstinterval, fragment_len, N_count)

        # get the sequence of the Crick strand
        crick_locus_seq = get_reverse_complementary_sequence(watson_locus_seq)
        Message.print('trace', 'crick_locus_seq: {0}'.format(crick_locus_seq))

        # initialize the list of the restriction sites found of the first enzyme in the Crick strand
        ressite1_positions_list = []

        # build the list position found corresponding to first enzyme in the Crick strand
        for unambigous_ressite1_seq in unambiguous_ressite1_seq_list:
            Message.print('trace', 'unambigous_ressite1_seq: {0}'.format(unambigous_ressite1_seq))
            for m in re.finditer(r'(?=({0}))'.format(unambigous_ressite1_seq), crick_locus_seq):
                ressite1_positions_list.append(m.start())

        # sort the list position found corresponding to first enzyme in the Crick strand
        ressite1_positions_list.sort()
        Message.print('trace', 'ressite1_positions_list: {0}'.format(ressite1_positions_list))

        # for each restriction site of the first enzyme in the Crick strand, verify if there is a cut with the second enzyme
        for i in range(len(ressite1_positions_list)):

            # initialize the list of restriction site of the second enzyme from the restriction site of the first enzyme in the Crick strand
            ressite2_positions_list = []
         
            # search the next restriction site of every unambiguous sequence of the second enzyme from the restriction site of the first enzyme in the Crick strand
            for unambigous_ressite2_seq in unambiguous_ressite2_seq_list:
                Message.print('trace', 'unambigous_ressite2_seq: {0}'.format(unambigous_ressite2_seq))
                unambigous_ressite2_position = crick_locus_seq.find(unambigous_ressite2_seq, (ressite1_positions_list[i] + len(ressite1_seq)))
                if unambigous_ressite2_position != -1:
                    ressite2_positions_list.append(unambigous_ressite2_position)

            # if any restriction site of the second enzyme is not found, exit of the while loop because there is not cut
            if ressite2_positions_list == []:
                break; 

            # calculate the next restriction site of every unambiguous sequence of the second enzyme from the restriction site of the first enzyme in the Crick strand
            ressite2_position = min(ressite2_positions_list)
            Message.print('trace', 'ressite1_positions_list[i]: {0} - ressite2_positions_list: {1} - ressite2_position: {2}'.format(ressite1_positions_list[i], ressite2_positions_list, ressite2_position))

            # if a restriction site of the second enzyme is found and this is previous to a restriction site of the first enzyme
            if i == (len(ressite1_positions_list) - 1) or ressite2_position < ressite1_positions_list[i + 1]:

                # add 1 to the count of total fragments
                total_fragments_count += 1

                # calculate the start and end positions of the fragment in the genome
                start_position = ressite1_positions_list[i] + len(ressite1_seq) - len(resoverhang1_seq)
                end_position = ressite2_position + len(resoverhang2_seq)

                # get the genome insert
                fragment_seq = crick_locus_seq[start_position:end_position].upper()

                # calculate the fragment length
                fragment_len = len(fragment_seq)

                # calculate the GC rate and the N count
                (GC_rate, N_count) = get_GC_N_data(fragment_seq)
                GC_rate_formatted = '{0:3.2f}'.format(GC_rate)

                # if the fragment length is between the lower and the upper loci fragments size
                if minfragsize <= fragment_len <= maxfragsize:

                    # add 1 to the count of fragments written
                    written_fragments_count += 1

                    # write the FASTA head and fragment in the fragments file
                    fragsfile_id.write('>fragment: {0:d} | length: {1:d} | GC: {2} | strand: {3} | start: {4:d} | end: {5:d} | locus: {6}\n'.format(written_fragments_count, fragment_len, GC_rate_formatted, '-', (len(crick_locus_seq) - start_position), (len(crick_locus_seq) - end_position + 1),  locus_info))
                    fragsfile_id.write('{0}\n'.format(fragment_seq))
                    Message.print('trace', 'fragment_seq: {0}'.format(fragment_seq))

                    # notify the reads have been written
                    Message.print('verbose', '\rFragments written ... {0:9d}'.format(written_fragments_count))

                    # update the GC distribution
                    GC_distribution_dict[GC_rate_formatted] = GC_distribution_dict.get(GC_rate_formatted, 0) + 1

                # update the intervals with the fragment length
                intervals_dict = update_fragments_intervals(intervals_dict, fragstinterval, fragment_len, N_count)

    # close files
    genfile_id.close()
    fragsfile_id.close()

    # show OK message 
    Message.print('verbose', '\n')
    Message.print('info', 'The file {0} containing the fragments of the double digest of the genome is created.'.format(get_file_name(fragsfile)))

    # write the statistics and save them in the statistics file
    title = 'Distribution of fragments after a double digest with {0} and {1}'.format(enzyme1, enzyme2)
    write_fragments_stats(fragstfile, intervals_dict, total_fragments_count, written_fragments_count, minfragsize, maxfragsize, title)
    if plot.upper() == 'YES':
        plot_fragments_graphic(fragstfile, intervals_dict, title)

    # write the GC distribution file
    write_GC_distribution(fragsfile, GC_distribution_dict)

#-------------------------------------------------------------------------------

def do_single_digest(options_dict):
    '''Do in silico a single digest of the genome.'''

    genfile = options_dict['genfile']['value']
    fragsfile = options_dict['fragsfile']['value']
    rsfile = options_dict['rsfile']['value']
    enzyme1 = options_dict['enzyme1']['value']
    enzyme2 = options_dict['enzyme2']['value']
    minfragsize = options_dict['minfragsize']['value']
    maxfragsize = options_dict['maxfragsize']['value']
    fragstfile = options_dict['fragstfile']['value']
    fragstinterval = options_dict['fragstinterval']['value']
    plot = options_dict['plot']['value']
    verbose = options_dict['verbose']['value']
    trace = options_dict['trace']['value']

    # set the verbose and trace status
    if verbose.upper() == 'YES':
        Message.set_verbose_status(True)
    else:
        Message.set_verbose_status(False)
    if trace.upper() == 'YES':
        Message.set_trace_status(True)
    else:
        Message.set_trace_status(False)

    # get the restriction site sequences
    (ressite1_seq, ressite1_lcut_seq, ressite1_rcut_seq, ressite2_seq, ressite2_lcut_seq, ressite2_rcut_seq) = get_ressites(rsfile, enzyme1, enzyme2)
    Message.print('trace', 'ressite1_seq: {0} - ressite1_lcut_seq: {1} - ressite1_rcut_seq: {2}'.format(ressite1_seq, ressite1_lcut_seq, ressite1_rcut_seq))

    # get the restriction overhangs (the restriction overhang sequences in both ends can be different)
    if len(ressite1_lcut_seq) >= len(ressite1_rcut_seq):
        resoverhang1_seq = get_reverse_complementary_sequence(ressite1_lcut_seq)
    else:
        resoverhang1_seq = ressite1_rcut_seq
    if len(ressite2_lcut_seq) >= len(ressite2_rcut_seq):
        resoverhang2_seq = ressite1_lcut_seq
    else:
        resoverhang2_seq = get_reverse_complementary_sequence(ressite2_rcut_seq)
    Message.print('trace', 'resoverhang1_seq: {0}'.format(resoverhang1_seq))
    Message.print('trace', 'resoverhang2_seq: {0}'.format(resoverhang2_seq))

    # get the list of sequences corresponding to each enzyme
    unambiguous_ressite1_seq_list = get_unambiguous_sequence_list(ressite1_seq.upper())
    Message.print('trace', 'unambiguous_ressite1_seq_list: {0}'.format(unambiguous_ressite1_seq_list))

    # open the genome file
    try:
        if genfile.endswith('.gz'):
            genfile_id = gzip.open(genfile, mode='rt', encoding='iso-8859-1')
        else:
            genfile_id = open(genfile, mode='r', encoding='iso-8859-1')
    except:
        raise ProgramError('F002', genfile)

    # open the fragments file
    try:
        fragsfile_id = open(fragsfile, mode='w', encoding='iso-8859-1')
    except:
        raise ProgramError('F002', fragsfile)

    # set the pattern of the head records (>locus_info)
    pattern = r'^>(.*)$'

    # initialize the count of the total fragments and written fragments
    total_fragments_count = 0
    written_fragments_count = 0

    # initialize the intervals
    intervals_dict = {}

    # initialize the GC distribution
    GC_distribution_dict = {}
 
    # read the first record
    record = genfile_id.readline()

    # while there are records
    while record != '':

        # process the head record 
        if record.startswith('>'):

            # extract the data 
            mo = re.search(pattern, record)
            locus_info = mo.group(1)

            # initialize the locus sequence of Watson strand
            watson_locus_seq = ''

            # read the next record
            record = genfile_id.readline()

        else:

            # control the FASTA format
            raise ProgramError('F003', genfile, 'FASTA')

        # while there are records and they are sequence
        while record != '' and not record.startswith('>'):

            # concatenate the record to the locus sequence of Watson strand
            watson_locus_seq += record.strip().upper()

            # read the next record
            record = genfile_id.readline()

        Message.print('trace', 'watson_locus_seq: {0}'.format(watson_locus_seq))

        # initialize the list of position corresponding to the restriction sites in the Watson strand
        ressite1_positions_list = []

        # build the list of position corresponding to the restriction sites in the Watson strand
        for unambigous_ressite1_seq in unambiguous_ressite1_seq_list:
            Message.print('trace', 'unambigous_ressite1_seq: {0}'.format(unambigous_ressite1_seq))
            for m in re.finditer(r'(?=({0}))'.format(unambigous_ressite1_seq), watson_locus_seq):
                ressite1_positions_list.append(m.start())

        # sort the list of position corresponding to the restriction sites in the Watson strand
        ressite1_positions_list.sort()
        Message.print('trace', 'ressite1_positions_list: {0}'.format(ressite1_positions_list))

        # inicialize the last position processed in the Watson strand 
        last_ressite1_position = 0

        # inicialize the indicator of first cut with True
        is_first_cut = True

        # for each restriction site in the Watson strand
        for ressite1_position in ressite1_positions_list:

            # add 1 to the count of total fragments
            total_fragments_count += 1

            # calculate the start and end positions of the fragment in the genome
            if is_first_cut == True:
                start_position = 0
            else:
                start_position = last_ressite1_position + len(ressite1_seq) - len(resoverhang1_seq)
            end_position = ressite1_position + len(resoverhang2_seq)

            # set the indicator of first cut to False
            is_first_cut = False

            # get the genome insert
            fragment_seq = watson_locus_seq[start_position:end_position].upper()

            # calculate the fragment length
            fragment_len = len(fragment_seq)

            # calculate the GC rate and the N count
            (GC_rate, N_count) = get_GC_N_data(fragment_seq)
            GC_rate_formatted = '{0:3.2f}'.format(GC_rate)

            # if the fragment length is between the lower and the upper loci fragments size
            if minfragsize <= fragment_len <= maxfragsize:

                # add 1 to the count of fragments written
                written_fragments_count += 1

                # write the FASTA head and fragment in the fragments file
                fragsfile_id.write('>fragment: {0:d} | length: {1:d} | GC: {2} | strand: {3} | start: {4:d} | end: {5:d} | locus: {6}\n'.format(written_fragments_count, fragment_len, GC_rate_formatted, '+', start_position + 1, end_position,  locus_info))
                fragsfile_id.write('{0}\n'.format(fragment_seq))
                Message.print('trace', 'fragment_seq: {0}'.format(fragment_seq))

                # notify the reads have been written
                Message.print('verbose', '\rFragments written ... {0:9d}'.format(written_fragments_count))

                # update the GC distribution
                GC_distribution_dict[GC_rate_formatted] = GC_distribution_dict.get(GC_rate_formatted, 0) + 1

            # update the intervals with the fragment length
            intervals_dict = update_fragments_intervals(intervals_dict, fragstinterval, fragment_len, N_count)

            # save the last position processed in the Watson strand 
            last_ressite1_position = ressite1_position

        # if there are nucleotides after the  last restriction site in the Watson strand
        if last_ressite1_position < len(watson_locus_seq):

            # add 1 to the count of total fragments
            total_fragments_count += 1

            # calculate the start and end positions of the fragment in the genome
            #start_position = last_ressite1_position
            #end_position = len(watson_locus_seq)
            start_position = last_ressite1_position + len(ressite1_seq) - len(resoverhang1_seq)
            end_position = len(watson_locus_seq)

            # get the genome insert
            fragment_seq = watson_locus_seq[start_position:end_position].upper()

            # calculate the fragment length
            fragment_len = len(fragment_seq)

            # calculate the GC rate and the N count
            (GC_rate, N_count) = get_GC_N_data(fragment_seq)
            GC_rate_formatted = '{0:3.2f}'.format(GC_rate)

            # if the fragment length is between the lower and the upper loci fragments size
            if minfragsize <= fragment_len <= maxfragsize:

                # add 1 to the count of fragments written
                written_fragments_count += 1

                # write the FASTA head and fragment in the fragments file
                fragsfile_id.write('>fragment: {0:d} | length: {1:d} | GC: {2} | strand: {3} | start: {4:d} | end: {5:d} | locus: {6}\n'.format(written_fragments_count, fragment_len, GC_rate_formatted, '+', start_position + 1, end_position,  locus_info))
                fragsfile_id.write('{0}\n'.format(fragment_seq))
                Message.print('trace', 'fragment_seq: {0}'.format(fragment_seq))

                # notify the reads have been written
                Message.print('verbose', '\rFragments written ... {0:9d}'.format(written_fragments_count))

                # update the GC distribution
                GC_distribution_dict[GC_rate_formatted] = GC_distribution_dict.get(GC_rate_formatted, 0) + 1

            # update the intervals with the fragment length
            intervals_dict = update_fragments_intervals(intervals_dict, fragstinterval, fragment_len, N_count)

    # close files
    genfile_id.close()
    fragsfile_id.close()

    # show OK message 
    Message.print('verbose', '\n')
    Message.print('info', 'The file {0} containing the fragments of the single digest of the genome is created.'.format(get_file_name(fragsfile)))

    # write the statistics and save them in the statistics file
    title = 'Distribution of fragments after a single digest with {0}'.format(enzyme1)
    write_fragments_stats(fragstfile, intervals_dict, total_fragments_count, written_fragments_count, minfragsize, maxfragsize, title)
    if plot.upper() == 'YES':
        plot_fragments_graphic(fragstfile, intervals_dict, title)

    # write the GC distribution file
    write_GC_distribution(fragsfile, GC_distribution_dict)

#-------------------------------------------------------------------------------

def build_options():
    '''Build a dictionary with the program options.'''

    # get all options dictionary
    all_options_dict = get_all_options_dict()

    # define the options dictionary
    options_dict = {
        'genfile': all_options_dict['genfile'],
        'fragsfile': all_options_dict['fragsfile'],
        'rsfile': all_options_dict['rsfile'],
        'enzyme1': all_options_dict['enzyme1'],
        'enzyme2': all_options_dict['enzyme2'],
        'minfragsize': all_options_dict['minfragsize'],
        'maxfragsize': all_options_dict['maxfragsize'],
        'fragstfile': all_options_dict['fragstfile'],
        'fragstinterval': all_options_dict['fragstinterval'],
        'plot': all_options_dict['plot'],
        'verbose': all_options_dict['verbose'],
        'trace': all_options_dict['trace']
    }

    # return the options dictionary
    return options_dict

#-------------------------------------------------------------------------------

def print_help(options_dict):
    '''Print the program help.'''

    # get general data
    project_name = get_project_name()
    project_version = get_project_version()
    program_file = get_file_name(__file__)
    config_file = get_config_file(__file__)

    # print the help
    Message.print('info', '')
    Message.print('info', '{0} version {1}'.format(project_name, project_version))
    Message.print('info', '')
    Message.print('info', '{0} searches the restriction sites of a genome, do a double digestion and write a file in FASTA format with the fragments gotten.'.format(program_file))
    Message.print('info', '')
    Message.print('info', 'Usage: {0} --help'.format(program_file))
    Message.print('info', '')
    Message.print('info', '       Show the help of {0}.'.format(program_file))
    Message.print('info', '')
    Message.print('info', '   or: {0} --config'.format(program_file))
    Message.print('info', '')
    Message.print('info', '       Create the config file {0} with the default value of the options.'.format(config_file))
    Message.print('info', '       The default value of the options can be modified.'.format(config_file))
    Message.print('info', '')
    Message.print('info', '   or: {0} [--option=<value> [--option=<value>, ...]]'.format(program_file))
    Message.print('info', '')
    Message.print('info', '       The options values are read from the config file {0}, but they can be modified'.format(config_file))
    Message.print('info', '       in command line. The options are:')
    Message.print('info', '')
    Message.print('info', '       {0:16}   {1}'.format('option', 'value'))
    Message.print('info', '       {0:16}   {1}'.format('=' * 16, '=' * 78))
    Message.print('info', '       {0:16}   {1}'.format('--genfile', options_dict['genfile']['comment']))
    Message.print('info', '       {0:16}   {1}'.format('--fragsfile', options_dict['fragsfile']['comment']))
    Message.print('info', '       {0:16}   {1}'.format('--rsfile', options_dict['rsfile']['comment']))
    Message.print('info', '       {0:16}   {1}'.format('--enzyme1', options_dict['enzyme1']['comment']))
    Message.print('info', '       {0:16}   {1}'.format('--enzyme2', options_dict['enzyme2']['comment']))
    Message.print('info', '       {0:16}   {1}'.format('--minfragsize', options_dict['minfragsize']['comment']))
    Message.print('info', '       {0:16}   {1}'.format('--maxfragsize', options_dict['maxfragsize']['comment']))
    Message.print('info', '       {0:16}   {1}'.format('--fragstfile', options_dict['fragstfile']['comment']))
    Message.print('info', '       {0:16}   {1}'.format('--fragstinterval', options_dict['fragstinterval']['comment']))
    Message.print('info', '       {0:16}   {1}'.format('--plot', options_dict['plot']['comment']))
    Message.print('info', '       {0:16}   {1}'.format('--verbose', options_dict['verbose']['comment']))
    Message.print('info', '       {0:16}   {1}'.format('--trace', options_dict['trace']['comment']))

#-------------------------------------------------------------------------------

def build_config(options_dict):
    '''Build the file with the options by default.'''

    # get the config file
    config_file = get_config_file(__file__)

    # create the config file and write the default options
    try:
        with open(config_file, mode='w', encoding='iso-8859-1') as config_file_id:
            config_file_id.write('{0:43} # {1}\n'.format('genfile' + '=' + options_dict['genfile']['default'], options_dict['genfile']['comment']))
            config_file_id.write('{0:43} # {1}\n'.format('fragsfile' + '=' + options_dict['fragsfile']['default'], options_dict['fragsfile']['comment']))
            config_file_id.write('{0:43} # {1}\n'.format('rsfile' + '=' + options_dict['rsfile']['default'], options_dict['rsfile']['comment']))
            config_file_id.write('{0:43} # {1}\n'.format('enzyme1' + '=' + options_dict['enzyme1']['default'], options_dict['enzyme1']['comment']))
            config_file_id.write('{0:43} # {1}\n'.format('enzyme2' + '=' + options_dict['enzyme2']['default'], options_dict['enzyme2']['comment']))
            config_file_id.write('{0:43} # {1}\n'.format('minfragsize' + '=' + options_dict['minfragsize']['default'], options_dict['minfragsize']['comment']))
            config_file_id.write('{0:43} # {1}\n'.format('maxfragsize' + '=' + options_dict['maxfragsize']['default'], options_dict['maxfragsize']['comment']))
            config_file_id.write('{0:43} # {1}\n'.format('fragstfile' + '=' + options_dict['fragstfile']['default'], options_dict['fragstfile']['comment']))
            config_file_id.write('{0:43} # {1}\n'.format('fragstinterval' + '=' + options_dict['fragstinterval']['default'], options_dict['fragstinterval']['comment']))
            config_file_id.write('{0:43} # {1}\n'.format('plot' + '=' + options_dict['plot']['default'], options_dict['plot']['comment']))
            config_file_id.write('{0:43} # {1}\n'.format('verbose' + '=' + options_dict['verbose']['default'], options_dict['verbose']['comment']))
            config_file_id.write('{0:43} # {1}\n'.format('trace' + '=' + options_dict['trace']['default'], options_dict['trace']['comment']))
    except:
        raise ProgramError('F001', config_file)

    # show OK message 
    Message.print('info', 'The configuration file {0} is created.'.format(get_file_name(config_file)))

#-------------------------------------------------------------------------------

if __name__ == '__main__':
    main(sys.argv[1:])
    sys.exit(0)

#-------------------------------------------------------------------------------
