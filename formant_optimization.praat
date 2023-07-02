# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# formant_optimization.praat
# created by Christopher Carignan
# Department of Speech, Hearing and Phonetic Sciences
# University College London
# 30 September, 2022
#
# Updates
# 26 January, 2023:  changed estimation of the point of measurement stability
#
#
# modified by Justin J. H. Lo
# 2 July, 2023:
# to work in conjunction with a TextGrid, extracting and optimizing formants
# only in non-empty intervals of a selected tier instead of whole sound file
# (output Table now contains interval labels in first column)
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

### IMPORTANT ###
### Select a Sound object and a TextGrid in the Praat window before running script
###

# parameters
ceil_lo = 3500
ceil_hi = 6000
timestep = 0.005
tier_extract = 1

fullsound$ = selected$("Sound")
tgname$ = selected$("TextGrid")

select TextGrid 'tgname$'
n_intervals = Get number of intervals... tier_extract
n_nonempty = 0

for i_int from 1 to n_intervals
	select TextGrid 'tgname$'
	curr_label$ = Get label of interval... tier_extract i_int

	# only operate on non-empty intervals
	if curr_label$ <> ""
		n_nonempty = n_nonempty + 1

		int_start = Get start time of interval... tier_extract i_int
		int_end = Get end time of interval... tier_extract i_int

		# extract interval and optimize formants on extracted sound
		select Sound 'fullsound$'
		Extract part: int_start - 0.025, int_end + 0.025, "rectangular", 1, 1
		Rename... 'fullsound$'_'i_int'

		table_names$[n_nonempty] = "'fullsound$'_'i_int'_formants"

		@formantOptimizeLabel: "'fullsound$'_'i_int'", curr_label$, ceil_lo, ceil_hi, timestep
		
		# clean up
		select Sound 'fullsound$'_'i_int'
		Remove
	endif
endfor

# combine results
if n_nonempty > 0
	select Table 'table_names$[1]'
	if n_nonempty > 1
		for i from 2 to n_nonempty
			curr_tname$ = table_names$['i']
			plus Table 'curr_tname$'
		endfor
	endif
	Append
	Rename... 'fullsound$'_formants

	# clean up
		select Table 'table_names$[1]'
	if n_nonempty > 1
		for i from 2 to n_nonempty
			curr_tname$ = table_names$['i']
			plus Table 'curr_tname$'
		endfor
	endif
	Remove

	select Table 'fullsound$'_formants
endif

### core function definition ###
procedure formantOptimizeLabel: .filename$, .label$, .ceil_lo, .ceil_hi, .timestep
	@formantOptimize: .filename$, .ceil_lo, .ceil_hi, .timestep
	select Table '.filename$'_formants
	Insert column... 1 label
	.n_row = Get number of rows
	for i from 1 to .n_row
		Set string value... i label '.label$'
	endfor
endproc

procedure formantOptimize: .filename$, .ceil_lo, .ceil_hi, .timestep
	# create baseline formant object
	select Sound '.filename$'
	To Formant (burg)... .timestep 5 .ceil_lo 0.025 50
	Rename... '.filename$'_baseline

	# create baseline formant track matrices
	for i from 1 to 5
		select Formant '.filename$'_baseline
		To Matrix... i
		Rename... '.filename$'_f'i'
	endfor

	# iterate through F5 ceilings (ceil_lo Hz - ceil_high Hz) in steps of 50 Hz
	.steps = (.ceil_hi - .ceil_lo)/50

	for i from 1 to .steps
		# get current F5 ceiling (Hz)
		.step = i*50
		.ceiling = .ceil_lo + .step

		# create formant tracks with the current ceiling
		select Sound '.filename$'
		To Formant (burg)... .timestep 5 .ceiling 0.025 50

		# add formant measures to their respective matrices
		for j from 1 to 5
			select Formant '.filename$'
			To Matrix... j
			Rename... '.filename$'_f'j'_new

			plus Matrix '.filename$'_f'j'
			Merge (append rows)

			select Matrix '.filename$'_f'j'
			Remove

			select Matrix '.filename$'_f'j'_'.filename$'_f'j'_new
			Rename... '.filename$'_f'j'
		endfor

		# clean up
		select Formant '.filename$'
		for j from 1 to 5
			plus Matrix '.filename$'_f'j'_new
		endfor
		Remove
	endfor

	select Matrix '.filename$'_f1
	.points = Get number of columns
	.measures = Get number of rows

	Create Table with column names... '.filename$'_formants .points time f1 f2 f3 f4 f5

	# iterate through each time step
	for i from 1 to .points

		# estimate the point of measurement stability for each formant at the time step
		for j from 1 to 5

    	    # get all estimated values for this formant at this time step
			select Matrix '.filename$'_f'j'
			.f'j'# = Get all values in column... i

			# calculate the first difference (velocity) of the formant values
			.diff# = zero#(.measures-1)		
			for k from 1 to .measures-1
				.diff#[k] = abs(.f'j'#[k+1] - .f'j'#[k])
			endfor

			# find the maximum absolute difference
			# this is the point of divergence between (potentially) two stability points
			# (we want the second one, since the formant ceilings are increasing)
			.maxidx = imax(.diff#)

			# trim from the maximum index
			.ftrim# = zero#(1+.measures-.maxidx)
			.y = 1
			for k from .maxidx to .measures
				.ftrim#[.y] = .f'j'#[k]
				.y = .y+1
			endfor

			# calculate the first difference of the trimmed data
			.diff# = zero#(.measures-.maxidx)
			for k from 1 to .measures-.maxidx-1
				.diff#[k] = abs(.ftrim#[k+1] - .ftrim#[k])
			endfor
		
			# find the minimum absolute difference of the trimmed data 
			# this is the estimate of the measurement stability point
			.minidx = imax(0-.diff#)
		
			# get the formant value at the stability point
			.f'j' = .ftrim#[.minidx]
		endfor 

		select Formant '.filename$'_baseline
		.time = Get time from frame number... i

		# add time step and median formant values to table
		select Table '.filename$'_formants
		Set numeric value... i "time" .time
		for j from 1 to 5
			Set numeric value... i "f'j'" .f'j'
		endfor
	endfor

	# clean up
	select Formant '.filename$'_baseline
	for j from 1 to 5
		plus Matrix '.filename$'_f'j'
	endfor
	Remove
endproc
