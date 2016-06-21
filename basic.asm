; ==================================================================
; MikeOS -- The Mike Operating System kernel
; Copyright (C) 2006 - 2016 MikeOS Developers -- see doc/LICENSE.TXT
;
; BASIC CODE INTERPRETER (4.5)
; ==================================================================

; ------------------------------------------------------------------
; Token types

%DEFINE VARIABLE 1
%DEFINE STRING_VAR 2
%DEFINE NUMBER 3
%DEFINE STRING 4
%DEFINE QUOTE 5
%DEFINE SYMBOL 6
%DEFINE LABEL 7
%DEFINE CHAR_VAR 8

%DEFINE MAX_TOKEN_LEN 127
; ------------------------------------------------------------------
; The BASIC interpreter execution starts here -- a parameter string
; is passed in SI and copied into the first string, unless SI = 0

os_run_basic:
	mov word [orig_stack], sp		; Save stack pointer -- we might jump to the
						; error printing code and quit in the middle
						; some nested loops, and we want to preserve
						; the stack

	mov word [load_point], ax		; AX was passed as starting location of code

	mov word [prog], ax			; prog = pointer to current execution point in code

	add bx, ax				; We were passed the .BAS byte size in BX
	mov word [prog_end], bx			; Make note of program end point


	call clear_ram				; Clear variables etc. from previous run
						; of a BASIC program

	cmp si, 0				; Passed a parameter string?
	je mainloop

	mov di, string_vars			; If so, copy it into $1
	call set_string_variable



mainloop:
	mov si, [prog]
	
	cmp si, [prog_end]
	jae .finish

	mov byte [var_stack_depth], 0

;	mov cx, 20
;	call os_print_newline
;	mov si, [prog]
;
;.next:
;	lodsb
;
;	cmp al, 10
;	je .next
;
;	cmp al, 13
;	je .next
;
;	mov ah, 0x0E
;	mov bh, 0
;	int 0x10
;	loop .next

	call get_numeric_variable		; If it's a variable at the start of the line,
	jnc .numeric_sum			; this is an assign (eg "X = Y + 5")

	call get_string_variable		; Same for a string variable (eg $1)
	jnc .string_sum

	call get_token				; Get a token from the start of the line
	movzx ax, [token_type]

	cmp ax, STRING				; Is the type a string of characters?
	je .keyword				; If so, let's see if it's a keyword to process

	cmp ax, LABEL				; Don't need to do anything here - skip
	je mainloop

	mov si, err_unknown_statement		; Otherwise show an error and quit
	jmp error

.finish:
	jmp do_end

.numeric_sum:
	mov al, '='				; A sum must have an '=' symbol
	call check_symbol			; (e.g. A = B + 5)
	jc .error

	mov cx, 0
	call get_numeric_sum			; Process the sum on the line
	
	call set_numeric_variable		; Set the original variable

	jmp mainloop

.string_sum:
	mov di, si
	
	mov al, '='
	call check_symbol
	jc .error

	call get_string_sum			; Build a string on the line
	jmp mainloop
	
.error:
	mov si, err_syntax
	jmp error
	
.keyword:
	mov si, token				; Start trying to match commands

	mov di, alert_cmd
	call os_string_compare
	jc near do_alert

	mov di, askfile_cmd
	call os_string_compare
	jc near do_askfile

	mov di, break_cmd
	call os_string_compare
	jc near do_break

	mov di, case_cmd
	call os_string_compare
	jc near do_case

	mov di, call_cmd
	call os_string_compare
	jc near do_call

	mov di, cls_cmd
	call os_string_compare
	jc near do_cls

	mov di, cursor_cmd
	call os_string_compare
	jc near do_cursor

	mov di, curschar_cmd
	call os_string_compare
	jc near do_curschar

	mov di, curscol_cmd
	call os_string_compare
	jc near do_curscol

	mov di, curspos_cmd
	call os_string_compare
	jc near do_curspos
	
	mov di, delete_cmd
	call os_string_compare
	jc near do_delete
	
	mov di, do_cmd
	call os_string_compare
	jc near do_do

	mov di, end_cmd
	call os_string_compare
	jc near do_end

	mov di, endif_cmd
	call os_string_compare
	jc near do_endif
	
	mov di, else_cmd
	call os_string_compare
	jc near do_else

	mov di, files_cmd
	call os_string_compare
	jc near do_files

	mov di, for_cmd
	call os_string_compare
	jc near do_for

	mov di, getkey_cmd
	call os_string_compare
	jc near do_getkey

	mov di, gosub_cmd
	call os_string_compare
	jc near do_gosub

	mov di, goto_cmd
	call os_string_compare
	jc near do_goto

	mov di, if_cmd
	call os_string_compare
	jc near do_if

	mov di, include_cmd
	call os_string_compare
	jc near do_include

	mov di, ink_cmd
	call os_string_compare
	jc near do_ink

	mov di, input_cmd
	call os_string_compare
	jc near do_input
	
	mov di, len_cmd
	call os_string_compare
	jc near do_len

	mov di, listbox_cmd
	call os_string_compare
	jc near do_listbox

	mov di, load_cmd
	call os_string_compare
	jc near do_load

	mov di, loop_cmd
	call os_string_compare
	jc near do_loop

	mov di, move_cmd
	call os_string_compare
	jc near do_move

	mov di, next_cmd
	call os_string_compare
	jc near do_next

	mov di, number_cmd
	call os_string_compare
	jc near do_number

	mov di, page_cmd
	call os_string_compare
	jc near do_page

	mov di, pause_cmd
	call os_string_compare
	jc near do_pause

	mov di, peek_cmd
	call os_string_compare
	jc near do_peek

	mov di, peekint_cmd
	call os_string_compare
	jc near do_peekint
	
	mov di, poke_cmd
	call os_string_compare
	jc near do_poke
	
	mov di, pokeint_cmd
	call os_string_compare
	jc near do_pokeint

	mov di, port_cmd
	call os_string_compare
	jc near do_port

	mov di, print_cmd
	call os_string_compare
	jc near do_print

	mov di, rand_cmd
	call os_string_compare
	jc near do_rand

	mov di, read_cmd
	call os_string_compare
	jc near do_read

	mov di, rem_cmd
	call os_string_compare
	jc near do_rem

	mov di, rename_cmd
	call os_string_compare
	jc near do_rename

	mov di, return_cmd
	call os_string_compare
	jc near do_return

	mov di, save_cmd
	call os_string_compare
	jc near do_save

	mov di, serial_cmd
	call os_string_compare
	jc near do_serial

	mov di, size_cmd
	call os_string_compare
	jc near do_size

	mov di, sound_cmd
	call os_string_compare
	jc near do_sound
	
	mov di, string_cmd
	call os_string_compare
	jc near do_string

	mov di, waitkey_cmd
	call os_string_compare
	jc near do_waitkey

	mov si, err_cmd_unknown			; Command not found?
	jmp error


; ------------------------------------------------------------------
; CLEAR RAM

clear_ram:
	pusha
	mov al, 0

	mov di, variables
	mov cx, 52
	rep stosb

	mov di, for_variables
	mov cx, 52
	rep stosb

	mov di, for_start_points
	mov cx, 52
	rep stosb
	
	mov di, do_loop_store
	mov cx, 10
	rep stosb

	mov byte [gosub_depth], 0
	mov byte [loop_in], 0

	mov di, gosub_points
	mov cx, 20
	rep stosb

	mov di, string_vars
	mov cx, 1024
	rep stosb

	mov byte [ink_colour], 7		; White ink

	popa
	ret


; ==================================================================
; SPECIFIC COMMAND CODE STARTS HERE

; ------------------------------------------------------------------
; ALERT

do_alert:
	call save_cursor

	call get_string_parameter
	jc .error
	mov ax, si
	
.display_message:
	mov bx, 0				; Others are blank
	mov cx, 0
	mov dx, 0				; One-choice box
	call os_dialog_box
	
	call restore_cursor
	
	jmp mainloop

.error:
	mov si, err_syntax
	jmp error


;-------------------------------------------------------------------
; ASKFILE

do_askfile:
	call save_cursor
	
	call get_string_variable
	jc .error
	mov di, si

	call os_file_selector			; Present the selector
	jc .cancelled
	
	mov si, ax
	call set_string_variable
	jc .error

.finish:
	call restore_cursor
	jmp mainloop
	
.error:
	mov si, err_syntax
	jmp error

.cancelled:
	mov byte [si], 0
	jmp .finish



; ------------------------------------------------------------------
; BREAK

do_break:
	mov si, err_break
	jmp error


; ------------------------------------------------------------------
; CALL

do_call:
	call get_numeric_parameter
	jc .error

.execute_call:
	mov bx, 0
	mov cx, 0
	mov dx, 0
	mov di, 0
	mov si, 0

	call ax

	jmp mainloop

.error:
	mov si, err_syntax
	jmp error


; ------------------------------------------------------------------
; CASE

do_case:
	mov si, .keywords
	call choose_keyword
	jmp .error
	
.uppercase:
	call get_string_variable
	jc .error

	mov ax, si
	call os_string_uppercase
	jmp mainloop
	
.lowercase:
	call get_string_variable	
	jc .error

	mov ax, si
	call os_string_lowercase
	jmp mainloop
	
.error:
	mov si, err_syntax
	jmp error

.keywords:		dw 2
			db 'UPPER', 0
			dw .uppercase
			db 'LOWER', 0
			dw .lowercase

; ------------------------------------------------------------------
; CLS

do_cls:
	call show_work_page

	mov ah, 0x06
	mov al, 0
	mov bh, 7
	mov ch, 0
	mov cl, 0
	mov dh, 24
	mov dl, 79
	int 0x10

	mov dx, 0
	call basic_set_cursor

	call show_disp_page

	jmp mainloop



; ------------------------------------------------------------------
; CURSOR

do_cursor:
	mov si, .keywords
	call choose_keyword
	jmp .error

.turn_on:
	call os_show_cursor
	jmp mainloop

.turn_off:
	call os_hide_cursor
	jmp mainloop

.error:
	mov si, err_syntax
	jmp error


.keywords:
	dw 2
	db "ON", 0
	dw .turn_on
	db "OFF", 0
	dw .turn_off


; ------------------------------------------------------------------
; CURSCHAR

do_curschar:
	call get_numeric_variable
	jc .error

	push si

	mov ax, 0x08
	mov bl, 0
	mov bh, [work_page]
	int 0x10

	pop si

	mov ah, 0

	call set_numeric_variable
	jc .error

	jmp mainloop

.error:
	mov si, err_expected_nvar
	jmp error


; ------------------------------------------------------------------
; CURSCOL

do_curscol:
	call get_numeric_variable
	jc .error

	push si
	mov ax, 0x08
	mov bl, 0
	mov bh, [work_page]
	int 0x10
	pop si

	movzx ax, ah
	call set_numeric_variable
	jmp mainloop

.error:
	mov si, err_expected_nvar
	jmp error


; ------------------------------------------------------------------
; CURSPOS

do_curspos:
	call get_numeric_variable
	jc .error
	mov di, si

	call get_numeric_variable
	jc .error

	mov ah, 0x03
	mov bh, [work_page]
	int 0x10
	
	movzx ax, dh
	call set_numeric_variable

	movzx ax, dl
	call set_numeric_variable

	jmp mainloop

.error:
	mov si, err_expected_nvar
	jmp error


; ------------------------------------------------------------------
; DELETE

do_delete:
	call get_string_parameter
	jc .error

.get_filename:
	mov ax, si
	call os_file_exists
	jc .no_file

	call os_remove_file
	jc .del_fail

	jmp .returngood

.no_file:
	mov ax, 'R'
	mov bx, 2
	call set_var
	jmp mainloop

.returngood:
	mov ax, 'R'
	mov bx, 0
	call set_var
	jmp mainloop

.del_fail:
	mov ax, 'R'
	mov bx, 1
	call set_var
	jmp mainloop

.error:
	mov si, err_expected_string
	jmp error
	

; ------------------------------------------------------------------
; DO

do_do:
	mov dx, [prog]
	sub dx, 2

	mov si, .keywords
	call choose_keyword

.start_loop:
	cmp byte [loop_in], 20
	je .loop_max

	mov word di, do_loop_store
	movzx word ax, [loop_in]
	add di, ax

	mov [di], dx

	add byte [loop_in], 2
	jmp mainloop

.while_keyword:
	call check_condition
	jc .start_loop
	jmp .skip_loop

.until_keyword:
	call check_condition
	jc .skip_loop
	jmp .start_loop

.skip_loop:
	mov si, .search_words
	call find_closing_statement
	jc .no_end

	jmp mainloop

.loop_max:
	mov si, err_doloop_maximum
	jmp error

.no_end:
	mov si, err_no_loop
	jmp error

.keywords:
	dw 2
	db 'WHILE', 0
	dw .while_keyword
	db 'UNTIL', 0
	dw .until_keyword

.search_words:
	db 'DO', 0
	db 0
	db 0
	db 'LOOP', 0

	
;-------------------------------------------------------------------
; ELSE

do_else:
	; Was the last IF statement false? Allow the else line/block to run.
	; Was the last IF statement true? Skip over the else line/block.	
	cmp byte [last_if_false], 0
	je .last_true
	
	jmp mainloop
	
.last_true:
	; Now check if there is anything else on the line.
	; If there is, it's a single line else, otherwise it's an else block.
	call is_at_eol
	jc .multiline

.single_line:
	call skip_remaining_line
	jmp mainloop

.multi_line:
	mov si, .search_words
	call find_closing_statement
	jmp mainloop

.search_words:
	db 'IF', 0
	db 'THEN', 0
	db 0
	db 'ENDIF', 0

; ------------------------------------------------------------------
; END

do_end:
	mov ah, 5				; Restore active page
	mov al, 0
	int 10h

	mov byte [work_page], 0
	mov byte [disp_page], 0

	mov word sp, [orig_stack]
	ret


; ------------------------------------------------------------------
; ENDIF

do_endif:
	jmp mainloop


; ------------------------------------------------------------------
; FILES

do_files:
	mov ax, .filelist			; get a copy of the filelist
	call os_get_file_list
	mov si, ax

	call basic_get_cursor
	mov dl, 0
	je .initial_newline

.next_file:
	mov dx, 20

.file_list_loop:
	lodsb					; get a byte from the list

	cmp al, ','				; a comma means the next file, so create a new line for it
	je .pad_filename
	
	cmp al, 0				; the list is null terminated
	je .end_of_list
	
	call basic_print_char
	dec dx

	jmp .file_list_loop			; keep going until the list is finished
	
.pad_filename:					; print spaces until the next column
	cmp dx, 0
	je .next_file

	mov al, ' '
	call basic_print_char
	dec dx

	jmp .pad_filename

.initial_newline:
	call basic_newline
	jmp .next_file

.end_of_list:
	call basic_get_cursor
	cmp dl, 0
	je .no_final_newline

	call basic_newline
.no_final_newline:
	jmp mainloop
	
.filelist		times 512	db 0
	


; ------------------------------------------------------------------
; FOR

do_for:
	mov al, [for_depth]

	cmp al, 9
	jae .overflow

	mov ah, 0
	shl ax, 1
	mov bx, ax

	; Get the loop variable
	call get_numeric_variable		; FOR X = A TO B
	jc .error				;     ^
	mov di, si

	; Make sure there is an equals sign
	mov al, '='				; FOR X = A TO B STEP C
	call check_symbol			;       ^
	jc .error
	
	; Get the initial value and set the loop variable
	call get_numeric_parameter		; FOR X = A TO B STEP C
	jc .error				;         ^
	call set_numeric_variable
	mov dx, ax

	; Check for the 'TO' keyword
	mov si, .to_keyword			; FOR X = A TO B STEP C
	call choose_keyword			;           ^

.error:
	mov si, err_syntax
	jmp error

.got_to:
	; Now find the target value for the loop
	call get_numeric_parameter		; FOR X = A TO B STEP C
	jc .error				;              ^

	mov [for_targets + bx], ax
	mov cx, ax

	; Check for the step keyword, note that this is optional.
	mov si, .step_keyword			; FOR X = A TO B STEP C
	call choose_keyword			;                ^
	
	; If it wasn't specified, just use one as the step.
	mov ax, 1
	jmp .store_step

.got_step:
	call get_numeric_parameter		; FOR X = A TO B STEP C
	jc .error				;                     ^ 

.store_step:
	mov [for_steps + bx], ax

	; That's all the parameters.
	; Now check if the initial value is already larger than the target.
	; If it is, there is no need to do even one loop.
	; Note that the step is to be treated as a signed number.
	cmp ax, 0
	jl .check_initial_negative

.check_initial:
	; For positive steps the loop finishes when: INITIAL > TARGET
	cmp dx, cx
	ja .skip_loop
	jmp .start_loop

.check_initial_negative:
	; For negative steps the loop finishes when: INITIAL < TARGET
	cmp dx, cx
	jb .skip_loop
	
.start_loop:
	; Remember where the loop starts.
	mov ax, [prog]
	mov [for_start_points], ax

	inc byte [for_depth]
	jmp mainloop

.skip_loop:
	mov si, .search_words
	call find_closing_statement
	jnc mainloop

.no_next:
	mov si, err_for
	jmp error

.overflow:
	mov si, err_forloop_maximum
	jmp error


.to_keyword		dw 1
			db 'TO', 0
			dw .got_to

.step_keyword		dw 1
			db 'STEP', 0
			dw .got_step

.search_words		db 'FOR', 0
			db 0
			db 0
			db 'NEXT', 0

	

; ------------------------------------------------------------------
; GETKEY

do_getkey:
	call get_numeric_variable
	jc .error

	call os_check_for_key

	cmp ax, 48E0h
	je .up_pressed

	cmp ax, 50E0h
	je .down_pressed

	cmp ax, 4BE0h
	je .left_pressed

	cmp ax, 4DE0h
	je .right_pressed

.store:
	call set_numeric_variable
	jmp mainloop

.up_pressed:
	mov ax, 1
	jmp .store

.down_pressed:
	mov ax, 2
	jmp .store

.left_pressed:
	mov ax, 3
	jmp .store

.right_pressed:
	mov ax, 4
	jmp .store

.error:
	mov si, err_syntax
	jmp error


; ------------------------------------------------------------------
; GOSUB

do_gosub:
	call get_label
	mov dx, ax

	inc byte [gosub_depth]

	mov ax, 0
	mov byte al, [gosub_depth]		; Get current GOSUB nest level

	cmp al, 9
	jle .within_limit

	mov si, err_nest_limit
	jmp error

.within_limit:
	mov di, gosub_points			; Move into our table of pointers
	add di, ax				; Table is words (not bytes)
	add di, ax
	mov word ax, [prog]
	stosw					; Store current location before jump

	mov [prog], dx				; Jump to the label
	jmp mainloop



; ------------------------------------------------------------------
; GOTO

do_goto:
	call get_label
	mov [prog], ax			; Continue the program at the label
	jmp mainloop


; ------------------------------------------------------------------
; IF

do_if:
	call check_condition
	jc .condition_true

	; If the condition is *not* true, skip over the rest of the statement.
	; If the 'THEN' keyword is present it is a single line IF.
	mov si, .then_keyword
	call choose_keyword

	; Otherwise it is an IF block spanning multiple lines
	mov si, .search_words
	call find_closing_statement
	jc .no_endif

	mov byte [last_if_false], 1
	jmp mainloop

.single_line:
	call skip_remaining_line
	mov byte [last_if_false], 1
	jmp mainloop

.condition_true:
	; If the condition was true, continue running the block.
	mov byte [last_if_false], 0
	jmp mainloop

.no_endif:
	mov si, err_no_endif
	jmp error

.error:
	mov si, err_syntax
	jmp error

.then_keyword		dw 1
			db 'THEN', 0
			dw .single_line

.search_words:		db 'IF', 0		
			db 'THEN', 0
			db 'ELSE', 0
			db 'ENDIF', 0


; ------------------------------------------------------------------
; INCLUDE

do_include:
	call get_string_parameter
	jc .error

	mov ax, si

	mov word di, [prog_end]
	mov byte [di], 10
	inc di
	mov cx, di
	push cx

	call os_load_file
	jc .load_fail

	pop cx
	add cx, bx
	mov word [prog_end], cx

	jmp mainloop


.load_fail:
	pop cx
	mov si, err_file_notfound
	jmp error

.error:
	mov si, err_syntax
	jmp error


; ------------------------------------------------------------------
; INK

do_ink:
	call get_numeric_parameter
	jc .error
	
	mov [ink_colour], al
	jmp mainloop

.error:
	mov si, err_expected_number
	jmp error


; ------------------------------------------------------------------
; INPUT

do_input:
	call get_string_variable
	jc .try_numeric

	mov ax, si
	mov bx, 128
	call os_input_string

	jmp mainloop

.try_numeric:
	call get_numeric_variable
	jc .error
	mov di, si

	mov ax, .number_buffer
	mov bx, 6
	call os_input_string

	mov si, ax
	call os_string_to_int
	
	mov si, di
	call set_numeric_variable

	jmp mainloop

.error:
	mov si, err_syntax
	jmp error

.number_buffer			times 6 db 0
	


; -----------------------------------------------------------
; LEN

do_len:
	call get_string_parameter
	jc .error
	mov di, si

	call get_numeric_variable
	jc .error

	mov ax, di
	call os_string_length
	
	call set_numeric_variable

	jmp mainloop
 
.error:
	mov si, err_syntax
	jmp error



; ------------------------------------------------------------------
; LISTBOX

do_listbox:
	call save_cursor
	
	call get_string_variable
	jc .error
	mov ax, si

	call get_string_variable
	jc .error
	mov bx, si

	call get_string_variable
	jc .error
	mov cx, si

	call get_numeric_variable
	jc .error
	mov dx, si

	call os_list_dialog
	jc .esc_pressed

.finish:
	call restore_cursor

	mov si, dx
	call set_numeric_variable
	jmp mainloop

.esc_pressed:
	mov ax, 0
	jmp .finish


.error:
	mov si, err_syntax
	jmp error


; ------------------------------------------------------------------
; LOAD

do_load:
	call get_string_parameter
	jc .error

	call get_numeric_parameter
	jc .error

	mov cx, ax
	mov ax, si
	call os_load_file
	jc .load_error

	mov ax, 0
	call .set_r

	mov ax, 'S'
	call set_var

	jmp mainloop

.load_error:
	mov ax, 1
	call .set_r
	jmp mainloop

.set_r:
	push bx

	mov bx, ax
	mov ax, 'R'
	call set_var

	pop bx
	ret

.error:
	mov si, err_syntax
	jmp error


; ------------------------------------------------------------------
; LOOP

do_loop:
	cmp byte [loop_in], 0
	je .no_do

	mov si, .keywords
	call choose_keyword

.loop_back:
	sub byte [loop_in], 2

	mov word si, do_loop_store
	movzx word ax, [loop_in]
	add si, ax

	mov ax, [si]
	mov word [prog], ax
	jmp mainloop
	
.while_keyword:
	call check_condition		; For WHILE keyword, loop if true
	jc .loop_back
	jmp mainloop
	
.until_keyword:
	call check_condition		; For UNTIL keyword, loop if false
	jnc .loop_back
	jmp mainloop
	
.no_do:
	mov si, err_no_do
	jmp error

.keywords:
	dw 2
	db 'WHILE', 0
	dw .while_keyword
	db 'UNTIL', 0
	dw .until_keyword

	
	
; ------------------------------------------------------------------
; MOVE

do_move:
	call get_numeric_parameter
	jc .error
	mov dl, al

	call get_numeric_parameter
	jc .error
	mov dh, al

	call basic_set_cursor
	jmp mainloop

.error:
	mov si, err_expected_number
	jmp error


; ------------------------------------------------------------------
; NEXT

do_next:
	cmp byte [for_depth], 0
	je .no_for

	dec byte [for_depth]
	movzx bx, [for_depth]
	shl bx, 1

	; First grab the loop variable. Note that we need both the current
	; value *and* a pointer from the next parameter to set a new value.
	; The solution is to trick the interpreter into processing it twice.
	push word [prog]
	call get_numeric_parameter
	jc .error
	pop word [prog]

	call get_numeric_variable
	jc .error

	mov dx, [for_targets + bx]
	mov cx, [for_steps + bx]

	cmp cx, 0
	jl .negative_step

.positive_step:
	; The loop is finished if VARIABLE >= TARGET before adding the step.
	; e.g. the loop in "FOR X = 1 TO 5 STEP 1" when X = 5
	cmp ax, dx
	jae .finish_loop

	; Increment the variable.
	add ax, cx
	call set_numeric_variable

	; Test if this takes us out of the loop's bounds, if VARIABLE > TARGET
	; e.g. the loop in "FOR X = 0 TO 3 STEP 2" when X = 2 before increment.
	cmp ax, dx
	ja .finish_loop

	; If neither of those condition are true, continue the loop.
	jmp .loop_back

.negative_step:
	; Basically the same as above except with a negative increment the loop
	; ends if the variable is below the target rather than above.
	cmp ax, dx
	jbe .finish_loop

	add ax, cx
	call set_numeric_variable

	cmp ax, dx
	jb .finish_loop

.loop_back:
	; If the loop is repeating, reset the program counter to the start.
	mov ax, [for_start_points + bx]
	mov [prog], ax

	inc byte [for_depth]

.finish_loop:
	; Continue onto the next instruction.
	jmp mainloop

.error:
	mov si, err_syntax
	jmp error

.no_for:
	mov si, err_next
	jmp error



;-------------------------------------------------------------------
; NUMBER

do_number:
	call get_numeric_parameter
	jc .try_string

	call get_string_variable
	jc .error
	mov di, si
	
	call os_int_to_string
	mov si, ax
	call set_string_variable

	jmp mainloop

.try_string:
	call get_string_parameter
	jc .error
	mov di, si

	call get_numeric_variable
	jc .error
	xchg si, di

	call os_string_to_int
	mov si, di
	call set_numeric_variable

	jmp mainloop

.error:
	mov si, err_syntax
	jmp error


;-------------------------------------------------------------------
; PAGE

do_page:
	call get_numeric_parameter
	jc .error

	mov byte [work_page], al	; Set work page variable

	call get_numeric_parameter
	jc .error

	mov byte [disp_page], al	; Set display page variable

	call show_disp_page

	jmp mainloop

.error:
	mov si, err_expected_number
	jmp error


; ------------------------------------------------------------------
; PAUSE

do_pause:
	call get_numeric_parameter
	jc .error

.finish:
	call os_pause
	jmp mainloop

.error:
	mov si, err_expected_number
	jmp error


; ------------------------------------------------------------------
; PEEK

do_peek:
	call get_numeric_variable
	jc .error

	call get_numeric_parameter
	jc .error

	mov di, ax
	movzx ax, [di]
	call set_numeric_variable

	jmp mainloop
	
.error:
	mov si, err_syntax
	jmp error


	.tmp_var	db 0
	
	
	
; ------------------------------------------------------------------
; PEEKINT

do_peekint:
	call get_numeric_variable
	jc .error

	call get_numeric_parameter
	jc .error

	mov di, ax
	mov ax, [di]
	call set_numeric_variable

	jmp mainloop
	
.error:
	mov si, err_syntax
	jmp error



; ------------------------------------------------------------------
; POKE

do_poke:
	call get_numeric_parameter
	jc .error
	mov bx, ax

	call get_numeric_parameter
	jc .error
	
	mov si, ax
	mov [si], bl
	mov di, [prog]
	jmp mainloop

.error:
	mov si, err_syntax
	jmp error



; ------------------------------------------------------------------
; POKEINT

do_pokeint:
	call get_numeric_parameter
	jc .error
	mov bx, ax

	call get_numeric_parameter
	jc .error

	mov si, ax
	mov [si], bx
	jmp mainloop

.error:
	mov si, err_syntax
	jmp error




; ------------------------------------------------------------------
; PORT

do_port:
	mov si, .keywords
	call choose_keyword
	jmp .error

.do_out_cmd:
	call get_numeric_parameter
	jc .error
	mov dx, ax

	call get_numeric_parameter
	jc .error

	call os_port_byte_out
	jmp mainloop

.do_in_cmd:
	call get_numeric_parameter
	jc .error
	mov dx, ax

	call get_numeric_variable
	jc .error

	call os_port_byte_in

	mov ah, 0
	call set_numeric_variable

	jmp mainloop

.error:
	mov si, err_syntax
	jmp error

.keywords:
	dw 2
	db 'OUT', 0
	dw .do_out_cmd
	db 'IN', 0
	dw .do_in_cmd


; ------------------------------------------------------------------
; PRINT

do_print:
	; Fetch the address of a string to print.
	; Numeric values will be converted to string.
	call  get_string_parameter
	jnc .print_string

	; Is there parameter cannot be expressed as a string, check for
	; a print keyword (e.g. CHR, HEX)
	mov si, .keywords
	call choose_keyword			

.error:
	mov si, err_syntax
	jmp error

.print_string:
	lodsb

	cmp al, 0
	je .newline_or_not

	call basic_print_char
	jmp .print_string


.is_chr:
	; Put the lower 8-bits of the character in a string buffer.
	; It can then be printed normally, as a one character string.
	call get_numeric_parameter
	jc .error

	mov [.print_buffer], al
	mov byte [.print_buffer + 1], 0

	mov si, .print_buffer
	jmp .print_string
	
.is_hex:
	; Convert the next number to a two digit hexadecimal string.
	; It can then be printed normally.
	call get_numeric_parameter
	jc .error

	call .get_hex_char
	mov [.print_buffer + 1], ah

	shr ax, 4
	call .get_hex_char
	mov [.print_buffer + 0], ah

	mov byte [.print_buffer + 2], 0

	mov si, .print_buffer
	jmp .print_string

.get_hex_char:
	; Lazy conversion --- look the character up on a lookup table.
	mov ah, al
	and ah, 0x0F
	movzx si, ah
	mov ah, [si + .hex_chars]
	ret

.newline_or_not:
	; Check for a semicolon at the end of the line.
	; If there is one, do not print a newline.

	mov al, ';'
	call check_symbol
	jnc .finish

	call basic_newline

.finish:
	jmp mainloop


.print_buffer			times 3 db 0
.hex_chars			db '0123456789ABCDEF'
.tmp_loc			dw 0

.keywords			dw 2
				db 'CHR', 0
				dw .is_chr
				db 'HEX', 0
				dw .is_hex


; ------------------------------------------------------------------
; RAND

do_rand:
	call get_numeric_variable
	jc .error

	call get_numeric_parameter
	jc .error
	mov bx, ax

	call get_numeric_parameter
	jc .error
	
	xchg ax, bx
	call os_get_random

	mov ax, cx
	call set_numeric_variable

	jmp mainloop

.error:
	mov si, err_syntax
	jmp error


; ------------------------------------------------------------------
; READ

do_read:
	call get_label
	mov di, ax

	call get_numeric_parameter
	jc .error

	cmp ax, 0
	je .range_error

	call get_numeric_variable
	jc .error

	mov dx, [prog]
	mov [prog], di

	mov cx, ax

.read_value:
	call get_numeric_parameter
	jc .range_error

	mov bx, [prog]
	cmp bx, [prog_end]
	jge .range_error

	loop .read_value

	call set_numeric_variable

	mov [prog], dx
	jmp mainloop

.error:
	mov si, err_syntax
	jmp mainloop

.range_error:
	mov si, err_read_range
	jmp mainloop


; ------------------------------------------------------------------
; REM

do_rem:
	call skip_remaining_line
	jmp mainloop


; ------------------------------------------------------------------
; RENAME

do_rename:
	call get_string_parameter
	jc .error

	mov di, .old_filename
	call .copy_filename

	call get_string_parameter
	jc .error

	mov di, .new_filename
	call .copy_filename

.check_exists:
	mov word ax, .old_filename	; Check if the source file exists
	call os_file_exists
	jc .file_not_found		; If it doesn't exists set "R = 1"

	mov ax, .new_filename		; The second file is the destination and should not exist
	call os_file_exists
	jnc .file_exists		; If it exists set "R = 3"
	
.rename:
	mov word ax, .old_filename	; Seem to be okay, lets rename
	mov word bx, .new_filename
	call os_rename_file

	jc .rename_failed		; If it failed set "R = 2", usually caused by a read-only disk

	mov ax, 0			; It worked sucessfully, so set "R = 0" to indicate no error
	mov byte al, 'R'
	mov bx, 0
	call set_var

	jmp mainloop

.copy_filename:
	mov ax, si
	call os_string_length

	cmp ax, 12
	ja .too_long

.begin_copy:
	mov cx, ax
	rep movsb
	mov al, 0
	stosb
	ret

.too_long:
	mov ax, 12
	jmp .begin_copy


.error:
	mov si, err_syntax
	jmp error

.file_not_found:
	mov al, 1
	jmp .set_r_var

.rename_failed:
	mov al, 2
	jmp .set_r_var

.file_exists:
	mov al, 3

.set_r_var:
	movzx bx, al
	mov ax, 'R'
	call set_var
	jmp mainloop

.data:
	.old_filename			times 13 db 0
	.new_filename			times 13 db 0


; ------------------------------------------------------------------
; RETURN

do_return:
	movzx bx, [gosub_depth]

	cmp bl, 0
	jne .is_ok

	mov si, err_return
	jmp error

.is_ok:
	shl bx, 1
	mov ax, [bx + gosub_points]
	mov word [prog], bx
	dec byte [gosub_depth]

	jmp mainloop	


; ------------------------------------------------------------------
; SAVE

do_save:
	call get_string_parameter
	jc .error

	call get_numeric_parameter
	jc .error
	mov bx, ax

	call get_numeric_parameter
	jc .error
	mov cx, ax

	mov ax, si
	call os_file_exists
	jc .file_exists

	mov ax, si
	call os_write_file
	jc .disk_error

	mov ax, 0
	call .set_r
	jmp mainloop

.file_exists:
	mov ax, 2
	call .set_r
	jmp mainloop

.disk_error:
	mov ax, 1
	call .set_r
	jmp mainloop

.set_r:
	mov bx, ax
	mov ax, 'R'
	call set_var
	ret

.error:
	mov si, err_syntax
	jmp error


; ------------------------------------------------------------------
; SERIAL

do_serial:
	mov si, .keywords
	call choose_keyword

.error:
	mov si, err_syntax
	jmp error

.do_on_cmd:
	call get_numeric_parameter
	jc .error

	cmp ax, 1200
	je .on_cmd_slow_mode
	cmp ax, 9600
	je .on_cmd_fast_mode

	mov si, err_serial_rate
	jmp error

.on_cmd_fast_mode:
	mov ax, 0
	call os_serial_port_enable
	jmp mainloop

.on_cmd_slow_mode:
	mov ax, 1
	call os_serial_port_enable
	jmp mainloop


.do_send_cmd:
	call get_numeric_parameter
	jc .error

	call os_send_via_serial
	jmp mainloop


.do_rec_cmd:
	call get_numeric_variable
	jc .error

	call os_get_via_serial
	test ah, 0x80
	jnz .recieve_error

	mov ah, 0
	call set_numeric_variable

	jmp mainloop

.recieve_error:
	mov ax, 0
	call set_numeric_variable
	jmp mainloop

.keywords		dw 3
			db 'ON', 0
			dw .do_on_cmd
			db 'SEND', 0
			dw .do_send_cmd
			db 'REC', 0
			dw .do_rec_cmd



; ------------------------------------------------------------------
; SIZE

do_size:
	call get_string_parameter
	jc .error

	mov ax, si
	call os_get_file_size
	jc .file_not_found

	mov ax, 'S'
	call set_var

	mov bx, 0
	jmp .set_r
	
.file_not_found:
	mov bx, 1

.set_r:
	mov ax, 'R'
	call set_var
	jmp mainloop

.error:
	mov si, err_expected_string
	jmp error



; ------------------------------------------------------------------
; SOUND

do_sound:
	call get_numeric_parameter
	jc .error
	mov bx, ax

	call get_numeric_parameter
	jc .error
	mov dx, ax

	mov ax, bx
	call os_speaker_tone

	mov ax, dx
	call os_pause
	call os_speaker_off

	jmp mainloop

.error:
	mov si, err_expected_number
	jmp error



;-------------------------------------------------------------------
; STRING
do_string:
	mov si, .keywords
	call choose_keyword

.error:
	mov si, err_syntax
	jmp error

.set_str:
	; Change one character in the string.
	call get_string_variable	; String to change (BX)
	jc .error
	mov bx, si

	call get_numeric_parameter	; Offset in string (SI)
	jc .error

	call get_numeric_parameter	; Character to set
	jc .error

	cmp si, 126
	ja .range_error

	mov [bx + si], al
	jmp mainloop

.get_str:
	; Retrieve one character from the string.
	call get_string_variable	; String to look in
	jc .error
	mov bx, si

	call get_numeric_parameter	; Offset in string
	jc .error
	mov di, si

	call get_numeric_variable	; Variable to store result in
	jc .error

	cmp si, 126
	ja .range_error

	movzx ax, [bx + di]
	call set_numeric_variable

	jmp mainloop

.load_str:
	; Retrieve a string in memory and copy it to a string variable.
	call get_string_variable
	jc .error
	mov di, si

	call get_numeric_parameter
	jc .error

	call set_string_variable
	jmp mainloop

.save_str:
	; Copy a string variable to memory.
	call get_string_variable
	jc .error

	call get_numeric_parameter
	jc .error

	mov di, ax
	call os_string_copy

	jmp mainloop	
	
.range_error:
	mov si, err_string_range
	jmp error

.keywords:
	dw 4
	db "GET", 0
	dw .get_str
	db "SET", 0
	dw .set_str
	db "LOAD", 0
	dw .load_str
	db "STORE", 0
	dw .save_str



; ------------------------------------------------------------------
; WAITKEY

do_waitkey:
	call get_numeric_variable
	jc .error

	call os_wait_for_key

	cmp ax, 48E0h
	je .up_pressed

	cmp ax, 50E0h
	je .down_pressed

	cmp ax, 4BE0h
	je .left_pressed

	cmp ax, 4DE0h
	je .right_pressed

.store:
	mov ah, 0
	call set_numeric_variable

	jmp mainloop

.error:
	mov si, err_expected_nvar
	jmp error


.up_pressed:
	mov ax, 1
	jmp .store

.down_pressed:
	mov ax, 2
	jmp .store

.left_pressed:
	mov ax, 3
	jmp .store

.right_pressed:
	mov ax, 4
	jmp .store


; ==================================================================
; INTERNAL ROUTINES FOR INTERPRETER

; ------------------------------------------------------------------
; get_numeric_parameter --- Fetches the value of any number token
; IN: nothing
; OUT: AX = value, CF = set if next token is not numeric

get_numeric_parameter:
	pusha
	push word [prog]

	call get_token
	movzx ax, [token_type]

	cmp ax, VARIABLE
	je .is_variable

	cmp ax, NUMBER
	je .is_number

	cmp ax, SYMBOL
	je .is_operator

	cmp ax, STRING
	je .is_string

.error:
	; If there next variable cannot be converted to a value,
	; reverse the program back to it and return with an error.
	; The caller might want to try other forms of a command or give up.
	pop word [prog]

	stc
	popa
	ret

.is_variable:
	mov si, [token]
	mov ax, [si]
	jmp .done

.is_number:
	mov ax, [token]
	jmp .done

.is_string:
	call get_special_value
	jc .error
	jmp .done

.is_operator:
	mov al, [token]

	cmp al, '~'
	je .bitwise_not

	cmp al, '-'
	je .negate

	cmp al, '&'
	je .address
	
	cmp al, '*'
	je .indirection

	cmp al, '@'
	je .byte_indirect

	cmp al, '!'
	je .logical_not

	jmp .error

.bitwise_not:
	; Recursively fetch the next numeric value and invert it.
	call get_numeric_parameter
	jc .error

	not ax
	jmp .done

.negate:
	call get_numeric_parameter
	jc .error

	neg ax
	jmp .done

.address:
	call get_numeric_variable
	jc .string_address
	
	mov ax, [si + 2]
	jmp .done

.string_address:
	call get_string_variable
	jc .error

	mov ax, si
	jmp .done

.indirection:
	call get_numeric_parameter
	jc .error

	mov si, ax
	mov ax, [si]
	jmp .done

.byte_indirect:
	call get_numeric_parameter
	jc .error

	mov si, ax
	movzx ax, [si]
	jmp .done

.logical_not:
	call get_numeric_parameter
	jc .error

	cmp ax, 0
	sete al
	mov ah, 0

.done:
	add sp, 2
	mov bp, sp
	mov [bp + 14], ax

	popa
	clc
	ret
	


; get_numeric_variable --- Reads the next token as a numeric variable.
;
; If the next token can be interpreted as a numeric variable, the address of
; the corrosponding variable is returned and the program pointer advances to
; the next token. Otherwise, an error is returned without advancing the
; program pointer.
; 
; IN: nothing
; OUT: SI = Pointer to variable data block
; OUT: CF = Set if error, otherwise clear

get_numeric_variable:
	pusha
	push word [prog]

	movzx bx, [var_stack_depth]
	
	cmp bx, 9
	je .error

	shl bx, 2

	call get_token
	mov al, [token_type]

	cmp al, SYMBOL
	je .modifier

	cmp al, VARIABLE
	jne .error

	mov word [var_stack + bx], VARIABLE
	mov ax, [token]
	mov [var_stack + bx + 2], ax

.finish:
	inc byte [var_stack_depth]
	add bx, var_stack

	add sp, 2
	mov bp, sp
	mov [bp + 2], bx
	popa
	clc
	ret

.modifier:
	mov al, [token]
	
	cmp al, '*'
	je .indirection

	cmp al, '@'
	je .byte_indirect

.error:
	pop word [prog]
	popa
	stc
	ret

.indirection:
	call get_numeric_parameter
	jc .error

	mov si, ax
	mov ax, [si]

	mov word [var_stack + bx], VARIABLE
	mov word [var_stack + bx + 2], ax
	jmp .finish

.byte_indirect:
	call get_numeric_parameter
	jc .error

	mov si, ax
	movzx ax, [si]

	mov word [var_stack + bx], CHAR_VAR
	mov word [var_stack + bx + 2], ax
	jmp .finish



set_numeric_variable:
	pusha

	mov dx, [si]
	
	cmp dx, VARIABLE
	je .is_word_var

	cmp dx, CHAR_VAR
	je .is_byte_var

	popa
	ret

.is_word_var:
	mov di, [si + 2]
	mov [di], ax
	jmp .done

.is_byte_var:
	mov di, [si + 2]
	mov [di], al

.done:
	popa
	ret



; ------------------------------------------------------------------
; get_string_parameter --- Retrieve a string pointer from the next token.
;
; Interprets the next token as a string an returns a pointer to the string.
; The returned pointer is not valid after this function is called again.
;
; IN: nothing
; OUT: SI = string address; CF = set if not string, otherwise clear

get_string_parameter:
	pusha
	push word [prog]		; Save current program pointer.

	call get_token
	movzx ax, [token_type]

	cmp ax, QUOTE			; Is is a quote (e.g. "Apple")?
	je .is_quote

	cmp ax, STRING_VAR		; Is it a string variable (e.g. $8)?
	je .is_string_var

	pop word [prog]			; Reverse pointer back if not found.

	call get_numeric_parameter	; Try to convert a number to a string.
	jnc .is_numeric
	
	popa
	ret

.is_quote:
	; Copy the token buffer to another buffer.
	; Otherwise it might be lost when another function retrieves a token.
	mov si, token
	mov di, .token_buffer
	call os_string_copy

	mov ax, .token_buffer	; Return a pointer to the internal buffer.
	jmp .done

.is_string_var:
	mov ax, [token]		; Return the given pointer to the storage.
	jmp .done

.is_numeric:
	; Is the parameter a numeric type?
	; If so, convert it to a string and return the string.
	call os_int_to_string
	mov si, ax
	mov di, .number_buffer
	call os_string_copy

	mov ax, .number_buffer
	jmp .got_number
	
.done:
	add sp, 2			; Ignore saved program pointer
.got_number:
	mov bp, sp
	mov [bp + 2], ax		; Change saved SI
	popa
	clc
	ret

.number_buffer			times 6 db 0
.token_buffer			times MAX_TOKEN_LEN + 1 db 0



; set_string_variable --- Safely store data in a string variable. 
; IN: SI = text to set; DI = output variable
; OUT: nothing
set_string_variable:
	pusha

	mov cx, 127		; Maximum 127 characters + terminator
	
.copy_string:
	lodsb			; Copy from input string to string variable
	cmp al, 0
	je .finish
	stosb
	loop .copy_string

.finish:
	mov al, 0		; Put a null terminator at the end.
	stosb

	popa
	ret


; get_string_variable --- Retrieves a string variable
get_string_variable:
	pusha
	push word [prog]

	call get_token
	cmp byte [token_type], STRING_VAR
	jne .incorrect_type

	mov ax, [token]

	add sp, 2
	mov bp, sp
	mov [bp + 2], ax

	popa
	clc
	ret

.incorrect_type:
	pop word [prog]
	popa
	stc
	ret


; get_special_value --- If the last token is a special keyword, the value is
;    returned.
; IN: nothing
; OUT: AX = value; CF = set if not valid special, otherwise clear

get_special_value:
	pusha

	cmp ax, STRING
	jne .not_special

	mov si, token
	
	mov di, ink_keyword
	call os_string_compare
	je .is_ink
	
	mov di, progstart_keyword
	call os_string_compare
	je .is_progstart

	mov di, ramstart_keyword
	call os_string_compare
	je .is_ramstart

	mov di, timer_keyword
	call os_string_compare
	je .is_timer
	
	mov di, variables_keyword
	call os_string_compare
	je .is_variables
	
	mov di, version_keyword
	call os_string_compare
	je .is_version

.not_special:
	stc
	popa
	ret

.is_ink:
	movzx ax, [ink_colour]
	jmp .done

.is_progstart:
	mov ax, [load_point]
	jmp .done

.is_ramstart:
	mov ax, [prog_end]
	jmp .done

.is_timer:
	mov ah, 0x00
	int 0x1A
	mov ax, dx
	jmp .done

.is_variables:
	mov ax, vars_loc
	jmp .done

.is_version:
	mov ax, MIKEOS_API_VER
	jmp .done

.done:
	mov [.tmp], ax
	popa
	mov ax, [.tmp]
	clc
	ret

.tmp				dw 0



; save_cursor
; IN/OUT: nothing

save_cursor:
	pusha
	mov ah, 0x03
	mov bh, [work_page]
	int 0x10
	mov [saved_cursor_pos], dx
	popa
	ret

restore_cursor:
	pusha
	mov ah, 0x02
	mov bh, [work_page]
	mov dx, [saved_cursor_pos]
	int 0x10
	popa
	ret

show_work_page:
	push ax
	mov ah, 0x05
	mov al, [work_page]
	int 0x10
	pop ax
	ret

show_disp_page:
	push ax
	mov ah, 0x05
	mov al, [disp_page]
	int 0x10
	pop ax
	ret


check_condition:
	pusha 

	call get_numeric_parameter
	jc .try_string

	mov bx, ax

	mov si, .numeric_operators
	call choose_symbol

	cmp bx, 0
	je .false
	jmp .true

.try_string:
	call get_string_variable
	jc .invalid_condition
	mov di, si

	mov si, .string_operators
	call choose_symbol

	jmp .invalid_condition

.equal: 
	call get_numeric_parameter
	jc .error

	cmp ax, bx
	je .true
	jmp .false
	
.not_equal:
	mov al, '='
	call check_symbol
	jc .invalid_condition

	call get_numeric_parameter
	jc .error

	cmp ax, bx
	jne .true
	jmp .false

.lesser:
	mov al, '='
	call check_symbol
	jnc .lesser_equal

	call get_numeric_parameter
	jc .error

	cmp ax, bx
	jb .true
	jmp .false

.lesser_equal:
	call get_numeric_parameter
	jc .error

	cmp ax, bx
	jbe .true
	jmp .false

.greater:
	mov al, '='
	call check_symbol
	jnc .greater_equal

	call get_numeric_parameter
	jc .error

	cmp ax, bx
	ja .true
	jmp .false

.greater_equal:
	call get_numeric_parameter
	jc .error

	cmp ax, bx
	jae .true
	jmp .false

.str_equal:
	call get_string_parameter
	jc .error

	call os_string_compare
	jc .true
	jmp .false

.str_not_equal:
	mov al, '='
	call check_symbol
	jc .error

	call get_string_parameter
	jc .error

	call os_string_compare
	jnc .true
	jmp .false

.true:
	mov cx, 1
	jmp .try_next

.false:
	mov cx, 0

.try_next:
	mov si, .keywords
	call choose_keyword

	; CX should be set to the final result (0 = false, 1 = true)
	; The negation instruction sets the carry flag for a non-zero value.
	; This should set the carry flag correctly for the result.
	neg cx
	popa
	ret

.has_and:
	call check_condition
	jcxz .false

	jc .true
	jmp .false

.has_or:
	call check_condition
	jcxz .test_or
	jmp .true

.test_or:
	jc .true
	jmp .false

.invalid_condition:
	mov si, err_condition
	jmp error



.numeric_operators	dw 4
			db '<'
			dw .lesser
			db '>'
			dw .greater
			db '='
			dw .equal
			db '!'
			dw .not_equal

.string_operators	dw 2
			db '='
			dw .equal
			db '!'
			dw .not_equal

.keywords		dw 2
			db 'AND', 0
			dw .has_and
			db 'OR', 0
			dw .has_or

	


; This function can operate in different "modes", currently line and bracket
; mode. 
; In line mode (CX=0) the sum will collect tokens until the end of the program 
; line is was called for.
; In bracket mode tokens are collected until the ')' closing bracket is
; encounted. 
; 
; CX = sum type
; AX = result
get_numeric_sum:
	pusha

	; Get the initial value of the sum. (e.g. 'A' in "A + B")
	call get_numeric_parameter
	jc .error
	mov bx, ax

	; Remember where the end of the current line is.
	push word [prog]
	call skip_remaining_line
	mov di, [prog]
	pop word [prog]
	jmp .next_part


.next_part:
	; Remember the pointer after the last valid token.
	mov si, [prog]

	; Ensure a sum does not go past the end of the line.
	cmp si, di
	jae .past_eol

	call get_token
	movzx ax, [token_type]

	cmp ax, SYMBOL
	jne .no_more_symbols

	mov al, [token]

	cmp al, '+'
	je .add

	cmp al, '-'
	je .take

	cmp al, '*'
	je .multiply

	cmp al, '/'
	je .divide

	cmp al, '%'
	je .modulo

	cmp al, '&'
	je .and

	cmp al, '|'
	je .or

	cmp al, '^'
	je .xor

	cmp al, ')'
	je .end_bracket

.no_more_symbols:
	cmp cx, 0
	jne .error

	mov [prog], bp

.finish_sum:
	mov bp, sp
	mov [bp + 14], bx
	popa
	ret

.add:
	call .get_value
	add bx, ax
	jmp .next_part

.take:
	call .get_value
	sub bx, ax
	jmp .next_part

.multiply:
	call .get_value
	mul bx 
	mov bx, ax
	jmp .next_part

.divide:
	call .get_value

	cmp ax, 0
	je .div_zero

	xchg ax, bx

	mov dx, 0
	div bx
	mov bx, ax
	jmp .next_part

.modulo:
	call .get_value

	cmp ax, 0
	je .div_zero

	xchg ax, bx
	mov dx, 0

	div bx
	mov bx, dx
	jmp .next_part

.and:
	call .get_value
	and bx, ax
	jmp .next_part

.or:
	call .get_value
	or bx, ax
	jmp .next_part

.xor:
	call .get_value
	xor bx, ax
	jmp .next_part

.end_bracket:
	cmp cx, 1
	jne .error
	jmp .finish_sum
	
.error:
	mov si, err_sum
	jmp error

.div_zero:
	mov si, err_divide_by_zero
	jmp error

.get_value:
	mov si, [prog]

	cmp si, di
	jge .error

	call get_numeric_parameter
	jc .error

	ret

.past_eol:
	; Line sums can terminate at the end of line.
	cmp cx, 0
	je .no_more_symbols

	; Bracket sums cannot, they expect a closing bracket.
	jmp .error




; Combines string tokens on a line into a string string.
; IN: DI = output string
get_string_sum:
	pusha

	mov dx, 127

.next_part:
	; Get a string component
	call get_string_parameter
	jc .error

	; Check length against the remaining space in the output string.
	; Copy only what will fit in the output string.
	mov ax, si
	call os_string_length

	cmp ax, dx
	jae .too_long

.add_part:
	mov cx, ax
	rep movsb

	sub dx, ax

	cmp dx, 0
	je .end_string

	; Check for the '+' symbol to indicate additional parts.
	mov al, '+'
	call check_symbol
	jnc .next_part

.end_string:
	mov byte [di], 0
	popa
	ret

.too_long:
	mov ax, dx
	jmp .add_part

.error:
	mov si, err_sum
	jmp error



skip_remaining_line:
	pusha

	mov di, [prog]		; Starting at the current point
	mov al, 10		; Search for the newline character

	mov cx, [prog_end]	; Until the proram end
	sub cx, di

	repne scasb		; ...and go!
	mov [prog], di		; Save the new location.
	jne .past_end		; Was the newline actually located?

	popa			; If so, we've found the next line.
	clc
	ret

.past_end:
	popa
	stc
	ret


get_label:
	pusha

	; Get a string token from the program that corrosponds to the label
	; that is to be found.
	call get_token
	cmp byte [token_type], STRING
	jne .not_found

	mov ax, token
	call os_string_length

	; Line labels end in a colon, so search the program for a colon.
	mov di, [load_point]		; Start from the start the program
	mov cx, [prog_end]		; Check all characters
	sub cx, di

	; Skip over the length of the label when searching for the colon.
	; It couldn't be long enough to hold the label.
	sub cx, ax
	add di, ax

	; Remember the length of the label.
	mov dx, ax

	mov bp, cx

.try_next:
	mov cx, bp
	jcxz .not_found

	; Scan until a colon is found or the all bytes have been scanned
	mov al, ':'
	repne scasb
	jne .not_found

	mov bp, cx

	; Compare the string before the colon to the requested label
	mov bx, di		; Use BX for the comparison to preserve DI
	dec bx			; Skip colon at program pointer
	mov si, token		; Find the address of the token's terminator
	add si, dx
	mov cx, dx

.check_label:
	dec bx			; Now do a backwards string compare
	dec si

	mov al, [bx]

	cmp al, 'a'		; Uppercase characters to match token.
	jb .check_char

	cmp al, 'z'
	ja .check_char

	sub al, 0x20

.check_char:
	cmp al, [si]
	jne .try_next

	loop .check_label	
	
	; We've determined the label we were searching for matches a string in
	; the program. However, we need to determine it is alone on the line
	; and not part of another token.
	; e.g. 'foo:' could be '     foo:' or part of 'print "foo:"'
.found_label:
	; Move pointer to the start of the label
	mov bx, di
	sub bx, dx
	dec bx

	; Check if the label is at the very start of the program
	cmp bx, [load_point]
	je .label_okay
	
.find_line_start:
	dec bx

	; Check for the start of the program when on the first line...
	cmp bx, [load_point]
	je .label_okay

	mov al, [bx]

	; ...or a previous newline for any other.
	cmp al, 0x0A
	je .label_okay
	
	; Skip over any whitespace
	cmp al, ' '
	je .find_line_start

	; Any other character indicates other tokens on the line. Not a label.
	jmp .try_next

.label_okay:
	; Everything looks good! Return the address past the end of the label.
	; No need to make the parser skip over the label.
	mov [.tmp], di
	popa
	mov ax, [.tmp]
	ret

.not_found:
	mov si, err_label_notfound
	jmp error
	
.tmp				dw 0



choose_keyword:
	pusha
	push word [prog]
	
	call get_token
	movzx ax, [token_type]
	
	cmp ax, STRING
	jne .not_found

	mov di, token

	lodsw
	mov cx, ax

.check_string:
	mov ax, si
	call os_string_length

	call os_string_compare
	jc .found_match

	add si, ax
	add si, 3

	loop .check_string

.not_found:
	pop word [prog]
	popa
	ret

.found_match:
	add si, ax
	inc si
	lodsw
	
	add sp, 2			; Ignore saved program pointer
	mov bp, sp			; Change return address to the address
	mov [bp + 16], ax		; specified by the keyword.
	popa
	ret


choose_symbol:
	pusha
	push word [prog]

	call get_token
	movzx ax, [token_type]

	cmp ax, SYMBOL
	jne .invalid_symbol

	mov dl, [token]

	lodsw
	mov cx, ax

.find_symbol:
	lodsb

	cmp al, dl
	je .found_symbol

	add si, 2
	loop .find_symbol

.invalid_symbol:
	pop word [prog]
	popa
	ret

.found_symbol:
	lodsw

	add sp, 2
	mov bp, sp
	mov [bp + 16], ax

	popa
	ret


check_symbol:
	pusha
	push word [prog]

	mov dl, al

	call get_token
	cmp byte [token_type], SYMBOL
	jne .not_found

	cmp dl, [token]
	jne .not_found

	add sp, 2
	popa
	clc
	ret

.not_found:
	pop word [prog]
	popa
	stc
	ret

	

basic_print_char:
	pusha
	
	cmp al, 10
	je .newline

	mov ah, 0x09
	mov bh, [work_page]
	mov bl, [ink_colour]
	mov cx, 1
	int 0x10

	call basic_adv_cursor
	popa
	ret

.newline:
	call basic_newline
	popa
	ret


basic_adv_cursor:
	push dx

	call basic_get_cursor

	cmp dl, 79
	jge .newline

	inc dl
	call basic_set_cursor

	pop dx
	ret

.newline:
	call basic_newline
	pop dx
	ret
	
	
basic_newline:
	pusha

	call basic_get_cursor

	mov dl, 0

	cmp dh, 24
	jge .scroll

	inc dh
	call basic_set_cursor

	popa
	ret

.scroll:
	pusha
	mov ah, 0x06		; Function: Scroll Up
	mov al, 1		; Lines to scroll: 1
	mov bh, 7		; Colour for new lines: 7
	mov ch, 0		; First row in "scroll" area: 0
	mov cl, 0		; First column in area: 0
	mov dh, 24		; Last row in area: 24
	mov dl, 79		; Last column in area: 79
	int 0x10		; ...and go! (finally!)
	popa

	mov dh, 24
	mov dl, 0
	call basic_set_cursor

	popa
	ret
	
basic_get_cursor:
	pusha
	mov ah, 0x03
	mov bh, [work_page]
	int 0x10
	mov [.tmp], dx
	popa
	mov dx, [.tmp]
	ret
.tmp			dw 0


basic_set_cursor:
	pusha
	mov ah, 0x02
	mov bh, [work_page]
	int 0x10
	popa
	ret


find_closing_statement:
	pusha

	mov di, .terms
	lodsw
	mov cx, ax

.get_keyword:
	mov ax, di
	stosw

	mov ax, si
	call os_string_length
	add di, ax
	inc di

	loop .get_keyword

.try_next_line:
	call skip_remaining_line
	jc .past_end

	; Check the first STRING token on each line.
	; This keyword should only be a command.
	call get_token

	cmp byte [token_type], STRING
	jne .try_next_line

	mov si, [prog]

	; Check for the keyword that starts a new block (e.g. IF, DO, FOR)
	mov di, [.terms]
	call os_string_compare
	jc .sub_block


	; Check for the keyword that stops the search midway (e.g. ELSE)
	mov di, [.terms + 4]
	call os_string_compare
	jc .found_keyword

	; Check for the keyword that ends the block (e.g. ENDIF, LOOP, NEXT)
	mov di, [.terms + 6]
	call os_string_compare
	jc .found_keyword

	; If none of those match, keep searching until the end of program.
	jmp .try_next_line


.sub_block:
	push word [prog]

.search_for_single_line:

.skip_block:
	inc byte [.recursion_level]

	call find_closing_statement
	jc .not_found

	dec byte [.recursion_level]

	jmp .try_next_line
	
	


	

.terms			times 4 dw 0
.recursion_level	db 0


hexadecimal_to_int:
	pusha

	mov bx, 0
	mov cx, 4

.next_char:
	lodsb

	cmp al, 0
	je .done

	cmp al, '0'
	jb .error

	cmp al, '9'
	jbe .number

	cmp al, 'F'
	ja .error

	cmp al, 'A'
	jb .error

.letter:
	sub al, 'A' - 10
	jmp .add_value

.number:
	sub al, '0'

.add_value:
	shl bx, 4
	mov ah, 0
	or bx, ax
	
	loop .next_char

.done:
	mov bp, sp
	mov [bp + 14], bx
	popa
	ret
	
.error:
	mov bx, 0
	jmp .done


; ------------------------------------------------------------------
; Get value of variable character specified in AL (eg 'A')

get_var:
	mov ah, 0
	sub al, 65
	mov si, variables
	add si, ax
	add si, ax
	lodsw
	ret


; ------------------------------------------------------------------
; Set value of variable character specified in AL (eg 'A')
; with number specified in BX

set_var:
	mov ah, 0
	sub al, 65				; Remove ASCII codes before 'A'

	mov di, variables			; Find position in table (of words)
	add di, ax
	add di, ax
	mov ax, bx
	stosw
	ret


; ------------------------------------------------------------------
; Get token from current position in prog

get_token:
	pusha

	mov word si, [prog]
	mov bx, char_types

.get_next:
	cmp si, [prog_end]
	jae .past_end

	call get_char

	cmp ah, 1
	je .get_next

	cmp al, '0'
	je get_hexadecimal_token

	cmp ah, 3
	je get_number_token

	cmp ah, 2
	je get_string_token

	cmp al, '$'
	je get_string_var_token

	cmp al, '"'
	je get_quote_token

	cmp al, 0x27
	je get_char_token

	cmp al, '('
	je get_sum_token

	cmp ah, 4
	je get_symbol_token

.bad_token:
	mov si, err_token
	jmp error

.past_end:
	mov byte [token], 0
	mov ax, SYMBOL
	ret

get_string_var_token:
	call get_char
	
	cmp ah, 3
	jne .invalid

	mov [prog], si

	cmp al, '$'
	je .pointer

	cmp al, '0'
	je .invalid

	cmp al, '9'
	je .invalid

	sub al, '1'
	mov ah, 0
	shl ax, 7
	add ax, string_vars
	
	mov [token], ax
	mov byte [token_type], STRING_VAR
	popa
	ret

.invalid:
	mov si, err_string_var
	jmp error

.pointer:
	call get_numeric_parameter
	jc .invalid

	mov [token], ax
	ret



get_number_token:
	mov bx, 0
	mov cx, 5
	jmp .add_numeral

.get_numeral:
	call get_char
	cmp ah, 3
	jne .finish

	; Multiply current value by ten
	shl bx, 1		; Double BX
	mov dx, bx		; Add eight times the original value
	shl dx, 2
	add bx, dx
	
.add_numeral:
	sub al, '0'
	mov ah, 0

	add bx, ax

	loop .get_numeral

	inc si

.finish:
	cmp ah, 0
	je .error

	cmp ah, 2
	je .error

	mov al, 0
	stosb

	dec si
	mov [prog], si

	mov [token], bx
	mov byte [token_type], NUMBER
	popa
	ret

.error:
	mov si, err_char_in_num
	jmp error

	
get_string_token:
	mov di, token
	mov cx, 127
	jmp .store_alphanumeric

.get_alphanumeric:
	call get_char
	cmp ah, 2
	je .store_alphanumeric
	
	cmp ah, 3
	jne .finish

.store_alphanumeric:
	stosb
	loop .get_alphanumeric

	inc si

.finish:
	cmp ah, 0
	je .error

	mov byte [di], 0

	cmp al, ':'
	je .label

	dec si
	mov [prog], si

	mov ax, token
	call os_string_uppercase

	cmp cx, 126
	je .variable

	mov byte [token_type], STRING
	popa
	ret

.variable:
	movzx ax, [token]
	sub ax, 'A'

	shl ax, 1
	add ax, variables
	mov [token], ax

	mov byte [token_type], VARIABLE
	popa
	ret

.label:
	mov al, 0
	stosb
	
	mov [prog], si

	mov byte [token_type], LABEL
	popa
	ret

.error:
	mov si, err_token
	jmp error



get_char_token:
	call get_char
	
	cmp ah, 0
	je .error

	cmp al, 0x0A
	je .error

	mov ah, 0
	mov [token], ax
	
	call get_char
	
	cmp al, 0x27
	jne .error

	mov [prog], si
	
	mov byte [token_type], NUMBER
	popa
	ret

.error:
	mov si, err_quote_term
	jmp error
	

get_quote_token:
	mov di, token
	mov cx, 127

.add_char:
	call get_char

	cmp ah, 0
	je .error

	cmp al, 0x0A
	je .error

	cmp al, '"'
	je .done

	stosb
	loop .add_char

.done:
	mov al, 0
	stosb

	mov [prog], si

	mov byte [token_type], QUOTE
	popa
	ret

.error:
	mov si, err_quote_term
	jmp error

get_symbol_token:
	mov di, token
	stosb
	mov al, 0
	stosb

	mov [prog], si

	mov byte [token_type], SYMBOL
	popa
	ret

get_sum_token:
	mov [prog], si

	mov cx, 1
	call get_numeric_sum

	mov word [token], ax
	mov byte [token_type], NUMBER
	popa
	ret

get_hexadecimal_token:
	mov bx, 0
	mov cx, 4

	call get_char
	cmp ah, 3
	je .add_numeral

	and al, ~0x20

	cmp al, 'X'
	je .get_hexchar

	cmp ah, 2
	je .check_hexchar

	; Might actually be just a zero?
	; In which case just put a zero in the buffer.
	dec si
	mov [prog], si

	mov word [token], 0
	mov byte [token_type], NUMBER
	popa
	ret

.get_hexchar:
	call get_char
	shl bx, 4
	
.check_hexchar:
	cmp ah, 3
	je .add_numeral

	cmp ah, 2
	jne .done

	; Lazy uppercase conversion
	and al, ~0x20

	cmp al, 'F'
	ja .error

.add_letter:
	sub al, 'A' - 10
	mov ah, 0
	or bx, ax
	jmp .next_hexchar

.add_numeral:
	sub al, '0'
	mov ah, 0
	or bx, ax
	
.next_hexchar:
	loop .get_hexchar

	inc si

.done:
	cmp ah, 0
	je .error

	dec si
	mov [prog], si

	mov [token], bx
	mov byte [token_type], NUMBER
	popa
	ret

.error:
	mov si, err_not_hexadecimal
	jmp error

	
	
get_char:
	lodsb
	mov ah, al
	xlatb 
	xchg ah, al
	ret


; Type table, provides the type number for each character.
; 0 - UNKNOWN - Cannot be used in the program.
; 1 - PADDING - Can be used for spacing between tokens, always ignored.
; 2 - LETTER - Any case. Can be used for variables, strings, quotes and chars.
; 3 - NUMBER - Can be used in numeric constants and as part of letter types.
; 4 - SYMBOL - Can be used as a unary or binary operator or part of a command.
char_types:
	;  0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 1, 0, 0 ; 0x0#
	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 0x1#
	db 1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4 ; 0x2#
	db 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4 ; 0x3#
	db 4, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ; 0x4#
	db 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 4, 4, 4, 4, 4 ; 0x5#
	db 4, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ; 0x6#
	db 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 4, 4, 4, 4, 0 ; 0x7#
	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 0x8#
	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 0x9#
	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 0xA#
	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 0xB#
	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 0xC#
	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 0xD#
	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 0xE#
	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 0xF#


; ------------------------------------------------------------------
; Print error message and quit out

error:
	mov ah, 5			; Revert display page
	mov al, 0
	int 10h

	mov byte [work_page], 0
	mov byte [disp_page], 0

	call os_print_newline
	call os_print_string		; Print error message


	mov si, line_num_starter
	call os_print_string


	; And now print the line number where the error occurred. We do this
	; by working from the start of the program to the current point,
	; counting the number of newline characters along the way

	mov word si, [load_point]
	mov word bx, [prog]
	mov cx, 1

.loop:
	lodsb
	cmp al, 10
	jne .not_newline
	inc cx
.not_newline:
	cmp si, bx
	je .finish
	jmp .loop
.finish:

	mov ax, cx
	call os_int_to_string
	mov si, ax
	call os_print_string


	call os_print_newline

	mov word sp, [orig_stack]	; Restore the stack to as it was when BASIC started

	ret				; And finish


	; Error messages text...

	err_char_in_num		db "Error: unexpected char in number", 0
	err_cmd_unknown		db "Error: unknown command", 0
	err_divide_by_zero	db "Error: attempt to divide by zero", 0
	err_forloop_maximum	db "Error: FOR/NEXT nexting limit exceeded", 0
	err_doloop_maximum	db "Error: DO/LOOP nesting limit exceeded", 0
	err_file_notfound	db "Error: file not found", 0
	err_label_notfound	db "Error: label not found", 0
	err_nest_limit		db "Error: FOR or GOSUB nest limit exceeded", 0
	err_for			db "Error: FOR without NEXT", 0
	err_next		db "Error: NEXT without FOR", 0
	err_no_do		db "Error: LOOP without DO", 0
	err_no_loop		db "Error: DO without LOOP", 0
	err_quote_term		db "Error: quoted string or char not terminated correctly", 0
	err_return		db "Error: RETURN without GOSUB", 0
	err_string_range	db "Error: string location out of range", 0
	err_read_range		db "Error: READ out of range", 0
	err_expected_number	db "Error: expected numeric type", 0
	err_expected_nvar	db "Error: expected numeric variable", 0
	err_expected_string	db "Error: expected string type", 0
	err_expected_svar	db "Error: expected string variable", 0
	err_condition		db "Error: invalid condition given", 0
	err_serial_rate		db "Error: invalid serial rate", 0
	err_sum			db "Error: bad sum", 0
	err_no_endif		db "Error: IF block without ENDIF", 0
	err_string_var		db "Error: unknown string variable", 0
	err_unknown_statement	db "Error: not a command, assignment or label", 0
	err_token		db "Error: unknown token", 0
	err_not_hexadecimal	db "Error: invalid hexadecimal number", 0
	err_syntax		db "Error: syntax error", 0
	err_break		db "BREAK CALLED", 0

	line_num_starter	db " - line ", 0


; ==================================================================
; DATA SECTION

	orig_stack		dw 0		; Original stack location when BASIC started

	prog			dw 0		; Pointer to current location in BASIC code
	prog_end		dw 0		; Pointer to final byte of BASIC code

	load_point		dw 0

	token_type		db 0		; Type of last token read (eg NUMBER, VARIABLE)
	token			times MAX_TOKEN_LEN + 1 db 0 ; Storage space for the token

vars_loc:
	variables		times 26 dw 0	; Storage space for variables A to Z

	for_depth		db 0
	for_variables		times 20 dw 0	; Address of variable to update
	for_targets		times 10 dw 0	; Target value of FOR loop
	for_start_points	times 10 dw 0	; Start of FOR loop
	for_steps		times 10 dw 0

	var_stack_depth		db 0
	var_stack		times 40 dw 0
	
	do_loop_store		times 10 dw 0	; Storage for DO loops
	loop_in			db 0		; Loop level

	last_if_false		db 0		; Checking for 'ELSE'

	ink_colour		db 0		; Text printing colour
	work_page		db 0		; Page to print to
	disp_page		db 0		; Page to display

	saved_cursor_pos	dw 0

	alert_cmd		db "ALERT", 0
	askfile_cmd		db "ASKFILE", 0
	break_cmd		db "BREAK", 0
	call_cmd		db "CALL", 0
	case_cmd		db "CASE", 0
	cls_cmd			db "CLS", 0
	cursor_cmd		db "CURSOR", 0
	curschar_cmd		db "CURSCHAR", 0
	curscol_cmd		db "CURSCOL", 0
	curspos_cmd		db "CURSPOS", 0
	delete_cmd		db "DELETE", 0
	do_cmd			db "DO", 0
	else_cmd		db "ELSE", 0
	end_cmd			db "END", 0
	endif_cmd		db "ENDIF", 0
	files_cmd		db "FILES", 0
	for_cmd 		db "FOR", 0
	gosub_cmd		db "GOSUB", 0
	goto_cmd		db "GOTO", 0
	getkey_cmd		db "GETKEY", 0
	if_cmd 			db "IF", 0
	include_cmd		db "INCLUDE", 0
	ink_cmd			db "INK", 0
	input_cmd 		db "INPUT", 0
	len_cmd			db "LEN", 0
	listbox_cmd		db "LISTBOX", 0
	load_cmd		db "LOAD", 0
	loop_cmd		db "LOOP", 0
	move_cmd 		db "MOVE", 0
	next_cmd 		db "NEXT", 0
	number_cmd		db "NUMBER", 0
	page_cmd		db "PAGE", 0
	pause_cmd 		db "PAUSE", 0
	peek_cmd		db "PEEK", 0
	peekint_cmd		db "PEEKINT", 0
	poke_cmd		db "POKE", 0
	pokeint_cmd		db "POKEINT", 0
	port_cmd		db "PORT", 0
	print_cmd 		db "PRINT", 0
	rand_cmd		db "RAND", 0
	read_cmd		db "READ", 0
	rem_cmd			db "REM", 0
	rename_cmd		db "RENAME", 0
	return_cmd		db "RETURN", 0
	save_cmd		db "SAVE", 0
	serial_cmd		db "SERIAL", 0
	size_cmd		db "SIZE", 0
	sound_cmd 		db "SOUND", 0
	string_cmd		db "STRING", 0
	waitkey_cmd		db "WAITKEY", 0

	ink_keyword		db "INK", 0
	progstart_keyword	db "PROGSTART", 0
	ramstart_keyword	db "RAMSTART", 0
	timer_keyword		db "TIMER", 0
	variables_keyword	db "VARIABLES", 0
	version_keyword		db "VERSION", 0

	gosub_depth		db 0
	gosub_points		times 10 dw 0	; Points in code to RETURN to

	string_vars		times 1024 db 0	; 8 * 128 byte strings


; ------------------------------------------------------------------

