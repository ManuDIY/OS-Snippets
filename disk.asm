; Rewrite to MikeOS Disk Drivers
; NOT FINISHED YET. Just posting what I have done so far.
; Don't try to compile it.

; What's new?
; * Modular design - Much easier to read and maintain, no repeated code
; * FAT16 and FAT32 support - For communicating with other media types
; * Directory support - Subsystem supports directories, needs an os_ call
; * Disk change support - Detects new media.
; * Better Filesysten handling - Supports a wide range of media parameters.
; * Better Performance - Remembers what was last put in the disk cache.
; * New API Calls - os_get_filename, os_enter_directory, os_get_subdirname
; * Timestamps Support - These get properly updated.
; * Better Documentation - os_ and disk_ functions are well documented.
; * Deprecates os_get_file_list - No more need for huge buffers.

; ==================================================================
; MikeOS -- The Mike Operating System kernel
; Copyright (C) 2006 - 2014 MikeOS Developers -- see doc/LICENSE.TXT
;
; FAT12 FLOPPY DISK ROUTINES
; ==================================================================

%define FAT_DIR_ENTRY_LENGTH		32

%define BUFFER_TYPE_UNUSED		0
%define BUFFER_TYPE_DIR			1
%define BUFFER_TYPE_FAT			2


; Directory entry attributes

; Read-only doesn't do anything at the moment since MikeOS doesn't overwrite files
%define ATTRIB_READ_ONLY		(1 << 0)
; Files with the hidden attribute won't show up in directory listings, i.e. os_get_file_list
; However, they can still be modified by specifying an exact name.
%define ATTRIB_HIDDEN			(1 << 1)
%define ATTRIB_SYSTEM			(1 << 2)
; Files with the volume ID attribute will be ignored entirely
%define ATTRIB_VOLUME_ID		(1 << 3)
; Directories will be ignored by the MikeOS API, but the disk subsystem supports them.
%define ATTRIB_DIRECTORY		(1 << 4)
; The archive bit is used by backup utilites, should be set when a file is created, renamed or modified.
%define ATTRIB_ARCHIVE			(1 << 5)
; Long File Names are use a heap of attributes at the same time
; This includes the Volume ID bit, so they get ignored by drives that don't support them
%define ATTRIB_LFN			(ATTRIB_READ_ONLY | ATTRIB_HIDDEN | ATTRIB_SYSTEM | ATTRIB_VOLUME_ID)

; Types of FAT filesystem
; The difference between FAT12 and FAT16 is just the number of bits in a cluster number.;
; However, FAT32 adds some useful features to work with larger files/volumes.
; The only way to check what type of FAT filesystem is in use is to check the number of clusters.
%define FSTYPE_FAT12			1
%define FSTYPE_FAT16			2
%define FSTYPE_FAT32			3

; The dirlist data structure helps keep track of the file number when parsing directories.
; It also keeps track of which sector it's metadata is located in.
%define DIRLIST_SIZE			8
%define DIRLIST_COUNT			5

; Flags given to directory listing (DE_TYPE_* flags) - 2 bytes
%define DIRLIST_FIELD_TYPE		0
; The cluster number the current file's metadata is located in - 4 bytes
%define DIRLIST_FIELD_CLUSTER		2
; The current file number in the directory - 2 bytes
; Can be stored in a word as there should never be more than 65536 files in a directory.
%define DIRLIST_FIELD_NUMBER		6


; Data types for a directory listing

; Finds deleted files
%define DE_TYPE_DELETED			(1 << 0)
; Includes hidden files
; Most functions need this, unless they are generating some kind of file listing
%define DE_TYPE_HIDDEN			(1 << 1)
; Includes system files
%define DE_TYPE_SYSTEM			(1 << 2)
; Includes directories alongside files
%define DE_TYPE_SUBDIR			(1 << 3)
; Excludes files and returns only directories
%define DE_TYPE_DIR_ONLY		(1 << 4)
; Includes all directory entries, including deleted items, volume ids, LFNs, etc.
; Everything except unused (blank) entries.
; Overwrites any other flags
%define DE_TYPE_ALL			(1 << 5)


; There's 8 KiB for the MikeOS disk buffer.
; The general purpose buffer is use to load/cache directories and the FAT.
; The default size of 7 KiB is allocated for the general purpose buffer
; This allows for 14 clusters at 512 bytes/cluster or 240 files in a directory.

; There is also 512 bytes allocated to cache the BPB.
; This helps keep track of disk changes if the 'changeline' is not supported.

; Finally, there are two cluster lists.
; These hold the a list of clusters numbers that are part of a file.
; The number of clusters they can hold depends of the FAT type.
; However, clusters in the list must occupy whole bytes.
; For FAT12 it can store 128 clusters.
; At 512 bytes/cluster, that's enough for a 64 KiB file.

; You can change the size of each but it must add up to 8 KiB (8096) overall.
%define DISK_BUFFER_SIZE		7 * 1024
%define BPB_BUFFER_SIZE			512
%define CLUSTER_LIST_SIZE		256

; Calculated from the above parameters.
%define DIR_ENTRY_LIMIT			DISK_BUFFER_SIZE / FAT_DIR_ENTRY_LENGTH

; Absolute locations of the above buffer items.
; The disk buffer is usually located at 0x6000, between kernel and user memory.
; The ordering of items in the buffer doesn't matter.
%define DISK_BUFFER			0x6000
%define CLUSTER_LIST_1			DISK_BUFFER + DISK_BUFFER_SIZE
%define CLUSTER_LIST_2			CLUSTER_LIST_1 + CLUSTER_LIST_SIZE
%define BPB_BUFFER			BPB_BUFFER_SIZE

; Uses a FAT time stamp
%define BPB_RELOAD_TIME			10


; Bochs debugging macros, these should be removed before the final build.
%macro E9LINE 0
	pushf
	push ax
	push si
	mov al, 0x20
	out 0xE9, al
	
	mov ax, __LINE__
	call os_int_to_string
	mov si, ax

%%print:
	lodsb
	cmp al, 0
	je %%done

	out 0xE9, al
	jmp %%print

%%done:
	mov al, 0x0A
	out 0xE9, al
	pop si
	pop ax
	popf
%endmacro
	
%macro BREAK 0
	E9LINE
	xchg bx, bx
%endmacro


; ------------------------------------------------------------------
; os_get_filename --- Get a filename from the list.
; IN: SI = location to store filename (13 bytes required)
; IN: AX = file number
; OUT: CF = set if list ended, otherwise clear

os_get_filename:
	pusha

	; Put a null terminator in the output string.
	; Just in case a disk error occurs
	; or the file number is past the end of the list.
	mov byte [si], 0

	call disk_get_parameters
	jc .done

	call disk_read_directory
	jc .done

	mov di, si
	inc ax		; Entries numbers start at zero
	mov cx, ax

	; Begin a list of the current directory.
	; Files only and no hidden files, neither don't show up in file lists.
	mov ax, DE_TYPE_SYSTEM
	mov bx, DIRLIST_4
	call disk_start_dir_listing
	
	; Skip all entries until the desired entry is reached
.next_file:
	call disk_get_dir_entry
	jc .done	; If end of list, disk error

	loop .next_file

	; Should have a pointer to the directory entry
	; The SFN should be right at the start.
	; Just convert and copy and everything should be fine (or empty)
	mov ax, si
	call disk_sfn_to_string
	call os_string_copy
	
	clc

.done:
	popa
	ret
	

; ------------------------------------------------------------------
; os_get_file_list --- Generate comma-separated string of files on floppy
; IN/OUT: AX = location to store zero-terminated filename string

; No limit to the output size, but OS buffers use 1024 byte buffers.
; This is enough for ~78 files. That should be okay.

; DEPRECATED: Use os_get_filename instead
; This function can cause buffer overflows.

os_get_file_list:
	pusha
	
	mov dx, ax
	mov di, ax
	
	call disk_get_parameters
	jc .done
	
	; Load the directory data into the general purpose buffer.
	; The disk read will be skipped if the data is already cached.
	; This is to cut down on unneeded disk reads.
	call disk_read_directory
	jc .done

	; Start a file listing for the current directory
	mov ax, DE_TYPE_SYSTEM
	mov bx, DIRLIST_2
	mov si, DISK_BUFFER
	call disk_start_dir_listing
	
.get_entry:
	; Load a directory entry if there are any left.
	call disk_get_dir_entry
	jc .done
	
	; Translate the SFN at the start of the entry data.
	mov ax, si
	call disk_sfn_to_string
	jc .get_entry
	
	; Copy filename to the output buffer.
	call os_string_copy
	mov ax, si
	call os_string_length
	add di, ax
	
	; Add a comma between files.
	mov al, ','
	stosb
	
	jmp .get_entry
	
.done:
	; If the output pointer has not advanced, just put a null at the start
	; of the list and finish. It still needs to be a valid string.
	cmp di, dx
	je .empty_list
	
	; Otherwise, still put the null terminator at the end but overwrite the
	; last comma.
	dec di
	
.empty_list:
	mov byte [di], 0
	; ...and done. Hopefully the caller allocated a large buffer!
	popa
	ret
	
	
	

; ------------------------------------------------------------------
; os_load_file -- Load file into RAM
; IN: AX = location of filename, CX = location in RAM to load file
; OUT: BX = file size (in bytes), carry set if file not found

os_load_file:
	E9LINE
	pusha

	call disk_string_to_sfn
	jc .error

	call disk_get_parameters
	jc .error
	
	call disk_read_directory
	jc .error

	; Get a pointer to the directory entry for the SFN we just found.
	; SI is passed from the disk_string_to_sfn routine.
	; Make sure to search hidden files.
	mov ax, DE_TYPE_SYSTEM | DE_TYPE_HIDDEN
	call disk_find_dir_entry
	jc .error

	; Retrieve the size and cluster from the directory entry
	; The maximum of these fields depends on the FAT type
	call disk_get_file_size
	mov [.size], eax
	call disk_get_file_cluster

	; Load the right part of the FAT and retrieve a list of clusters
	; starting from the first clusters from the entry.
	mov si, CLUSTER_LIST_1
	call disk_fetch_fat_chain
	
	mov eax, [.size]	; Recorded file size
	mov si, CLUSTER_LIST_1	; Pointer to cluster list
	mov di, cx		; Location to store loaded clusters
	call disk_read_fat_chain; ...and go!
	jc .error		; Might encounter a bad cluster or I/O error

	; Looks like we're done, note that the FAT data has replaced the
	; directory data in the general purpose buffer.
	; The next call to disk_read_directory will reload it.
	popa
	mov bx, [.size]
	clc
	ret

.error:
	; If an error occurs, the carry flag is set and the size returned zero.
	E9LINE
	popa
	mov bx, 0
	stc
	ret
	
.size						dd 0


; --------------------------------------------------------------------------
; os_write_file -- Save (max 64K) file to disk
; IN: AX = filename, BX = data location, CX = bytes to write
; OUT: Carry clear if OK, set if failure

os_write_file:
	E9LINE
	pusha

	call disk_string_to_sfn
	jc .error
	E9LINE

	call disk_get_parameters
	jc .error
	E9LINE

	; Read the directory, add a new entry and write it back to the disk.
	; This needs to be done first to check the file doesn't already exist.
	; There's no space allocated in the FAT yet, 
	; so leave the cluster as zero.
	call disk_read_directory
	jc .error
	E9LINE

	; Search for an existing file should include subdirectories.
	; These can't have the same name as a file.
	mov ax, DE_TYPE_SYSTEM | DE_TYPE_HIDDEN | DE_TYPE_SUBDIR
	call disk_find_dir_entry
	jnc .error	; Bailout if file was found
	E9LINE

	; Create a new directory entry to write to
	pusha
			; AX = filename pointer
	movzx ebx, cx	; File size
	mov cx, 0x20	; Set 'Archive' attribute when creating a file
	mov edx, 0	; Start cluster, save this for later
	call disk_new_dir_entry
	popa
	jc .error	; The directory might be full
	E9LINE

	; Write the cached directory back to the disk.
	call disk_write_directory
	jc .error
	E9LINE

	; Now create a FAT chain for the new file
	; Reading and writing the FAT is handled automatically.
	movzx eax, cx
	call disk_bytes_to_clusters	; Find the number of clusters needed
	jc .error			; Could have garbage values...
	call disk_make_fat_chain	; Make a FAT chain with that number
	jc .error	; Could fail if there is not enough free space on disk
	mov edx, eax
	E9LINE

	; Finally, go back to the directory and link them together

	; Any disk errors now will cause problems, but if it was okay before, 
	; everything should be fine now (unless the disk was removed)
	
	call disk_read_directory
	jc .error
	E9LINE

	mov ax, DE_TYPE_SYSTEM | DE_TYPE_HIDDEN
	call disk_find_dir_entry
	jc .error			; Huh, I guess our entry got lost. :(
	E9LINE
	
	; Put the first cluster number for the new chain in the file entry.
	mov eax, edx
	call disk_set_file_cluster

	; Write back directory changes to the disk.
	call disk_write_directory
	jc .error
	E9LINE
	
	popa
	clc
	ret

.error:
	popa
	stc
	ret


; --------------------------------------------------------------------------
; os_file_exists -- Check for presence of file on the floppy
; IN: AX = filename location; OUT: carry clear if found, set if not

os_file_exists:
	E9LINE
	pusha

	call disk_string_to_sfn
	jc .error

	call disk_get_parameters
	jc .error

	; Grab the directory data
	call disk_read_directory
	jc .error

	; Find the entry in the directory, but discard the data pointer.
	; All that matters what that it was found.
	; disk_find_dir_entry should set the carry flag appropriately.
	mov ax, DE_TYPE_SYSTEM | DE_TYPE_HIDDEN
	call disk_find_dir_entry
	popa
	E9LINE
	ret

.error:
	E9LINE
	popa
	stc
	ret

; --------------------------------------------------------------------------
; os_create_file -- Creates a new 0-byte file on the floppy disk
; IN: AX = location of filename; OUT: Nothing

os_create_file:
	E9LINE
	pusha

	; Convert the given string to a FAT Short File Name
	call disk_string_to_sfn
	jc .error

	; Find all information about the current disk
	call disk_get_parameters
	jc .error

	; Read the current directory into memory
	call disk_read_directory
	jc .error

	; Bailout if the file already exists
	; Check both files and directories, they can't have the same name.
	mov ax, DE_TYPE_SYSTEM | DE_TYPE_HIDDEN | DE_TYPE_SUBDIR
	call disk_find_dir_entry
	jnc .error

				; AX = Pointer to SFN
	mov ebx, 0		; File size is zero
	mov cx, ATTRIB_ARCHIVE	; Default attributes
	mov edx, 0		; No associated cluster chain
	call disk_new_dir_entry
	jc .error

	call disk_write_directory
	jc .error

	popa
	ret

.error:
	popa
	stc
	ret


; --------------------------------------------------------------------------
; os_remove_file -- Deletes the specified file from the filesystem
; IN: AX = location of filename to remove

os_remove_file:
	E9LINE
	pusha

	call disk_string_to_sfn
	jc .error
	E9LINE

	call disk_get_parameters
	jc .error
	E9LINE

	call disk_read_directory
	jc .error
	E9LINE

	; Search the directory for the file, include hidden files.
	mov ax, DE_TYPE_HIDDEN
	call disk_find_dir_entry
	jc .error
	E9LINE

	; Setting 0xE5 as the first by of a filename makes it "deleted"
	; This allows the entry to be allocated to another file.
	mov byte [di], 0xE5
	
	call disk_write_directory
	jc .error
	E9LINE

	popa
	ret

.error:
	popa
	stc
	ret


; --------------------------------------------------------------------------
; os_rename_file -- Change the name of a file on the disk
; IN: AX = filename to change, BX = new filename (zero-terminated strings)
; OUT: carry set on error

os_rename_file:
	pusha
	E9LINE
	
	call disk_string_to_sfn
	jc .error

	call disk_get_parameters
	jc .error

	call disk_read_directory
	jc .error

	; Make sure the source file exists and find the entry
	mov ax, DE_TYPE_SYSTEM | DE_TYPE_HIDDEN
	call disk_find_dir_entry
	jc .error

	; Convert the new filename into a SFN.
	; Only one filename can be in the SFN buffer at once,
	; so this needs to be done after finding file entry.
	mov ax, bx
	call disk_string_to_sfn
	jc .error

	; Make sure the destination file does not exist (but ignore the entry)
	; Also search for subdirectories as they cannot have the same name as
	; a file.
	push di
	mov ax, DE_TYPE_SYSTEM | DE_TYPE_HIDDEN | DE_TYPE_SUBDIR
	call disk_find_dir_entry
	pop di
	jnc .error

	; Copy the new name to the file efile entry.
	mov cx, 11
	rep movsb

	; Write back changes to disk.
	call disk_write_directory
	jc .error

	popa
	ret

.error:
	popa
	stc
	ret


; --------------------------------------------------------------------------
; os_get_file_size -- Get file size information for specified file
; IN: AX = filename; OUT: BX = file size in bytes (up to 64K)
; or carry set if file not found

os_get_file_size:
	E9LINE
	pusha

	call disk_string_to_sfn
	jc .error

	call disk_get_parameters
	jc .error

	call disk_read_directory
	jc .error

	mov ax, DE_TYPE_SYSTEM | DE_TYPE_HIDDEN
	call disk_find_dir_entry
	jc .error
	
	call disk_get_file_size
	mov [.size], ax
	E9LINE

	popa
	mov bx, [.size]
	clc
	ret

.error:
	E9LINE
	popa
	stc
	ret

	.size					dw 0


; ==================================================================
; INTERNAL OS ROUTINES -- Not accessible to user programs

; --------------------------------------------------------------------------
; disk_string_to_sfn
; IN: AX = Filename string
; OUT: SI = Pointer to internal buffer with new name
; OUT: Carry set if invalid filename, otherwise clear

disk_string_to_sfn:
	pusha
	
	; The SFN - Short File Name is a fixed length 11 character format
	; There are 8 character for the base name and 3 character for the extension
	; The dot in between is implied.
	
	call os_string_uppercase
	mov dx, ax
	
	; Pad the name out with spaces
	mov di, .filename
	mov al, ' '
	mov cx, 11
	rep stosb
	
	mov si, dx
	mov di, .filename
	mov cx, 9
	
	cmp byte [si], 0xE5
	je .kanji_start
	
	cmp byte [si], '.'
	je .dot_file
	
.parse_basename:
	lodsb
	
	cmp al, '.'
	je .start_extension
	
	; Check for end of string
	cmp al, 0
	je .done
	
	; Make sure character is legal for use in SFNs
	call disk_check_filename_char
	jc .invalid_base_char
	
.valid_char:
	stosb
	loop .parse_basename
	
	jmp .invalid_name
	
.start_extension:
	; Make sure the base name is at least one character.
	; SFNs cannot start with or contain a dot (except for the dot and dotdot files)
	cmp di, .filename
	je .invalid_name
	
	mov di, .filename + 8
	mov cx, 4
	
.parse_extension:
	lodsb
	
	cmp al, 0
	je .done
	
	call disk_check_filename_char
	jc .invalid_ext_char
	
	stosb
	loop .parse_extension
	
	jmp .invalid_name
	
.done:
	; Make sure the filename string was at least one character long
	cmp di, .filename
	je .invalid_name
	
	popa
	mov si, .filename
	clc
	ret
	

.kanji_start:
	mov al, 0x05
	jmp .valid_char
	
; Check if filename string is a "dot" or "dotdot" file.
.dot_file:
	mov al, '.'
	stosb
	cmp byte [si + 1], '.'
	je .dbl_dot_file
	
	cmp byte [si + 1], 0
	jne .invalid_name
	
	jmp .done
	
.dbl_dot_file:
	stosb
	cmp byte [si + 2], 0
	jne .invalid_name
	
	jmp .done
	
; TODO: Better handling for invalid characters. 
; Should probably implement The Basis-Name Generation Algorithm and The Numeric-Tail Generation Algorithm
; For now, invalid filenames just return an error (carry set & blank name).
; Disk functions should treat this as a non-existant filename.
.invalid_name:
.invalid_base_char:
.invalid_ext_char:
	mov di, .filename
	mov cx, 11
	mov al, ' '
	rep stosb
	
	popa
	mov si, .filename
	stc
	ret

.filename			times 12 db 0

; --------------------------------------------------------------------------
; disk_sfn_to_string --- Converts a DOS Short File Name to a string
; IN: AX = Pointer to buffer with SFN
; OUT: SI = Pointer to buffer with filename string

disk_sfn_to_string:
	pusha

	mov dx, ax
	mov si, ax
	mov di, .filename
	mov cx, 8
	
	cmp byte [si], '.'
	je .dot_file
	
	cmp byte [si], 0x05
	je .kanji_start
	
.parse_basename:
	lodsb
	
	; The base name is padded with spaces if less than eight characters
	cmp al, ' '
	je .start_extension
	
	call disk_check_filename_char
	jc .invalid_char
	
.valid_char:
	stosb
	loop .parse_basename
	
	; If eight characters have been processed, there is no padding.
	; Fall through the loop and start the extension.
	
.start_extension:
	; Add the implied dot
	mov al, '.'
	stosb
	
	mov si, dx
	add si, 8
	mov cx, 3
	
.parse_extension:
	lodsb
	
	cmp al, ' '
	je .finished
	
	call disk_check_filename_char
	jc .invalid_char
	
	stosb
	loop .parse_extension
	
.finished:
	; Terminate the string
	mov byte [di], 0
	popa
	mov si, .filename
	clc
	ret
	
	
.invalid_char:
	; If the SFN cannot be successfully translated to a string, just return an error and a blank string.
	; It might be possible to return the name anyway, but could lead to trouble later.
	mov byte [.filename], 0
	popa
	mov si, .filename
	stc
	ret
	
.kanji_start:
	mov al, 0xE5
	jmp .valid_char
	
.dot_file:
	; Normal filenames cannot begin with a dot
	; However every directory (apart from the root directory) contains the "dot" file and the "dotdot" file.
	; The filenames of these files should be just single or double period, respectively, with no extension.
	mov al, '.'
	stosb
	
	inc si
	cmp byte [si], '.'
	je .dbl_dot_file
	
	cmp byte [si], '.'
	je .dbl_dot_file
	
	mov cx, 10
.check_remaining:
	mov al, ' '
	repe cmpsb
	jne .invalid_char
	
	jmp .finished
	
.dbl_dot_file:
	stosb
	
	inc si
	mov cx, 9
	jmp .check_remaining
	
	
	
	
.filename			times 13 db 0


; --------------------------------------------------------------------------
; disk_check_filename_char
; Detects if a character is valid for use in a filename.
; IN: AL = char to check
; OUT: CF = set if invalid, otherwise clear

disk_check_filename_char:
	; SFN entries do not allow any characters in the following ranges in any part of the filename:
	; 0x00-0x1F, 0x2A-0x2F, 0x3A-0x3F, 0x5B-0x5D, 0x7C
	; There are some exceptions but whatever function is translating the name should handle these.
	; It would be simpler/faster to do a lookup table but it would use too much memory.
	cmp al, 0x20
	jle .invalid
	
	cmp al, 0x22
	je .invalid
	
	cmp al, 0x2A
	jl .valid
	
	cmp al, 0x2F
	jle .invalid
	
	cmp al, 0x3A
	jl .valid
	
	cmp al, 0x3F
	jle .invalid
	
	cmp al, 0x5B
	jl .valid
	
	cmp al, 0x5D
	jle .invalid
	
	cmp al, 0x7C
	je .invalid
	
.valid:
	clc
	ret
	
.invalid:
	stc
	ret
	
	


; --------------------------------------------------------------------------
; disk_find_dir_entry
; Disk Buffer: Must contain directory
; IN: AX = Entry type flags
; IN: SI = Pointer to filename to find (in FAT internal format)
; OUT: DI = Address of the directory entry
; OUT: Carry set if not found, clear if found

disk_find_dir_entry:
	pusha
	
	mov dx, si
	
	; Start a directory listing with the type flags given in parameters.
	; The directory should be in the general purpose buffer.
	; List 0 is always used for lists used completely internally by
	; disk_ functions.
	mov si, DISK_BUFFER
	mov bx, DIRLIST_0
	call disk_start_dir_listing
	
.check_entry:
	; Get the new directory entry matching the type flags.
	call disk_get_dir_entry
	jc .not_found
	
	; Check the SFN is matches the one given by the parameters.
	mov di, dx
	mov cx, 11
	repe cmpsb
	; If not, keep searching!
	jne .check_entry
	
	; If so, return a pointer.
	; Remember to reverse the pointer back to the start after the compare.
	sub si, 11
	mov [.entry], si
	
	E9LINE
	popa
	mov di, [.entry]
	clc
	ret
	
.not_found:
	E9LINE
	popa
	; Don't bother returning a pointer if there is no matching entry.
	; Callers should be checking for carry anyway.
	stc
	ret
	
.entry				dw 0
	
	
	
	


; --------------------------------------------------------------------------
; disk_start_dir_listing
; IN: AX = type flags, see DE_TYPE_* flags at the top of the file.
; IN: BX = listing pointer, must be a valid DIRLIST structure.
; IN: SI = pointer to the buffer containing the directory.
; OUT: Nothing

disk_start_dir_listing:
	pusha
	
	; Preset the fields inside the dirlist structure.
	; Don't do anything else yet.
	mov [bx + DIRLIST_OFFSET_FIELD], si	; Directory offset in memory.
	mov [bx + DIRLIST_TYPE_FIELD], ax	; Type flags to search for.
	mov word [bx + DIRLIST_COUNT_FIELD], 0	; Current entry number.

	popa
	ret



; --------------------------------------------------------------------------
; disk_get_dir_entry
; IN: BX = listing pointer
; OUT:  SI = pointer to entry, CF set if no more files available

disk_get_dir_entry:
	pusha
	
	mov bx, [dirlist_type]
	mov si, [dirlist_next]
	
	mov ax, [current_dir_entries]
	mov dx, FAT_DIR_ENTRY_LENGTH
	mul dx
	add ax, DISK_BUFFER
	mov dx, ax
	
	cmp si, DISK_BUFFER
	jl .end_of_list
	
.get_entry:
	cmp si, dx
	je .end_of_list
	
	cmp byte [si], 0
	je .end_of_list
	
	test bx, DE_TYPE_ALL
	jnz .found_entry
	
	mov al, [si + 11]
	
	test al, ATTRIB_LFN
	jnz .next_entry
	
	cmp byte [si], 0xE9
	je .check_deleted_okay
	
.deleted_okay:
	test al, ATTRIB_HIDDEN
	jnz .check_hidden_okay
	
.hidden_okay:
	test al, ATTRIB_SYSTEM
	jnz .check_system_okay
	
.system_okay:
	test al, ATTRIB_DIRECTORY
	jnz .check_subdir_okay
	
.subdir_okay:
	test al, ATTRIB_DIRECTORY
	jz .check_file_okay
	
.file_okay:
.found_entry:
	mov [.tmp], si
	add si, FAT_DIR_ENTRY_LENGTH
	mov [dirlist_next], si
	
	popa
	mov si, [.tmp]
	clc
	ret
	
.next_entry:
	add si, FAT_DIR_ENTRY_LENGTH
	jmp .get_entry
	
.end_of_list:
	popa
	stc
	ret

.check_deleted_okay:
	test bx, DE_TYPE_DELETED
	jnz .deleted_okay
	jmp .next_entry
	
.check_hidden_okay:
	test bx, DE_TYPE_HIDDEN
	jnz .hidden_okay
	jmp .next_entry
	
.check_system_okay:
	test bx, DE_TYPE_SYSTEM
	jnz .system_okay
	jmp .next_entry
	
.check_subdir_okay:
	test bx, DE_TYPE_SUBDIR
	jnz .subdir_okay
	jmp .next_entry
	
.check_file_okay:
	test bx, DE_TYPE_DIR_ONLY
	jz .file_okay
	jmp .next_entry
	
.tmp					dw 0


; --------------------------------------------------------------------------
; disk_read_bpb --- Reads the first sector from the disk into the buffer
; IN: Nothing
; OUT: Carry set if error, otherwise clear

disk_read_bpb:
	pusha
	mov ax, 0
	mov bx, BPB_BUFFER
	mov cx, 1
	mov dx, BPB_BUFFER_SIZE
	call disk_read_sectors
	popa
	ret


; --------------------------------------------------------------------------
; disk_read_fat -- Read FAT from floppy into DISK_BUFFER
; IN: Nothing; OUT: carry set if failure

disk_read_fat:
	pusha
	
	cmp byte [DISK_BUFFER_contents], DISK_BUFFER_HAS_FAT
	je .done
	
	mov ax, [fat_start_sector]
	mov bx, DISK_BUFFER
	mov cx, [fat_sector_count]
	mov dx, DISK_BUFFER_SIZE
	call disk_read_sectors
	
	mov byte [DISK_BUFFER_contents], DISK_BUFFER_HAS_FAT
	
.done:
	popa
	ret

; --------------------------------------------------------------------------
; disk_write_fat -- Save FAT contents from DISK_BUFFER in RAM to disk
; IN: FAT in DISK_BUFFER; OUT: carry set if failure

disk_write_fat:
	pusha
	mov ax, [fat_start_sector]
	mov bx, DISK_BUFFER
	mov cx, [fat_sector_count]
	mov dx, DISK_BUFFER_SIZE
	call disk_write_sectors
	popa
	ret


; --------------------------------------------------------------------------
; disk_read_directory --- Read the current directory's contents from disk.
; IN: nothing
; OUT: Carry set if error, otherwise clear

disk_read_directory:
	pusha
	
	cmp byte [DISK_BUFFER_contents], DISK_BUFFER_HAS_DIR
	je .read_okay

	cmp word [current_dir_cluster], 0
	je .read_root_dir

	mov ax, [current_dir_cluster]
	mov bx, DISK_BUFFER
	call disk_read_fat_chain
	jc .read_failed
	
.read_okay:
	mov byte [DISK_BUFFER_contents], DISK_BUFFER_HAS_DIR
	clc
.read_failed:
	popa
	ret

.read_root_dir:
	mov ax, [root_dir_start_sector]
	mov bx, DISK_BUFFER
	mov cx, [root_dir_sector_count]
	mov dx, DISK_BUFFER_SIZE
	call disk_read_sectors
	
	jnc .read_okay
	jmp .read_failed
	
	


; --------------------------------------------------------------------------
; disk_write_directory -- Write contents of the current directory to disk.
; IN: nothing
; OUT: Carry set if error, otherwise clear

disk_write_directory:
	pusha

	cmp word [current_dir_cluster], 0
	je .write_root_dir

	mov ax, [current_dir_cluster]
	mov bx, DISK_BUFFER
	call disk_write_fat_chain
	popa
	ret

.write_root_dir:
	mov ax, [root_dir_start_sector]
	mov bx, DISK_BUFFER
	mov cx, [root_dir_sector_count]
	mov dx, DISK_BUFFER_SIZE
	call disk_write_sectors
	popa
	ret


; --------------------------------------------------------------------------
; disk_read_sectors --- Reads a given number of sectors from the disk.
; IN: AX = first sector number, BX = memory location
; IN: CX = number of sectors, DX = maximum size in bytes
; OUT: Carry set if error, otherwise clear

disk_read_sectors:
	pusha

	cmp byte [force_disk_reload], 1
	je .failure

	push ax
	mov ax, [bytes_per_sector]
	call disk_fix_size
	pop ax
	
.next_sector:
	push cx
	mov byte [.retries], 4		; Reset the retry count

.try_load:
	push ax
	call disk_convert_l2hts		; Get CHS values for this sector

	mov ah, 02h			; Load the sector
	mov al, 1
	movzx eax, ax
	stc
	int 13h
	jc .retry

	pop ax
	pop cx

	inc ax				; Increase the next sector
	add bx, [bytes_per_sector]	; Increase the memory location

	loop .next_sector		; Decrease count and repeat if needed

	popa				; If all sectors have been read okay
	clc				; return with success.
	ret
	
.retry:
	pop ax
	call disk_reset_floppy		; Reset disk between attempts
	jc .failure			; Bailout if this fails
	dec byte [.retries]		; Remove one retry
	jnz .try_load			; If there are any left, try again

	pop cx				; Otherwise, give up
.failure:
	mov byte [force_disk_reload], 1
	popa
	stc				; Return failure
	ret

.retries 				db 0


; --------------------------------------------------------------------------
; disk_write_sectors --- Writes a given number of sectors to the disk.
; IN: AX = first sector number, BX = memory location
; IN: CX = number of sectors, DX = maximum write size in bytes

disk_write_sectors:
	pusha

	cmp byte [force_disk_reload], 1
	je .failure

	pusha

	mov ax, cx
	mov cx, dx
	mul word [bytes_per_sector]

	; The write size should not be larger than a 64 KiB segment
	cmp dx, 0
	jne .write_too_large

	; The write size should not be larger than given maximum
	cmp ax, cx
	jg .write_too_large

	; The final address should not overflow
	add ax, bx
	cmp ax, bx
	jl .overflow

	popa

.next_sector:
	push cx
	mov byte [.retries], 4		; Reset the retry count

.try_load:
	push ax
	call disk_convert_l2hts		; Get CHS values for this sector

	mov ah, 03h			; Store the sector
	mov al, 1
	movzx eax, ax
	stc
	int 13h
	E9LINE
	jc .retry

	pop ax
	pop cx

	inc ax
	add bx, [bytes_per_sector]

	loop .next_sector

	popa
	clc
	ret
	
.retry:
	pop ax
	dec byte [.retries]
	jnz .try_load

	pop cx
.failure:
	mov byte [force_disk_reload], 1
	popa
	stc
	ret

.overflow:
	mov cx, bx
	not cx
	
.write_too_large:
	; Make the number of sectors written equal to the maximum number
	mov ax, cx
	mov dx, 0
	div word [bytes_per_sector]

	mov [.tmp], ax
	popa
	mov cx, [.tmp]

	jmp .next_sector

.tmp					dw 0
.retries 				db 0


; --------------------------------------------------------------------------
; Reset floppy disk

disk_reset_floppy:
	push ax
	push dx
	mov ax, 0
; ******************************************************************
	mov dl, [bootdev]
; ******************************************************************
	stc
	int 13h
	pop dx
	pop ax
	ret


; --------------------------------------------------------------------------
; disk_convert_l2hts -- Calculate head, track and sector for int 13h
; IN: logical sector in AX; OUT: correct registers for int 13h

disk_convert_l2hts:
	push bx
	push ax

	mov bx, ax			; Save logical sector

	mov dx, 0			; First the sector
	div word [SecsPerTrack]		; Sectors per track
	add dl, 01h			; Physical sectors start at 1
	mov cl, dl			; Sectors belong in CL for int 13h
	mov ax, bx

	mov dx, 0			; Now calculate the head
	div word [SecsPerTrack]		; Sectors per track
	mov dx, 0
	div word [Sides]		; Floppy sides
	mov dh, dl			; Head/side
	mov ch, al			; Track

	pop ax
	pop bx

; ******************************************************************
	mov dl, [bootdev]		; Set correct device
; ******************************************************************

	ret


; --------------------------------------------------------------------------
; disk_get_parameters --- Find the parameter values of the disk current disk
; IN/OUT: nothing

disk_get_parameters:
	pusha

	cmp byte [force_disk_reload], 1
	je .force_reload

	cmp byte [changeline_supported], 1
	jne .no_changeline

	call disk_detect_changeline
	jc .changeline_set

	jmp .done

.force_reload:
	mov byte [force_disk_reload], 0

	call disk_reset_changeline
	call disk_read_bpb
	jc .error

	call disk_make_hash
	jmp .reload_params

.no_changeline:
	E9LINE
	call disk_read_bpb
	jc .error

	E9LINE
	call disk_check_hash
	jnc .done

	E9LINE
	call disk_make_hash
	E9LINE
	jmp .reload_params

.changeline_set:
	call disk_reset_changeline
	call disk_read_bpb
	jc .error

.reload_params:
	mov ah, 0x08
	mov dl, [bootdev]
	push es
	int 13h
	pop es

	; Reset directory to root directory
	mov word [current_dir_cluster], 0

	mov si, DISK_BUFFER
	; Get the number of reserved sectors
	; For hard drives, you may also want to add hidden sectors
	mov cx, [si + 14]
	; The reserved sectors count is equivalent to the 
	; sector of the first FAT
	mov [fat_start_sector], cx

	; Fetch the number of sectors per FAT
	mov ax, [si + 22]
	mov [fat_sector_count], ax
	; Multiply it by the number of FATs
	movzx bx, byte [si + 16]
	mul bx
	; Adding this to the reserved sectors to get the sector
	; of the root directory.
	add ax, cx
	mov [root_dir_start_sector], ax

	; Fetch the number of entrys in the root directory
	mov ax, [si + 17]
	mov [root_dir_entries], ax
	mov [current_dir_entries], ax

	; Retrieve the number of bytes in a sector
	mov ax, [si + 11]
	mov [bytes_per_sector], ax

	; Find the number of sectors in the root directory
	; root_sectors = ((root_entries * 32) + SectorSize - 1) / SectorSize
	mov ax, [root_dir_entries]
	mov cx, FAT_DIR_ENTRY_LENGTH
	mul cx
	add ax, [bytes_per_sector]
	dec ax
	mov dx, 0
	div word [bytes_per_sector]
	mov [root_dir_sector_count], ax

	; Adding first sector of the root directory to the total gives
	; the first data sector.
	add ax, [root_dir_start_sector]
	mov [first_data_sector], ax

	; Retrieve the sectors per cluster
	mov al, [si + 13]
	movzx ax, al
	mov [sectors_per_cluster], ax
	; Multiply it by the number of bytes per sector
	mul word [bytes_per_sector]
	; To get the number of bytes per cluster
	mov [bytes_per_cluster], ax

	; Finally, retrieve the total number of sectors
	mov ax, [si + 19]
	mov [total_sectors], ax

	; Find the number of clusters
	; total = (total_sectors - first_data_sector) / sectors_per_cluster
	mov dx, 0
	sub ax, [first_data_sector]
	div word [sectors_per_cluster]
	mov [total_clusters], ax 

	; That's it!
.done:
	E9LINE
	popa
	clc
	ret

.error:
	E9LINE
	popa
	stc
	ret


; --------------------------------------------------------------------------
; disk_get_fat_entry
; IN: AX = Entry Number
; OUT: BX = Value at entry

disk_get_fat_entry:
	push ax
	push si

	; Find the entry on the table FAT12 table
	; The table consists of 12-bit values
	test ax, 0xF000
	jnz .out_of_range

	; FAT12_entry = FAT_table + (entry * 1.5)
	; entry * 1.5 = entry + entry / 2
	mov si, DISK_BUFFER
	add si, ax
	mov bx, ax
	shr ax, 1
	add si, ax

	; Retrieve a 16-bit value from the table
	mov ax, [si]

	; Part of the 16-bit value is from another entry, this
	; part should be removed. It could be the lowest of highest bits.

	; Check if entry is odd or even
	test bx, 0x0001
	jz .even

.odd:
	; For odd entries, remove the lower bits
	shr ax, 4
jmp .done

.even:
	; For even entries, remove the higher bits
	and ax, 0x0FFF

.done:
	mov bx, ax
	pop si
	pop ax
	ret

.out_of_range:
	mov bx, 0x0FFF
	pop si
	pop ax
	ret
	

; --------------------------------------------------------------------------
; disk_set_fat_entry
; IN: AX = entry number, BX = new value
; OUT: nothing
	
disk_set_fat_entry:
	pusha
	
	test bx, 0xF000
	jnz .out_of_range
	
	; Make sure the value fits into 12 bits
	and bx, 0x0FFF

	; Locate the first byte of the FAT12 entry
	mov si, DISK_BUFFER
	mov cx, ax
	add si, ax
	shr ax, 1
	add si, ax

	; Test if the entry is odd or even
	; Odd numbers have the least significant bit set
	test cx, 0x0001
	jz .even

.odd:
	; With odd entrys the lower 4 bits below to another entry.
	; AND in the higher 12 bits while preserving the lower ones.
	mov ax, [si]
	add ax, 0x000F
	shl bx, 4
	and ax, bx
	mov [si], ax
	jmp .done

.even:
	; With even entrys the upper 4 bits belong to another entry.
	; AND in the lower 12 bits while preserving the higher ones.
	mov ax, [si]
	add ax, 0xF000
	and ax, bx
	mov [si], ax

.done:
.out_of_range:
	popa
	ret
	

; --------------------------------------------------------------------------
; disk_read_cluster --- Reads a FAT12 cluster from the disk
; IN: AX = cluster number, BX = memory location, DX = maximum bytes to read
; OUT: Carry flags set if error, otherwise clear

disk_read_cluster:
	pusha

	; Make sure this cluster exists on disk
	cmp ax, [total_clusters]
	jge .error

	; Find starting sector of the cluster
	; sector = ((cluster - 2) * sectors_per_cluster) + first_data_sector
	push dx
	sub ax, 2
	mul word [sectors_per_cluster]
	add ax, [first_data_sector]
	pop dx

	; Load all sectors in the cluster
	mov cx, [sectors_per_cluster]
	call disk_read_sectors
	popa
	ret

.error:
	popa
	stc
	ret


; --------------------------------------------------------------------------
; disk_write_cluster --- Writes a FAT12 cluster to the disk
; Disk Buffer: Nothing Required
; IN: AX = cluster number, BX = memory location, DX = maximum bytes to write
; OUT: Carry flag set if error, otherwise clear

disk_write_cluster:
	pusha

	cmp ax, [total_clusters]
	jge .error

	mul word [sectors_per_cluster]
	add ax, [first_data_sector]

	mov cx, [sectors_per_cluster]
	call disk_write_sectors
	popa
	ret

.error:
	popa
	stc
	ret

	
; --------------------------------------------------------------------------
; disk_count_free_clusters --- Find the number of free clusters in the FAT
; Disk Buffer: Must contain FAT
; IN: nothing
; OUT: AX = number of free cluster

disk_count_free_clusters:
	push bx
	push cx
	push dx

	mov ax, 0
	mov cx, [total_clusters]
	mov dx, 0

.get_cluster:
	jcxz .done

	call disk_get_fat_entry
	inc ax
	dec cx

	cmp bx, 0
	jne .get_cluster

	inc dx
	jmp .get_cluster

.done:
	mov ax, dx
	pop dx
	pop cx
	pop bx
	ret


; --------------------------------------------------------------------------
; disk_get_free_cluster --- Return the first free cluster
; Disk Buffer: Must contain FAT
; IN: nothing
; OUT: AX = cluster number
; OUT: Carry set if no free clusters, otherwise clear

disk_get_free_cluster:
	push bx
	push cx

	mov ax, 0
	mov cx, [total_clusters]

.get_cluster:
	cmp cx, 0
	je .none_free

	call disk_get_fat_entry
	cmp bx, 0
	je .found_free

	inc ax
	dec cx
	jmp .get_cluster

.found_free:
	pop cx
	pop bx
	clc
	ret

.none_free:
	pop cx
	pop bx
	stc
	ret


; --------------------------------------------------------------------------
; disk_new_dir_entry --- Adds a new file entry to a directory
; IN: AX = filename (FAT format), BX = file size
; IN: CX = attributes, DX = first cluster
; OUT: SI = pointer to new file entry (if success)
; OUT: Carry set if error, otherwise clear

disk_new_dir_entry:
	push bx
	push cx
	push dx
	push di
	push ax

	mov cx, [current_dir_entries]
	mov si, DISK_BUFFER

.find_entry:
	jcxz .none_free

	mov al, [si]

	cmp al, 0xE5
	je .found_free

	cmp al, 0x00
	je .found_free

	dec cx
	add si, FAT_DIR_ENTRY_LENGTH
	jmp .find_entry

.found_free:
	; Set the NT reserved field to zero
	mov byte [si + 12], 0

	; Set the creation and write time, respectively
	call disk_get_time_stamp
	mov [si + 14], ax
	mov [si + 22], ax
	; Set the creation and write date, respectively
	call disk_get_date_stamp
	mov [si + 16], ax
	mov [si + 24], ax
	; Set the access date to zero (not supported)
	mov word [si + 18], 0

	; Set the 1/10 second field for creation time to zero
	mov byte [si + 13], 0

	pop ax
	
	; Copy the filename to the entry
	push si
	mov di, si
	mov si, ax
	mov cx, 11
	rep movsb
	pop si
	pop di

	; Set the first cluster number
	pop dx
	mov [si + 26], dx
	mov word [si + 20], 0

	; Set the file attributes
	pop cx
	mov [si + 11], cl

	; Set the file size
	pop bx
	mov [si + 28], bx
	mov word [si + 30], 0

	clc
	ret

.none_free:
	pop ax
	pop di
	pop dx
	pop cx
	pop bx
	stc
	ret
	

; --------------------------------------------------------------------------
; disk_get_time_stamp --- Creates a FAT timestamp from current system time.
; IN: nothing
; OUT: AX = time stamp

disk_get_time_stamp:
	push bx
	push cx
	push dx

	mov bx, 0
	mov byte [.retries], 3

	; Retrieve the time from the Real Time Clock
	; Might fail if it is currently updating
.try_load_time:
	mov ah, 02h
	clc
	int 1Ah
	jc .retry

	; Convert hours
	movzx ax, ch
	call bcd_to_bin
	and ax, 0x001F
	shl ax, 11
	and bx, ax

	; Convert minutes
	movzx ax, cl
	call bcd_to_bin
	and ax, 0x003F
	shl ax, 5
	and bx, ax

	; Convert seconds
	movzx ax, dh
	call bcd_to_bin
	shr ax, 1	; Accurate to 2 second intervals
	and ax, 0x001F
	and bx, ax

	; Return the new time stamp
	mov ax, bx

	pop dx
	pop cx
	pop bx
	ret

.retry:
	; If the real time clock is busy, try again a few times
	dec byte [.retries]
	jnz .try_load_time

	; After retries have been exceeded just return zero.
	; The real time clock might not be running.
	mov ax, 0
	pop dx
	pop cx
	pop bx
	ret


.retries				db 0
	

; --------------------------------------------------------------------------
; disk_get_date_stamp --- Creates a FAT timestamp from current system date.
; IN: nothing
; OUT: date stamp

disk_get_date_stamp:
	push bx
	push cx
	push dx

	mov bx, 0
	mov byte [.retries], 3

	; Retrieve the date from the Real Time Clock
	; Might fail if it is currently updating
.try_load_date:
	mov ah, 02h
	clc
	int 1Ah
	jc .retry

	; Convert century and year
	mov ax, cx
	call bcd_to_bin
	and ax, 0x007F
	shl ax, 9
	and bx, ax

	; Convert month
	movzx ax, dh
	call bcd_to_bin
	and ax, 0x000F
	shl ax, 5
	and bx, ax

	; Convert day
	movzx ax, dl
	call bcd_to_bin
	and ax, 0x001F
	and bx, ax

	; Return the new time stamp
	mov ax, bx

	pop dx
	pop cx
	pop bx
	ret

.retry:
	; If the real time clock is busy, try again a few times
	dec byte [.retries]
	jnz .try_load_date

	; After retries have been exceeded just return zero.
	; The real time clock might not be running.
	mov ax, 0
	pop dx
	pop cx
	pop bx
	ret


.retries				db 0


; --------------------------------------------------------------------------
; disk_make_fat_chain --- Creates a new FAT chain with a given cluster count.
; IN: AX = cluster count
; OUT: AX = first cluster of chain
; OUT: Carry set if failed, otherwise clear

disk_make_fat_chain:
	push bx
	push cx
	push dx
	
	cmp ax, 0
	je .empty_file

	mov cx, ax

	call disk_get_free_cluster
	jc .disk_full
	mov dx, ax
	mov bx, ax

	dec cx
	jz .done

.add_cluster:
	; Get the next free cluster, may fail if there are no free clusters
	call disk_get_free_cluster
	jc .disk_full

	;  Store the new cluster (in AX) in the old cluster's entry (in BX)
	xchg ax, bx
	call disk_set_fat_entry
	mov ax, bx
	; Now both AX and BX have then number of a new cluster.

	loop .add_cluster

.done:
	mov bx, 0x0FFF
	call disk_set_fat_entry
	mov ax, dx

.empty_file:
	pop dx
	pop cx
	pop bx
	clc
	ret

.disk_full:
	mov ax, 0
	pop dx
	pop cx
	pop bx
	stc
	ret

; --------------------------------------------------------------------------
; disk_remove_fat_chain --- Removes all entries from the given FAT chain.
; IN: AX = first sector of chain
; OUT: nothing

disk_remove_fat_chain:
	pusha

	cmp ax, 0
	je .done

.next:
	; Get the value of the next FAT entry
	call disk_get_fat_entry
	; Remember it and set the current entry to zero (unallocated)
	mov dx, bx
	mov bx, 0
	call disk_set_fat_entry
	
	; If not the end of the chain, continue with next entry
	mov ax, dx
	cmp ax, 0x0FF7
	jb .next

.done:
	popa
	ret
	
	
; --------------------------------------------------------------------------
; disk_count_fat_chain --- Finds the number of sectors in a FAT chain
; Disk Cache: Must contain FAT
; IN: AX = first sector of chain
; OUT: BX = length of FAT chain

disk_count_fat_chain:
	push ax
	push cx
	
	mov cx, 0
	cmp ax, 0
	je .done

.next:
	inc cx
	call disk_get_fat_entry

	mov ax, bx
	
	cmp bx, 0x0FF7
	jb .next

.done:
	mov bx, cx
	pop cx
	pop ax
	ret
	


; --------------------------------------------------------------------------
; disk_read_fat_chain --- Reads all sectors from a FAT chain into memory.
; Disk Buffer: Must contain FAT
; IN: AX = pointer to cluster list, BX = memory location
; IN: CX = start offset, DX = maximum bytes to read
; OUT: CF = set if error, otherwise clear

disk_read_fat_chain:
	pusha
	
	mov si, ax
	
	cmp word [si], 0
	je .done
	

.load_cluster:
	mov ax, [si]
	call disk_read_cluster
	jc .error
	
	add si, CLUSTER_LIST_ENTRY_SIZE

	cmp dx, [bytes_per_cluster]
	jb .done

	add bx, [bytes_per_cluster]
	sub dx, [bytes_per_cluster]
	cmp ax, 0x0FF7
	jb .load_cluster

.done:
	popa
	clc
	ret

.error:
	pop bx
	popa
	stc
	ret


; --------------------------------------------------------------------------
; disk_write_fat_chain --- Write all sectors from a FAT chain into memory.
; IN: AX = first cluster, BX = memory location to write from
; OUT: CF = set if error, otherwise clear

disk_write_fat_chain:
	pusha

	mov si, ax
	
	cmp word [si], 0
	je .done
	mov dx, bx

.write_cluster:
	mov dx, bx
	call disk_write_cluster
	jc .error

	call disk_get_fat_entry
	mov ax, bx

	add dx, [bytes_per_cluster]
	cmp ax, 0x0FF7
	jb .write_cluster

.done:
	popa
	clc
	ret

.error:
	popa
	stc
	ret


; --------------------------------------------------------------------------
; bcd_to_bin --- Convert a 4 digit BCD to a binary value
; IN: AX = BCD value
; OUT: AX = Binary value

bcd_to_bin:
	push bx
	push dx

	mov dx, ax
	
	; Extract the first digit, this will already be in binary
	; num = D1
	and ax, 0x000F
	cmp al, 9
	jg .error
	
	; Extract the second digit
	mov bx, dx
	and bx, 0x00F0

	; num += D2 * 10 = num += D2 * 8 + D2 * 2
	; Starts shifted left by 4 bits (x16), shift right by one (x8)
	shr bx, 1
	add ax, bx
	; Shift by another two and add (x2)
	shr bx, 2
	add ax, bx

	mov bx, dx
	and bx, 0x0F00
	cmp ax, 9
	jg .error

	;num += D3 * 100 = num += D3 * 64 + D3 * 32 + D3 * 4
	shr bx, 2
	add ax, bx
	shr bx, 1
	add ax, bx
	shr bx, 3
	add ax, bx

	mov bx, dx
	and bx, 0xF000
	jg .error

	; num += D4 * 1000 = num += D4 * 1024 - D4 * 16 - D4 * 8
	shr bx, 2
	add ax, bx
	shr bx, 6
	sub ax, bx
	shr bx, 1
	sub ax, bx

	pop dx
	pop bx
	ret

.error:
	mov ax, 0
	pop dx
	pop bx
	ret


; --------------------------------------------------------------------------
; disk_detect_media_change --- Determine if disk has been changed.
; IN: none
; OUT: Set if changed detected, otherwise clear

disk_detect_changeline:
	pusha

	cmp byte [changeline_supported], 0
	je .no_change

	cmp al, 2
	jne .no_change

	mov ah, 16h
	mov dl, [bootdev]
	mov si, 0
	int 13h

	cmp ah, 6
	je .changed

.no_change:
	popa
	clc
	ret

.changed:
	popa
	stc
	ret

; --------------------------------------------------------------------------
; disk_reset_changeline --- Determines changelist support and resets it
; IN: nothing
; OUT: nothing

disk_reset_changeline:
	pusha

	mov ah, 15h
	mov al, 0xFF
	mov cx, 0xFFFF
	mov dl, [bootdev]
	int 13h

	pushf
	; Some disk controllers do not reset correctly without this
	mov ah, 01h
	mov dl, [bootdev]
	int 13h
	popf

	jc .not_supported

	cmp al, 2
	jne .not_supported

	mov byte [changeline_supported], 1
	popa
	ret

.not_supported:
	mov byte [changeline_supported], 0
	popa
	ret


; --------------------------------------------------------------------------
; disk_make_hash --- Create a new hash by summing all word in a sector.
; IN: nothing
; OUT: nothing (disk hash set)

disk_make_hash:
	pusha

	mov cx, [bytes_per_sector]
	shr cx, 1
	mov bx, 0
	mov si, DISK_BUFFER
.sum_word:
	lodsw
	add bx, ax
	loop .sum_word

	mov [current_disk_hash], ax
	popa
	ret

	
; --------------------------------------------------------------------------
; disk_check_hash --- Compares the cached disk hash to the current hash
; IN: nothing
; OUT: Carry set if hash different, otherwise clear

disk_check_hash:
	pusha

	mov cx, [bytes_per_sector]
	shr cx, 1
	mov bx, 0
	mov si, DISK_BUFFER

.sum_word:
	lodsw
	add bx, ax
	loop .sum_word

	cmp bx, [current_disk_hash]
	jne .changed

	popa
	clc
	ret

.changed:
	popa
	stc
	ret


; --------------------------------------------------------------------------
; disk_bytes_to_clusters --- Converts a size in bytes to a cluster count.
; IN: AX = size in bytes
; OUT: AX = number of clusters
; OUT: Carry set if error (data not initialised), otherwise clear

disk_bytes_to_clusters:
	push dx

	cmp word [bytes_per_cluster], 0
	je .error

	add ax, [bytes_per_cluster]
	dec ax
	mov dx, 0
	div word [bytes_per_cluster]

	pop dx
	clc
	ret

.error:
	pop dx
	stc
	ret
	
	
; --------------------------------------------------------------------------
; disk_check_overflow --- Tests if a buffer is large enough for cluster chain.
; IN: AX = first cluster, BX = size of buffer
; OUT: Carry set if buffer too small, otherwise clear
disk_check_overflow:
	pusha

	push bx
	call disk_count_fat_chain
	mul word [bytes_per_cluster]
	mov ax, bx
	pop bx

	cmp dx, 0
	jne .overflow

	cmp ax, bx
	jg .overflow

	clc
	popa
	ret

.overflow:
	stc
	popa
	ret
	
	
; --------------------------------------------------------------------------
; disk_enter_directory --- Uses a directory entry to set the new directory.
; IN: DI = directory entry
; OUT: Carry set if failed, otherwise new directory set

disk_enter_directory:
	pusha

	cmp di, 0
	je .root_dir

	; Test if the entry is a directory
	test byte [di + 11], 0x10
	jz .error

	; Grab the first cluster of the directory entry
	mov ax, [si+26]

	; Make sure a cluster chain actually exists
	cmp ax, 0
	je .error

	mov [current_dir_cluster], ax

	call disk_count_fat_chain
	mov ax, bx
	mul word [bytes_per_cluster]
	shr ax, 5
	mov [current_dir_entries], ax

.done:
	cmp word [current_dir_entries], 256
	jg .too_many_entries

.finish:
	popa
	clc
	ret

.too_many_entries:
	mov word [current_dir_entries], 256
	jmp .finish

.root_dir:
	mov word [current_dir_cluster], 0
	mov ax, [root_dir_entries]
	mov [current_dir_entries], ax

	jmp .done

.error:
	popa
	stc
	ret


; --------------------------------------------------------------------------
; disk_fix_size --- Changes the number of blocks to fit inside a buffer
; IN: AX = block size, BX = load point
; IN: CX = blocks to load, DX = buffer size
; OUT: CX = new block count

disk_fix_size:
	cmp ax, 0
	je .done

	push ax
	push dx

	; Find the size of the load operation
	mul cx
	; If > 64k it is too large
	cmp dx, 0
	pop dx
	ja .overflow

	; Check if it is larger than the assigned buffer
	cmp ax, dx
	ja .overflow

	; Check if the requested load will run past the end of the segment
	not bx
	inc bx

	cmp ax, bx
	ja .overflow

	not bx
	inc bx

	pop ax
.done:
	ret

.overflow:
	E9LINE
	pop ax

	; BX should be the maximum within the segment
	mov cx, bx

	; Check if it is larger than the buffer
	cmp cx, dx
	jbe .find_blocks

	; If it is set the size buffer as the resulting size
	mov cx, dx

.find_blocks:
	; Now convert the size to a number of blocks (round down)
	push dx
	push ax
	xchg ax, cx
	mov dx, 0
	div cx
	mov cx, ax
	pop ax
	pop dx

	not bx
	inc bx

	ret

	




	force_disk_reload		db 1
	changeline_supported		db 0
	current_disk_hash		dw 0
	buffer_contents			db BUFFER_INVALID

	root_dir_start_sector		dw 0
	root_dir_sector_count		dw 0
	root_dir_entries		dw 0
	fat_start_sector		dw 0
	fat_sector_count		dw 0
	bytes_per_sector		dw 0
	bytes_per_cluster		dw 0
	sectors_per_cluster		dw 0
	first_data_sector		dw 0
	total_sectors			dw 0
	total_clusters			dw 0

	current_dir_cluster		dw 0
	current_dir_entries		dw 0
	
	dirlist_next			dw 0
	dirlist_type			dw 0

	Sides dw 2
	SecsPerTrack dw 18
; ******************************************************************
	bootdev db 0			; Boot device number
; ******************************************************************


; ==================================================================


