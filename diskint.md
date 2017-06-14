<a id="top"></a>
# Disk Functions #
* [disk\_string\_to\_sfn](#disk_string_to_sfn)
* [disk\_sfn\_to\_string](#disk_sfn_to_string)
* [disk\_find\_dir\_entry](#disk_find_dir_entry)
* [disk\_read\_bpb](#disk_read_bpb)
* [disk\_read\_fat](#disk_read_fat)
* [disk\_write\_fat](#disk_write_fat)
* [disk\_read\_directory](#disk_read_directory)
* [disk\_write\_directory](#disk_write_directory)
* [disk\_read\_sectors](#disk_read_sectors)
* [disk\_write\_sectors](#disk_write_sectors)
* [disk\_reset\_floppy](#disk_reset_floppy)
* [disk\_convert\_l2hts](#disk_convert_l2hts)
* [disk\_get\_parameters](#disk_get_parameters)
* [disk\_get\_fat\_entry](#disk_get_fat_entry)
* [disk\_set\_fat\_entry](#disk_set_fat_entry)
* [disk\_read\_cluster](#disk_read_cluster)
* [disk\_write\_cluster](#disk_write_cluster)
* [disk\_count\_free\_clusters](#disk_count_free_clusters)
* [disk\_get\_free\_cluster](#disk_get_free_cluster)
* [disk\_new\_dir\_entry](#disk_new_dir_entry)
* [disk\_get\_time\_stamp](#disk_get_time_stamp)
* [disk\_get\_date\_stamp](#disk_get_date_stamp)
* [disk\_make\_fat\_chain](#disk_make_fat_chain)
* [disk\_remove\_fat\_chain](#disk_remove_fat_chain)
* [disk\_count\_fat\_chain](#disk_count_fat_chain)
* [disk\_read\_fat\_chain](#disk_read_fat_chain)
* [disk\_write\_fat\_chain](#disk_write_fat_chain)
* [disk\_detect\_changeline](#disk_detect_changeline)
* [disk\_reset\_changeline](#disk_reset_changeline)
* [disk\_make\_hash](#disk_make_hash)
* [disk\_check\_hash](#disk_check_hash)
* [bcd\_to\_bin](#bcd_to_bin)
* [disk\_check\_filename\_char](#disk_check_filename_char)

---
[back](#top)
<a id="disk_string_to_sfn"></a>

## disk\_string\_to\_sfn ##

#### Inputs ####
* **[AX]**    Filename string

#### Outputs ####
* **[SI]**    Pointer to internal buffer containing SFN.
* **[CF]**    Set if invalid name, otherwise clear

#### Prerequisites ####
* _none_

#### Description ####
Converts a filename from string format to the closest FAT12 short
filename representation. Lowercase letters will be converted to uppercase in
the output data and any invalid character may change the resulting data or
cause the function to fail.

Other functions in the disk subsystem expect to be given filenames in SFN
representation. Thus, this function must be called to translate it.

#### Errors ####
* This function may fail if the filename cannot be represented as a SFN.

#### Example ####
	mov ax, .filename
	call disk_string_to_sfn
	jc .name_error
	...

	.filename   db 'FOO.BAS', 0



---
[back](#top)
<a id="disk_sfn_to_string"></a>

## disk\_sfn\_to\_string ##

#### Inputs ####
* **[AX]**    Pointer to buffer with SFN
* **[SI]**    Output buffer for string. 
  Maximum output length is thirteen bytes, including null terminator. !!!

#### Outputs ####
* _none_

#### Prerequisites ####
* _none_

#### Description ####
Converts a DOS 8.3 Short File Name to a string.

#### Errors ####
* _none_

#### Example ####
	mov ax, .disk_filename
	mov si, .buffer
	call disk_sfn_to_string
	call os_print_string
	...

	.disk_filename db 'FOO     BAR'
	.buffer        FILENAME_BUFFER



---
[back](#top)
<a id="disk_find_dir_entry"></a>

## disk\_find\_dir\_entry ##
Locates a file with a given name inside the current directory.

#### Inputs ####
* **[AX]**    The types of files to search, see [Directory Entry Flags](#de_flags).
* **[SI]**    Pointer to SFN filename.

#### Outputs ####
* **[CF]**    Set if not entry is found, otherwise clear.
* **[DI]**    Pointer to file entry, if an entry is found.

#### Prerequisites ####
* A file directory must be loaded into the disk buffer.

#### Description ####

#### Errors ####
* The given filename was not found in the loaded directory.

#### Example ####
	mov ax, search_file
	call disk_find_dir_entry
	jc .not_found
	mov si, file_exists_msg
	call os_print_string



---
[back](#top)
<a id="disk_read_bpb"></a>

## disk\_read\_bpb ##
Reads the first sector from the disk to the disk buffer.

#### Inputs ####
* _none_

#### Outputs ####
* **[CF]**    Set if a disk error occurs, otherwise clear.

#### Prerequisites ####
* _none_

#### Description ####
Reads the first sector of the filesystem, containing file system parameters.

#### Errors ####
* A read error may occur if the disk has been reset or is not present.

#### Example ####
	call disk_read_bpb
	jc .disk_error

	cmp word [disk_buffer + 510], 0xAA55
	je .is_bootable
	

---
[back](#top)
<a id="disk_read_fat"></a>

## disk\_read\_fat ##
Reads the FAT from the disk and stores it in the disk buffer.

#### Inputs ####
* _none_

#### Outputs ####
* **[CF]**    Set if a disk error occurs, otherwise clear.

#### Prerequisites ####
* The disk parameters must have been loaded 
(see [disk\_get\_parameters](#disk\_get\_parameters).

#### Description ####
Loads all sectors that are part of the filesystem's cluster mapping.

The reported size of the FAT may have been reduced to fit into the disk buffer
when the disk parameters were loaded. This means that the FAT will not be
entirely loaded if it is larger than the disk buffer.

#### Errors ####
 * A read error may occurs when loading a sector in the FAT.

#### Example ####
	call disk_read_fat
	jc .disk_error




---
[back](#top)
<a id="disk_write_fat"></a>

## disk\_write\_fat ##
Writes the FAT stored in the disk buffer to the disk.

#### Inputs ####
* _none_

#### Outputs ####
* **[CF]**    Set if a disk error occurs, otherwise clear.

#### Prerequisites ####
* Disk parameters must have been loaded
  see [disk\_get\_parameters](#disk\_get\_parameters).

#### Description #### 
Writes all sectors given by the disk parameters as being part of the file
allocation table. Sectors are contiguous and start from the disk buffer.

#### Errors ####
* A disk write error may occur when writing any sector.

#### Example ####
	call disk_read_fat
	jc .disk_error

	... add/edit files ...

	call disk_write_fat
	jc .disk_error



---
[back](#top)
<a id="disk_read_directory"></a>

## disk\_read\_directory ##
Reads the current directory data from the disk and stores it in the disk 
buffer.

#### Inputs ####
* _none_

#### Outputs ####
* **[CF]**    Set if disk error, otherwise clear

#### Prerequisites ####
* The disk parameters must be known, via a call to _disk\_read\_parameters_.

#### Description #### 

Loads all clusters in the current directory from the disk and places them in
the disk cache.

The size of the directory is limited to the size of the disk cache, 

If no directory has been specified since the last disk reset or the directory
loaded or at the top level directory, the root directory will be loaded, with
sectors given by the disk parameters.

There is no need to read the FAT before calling this function, as it will read
the FAT if needed.

This function may return without loading any data from the disk if the currect
directory was the last data copied into the disk buffer and no directory change
or disk reset has occurred.

#### Errors ####

#### Example ####



---
[back](#top)
<a id="disk_write_directory"></a>

## disk\_write\_directory ##
Writes the current directory back to the disk.

#### Inputs ####
* _none_

#### Outputs ####
* **[CF]**    Set if disk error, otherwise clear.

#### Prerequisites ####

#### Description ####

#### Errors ####

#### Example ####



---
[back](#top)
<a id="disk_read_sectors"></a>

## disk\_read\_sectors ##
Reads a number of concecutive sectors from the disk.

#### Inputs ####
* **[AX]**    The sector number of the first sector to read.
* **[BX]**    the address of the buffer to output data to.
* **[CX]**    Number of sectors to read.
* **[DX]**    Maximum read size in bytes.

#### Outputs ####
* **[CF]**    Set if disk error, otherwise clear.

#### Prerequisites ####
* _none_

#### Description ####
Reads a given number of sectors from the disk and places them in a buffer. The
sectors read are a concecutive range, the first sector being the sector number
given in AX and the last being the sector number with the number given by
AX + CX - 1.

The number of bytes read depends on the number of bytes per sector but will be
no more than the maximum number of bytes given in DX. If the number of sectors
to be read would equate to a larger number of bytes than is given in DX, the
number of sectors to be read will be decreased to the maximum number that would
fit within the limit. If the maximum bytes to be read is less than the size of
one sector then the read is cancelled but will return success.

#### Errors ####
* If the sectors requested is past the last sector on disk the read will fail
  without being attempted.
* The read will fail if a disk error occurs such as bad sector or missing disk.
* The read will fail if a disk error has previously occurred and no call to
  `disk_init` has been made to reinitalise the disk.

#### Example ####
	; Read the FAT
	mov ax, [fat_start_sector]
	mov bx, disk_buffer
	mov cx, [fat_sector_count]
	mov dx, DISK_BUFFER_SIZE
	call disk_read_sectors
	jc .error



---
[back](#top)
<a id="disk_write_sectors"></a>

## disk\_write\_sectors ##

#### Inputs ####
* **[AX]**    First sector to read.
* **[BX]**    Pointer to read buffer.
* **[CX]**    Number to sectors to read.
* **[DX]**    Maximum bytes to read.

#### Outputs ####

#### Prerequisites ####

#### Description ####

#### Errors ####

#### Example ####



---
[back](#top)
<a id="disk_reset_floppy"></a>

## disk\_reset\_floppy ##
Resets the disk heads.

#### Inputs ####

#### Outputs ####

#### Prerequisites ####

#### Description ####

#### Errors ####

#### Example ####



---
[back](#top)
<a id="disk_convert_l2hts"></a>

## disk\_convert\_l2hts ##
Converts a logical sector to CHS format

#### Inputs ####
* **[AX]**    Logical sector

#### Outputs ####


#### Prerequisites ####

#### Description ####

#### Errors ####

#### Example ####



---
[back](#top)
<a id="disk_get_parameters"></a>

## disk\_get\_parameters ##
Collect information about the current disk for other functions.

#### Inputs ####
* _none_

#### Outputs ####
* _none_

#### Prerequisites ####
* _none_

#### Description ####

#### Errors ####

#### Example ####



---
[back](#top)
<a id="disk_get_fat_entry"></a>

## disk\_get\_fat\_entry ##

#### Inputs ####
* **[AX]**    Cluster Number

#### Outputs ####
* **[BX]**    Data value at cluster

#### Prerequisites ####
* The cluster table must have been loaded.

#### Description ####

#### Errors ####
* An end of chain marker is returned if the cluster is out of range.

#### Example ####



---
[back](#top)
<a id="disk_set_fat_entry"></a>

## disk\_set\_fat\_entry ##
Sets a value in the FAT table.

#### Inputs ####
* **[AX]**    Entry number
* **[BX]**    Value to set

#### Outputs ####
* _none_

#### Prerequisites ####
* The FAT must have been loaded.

#### Description ####

#### Errors ####

#### Example ####



---
[back](#top)
<a id="disk_read_cluster"></a>

## disk\_read\_cluster ##
Reads a FAT cluster from the disk.

#### Inputs ####
* **[AX]**    Cluster number to read.
* **[BX]**    Pointer to read buffer.
* **[DX]**    Maximum number of bytes to read.

#### Outputs ####
* **[CF]**    Set if disk error, otherwise clear.

#### Prerequisites ####

#### Description ####

#### Errors ####

#### Example ####



---
[back](#top)
<a id="disk_write_cluster"></a>

## disk\_write\_cluster ##
Write a FAT cluster to the disk.

#### Inputs ####
* **[AX]**    Cluster number to write.
* **[BX]**    Pointer to data buffer.
* **[DX]**    Maximum number of bytes to write.

#### Outputs ####
* **[CF]**    Set if disk error, otherwise clear.

#### Prerequisites ####

#### Description ####

#### Errors ####

#### Example ####



---
[back](#top)
<a id="disk_count_free_clusters"></a>

## disk\_count\_free\_clusters ##
Count the number of free data clusters.

#### Inputs ####
* _none_

#### Outputs ####
* **[AX]**    Number of free clusters.

#### Prerequisites ####

#### Description ####

#### Errors ####

#### Example ####



---
[back](#top)
<a id="disk_get_free_cluster"></a>

## disk\_get\_free\_cluster ##
Find the first unused data cluster.

#### Inputs ####
* _none_

#### Outputs ####
* **[AX]**    The cluster number of a free cluster.
* **[CF]**    Set if no free clusters.

#### Prerequisites ####

#### Description ####

#### Errors ####

#### Example ####



---
[back](#top)
<a id="disk_new_dir_entry"></a>

## disk\_new\_dir\_entry ##
Create a new directory entry.

#### Inputs ####
* **[AX]**    Pointer to SFN to use as filename.
* **[BX]**    File size of new entry.
* **[CX]**    File attributes of new entry (see [File Attributes](#file_attrib)).
* **[DX]**    First data cluster of new entry.

#### Outputs ####
* **[SI]**    Pointer to new file entry.
* **[CF]**    Set if new entry cannot be created.

#### Prerequisites ####

#### Description ####

#### Errors ####

#### Example ####



---
[back](#top)
<a id="disk_get_time_stamp"></a>

## disk\_get\_time\_stamp ##
Generates a FAT formatted time stamp from the current time.

#### Inputs ####
* _none_

#### Outputs ####
* **[AX]**    Time stamp value.

#### Prerequisites ####

#### Description ####

#### Errors ####

#### Example ####



---
[back](#top)
<a id="disk_get_date_stamp"></a>

## disk\_get\_date\_stamp ##
Generates a FAT formatted date stamp from the current date.

#### Inputs ####
* _none_

#### Outputs ####
* **[AX]**    Date stamp value.

#### Prerequisites ####

#### Description ####

#### Errors ####

#### Example ####



---
[back](#top)
<a id="disk_make_fat_chain"></a>

## disk\_make\_fat\_chain ##
Allocate a given number of linked clusters in the FAT.

#### Inputs ####
* **[AX]**    Number of clusters to allocate.

#### Outputs ####
* **[AX]**    The first cluster allocated.

#### Prerequisites ####

#### Description ####

#### Errors ####

#### Example ####



---
[back](#top)
<a id="disk_remove_fat_chain"></a>

## disk\_remove\_fat\_chain ##
Deallocates a cluster and all clusters linked to it.

#### Inputs ####
* **[AX]**    First cluster to remove.

#### Outputs ####
* _none_

#### Prerequisites ####

#### Description ####

#### Errors ####

#### Example ####



---
[back](#top)
<a id="disk_count_fat_chain"></a>

## disk\_count\_fat\_chain ##
Counts the number of clusters in a FAT chain.

#### Inputs ####
* **[AX]**    The first sector of the chain.

#### Outputs ####
* **[BX]**    The total number of sector in the chain.

#### Prerequisites ####

#### Description ####

#### Errors ####

#### Example ####



---
[back](#top)
<a id="disk_read_fat_chain"></a>

## disk\_read\_fat\_chain ##
Reads data from clusters in a FAT chain.

#### Inputs ####
* **[EAX]**     First cluster to read.
* **[ES:BX]**   Pointer to read buffer.
* **[ECX]**     Byte offset in FAT chain to start at.
* **[EDX]**     Maximum number of bytes to read.

#### Outputs ####
* **[CF]**      Set if read error, otherwise clear.

#### Prerequisites ####

#### Description ####

#### Errors ####

#### Example ####



---
[back](#top)
<a id="disk_write_fat_chain"></a>

## disk\_write\fat\_chain ##
Writes data to cluster in a FAT chain.

#### Inputs ####
* **[EAX]**     First cluster to write.
* **[ES:BX]**   Pointer to data buffer.
* **[ECX]**     Byte offset in FAT chain to start at.
* **[DX]**     Maximum bytes to write.

#### Outputs ####
* **[CF]**      Set if write error, otherwise clear.

#### Prerequisites ####

#### Description ####

#### Errors ####

#### Example ####



---
[back](#top)
<a id="disk_detect_changeline"></a>

## disk\_detect\_changeline ##
Detects if the disk has change (if supported).

#### Inputs ####
* _none_

#### Outputs ####
* **[CF]**      Set if disk change, otherwise clear

#### Prerequisites ####

#### Description ####

#### Errors ####

#### Example ####



---
[back](#top)
<a id="disk_reset_changeline"></a>

## disk\_reset\_changeline ##
Detect changeline support and clears the disk change bit.

#### Inputs ####
* _none_

#### Outputs ####
* _none_

#### Prerequisites ####

#### Description ####

#### Errors ####

#### Example ####



---
[back](#top)
<a id="disk_make_hash"></a>

## disk\_make\_hash ##

#### Inputs ####
* _none_

#### Outputs ####
* _none_

#### Prerequisites ####
* At least one sector must be loaded into the disk buffer.

#### Description ####
Finds a hash by adding together the amount of 2-byte words equal to one sector
from the disk buffer.

#### Errors ####
* _none_

#### Example ####
	call os_load_bpb
	jc .error
	call os_make_hash



---
[back](#top)
<a id="disk_check_hash"></a>

## disk\_check\_hash ##

#### Inputs ####
* _none_ 

#### Outputs ####
* **[CF]**    Set if hash does not match the last hash created, otherwise clear.

#### Prerequisites ####
* At least one sector must be loaded into the disk buffer.

#### Description ####
Finds a hash by adding together the amount of 2-byte words in one sector and
compares it to the last has made with disk_make_hash. The carry flags is set if
the hashes differ. The new hash is discarded and not saved.

#### Errors ####
* _none_

#### Example ####
	call os_load_bpb
	jc .error
	call os_check_hash
	jnc .same

	mov si, hash_change_msg
	call os_print_string



---
[back](#top)
<a id="bcd_to_bin"></a>

## bcd\_to\_bin ##
Converts a BCD value into binary.

#### Inputs ####
* **[AX]**    BCD number, up to four digits.

#### Outputs ####
* **[AX]**    Binary value.

#### Prerequisites ####
* _none_

#### Description ####
Converts a BCD number containing up to four BCD digits in AX into a binary 
number. If a valid BCD number is given, the resulting number will be returned 
in the same register zero if an error occurs processing any of the BCD digits.

#### Errors ####
* If any digit of the BCD number is invalid, zero will be returned.

#### Example ####
	; Print a BCD value
	mov ax, 0x1234
	call bcd_to_bin
	call os_int_to_string
	mov si, ax
	call os_print_string
	

---
[back](#top)
<a id="disk_check_filename_char">

## disk\_check\_filename\_char ##
Detects if a character can be used in an SFN.

#### Inputs ####
* **[AL]**    Character to check

#### Outputs ####
* **[CF]**    Set if character invalid, otherwise clear

---
[back](#top)
<a id="disk_start_dir_listing"></a>

## disk\_start\_dir\_listing ##
Prepares to list files from a directory.

#### Inputs ####
* **[AX]**     The types of files to list (see [Directory Entry Flags](#de_flags))
* **[SI]**     A pointer to the first file entry.

#### Outputs ####
* _none_

#### Prerequisites ####
* _none_

#### Description ####


#### Errors ####


#### Example ####


---
[back](#top)
<a id="disk_get_dir_entry"></a>

## disk\_get\_dir\_entry ##
Retrieves a file entry from the current directory listing.

#### Inputs ####
* _none_

#### Outputs ####
* **[SI]**     A pointer to the file entry. Valid only is CF is clear.
* **[CF]**     Set if list if empty, otherwise clear.

#### Prerequisites ####
* _none_

#### Description ####


#### Errors ####


---
[back](#top)
<a id="disk_bytes_to_clusters"></a>

## disk\_bytes\_to\_clusters ##
Calculates the number of clusters required to hold the given number of bytes.

#### Inputs ####
* **[AX]**      Number of bytes required.

#### Outputs ####
* **[AX]**      Number of clusters needed.
* **[BX]**      Bytes used in last cluster.

#### Prerequisites ####
* _none_

#### Description ####


#### Errors ####


#### Example ####


---
[back](#top)
<a id="disk_check_overflow"></a>

## disk\_check\_overflow ##
Determines if buffer is large enough to support the given FAT chain.

#### Inputs ####
* **[EAX]**     First Cluster
* **[BX]**      Number of bytes in buffer.

#### Outputs ####
* **[CF]**      Set if overflow, otherwise clear.

#### Prerequisites ####
* _none_

#### Description ####


#### Errors ####


#### Example ####


---
[back](#top)
<a id="disk_enter_directory"></a>

## disk\_enter\_directory ##
Change the current directory to a subdirectory of the current directory.

#### Inputs ####
* **[DI]**      Pointer to the directory entry to the subdirectory or zero for root directory.

#### Outputs ####
* **[CF]**      Set if directory change failed, otherwise clear.

#### Prerequisites ####
* _none_

#### Description ####


#### Errors ####


#### Example ####


---
[back](#top)
<a id="disk_fix_size"></a>

## disk\_fix\_size ##
Finds the largest number of blocks that will fit in a buffer.

#### Inputs ####
* **[AX]**      Size of each block.
* **[BX]**      Load point for first block.
* **[CX]**      Number of block to load.
* **[DX]**	Size of buffer to load to.

#### Outputs ####
* **[CX]**      Maximum blocks for buffer.
* **[DX]**      Remaining space after last block.

#### Prerequisites ####
* _none_

#### Description ####


#### Errors ####


#### Example ####

---
[back](#top)
<a id="template"></a>

## Template ##
(short description)

#### Inputs ####
* _none_

#### Outputs ####
* _none_

#### Prerequisites ####
* _none_

#### Description ####


#### Errors ####


#### Example ####


---
[back](#top)
<a id="de_flags"></a>

## Directory Entry Flags ##
Used to indicate the type required when searching a directory.

### DE\_TYPE\_DELETED ###
Files that have been deleted but not overwritten are including in the listing.
This may be useful for a function that recovers deleted files.

### DE\_TYPE\_HIDDEN ###
Files that have been set as hidden. These files should not show up on normal
listing but can be read and written to.

### DE\_TYPE\_SYSTEM ###
Files that have been marked 

### DE\_TYPE\_SUBDIR ###
Entries that have are subdirectories of the current directory are included in 
the listing. This is
