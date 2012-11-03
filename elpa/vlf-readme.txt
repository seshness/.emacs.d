This package provides the M-x vlf command, which visits part of a
large file in a read-only buffer without visiting the entire file.
The buffer uses VLF mode, which defines the commands M-<next>
(vlf-next-batch) and M-<prior> (vlf-prev-batch) to visit other
parts of the file.  The option `vlf-batch-size' specifies the size
of each batch, in bytes.

This package was inspired by a snippet posted by Kevin Rodgers,
showing how to use `insert-file-contents' to extract part of a
file.
