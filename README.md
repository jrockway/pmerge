`pmerge` - merge multiple input streams into one output stream

Example:

Imagine you have three input sources; a "control source" on STDIN, and
two "data soruces", as named pipes (fifos), `a.fifo` and `b.fifo`.  To
merge these streams, invoke pmerge like:

    $ mkfifo a.fifo b.fifo
    $ pmerge a.fifo b.fifo

When you type the first line into STDIN, pmerge echoes it back to you:

    > Hello, world!
     Hello, world!

(A note about the examples; `>` represents input on pmerge's STDIN,
`$` represents shell commands, and anything else represents the output
of pmerge on STDOUT.)

When a.fifo becomes readable and an entire line is consumed, pmerge
merges the data from STDIN with the new data from a.fifo (and any
existing data on b.fifo):

    $ echo "This is A" > a.fifo
     Hello, world.

Right now, this is a "no op", since the "merge specification" is read
from STDIN, and the data on STDIN didn't specify anything to merge
(with `[_#]` margers).  So in this case, whatever was in STDIN's
"buffer" before is printed again.  If we change the data in STDIN's
buffer to something with a merge spec, the merge is actually
performed:

    > This will merge A into here -> "[_1]"
     This will merge A into here -> "This is A"

Now when we write to a.fifo again:

    $ echo "This is new data on A" > a.fifo
    This will merge A into here -> "This is new data on A"

We can change the spec to include b.fifo, and watch the result change:

    > A: [_1], B: [_2]
     A: This is new data on A, B:
    $ echo "A" > a.fifo
     A: A, B:
    $ echo "B" > b.fifo
     A: A, B: B
    > B: [_2], A: [_1]
     B: B, A: A

So basically, whenever any new data or format information arrives,
pmerge immediately merges that data and prints the result.  This is
slightly different than "tail -f" on multiple files; as tail -f
monitors the files and prints lines, but doesn't keep any state.
pmerge remembers the last line read from each pipe, and uses all of
that data to compute the output.


