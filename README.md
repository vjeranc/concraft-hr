Concraft-hr
===========

This package provides a morphosyntactic tagger for the Croatian language.
It is possible with a few tweaks to make this tagger configurable, so that it can work on any language (future work).

The tool combines the following components into a pipeline:
* A morphosyntactic analyzer constructed using [moan][moan],
* A morphosyntactic disambiguation library [Concraft][concraft],

The package is the result of the adaptation of the Polish tagger [Concraft-pl][concraft-pl]. All of the code (even this README) has been taken and rewritten to match the needs, but the functionality has stayed mostly the same.

Installation
============

You will need [Glasgow Haskell Compiler (GHC)][ghc] and the [Cabal][cabal] tool to build Concraft-hr.  The easiest way to get both [GHC][ghc] and [Cabal][cabal] is to install the latest [Haskell Platform][haskell-platform].

Unless you plan to use a custom preprocessing pipeline that would provide the set of possible tags for each word in a sentence you should use an already constructed analyzer from the [moan][moan] library. On the library page there is a short description of how to construct it with available data.

To install Concraft-hr from the official [Hackage repository][hackage-repo] just run:

    cabal install concraft-hr

The `concraft-hr` tool will be installed in the `~/.cabal/bin` directory by default.

If you want to upgrade Concraft-hr to a newer version you should update the package list first:

    cabal update
    cabal install concraft-hr

To install the latest development version from github just run

    cabal install

from the `concraft-hr` toplevel directory.


Data format
===========

There isn't any tokenization involved, it is expected that input is fully tokenized and that sentences are separated with more than one whitespace.


Training
========

If you have the training material with disambiguation annotations you can train the Concraft-hr model yourself.

    concraft-hr train train.plain -e eval.plain -o model.gz

Concraft-hr uses the [Multext East v4][MultextEast] [morphosyntactic tagset definition](config/tagset.cfg) by default.  It will also reanalyse the input data before the actual training.  If you want to change this behaviour, use the `--tagset` and `--noana` command-line options. For many more options (training paramaters) type `--help`.

Consider using [runtime system options][ghc-rts].  You can speed up processing
by making use of multiple cores by using the `-N` option.  The `-s` option will
produce the runtime statistics, such as the time spent in the garbage collector.
If the program is spending too much time collecting garbage, you can try to
increase the allocation area size with the `-A` option.  If you have a big
dataset and it doesn't fit in the computer memory, use the `--disk` flag.
For example, to train the model using four threads and 256M allocation area
size, run:

    concraft-hr train train.plain -e eval.plain -o model.gz +RTS -N4 -A256M -s

Run `concraft-hr train --help` to learn more about the program arguments and
possible training options.

Finally, you may consider pruning the resultant model in order to reduce its size.
Features with values close to 0 (in log-domain) have little effect on the modeled
probability and, therefore, it should be safe to discard them.

    concraft-hr prune -t 0.05 input-model.gz pruned-model.gz


Tagging
=======

Once you have a Concraft-hr model you can use the following command tag `input.txt` file:

    concraft-hr tag model.gz < input.txt > output.plain

The input file should be tokenized and sentences separated by more than one whitespace. For each word one can put the list of possible tags in the row of the word, or just leave it to the analyzer. After that the output is written to the ```stdout```.

With the `--marginals` option enabled, Concraft-hr will output marginal probabilities corresponding to individual tags (determined on the basis of the disambiguation model) instead of the correct tag. Output format is similar to the input. Each word is in its own row, and all of the probabilities and tags in the same row with the word separated by whitespace.

Run `concraft-hr tag --help` to learn more about possible tagging options.


Server
======

Concraft-hr provides also a client/server mode.  It is handy when, for example,
you need to tag a large collection of small files (it might be possible to have a long TCP connection and lazily tag the input - future work). Loading Concraft-hr model from a disk takes considerable amount of time which makes the tagging method described above very slow in such a setting.

To start the Concraft-hr server, run:

    concraft-hr server --inmodel model.gz --inanalyzer analyzer.gz

You can supply a custom port number using a `--port` option.  For example,
to run the server on the `10101` port, use the following command:

    concraft-hr server --inmodel model.gz --inanalyzer analyzer.gz --port 10101

To use the server in a multi-threaded environment, you need to specify the
`-N` [RTS][ghc-rts] option.  A set of options which usually yields good
server performance is presented in the following example:

    concraft-hr server --inmodel model.gz +RTS -N -A4M -qg1 -I0

Run `concraft-hr server --help` to learn more about possible server-mode options.

The client mode works just like the tagging mode.  The only difference is that, instead of supplying your client with a model, you need to specify the port number (in case you used a custom one when starting the server; otherwise, the default port number will be used).

    concraft-hr client --port 10101 < input.txt > output.plain

Run `concraft-hr client --help` to learn more about possible client-mode options.


[concraft]: https://github.com/kawu/concraft "Concraft"
[concraft-pl]: https://github.com/kawu/concraft-pl "Concraft-pl"
[hackage-repo]: http://hackage.haskell.org/package/concraft-hr "Concraft-hr Hackage repository"
[moan]: https://github.com/vjeranc/moan "moan"
[ghc]: http://www.haskell.org/ghc "Glasgow Haskell Compiler"
[ghc-rts]: http://www.haskell.org/ghc/docs/latest/html/users_guide/runtime-control.html "GHC runtime system options"
[cabal]: http://www.haskell.org/cabal "Cabal"
[haskell-platform]: http://www.haskell.org/platform "Haskell Platform"

