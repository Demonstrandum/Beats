# Beats

Track your BPM.

## Run

First, update your package list
```sh
caba update
```

Then to run the program, run
```sh
cabal run Beats
```

To install locally, run
```sh
cabal install
```
and make sure the install location is in your `$PATH`.

## TODO
- [ ] Space-bar is broken, tapping the space bar gives a super-high BPM.
- [ ] Resizing works, but resetting (r) after resizing assumes the windows was never resized.
- [ ] Adjust axis automatically based on how fast your BPM is.
