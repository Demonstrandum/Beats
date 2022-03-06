# Beats

Track your BPM.

## Run

First, update your package list:
```sh
cabal update
```

Then to run the program, run:
```sh
cabal run Beats
```

To install locally, run:
```sh
cabal install
```
and make sure the install location is in your `$PATH`.

## TODO
- [ ] Space-bar is broken, tapping the space bar gives a super-high BPM.
- [ ] Resizing works, but resetting (r) after resizing assumes the windows was never resized.
- [ ] Adjust axis automatically based on how fast your BPM is.
- [ ] Native Wayland support.
- [ ] Better contrast.

## Looks like

Right now it looks like this:
![screenshot of application being poorly used](https://user-images.githubusercontent.com/26842759/156935552-3825262a-7b9e-4ceb-a776-8c2a59620268.png)
