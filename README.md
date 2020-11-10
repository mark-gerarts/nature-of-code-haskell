# Nature of Code - Haskell edition

[Nature of Code][noc] examples and exercises implemented in Haskell using
[gloss][gloss].

## Running examples manually

Run each sketch individually using the following command:

```
$ stack runghci src/<chapter>/<name>/Sketch.hs
```

E.g. to run the first example:

```
$ stack runghci src/Introduction/TraditionalRandomWalk/Sketch.hs
```

The README of each sketch provides its full command as well, ready to be
copy-pasted.

## Overview

- [Introduction](src/Introduction/)
  - [Exercise I.1: Traditional Random Walk](src/Introduction/TraditionalRandomWalk/Sketch.hs)

## Recordings

Recordings of the sketches are made using [SimpleScreenRecorder][ssr], as it
allows to directly record an OpenGL application.

The recording is saved as an mp4, and then converted to gif using ffmpeg:

```
$ ffmpeg -i sketch.mp4 sketch.gif
```

## Related

This is something I like to do to gain a better understanding of a language.
See also:

- [Nature of Code - Lisp edition][noc-lisp]
- [Nature of Code - Elm edition][noc-elm]

[noc]: http://natureofcode.com/
[gloss]: http://gloss.ouroborus.net/
[ssr]: https://www.maartenbaert.be/simplescreenrecorder/
[noc-lisp]: https://github.com/mark-gerarts/nature-of-code
[noc-elm]: https://github.com/mark-gerarts/nature-of-code-elm
