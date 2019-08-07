# protobluff

`protobluff` is a functional scala parser for [protocol buffers
v3][protov3].

## Goals

- parse protobuf v3 specs from strings
- not need to call command line tools


## Non-Goals

- parse non-v3 protobuf (maybe in the future?)
- talk to the filesystem
- parse multiple _files_ at once
- do any kind of post-processing of the AST.
- resolve names
- materialize dependency graph between protobuf specs

[protov3]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec
[Skeuomorph]: https://github.com/higherkindness/skeuomorph
