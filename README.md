# Eric
Erlang IRC Client

![Example](https://i.cloudup.com/uhlBCVfF6YK/aTinFt.gif)

## Build

```
$ make build
```

## Tests

```
$ make test
```

## Usage

### As a library

``` erlang
application:start(eric).
eric:connect().
eric:join("#channel").
eric:msg("#channel", "Hello world").
eric:nick("EricII").
eric:whois("eric").
```

### As a client

```
$ make run
erl -pa deps/*/ebin/ -pa ebin/ -eval "application:start(eric)"
Erlang/OTP 17 [erts-6.0] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V6.0  (abort with ^G)
1> eric:connect().
```

## The MIT License (MIT)

Copyright (c) 2014 Julian Duque

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation nfiles (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
