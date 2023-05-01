# rebar3_external

[![Erlang/OTP Version](https://img.shields.io/badge/erlang%2Fotp-%2024-blue)](http://www.erlang.org)
[![Hex Version](https://img.shields.io/hexpm/v/rebar3_external.svg)](https://hex.pm/packages/rebar3_external)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/rebar3_external/)
[![Total Download](https://img.shields.io/hexpm/dt/rebar3_external.svg)](https://hex.pm/packages/rebar3_external)
[![License](https://img.shields.io/hexpm/l/rebar3_ex_doc.svg)](https://github.com/joaohf/rebar3_external/blob/main/LICENSE)
[![Last Updated](https://img.shields.io/github/last-commit/joaohf/rebar3_external.svg)](https://github.com/joaohf/rebar3_external/commits/main)

A rebar3 plugin to get external dependencies.

## How to use

Add the plugin to the rebar3.config file:

```
{plugins, [rebar3_external]}.
```

Next, define some external dependencies that your project will fetch:

```
{deps, [
    % Download external dependency from HTTP URL
    {snappy,
        {external,
            {http, "https://github.com/google/snappy/archive/refs/tags/1.1.9.tar.gz",
                {md5, "213b6324b7790d25f5368629540a172c"}},
            "c_src", [
                {description, "Google's Snappy compression library"},
                {vsn, "1.1.9"}
            ]}},
    % Clone external dependency from git a repository
    {leveldb,
        {external, {git, "https://github.com/basho/leveldb", {tag, "2.0.38"}}, "c_src", [
            {description, "leveldb: A key-value store"},
            {vsn, "2.0.38"}
        ]}}
]}.
```

It is possible to declare two types of external dependencies:

 * An external HTTP(s) URL pointing to a package file like `tar.gz`, `tar.bz2`, `tar.xz`. It is mandatory to specify a checksum hex digest which
 could be a md5 or sha256
 * A git repository where is possible to use the same git syntax when [declaring a git rebar3](http://rebar3.org/docs/configuration/dependencies/)

For both declaration, it is also necessary to add two extra information like:

 * The destination folder, where the external resource will be fetched. It could be a folder like `c_src` or any other.
 However, it is important to note that your custom makefiles should point to the same folder later
 * A property list having `description` and `vsn` properties specifying a small description and its version  

Also, it is necessary to define some hooks for _compile_ and _clean_. Usually here is where your Makefile is going to be
called:

```
{pre_hooks, [{compile, "make -C c_src compile"}]}.

{post_hooks, [{clean, "make -C c_src clean"}]}.
```

That is all the rebar3 configuration needed. The next step is to change the c_src/Makefile (or whatever you are using as build tool)
to use the fetched source code. By default rebar3 exports an environment variable called: _REBAR_DEPS_DIR_ and it has the full path
to the dependency folder with the current profile being used. So, in the Makefile we have to use that variable like the following:

```
LEVELDB_SRC := $(REBAR_DEPS_DIR)/leveldb/c_src
```

That way the Makefile will include the correct path.

It's done. Now your project is ready to fetch external dependencies. And all rebar3 commands should
work as expected:

  * `rebar3 get-deps`
  * `rebar3 compile`
  * `rebar3 clean`
  * `rebar3 tree`

## How does it work ?

This plugin is a follow up for [Implement hooks for get-deps commands](https://github.com/erlang/rebar3/issues/2784).
The intend is to provide a plugin which fetches external dependency (non OTP like dependency, like a library in C for example); so
a project can declare any external dependency and get them fetched. For each dependency the plugins creates a
"fake" OTP application layout, that is, writing an .src.app into `src` folder. That way rebar3 "thinks" that the
dependency is a real one (this idea is not new and I got it from plugins like [rebar_raw_resosurce](https://github.com/basho/rebar_raw_resource)).

For example, if you declare a dependency such as:

```
{deps, [
    {snappy,
        {external,
            {http, "https://github.com/google/snappy/archive/refs/tags/1.1.9.tar.gz",
                {md5, "213b6324b7790d25f5368629540a172c"}},
            "c_src", [
                {description, "Google's Snappy compression library"},
                {vsn, "1.1.9"}
            ]}}
]}.
```

Then, this plugin downloads the snappy source code from github's URL and unpack it into `_build/default/lib/snappy/c_src` folder. Also
it stores the download file into `_build/default/lib/snappy/1.1.9.tar.gz`. _description_  and _vsn_ are used to write
a "fake" .app.src into `_build/default/lib/snappy/src/snappy.src.app`. Moreover, the plugin also stores the downloaded package checksum into a file
`_build/default/lib/snappy/1.1.9.tar.gz.checksum`, that is necessary only for `http` resources.

It's important to note that the plugins does the full rebar3 dependency management, including updating the `rebar.lock` file.

The basic idea is to provide better external (non OTP dependency) for projects that needs to interface with languages like C, C++, Rust, ...

Please, check out the [rebar3_external_example](https://github.com/joaohf/rebar3_external_example) to check full working example.