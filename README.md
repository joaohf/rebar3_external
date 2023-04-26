# rebar3_external

A rebar3 plugin to get external dependencies.

## How to use

Add the plugin to the rebar3.config file:

```
{plugins, [rebar3_external]}.
```

Next, define some external dependencies that your project will use:

```
{deps, [
    % Download external dependency from HTTP URL
    {snappy,
        {external,
            {http, "https://github.com/google/snappy/archive/refs/tags/1.1.9.tar.gz",
                "213b6324b7790d25f5368629540a172c"},
            "c_src", [
                {description, "Google's Snappy compression library"},
                {vsn, "1.1.9"}
            ]}},
    % Clone external dependency from git a repository
    {eleveldb,
        {external, {git, "https://github.com/basho/leveldb", {tag, "2.0.38"}}, "c_src", [
            {description, "leveldb: A key-value store"},
            {vsn, "2.0.38"}
        ]}}
]}.
```

And define some hooks for _compile_ and _clean_. Usually here is where your Makefile is going to be
called:

```
{pre_hooks, [{compile, "make -C c_src compile"}]}.

{post_hooks, [{clean, "make -C c_src clean"}]}.
```

It's done. Now your project is ready to fetch external dependencies. And all rebar3 commands should
work as expected:

```
    $ rebar3 get-deps
```

```
    $ rebar3 compile
```

```
    $ rebar3 clean
```

```
    $ rebar3 tree
```

## But what does it do ?

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
                "213b6324b7790d25f5368629540a172c"},
            "c_src", [
                {description, "Google's Snappy compression library"},
                {vsn, "1.1.9"}
            ]}}
]}.
```

Then, this plugin downloads the snappy source code from github's URL and unpack it into `_build/default/lib/snappy/c_src` folder. Also
it stores the download file into `_build/default/lib/snappy/1.1.9.tar.gz`. _description_  and _vsn_ are used to write
a "fake" .app.src into `_build/default/lib/snappy/src/snappy.src.app`.

It's important to note that the plugins does the full rebar3 dependency management, including updating the `rebar.lock` file.

The basic idea is to provide better external (non OTP dependency) for projects that needs to interface with languages like C, C++, Rust, ...

Please, check out the [rebar3_external_example](https://github.com/joaohf/rebar3_external_example) to get a full example.