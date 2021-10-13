
A URL shortner

this serves two routes

+ `/shorten`: submit a url to shorten
+ `/${url}`: follow a shortened url

## TODO 

I think this is a reasonable prototype,
but as discussed they prefer having it set in stone.

+ [ ] Ensure correct length
+ [ ] Do some checking on validations?
+ [ ] Make tests pass.
+ [ ] Write stubout tests.
+ [ ] write json versioning tests.

## Usage

```
nix-shell
make run
```

in a different terminal

```
make get
make post
```
### Tools
Enter the nix shell.
```
nix-shell
```
You can checkout the makefile to see what's available:
```
cat makefile
```

### Running
```
make run
```

### Fast filewatch which runs tests
```
make ghcid
```
