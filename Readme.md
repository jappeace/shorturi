
A URL shortner

this serves two routes

+ `/shorten`: submit a url to shorten
+ `/${url}`: follow a shortened url

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
