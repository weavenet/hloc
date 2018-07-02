# hloc (Haskell Lines Of Code)

Program for couting lines of code in common languages

## Usage

Output comments in a file

```
$ hloc -l bash /tmp/test.sh
# this is a test
```

Toggle output to only show non-commented lines

```
$ hloc -l bash -t /tmp/test.sh
echo "Hello World"
```

Show count of comments, blannk lines and total lines of code in file

```
$ hloc -c /tmp/test.sh
       1       1       3 /tmp/test.sh (bash)
```

Show count of comments in a directory

```
$ find /target_dir -type f | xargs hloc -c
-c
       3       3      11 /tmp/2/test2.sh (bash)
       2       3       7 /tmp/2/test3.sh (bash)
       9       7      25 /tmp/2/test.php (php)
       1       1       3 /tmp/2/test1.sh (bash)
```

Show supported languages

```
$ hloc -L
Supported languages and corresponding extensions:

bash: sh, bash
java: java
php: php
```

## Adding New Languages

New languages are defined by adding them to **Hloc.hs**.

You will need to define the following for a new language

* single line comment characters
* multi line comment pairs
* exceptions
* file extensions

Submit a PR to update [languages](https://github.com/weavenet/hloc/blob/8338a1a0b84403e0f9fe1f666fbe87411bb1a35b/Hloc.hs#L39-L50) as well as adding a test to **languageTests** in **Test.hs**.

## Build

```
stack build
```

## Test

```
stack test
```
