
**Build and run server**

```
$ stack build && stack exec rlt -- example.conf
```


**Post render job**

```
$ curl \
    -X POST \
    -H "Content-Type: application/json" \
    -d '{}' \
    http://localhost:3000/document?tplId=1

{"docUrl":"/document/6b44c6c1-2c51-4efc-a5f0-705b48dd5213.pdf","docId":"6b44c6c1-2c51-4efc-a5f0-705b48dd5213"}
```

**Get rendered document**

```
$ curl -s -O \
    http://localhost:3000/document/6b44c6c1-2c51-4efc-a5f0-705b48dd5213.pdf

$ ls *.pdf
6b44c6c1-2c51-4efc-a5f0-705b48dd5213.pdf
```
