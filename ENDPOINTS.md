## POST /calculate

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- This should be just JSON-encoded strnig with an article text (`application/json;charset=utf-8`, `application/json`):

```javascript
"Dad is riding a bike"
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Probabilities for each subject (`application/json;charset=utf-8`, `application/json`):

```javascript
[["subj1",0.3],["subj2",0.8]]
```

## GET /subjects

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- This is just a number of articles per subject (`application/json;charset=utf-8`, `application/json`):

```javascript
[["subj1",3],["subj2",5]]
```

## POST /update

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Article Object with text and a list of subjects (`application/json;charset=utf-8`, `application/json`):

```javascript
{"articleSubjects":["subj1","subj2"],"articleText":"Dad is riding a bike"}
```

### Response:

- Status code 204
- Headers: []

- No response body

