## Welcome

This is the decker engine webservice's API.

Enjoy!

## GET /

### Response:

- Status code 200
- Headers: []

- No response body

## GET /authors

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[{"token":"decafbad"}]
```

- Example (`application/json;charset=utf-8`):

```javascript
[{"token":"decafbad"},{"token":"decafbad"}]
```

## GET /authors/:id

### Captures:

- *id*: Integer id a person

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"token":"decafbad"}
```

## GET /comments

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

## GET /comments/:deck

### Captures:

- *deck*: String id of the deck

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

## GET /comments/:deck/:slide

### Captures:

- *deck*: String id of the deck
- *slide*: String id of the slide inside a deck

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

## POST /comments/:deck/:slide

### Captures:

- *deck*: String id of the deck
- *slide*: String id of the slide inside a deck

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- No response body

## GET /comments/:deck/:slide/:author

### Captures:

- *deck*: String id of the deck
- *slide*: String id of the slide inside a deck
- *author*: Identifying token for a person

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

## POST /comments/:deck/:slide/:author

### Captures:

- *deck*: String id of the deck
- *slide*: String id of the slide inside a deck
- *author*: Identifying token for a person

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- No response body

## DELETE /comments/:id/:token

### Captures:

- *id*: Integer id a comment
- *token*: Identifying token for a person

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- No response body

## GET /token

### Headers:

- This endpoint is sensitive to the value of the **Authorization** HTTP header.

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- No response body

